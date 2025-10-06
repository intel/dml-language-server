//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Queue of analysis to perform

#![allow(missing_docs)]

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::panic::RefUnwindSafe;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread::{self, Thread};
use std::time::SystemTime;
use crate::lint::LintCfg;
use crate::lint::LinterAnalysis;

use itertools::{Either, Itertools};

use crate::actions::analysis_storage::{AnalysisStorage, ResultChannel,
                                       TimestampedStorage, timestamp_is_newer};
use crate::analysis::{DeviceAnalysis, IsolatedAnalysis};
use crate::analysis::structure::objects::Import;

use crate::concurrency::JobToken;
use crate::file_management::CanonPath;
use crate::vfs::{TextFile, Vfs};
use crate::server::ServerToHandle;

use log::{info, debug, trace, error};
use crossbeam::channel;

// Maps in-process device jobs the timestamps of their dependencies
type InFlightDeviceJobTracker = HashMap<u64, (CanonPath, HashMap<CanonPath, SystemTime>)>;
type InFlightIsolatedJobTracker = HashMap<u64, CanonPath>;
// Queue up analysis tasks and execute them on the same thread (this is slower
// than executing in parallel, but allows us to skip indexing tasks).
pub struct AnalysisQueue {
    queue: Arc<Mutex<Vec<QueuedJob>>>,
    // Need this so we do not queue up redundant jobs
    // TODO: Allow us to cancel in-flight jobs if they are going to be
    // made redundant by a later job
    device_tracker: Arc<Mutex<InFlightDeviceJobTracker>>,
    isolated_tracker: Arc<Mutex<InFlightIsolatedJobTracker>>,
    // Handle to the worker thread where we handle analysis tasks.
    worker_thread: Thread,
}

impl AnalysisQueue {
    // Create a new queue and start the worker thread.
    pub fn init() -> AnalysisQueue {
        let queue = Arc::default();
        let device_tracker = Arc::default();
        let isolated_tracker = Arc::default();
        let worker_thread = thread::spawn({
            let queue = Arc::clone(&queue);
            let device_tracker = Arc::clone(&device_tracker);
            let isolated_tracker = Arc::clone(&isolated_tracker);
            || AnalysisQueue::run_worker_thread(queue,
                                                device_tracker,
                                                isolated_tracker)
        })
        .thread()
        .clone();

        AnalysisQueue { queue, worker_thread, device_tracker, isolated_tracker}
    }

    pub fn enqueue_linter_job(&self,
                              storage: &mut AnalysisStorage,
                              cfg: LintCfg,
                              vfs: &Vfs,
                              file: CanonPath,
                              tracking_token: JobToken) -> bool {
        match LinterJob::new(tracking_token, storage, cfg, vfs, file) {
            Ok(newjob) => {
                self.enqueue(QueuedJob::FileLinterJob(newjob));
                true
            },
            Err(desc) => {
                error!("Failed to enqueue Linter job: {}", desc);
                false
            }
        }
    }

    pub fn enqueue_isolated_job(&self,
                                storage: &mut AnalysisStorage,
                                vfs: &Vfs,
                                context: Option<CanonPath>,
                                path: CanonPath,
                                client_path: PathBuf,
                                tracking_token: JobToken) -> bool {
        match IsolatedAnalysisJob::new(tracking_token,
                                       storage,
                                       context,
                                       vfs,
                                       path,
                                       client_path) {
            // NOTE: An enqueued isolated job is always considered newer
            // than an ongoing one, so we always queue
            Ok(newjob) => {
                debug!("Enqueued isolated analysis job of {}",
                       newjob.path.as_str());
                self.enqueue(QueuedJob::IsolatedAnalysisJob(newjob));
                true
            },
            Err(desc) => {
                error!("Failed to enqueue isolated job: {}", desc);
                false
            }
        }
    }

    // Returns true if job actually queued
    pub fn enqueue_device_job(&self,
                              storage: &mut AnalysisStorage,
                              device: &CanonPath,
                              bases: HashSet<CanonPath>,
                              tracking_token: JobToken) -> bool {
        match DeviceAnalysisJob::new(tracking_token, storage, bases, device) {
            Ok(newjob) => {
                if let Some((_, previous_bases)) = self.device_tracker
                    .lock().unwrap()
                    .get(&newjob.hash) {
                        let mut newer_bases = false;
                        for base in &newjob.bases {
                            // If any base is missing or newer,
                            // the job is ok to go
                            if let Some(old_base_timestamp) =
                                previous_bases.get(&base.stored.path) {
                                    if !timestamp_is_newer(base.timestamp,
                                                           *old_base_timestamp) {
                                        continue;
                                    }
                                }
                            newer_bases = true;
                            break;
                        }
                        if !newer_bases {
                            debug!("Skipped enqueueing device analysis job of \
                                    {:?}, no new dependencies", device);
                            return false;
                        }
                    }
                debug!("Enqueued device analysis job of {:?}", device);
                self.enqueue(QueuedJob::DeviceAnalysisJob(newjob));
                true
            },
            Err(desc) => {
                error!("Failed to create device analysis job; {}", desc);
                false
            }
        }
    }

    fn enqueue(&self, queuedjob: QueuedJob) {
        trace!("enqueue job");

        {
            let mut queue = self.queue.lock().unwrap();
            // Remove any analysis jobs which this job obsoletes.
            trace!("Pre-prune queue len: {}", queue.len());
            queue.retain(|j|j.hash() != queuedjob.hash());
            trace!("Post-prune queue len: {}", queue.len());
            queue.push(queuedjob);
        }

        self.worker_thread.unpark();
    }

    fn run_worker_thread(queue: Arc<Mutex<Vec<QueuedJob>>>,
                         device_tracker: Arc<Mutex<InFlightDeviceJobTracker>>,
                         isolated_tracker: Arc<Mutex<InFlightIsolatedJobTracker>>) {
        loop {
            let job = {
                let mut queue = queue.lock().unwrap();
                if queue.is_empty() {
                    trace!("Worker thread: Queue empty");
                    None
                } else {
                    // We peek first here, so we can make sure the job is always
                    // either in the queue or the flight tracker, and as such
                    // we avoid double job queueing
                    match queue.first() {
                        Some(QueuedJob::DeviceAnalysisJob(job)) => {
                            device_tracker.lock().unwrap().insert(
                                job.hash,
                                (job.root.path.clone(),
                                 job.bases.iter().map(
                                     |base|(base.stored.path.clone(),
                                            base.timestamp)).collect()));
                        },
                        Some(QueuedJob::IsolatedAnalysisJob(job)) => {
                            isolated_tracker.lock().unwrap()
                                .insert(job.hash, job.path.clone());
                        },
                        _ => (),
                    }

                    let job = queue.remove(0);
                    match &job {
                        QueuedJob::IsolatedAnalysisJob(_) |
                        QueuedJob::DeviceAnalysisJob(_) |
                        QueuedJob::FileLinterJob(_) =>
                            queue.push(QueuedJob::Sentinel),
                        _ => (),
                    }

                    Some(job)
                }
            };

            match job {
                Some(QueuedJob::Terminate) => return,
                Some(QueuedJob::IsolatedAnalysisJob(job)) => {
                    thread::spawn({
                        let iso_tracker = Arc::clone(&isolated_tracker);
                        let hash = job.hash;
                        move ||{
                            job.process();
                            iso_tracker.lock().unwrap().remove(&hash);
                        }});
                },
                Some(QueuedJob::FileLinterJob(job)) => {
                    job.process()
                },
                Some(QueuedJob::DeviceAnalysisJob(job)) => {
                    thread::spawn({
                        let dev_tracker = Arc::clone(&device_tracker);
                        let hash = job.hash;
                        move ||{
                            job.process();
                            dev_tracker.lock().unwrap().remove(&hash);
                        }});
                },
                Some(QueuedJob::Sentinel) => {
                    trace!("Consumed sentinel");
                },
                None => thread::park(),
            }
        }
    }

    pub fn has_work(&self) -> bool {
        let queue_lock = self.queue.lock().unwrap();
        let device_lock = self.device_tracker.lock().unwrap();
        let isolated_lock = self.isolated_tracker.lock().unwrap();
        let has_work = !queue_lock.is_empty()
            || !device_lock.is_empty()
            || !isolated_lock.is_empty();
        if has_work {
            debug!("Queue still has work ({:?}, {:?}, {:?})",
                   queue_lock, device_lock, isolated_lock);
        }
        has_work
    }

    pub fn has_isolated_work(&self) -> bool {
        {
            let queue_lock = self.queue.lock().unwrap();
            for job in queue_lock.iter() {
                if matches!(job, QueuedJob::IsolatedAnalysisJob(_)) {
                    return true;
                }
            }
        }
        let isolated_lock = self.isolated_tracker.lock().unwrap();
        !isolated_lock.is_empty()
    }

    pub fn has_device_work(&self) -> bool {
        {
            let queue_lock = self.queue.lock().unwrap();
            for job in queue_lock.iter() {
                if matches!(job, QueuedJob::DeviceAnalysisJob(_)) {
                    return true;
                }
            }
        }
        let device_lock = self.device_tracker.lock().unwrap();
        !device_lock.is_empty()
    }

    pub fn working_on_isolated_for_paths(&self, paths: &HashSet<CanonPath>)
                                         -> bool {
        {
            let queue_lock = self.queue.lock().unwrap();
            for job in queue_lock.iter() {
                if let QueuedJob::IsolatedAnalysisJob(ijob) = job {
                    if paths.contains(&ijob.path) {
                        debug!("Detected there is still isolated \
                                work in queue on {:?}",
                               &ijob.path);
                        return true;
                    }
                }
            }
        }
        {
            let isolated_lock = self.isolated_tracker.lock().unwrap();
            for (_, path) in isolated_lock.iter() {
                if paths.contains(path) {
                    debug!("Detected there is still isolated \
                            work in-flight on {:?}",
                           path);
                    return true;
                }
            }
        }

        false
    }
    // NOTE: it is caller responsibility to check for device dependencies
    pub fn working_on_device_for_paths(&self, paths: &HashSet<CanonPath>)
                                       -> bool {
        {
            let queue_lock = self.queue.lock().unwrap();
            for job in queue_lock.iter() {
                if let QueuedJob::DeviceAnalysisJob(ijob) = job {
                    if paths.contains(&ijob.root.path) {
                        debug!("Detected there is still device \
                                work in queue on {:?}",
                               &ijob.root.path);
                        return true;
                    }
                }
            }
        }
        {
            let device_lock = self.device_tracker.lock().unwrap();
            for (path, _) in device_lock.values() {
                if paths.contains(path) {
                    debug!("Detected there is still device \
                            work in-flight on {:?}",
                           path);
                    return true;
                }
            }
        }

        false
    }
}

impl RefUnwindSafe for AnalysisQueue {}

impl Drop for AnalysisQueue {
    fn drop(&mut self) {
        if let Ok(mut queue) = self.queue.lock() {
            queue.push(QueuedJob::Terminate);
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum QueuedJob {
    IsolatedAnalysisJob(IsolatedAnalysisJob),
    FileLinterJob(LinterJob),
    DeviceAnalysisJob(DeviceAnalysisJob),
    Sentinel,
    Terminate,
}

impl QueuedJob {
    fn hash(&self) -> Option<u64> {
        match self {
            QueuedJob::IsolatedAnalysisJob(job) => Some(job.hash),
            QueuedJob::DeviceAnalysisJob(job) => Some(job.hash),
            QueuedJob::Sentinel => Some(0),
            QueuedJob::FileLinterJob(job) => { Some(job.hash) },
            QueuedJob::Terminate => None,
        }
    }
}

// An analysis task to be queued and executed by `AnalysisQueue`.
#[derive(Debug)]
pub struct IsolatedAnalysisJob {
    path: CanonPath,
    client_path: PathBuf,
    timestamp: SystemTime,
    report: ResultChannel,
    notify: channel::Sender<ServerToHandle>,
    content: TextFile,
    context: Option<CanonPath>,
    hash: u64,
    token: JobToken,
}

impl IsolatedAnalysisJob {
    fn new(token: JobToken,
           analysis: &mut AnalysisStorage,
           context: Option<CanonPath>,
           vfs: &Vfs,
           path: CanonPath,
           client_path: PathBuf)
           -> Result<IsolatedAnalysisJob, String> {

        // TODO: Use some sort of timestamp from VFS instead of systemtime
        let timestamp = SystemTime::now();
        // We make a hash from the device path
        let mut hasher = DefaultHasher::new();
        Hash::hash(&path, &mut hasher);
        let hash = hasher.finish();
        // TODO: error handling
        let content = vfs.snapshot_file(&path)?;
        Ok(IsolatedAnalysisJob {
            path,
            client_path,
            timestamp,
            report: analysis.report.clone(),
            notify: analysis.notify.clone(),
            hash,
            context,
            content,
            token,
        })
    }

    fn process(self) {
        info!("Started work on isolated analysis of {}", self.path.as_str());
        match IsolatedAnalysis::new(&self.path,
                                    &self.client_path,
                                    self.content,
                                    self.token.status) {
            Ok(analysis) => {
                let new_context = if analysis.is_device_file() {
                    Some(self.path.clone())
                } else {
                    self.context.clone()
                };
                let import_paths = analysis.get_import_names();
                self.report.send(TimestampedStorage::make_isolated_result(
                    self.timestamp,
                    analysis)).ok();
                self.notify.send(ServerToHandle::IsolatedAnalysisDone(
                    self.path.clone(),
                    new_context,
                    import_paths
                )).ok();
            },
            Err(e) => {
                trace!("Failed to create isolated analysis: {}", e);
                // TODO: perhaps collect this for reporting to server
            }
        }
    }
}

// An analysis task to be queued and executed by `AnalysisQueue`.
#[derive(Debug)]
pub struct DeviceAnalysisJob {
    bases: Vec<TimestampedStorage<IsolatedAnalysis>>,
    root: IsolatedAnalysis,
    import_sources: HashMap<Import, String>,
    timestamp: SystemTime,
    report: ResultChannel,
    notify: channel::Sender<ServerToHandle>,
    hash: u64,
    token: JobToken,
}

impl DeviceAnalysisJob {
    fn new(token: JobToken,
           analysis: &mut AnalysisStorage,
           bases: HashSet<CanonPath>,
           root: &CanonPath)
           -> Result<DeviceAnalysisJob, String> {
        info!("Creating a device analysis job of {:?}", root);
        // TODO: Use some sort of timestamp from VFS instead of systemtime
        let timestamp = SystemTime::now();
        // We make a hash from the sorted paths of the bases
        // NOTE/TODO: Might be a need to consider compiler options too, when
        // they are properly supported
        let mut hasher = DefaultHasher::new();

        let mut sorted_paths: Vec<_> = bases.iter().collect();
        sorted_paths.sort();
        for path in &sorted_paths {
            Hash::hash(path, &mut hasher);
        }
        let hash = hasher.finish();

        trace!("Bases are {:?}", bases.iter().collect::<Vec<&CanonPath>>());
        let root_analysis = analysis.get_isolated_analysis(root)
            .map_err(
                // NOTE/TODO: Could sanity that we fail in an expected way here
                |_|"Failed to get root isolated analysis".to_string())?.clone();
        let (bases, missing) : (Vec<TimestampedStorage<IsolatedAnalysis>>,
                                HashSet<CanonPath>) =
            bases.iter().map(|p|(p, analysis.isolated_analysis.get(p).cloned()))
            .partition_map(|(p, a)|
                           match a {
                               Some(ia) => Either::Left(ia),
                               None => Either::Right((*p).clone()),
                           });
        trace!("Missing bases are {:?}", missing.iter()
               .map(|a|a.as_str()).collect::<Vec<&str>>());
        let mut import_sources: HashMap<Import, String> = HashMap::default();
        trace!("Dependency map is {:?}", analysis.dependencies);
        trace!("Root is {:?}", root);
        for base in &bases {
            trace!("wants dependencies of {:?}", base.stored.path);
            let import_maps_of_path_at_context =
                analysis.import_map
                .get(&base.stored.path).unwrap()
                .get(&Some(root.clone())).unwrap();
            import_sources.extend(import_maps_of_path_at_context.clone());
        }

        Ok(DeviceAnalysisJob {
            bases,
            root: root_analysis,
            timestamp,
            import_sources,
            report: analysis.report.clone(),
            notify: analysis.notify.clone(),
            hash,
            token,
        })
    }

    fn process(self) {
        info!("Started work on deviceanalysis of {:?}, depending on {:?}",
              self.root.path,
              self.bases.iter().map(|i|&i.stored.path)
              .collect::<Vec<&CanonPath>>());
        match DeviceAnalysis::new(self.root,
                                  self.bases,
                                  self.import_sources,
                                  self.token.status) {
            Ok(analysis) => {
                info!("Finished device analysis of {:?}", analysis.name);
                self.notify.send(ServerToHandle::DeviceAnalysisDone(
                    analysis.path.clone())).ok();
                self.report.send(TimestampedStorage::make_device_result(
                    self.timestamp,
                    analysis)).ok();
            },
            // In general, an analysis shouldn't fail to be created
            Err(e) => {
                trace!("Failed to create device analysis: {}", e);
                // TODO: perhaps collect this for reporting to server
            }
        }
    }
}

#[derive(Debug)]
pub struct LinterJob {
    file: CanonPath,
    timestamp: SystemTime,
    report: ResultChannel,
    notify: channel::Sender<ServerToHandle>,
    content: TextFile,
    hash: u64,
    ast: IsolatedAnalysis,
    cfg: LintCfg,
    token: JobToken,
}

impl LinterJob {
    fn new(token: JobToken,
           analysis: &mut AnalysisStorage,
           cfg: LintCfg,
           vfs: &Vfs,
           file: CanonPath)
           -> Result<LinterJob, String> {

        // TODO: Use some sort of timestamp from VFS instead of systemtime
        let timestamp = SystemTime::now();
        let mut hasher = DefaultHasher::new();
        Hash::hash(&file, &mut hasher);
        let hash = hasher.finish();
        if let Ok(isolated_analysis) = analysis.get_isolated_analysis(&file) {
            Ok(LinterJob {
                file: file.to_owned(),
                timestamp,
                report: analysis.report.clone(),
                notify: analysis.notify.clone(),
                hash,
                ast: isolated_analysis.to_owned(),
                cfg,
                token,
                content: vfs.snapshot_file(&file)?,
            })
        } else {
            Err("Failed to get isolated analysis to trigger LinterJob".to_string())
        }
    }

    fn process(self) {
        debug!("Started work on isolated linting of {:?}", self.file);
        match LinterAnalysis::new(&self.file,
                                  self.content,
                                  self.cfg,
                                  self.ast,
                                  self.token.status) {
            Ok(analysis) => {
                self.report.send(TimestampedStorage::make_linter_result(
                    self.timestamp,
                    analysis)).ok();
                self.notify.send(ServerToHandle::LinterDone(
                    self.file.clone())).ok();
            },
            Err(e) => {
                trace!("Failed to create isolated linter analysis: {}", e);
            }
        }
    }
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::thread;
use std::collections::HashMap;
use std::sync::{Arc, Weak};

use crossbeam::channel::{bounded, select, Receiver, Select, Sender};

/// `ConcurrentJob` is a handle for some long-running computation
/// off the main thread. It can be used, indirectly, to wait for
/// the completion of the said computation, or directly to kill
/// the managed job
///
/// All `ConcurrentJob`s must eventually be stored in a `Jobs` table.
///
/// All concurrent activities, like spawning a thread or pushing
/// a work item to a job queue, should be covered by `ConcurrentJob`.
/// This way, the set of `Jobs` table will give a complete overview of
/// concurrency in the system, and it will be possible to wait for all
/// jobs to finish, which helps tremendously with making tests deterministic.
///
/// `JobToken` is the worker-side counterpart of `ConcurrentJob`. Dropping
/// a `JobToken` signals that the corresponding job has finished, and checking
/// if the status is still valid let's the worker know if it should bail early
///
pub struct JobStatusKeeper(Option<Arc<()>>);

impl JobStatusKeeper {
    pub fn new() -> (JobStatusKeeper, AliveStatus) {
        let arc = Arc::default();
        let weak = Arc::downgrade(&arc);
        (JobStatusKeeper(Some(arc)), AliveStatus(weak))
    }

    pub fn kill(&mut self) {
        self.0 = None;
    }

    pub fn is_killed(&self) -> bool {
        self.0.is_none()
    }
}

#[derive(Clone, Debug)]
pub struct AliveStatus(Weak<()>);

impl AliveStatus {
    pub fn is_alive(&self) -> bool {
        self.0.strong_count() > 0
    }
    pub fn assert_alive(&self) {
        if !self.is_alive() {
            panic!("Sub-job killed");
        }
    }
}

#[must_use]
pub struct ConcurrentJob {
    chan: Receiver<Never>,
    keeper: JobStatusKeeper,
}

#[derive(Debug)]
pub struct JobToken {
    _chan: Sender<Never>,
    pub status: AliveStatus,
}

#[derive(Default)]
pub struct Jobs<T> where T: PartialEq + std::hash::Hash + Clone {
    jobs: HashMap<T, ConcurrentJob>,
}

impl <T: Eq + std::hash::Hash + Clone> Jobs<T> {
    pub fn add(&mut self, ident: T, job: ConcurrentJob) {
        self.gc();
        if let Some(mut previous_job) = self.jobs.insert(ident, job) {
            // Calling 'kill' here is not meaningless (even though the Arc would
            // naturally decrement on drop), as it will prevent
            // the panic when 'previous_job' drop method is called
            previous_job.kill();
        }
    }

    pub fn kill_all(&mut self) {
        for job in self.jobs.values_mut() {
            job.kill()
        }
    }

    pub fn kill_ident(&mut self, ident: &T) {
        if let Some(job) = self.jobs.get_mut(ident) {
            job.kill();
        }
    }

    /// Blocks the current thread until all pending jobs are finished.
    pub fn wait_for_all(&mut self) {
        while !self.jobs.is_empty() {
            let done_key = {
                let mut indices = Vec::default();
                let mut select = Select::new();
                for (ident, job) in &self.jobs {
                    select.recv(&job.chan);
                    indices.push(ident);
                }

                let oper = select.select();
                let oper_index = oper.index();
                let chan = &self.jobs.get(indices[oper_index]).unwrap().chan;
                assert!(oper.recv(chan).is_err());
                indices[oper_index].clone()
            };
            drop(self.jobs.remove(&done_key).unwrap());
        }
    }

    fn gc(&mut self) {
        self.jobs.retain(|_, job| !job.is_completed())
    }
}

impl ConcurrentJob {
    pub fn new() -> (ConcurrentJob, JobToken) {
        let (tx, rx) = bounded(0);
        let (keeper, status) = JobStatusKeeper::new();
        let token = JobToken { _chan: tx, status };
        let job = ConcurrentJob { chan: rx, keeper };
        (job, token)
    }

    fn is_completed(&self) -> bool {
        is_closed(&self.chan)
    }

    fn is_killed(&self) -> bool {
        self.keeper.is_killed()
    }

    pub fn kill(&mut self) {
        self.keeper.kill();
    }
}

impl Drop for ConcurrentJob {
    fn drop(&mut self) {
        if self.is_completed() || thread::panicking() || self.is_killed() {
            return;
        }
        panic!("Internal Job Error: Orphaned concurrent job");
    }
}

impl JobToken {
    pub fn is_alive(&self) -> bool {
        self.status.is_alive()
    }

}

// We don't actually send messages through the channels,
// and instead just check if the channel is closed,
// so we use uninhabited enum as a message type
enum Never {}

/// Non-blocking.
fn is_closed(chan: &Receiver<Never>) -> bool {
    select! {
        recv(chan) -> msg => match msg {
            Err(_) => true,
            Ok(never) => match never {}
        },
        default => false,
    }
}

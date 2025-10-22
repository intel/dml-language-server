//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Stores currently completed analysis.

use log::{debug, trace, info};

use crossbeam::channel;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{Duration, SystemTime};

use crate::actions::ContextDefinition;
use crate::analysis::scope::{ContextedSymbol, ContextKey};
use crate::analysis::structure::objects::Import;
use crate::analysis::{IsolatedAnalysis, DeviceAnalysis, DMLError};

use crate::lsp_data::*;
use crate::analysis::parsing::tree::{ZeroSpan, ZeroFilePosition};
use crate::analysis::reference::Reference;
use crate::server::ServerToHandle;

use crate::lint::LinterAnalysis;

use crate::file_management::{PathResolver, CanonPath};

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum AnalysisResult {
    Isolated(IsolatedAnalysis),
    Linter(LinterAnalysis),
    Device(DeviceAnalysis),
}

impl AnalysisResult {
    pub fn path(&self) -> &Path {
        match self {
            AnalysisResult::Isolated(analysis) => &analysis.path,
            AnalysisResult::Device(analysis) => &analysis.path,
            AnalysisResult::Linter(analysis) => &analysis.path,
        }
    }
}

pub type ResultChannel = channel::Sender<TimestampedStorage<AnalysisResult>>;

#[derive(Debug, Clone)]
pub struct TimestampedStorage<T> {
    pub timestamp: SystemTime,
    pub stored: T,
}

impl TimestampedStorage<AnalysisResult> {
    pub fn make_isolated_result(timestamp: SystemTime,
                                analysis: IsolatedAnalysis)
                                -> TimestampedStorage<AnalysisResult>{
        TimestampedStorage {
            timestamp,
            stored : AnalysisResult::Isolated(analysis),
        }
    }
    pub fn make_device_result(timestamp: SystemTime,
                              analysis: DeviceAnalysis)
                              -> TimestampedStorage<AnalysisResult>{
        TimestampedStorage {
            timestamp,
            stored : AnalysisResult::Device(analysis),
        }
    }
    pub fn make_linter_result(timestamp: SystemTime,
                              analysis: LinterAnalysis)
                              -> TimestampedStorage<AnalysisResult> {
        TimestampedStorage {
            timestamp,
            stored: AnalysisResult::Linter(analysis),
        }
    }
}

// Maps file paths to maps of
// contexts to file paths that files at the path under that
// context might directly import
// {File : { Context : ImportedFiles}}
type AnalysisDirectDependencies =
    HashMap<CanonPath,
            HashMap<Option<CanonPath>, HashSet<CanonPath>>>;

// Maps paths -> contexts -> imports -> string name of resolved path
// Keeps track of how specific imports were resolved under
// specific contexts. The path indirection is used for easier
// clearing when recalculation is needed
type AnalysisImportMap =
    HashMap<CanonPath,
            HashMap<Option<CanonPath>,
                    HashMap<Import, String>>>;

// General note, all functions on AnalysisStorage assume that incoming PathBufs
// are canonicalized
#[derive(Debug)]
pub struct AnalysisStorage {
    pub notify: channel::Sender<ServerToHandle>,

    pub results: channel::Receiver<TimestampedStorage<AnalysisResult>>,
    pub report: ResultChannel,

    last_use: HashMap<CanonPath, Mutex<SystemTime>>,
    invalidators: HashMap<CanonPath, SystemTime>,
    pub isolated_analysis: HashMap<
            CanonPath, TimestampedStorage<IsolatedAnalysis>>,
    pub device_analysis: HashMap<
            CanonPath, TimestampedStorage<DeviceAnalysis>>,
    pub lint_analysis: HashMap<
            CanonPath, TimestampedStorage<LinterAnalysis>>,
    // Maps file paths to device paths that depend on them
    pub device_triggers: HashMap<CanonPath, HashSet<CanonPath>>,
    // The inverse of the above
    pub device_dependencies: HashMap<CanonPath, HashSet<CanonPath>>,

    pub dependencies: AnalysisDirectDependencies,
    pub import_map: AnalysisImportMap,

    // Maps files that need to be analyzed to contexts in which
    // they have been imported from
    pub unresolved_dependency: HashMap<CanonPath,
                                       HashSet<Option<CanonPath>>>,
}

pub fn timestamp_is_newer(later: SystemTime, previous: SystemTime) -> bool {
    previous.duration_since(later).is_err()
}

#[derive(Debug)]
pub enum AnalysisLookupError {
    NoIsolatedAnalysis,
    NoLintAnalysis,
    NoDeviceAnalysis,
    NoFile,
}

impl std::fmt::Display for AnalysisLookupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoIsolatedAnalysis => write!(f, "No Isolated Analysis Found"),
            Self::NoLintAnalysis => write!(f, "No Linting Analysis Found"),
            Self::NoDeviceAnalysis => write!(f, "No Device Analysis Found"),
            Self::NoFile => write!(f, "The file requested for could not be found"),
        }
    }
}

impl Error for AnalysisLookupError {}

// (IsolatedErrors, SemanticErrors, LintErrors)
pub type FilteredErrors = (HashMap<PathBuf, HashSet<DMLError>>,
                           HashMap<PathBuf, HashSet<DMLError>>,
                           HashMap<PathBuf, HashSet<DMLError>>);

impl AnalysisStorage {
    pub fn manipulate_isolated_analysises(&mut self) ->
        HashMap<&CanonPath, &mut IsolatedAnalysis> {
            self.isolated_analysis.iter_mut().map(
                |(p, tss)|(p, &mut tss.stored)).collect()
        }

    pub fn all_isolated_analysises(&self) ->
        HashMap<&CanonPath, &IsolatedAnalysis> {
            self.isolated_analysis.iter().map(
                |(p, tss)|(p, &tss.stored)).collect()
        }

    pub fn filtered_device_analysises_containing_file(
        &self,
        path: &CanonPath,
        filter: Option<&HashSet<ContextDefinition>>)
        -> Vec<&DeviceAnalysis>{
        self.device_triggers.get(path)
            .map(|triggers|triggers.iter()
                 .filter(
                     |p|filter.map_or(
                         true,
                         |f|f.contains(&ContextDefinition::Device((*p).clone()))))
                 .filter_map(|p|self.get_device_analysis(p).ok())
                 .collect())
            .unwrap_or_else(||vec![])
    }

    pub fn all_device_analysises_containing_file(
        &self, path: &CanonPath) -> Vec<&DeviceAnalysis> {
        self.filtered_device_analysises_containing_file(path, None)
    }

    pub fn init(notify: channel::Sender<ServerToHandle>) -> Self {
        let (report, results) = channel::unbounded();
        AnalysisStorage {
            notify,
            results,
            report,
            lint_analysis: HashMap::default(),
            isolated_analysis: HashMap::default(),
            device_analysis:  HashMap::default(),
            device_triggers:  HashMap::default(),
            device_dependencies: HashMap::default(),
            dependencies: HashMap::default(),
            import_map: HashMap::default(),
            unresolved_dependency: HashMap::default(),
            invalidators: HashMap::default(),
            last_use: HashMap::default(),
        }
    }

    pub fn context_symbol_at_pos<'t>(&'t self, pos: &ZeroFilePosition) ->
        Result<Option<ContextedSymbol<'t>>, AnalysisLookupError> {
            let canon_path = CanonPath::from_path_buf(pos.path())
                .ok_or(AnalysisLookupError::NoFile)?;
            let analysis = self.get_isolated_analysis(&canon_path)?;
            let mut context = analysis.lookup_context_symbol(pos);
            // Patch out leading 'device' context, unneeded
            if let Some(ref mut ic) = context {
                ic.remove_head_context();
            }
            Ok(context)
        }

    pub fn reference_at_pos(&self, pos: &ZeroFilePosition) ->
        Result<Option<&Reference>, AnalysisLookupError> {
            let canon_path = CanonPath::from_path_buf(pos.path())
                .ok_or(AnalysisLookupError::NoFile)?;
            let analysis = self.get_isolated_analysis(&canon_path)?;
            Ok(analysis.lookup_reference(pos))
        }

    pub fn first_context_at_pos(&self, pos: &ZeroFilePosition) ->
        Result<Option<ContextKey>, AnalysisLookupError> {
            let canon_path = CanonPath::from_path_buf(pos.path())
                .ok_or(AnalysisLookupError::NoFile)?;
            let analysis = self.get_isolated_analysis(&canon_path)?;
            Ok(analysis.lookup_first_context(pos))
        }

    pub fn has_client_file(&self, path: &Path) -> bool {
        self.isolated_analysis.keys().any(
            |cp|cp.as_path().ends_with(path))
    }

    pub fn has_isolated_analysis(&self, path: &CanonPath) -> bool {
        self.isolated_analysis.contains_key(path)
    }

    pub fn has_lint_analysis(&self, path: &CanonPath) -> bool {
        self.lint_analysis.contains_key(path)
    }

    pub fn all_dependencies(&self,
                            path: &CanonPath,
                            context: Option<&CanonPath>)
                            -> HashSet<CanonPath> {
        let mut queue = vec![path.clone()];

        let mut to_return = HashSet::default();
        while let Some(next) = queue.pop() {
            trace!("Next to handle is {:?}", next);
            if to_return.contains(&next) {
                trace!("Already handled, skip");
                continue;
            }
            to_return.insert(next.clone());

            if let Some(dependencies) = self.dependencies
                .get(&next).and_then(|cm|cm.get(&context.cloned())) {
                    trace!("Extended with {:?} through {:?} at {:?}",
                           dependencies.iter().collect::<Vec<_>>(),
                           next, context);
                    queue.extend(
                        dependencies.iter().cloned());
                }
        }
        debug!("Full dependencies of {} under {:?} are {:?}",
               path.as_str(), context, to_return);
        to_return
    }

    pub fn get_file_contexts(&self, path: &CanonPath)
                             -> HashSet<Option<CanonPath>> {
        if let Some(deps) = self.dependencies.get(path) {
            deps.keys().cloned().collect()
        } else {
            vec![None].into_iter().collect()
        }
    }

    /// Update all dependency info involving a specific canon path
    pub fn update_dependencies(&mut self,
                               path: &CanonPath,
                               resolver: &PathResolver) {
        debug!("Updating dependencies of {}", path.as_str());

        let mut contexts: HashSet<Option<CanonPath>> =
            if let Some(dependency_map) =
            self.dependencies.get(path) {
                dependency_map.keys().cloned().collect()
            } else {
                HashSet::default()
            };
        contexts.insert(None);

        debug!("Full contexts for {:?} are {:?}", path, contexts);

        if self.get_isolated_analysis(path).map_or(
            false, |a|a.is_device_file()) {
            contexts.insert(Some(path.clone()));
        }

        if let Some(previously_failed) =
            self.unresolved_dependency.remove(path) {
            contexts.extend(previously_failed);
        }

        trace!("Will update for contexts {:?}", contexts);

        for context in &contexts {
            // This is needed because circular dependencies might occur here
            let mut updated = HashSet::default();
            self.update_dependencies_aux(path, context.as_ref(), resolver,
                                         &mut updated);

        }


        // Set up device triggers
        let paths: HashSet<CanonPath> = contexts.iter()
            .flat_map(|c|self.all_dependencies(path, c.as_ref())
                      .into_iter())
            .collect();

        let mut target_devices: Vec<CanonPath> = vec![];

        if self.get_isolated_analysis(path)
            .map_or(false, |a|a.is_device_file()) {
                target_devices.push(path.clone());
            } else {
                // Remove ourselves from the trigee list of any file
                // we depend on
                for trigger_path in &paths {
                    self.device_triggers.get_mut(trigger_path)
                        .map(|e|e.remove(path));
                    if self.device_triggers.get(trigger_path)
                        .map_or(false, |e|e.is_empty()) {
                        self.device_triggers.remove(trigger_path);
                    }
                }
            }

        if let Some(trigger_paths) = self.device_triggers.get(path) {
            target_devices.extend(trigger_paths.iter().cloned());
        }

        // If we trigger some device, things that depend on us should
        // also trigger that device

        for trigger_path in &paths {
            for device in &target_devices {
                let entry = self.device_triggers
                    .entry(trigger_path.clone()).or_default();
                entry.insert((*device).clone());
            }
        }

        // rebuild dependencies
        // TODO: this could be optimized by folding it into the logic above,
        // but I cannot imagine it's worth the hassle
        self.device_dependencies.clear();
        for (path, devices) in &self.device_triggers {
            for device in devices {
                self.device_dependencies.entry(device.clone())
                    .or_default().insert(path.clone());
            }
        }
    }

    fn update_dependencies_aux(&mut self,
                               path: &CanonPath,
                               context: Option<&CanonPath>,
                               resolver: &PathResolver,
                               updated: &mut HashSet<CanonPath>) {
        // TODO: This is all painfully un-optimized. We resolve many
        // paths several times and we do a lot of copying of paths around
        if !updated.contains(path) {
            updated.insert(path.clone());
        } else {
            return;
        }
        debug!("Updating dependencies of {} under {:?}",
               path.as_str(), context);

        if !self.isolated_analysis.contains_key(path) {
            trace!("Supposedly real file {} did not have an analysis",
                   path.as_str());
            // Clear out previous dependencies
            self.dependencies.remove(path);
            self.import_map.remove(path);
            return;
        }

        let mut next_to_recurse: HashSet<CanonPath> = HashSet::default();

        {
            let analysis = &mut self.isolated_analysis.get_mut(path)
                .unwrap().stored;
            // Dependencies for this path
            let all_dependencies = self.dependencies.entry(path.clone())
                .or_default();
            // Dependencies for this context
            let context_dependencies =
                all_dependencies.entry(context.cloned()).or_default();
            context_dependencies.clear();
            context_dependencies.insert(path.clone());

            let (direct_dependencies, missing) =
                analysis.resolve_imports(resolver, context);

            // Similarly, import map for this path and context
            let all_import_maps =
                self.import_map.entry(path.clone()).or_default();
            let context_import_maps =
                all_import_maps.entry(context.cloned()).or_default();

            debug!("While updating dependencies, these were not resolved {:?}",
                   missing);

            for (dependency, import) in direct_dependencies {
                context_dependencies.insert(dependency.clone());
                context_import_maps.insert(
                    import, dependency.as_str().to_string());

                // We need to request an analysis here, because this dependency
                // might have just been found due to changed context
                if self.isolated_analysis.contains_key(&dependency) {
                    next_to_recurse.insert(dependency);
                } else {
                    debug!("-> server; analyze {}", dependency.as_str());
                    self.notify.send(ServerToHandle::AnalysisRequest(
                        dependency.clone().to_path_buf(),
                        context.cloned())).ok();
                    self.unresolved_dependency
                        .entry(dependency.clone())
                        .or_default().insert(context.cloned());
                }
            }

            trace!("Updated dependencies of {} under {:?} to: {:?}",
                   path.as_str(), context, context_dependencies);
        }

        // Recurse so that analysises we depend on also have their
        // dependencies correct
        for dependency in &next_to_recurse {
            self.update_dependencies_aux(dependency, context,
                                         resolver, updated);
        }
    }

    // update dependencies based on each device file context, and None
    pub fn update_all_context_dependencies(&mut self, resolver: PathResolver) {
        trace!("Updating all dependencies");
        // Technically, we're overkilling here. We will double-update
        // dependenies for paths which have new analysises
        self.update_analysis(&resolver);
        let dependencies_to_update: HashSet<CanonPath> =
            self.isolated_analysis.keys().cloned().collect();
        for file in dependencies_to_update {
            self.update_dependencies(&file, &resolver);
        }
    }

    pub fn discard_dependant_device_analysis(&mut self, path: &Path) {
        // There is probably a more rustic way to do this
        let device_trigger_holder = self.device_triggers.clone();
        self.device_analysis.retain(
            |k, _| !device_trigger_holder.get(k)
                .unwrap()
                .contains(&CanonPath::from_path_buf(
                    path.to_path_buf()).unwrap()));
    }

    pub fn update_analysis(&mut self, resolver: &PathResolver) {
        let mut device_analysis = vec![];
        let mut results_holder = vec![];
        debug!("Updating stored analysises");
        for r in self.results.try_iter() {
            results_holder.push(r);
        }

        // Do this in two passes, once to get new analysis, and once to
        // update the dependencies
        let mut dependencies_to_update: HashSet<CanonPath> = HashSet::default();

        for result in results_holder {
            let timestamp = result.timestamp;
            match result.stored {
                AnalysisResult::Isolated(analysis) => {
                    let canon_path = analysis.path.clone();
                    trace!("Handling isolated analysis on {}",
                           canon_path.as_str());
                    if self.isolated_analysis.get(&canon_path).map_or(
                        true,
                        |prev| timestamp_is_newer(timestamp, prev.timestamp)) {
                        trace!("invalidators are {:?}", self.invalidators);
                        if self.invalidators.get(&canon_path).map_or(
                            true, |invalid| timestamp_is_newer(timestamp,
                                                               *invalid)) {
                            trace!("was new, or fresh compared to previous");
                            dependencies_to_update.insert(canon_path.clone());
                            self.isolated_analysis.insert(canon_path.clone(),
                                                          TimestampedStorage {
                                                              timestamp,
                                                              stored: analysis,
                                                          });
                            self.last_use.insert(canon_path.clone(),
                                                 Mutex::new(SystemTime::now()));
                            self.update_last_use(&canon_path);
                            self.discard_dependant_device_analysis(&canon_path);
                            self.invalidators.remove(&canon_path);
                        } else {
                            trace!("was pre-emptively invalidated");
                        }
                    }
                },
                AnalysisResult::Device(_) => {
                    device_analysis.push(result)
                },
                AnalysisResult::Linter(analysis) => {
                    let canon_path = analysis.path.clone();
                    self.lint_analysis.insert(canon_path.clone(),
                                              TimestampedStorage {
                                                timestamp,
                                                stored: analysis,
                                            });
                },
            }
        }

        for path in dependencies_to_update {
            self.update_dependencies(&path, resolver);
        }

        for analysisresult in device_analysis {
            let timestamp = analysisresult.timestamp;
            if let AnalysisResult::Device(analysis) = analysisresult.stored {
                let canon_path = analysis.path.clone();
                trace!("Handling device analysis on {}", canon_path.as_str());
                if self.device_analysis.get(&canon_path).map_or(
                    true, |prev| timestamp_is_newer(timestamp,
                                                    prev.timestamp)) {
                    trace!("was new, or fresh compared to previous");
                    // This should be guaranteed
                    let invalidators = self.all_dependencies(
                        &canon_path, Some(&canon_path));
                    if !invalidators.iter().any(
                        // NOTE: This is where last-use gets updated for
                        // dependee analysises
                        |p|self.isolated_analysis.get(p).map(
                            |i|!timestamp_is_newer(timestamp,
                                                   i.timestamp))
                            .unwrap_or(false)) {
                        debug!("was not invalidated by recent \
                                isolated analysis");
                        self.device_analysis.insert(canon_path,
                                                    TimestampedStorage {
                                                        timestamp,
                                                        stored: analysis,
                                                    });
                    }
                }
            } else {
                unreachable!("Enum variant should be device analysis");
            }
        }

        trace!("Now knows about these isolated analysises: {:?}",
               self.isolated_analysis.keys().collect::<Vec<&CanonPath>>());
        trace!("Now knows about these device analysises: {:?}",
               self.device_analysis.keys().collect::<Vec<&CanonPath>>());
    }

    pub fn get_linter_analysis<'a>(&'a self, path: &CanonPath) ->
        Result<&'a LinterAnalysis, AnalysisLookupError> {
            trace!("Looking for linter analysis of {:?}", path);
            let analysis = self.lint_analysis.get(path)
                .map(|storage|&storage.stored)
                .ok_or(AnalysisLookupError::NoLintAnalysis);
            if analysis.is_err() {
                trace!("Failed to find linter analysis");
            }
            analysis
        }

    pub fn discard_overly_old_analysis(&mut self, max_age: Duration) {
        let now = SystemTime::now();
        for path in self.last_use.keys().cloned().collect::<Vec<CanonPath>>() {
            if now.duration_since(*self.last_use.get(&path)
                                  .unwrap().lock().unwrap())
                .map_or(false, |duration|duration > max_age) {
                    info!("Discarded analysis of {} due to it being \
                           unused for too long.", path.as_str());
                    self.mark_file_dirty(&path);
                }
        }
    }

    pub fn get_isolated_analysis<'a>(&'a self, path: &CanonPath) ->
        Result<&'a IsolatedAnalysis, AnalysisLookupError> {
            trace!("Looking for isolated analysis of {}", path.as_str());
            let analysis = self.isolated_analysis.get(path)
                .map(|storage|&storage.stored)
                .ok_or(AnalysisLookupError::NoIsolatedAnalysis);
            if analysis.is_err() {
                trace!("Failed to find isolated analysis");
            } else {
                self.update_last_use(path);
            }
            analysis
        }

    fn update_last_use(&self, path: &CanonPath) {
        if let Some(mut_lock) = self.last_use.get(path) {
            let now = SystemTime::now();
            debug!("Updated last-use of {} to {:?}", path.as_str(), now);
            *mut_lock.lock().unwrap() = now;
        }
    }

    pub fn get_device_analysis<'a>(&'a self, path: &CanonPath) ->
        Result<&'a DeviceAnalysis, AnalysisLookupError> {
            trace!("Looking for device analysis of {:?}", path);
            let analysis = self.device_analysis.get(path)
                .map(|storage|&storage.stored)
                .ok_or(AnalysisLookupError::NoDeviceAnalysis);
            if analysis.is_err() {
                trace!("Failed to find device analysis");
            } else {
                for p in self.dependencies.get(path)
                    .map_or_else(||vec![],
                                 |map|map.values().collect())
                    .into_iter()
                    .flat_map(|set|set.iter())
                {
                    self.update_last_use(p);
                }
            }
            analysis
        }

    pub fn mark_file_dirty(&mut self, path: &CanonPath) {
        trace!("Marked {} as dirty", path.as_str());
        self.isolated_analysis.remove(path);
        self.lint_analysis.remove(path);
        self.discard_dependant_device_analysis(path.as_path());
        self.invalidators.insert(path.clone(), SystemTime::now());
        self.last_use.remove(path);
    }

    pub fn has_dependencies(&mut self, path: &CanonPath) -> bool {
        for path in self.all_dependencies(path, Some(path)) {
            if !self.isolated_analysis.contains_key(&path) {
                return false;
            }
        }
        true
    }

    pub fn device_newer_than_dependencies(&self, path: &CanonPath) -> bool {
        debug!("Checking if {:?} needs a newer analysis", path);
        if let Some(device_timestamp) = self.device_analysis.get(path)
            .map(|device|device.timestamp)
        {
            debug!("Timestamp is {:?}", device_timestamp);
            for dependee_path in self.all_dependencies(path, Some(path)) {
                // NOTE: This means that calling this function with a missing
                // isolated analysis will not tell you the device needs to be
                // remade, which is correct (later adding the missing analysis
                // will then have a newer timestamp)
                if let Some(dependee_timestamp) = self.isolated_analysis
                    .get(&dependee_path)
                    .map(|dependee|dependee.timestamp) {
                        if timestamp_is_newer(dependee_timestamp,
                                              device_timestamp) {
                            debug!("Outdated by {:?}", dependee_timestamp);
                            return false;
                        }
                    }
            }
            true
        } else {
            debug!("No analysis, needs analysis");
            false
        }
    }

    pub fn gather_errors(&mut self, filter: Option<&HashSet<ContextDefinition>>)
                         -> FilteredErrors {
        // By this being a hashset, we will not double-report any errors
        let mut isolated_errors: HashMap<PathBuf, HashSet<DMLError>>
            = HashMap::default();
        let mut device_errors: HashMap<PathBuf, HashSet<DMLError>>
            = HashMap::default();
        let mut lint_errors: HashMap<PathBuf, HashSet<DMLError>>
            = HashMap::default();
        let all_files: HashSet<&CanonPath> =
            self.device_analysis.keys()
            .chain(self.isolated_analysis.keys())
            .chain(self.lint_analysis.keys())
            .collect();
        for file in all_files {
            if let Some((ifile, ierrors)) = self.gather_local_errors(file) {
                isolated_errors.entry(ifile)
                    .or_default()
                    .extend(ierrors.into_iter());
            }

            lint_errors.entry(file.clone().into())
                .or_default()
                .extend(self.gather_linter_errors(file).into_iter());

            // Only report device errors if this analysis context is active
            if filter.map_or(true, |f|f.contains(&file.clone().into())) {
                for (dfile, errors) in self.gather_device_errors(file) {
                    device_errors.entry(dfile.clone())
                    .or_default()
                    .extend(errors.into_iter());
                    if !self.has_client_file(&PathBuf::from("dml-builtins.dml")) {
                        device_errors.entry(dfile.clone())
                            .or_default().insert(
                                DMLError {
                                    span: ZeroSpan::invalid(dfile.clone()),
                                    description: "Could not find required builtin \
                                                  file 'dml-builtins.dml'".to_string(),
                                    related: vec![],
                                    severity: Some(DiagnosticSeverity::ERROR),
                                });
                    }
                }
            }
        }
        (isolated_errors, device_errors, lint_errors)
    }

    pub fn gather_linter_errors(&self, path: &CanonPath) -> Vec<DMLError> {
        // This is not a user-initiated request, so it's ok to drop
        // the error here
        if let Ok(linter_analysis) = self.get_linter_analysis(path) {
            linter_analysis.errors.clone()
        } else {
            vec![]
        }
    }

    pub fn gather_local_errors(&self, path: &CanonPath)
                               -> Option<(PathBuf, Vec<DMLError>)> {
        // This is not a user-initiated request, so it's ok to drop
        // the error here
        self.get_isolated_analysis(path)
            .ok()
            .map(|a|(a.clientpath.clone(), a.errors.clone()))
    }

    pub fn gather_device_errors(&self, path: &CanonPath)
                                -> HashMap<PathBuf, Vec<DMLError>> {
        // This is not a user-initiated request, so it's ok to drop
        // the error here
        self.get_device_analysis(path)
            .ok()
            .map_or(HashMap::default(),|a|a.errors.clone())
    }
}

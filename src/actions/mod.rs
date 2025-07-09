//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Actions that the DLS can perform: responding to requests, watching files,
//! etc.

use log::{debug, info, trace, error, warn};
use thiserror::Error;
use crossbeam::channel;
use serde::Deserialize;
use serde_json::json;

use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::path::{Path, PathBuf};
use std::fs;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

use lsp_types::notification::{DidChangeWatchedFiles};
use lsp_types::request::{RegisterCapability, UnregisterCapability};
use lsp_types::Unregistration;

use crate::actions::analysis_storage::AnalysisStorage;
use crate::actions::analysis_queue::AnalysisQueue;
use crate::actions::progress::{AnalysisProgressNotifier,
                               AnalysisDiagnosticsNotifier,
                               DiagnosticsNotifier,
                               ProgressNotifier};
use crate::analysis::DMLError;
use crate::analysis::IMPLICIT_IMPORTS;
use crate::analysis::structure::expressions::Expression;
use crate::concurrency::{Jobs, ConcurrentJob};
use crate::config::{Config, DeviceContextMode};
use crate::file_management::{PathResolver, CanonPath};
use crate::lint::{LintCfg, maybe_parse_lint_cfg};
use crate::lsp_data;
use crate::lsp_data::*;
use crate::lsp_data::ls_util::{dls_to_range, dls_to_location};

use crate::server::{Output, ServerToHandle, error_message,
                    Request, RequestId, SentRequest};
use crate::server::message::RawResponse;
use crate::server::dispatch::HandleResponseType;
use crate::Span;
use crate::span;
use crate::span::{ZeroIndexed, FilePosition};
use crate::vfs::Vfs;

// Define macros before submodules
macro_rules! parse_file_path {
    ($uri: expr, $log_name: expr) => {
        ignore_non_file_uri!(parse_file_path($uri), $uri, $log_name)
    };
}

// TODO: Support non-`file` URI schemes in VFS. We're currently ignoring them because
// we don't want to crash the DLS in case a client opens a file under different URI scheme
// like with git:/ or perforce:/ (Probably even http:/? We currently don't support remote schemes).
macro_rules! ignore_non_file_uri {
    ($expr: expr, $uri: expr, $log_name: expr) => {
        $expr.map_err(|_| {
            log::trace!("{}: Non-`file` URI scheme, ignoring: {:?}", $log_name, $uri);
        })
    };
}

pub mod analysis_storage;
pub mod analysis_queue;
pub mod hover;
pub mod notifications;
pub mod requests;
pub mod progress;
pub mod work_pool;

/// Persistent context shared across all requests and notifications.
pub enum ActionContext<O: Output> {
    /// Context after server initialization.
    Init(InitActionContext<O>),
    /// Context before initialization.
    Uninit(UninitActionContext),
}

#[derive(Error, Debug)]
#[error("Initialization error")]
pub struct InitError;

impl From<()> for InitError {
    fn from(_: ()) -> InitError {
        InitError {}
    }
}

impl <O: Output> ActionContext<O> {
    /// Construct a new, uninitialized context.
    pub fn new(
        vfs: Arc<Vfs>,
        config: Arc<Mutex<Config>>,
        notify: channel::Sender<ServerToHandle>,
    ) -> ActionContext<O> {
        ActionContext::Uninit(UninitActionContext::new(
            Arc::new(Mutex::new(AnalysisStorage::init(notify))),
            vfs, config))
    }

    /// Initialize this context, returns `Err(())` if it has already been initialized.
    pub fn init(
        &mut self,
        init_options: InitializationOptions,
        client_capabilities: lsp_data::ClientCapabilities,
        out: O,
    ) -> Result<(), InitError> {
        let ctx = match *self {
            ActionContext::Uninit(ref uninit) => {
                // This means other references to the config will mismatch if
                // we update it, but I am fairly sure they do not exist
                let new_config = init_options.settings.as_ref()
                    .map(|settings|Arc::new(Mutex::new(settings.dml.clone())))
                    .unwrap_or(Arc::clone(&uninit.config));

                let mut ctx = InitActionContext::new(
                    Arc::clone(&uninit.analysis),
                    Arc::clone(&uninit.vfs),
                    new_config,
                    client_capabilities,
                    uninit.pid,
                    init_options.cmd_run,
                );
                ctx.init(init_options, out);
                ctx
            }
            ActionContext::Init(_) => return Err(().into()),
        };
        trace!("Inited context has {:?} as config",
               ctx.config.lock().unwrap());
        *self = ActionContext::Init(ctx);

        Ok(())
    }

    /// Returns an initialiased wrapped context,
    /// or `Err(())` if not initialised.
    pub fn inited(&self) -> Result<InitActionContext<O>, InitError> {
        match *self {
            ActionContext::Uninit(_) => Err(().into()),
            ActionContext::Init(ref ctx) => Ok(ctx.clone()),
        }
    }

    pub fn pid(&self) -> u32 {
        match self {
            ActionContext::Uninit(ctx) => ctx.pid,
            ActionContext::Init(ctx) => ctx.pid,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompilationDefine {
    pub name: String,
    pub expression: Expression,
}


#[derive(Clone, Debug, Default)]
pub struct CompilationInfo {
    pub extra_defines: Vec<CompilationDefine>,
    pub include_paths: HashSet<PathBuf>,
}

pub type CompilationInfoStorage = HashMap<CanonPath, CompilationInfo>;

#[derive(PartialEq, Debug)]
pub enum AnalysisProgressKind {
    Isolated,
    // NOTE: device implies waiting on isolated analysis
    // for that file and any dependencies
    Device,
    // NOTE: this implies waiting on ALL isolated analysises,
    // and then for all device dependencies that
    // might trigger any of the paths specified
    // This means that All|Device and All|DeviceDependencies
    // are equivalent
    DeviceDependencies,
}

#[derive(PartialEq, Debug)]
pub enum AnalysisCoverageSpec {
    Paths(Vec<CanonPath>),
    All,
}

#[derive(PartialEq, Debug)]
pub enum AnalysisWaitKind {
    // Waits for work on the specified to finish
    Work,
    // Wait for the specified to exist at all, even if it may
    // soon be out-of-date
    Existence,
}


#[derive(PartialEq, Debug)]
pub enum AnalysisStateResponse {
    // The spec is true, AND the related analysises actually exist
    Achieved,
    // The spec is true, but we dont actually have all the anlysises
    // that were requested
    Satisfied,
    // The server cancelled this wait, for whatever reason (currently unused)
    Cancelled,
    // Used to ping a channel to see if it is still alive, needed to drop
    // waits in requests that time out
    Ping,
}

#[derive(Debug)]
pub struct AnalysisStateWaitDefinition {
    progress_kind: AnalysisProgressKind,
    coverage: AnalysisCoverageSpec,
    wait_kind: AnalysisWaitKind,
    response: channel::Sender<AnalysisStateResponse>,
}

/// Persistent context shared across all requests and actions after the DLS has
/// been initialized.
// NOTE: This is sometimes cloned before being passed to a handler
// (not concurrent), so make sure shared info is behind Arcs, and that no overly
// large data structures are stored.
#[derive(Clone)]
pub struct InitActionContext<O: Output> {
    pub analysis: Arc<Mutex<AnalysisStorage>>,
    vfs: Arc<Vfs>,
    // Queues analysis jobs so that we don't over-use the CPU.
    analysis_queue: Arc<AnalysisQueue>,
    current_notifier: Arc<Mutex<Option<String>>>,

    // Set to true when a potentially mutating request is received. Set to false
    // if a change arrives. We can thus tell if the DLS has been quiescent while
    // waiting to mutate the client state.
    pub quiescent: Arc<AtomicBool>,

    // the root workspaces
    pub workspace_roots: Arc<Mutex<Vec<Workspace>>>,

    // directly opened files
    pub direct_opens: Arc<Mutex<HashSet<CanonPath>>>,
    pub compilation_info: Arc<Mutex<CompilationInfoStorage>>,

    // maps files to the paths of device contexts they should be
    // analyzed under
    pub device_active_contexts: Arc<Mutex<ActiveDeviceContexts>>,
    previously_checked_contexts: Arc<Mutex<ActiveDeviceContexts>>,

    prev_changes: Arc<Mutex<HashMap<PathBuf, i32>>>,

    active_waits: Arc<Mutex<Vec<AnalysisStateWaitDefinition>>>,

    outstanding_requests: Arc<Mutex<HashMap<RequestId,
                                            Box<HandleResponseType<O>>>>>,

    pub config: Arc<Mutex<Config>>,
    pub lint_config: Arc<Mutex<LintCfg>>,
    pub active_watch: Arc<Mutex<Option<FileWatch>>>,
    pub sent_warnings: Arc<Mutex<HashSet<(u64, PathBuf)>>>,
    jobs: Arc<Mutex<Jobs>>,
    pub client_capabilities: Arc<lsp_data::ClientCapabilities>,
    pub has_notified_missing_builtins: bool,
    /// Whether the server is performing cleanup (after having received
    /// 'shutdown' request), just before final 'exit' request.
    pub shut_down: Arc<AtomicBool>,
    pub pid: u32,
}

/// Persistent context shared across all requests and actions before the DLS has
/// been initialized.
pub struct UninitActionContext {
    analysis: Arc<Mutex<AnalysisStorage>>,
    vfs: Arc<Vfs>,
    config: Arc<Mutex<Config>>,
    pid: u32,
}

impl UninitActionContext {
    fn new(
        analysis: Arc<Mutex<AnalysisStorage>>,
        vfs: Arc<Vfs>,
        config: Arc<Mutex<Config>>,
    ) -> UninitActionContext {
        UninitActionContext { analysis, vfs, config, pid: ::std::process::id() }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ContextDefinition {
    // The path here is the "topmost" file which the simulated context imports
    Simulated(CanonPath), // TODO: Actually implement support for this
    Device(CanonPath),
}

impl From<CanonPath> for ContextDefinition {
    fn from(val: CanonPath) -> ContextDefinition {
        ContextDefinition::Device(val)
    }
}

impl ContextDefinition {
    fn as_canon_path(&self) -> &CanonPath {
        match self {
            ContextDefinition::Simulated(p) => p,
            ContextDefinition::Device(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourcedDMLError {
    pub error: DMLError,
    pub source: &'static str,
}

impl SourcedDMLError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::new(
            dls_to_range(self.error.span.range),
            self.error.severity,
            None,
            Some(self.source.to_string()),
            self.error.description.clone(),
            Some(
                self.error.related.iter().map(
                    |(span, desc)|DiagnosticRelatedInformation {
                        location: dls_to_location(span),
                        message: desc.clone(),
                    }).collect()),
            None
        )
    }
}

pub type ActiveDeviceContexts = HashSet<ContextDefinition>;

impl <O: Output> InitActionContext<O> {
    fn new(
        analysis: Arc<Mutex<AnalysisStorage>>,
        vfs: Arc<Vfs>,
        config: Arc<Mutex<Config>>,
        client_capabilities: lsp_data::ClientCapabilities,
        pid: u32,
        _client_supports_cmd_run: bool,
    ) -> InitActionContext<O> {
        let lint_config = Arc::new(Mutex::new(
            config.lock().unwrap().lint_cfg_path.clone()
                .and_then(maybe_parse_lint_cfg)
                .unwrap_or_default()));
        InitActionContext {
            vfs,
            analysis,
            analysis_queue: Arc::new(AnalysisQueue::init()),
            current_notifier: Arc::default(),
            config,
            lint_config,
            jobs: Arc::default(),
            direct_opens: Arc::default(),
            quiescent: Arc::new(AtomicBool::new(false)),
            prev_changes: Arc::default(),
            client_capabilities: Arc::new(client_capabilities),
            active_watch: Arc::default(),
            has_notified_missing_builtins: false,
            //client_supports_cmd_run,
            active_waits: Arc::default(),
            outstanding_requests: Arc::default(),
            shut_down: Arc::new(AtomicBool::new(false)),
            pid,
            workspace_roots: Arc::default(),
            compilation_info: Arc::default(),
            sent_warnings: Arc::default(),
            device_active_contexts: Arc::default(),
            previously_checked_contexts: Arc::default(),
        }
    }

    fn add_direct_open(&self, path: PathBuf) {
        let canon_path: CanonPath = path.into();
        self.direct_opens.lock().unwrap().insert(canon_path);
    }

    fn remove_direct_open(&self, path: PathBuf) {
        let canon_path: CanonPath = path.into();
        if !self.direct_opens.lock().unwrap().remove(&canon_path) {
            debug!("Tried to remove a directly opened file ({:?}) \
                    that wasnt tracked", canon_path);
        }
    }

    fn init(&mut self,
            _init_options: InitializationOptions,
            out: O) {
        self.update_compilation_info(&out)
    }

    pub fn update_workspaces(&self,
                             mut add: Vec<Workspace>,
                             remove: Vec<Workspace>) {
        if let Ok(mut workspaces) = self.workspace_roots.lock() {
            workspaces.retain(|workspace|
                              remove.iter().all(|rem|rem != workspace));
            workspaces.append(&mut add);
        }
    }

    fn update_linter_config(&self, _out: &O) {
        trace!("Updating linter config");
        if let Ok(config) = self.config.lock() {
            *self.lint_config.lock().unwrap() =
                config.lint_cfg_path.clone()
                .and_then(maybe_parse_lint_cfg)
                .unwrap_or_default();
        }
    }

    pub fn update_compilation_info(&self, out: &O) {
        trace!("Updating compile info");
        if let Ok(config) = self.config.lock() {
            if let Some(compile_info) = &config.compile_info_path {
                if let Some(canon_path) = CanonPath::from_path_buf(
                    compile_info.clone()) {
                    let workspaces = self.workspace_roots.lock().unwrap();
                    if !workspaces.is_empty() &&
                        workspaces.iter().any(
                            |root|parse_file_path!(&root.uri, "workspace")
                                .map_or(false, |p|canon_path.as_path()
                                        .starts_with(p))) {
                            crate::server::warning_message(
                                out,
                                "Compilation info file is not under \
                                 any workspace root, might be configured \
                                 for a different workspace.".to_string());
                        }
                }
                match self.compilation_info_from_file(compile_info) {
                    Ok(compilation_info) => {
                        trace!("Updated to {:?}", compilation_info);
                        {
                            let mut ci = self.compilation_info.lock().unwrap();
                            *ci = compilation_info;
                        }
                        self.analysis.lock().unwrap()
                            .update_all_context_dependencies(
                                self.construct_resolver());
                    },
                    Err(e) => {
                        error!("Failed to update compilation info: {}", e);
                        error_message(
                            out,
                            format!("Could not update compilation info: {}",
                                    e));
                    }
                }
            } else {
                trace!("No compile info path");
            }
        } else {
            trace!("Failed to lock config");
        }
    }

    pub fn report_errors(&self, output: &O) {
        self.update_analysis();
        let filter = Some(self.device_active_contexts.lock().unwrap().clone());
        let (isolated, device, mut lint) =
            self.analysis.lock().unwrap().gather_errors(filter.as_ref());
        let notifier = AnalysisDiagnosticsNotifier::new("indexing".to_string(),
                                                        output.clone());
        notifier.notify_begin_diagnostics();
        let config = self.config.lock().unwrap();
        if !config.linting_enabled {
            // A slightly hacky way to not report linting errors
            // when we have done linting but then turned off the setting
            lint.clear();
        }
        let files: HashSet<&PathBuf> =
            isolated.keys()
            .chain(device.keys())
            .chain(lint.keys())
            .collect();
        let direct_opens = self.direct_opens.lock().unwrap();
        for file in files {
            let mut sorted_errors: Vec<SourcedDMLError> =
                isolated.get(file).into_iter().flatten().cloned()
                .map(|e|e.with_source("dml"))
                .chain(device.get(file).into_iter().flatten().cloned()
                       .map(|e|e.with_source("dml")))
                .chain(
                    lint.get(file).into_iter().flatten()
                        .filter(
                            |_|!config.lint_direct_only
                                || direct_opens.contains(
                                    &file.clone().into())
                        ).cloned()
                        .map(|e|e.with_source("dml-lint")))
                .collect();
            debug!("Reporting errors for {:?}", file);
            // Sort by line
            sorted_errors.sort_unstable_by(
                |e1, e2|if e1.error.span.range > e2.error.span.range {
                    Ordering::Greater
                } else {
                    Ordering::Less
                });
            match parse_uri(file.to_str().unwrap()) {
                Ok(url) => notifier.notify_publish_diagnostics(
                    PublishDiagnosticsParams::new(
                        url,
                        sorted_errors.iter()
                            .map(SourcedDMLError::to_diagnostic).collect(),
                        None)),
                // The Url crate does not report interesting errors
                Err(_) => error!("Could not convert {:?} to Url", file),
            }
        }
        notifier.notify_end_diagnostics();
    }

    pub fn compilation_info_from_file(&self, path: &PathBuf) ->
        Result<CompilationInfoStorage, String> {
            debug!("Reading compilation info from {:?}",
                   path);
            let file_content = fs::read_to_string(path).map_err(
                |e|e.to_string())?;
            trace!("Content is {:?}", file_content);
            #[allow(dead_code)]
            #[derive(Deserialize)]
            struct FileInfo {
                dmlc_flags: Vec<String>,
                includes: Vec<PathBuf>,
            }
            type CompileCommands = HashMap<PathBuf, FileInfo>;
            let json_structure: CompileCommands =
                serde_json::from_str(&file_content).map_err(|e|e.to_string())?;
            let mut new_compinfo = CompilationInfoStorage::default();
            for (file, file_info) in json_structure {
                // This is sanity, by design all files in this file should be
                // .dml
                if let Some(extension) = file.extension() {
                    if extension == "dml" {
                        let FileInfo {
                            includes, ..
                        } = file_info;
                        if let Some(canon_path) = CanonPath::from_path_buf(file)
                        {
                            let compentry = new_compinfo.entry(
                                canon_path).or_insert(CompilationInfo {
                                    extra_defines: vec![],
                                    include_paths : HashSet::default(),
                                });
                            // TODO: For now, ignore flags since we have no
                            // means to pass them to device analysis anyway
                            compentry.include_paths
                                .extend(includes.into_iter());
                        }
                    } else {
                        warn!(
                            "File in compile information file is not .dml; \
                             {:?}",
                            file
                        );
                    }
                }
            }
            Ok(new_compinfo)
        }

    pub fn update_analysis(&self) {
        self.analysis.lock().unwrap()
            .update_analysis(&self.construct_resolver());
    }

    pub fn trigger_device_analysis(&self, file: &Path, out: &O) {
        let canon_path: CanonPath = file.to_path_buf().into();
        debug!("triggering devices dependant on {}", canon_path.as_str());
        self.update_analysis();
        let maybe_triggers = self.analysis.lock().unwrap().device_triggers
            .get(&canon_path).cloned();
        trace!("should trigger: {:?}", maybe_triggers);
        if let Some(triggers) = maybe_triggers {
            for trigger in triggers {
                debug!("Wants to trigger {}", trigger.as_str());
                let ready = {
                    let mut analysis = self.analysis.lock().unwrap();
                    let has_dependencies = analysis.has_dependencies(&trigger)
                        && analysis.get_isolated_analysis(&trigger).unwrap()
                        .is_device_file();
                    // Skip triggering if the device cannot be outdated,
                    // i.e. it's newer than all it's dependencies
                    let not_outdated = analysis.device_newer_than_dependencies(
                        &trigger);
                    has_dependencies && !not_outdated
                };
                if ready {
                    debug!("Triggered device analysis {}", trigger.as_str());
                    self.device_analyze(&trigger, out);
                }
            }
        }
    }

    // Called when config might have changed, re-update include paths
    // and similar
    pub fn maybe_changed_config(&self,
                                old_config: Config,
                                out: &O) {
        trace!("Compilation info might have changed");
        enum LintReissueRequirement {
            None,
            ReReport,
            AnalyzeMissing,
            AnalyzeAll,
        }
        impl LintReissueRequirement {
            fn upgrade_to(self, to: LintReissueRequirement) ->
                LintReissueRequirement {
                    match (self, to) {
                        (Self::None, o) => o,
                        (o, Self::None) => o,
                        (Self::AnalyzeAll, _) => Self::AnalyzeAll,
                        (_, Self::AnalyzeAll) => Self::AnalyzeAll,
                        (Self::AnalyzeMissing, _) => Self::AnalyzeMissing,
                        (_, Self::AnalyzeMissing) => Self::AnalyzeMissing,
                        _ => Self::ReReport,
                    }
                }
        }
        let mut lint_reissue = LintReissueRequirement::None;
        {
            let config = self.config.lock().unwrap().clone();
            if config.compile_info_path != old_config.compile_info_path {
                self.update_compilation_info(out);
            }
            if config.linting_enabled != old_config.linting_enabled {
                if config.linting_enabled {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::AnalyzeMissing);
                } else {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::ReReport);
                }
            }
            if config.suppress_imports != old_config.suppress_imports {
                if config.suppress_imports {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::ReReport);
                } else {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::AnalyzeMissing);
                }
            }
            if config.lint_direct_only != old_config.lint_direct_only {
                if config.lint_direct_only {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::ReReport);
                } else {
                    lint_reissue = lint_reissue.upgrade_to(
                        LintReissueRequirement::AnalyzeMissing);
                }
            }
            if config.lint_cfg_path != old_config.lint_cfg_path {
                self.update_linter_config(out);
                lint_reissue = LintReissueRequirement::AnalyzeAll;
            }
        }
        match lint_reissue {
            LintReissueRequirement::None => (),
            LintReissueRequirement::ReReport =>
                self.report_errors(out),
            LintReissueRequirement::AnalyzeMissing => {
                // Because lint_analyze re-locks self.analysis, we need
                // to copy out the paths before iterating here
                let paths: Vec<_> = {
                    let storage = self.analysis.lock().unwrap();
                    storage.isolated_analysis.keys().filter(
                        |p|!storage.has_lint_analysis(p)).cloned().collect()
                };
                for path in paths {
                    self.maybe_trigger_lint_analysis(path.as_path(), out);
                }
                self.report_errors(out);
            },
            LintReissueRequirement::AnalyzeAll => {
                let paths: Vec<_> = {
                    self.analysis.lock().unwrap()
                        .isolated_analysis.keys().cloned().collect()
                };
                for path in paths {
                    self.maybe_trigger_lint_analysis(path.as_path(), out);
                }
                self.report_errors(out);
            },
        }
        self.update_file_watchers(out);
    }


    const WATCH_ID: &str = "dls-watch";
    pub fn update_file_watchers(&self, out: &O) {
        if self.active_watch.lock().unwrap().take().is_some() {
            self.send_request::<UnregisterCapability>(
                UnregistrationParams {
                    unregisterations: vec![Unregistration {
                        id: Self::WATCH_ID.to_string(),
                        method: <DidChangeWatchedFiles
                            as LSPNotification>::METHOD
                            .to_string(),
                    }]
                },
                out);
        } else {
            self.register_new_watchers(out);
        }
    }

    pub fn register_new_watchers(&self, out: &O) {
        let mut watchers = self.active_watch.lock().unwrap();
        if let Some(previous) = watchers.take() {
            error!("Wanted to register new watchers, but the previous ones \
                    were not cleared. (were: {:?})", previous);
        }
        *watchers = FileWatch::new(self);
        if let Some(watchers_spec) = watchers.as_ref() {
            let reg_params = RegistrationParams {
                registrations: vec![Registration {
                    id: Self::WATCH_ID.to_string(),
                    method: <DidChangeWatchedFiles as LSPNotification>
                        ::METHOD.to_string(),
                    register_options: Some(watchers_spec.watchers_config()),
                }],
            };
            self.send_request::<RegisterCapability>(reg_params, out);
        } else {
            error!("Failed to register file watchers with config: {:?}",
                   self.config.lock().unwrap());
        }
    }

    // Call before adding new analysis
    pub fn maybe_start_progress(&self, out: &O) {

        let mut notifier = self.current_notifier.lock().unwrap();

        if notifier.is_none() {
            debug!("started progress status");
            let new_notifier = AnalysisProgressNotifier::new(
                "Analysing".to_string(), out.clone());
            *notifier = Some(new_notifier.id());
            new_notifier.notify_begin_progress();
        }
    }
    pub fn maybe_end_progress(&mut self, out: &O) {
        if !self.analysis_queue.has_work() {
            // Need the scope here to succesfully drop the guard lock before
            // going into maybe_warn_missing_builtins below
            let lock_id = { self.current_notifier.lock().unwrap().clone() };
            if let Some(id) = lock_id {
                debug!("ended progress status");
                let notifier = AnalysisProgressNotifier::new_with_id(
                    id,
                    "Analysing".to_string(),
                    out.clone());
                notifier.notify_end_progress();
                self.maybe_warn_missing_builtins(out);
                *self.current_notifier.lock().unwrap() = None;
            }
        }
    }

    // NOTE: Do not call this method from outside the main server loop,
    // as notification/request handlers obtain _copies_ of the context,
    // and not the context itself
    fn maybe_warn_missing_builtins(&mut self, out: &O) {
        if !self.has_notified_missing_builtins &&
            !self.analysis.lock().unwrap().has_client_file(
                &PathBuf::from("dml-builtins.dml")) {
                self.has_notified_missing_builtins = true;
                crate::server::warning_message(
                    out,
                    "Unable to find dml-builtins, various essential \
                     built-in templates, methods, and paramters will \
                     be unavailable and semantic analysis is likely \
                     to produce errors as a result".to_string());
            }
    }

    pub fn construct_resolver(&self) -> PathResolver {
        trace!("About to construct resolver");
        let mut toret: PathResolver =
               self.client_capabilities.root.clone().into();
        toret.add_paths(self.workspace_roots.lock().unwrap()
                        .iter().map(|w|parse_file_path!(&w.uri, "workspace")
                                    .unwrap()));
        toret.set_include_paths(&self.compilation_info.lock().unwrap().iter()
                                .map(|(r, info)|(r.clone(),
                                                 info.include_paths.clone()
                                                 .into_iter().collect()))
                                .collect());
        trace!("Constructed resolver: {:?}", toret);
        toret
    }

    pub fn isolated_analyze(&self,
                            client_path: &Path,
                            context: Option<CanonPath>,
                            out: &O) {
        debug!("Wants isolated analysis of {:?}{}",
               client_path,
               context.as_ref().map(|s|format!(" under context {}", s.as_str()))
               .unwrap_or_default());
        let path = if let Some(p) =
            self.construct_resolver()
            .resolve_with_maybe_context(client_path, context.as_ref()) {
                p
            } else {
                debug!("Could not canonicalize client path {:?}", client_path);
                return;
            };
        if self.analysis.lock().unwrap().has_isolated_analysis(&path) {
            debug!("Was already analyzed");
            return;
        }
        self.maybe_start_progress(out);
        let (job, token) = ConcurrentJob::new();
        self.add_job(job);

        self.analysis_queue.enqueue_isolated_job(
            &mut self.analysis.lock().unwrap(),
            &self.vfs, context, path, client_path.to_path_buf(), token);
    }

    fn device_analyze(&self, device: &CanonPath, out: &O) {
        debug!("Wants device analysis of {:?}", device);
        self.maybe_start_progress(out);
        self.maybe_add_device_context(device);
        let (job, token) = ConcurrentJob::new();
        self.add_job(job);
        let locked_analysis = &mut self.analysis.lock().unwrap();
        let dependencies = locked_analysis.all_dependencies(device,
                                                            Some(device));
        self.analysis_queue.enqueue_device_job(
            locked_analysis,
            device,
            dependencies,
            token);
    }

    // (DeviceContextPath, IsActive, IsReady)
    pub fn get_all_context_info(&self)
                                -> HashSet<(ContextDefinition, bool, bool)> {
        let mut analysis = self.analysis.lock().unwrap();
        analysis.update_analysis(&self.construct_resolver());
        let contexts = self.device_active_contexts.lock().unwrap();
        analysis.device_triggers.values().flatten()
            .map(|path|ContextDefinition::from(path.clone()))
            .map(|con|{
                let b = contexts.contains(&con);
                let r = analysis.get_device_analysis(
                    con.as_canon_path()).is_ok();
                (con, b, r)
            }).collect()
    }

    pub fn get_context_info(&self, path: &CanonPath)
                            -> HashSet<(ContextDefinition, bool, bool)> {
        let mut analysis = self.analysis.lock().unwrap();
        analysis.update_analysis(&self.construct_resolver());
        let contexts = self.device_active_contexts.lock().unwrap();
        analysis.device_triggers.get(path)
            .into_iter().flatten()
            .map(|path|ContextDefinition::from(path.clone()))
            .map(|con|{
                let b = contexts.contains(&con);
                let r = analysis.get_device_analysis(
                    con.as_canon_path()).is_ok();
                (con, b, r)
            }).collect()
    }

    pub fn maybe_add_device_context(&self, path: &CanonPath) {
        {
            let mut previous_checks =
                self.previously_checked_contexts.lock().unwrap();
            let context = ContextDefinition::Device(path.clone());
            if previous_checks.contains(&context) {
                return;
            } else {
                previous_checks.insert(context);
            }
        }
        if match self.config.lock().unwrap().new_device_context_mode {
            DeviceContextMode::Always => true,
            DeviceContextMode::AnyNew => {
                let mut any_uncontexted = false;
                let analysis = self.analysis.lock().unwrap();
                for dependency in analysis
                    .device_dependencies
                    .get(path)
                    .iter().flat_map(|h|h.iter())
                {
                        let mut had_context = false;
                    for device in analysis
                        .device_triggers
                        .get(dependency)
                        .iter().flat_map(|h|h.iter())
                    {
                        if self.device_active_contexts.lock().unwrap()
                            .contains(&device.clone().into()) {
                                had_context = true;
                                break;
                            }
                    }
                    if !had_context {
                        any_uncontexted = true;
                        break;
                    }
                }
                any_uncontexted
            },
            DeviceContextMode::First => {
                let mut all_uncontexted = true;
                let analysis = self.analysis.lock().unwrap();
                for dependency in analysis
                    .device_dependencies
                    .get(path)
                    .iter().flat_map(|h|h.iter())
                // NOTE: do not include files that seem like core
                // files in this. TODO: For now, just do implicit imports but
                // things like 'utility.dml' should probably be included too
                    .filter(|dep|
                            !IMPLICIT_IMPORTS.iter().any(
                                |ii|dep.file_name()
                                    .and_then(|f|f.to_str()).unwrap() == *ii))
                {
                    let mut had_context = false;
                    for device in analysis
                        .device_triggers.get(dependency)
                        .iter().flat_map(|h|h.iter())
                    {
                        if self.device_active_contexts.lock().unwrap()
                            .contains(&device.clone().into()) {
                                had_context = true;
                                break;
                            }
                    }
                    if had_context {
                        all_uncontexted = false;
                        break;
                    }
                }
                all_uncontexted
            },
            DeviceContextMode::SameModule => {
                let mut matched_dir = false;
                let analysis = self.analysis.lock().unwrap();
                for device in analysis.device_dependencies.keys() {
                    if path.starts_with(device.parent().unwrap()) ||
                        device.starts_with(path.parent().unwrap()) {
                            matched_dir = true;
                            break;
                        }
                }
                matched_dir
            },
            DeviceContextMode::Never => false,
        } {
            debug!("Automatically activated device context at {}",
                   path.to_str().unwrap());
            self.device_active_contexts.lock().unwrap().insert(
                ContextDefinition::Device(path.clone()));
        }
    }

    // Updates active contexts in such a way that the only active contexts
    // for path are the provided ones, effectively disabling all contexts
    // that are available for the specified path but not
    // provided
    pub fn update_contexts(&self,
                           path: &CanonPath,
                           new_active_contexts: HashSet<ContextDefinition>) {
        let all_device_contexts: Vec<CanonPath> =
            self.analysis.lock().unwrap()
            .device_triggers
            .get(path)
            .into_iter()
            .flatten()
            .cloned()
            .collect();
        let mut current_contexts = self.device_active_contexts.lock().unwrap();
        current_contexts.retain(
            |context|match context {
                c @ ContextDefinition::Device(ref p) =>
                    !all_device_contexts.contains(p)
                    || new_active_contexts.contains(c),
                // TODO: handle synthetic contexts
                ContextDefinition::Simulated(_) => true,
            });
        for context in new_active_contexts {
            match context {
                ref c @ ContextDefinition::Device(ref p) =>
                    if !all_device_contexts.contains(p) {
                        error!("Tried to activate context {:?} for {:?},\
                                but it is not a known device context for that \
                                path", c, path);
                    } else {
                        current_contexts.insert(c.clone());
                    },
                // TODO: handle synthetic contexts
                ContextDefinition::Simulated(_) => (),
            }
        }
    }

    pub fn maybe_trigger_lint_analysis(&self, file: &Path, out: &O) {
        if !self.config.lock().unwrap().linting_enabled {
            return;
        }
        let config = self.config.lock().unwrap().to_owned();
        if config.suppress_imports {
            let canon_path: CanonPath = file.to_path_buf().into();
            if !self.direct_opens.lock().unwrap().contains(&canon_path) {
                return;
            }
        }
        let lint_config = self.lint_config.lock().unwrap().to_owned();
        debug!("Triggering linting analysis of {:?}", file);
        self.lint_analyze(file,
                          None,
                          lint_config,
                          out);
    }

    fn lint_analyze(&self,
                    file: &Path,
                    context: Option<CanonPath>,
                    cfg: LintCfg,
                    out: &O) {
        debug!("Wants to lint {:?}", file);
        self.maybe_start_progress(out);
        let path = if let Some(p) =
        self.construct_resolver()
        .resolve_with_maybe_context(file, context.as_ref()) {
            p
        } else {
            debug!("Could not canonicalize client path {:?}", file);
            return;
        };
        let (job, token) = ConcurrentJob::new();
        self.add_job(job);

        self.analysis_queue.enqueue_linter_job(
            &mut self.analysis.lock().unwrap(),
            cfg,
            &self.vfs, path, token);
    }

    pub fn add_job(&self, job: ConcurrentJob) {
        self.jobs.lock().unwrap().add(job);
    }

    pub fn wait_for_concurrent_jobs(&self) {
        self.jobs.lock().unwrap().wait_for_all();
    }

    /// See docs on VersionOrdering
    fn check_change_version(&self, file_path: &Path,
                            version_num: i32) -> VersionOrdering {
        let file_path = file_path.to_owned();
        let mut prev_changes = self.prev_changes.lock().unwrap();

        if prev_changes.contains_key(&file_path) {
            let prev_version = prev_changes[&file_path];
            if version_num <= prev_version {
                debug!(
                    "Out of order or duplicate change {:?}, prev: {}, current: {}",
                    file_path, prev_version, version_num,
                );

                if version_num == prev_version {
                    return VersionOrdering::Duplicate;
                } else {
                    return VersionOrdering::OutOfOrder;
                }
            }
        }

        prev_changes.insert(file_path, version_num);
        VersionOrdering::Ok
    }

    fn reset_change_version(&self, file_path: &Path) {
        let file_path = file_path.to_owned();
        let mut prev_changes = self.prev_changes.lock().unwrap();
        prev_changes.remove(&file_path);
    }

    fn text_doc_pos_to_pos(&self,
                           params: &TextDocumentPositionParams,
                           context: &str)
                           -> Option<FilePosition<ZeroIndexed>> {
        let file_path = parse_file_path!(
            &params.text_document.uri, context)
            .ok()?;
        // run this through pos_to_span once to get the word range, then return
        // the front of it
        Some(self.convert_pos_to_span(file_path, params.position)
             .start_position())
    }

    fn convert_pos_to_span(&self, file_path: PathBuf, pos: Position) -> Span {
        trace!("convert_pos_to_span: {:?} {:?}", file_path, pos);

        let pos = ls_util::position_to_dls(pos);
        let line = self.vfs.load_line(&file_path, pos.row).unwrap();
        trace!("line: `{}`", line);

        let (start, end) = find_word_at_pos(&line, pos.col);
        trace!("start: {}, end: {}", start.0, end.0);

        Span::from_positions(
            span::Position::new(pos.row, start),
            span::Position::new(pos.row, end),
            file_path,
        )
    }

    fn add_state_wait(&self, def: AnalysisStateWaitDefinition) {
        self.active_waits.lock().unwrap().push(def);
    }

    pub fn retain_live_waits(&self) {
        self.active_waits.lock().unwrap().
            retain(|w|w.response.send(AnalysisStateResponse::Ping).is_ok());
    }

    pub fn wait_for_state(&self,
                          progress_kind: AnalysisProgressKind,
                          wait_kind: AnalysisWaitKind,
                          coverage: AnalysisCoverageSpec) ->
        Result<AnalysisStateResponse, crossbeam::channel::RecvError> {
            let (sender, receiver) = channel::unbounded();
            let wait = AnalysisStateWaitDefinition {
                progress_kind,
                coverage,
                wait_kind,
                response: sender,
            };
            if self.check_wait(&wait) {
                debug!("Wait {:?} was immediately completed", wait);
                Ok(self.check_wait_satisfied(&wait))
            } else {
                debug!("Wait {:?} needs to wait", wait);
                self.add_state_wait(wait);
                loop {
                    match receiver.recv() {
                        Ok(AnalysisStateResponse::Ping) => (),
                        r => return r,
                    }
                }
            }
        }

    fn check_wait(&self, wait: &AnalysisStateWaitDefinition) -> bool {
        debug!("Checking wait {:?}", wait);
        let mut wait_done = true;
        let analysis = self.analysis.lock().unwrap();
        let queue = &self.analysis_queue;
        if let AnalysisCoverageSpec::Paths(path) = &wait.coverage {
            let mut isolated_paths: HashSet<CanonPath>
                = path.iter().cloned().collect();
            let mut device_paths: HashSet<CanonPath>
                = HashSet::default();
            match wait.progress_kind {
                AnalysisProgressKind::Device => {
                    device_paths.extend(isolated_paths.clone());
                    let extra_paths: Vec<CanonPath> =
                        isolated_paths.iter().flat_map(
                            |p|analysis.all_dependencies(p, Some(p)))
                        .collect();
                    isolated_paths.extend(extra_paths);
                },
                AnalysisProgressKind::DeviceDependencies => {
                    device_paths.extend(
                        isolated_paths.iter().flat_map(
                            |p|analysis.device_triggers
                                .get(p).into_iter().flatten())
                            .cloned());
                    if wait.wait_kind == AnalysisWaitKind::Work {
                        wait_done = !queue.has_isolated_work();
                    } else {
                        isolated_paths.extend(device_paths.iter().flat_map(
                            |p|analysis.all_dependencies(p, Some(p))));
                    }
                },
                _ => (),
            }
            if wait.wait_kind == AnalysisWaitKind::Work {
                wait_done = wait_done &&
                    !queue.working_on_isolated_for_paths(&isolated_paths);
                wait_done = wait_done &&
                    !queue.working_on_device_for_paths(&device_paths);
            } else {
                wait_done = wait_done && !isolated_paths.iter().any(
                    |p|analysis.get_isolated_analysis(p).is_err());
                wait_done = wait_done && !device_paths.iter().any(
                    |p|analysis.get_device_analysis(p).is_err());
            }
        } else if wait.wait_kind == AnalysisWaitKind::Work {
            wait_done = !queue.has_isolated_work();
            if wait.progress_kind != AnalysisProgressKind::Isolated {
                wait_done = wait_done && !queue.has_device_work();
            }
        } else {
            // It's a little bit weird to wait for 'any' existence, but we
            // interpret it as just the existence of anything
            wait_done = !analysis.isolated_analysis.is_empty();
            if wait.progress_kind != AnalysisProgressKind::Isolated {
                wait_done = wait_done
                    && !analysis.device_analysis.is_empty();
            }
        }

        if wait_done {
            debug!("{:?} was done", wait);
        } else {
            debug!("{:?} not done", wait);
        }
        wait_done
    }

    // TODO: This is equivalent between existence and work kinds, I think
    fn check_wait_satisfied(&self, wait: &AnalysisStateWaitDefinition)
                            -> AnalysisStateResponse {
        let mut achieved = true;
        let analysis = self.analysis.lock().unwrap();

        if let AnalysisCoverageSpec::Paths(paths) = &wait.coverage {
            for path in paths {
                if analysis.get_isolated_analysis(path).is_err() {
                    achieved = false;
                    break;
                }
                match wait.progress_kind {
                    AnalysisProgressKind::Device =>
                        if analysis.get_device_analysis(path).is_err() {
                            achieved = false;
                            break;
                        },
                    AnalysisProgressKind::DeviceDependencies =>
                        if !analysis.device_triggers.get(path)
                        .into_iter().flatten().any(
                            |t|analysis.get_device_analysis(t).is_ok()) {
                            achieved = false;
                            break;
                        },
                    _ => (),
                }
            }
        } else {
            // NOTE: A little difficult to reason what this means, but we will
            // call it satisfied as long as at least one analysis exists
            if wait.progress_kind == AnalysisProgressKind::Isolated {
                achieved = !analysis.isolated_analysis.is_empty();
            } else {
                achieved = !analysis.device_analysis.is_empty();
            }
        }
        if achieved {
            AnalysisStateResponse::Achieved
        } else {
            AnalysisStateResponse::Satisfied
        }
    }


    // NOTE: we could potentially optimize this by checking only
    // the waits on an updated path, but the expectation is that
    // the number of waits is small
    pub fn check_state_waits(&self) {
        self.active_waits.lock().unwrap().retain(
            |wait|
            if self.check_wait(wait) {
                // Dont care about error here
                wait.response.send(self.check_wait_satisfied(wait)).ok();
                false
            } else {
                true
            });
    }

    pub fn send_request<R>(&self, params: R::Params, out: &O)
    where
        R: SentRequest,
        <R as LSPRequest>::Params: std::fmt::Debug
    {
        let id = out.provide_id();
        info!("Sending a request {} with params {:?} and id {}",
               <R as LSPRequest>::METHOD, params, id);
        let request = Request::<R>::new(id.clone(), params);
        self.outstanding_requests.lock().unwrap().insert(
            id, Box::new(<R as SentRequest>::handle_response));
        out.request(request);
    }

    pub fn handle_request_response(&self, resp: RawResponse, out: &O) {
        info!("Got a response to some request: {:?}", resp);
        let maybe_req_fn = { self.outstanding_requests
                             .lock().unwrap().remove(&resp.id) };
        if let Some(request_fn) = maybe_req_fn {
            request_fn(self, resp, out);
        } else {
            info!("Got response for request id {:?} but it was not tracked\
                   (either timed out or was never sent)",
                  resp.id);
        }
    }
}

/// Some notifications come with sequence numbers, we check that these are in
/// order. However, clients might be buggy about sequence numbers so we do cope
/// with them being wrong.
///
/// This enum defines the state of sequence numbers.
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum VersionOrdering {
    /// Sequence number is in order (note that we don't currently check that
    /// sequence numbers are sequential, but we probably should).
    Ok,
    /// This indicates the client sent us multiple copies of the same notification
    /// and some should be ignored.
    Duplicate,
    /// Just plain wrong sequence number. No obvious way for us to recover.
    OutOfOrder,
}

/// Represents a text cursor between characters, pointing at the next character
/// in the buffer.
type Column = span::Column<span::ZeroIndexed>;

/// Returns a text cursor range for a found word inside `line` at which `pos`
/// text cursor points to. Resulting type represents a (`start`, `end`) range
/// between `start` and `end` cursors.
/// For example (4, 4) means an empty selection starting after first 4 characters.
fn find_word_at_pos(line: &str, pos: Column) -> (Column, Column) {
    let col = pos.0 as usize;
    let is_ident_char = |c: char| c.is_alphanumeric() || c == '_';

    let start = line
        .chars()
        .enumerate()
        .take(col)
        .filter(|&(_, c)| !is_ident_char(c))
        .last()
        .map(|(i, _)| i + 1)
        .unwrap_or(0) as u32;

    #[allow(clippy::filter_next)]
    let end = line
        .chars()
        .enumerate()
        .skip(col)
        .filter(|&(_, c)| !is_ident_char(c))
        .next()
        .map(|(i, _)| i)
        .unwrap_or(col) as u32;

    (span::Column::new_zero_indexed(start), span::Column::new_zero_indexed(end))
}

// /// Client file-watching request / filtering logic
#[derive(Debug, Clone)]
pub struct FileWatch {
    file_paths: Vec<PathBuf>,
}

impl FileWatch {
    /// Construct a new `FileWatch`.
    pub fn new<O: Output>(ctx: &InitActionContext<O>) -> Option<Self> {
        let mut file_paths = vec![];
        match ctx.config.lock() {
            Ok(config) => {
                if let Some(compile_info) = config.compile_info_path.as_ref() {
                    file_paths.push(compile_info.clone());
                }
                if let Some(lint_cfg_path) = config.lint_cfg_path.as_ref() {
                    file_paths.push(lint_cfg_path.clone());
                }
                Some(FileWatch {
                    file_paths,
                })
            },
            Err(e) => {
                error!("Unable to access configuration: {:?}", e);
                None
            }
        }
    }

    /// Returns if a file change is relevant to the files we
    /// actually wanted to watch
    /// Implementation note: This is expected to be called a
    /// large number of times in a loop so should be fast / avoid allocation.
    #[inline]
    fn relevant_change_kind(&self, change_uri: &Uri,
                            _kind: FileChangeType) -> bool {
        let path = change_uri.as_str();
        self.file_paths.iter()
            .filter_map(|p|p.to_str())
            .any(|our_path|our_path == path)
    }

    #[inline]
    pub fn is_relevant(&self, change: &FileEvent) -> bool {
        self.relevant_change_kind(&change.uri, change.typ)
    }

    #[inline]
    pub fn is_relevant_save_doc(&self, did_save: &DidSaveTextDocumentParams)
                                -> bool {
        self.relevant_change_kind(&did_save.text_document.uri,
                                  FileChangeType::CHANGED)
    }

    /// Returns json config for desired file watches
    pub fn watchers_config(&self) -> serde_json::Value {
        fn watcher(pat: String) -> FileSystemWatcher {
            FileSystemWatcher { glob_pattern: GlobPattern::String(pat),
                                kind: None }
        }
        fn _watcher_with_kind(pat: String, kind: WatchKind)
                             -> FileSystemWatcher {
            FileSystemWatcher { glob_pattern: GlobPattern::String(pat),
                                kind: Some(kind) }
        }

        let watchers: Vec<_> = self.file_paths.iter()
            .map(|p|p.to_string_lossy().to_string())
            .map(watcher)
            .collect();
        let watchers = DidChangeWatchedFilesRegistrationOptions {
            watchers,
        };
        json!(watchers)
    }
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Implementation of the server loop, and traits for extending server
//! interactions (for example, to add support for handling new types of
//! requests).

use crate::actions::{notifications, requests, ActionContext};
use crate::analysis::IMPLICIT_IMPORTS;
use crate::config::{Config, DeviceContextMode, DEPRECATED_OPTIONS};
use crate::file_management::CanonPath;
use crate::lsp_data;
use crate::lsp_data::{
    InitializationOptions, LSPNotification, LSPRequest, MessageType,
    ShowMessageParams, Workspace,
};
use crate::server::dispatch::Dispatcher;
pub use crate::server::dispatch::{RequestAction, SentRequest,
                                  DEFAULT_REQUEST_TIMEOUT};
pub use crate::server::io::{MessageReader, Output};
use crate::server::io::{StdioMsgReader, StdioOutput};
use crate::server::message::{RawMessage, RawMessageOrResponse, RawResponse};
pub use crate::server::message::{
    Ack, BlockingNotificationAction, BlockingRequestAction,
    NoResponse, Notification, Request,
    RequestId, Response, ResponseError, ResponseWithMessage,
};
use crate::version;
use jsonrpc::error::StandardError;
use log::{debug, error, info, trace, warn};
pub use lsp_types::notification::{Exit as ExitNotification, ShowMessage};
pub use lsp_types::request::Initialize as InitializeRequest;
pub use lsp_types::request::Shutdown as ShutdownRequest;
use lsp_types::{
    DeclarationCapability,
    HoverProviderCapability,
    ImplementationProviderCapability,
    InitializeResult, OneOf, ServerCapabilities,
    ServerInfo,
    TextDocumentSyncCapability,
    TextDocumentSyncOptions,
    TextDocumentSyncKind,
    TextDocumentSyncSaveOptions,
    WorkspaceServerCapabilities,
    WorkspaceFoldersServerCapabilities,
};
use crossbeam::channel;
use serde::Serialize;
use serde_json::Value;

use std::path::{Path, PathBuf};
use std::time::Duration;
use std::thread;

use crate::vfs::Vfs;

use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

pub mod dispatch;
pub mod io;
pub mod message;

/// Implementation-defined server-error, according to chapter 5.1:
/// https://www.jsonrpc.org/specification
const NOT_INITIALIZED_CODE: i32 = -32002;

/// Runs the DML Language Server.
pub fn run_server(vfs: Arc<Vfs>) -> i32 {
    debug!("Language Server starting up. Version: {}", version());
    let config = Arc::new(Mutex::new(Config::default()));
    debug!("made config");
    let msgreader = Box::new(StdioMsgReader);
    debug!("made msgreader");
    let service = LsService::new(
        vfs,
        config,
        msgreader,
        StdioOutput::new(),
    );
    debug!("made service");
    let exit_code = LsService::run(service);
    debug!("Server shutting down");
    exit_code
}

impl BlockingRequestAction for ShutdownRequest {
    type Response = Ack;

    fn handle<O: Output>(
        _id: RequestId,
        _params: Self::Params,
         ctx: &mut ActionContext<O>,
        _out: O,
    ) -> Result<Self::Response, ResponseError> {
        if let Ok(ctx) = ctx.inited() {
            // wait for pending jobs before ack-ing
            ctx.wait_for_concurrent_jobs();
            ctx.shut_down.store(true, Ordering::SeqCst);
            Ok(Ack)
        } else {
            Err(ResponseError::Message(
                Value::from(NOT_INITIALIZED_CODE),
                "not yet received `initialize` request".to_owned(),
            ))
        }
    }
}

pub(crate) fn maybe_notify_unknown_configs<O: Output>(out: &O, unknowns: &[String]) {
    use std::fmt::Write;
    if unknowns.is_empty() {
        return;
    }
    let mut msg = "Unknown DLS configuration:".to_string();
    let mut first = true;
    for key in unknowns {
        write!(msg, "{}`{}` ", if first { ' ' } else { ',' }, key).unwrap();
        first = false;
    }
    out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
        typ: MessageType::WARNING,
        message: msg,
    }));
}

#[allow(dead_code)]
pub(crate) fn info_message<O: Output>(out: &O, message: String) {
    out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
        typ: MessageType::INFO,
        message,
    }));
}

pub(crate) fn warning_message<O: Output>(out: &O, message: String) {
    out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
        typ: MessageType::WARNING,
        message,
    }));
}

pub(crate) fn error_message<O: Output>(out: &O, message: String) {
    out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
        typ: MessageType::ERROR,
        message,
    }));
}

/// For each deprecated configuration key an appropriate warning is emitted via
/// LSP, along with a deprecation notice (if there is one).
pub(crate) fn maybe_notify_deprecated_configs<O: Output>(out: &O, keys: &[String]) {
    for key in keys {
        let notice = DEPRECATED_OPTIONS.get(key.as_str()).and_then(|x| *x);
        let message = format!(
            "DLS configuration option `{}` is deprecated{}",
            key,
            notice.map(|notice| format!(": {}", notice)).unwrap_or_default()
        );

        out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
            typ: MessageType::WARNING,
            message,
        }));
    }
}

pub(crate) fn maybe_notify_duplicated_configs<O: Output>(
    out: &O,
    dups: &std::collections::HashMap<String, Vec<String>>,
) {
    use std::fmt::Write;
    if dups.is_empty() {
        return;
    }
    let mut msg = String::new();
    for kv in dups {
        write!(msg, "{}:", kv.0).unwrap();
        let mut first = true;
        for v in kv.1 {
            write!(msg, "{}{}, ", if first { ' ' } else { ',' }, v).unwrap();
            first = false;
        }
        msg += "; ";
    }
    out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
        typ: MessageType::WARNING,
        message: format!("Duplicated DLS configuration: {}", msg),
    }));
}

impl BlockingRequestAction for InitializeRequest {
    type Response = NoResponse;

    fn handle<O: Output>(
        id: RequestId,
        mut params: Self::Params,
        ctx: &mut ActionContext<O>,
        out: O,
    ) -> Result<NoResponse, ResponseError> {
        let mut dups = std::collections::HashMap::new();
        let mut unknowns = Vec::new();
        let mut deprecated = Vec::new();
        let init_options = params
            .initialization_options
            .take()
            .and_then(|opt| {
                InitializationOptions::try_deserialize(
                    opt,
                    &mut dups,
                    &mut unknowns,
                    &mut deprecated,
                )
                .ok()
            })
            .unwrap_or_default();

        debug!("init: {:?} -> {:?}", params.initialization_options, init_options);

        if ctx.inited().is_ok() {
            return Err(ResponseError::Message(
                // No code in the spec; just use some number.
                Value::from(123),
                "Already received an initialize request".to_owned(),
            ));
        }

        maybe_notify_unknown_configs(&out, &unknowns);
        maybe_notify_deprecated_configs(&out, &deprecated);
        maybe_notify_duplicated_configs(&out, &dups);

        let result = InitializeResult {
            server_info: Some(ServerInfo {
                name: "DML Language Server".to_string(),
                version: Some(crate::version()),
            }),
            capabilities: server_caps(ctx),
        };

        // Send response early before `ctx.init` to enforce
        // initialize-response-before-all-other-messages constraint.
        result.send(id, &out);

        let capabilities = lsp_data::ClientCapabilities::new(&params);
        ctx.init(init_options, capabilities, out.clone()).unwrap();
        let mut workspaces = vec![];
        // TODO/NOTE: Do we want to disallow root definition?
        #[allow(deprecated)]
        if let Some(folders) = params.workspace_folders {
            workspaces = folders;
        } else if let Some(uri) = params.root_uri {
            workspaces.push(Workspace {
                uri,
                name: "root".to_string(),
            });
        }
        if let ActionContext::Init(ref mut initctx) = ctx {
            initctx.update_workspaces(workspaces, vec![]);
            let temp_resolver = initctx.construct_resolver();
            for file in IMPLICIT_IMPORTS {
                debug!("Requesting analysis of builtin file {}", file);
                if let Some(path) = temp_resolver
                    .resolve_under_any_context(Path::new(file)) {
                        let pathb: PathBuf = path.into();
                        initctx.isolated_analyze(pathb.as_path(), None, &out);
                    }
            }
        } else {
            unreachable!("Context failed to init");
        }

        Ok(NoResponse)
    }
}

/// A service implementing a language server.
pub struct LsService<O: Output> {
    msg_reader: Arc<Box<dyn MessageReader + Send + Sync>>,
    output: O,
    server_send: channel::Sender<ServerToHandle>,
    server_receive: channel::Receiver<ServerToHandle>,
    ctx: ActionContext<O>,
    dispatcher: Dispatcher<O>,
}

impl<O: Output> LsService<O> {
    /// Constructs a new language server service.
    pub fn new(
        vfs: Arc<Vfs>,
        config: Arc<Mutex<Config>>,
        reader: Box<dyn MessageReader + Send + Sync>,
        output: O,
    ) -> LsService<O> {
        let dispatcher = Dispatcher::new(output.clone());
        let (server_send, server_receive) = channel::unbounded();
        let ctx = ActionContext::new(vfs, config, server_send.clone());
        debug!("ctx made");
        LsService {
            msg_reader: Arc::new(reader),
            output,
            server_send, server_receive,
            ctx,
            dispatcher,
        }
    }

    /// Runs this language service.
    pub fn run(mut self) -> i32 {
        let client_reader_send = self.server_send.clone();
        let client_reader_output = self.output.clone();
        let client_reader_msg_reader = Arc::clone(&self.msg_reader);
        // Start client reader thread
        thread::spawn(move || {
            let msg_reader = client_reader_msg_reader;
            let output = client_reader_output;
            let send = client_reader_send;
            loop {
                debug!("Awaiting message");
                let msg_string = match msg_reader.read_message() {
                    Some(m) => m,
                    None => {
                        error!("Can't read message");
                        output.custom_failure(
                            RequestId::Null,
                            StandardError::ParseError,
                            Some("Cannot read message"));
                        send.send(ServerToHandle::ExitCode(101)).ok();
                        continue;
                    }
                };
                trace!("Read a message `{}`", msg_string);
                match RawMessageOrResponse::try_parse(&msg_string) {
                    Ok(RawMessageOrResponse::Message(rm)) => {
                        debug!("Parsed a message: {}", rm.method);
                        send.send(ServerToHandle::ClientMessage(rm)).ok();
                    },
                    Ok(RawMessageOrResponse::Response(rr)) => {
                        debug!("Parsed a response: {}", rr.id);
                        send.send(ServerToHandle::ClientResponse(rr)).ok();
                    },
                    Err(e) => {
                        error!("parsing error, {:?}", e);
                        output.custom_failure(
                            RequestId::Null,
                            StandardError::ParseError,
                            Some(e));
                        send.send(ServerToHandle::ExitCode(101)).ok();
                        continue;
                    }
                };
            }
        });

        info!("Language server entered active loop");
        loop {
            if self.server_receive.is_empty() {
                if let ActionContext::Init(ctx) = &mut self.ctx {
                    ctx.maybe_end_progress(&self.output);
                    if let Some(max_retain) = ctx.config.lock()
                        .unwrap().analysis_retain_duration {
                            ctx.analysis.lock().unwrap()
                                .discard_overly_old_analysis(
                                    Duration::new(max_retain as u64, 0));
                        }
                }
            }
            match self.server_receive.recv().unwrap() {
                ServerToHandle::ClientMessage(mess) =>
                    match self.handle_message(mess) {
                        ServerStateChange::Continue => (),
                        ServerStateChange::Break { exit_code }
                        => return exit_code,
                    },
                ServerToHandle::ClientResponse(resp) => {
                    if let ActionContext::Init(ctx) = &mut self.ctx {
                        ctx.handle_request_response(resp, &self.output);
                    }
                },
                ServerToHandle::ExitCode(code) => return code,
                ServerToHandle::IsolatedAnalysisDone(path, context,
                                                     requests) => {
                    debug!("Received isolated analysis of {:?}", path);
                    if let ActionContext::Init(ctx) = &mut self.ctx {
                        // hack where we try to activate a device context
                        // as early as we possibly can, unless device context
                        // mode _requires_ that we wait
                        {
                            if ctx.analysis.lock().unwrap()
                                .get_isolated_analysis(&path)
                                .map_or(false, |a|a.is_device_file()) {
                                    // We cannot be sure that all the imported are
                                    // uncovered by contexts until we know
                                    // what all the imported files are, which
                                    // requires more info than we might have here
                                    if ctx.config.lock().unwrap()
                                        .new_device_context_mode
                                        != DeviceContextMode::First {
                                            ctx.maybe_add_device_context(&path);
                                        }
                                }
                        }
                        let config = ctx.config.lock().unwrap().to_owned();
                        ctx.report_errors(&self.output);
                        for file in requests {
                            // A little bit of redundancy here, we need to
                            // pre-resolve this import into an absolute path
                            // or error reporting will complain later
                            if let Some(file) = ctx.construct_resolver()
                                .resolve_with_maybe_context(&file,
                                                            context.as_ref()) {
                                    if !config.suppress_imports {
                                        trace!("Analysing imported file {}",
                                            file.to_str().unwrap());
                                        ctx.isolated_analyze(&file,
                                                            context.clone(),
                                                            &self.output);
                                    }
                                } else {
                                    trace!("Imported file {:?} did not resolve",
                                           file);
                                }
                        }
                        if !ctx.config.lock().unwrap().suppress_imports {
                            ctx.trigger_device_analysis(&path, &self.output);
                        }
                        ctx.maybe_trigger_lint_analysis(&path, &self.output);
                        ctx.check_state_waits();
                    }
                },
                ServerToHandle::DeviceAnalysisDone(path) => {
                    debug!("Received device analysis of {:?}", path);
                    if let ActionContext::Init(ctx) = &mut self.ctx {
                        ctx.report_errors(&self.output);
                        ctx.check_state_waits();
                    }
                },
                ServerToHandle::LinterDone(path) => {
                    debug!("Received linter analysis of {:?}", path);
                    if let ActionContext::Init(ctx) = &mut self.ctx {
                        ctx.report_errors(&self.output);
                    }
                },
                ServerToHandle::AnalysisRequest(importpath, context) => {
                    if let ActionContext::Init(ctx) = &mut self.ctx {
                        if !ctx.config.lock().unwrap().to_owned().suppress_imports {
                            debug!("Analysing imported file {}",
                               &importpath.to_str().unwrap());
                            ctx.isolated_analyze(
                                &importpath, context, &self.output);
                        }
                    }
                }
            }
        }
    }

    fn dispatch_message(&mut self, msg: &RawMessage) -> Result<(), jsonrpc::Error> {
        macro_rules! match_action {
            (
                $method: expr;
                notifications: $($n_action: ty),*;
                blocking_requests: $($br_action: ty),*;
                requests: $($request: ty),*;
            ) => {
                debug!("Handling `{}`", $method);

                match $method.as_str() {
                $(
                    <$n_action as LSPNotification>::METHOD => {
                        let notification: Notification<$n_action> = msg.parse_as_notification()?;
                        if let Ok(mut ctx) = self.ctx.inited() {
                            debug!("Notified: {}", $method);
                            if notification.dispatch(&mut ctx, self.output.clone()).is_err() {
                                debug!("Error handling notification: {:?}", msg);
                            }
                        }
                        else {
                            warn!(
                                "Server has not yet received an `initialize` request, ignoring {}", $method,
                            );
                        }
                    }
                )*

                $(
                    <$br_action as LSPRequest>::METHOD => {
                        let request: Request<$br_action> = msg.parse_as_request()?;
                        debug!("Blockingly Requested: {:?}", $method);

                        // TODO: Re-examine this
                        // Block until all non-blocking requests have been handled ensuring
                        // ordering.
                        // self.wait_for_concurrent_jobs();

                        let req_id = request.id.clone();
                        match request.blocking_dispatch(&mut self.ctx, &self.output) {
                            Ok(res) => res.send(req_id, &self.output),
                            Err(ResponseError::Empty) => {
                                debug!("error handling {}", $method);
                                self.output.custom_failure(
                                    req_id,
                                    StandardError::InternalError,
                                    Some("Empty response"))
                            }
                            Err(ResponseError::Message(code, msg)) => {
                                debug!("error handling {}: {}", $method, msg);
                                self.output.failure_message(req_id, code, msg)
                            }
                        }
                    }
                )*

                $(
                    <$request as LSPRequest>::METHOD => {
                        let request: Request<$request> = msg.parse_as_request()?;
                        if let Ok(ctx) = self.ctx.inited() {
                            debug!("Unblockingly Requested: {}", $method);
                            self.dispatcher.dispatch(request, ctx);
                        }
                        else {
                            warn!(
                                "Server has not yet received an `initialize` request, cannot handle {}", $method,
                            );
                            self.output.failure_message(
                                request.id,
                                Value::from(NOT_INITIALIZED_CODE),
                                "not yet received `initialize` request".to_owned(),
                            );
                        }
                    }
                )*
                    _ => debug!("Method not found: {}", $method)
                }
            }
        }

        // Notifications and blocking requests are handled immediately on the
        // main thread. They will never be dropped.
        // Blocking requests wait for all non-blocking requests to complete,
        // notifications do not.
        // Other requests are read and then forwarded to a worker thread, they
        // might timeout and will return an error but should not be dropped.
        // Some requests might block again when executing due to waiting for a
        // build or access to the VFS or real file system.
        // Requests must not mutate DLS state, but may ask the client to mutate
        // the client state.
        match_action!(
            msg.method;
            notifications:
                notifications::Initialized,
                notifications::DidOpenTextDocument,
                notifications::DidCloseTextDocument,
                notifications::DidChangeTextDocument,
                notifications::DidSaveTextDocument,
                notifications::DidChangeConfiguration,
                notifications::DidChangeWatchedFiles,
                notifications::DidChangeWorkspaceFolders,
                notifications::Cancel,
                notifications::ChangeActiveContexts;
            blocking_requests:
                ShutdownRequest,
                InitializeRequest;
            requests:
                requests::ExecuteCommand,
                requests::Formatting,
                requests::RangeFormatting,
                requests::ResolveCompletion,
                requests::Rename,
                requests::CodeActionRequest,
                requests::DocumentHighlightRequest,
                requests::GotoImplementation,
                requests::WorkspaceSymbolRequest,
                requests::DocumentSymbolRequest,
                requests::HoverRequest,
                requests::GotoDefinition,
                requests::GotoDeclaration,
                requests::References,
                requests::Completion,
                requests::CodeLensRequest,
                requests::GetKnownContextsRequest;
        );
        Ok(())
    }

    /// Handle a raw message with
    /// the appropriate action. Returns a `ServerStateChange` that describes how
    /// the service should proceed now that the message has been handled.
    pub fn handle_message(&mut self, mess: RawMessage) -> ServerStateChange {

        // If we're in shutdown mode, ignore any messages other than 'exit'.
        // This is not actually in the spec; I'm not sure we should do this,
        // but it kinda makes sense.
        {
            let shutdown_mode = match self.ctx {
                ActionContext::Init(ref ctx) => ctx.shut_down.load(
                    Ordering::SeqCst),
                _ => false,
            };
            if mess.method == <ExitNotification as LSPNotification>::METHOD {
                let exit_code = if shutdown_mode { 0 } else { 1 };
                return ServerStateChange::Break { exit_code };
            }
            if shutdown_mode {
                trace!("In shutdown mode, ignoring {:?}!", mess);
                return ServerStateChange::Continue;
            }
        }

        if let Err(e) = self.dispatch_message(&mess) {
            // TODO: Implement display for raw messages
            error!("dispatch error: {:?}, method: `{}`", e, mess.method);
            self.output.custom_failure(
                RequestId::from(mess.id),
                StandardError::InternalError,
                Some(e));
            return ServerStateChange::Break { exit_code: 101 };
        }

        ServerStateChange::Continue
    }

    pub fn wait_for_concurrent_jobs(&mut self) {
        match &self.ctx {
            ActionContext::Init(_) => {},
            ActionContext::Uninit(_) => {}
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ServerToHandle {
    ClientMessage(RawMessage),
    ClientResponse(RawResponse),
    ExitCode(i32),
    IsolatedAnalysisDone(CanonPath, Option<CanonPath>, Vec<PathBuf>),
    DeviceAnalysisDone(CanonPath),
    LinterDone(CanonPath),
    AnalysisRequest(PathBuf, Option<CanonPath>),
}

// Indicates how the server should proceed.
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum ServerStateChange {
    /// Continue serving responses to requests and sending notifications to the client.
    Continue,
    /// Stop the server.
    Break { exit_code: i32 },
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExperimentalFeatures {
    context_control: bool,
}

fn experimental_caps() -> Value {
    serde_json::to_value(ExperimentalFeatures {
        context_control: true
    }).unwrap()
}

fn server_caps<O: Output>(_ctx: &ActionContext<O>) -> ServerCapabilities {
    ServerCapabilities {
        call_hierarchy_provider: None,
        declaration_provider: Some(DeclarationCapability::Simple(true)),
        diagnostic_provider: None,
        document_link_provider: None,
        experimental: Some(experimental_caps()),
        inlay_hint_provider: None,
        inline_value_provider: None,
        linked_editing_range_provider: None,
        moniker_provider: None,
        position_encoding: None,
        semantic_tokens_provider: None,
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
            }
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: None,
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: None,
        implementation_provider: Some(
            ImplementationProviderCapability::Simple(true)),
        references_provider: None,
        document_highlight_provider: None,
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: None,
        document_formatting_provider: None,
        execute_command_provider: None,
        rename_provider: None,
        color_provider: None,
        document_range_formatting_provider: None,
        code_lens_provider: None,
        document_on_type_formatting_provider: None,
        signature_help_provider: None,
        folding_range_provider: None,
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true))
            }),
            file_operations: None,
        }),
        selection_range_provider: None,
        notebook_document_sync: None,
    }
}


#[cfg(test)]
mod test {
    use super::*;

    use std::path::PathBuf;
    use crate::lsp_data::{InitializeParams, parse_file_path};
    use lsp_types::Uri;
    use std::str::FromStr;

    #[allow(deprecated)]
    fn get_root_path(params: &InitializeParams) -> PathBuf {
        params
            .root_uri
            .as_ref()
            .map(|uri| {
                assert!(uri.scheme().map_or(false,|s| s.as_str() == "file"));
                parse_file_path(uri).expect("Could not convert URI to path")
            })
            .unwrap_or_else(|| {
                params.root_path.as_ref().map(PathBuf::from).expect("No root path or URI")
            })
    }

    fn get_default_params() -> InitializeParams {
        #[allow(deprecated)]
        InitializeParams {
            client_info: None,
            locale: None,
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: lsp_types::ClientCapabilities {
                general: None,
                workspace: None,
                window: None,
                text_document: None,
                experimental: None,
                notebook_document: None,
            },
            trace: Some(lsp_types::TraceValue::Off),
            workspace_folders: None,
            work_done_progress_params: lsp_types::WorkDoneProgressParams {
                work_done_token: None,
            },
        }
    }

    fn make_platform_path(path: &'static str) -> PathBuf {
        if cfg!(windows) {
            PathBuf::from(format!("C:/{}", path))
        } else {
            PathBuf::from(format!("/{}", path))
        }
    }

    fn make_uri(path: PathBuf) -> Uri {
        let extra_slash = if cfg!(windows) {
            "/"
        } else {
            ""
        };
        Uri::from_str(&format!(r"file://{}{}",
                               extra_slash,
                               path.display())).unwrap()
    }

    #[test]
    #[allow(deprecated)]
    fn test_use_root_uri() {
        let mut params = get_default_params();

        let root_path = make_platform_path("path/a");
        let root_uri = make_platform_path("path/b");
        params.root_path = Some(root_path.to_str().unwrap().to_owned());
        params.root_uri = Some(make_uri(root_uri.clone()));

        assert_eq!(get_root_path(&params), root_uri);
    }

    #[test]
    #[allow(deprecated)]
    fn test_use_root_path() {
        let mut params = get_default_params();

        let root_path = make_platform_path("path/a");
        params.root_path = Some(root_path.to_str().unwrap().to_owned());
        params.root_uri = None;

        assert_eq!(get_root_path(&params), root_path);
    }

    /// Some clients send empty object params for void params requests (see issue #1038).
    #[test]
    fn parse_shutdown_object_params() {
        let raw = RawMessageOrResponse::try_parse(
            r#"{"jsonrpc": "2.0", "id": 2, "method": "shutdown", "params": {}}"#,
        ).unwrap();
        let parsed = raw.as_message().unwrap();

        let _request: Request<ShutdownRequest> =
            parsed.parse_as_request().expect("Boring validation is happening");
    }
}

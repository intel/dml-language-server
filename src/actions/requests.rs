//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Requests that the DLS can respond to.

use jsonrpc::error::StandardError;
use log::{debug, error, info, trace, warn};
use serde::{Deserialize, Serialize};

use std::collections::HashSet;
use std::path::Path;

use crate::actions::hover;
use crate::actions::{AnalysisProgressKind, AnalysisWaitKind,
                     AnalysisCoverageSpec,
                     ContextDefinition, InitActionContext,
                     rpc_error_code};
use crate::actions::notifications::ContextDefinitionKindParam;
use crate::actions::semantic_lookup::{DLSLimitation, declarations_at_fp, definitions_at_fp, implementations_at_fp, references_at_fp};
use crate::analysis::{Named, DeclarationSpan, LocationSpan};
use crate::analysis::symbols::SimpleSymbol;
use crate::config::Config;

pub use crate::lsp_data::request::{
    ApplyWorkspaceEdit,
    CodeActionRequest,
    CodeLensRequest,
    Completion,
    DocumentHighlightRequest,
    DocumentSymbolRequest,
    ExecuteCommand,
    Formatting,
    GotoDeclaration, GotoDeclarationResponse,
    GotoDefinition,
    GotoImplementation, GotoImplementationResponse,
    HoverRequest,
    RangeFormatting,
    References,
    RegisterCapability,
    Rename,
    ResolveCompletionItem as ResolveCompletion,
    WorkspaceConfiguration,
    WorkspaceSymbolRequest,
};

pub use crate::lsp_data::{self as lsp_data, *};

use crate::analysis::structure::objects::CompObjectKind;

use crate::analysis::scope::{SymbolContext, SubSymbol, ContextKey, Scope};
use crate::analysis::symbols::{DMLSymbolKind, StructureSymbol};
use crate::actions::analysis_storage::AnalysisLookupError;
use crate::config::WarningFrequency;
use crate::file_management::CanonPath;
use crate::server;
use crate::server::{Ack, Output, Request, RequestAction, SentRequest,
                    Response, ResponseError, ResponseWithMessage};

// Gives a slightly-better error message than the short-form one specified by
// the error.
fn warn_miss_lookup(error: AnalysisLookupError, file: Option<&str>) {
    match error {
        AnalysisLookupError::NoFile =>
            error!(
                "Could not find a real file corresponding to '{}'",
                if let Some(f) = file { f.to_string() }
                else { "the opened file".to_string() }),
        AnalysisLookupError::NoIsolatedAnalysis =>
            warn!(
                "No syntactical analysis available{}",
                if let Some(f) = file { format!(" for the file '{}'", f) }
                else { "".to_string() }),
        AnalysisLookupError::NoLintAnalysis =>
            warn!(
                "No linting analysis available{}",
                if let Some(f) = file { format!(" for the file '{}'", f) }
                else { "".to_string() }),
        AnalysisLookupError::NoDeviceAnalysis =>
            warn!(
                "No semantic analysis available{}, may need to open a file \
                 with a 'device' declaration that imports{}, directly or \
                 indirectly.",
                if let Some(f) = file { format!(" that includes the file '{}'", f) }
                else { "".to_string() },
                if file.is_some() { " it".to_string() }
                else { " your file".to_string() }),
    }
}

fn response_maybe_with_limitations<I, R, O: Output>(
    path: &Path,
    response: R,
    limitations: I,
    ctx: &InitActionContext<O>)
    -> ResponseWithMessage<R>
where
     I: IntoIterator<Item = DLSLimitation>,
     R: Response + std::fmt::Debug + Serialize
{
    let filtered_limitations =
        match ctx.config.lock().unwrap().show_warnings {
            WarningFrequency::Never => vec![],
            WarningFrequency::Once => {
                let filtered = limitations.into_iter()
                    .filter(|lim|
                            !ctx.sent_warnings.lock().unwrap()
                            .contains(&(lim.issue_num, path.to_path_buf())))
                    .collect::<Vec<_>>();
                ctx.sent_warnings.lock().unwrap()
                    .extend(filtered.iter().map(
                        |lim|(lim.issue_num, path.to_path_buf())));
                filtered
            },
            _ => limitations.into_iter().collect(),
        };
    let formatted = "The DML Language server could only obtain partial results \
                     due to internal limitations:";
    let collect = filtered_limitations.into_iter().map(|lim|lim.to_string())
        .collect::<Vec<String>>().join("\n -");
    if collect.is_empty() {
        response.into()
    } else {
        ResponseWithMessage::Warn(
            response,
            format!("{}\n- {}", formatted, collect))
    }
}

/// The result of a deglob action for a single wildcard import.
///
/// The `location` is the position of the wildcard.
/// `new_text` is the text which should replace the wildcard.
#[derive(Debug, Deserialize, Serialize)]
pub struct DeglobResult {
    /// The `Location` of the "*" character in a wildcard import.
    pub location: Location,
    /// The replacement text.
    pub new_text: String,
}

// DML Structure kinds to not map 1-1 with the kinds supported by
// the lsp protocol
// TODO: Some of these should, maybe, be re-thought into different terms
fn context_to_symbolkind(context: &SymbolContext) -> lsp_types::SymbolKind {
    match &context.context {
            ContextKey::Structure(sym) |
            ContextKey::Method(sym) |
            ContextKey::Template(sym) => structure_to_symbolkind(sym.kind()),
            ContextKey::AllWithTemplate(_, _) => SymbolKind::NAMESPACE,
    }
}

fn structure_to_symbolkind(kind: DMLSymbolKind)
                           -> lsp_types::SymbolKind {
    match kind {
        DMLSymbolKind::Parameter |
        DMLSymbolKind::Constant |
        DMLSymbolKind::Loggroup
            => SymbolKind::CONSTANT,
        DMLSymbolKind::Extern |
        DMLSymbolKind::Saved |
        DMLSymbolKind::Session |
        DMLSymbolKind::Local |
        DMLSymbolKind::MethodArg =>
            SymbolKind::VARIABLE,
        DMLSymbolKind::Hook |
        DMLSymbolKind::Method =>
            SymbolKind::FUNCTION,
        DMLSymbolKind::Template =>
            SymbolKind::CLASS,
        // TODO: There is no typedef kind?
        DMLSymbolKind::Typedef =>
            SymbolKind::CONSTANT,
        DMLSymbolKind::CompObject(kind) => match kind {
            CompObjectKind::Interface => SymbolKind::INTERFACE,
            CompObjectKind::Implement => SymbolKind::STRUCT,
            // Generic comp objects most easily map to namespaces, I think?
            _ => SymbolKind::NAMESPACE,
        },
    }
}

fn subsymbol_to_document_symbol(sub: &SubSymbol) -> DocumentSymbol {
    match sub {
        SubSymbol::Context(con) => context_to_document_symbol(con),
        SubSymbol::Simple(simple) => {
            #[allow(deprecated)]
            DocumentSymbol {
                name: simple.get_name(),
                detail: None,
                kind: structure_to_symbolkind(simple.kind()),
                tags: None,
                deprecated: None,
                range: ls_util::dls_to_range(
                    simple.loc_span().range),
                selection_range: ls_util::dls_to_range(
                    simple.loc_span().range),
                children: None,
            }
        },
    }
}

fn simplesymbol_to_workspace_symbol(parent_name: &str,
                                    simple: &SimpleSymbol) -> WorkspaceSymbol {
    WorkspaceSymbol {
        name: simple.get_name(),
        container_name: Some(parent_name.to_string()),
        kind: structure_to_symbolkind(simple.kind()),
        tags: None,
        location: OneOf::Left(ls_util::dls_to_location(
            simple.loc_span())),
        data: None,
    }
}

// Filter out the flat simplesymbols that will also be output as
// contexts
fn should_output_symbol_for_subsymbol(sym: &SubSymbol) -> bool {
    match sym {
        SubSymbol::Context(_) => true,
        SubSymbol::Simple(simple) => !matches!(simple.kind(),
            DMLSymbolKind::CompObject(_)
          | DMLSymbolKind::Template
          | DMLSymbolKind::Method),
    }
}

fn context_to_document_symbol(context: &SymbolContext) -> DocumentSymbol {
    // Note: This is probably slightly inefficient for simple contexts,
    // but is unlikely to be a large problem
    let name = context.get_name();
    let span = context.span();
    let loc = context.loc_span();

    #[allow(deprecated)]
    DocumentSymbol {
        name,
        detail: None,
        kind: context_to_symbolkind(context),
        tags: None,
        deprecated: None,
        range: ls_util::dls_to_range(span.range),
        selection_range: ls_util::dls_to_range(loc.range),
        children: Some(context.subsymbols.iter()
                       .filter(|subsymbol|should_output_symbol_for_subsymbol(subsymbol))
                       .map(subsymbol_to_document_symbol)
                       .collect()),
    }
}

fn context_to_workspace_symbols_aux(context: &SymbolContext,
                                    parent_name: Option<&str>,
                                    symbols: &mut Vec<WorkspaceSymbol>) {
    // Note: This is probably slightly inefficient for simple contexts,
    // but is unlikely to be a large problem
    let name = context.get_name();
    let loc = context.loc_span();

    let full_name = if let Some(pname) = parent_name {
        format!("{}.{}", pname, name)
    } else {
        name.clone()
    };

    symbols.push(WorkspaceSymbol {
        name,
        container_name: parent_name.map(|name|name.to_string()),
        kind: context_to_symbolkind(context),
        tags: None,
        location: OneOf::Left(ls_util::dls_to_location(loc)),
        data: None,
    });

    for child in &context.subsymbols {
        if !should_output_symbol_for_subsymbol(child) {
            continue;
        }
        match child {
            SubSymbol::Context(con) => context_to_workspace_symbols_aux(
                con, Some(&full_name), symbols),
            SubSymbol::Simple(simple) => symbols.push(
                simplesymbol_to_workspace_symbol(&full_name, simple)),
        };
    }
}

fn context_to_workspace_symbols(context: &SymbolContext,
                                symbols: &mut Vec<WorkspaceSymbol>) {
    context_to_workspace_symbols_aux(context, None, symbols);
}

impl RequestAction for WorkspaceSymbolRequest {
    type Response = Option<WorkspaceSymbolResponse>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None)
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(params.query.as_str())
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        let analysis = ctx.analysis.lock().unwrap();
        let mut workspace_symbols = vec![];
        for context in analysis.all_isolated_analysises().values()
            .map(|i|&i.top_context) {
                context_to_workspace_symbols(context,
                                             &mut workspace_symbols);
            }
        Ok(Some(WorkspaceSymbolResponse::Nested(
            workspace_symbols.into_iter()
                .filter(|sym|sym.name.contains(&params.query)).collect()
        )))
    }
}

impl RequestAction for DocumentSymbolRequest {
    type Response = Option<DocumentSymbolResponse>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None)
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(params.text_document.uri.as_str())
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Handing doc symbol request {:?}", params);
        let canon_path = {
            let parsed = parse_file_path!(
                &params.text_document.uri, "document symbols")
                .map_err(|_|ResponseError::Message(
                    rpc_error_code(StandardError::ParseError),
                    "Failed to parse file path".to_string()))?;
            make_canon_path!(parsed)?
        };

        ctx.analysis.lock().unwrap()
        // TODO: Info about missing isolated analysis?
            .get_isolated_analysis(&canon_path)
            .map(|isolated|{
                let context = isolated.toplevel.to_context();
                // Fold out the toplevel context
                let symbols = context.subsymbols.iter()
                    .filter(|s|should_output_symbol_for_subsymbol(s))
                    .map(subsymbol_to_document_symbol).collect();
                Some(DocumentSymbolResponse::Nested(symbols))
            })
            .or(Self::fallback_response())
    }
}

fn text_document_position_to_ident(doc_pos: &TextDocumentPositionParams)
                                   -> String {
    format!(
        "{}-{}-{}",
        doc_pos.text_document.uri.as_str(),
        doc_pos.position.line,
        doc_pos.position.character
    )
}

impl RequestAction for HoverRequest {
    type Response = lsp_data::Hover;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(lsp_data::Hover { contents: HoverContents::Array(vec![]),
                             range: None })
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position_params))
    }

    fn handle<O: Output>(mut ctx: InitActionContext<O>,
              params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("handling hover ({:?})", params);
        let tooltip = hover::tooltip(&mut ctx,
                                     &params.text_document_position_params)?;

        Ok(lsp_data::Hover {
            contents: HoverContents::Array(tooltip.contents),
            range: Some(ls_util::dls_to_range(tooltip.range)),
        })
    }
}

// this pattern repeats enough to warrant factoring out

impl RequestAction for GotoImplementation {
    type Response = ResponseWithMessage<Option<GotoImplementationResponse>>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 5
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None.into())
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position_params))
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Requesting implementations with params {:?}", params);
        let fp = {
            let maybe_fp = ctx.text_doc_pos_to_pos(
                &params.text_document_position_params,
                "goto_impl");
            if maybe_fp.is_none() {
                return Self::fallback_response();
            }
            maybe_fp.unwrap()
        };
        let canon_path = make_canon_path!(fp.path())?;
        wait_for_device_path!(ctx, canon_path);

        let mut limitations = HashSet::new();
        
        match implementations_at_fp(&ctx, &fp, &mut limitations) {
            Ok(locs) => {
                let lsp_locations: Vec<_> = locs.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                debug!("Requested implementations are {:?}", lsp_locations);
                Ok(response_maybe_with_limitations(
                    // NOTE: this ends up being the client-path, which is
                    // actually what we want
                    &fp.path(),
                    Some(GotoImplementationResponse::Array(lsp_locations)),
                    limitations,
                    &ctx))
            },
            Err(lookuperror) => {
                let main_file_name = fp.path();
                warn_miss_lookup(lookuperror, main_file_name.to_str());
                Self::fallback_response()
            },
        }
    }
}

impl RequestAction for GotoDeclaration {
    type Response = ResponseWithMessage<Option<GotoDeclarationResponse>>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 5
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None.into())
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position_params))
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Requesting declarations with params {:?}", params);
        let fp = {
            let maybe_fp = ctx.text_doc_pos_to_pos(
                &params.text_document_position_params,
                "goto_decl");
            if maybe_fp.is_none() {
                return Self::fallback_response();
            }
            maybe_fp.unwrap()
        };
        let canon_path = make_canon_path!(fp.path())?;
        wait_for_device_path!(ctx, canon_path);

        let mut limitations = HashSet::new();
        match declarations_at_fp(&ctx, &fp, &mut limitations) {
            Ok(locs) => {
                let lsp_locations = locs.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                trace!("Requested declarations are {:?}", lsp_locations);
                Ok(response_maybe_with_limitations(
                    &fp.path(),
                    Some(GotoDefinitionResponse::Array(lsp_locations)),
                    limitations,
                    &ctx))
            },
            Err(lookuperror) => {
                let main_file_name = fp.path();
                warn_miss_lookup(lookuperror,
                                 main_file_name.to_str());
                Self::fallback_response()
            },
        }
    }
}

impl RequestAction for GotoDefinition {
    type Response = ResponseWithMessage<Option<GotoDefinitionResponse>>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 5
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None.into())
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position_params))
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Requesting definitions with params {:?}", params);
        let fp = {
            let maybe_fp = ctx.text_doc_pos_to_pos(
                &params.text_document_position_params,
                "goto_def");
            if maybe_fp.is_none() {
                return Self::fallback_response();
            }
            maybe_fp.unwrap()
        };
        let canon_path = make_canon_path!(fp.path())?;
        wait_for_device_path!(ctx, canon_path);

        let mut limitations = HashSet::new();
        match definitions_at_fp(&ctx, &fp, &mut limitations) {
            Ok(locs) => {
                let lsp_locations: Vec<_> = locs.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                Ok(response_maybe_with_limitations(
                    &fp.path(),
                    Some(GotoDefinitionResponse::Array(lsp_locations)),
                    limitations,
                    &ctx))
            },
            Err(lookuperror) => {
                let main_file_name = fp.path();
                warn_miss_lookup(lookuperror,
                                 main_file_name.to_str());
                Self::fallback_response()
            },
        }
    }
}

impl RequestAction for References {
    type Response = ResponseWithMessage<Vec<Location>>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 5
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(vec![].into())
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position))
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Requesting references with params {:?}", params);
        let fp = {
            let maybe_fp = ctx.text_doc_pos_to_pos(
                &params.text_document_position,
                "find_refs");
            if maybe_fp.is_none() {
                return Self::fallback_response();
            }
            maybe_fp.unwrap()
        };

        let canon_path = make_canon_path!(fp.path())?;
        wait_for_device_path!(ctx, canon_path);

        let mut limitations = HashSet::new();
        match references_at_fp(&ctx, &fp, &mut limitations) {
            Ok(locs) => {
                let lsp_locations: Vec<_> = locs.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                trace!("Requested references are {:?}", lsp_locations);
                Ok(response_maybe_with_limitations(
                    &fp.path(),
                    lsp_locations,
                    limitations,
                    &ctx))
            },
            Err(lookuperror) => {
                let main_file_name = fp.path();
                warn_miss_lookup(lookuperror,
                                 main_file_name.to_str());
                Self::fallback_response()
            },
        }
    }
}

impl RequestAction for Completion {
    type Response = Vec<CompletionItem>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(vec![])
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("COMPLETION REQUEST NOT IMPLEMENTED");
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: Acquire completions for location
        Self::fallback_response()
    }
}

impl RequestAction for DocumentHighlightRequest {
    type Response = Vec<lsp_data::DocumentHighlight>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(vec![])
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position_params))
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: Acquire highlighting info for file and span
        Self::fallback_response()
    }
}

impl RequestAction for Rename {
    type Response = ResponseWithMessage<WorkspaceEdit>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(ResponseWithMessage::Response(
            WorkspaceEdit { changes: None,
                            document_changes: None,
                            change_annotations: None }))
    }

    fn get_identifier(params: &Self::Params) -> String {
        // NOTE: This intentionally ignores the new name we are setting this to,
        // as cancelling a prior rename due to a new one over the same
        // position seems reasonable
        Self::request_identifier(
            &text_document_position_to_ident(
                &params.text_document_position))
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: Perform a rename
        Self::fallback_response()
    }
}

#[derive(Debug)]
pub enum ExecuteCommandResponse {
    /// Response/client request containing workspace edits.
    ApplyEdit(ApplyWorkspaceEditParams),
}

impl server::Response for ExecuteCommandResponse {
    fn send<O: Output>(self, id: server::RequestId, out: &O) {
        // TODO: should handle the client's responses
        match self {
            ExecuteCommandResponse::ApplyEdit(ref params) => {
                let id = out.provide_id();
                let params = ApplyWorkspaceEditParams {
                    label: None,
                    edit: params.edit.clone() };

                let request = Request::<ApplyWorkspaceEdit>::new(id, params);
                out.request(request);
            }
        }

        // The formal request response is a simple ACK, though the objective
        // is the preceding client requests.
        Ack.send(id, out);
    }
}

impl RequestAction for ExecuteCommand {
    type Response = ExecuteCommandResponse;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Empty)
    }

    fn get_identifier(params: &Self::Params) -> String {
        Self::request_identifier(
            &params.arguments.iter()
                .fold(params.command.to_string(),
                      |s, n|format!("{}-{}", s, n))
        )
    }

    /// Currently, no support for this
    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: ExecuteCommandParams,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: handle specialized commands. or if no such commands, remove
        Self::fallback_response()
    }
}

impl RequestAction for CodeActionRequest {
    type Response = Vec<Command>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(vec![])
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("CODE ACTION REQUEST NOT IMPLEMENTED");
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: figure out if we want to use this
        // note: a "code action" is like a command tied to a code position, I think
        Self::fallback_response()
    }
}

impl RequestAction for Formatting {
    type Response = Vec<TextEdit>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Message(
            rpc_error_code(StandardError::InternalError),
            "Reformat failed to complete successfully".into(),
        ))
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("FORMATTING REQUEST NOT IMPLEMENTED");
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: format document
        Self::fallback_response()
    }
}

impl RequestAction for RangeFormatting {
    type Response = Vec<TextEdit>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Message(
            rpc_error_code(StandardError::InternalError),
            "Reformat failed to complete successfully".into(),
        ))
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("RANGE FORMATTING REQUEST NOT IMPLEMENTED");
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: format range
        Self::fallback_response()
    }
}

impl RequestAction for ResolveCompletion {
    type Response = CompletionItem;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Empty)
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("COMPLETION ITEMS NOT IMPLEMENTED");
    }

    fn handle<O: Output>(_: InitActionContext<O>, _params: Self::Params) -> Result<Self::Response, ResponseError> {
        // TODO: figure out if we want to use this
        Self::fallback_response()
    }
}

impl RequestAction for CodeLensRequest {
    type Response = Vec<CodeLens>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Empty)
    }

    fn get_identifier(_params: &Self::Params) -> String {
        todo!("CODE LENSES NOT IMPLEMENTED");
    }

    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: figure out if we want to use this
        Self::fallback_response()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ContextDefinitionParam {
    kind: ContextDefinitionKindParam,
    active: bool,
    ready: bool,
}

#[derive(Debug, Clone)]
pub struct GetKnownContextsRequest;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetKnownContextsParams {
    // None or empty implies to get ALL contexts
    pub paths: Option<Vec<lsp_types::Uri>>,
}

impl LSPRequest for GetKnownContextsRequest {
    type Params = GetKnownContextsParams;
    type Result = Option<Vec<(lsp_types::Uri, Vec<ContextDefinitionParam>)>>;

    const METHOD: &'static str = "$/getKnownContexts";
}

impl RequestAction for GetKnownContextsRequest {
    type Response = Vec<ContextDefinitionParam>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 10
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Err(ResponseError::Empty)
    }

    fn get_identifier(params: &Self::Params) -> String {
        let path_args = if let Some(paths) = &params.paths {
            if paths.is_empty() {
                "empty".to_string()
            } else {
                paths.iter().fold("".to_string(),|s, n|format!("{}-{}",
                                                               s, n.as_str()))
            }
        } else {
            "empty".to_string()
        };
        Self::request_identifier(path_args.as_str())
    }

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        let for_these_paths: Vec<CanonPath> =
            if let Some(params) = params.paths {
                params.iter().filter_map(
                    |uri|parse_file_path!(&uri, "GetKnownContexts")
                        .ok()
                        .and_then(CanonPath::from_path_buf))
                    .collect()
            } else {
                vec![]
            };
        ctx.wait_for_state(
            AnalysisProgressKind::Isolated,
            AnalysisWaitKind::Existence,
            AnalysisCoverageSpec::Paths(for_these_paths.clone())).ok();

        let contexts: HashSet<(ContextDefinition, bool, bool)>
            = if for_these_paths.is_empty() {
                ctx.get_all_context_info()
            } else {
                for_these_paths
                    .into_iter()
                    .flat_map(|canon|{
                        let info = ctx.get_context_info(&canon);
                        info.into_iter()
                    })
                    .collect()
            };
        Ok(contexts.into_iter().filter_map(
            |(context, b, r)|
            if let ContextDefinition::Device(dev) = context {
                Some((dev, b, r))
            } else {
                None
            })
           .filter_map(
               |(path, b, r)|
               parse_uri(path.as_str())
                   .map_or_else(
                    |e|{
                        internal_error!("Wanted to report a device context which could not be converted to an URI; {}", e);
                        None
                    },
                    |uri|
                        Some(ContextDefinitionParam {
                            kind: ContextDefinitionKindParam::Device(uri),
                            active: b,
                            ready: r,
                        })))
           .collect()
        )
    }
}

/// Server-to-client requests
impl SentRequest for RegisterCapability {
    type Response = <Self as lsp_data::request::Request>::Result;
    fn on_response<O: Output>
        (_ctx: &InitActionContext<O>, _response: Self::Response, _out: &O) {
            info!("Successful registration on some capability");
        }
}

impl SentRequest for WorkspaceConfiguration {
    type Response = <Self as lsp_data::request::Request>::Result;
    fn on_response<O: Output>
        (ctx: &InitActionContext<O>, response: Self::Response, out: &O)
    {
        info!("Acquired configuration updates {:?}", response);

        let dml_object = if response.len() == 1 {
            &response[0]
        } else {
            error!("Received incorrectly formatted response to \
                    'workspace/configuration' from client (array length \
                    should be 1)\
                    : {:?}",
                   response);
            return;
        };
        use std::collections::HashMap;
        let mut dups = HashMap::new();
        let mut unknowns = vec![];
        let mut deprecated = vec![];
        let settings = Config::try_deserialize(
            dml_object,
            &mut dups,
            &mut unknowns,
            &mut deprecated,
        );
        crate::server::maybe_notify_unknown_configs(out, &unknowns);
        crate::server::maybe_notify_deprecated_configs(out, &deprecated);
        crate::server::maybe_notify_duplicated_configs(out, &dups);

        let new_config = match settings {
            Ok(value) => value,
            Err(err) => {
                warn!("Received unactionable config: {:?} (error: {:?})",
                      &response, err);
                return;
            }
        };
        let old = ctx.config.lock().unwrap().clone();
        ctx.config.lock().unwrap().update(new_config);
        ctx.maybe_changed_config(old, out);
    }
}

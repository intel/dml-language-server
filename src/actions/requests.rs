//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Requests that the DLS can respond to.

use jsonrpc::error::{StandardError, standard_error};
use log::{debug, error, info, trace, warn};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

use crate::actions::hover;
use crate::actions::{AnalysisProgressKind, AnalysisWaitKind,
                     AnalysisCoverageSpec,
                     ContextDefinition, InitActionContext};
use crate::actions::notifications::ContextDefinitionKindParam;
use crate::analysis::{ZeroSpan, ZeroFilePosition, SymbolRef};
use crate::analysis::reference::ReferenceKind;
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
use crate::analysis::{Named, DeclarationSpan, LocationSpan,
                      DLSLimitation, ISOLATED_TEMPLATE_LIMITATION};
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


pub const TYPE_SEMANTIC_LIMITATION: DLSLimitation = DLSLimitation {
    issue_num: 65,
    description: "The DLS does not currently support semantic analysis of \
                  types, including reference finding",
};

// TODO: This function is getting bloated, refactor into several smaller ones
fn fp_to_symbol_refs<O: Output>
    (fp: &ZeroFilePosition,
     ctx: &InitActionContext<O>,
     relevant_limitations: &mut HashSet<DLSLimitation>)
     -> Result<Vec<SymbolRef>, AnalysisLookupError>
{
    let analysis = ctx.analysis.lock().unwrap();
    // This step-by-step approach could be folded into analysis_storage,
    // but I keep it as separate here so that we could, perhaps,
    // returns different information for "no symbols found" and
    // "no info at pos"
    debug!("Looking up symbols/references at {:?}", fp);
    let (context_sym, reference) = (analysis.context_symbol_at_pos(fp)?,
                                    analysis.reference_at_pos(fp)?);
    debug!("Got {:?} and {:?}", context_sym, reference);
    let canon_path = CanonPath::from_path_buf(fp.path()).unwrap();
    // Rather than holding the lock throughout the request, clone
    // the filter
    let filter = Some(ctx.device_active_contexts.lock().unwrap().clone());
    let mut definitions = vec![];
    let analysises = analysis.all_device_analysises_containing_file(
        &CanonPath::from_path_buf(fp.path()).unwrap());
    if analysises.is_empty() {
        return Err(AnalysisLookupError::NoDeviceAnalysis);
    }
    match (context_sym, reference) {
        (None,  None) => {
            debug!("No symbol or reference at point");
            return Ok(vec![]);
        },
        (Some(sym), refer) => {
            if refer.is_some() {
                error!("Obtained both symbol and reference at {:?}\
                        (reference is {:?}), defaulted to symbol",
                       &fp, refer);
            }
            for device in analysis.filtered_device_analysises_containing_file(
                &canon_path,
                filter.as_ref()) {
                definitions.extend(
                    device.lookup_symbols_by_contexted_symbol(
                        &sym, relevant_limitations)
                        .into_iter());
            }
        },
        (None, Some(refr)) => {
            debug!("Mapping {:?} to symbols", refr.loc_span());
            // Should be guaranteed by the context reference lookup above
            // (isolated analysis does exist)
            if refr.reference_kind() == ReferenceKind::Type {
                relevant_limitations.insert(TYPE_SEMANTIC_LIMITATION);
            }

            let first_context = analysis.first_context_at_pos(fp).unwrap();
            let mut any_template_used = false;
            for device in analysis.filtered_device_analysises_containing_file(
                &canon_path,
                filter.as_ref()) {
                debug!("reference info is {:?}", device.reference_info.keys());
                // NOTE: This ends up being the correct place to warn users
                // about references inside uninstantiated templates,
                // but we have to perform some extra work to find out we are
                // in that case
                if let Some(ContextKey::Template(ref sym)) = first_context {
                    if device.templates.templates.get(sym.name_ref())
                        .and_then(|t|t.location.as_ref())
                        .and_then(
                            |loc|device.template_object_implementation_map.get(loc))
                        .map_or(false, |impls|!impls.is_empty()) {
                            any_template_used = true;
                        }
                }
                if let Some(defs) = device.reference_info.get(
                    refr.loc_span()) {
                    for def in defs {
                        definitions.push(Arc::clone(def));
                    }
                }
            }
            if let Some(ContextKey::Template(_)) = first_context {
                if !any_template_used {
                    relevant_limitations.insert(ISOLATED_TEMPLATE_LIMITATION);
                }
            }
        },
    }
    Ok(definitions)
}

fn handle_default_remapping<O: Output>
    (ctx: &InitActionContext<O>,
     symbols: Vec<SymbolRef>,
     fp: &ZeroFilePosition) -> HashSet<ZeroSpan>
{
    let analysis = ctx.analysis.lock().unwrap();
    let refr_opt = analysis.reference_at_pos(fp);
    // NOTE: Because the call to this is preceded by a symbol lookup,
    // it is probably safe to discard an error here
    if let Ok(Some(refr)) = refr_opt {
        if refr.to_string().as_str() == "default" {
            // If we are at a defaut reference,
            // remap symbol references to methods
            // to the correct decl site, leave others as-is
            return symbols.into_iter()
                .flat_map(|d|'sym: {
                    let sym = d.lock().unwrap();
                    if sym.kind == DMLSymbolKind::Method {
                        if let Some(loc) = sym.default_mappings
                            .get(refr.loc_span()) {
                                break 'sym vec![*loc];
                            }
                    }
                    sym.declarations.clone()
                }).collect();
        }
    }
    symbols.into_iter()
        .flat_map(|d|d.lock().unwrap().declarations.clone())
        .collect()
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

    fn handle<O: Output>(
        ctx: InitActionContext<O>,
        params: Self::Params,
    ) -> Result<Self::Response, ResponseError> {
        debug!("Handing doc symbol request {:?}", params);
        let parse_canon_path = parse_file_path!(
            &params.text_document.uri, "document symbols")
            .map(CanonPath::from_path_buf);

        if let Ok(Some(canon_path)) = parse_canon_path {
            ctx.analysis.lock().unwrap()
                // TODO: Info about missing isolated analysis?
                .get_isolated_analysis(&canon_path)
                .map(|isolated|{
                    let context = isolated.toplevel.to_context();
                    // Fold out the toplevel context
                    let symbols = context.subsymbols.iter().map(
                        subsymbol_to_document_symbol).collect();
                    Some(DocumentSymbolResponse::Nested(symbols))
                })
                .or(Self::fallback_response())
        } else {
            Self::fallback_response()
        }
    }
}

impl RequestAction for HoverRequest {
    type Response = lsp_data::Hover;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(lsp_data::Hover { contents: HoverContents::Array(vec![]),
                             range: None })
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

impl RequestAction for GotoImplementation {
    type Response = ResponseWithMessage<Option<GotoImplementationResponse>>;

    fn timeout() -> std::time::Duration {
        crate::server::dispatch::DEFAULT_REQUEST_TIMEOUT * 5
    }

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(None.into())
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
        ctx.wait_for_state(
            AnalysisProgressKind::DeviceDependencies,
            AnalysisWaitKind::Work,
            AnalysisCoverageSpec::Paths(
                std::iter::once(CanonPath::from_path_buf(fp.path()).unwrap())
                    .collect())).ok();

        let mut limitations = HashSet::new();
        match fp_to_symbol_refs(&fp, &ctx, &mut limitations) {
            Ok(symbols) => {
                let mut unique_locations: HashSet<ZeroSpan>
                    = HashSet::default();
                for symbol in symbols {
                    for implementation in &symbol.lock().unwrap().implementations {
                        unique_locations.insert(*implementation);
                    }
                }
                let lsp_locations: Vec<_> = unique_locations.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                trace!("Requested implementations are {:?}", lsp_locations);
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
                warn_miss_lookup(lookuperror,
                                 main_file_name.to_str());
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
        ctx.wait_for_state(
            AnalysisProgressKind::DeviceDependencies,
            AnalysisWaitKind::Work,
            AnalysisCoverageSpec::Paths(
                std::iter::once(CanonPath::from_path_buf(fp.path()).unwrap())
                    .collect())).ok();

        let mut limitations = HashSet::new();
        match fp_to_symbol_refs(&fp, &ctx, &mut limitations) {
            Ok(symbols) => {
                let unique_locations = handle_default_remapping(&ctx,
                                                                symbols,
                                                                &fp);
                let lsp_locations = unique_locations.into_iter()
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
        ctx.wait_for_state(
            AnalysisProgressKind::DeviceDependencies,
            AnalysisWaitKind::Work,
            AnalysisCoverageSpec::Paths(
                std::iter::once(CanonPath::from_path_buf(fp.path()).unwrap())
                    .collect())).ok();

        let mut limitations = HashSet::new();
        match fp_to_symbol_refs(&fp, &ctx, &mut limitations) {
            Ok(symbols) => {
                let unique_locations: HashSet<ZeroSpan> =
                    symbols.into_iter()
                    .flat_map(|d|d.lock().unwrap().definitions.clone()).collect();
                let lsp_locations: Vec<_> = unique_locations.into_iter()
                    .map(|l|ls_util::dls_to_location(&l))
                    .collect();
                trace!("Requested definitions are {:?}", lsp_locations);
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
        ctx.wait_for_state(
            AnalysisProgressKind::DeviceDependencies,
            AnalysisWaitKind::Work,
            AnalysisCoverageSpec::Paths(
                std::iter::once(CanonPath::from_path_buf(fp.path()).unwrap())
                    .collect())).ok();
        let mut limitations = HashSet::new();
        match fp_to_symbol_refs(&fp, &ctx, &mut limitations) {
            Ok(symbols) => {
                let unique_locations: HashSet<ZeroSpan> =
                    symbols.into_iter()
                    .flat_map(|d|d.lock().unwrap().references.clone()).collect();
                let lsp_locations: Vec<_> = unique_locations.into_iter()
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
        // FIXME should handle the client's responses
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

    /// Currently, no support for this
    fn handle<O: Output>(
        _ctx: InitActionContext<O>,
        _params: ExecuteCommandParams,
    ) -> Result<Self::Response, ResponseError> {
        // TODO: handle specialized commands. or if no such commands, remove
        Self::fallback_response()
    }
}

fn rpc_error_code(code: StandardError) -> Value {
    Value::from(standard_error(code, None).code)
}

impl RequestAction for CodeActionRequest {
    type Response = Vec<Command>;

    fn fallback_response() -> Result<Self::Response, ResponseError> {
        Ok(vec![])
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
               parse_uri(path.as_str()).ok()
                   .map(|uri|
                        ContextDefinitionParam {
                            kind: ContextDefinitionKindParam::Device(uri),
                            active: b,
                            ready: r,
                        }))
           .collect()
        )
    }
}

/// Server-to-client requests
impl SentRequest for RegisterCapability {
    type Response = <Self as lsp_data::request::Request>::Result;
    fn on_response<O: Output>
        (_ctx: &mut InitActionContext<O>, _response: Self::Response, _out: &O) {
            info!("Successful registration on some capability");
        }
}

impl SentRequest for WorkspaceConfiguration {
    type Response = <Self as lsp_data::request::Request>::Result;
    fn on_response<O: Output>
        (ctx: &mut InitActionContext<O>, response: Self::Response, out: &O)
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

        ctx.config.lock().unwrap().update(new_config);
        ctx.maybe_changed_config(out);
    }
}

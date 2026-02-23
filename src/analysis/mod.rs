//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
// Load parser and tree first to ensure existance of macros
#[macro_use]
pub mod parsing;
#[macro_use]
pub mod symbols;
pub mod provisionals;
pub mod scope;
pub mod reference;
pub mod structure;
pub mod templating;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::Mutex;

use lsp_types::{DiagnosticSeverity};
use logos::Logos;
use log::{debug, error, info, trace};
use rayon::prelude::*;

use crate::actions::SourcedDMLError;
use crate::actions::analysis_storage::TimestampedStorage;
use crate::actions::semantic_lookup::{DLSLimitation, isolated_template_limitation};
use crate::analysis::symbols::{DMLSymbolKind, SimpleSymbol, StructureSymbol, SymbolContainer, SymbolMaker, SymbolSource};
pub use crate::analysis::symbols::SymbolRef;
use crate::analysis::reference::{GlobalReference, NodeRef, Reference, ReferenceKind, ReferenceVariant, VariableReference};
use crate::analysis::scope::{Scope, SymbolContext,
                             ContextKey, ContextedSymbol};
use crate::analysis::parsing::parser::{FileInfo, FileParser};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::provisionals::ProvisionalsManager;

pub use crate::analysis::parsing::tree::
{ZeroRange, ZeroSpan, ZeroPosition, ZeroFilePosition};

use crate::analysis::parsing::tree::{MissingToken, MissingContent, TreeElement};
use crate::analysis::structure::objects::{CompObjectKind, Import, MaybeAbstract, ParamValue, Template};
use crate::analysis::structure::statements::{ForPre, Statement, StatementKind};
use crate::analysis::structure::toplevel::{ObjectDecl, TopLevel};
use crate::analysis::structure::types::DMLType;
use crate::analysis::structure::expressions::{Expression, ExpressionKind,
                                              DMLString};
use crate::analysis::templating::objects::{make_device, DMLObject,
                                           DMLResolvedObject,
                                           DMLShallowObjectVariant,
                                           DMLCompositeObject,
                                           DMLShallowObject,
                                           DMLHierarchyMember,
                                           DMLNamedMember,
                                           StructureContainer, StructureKey};
use crate::analysis::templating::topology::{RankMaker,
                                            rank_templates,
                                            create_templates_traits};
use crate::analysis::templating::methods::{DMLMethodArg, DMLMethodRef, DefaultCallReference, MethodDeclaration};
use crate::analysis::templating::traits::{DMLTemplate,
                                          TemplateTraitInfo};
use crate::analysis::templating::types::DMLResolvedType;

use crate::concurrency::AliveStatus;
use crate::file_management::{PathResolver, CanonPath};

use crate::vfs::{TextFile, Error};

#[derive(Clone, Copy)]
pub struct FileSpec<'a> {
    pub path: &'a Path,
    pub file: &'a TextFile,
}

pub const IMPLICIT_IMPORTS: [&str; 2] = ["dml-builtins.dml",
                                         "simics/device-api.dml"];

// For things whose names are in one spot as a dmlstring
pub trait DMLNamed {
    fn name(&self) -> &DMLString;
}

impl <T: DMLNamed> LocationSpan for T {
    fn loc_span(&self) -> &ZeroSpan {
        &self.name().span
    }
}

// serves as the catch-all when we _might_ need something whose
// name is not present as a literal in analyzed source
pub trait Named {
    fn get_name(&self) -> String;
}

impl <T: DMLNamed> Named for T {
    fn get_name(&self) -> String {
        self.name().val.clone()
    }
}

// These traits relate to things knowing the parts of the file
// that their declaration covers
pub trait DeclarationRange {
    fn range(&self) -> &ZeroRange;
}

pub trait DeclarationFile {
    fn file(&self) -> PathBuf;
}

pub trait DeclarationSpan {
    fn span(&self) -> &ZeroSpan;
}

impl <T: DeclarationSpan> DeclarationRange for T {
    fn range(&self) -> &ZeroRange {
        &self.span().range
    }
}

impl <T: DeclarationSpan> DeclarationFile for T {
    fn file(&self) -> PathBuf {
        self.span().path()
    }
}

// For things that know a more specific range where their named declaration
// is, these traits apply
pub trait LocationRange {
    fn loc_range(&self) -> &ZeroRange;
}

pub trait LocationFile {
    fn loc_file(&self) -> PathBuf;
}

pub trait LocationSpan {
    fn loc_span(&self) -> &ZeroSpan;
}

fn combine_vec_of_decls<T: DeclarationSpan>(vec: &[T])
                                            -> ZeroSpan {
    // We do not have an 'invalid' file value,
    // so doing this for empty vecs is not allowed
    let file = &vec.first().unwrap().file();
    assert!(!vec.iter().any(|f|&f.file() != file));
    ZeroSpan::combine(*vec.first().unwrap().span(),
                      *vec.last().unwrap().span())
}

impl <T: LocationSpan> LocationRange for T {
    fn loc_range(&self) -> &ZeroRange {
        &self.loc_span().range
    }
}

impl <T: LocationSpan> LocationFile for T {
    fn loc_file(&self) -> PathBuf {
        self.loc_span().path()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLError {
    pub span: ZeroSpan,
    pub description: String,
    pub severity: Option<DiagnosticSeverity>,
    pub related: Vec<(ZeroSpan, String)>,
}

impl Hash for DMLError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.description.hash(state);
        match self.severity {
            Some(DiagnosticSeverity::ERROR) => 1.hash(state),
            Some(DiagnosticSeverity::WARNING) => 2.hash(state),
            Some(DiagnosticSeverity::INFORMATION) => 3.hash(state),
            Some(DiagnosticSeverity::HINT) => 4.hash(state),
            _ => panic!(),
        }
        self.related.hash(state);
    }
}

impl DMLError {
    pub fn with_source(self, source: &'static str) -> SourcedDMLError {
        SourcedDMLError {
            error: self,
            source,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalDMLError {
    pub range: ZeroRange,
    pub description: String,
}

impl LocalDMLError {
    fn with_file<F: Into<PathBuf>>(self, file: F) -> DMLError {
        DMLError {
            span: ZeroSpan::from_range(self.range, file),
            description: self.description,
            severity: Some(DiagnosticSeverity::ERROR),
            related: vec![],
        }
    }
    pub fn warning_with_file<F: Into<PathBuf>>(self, file: F) -> DMLError {
        DMLError {
            span: ZeroSpan::from_range(self.range, file),
            description: self.description,
            related: vec![],
            severity: Some(DiagnosticSeverity::WARNING),
        }
    }
}

impl From<&MissingToken> for LocalDMLError {
    fn from(miss: &MissingToken) -> Self {
        LocalDMLError {
            range: ZeroRange::from_positions(miss.position, miss.position),
            description: format!("Expected {}, got {}", miss.description,
                                 match miss.ended_by {
                                     Some(endtok) => endtok.kind.description(),
                                     None => "EOF",
                                 }),
        }
    }
}

pub fn make_error_from_missing_content(
    range: ZeroRange, content: &MissingContent) -> LocalDMLError {
    LocalDMLError {
        range,
        description: format!("Expected {}, got {}", content.description,
                             match content.ended_by {
                                 Some(endtok) => endtok.kind.description(),
                                 None => "EOF",
                             }),
    }
}

// Analysis from the perspective of a particular DML file
#[derive(Debug, Clone)]
pub struct IsolatedAnalysis {
    // Toplevel ast of file
    pub ast: parsing::structure::TopAst,

    // Toplevel structure of file
    pub toplevel: TopLevel,

    // Cached structural contexts of file
    pub top_context: SymbolContext,

    // File info
    pub path: CanonPath,
    // This is the path the client has the file open as,
    // which is relevant so that we report errors correctly
    // NOTE: This might not be a canonpath, but it still needs
    // to be an absolute path or we'll run into
    // trouble reporting errors
    pub clientpath: PathBuf,

    // Errors are used as input for various responses/requests to the client
    pub errors: Vec<DMLError>,
}

// Invariant: range covers all ranges in sub_ranges
#[derive(Debug, Clone)]
pub struct RangeEntry {
    range: ZeroRange,
    symbols: HashMap<String, SymbolRef>,
    // TODO: Consider replacing with a spatial-search data-structure
    // e.g. segment or interval tree
    sub_ranges: Vec<RangeEntry>,
}

impl RangeEntry {
    fn is_empty(&self) -> bool {
        self.symbols.is_empty()
            && self.sub_ranges.iter().all(|sub|sub.is_empty())
    }
    fn find_symbol_for_dmlname(&self, name: &DMLString) -> Option<&SymbolRef> {
        self.find_symbol_for_name(
            &name.val, name.span.start_position().position)
    }

    fn find_smallest_scope_around(&self, loc: ZeroPosition)
                                  -> Option<ZeroRange> {
        if self.range.contains_pos(loc) {
            self.sub_ranges.iter()
                .find_map(|sub|sub.find_smallest_scope_around(loc))
                .or(Some(self.range))
        } else {
            None
        }
    }

    fn find_symbol_for_name(&self,
                            name: &str,
                            loc: ZeroPosition) -> Option<&SymbolRef> {
        let possible_symbol = self.find_symbol_for_name_aux(name, loc)?;
        // Don't find symbols in the same scope that are declared after
        // the reference
        if loc > possible_symbol.lock().unwrap().loc.range.start() {
            Some(possible_symbol)
        } else {
            None
        }
    }

    fn find_symbol_for_name_aux(&self,
                                name: &str,
                                loc: ZeroPosition) -> Option<&SymbolRef> {
        if self.range.contains_pos(loc) {
            // Look into deeper scopes first
            self.sub_ranges.iter()
                .find_map(|re|re.find_symbol_for_name_aux(name, loc))
                .or_else(||self.symbols.get(name))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolStorage {
    pub template_symbols: HashMap<ZeroSpan, SymbolRef>,
    // Because some implicit parameters are defined in the same
    // place, we need to disambiguate this by name
    pub param_symbols: HashMap<(ZeroSpan, String),
                               HashMap<StructureKey, SymbolRef>>,
    pub object_symbols: HashMap<StructureKey, SymbolRef>,
    // This is doubly-indexed, by decl location and then
    // by parent object key
    pub method_symbols: HashMap<ZeroSpan, HashMap<StructureKey, SymbolRef>>,
    // constants, sessions, saveds, hooks, method args
    pub variable_symbols: HashMap<ZeroSpan, SymbolRef>,
}

impl SymbolStorage {
    pub fn all_symbols<'a>(&'a self) -> impl Iterator<Item = &'a SymbolRef> {
        self.template_symbols.values()
            .chain(self.param_symbols.values().flat_map(|h|h.values()))
            .chain(self.object_symbols.values())
            .chain(self.method_symbols.values().flat_map(|h|h.values()))
            .chain(self.variable_symbols.values())
    }
}

// This maps references to the symbol they reference, made as a lock
// because we need to incrementally fill it as requests are made
type ReferenceStorage = Arc<Mutex<HashMap<ZeroSpan, Vec<SymbolRef>>>>;

// Analysis from the perspective of a particular DML device
#[derive(Debug, Clone)]
pub struct DeviceAnalysis {
    // Device name
    pub name: String,
    pub errors: HashMap<PathBuf, Vec<DMLError>>,
    pub objects: StructureContainer,
    pub device_obj: DMLObject,
    pub templates: TemplateTraitInfo,
    pub symbol_info: SymbolStorage,
    pub reference_info: ReferenceStorage,
    pub template_object_implementation_map: HashMap<ZeroSpan,
                                                    Vec<StructureKey>>,
    pub path: CanonPath,
    pub dependant_files: Vec<CanonPath>,
    pub clientpath: PathBuf,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ReferenceMatchKind {
    Found,
    MismatchedFind,
    NotFound,
}

#[derive(Debug, Clone)]
pub struct ReferenceMatches {
    pub kind: ReferenceMatchKind,
    // How these references are interpreted depends on 'kind',
    // for NotFound and MismatchedFind they are generally suggestions
    // whereas for Found they are actual matches
    pub references: HashSet<SymbolRef>,
    // These will be reported as-is, regardless of kind
    pub messages: Vec<DMLError>,
}

impl Default for ReferenceMatches {
    fn default() -> Self {
        ReferenceMatches {
            kind: ReferenceMatchKind::NotFound,
            references: HashSet::default(),
            messages: vec![],
        }
    }
}

impl ReferenceMatches {
    pub fn add_match(&mut self, reference: SymbolRef) {
        if self.kind != ReferenceMatchKind::MismatchedFind {
            self.kind = ReferenceMatchKind::Found;
            self.references.insert(reference);
        }
    }

    pub fn add_suggestion(&mut self, reference: SymbolRef) {
        if !matches!(self.kind, ReferenceMatchKind::Found
                              | ReferenceMatchKind::MismatchedFind) {
            self.references.insert(reference);
        }
    }

    pub fn set_mismatched(&mut self, reference: SymbolRef) {
        self.kind = ReferenceMatchKind::MismatchedFind;
        self.references.insert(reference);
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn as_matches(&self) -> Option<HashSet<SymbolRef>> {
        if self.kind == ReferenceMatchKind::Found {
            Some(self.references.clone())
        } else {
            None
        }
    }

    pub fn as_suggestions(&self) -> Option<HashSet<SymbolRef>> {
        if self.kind == ReferenceMatchKind::NotFound {
            Some(self.references.clone())
        } else {
            None
        }
    }

    pub fn as_mismatched(&self) -> Option<HashSet<SymbolRef>> {
        if self.kind == ReferenceMatchKind::MismatchedFind {
            Some(self.references.clone())
        } else {
            None
        }
    }

    pub fn add_message(&mut self, message: DMLError) {
        self.messages.push(message);
    }

    pub fn merge_with(&mut self, other: Self) {
        match other.kind {
            ReferenceMatchKind::MismatchedFind =>
                for sym in other.references {
                    self.set_mismatched(sym);
                },
            ReferenceMatchKind::Found =>
                for sym in other.references {
                    self.add_match(sym);
                },
            ReferenceMatchKind::NotFound =>
                for sym in other.references {
                    self.add_suggestion(sym);
                },
        }
        self.messages.extend(other.messages);
    }
}

/// TODO: Consider usage and variants of type hints
pub type TypeHint = DMLResolvedType;

// We replicate some of the structures from scope and reference here, because
// we need to _discard_ the location information for the caching to work

// agnostic context key
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum AgnConKey {
    Object(String),
    Template(String),
    AllWithTemplate(Vec<String>),
}

// Agnostic reference
type AgnRef = Vec<String>;

type ReferenceCacheKey = (Vec<AgnConKey>, AgnRef, Option<ZeroRange>);
#[derive(Default)]
struct ReferenceCache {
    underlying_cache: HashMap<ReferenceCacheKey, ReferenceMatches>,
}

impl ReferenceCache {
    fn flatten_ref(refr: &NodeRef, agn: &mut Vec<String>) {
        match refr {
            NodeRef::Simple(dmlstring) => agn.push(dmlstring.val.clone()),
            NodeRef::Sub(sub, dmlstring, _) => {
                Self::flatten_ref(sub.as_ref(), agn);
                agn.push(dmlstring.val.clone());
            },
        }
    }
    fn convert_to_key(key: (Vec<ContextKey>, VariableReference),
                      method_structure: &HashMap<ZeroSpan, RangeEntry>)
                      -> ReferenceCacheKey {
        let (contexts, refr) = key;
        let method_scope = contexts.last()
            .and_then(|ck|if let ContextKey::Method(meth) = ck {
                method_structure.get(meth.loc_span())
                    .and_then(
                        |re|re.find_smallest_scope_around(
                            refr.loc_span().range.start()))
            } else {
                None
            });
        let agnostic_context = contexts.into_iter().map(
            |con|match con {
                ContextKey::Structure(sym) |
                ContextKey::Method(sym) =>  AgnConKey::Object(sym.get_name()),
                ContextKey::Template(sym) => AgnConKey::Template(
                    sym.get_name()),
                ContextKey::AllWithTemplate(_, names) =>
                    AgnConKey::AllWithTemplate(names.clone()),
            }).collect();
        let mut agnostic_reference = vec![];
        Self::flatten_ref(&refr.reference, &mut agnostic_reference);
        (agnostic_context, agnostic_reference, method_scope)
    }

    pub fn get(&self,
               key: (Vec<ContextKey>,
                     VariableReference),
               method_structure: &HashMap<ZeroSpan, RangeEntry>)
               -> Option<&ReferenceMatches> {
        let agn_key = Self::convert_to_key(key, method_structure);
        self.underlying_cache.get(&agn_key)
    }

    pub fn insert(&mut self,
                  key: (Vec<ContextKey>,
                        VariableReference),
                  val: ReferenceMatches,
                  method_structure: &HashMap<ZeroSpan, RangeEntry>)
    {
        let agn_key = Self::convert_to_key(key, method_structure);
        self.underlying_cache.insert(agn_key, val);
    }
}

fn all_scopes<'c>(bases: &'c Vec<IsolatedAnalysis>)
                  -> Vec<Vec<&'c dyn Scope>> {
    let mut scopes = vec![];
    for base in bases {
        gather_scopes(vec![&base.toplevel as &dyn Scope],
                      vec![],
                      &mut scopes);
    }
    scopes
}

fn gather_scopes<'c>(next_scopes: Vec<&'c dyn Scope>,
                     scope_chain: Vec<&'c dyn Scope>,
                     collected_scopes: &mut Vec<Vec<&'c dyn Scope>>) {
    for scope in next_scopes {
        let mut new_chain = scope_chain.clone();
        new_chain.push(scope);
        collected_scopes.push(new_chain.clone());
        gather_scopes(scope.defined_scopes(),
                      new_chain,
                      collected_scopes);
    }
}

impl DeviceAnalysis {
    pub fn lookup_symbols<'t>(&self, context_sym: &ContextedSymbol<'t>, limitations: &mut HashSet<DLSLimitation>)
        -> Vec<SymbolRef> {
        trace!("Looking up symbols at {:?}", context_sym);

        // Special handling for methods, since each method decl has its
        // own symbol
        if context_sym.kind() == DMLSymbolKind::Method {
            return self.symbol_info.method_symbols
                .get(context_sym.loc_span())
                .map(|m|m.values().cloned())
                .into_iter().flatten().collect();
        }
        self.lookup_symbols_by_contexted_symbol(context_sym, limitations)
    }

    pub fn get_device_obj(&self) -> &DMLObject {
        &self.device_obj
    }

    pub fn get_device_symbol(&self) -> SymbolRef {
        Arc::clone(
            self.symbol_info.object_symbols
                .get(&self.get_device_obj_key()).unwrap()
        )
    }

    pub fn get_device_obj_key(&self) -> StructureKey {
        if let DMLObject::CompObject(key) = &self.device_obj {
            *key
        } else {
            panic!("Internal Error: DeviceAnalysis device object was \
                    not a composite object");
        }
    }

    pub fn get_device_comp_obj(&self) -> &DMLCompositeObject {
        self.objects.get(self.get_device_obj_key()).unwrap()
    }

    // TODO: Currently these functions do a lot of dumb cast-to-comp-then-back
    // which is the result of us wanting to return DMLObject _references.
    // A DMLObject is (relatively) small so we could consider returning
    // values, allowing us to re-construct DMLObjects and having
    // get_objs_matching_templates operate on composite objects as roots
    fn get_objs_matching_templates_aux(&self,
                                       root: &DMLCompositeObject,
                                       spec: &[&Arc<DMLTemplate>],
                                       aux: &mut Vec<DMLObject>) {
        // TODO: Remind yourself of how dmlc in-each works, does a match
        // blocks further recursion?
        let mut all_match = true;
        for templ in spec {
            if !root.templates.contains_key(&templ.name) {
                all_match = false;
            }
        }
        if all_match {
            aux.push(DMLObject::CompObject(root.key));
        } else {
            for obj in root.components.values() {
                if let DMLResolvedObject::CompObject(robj)
                    = obj.resolve(&self.objects) {
                        self.get_objs_matching_templates_aux(
                            robj, spec, aux);
                    }
            }
        }
    }

    fn get_objs_matching_templates(&self,
                                   root: &DMLCompositeObject,
                                   spec: &[&str])
                                   -> Vec<DMLObject> {
        // TODO: Are we guaranteed that each-ins handled here specify
        // _only_ existing template names? I suspect they do not
        let unique_names: HashSet<&str> = spec.iter().cloned().collect();
        let set: Vec<&Arc<DMLTemplate>> = unique_names.into_iter()
            .map(|name|self.templates.templates.get(name).unwrap())
            .collect();
        let mut result = vec![];
        for obj in root.components.values() {
            if let DMLResolvedObject::CompObject(robj)
                = obj.resolve(&self.objects) {
                    self.get_objs_matching_templates_aux(robj, &set,
                                                         &mut result);
                }
        }
        result
    }


    // TODO: Reconsider function signature
    // currently returning whole DMLObject so we can faux
    // them from structurekeys obtained through
    fn contexts_to_objs(&self,
                        curr_objs: Vec<DMLObject>,
                        context_chain: &[ContextKey],
                        limitations: &mut HashSet<DLSLimitation>)
                        -> Option<Vec<DMLObject>> {
        if context_chain.is_empty() {
            Some(curr_objs)
        } else {
            self.contexts_to_objs_aux(
                curr_objs.into_iter().filter_map(
                    |obj|match obj.resolve(&self.objects) {
                        DMLResolvedObject::CompObject(robj) => Some(robj),
                        DMLResolvedObject::ShallowObject(sobj) => {
                            internal_error!(
                                "Internal Error: \
                                 Wanted to find context {:?} in {:?} \
                                 which is not \
                                 a composite object", sobj, context_chain);
                            None
                        }
                    }).collect(),
                context_chain,
                limitations)
        }
    }

    fn contexts_to_objs_aux(&self,
                            curr_objs: Vec<&DMLCompositeObject>,
                            context_chain: &[ContextKey],
                            limitations: &mut HashSet<DLSLimitation>)
                            -> Option<Vec<DMLObject>> {
        let result: Vec<DMLObject> = curr_objs.into_iter()
            .filter_map(|o|self.context_to_objs(o, context_chain, limitations))
            .flatten().collect();
        if result.is_empty() {
            None
        } else {
            Some(result)
        }
    }

    fn context_to_objs(&self,
                       curr_obj: &DMLCompositeObject,
                       context_chain: &[ContextKey],
                       limitations: &mut HashSet<DLSLimitation>)
                       -> Option<Vec<DMLObject>> {
        // Should be guaranteed by caller responsibility
        if context_chain.is_empty() {
            internal_error!(
                "context chain invariant broken at {:?}",
                curr_obj.identity());
        }

        let (first, rest) = context_chain.split_first().unwrap();
        let next_objs = match first {
            ContextKey::Structure(sym) =>
                curr_obj.get_object(sym.name_ref()).cloned()
                .into_iter().collect(),
            ContextKey::Method(sym) => {
                if let Some(found_obj)
                    = curr_obj.get_object(sym.name_ref()) {
                        if found_obj.resolve(&self.objects).as_shallow()
                            .map_or(false, |s|matches!(
                                &s.variant,
                                DMLShallowObjectVariant::Method(_))) {
                                vec![found_obj.clone()]
                            }
                        else {
                            internal_error!(
                                "Context chain suggested {:?} should be a \
                                 method, but it wasn't",
                                found_obj.resolve(&self.objects));
                            vec![]
                        }
                    }
                else {
                    // If it is an error to not find an result, handle
                    // it higher up in stack
                    return None;
                }
            },
            ContextKey::Template(sym) =>
                if let Some(templ) = self.templates.templates.get(sym.name_ref()) {
                    if let Some(templ_impls) =
                        templ.location.as_ref().and_then(
                            |loc|self
                                .template_object_implementation_map
                                .get(loc))
                    {
                        if templ_impls.is_empty() {
                            limitations.insert(
                                isolated_template_limitation(&templ.name)
                            );
                        }
                        return self.contexts_to_objs(
                            templ_impls
                                .iter()
                                .map(|key|DMLObject::CompObject(*key))
                                .collect(),
                            rest,
                            limitations);
                    }
                    else {
                        internal_error!(
                            "No template->objects map for {:?}", sym);
                        vec![]
                    }
                } else {
                    internal_error!(
                        "Wanted to find context {:?} in {:?} which is not \
                         a known template object",
                        first, curr_obj);
                    return None;
                },
            ContextKey::AllWithTemplate(_, templates) =>
                return self.contexts_to_objs(
                    self.get_objs_matching_templates(
                        curr_obj, templates.iter()
                            .map(|s|s.as_str())
                            .collect::<Vec<&str>>()
                            .as_slice()),
                    rest,
                    limitations),
        };
        self.contexts_to_objs(next_objs, rest, limitations)
    }

    // Part of constant folding, try to resolve an expression that
    // is perhaps a noderef into the symbol of the node it refers to
    fn expression_to_resolved_objects<'t, 'c>(
        &'c self,
        expr: &Expression,
        _scope: DMLResolvedObject<'t, 'c>)
        -> Vec<DMLResolvedObject<'t, 'c>> {
        // TODO: For now, we can only resolve some simple expressions
        #[allow(clippy::single_match)]
        match expr.as_ref() {
            ExpressionKind::AutoObjectRef(key, _) =>
                if let Some(obj) = self.objects.get(*key) {
                    return vec![DMLResolvedObject::CompObject(obj)]
                },
            _ => (),
        }
        vec![]
    }

    fn resolved_to_symbol<'t, 'c>(&'c self,
                                  parent: &'c DMLCompositeObject,
                                  obj: DMLResolvedObject<'t, 'c>)
                                  -> Option<&'c SymbolRef> {
        match obj {
            DMLResolvedObject::CompObject(comp) =>
                self.symbol_info.object_symbols.get(&comp.key),
            DMLResolvedObject::ShallowObject(shallow) =>
                match &shallow.variant {
                    DMLShallowObjectVariant::Method(m) =>
                    // For resolutions, a methods symbols is just the symbol
                    // of the method we resolved to
                        self.symbol_info.method_symbols
                            .get(m.location())
                            .and_then(|m|m.get(&parent.key)),
                    DMLShallowObjectVariant::Session(s) |
                    DMLShallowObjectVariant::Saved(s) =>
                        self.symbol_info.variable_symbols.get(s.loc_span()),
                    DMLShallowObjectVariant::Constant(c) =>
                        self.symbol_info.variable_symbols.get(c.loc_span()),
                    DMLShallowObjectVariant::Hook(h) =>
                        self.symbol_info.variable_symbols.get(h.loc_span()),
                    DMLShallowObjectVariant::Parameter(p) =>
                        self.symbol_info.param_symbols.get(
                            &(*p.loc_span(), p.name().val.to_string()))
                        .and_then(|m|m.get(&parent.key))
                },
        }
    }

    fn lookup_def_in_comp_object<'c>(&'c self,
                                     obj: &'c DMLCompositeObject,
                                     name: &str,
                                     _type_hint: Option<TypeHint>,
                                     ref_matches: &mut ReferenceMatches) {
        debug!("Looking up {} in {:?}", name, obj.identity());
        match name {
            "this" => ref_matches.add_match(Arc::clone(
                self.symbol_info.object_symbols.get(&obj.key).unwrap())),
            _ => if let Some(res) = obj.get_object(name)
                .map(|o|o.resolve(&self.objects))
                .and_then(|r|self.resolved_to_symbol(obj, r)) {
                    ref_matches.add_match(Arc::clone(res))
                },
        }
    }

    fn lookup_def_in_resolved<'t, 'c>(&'c self,
                                      obj: DMLResolvedObject<'t, 'c>,
                                      name: &str,
                                      type_hint: Option<TypeHint>,
                                      ref_matches: &mut ReferenceMatches) {
        match obj {
            DMLResolvedObject::CompObject(o) =>
                self.lookup_def_in_comp_object(o, name, type_hint, ref_matches),
            DMLResolvedObject::ShallowObject(o) =>
                match &o.variant {
                    DMLShallowObjectVariant::Method(m) => {
                        for sym in m.get_decl().symbols().into_iter()
                            .filter(|struct_sym|
                                    struct_sym.get_name().as_str() == name)
                            .filter_map(|struct_sym|
                                        self.symbol_info.variable_symbols.get(
                                            struct_sym.loc_span())) {
                                ref_matches.add_match(Arc::clone(sym));
                            }
                    },
                    DMLShallowObjectVariant::Parameter(p) => {
                        if let Some(param) = p.get_unambiguous_def() {
                            // TODO: Remove this when we can resolve 'dev' param
                            // using constant folding
                            if param.name().val.as_str() == "dev" {
                                self.lookup_def_in_resolved(
                                    self.get_device_obj().resolve(
                                        &self.objects),
                                    name,
                                    type_hint,
                                    ref_matches);
                            } else {
                                // TODO: pre-evaluate params in objects that are
                                // noderefs, so it is simple to re-do the lookup
                                // here
                                #[allow(clippy::single_match)]
                                match param.value.as_ref() {
                                    Some(ParamValue::Set(expr)) => {
                                        let _: Vec<_> = self
                                            .expression_to_resolved_objects(
                                                expr, obj)
                                            .into_iter()
                                            .map(|res|
                                                 self.lookup_def_in_resolved(
                                                     res,
                                                     name,
                                                     type_hint.clone(),
                                                     ref_matches))
                                            .collect();
                                    },
                                    _ => (),
                                }
                            }
                        }
                    },
                    // TODO: Can we do _anything_ here? Perhaps defer to the
                    // default value (if any)?
                    DMLShallowObjectVariant::Session(_) |
                    DMLShallowObjectVariant::Saved(_) => (),
                    // Special case for hooks, 'send_now' is the only currently
                    // allowed member
                    DMLShallowObjectVariant::Hook(_) => if name == "send_now" {
                        ref_matches.add_match(
                            Arc::clone(self.symbol_info.variable_symbols
                                       .get(obj.location()).unwrap()));
                    },
                    _ =>
                    // NOTE: This is not a typeerror, but an internal error
                        error!("Internal error: Wanted to lookup symbol {} in \
                                {:?}, but that's not something that can \
                                contain symbols",
                               name, obj),
                }
        }
    }

    fn lookup_global_from_noderef(&self,
                                  node: &NodeRef,
                                  ref_matches: &mut ReferenceMatches) {

        if let NodeRef::Simple(name) = node {
            if let Some(templ) = self.templates.templates.get(
                name.val.as_str()) {
                if let Some(templ_loc) = &templ.location {
                    if let Some(templ_sym) = self.symbol_info
                        .template_symbols.get(templ_loc) {
                            ref_matches.add_match(Arc::clone(templ_sym));
                        } else {
                            error!("Unexpectedly missing a template {}",
                                   name.val);
                        }
                }
            }
        }
        // TODO: types
        // TODO: externs
    }

    fn lookup_global_from_ref(&self,
                              reference: &GlobalReference)
                              -> ReferenceMatches {
        let mut ref_matches = ReferenceMatches::new();
        match &reference.kind {
            ReferenceKind::Template => {
                if let Some(templ) = self.templates
                    .templates.get(&reference.name) {
                        // Dummy templates have no loc, do not actually exist
                        if let Some(templ_loc) = &templ.location {
                            if let Some(templ_sym) =
                                self.symbol_info.template_symbols
                                .get(templ_loc) {
                                    ref_matches.add_match(Arc::clone(templ_sym))
                                } else {
                                    error!("Unexpectedly missing a template {}",
                                           reference.name);
                                }
                        }
                    }
            },
            // TODO: type lookup
            ReferenceKind::Type => (),
            _ => error!("Invalid global reference kind in {:?}", reference),
        }
        ref_matches
    }

    fn lookup_global_symbol(&self,
                            sym: &SimpleSymbol,
                            ref_matches: &mut ReferenceMatches) {
        if DMLSymbolKind::Template == sym.kind  {
            if let Some(sym) = self.symbol_info.template_symbols.get(&sym.loc) {
                ref_matches.add_match(Arc::clone(sym));
            } else {
                // I dont think this can happen, so show an error
                error!("Unexpectedly missing a template {}", sym.name);
            }
        }
    }

    fn lookup_def_in_obj(&self,
                         obj: &DMLObject,
                         sym: &SimpleSymbol,
                         ref_matches: &mut ReferenceMatches) {
        let resolved = obj.resolve(&self.objects);
        if resolved.as_comp().map_or(false, |c|c.kind == CompObjectKind::Device)
            && sym.kind == DMLSymbolKind::Template {
                self.lookup_global_symbol(sym, ref_matches);
            } else {
                self.lookup_def_in_resolved(resolved,
                                            sym.name_ref(),
                                            None,
                                            ref_matches);
            }
    }

    fn lookup_global_sym(&self, sym: &SimpleSymbol) -> Vec<SymbolRef> {
        //TODO: being able to fail a re-match here feels silly. consider
        // adding sub-enum for DMLGlobalSymbolKind
        match sym.kind {
            DMLSymbolKind::Template => {
                if let Some((templ, _)) = self.templates.get_template(
                    sym.name.as_str()) {
                    // This _should_ be guaranteed, since the SimpleSymbol
                    // ref comes from structure
                    vec![Arc::clone(self.symbol_info.template_symbols.get(
                        templ.location.as_ref().unwrap()).unwrap())]
                } else {
                    error!("Unexpectedly missing template matching {:?}", sym);
                    vec![]
                }
            },
            // TODO: DMLType lookup
            DMLSymbolKind::Typedef => vec![],
            // TODO: Extern lookup
            DMLSymbolKind::Extern => vec![],
            e => {
                error!("Internal error: Unexpected symbol kind of global \
                        symbol: {:?}", e);
                vec![]
            },
        }
    }

    // TODO/NOTE: This method seems slightly misplaced, consider moving to semantic_lookup
    pub fn lookup_symbols_by_contexted_symbol<'t>(&self,
                                                  sym: &ContextedSymbol<'t>,
                                                  limitations: &mut HashSet<DLSLimitation>)
        -> Vec<SymbolRef> {
        if matches!(sym.symbol.kind, DMLSymbolKind::Template |
                    DMLSymbolKind::Typedef | DMLSymbolKind::Extern)
        {
            return self.lookup_global_sym(sym.symbol);
        }

        debug!("Looking up {:?} in device tree", sym);

        let mb_objs = if sym.contexts.is_empty() {
            Some(vec![self.get_device_obj().clone()])
        } else {
            self.context_to_objs(self.get_device_comp_obj(),
                                 sym.contexts.iter()
                                 .cloned().cloned()
                                 .collect::<Vec<ContextKey>>()
                                 .as_slice(),
                                 limitations)
        };

        if let Some(objs) = mb_objs {
            trace!("Found {:?}", objs);
            let mut refs = ReferenceMatches::new();
            let _: Vec<_> = objs.into_iter()
                .map(|o|self.lookup_def_in_obj(&o, sym.symbol, &mut refs))
                .collect();
            // We can ignore messages from lookup defs here, as this lookup is
            // live and reporting additional things from here makes no sense
            if let Some(matches) = refs.as_matches() {
                matches.into_iter().collect()
            } else {
                vec![]
            }
        } else {
            // TODO: Do we need to fall back on globals here? Can we get an
            // identifier from a spot that is generic enough to refer to a
            // global, but also is in a context where a global makes sense?
            internal_error!("Failed to find objects matching {:?}", sym);
            vec![]
        }
    }

    fn resolve_noderef_in_symbol<'t>(&'t self,
                                     symbol: &'t SymbolRef,
                                     node: &NodeRef,
                                     method_structure:
                                     &HashMap<ZeroSpan, RangeEntry>,
                                     ref_matches: &mut ReferenceMatches) {
        let sym = symbol.lock().unwrap();
        match &sym.source {
            SymbolSource::DMLObject(obj) => {
                // The performance overhead is cloning here
                // is _probably_ smaller than the one of holding the key
                let obj_copy = obj.clone();
                drop(sym);
                self.resolve_noderef_in_obj(&obj_copy,
                                            node,
                                            method_structure,
                                            ref_matches);
            },
            SymbolSource::Method(key, method) => {
                self.resolve_noderef_in_method(key, method, node, method_structure, ref_matches);
            },
            // TODO: Cannot be resolved without constant folding
            SymbolSource::MethodArg(_method, _name) => (),
            SymbolSource::MethodLocal(_method, _name) => (),
            // TODO: Fix once type system is sorted
            SymbolSource::Type(_typed) => (),
            // TODO: Handle lookups inside templates
            SymbolSource::Template(_templ) => (),
        }
    }

    fn get_method_symbol(&self,
                         method: &Arc<DMLMethodRef>,
                         parent_obj_key: &StructureKey)
                         -> Option<&SymbolRef> {
        let to_ret = self.symbol_info.method_symbols.get(method.location())
            .and_then(|m|m.get(parent_obj_key));
        if to_ret.is_none() {
            internal_error!("Missing method symbol for method ref {:?}", method);
        }
        to_ret
    }

    fn resolve_simple_noderef_in_method<'c>(&'c self,
                                            parent_key: &StructureKey,
                                            meth: &Arc<DMLMethodRef>,
                                            node: &DMLString,
                                            _type_hint: Option<()>,
                                            method_structure: &HashMap
                                            <ZeroSpan, RangeEntry>,
                                            ref_matches: &mut ReferenceMatches) {
        // When resolving a noderef from an overridden method, meth here will be
        // a bottom-most overriding method. So instead find the correct method
        // by recursing to parent
        if !meth.span().contains_pos(&node.span.start_position()) {
            match meth.get_default() {
                Some(DefaultCallReference::Valid(defmeth)) => {
                    if let Some(defmeth_sym) = self.get_method_symbol(defmeth,
                                                                     parent_key) {
                        self.resolve_noderef_in_symbol(
                            defmeth_sym,
                            &NodeRef::Simple(node.clone()),
                            method_structure,
                            ref_matches);
                    }
                },
                Some(DefaultCallReference::Ambiguous(defmeths)) => {
                    for defmeth in defmeths {
                        if let Some(defmeth_sym) = self.get_method_symbol(defmeth,
                                                                         parent_key) {
                            self.resolve_noderef_in_symbol(
                                defmeth_sym,
                                &NodeRef::Simple(node.clone()),
                                method_structure,
                                ref_matches);
                        }
                    }
                },
                _ => {
                    trace!("Fell through recursive method noderef resolution for {:?} in method {:?}", node, meth);
                }
            }
            return;
        }
        trace!("Resolving simple noderef {:?} in method {:?}", node, meth);
        match node.val.as_str() {
            "this" =>
                ref_matches.add_match(Arc::clone(
                    self.symbol_info.object_symbols.get(
                        parent_key).unwrap())),
            "default" =>
            // NOTE: Here we match a default ref to the symbol of the method decl
            // that we directly overrode. Meaning that further lookups on that symbol
            // will be specialized for that overriding chain
                if let Some(defref) = meth.get_default() {
                    match defref {
                        DefaultCallReference::Valid(refr) =>  {
                            if let Some(s) = self.get_method_symbol(refr, parent_key) {
                                ref_matches.add_match(Arc::clone(s));
                            }
                        },
                        DefaultCallReference::Ambiguous(refs) => {
                            // TODO/NOTE: These are currently unused, but since we want to mark a mismatch anyway may as well
                            // pass all these references in
                            for reference in refs {
                                if let Some(s) = self.get_method_symbol(reference, parent_key) {
                                   ref_matches.set_mismatched(Arc::clone(s));
                                }
                            }
                             let ambiguous_desc: &'static str 
                            = "Ambiguous default call, you may need to clarify the template ordering or use a template-qualified-method-implementation-call";
                            // TODO: curren
                            ref_matches.add_message(DMLError {
                                    span: *node.span(),
                                    description: ambiguous_desc.to_string(),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    related: refs
                                        .iter()
                                        .map(|d|(*d.location(),
                                            format!("Possible candidate for default call{}",
                                            d.get_disambiguation_name()
                                                .map_or_else(||"".to_string(),
                                                             |n|format!(", can be explicitly called as 'this.{}'", n)))
                                        ))
                                        .collect(),
                                });
                        }
                        _ => {
                            // 'default' when overriding nothing is an error, similar to the else-case below
                        }
                    }
                } else {
                    // fairly sure 'default' cannot be a
                    // reference otherwise
                    // TODO: better error message here, somehow?
                },
            _ => {
                let local_sym = method_structure.get(meth.location())
                    .and_then(|locals|locals.find_symbol_for_dmlname(node));
                let arg_sym = meth.args().iter()
                    .find(|a|a.name().val == node.val)
                    .map(|a|self.symbol_info.variable_symbols
                         .get(a.loc_span()).unwrap());
                match (local_sym, arg_sym) {
                    // NOTE: it's possible to get a local and an arg symbol,
                    // this is not an internal error and is reported as a
                    // conflict while building the symbols
                    (Some(sym), _) |
                    (_, Some(sym)) => ref_matches.add_match(Arc::clone(sym)),
                    _ => (),
                }
            },
        }
    }

    fn resolve_noderef_in_method<'c>(&'c self,
                                     parent_key: &StructureKey,
                                     method: &Arc<DMLMethodRef>,
                                     node: &NodeRef,
                                     method_structure: &HashMap<ZeroSpan,
                                                                RangeEntry>,
                                     ref_matches: &mut ReferenceMatches) {
        match node {
            NodeRef::Simple(simple) => {
                self.resolve_simple_noderef_in_method(parent_key, method, simple,
                    None,
                    method_structure,
                    ref_matches);
            },
            NodeRef::Sub(subnode, simple, _) => {
                let mut intermediate_matches = ReferenceMatches::new();
                self.resolve_noderef_in_method(parent_key,
                                               method,
                                               subnode,
                                               method_structure,
                                               &mut intermediate_matches);
                if let Some(syms) = intermediate_matches.as_matches() {
                    let wrapped_simple = NodeRef::Simple(simple.clone());
                    for sym in syms {
                        self.resolve_noderef_in_symbol(
                            &sym,
                            &wrapped_simple,
                            method_structure,
                            ref_matches);
                    }
                } else {
                    ref_matches.merge_with(intermediate_matches);
                }
            }
        }
    }

    fn resolve_noderef_in_obj<'c>(&'c self,
                                  obj: &DMLObject,
                                  node: &NodeRef,
                                  method_structure: &HashMap<ZeroSpan,
                                                             RangeEntry>,
                                  ref_matches: &mut ReferenceMatches) {
        trace!("Resolving noderef {:?} in obj {:?}", node, obj);
        match node {
            NodeRef::Simple(simple) => {
                let resolvedobj = obj.resolve(&self.objects);
                if let DMLResolvedObject::ShallowObject(
                    shallow @ DMLShallowObject {
                        variant: DMLShallowObjectVariant::Method(ref m),
                        ..
                    }) = resolvedobj {

                    self.resolve_simple_noderef_in_method(&shallow.parent,
                                                          m,
                                                          simple,
                                                          None,
                                                          method_structure,
                                                          ref_matches);
                } else {
                    self.lookup_def_in_resolved(resolvedobj,
                                                &simple.val,
                                                None,
                                                ref_matches);
                }
            },
            NodeRef::Sub(subnode, simple, _) => {
                let mut intermediate_matches = ReferenceMatches::new();
                self.resolve_noderef_in_obj(obj,
                                            subnode,
                                            method_structure,
                                            &mut intermediate_matches);
                
                if let Some(syms) = intermediate_matches.as_matches() {
                    let wrapped_simple = NodeRef::Simple(simple.clone());
                    for sym in syms {
                        self.resolve_noderef_in_symbol(
                            &sym,
                            &wrapped_simple,
                            method_structure,
                            ref_matches);
                    }
                } else {
                    ref_matches.merge_with(intermediate_matches);
                }
            }
        }
    }

    fn lookup_ref_in_obj(&self,
                         obj: &DMLObject,
                         reference: &VariableReference,
                         method_structure: &HashMap<ZeroSpan, RangeEntry>,
                         ref_matches: &mut ReferenceMatches) {
        match &reference.kind {
            ReferenceKind::Template |
            ReferenceKind::Type => {
                internal_error!("Attempted to do a contexted lookup \
                                 of a global reference {:?}", reference);
                return;
            },
            _ => (),
        }
        self.resolve_noderef_in_obj(obj,
                                    &reference.reference,
                                    method_structure,
                                    ref_matches);
    }

    pub fn lookup_symbols_by_contexted_reference(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        ref_matches: &mut ReferenceMatches) {
        debug!("Looking up {:?} : {:?} in device tree", context_chain,
               reference);
        // NOTE: This is actually unused, but contexts_to_objs is used
        // both from user- and server- context and thus needs this argument
        let mut limitations = HashSet::new();
        if let Some(objs) = self.contexts_to_objs(
            vec![self.get_device_obj().clone()],
            context_chain,
            &mut limitations) {
            for o in objs {
                self.lookup_ref_in_obj(&o,
                                       reference,
                                       method_structure,
                                       ref_matches);
            }
        } else {
            debug!("Failed to obtain obj from context {:?}", context_chain);
        }
    }

    fn find_target_for_reference(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>)
        -> ReferenceMatches {
        let mut recursive_cache = HashSet::default();
        self.find_target_for_reference_without_loop(context_chain,
                                                    reference,
                                                    method_structure,
                                                    reference_cache,
                                                    &mut recursive_cache)
    }

    fn find_target_for_reference_without_loop<'t>(
        &self,
        context_chain: &'t [ContextKey],
        reference: &'t VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>,
        recursive_cache: &'t mut HashSet<(&'t [ContextKey],
                                          &'t VariableReference)>)
        -> ReferenceMatches {
        // Prevent us from calling into the exact same reference lookup twice
        // within one lookup.
        if !recursive_cache.insert((context_chain, reference)) {
            internal_error!("Recursive reference lookup detected at \
                             {:?} under {:?}", reference, context_chain);
            return ReferenceMatches::new();
        }
        let index_key = (context_chain.to_vec(),
                         reference.clone());
        {
            if let Some(cached_result) = reference_cache.lock().unwrap()
                .get(index_key.clone(), method_structure) {
                    return cached_result.clone();
                }
        }
        let mut result = ReferenceMatches::new();
        self.find_target_for_reference_aux(context_chain,
                                           reference,
                                           method_structure,
                                           reference_cache,
                                           &mut result);
        reference_cache.lock().unwrap().insert(index_key,
                                               result.clone(),
                                               method_structure);
        result
    }

    fn find_target_for_reference_aux(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>,
        reference_matches: &mut ReferenceMatches) {
        if context_chain.is_empty() {
            // Nothing matches the noderef except maybe globals
            self.lookup_global_from_noderef(
                &reference.reference,
                reference_matches,
            );
            return;
        }

        self.lookup_symbols_by_contexted_reference(
            // Ignore first element of chain, it is the device context
            &context_chain[1..],
            reference,
            method_structure,
            reference_matches);
        if reference_matches.as_matches().is_none() {
            let (_, new_chain) = context_chain.split_last().unwrap();
            let sub_matches = self.find_target_for_reference(new_chain,
                                                             reference,
                                                             method_structure,
                                                             reference_cache);
            reference_matches.merge_with(sub_matches);
        }
    }

    fn handle_symbol_ref(symbol: &SymbolRef,
                         reference: &Reference) {
        let mut sym = symbol.lock().unwrap();
        sym.references.insert(*reference.loc_span());
        if sym.kind == DMLSymbolKind::Template && reference.extra_info.was_instantiation {
            sym.implementations.insert(*reference.loc_span());
        }
    }

    #[allow(clippy::ptr_arg)]
    fn match_references_in_scope<'c>(
        &'c self,
        scope_chain: Vec<&'c dyn Scope>,
        report: &mut Vec<DMLError>,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>,
        status: &AliveStatus) {
        let current_scope = scope_chain.last().unwrap();
        let context_chain: Vec<ContextKey> = scope_chain
            .iter().map(|s|s.create_context()).collect();
        // NOTE: chunk number is arbitrarily picked that benches well
        report.extend(current_scope.defined_references().par_chunks(25).flat_map(|references|{
            status.assert_alive();
            let mut local_reports = vec![];
            for reference in references {
                trace!("In {:?}, Matching {:?}", context_chain, reference);
                let symbol_lookup = match &reference.variant {
                    ReferenceVariant::Variable(var) => self.find_target_for_reference(
                        context_chain.as_slice(),
                        var,
                        method_structure,
                        reference_cache),
                    ReferenceVariant::Global(glob) =>
                        self.lookup_global_from_ref(glob),
                };

                match symbol_lookup.kind {
                    ReferenceMatchKind::NotFound =>
                    // TODO: report suggestions?
                    // TODO: Uncomment reporting of errors here when
                    // semantics are strong enough that they are rare
                    // for correct devices
                    // report.lock().unwrap().push(DMLError {
                    //     span: reference.span().clone(),
                    //     description: format!("Unknown reference {}",
                    //                          reference.to_string()),
                    //     related: vec![],
                    // })
                        (),
                    // This maps symbols->references, this is later
                    // used to create the inverse map
                    // (not done here because of ownership issues)
                    ReferenceMatchKind::Found =>
                        for symbol in &symbol_lookup.references {
                            Self::handle_symbol_ref(symbol, reference);
                        },
                    ReferenceMatchKind::MismatchedFind =>
                    //TODO: report mismatch,
                        (),
                }
                local_reports.extend(symbol_lookup.messages);
            }
            local_reports.into_par_iter()
        }).collect::<Vec<_>>());
    }
}

pub fn parse_file(path: &Path, file: FileSpec<'_>)
                  -> Result<(parsing::structure::TopAst,
                             ProvisionalsManager,
                             Vec<DMLError>), Error>
{
    let content = &file.file.text;
    let lexer = TokenKind::lexer(content);
    let mut parser = FileParser::new(lexer);
    let mut parse_state = FileInfo::default();
    let ast = parsing::structure::parse_toplevel(
        &mut parser, &mut parse_state, file);
    let mut skipped_errors = parser.report_skips();
    let mut missing_errors = ast.report_missing();
    missing_errors.append(&mut skipped_errors);
    parsing::structure::post_parse_toplevel(
        &ast, file.file, &mut missing_errors);
    // TODO: sort errors
    // NOTE: I dont know how to do rust iterators
    let errors = missing_errors.into_iter()
        .map(|e|e.with_file(path)).collect();
    Ok((ast, parse_state.provisionals, errors))
}

fn collect_toplevel(path: &Path, tree: &parsing::structure::TopAst,
                    errors: &mut Vec<DMLError>,
                    file: FileSpec<'_>) -> TopLevel {
    let mut report = vec![];
    let toplevel = TopLevel::from_ast(tree, &mut report, file);
    for error in report {
        errors.push(error.with_file(path));
    }
    toplevel
}

pub fn deconstruct_import(import: &ObjectDecl<Import>) -> PathBuf {
    PathBuf::from(import.obj.imported_name())
}

impl fmt::Display for IsolatedAnalysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "IsolatedAnalysis {{")?;
        writeln!(f, "\tpath: {}", self.path.as_str())?;
        writeln!(f, "\timports: [\n{}]",
                 self.toplevel.spec.imports.iter().map(deconstruct_import)
                 .fold("".to_string(),
                       |mut s, path|{
                           write!(s, "\t\t{:?}\n, ", path)
                               .expect("write string error");
                            s
                       }))?;
        writeln!(f, "\ttoplevel: {}\n}}", self.toplevel)?;
        Ok(())
    }
}

type ResolvedImports = (HashSet<(CanonPath, Import)>,
                        HashSet<(PathBuf, Import)>);

impl IsolatedAnalysis {
    pub fn new(path: &CanonPath,
               clientpath: &PathBuf,
               file: TextFile,
               status: AliveStatus)
               -> Result<IsolatedAnalysis, Error> {
        trace!("local analysis: {} at {}", path.as_str(), path.as_str());
        status.assert_alive();
        let filespec = FileSpec {
            path, file: &file
        };
        let (mut ast, provisionals, mut errors) = parse_file(path, filespec)?;
        status.assert_alive();
        // Add invalid provisionals to errors
        for duped_provisional in &provisionals.duped_provisionals {
            errors.push(DMLError {
                span: duped_provisional.span,
                description: format!("Duplicate activation of provisional '{}'",
                                     duped_provisional.val),
                severity: Some(DiagnosticSeverity::ERROR),
                // TODO: Could report the original declaration here, but they
                // are very close in source so probably unneccessary
                related: vec![],
            });
        }
        for invalid_provisional in &provisionals.invalid_provisionals {
            errors.push(DMLError {
                span: invalid_provisional.span,
                description: format!("Invalid or unknown provisional '{}', \
                                      will be ignored",
                                     invalid_provisional.val),
                severity: Some(DiagnosticSeverity::ERROR),
                related: vec![],
            });
        }

        // Peek into the ast and read from the file to check if version is 1.4
        let non_14_version =
            if let Some(version) = ast.version.version.read_leaf(&file) {
                if version != "1.4" {
                    Some(ast.version.range())
                } else {
                    None
                }
            } else {
                None
            };

        if let Some(location) = non_14_version {
            errors = vec![LocalDMLError {
                range: location,
                description:
                "The language server only supports DML 1.4 files"
                    .to_string(),
            }.with_file(path.as_path())];
            ast = parsing::structure::TopAst {
                version: ast.version,
                device: None,
                provisionals : None,
                bitorder: None,
                declarations: vec![],
            };
            info!("Bailed on further analysis of {} due to it not being a \
                   DML 1.4 file", clientpath.display());
        }

        let toplevel = collect_toplevel(path, &ast,
                                        &mut errors, filespec);
        status.assert_alive();
        // sanity, clientpath and path should be the same file
        if CanonPath::from_path_buf(clientpath.clone()).map_or(
            true, |cp|&cp != path) {
            error!("Clientpath did not describe the same \
                    file as the actual path; {:?} vs {:?}",
                   path,
                   clientpath);
            return Err(Error::InternalError(
                "Clientpath did not describe the same file as path"));
        }
        let top_context = toplevel.to_context();
        let res = IsolatedAnalysis {
            ast,
            toplevel,
            top_context,
            path: path.clone(),
            clientpath: clientpath.clone(),
            errors,
        };
        status.assert_alive();
        info!("Produced an isolated analysis of {:?}", res.path);
        debug!("Produced an isolated analysis: {}", res);
        Ok(res)
    }

    pub fn get_imports(&self) -> &Vec<ObjectDecl<Import>> {
        &self.toplevel.spec.imports
    }

    pub fn get_import_names(&self) -> Vec<PathBuf> {
        self.get_imports().iter().map(
            |imp|deconstruct_import(imp)).collect()
    }

    pub fn resolve_imports(&self,
                           resolver: &PathResolver,
                           context: Option<&CanonPath>)
                           -> ResolvedImports
    {
        let mut found = HashSet::default();
        let mut missing = HashSet::default();
        let import_paths = self.get_imports().iter()
            .map(|i|(deconstruct_import(i),
                     i.clone()));
        // Patch in implicit dependencies here. These won't affect template
        // or file ordering. But we DO want to make sure they are imported
        let import_paths = import_paths.chain(
            IMPLICIT_IMPORTS.iter().map(
                |import|(import.into(),
                         ObjectDecl::always(&Import {
                             span: ZeroSpan::invalid(self.path.clone()),
                             name: DMLString {
                                 val: format!("\"{}\"", import),
                                 span: ZeroSpan::invalid(self.path.clone()),
                             }
                         }))));

        for (path, import) in import_paths {
            if let Some(found_path) = resolver.resolve_with_maybe_context(
                &path, context, Some(&self.path)) {
                found.insert((found_path, import.obj));
            } else {
                missing.insert((path, import.obj));
            }
        }
        (found, missing)
    }

    pub fn is_device_file(&self) -> bool {
        self.toplevel.device.is_some()
    }

    pub fn lookup_context_symbol<'t>(&'t self, pos: &ZeroFilePosition)
                                     -> Option<ContextedSymbol<'t>> {
        self.top_context.lookup_symbol(pos)
    }

    pub fn lookup_reference(&self, pos: &ZeroFilePosition)
                            -> Option<&Reference> {
        self.toplevel.reference_at_pos(pos)
    }

    pub fn lookup_first_context(&self, pos: &ZeroFilePosition)
                                -> Option<ContextKey> {
        self.toplevel.defined_scopes().iter()
            .find(|scope|scope.span().contains_pos(pos))
            .map(|scope|scope.create_context())
    }
}

fn objects_to_symbols(maker: &SymbolMaker,
                      objects: &StructureContainer,
                      errors: &mut Vec<DMLError>,
                      method_structure: &mut HashMap
                      <ZeroSpan, RangeEntry>) -> SymbolStorage {
    let mut storage = SymbolStorage::default();

    for obj in objects.values() {
        let new_symbol: SymbolRef = new_symbol_from_object(maker, obj);
        debug!("Created comp obj symbol: {:?}", new_symbol);
        storage.object_symbols.insert(obj.key, new_symbol);
        for subobj in obj.components.values() {
            // Non-shallow objects will be handled by the iteration
            // over objects
            if let DMLObject::ShallowObject(shallow) = subobj {
                add_new_symbol_from_shallow(maker,
                                            shallow,
                                            errors,
                                            &mut storage,
                                            method_structure);
            }
        }
    }
    storage
}

fn template_to_symbol(maker: &SymbolMaker,
                      template: &Arc<DMLTemplate>) -> Option<SymbolRef> {
    // Do not create symbols for templates without location, they are dummy
    // missing templates
    template.location.as_ref().map(|location|symbol_ref!(
        maker,
        *location,
        DMLSymbolKind::Template,
        SymbolSource::Template(Arc::clone(template)),
        definitions = vec![*location],
        declarations = vec![*location]
    ))
}

fn extend_with_templates(maker: &SymbolMaker,
                         storage: &mut SymbolStorage,
                         templates: &TemplateTraitInfo) {
    for template in templates.templates.values() {
        if let Some(new_templ) = template_to_symbol(maker, template) {
            let loc = new_templ.lock().unwrap().loc;
            if let Some(prev) = storage.template_symbols
                .insert(loc, new_templ) {
                    internal_error!("Unexpectedly two template symbols
                        defined in the same location");
                    error!("Previous was {:?}", prev);
                    error!("New is {:?}", storage.template_symbols.get(&loc));
                }
        }
    }
}

fn new_symbol_from_object(maker: &SymbolMaker,
                          object: &DMLCompositeObject) -> SymbolRef {
    let all_decl_defs: Vec<ZeroSpan> = object.all_decls.iter().map(
        |spec|*spec.loc_span()).collect();
    symbol_ref!(
        maker,
        object.declloc,
        DMLSymbolKind::CompObject(object.kind),
        SymbolSource::DMLObject(DMLObject::CompObject(object.key)),
        definitions = all_decl_defs.clone(),
        declarations = all_decl_defs.clone(),
        implementations = object.used_ineach_locs.clone().into_iter().collect(),
        // TODO: this does not follow from the new definition of bases
        bases = all_decl_defs)
}

fn new_symbol_from_arg(maker: &SymbolMaker,
                       methref: &Arc<DMLMethodRef>,
                       arg: &DMLMethodArg) -> SymbolRef {
    let bases = vec![*arg.loc_span()];
    let definitions = vec![*arg.loc_span()];
    let declarations = vec![*arg.loc_span()];
    symbol_ref!(
        maker,
        *arg.loc_span(),
        DMLSymbolKind::MethodArg,
        SymbolSource::MethodArg(Arc::clone(methref), arg.name().clone()),
        bases = bases,
        definitions = definitions,
        declarations = declarations
    )
}

fn log_non_same_insert<K>(map: &mut HashMap<K, SymbolRef>,
                          key: K,
                          val: SymbolRef) -> bool
where K: std::hash::Hash + Eq + Clone + std::fmt::Debug,
{
    // NOTE: We should not need to do these comparisons, when
    // object symbol creation is properly guided by structural AST
    // this code can be simplified and the comparison discarded

    // NOTE: Insert first rather than checking for key is faster, I think
    if let Some(old) = map.insert(key.clone(), val) {
        // NOTE: the equivalent operation is a slight-better-than-eq
        // comparison, skipping the comparison of structure keys and
        // some meta-info
        if !old.lock().unwrap().equivalent(
            &map.get(&key).unwrap().lock().unwrap()) {
            internal_error!(
                "Overwrote previous symbol {:?} at {:?} with non-similar symbol {:?}",
                old, key, map.get(&key));
            return true;
        }
    }
    false
}

// The current strategy for method symbols is:
// Create a symbol for each level of overriding for each object where the method
// is actualized. We then end up with many symbols for the same decl location,
// and leave it to requests to collect the aggregate information at the point
fn add_new_symbol_from_method(maker: &SymbolMaker, parent_obj_key: &StructureKey, method_ref: &Arc<DMLMethodRef>, errors: &mut Vec<DMLError>, storage: &mut SymbolStorage, method_structure: &mut HashMap<ZeroSpan, RangeEntry>) {
    let (bases, definitions, declarations) = (
        method_ref.get_bases().iter().map(|b|*b.location()).collect(),
        vec![*method_ref.get_decl().location()],
        vec![*method_ref.get_decl().location()],
    );
    debug!("Made symbol for method {:?}", method_ref);
    let new_sym = symbol_ref!(
        maker,
        *method_ref.location(),
        DMLSymbolKind::Method,
        SymbolSource::Method(*parent_obj_key, Arc::clone(method_ref)),
        bases = bases,
        definitions = definitions,
        declarations = declarations);
    let insert_at = storage.method_symbols.entry(*method_ref.location())
        .or_default();
    if !log_non_same_insert(insert_at, *parent_obj_key, new_sym) {
        for arg in method_ref.args() {
            let new_argsymbol = new_symbol_from_arg(maker, method_ref, arg);
            log_non_same_insert(&mut storage.variable_symbols, *arg.loc_span(), new_argsymbol);
        }
        add_method_scope_symbols(maker, method_ref, method_structure, storage, errors);
        if let Some(defaults) = method_ref.get_default() {
            for default in defaults.flat_refs() {
                add_new_symbol_from_method(maker, parent_obj_key, default, errors, storage, method_structure);
            }
        }
    }   
}

#[allow(clippy::ptr_arg)]
fn add_new_symbol_from_shallow(maker: &SymbolMaker,
                               shallow: &DMLShallowObject,
                               errors: &mut Vec<DMLError>,
                               storage: &mut SymbolStorage,
                               method_structure: &mut HashMap
                               <ZeroSpan, RangeEntry>) {
    let (bases, definitions, declarations) = match &shallow.variant {
        DMLShallowObjectVariant::Parameter(param) =>
            (vec![*param.get_last_declaration().loc_span()],
             param.used_definitions.iter()
                .map(|(_, def)|*def.loc_span()).collect(),
             param.declarations.iter()
                .map(|(_, def)|*def.loc_span()).collect()),
        DMLShallowObjectVariant::Method(method_ref) =>
            return add_new_symbol_from_method(maker, &shallow.parent, method_ref, errors, storage, method_structure),
        DMLShallowObjectVariant::Constant(constant) =>
            (vec![*constant.loc_span()],
             vec![*constant.loc_span()],
             vec![*constant.loc_span()]),
        DMLShallowObjectVariant::Session(data) |
        DMLShallowObjectVariant::Saved(data) =>
            (vec![*data.declaration.loc_span()],
             vec![*data.declaration.loc_span()],
             vec![*data.declaration.loc_span()]),
        DMLShallowObjectVariant::Hook(hook) =>
            (vec![*hook.loc_span()],
             vec![*hook.loc_span()],
             vec![*hook.loc_span()]),
    };
    
    let new_sym = symbol_ref!(
        maker,
        *shallow.location(),
        shallow.kind(),
        SymbolSource::DMLObject(
            // TODO: Inefficient clone. Not terribly so, but worth
            // noting
            DMLObject::ShallowObject(shallow.clone())),
        bases = bases,
        definitions = definitions,
        declarations = declarations);
    debug!("Made shallow symbol {:?}", new_sym);
    match &shallow.variant {
        DMLShallowObjectVariant::Parameter(_) => {
            log_non_same_insert(storage.param_symbols.entry(
                (*shallow.location(),
                 shallow.identity().to_string()))
                                .or_default(),
                                shallow.parent,
                                new_sym);
        },
        DMLShallowObjectVariant::Method(method_ref) => {
           internal_error!("Unreachable method_ref case reached, ignored. ({:?})", method_ref);
        },
        DMLShallowObjectVariant::Constant(_) |
        DMLShallowObjectVariant::Session(_) |
        DMLShallowObjectVariant::Saved(_) |
        DMLShallowObjectVariant::Hook(_) => {
            log_non_same_insert(&mut storage.variable_symbols,
                                *shallow.location(),
                                new_sym);
        },
    }
}

fn add_method_scope_symbols(maker: &SymbolMaker,
                            method: &Arc<DMLMethodRef>,
                            method_structure: &mut HashMap<ZeroSpan,
                                                           RangeEntry>,
                            storage: &mut SymbolStorage,
                            errors: &mut Vec<DMLError>) {
    let mut entry = RangeEntry {
        range: method.get_decl().span().range,
        symbols: HashMap::default(),
        sub_ranges: vec![],
    };
    if matches!(&*method.get_decl().body, StatementKind::Compound(_)) {
        add_new_method_scope_symbols(maker,
                                     method,
                                     &method.get_decl().body,
                                     errors,
                                     storage,
                                     &mut entry);
    }
    if !entry.is_empty() {
        method_structure.insert(
            *method.get_decl().location(),
            entry);
    }
}

fn add_new_method_scope_symbol<T>(maker: &SymbolMaker,
                                  method: &Arc<DMLMethodRef>,
                                  sym: &T,
                                  _typ: &DMLType,
                                  storage: &mut SymbolStorage,
                                  scope: &mut RangeEntry)
where
    T : StructureSymbol + DMLNamed + LocationSpan
{
    let symbol = symbol_ref!(
        maker,
        *sym.loc_span(),
        sym.kind(),
        SymbolSource::MethodLocal(Arc::clone(method), sym.name().clone()),
        definitions = vec![*sym.loc_span()],
        declarations = vec![*sym.loc_span()]
        // TODO: resolve type
    );
    scope.symbols.insert(sym.name().val.clone(), Arc::clone(&symbol));
    storage.variable_symbols.insert(*sym.loc_span(), symbol);
}

fn enter_new_method_scope(maker: &SymbolMaker,
                          method: &Arc<DMLMethodRef>,
                          outer_stmnt_desc: &'static str,
                          stmnt: &Statement,
                          scope_span: &ZeroSpan,
                          errors: &mut Vec<DMLError>,
                          storage: &mut SymbolStorage,
                          scope: &mut RangeEntry) {
    // In DMLC, there is no error or warning about this (even if a declaration
    // is referred to outside the scope of the containing statement), instead
    // relying on the C compilation failing
    // We will instead warn in general, and actually always create the scope
    // so that lookups will fail
    if let StatementKind::VariableDecl(content) = stmnt.as_ref() {
        errors.push(DMLError {
            span: *content.span(),
            description: format!("Declaration will not be available \
                                  outside {} scope", outer_stmnt_desc),
            severity: Some(DiagnosticSeverity::WARNING),
            related: vec![],
        });
    }
    let mut entry = RangeEntry {
        range: scope_span.range,
        symbols: HashMap::default(),
        sub_ranges: vec![],
    };
    add_new_method_scope_symbols(maker,
                                 method,
                                 stmnt,
                                 errors,
                                 storage,
                                 &mut entry);
    scope.sub_ranges.push(entry);
}

fn add_new_method_scope_symbols(maker: &SymbolMaker,
                                method: &Arc<DMLMethodRef>,
                                stmnt: &Statement,
                                errors: &mut Vec<DMLError>,
                                storage: &mut SymbolStorage,
                                scope: &mut RangeEntry) {
    match &**stmnt {
        StatementKind::Compound(content) =>
            for sub_stmnt in &content.statements {
                add_new_method_scope_symbols(maker,
                                             method,
                                             sub_stmnt,
                                             errors,
                                             storage,
                                             scope);
            },
        StatementKind::If(content) => {
            enter_new_method_scope(maker,
                                   method,
                                   "if",
                                   &content.ifbody,
                                   content.ifbody.span(),
                                   errors,
                                   storage,
                                   scope);
            if let Some(elsebody) = &content.elsebody {
                enter_new_method_scope(maker,
                                       method,
                                       "else",
                                       elsebody,
                                       elsebody.span(),
                                       errors,
                                       storage,
                                       scope);
            }
        },
        StatementKind::HashIf(content) => {
            // TODO: this should be constant-folded if possible
            enter_new_method_scope(maker,
                                   method,
                                   "#if",
                                   &content.ifbody,
                                   content.ifbody.span(),
                                   errors,
                                   storage,
                                   scope);
            if let Some(elsebody) = &content.elsebody {
                enter_new_method_scope(maker,
                                       method,
                                       "#else",
                                       elsebody,
                                       elsebody.span(),
                                       errors,
                                       storage,
                                       scope);
            }
        },
        StatementKind::While(content) =>
            enter_new_method_scope(maker,
                                   method,
                                   "while",
                                   &content.body,
                                   content.body.span(),
                                   errors,
                                   storage,
                                   scope),
        StatementKind::DoWhile(content) =>
            enter_new_method_scope(maker,
                                   method,
                                   "do-while",
                                   &content.body,
                                   content.body.span(),
                                   errors,
                                   storage,
                                   scope),
        StatementKind::For(content) => {
            let mut entry = RangeEntry {
                range: content.span().range,
                symbols: HashMap::default(),
                sub_ranges: vec![],
            };

            if let Some(ForPre::Declaration(variable)) = &content.pre {
                for decl in &variable.vars {
                    add_new_method_scope_symbol(maker,
                                                method,
                                                decl,
                                                &decl.typed,
                                                storage,
                                                &mut entry);
                }
            }
            enter_new_method_scope(maker,
                                   method,
                                   "for",
                                   &content.body,
                                   content.body.span(),
                                   errors,
                                   storage,
                                   &mut entry);
            scope.sub_ranges.push(entry);
        },
        StatementKind::HashSelect(content) => {
            let mut entry = RangeEntry {
                range: ZeroRange::combine(
                    content.selectbranch.span().range,
                    content.whereexpr.span().range),
                symbols: HashMap::default(),
                sub_ranges: vec![],
            };
            add_new_method_scope_symbol(maker,
                                        method,
                                        &content.ident,
                                        // TODO: infer type
                                        content.ident.loc_span(),
                                        storage,
                                        &mut entry);
            enter_new_method_scope(maker,
                                   method,
                                   "select",
                                   &content.selectbranch,
                                   content.selectbranch.span(),
                                   errors,
                                   storage,
                                   &mut entry);
            scope.sub_ranges.push(entry);
            enter_new_method_scope(maker,
                                   method,
                                   "select-else",
                                   &content.elsebranch,
                                   content.elsebranch.span(),
                                   errors,
                                   storage,
                                   scope);
        },
        StatementKind::TryCatch(content) => {
            enter_new_method_scope(maker,
                                   method,
                                   "try",
                                   &content.tryblock,
                                   content.tryblock.span(),
                                   errors,
                                   storage,
                                   scope);
            enter_new_method_scope(maker,
                                   method,
                                   "catch",
                                   &content.catchblock,
                                   content.catchblock.span(),
                                   errors,
                                   storage,
                                   scope);
        },
        StatementKind::VariableDecl(content) =>
            for decl in &content.vars {
                add_new_method_scope_symbol(
                    maker,
                    method,
                    decl,
                    &decl.typed,
                    storage,
                    scope);
            },
        _ => (),
    }
}


impl DeviceAnalysis {
    fn make_templates_traits(start_of_file: &ZeroSpan,
                             rank_maker: &mut RankMaker,
                             unique_templates: &HashMap<
                                     &str, &ObjectDecl<Template>>,
                             files: &HashMap<&str, &TopLevel>,
                             imp_map: &HashMap<Import, String>,
                             errors: &mut Vec<DMLError>)
                             -> TemplateTraitInfo {
        info!("Rank templates");
        let (templates, order, invalid_isimps, rank_struct)
            = rank_templates(unique_templates, files, imp_map, errors);
        info!("Templates+traits");
        create_templates_traits(
            start_of_file,
            rank_maker, templates, order,
            invalid_isimps, imp_map, rank_struct, errors)
    }

    fn match_references(&mut self,
                        bases: &Vec<IsolatedAnalysis>,
                        method_structure: &HashMap<ZeroSpan, RangeEntry>,
                        errors: &mut Vec<DMLError>,
                        status: &AliveStatus) {
        info!("Match references");
        let reference_cache: Mutex<ReferenceCache> = Mutex::default();
        for scope_chain in all_scopes(bases) {
            debug!("Got scope at {:?}", scope_chain.last()
                   .map(|s|s.span().start_position()));
            self.match_references_in_scope(scope_chain,
                                           errors,
                                           method_structure,
                                           &reference_cache,
                                           status);
        }
    }

    fn template_object_map(tt_info: &TemplateTraitInfo,
                           container: &StructureContainer)
                           -> HashMap<ZeroSpan, Vec<StructureKey>>{
        // Tie objects to their implemented templates, and vice-versa
        // NOTE: This cannot be inside DMLTemplates, because due to RC
        // references they are immutable once created
        trace!("template->object map");
        // maps template declaration loc to objects
        let mut template_object_implementation_map: HashMap<ZeroSpan,
                                                            Vec<StructureKey>>
            = HashMap::new();
        for template in tt_info.templates.values() {
            if let Some(loc) = template.location.as_ref() {
                template_object_implementation_map.insert(*loc, vec![]);
            }
        }
        for object in container.values() {
            for template in object.templates.values() {
                if let Some(loc) = template.location.as_ref() {
                    template_object_implementation_map
                        .entry(*loc)
                        .or_default().push(object.key);
                }
            }
        }
        template_object_implementation_map
    }

    pub fn new(root: IsolatedAnalysis,
               timed_bases: Vec<TimestampedStorage<IsolatedAnalysis>>,
               imp_map: HashMap<Import, String>,
               status: AliveStatus)
               -> Result<DeviceAnalysis, Error> {
        info!("device analysis: {:?}", root.path);
        status.assert_alive();

        if root.toplevel.device.is_none() {
            return Err(Error::InternalError(
                "Attempted to device analyze a file without device decl"));
        }

        let mut bases: Vec<_> =
            timed_bases.into_iter().map(|tss|tss.stored).collect();

        // Fake the implicit imports into the root toplevel
        // We dro this into bases, because that is the analysises that are
        // used in analysis
        for base in &mut bases {
            if base.path == root.path {
                for import in IMPLICIT_IMPORTS {
                    base.toplevel.spec.imports.push(ObjectDecl::always(&Import {
                        span: ZeroSpan::invalid(root.path.clone()),
                        name: DMLString {
                            val: format!("\"{}\"", import),
                            span: ZeroSpan::invalid(root.path.clone()),
                        }
                    }));
                }
            }
        }

        let base_templates = from_device_and_bases(&root, &bases);

        trace!("base template names: {:?}", base_templates.iter()
               .fold("".to_string(),
                     |mut s, odecl|{
                         write!(s, "{}, ", odecl.obj.object.name.val)
                             .expect("write string error");
                         s
                     }));
        status.assert_alive();
        let mut errors = vec![];

        // Remove duplicate templates
        let mut unique_templates: HashMap<&str, &ObjectDecl<Template>>
            = HashMap::default();
        for template in &base_templates {
            // TODO: Currently duplicate templates are discarded from
            // further analysis completely. We should have a better
            // strategy (likely mapping name->templates and make
            // things instantiating one of them instantiate both
            let name = &template.obj.object.name.val;
            if unique_templates.contains_key(name.as_str()) {
                errors.push(DMLError {
                    span: template.obj.object.span,
                    description: format!("Duplicate template name; '{}'", name),
                    severity: Some(DiagnosticSeverity::ERROR),
                    related: vec![(
                        unique_templates.get(name.as_str()).unwrap()
                            .obj.object.span,
                        "Previously defined here".to_string()
                    )],
                });
            } else {
                unique_templates.insert(name.as_str(), template);
            }
        }

        // Add files-as-templates
        let mut files: HashMap<&str, &TopLevel> = HashMap::default();
        for base in &bases {
            files.insert(base.path.to_str().unwrap(),
                         &base.toplevel);
            trace!("Tracked file {} as template",
                   base.path.to_str().unwrap_or("no path"));
        }
        status.assert_alive();
        let mut rank_maker = RankMaker::new();
        let tt_info = Self::make_templates_traits(&root.toplevel.start_of_file,
                                                  &mut rank_maker,
                                                  &unique_templates,
                                                  &files,
                                                  &imp_map,
                                                  &mut errors);
        status.assert_alive();
        // TODO: catch typedef/traitname overlaps

        // TODO: this is where we would do type resolution
        let mut container = StructureContainer::default();
        info!("Make device");
        let device_key = make_device(root.path.as_str(), &root.toplevel,
                                     &tt_info, imp_map, &mut container,
                                     &mut rank_maker, &mut errors).key;
        status.assert_alive();
        // maps template declaration loc to objects
        let template_object_implementation_map =
            Self::template_object_map(&tt_info, &container);
        status.assert_alive();
        info!("Generate symbols");
        // Used to handle scoping in methods when looking up local symbols,
        // NOTE: if this meta-information becomes relevant later, move this
        // structure to symbol storage
        // The zerospan here is the span corresponding to the span of the
        // NAME of the methoddecl
        let mut method_structure: HashMap<ZeroSpan, RangeEntry>
            = HashMap::default();
        let maker = SymbolMaker::new();
        let mut symbol_info = objects_to_symbols(&maker,
                                                 &container,
                                                 &mut errors,
                                                 &mut method_structure);
        // This needs to be done after all symbols are created, because method
        // symbol order is not correlated to the object iteration order
        bind_method_implementations(&mut symbol_info.method_symbols);

        status.assert_alive();
        // TODO: how do we store type info?
        extend_with_templates(&maker, &mut symbol_info, &tt_info);
        //extend_with_types(&mut symbols, ??)
        let mut device = DeviceAnalysis {
            name: root.toplevel.device.unwrap().name.val,
            errors: HashMap::default(),
            objects: container,
            device_obj: DMLObject::CompObject(device_key),
            templates: tt_info,
            symbol_info,
            reference_info: ReferenceStorage::default(),
            template_object_implementation_map,
            path: root.path.clone(),
            clientpath: root.path.clone().into(),
            dependant_files: bases.iter().map(|b|&b.path).cloned().collect(),
        };
        status.assert_alive();
        device.match_references(&bases,
                                &method_structure,
                                &mut errors,
                                &status);

        // NOTE: This is when we previously pre-calculated the ref->symbol map,
        // however the up-front analysis cost of this was too heavy
        // device.inverse_references();

        info!("Invariant check");
        for obj in device.objects.values() {
            device.param_invariants(obj, &mut errors);
        }

        for error in errors {
            device.errors.entry(error.span.path())
                .or_default()
                .push(error);
        }
        trace!("Errors are {:?}", device.errors);
        status.assert_alive();
        info!("Done with device");
        Ok(device)
    }

    #[allow(clippy::ptr_arg)]
    fn param_invariants(&self,
                        obj: &DMLCompositeObject,
                        _report: &mut Vec<DMLError>) {
        #[allow(clippy::single_match)]
        match &obj.kind {
            // TODO: Check 'name' parameter towards 'ident' parameter
            CompObjectKind::Register => {
                // TODO: Cannot be sufficiently checked until
                // constant-folding is in
                // NOTE: 'offset' is checked by the requirement of
                // the register template. Might be better to give
                // a nicer error here
                // match self.lookup_def_in_comp_object(
                //     obj, "size", None) {
                //     // TODO: verify size is an integer
                //     ReferenceMatch::Found(_) => (),
                //     _ => {
                //         report.push(DMLError {
                //             span: obj.declloc,
                //             description: "No assignment to 'size' parameter \
                //                           in register".to_string(),
                //             related: vec![],
                //             severity: Some(DiagnosticSeverity::ERROR),
                //         });
                //     },
                // }
            },
            _ => (),
        }
    }

    pub fn symbols_of_ref(&self, loc: ZeroSpan) -> Vec<SymbolRef> {
        // Would like to use .entry here, but it does not play nice with the
        // mutable borrow of .reference_info and .symbol_info
        let mut locked_info = self.reference_info.lock().unwrap();
        if let Some(syms) = locked_info.get(&loc) {
            syms.clone()
        } else {
            let mut syms = vec![];
            for sym in self.symbol_info.all_symbols() {
                if sym.lock().unwrap().references.contains(&loc) {
                    syms.push(Arc::clone(sym));
                }
            }
            locked_info.insert(loc, syms);
            locked_info.get(&loc).unwrap().clone()
        }
    }
}

pub fn from_device_and_bases<'a>(_device: &'a IsolatedAnalysis,
                                 bases: &'a [IsolatedAnalysis])
                                 -> Vec<&'a ObjectDecl<Template>> {
    // Need to be sure we get the device toplevel from device here, since
    // it has been modified a little from the original isolatedanalysis
    let toplevels: Vec<&'a TopLevel> =
        bases.iter().map(|ia|&ia.toplevel).collect();
    toplevels.iter().flat_map(
        |tl|&tl.templates).collect()
}

fn bind_method_implementations(method_symbols: &mut HashMap<ZeroSpan, HashMap<StructureKey, SymbolRef>>) {
    debug!("Bind method implementations");
    for (parent, method_symbol) in method_symbols.values().flat_map(|m|m.iter()) {
        debug!("Binding for method {} under {:?}", method_symbol.lock().unwrap().medium_debug_info(), parent);

        // Cloning the arc is not strictly necessary, but avoiding holding the lock is good practice
        let method = match &method_symbol.lock().unwrap().source {
            SymbolSource::Method(_, methref) => Arc::clone(methref),
            _ => {
                internal_error!("Method symbol {:?} did not have method symbol source", method_symbol);
                continue;
            }
        };
        let default_decls = method.get_default().into_iter()
            .flat_map(|d|d.flat_refs().into_iter().cloned().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        debug!("Default decls are {:?}", default_decls);
        let overridden_methods = default_decls.into_iter()
            .filter_map(|d|
                if let Some(parent_syms) = method_symbols.get(d.location()) {
                    Some(parent_syms)
                } else {
                    internal_error!("Method symbol {:?} did not have method symbol source", d);
                    None
                })
            .filter_map(|ps|ps.get(parent));
        for overridden_method in overridden_methods {
            debug!("Inserted into parent {}", overridden_method.lock().unwrap().medium_debug_info());
            if method.is_abstract() {
                internal_error!("Unexpectedly bound abstract method {:?} as implementation of {:?}",
                method, overridden_method);
            } else {
                overridden_method.lock().unwrap().implementations.insert(*method.location());
            }
        }
    }
}

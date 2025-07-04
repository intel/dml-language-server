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

use itertools::Itertools;

use lsp_types::{DiagnosticSeverity};
use logos::Logos;
use log::{debug, error, info, trace};
use rayon::prelude::*;

use crate::actions::SourcedDMLError;
use crate::actions::analysis_storage::TimestampedStorage;
use crate::analysis::symbols::{SimpleSymbol, DMLSymbolKind, Symbol,
                               SymbolSource};
use crate::analysis::reference::{Reference,
                                 GlobalReference, VariableReference,
                                 ReferenceKind, NodeRef};
use crate::analysis::scope::{Scope, SymbolContext,
                             ContextKey, ContextedSymbol};
use crate::analysis::parsing::parser::{FileInfo, FileParser};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::provisionals::ProvisionalsManager;

pub use crate::analysis::parsing::tree::
{ZeroRange, ZeroSpan, ZeroPosition, ZeroFilePosition};

use crate::analysis::parsing::tree::{MissingToken, MissingContent, TreeElement};
use crate::analysis::structure::objects::{Template, Import, CompObjectKind,
                                          ParamValue};
use crate::analysis::structure::toplevel::{ObjectDecl, TopLevel};
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
use crate::analysis::templating::methods::{MethodDeclaration, DMLMethodRef,
                                           DMLMethodArg};
use crate::analysis::templating::traits::{DMLTemplate,
                                          TemplateTraitInfo};
use crate::analysis::templating::types::DMLResolvedType;

use crate::file_management::{PathResolver, CanonPath};

use crate::vfs::{TextFile, Error};

#[derive(Clone, Copy)]
pub struct FileSpec<'a> {
    pub path: &'a Path,
    pub file: &'a TextFile,
}

pub const IMPLICIT_IMPORTS: [&str; 2] = ["dml-builtins.dml",
                                         "simics/device-api.dml"];
// The issue number is used as the _unique identifier_ for this
// limitation, and should be the number of an open GITHUB
// issue.
#[allow(dead_code)]
const EXAMPLE: DLSLimitation =
    DLSLimitation {
        issue_num: 42,
        description: "Example of a DLS limitation",
    };

#[derive(Clone, Debug, Eq, PartialEq, Hash,)]
pub struct DLSLimitation {
    pub issue_num: u64,
    pub description: &'static str,
}

impl fmt::Display for DLSLimitation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (issue#{})", self.description, self.issue_num)
    }
}

fn collapse_referencematches<T>(matches: T) -> ReferenceMatch
where T : IntoIterator<Item = ReferenceMatch> {
    matches.into_iter().fold(
        ReferenceMatch::NotFound(vec![]),
        |acc, rf|match (acc, rf) {
            (ReferenceMatch::Found(mut f1), ReferenceMatch::Found(mut f2)) => {
                f1.append(&mut f2);
                ReferenceMatch::Found(f1)
            },
            (ReferenceMatch::NotFound(mut f1),
             ReferenceMatch::NotFound(mut f2)) => {
                f1.append(&mut f2);
                ReferenceMatch::NotFound(f1)
            },
            (f @ ReferenceMatch::Found(_), _) => f,
            (_, f @ ReferenceMatch::Found(_)) => f,
            (f @ ReferenceMatch::WrongType(_), _) => f,
            (_, f) => f,
        })
}

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

// Mas symbol decl locations to symbols,
pub type SymbolRef = Arc<Mutex<Symbol>>;
#[derive(Debug, Clone, Default)]
pub struct SymbolStorage {
    pub template_symbols: HashMap<ZeroSpan, SymbolRef>,
    // Because some implicit parameters are defined in the same
    // place, we need to disambiguate this by name
    pub param_symbols: HashMap<(ZeroSpan, String),
                               HashMap<StructureKey, SymbolRef>>,
    pub object_symbols: HashMap<StructureKey, SymbolRef>,
    pub method_symbols: HashMap<ZeroSpan, SymbolRef>,
    // constants, sessions, saveds, hooks, method args
    pub variable_symbols: HashMap<ZeroSpan, SymbolRef>,
}

// This maps non-auth symbol decls to auth decl
// and references to the symbol decl they ref
type ReferenceStorage = HashMap<ZeroSpan, Vec<SymbolRef>>;

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

#[derive(Debug, Clone)]
pub enum ReferenceMatch {
    Found(Vec<SymbolRef>),
    WrongType(SymbolRef),
    NotFound(Vec<SymbolRef>),
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

type ReferenceCacheKey = (Vec<AgnConKey>, AgnRef);
#[derive(Default)]
struct ReferenceCache {
    underlying_cache: HashMap<ReferenceCacheKey, ReferenceMatch>,
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
    fn convert_to_key(key: (Vec<ContextKey>, VariableReference))
                      -> ReferenceCacheKey {
        let (contexts, refr) = key;
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
        (agnostic_context, agnostic_reference)
    }

    pub fn get(&self, key: (Vec<ContextKey>,
                            VariableReference))
               -> Option<&ReferenceMatch> {
        let agn_key = Self::convert_to_key(key);
        self.underlying_cache.get(&agn_key)
    }

    pub fn insert(&mut self,
                  key: (Vec<ContextKey>,
                        VariableReference),
                  val: ReferenceMatch)
    {
        let agn_key = Self::convert_to_key(key);
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

pub const ISOLATED_TEMPLATE_LIMITATION: DLSLimitation = DLSLimitation {
    issue_num: 31,
    description: "References from, and definitions inside, templates cannot \
                  be evaluated without an instantiating object",
};

impl DeviceAnalysis {
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
                            error!("Internal Error: \
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
            error!("Internal Error: context chain invariant broken at {:?}",
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
                            error!("Internal Error: Context chain \
                                    suggested {:?} should be a method, \
                                    but it wasn't",
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
                            limitations.insert(ISOLATED_TEMPLATE_LIMITATION);
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
                        error!("Internal Error: No template->objects map for\
                                {:?}", sym);
                        vec![]
                    }
                } else {
                    error!("Internal Error: \
                            Wanted to find context {:?} in {:?} which is not \
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

    fn resolved_to_symbol<'t, 'c>(&'c self, obj: DMLResolvedObject<'t, 'c>)
                                  -> Option<Vec<&'c SymbolRef>> {
        match obj {
            DMLResolvedObject::CompObject(comp) =>
                self.symbol_info.object_symbols.get(&comp.key)
                .map(|r|vec![r]),
            DMLResolvedObject::ShallowObject(shallow) =>
                match &shallow.variant {
                    DMLShallowObjectVariant::Method(m) =>
                        self.symbol_info.method_symbols.get(m.location())
                        .map(|r|vec![r]),
                    DMLShallowObjectVariant::Session(s) |
                    DMLShallowObjectVariant::Saved(s) =>
                        self.symbol_info.variable_symbols.get(s.loc_span())
                        .map(|r|vec![r]),
                    DMLShallowObjectVariant::Constant(c) =>
                        self.symbol_info.variable_symbols.get(c.loc_span())
                        .map(|r|vec![r]),
                    DMLShallowObjectVariant::Hook(h) =>
                        self.symbol_info.variable_symbols.get(h.loc_span())
                        .map(|r|vec![r]),
                    DMLShallowObjectVariant::Parameter(p) =>
                        self.symbol_info.param_symbols.get(
                            &(*p.loc_span(),
                              p.name().val.to_string()))
                        .map(|m|m.values().collect()),
                },
        }
    }

    fn lookup_def_in_comp_object<'c>(&'c self,
                                     obj: &'c DMLCompositeObject,
                                     name: &str,
                                     _type_hint: Option<TypeHint>)
                                     -> ReferenceMatch {
        debug!("Looking up {} in {:?}", name, obj.identity());
        match name {
            "this" => ReferenceMatch::Found(
                vec![Arc::clone(self.symbol_info
                                .object_symbols.get(&obj.key).unwrap())]),
            _ => obj.get_object(name)
                .map(|o|o.resolve(&self.objects))
                .and_then(|r|self.resolved_to_symbol(r))
                .map_or(ReferenceMatch::NotFound(vec![]),
                        |res|ReferenceMatch::Found(res.into_iter()
                                                   .map(Arc::clone)
                                                   .collect()))
        }
    }

    fn lookup_def_in_resolved<'t, 'c>(&'c self,
                                      obj: DMLResolvedObject<'t, 'c>,
                                      name: &str,
                                      type_hint: Option<TypeHint>)
                                      -> ReferenceMatch {
        match obj {
            DMLResolvedObject::CompObject(o) =>
                self.lookup_def_in_comp_object(o, name, type_hint),
            DMLResolvedObject::ShallowObject(o) => match
                &o.variant {
                    DMLShallowObjectVariant::Method(m) =>
                        match name {
                            "this" =>
                                ReferenceMatch::Found(
                                    vec![Arc::clone(
                                        self.symbol_info.object_symbols.get(
                                            &o.parent).unwrap())]),
                            "default" =>
                            // NOTE: This is part of the hack that maps default
                            // references in methods to the corret method decl.
                            // Here, we simply check if the method has any
                            // default call, and if so map the reference to the
                            // method symbol
                                if m.get_default().is_some() {
                                    self.symbol_info.method_symbols
                                        .get(obj.location())
                                        .map_or_else(
                                            ||ReferenceMatch::NotFound(vec![]),
                                            |sym|ReferenceMatch::Found(vec![
                                                Arc::clone(sym)]))
                                } else {
                                    // fairly sure 'default' cannot be a
                                    // reference otherwise
                                    // TODO: better error message here, somehow?
                                    ReferenceMatch::NotFound(vec![])
                                },
                            _ => if let Some(sym) = m.args().iter()
                                .find(|a|a.name().val == name)
                                .map(|a|self.symbol_info.variable_symbols
                                     .get(a.loc_span()).unwrap())
                            {
                                ReferenceMatch::Found(vec![Arc::clone(sym)])
                            } else {
                                ReferenceMatch::NotFound(vec![])
                            },
                        },
                    DMLShallowObjectVariant::Parameter(p) => {
                        if let Some(param) = p.get_unambiguous_def() {
                            // TODO: Remove this when we can resolve 'dev' param
                            // using constant folding
                            if param.name().val.as_str() == "dev" {
                                return self.lookup_def_in_resolved(
                                    self.get_device_obj().resolve(
                                        &self.objects),
                                    name,
                                    type_hint);
                            } else {
                                // TODO: pre-evaluate params in objects that are
                                // noderefs, so it is simple to re-do the lookup
                                // here
                                #[allow(clippy::single_match)]
                                match param.value.as_ref() {
                                    Some(ParamValue::Set(expr)) =>
                                        return
                                        collapse_referencematches(
                                            self.expression_to_resolved_objects(
                                                expr, obj)
                                                .into_iter()
                                                .map(|res|
                                                     self.lookup_def_in_resolved(
                                                         res,
                                                         name,
                                                         type_hint.clone()))),
                                    _ => (),
                                }
                            }
                        }
                        ReferenceMatch::NotFound(vec![])
                    },
                    // TODO: Can we do _anything_ here? Perhaps defer to the
                    // default value (if any)?
                    DMLShallowObjectVariant::Session(_) |
                    DMLShallowObjectVariant::Saved(_) => ReferenceMatch::NotFound(vec![]),
                    // Special case for hooks, 'send_now' is the only currently
                    // allowed member
                    DMLShallowObjectVariant::Hook(_) => if name == "send_now" {
                        ReferenceMatch::Found(
                            vec![Arc::clone(
                                self.symbol_info.variable_symbols
                                    .get(obj.location()).unwrap())])
                    } else {
                        ReferenceMatch::NotFound(vec![])
                    },
                    _ => {
                        error!("Internal error: Wanted to lookup symbol {} in \
                                {:?}, but that's not something that can \
                                contain symbols",
                               name, obj);
                        // NOTE: This is not a typeerror, but an internal error
                        ReferenceMatch::NotFound(vec![])
                    },
                }
        }
    }

    fn lookup_global_from_noderef(&self, node: &NodeRef)
                                  -> ReferenceMatch {
        let mut symbols = vec![];
        let suggestions = vec![];
        if let NodeRef::Simple(name) = node {
            if let Some(templ) = self.templates.templates.get(
                name.val.as_str()) {
                if let Some(templ_loc) = &templ.location {
                    if let Some(templ_sym) = self.symbol_info
                        .template_symbols.get(templ_loc) {
                            symbols.push(Arc::clone(templ_sym));
                        } else {
                            error!("Unexpectedly missing a template {}",
                                   name.val);
                        }
                }
            }
        }
        // TODO: types
        // TODO: externs
        if !symbols.is_empty() {
            ReferenceMatch::Found(symbols)
        } else {
            ReferenceMatch::NotFound(suggestions)
        }
    }

    fn lookup_global_from_ref(&self, reference: &GlobalReference)
                              -> ReferenceMatch {
        match &reference.kind {
            ReferenceKind::Template => {
                if let Some(templ) = self.templates
                    .templates.get(&reference.name) {
                        // Dummy templates have no loc, do not actually exist
                        if let Some(templ_loc) = &templ.location {
                            if let Some(templ_sym) =
                                self.symbol_info.template_symbols
                                .get(templ_loc) {
                                    return ReferenceMatch::Found(
                                        vec![Arc::clone(templ_sym)])
                                } else {
                                    error!("Unexpectedly missing a template {}",
                                           reference.name);
                                }
                        }
                    }
                ReferenceMatch::NotFound(vec![])
            },
            ReferenceKind::Type =>
                ReferenceMatch::NotFound(vec![]),
            _ => {
                error!("Invalid global reference kind in {:?}", reference);
                ReferenceMatch::NotFound(vec![])
            },
        }
    }

    fn lookup_global_symbol(&self, sym: &SimpleSymbol)
                            -> ReferenceMatch {
        match sym.kind {
            DMLSymbolKind::Template => {
                if let Some(sym) = self.symbol_info.template_symbols.get(&sym.loc) {
                    ReferenceMatch::Found(vec![Arc::clone(sym)])
                } else {
                    // I dont think this can happen, so show an error
                    error!("Unexpectedly missing a template {}",
                           sym.name);
                    ReferenceMatch::NotFound(vec![])
                }
            },
            _ => ReferenceMatch::NotFound(vec![]),
        }
    }

    fn lookup_def_in_obj(&self, obj: &DMLObject, sym: &SimpleSymbol)
                         -> ReferenceMatch {
        let resolved = obj.resolve(&self.objects);
        if resolved.as_comp().map_or(false, |c|c.kind == CompObjectKind::Device)
            && sym.kind == DMLSymbolKind::Template {
                self.lookup_global_symbol(sym)
            } else {
                self.lookup_def_in_resolved(resolved,
                                            sym.name_ref(),
                                            None)
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

    pub fn lookup_symbols_by_contexted_symbol<'t>(
        &self,
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
            debug!("Found {:?}", objs);
            objs.into_iter()
                .map(|o|self.lookup_def_in_obj(&o, sym.symbol))
                .filter_map(|rm|match rm {
                    ReferenceMatch::Found(syms) => Some(syms),
                    _ => None,
                })
                .flatten()
                .unique_by(|s|s.lock().unwrap().loc)
                .collect()

        } else {
            // TODO: Do we need to fall back on globals here? Can we get an
            // identifier from a spot that is generic enough to refer to a
            // global, but also is in a context where a global makes sense?
            error!("Internal Error: Failed to find objects matching {:?}",
                   sym);
            vec![]
        }
    }

    fn resolve_noderef_in_symbol<'t>(&'t self,
                                     symbol: &'t SymbolRef,
                                     node: &NodeRef)
                                     -> ReferenceMatch {
        if let Ok(sym) = symbol.lock() {
            match &sym.source {
                SymbolSource::DMLObject(key) => {
                    self.resolve_noderef_in_obj(key, node)
                },
                // TODO: Cannot be resolved without constant folding
                SymbolSource::MethodArg(_method, _name) =>
                    ReferenceMatch::NotFound(vec![]),
                // TODO: Fix once type system is sorted
                SymbolSource::Type(_typed) =>
                    ReferenceMatch::NotFound(vec![]),
                // TODO: Handle lookups inside templates
                SymbolSource::Template(_templ) =>
                    ReferenceMatch::NotFound(vec![]),
            }
        } else {
            error!("Internal Error?: Circular noderef resolve");
            ReferenceMatch::NotFound(vec![])
        }
    }

    fn resolve_noderef_in_obj<'c>(&'c self,
                                  obj: &DMLObject,
                                  node: &NodeRef)
                                  -> ReferenceMatch {
        match node {
            NodeRef::Simple(simple) => {
                let resolvedobj = obj.resolve(&self.objects);
                self.lookup_def_in_resolved(resolvedobj,
                                            &simple.val,
                                            None)
            },
            NodeRef::Sub(subnode, simple, _) => {
                let sub = self.resolve_noderef_in_obj(obj, subnode);
                match sub {
                    ReferenceMatch::Found(syms) => {
                        let wrapped_simple = NodeRef::Simple(simple.clone());
                        let mut found_syms = vec![];
                        let mut suggestions = vec![];
                        for sub_result in syms.into_iter().map(
                            |sym|self.resolve_noderef_in_symbol(
                                &sym, &wrapped_simple)) {
                            match sub_result {
                                ReferenceMatch::Found(mut sub_syms) =>
                                    found_syms.append(&mut sub_syms),
                                ReferenceMatch::NotFound(mut sub_suggestions) =>
                                    suggestions.append(&mut sub_suggestions),
                                // No need to have this be exhaustive,
                                // early return is ok
                                wt @ ReferenceMatch::WrongType(_) => return wt,
                            }
                        }
                        if !found_syms.is_empty() {
                            ReferenceMatch::Found(found_syms)
                        } else {
                            ReferenceMatch::NotFound(suggestions)
                        }
                    },
                    other => other,
                }
            }
        }
    }

    fn lookup_ref_in_obj(&self,
                         obj: &DMLObject,
                         reference: &VariableReference)
                         -> ReferenceMatch {
        match &reference.kind {
            ReferenceKind::Template |
            ReferenceKind::Type => {
                error!("Internal Error: Attempted to do a contexted lookup \
                        of a global reference {:?}", reference);
                return ReferenceMatch::NotFound(vec![]);
            },
            _ => (),
        }
        self.resolve_noderef_in_obj(obj, &reference.reference)
        // TODO: Could sanity the result towards the referencekind here
    }

    pub fn lookup_symbols_by_contexted_reference(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference) -> ReferenceMatch {
        debug!("Looking up {:?} : {:?} in device tree", context_chain,
               reference);
        // NOTE: This is actually unused, but contexts_to_objs is used
        // both from user- and server- context and thus needs this argument
        let mut limitations = HashSet::new();
        if let Some(objs) = self.contexts_to_objs(
            vec![self.get_device_obj().clone()],
            context_chain,
            &mut limitations) {
            let mut syms = vec![];
            let mut suggestions = vec![];
            for result in objs.into_iter().map(
                |o|self.lookup_ref_in_obj(&o, reference)) {
                match result {
                    ReferenceMatch::Found(mut found_syms) =>
                        syms.append(&mut found_syms),
                    w @ ReferenceMatch::WrongType(_) =>
                        return w,
                    ReferenceMatch::NotFound(mut more_suggestions) =>
                        suggestions.append(&mut more_suggestions),
                }
            }
            if syms.is_empty() {
                ReferenceMatch::NotFound(suggestions)
            } else {
                ReferenceMatch::Found(syms)
            }
        } else {
            ReferenceMatch::NotFound(vec![])
        }
    }

    fn find_target_for_reference(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference,
        reference_cache: &Mutex<ReferenceCache>)
        -> ReferenceMatch {
        let index_key = (context_chain.to_vec(),
                         reference.clone());
        {
            if let Some(cached_result) = reference_cache.lock().unwrap()
                .get(index_key.clone()) {
                    return cached_result.clone();
                }
        }

        let result = self.find_target_for_reference_aux(
            context_chain, reference, reference_cache);
        reference_cache.lock().unwrap()
            .insert(index_key, result.clone());
        result
    }

    fn find_target_for_reference_aux(
        &self,
        context_chain: &[ContextKey],
        reference: &VariableReference,
        reference_cache: &Mutex<ReferenceCache>)
        -> ReferenceMatch {
        if context_chain.is_empty() {
            // Nothing matches the noderef except maybe globals
            return self.lookup_global_from_noderef(&reference.reference);
        }

        match self.lookup_symbols_by_contexted_reference(
            // Ignore first element of chain, it is the device context
            &context_chain[1..], reference) {
            f @ ReferenceMatch::Found(_) => f,
            c @ ReferenceMatch::WrongType(_) => c,
            ReferenceMatch::NotFound(mut suggestions) => {
                let (_, new_chain) = context_chain.split_last().unwrap();
                match self.find_target_for_reference(new_chain,
                                                     reference,
                                                     reference_cache) {
                    f @ ReferenceMatch::Found(_) => f,
                    c @ ReferenceMatch::WrongType(_) => c,
                    ReferenceMatch::NotFound(mut rest_suggestions) => {
                        suggestions.append(&mut rest_suggestions);
                        ReferenceMatch::NotFound(suggestions)
                    },
                }
            },
        }
    }
}

impl DeviceAnalysis {
    fn match_references_in_scope<'c>(
        &'c self,
        scope_chain: Vec<&'c dyn Scope>,
        _report: &Mutex<Vec<DMLError>>,
        reference_cache: &Mutex<ReferenceCache>) {
        let current_scope = scope_chain.last().unwrap();
        let context_chain: Vec<ContextKey> = scope_chain
            .iter().map(|s|s.create_context()).collect();
        // NOTE: chunk number is arbitrarily picked that benches well
        current_scope.defined_references().par_chunks(25).for_each(|references|{
            for reference in references {
                debug!("In {:?}, Matching {:?}", context_chain, reference);
                let symbol_lookup = match &reference {
                    Reference::Variable(var) => self.find_target_for_reference(
                        context_chain.as_slice(), var, reference_cache),
                    Reference::Global(glob) =>
                        self.lookup_global_from_ref(glob),
                };

                match symbol_lookup {
                    ReferenceMatch::NotFound(_suggestions) =>
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
                    ReferenceMatch::Found(symbols) =>
                        for symbol in &symbols {
                            let mut sym = symbol.lock().unwrap();
                            sym.references.push(*reference.loc_span());
                            if let Some(meth) = sym.source
                                .as_object()
                                .and_then(DMLObject::as_shallow)
                                .and_then(|s|s.variant.as_method()) {
                                    if let Some(default_decl) = meth.get_default() {
                                        if let Some(var) = reference.as_variable_ref() {
                                            if var.reference.to_string().as_str()
                                                == "default" {
                                                    sym.default_mappings.insert(
                                                        *var.loc_span(),
                                                        *default_decl.location());
                                                }
                                        }
                                    }
                                }
                        },
                    ReferenceMatch::WrongType(_) =>
                    //TODO: report mismatch,
                        (),
                }
            }
        })
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
    pub fn new(path: &CanonPath, clientpath: &PathBuf, file: TextFile)
               -> Result<IsolatedAnalysis, Error> {
        trace!("local analysis: {} at {}", path.as_str(), path.as_str());
        let filespec = FileSpec {
            path, file: &file
        };
        let (mut ast, provisionals, mut errors) = parse_file(path, filespec)?;

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
                &path, context) {
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

fn objects_to_symbols(objects: &StructureContainer) -> SymbolStorage {
    let mut storage = SymbolStorage::default();

    for obj in objects.values() {
        let new_symbol: SymbolRef = new_symbol_from_object(obj);
        debug!("Comp obj symbol is: {:?}", new_symbol);
        storage.object_symbols.insert(obj.key, new_symbol);
        for subobj in obj.components.values() {
            // Non-shallow objects will be handled by the iteration
            // over objects
            if let DMLObject::ShallowObject(shallow) = subobj {
                add_new_symbol_from_shallow(shallow, &mut storage);
            }
        }
    }
    storage
}

fn template_to_symbol(template: &Arc<DMLTemplate>) -> Option<SymbolRef> {
    // Do not create symbols for templates without location, they are dummy
    // missing templates
    template.location.as_ref().map(|location| {
        Arc::new(Mutex::new(Symbol {
            loc: *location,
            kind: DMLSymbolKind::Template,
            references: vec![],
            definitions: vec![*location],
            declarations: vec![*location],
            implementations: vec![],
            bases: vec![],
            source: SymbolSource::Template(Arc::clone(template)),
            default_mappings: HashMap::default(),
        }))})
}

fn extend_with_templates(storage: &mut SymbolStorage,
                         templates: &TemplateTraitInfo) {
    for template in templates.templates.values() {
        if let Some(new_templ) = template_to_symbol(template) {
            let loc = new_templ.lock().unwrap().loc;
            if let Some(prev) = storage.template_symbols
                .insert(loc, new_templ) {
                    error!("Internal Error: Unexpectedly two template symbols
                        defined in the same location");
                    error!("Previous was {:?}", prev);
                    error!("New is {:?}", storage.template_symbols.get(&loc));
                }
        }
    }
}

fn new_symbol_from_object(object: &DMLCompositeObject) -> SymbolRef {
    let all_decl_defs: Vec<ZeroSpan> = object.all_decls.iter().map(
        |spec|*spec.loc_span()).collect();
    Arc::new(Mutex::new(Symbol {
        loc: object.declloc,
        kind: DMLSymbolKind::CompObject(object.kind),
        definitions: all_decl_defs.clone(),
        declarations: all_decl_defs.clone(),
        bases: all_decl_defs,
        references: vec![],
        implementations: vec![],
        source: SymbolSource::DMLObject(
            DMLObject::CompObject(object.key)),
        default_mappings: HashMap::default(),
    }))
}

fn new_symbol_from_arg(methref: &Arc<DMLMethodRef>,
                       arg: &DMLMethodArg) -> SymbolRef {
    let bases = vec![*arg.loc_span()];
    let definitions = vec![*arg.loc_span()];
    let declarations = vec![*arg.loc_span()];
    Arc::new(Mutex::new(Symbol {
        loc: *arg.loc_span(),
        kind: DMLSymbolKind::MethodArg,
        bases,
        definitions,
        declarations,
        references: vec![],
        implementations: vec![],
        source: SymbolSource::MethodArg(Arc::clone(methref),
                                        arg.name().clone()),
        default_mappings: HashMap::default(),
    }))
}

fn log_non_same_insert<K>(map: &mut HashMap<K, SymbolRef>,
                          key: K,
                          val: SymbolRef) -> bool
where K: std::hash::Hash + Eq + Clone,
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
            error!("Unexpected Internal Error: Overwrote previous symbol \
                    {:?} with non-similar symbol {:?}",
                   old, map.get(&key));
            return true;
        }
    }
    false
}

fn add_new_symbol_from_shallow(shallow: &DMLShallowObject,
                               storage: &mut SymbolStorage) {
    let (bases, definitions, declarations) = match &shallow.variant {
        DMLShallowObjectVariant::Parameter(param) =>
            (vec![*param.get_likely_declaration().loc_span()],
             param.used_definitions.iter()
             .map(|(_, def)|*def.loc_span()).collect(),
             param.declarations.iter()
             .map(|(_, def)|*def.loc_span()).collect()),
        DMLShallowObjectVariant::Method(method_ref) =>
            (vec![*method_ref.get_base().location()],
             method_ref.get_all_defs(),
             method_ref.get_all_decls()),
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
    debug!("Made symbol for {:?}", shallow);
    let new_sym = Arc::new(Mutex::new(Symbol {
        loc: *shallow.location(),
        kind: shallow.kind(),
        definitions,
        declarations,
        implementations: vec![],
        references: vec![],
        bases,
        source: SymbolSource::DMLObject(
            // TODO: Inefficient clone. Not terribly so, but worth
            // noting
            DMLObject::ShallowObject(shallow.clone())),
        default_mappings: HashMap::default(),
    }));
    match &shallow.variant {
        DMLShallowObjectVariant::Parameter(_) => {
            log_non_same_insert(storage.param_symbols.entry(
                (*shallow.location(),
                 shallow.identity().to_string()))
                                .or_default(),
                                shallow.parent,
                                new_sym);
        },
        DMLShallowObjectVariant::Method(method_ref) =>
            if !log_non_same_insert(&mut storage.method_symbols,
                                    *shallow.location(),
                                    new_sym) {
                for arg in method_ref.args() {
                    let new_argsymbol = new_symbol_from_arg(method_ref, arg);
                    log_non_same_insert(&mut storage.variable_symbols,
                                        *arg.loc_span(),
                                        new_argsymbol);
                }
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

impl DeviceAnalysis {
    pub fn new(root: IsolatedAnalysis,
               timed_bases: Vec<TimestampedStorage<IsolatedAnalysis>>,
               imp_map: HashMap<Import, String>)
               -> Result<DeviceAnalysis, Error> {
        info!("device analysis: {:?}", root.path);

        if root.toplevel.device.is_none() {
            return Err(Error::InternalError(
                "Attempted to device analyze a file without device decl"));
        }

        let mut bases: Vec<_> =
            timed_bases.into_iter().map(|tss|tss.stored).collect();

        // Fake the implicit imports into the root toplevel
        // We do this into bases, because that is the analysises that are
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
        info!("Rank templates");
        let (templates, order, invalid_isimps, rank_struct)
            = rank_templates(&unique_templates, &files, &imp_map, &mut errors);
        let mut rankmaker = RankMaker::new();
        info!("Templates+traits");
        let tt_info = create_templates_traits(
            &root.toplevel.start_of_file,
            &mut rankmaker, templates, order,
            invalid_isimps, &imp_map, rank_struct, &mut errors);

        // TODO: catch typedef/traitname overlaps

        // TODO: this is where we would do type resolution
        let mut container = StructureContainer::default();
        info!("Make device");
        let device_key = make_device(root.path.as_str(), &root.toplevel,
                                     &tt_info, imp_map, &mut container,
                                     &mut rankmaker, &mut errors).key;
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

        let mut mapped_errors = HashMap::default();
        for error in errors {
            let entr: &mut Vec<_> = mapped_errors.entry(error.span.path())
                .or_default();
            entr.push(error);
        }

        trace!("Errors are {:?}", mapped_errors);

        let mut symbol_info = objects_to_symbols(&container);
        info!("Generate symbols");
        // TODO: how do we store type info?
        extend_with_templates(&mut symbol_info, &tt_info);
        //extend_with_types(&mut symbols, ??)
        let mut device = DeviceAnalysis {
            name: root.toplevel.device.unwrap().name.val,
            errors: mapped_errors,
            objects: container,
            device_obj: DMLObject::CompObject(device_key),
            templates: tt_info,
            symbol_info,
            reference_info: ReferenceStorage::new(),
            template_object_implementation_map,
            path: root.path.clone(),
            clientpath: root.path.clone().into(),
            dependant_files: bases.iter().map(|b|&b.path).cloned().collect(),
        };

        info!("Match references");
        let reference_cache: Mutex<ReferenceCache> = Mutex::default();
        let new_errors: Mutex<Vec<DMLError>> = Mutex::default();
        for scope_chain in all_scopes(&bases) {
            device.match_references_in_scope(scope_chain,
                                             &new_errors,
                                             &reference_cache);
        }
        for err in new_errors.into_inner().unwrap() {
            device.errors.entry(err.span.path())
                .or_default()
                .push(err);
        }

        info!("Inverse map");
        // Set up the inverse map of references->symbols
        for symbol in device.symbol_info.template_symbols.values()
            .chain(device.symbol_info.param_symbols
                   .values().flat_map(|h|h.values()))
            .chain(device.symbol_info.object_symbols.values())
            .chain(device.symbol_info.method_symbols.values())
            .chain(device.symbol_info.variable_symbols.values()) {
                debug!("Inverse of {:?}", symbol);
                for ref_loc in &symbol.lock().unwrap().references {
                    device.reference_info.entry(*ref_loc)
                        .or_default().push(Arc::clone(symbol));
                }
            }

        info!("Implementation map");
        for (templ_loc, objects) in &device.template_object_implementation_map {
            if let Some(templ) = device.symbol_info
                .template_symbols.get(templ_loc) {
                    let mut sym_mut = templ.lock().unwrap();
                    sym_mut.implementations.extend(
                        objects.iter().flat_map(
                            |obj|device.objects.get(*obj).unwrap()
                                .all_decls.iter().map(
                                    |spec|*spec.loc_span())));
                }
        }

        info!("Done with device");
        Ok(device)
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

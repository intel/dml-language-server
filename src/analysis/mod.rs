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

use hashlink::LinkedHashMap;
use itertools::Itertools;
use lsp_types::{DiagnosticSeverity};
use logos::Logos;
use crate::logging::{debug, error, info, trace};
use rayon::prelude::*;

use crate::actions::{SourcedDMLError, DeviceAnalysisJobOptions};
use crate::actions::analysis_storage::{TimestampedStorage};
use crate::actions::semantic_lookup::{DLSLimitation, isolated_template_limitation};
use crate::analysis::templating::evaluation::{EvaluationContext};
use crate::analysis::symbols::{DMLSymbolKind, SimpleSymbol, StructureSymbol, SymbolContainer, SymbolMaker, SymbolSource};
pub use crate::analysis::symbols::SymbolRef;
use crate::analysis::reference::{GlobalReference, NodeRef, CodeReference, Reference, ReferenceKind, ReferenceVariant, VariableReference};
use crate::analysis::scope::{Scope, SymbolContext,
                             ContextKey, ContextedSymbol};
use crate::analysis::parsing::parser::{FileInfo, FileParser};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::provisionals::ProvisionalsManager;

pub use crate::analysis::parsing::tree::
{ZeroRange, ZeroSpan, ZeroPosition, ZeroFilePosition};

use crate::analysis::parsing::tree::{MissingToken, MissingContent, TreeElement};
use crate::analysis::structure::objects::{CompObjectKind, Import, Loggroup, MaybeAbstract, ParamValue, Template};
use crate::analysis::structure::statements::{ForPre, Statement, StatementKind,
                                             SwitchCase};
use crate::analysis::structure::toplevel::{ExistCondition, ObjectDecl,
                                           StatementSpec, TopLevel};
use crate::analysis::structure::types::{DMLType, DMLTypeKind, DMLTypeMember};
use crate::analysis::structure::expressions::{Expression, ExpressionKind,
                                              DMLString, MemberOp};
use crate::analysis::templating::objects::{make_device,
                                           check_method_body_types_for_objects,
                                           DMLObject,
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
use crate::analysis::templating::traits::{lookup_impl, DMLTemplate,
                                          TemplateTraitInfo, TraitMemberKind};
use crate::analysis::templating::types::{
    direct_members, DMLResolvedType, TypeDatabase, TypeId, TypeUse,
};

use crate::concurrency::AliveStatus;
use crate::file_management::{PathResolver, CanonPath};

use crate::vfs::{TextFile, Error as VFSError};

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
    fn size_of(&self) -> usize {
        std::mem::size_of_val(self)
         + self.description.len()
         + self.related.iter().fold(0, |a, (_, m)| a + std::mem::size_of::<ZeroSpan>() + m.len())
    }

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
        trace!("Finding symbol for name {:?} at position {:?} in rangeentry {:?}",
              name.val, name.span.start_position(), self);
        self.find_symbol_for_name(
            &name.val, name.span.start_position().position)
    }

    fn find_smallest_scope_around(&self, loc: ZeroPosition)
                                  -> Option<ZeroRange> {
        trace!("Finding smallest scope around position {:?} in rangeentry {:?}", loc, self);
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
    pub typedef_symbols: HashMap<TypeId, SymbolRef>,
    pub type_member_symbols: HashMap<ZeroSpan, SymbolRef>,
    pub loggroup_symbols: HashMap<ZeroSpan, SymbolRef>,
    // Member names of anonymous struct-like types, keyed by type span
    pub anonymous_type_members: HashMap<ZeroSpan, Vec<DMLString>>,
    // Because some implicit parameters are defined in the same
    // place, we need to disambiguate this by name
    pub param_symbols: HashMap<(ZeroSpan, String),
                               HashMap<StructureKey, SymbolRef>>,
    pub object_symbols: HashMap<StructureKey, SymbolRef>,
    // This is doubly-indexed, by decl location and then
    // by parent object key
    pub method_symbols: HashMap<ZeroSpan, HashMap<StructureKey, SymbolRef>>,
    // variable symbols keyed by canonical decl location
    // Includes externs, constants, sessions, saveds, hooks and method args.
    pub variable_symbols: HashMap<ZeroSpan, SymbolRef>,
}

impl SymbolStorage {
    pub fn all_symbols<'a>(&'a self) -> impl Iterator<Item = &'a SymbolRef> {
        self.template_symbols.values()
            .chain(self.typedef_symbols.values())
            .chain(self.type_member_symbols.values())
            .chain(self.loggroup_symbols.values())
            .chain(self.param_symbols.values().flat_map(|h|h.values()))
            .chain(self.object_symbols.values())
            .chain(self.method_symbols.values().flat_map(|h|h.values()))
            .chain(self.variable_symbols.values())
    }
}

// This maps references to the symbol they reference, made as a lock
// because we need to incrementally fill it as requests are made
type ReferenceStorage = Arc<Mutex<HashMap<ZeroSpan, Vec<SymbolRef>>>>;
type ResolvedMethodCallCandidates = HashMap<
    (StructureKey, ZeroSpan), Vec<Arc<DMLMethodRef>>>;

// Analysis from the perspective of a particular DML device
#[derive(Debug, Clone)]
pub struct DeviceAnalysis {
    // Device name
    pub name: String,
    pub errors: HashMap<PathBuf, Vec<DMLError>>,
    pub objects: StructureContainer,
    pub device_obj: DMLObject,
    pub templates: TemplateTraitInfo,
    pub types: TypeDatabase,
    // Non-colliding loggroup declarations by name; more than one only when
    // their conditions are mutually exclusive
    pub loggroups: HashMap<String, Vec<ObjectDecl<Loggroup>>>,
    pub symbol_info: SymbolStorage,
    pub reference_info: ReferenceStorage,
    pub template_object_implementation_map: HashMap<ZeroSpan,
                                                    Vec<StructureKey>>,
    // Name spans of object and method declarations guarded by '#if'
    pub conditional_declarations: HashSet<ZeroSpan>,
    pub path: CanonPath,
    pub dependant_files: Vec<CanonPath>,
    pub clientpath: PathBuf,
    pub reference_cache_max_size : usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ReferenceMatchKind {
    Found,
    MismatchedFind,
    NotFound,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    fn size_of(&self) -> usize {
        // NOTE: This does NOT count the size of the values under the symbolrefs, as these should be shared with
        // the analysis
        std::mem::size_of_val(self)
         + self.references.len() * std::mem::size_of::<SymbolRef>()
         + self.messages.iter().fold(0, |a, m| a + m.size_of())
    }

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

// Agnostic reference
type AgnRef = Vec<String>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ReferenceCacheKey {
    object_path: String,
    agnostic_reference: AgnRef,
    method_scope: Option<ZeroRange>,
}

impl ReferenceCacheKey {
    fn size_of(&self) -> usize {
        std::mem::size_of_val(self)
         + self.object_path.len()
         + self.agnostic_reference.iter().fold(0, |a, s| a + s.len())
    }
}

struct ReferenceCache {
    underlying_cache: LinkedHashMap<ReferenceCacheKey, ReferenceMatches>,
    used_size: usize,
    allowed_size: usize,
    enabled: bool,
    num_insert_failures: usize,
}

fn object_to_path_name(object: Option<&DMLObject>, container: &StructureContainer) -> String {
    if let Some(obj) = object.map(|o|o.resolve(container)) {
        let parent = obj.parent().map(DMLObject::CompObject);
        object_to_path_name_aux(parent.as_ref(),
                                container,
                                obj.identity().to_string())
    } else {
        String::new()
    }
}

fn object_to_path_name_aux(object: Option<&DMLObject>, container: &StructureContainer, acc: String) -> String {
    if let Some(obj) = object {
        let o = obj.resolve(container);
        let parent = o.parent().map(DMLObject::CompObject);
        object_to_path_name_aux(parent.as_ref(),
                                container,
                                format!("{}.{}", acc, o.identity()))
    } else {
        acc
    }
}

impl ReferenceCache {
    fn new(size: usize) -> Self {
        if size == 0 {
            debug!("Auto-disabled reference cache due to designated size being 0");
        }
        ReferenceCache {
            underlying_cache: LinkedHashMap::new(),
            used_size: 0,
            allowed_size: size,
            enabled: size > 0,
            num_insert_failures: 0,
        }
    }

    fn flatten_ref(refr: &NodeRef, agn: &mut Vec<String>) {
        match refr {
            NodeRef::Simple(dmlstring) => agn.push(dmlstring.val.clone()),
            NodeRef::Sub(sub, op, dmlstring, _) => {
                Self::flatten_ref(sub.as_ref(), agn);
                agn.push(match op {
                    MemberOp::Dot => dmlstring.val.clone(),
                    MemberOp::Arrow => format!("->{}", dmlstring.val),
                });
            },
            NodeRef::Index(inner) => {
                Self::flatten_ref(inner.as_ref(), agn);
                agn.push("[]".to_string());
            },
        }
    }
    fn convert_to_key(key: (Option<&DMLObject>, VariableReference),
                      container: &StructureContainer,
                      method_structure: &HashMap<ZeroSpan, RangeEntry>)
                      -> ReferenceCacheKey {
        let (object, refr) = key;
        let method_scope =
            // contexts-to-obj will always give the most-overriding method, but our reference may actually be from a scope in an overridden method.
            // So here we adjust and find the correct method
            if let Some(meth) = object.and_then(|o|o.as_shallow()).and_then(|s|s.variant.as_method()) {
                let adjusted_meth = DeviceAnalysis::adjust_method_for_pos(meth, &refr.span().start_position())
                    .unwrap_or(meth);
                method_structure.get(adjusted_meth.location()).and_then(
                        |re|re.find_smallest_scope_around(refr.loc_span().range.start()))
            } else {
                None
            };
        let object_path = object_to_path_name(object, container);
        let mut agnostic_reference = vec![];
        Self::flatten_ref(&refr.reference, &mut agnostic_reference);
        let res = ReferenceCacheKey {
            object_path,
            agnostic_reference,
            method_scope
        };
        trace!("Converted {:?} to agnostic key {:?}", (object, refr), res);
        res
    }

    pub fn get(&mut self,
               key: (Option<&DMLObject>, VariableReference),
               container: &StructureContainer,
               method_structure: &HashMap<ZeroSpan, RangeEntry>)
               -> Option<&ReferenceMatches> {
        if !self.enabled {
            return None;
        }
        let agn_key = Self::convert_to_key(key, container, method_structure);
        match self.underlying_cache.raw_entry_mut().from_key(&agn_key) {
            hashlink::linked_hash_map::RawEntryMut::Occupied(mut e) => {
                // Update cache entry to be recently used
                e.to_back();
                Some(e.into_mut())
            },
            _ => None,
        }
    }

    fn size_of_key_val(key: &ReferenceCacheKey, val: &ReferenceMatches) -> usize {
        key.size_of() + val.size_of()
    }

    fn remove_oldest(&mut self) {
        // Unwrap is guaranteed by caller, a.k.a. do not speculatively call this function
        let (oldest_key, oldest_val) = self.underlying_cache.pop_front().unwrap();
        let removed_size = Self::size_of_key_val(&oldest_key, &oldest_val);
        self.used_size -= removed_size;
    }

    fn prepare_space(&mut self, added_size: usize) -> Result<(), &'static str>{
        if added_size > self.allowed_size {
            return Err("Not enough space allowed in cache");
        }
        while self.used_size + added_size > self.allowed_size {
            self.remove_oldest();
        }
        Ok(())
    }

    pub fn insert(&mut self,
                  key: (Option<&DMLObject>, VariableReference),
                  val: ReferenceMatches,
                  container: &StructureContainer,
                  method_structure: &HashMap<ZeroSpan, RangeEntry>)
    {
        if !self.enabled {
            return;
        }
        let agn_key = Self::convert_to_key(key, container, method_structure);
        if let Some(previous_result) = self.underlying_cache.get(&agn_key) {
            if val != *previous_result {
                internal_error!("Invariant broken: agnostic keys resulted in different matches, old: {:?}, new: {:?}, agn key: {:?}", previous_result, val, agn_key);
            }
            return;
        }
        let added_size = Self::size_of_key_val(&agn_key, &val);
        if let Err(e) = self.prepare_space(added_size) {
            internal_error!("Failed to insert '{:?}' into reference cache: {}", agn_key, e);
            self.num_insert_failures += 1;
            if self.num_insert_failures > 10 {
                info!("Auto-disabling reference cache due to repeated insert failures, consider increasing the cache size limit");
                self.enabled = false;
            }
            return;
        }
        self.used_size += added_size;
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

pub enum AnalysisError {
    VFSError(VFSError),
    Cancelled,
}

impl std::fmt::Debug for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalysisError::VFSError(v) => write!(f, "VFSError({:?})", v),
            AnalysisError::Cancelled => write!(f, "Cancelled"),
        }
    }
}

pub type AnalysisProcessResult<T> = Result<T, AnalysisError>;

impl From<VFSError> for AnalysisError {
    fn from(e: VFSError) -> Self {
        AnalysisError::VFSError(e)
    }
}

impl From<AnalysisError> for AnalysisProcessResult<()> {
    fn from(e: AnalysisError) -> Self {
        Err(e)
    }
}

impl DeviceAnalysis {
    pub fn lookup_symbols<'t>(&self, context_sym: &ContextedSymbol<'t>, limitations: &mut HashSet<DLSLimitation>)
        -> Result<Vec<SymbolRef>, String> {
        trace!("Looking up symbols at {:?}", context_sym);

        // Special handling for methods, since each method decl has its
        // own symbol
        if context_sym.kind() == DMLSymbolKind::Method {
            return Ok(self.symbol_info.method_symbols
                .get(context_sym.loc_span())
                .map(|m|m.values().cloned())
                .into_iter().flatten().collect());
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
                        -> Result<Vec<DMLObject>, String> {
        if context_chain.is_empty() {
            Ok(curr_objs)
        } else {
            self.contexts_to_objs_aux(
                curr_objs.into_iter().map(
                    |obj|match obj.resolve(&self.objects) {
                        DMLResolvedObject::CompObject(robj) => Ok(robj),
                        DMLResolvedObject::ShallowObject(sobj) => {
                            Err(format!(
                                "Internal Error: \
                                 Wanted to find context {:?} in {:?} \
                                 which is not \
                                 a composite object", sobj, context_chain))
                        }
                    })
                    .collect::<Result<Vec<_>, String>>()?,
                context_chain,
                limitations)
        }
    }

    fn contexts_to_objs_aux(&self,
                            curr_objs: Vec<&DMLCompositeObject>,
                            context_chain: &[ContextKey],
                            limitations: &mut HashSet<DLSLimitation>)
                            -> Result<Vec<DMLObject>, String> {
        let (matches, mut errors): (Vec<_>, Vec<_>) = curr_objs.into_iter()
            .map(|o|self.context_to_objs(o, context_chain, limitations))
            .partition_result();
        let matches: Vec<_> = matches.into_iter().flatten().collect();
        if matches.is_empty() {
            // Hard to tell which error would be the most relevant, so
            // pick the first one, if any
            return errors.pop().map(Err).unwrap_or(Ok(vec![]));
        }
        Ok(matches)
    }

    // TODO: This function is called from two contexts, and this is a reoccuring pain-point
    // From the analysis reference-matching side, this would like to have an entry point where
    // limitations are not considered and the first entry in the chain is the device scope
    // From the semantic lookup side, this would like to have an entry point where limitations
    // are relevant and we do not have the device scope at all
    fn context_to_objs(&self,
                       curr_obj: &DMLCompositeObject,
                       context_chain: &[ContextKey],
                       limitations: &mut HashSet<DLSLimitation>)
                       -> Result<Vec<DMLObject>, String> {
        // Should be guaranteed by caller responsibility
        if context_chain.is_empty() {
            return Err(format!("context chain invariant broken at {:?}", curr_obj.identity()));
        }

        let (first, rest) = context_chain.split_first().unwrap();
        let next_objs = match first {
            ContextKey::Structure(sym) => {
                match curr_obj.get_object(sym.name_ref()) {
                    Some(found_obj) => if found_obj.resolve(&self.objects)
                        .as_comp().is_some() {
                            vec![found_obj.clone()]
                        } else {
                            // A higher-precedence non-composite declaration
                            // won the name collision
                            return Ok(vec![]);
                        },
                    None if self.conditional_declarations
                        .contains(sym.loc_span()) => return Ok(vec![]),
                    None => return Err(format!(
                        "Failed to find structural object corresponding to \
                         {:?} at {:?}", first, curr_obj.identity())),
                }
            },
            ContextKey::Method(sym) => {
                if let Some(found_obj)
                    = curr_obj.get_object(sym.name_ref()) {
                        if found_obj.resolve(&self.objects).as_shallow()
                            .is_some_and(|s|matches!(
                                &s.variant,
                                DMLShallowObjectVariant::Method(_))) {
                                vec![found_obj.clone()]
                            }
                        else {
                            // A higher-precedence symbol kind can legitimately
                            // discard this method during object construction.
                            // See ISSUE#241
                            return Ok(vec![]);
                        }
                    }
                else if self.conditional_declarations
                    .contains(sym.loc_span()) {
                    return Ok(vec![]);
                } else {
                    return Err(format!(
                        "Failed to find method object corresponding to \
                         {:?} at {:?}", first, curr_obj.identity()));
                }
            },
            ContextKey::Template(sym) =>
                if let Some(templ) = self.templates.templates.get(sym.name_ref()) {
                    // A discarded duplicate declaration has no instances
                    if templ.location.as_ref().is_some_and(
                        |location|location != sym.loc_span()) {
                        return Ok(vec![]);
                    }
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
                            return Ok(vec![]);
                        } else {
                            return self.contexts_to_objs(
                                templ_impls
                                    .iter()
                                    .map(|key|DMLObject::CompObject(*key))
                                    .collect(),
                                rest,
                                limitations);
                        }
                    }
                    else {
                        return Err(format!("No template->objects map for {:?}", sym));
                    }
                } else {
                    return Err(format!(
                        "Wanted to find context {:?} in {:?} which is not \
                         a known template object",
                        first, curr_obj));
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
                    DMLShallowObjectVariant::Extern(e) =>
                        self.symbol_info.variable_symbols.get(
                            e.get_likely_definition().loc_span()),
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
                    DMLShallowObjectVariant::Saved(_) |
                    DMLShallowObjectVariant::Extern(_) => (),
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
            for loggroup in self.loggroups.get(&name.val)
                .into_iter().flatten() {
                if let Some(symbol) = self.symbol_info.loggroup_symbols
                    .get(&loggroup.obj.name.span) {
                    ref_matches.add_match(Arc::clone(symbol));
                } else {
                    error!("Unexpectedly missing a loggroup {}", name.val);
                }
            }
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
            ReferenceKind::Type => {
                for type_id in self.types.typedefs_named(&reference.name) {
                    if let Some(declaration) = self.types.declaration(*type_id) {
                        let symbols = if declaration.underlying.is_some() {
                            self.symbol_info.typedef_symbols.get(type_id)
                        } else {
                            self.symbol_info.template_symbols
                                .get(&declaration.declaration_span)
                        };
                        if let Some(symbol) = symbols {
                            ref_matches.add_match(Arc::clone(symbol));
                        } else {
                            error!("Unexpectedly missing type symbol {}",
                                   reference.name);
                        }
                    }
                }
            },
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
        // Method-local declarations are identified by their location; a
        // name lookup would also hit same-named locals in other scopes and
        // the overriding method's locals
        if matches!(sym.kind, DMLSymbolKind::Local | DMLSymbolKind::MethodArg) {
            if let Some(symbol) = self.symbol_info.variable_symbols.get(&sym.loc) {
                ref_matches.add_match(Arc::clone(symbol));
                return;
            }
        }
        if resolved.as_comp().is_some_and(|c|c.kind == CompObjectKind::Device)
            && sym.kind == DMLSymbolKind::Template {
                self.lookup_global_symbol(sym, ref_matches);
            } else {
                self.lookup_def_in_resolved(resolved,
                                            sym.name_ref(),
                                            None,
                                            ref_matches);
            }
    }

    fn lookup_global_sym(&self, sym: &SimpleSymbol) -> Result<Vec<SymbolRef>, String> {
        //TODO: being able to fail a re-match here feels silly. consider
        // adding sub-enum for DMLGlobalSymbolKind
        match sym.kind {
            DMLSymbolKind::Template => {
                if let Some((templ, _)) = self.templates.get_template(
                    sym.name.as_str()) {
                    // This _should_ be guaranteed, since the SimpleSymbol
                    // ref comes from structure
                    Ok(vec![Arc::clone(self.symbol_info.template_symbols.get(
                        templ.location.as_ref().unwrap()).unwrap())])
                } else {
                    Err(format!("Unexpectedly missing template matching {:?}", sym))
                }
            },
            DMLSymbolKind::Typedef => {
                let matches = self.types.typedefs_named(sym.name.as_str())
                    .iter()
                    .filter(|type_id|self.types.declaration(**type_id)
                        .is_some_and(|decl|decl.name.span == sym.loc))
                    .filter_map(|type_id|self.symbol_info.typedef_symbols
                        .get(type_id).map(Arc::clone))
                    .collect::<Vec<_>>();
                if matches.is_empty() {
                    Err(format!("Unexpectedly missing typedef matching {:?}", sym))
                } else {
                    Ok(matches)
                }
            },
            DMLSymbolKind::Field => self.symbol_info.type_member_symbols
                .get(&sym.loc)
                .map(|symbol|vec![Arc::clone(symbol)])
                .ok_or_else(||format!(
                    "Unexpectedly missing type member matching {:?}", sym)),
            DMLSymbolKind::Loggroup => self.symbol_info.loggroup_symbols
                .get(&sym.loc)
                .map(|symbol|vec![Arc::clone(symbol)])
                .ok_or_else(||format!(
                    "Unexpectedly missing loggroup matching {:?}", sym)),
            e => {
                Err(format!("Unexpected symbol kind of global symbol: {:?}", e))
            },
        }
    }

    // TODO/NOTE: This method seems slightly misplaced, consider moving to semantic_lookup
    pub fn lookup_symbols_by_contexted_symbol<'t>(&self,
                                                  sym: &ContextedSymbol<'t>,
                                                  limitations: &mut HashSet<DLSLimitation>)
        -> Result<Vec<SymbolRef>, String> {
        if matches!(sym.symbol.kind,
                    DMLSymbolKind::Template |
                    DMLSymbolKind::Typedef |
                    DMLSymbolKind::Field |
                    DMLSymbolKind::Loggroup)
        {
            return self.lookup_global_sym(sym.symbol);
        }

        debug!("Looking up {:?} in device tree", sym);

        let objs = if sym.contexts.is_empty() {
            vec![self.get_device_obj().clone()]
        } else {
            self.context_to_objs(self.get_device_comp_obj(),
                                 sym.contexts.iter()
                                 .cloned().cloned()
                                 .collect::<Vec<ContextKey>>()
                                 .as_slice(),
                                 limitations)?
        };

        trace!("Found {:?}", objs);
        let mut refs = ReferenceMatches::new();
        let _: Vec<_> = objs.into_iter()
            .map(|o|self.lookup_def_in_obj(&o, sym.symbol, &mut refs))
            .collect();
        // We can ignore messages from lookup defs here, as this lookup is
        // live and reporting additional things from here makes no sense
        if let Some(matches) = refs.as_matches() {
            Ok(matches.into_iter().collect())
        } else {
            Ok(vec![])
        }
    }

    fn resolve_noderef_in_symbol<'t>(&'t self,
                                     symbol: &'t SymbolRef,
                                     member_name: &DMLString,
                                     indices: u32,
                                     op: &MemberOp,
                                     method_structure:
                                     &HashMap<ZeroSpan, RangeEntry>,
                                     ref_matches: &mut ReferenceMatches) {
        let (typed, indirections) = {
            let sym = symbol.lock().unwrap();
            (sym.typed, sym.indirections)
        };
        if let Some((core, extra)) = typed.and_then(
            |typed|self.types.struct_core(typed)) {
            // '.' needs the struct itself and '->' a pointer to (or array of) it
            let dereferences = indices + u32::from(*op == MemberOp::Arrow);
            if indirections + extra == dereferences {
                for span in self.struct_member_spans(core, &member_name.val) {
                    if let Some(member_symbol) = self.symbol_info
                        .type_member_symbols.get(&span) {
                        ref_matches.add_match(Arc::clone(member_symbol));
                    }
                }
            }
            return;
        }
        if let Some(TypeUse::Declared(type_id)) = typed {
            if let Some(declaration) = self.types.declaration(type_id)
                .filter(|declaration|declaration.underlying.is_none()) {
                // Template types expose the members of their trait
                if indirections == indices && *op == MemberOp::Dot {
                    for member_symbol in self.trait_member_symbols(
                        &declaration.name.val, &member_name.val) {
                        ref_matches.add_match(member_symbol);
                    }
                }
                return;
            }
        }
        let node = NodeRef::Simple(member_name.clone());
        let node = &node;
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
            // NOTE: types handled near start-of-function
            SymbolSource::Type(_) => (),
            SymbolSource::TypeMember(_, _) => (),
            SymbolSource::Template(_) => (),
            SymbolSource::Loggroup(_) => (),
        }
    }

    // Symbols of the declaration that the trait of 'template' exposes as
    // 'member', across all objects instantiating it
    fn trait_member_symbols(&self, template: &str, member: &str)
                            -> Vec<SymbolRef> {
        let Some(owner) = self.templates.traits.get(template)
            .and_then(|template_trait|lookup_impl(template_trait, &member))
        else {
            return vec![];
        };
        let Some(declaration) = owner.get_member(member) else {
            return vec![];
        };
        let location = *declaration.location();
        match declaration {
            TraitMemberKind::Method(_) => self.symbol_info.method_symbols
                .get(&location).into_iter()
                .flat_map(|symbols|symbols.values()).map(Arc::clone)
                .collect(),
            TraitMemberKind::Session(_) | TraitMemberKind::Saved(_) =>
                self.symbol_info.variable_symbols.get(&location)
                .map(Arc::clone).into_iter().collect(),
            TraitMemberKind::Parameter(_) => self.symbol_info.param_symbols
                .iter()
                .filter(|((_, name), _)|name == member)
                .flat_map(|(_, symbols)|symbols.values())
                .filter(|symbol|symbol.lock().unwrap().declarations
                        .contains(&location))
                .map(Arc::clone)
                .collect(),
        }
    }

    fn struct_member_spans(&self, core: TypeUse, name: &str) -> Vec<ZeroSpan> {
        match core {
            TypeUse::Declared(type_id) => self.types.declaration(type_id)
                .and_then(|declaration|declaration.underlying.as_ref())
                .and_then(direct_members)
                .into_iter().flatten()
                .filter_map(|member|member.name.as_ref())
                .filter(|member_name|member_name.val == name)
                .map(|member_name|member_name.span)
                .collect(),
            TypeUse::Anonymous(span) => self.symbol_info
                .anonymous_type_members.get(&span).into_iter().flatten()
                .filter(|member_name|member_name.val == name)
                .map(|member_name|member_name.span)
                .collect(),
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

    // Returns the method ref in the default chain that contains the loc, if any
    fn adjust_method_for_pos<'t>(method: &'t Arc<DMLMethodRef>,
                                 pos: &ZeroFilePosition)
                            -> Option<&'t Arc<DMLMethodRef>> {
        if !method.span().contains_pos(pos) {
            match method.get_default() {
                Some(DefaultCallReference::Valid(defmeth)) => {
                    return Self::adjust_method_for_pos(defmeth, pos);
                },
                Some(DefaultCallReference::Ambiguous(defmeths)) => {
                    // NOTE: We need to make results unique here, since diamond inheritance could cause us
                    // to hit the same method twice when adjusting
                    let mut res_make_unique = HashSet::new();
                    let mut res: Vec<_> = defmeths.iter()
                        .filter_map(|df|Self::adjust_method_for_pos(df, pos))
                        .filter(|m|res_make_unique.insert(m.location()))
                        .collect();
                    if res.len() > 1 {
                        internal_error!("Invariant broken: multiple methods contain the pos {:?} over method {:?} at {:?}\nhit methods are: {:?}",
                        pos, method.identity(), method.location(),
                        res.iter().map(|m|format!("{:?} at {:?}", m.identity(), m.location())).collect::<Vec<_>>());
                    }
                    return res.pop();
                },
                // NOTE: we would like to report this as an internal error somewhere, but it turns out to be too
                // costly to do so
                _ => {
                    return None;
                }
            }
        }
        Some(method)
    }

    fn resolve_simple_noderef_in_method<'c>(&'c self,
                                            parent_key: &StructureKey,
                                            meth: &Arc<DMLMethodRef>,
                                            node: &DMLString,
                                            _type_hint: Option<()>,
                                            method_structure: &HashMap
                                            <ZeroSpan, RangeEntry>,
                                            ref_matches: &mut ReferenceMatches) {
        // Context-to-obj will always give the most-overriding method, but our reference may actually
        // be from a scope in an overridden method. So here we adjust and find the correct method
        // by recursing to parent
        trace!("Resolving simple noderef {:?} in method {:?}", node, meth);
        let Some(adjusted_meth) = Self::adjust_method_for_pos(meth, &node.span().start_position())
        else {
            return;
        };
        trace!("After adjusting, looking up in {:?}", adjusted_meth);
        match node.val.as_str() {
            "this" =>
                ref_matches.add_match(Arc::clone(
                    self.symbol_info.object_symbols.get(
                        parent_key).unwrap())),
            "default" =>
            // NOTE: Here we match a default ref to the symbol of the method decl
            // that we directly overrode. Meaning that further lookups on that symbol
            // will be specialized for that overriding chain
                if let Some(defref) = adjusted_meth.get_default() {
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
                let local_sym = method_structure.get(adjusted_meth.location())
                    .and_then(|locals|locals.find_symbol_for_dmlname(node));
                let arg_sym = adjusted_meth.args().iter()
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

    // Handles 'templates.T' and 'templates.T.m', optionally prefixed by
    // 'this.'; returns false if 'node' is not of that form
    fn resolve_template_qualified(&self,
                                  parent_key: &StructureKey,
                                  node: &NodeRef,
                                  ref_matches: &mut ReferenceMatches) -> bool {
        fn is_templates(node: &NodeRef) -> bool {
            match node {
                NodeRef::Simple(name) => name.val == "templates",
                NodeRef::Sub(base, MemberOp::Dot, name, _) =>
                    name.val == "templates" && matches!(
                        base.as_ref(),
                        NodeRef::Simple(this) if this.val == "this"),
                _ => false,
            }
        }
        let (template_name, member) = match node {
            NodeRef::Sub(root, MemberOp::Dot, template, _)
                if is_templates(root) => (template, None),
            NodeRef::Sub(qualified, MemberOp::Dot, member, _) =>
                match qualified.as_ref() {
                    NodeRef::Sub(root, MemberOp::Dot, template, _)
                        if is_templates(root) => (template, Some(member)),
                    _ => return false,
                },
            _ => return false,
        };
        let Some(template) = self.templates.templates.get(&template_name.val)
        else {
            return false;
        };
        let Some(member) = member else {
            if let Some(symbol) = template.location.as_ref().and_then(
                |location|self.symbol_info.template_symbols.get(location)) {
                ref_matches.add_match(Arc::clone(symbol));
            }
            return true;
        };
        let Some(method) = self.objects.get(*parent_key)
            .and_then(|object|object.get_method(member.val.as_str())) else {
            return true;
        };
        // The implementation 'T' sees is the first one, in override order,
        // declared in 'T' or a template it instantiates
        let mut spans = vec![];
        let mut pending = vec![template];
        while let Some(next) = pending.pop() {
            if spans.contains(&next.spec.span) {
                continue;
            }
            spans.push(next.spec.span);
            pending.extend(next.spec.instantiations.values().flatten());
        }
        fn find<'m>(method: &'m Arc<DMLMethodRef>,
                    spans: &[ZeroSpan]) -> Option<&'m Arc<DMLMethodRef>> {
            if spans.iter().any(|span|span.contains_pos(
                &method.location().start_position())) {
                return Some(method);
            }
            match method.get_default() {
                Some(DefaultCallReference::Valid(next)) => find(next, spans),
                Some(DefaultCallReference::Ambiguous(nexts)) =>
                    nexts.iter().find_map(|next|find(next, spans)),
                _ => None,
            }
        }
        if let Some(symbol) = find(&method, &spans)
            .and_then(|found|self.get_method_symbol(found, parent_key)) {
            ref_matches.add_match(Arc::clone(symbol));
        }
        true
    }

    fn resolve_noderef_in_method<'c>(&'c self,
                                     parent_key: &StructureKey,
                                     method: &Arc<DMLMethodRef>,
                                     node: &NodeRef,
                                     method_structure: &HashMap<ZeroSpan,
                                                                RangeEntry>,
                                     ref_matches: &mut ReferenceMatches) {
        if self.resolve_template_qualified(parent_key, node, ref_matches) {
            return;
        }
        match node {
            NodeRef::Simple(simple) => {
                self.resolve_simple_noderef_in_method(parent_key, method, simple,
                    None,
                    method_structure,
                    ref_matches);
            },
            NodeRef::Sub(subnode, op, simple, _) => {
                let (base, indices) = subnode.strip_indices();
                let mut intermediate_matches = ReferenceMatches::new();
                self.resolve_noderef_in_method(parent_key,
                                               method,
                                               base,
                                               method_structure,
                                               &mut intermediate_matches);
                if let Some(syms) = intermediate_matches.as_matches() {
                    for sym in syms {
                        self.resolve_noderef_in_symbol(
                            &sym,
                            simple,
                            indices,
                            op,
                            method_structure,
                            ref_matches);
                    }
                } else {
                    ref_matches.merge_with(intermediate_matches);
                }
            },
            NodeRef::Index(inner) => self.resolve_noderef_in_method(
                parent_key, method, inner, method_structure, ref_matches),
        }
    }

    fn resolve_noderef_in_obj<'c>(&'c self,
                                  obj: &DMLObject,
                                  node: &NodeRef,
                                  method_structure: &HashMap<ZeroSpan,
                                                             RangeEntry>,
                                  ref_matches: &mut ReferenceMatches) {
        trace!("Resolving noderef {:?} in obj {:?}", node, obj);
        let parent_key = match obj.resolve(&self.objects) {
            DMLResolvedObject::CompObject(object) => object.key,
            DMLResolvedObject::ShallowObject(shallow) => shallow.parent,
        };
        if self.resolve_template_qualified(&parent_key, node, ref_matches) {
            return;
        }
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
            NodeRef::Sub(subnode, op, simple, _) => {
                let (base, indices) = subnode.strip_indices();
                let mut intermediate_matches = ReferenceMatches::new();
                self.resolve_noderef_in_obj(obj,
                                            base,
                                            method_structure,
                                            &mut intermediate_matches);

                if let Some(syms) = intermediate_matches.as_matches() {
                    for sym in syms {
                        self.resolve_noderef_in_symbol(
                            &sym,
                            simple,
                            indices,
                            op,
                            method_structure,
                            ref_matches);
                    }
                } else {
                    ref_matches.merge_with(intermediate_matches);
                }
            },
            NodeRef::Index(inner) => self.resolve_noderef_in_obj(
                obj, inner, method_structure, ref_matches),
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

    fn find_target_for_reference(
        &self,
        in_object: Option<&DMLObject>,
        reference: &VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>)
        -> ReferenceMatches {
        // NOTE: There was previously a check here to detect infinite recursion loops in reference
        // lookups, but it was discarded for being overly memory-intensive
        let index_key = (in_object, reference.clone());
        {
            if let Some(cached_result) = reference_cache.lock().unwrap()
                .get(index_key.clone(), &self.objects, method_structure) {
                    return cached_result.clone();
                }
        }
        let mut result = ReferenceMatches::new();
        self.find_target_for_reference_aux(in_object,
                                           reference,
                                           method_structure,
                                           reference_cache,
                                           &mut result);
        reference_cache.lock().unwrap().insert(index_key,
                                               result.clone(),
                                               &self.objects,
                                               method_structure);
        result
    }

    fn find_target_for_reference_aux<'t>(
        &'t self,
        in_object: Option<&DMLObject>,
        reference: &VariableReference,
        method_structure: &HashMap<ZeroSpan, RangeEntry>,
        reference_cache: &Mutex<ReferenceCache>,
        reference_matches: &mut ReferenceMatches) {
        if in_object.is_none() {
            // Nothing matches the noderef except maybe globals
            self.lookup_global_from_noderef(
                &reference.reference,
                reference_matches,
            );
            return;
        }

        let in_object = in_object.unwrap();

        self.lookup_ref_in_obj(in_object, reference, method_structure, reference_matches);

        if reference_matches.as_matches().is_none() {
            let parent = in_object.resolve(&self.objects).parent().map(DMLObject::CompObject);
            let sub_matches = self.find_target_for_reference(parent.as_ref(),
                                                             reference,
                                                             method_structure,
                                                             reference_cache);
            reference_matches.merge_with(sub_matches);
        }
    }

    fn handle_symbol_ref(symbol: &SymbolRef,
                         reference: &CodeReference) {
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
        resolved_method_calls: &Mutex<ResolvedMethodCallCandidates>,
        status: &AliveStatus) -> AnalysisProcessResult<()> {
        if scope_chain.is_empty() {
            internal_error!("Attempted to match references in empty scope chain");
            return Ok(());
        }
        let current_scope = scope_chain.last().unwrap();
        let context_chain: Vec<ContextKey> = scope_chain
            .iter().map(|s|s.create_context()).collect();
        // NOTE: Awkwardly, this will _not_ give the correct override level
        // for scopes that are overridden methods. So we will re-adjust
        // the method reference at later points (when caching and looking up
        // refs in method). This cannot be done at this level because we are
        // dealing with a concrete object, and not a method reference.
        let objects_of_scope =
            if context_chain.len() == 1 {
                // Must be device object
                vec![self.get_device_obj().clone()]
            } else {
                match self.context_to_objs(
                    self.get_device_comp_obj(),
                    // Skip first context, it's the device one
                    &context_chain[1..],
                    // Intentionally avoid storing limitations
                    &mut HashSet::new()) {
                        Ok(objs) => objs,
                        Err(e) => {
                            internal_error!("Failed to resolve context chain {:?}: {:?}",
                                            context_chain, e);
                            return Ok(());
                        }
                    }
            };
        // NOTE: chunk number is arbitrarily picked that benches well
        let errs: AnalysisProcessResult<Vec<_>> = current_scope.defined_references().par_chunks(25).
            try_fold(|| Vec::new(),
            |mut local_reports, references|{
                status.check_alive()?;
                for reference in references {
                    // Non-coderef references will be resolved on-demand as requests are made
                    if let Some(code_ref) = reference.as_code_ref() {
                        for object in &objects_of_scope {
                            trace!("In {:?}, Matching {:?}", object, reference);
                            let symbol_lookup = match &code_ref.variant {
                                ReferenceVariant::Variable(var) => self.find_target_for_reference(
                                    Some(object),
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
                                    Self::handle_symbol_ref(symbol, code_ref);
                                    let caller = match object {
                                        DMLObject::CompObject(caller) => *caller,
                                        DMLObject::ShallowObject(caller) =>
                                            caller.parent,
                                    };
                                    let symbol = symbol.lock().unwrap();
                                    let SymbolSource::Method(_, method) =
                                        &symbol.source
                                    else { continue; };
                                    resolved_method_calls.lock().unwrap()
                                        .entry((caller, *code_ref.loc_span()))
                                        .or_default().push(Arc::clone(method));
                                },
                            ReferenceMatchKind::MismatchedFind =>
                                //TODO: report mismatch,
                                (),
                            }
                            local_reports.extend(symbol_lookup.messages);
                        }
                    }
                }
                Ok(local_reports)
        }).try_reduce(Vec::new, |mut vec1, vec2|{ vec1.extend(vec2); Ok(vec1) });
        report.extend(errs?);
        Ok(())
    }
}

pub fn parse_file(path: &Path, file: FileSpec<'_>)
                  -> Result<(parsing::structure::TopAst,
                             ProvisionalsManager,
                             Vec<DMLError>), VFSError>
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
               -> AnalysisProcessResult<IsolatedAnalysis> {
        trace!("local analysis: {} at {}", path.as_str(), path.as_str());
        status.check_alive()?;
        let filespec = FileSpec {
            path, file: &file
        };
        let (mut ast, provisionals, mut errors) = parse_file(path, filespec)?;
        status.check_alive()?;
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
        status.check_alive()?;
        // sanity, clientpath and path should be the same file
        if CanonPath::from_path_buf(clientpath.clone()).is_none_or(
            |cp|&cp != path) {
            error!("Clientpath did not describe the same \
                    file as the actual path; {:?} vs {:?}",
                   path,
                   clientpath);
            return Err(VFSError::InternalError(
                "Clientpath did not describe the same file as path").into());
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

    fn get_active_imports(&self) -> Vec<&ObjectDecl<Import>> {
        let mut eval_context = EvaluationContext::new();
        self.toplevel.spec.imports.iter()
        // We intentionally discard errors here, they will have been reported earlier
        .filter(|imp|imp.cond.exists_no_report(&mut eval_context))
        .collect()
    }

    pub fn get_imports(&self) -> impl Iterator<Item = &ObjectDecl<Import>> {
        self.get_active_imports().into_iter()
    }

    pub fn get_import_names(&self) -> Vec<PathBuf> {
        self.get_imports().map(
            |imp|deconstruct_import(imp)).collect()
    }

    pub fn resolve_imports(&self,
                           resolver: &PathResolver,
                           context: Option<&CanonPath>)
                           -> ResolvedImports
    {
        let mut found = HashSet::default();
        let mut missing = HashSet::default();
        let import_paths = self.get_imports()
            .map(|i|(deconstruct_import(i), i.clone()));
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
                      types: &TypeDatabase,
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
                                            types,
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
                         templates: &TemplateTraitInfo,
                         types: &TypeDatabase) {
    for template in templates.templates.values() {
        if let Some(new_templ) = template_to_symbol(maker, template) {
            let loc = new_templ.lock().unwrap().loc;
            let typed = types
                .typedefs_named(&template.name)
                .iter()
                .find(|type_id|types.declaration(**type_id)
                    .is_some_and(|declaration|declaration.underlying.is_none()))
                    .map(|type_id|TypeUse::Declared(*type_id));
            new_templ.lock().unwrap().typed = typed;
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

fn extend_with_loggroups(maker: &SymbolMaker,
                         storage: &mut SymbolStorage,
                         bases: &[IsolatedAnalysis]) {
    for loggroup in bases.iter()
        .flat_map(|base|&base.toplevel.loggroups) {
        let name = &loggroup.obj.name;
        storage.loggroup_symbols.insert(name.span, symbol_ref!(
            maker,
            name.span,
            DMLSymbolKind::Loggroup,
            SymbolSource::Loggroup(name.clone()),
            definitions = vec![name.span],
            declarations = vec![name.span]
        ));
    }
}

// Later declarations are reported as collisions unless their condition
// excludes every earlier one with the same name
fn collect_loggroups(bases: &[IsolatedAnalysis],
                     errors: &mut Vec<DMLError>)
                     -> HashMap<String, Vec<ObjectDecl<Loggroup>>> {
    let mut loggroups: Vec<&ObjectDecl<Loggroup>> = bases.iter()
        .flat_map(|base|&base.toplevel.loggroups).collect();
    loggroups.sort_by_key(|loggroup|loggroup.obj.name.span);
    let mut unique: HashMap<String, Vec<ObjectDecl<Loggroup>>>
        = HashMap::default();
    for loggroup in loggroups {
        let name = &loggroup.obj.name;
        let previous = unique.entry(name.val.clone()).or_default();
        if let Some(collision) = previous.iter().find(
            |other|!loggroup.cond.guaranteed_excluded_from(&other.cond)) {
            errors.push(DMLError {
                span: name.span,
                description: format!("Duplicate loggroup name; '{}'",
                                     name.val),
                severity: Some(DiagnosticSeverity::ERROR),
                related: vec![(collision.obj.name.span,
                               "Previously defined here".to_string())],
            });
        } else {
            previous.push(loggroup.clone());
        }
    }
    unique
}

fn extend_with_types(maker: &SymbolMaker,
                     storage: &mut SymbolStorage,
                     types: &TypeDatabase) {
    for declaration in types.declarations() {
        let Some(underlying) = declaration.underlying.as_ref() else {
            continue;
        };
        let symbol = symbol_ref!(
            maker,
            declaration.name.span,
            DMLSymbolKind::Typedef,
            SymbolSource::Type(declaration.id),
            definitions = vec![declaration.name.span],
            declarations = vec![declaration.name.span]
        );
        symbol.lock().unwrap().typed = Some(TypeUse::Declared(declaration.id));
        storage.typedef_symbols.insert(declaration.id, symbol);
        match direct_members(underlying) {
            Some(members) => add_member_symbols(
                maker, types, TypeUse::Declared(declaration.id), members,
                &mut storage.anonymous_type_members,
                &mut storage.type_member_symbols),
            None => register_anonymous_types(
                maker, types, underlying,
                &mut storage.anonymous_type_members,
                &mut storage.type_member_symbols),
        }
    }
}

fn set_symbol_type(symbol: &SymbolRef, types: &TypeDatabase, typed: &DMLType) {
    let shape = types.resolve_type_shape(typed);
    let mut symbol = symbol.lock().unwrap();
    symbol.typed = shape.map(|(type_use, _)|type_use);
    symbol.indirections = shape.map_or(0, |(_, layers)|layers);
    symbol.typeof_declared = types.is_typeof_shape(typed);
}

fn add_member_symbols(maker: &SymbolMaker,
                      types: &TypeDatabase,
                      owner: TypeUse,
                      members: Vec<&DMLTypeMember>,
                      anonymous: &mut HashMap<ZeroSpan, Vec<DMLString>>,
                      fields: &mut HashMap<ZeroSpan, SymbolRef>) {
    for member in members {
        if let Some(name) = &member.name {
            fields.entry(name.span).or_insert_with(|| {
                let member_symbol = symbol_ref!(
                    maker,
                    name.span,
                    DMLSymbolKind::Field,
                    SymbolSource::TypeMember(owner, name.span),
                    definitions = vec![name.span],
                    declarations = vec![name.span]
                );
                set_symbol_type(&member_symbol, types, &member.typed);
                member_symbol
            });
        }
        register_anonymous_types(maker, types, &member.typed,
                                 anonymous, fields);
    }
}

// Creates field symbols for every anonymous struct-like type within 'typed'
fn register_anonymous_types(maker: &SymbolMaker,
                            types: &TypeDatabase,
                            typed: &DMLType,
                            anonymous: &mut HashMap<ZeroSpan, Vec<DMLString>>,
                            fields: &mut HashMap<ZeroSpan, SymbolRef>) {
    if let Some(members) = direct_members(typed) {
        if anonymous.contains_key(&typed.span) {
            return;
        }
        anonymous.insert(typed.span, members.iter()
            .filter_map(|member|member.name.clone()).collect());
        add_member_symbols(maker, types, TypeUse::Anonymous(typed.span),
                           members, anonymous, fields);
        return;
    }
    match &typed.kind {
        DMLTypeKind::Const(inner) |
        DMLTypeKind::Pointer(inner) |
        DMLTypeKind::Vector(inner) =>
            register_anonymous_types(maker, types, inner, anonymous, fields),
        DMLTypeKind::Array { element, .. } =>
            register_anonymous_types(maker, types, element, anonymous, fields),
        DMLTypeKind::Function { returns, parameters, .. } => {
            register_anonymous_types(maker, types, returns, anonymous, fields);
            for parameter in parameters {
                register_anonymous_types(maker, types, parameter,
                                         anonymous, fields);
            }
        },
        DMLTypeKind::Hook(arguments) =>
            for argument in arguments {
                register_anonymous_types(maker, types, argument,
                                         anonymous, fields);
            },
        _ => (),
    }
}

fn populate_symbol_types(maker: &SymbolMaker,
                         storage: &mut SymbolStorage,
                         types: &TypeDatabase) {
    let mut anonymous = HashMap::default();
    let mut fields = HashMap::default();
    for symbol in storage.all_symbols() {
        let mut symbol = symbol.lock().unwrap();
        if symbol.typed.is_some() {
            continue;
        }
        let type_syntax = match &symbol.source {
            SymbolSource::DMLObject(DMLObject::ShallowObject(object)) =>
                match &object.variant {
                    DMLShallowObjectVariant::Session(variable) |
                    DMLShallowObjectVariant::Saved(variable) =>
                        Some(&variable.declaration.type_syntax),
                    DMLShallowObjectVariant::Extern(external) =>
                        Some(&external.get_likely_definition().var.typed),
                    // NOTE: Outside shared contexts the assigned value is
                    // known, so once constant propagation is in place member
                    // lookups should follow the value rather than the type
                    DMLShallowObjectVariant::Parameter(parameter) =>
                        parameter.used_definitions.iter()
                            .chain(&parameter.definitions)
                            .chain(&parameter.declarations)
                            .find_map(|(_, declaration)|
                                declaration.typed.as_ref()),
                    _ => None,
                },
            SymbolSource::MethodArg(method, name) => method.args().iter()
                .find(|argument|argument.name().val == name.val)
                .and_then(|argument|match argument {
                    DMLMethodArg::Typed(declaration) =>
                        Some(&declaration.type_syntax),
                    DMLMethodArg::Inline(_) => None,
                }),
            _ => None,
        };
        // Symbols without syntax here (e.g. locals) were typed on creation
        let Some(type_syntax) = type_syntax else {
            continue;
        };
        let shape = types.resolve_type_shape(type_syntax);
        let typeof_declared = types.is_typeof_shape(type_syntax);
        register_anonymous_types(maker, types, type_syntax,
                                 &mut anonymous, &mut fields);
        symbol.typed = shape.map(|(type_use, _)|type_use);
        symbol.indirections = shape.map_or(0, |(_, layers)|layers);
        symbol.typeof_declared = typeof_declared;
    }
    for (span, members) in anonymous {
        storage.anonymous_type_members.entry(span).or_insert(members);
    }
    for (span, field) in fields {
        storage.type_member_symbols.entry(span).or_insert(field);
    }
}

fn new_symbol_from_object(maker: &SymbolMaker,
                          object: &DMLCompositeObject) -> SymbolRef {
    let all_decl_defs = &object.all_decls;
    symbol_ref!(
        maker,
        object.declloc,
        DMLSymbolKind::CompObject(object.kind),
        SymbolSource::DMLObject(DMLObject::CompObject(object.key)),
        definitions = all_decl_defs.clone(),
        declarations = all_decl_defs.clone(),
        implementations = object.used_ineach_locs.clone().into_iter().collect(),
        // TODO: this does not follow from the new definition of bases
        bases = all_decl_defs.clone())
}

fn bind_interface_implementations(storage: &SymbolStorage,
                                  objects: &StructureContainer,
                                  types: &TypeDatabase) {
    for object in objects.values()
        .filter(|object|object.kind == CompObjectKind::Implement) {
        let interface_name = format!("{}_interface_t", object.identity.val);
        let type_ids = types.typedefs_named(&interface_name).iter()
            .filter(|type_id|types.declaration(**type_id)
                .is_some_and(|declaration|declaration.underlying.is_some()))
            .copied().collect::<Vec<_>>();
        let [type_id] = type_ids.as_slice() else {
            continue;
        };
        let Some(declaration) = types.declaration(*type_id) else {
            continue;
        };
        let Some(typedef_symbol) = storage.typedef_symbols.get(type_id) else {
            continue;
        };
        let Some(object_symbol) = storage.object_symbols.get(&object.key) else {
            continue;
        };

        object_symbol.lock().unwrap().type_definitions
            .insert(declaration.declaration_span);
        {
            let mut symbol = typedef_symbol.lock().unwrap();
            symbol.references.insert(object.declloc);
            symbol.implementations.insert(object.declloc);
        }

        let Some(underlying) = declaration.underlying.as_ref() else {
            continue;
        };
        for member in underlying.declaration_members() {
            let Some(member_name) = &member.name else {
                continue;
            };
            let Some(method) = object.get_method(member_name.val.as_str()) else {
                continue;
            };
            let Some(method_symbol) = storage.method_symbols
                .get(method.location())
                .and_then(|symbols|symbols.get(&object.key)) else {
                continue;
            };
            let Some(member_symbol) = storage.type_member_symbols
                .get(&member_name.span) else {
                continue;
            };
            {
                let mut symbol = method_symbol.lock().unwrap();
                symbol.bases = vec![member_name.span];
                symbol.declarations = vec![member_name.span];
            }
            member_symbol.lock().unwrap().implementations
                .insert(*method.location());
        }
    }
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
    // NOTE: We should not need these equivalence comparisons, once symbol
    // creation is properly guided by the structural AST this code can be
    // probably be simplified and the comparisons discarded
    let mut conflicting = false;
    map.entry(key.clone()).and_modify(|stored| {
        if Arc::ptr_eq(stored, &val) {
            return;
        }
        let old = std::mem::replace(stored, Arc::clone(&val));
        // Compare declaration equivalence, ignoring structure keys and metadata.
        if !old.lock().unwrap().equivalent(&stored.lock().unwrap()) {
            internal_error!(
                "Overwrote previous symbol {:?} at {:?} with non-similar symbol {:?}",
                old, key, stored);
            conflicting = true;
        }
    }).or_insert(val);
    conflicting
}

#[allow(clippy::ptr_arg)]
fn add_new_symbol_from_shallow(maker: &SymbolMaker,
                               shallow: &DMLShallowObject,
                               types: &TypeDatabase,
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
            return add_new_symbol_from_method(maker, &shallow.parent, method_ref, types, errors, storage, method_structure),
        DMLShallowObjectVariant::Constant(constant) =>
            (vec![*constant.loc_span()],
             vec![*constant.loc_span()],
             vec![*constant.loc_span()]),
        DMLShallowObjectVariant::Extern(e) => {
            let mut locations: Vec<_> = e.used_definitions.iter()
                .map(|(_, def)|*def.loc_span()).collect();
            locations.sort();
            locations.dedup();
            (locations.clone(), locations.clone(), locations)
        },
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
            DMLObject::ShallowObject(Box::new(shallow.clone()))),
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
        DMLShallowObjectVariant::Extern(_) |
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
                            types: &TypeDatabase,
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
                                     types,
                                     errors,
                                     storage,
                                     &mut entry);
    }
    // Top-level locals share their block with the method arguments
    for argument in method.args() {
        if let Some(local) = entry.symbols.get(&argument.name().val) {
            errors.push(DMLError {
                span: local.lock().unwrap().loc,
                description: format!("Duplicate definition of variable '{}'",
                                     argument.name().val),
                related: vec![(*argument.loc_span(),
                               "Previously defined here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
    }
    if !entry.is_empty() {
        method_structure.insert(
            *method.location(),
            entry);
    }
}

fn add_new_method_scope_symbol<T>(maker: &SymbolMaker,
                                  method: &Arc<DMLMethodRef>,
                                  sym: &T,
                                  typ: &DMLType,
                                  types: &TypeDatabase,
                                  errors: &mut Vec<DMLError>,
                                  storage: &mut SymbolStorage,
                                  scope: &mut RangeEntry)
where
    T : StructureSymbol + DMLNamed + LocationSpan
{
    if let Some(previous) = scope.symbols.get(&sym.name().val) {
        errors.push(DMLError {
            span: *sym.loc_span(),
            description: format!("Duplicate definition of variable '{}'",
                                 sym.name().val),
            related: vec![(previous.lock().unwrap().loc,
                           "Previously defined here".to_string())],
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }
    let symbol = symbol_ref!(
        maker,
        *sym.loc_span(),
        sym.kind(),
        SymbolSource::MethodLocal(Arc::clone(method), sym.name().clone()),
        definitions = vec![*sym.loc_span()],
        declarations = vec![*sym.loc_span()]
    );
    set_symbol_type(&symbol, types, typ);
    register_anonymous_types(maker, types, typ,
                             &mut storage.anonymous_type_members,
                             &mut storage.type_member_symbols);
    scope.symbols.insert(sym.name().val.clone(), Arc::clone(&symbol));
    storage.variable_symbols.insert(*sym.loc_span(), symbol);
}

fn enter_new_method_scope(maker: &SymbolMaker,
                          method: &Arc<DMLMethodRef>,
                          outer_stmnt_desc: &'static str,
                          stmnt: &Statement,
                          scope_span: &ZeroSpan,
                          types: &TypeDatabase,
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
                                 types,
                                 errors,
                                 storage,
                                 &mut entry);
    scope.sub_ranges.push(entry);
}

// 'each T in (...)' yields values of template type 'T'; other lists can be
// heterogeneous, so their elements are left untyped
fn iteration_variable_type(list: &Expression, fallback: ZeroSpan) -> DMLType {
    match list.as_ref() {
        ExpressionKind::EachIn(each) => DMLType {
            kind: DMLTypeKind::Named(each.templ.clone()),
            span: each.templ.span,
        },
        _ => DMLType::unknown(fallback),
    }
}

fn add_switch_case_symbols(maker: &SymbolMaker,
                           method: &Arc<DMLMethodRef>,
                           cases: &[SwitchCase],
                           types: &TypeDatabase,
                           errors: &mut Vec<DMLError>,
                           storage: &mut SymbolStorage,
                           scope: &mut RangeEntry) {
    for case in cases {
        match case {
            SwitchCase::Statement(statement) =>
                if matches!(statement.as_ref(), StatementKind::Compound(_)) {
                    enter_new_method_scope(maker, method, "block", statement,
                                           statement.span(), types, errors,
                                           storage, scope);
                } else {
                    add_new_method_scope_symbols(maker, method, statement,
                                                 types, errors, storage, scope);
                },
            // Each '#if' branch is a block of its own
            SwitchCase::HashIf(hashif) =>
                for branch in [&hashif.truecases, &hashif.falsecases] {
                    let Some(range) = branch.iter()
                        .map(|case|case.span().range)
                        .reduce(ZeroRange::combine) else {
                        continue;
                    };
                    let mut entry = RangeEntry {
                        range,
                        symbols: HashMap::default(),
                        sub_ranges: vec![],
                    };
                    add_switch_case_symbols(maker, method, branch, types,
                                            errors, storage, &mut entry);
                    scope.sub_ranges.push(entry);
                },
            SwitchCase::Case(_) | SwitchCase::Default(_) => (),
        }
    }
}

fn add_new_method_scope_symbols(maker: &SymbolMaker,
                                method: &Arc<DMLMethodRef>,
                                stmnt: &Statement,
                                types: &TypeDatabase,
                                errors: &mut Vec<DMLError>,
                                storage: &mut SymbolStorage,
                                scope: &mut RangeEntry) {
    match &**stmnt {
        StatementKind::Compound(content) =>
            for sub_stmnt in &content.statements {
                if matches!(sub_stmnt.as_ref(), StatementKind::Compound(_)) {
                    enter_new_method_scope(maker,
                                           method,
                                           "block",
                                           sub_stmnt,
                                           sub_stmnt.span(),
                                           types,
                                           errors,
                                           storage,
                                           scope);
                } else {
                    add_new_method_scope_symbols(maker,
                                                 method,
                                                 sub_stmnt,
                                                 types,
                                                 errors,
                                                 storage,
                                                 scope);
                }
            },
        // DMLC declares locals of a switch body in the enclosing block, so
        // they remain visible after the switch
        StatementKind::Switch(content) =>
            add_switch_case_symbols(maker, method, &content.cases, types,
                                    errors, storage, scope),
        StatementKind::If(content) => {
            enter_new_method_scope(maker,
                                   method,
                                   "if",
                                   &content.ifbody,
                                   content.ifbody.span(),
                                   types,
                                   errors,
                                   storage,
                                   scope);
            if let Some(elsebody) = &content.elsebody {
                enter_new_method_scope(maker,
                                       method,
                                       "else",
                                       elsebody,
                                       elsebody.span(),
                                       types,
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
                                   types,
                                   errors,
                                   storage,
                                   scope);
            if let Some(elsebody) = &content.elsebody {
                enter_new_method_scope(maker,
                                       method,
                                       "#else",
                                       elsebody,
                                       elsebody.span(),
                                       types,
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
                                   types,
                                   errors,
                                   storage,
                                   scope),
        StatementKind::DoWhile(content) =>
            enter_new_method_scope(maker,
                                   method,
                                   "do-while",
                                   &content.body,
                                   content.body.span(),
                                   types,
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
                                                types,
                                                errors,
                                                storage,
                                                &mut entry);
                }
            }
            enter_new_method_scope(maker,
                                   method,
                                   "for",
                                   &content.body,
                                   content.body.span(),
                                   types,
                                   errors,
                                   storage,
                                   &mut entry);
            scope.sub_ranges.push(entry);
        },
        StatementKind::ForEach(content) => {
            let mut entry = RangeEntry {
                range: content.span().range,
                symbols: HashMap::default(),
                sub_ranges: vec![],
            };
            let identifier_type = iteration_variable_type(
                &content.inexpr, *content.identifier.loc_span());
            add_new_method_scope_symbol(maker,
                                        method,
                                        &content.identifier,
                                        &identifier_type,
                                        types,
                                        errors,
                                        storage,
                                        &mut entry);
            enter_new_method_scope(maker,
                                   method,
                                   "foreach",
                                   &content.body,
                                   content.body.span(),
                                   types,
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
            let identifier_type = iteration_variable_type(
                &content.inexpr, *content.ident.loc_span());
            add_new_method_scope_symbol(maker,
                                        method,
                                        &content.ident,
                                        &identifier_type,
                                        types,
                                        errors,
                                        storage,
                                        &mut entry);
            enter_new_method_scope(maker,
                                   method,
                                   "select",
                                   &content.selectbranch,
                                   content.selectbranch.span(),
                                   types,
                                   errors,
                                   storage,
                                   &mut entry);
            scope.sub_ranges.push(entry);
            enter_new_method_scope(maker,
                                   method,
                                   "select-else",
                                   &content.elsebranch,
                                   content.elsebranch.span(),
                                   types,
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
                                   types,
                                   errors,
                                   storage,
                                   scope);
            enter_new_method_scope(maker,
                                   method,
                                   "catch",
                                   &content.catchblock,
                                   content.catchblock.span(),
                                   types,
                                   errors,
                                   storage,
                                   scope);
        },
        // Message component names are only in scope in the call
        StatementKind::After(content) => {
            let mut entry = RangeEntry {
                range: content.call.span().range,
                symbols: HashMap::default(),
                sub_ranges: vec![],
            };
            for component in content.message_components() {
                let unknown_type = DMLType::unknown(*component.loc_span());
                add_new_method_scope_symbol(maker,
                                            method,
                                            component,
                                            &unknown_type,
                                            types,
                                            errors,
                                            storage,
                                            &mut entry);
            }
            if !entry.symbols.is_empty() {
                scope.sub_ranges.push(entry);
            }
        },
        StatementKind::VariableDecl(content) =>
            for decl in &content.vars {
                add_new_method_scope_symbol(
                    maker,
                    method,
                    decl,
                    &decl.typed,
                    types,
                    errors,
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
                             imp_map: &HashMap<Import, CanonPath>,
                             eval_context: &mut EvaluationContext,
                             types: &TypeDatabase,
                             errors: &mut Vec<DMLError>)
                             -> TemplateTraitInfo {
        info!("Rank templates");
        let (templates, order, invalid_isimps, rank_struct)
            = rank_templates(unique_templates, files, imp_map,
                             eval_context, errors);
        info!("Templates+traits");
        create_templates_traits(
            start_of_file,
            rank_maker, templates, order,
            invalid_isimps, imp_map, rank_struct, eval_context, types, errors)
    }

    fn match_references(&mut self,
                        bases: &Vec<IsolatedAnalysis>,
                        method_structure: &HashMap<ZeroSpan, RangeEntry>,
                        errors: &mut Vec<DMLError>,
                        status: &AliveStatus) -> AnalysisProcessResult<()> {
        info!("Match references");
        let reference_cache: Mutex<ReferenceCache> = Mutex::new(
            ReferenceCache::new(self.reference_cache_max_size)
        );
        let resolved_method_calls = Mutex::new(HashMap::new());
        for scope_chain in all_scopes(bases) {
            debug!("Got scope at {:?}", scope_chain.last()
                   .map(|s|s.span().start_position()));
            self.match_references_in_scope(scope_chain,
                                           errors,
                                           method_structure,
                                           &reference_cache,
                                           &resolved_method_calls,
                                           status)?;
        }
        for ((object, reference), methods) in
            resolved_method_calls.into_inner().unwrap()
        {
            let Some(method) = methods.first() else { continue; };
            if methods.iter().all(|candidate|
                candidate.location() == method.location())
            {
                if let Some(object) = self.objects.get_mut(object) {
                    object.resolved_method_calls.insert(
                        reference, Arc::clone(method));
                }
            }
        }
        Ok(())
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
               imp_map: HashMap<Import, CanonPath>,
               device_job_options: DeviceAnalysisJobOptions,
               status: AliveStatus)
               -> AnalysisProcessResult<DeviceAnalysis> {
        info!("device analysis: {:?}", root.path);
        status.check_alive()?;

        if root.toplevel.device.is_none() {
            return Err(VFSError::InternalError(
                "Attempted to device analyze a file without device decl").into());
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

        let mut base_templates = templates_from_device_and_bases(&root, &bases);
        // Bases arrive in arbitrary order; earliest span wins among duplicates
        base_templates.sort_by_key(|template|*template.obj.object.loc_span());

        trace!("base template names: {:?}", base_templates.iter()
               .fold("".to_string(),
                     |mut s, odecl|{
                         write!(s, "{}, ", odecl.obj.object.name.val)
                             .expect("write string error");
                         s
                     }));
        status.check_alive()?;
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
                    span: *template.obj.object.loc_span(),
                    description: format!("Duplicate template name; '{}'", name),
                    severity: Some(DiagnosticSeverity::ERROR),
                    related: vec![(
                        *unique_templates.get(name.as_str()).unwrap()
                            .obj.object.loc_span(),
                        "Previously defined here".to_string()
                    )],
                });
            } else {
                unique_templates.insert(name.as_str(), template);
            }
        }

        let mut types = TypeDatabase::default();
        report_global_name_collisions(&bases, &mut errors);
        let loggroups = collect_loggroups(&bases, &mut errors);
        for base in &bases {
            for typedef in &base.toplevel.typedefs {
                let name = typedef.object.name.val.as_str();
                if let Some(previous_span) = types.typedefs_named(name).first()
                    .and_then(|id|types.declaration(*id))
                    .map(|decl|decl.name.span) {
                    errors.push(DMLError {
                        span: typedef.object.name.span,
                        description: format!("Duplicate typedef name; '{}'", name),
                        severity: Some(DiagnosticSeverity::ERROR),
                        related: vec![(previous_span,
                                       "Previously defined here".to_string())],
                    });
                }
                if let Some(template) = unique_templates.get(name) {
                    errors.push(DMLError {
                        span: typedef.object.name.span,
                        description: format!(
                            "Typedef name '{}' collides with a template name",
                            name),
                        severity: Some(DiagnosticSeverity::ERROR),
                        related: vec![(*template.obj.object.loc_span(),
                                       "Template defined here".to_string())],
                    });
                }
                types.add_typedef(typedef);
            }
        }
        for template in unique_templates.values() {
            types.add_template(template.obj.object.name.clone());
        }
        for base in &bases {
            for typedef in &base.toplevel.typedefs {
                types.report_unknown_types(&typedef.typed, &mut errors);
            }
            report_unknown_declared_types(
                &base.toplevel.spec, &types, &mut EvaluationContext::new(),
                &mut errors);
        }

        // Add files-as-templates
        let mut files: HashMap<&str, &TopLevel> = HashMap::default();
        for base in &bases {
            files.insert(base.path.to_str().unwrap(),
                         &base.toplevel);
            trace!("Tracked file {} as template",
                   base.path.to_str().unwrap_or("no path"));
        }
        status.check_alive()?;
        let mut rank_maker = RankMaker::new();
        let mut eval_context = EvaluationContext::new();
        let tt_info = Self::make_templates_traits(&root.toplevel.start_of_file,
                                                  &mut rank_maker,
                                                  &unique_templates,
                                                  &files,
                                                  &imp_map,
                              &mut eval_context,
                                                  &types,
                                                  &mut errors);
        status.check_alive()?;
        // TODO: catch typedef/traitname overlaps

        // TODO: this is where we would do type resolution
        let mut container = StructureContainer::default();
        info!("Make device");
        let device_key = make_device(&root.path, &root.toplevel,
                                     &tt_info, imp_map, &mut container,
                                     &mut rank_maker, &mut eval_context, &types,
                                     &mut errors).key;
        status.check_alive()?;
        // maps template declaration loc to objects
        let template_object_implementation_map =
            Self::template_object_map(&tt_info, &container);
        status.check_alive()?;
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
                                                 &types,
                                                 &mut errors,
                                                 &mut method_structure);
        // This needs to be done after all symbols are created, because method
        // symbol order is not correlated to the object iteration order
        bind_method_implementations(&mut symbol_info.method_symbols);

        status.check_alive()?;

        extend_with_templates(&maker, &mut symbol_info, &tt_info, &types);
        extend_with_types(&maker, &mut symbol_info, &types);
        extend_with_loggroups(&maker, &mut symbol_info, &bases);
        populate_symbol_types(&maker, &mut symbol_info, &types);
        bind_interface_implementations(&symbol_info, &container, &types);
        let mut device = DeviceAnalysis {
            name: root.toplevel.device.unwrap().name.val,
            errors: HashMap::default(),
            objects: container,
            device_obj: DMLObject::CompObject(device_key),
            templates: tt_info,
            types,
            loggroups,
            symbol_info,
            reference_info: ReferenceStorage::default(),
            template_object_implementation_map,
            conditional_declarations: conditional_declarations(&bases),
            path: root.path.clone(),
            clientpath: root.path.clone().into(),
            dependant_files: bases.iter().map(|b|&b.path).cloned().collect(),
            reference_cache_max_size: device_job_options.max_reference_cache_size,
        };
        status.check_alive()?;
        device.match_references(&bases,
                                &method_structure,
                                &mut errors,
                                &status)?;
        check_method_body_types_for_objects(
            &device.objects, &device.types, &mut errors);

        // NOTE: This is when we previously pre-calculated the ref->symbol map,
        // however the up-front analysis cost of this was too heavy
        // device.inverse_references();

        info!("Invariant check");
        for obj in device.objects.values() {
            device.param_invariants(obj, &mut errors);
            device.report_subobject_name_collisions(obj, &mut errors);
        }

        // Object-level checks repeat for every instance of a template
        let mut seen = HashSet::new();
        for error in drop_repeated_collisions(errors) {
            if !seen.insert(error.clone()) {
                continue;
            }
            device.errors.entry(error.span.path())
                .or_default()
                .push(error);
        }
        trace!("Errors are {:?}", device.errors);
        status.check_alive()?;
        info!("Done with device");
        Ok(device)
    }

    // A 'name' parameter can make subobjects collide (DMLC ENAMECOLL)
    fn report_subobject_name_collisions(&self,
                                        obj: &DMLCompositeObject,
                                        report: &mut Vec<DMLError>) {
        let mut children: Vec<_> = obj.components.iter()
            .filter_map(|(identity, component)|
                        component.resolve(&self.objects).as_comp()
                        .map(|child|(identity, child)))
            .collect();
        children.sort_by_key(|(identity, _)|identity.as_str());
        let mut name_sites: HashMap<String, ZeroSpan> = HashMap::default();
        for (identity, child) in children {
            let renamed = child.get_param("name")
                .and_then(|param|param.get_unambiguous_def())
                .and_then(|def|match def.value.as_ref() {
                    Some(ParamValue::Set(expression)) =>
                        match expression.as_ref() {
                            ExpressionKind::StringLiteral(name) =>
                                Some((name.val.clone(), name.span)),
                            _ => None,
                        },
                    _ => None,
                });
            let (name, site) = renamed.unwrap_or_else(
                ||(identity.clone(), child.declloc));
            let previous = name_sites.get(&name)
                .map(|site|(*site, "Previously defined here"))
                .or_else(||(name == "dev" && obj.kind == CompObjectKind::Device)
                         .then_some((obj.declloc,
                                     "The device itself is logged as 'dev'")));
            if let Some((previous, label)) = previous {
                report.push(DMLError {
                    span: site,
                    description: format!("Name collision on '{}'", name),
                    related: vec![(previous, label.to_string())],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            } else {
                name_sites.insert(name, site);
            }
        }
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

// Checks data, hook, extern and typed parameter declarations, including those
// in templates that are never instantiated
// Export names share a device-global namespace (DMLC ENAMECOLL)
fn report_global_name_collisions(bases: &[IsolatedAnalysis],
                                 errors: &mut Vec<DMLError>) {
    fn report(kind: &str,
              mut names: Vec<(DMLString, ExistCondition)>,
              errors: &mut Vec<DMLError>) {
        names.sort_by_key(|(name, _)|name.span);
        for (index, (name, cond)) in names.iter().enumerate() {
            if let Some((previous, _)) = names[..index].iter().find(
                |(other, other_cond)|other.val == name.val
                    && !cond.guaranteed_excluded_from(other_cond)) {
                errors.push(DMLError {
                    span: name.span,
                    description: format!("Duplicate {} name; '{}'",
                                         kind, name.val),
                    severity: Some(DiagnosticSeverity::ERROR),
                    related: vec![(previous.span,
                                   "Previously defined here".to_string())],
                });
            }
        }
    }
    report("export", bases.iter()
           .flat_map(|base|&base.toplevel.spec.exports)
           .filter_map(|export|match export.obj.name.as_ref() {
               ExpressionKind::StringLiteral(name) => Some((DMLString {
                   val: name.val.clone(),
                   span: name.span,
               }, export.cond.clone())),
               _ => None,
           })
           .collect(), errors);
}

fn report_unknown_declared_types(spec: &StatementSpec,
                                 types: &TypeDatabase,
                                 context: &mut EvaluationContext,
                                 errors: &mut Vec<DMLError>) {
    for external in &spec.externs {
        types.report_unknown_types(&external.var.typed, errors);
    }
    for variable in spec.sessions.iter().chain(&spec.saveds) {
        if variable.cond.exists_no_report(context) {
            for declaration in &variable.obj.vars {
                types.report_unknown_types(&declaration.typed, errors);
            }
        }
    }
    for hook in &spec.hooks {
        if hook.cond.exists_no_report(context) {
            for argument in &hook.obj.args {
                types.report_unknown_types(argument, errors);
            }
        }
    }
    for param in &spec.params {
        if let Some(typed) = param.obj.typed.as_ref()
            .filter(|_|param.cond.exists_no_report(context)) {
            types.report_unknown_types(typed, errors);
        }
    }
    for object in &spec.objects {
        if object.cond.exists_no_report(context) {
            report_unknown_declared_types(&object.spec, types, context, errors);
        }
    }
    for nested in spec.ineachs.iter().map(|ineach|(&ineach.cond, &ineach.spec))
        .chain(spec.templates.iter()
               .map(|template|(&template.cond, &template.spec))) {
        if nested.0.exists_no_report(context) {
            report_unknown_declared_types(nested.1, types, context, errors);
        }
    }
}

fn conditional_declarations(bases: &[IsolatedAnalysis]) -> HashSet<ZeroSpan> {
    fn gather(spec: &StatementSpec, found: &mut HashSet<ZeroSpan>) {
        for object in &spec.objects {
            if !matches!(object.cond, ExistCondition::Always) {
                found.insert(*object.loc_span());
            }
            gather(&object.spec, found);
        }
        for method in &spec.methods {
            if !matches!(method.cond, ExistCondition::Always) {
                found.insert(*method.loc_span());
            }
        }
        for ineach in &spec.ineachs {
            gather(&ineach.spec, found);
        }
        for template in &spec.templates {
            gather(&template.spec, found);
        }
    }
    let mut found = HashSet::default();
    for base in bases {
        gather(&base.toplevel.spec, &mut found);
    }
    found
}

// The same name clash can be found both when processing a template and when
// constructing an object, keep one report per set of clashing declarations
fn drop_repeated_collisions(errors: Vec<DMLError>) -> Vec<DMLError> {
    fn clashing_declarations(error: &DMLError) -> Option<Vec<ZeroSpan>> {
        if !error.description.starts_with("Name collision") {
            return None;
        }
        let mut spans: Vec<ZeroSpan> = std::iter::once(error.span)
            .chain(error.related.iter().map(|(span, _)|*span)).collect();
        spans.sort();
        spans.dedup();
        Some(spans)
    }
    let mut reported: Vec<Vec<ZeroSpan>> = vec![];
    let mut kept = vec![];
    let mut collisions = vec![];
    for error in errors {
        match clashing_declarations(&error) {
            Some(spans) => collisions.push((spans, error)),
            None => kept.push(error),
        }
    }
    // Widest reports first, so narrower repeats are recognized as covered
    collisions.sort_by_key(|(spans, _)|std::cmp::Reverse(spans.len()));
    for (spans, error) in collisions {
        if reported.iter().any(|covered|spans.iter().all(
            |span|covered.contains(span))) {
            continue;
        }
        reported.push(spans);
        kept.push(error);
    }
    kept

}

pub fn templates_from_device_and_bases<'a>(_device: &'a IsolatedAnalysis,
                                 bases: &'a [IsolatedAnalysis])
                                 -> Vec<&'a ObjectDecl<Template>> {
    // Need to be sure we get the device toplevel from device here, since
    // it has been modified a little from the original isolatedanalysis
    let toplevels: Vec<&'a TopLevel> =
        bases.iter().map(|ia|&ia.toplevel).collect();
    toplevels.into_iter().flat_map(|tl|tl.gather_templates()).collect()
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

// The current strategy for method symbols is:
// Create a symbol for each level of overriding for each object where the method
// is actualized. We then end up with many symbols for the same decl location,
// and leave it to requests to collect the aggregate information at the point
fn add_new_symbol_from_method(maker: &SymbolMaker, parent_obj_key: &StructureKey, method_ref: &Arc<DMLMethodRef>,
                              types: &TypeDatabase,
                              errors: &mut Vec<DMLError>, storage: &mut SymbolStorage, method_structure: &mut HashMap<ZeroSpan, RangeEntry>) {
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
        add_method_scope_symbols(maker, method_ref, types, method_structure, storage, errors);
        if let Some(defaults) = method_ref.get_default() {
            for default in defaults.flat_refs() {
                add_new_symbol_from_method(maker, parent_obj_key, default, types, errors, storage, method_structure);
            }
        }
    }
}

#[cfg(test)]
mod symbol_storage_tests {
    use super::*;
    use crate::analysis::structure::objects::{
        DMLObjectCommon, Extern, VariableDecl, VariableDeclKind,
    };
    use crate::analysis::templating::objects::DMLAmbiguousDef;

    fn symbol(maker: &SymbolMaker) -> SymbolRef {
        let location = ZeroSpan::invalid("symbol-storage-test.dml");
        let name = "dummy_extern".to_string();
        let extrn = Extern {
            var: VariableDecl {
                object: DMLObjectCommon {
                    name: DMLString { val: name.clone(), span: location },
                    span: location,
                },
                kind: VariableDeclKind::Extern,
                typed: DMLType::unknown(location),
            },
            span: location,
        };
        let object = DMLObject::ShallowObject(Box::new(DMLShallowObject {
            parent: StructureKey::default(),
            variant: DMLShallowObjectVariant::Extern(
                DMLAmbiguousDef::new_simple(name, extrn)),
        }));
        maker.new_symbol(location, DMLSymbolKind::Extern,
                         SymbolSource::DMLObject(object))
    }

    #[test]
    fn inserting_the_same_symbol_does_not_lock_it_again() {
        let symbol = symbol(&SymbolMaker::new());
        let mut map = HashMap::new();
        assert!(!log_non_same_insert(&mut map, 0, Arc::clone(&symbol)));
        let _guard = symbol.lock().unwrap();
        assert!(!log_non_same_insert(&mut map, 0, Arc::clone(&symbol)));
        assert_eq!(map.len(), 1);
        assert!(Arc::ptr_eq(&map[&0], &symbol));
    }

    #[test]
    fn inserting_equivalent_symbols_preserves_replacement_behavior() {
        let maker = SymbolMaker::new();
        let first = symbol(&maker);
        let second = symbol(&maker);
        let mut map = HashMap::new();
        assert!(!log_non_same_insert(&mut map, 0, first));
        assert!(!log_non_same_insert(&mut map, 0, Arc::clone(&second)));
        assert!(Arc::ptr_eq(&map[&0], &second));
    }

    #[test]
    fn inserting_different_symbols_reports_the_replacement() {
        let maker = SymbolMaker::new();
        let first = symbol(&maker);
        let second = symbol(&maker);
        second.lock().unwrap().kind = DMLSymbolKind::Constant;
        let mut map = HashMap::new();
        assert!(!log_non_same_insert(&mut map, 0, first));
        assert!(log_non_same_insert(&mut map, 0, Arc::clone(&second)));
        assert!(Arc::ptr_eq(&map[&0], &second));
    }

}

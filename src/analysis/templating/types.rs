//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

// This file describes RESOLVED DML types

use crate::analysis::structure::expressions::DMLString;
// These are the types which we can directly re-use from the structural part,
// as they are the same resolved and unresolved
use crate::analysis::structure::types::{
    BitfieldsType, BoolType, DeviceType, DoubleType, EndianIntType, Endianness, FloatType, IntType, NamedType, ResolveableType, TypeBase, UnresolvedType, VoidType};
use crate::analysis::structure::objects::Typedef;
use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::{DMLError, DeclarationSpan, LocationSpan, IdentitySpan, DMLNamed};

use lsp_types::DiagnosticSeverity;

use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypedefEntry {
    unresolved: Arc<UnresolvedType>,
    decl_name: DMLString,
    is_extern: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TemplateEntry {
    decl_name: DMLString,
}

/// Global storage of typedef information and a cache of resolved types, used
/// when resolving `UnresolvedType`s during templating.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalTypeStorage {
    /// Map from typedef name to its declaration info.
    type_decls: HashMap<String, TypedefEntry>,
    /// Map from template name to its declaration info
    template_decls: HashMap<String, TemplateEntry>,
    /// Cache of fully resolved underlying types
    resolved_cache: HashMap<String, DMLType>,
    /// Built-in named types that are not parser-level primitives (size_t and the like)
    builtin_named_types: HashMap<String, DMLType>,
}

// Used as recursion guard while resolving types
#[derive(Debug, Default)]
pub struct ResolvingState {
    in_progress: HashSet<String>,
    // Path of resolution, used for diagnostic info
    path: Vec<(String, ZeroSpan)>,
}

impl GlobalTypeStorage {
    /// `path` is only used to construct the (location-less) spans of the
    /// built-in named types, and is typically the device's root file.
    pub fn new<F: Into<PathBuf>>(path: F) -> Self {
        Self {
            type_decls: HashMap::new(),
            template_decls: HashMap::new(),
            resolved_cache: HashMap::new(),
            builtin_named_types: make_builtin_named_types(path.into()),
        }
    }

    /// Registers a typedef, returning the existing declaration span if one
    /// with this name was already registered
    pub fn add_typedef(&mut self, typedef: &Typedef) -> Option<ZeroSpan> {
        let name = typedef.name();
        match self.type_decls.entry(name.val.clone()) {
            std::collections::hash_map::Entry::Occupied(entry) =>
                Some(entry.get().decl_name.span),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(TypedefEntry {
                    unresolved: Arc::new(typedef.typed.clone()),
                    decl_name: name.clone(),
                    is_extern: typedef.is_extern,
                });
                None
            }
        }
    }

    /// Register a template as a type
    pub fn add_template(&mut self, decl_name: DMLString) {
        self.template_decls.insert(decl_name.val.clone(),
                                   TemplateEntry { decl_name });
    }

    fn template_decl_name(&self, name: &str) -> Option<DMLString> {
        self.template_decls.get(name).map(|entry| entry.decl_name.clone())
    }

    /// resolves a named type to a typedef
    pub fn resolve_named(&mut self, named: &NamedType,
                         resolving: &mut ResolvingState,
                         is_extern: bool, via_indirection: bool,
                         errors: &mut Vec<DMLError>) -> DMLType {
        let name_str = named.name.val.clone();

        let Some(entry) = self.type_decls.get(&name_str).cloned() else {
            // Fall back to seeing if we have a template of this name to use
            // as a type
            if let Some(decl_name) = self.template_decl_name(&name_str) {
                return Some(Arc::new(DMLConcreteType::Trait(DMLTraitType {
                    base: named.base.clone(),
                    decl_name,
                })));
            }
            if let Some(ty) = self.builtin_named_types.get(&name_str) {
                return ty.clone();
            }

            errors.push(DMLError {
                span: named.name.span,
                description: format!("Unknown type '{}'", &name_str),
                // TODO: It is relatively easy to find suggestions for closely-named types here,
                // however I want the suggestion-feature-search to be its own module so for now this is
                // unimplemented
                related: vec![],
                severity: Some(DiagnosticSeverity::ERROR),
            });
            return None;
        };

        if resolving.in_progress.contains(&name_str) {
            // Skip reporting cycles directly under externs or
            // pointers
            if !is_extern && !via_indirection {
                let cycle_start = resolving.path.iter()
                    .position(|(n, _)| n == &name_str)
                    .unwrap_or(0);
                let mut related: Vec<(ZeroSpan, String)> =
                    resolving.path[cycle_start + 1..].iter()
                    .map(|(n, span)| (*span,
                                      format!("through '{}' here", n)))
                    .collect();
                related.push((named.name.span,
                              "Cyclic reference occurs here".to_string()));
                errors.push(DMLError {
                    span: entry.decl_name.span,
                    description: format!(
                        "Typedef '{}' is cyclically defined", &name_str),
                    related,
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
            // Blank out the alias type
            return Some(Arc::new(DMLConcreteType::Typedef(DMLTypedefType {
                base: named.base.clone(),
                decl_name: entry.decl_name.clone(),
                underlying: None,
            })));
        }

        let underlying = self.resolve_entry_underlying(
            &name_str, &entry, resolving, errors);

        Some(Arc::new(DMLConcreteType::Typedef(DMLTypedefType {
            base: named.base.clone(),
            decl_name: entry.decl_name.clone(),
            underlying,
        })))
    }

    fn resolve_entry_underlying(&mut self, name: &str, entry: &TypedefEntry,
                               resolving: &mut ResolvingState,
                               errors: &mut Vec<DMLError>) -> DMLType {
        if let Some(cached) = self.resolved_cache.get(name) {
            return cached.clone();
        }
        resolving.in_progress.insert(name.to_string());
        resolving.path.push((name.to_string(), entry.decl_name.span));
        let resolved = entry.unresolved.resolve(self, resolving, entry.is_extern, false, errors);
        resolving.path.pop();
        resolving.in_progress.remove(name);
        self.resolved_cache.insert(name.to_string(), resolved.clone());
        resolved
    }

    pub fn get_underlying(&self, name: &str) -> Option<&DMLType> {
        self.resolved_cache.get(name)
    }

    pub fn typedef_decl_span(&self, name: &str) -> Option<ZeroSpan> {
        self.type_decls.get(name).map(|entry|entry.decl_name.span)
    }

    pub fn typedef_decl_spans(&self) -> impl Iterator<Item = (&str, ZeroSpan)> + '_ {
        self.type_decls.iter().map(|(name, entry)|(name.as_str(), entry.decl_name.span))
    }

    // re-constructs a typedef wrapper around a name
    pub fn typedef_as_type(&self, name: &str) -> DMLType {
        let entry = self.type_decls.get(name)?;
        let underlying = self.resolved_cache.get(name).cloned().unwrap_or(None);
        Some(Arc::new(DMLConcreteType::Typedef(DMLTypedefType {
            base: TypeBase {
                is_const: false,
                decl_span: entry.decl_name.span,
            },
            decl_name: entry.decl_name.clone(),
            underlying,
        })))
    }

    // Resolves all typedefs
    pub fn resolve_all(&mut self, errors: &mut Vec<DMLError>) {
        let mut names: Vec<String> = self.type_decls.keys().cloned().collect();
        names.sort();
        for name in names {
            if self.resolved_cache.contains_key(&name) {
                continue;
            }
            let entry = self.type_decls[&name].clone();
            let mut resolving = ResolvingState::default();
            self.resolve_entry_underlying(&name, &entry, &mut resolving, errors);
        }
    }

    pub fn resolved_types(&self) -> impl Iterator<Item = &DMLType> {
        self.resolved_cache.values()
    }
}

// The location of built-in types is faked as an invalid span of the
// device file
fn make_builtin_named_types(path: PathBuf) -> HashMap<String, DMLType> {
    let base = TypeBase {
        is_const: false,
        decl_span: ZeroSpan::invalid(path),
    };
    [
        ("char", IntType::char(base.clone())),
        ("int", IntType::int(base.clone(), true)),
        ("uint", IntType::int(base.clone(), false)),
        ("int64_t", IntType::int64(base.clone(), true)),
        ("uint64_t", IntType::int64(base.clone(), false)),
        ("long", IntType::long(base.clone(), true)),
        ("ulong", IntType::long(base.clone(), false)),
        ("size_t", IntType::size_t(base.clone(), false)),
        ("ssize_t", IntType::size_t(base, true)),
    ].iter()
        .map(|(name, ty)| (
            name.to_string(),
            Some(Arc::new(DMLConcreteType::Int(ty.clone())))))
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum DMLStructLabel {
    Labeled(DMLString),
    Anonymous(u64),
}

impl DMLStructLabel {
    pub fn new_from_maybe_string(s: Option<DMLString>) -> Self {
        match s {
            Some(label) => DMLStructLabel::Labeled(label),
            None => DMLStructLabel::Anonymous(next_anon_id()),
        }
    }
}

// u64 should be more than large enough to generate unique IDs for even long-running langserver processes
static ANON_ID_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn next_anon_id() -> u64 {
    ANON_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLStructType {
    pub base: TypeBase,
    pub label: DMLStructLabel,
    pub members: Vec<(Option<DMLString>, DMLType)>,
}
impl_trait_fns!(DMLStructType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);

// For identification of structs, use covering-span (they dont always have a name decl)
impl IdentitySpan for DMLStructType {
    fn identity_span(&self) -> &ZeroSpan {
        self.span()
    }
}
impl DMLStructType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.label == other.label
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLArrayType {
    pub base: TypeBase,
    pub typing: DMLType,
    pub size: Option<u64>,
}
impl_trait_fns!(DMLArrayType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLArrayType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.size == other.size
            && dmltype_equivalent(&self.typing, &other.typing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLPointerType {
    pub base: TypeBase,
    pub typing: DMLType,
}
impl_trait_fns!(DMLPointerType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLPointerType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && dmltype_equivalent(&self.typing, &other.typing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLFunctionType {
    pub base: TypeBase,
    pub arg_types: Vec<DMLType>,
    pub vararg: bool,
    // NOTE: Empty return type means an INVALID type, non-returning
    // methods will get a function type with voidtype return
    pub return_ty: DMLType,
}
impl_trait_fns!(DMLFunctionType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLFunctionType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.arg_types.len() == other.arg_types.len()
            && self.arg_types.iter().zip(
                other.arg_types.iter()).all(
                |(ty1, ty2)|dmltype_equivalent(ty1, ty2))
            && dmltype_equivalent(&self.return_ty, &other.return_ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLVectorType {
    pub base: TypeBase,
    pub typing: DMLType,
}
impl_trait_fns!(DMLVectorType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLVectorType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && dmltype_equivalent(&self.typing, &other.typing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLLayoutType {
    pub base: DMLStructType,
    pub endianness: Option<Endianness>,
}
impl_trait_fns!(DMLLayoutType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLLayoutType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.endianness == other.endianness
    }
}

// NOTE: A TypeSequence is NOT the same as a SequenceType
// A TypeSequence is heterogeneous and not something declared
// in-code
// TODO: How do handle semantic-only types?
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLTypeSequence {
    pub base: TypeBase,
    pub types: Vec<DMLType>,
}
impl_trait_fns!(DMLTypeSequence, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLTypeSequence {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.types.len() == other.types.len()
            && self.types.iter().zip(other.types.iter())
            .all(|(ty1, ty2)|dmltype_equivalent(ty1, ty2))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLSequenceType {
    pub base: TypeBase,
    pub trait_type: DMLTraitType,
}
impl_trait_fns!(DMLSequenceType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLSequenceType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.trait_type.equivalent(&other.trait_type)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLHookType {
    pub base: TypeBase,
    pub arg_types: Vec<DMLType>,
}
impl_trait_fns!(DMLHookType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLHookType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.arg_types.len() == other.arg_types.len()
            && self.arg_types.iter().zip(other.arg_types.iter())
            .all(|(ty1, ty2)|dmltype_equivalent(ty1, ty2))
    }
}

/// A resolved def, maintaining info about the typedef that resolved it
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLTypedefType {
    // Use-site info (const and the like)
    pub base: TypeBase,
    // The typedef's name and the span of its declaration (used for
    // goto-type-def).
    pub decl_name: DMLString,
    // Type the tpedef points to
    pub underlying: DMLType,
}
impl_trait_fns!(DMLTypedefType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl DMLTypedefType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.decl_name.span == other.decl_name.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DMLTraitType {
    // use-site info
    pub base: TypeBase,
    pub decl_name: DMLString,
}
impl_trait_fns!(DMLTraitType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);

// loc span is the location of the declaration
impl LocationSpan for DMLTraitType {
    fn loc_span(&self) -> &ZeroSpan {
        &self.decl_name.span
    }
}

impl IdentitySpan for DMLTraitType {
    fn identity_span(&self) -> &ZeroSpan {
        self.loc_span()
    }
}
impl DMLTraitType {
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.decl_name.span == other.decl_name.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum DMLConcreteType {
    // These are re-used directly from structure, as there is nothing to
    // resolve in them
    Void(VoidType),
    Device(DeviceType),
    Bool(BoolType),
    EndianInt(EndianIntType),
    Int(IntType),
    Float(FloatType),
    Double(DoubleType),
    Bitfields(BitfieldsType),
    // And these are the resolved results of structural types
    Array(DMLArrayType),
    Pointer(DMLPointerType),
    Function(DMLFunctionType),
    Vector(DMLVectorType),
    Layout(DMLLayoutType),
    Hook(DMLHookType),
    TypeSequence(DMLTypeSequence),
    Sequence(DMLSequenceType),
    StructType(DMLStructType),
    // separated due to needing to maintain meta-info
    Typedef(DMLTypedefType),
    Trait(DMLTraitType),
}

impl From<DMLConcreteType> for DMLType {
    fn from(fr: DMLConcreteType) -> DMLType {
        Some(Arc::new(fr))
    }
}

impl DMLConcreteType {
    pub fn span(&self) -> &ZeroSpan {
        match self {
            Self::Void(i) => i.span(),
            Self::Device(i) => i.span(),
            Self::Bool(i) => i.span(),
            Self::EndianInt(i) => i.span(),
            Self::Int(i) => i.span(),
            Self::Float(i) => i.span(),
            Self::Double(i) => i.span(),
            Self::Bitfields(i) => i.span(),
            Self::Array(i) => i.span(),
            Self::Pointer(i) => i.span(),
            Self::Function(i) => i.span(),
            Self::Vector(i) => i.span(),
            Self::Layout(i) => i.span(),
            Self::Hook(i) => i.span(),
            Self::TypeSequence(i) => i.span(),
            Self::Sequence(i) => i.span(),
            Self::StructType(i) => i.span(),
            Self::Typedef(i) => i.span(),
            Self::Trait(i) => i.span(),
        }
    }

    pub fn peel_one(&self) -> Option<&DMLType> {
        match self {
            Self::Typedef(td) => Some(&td.underlying),
            Self::Array(a) => Some(&a.typing),
            Self::Pointer(p) => Some(&p.typing),
            Self::Vector(v) => Some(&v.typing),
            _ => None,
        }
    }
}

pub type DMLType = Option<Arc<DMLConcreteType>>;

// Resolved a concrete type past typedefs
fn fully_resolved(ty: &DMLType) -> &DMLType {
    match ty {
        Some(t) => match t.as_ref() {
            DMLConcreteType::Typedef(td) => fully_resolved(&td.underlying),
            _ => ty,
        },
        None => ty,
    }
}

pub fn dmltype_equivalent(ty1: &DMLType, ty2: &DMLType) -> bool {
    match (fully_resolved(ty1), fully_resolved(ty2)) {
        (Some(t1), Some(t2)) => t1.equivalent(t2),
        // I think it is correct to say missing types are equivalent,
        // so as to not report mismatching errors when there will be
        // a report about the invalid type anyway
        (_, _) => true,
    }
}

impl DMLConcreteType {
    // Equivalent is a fairly strict comparison, not the same
    // as a canstore
    pub fn equivalent(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Void(i1), Self::Void(i2)) => i1.equivalent(i2),
            (Self::Device(i1), Self::Device(i2)) => i1.equivalent(i2),
            (Self::Bool(i1), Self::Bool(i2)) => i1.equivalent(i2),
            (Self::EndianInt(i1), Self::EndianInt(i2)) => i1.equivalent(i2),
            (Self::Int(i1), Self::Int(i2)) => i1.equivalent(i2),
            (Self::Float(i1), Self::Float(i2)) => i1.equivalent(i2),
            (Self::Double(i1), Self::Double(i2)) => i1.equivalent(i2),
            (Self::Bitfields(i1), Self::Bitfields(i2)) => i1.equivalent(i2),
            (Self::Array(i1), Self::Array(i2)) => i1.equivalent(i2),
            (Self::Pointer(i1), Self::Pointer(i2)) => i1.equivalent(i2),
            (Self::Function(i1), Self::Function(i2)) => i1.equivalent(i2),
            (Self::Vector(i1), Self::Vector(i2)) => i1.equivalent(i2),
            (Self::Layout(i1), Self::Layout(i2)) => i1.equivalent(i2),
            (Self::Hook(i1), Self::Hook(i2)) => i1.equivalent(i2),
            (Self::TypeSequence(i1), Self::TypeSequence(i2)) =>
                i1.equivalent(i2),
            (Self::Sequence(i1), Self::Sequence(i2)) => i1.equivalent(i2),
            (Self::StructType(i1), Self::StructType(i2)) => i1.equivalent(i2),
            (Self::Typedef(i1), Self::Typedef(i2)) => i1.equivalent(i2),
            (Self::Trait(i1), Self::Trait(i2)) => i1.equivalent(i2),
            (_, _) => false,
        }
    }
}

pub fn eval_type(ast: &UnresolvedType, global: &mut GlobalTypeStorage,
                 in_extern: bool, _typename_hint: Option<&str>,
                 _allow_void: bool, errors: &mut Vec<DMLError>)
                 -> DMLType {
    let mut resolving = ResolvingState::default();
    ast.resolve(global, &mut resolving, in_extern, false, errors)
}

pub fn eval_type_simple(ast: &UnresolvedType, global: &mut GlobalTypeStorage,
                        errors: &mut Vec<DMLError>) -> DMLType {
    eval_type(ast, global, false, None, false, errors)
}

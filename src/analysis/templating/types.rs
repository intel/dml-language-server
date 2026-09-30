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
use std::collections::HashMap;
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
    // Path of resolution and whether each typedef was reached through an
    // indirection, used for cycle classification and diagnostic info.
    path: Vec<(String, ZeroSpan, bool)>,
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
                return match ty.as_deref() {
                    Some(DMLConcreteType::Int(int)) => Some(Arc::new(
                        DMLConcreteType::Int(int.clone().with_base(named.base.clone())))),
                    _ => ty.clone(),
                };
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

        if let Some(cycle_start) = resolving.path.iter()
            .position(|(n, _, _)| n == &name_str)
        {
            let cycle_has_indirection = via_indirection
                || resolving.path[cycle_start + 1..].iter()
                    .any(|(_, _, via_indirection)| *via_indirection);
            // Skip reporting cycles directly under externs or
            // pointers
            if !is_extern && !cycle_has_indirection {
                let mut related: Vec<(ZeroSpan, String)> =
                    resolving.path[cycle_start + 1..].iter()
                    .map(|(n, span, _)| (*span,
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
            &name_str, &entry, resolving, via_indirection, errors);

        Some(Arc::new(DMLConcreteType::Typedef(DMLTypedefType {
            base: named.base.clone(),
            decl_name: entry.decl_name.clone(),
            underlying,
        })))
    }

    fn resolve_entry_underlying(&mut self, name: &str, entry: &TypedefEntry,
                               resolving: &mut ResolvingState,
                               via_indirection: bool,
                               errors: &mut Vec<DMLError>) -> DMLType {
        if let Some(cached) = self.resolved_cache.get(name) {
            return cached.clone();
        }
        resolving.path.push((
            name.to_string(), entry.decl_name.span, via_indirection));
        let resolved = entry.unresolved.resolve(
            self, resolving, entry.is_extern, false, errors);
        resolving.path.pop();
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
            self.resolve_entry_underlying(
                &name, &entry, &mut resolving, false, errors);
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
            && self.vararg == other.vararg
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
    fn make_const(mut self) -> Self {
        match &mut self {
            Self::Void(ty) => *ty = ty.clone().make_const(),
            Self::Device(ty) => *ty = ty.clone().make_const(),
            Self::Bool(ty) => *ty = ty.clone().make_const(),
            Self::EndianInt(ty) => *ty = ty.clone().make_const(),
            Self::Int(ty) => *ty = ty.clone().make_const(),
            Self::Float(ty) => *ty = ty.clone().make_const(),
            Self::Double(ty) => *ty = ty.clone().make_const(),
            Self::Bitfields(ty) => *ty = ty.clone().make_const(),
            Self::Array(ty) => ty.base.is_const = true,
            Self::Pointer(ty) => ty.base.is_const = true,
            Self::Function(ty) => ty.base.is_const = true,
            Self::Vector(ty) => ty.base.is_const = true,
            Self::Layout(ty) => ty.base.base.is_const = true,
            Self::Hook(ty) => ty.base.is_const = true,
            Self::TypeSequence(ty) => ty.base.is_const = true,
            Self::Sequence(ty) => ty.base.is_const = true,
            Self::StructType(ty) => ty.base.is_const = true,
            Self::Typedef(ty) => ty.base.is_const = true,
            Self::Trait(ty) => ty.base.is_const = true,
        }
        self
    }

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

// Resolve a concrete type past typedefs while composing their qualifiers.
fn fully_resolved(ty: &DMLType) -> DMLType {
    let mut current = ty.clone();
    let mut is_const = false;
    loop {
        let concrete = current?;
        match concrete.as_ref() {
            DMLConcreteType::Typedef(td) => {
                is_const |= td.base.is_const;
                current = td.underlying.clone();
            }
            _ if is_const => {
                return Some(Arc::new(concrete.as_ref().clone().make_const()));
            }
            _ => return Some(concrete),
        }
    }
}

pub fn dmltype_equivalent(ty1: &DMLType, ty2: &DMLType) -> bool {
    match (fully_resolved(ty1), fully_resolved(ty2)) {
        (Some(t1), Some(t2)) => t1.equivalent(&t2),
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::parsing::misc::{CDecl, CDeclContent};
    use crate::analysis::structure::objects::DMLObjectCommon;
    use crate::analysis::structure::test::parse_to_structure;
    use crate::analysis::structure::types::deconstruct_cdecl;

    fn span(name: &str) -> ZeroSpan {
        ZeroSpan::invalid(format!("{name}.dml"))
    }

    fn base() -> TypeBase {
        TypeBase { is_const: false, decl_span: span("type") }
    }

    fn const_base() -> TypeBase {
        TypeBase { is_const: true, decl_span: span("const_type") }
    }

    fn name(value: &str) -> DMLString {
        DMLString { val: value.to_string(), span: span(value) }
    }

    fn typedef(value: &str, typed: UnresolvedType) -> Typedef {
        Typedef {
            is_extern: false,
            object: DMLObjectCommon { name: name(value), span: span(value) },
            typed,
        }
    }

    fn named(value: &str) -> NamedType {
        NamedType { base: base(), name: name(value) }
    }

    fn named_at(value: &str, at: ZeroSpan) -> DMLString {
        DMLString { val: value.to_string(), span: at }
    }

    fn unresolved_cdecl(source: &str) -> UnresolvedType {
        let Some(((_, ty), parse_errors)) =
            parse_to_structure::<CDeclContent, CDecl, _, _>(source, deconstruct_cdecl)
        else {
            panic!("failed to structurally parse {}", source);
        };
        assert!(parse_errors.is_empty(), "{}: {:?}", source, parse_errors);
        ty
    }

    #[test]
    fn storage_accessors_and_resolution_branches() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        let alias = typedef("alias_t", UnresolvedType::Int(IntType::int(base(), true)));
        assert_eq!(storage.add_typedef(&alias), None);
        assert_eq!(storage.add_typedef(&alias), Some(alias.object.name.span));
        assert_eq!(storage.typedef_decl_span("alias_t"), Some(alias.object.name.span));
        assert_eq!(storage.typedef_decl_span("missing"), None);
        assert!(storage.typedef_as_type("alias_t").is_some());
        assert!(storage.get_underlying("alias_t").is_none());

        storage.add_template(name("template_t"));
        let mut errors = vec![];
        let mut resolving = ResolvingState::default();
        let trait_ty = storage.resolve_named(&named("template_t"), &mut resolving,
                                             false, false, &mut errors);
        assert!(matches!(trait_ty.as_deref(), Some(DMLConcreteType::Trait(_))));

        let builtin_ty = storage.resolve_named(&named("size_t"), &mut resolving,
                                               false, false, &mut errors);
        assert!(matches!(builtin_ty.as_deref(), Some(DMLConcreteType::Int(_))));

        let unknown_ty = storage.resolve_named(&named("missing_t"), &mut resolving,
                                               false, false, &mut errors);
        assert!(unknown_ty.is_none());
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].description, "Unknown type 'missing_t'");

        storage.resolve_all(&mut errors);
        assert!(storage.get_underlying("alias_t").is_some());
        let mut resolving = ResolvingState::default();
        let resolved = storage.resolve_named(&named("alias_t"), &mut resolving,
                                             false, false, &mut errors);
        assert!(matches!(resolved.as_deref(), Some(DMLConcreteType::Typedef(_))));
        assert_eq!(errors.len(), 1, "cache lookup must not repeat diagnostics");
    }

    #[test]
    fn storage_accessors_report_complete_registered_state() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        let alpha = typedef("alpha_t", UnresolvedType::Int(IntType::int(base(), true)));
        let beta = typedef("beta_t", UnresolvedType::Named(named("alpha_t")));
        assert_eq!(storage.add_typedef(&alpha), None);
        assert_eq!(storage.add_typedef(&beta), None);

        let spans: HashMap<&str, ZeroSpan> = storage.typedef_decl_spans().collect();
        assert_eq!(spans.len(), 2);
        assert_eq!(spans["alpha_t"], alpha.object.name.span);
        assert_eq!(spans["beta_t"], beta.object.name.span);
        assert!(storage.typedef_as_type("missing_t").is_none());
        assert!(storage.template_decl_name("missing_template").is_none());

        let first_template = named_at("template_t", span("first_template"));
        let replacement = named_at("template_t", span("replacement_template"));
        storage.add_template(first_template);
        storage.add_template(replacement.clone());
        assert_eq!(storage.template_decl_name("template_t"), Some(replacement));
    }

    #[test]
    fn builtin_named_type_shapes_are_complete() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        let expected = [
            ("char", IntType::char(base())),
            ("int", IntType::int(base(), true)),
            ("uint", IntType::int(base(), false)),
            ("int64_t", IntType::int64(base(), true)),
            ("uint64_t", IntType::int64(base(), false)),
            ("long", IntType::long(base(), true)),
            ("ulong", IntType::long(base(), false)),
            ("size_t", IntType::size_t(base(), false)),
            ("ssize_t", IntType::size_t(base(), true)),
        ];
        let mut errors = vec![];
        for (type_name, expected_shape) in expected {
            let resolved = storage.resolve_named(
                &named(type_name), &mut ResolvingState::default(), false, false, &mut errors);
            let Some(DMLConcreteType::Int(actual)) = resolved.as_deref() else {
                panic!("{} did not resolve to an integer type", type_name);
            };
                assert!(actual.equivalent(&expected_shape),
                    "{}: resolved to the wrong integer shape", type_name);
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn user_typedefs_precede_every_builtin_name() {
        for builtin_name in ["char", "int", "uint", "int64_t", "uint64_t",
                             "long", "ulong", "size_t", "ssize_t"] {
            let mut storage = GlobalTypeStorage::new("root.dml");
            storage.add_typedef(&typedef(
                builtin_name, UnresolvedType::Bool(base().into())));
            let mut errors = vec![];
            storage.resolve_all(&mut errors);
            let resolved = storage.resolve_named(
                &named(builtin_name), &mut ResolvingState::default(),
                false, false, &mut errors);
            let Some(DMLConcreteType::Typedef(alias)) = resolved.as_deref() else {
                panic!("{} resolved as a built-in instead of the user typedef", builtin_name);
            };
            assert!(matches!(alias.underlying.as_deref(), Some(DMLConcreteType::Bool(_))));
            assert!(errors.is_empty());
        }
    }

    #[test]
    fn typedef_use_site_base_is_independent_of_cached_underlying() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        storage.add_typedef(&typedef(
            "alias_t", UnresolvedType::Int(IntType::int(base(), true))));
        storage.resolve_all(&mut vec![]);

        let qualified_name = NamedType {
            base: const_base(), name: name("alias_t"),
        };
        let qualified = storage.resolve_named(
            &qualified_name, &mut ResolvingState::default(), false, false, &mut vec![]);
        let unqualified = storage.resolve_named(
            &named("alias_t"), &mut ResolvingState::default(), false, false, &mut vec![]);
        let (Some(DMLConcreteType::Typedef(qualified)),
             Some(DMLConcreteType::Typedef(unqualified))) =
            (qualified.as_deref(), unqualified.as_deref()) else {
                panic!("alias_t did not resolve as a typedef");
            };
        assert!(qualified.base.is_const);
        assert!(!unqualified.base.is_const);
        assert_eq!(qualified.underlying, unqualified.underlying,
                   "use-site qualification must not alter cached underlying data");
    }

    #[test]
    fn resolve_all_is_deterministic_and_caches_failures() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        storage.add_typedef(&typedef("z_alias", UnresolvedType::Named(named("z_missing"))));
        storage.add_typedef(&typedef("a_alias", UnresolvedType::Named(named("a_missing"))));
        storage.add_typedef(&typedef("middle_alias", UnresolvedType::Named(named("a_alias"))));

        let mut errors = vec![];
        storage.resolve_all(&mut errors);
        assert_eq!(errors.iter().map(|e| e.description.as_str()).collect::<Vec<_>>(),
                   ["Unknown type 'a_missing'", "Unknown type 'z_missing'"]);
        assert!(storage.get_underlying("a_alias").is_some());
        assert!(storage.get_underlying("middle_alias").is_some());
        assert!(storage.get_underlying("z_alias").is_some());

        storage.resolve_all(&mut errors);
        assert_eq!(errors.len(), 2, "cached failures must not be reported again");
    }

    #[test]
    fn unresolved_type_families_resolve_to_expected_concrete_variants() {
        type ResolutionCase = (&'static str, fn(&DMLConcreteType) -> bool);
        let mut storage = GlobalTypeStorage::new("root.dml");
        storage.add_template(name("template_t"));
        let cases: [ResolutionCase; 9] = [
            ("int value;", |ty| matches!(ty, DMLConcreteType::Int(_))),
            ("int *value;", |ty| matches!(ty, DMLConcreteType::Pointer(_))),
            ("int value[2];", |ty| matches!(ty, DMLConcreteType::Array(_))),
            ("int vect value;", |ty| matches!(ty, DMLConcreteType::Vector(_))),
            ("int callback(float);", |ty| matches!(ty, DMLConcreteType::Function(_))),
            ("struct { int member; } value;",
             |ty| matches!(ty, DMLConcreteType::StructType(_))),
            ("layout \"big-endian\" { int member; } value;",
             |ty| matches!(ty, DMLConcreteType::Layout(_))),
            ("hook(int) value;", |ty| matches!(ty, DMLConcreteType::Hook(_))),
            ("sequence(template_t) value;",
             |ty| matches!(ty, DMLConcreteType::Sequence(_))),
        ];
        let mut errors = vec![];
        for (source, expected) in cases {
            let resolved = eval_type_simple(&unresolved_cdecl(source), &mut storage, &mut errors);
            assert!(resolved.as_deref().is_some_and(expected),
                    "{} resolved to {:?}", source, resolved);
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn nested_unknown_type_preserves_wrapper_and_reports_once() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        let mut errors = vec![];
        let resolved = eval_type_simple(
            &unresolved_cdecl("missing_t *value;"), &mut storage, &mut errors);
        let Some(DMLConcreteType::Pointer(pointer)) = resolved.as_deref() else {
            panic!("unknown pointee should retain its pointer wrapper");
        };
        assert!(pointer.typing.is_none());
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].description, "Unknown type 'missing_t'");
    }

    #[test]
    fn builtin_named_types_preserve_use_site_qualifiers_and_spans() {
        let mut storage = GlobalTypeStorage::new("root.dml");
        let mut errors = vec![];
        for type_name in ["char", "int", "uint", "int64_t", "uint64_t",
                          "long", "ulong", "size_t", "ssize_t"] {
            let use_span = span(&format!("use_{type_name}"));
            let named = NamedType {
                base: TypeBase { is_const: true, decl_span: use_span },
                name: named_at(type_name, use_span),
            };
            let qualified = storage.resolve_named(
                &named, &mut ResolvingState::default(), false, false, &mut errors);
            let Some(DMLConcreteType::Int(qualified)) = qualified.as_deref() else {
                panic!("{} did not resolve to an integer type", type_name);
            };
            let expected_qualified = match type_name {
                "char" => IntType::char(named.base.clone()),
                "int" => IntType::int(named.base.clone(), true),
                "uint" => IntType::int(named.base.clone(), false),
                "int64_t" => IntType::int64(named.base.clone(), true),
                "uint64_t" => IntType::int64(named.base.clone(), false),
                "long" => IntType::long(named.base.clone(), true),
                "ulong" => IntType::long(named.base.clone(), false),
                "size_t" => IntType::size_t(named.base.clone(), false),
                "ssize_t" => IntType::size_t(named.base.clone(), true),
                _ => unreachable!(),
            };
            assert!(qualified.equivalent(&expected_qualified),
                    "{} lost use-site const", type_name);
            assert_eq!(*qualified.span(), use_span,
                       "{type_name} retained the synthetic built-in span");

            let unqualified = storage.resolve_named(
                &self::named(type_name), &mut ResolvingState::default(),
                false, false, &mut errors);
            let Some(DMLConcreteType::Int(unqualified)) = unqualified.as_deref() else {
                panic!("{} did not resolve to an integer type", type_name);
            };
            assert!(!unqualified.equivalent(&expected_qualified),
                    "qualified lookup mutated the canonical built-in");
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn equivalent_struct_label_identity() {
        let labeled = |value| DMLStructType {
            base: base(), label: DMLStructLabel::Labeled(name(value)), members: vec![],
        };
        assert!(labeled("foo_t").equivalent(&labeled("foo_t")));
        assert!(!labeled("foo_t").equivalent(&labeled("bar_t")));

        let anonymous = |id| DMLStructType {
            base: base(), label: DMLStructLabel::Anonymous(id), members: vec![],
        };
        assert!(anonymous(1).equivalent(&anonymous(1)));
        assert!(!anonymous(1).equivalent(&anonymous(2)));
    }

    #[test]
    fn equivalent_array_pointer_vector() {
        let signed: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let unsigned: DMLType = DMLConcreteType::Int(IntType::int(base(), false)).into();

        let array = |typing, size| DMLArrayType { base: base(), typing, size };
        assert!(array(signed.clone(), Some(4)).equivalent(&array(signed.clone(), Some(4))));
        assert!(!array(signed.clone(), Some(4)).equivalent(&array(signed.clone(), Some(8))));
        assert!(!array(signed.clone(), Some(4)).equivalent(&array(unsigned.clone(), Some(4))));

        let pointer = |typing| DMLPointerType { base: base(), typing };
        assert!(pointer(signed.clone()).equivalent(&pointer(signed.clone())));
        assert!(!pointer(signed.clone()).equivalent(&pointer(unsigned.clone())));

        let vector = |typing| DMLVectorType { base: base(), typing };
        assert!(vector(signed.clone()).equivalent(&vector(signed.clone())));
        assert!(!vector(signed).equivalent(&vector(unsigned)));
    }

    #[test]
    fn equivalent_function_type() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let float_ty: DMLType = DMLConcreteType::Float(base().into()).into();
        let void_ty: DMLType = DMLConcreteType::Void(base().into()).into();
        let function = |args, vararg, ret| DMLFunctionType {
            base: base(), arg_types: args, vararg, return_ty: ret,
        };
        let expected = function(vec![int_ty.clone()], false, void_ty.clone());
        assert!(expected.equivalent(&function(vec![int_ty.clone()], false, void_ty.clone())));
        assert!(!expected.equivalent(&function(
            vec![int_ty.clone(), int_ty.clone()], false, void_ty.clone())));
        assert!(!expected.equivalent(&function(vec![float_ty], false, void_ty.clone())));
        assert!(!expected.equivalent(&function(vec![int_ty.clone()], false, int_ty.clone())));
        assert!(!expected.equivalent(&function(vec![int_ty], true, void_ty)));
    }

    #[test]
    fn equivalent_layout_same_struct_different_endianness() {
        let shared = DMLStructType {
            base: base(), label: DMLStructLabel::Anonymous(42), members: vec![],
        };
        let layout = |endianness| DMLLayoutType { base: shared.clone(), endianness };
        assert!(layout(Some(Endianness::BE)).equivalent(&layout(Some(Endianness::BE))));
        assert!(!layout(Some(Endianness::BE)).equivalent(&layout(Some(Endianness::LE))));
        assert!(layout(None).equivalent(&layout(None)));
        assert!(!layout(None).equivalent(&layout(Some(Endianness::BE))));
    }

    #[test]
    fn equivalent_hook_and_sequence() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let float_ty: DMLType = DMLConcreteType::Float(base().into()).into();
        let hook = |args| DMLHookType { base: base(), arg_types: args };
        let expected = hook(vec![int_ty.clone()]);
        assert!(expected.equivalent(&hook(vec![int_ty.clone()])));
        assert!(!expected.equivalent(&hook(vec![int_ty.clone(), int_ty])));
        assert!(!expected.equivalent(&hook(vec![float_ty])));

        let trait_a_span = span("trait_a");
        let trait_b_span = span("trait_b");
        let trait_type = |value, at| DMLTraitType {
            base: base(), decl_name: named_at(value, at),
        };
        let trait_a = trait_type("trait_t", trait_a_span);
        let trait_a_again = trait_type("trait_t", trait_a_span);
        let trait_b = trait_type("trait_t", trait_b_span);
        assert!(trait_a.equivalent(&trait_a_again));
        assert!(!trait_a.equivalent(&trait_b));
        assert!(DMLSequenceType { base: base(), trait_type: trait_a.clone() }.equivalent(
            &DMLSequenceType { base: base(), trait_type: trait_a_again }));
        assert!(!DMLSequenceType { base: base(), trait_type: trait_a }.equivalent(
            &DMLSequenceType { base: base(), trait_type: trait_b }));
    }

    #[test]
    fn equivalent_type_sequence() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let float_ty: DMLType = DMLConcreteType::Float(base().into()).into();
        let sequence = |types| DMLTypeSequence { base: base(), types };
        let expected = sequence(vec![int_ty.clone(), float_ty.clone()]);
        assert!(expected.equivalent(&sequence(vec![int_ty.clone(), float_ty.clone()])));
        assert!(!expected.equivalent(&sequence(vec![float_ty, int_ty.clone()])));
        assert!(!expected.equivalent(&sequence(vec![int_ty])));
    }

    #[test]
    fn equivalent_typedef_identity_and_unwrap() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let span_a = span("typedef_a");
        let span_b = span("typedef_b");
        let typedef_type = |value, at| DMLTypedefType {
            base: base(), decl_name: named_at(value, at), underlying: int_ty.clone(),
        };
        let typedef_a = typedef_type("int_t", span_a);
        assert!(typedef_a.equivalent(&typedef_type("int_t", span_a)));
        assert!(!typedef_a.equivalent(&typedef_type("int_t", span_b)));
        let alias: DMLType = DMLConcreteType::Typedef(typedef_a).into();
        assert!(dmltype_equivalent(&alias, &int_ty));
    }

    #[test]
    fn deep_unqualified_typedefs_are_equivalent_to_underlying_type() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let wrap = |value: &str, underlying: DMLType| -> DMLType {
            DMLConcreteType::Typedef(DMLTypedefType {
                base: base(), decl_name: name(value), underlying,
            }).into()
        };
        let deep = wrap("outer_t", wrap("middle_t", wrap("inner_t", int_ty.clone())));
        assert!(dmltype_equivalent(&deep, &int_ty));
    }

    #[test]
    fn typedef_qualifiers_compose_across_alias_chains() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let const_int_ty: DMLType =
            DMLConcreteType::Int(IntType::int(const_base(), true)).into();
        let wrap = |value: &str, wrapper_base: TypeBase, underlying: DMLType| -> DMLType {
            DMLConcreteType::Typedef(DMLTypedefType {
                base: wrapper_base, decl_name: name(value), underlying,
            }).into()
        };

        let plain = wrap("plain_t", base(), int_ty.clone());
        let outer_const = wrap("outer_const_t", const_base(), int_ty.clone());
        let declared_const = wrap("declared_const_t", base(), const_int_ty.clone());
        let middle_const = wrap(
            "outer_t", base(),
            wrap("middle_t", const_base(), wrap("inner_t", base(), int_ty.clone())));
        let already_const = wrap("already_const_t", const_base(), const_int_ty);

        assert!(dmltype_equivalent(&plain, &int_ty));
        assert!(!dmltype_equivalent(&outer_const, &plain));
        assert!(!dmltype_equivalent(&declared_const, &int_ty));
        assert!(!dmltype_equivalent(&middle_const, &int_ty));
        assert!(dmltype_equivalent(&already_const, &outer_const));
    }

    #[test]
    fn nested_typedef_qualifiers_participate_in_composite_equivalence() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let plain: DMLType = DMLConcreteType::Typedef(DMLTypedefType {
            base: base(), decl_name: name("plain_t"), underlying: int_ty.clone(),
        }).into();
        let outer_const: DMLType = DMLConcreteType::Typedef(DMLTypedefType {
            base: const_base(), decl_name: name("plain_t"), underlying: int_ty,
        }).into();
        let actual = [
            DMLPointerType { base: base(), typing: outer_const.clone() }.equivalent(
                &DMLPointerType { base: base(), typing: plain.clone() }),
            DMLArrayType {
            base: base(), typing: outer_const.clone(), size: None,
            }.equivalent(&DMLArrayType {
                base: base(), typing: plain.clone(), size: None,
            }),
            DMLVectorType { base: base(), typing: outer_const.clone() }.equivalent(
                &DMLVectorType { base: base(), typing: plain.clone() }),
            DMLFunctionType {
            base: base(), arg_types: vec![outer_const.clone()],
            vararg: false, return_ty: outer_const.clone(),
            }.equivalent(&DMLFunctionType {
                base: base(), arg_types: vec![plain.clone()],
                vararg: false, return_ty: plain.clone(),
            }),
            DMLHookType {
                base: base(), arg_types: vec![outer_const],
            }.equivalent(&DMLHookType {
                base: base(), arg_types: vec![plain],
            }),
        ];
        assert_eq!(actual, [false; 5]);
    }

    #[test]
    fn composite_outer_qualifiers_participate_in_equivalence() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let arrays = (
            DMLArrayType { base: base(), typing: int_ty.clone(), size: None },
            DMLArrayType { base: const_base(), typing: int_ty.clone(), size: None });
        assert!(!arrays.0.equivalent(&arrays.1));

        let pointers = (
            DMLPointerType { base: base(), typing: int_ty.clone() },
            DMLPointerType { base: const_base(), typing: int_ty.clone() });
        assert!(!pointers.0.equivalent(&pointers.1));

        let vectors = (
            DMLVectorType { base: base(), typing: int_ty.clone() },
            DMLVectorType { base: const_base(), typing: int_ty.clone() });
        assert!(!vectors.0.equivalent(&vectors.1));

        let hooks = (
            DMLHookType { base: base(), arg_types: vec![int_ty.clone()] },
            DMLHookType { base: const_base(), arg_types: vec![int_ty.clone()] });
        assert!(!hooks.0.equivalent(&hooks.1));

        let functions = (
            DMLFunctionType {
                base: base(), arg_types: vec![], vararg: false, return_ty: int_ty.clone(),
            },
            DMLFunctionType {
                base: const_base(), arg_types: vec![], vararg: false, return_ty: int_ty,
            });
        assert!(!functions.0.equivalent(&functions.1));

        let structs = (
            DMLStructType {
                base: base(), label: DMLStructLabel::Anonymous(7), members: vec![],
            },
            DMLStructType {
                base: const_base(), label: DMLStructLabel::Anonymous(7), members: vec![],
            });
        assert!(!structs.0.equivalent(&structs.1));
        assert!(!DMLLayoutType {
            base: structs.0.clone(), endianness: Some(Endianness::BE),
        }.equivalent(&DMLLayoutType {
            base: structs.1, endianness: Some(Endianness::BE),
        }));

        let trait_name = name("template_t");
        let traits = (
            DMLTraitType { base: base(), decl_name: trait_name.clone() },
            DMLTraitType { base: const_base(), decl_name: trait_name });
        assert!(!traits.0.equivalent(&traits.1));
        assert!(!DMLSequenceType {
            base: base(), trait_type: traits.0.clone(),
        }.equivalent(&DMLSequenceType {
            base: const_base(), trait_type: traits.0,
        }));

        assert!(!DMLTypeSequence { base: base(), types: vec![] }.equivalent(
            &DMLTypeSequence { base: const_base(), types: vec![] }));
        assert!(!DMLTypedefType {
            base: base(), decl_name: name("alias_t"), underlying: None,
        }.equivalent(&DMLTypedefType {
            base: const_base(), decl_name: name("alias_t"), underlying: None,
        }));
    }

    #[test]
    fn invalid_nested_types_suppress_secondary_equivalence_errors() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        assert!(DMLPointerType { base: base(), typing: None }.equivalent(
            &DMLPointerType { base: base(), typing: int_ty.clone() }));
        assert!(DMLArrayType { base: base(), typing: None, size: None }.equivalent(
            &DMLArrayType { base: base(), typing: int_ty.clone(), size: None }));
        assert!(DMLHookType { base: base(), arg_types: vec![None] }.equivalent(
            &DMLHookType { base: base(), arg_types: vec![int_ty.clone()] }));
        assert!(DMLFunctionType {
            base: base(), arg_types: vec![None], vararg: false, return_ty: None,
        }.equivalent(&DMLFunctionType {
            base: base(), arg_types: vec![int_ty.clone()], vararg: false, return_ty: int_ty,
        }));
    }

    #[test]
    fn cross_variant_types_are_never_equivalent() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let pointer = DMLConcreteType::Pointer(DMLPointerType {
            base: base(), typing: int_ty.clone(),
        });
        let array = DMLConcreteType::Array(DMLArrayType {
            base: base(), typing: int_ty, size: None,
        });
        assert!(!pointer.equivalent(&array));

        let trait_ty = DMLTraitType { base: base(), decl_name: name("template_t") };
        let sequence = DMLConcreteType::Sequence(DMLSequenceType {
            base: base(), trait_type: trait_ty.clone(),
        });
        assert!(!DMLConcreteType::Trait(trait_ty).equivalent(&sequence));
    }

    #[test]
    fn anonymous_struct_labels_are_fresh() {
        let first = DMLStructLabel::new_from_maybe_string(None);
        let second = DMLStructLabel::new_from_maybe_string(None);
        assert_ne!(first, second);
        let labeled = name("named_t");
        assert_eq!(DMLStructLabel::new_from_maybe_string(Some(labeled.clone())),
                   DMLStructLabel::Labeled(labeled));
    }

    #[test]
    fn dmltype_equivalent_none_and_mismatch() {
        let int_ty: DMLType = DMLConcreteType::Int(IntType::int(base(), true)).into();
        let bool_ty: DMLType = DMLConcreteType::Bool(base().into()).into();
        let broken_alias: DMLType = DMLConcreteType::Typedef(DMLTypedefType {
            base: base(), decl_name: name("broken_t"), underlying: None,
        }).into();
        assert!(dmltype_equivalent(&None, &None));
        assert!(dmltype_equivalent(&None, &int_ty));
        assert!(dmltype_equivalent(&int_ty, &None));
        assert!(dmltype_equivalent(&broken_alias, &int_ty));
        assert!(!dmltype_equivalent(&int_ty, &bool_ty));
    }

    #[test]
    fn peel_one_variants() {
        let concrete_int = DMLConcreteType::Int(IntType::int(base(), true));
        let int_ty: DMLType = concrete_int.clone().into();
        let typedef_type = DMLConcreteType::Typedef(DMLTypedefType {
            base: base(), decl_name: name("int_t"), underlying: int_ty.clone(),
        });
        let array = DMLConcreteType::Array(DMLArrayType {
            base: base(), typing: int_ty.clone(), size: None,
        });
        let pointer = DMLConcreteType::Pointer(DMLPointerType {
            base: base(), typing: int_ty.clone(),
        });
        let vector = DMLConcreteType::Vector(DMLVectorType {
            base: base(), typing: int_ty.clone(),
        });
        assert_eq!(typedef_type.peel_one(), Some(&int_ty));
        assert_eq!(array.peel_one(), Some(&int_ty));
        assert_eq!(pointer.peel_one(), Some(&int_ty));
        assert_eq!(vector.peel_one(), Some(&int_ty));
        assert_eq!(concrete_int.peel_one(), None);
        assert_eq!(DMLConcreteType::StructType(DMLStructType {
            base: base(), label: DMLStructLabel::Anonymous(99), members: vec![],
        }).peel_one(), None);
    }
}

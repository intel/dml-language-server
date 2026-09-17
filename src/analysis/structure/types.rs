//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::parsing::{misc, types};
use crate::analysis::parsing::tree::{LeafToken, TreeElement, ZeroSpan};
use crate::analysis::{DMLError, LocalDMLError, TokenKind};
use crate::analysis::structure::expressions::{DMLString, Expression,
                                              ExpressionKind};
use crate::analysis::{FileSpec, DeclarationSpan};
use crate::analysis::templating::types::{self as concrete_types, DMLConcreteType, DMLStructLabel, DMLType, GlobalTypeStorage, ResolvingState};
use crate::analysis::parsing::types::CTypeDeclSimple;

use std::collections::BTreeMap;
use std::cmp::Ordering;

use lsp_types::DiagnosticSeverity;
use regex::Regex;
use log::error;
use lazy_static::lazy_static;

/// This file describes UNRESOLVED DML types, these are later converted
/// to resolved types with actual semantics later
/// Structures that do not change when resolved are re-used in the final type
/// description

// Common information for all types
#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct TypeBase {
    pub is_const: bool,
    // This is the area in the file where you would say the type is 'declared'
    // roughly corresponding to some meaningful range that does not necessarily
    // include the binding name
    // For example, here the def area is the area of the "| ... |":
    // |  |
    // long foo;
    // |           | (of the fn pointer type)
    // long (*foo)();
    // |                    |
    // typedef struct { ... } foo;
    pub decl_span: ZeroSpan,
}

impl TypeBase {
    fn from_span(span: ZeroSpan) -> TypeBase {
        TypeBase {
            is_const: false,
            decl_span: span,
        }
    }
    fn from_leaf<'a>(leaf: &LeafToken, file: FileSpec<'a>) -> TypeBase {
        TypeBase::from_span(ZeroSpan::from_range(leaf.range(), file.path))
    }
    fn make_const(self) -> TypeBase {
        TypeBase {
            is_const: true,
            decl_span: self.decl_span,
        }
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.is_const == other.is_const
    }
}

impl DeclarationSpan for TypeBase {
    fn span(&self) -> &ZeroSpan {
        &self.decl_span
    }
}

// Human-readable description of a type
pub trait DescribableType {
    fn describe(&self) -> String;
}

pub trait ResolveableType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType;
}

macro_rules! impl_simple_from {
    ($from_type: ty, $to_type: tt) => {
        impl From<$from_type> for $to_type {
            fn from(val: $from_type) -> $to_type {
                $to_type(val)
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct VoidType(TypeBase);
impl_simple_from!(TypeBase, VoidType);
impl_trait_fns!(VoidType, DeclarationSpan, 0, fn span(&self) -> &ZeroSpan);
impl VoidType {
    pub fn make_const(self) -> VoidType {
        VoidType(self.0.make_const())
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.0.equivalent(&other.0)
    }
}
impl DescribableType for VoidType {
    fn describe(&self) -> String {
        "void".to_string()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct DeviceType(TypeBase);
impl_simple_from!(TypeBase, DeviceType);
impl_trait_fns!(DeviceType, DeclarationSpan, 0, fn span(&self) -> &ZeroSpan);
impl DeviceType {
    pub fn make_const(self) -> DeviceType {
        DeviceType(self.0.make_const())
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.0.equivalent(&other.0)
    }
}
impl DescribableType for DeviceType {
    fn describe(&self) -> String {
        "device".to_string()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct NamedType {
    pub base: TypeBase,
    pub name: DMLString,
}
impl_trait_fns!(NamedType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl NamedType {
    fn make_const(self) -> NamedType {
        NamedType {
            base: self.base.make_const(),
            name: self.name,
        }
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
    }
}
impl ResolveableType for NamedType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        global_info.resolve_named(self, resolving, is_extern, via_indirection, errors)
    }
}

// NOTE: If wanting to describe the underlying type, resolve first
impl DescribableType for NamedType {
    fn describe(&self) -> String {
        self.name.val.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct BoolType(TypeBase);
impl_simple_from!(TypeBase, BoolType);
impl_trait_fns!(BoolType, DeclarationSpan, 0, fn span(&self) -> &ZeroSpan);
impl BoolType {
    pub fn make_const(self) -> BoolType {
        BoolType(self.0.make_const())
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.0.equivalent(&other.0)
    }
}
impl DescribableType for BoolType {
    fn describe(&self) -> String {
        "bool".to_string()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Endianness {
    LE, BE
}

impl Endianness {
    pub fn short_desc(&self) -> &'static str {
        match self {
            Endianness::LE => "le",
            Endianness::BE => "be",
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct EndianIntType {
    base: TypeBase,
    endian: Endianness,
    size: u16,
    signed: bool,
}
impl_trait_fns!(EndianIntType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl EndianIntType {
    pub fn make_const(self) -> EndianIntType {
        EndianIntType {
            base: self.base.make_const(),
            endian: self.endian,
            size: self.size,
            signed: self.signed,
        }
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.endian == other.endian
            && self.size == other.size
            && self.signed == other.signed
    }
}

impl DescribableType for EndianIntType {
    fn describe(&self) -> String {
        format!("{}int{}_{}_t",
                if self.signed {
                    ""
                } else {
                    "u"
                },
                self.size,
                self.endian.short_desc(),
        )
    }
}

// Some types have differing sizes depending on operating system
// Here we list currently tracked operating systems
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum OSVariant {
    Win64,
    Win32,
    Lin64,
    Lin32,
    Other,
}

impl OSVariant {
    // User-friendly (moderately) description
    pub fn describe(&self) -> &'static str {
        match self {
            OSVariant::Win64 => "64b windows",
            OSVariant::Win32 => "32b windows",
            OSVariant::Lin64 => "64b linux",
            OSVariant::Lin32 => "32b linux",
            OSVariant::Other => "unknown",
        }
    }
}

// Invariant: All VariantSizeMap should contain the 'Other' key
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct VariantSizeMap(BTreeMap<OSVariant,u16>);

impl VariantSizeMap {
    pub fn simple_size(size: u16) -> VariantSizeMap {
        let mut hm: BTreeMap<OSVariant, u16> = BTreeMap::default();
        hm.insert(OSVariant::Other, size);
        VariantSizeMap(hm)
    }

    pub fn new<const N: usize>(
        default_size: u16,
        other_sizes: [(OSVariant, u16); N]) -> VariantSizeMap {
        let mut hm: BTreeMap<OSVariant, u16> = BTreeMap::default();
        hm.insert(OSVariant::Other, default_size);
        for (var, size) in other_sizes {
            if hm.contains_key(&var) {
                error!("Internal Error: Attempting to create a variant size \
                        with duplicate key {:?} (duplicate discarded)", var);
                continue;
            }
            hm.insert(var, size);
        }
        VariantSizeMap(hm)
    }

    // Returns the OSVariants for which the sizes are different
    fn compare_size(&self, other: &VariantSizeMap) -> Vec<OSVariant> {
        let mut conflicting_sizes: Vec<OSVariant> = Vec::default();

        for (var, sz) in &self.0 {
            let cmp_with = if !other.0.contains_key(var) {
                OSVariant::Other
            } else {
                *var
            };
            // Guaranteed by if check + invariant
            if sz != other.0.get(&cmp_with).unwrap() {
                conflicting_sizes.push(*var);
            }
        }
        for (var, sz) in &other.0 {
            // Already checked
            if self.0.contains_key(var) {
                continue;
            }
            // Guaranteed by invariant
            if sz != self.0.get(&OSVariant::Other).unwrap() {
                conflicting_sizes.push(*var);
            }
        }
        conflicting_sizes
    }

    pub fn compare_size_with_fixed(&self, other: u16) -> Vec<OSVariant> {
        let mut conflicting_sizes: Vec<OSVariant> = Vec::default();

        for (var, sz) in &self.0 {
           if sz != &other {
                conflicting_sizes.push(*var);
            }
        }
        conflicting_sizes
    }

    pub fn compare_with_int<T>(&self, other: T) -> Vec<OSVariant>
    where T: Into<u16> {
        self.compare_size_with_fixed(other.into())
    }
}

// Ordering a variantsizemap is not entirely well defined,
// - Inheriting from eq, A == B iff there are no conflicting sizes
// - A > B iff
//   for every size S in A, if the corresponding key exists in B, A(S) > B(S)
//                          if it does not A(S) > B(OTHER)
//   for every size S in B, if the corresponding key exists in B, A(S) < B(S)
//                          if it does not B(S) < A(OTHER)
// Similarly for B < A
// For inconsistent sizes (there is a S1 and S2 so that A(S1) > B(S1)
//                         and A(S2) < B(S2))
// this returns None
// These functions are similar to the ones provided by
// Ord and Eq, however we will use the std:: ones for content-based
// equality and ordering for containers, and these for semantics
impl VariantSizeMap {
    pub fn equal(&self, other: &Self) -> bool {
        self.compare_size(other).is_empty()
    }
    pub fn compare(&self, other: &Self) -> Option<Ordering> {
        let mut can_be_larger = true;
        let mut can_be_smaller = true;
        let mut is_equal = true;
        for (var, sz) in &self.0 {
            let cmp_with = if !other.0.contains_key(var) {
                OSVariant::Other
            } else {
                *var
            };
            // Guaranteed by if check + invariant
            let cmp = sz.cmp(other.0.get(&cmp_with).unwrap());
            if cmp.is_ne() {
                is_equal = false;
            }
            match cmp {
                Ordering::Less => can_be_larger = false,
                Ordering::Greater => can_be_smaller = false,
                _ => (),
            }
        }
        for (var, sz) in &other.0 {
            // Already checked
            if self.0.contains_key(var) {
                continue;
            }
            // Guaranteed by invariant
            let cmp = self.0.get(&OSVariant::Other).unwrap().cmp(sz);
            if cmp.is_ne() {
                is_equal = false;
            }
            match cmp {
                Ordering::Less => can_be_larger = false,
                Ordering::Greater => can_be_smaller = false,
                _ => (),
            }
        }
        if is_equal {
            return Some(Ordering::Equal);
        }
        match (can_be_larger, can_be_smaller) {
            (true, true) => unreachable!(
                "INTERNAL LOGIC ERROR: Comparison of VariantSizeMap gave \
                 impossible result between {:?} and {:?}",
                self, other),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            _ => None,
        }
    }
}


impl_trait_fns!(IntType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IntType {
    base: TypeBase,
    size: VariantSizeMap,
    signed: bool,
}

impl IntType {
    pub fn make_const(self) -> IntType {
        IntType {
            base: self.base.make_const(),
            size: self.size,
            signed: self.signed,
        }
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.size == other.size
            && self.signed == other.signed
    }

    pub fn with_base(self, base: TypeBase) -> IntType {
        IntType {
            base,
            size: self.size,
            signed: self.signed,
        }
    }
}

impl IntType {
    pub fn char(base: TypeBase) -> IntType {
        IntType {
            base,
            signed: true,
            size: VariantSizeMap::simple_size(8),
        }
    }
    pub fn int(base: TypeBase, signed: bool) -> IntType {
        IntType {
            base,
            signed,
            size: VariantSizeMap::simple_size(32),
        }
    }

    pub fn int64(base: TypeBase, signed: bool) -> IntType {
        IntType {
            base,
            signed,
            size: VariantSizeMap::simple_size(64),
        }
    }

    pub fn long(base: TypeBase, signed: bool) -> IntType {
        IntType {
            base,
            signed,
            size: VariantSizeMap::new(
                64,
                [(OSVariant::Win64, 32),
                 (OSVariant::Win32, 32)])
        }
    }
    pub fn size_t(base: TypeBase, signed: bool) -> IntType {
        IntType {
            base,
            signed,
            size: VariantSizeMap::simple_size(64),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct FloatType(TypeBase);
impl_simple_from!(TypeBase, FloatType);
impl_trait_fns!(FloatType, DeclarationSpan, 0, fn span(&self) -> &ZeroSpan);
impl FloatType {
    pub fn make_const(self) -> FloatType {
        FloatType(self.0.make_const())
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.0.equivalent(&other.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct DoubleType(TypeBase);
impl_simple_from!(TypeBase, DoubleType);
impl_trait_fns!(DoubleType, DeclarationSpan, 0, fn span(&self) -> &ZeroSpan);
impl DoubleType {
    pub fn make_const(self) -> DoubleType {
        DoubleType(self.0.make_const())
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.0.equivalent(&other.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedArrayType {
    base: TypeBase,
    size: Option<Expression>,
    unresolved: Box<UnresolvedType>,
}
impl_trait_fns!(UnresolvedArrayType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl UnresolvedArrayType {
    fn make_const(self) -> Self {
        UnresolvedArrayType {
            base: self.base.make_const(),
            size: self.size,
            unresolved: self.unresolved,
        }
    }
}
impl ResolveableType for UnresolvedArrayType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Array(concrete_types::DMLArrayType {
            base: self.base.clone(),
            // TODO: Expression-to-constant
            size: None,
            typing: self.unresolved.resolve(global_info, resolving, is_extern, via_indirection, errors),
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedPointerType {
    base: TypeBase,
    unresolved: Box<UnresolvedType>,
}
impl_trait_fns!(UnresolvedPointerType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl UnresolvedPointerType {
    fn make_const(self) -> Self {
        UnresolvedPointerType {
            base: self.base.make_const(),
            unresolved: self.unresolved,
        }
    }
}
impl ResolveableType for UnresolvedPointerType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, _via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Pointer(concrete_types::DMLPointerType {
            base: self.base.clone(),
            typing: self.unresolved.resolve(global_info, resolving, is_extern, true, errors),
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedFunctionType {
    base: TypeBase,
    return_type: Box<UnresolvedType>,
    argument_types: Vec<UnresolvedType>,
    varargs: bool,
}
impl_trait_fns!(UnresolvedFunctionType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl UnresolvedFunctionType {
    fn make_const(self) -> Self {
        UnresolvedFunctionType {
            base: self.base.make_const(),
            return_type: self.return_type,
            argument_types: self.argument_types,
            varargs: self.varargs,
        }
    }
}
impl ResolveableType for UnresolvedFunctionType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Function(
            concrete_types::DMLFunctionType {
                base: self.base.clone(),
                arg_types: self.argument_types.iter()
                    .map(|t|t.resolve(global_info, resolving, is_extern, via_indirection, errors))
                    .collect(),
                vararg: self.varargs,
                return_ty: self.return_type.resolve(global_info, resolving, is_extern, via_indirection, errors),
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedVectorType {
    base: TypeBase,
    unresolved: Box<UnresolvedType>
}
impl_trait_fns!(UnresolvedVectorType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl UnresolvedVectorType {
    fn make_const(self) -> Self {
        UnresolvedVectorType {
            base: self.base.make_const(),
            unresolved: self.unresolved,
        }
    }
}
impl ResolveableType for UnresolvedVectorType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, _via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Vector(concrete_types::DMLVectorType {
            base: self.base.clone(),
            typing: self.unresolved.resolve(global_info, resolving, is_extern, true, errors),
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct BitSlice {
    pub lsb: Option<Expression>,
    pub msb: Option<Expression>,
    pub typing: IntType,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct BitfieldsType {
    pub base: TypeBase,
    pub sizing: u8,
    pub members: Vec<(DMLString, BitSlice)>,
}
impl_trait_fns!(BitfieldsType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for BitfieldsType {
    fn resolve(&self, _global_info: &mut GlobalTypeStorage,
              _resolving: &mut ResolvingState,
              _is_extern: bool, _via_indirection: bool,
              _errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Bitfields(self.clone()).into()
    }
}
impl BitfieldsType {
    pub fn make_const(self) -> BitfieldsType {
        BitfieldsType {
            base: self.base.make_const(),
            sizing: self.sizing,
            members: self.members,
        }
    }
    pub fn equivalent(&self, other: &Self) -> bool {
        self.base.equivalent(&other.base)
            && self.sizing == other.sizing
            && self.members.len() == other.members.len()
            && self.members.iter().zip(other.members.iter())
            .all(|((i1_name, i1_bitslice), (i2_name, i2_bitslice))|
                 i1_name.val == i2_name.val
                 && i1_bitslice == i2_bitslice)
    }
}

fn ast_to_bitfield<'a>(content: &types::BitfieldsContent,
                       report: &mut Vec<LocalDMLError>,
                       file: FileSpec<'a>) -> UnresolvedType {
    let mut sizing = content.iconst.read_leaf(file.file)
        // Should be guaranteed by parser
        .map(|s|s.as_str().parse::<i64>().unwrap())
        // Default to largest available size if the token was missing
        .unwrap_or(64);
    if sizing > 64 {
        report.push(LocalDMLError {
            range: content.iconst.range(),
            description: "Bitfields size cannot be > than 64 bits".to_string(),
        });
        sizing = 64;
    } else if sizing < 1 {
        report.push(LocalDMLError {
            range: content.iconst.range(),
            description: "Bitfields size cannot be less than 1".to_string(),
        });
        sizing = 1;
    };
    let sizing = sizing as u8;
    fn content_to_bitslice<'a>(content: &types::BitfieldsDeclContent,
                               _bitfield_size: u8,
                               report: &mut Vec<LocalDMLError>,
                               file: FileSpec<'a>)
                               -> Option<(DMLString, BitSlice)> {
        let (name, typing) = deconstruct_cdecl(content.cdecl.as_actual()?,
                                               report,
                                               file);
        let (lsb, msb) = match &content.range {
            types::BitfieldsRange::Expression(expr) => {
                let expression = ExpressionKind::to_expression(expr, report, file);
                (expression.clone(), expression)
            },
            types::BitfieldsRange::Range(msb, _, lsb) =>
                (ExpressionKind::to_expression(msb, report,file),
                 ExpressionKind::to_expression(lsb, report,file)),
        };
        let UnresolvedType::Int(inttype) = typing else {
            report.push(LocalDMLError {
                range: typing.span().range,
                description:
                    "Bitfield type must be simple integer type".to_string(),
            });
            return None;
        };
        // TODO: verify that LSB and MSB are constant expressions through
        // constant-folding, and verify that their size matches typing
        // and that they are in-bounds for the bitfields
        Some(
            // We can discard unnamed fields at this point, since nothing will
            // refer to them
            (name?,
             BitSlice {
                 lsb, msb, typing: inttype,
             })
        )
    }

    let members = content.fields.iter()
        .filter_map(|content|content_to_bitslice(content, sizing, report, file))
        .collect();
    UnresolvedType::Bitfields(BitfieldsType {
        base: TypeBase::from_span(
            ZeroSpan::from_range(content.range(), file.path)),
        sizing,
        members,
    })
}

// Some labels are internally auto-generated, and thus do not have a span
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct Label {
    name: String,
    span: Option<ZeroSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedStructType {
    pub base: TypeBase,
    pub members: Vec<(Option<DMLString>, UnresolvedType)>,
}
impl_trait_fns!(UnresolvedStructType, DeclarationSpan, base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for UnresolvedStructType {
    // NOTE: struct types resolved in this way are always anonymous, when we 'resolve' a proper typedeffed
    // struct type we will add the proper label
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::StructType(
            self.resolve_to_struct(None, global_info, resolving,
                                   is_extern, via_indirection, errors))
            .into()
    }
}

impl UnresolvedStructType {
    fn resolve_to_struct(&self, label: Option<DMLString>, global_info: &mut GlobalTypeStorage,
                        resolving: &mut ResolvingState,
                        is_extern: bool, via_indirection: bool,
                        errors: &mut Vec<DMLError>)
    -> concrete_types::DMLStructType {
        concrete_types::DMLStructType {
            label: DMLStructLabel::new_from_maybe_string(label),
            base: self.base.clone(),
            members: self.members.iter()
                .map(|(name, t)| {
                    (name.clone(),
                     t.resolve(global_info, resolving, is_extern, via_indirection, errors))
                })
                .collect(),
        }
    }
    fn make_const(self) -> UnresolvedStructType {
        UnresolvedStructType {
            base: self.base.make_const(),
            members: self.members,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedLayoutType {
    pub base: UnresolvedStructType,
    pub endianness: Option<Endianness>,
}
impl_trait_fns!(UnresolvedLayoutType, DeclarationSpan,
                base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for UnresolvedLayoutType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Layout(
            concrete_types::DMLLayoutType {
                base: self.base.resolve_to_struct(None, global_info, resolving,
                                                   is_extern, via_indirection, errors),
                endianness: self.endianness.clone(),
        }).into()
    }
}
impl UnresolvedLayoutType {
    fn make_const(self) -> UnresolvedLayoutType {
        UnresolvedLayoutType {
            base: self.base.make_const(),
            endianness: self.endianness,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedSequenceType {
    pub base: TypeBase,
    pub trait_name: NamedType,
}
impl_trait_fns!(UnresolvedSequenceType, DeclarationSpan,
                base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for UnresolvedSequenceType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        let resolved = global_info.resolve_named(
            &self.trait_name, resolving, is_extern, via_indirection, errors);
        match resolved.as_deref() {
            Some(DMLConcreteType::Trait(trait_type)) =>
                DMLConcreteType::Sequence(concrete_types::DMLSequenceType {
                    base: self.base.clone(),
                    trait_type: trait_type.clone(),
                }).into(),
            Some(_) => {
                errors.push(DMLError {
                    span: self.trait_name.name.span,
                    description: format!(
                        "'{}' is not a template, and cannot be \
                         used in 'sequence(...)'", self.trait_name.name.val),
                    related: vec![],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
                None
            },
            // resolve_named() already reported an "unknown type" error
            None => None,
        }
    }
}
impl UnresolvedSequenceType {
    fn make_const(self) -> UnresolvedSequenceType {
        UnresolvedSequenceType {
            base: self.base.make_const(),
            trait_name: self.trait_name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedHookType {
    pub base: TypeBase,
    pub arg_types: Vec<UnresolvedType>,
}
impl_trait_fns!(UnresolvedHookType, DeclarationSpan,
                base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for UnresolvedHookType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        DMLConcreteType::Hook(
            concrete_types::DMLHookType {
                base: self.base.clone(),
                arg_types: self.arg_types.iter()
                    .map(|t|t.resolve(global_info, resolving, is_extern, via_indirection, errors))
                    .collect(),
        }).into()
    }
}
impl UnresolvedHookType {
    fn make_const(self) -> UnresolvedHookType {
        UnresolvedHookType {
            base: self.base.make_const(),
            arg_types: self.arg_types,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct UnresolvedTypeOf {
    pub base: TypeBase,
    pub of: Expression,
}
impl_trait_fns!(UnresolvedTypeOf, DeclarationSpan,
                base, fn span(&self) -> &ZeroSpan);
impl ResolveableType for UnresolvedTypeOf {
    fn resolve(&self, _global_info: &mut GlobalTypeStorage,
              _resolving: &mut ResolvingState,
              _is_extern: bool, _via_indirection: bool,
              _errors: &mut Vec<DMLError>) -> DMLType {
        // TODO: Figure out the type on an expression
        DMLType::None
    }
}
impl UnresolvedTypeOf {
    fn make_const(self) -> UnresolvedTypeOf {
        UnresolvedTypeOf {
            base: self.base.make_const(),
            of: self.of,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum UnresolvedType {
    // NOTE: Named types here may be further resolved later during resolution,
    // especially notable for trait types
    Named(NamedType),
    Device(DeviceType),
    Bool(BoolType),
    EndianInt(EndianIntType),
    Int(IntType),
    Float(FloatType),
    Double(DoubleType),
    Array(UnresolvedArrayType),
    Pointer(UnresolvedPointerType),
    Function(UnresolvedFunctionType),
    Vector(UnresolvedVectorType),
    Bitfields(BitfieldsType),
    Struct(UnresolvedStructType),
    Layout(UnresolvedLayoutType),
    Sequence(UnresolvedSequenceType),
    Hook(UnresolvedHookType),
    TypeOf(UnresolvedTypeOf),
    Void(VoidType),
    // For types that fail to resolve for whatever reason, but we still
    // need to hold a type for. The span is the decl span of the type,
    // the vector is valid (or invalid) subtypes it might contain
    // For example void (*)(*) would be a pointer to a function type
    // returning void with one argument that is a pointer to an invalid
    // type
    Invalid(ZeroSpan),
}

impl UnresolvedType {
    pub fn make_invalid(span: ZeroSpan) -> Self {
        Self::Invalid(span)
    }
    pub fn make_const(self) -> Self {
        match self {
            Self::Named(i) => Self::Named(i.make_const()),
            Self::Device(i) => Self::Device(i.make_const()),
            Self::Bool(i) => Self::Bool(i.make_const()),
            Self::EndianInt(i) => Self::EndianInt(i.make_const()),
            Self::Int(i) => Self::Int(i.make_const()),
            Self::Float(i) => Self::Float(i.make_const()),
            Self::Double(i) => Self::Double(i.make_const()),
            Self::Array(i) => Self::Array(i.make_const()),
            Self::Pointer(i) => Self::Pointer(i.make_const()),
            Self::Function(i) => Self::Function(i.make_const()),
            Self::Vector(i) => Self::Vector(i.make_const()),
            Self::Struct(i) => Self::Struct(i.make_const()),
            Self::Bitfields(i) => Self::Bitfields(i.make_const()),
            Self::Layout(i) => Self::Layout(i.make_const()),
            Self::Sequence(i) => Self::Sequence(i.make_const()),
            Self::Hook(i) => Self::Hook(i.make_const()),
            Self::TypeOf(i) => Self::TypeOf(i.make_const()),
            // This is a funny, but valid, type
            Self::Void(i) => Self::Void(i.make_const()),
            invalid @ Self::Invalid(_) => invalid,
        }
    }
}

impl DeclarationSpan for UnresolvedType {
    fn span(&self) -> &ZeroSpan {
        match self {
            Self::Named(i) => i.span(),
            Self::Device(i) => i.span(),
            Self::Bool(i) => i.span(),
            Self::EndianInt(i) => i.span(),
            Self::Int(i) => i.span(),
            Self::Float(i) => i.span(),
            Self::Double(i) => i.span(),
            Self::Array(i) => i.span(),
            Self::Pointer(i) => i.span(),
            Self::Function(i) => i.span(),
            Self::Vector(i) => i.span(),
            Self::Struct(i) => i.span(),
            Self::Bitfields(i) => i.span(),
            Self::Layout(i) => i.span(),
            Self::Sequence(i) => i.span(),
            Self::Hook(i) => i.span(),
            Self::TypeOf(i) => i.span(),
            Self::Void(i) => i.span(),
            Self::Invalid(s) => s,
        }
    }
}

impl ResolveableType for UnresolvedType {
    fn resolve(&self, global_info: &mut GlobalTypeStorage,
              resolving: &mut ResolvingState,
              is_extern: bool, via_indirection: bool,
              errors: &mut Vec<DMLError>) -> DMLType {
        match self {
            Self::Device(i) => DMLConcreteType::Device(i.clone()).into(),
            Self::Bool(i) => DMLConcreteType::Bool(i.clone()).into(),
            Self::EndianInt(i) => DMLConcreteType::EndianInt(i.clone()).into(),
            Self::Int(i) => DMLConcreteType::Int(i.clone()).into(),
            Self::Float(i) => DMLConcreteType::Float(i.clone()).into(),
            Self::Double(i) => DMLConcreteType::Double(i.clone()).into(),
            Self::Void(i) => DMLConcreteType::Void(i.clone()).into(),
            Self::Named(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Array(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Pointer(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Function(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Vector(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Struct(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Bitfields(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Layout(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Sequence(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Hook(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::TypeOf(i) => i.resolve(global_info, resolving, is_extern, via_indirection, errors),
            Self::Invalid(_) => None,
        }
    }
}

// Returns the (binding name, type) of the typedecl
#[allow(clippy::ptr_arg)]
pub fn deconstruct_typedecl<'a>(
    content: &misc::TypeDeclContent,
    outside_type: UnresolvedType,
    report: &mut Vec<LocalDMLError>,
    file: FileSpec<'a>) -> (Option<DMLString>,
                            Option<UnresolvedType>) {
    // TODO/NOTE: Careful thought needs to be put into the failure
    // modes of this function
    // inner_decl = typedeclcontent to further handle
    // new_ident = identifier from inner handling
    // inner_type = type after inner handling
    let (inner_decl, new_ident, inner_type) = match content {
        misc::TypeDeclContent::Ident(tok) =>
            (None, DMLString::from_token(tok, file), outside_type),
        misc::TypeDeclContent::Array(inner_decl, _, size, _) =>
            (Some(inner_decl),
             None,
             UnresolvedType::Array(UnresolvedArrayType {
                 unresolved: Box::new(outside_type),
                 size: ExpressionKind::to_expression(size, report, file),
                 base: TypeBase::from_span(
                     ZeroSpan::from_range(content.range(), file.path)),
             })),
        misc::TypeDeclContent::Fun(inner_decl, _, args, varargs, _) => {
            let unresolved_args =
                cdecls_to_members(args.iter().map(|(cdecl,_)|cdecl),
                                  report, file)
                .into_iter().map(|(_,arg_type)|arg_type)
                .collect();
            (Some(inner_decl),
             None,
             UnresolvedType::Function(UnresolvedFunctionType {
                 return_type: Box::new(outside_type),
                 argument_types: unresolved_args,
                 varargs: varargs.is_some(),
                 base: TypeBase::from_span(
                     ZeroSpan::from_range(content.range(), file.path)),
             }))
        },
        misc::TypeDeclContent::Parens(_, modifiers, inner_decl, _) => {
            let mut modified_outer = outside_type;
            for modifier in modifiers {
                // Should be unable to fail based on parsing rules
                match modifier.get_token().unwrap().kind {
                    TokenKind::Const =>
                        modified_outer = modified_outer.make_const(),
                    TokenKind::Multiply =>
                        modified_outer = UnresolvedType::Pointer(
                            UnresolvedPointerType {
                                base: TypeBase::from_span(ZeroSpan::combine(
                                    *modified_outer.span(),
                                    ZeroSpan::from_range(modifier.range(),
                                                         file.path))),
                                unresolved: Box::new(modified_outer),
                            }),
                    _ => internal_error!(
                        "Unexpected token in cdecl modifier list: \
                         {:?}", modifier.read_leaf(file.file).unwrap()),
                }
            }
            (Some(inner_decl), None, modified_outer)
        },
    };
    if let Some(decl) = inner_decl {
        if let Some(inner_content) = &decl.content {
            deconstruct_typedecl(
                inner_content.as_actual()
                    .expect("Inner cdecl was some-d but missing."),
                inner_type,
                report,
                file)
        } else {
            // TODO: is this a syntax error?
            (new_ident, Some(inner_type))
        }
    } else {
        (new_ident, Some(inner_type))
    }
}

#[allow(clippy::ptr_arg)]
fn name_to_integer<'a>(name: &str,
                       leaf: &LeafToken,
                       _report: &mut Vec<LocalDMLError>,
                       file: FileSpec<'a>) -> Option<UnresolvedType> {
    lazy_static! {
        static ref INT_RE: Regex =
            Regex::new(r"^(u?)int([1-5][0-9]?|6[0-4]?|[789])(_be_t|_le_t)?$")
            .unwrap();
    }
    if let Some(captures) = INT_RE.captures(name) {
        let signed = captures.get(1).is_none_or(|m|m.as_str().is_empty());
        let size = match captures.get(2).unwrap()
            .as_str().parse::<u16>()
        {
            Ok(s) => s,
            Err(e) => {
                error!("Internal Error: Wanted to parse '{}' to integer in \
                        type, but couldnt. {:?}",
                       captures.get(2).unwrap().as_str(), e);
                return None;
            }
        };
        let endianness = captures.get(3).map(
            |en|if en.as_str() == "_be_t" { Endianness::BE }
            else { Endianness::LE });
        if let Some(en) = endianness {
            Some(UnresolvedType::EndianInt(EndianIntType {
                base: TypeBase::from_leaf(leaf, file),
                endian: en,
                size,
                signed,
            }))
        } else {
            Some(UnresolvedType::Int(IntType {
                base: TypeBase::from_leaf(leaf, file),
                size: VariantSizeMap::simple_size(size),
                signed,
            }))
        }
    } else {
        None
    }
}

pub fn name_to_primitive_or_named<'a>(leaf: &LeafToken,
                                      report: &mut Vec<LocalDMLError>,
                                      file: FileSpec<'a>)
                                      -> Option<UnresolvedType> {
    let name = leaf.read_leaf(file.file)?;
    // NOTE: Some built-in types not mentioned here are defined as
    // built-in named types instead
    // NOTE/TODO: In simics 7, there is an integer_t type available
    Some(match name.as_str() {
        "void" => UnresolvedType::Void(TypeBase::from_leaf(leaf, file).into()),
        "bool" => UnresolvedType::Bool(TypeBase::from_leaf(leaf, file).into()),
        "float" => UnresolvedType::Float(
            TypeBase::from_leaf(leaf, file).into()),
        "double" => UnresolvedType::Double(
            TypeBase::from_leaf(leaf, file).into()),
        _ => if let Some(typ) = name_to_integer(
            name.as_str(), leaf, report, file) {
            typ
        } else {
            UnresolvedType::Named(NamedType {
                base: TypeBase::from_leaf(leaf, file),
                // Guaranteed by the read above
                name: DMLString::from_token(leaf, file).unwrap(),
            })
        }
    })
}

pub fn cdecls_to_members<'t, 'a, T>(i: T,
                                    report: &mut Vec<LocalDMLError>,
                                    file: FileSpec<'a>)
                                    -> Vec<(Option<DMLString>, UnresolvedType)>
where
    T: Iterator<Item = &'t misc::CDecl>
{
    i.flat_map(misc::CDecl::as_actual)
     .map(|cdecl|deconstruct_cdecl(cdecl, report, file))
     .collect()
}

pub fn string_to_endianness(str: &str) -> Option<Endianness> {
    match str {
        r#""big-endian""# => Some(Endianness::BE),
        r#""little-endian""# => Some(Endianness::LE),
        _ => None,
    }
}

fn ast_to_unresolved_layout<'a>(layout_ast: &types::LayoutContent,
                                report: &mut Vec<LocalDMLError>,
                                file: FileSpec<'a>)
                                -> UnresolvedType {
    let endianness = layout_ast.byteorder.read_leaf(file.file)
        .as_deref()
        .and_then(string_to_endianness);
    let struct_base = ast_to_unresolved_struct_base(
        &layout_ast.layout,
        &layout_ast.rbrace,
        &layout_ast.fields,
        report,
        file);
    UnresolvedType::Layout(UnresolvedLayoutType {
        base: struct_base,
        endianness,
    })
}

#[allow(clippy::ptr_arg)]
fn ast_to_unresolved_struct_base<'a>(
    token: &LeafToken,
    rbrace: &LeafToken,
    members: &Vec<(misc::CDecl, LeafToken)>,
    report: &mut Vec<LocalDMLError>,
    file: FileSpec<'a>) -> UnresolvedStructType {
    let members = cdecls_to_members(
        members.iter().map(|(cdecl,_)|cdecl),
        report,
        file);
    let start = ZeroSpan::from_range(token.range(), file.path);
    let end = ZeroSpan::from_range(rbrace.range(), file.path);
    UnresolvedStructType {
        base: TypeBase::from_span(ZeroSpan::combine(start, end)),
        members,
    }
}

fn ast_to_unresolved_struct<'a>(struct_ast: &types::StructTypeContent,
                                report: &mut Vec<LocalDMLError>,
                                file: FileSpec<'a>)
                                -> Option<UnresolvedType> {
    Some(UnresolvedType::Struct(ast_to_unresolved_struct_base(
        &struct_ast.structtok,
        &struct_ast.rbrace,
        &struct_ast.members,
        report, file)))
}

fn ast_to_unresolved_sequence<'a>(content: &types::SequenceContent,
                                  _report: &mut Vec<LocalDMLError>,
                                  file: FileSpec<'a>) -> UnresolvedType {
    if let Some(name) = DMLString::from_token(&content.ident, file) {
        return UnresolvedType::Sequence(
            UnresolvedSequenceType {
                trait_name: NamedType {
                    base: TypeBase::from_leaf(&content.ident, file),
                    name,
                },
                base: TypeBase::from_span(
                    ZeroSpan::from_range(content.range(), file.path)),
            });
    }

    UnresolvedType::make_invalid(
        ZeroSpan::from_range(content.range(), file.path))
}

fn ast_to_unresolved_hook<'a>(content: &types::HookTypeContent,
                              report: &mut Vec<LocalDMLError>,
                              file: FileSpec<'a>) -> UnresolvedType {
    UnresolvedType::Hook(UnresolvedHookType {
        base: TypeBase::from_span(
            ZeroSpan::from_range(content.range(), file.path)),
        arg_types: content.args.iter()
            .map(|(maybe_cdecl, _)| {
                if let Some(cdecl) = maybe_cdecl.as_actual() {
                    let (_name, typ) = deconstruct_cdecl(cdecl, report, file);
                    // TODO: Do we need to check name here? Existence? Uniqueness?
                    typ
                } else {
                    UnresolvedType::make_invalid(
                        ZeroSpan::from_range(maybe_cdecl.range(), file.path))
                }
            })
            .collect(),
    })
}

fn ast_to_unresolved_typeof<'a>(content: &types::TypeOfContent,
                                report: &mut Vec<LocalDMLError>,
                                file: FileSpec<'a>) -> UnresolvedType {
    if let Some(expr) = ExpressionKind::to_expression(
                            &content.of, report, file) {
        UnresolvedType::TypeOf(
            UnresolvedTypeOf {
                base: TypeBase::from_span(
                    ZeroSpan::from_range(content.range(), file.path)),
                of: expr,
            })
    } else {
        UnresolvedType::make_invalid(
            ZeroSpan::from_range(content.range(), file.path))
    }
}

pub fn deconstruct_type<'a>(content: &types::BaseTypeContent,
                            report: &mut Vec<LocalDMLError>,
                            file: FileSpec<'a>) -> Option<UnresolvedType> {
    match content {
        types::BaseTypeContent::Ident(leaf) =>
            name_to_primitive_or_named(leaf, report, file),
        types::BaseTypeContent::Struct(struct_ast) =>
            ast_to_unresolved_struct(struct_ast, report, file),
        types::BaseTypeContent::Layout(layout_ast) =>
            Some(ast_to_unresolved_layout(layout_ast, report, file)),
        types::BaseTypeContent::Bitfields(bitfield_ast) =>
            Some(ast_to_bitfield(bitfield_ast, report, file)),
        types::BaseTypeContent::Sequence(sequence_ast) =>
            Some(ast_to_unresolved_sequence(sequence_ast, report, file)),
        types::BaseTypeContent::Hook(hook_ast) =>
            Some(ast_to_unresolved_hook(hook_ast, report, file)),
        types::BaseTypeContent::TypeOf(typeof_ast) =>
            Some(ast_to_unresolved_typeof(typeof_ast, report, file)),
    }
}

pub fn deconstruct_cdecl<'a>(content: &misc::CDeclContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) ->
    (Option<DMLString>, UnresolvedType)
{
    let mut base = content.base
        .with_content(|c|deconstruct_type(c, report, file), None)
        .unwrap_or_else(||UnresolvedType::make_invalid(
            ZeroSpan::from_range(content.range(), file.path)));
    if content.consttok.is_some() {
        base = base.make_const()
    }
    for modifier in &content.modifiers {
        // Should be unable to fail based on parsing rules
        match modifier.get_token().unwrap().kind {
            TokenKind::Const => base = base.make_const(),
            TokenKind::Multiply => base = UnresolvedType::Pointer(
                UnresolvedPointerType {
                    base: TypeBase::from_span(ZeroSpan::combine(
                        *base.span(),
                        ZeroSpan::from_range(
                            modifier.range(),
                            file.path))),
                    unresolved: Box::new(base),
                }),
            _ => internal_error!("Unexpected token in cdecl modifier list: \
                                  {:?}",
                                 modifier.read_leaf(file.file).unwrap()),
        }
    }

    match content.decl.content.as_ref() {
        Some(typedecl) => {
            let (name, typing) = deconstruct_typedecl(
                typedecl.as_actual().unwrap(), base, report, file);
            (name, typing.unwrap_or_else(||UnresolvedType::make_invalid(
                ZeroSpan::from_range(content.range(), file.path))))
        },
        None => (None, base),
    }
}


#[allow(clippy::ptr_arg)]
pub fn to_type<'a>(maybe_content: &types::CTypeDecl,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'a>) -> Option<UnresolvedType> {
    let content = maybe_content.as_actual()?;
    let mut base = content.base
        .with_content(|c|deconstruct_type(c, report, file), None)
        .unwrap_or_else(||UnresolvedType::make_invalid(
            ZeroSpan::from_range(content.range(), file.path)));
    if content.consttok.is_some() {
        base = base.make_const();
    }

    fn modify_with_inner(mut b: UnresolvedType,
                         maybe_inner: &CTypeDeclSimple,
                         report: &mut Vec<LocalDMLError>,
                         file: FileSpec<'_>) -> UnresolvedType {
        let Some(inner) = maybe_inner.as_actual() else {
            return b;
        };
        for modifier in &inner.modifiers {
            // Guaranteed by parser
            match modifier.get_token().unwrap().kind {
                TokenKind::Const => b = b.make_const(),
                TokenKind::Multiply => b = UnresolvedType::Pointer(
                    UnresolvedPointerType {
                        base: TypeBase::from_span(ZeroSpan::combine(
                            *b.span(),
                            ZeroSpan::from_range(
                                modifier.range(),
                                file.path))),
                        unresolved: Box::new(b),
                }),
                _ => internal_error!(
                    "Unexpected token in cdecl modifier list: {:?}",
                    modifier.read_leaf(file.file).unwrap()),
            }
        }
        if let Some((_, simple, _)) = &inner.inner {
            b = modify_with_inner(b, simple, report, file);
        }
        b
    }

    Some(modify_with_inner(base, &content.simple, report, file))
}

// TODO: Expand unit tests
#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::parsing::misc::{CDeclContent, CDecl};
    use crate::analysis::structure::expressions::IntegerLiteral;
    use crate::analysis::structure::test::*;

    // NOTE/TODO: For now we are just testing the cdecl re-structuring
    // since it's the most complicated/confusing
    // TODO: Add tests for errors reported in structural conversion
    #[test]
    fn cdecl_fun() {
        let Some(((name, ty), ast_errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "float foo();",
            deconstruct_cdecl,
        ) else {
            panic!("Structural test failed to parse");
        };
        assert!(ast_errors.is_empty(), "AST errors: {:?}", ast_errors);
        assert_eq!(name.as_ref().map(|n|n.val.as_str()), Some("foo"));
        let (return_type, argument_types) =
            assert_match_destruct!(
                &ty,
                UnresolvedType::Function(UnresolvedFunctionType {
                    return_type,
                    argument_types,
                    varargs: false,
                    ..
                }),
                return_type, argument_types);
        assert!(argument_types.is_empty());
        assert_match_destruct!(
            return_type.as_ref(),
            UnresolvedType::Float(FloatType(_)));
    }

    #[test]
    fn cdecl_struct() {
        let Some(((name, ty), ast_errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "struct {
                float a;
                int b;
             } struct_value;",
            deconstruct_cdecl,
        ) else {
            panic!("Structural test failed to parse");
        };
        assert!(ast_errors.is_empty(), "AST errors: {:?}", ast_errors);
        assert_eq!(name.as_ref().map(|n|n.val.as_str()), Some("struct_value"));
        let member_types =
            assert_match_destruct!(
                &ty,
                UnresolvedType::Struct(UnresolvedStructType {
                    members,
                    ..
                }),
                members);
        // Fairly sure lexical order is guaranteed
        let ((floatname, floatty), (intname, intty)) =
            assert_match_destruct!(
                &member_types[..],
                [
                    floatfield,
                    intfield,
                ],
                floatfield, intfield);
        assert_eq!(floatname.as_ref().map(|n|n.val.as_str()), Some("a"));
        assert_match_destruct!(
            floatty,
            UnresolvedType::Float(FloatType(_)));
        assert_eq!(intname.as_ref().map(|n|n.val.as_str()), Some("b"));
        let int_name = assert_match_destruct!(
            intty,
            UnresolvedType::Named(NamedType { name, .. }),
            name);
        assert_eq!(int_name.val.as_str(), "int");
    }

    #[test]
    fn cdecl_const() {
        let Some(((name, ty), ast_errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "const float foo;",
            deconstruct_cdecl,
        ) else {
            panic!("Structural test failed to parse");
        };
        assert!(ast_errors.is_empty(), "AST errors: {:?}", ast_errors);
        assert_eq!(name.as_ref().map(|n|n.val.as_str()), Some("foo"));
        let base = assert_match_destruct!(
            &ty,
            UnresolvedType::Float(FloatType(base)),
            base);
        assert!(base.is_const);
    }

    #[test]
    fn cdecl_complicated() {
        let Some(((name, ty), ast_errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "char (*(*foo())[5])();",
            deconstruct_cdecl,
        ) else {
            panic!("Structural test failed to parse");
        };
        assert!(ast_errors.is_empty(), "AST errors: {:?}", ast_errors);
        assert_eq!(name.as_ref().map(|n|n.val.as_str()), Some("foo"));
        let (return_type, argument_types) =
            assert_match_destruct!(
                &ty,
                UnresolvedType::Function(UnresolvedFunctionType {
                    return_type,
                    argument_types,
                    varargs: false,
                    ..
                }),
                return_type, argument_types);
        let pointer = assert_match_destruct!(
                return_type.as_ref(),
                UnresolvedType::Pointer(UnresolvedPointerType {
                    unresolved,
                    ..
                }),
                unresolved);
        assert_eq!(argument_types, &vec![]);
        let (arr_ty, arr_size) = assert_match_destruct!(
                pointer.as_ref(),
                UnresolvedType::Array(UnresolvedArrayType {
                    unresolved,
                    size: Some(size_expr),
                    ..
                }),
                unresolved,
                size_expr);
        let array_size_val = assert_match_destruct!(
            arr_size.as_ref(),
            ExpressionKind::IntegerLiteral(IntegerLiteral::Unsigned(val)),
            val);
        assert_eq!(array_size_val.val, 5);
        let inner_fn_ty = assert_match_destruct!(
                arr_ty.as_ref(),
                UnresolvedType::Pointer(UnresolvedPointerType {
                    unresolved,
                    ..
                }),
                unresolved);
        let (inner_return_type, inner_argument_types) =
            assert_match_destruct!(
                inner_fn_ty.as_ref(),
                UnresolvedType::Function(UnresolvedFunctionType {
                    return_type,
                    argument_types,
                    varargs: false,
                    ..
                }),
                return_type, argument_types);
        assert_eq!(inner_argument_types, &vec![]);
        let inner_name = assert_match_destruct!(
            inner_return_type.as_ref(),
            UnresolvedType::Named(NamedType { name, .. }),
            name);
        assert_eq!(inner_name.val.as_str(), "char");
    }

    fn unresolved_shape(ty: &UnresolvedType) -> String {
        match ty {
            UnresolvedType::Named(_) => "named".to_string(),
            UnresolvedType::Array(array) =>
                format!("array({})", unresolved_shape(&array.unresolved)),
            UnresolvedType::Pointer(pointer) =>
                format!("pointer({})", unresolved_shape(&pointer.unresolved)),
            UnresolvedType::Vector(vector) =>
                format!("vector({})", unresolved_shape(&vector.unresolved)),
            UnresolvedType::Function(function) =>
                format!("function({})", unresolved_shape(&function.return_type)),
            other => format!("{:?}", other),
        }
    }

    #[test]
    fn cdecl_association_order_matrix() {
        for (source, expected) in [
            ("int *values[2];", "array(pointer(named))"),
            ("int (*values)[2];", "pointer(array(named))"),
            ("int vect *values;", "pointer(vector(named))"),
            ("int vect *values[2];", "array(pointer(vector(named)))"),
            ("int (*callbacks[2])(float);", "array(pointer(function(named)))"),
        ] {
            let Some(((_, ty), errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                source, deconstruct_cdecl) else {
                    panic!("Structural test failed to parse {}", source);
                };
            assert!(errors.is_empty(), "{}: {:?}", source, errors);
            assert_eq!(unresolved_shape(&ty), expected, "{}", source);
        }
    }

    #[test]
    fn cdecl_preserves_nonconstant_array_size_expression() {
        let Some(((_, ty), errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "int values[N + 1];", deconstruct_cdecl) else {
                panic!("Structural test failed to parse");
            };
        assert!(errors.is_empty(), "AST errors: {:?}", errors);
        let size = assert_match_destruct!(
            &ty,
            UnresolvedType::Array(UnresolvedArrayType { size: Some(size), .. }),
            size);
        assert!(!matches!(size.as_ref(), ExpressionKind::IntegerLiteral(_)),
                "non-constant expression must survive structural conversion");
    }

    #[test]
    fn make_const_is_effective_and_idempotent_for_every_type_family() {
        let sources = [
            "named_t value;",
            "bool value;",
            "int32_be_t value;",
            "uint32 value;",
            "float value;",
            "double value;",
            "int value[2];",
            "int *value;",
            "int value(float);",
            "int vect value;",
            "struct { int member; } value;",
            "bitfields 8 { uint8 all @ [7:0]; } value;",
            "layout \"big-endian\" { int member; } value;",
            "sequence(template_t) value;",
            "hook(int) value;",
            "typeof(value) other;",
            "void value;",
        ];
        for source in sources {
            let Some(((_, original), errors)) =
                parse_to_structure::<CDeclContent, CDecl, _, _>(source, deconstruct_cdecl)
            else {
                panic!("Structural test failed to parse {}", source);
            };
            assert!(errors.is_empty(), "{}: {:?}", source, errors);
            let qualified = original.clone().make_const();
            assert_ne!(qualified, original, "{} did not acquire const", source);
            assert_eq!(qualified.clone().make_const(), qualified,
                       "{} const application was not idempotent", source);
        }

        let device = UnresolvedType::Device(eq_test_base().into());
        let const_device = device.clone().make_const();
        assert_ne!(const_device, device);
        assert_eq!(const_device.clone().make_const(), const_device);

        let invalid = UnresolvedType::Invalid(eq_test_span());
        assert_eq!(invalid.clone().make_const(), invalid,
                   "Invalid must remain a stable recovery sentinel");
    }

    // ---- Structural conversion matrix: additional declarator/type kinds ----

    #[test]
    fn cdecl_vect() {
        let Some(((name, ty), ast_errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "int vect foo;",
            deconstruct_cdecl,
        ) else {
            panic!("Structural test failed to parse");
        };
        assert!(ast_errors.is_empty(), "AST errors: {:?}", ast_errors);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
        let inner = assert_match_destruct!(
            &ty,
            UnresolvedType::Vector(UnresolvedVectorType { unresolved, .. }),
            unresolved);
        let int_name = assert_match_destruct!(
            inner.as_ref(),
            UnresolvedType::Named(NamedType { name, .. }),
            name);
        assert_eq!(int_name.val.as_str(), "int");
    }

    #[test]
    fn cdecl_pointer_const_variants() {
        // "const T *p" -- pointee is const, the pointer itself is not.
        let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "const int *foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        let (ptr_base, inner) = assert_match_destruct!(
            &ty,
            UnresolvedType::Pointer(UnresolvedPointerType { base, unresolved, .. }),
            base, unresolved);
        assert!(!ptr_base.is_const, "pointer itself should not be const");
        let pointee_base = assert_match_destruct!(
            inner.as_ref(),
            UnresolvedType::Named(NamedType { base, .. }),
            base);
        assert!(pointee_base.is_const, "pointee should be const");

        // "T * const p" -- the pointer itself is const, the pointee is not.
        let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "int * const foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        let (ptr_base, inner) = assert_match_destruct!(
            &ty,
            UnresolvedType::Pointer(UnresolvedPointerType { base, unresolved, .. }),
            base, unresolved);
        assert!(ptr_base.is_const, "pointer itself should be const");
        let pointee_base = assert_match_destruct!(
            inner.as_ref(),
            UnresolvedType::Named(NamedType { base, .. }),
            base);
        assert!(!pointee_base.is_const, "pointee should not be const");
    }

    #[test]
    fn cdecl_multilevel_pointer_const_positions() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "const int * const *foo;", deconstruct_cdecl) else {
                panic!("Structural test failed to parse");
            };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));

        let (outer_base, middle) = assert_match_destruct!(
            &ty,
            UnresolvedType::Pointer(UnresolvedPointerType { base, unresolved, .. }),
            base, unresolved);
        assert!(!outer_base.is_const, "outer pointer should be unqualified");
        let (middle_base, pointee) = assert_match_destruct!(
            middle.as_ref(),
            UnresolvedType::Pointer(UnresolvedPointerType { base, unresolved, .. }),
            base, unresolved);
        assert!(middle_base.is_const, "middle pointer should be const");
        let pointee_base = assert_match_destruct!(
            pointee.as_ref(), UnresolvedType::Named(NamedType { base, .. }), base);
        assert!(pointee_base.is_const, "base pointee should be const");
    }

    #[test]
    fn cdecl_function_multi_args_and_varargs() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "void foo(int a, float b, ...);", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
        let (return_type, argument_types) = assert_match_destruct!(
            &ty,
            UnresolvedType::Function(UnresolvedFunctionType {
                return_type, argument_types, varargs: true, ..
            }),
            return_type, argument_types);
        assert_match_destruct!(return_type.as_ref(), UnresolvedType::Void(_));
        assert_eq!(argument_types.len(), 2);
        assert_match_destruct!(
            &argument_types[0], UnresolvedType::Named(NamedType { .. }));
        assert_match_destruct!(
            &argument_types[1], UnresolvedType::Float(FloatType(_)));
    }

    #[test]
    fn cdecl_layout() {
        for (byteorder_str, expected) in
            [(r#""big-endian""#, Endianness::BE),
             (r#""little-endian""#, Endianness::LE)] {
            let src = format!(
                "layout {} {{ uint32 a; int32 b; }} foo;", byteorder_str);
            let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                &src, deconstruct_cdecl) else {
                panic!("Structural test failed to parse for {}", byteorder_str);
            };
            assert!(errs.is_empty(), "AST errors for {}: {:?}", byteorder_str, errs);
            assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
            let (endianness, members) = assert_match_destruct!(
                &ty,
                UnresolvedType::Layout(UnresolvedLayoutType {
                    endianness: Some(endianness),
                    base: UnresolvedStructType { members, .. },
                    ..
                }),
                endianness, members);
            assert_eq!(*endianness, expected, "wrong endianness for {}", byteorder_str);
            assert_eq!(members.len(), 2);
            assert_eq!(members[0].0.as_ref().map(|n| n.val.as_str()), Some("a"));
            assert_eq!(members[1].0.as_ref().map(|n| n.val.as_str()), Some("b"));
        }
    }

    #[test]
    fn cdecl_layout_invalid_byteorder() {
        let src = "layout \"middle-endian\" { uint32 a; } foo;";
        let Some(((_, ty), _errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            src, deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        let endianness = assert_match_destruct!(
            &ty,
            UnresolvedType::Layout(UnresolvedLayoutType { endianness, .. }),
            endianness);
        assert!(endianness.is_none(),
                "unrecognized byteorder should not resolve to any Endianness");
    }

    #[test]
    fn cdecl_sequence() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "sequence(my_trait) foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
        let trait_name = assert_match_destruct!(
            &ty,
            UnresolvedType::Sequence(UnresolvedSequenceType {
                trait_name: NamedType { name, .. }, ..
            }),
            name);
        assert_eq!(trait_name.val.as_str(), "my_trait");
    }

    #[test]
    fn cdecl_hook_zero_and_multi_args() {
        let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "hook() foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        let arg_types = assert_match_destruct!(
            &ty,
            UnresolvedType::Hook(UnresolvedHookType { arg_types, .. }),
            arg_types);
        assert!(arg_types.is_empty());

        let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "hook(int a, float b) foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        let arg_types = assert_match_destruct!(
            &ty,
            UnresolvedType::Hook(UnresolvedHookType { arg_types, .. }),
            arg_types);
        assert_eq!(arg_types.len(), 2);
        assert_match_destruct!(&arg_types[0], UnresolvedType::Named(NamedType { .. }));
        assert_match_destruct!(&arg_types[1], UnresolvedType::Float(FloatType(_)));
    }

    // Regression test: an argument declaration with no binding name (as is
    // legal inside "hook(...)" and bare function-typed parameters, e.g.
    // "int cb(int, float)") must keep its base type after structural
    // conversion. deconstruct_cdecl previously discarded the base type and
    // produced UnresolvedType::Invalid whenever the declarator had no
    // identifier, which meant such arguments always resolved to `None` and
    // silently defeated method-override argument-type equivalence checks.
    #[test]
    fn cdecl_unnamed_declarator_preserves_base_type() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "int", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert!(name.is_none(), "unnamed declaration should have no name");
        // At this unresolved structural stage, bare identifiers (including
        // builtin type names) are represented as Named references, and are
        // only resolved to concrete types (e.g. Int) in a later pass. The
        // key assertion here is that the base type is preserved as a Named
        // reference to "int", rather than discarded as Invalid.
        let name_ref = assert_match_destruct!(
            &ty, UnresolvedType::Named(NamedType { name, .. }), name);
        assert_eq!(name_ref.val.as_str(), "int");
    }

    #[test]
    fn cdecl_hook_unnamed_args_preserve_types() {
        let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "hook(int, float) foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        let arg_types = assert_match_destruct!(
            &ty,
            UnresolvedType::Hook(UnresolvedHookType { arg_types, .. }),
            arg_types);
        assert_eq!(arg_types.len(), 2);
        assert_match_destruct!(&arg_types[0], UnresolvedType::Named(NamedType { .. }));
        assert_match_destruct!(&arg_types[1], UnresolvedType::Float(FloatType(_)));
    }

    #[test]
    fn cdecl_function_unnamed_args_preserve_types() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "int cb(int, float);", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("cb"));
        let (return_type, argument_types) = assert_match_destruct!(
            &ty,
            UnresolvedType::Function(UnresolvedFunctionType {
                return_type, argument_types, varargs: false, ..
            }),
            return_type, argument_types);
        assert_match_destruct!(return_type.as_ref(), UnresolvedType::Named(NamedType { .. }));
        assert_eq!(argument_types.len(), 2);
        assert_match_destruct!(&argument_types[0], UnresolvedType::Named(NamedType { .. }));
        assert_match_destruct!(&argument_types[1], UnresolvedType::Float(FloatType(_)));
    }

    #[test]
    fn cdecl_typeof() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "typeof(x) foo;", deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
        assert_match_destruct!(&ty, UnresolvedType::TypeOf(UnresolvedTypeOf { .. }));
    }

    #[test]
    fn cdecl_bitfields_positive() {
        let Some(((name, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "bitfields 16 { uint8 hi @ [15:8]; uint1 flag @ [7]; } foo;",
            deconstruct_cdecl) else {
            panic!("Structural test failed to parse");
        };
        assert!(errs.is_empty(), "AST errors: {:?}", errs);
        assert_eq!(name.as_ref().map(|n| n.val.as_str()), Some("foo"));
        let (sizing, members) = assert_match_destruct!(
            &ty,
            UnresolvedType::Bitfields(BitfieldsType { sizing, members, .. }),
            sizing, members);
        assert_eq!(*sizing, 16);
        assert_eq!(members.len(), 2);
        assert_eq!(members[0].0.val.as_str(), "hi");
        assert!(members[0].1.msb.is_some());
        assert!(members[0].1.lsb.is_some());
        // Single-bit form: msb and lsb are both set from the same expression.
        assert_eq!(members[1].0.val.as_str(), "flag");
        assert!(members[1].1.msb.is_some());
        assert!(members[1].1.lsb.is_some());
        assert_eq!(members[1].1.msb, members[1].1.lsb,
               "single-bit syntax must copy one expression to both bounds");
        assert_ne!(members[0].1.msb, members[0].1.lsb,
               "explicit msb:lsb syntax must preserve both operands");
    }

    #[test]
    fn cdecl_bitfields_accept_width_boundaries_and_retain_duplicate_members() {
        for (source, expected_width) in [
            ("bitfields 1 { uint1 bit @ [0]; } value;", 1),
            ("bitfields 64 { uint64 all @ [63:0]; } value;", 64),
        ] {
            let Some(((_, ty), errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                source, deconstruct_cdecl) else {
                    panic!("Structural test failed to parse {}", source);
                };
            assert!(errors.is_empty(), "{}: {:?}", source, errors);
            let sizing = assert_match_destruct!(
                &ty, UnresolvedType::Bitfields(BitfieldsType { sizing, .. }), sizing);
            assert_eq!(*sizing, expected_width);
        }

        let Some(((_, ty), errors)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
            "bitfields 8 { uint4 same @ [3:0]; uint4 same @ [7:4]; } value;",
            deconstruct_cdecl) else {
                panic!("Structural test failed to parse duplicate members");
            };
        assert!(errors.is_empty(), "AST errors: {:?}", errors);
        let members = assert_match_destruct!(
            &ty, UnresolvedType::Bitfields(BitfieldsType { members, .. }), members);
        assert_eq!(members.len(), 2, "structuring must not silently overwrite a member");
        assert_eq!(members[0].0.val, "same");
        assert_eq!(members[1].0.val, "same");
    }

    // ---- Primitive-name / fixed-width / endianness table ----

    #[test]
    fn fixed_width_and_endian_int_table() {
        struct Case {
            src: &'static str,
            size: u16,
            signed: bool,
            endian: Option<Endianness>,
        }
        let cases = [
            Case { src: "int1", size: 1, signed: true, endian: None },
            Case { src: "uint1", size: 1, signed: false, endian: None },
            Case { src: "int9", size: 9, signed: true, endian: None },
            Case { src: "uint10", size: 10, signed: false, endian: None },
            Case { src: "int59", size: 59, signed: true, endian: None },
            Case { src: "uint60", size: 60, signed: false, endian: None },
            Case { src: "int63", size: 63, signed: true, endian: None },
            Case { src: "int64", size: 64, signed: true, endian: None },
            Case { src: "uint64", size: 64, signed: false, endian: None },
            Case { src: "int1_be_t", size: 1, signed: true, endian: Some(Endianness::BE) },
            Case { src: "int32_be_t", size: 32, signed: true, endian: Some(Endianness::BE) },
            Case { src: "uint32_le_t", size: 32, signed: false, endian: Some(Endianness::LE) },
            Case { src: "uint64_le_t", size: 64, signed: false, endian: Some(Endianness::LE) },
        ];
        for case in cases {
            let src = format!("{} foo;", case.src);
            let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                &src, deconstruct_cdecl) else {
                panic!("Structural test failed to parse for {}", case.src);
            };
            assert!(errs.is_empty(), "AST errors for {}: {:?}", case.src, errs);
            match case.endian {
                None => {
                    let (size, signed) = assert_match_destruct!(
                        &ty,
                        UnresolvedType::Int(IntType { size, signed, .. }),
                        size, signed);
                    assert!(size.equal(&VariantSizeMap::simple_size(case.size)),
                            "{}: expected size {}, got {:?}", case.src, case.size, size);
                    assert_eq!(*signed, case.signed, "{}: signed mismatch", case.src);
                }
                Some(expected_endian) => {
                    let (size, signed, endian) = assert_match_destruct!(
                        &ty,
                        UnresolvedType::EndianInt(EndianIntType { size, signed, endian, .. }),
                        size, signed, endian);
                    assert_eq!(*size, case.size, "{}: size mismatch", case.src);
                    assert_eq!(*signed, case.signed, "{}: signed mismatch", case.src);
                    assert_eq!(*endian, expected_endian, "{}: endian mismatch", case.src);
                }
            }
        }
    }

    #[test]
    fn fixed_width_int_near_miss_table() {
        // Names that must NOT be interpreted as fixed-width integers.
        for src_name in ["int0", "int65", "my_uint32", "uint32_be_t_suffix"] {
            let src = format!("{} foo;", src_name);
            let Some(((_, ty), _errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                &src, deconstruct_cdecl) else {
                panic!("Structural test failed to parse for {}", src_name);
            };
            let named = assert_match_destruct!(
                &ty,
                UnresolvedType::Named(NamedType { name, .. }),
                name);
            assert_eq!(named.val.as_str(), src_name,
                       "{} should not be recognized as a fixed-width integer", src_name);
        }
    }

    #[test]
    fn primitive_name_table() {
        for src_name in ["void", "bool", "float", "double"] {
            let src = format!("{} foo;", src_name);
            let Some(((_, ty), errs)) = parse_to_structure::<CDeclContent, CDecl, _, _>(
                &src, deconstruct_cdecl) else {
                panic!("Structural test failed to parse for {}", src_name);
            };
            assert!(errs.is_empty(), "AST errors for {}: {:?}", src_name, errs);
            match src_name {
                "void" => assert_match_destruct!(&ty, UnresolvedType::Void(_)),
                "bool" => assert_match_destruct!(&ty, UnresolvedType::Bool(_)),
                "float" => assert_match_destruct!(&ty, UnresolvedType::Float(FloatType(_))),
                "double" => assert_match_destruct!(&ty, UnresolvedType::Double(DoubleType(_))),
                _ => unreachable!(),
            };
        }
    }

    // ---- VariantSizeMap pure unit tests ----

    #[test]
    fn variant_size_map_simple_and_new() {
        let simple = VariantSizeMap::simple_size(32);
        assert!(simple.equal(&VariantSizeMap::simple_size(32)));
        assert!(!simple.equal(&VariantSizeMap::simple_size(64)));

        let with_variants = VariantSizeMap::new(
            64, [(OSVariant::Win64, 32), (OSVariant::Win32, 32)]);
        let conflicts = with_variants.compare_size_with_fixed(64);
        assert!(conflicts.contains(&OSVariant::Win64));
        assert!(conflicts.contains(&OSVariant::Win32));
        assert!(!conflicts.contains(&OSVariant::Other));
    }

    #[test]
    fn variant_size_map_compare_with_int() {
        let map = VariantSizeMap::simple_size(32);
        assert!(map.compare_with_int(32u16).is_empty());
        assert_eq!(map.compare_with_int(64u16), vec![OSVariant::Other]);
    }

    #[test]
    fn variant_size_map_equal_and_compare() {
        let a = VariantSizeMap::simple_size(32);
        let b = VariantSizeMap::simple_size(32);
        let c = VariantSizeMap::simple_size(64);
        assert!(a.equal(&b));
        assert_eq!(a.compare(&b), Some(Ordering::Equal));
        assert_eq!(a.compare(&c), Some(Ordering::Less));
        assert_eq!(c.compare(&a), Some(Ordering::Greater));

        // 'long' is 64-bit by default, but 32-bit on the Win32/Win64
        // variants; compared against a plain fixed 64-bit map, it is
        // consistently smaller-or-equal everywhere.
        let long_like = VariantSizeMap::new(
            64, [(OSVariant::Win64, 32), (OSVariant::Win32, 32)]);
        let fixed_64 = VariantSizeMap::simple_size(64);
        assert_eq!(long_like.compare(&fixed_64), Some(Ordering::Less));

        // Genuinely incomparable: bigger on Win64, smaller on Win32.
        let mixed = VariantSizeMap::new(
            32, [(OSVariant::Win64, 64), (OSVariant::Win32, 16)]);
        assert_eq!(long_like.compare(&mixed), None);
    }

    #[test]
    fn variant_size_map_compares_asymmetric_platform_keys() {
        let default_32 = VariantSizeMap::simple_size(32);
        let win64_64 = VariantSizeMap::new(32, [(OSVariant::Win64, 64)]);
        let lin64_64 = VariantSizeMap::new(32, [(OSVariant::Lin64, 64)]);

        assert_eq!(win64_64.compare(&default_32), Some(Ordering::Greater));
        assert_eq!(default_32.compare(&win64_64), Some(Ordering::Less));
        assert_eq!(win64_64.compare(&lin64_64), None,
                   "each map differs from the other's default on a different platform");
    }

    #[test]
    fn string_to_endianness_rejects_non_byte_orders() {
        for value in [r#"\"big\""#, r#"\"little\""#, r#"\"middle-endian\""#, "", "unknown"] {
            assert_eq!(string_to_endianness(value), None, "{value:?} must be rejected");
        }
    }

    // ---- Resolved-type equivalence and peeling matrix ----

    fn eq_test_span() -> ZeroSpan {
        ZeroSpan::invalid("test.dml")
    }
    fn eq_test_base() -> TypeBase {
        TypeBase { is_const: false, decl_span: eq_test_span() }
    }
    fn eq_const_base() -> TypeBase {
        TypeBase { is_const: true, decl_span: eq_test_span() }
    }
    fn eq_dml_string(s: &str) -> DMLString {
        DMLString { val: s.to_string(), span: eq_test_span() }
    }

    #[test]
    fn type_base_equivalent_const_mismatch() {
        assert!(eq_test_base().equivalent(&eq_test_base()));
        assert!(!eq_test_base().equivalent(&eq_const_base()));
    }

    #[test]
    fn equivalent_void_device_bool() {
        let void1: VoidType = eq_test_base().into();
        let void2: VoidType = eq_test_base().into();
        let void_const: VoidType = eq_const_base().into();
        assert!(void1.equivalent(&void2));
        assert!(!void1.equivalent(&void_const));

        let dev1: DeviceType = eq_test_base().into();
        let dev2: DeviceType = eq_test_base().into();
        let dev_const: DeviceType = eq_const_base().into();
        assert!(dev1.equivalent(&dev2));
        assert!(!dev1.equivalent(&dev_const));

        let bool1: BoolType = eq_test_base().into();
        let bool2: BoolType = eq_test_base().into();
        let bool_const: BoolType = eq_const_base().into();
        assert!(bool1.equivalent(&bool2));
        assert!(!bool1.equivalent(&bool_const));
    }

    #[test]
    fn equivalent_int_variants() {
        let int_signed = IntType::int(eq_test_base(), true);
        let int_signed2 = IntType::int(eq_test_base(), true);
        let int_unsigned = IntType::int(eq_test_base(), false);
        let int64_signed = IntType::int64(eq_test_base(), true);
        assert!(int_signed.equivalent(&int_signed2));
        assert!(!int_signed.equivalent(&int_unsigned),
                "signedness should be part of equivalence");
        assert!(!int_signed.equivalent(&int64_signed),
                "size should be part of equivalence");
    }

    #[test]
    fn equivalent_endian_int() {
        let be32 = EndianIntType {
            base: eq_test_base(), endian: Endianness::BE, size: 32, signed: true };
        let be32_2 = EndianIntType {
            base: eq_test_base(), endian: Endianness::BE, size: 32, signed: true };
        let le32 = EndianIntType {
            base: eq_test_base(), endian: Endianness::LE, size: 32, signed: true };
        let be16 = EndianIntType {
            base: eq_test_base(), endian: Endianness::BE, size: 16, signed: true };
        let be32_unsigned = EndianIntType {
            base: eq_test_base(), endian: Endianness::BE, size: 32, signed: false };
        assert!(be32.equivalent(&be32_2));
        assert!(!be32.equivalent(&le32), "endianness should be part of equivalence");
        assert!(!be32.equivalent(&be16), "size should be part of equivalence");
        assert!(!be32.equivalent(&be32_unsigned), "signedness should be part of equivalence");
    }

    #[test]
    fn equivalent_float_double() {
        let f1: FloatType = eq_test_base().into();
        let f2: FloatType = eq_test_base().into();
        let f_const: FloatType = eq_const_base().into();
        assert!(f1.equivalent(&f2));
        assert!(!f1.equivalent(&f_const));

        let d1: DoubleType = eq_test_base().into();
        let d2: DoubleType = eq_test_base().into();
        let d_const: DoubleType = eq_const_base().into();
        assert!(d1.equivalent(&d2));
        assert!(!d1.equivalent(&d_const));
    }

    #[test]
    fn equivalent_bitfields() {
        let member_int = IntType::int(eq_test_base(), true);
        let slice_a = BitSlice { lsb: None, msb: None, typing: member_int.clone() };
        let slice_b = BitSlice { lsb: None, msb: None, typing: member_int.clone() };
        let bf1 = BitfieldsType {
            base: eq_test_base(), sizing: 16,
            members: vec![(eq_dml_string("hi"), slice_a.clone())],
        };
        let bf2 = BitfieldsType {
            base: eq_test_base(), sizing: 16,
            members: vec![(eq_dml_string("hi"), slice_b.clone())],
        };
        assert!(bf1.equivalent(&bf2));

        let bf_wrong_sizing = BitfieldsType {
            base: eq_test_base(), sizing: 32,
            members: vec![(eq_dml_string("hi"), slice_a.clone())],
        };
        assert!(!bf1.equivalent(&bf_wrong_sizing), "sizing should be part of equivalence");

        let bf_wrong_name = BitfieldsType {
            base: eq_test_base(), sizing: 16,
            members: vec![(eq_dml_string("lo"), slice_a.clone())],
        };
        assert!(!bf1.equivalent(&bf_wrong_name), "member name should be part of equivalence");

        let bf_wrong_range = BitfieldsType {
            base: eq_test_base(), sizing: 16,
            members: vec![(eq_dml_string("hi"), BitSlice {
                lsb: None,
                msb: ExpressionKind::Undefined(eq_test_span()).into(),
                typing: member_int.clone(),
            })],
        };
        assert!(!bf1.equivalent(&bf_wrong_range), "member range should be part of equivalence");

        let bf_wrong_member_type = BitfieldsType {
            base: eq_test_base(), sizing: 16,
            members: vec![(eq_dml_string("hi"), BitSlice {
                lsb: None, msb: None,
                typing: IntType::int(eq_test_base(), false),
            })],
        };
        assert!(!bf1.equivalent(&bf_wrong_member_type),
                "member underlying type should be part of equivalence");
    }

}

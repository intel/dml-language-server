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
    fn make_const(self) -> VoidType {
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
    fn make_const(self) -> DeviceType {
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
    fn make_const(self) -> BoolType {
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
    fn make_const(self) -> EndianIntType {
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
    fn make_const(self) -> IntType {
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
    fn make_const(self) -> FloatType {
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
    fn make_const(self) -> DoubleType {
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
    fn make_const(self) -> BitfieldsType {
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
                    TokenKind::Vect =>
                        modified_outer = UnresolvedType::Vector(
                            UnresolvedVectorType {
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
            Regex::new(r"(u?)int([1-5][0-9]?|6[0-4]?|[789])(_be_t|_le_t)?$")
            .unwrap();
    }
    if let Some(captures) = INT_RE.captures(name) {
        let signed = captures.get(1).is_none();
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
            |en|if en.as_str() == "be_t" { Endianness::BE }
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
            TokenKind::Vect =>
                base = UnresolvedType::Vector(
                    UnresolvedVectorType {
                        base: TypeBase::from_span(ZeroSpan::combine(
                            *base.span(),
                            ZeroSpan::from_range(modifier.range(),
                                                 file.path))),
                        unresolved: Box::new(base),
                    }),
            _ => internal_error!("Unexpected token in cdecl modifier list: \
                                  {:?}",
                                 modifier.read_leaf(file.file).unwrap()),
        }
    }

    let (name, typing) = content.decl.content.as_ref().map(|typedecl|{
        // I _think_ a typedecl can never be an empty object, (since an
        // empty typedecl will just become None)
        deconstruct_typedecl(typedecl.as_actual().unwrap(),
                             base, report, file)
    }).unzip();
    (name.flatten(),
     typing.flatten().unwrap_or_else(||UnresolvedType::make_invalid(
         ZeroSpan::from_range(content.range(), file.path))))
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
                TokenKind::Vect => b = UnresolvedType::Vector(
                    UnresolvedVectorType {
                        base: TypeBase::from_span(ZeroSpan::combine(
                            *b.span(),
                            ZeroSpan::from_range(modifier.range(),
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
}

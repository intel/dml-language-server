//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::collections::HashMap;
use std::convert::TryFrom;

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::expressions::{DMLString, ExpressionKind};
use crate::analysis::structure::objects::Typedef;
use crate::analysis::structure::types::{DMLType, DMLTypeKind, DMLTypeMember};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeUse {
    pub type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub id: TypeId,
    pub name: DMLString,
    pub declaration_span: ZeroSpan,
    pub underlying: Option<DMLType>,
    pub is_extern: bool,
}

#[derive(Debug, Clone, Default)]
pub struct TypeDatabase {
    declarations: Vec<TypeDeclaration>,
    typedefs_by_name: HashMap<String, Vec<TypeId>>,
}

impl TypeDatabase {
    pub fn add_typedef(&mut self, typedef: &Typedef) -> TypeId {
        let id = TypeId(self.declarations.len());
        self.declarations.push(TypeDeclaration {
            id,
            name: typedef.object.name.clone(),
            declaration_span: typedef.object.name.span,
            underlying: Some(typedef.typed.clone()),
            is_extern: typedef.is_extern,
        });
        self.typedefs_by_name.entry(typedef.object.name.val.clone())
            .or_default().push(id);
        id
    }

    pub fn add_template(&mut self, name: DMLString) -> TypeId {
        let id = TypeId(self.declarations.len());
        self.declarations.push(TypeDeclaration {
            id,
            declaration_span: name.span,
            name: name.clone(),
            underlying: None,
            is_extern: false,
        });
        self.typedefs_by_name.entry(name.val).or_default().push(id);
        id
    }

    pub fn typedefs_named(&self, name: &str) -> &[TypeId] {
        self.typedefs_by_name.get(name).map(Vec::as_slice).unwrap_or(&[])
    }

    pub fn declaration(&self, id: TypeId) -> Option<&TypeDeclaration> {
        self.declarations.get(id.0)
    }

    pub fn declarations(&self) -> impl Iterator<Item = &TypeDeclaration> {
        self.declarations.iter()
    }

    pub fn resolve_type_use(&self, typed: &DMLType) -> Option<TypeUse> {
        let written = match &typed.kind {
            DMLTypeKind::Named(name) => Some(name),
            DMLTypeKind::Const(inner) |
            DMLTypeKind::Pointer(inner) |
            DMLTypeKind::Vector(inner) =>
                return self.resolve_type_use(inner),
            DMLTypeKind::Array { element, .. } =>
                return self.resolve_type_use(element),
            DMLTypeKind::Function { returns, .. } =>
                return self.resolve_type_use(returns),
            _ => None,
        }?;
        let ids = self.typedefs_named(&written.val);
        if let [type_id] = ids {
            Some(TypeUse { type_id: *type_id })
        } else {
            None
        }
    }

    pub fn members_named<'a>(&'a self,
                             type_id: TypeId,
                             name: &str) -> Vec<&'a DMLTypeMember> {
        fn find<'a>(database: &'a TypeDatabase,
                    typed: &'a DMLType,
                    name: &str,
                    visited: &mut Vec<TypeId>,
                    found: &mut Vec<&'a DMLTypeMember>) {
            match &typed.kind {
                DMLTypeKind::Struct(members) |
                DMLTypeKind::Layout { fields: members, .. } => found.extend(
                    members.iter().filter(|member|member.name.as_ref()
                        .is_some_and(|member_name|member_name.val == name))),
                DMLTypeKind::Bitfields { fields, .. } => found.extend(
                    fields.iter().map(|field|&field.member)
                        .filter(|member|member.name.as_ref()
                            .is_some_and(|member_name|member_name.val == name))),
                DMLTypeKind::Named(alias) => {
                    for next in database.typedefs_named(&alias.val) {
                        if !visited.contains(next) {
                            visited.push(*next);
                            if let Some(underlying) = database.declaration(*next)
                                .and_then(|declaration|declaration.underlying.as_ref()) {
                                find(database, underlying, name, visited, found);
                            }
                        }
                    }
                },
                DMLTypeKind::Const(inner) |
                DMLTypeKind::Pointer(inner) |
                DMLTypeKind::Vector(inner) =>
                    find(database, inner, name, visited, found),
                DMLTypeKind::Array { element, .. } =>
                    find(database, element, name, visited, found),
                _ => (),
            }
        }

        let mut found = vec![];
        if let Some(underlying) = self.declaration(type_id)
            .and_then(|declaration|declaration.underlying.as_ref()) {
            find(self, underlying, name,
                 &mut vec![type_id], &mut found);
        }
        found
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLStructType {
    span: ZeroSpan,
}

impl DMLStructType {
    pub fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLResolvedMember {
    pub name: Option<String>,
    pub typed: DMLConcreteType,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLConcreteTypeKind {
    Named(String),
    Template(TypeId),
    Typedef {
        id: TypeId,
        underlying: Option<Box<DMLConcreteType>>,
    },
    Struct {
        identity: ZeroSpan,
        fields: Vec<DMLResolvedMember>,
    },
    Layout {
        identity: ZeroSpan,
        byte_order: Option<String>,
        fields: Vec<DMLResolvedMember>,
    },
    Bitfields {
        identity: ZeroSpan,
        width: Option<String>,
        fields: Vec<DMLResolvedMember>,
    },
    TypeOf,
    Sequence {
        template: Option<TypeId>,
        name: String,
    },
    Hook(Vec<DMLConcreteType>),
    Const(Box<DMLConcreteType>),
    Pointer(Box<DMLConcreteType>),
    Vector(Box<DMLConcreteType>),
    Array {
        element: Box<DMLConcreteType>,
        size: DMLArraySize,
    },
    Function {
        returns: Box<DMLConcreteType>,
        parameters: Vec<DMLConcreteType>,
        variadic: bool,
    },
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLArraySize {
    Unspecified,
    Constant(u64),
    Expression(Box<ExpressionKind>),
}

impl DMLArraySize {
    fn constant(&self) -> Option<u64> {
        match self {
            Self::Constant(value) => Some(*value),
            Self::Unspecified | Self::Expression(_) => None,
        }
    }

    fn description(&self) -> String {
        match self {
            Self::Unspecified => String::new(),
            Self::Constant(value) => value.to_string(),
            Self::Expression(_) => "...".to_string(),
        }
    }

    fn equivalent(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLConcreteType {
    pub kind: DMLConcreteTypeKind,
    pub span: ZeroSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerTruncation {
    AllTargets,
    Windows,
    NonWindows,
}

impl IntegerTruncation {
    pub fn target_suffix(self) -> &'static str {
        match self {
            IntegerTruncation::AllTargets => "",
            IntegerTruncation::Windows => " on Windows targets",
            IntegerTruncation::NonWindows => " on non-Windows targets",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageCompatibility {
    Compatible,
    Truncates(IntegerTruncation),
    Incompatible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageValue {
    Integer(i128),
}

pub fn check_storage_compatibility(
    target: &DMLConcreteType,
    source: &DMLConcreteType,
    value: Option<StorageValue>,
) -> StorageCompatibility {
    let compatibility = target.storage_compatibility(source);
    if compatibility == StorageCompatibility::Incompatible {
        return compatibility;
    }
    match value {
        Some(StorageValue::Integer(value)) => target
            .integer_value_truncation(value)
            .map(StorageCompatibility::Truncates)
            .unwrap_or(StorageCompatibility::Compatible),
        None => compatibility,
    }
}

impl StorageCompatibility {
    pub fn is_compatible(self) -> bool {
        !matches!(self, StorageCompatibility::Incompatible)
    }
}

impl DMLConcreteType {
    pub fn span(&self) -> &ZeroSpan {
        &self.span
    }

    pub fn description(&self, types: &TypeDatabase) -> String {
        use DMLConcreteTypeKind::*;

        match &self.kind {
            Named(name) => name.clone(),
            Template(id) | Typedef { id, .. } => types.declaration(*id)
                .map(|declaration|declaration.name.val.clone())
                .unwrap_or_else(||"unknown".to_string()),
            Struct { .. } => "struct".to_string(),
            Layout { .. } => "layout".to_string(),
            Bitfields { .. } => "bitfields".to_string(),
            TypeOf => "typeof(...)".to_string(),
            Sequence { name, .. } => format!("sequence({})", name),
            Hook(_) => "hook".to_string(),
            Const(inner) => format!("const {}", inner.description(types)),
            Pointer(inner) => format!("{} *", inner.description(types)),
            Vector(inner) => format!("vector<{}>", inner.description(types)),
            Array { element, size } => format!(
                "{}[{}]", element.description(types),
                size.description()),
            Function { .. } => "function".to_string(),
            Unknown => "unknown".to_string(),
        }
    }

    pub fn array_element(&self) -> Option<&DMLConcreteType> {
        match &self.kind {
            DMLConcreteTypeKind::Array { element, .. } => Some(element),
            DMLConcreteTypeKind::Typedef {
                underlying: Some(underlying), ..
            } | DMLConcreteTypeKind::Const(underlying) =>
                underlying.array_element(),
            _ => None,
        }
    }

    pub fn array_length(&self) -> Option<u64> {
        match &self.kind {
            DMLConcreteTypeKind::Array { size, .. } => size.constant(),
            DMLConcreteTypeKind::Typedef {
                underlying: Some(underlying), ..
            } | DMLConcreteTypeKind::Const(underlying) =>
                underlying.array_length(),
            _ => None,
        }
    }

    pub fn can_store(&self, source: &DMLConcreteType) -> bool {
        self.storage_compatibility(source).is_compatible()
    }

    pub fn storage_compatibility(
        &self,
        source: &DMLConcreteType,
    ) -> StorageCompatibility {
        use DMLConcreteTypeKind::*;

        match (&self.kind, &source.kind) {
            (Unknown | TypeOf, _) | (_, Unknown | TypeOf) =>
                StorageCompatibility::Compatible,
            (Typedef { underlying: Some(underlying), .. }, _) =>
                underlying.storage_compatibility(source),
            (_, Typedef { underlying: Some(underlying), .. }) =>
                self.storage_compatibility(underlying),
            (Const(target), _) => target.storage_compatibility(source),
            (_, Const(source)) => self.storage_compatibility(source),
            (Named(target), Named(source)) =>
                named_type_storage_compatibility(target, source),
            (Pointer(target), Pointer(source)) |
            (Pointer(target), Array { element: source, .. }) =>
                if pointer_target_can_store(target, source) {
                    StorageCompatibility::Compatible
                } else {
                    StorageCompatibility::Incompatible
                },
            _ if self.equivalent(source) => StorageCompatibility::Compatible,
            _ => StorageCompatibility::Incompatible,
        }
    }

    pub fn integer_value_truncation(&self, value: i128)
                                    -> Option<IntegerTruncation> {
        use DMLConcreteTypeKind::{Const, Named, Typedef};

        match &self.kind {
            Typedef { underlying: Some(underlying), .. } |
            Const(underlying) => underlying.integer_value_truncation(value),
            Named(name) => integer_type(name)
                .and_then(|target|target.value_truncation(value)),
            _ => None,
        }
    }

    fn equivalent(&self, other: &DMLConcreteType) -> bool {
        use DMLConcreteTypeKind::*;

        match (&self.kind, &other.kind) {
            (Unknown | TypeOf, _) | (_, Unknown | TypeOf) => true,
            (Typedef { underlying: Some(underlying), .. }, _) =>
                underlying.equivalent(other),
            (_, Typedef { underlying: Some(underlying), .. }) =>
                self.equivalent(underlying),
            (Named(left), Named(right)) => left == right,
            (Template(left), Template(right)) => left == right,
            (Typedef { id: left, underlying: None },
             Typedef { id: right, underlying: None }) => left == right,
            (Struct { identity: left_identity, fields: left },
             Struct { identity: right_identity, fields: right }) =>
                left_identity == right_identity
                    && members_equivalent(left, right),
            (Layout {
                identity: left_identity,
                byte_order: left_order,
                fields: left,
            }, Layout {
                identity: right_identity,
                byte_order: right_order,
                fields: right,
            }) => left_identity == right_identity
                && left_order == right_order
                && members_equivalent(left, right),
            (Bitfields {
                identity: left_identity,
                width: left_width,
                fields: left,
            }, Bitfields {
                identity: right_identity,
                width: right_width,
                fields: right,
            }) => left_identity == right_identity
                && left_width == right_width
                && members_equivalent(left, right),
            (Sequence { template: left, name: left_name },
             Sequence { template: right, name: right_name }) =>
                left == right && left_name == right_name,
            (Hook(left), Hook(right)) => types_equivalent(left, right),
            (Const(left), Const(right)) |
            (Pointer(left), Pointer(right)) |
            (Vector(left), Vector(right)) => left.equivalent(right),
                (Array { element: left, size: left_size },
                 Array { element: right, size: right_size }) =>
                     left_size.equivalent(right_size) && left.equivalent(right),
            (Function {
                returns: left_returns,
                parameters: left_parameters,
                variadic: left_variadic,
            }, Function {
                returns: right_returns,
                parameters: right_parameters,
                variadic: right_variadic,
            }) => left_variadic == right_variadic
                && left_returns.equivalent(right_returns)
                && types_equivalent(left_parameters, right_parameters),
            _ => false,
        }
    }
}

fn named_type_storage_compatibility(target: &str, source: &str)
                                    -> StorageCompatibility {
    if target == "bool" {
        return if source == "bool" || source == "uint1" {
            StorageCompatibility::Compatible
        } else {
            StorageCompatibility::Incompatible
        };
    }
    if source == "bool" {
        return if matches!(target, "int1" | "uint1") {
            StorageCompatibility::Compatible
        } else {
            StorageCompatibility::Incompatible
        };
    }
    if let Some(target) = integer_type(target) {
        if let Some(source) = integer_type(source) {
            return target.storage_compatibility(source);
        }
        return if is_float_type(source) {
            StorageCompatibility::Truncates(IntegerTruncation::AllTargets)
        } else {
            StorageCompatibility::Incompatible
        };
    }
    if is_float_type(target) {
        return if is_integer_type(source) || is_float_type(source) {
            StorageCompatibility::Compatible
        } else {
            StorageCompatibility::Incompatible
        };
    }
    if target == source {
        StorageCompatibility::Compatible
    } else {
        StorageCompatibility::Incompatible
    }
}

fn is_integer_type(name: &str) -> bool {
    integer_type(name).is_some()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct IntegerWidths {
    windows: u16,
    non_windows: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct IntegerType {
    widths: IntegerWidths,
    signed: bool,
}

impl IntegerType {
    fn fixed(width: u16, signed: bool) -> Self {
        Self {
            widths: IntegerWidths {
                windows: width,
                non_windows: width,
            },
            signed,
        }
    }

    fn storage_compatibility(self, source: Self) -> StorageCompatibility {
        // Match DMLC's IntegerType.canstore: signedness does not affect
        // assignment compatibility, while a wider source may be truncated.
        truncation(
            source.widths.windows > self.widths.windows,
            source.widths.non_windows > self.widths.non_windows,
        ).map(StorageCompatibility::Truncates)
            .unwrap_or(StorageCompatibility::Compatible)
    }

    fn value_truncation(self, value: i128) -> Option<IntegerTruncation> {
        truncation(
            !integer_value_fits(value, self.widths.windows, self.signed),
            !integer_value_fits(value, self.widths.non_windows, self.signed),
        )
    }
}

fn truncation(windows: bool, non_windows: bool) -> Option<IntegerTruncation> {
    match (windows, non_windows) {
        (true, true) => Some(IntegerTruncation::AllTargets),
        (true, false) => Some(IntegerTruncation::Windows),
        (false, true) => Some(IntegerTruncation::NonWindows),
        (false, false) => None,
    }
}

fn integer_value_fits(value: i128, width: u16, signed: bool) -> bool {
    if signed {
        let limit = 1_i128 << (width - 1);
        (-limit..limit).contains(&value)
    } else {
        value >= 0 && value < (1_i128 << width)
    }
}

fn integer_type(name: &str) -> Option<IntegerType> {
    match name {
        "char" => return Some(IntegerType::fixed(8, true)),
        "int" => return Some(IntegerType::fixed(32, true)),
        "long" => return Some(IntegerType {
            widths: IntegerWidths { windows: 32, non_windows: 64 },
            signed: true,
        }),
        "ulong" => return Some(IntegerType {
            widths: IntegerWidths { windows: 32, non_windows: 64 },
            signed: false,
        }),
        "ssize_t" | "int64_t" => return Some(IntegerType::fixed(64, true)),
        "size_t" | "uint64_t" => return Some(IntegerType::fixed(64, false)),
        _ => (),
    }

    let (signed, width) = if let Some(width) = name.strip_prefix("uint") {
        (false, width)
    } else if let Some(width) = name.strip_prefix("int") {
        (true, width)
    } else {
        return None;
    };
    let width = width.strip_suffix("_be_t")
        .or_else(||width.strip_suffix("_le_t"))
        .unwrap_or(width)
        .parse::<u16>().ok()?;
    (1..=64).contains(&width).then(||IntegerType::fixed(width, signed))
}

fn is_float_type(name: &str) -> bool {
    matches!(name, "float" | "double")
}

fn pointer_target_can_store(target: &DMLConcreteType,
                            source: &DMLConcreteType) -> bool {
    use DMLConcreteTypeKind::{Const, Named, Typedef};

    fn transparent(typed: &DMLConcreteType) -> &DMLConcreteType {
        match &typed.kind {
            Typedef { underlying: Some(underlying), .. } => transparent(underlying),
            _ => typed,
        }
    }

    let target = transparent(target);
    let source = transparent(source);
    let target_const = matches!(target.kind, Const(_));
    let source_const = matches!(source.kind, Const(_));
    if source_const && !target_const {
        return false;
    }
    let target = match &target.kind { Const(inner) => inner.as_ref(), _ => target };
    let source = match &source.kind { Const(inner) => inner.as_ref(), _ => source };
    matches!(&target.kind, Named(name) if name == "void")
        || matches!(&source.kind, Named(name) if name == "void")
        || target.equivalent(source)
}

fn types_equivalent(left: &[DMLConcreteType],
                    right: &[DMLConcreteType]) -> bool {
    left.len() == right.len() && left.iter().zip(right)
        .all(|(left, right)|left.equivalent(right))
}

fn members_equivalent(left: &[DMLResolvedMember],
                      right: &[DMLResolvedMember]) -> bool {
    left.len() == right.len() && left.iter().zip(right).all(|(left, right)|
        left.name == right.name && left.typed.equivalent(&right.typed))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLResolvedType {
    Actual(DMLConcreteType),
    // In some cases, a type fails to resolve but we do not want to drop
    // the thing that depends on it, in this case we provide the dummy variant
    Dummy(ZeroSpan),
}

impl DMLResolvedType {
    pub fn span(&self) -> &ZeroSpan {
        match self {
            DMLResolvedType::Actual(typ) => typ.span(),
            DMLResolvedType::Dummy(span) => span,
        }
    }

    pub fn is_dummy(&self) -> bool {
        matches!(self, DMLResolvedType::Dummy(_))
    }

    pub fn equivalent(&self, other: &DMLResolvedType) -> bool {
        match (self, other) {
            (DMLResolvedType::Actual(left), DMLResolvedType::Actual(right)) =>
                left.equivalent(right),
            _ => true,
        }
    }

    pub fn equivalent_unqualified(&self, other: &DMLResolvedType) -> bool {
        fn unqualified(typ: &DMLConcreteType) -> &DMLConcreteType {
            match &typ.kind {
                DMLConcreteTypeKind::Typedef {
                    underlying: Some(underlying), ..
                } => unqualified(underlying),
                DMLConcreteTypeKind::Const(inner) => inner,
                _ => typ,
            }
        }

        match (self, other) {
            (DMLResolvedType::Actual(left), DMLResolvedType::Actual(right)) =>
                unqualified(left).equivalent(unqualified(right)),
            _ => true,
        }
    }

    pub fn can_store(&self, source: &DMLResolvedType) -> bool {
        self.storage_compatibility(source).is_compatible()
    }

    pub fn storage_compatibility(
        &self,
        source: &DMLResolvedType,
    ) -> StorageCompatibility {
        match (self, source) {
            (DMLResolvedType::Actual(target),
             DMLResolvedType::Actual(source)) =>
                target.storage_compatibility(source),
            _ => StorageCompatibility::Compatible,
        }
    }
}

impl From<DMLConcreteType> for DMLResolvedType {
    fn from(frm: DMLConcreteType) -> DMLResolvedType {
        DMLResolvedType::Actual(frm)
    }
}

fn resolve_members(database: &TypeDatabase,
                   members: &[DMLTypeMember],
                   visited: &mut Vec<TypeId>,
                   anonymous: &mut Vec<DMLStructType>,
                   expression_type: &mut dyn FnMut(
                       &ExpressionKind) -> Option<DMLConcreteType>)
                   -> Vec<DMLResolvedMember> {
    members.iter().map(|member|DMLResolvedMember {
        name: member.name.as_ref().map(|name|name.val.clone()),
        typed: resolve_concrete_type(
            database, &member.typed, visited, anonymous, true,
            expression_type),
    }).collect()
}

fn resolve_concrete_type(database: &TypeDatabase,
                         ast: &DMLType,
                         visited: &mut Vec<TypeId>,
                         anonymous: &mut Vec<DMLStructType>,
                         collect_anonymous: bool,
                         expression_type: &mut dyn FnMut(
                             &ExpressionKind) -> Option<DMLConcreteType>)
                         -> DMLConcreteType {
    use DMLConcreteTypeKind::*;

    let kind = match &ast.kind {
        DMLTypeKind::Named(name) => {
            let ids = database.typedefs_named(&name.val);
            if let [id] = ids {
                if let Some(declaration) = database.declaration(*id) {
                    if let Some(underlying) = &declaration.underlying {
                        if visited.contains(id) {
                            Typedef { id: *id, underlying: None }
                        } else {
                            visited.push(*id);
                            let resolved = resolve_concrete_type(
                                database, underlying, visited, anonymous, false,
                                expression_type);
                            visited.pop();
                            Typedef {
                                id: *id,
                                underlying: Some(Box::new(resolved)),
                            }
                        }
                    } else {
                        Template(*id)
                    }
                } else {
                    Named(name.val.clone())
                }
            } else {
                Named(name.val.clone())
            }
        },
        DMLTypeKind::Struct(members) => {
            if collect_anonymous {
                anonymous.push(DMLStructType { span: ast.span });
            }
            Struct {
                identity: ast.span,
                fields: resolve_members(
                    database, members, visited, anonymous, expression_type),
            }
        },
        DMLTypeKind::Layout { byte_order, fields } => Layout {
            identity: ast.span,
            byte_order: byte_order.clone(),
            fields: resolve_members(
                database, fields, visited, anonymous, expression_type),
        },
        DMLTypeKind::Bitfields { width, fields } => Bitfields {
            identity: ast.span,
            width: width.clone(),
            fields: resolve_members(database, &fields.iter()
                .map(|field|field.member.clone()).collect::<Vec<_>>(),
                visited, anonymous, expression_type),
        },
        DMLTypeKind::TypeOf(expression) => expression_type(expression)
            .map(|typed|typed.kind)
            .unwrap_or(TypeOf),
        DMLTypeKind::Sequence(name) => {
            let template = database.typedefs_named(&name.val).iter()
                .copied().find(|id|database.declaration(*id)
                    .is_some_and(|declaration|declaration.underlying.is_none()));
            Sequence { template, name: name.val.clone() }
        },
        DMLTypeKind::Hook(arguments) => Hook(arguments.iter().map(|argument|
            resolve_concrete_type(
                database, argument, visited, anonymous, true,
                expression_type)).collect()),
        DMLTypeKind::Const(inner) => Const(Box::new(resolve_concrete_type(
            database, inner, visited, anonymous, collect_anonymous,
            expression_type))),
        DMLTypeKind::Pointer(inner) => Pointer(Box::new(resolve_concrete_type(
            database, inner, visited, anonymous, collect_anonymous,
            expression_type))),
        DMLTypeKind::Vector(inner) => Vector(Box::new(resolve_concrete_type(
            database, inner, visited, anonymous, collect_anonymous,
            expression_type))),
        DMLTypeKind::Array { element, size } => Array {
            element: Box::new(resolve_concrete_type(
                database, element, visited, anonymous, collect_anonymous,
                expression_type)),
            size: match size.as_deref() {
                None => DMLArraySize::Unspecified,
                Some(expression @ ExpressionKind::IntegerLiteral(integer)) =>
                    u64::try_from(integer.value())
                        .map(DMLArraySize::Constant)
                        .unwrap_or_else(|_|DMLArraySize::Expression(
                            Box::new(expression.clone()))),
                Some(expression) =>
                    DMLArraySize::Expression(Box::new(expression.clone())),
            },
        },
        DMLTypeKind::Function { returns, parameters, variadic } => Function {
            returns: Box::new(resolve_concrete_type(
                database, returns, visited, anonymous, collect_anonymous,
                expression_type)),
            parameters: parameters.iter().map(|parameter|
                resolve_concrete_type(
                    database, parameter, visited, anonymous, true,
                    expression_type)).collect(),
            variadic: *variadic,
        },
        DMLTypeKind::Unknown => Unknown,
    };
    DMLConcreteType { kind, span: ast.span }
}

pub fn eval_type(ast: &DMLType, database: &TypeDatabase,
                 _in_extern: bool, _typename_hint: Option<&str>,
                 _allow_void: bool)
                 -> (Vec<DMLStructType>, DMLConcreteType) {
    eval_type_with_expression(ast, database, |_|None)
}

pub fn eval_type_with_expression(
    ast: &DMLType,
    database: &TypeDatabase,
    mut expression_type: impl FnMut(
        &ExpressionKind) -> Option<DMLConcreteType>,
) -> (Vec<DMLStructType>, DMLConcreteType) {
    let mut anonymous = vec![];
    let resolved = resolve_concrete_type(
        database, ast, &mut vec![], &mut anonymous, true,
        &mut expression_type);
    (anonymous, resolved)
}

pub fn eval_type_simple(ast: &DMLType, database: &TypeDatabase)
    -> (Vec<DMLStructType>, DMLConcreteType) {
    eval_type(ast, database, false, None, false)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn named_type(name: &str) -> DMLType {
        let span = ZeroSpan::invalid("resolved-type-test.dml");
        DMLType {
            kind: DMLTypeKind::Named(DMLString {
                val: name.to_string(),
                span,
            }),
            span,
        }
    }

    fn wrapped_type(kind: impl FnOnce(Box<DMLType>) -> DMLTypeKind,
                    inner: DMLType) -> DMLType {
        DMLType {
            span: inner.span,
            kind: kind(Box::new(inner)),
        }
    }

    fn add_type(database: &mut TypeDatabase,
                name: &str,
                underlying: Option<DMLType>) -> TypeId {
        let span = ZeroSpan::invalid("resolved-type-test.dml");
        let id = TypeId(database.declarations.len());
        database.declarations.push(TypeDeclaration {
            id,
            name: DMLString { val: name.to_string(), span },
            declaration_span: span,
            underlying,
            is_extern: false,
        });
        database.typedefs_by_name.entry(name.to_string())
            .or_default().push(id);
        id
    }

    fn resolved(ast: &DMLType, database: &TypeDatabase) -> DMLResolvedType {
        eval_type_simple(ast, database).1.into()
    }

    fn array_type(element: DMLType, length: Option<i64>) -> DMLType {
        let span = element.span;
        DMLType {
            span,
            kind: DMLTypeKind::Array {
                element: Box::new(element),
                size: length.map(|value|Box::new(
                    ExpressionKind::IntegerLiteral(
                        crate::analysis::structure::expressions::IntegerLiteral::Signed(
                            crate::analysis::structure::expressions::Value {
                                val: value,
                                span,
                            })))),
            },
        }
    }

    #[test]
    fn distinct_primitive_types_are_not_equivalent() {
        let database = TypeDatabase::default();
        let (_, integer) = eval_type_simple(&named_type("int"), &database);
        let (_, floating) = eval_type_simple(&named_type("float"), &database);

        assert!(!DMLResolvedType::from(integer)
            .equivalent(&DMLResolvedType::from(floating)));
    }

    #[test]
    fn typedefs_are_transparent_but_templates_are_nominal() {
        let mut database = TypeDatabase::default();
        add_type(&mut database, "integer_t", Some(named_type("int")));
        add_type(&mut database, "first_template", None);
        add_type(&mut database, "second_template", None);

        assert!(resolved(&named_type("integer_t"), &database)
            .equivalent(&resolved(&named_type("int"), &database)));
        assert!(!resolved(&named_type("first_template"), &database)
            .equivalent(&resolved(&named_type("second_template"), &database)));
    }

    #[test]
    fn pointer_and_array_types_are_distinct() {
        let database = TypeDatabase::default();
        let pointer = wrapped_type(DMLTypeKind::Pointer, named_type("int"));
        let array = array_type(named_type("int"), None);

        assert!(!resolved(&pointer, &database).equivalent(&resolved(
            &array, &database)));
    }

    #[test]
    fn arrays_retain_literal_lengths() {
        let database = TypeDatabase::default();
        let known = eval_type_simple(
            &array_type(named_type("int"), Some(3)), &database).1;
        let unknown = eval_type_simple(
            &array_type(named_type("int"), None), &database).1;
        let negative = eval_type_simple(
            &array_type(named_type("int"), Some(-1)), &database).1;
        let span = ZeroSpan::invalid("resolved-type-test.dml");
        let dynamic = eval_type_simple(&DMLType {
            span,
            kind: DMLTypeKind::Array {
                element: Box::new(named_type("int")),
                size: Some(Box::new(ExpressionKind::Identifier(
                    crate::analysis::structure::expressions::Identifier {
                        name: DMLString {
                            val: "dimension".to_string(),
                            span,
                        },
                    }))),
            },
        }, &database).1;

        assert!(matches!(known.kind,
                         DMLConcreteTypeKind::Array {
                             size: DMLArraySize::Constant(3), ..
                         }));
        assert!(matches!(unknown.kind,
                         DMLConcreteTypeKind::Array {
                             size: DMLArraySize::Unspecified, ..
                         }));
        assert!(matches!(negative.kind,
                         DMLConcreteTypeKind::Array {
                             size: DMLArraySize::Expression(_), ..
                         }));
        assert!(matches!(dynamic.kind,
                         DMLConcreteTypeKind::Array {
                             size: DMLArraySize::Expression(_), ..
                         }));
    }

    #[test]
    fn array_lengths_participate_in_equivalence() {
        let database = TypeDatabase::default();
        let three = resolved(
            &array_type(named_type("int"), Some(3)), &database);
        let another_three = resolved(
            &array_type(named_type("int"), Some(3)), &database);
        let four = resolved(
            &array_type(named_type("int"), Some(4)), &database);
        let unknown = resolved(
            &array_type(named_type("int"), None), &database);
        let negative = resolved(
            &array_type(named_type("int"), Some(-1)), &database);
        let another_negative = resolved(
            &array_type(named_type("int"), Some(-1)), &database);

        assert!(three.equivalent(&another_three));
        assert!(!three.equivalent(&four));
        assert!(!three.equivalent(&unknown));
        assert!(!negative.equivalent(&unknown));
        assert!(negative.equivalent(&another_negative));
    }

    #[test]
    fn recursive_typedef_resolution_terminates() {
        let mut database = TypeDatabase::default();
        add_type(&mut database, "node_t", Some(wrapped_type(
            DMLTypeKind::Pointer, named_type("node_t"))));

        let node = resolved(&named_type("node_t"), &database);
        assert!(node.equivalent(&node));
    }

    #[test]
    fn distinct_struct_declarations_are_nominal() {
        let database = TypeDatabase::default();
        let first_span = ZeroSpan::invalid("first-struct.dml");
        let second_span = ZeroSpan::invalid("second-struct.dml");
        let first = DMLType {
            kind: DMLTypeKind::Struct(vec![]),
            span: first_span,
        };
        let second = DMLType {
            kind: DMLTypeKind::Struct(vec![]),
            span: second_span,
        };

        assert!(!resolved(&first, &database)
            .equivalent(&resolved(&second, &database)));
    }

    #[test]
    fn arithmetic_types_can_store_each_other() {
        let database = TypeDatabase::default();

        assert!(resolved(&named_type("int"), &database)
            .can_store(&resolved(&named_type("uint32"), &database)));
        assert!(resolved(&named_type("float"), &database)
            .can_store(&resolved(&named_type("int"), &database)));
        assert!(!resolved(&named_type("bool"), &database)
            .can_store(&resolved(&named_type("int"), &database)));
        assert!(resolved(&named_type("bool"), &database)
            .can_store(&resolved(&named_type("uint1"), &database)));
        assert!(resolved(&named_type("uint1"), &database)
            .can_store(&resolved(&named_type("bool"), &database)));
        assert!(!resolved(&named_type("uint8"), &database)
            .can_store(&resolved(&named_type("bool"), &database)));
    }

    #[test]
    fn integer_storage_reports_platform_specific_truncation() {
        let database = TypeDatabase::default();
        let compatibility = |target: &str, source: &str|resolved(
            &named_type(target), &database).storage_compatibility(
                &resolved(&named_type(source), &database));

        assert_eq!(compatibility("uint8", "uint64"),
                   StorageCompatibility::Truncates(
                       IntegerTruncation::AllTargets));
        assert_eq!(compatibility("long", "uint64"),
                   StorageCompatibility::Truncates(
                       IntegerTruncation::Windows));
        assert_eq!(compatibility("int", "long"),
                   StorageCompatibility::Truncates(
                       IntegerTruncation::NonWindows));
        assert_eq!(compatibility("uint64", "long"),
                   StorageCompatibility::Compatible);
    }

    #[test]
    fn integer_storage_matches_dmlc_signedness_rules() {
        let database = TypeDatabase::default();
        let compatibility = |target: &str, source: &str|resolved(
            &named_type(target), &database).storage_compatibility(
                &resolved(&named_type(source), &database));

        assert_eq!(compatibility("int8", "uint8"),
                   StorageCompatibility::Compatible);
        assert_eq!(compatibility("uint8", "int8"),
                   StorageCompatibility::Compatible);
        assert_eq!(compatibility("int64", "uint64"),
                   StorageCompatibility::Compatible);
        assert_eq!(compatibility("uint64", "int64"),
                   StorageCompatibility::Compatible);
        assert_eq!(compatibility("int16", "uint8"),
                   StorageCompatibility::Compatible);
        assert_eq!(compatibility("uint8", "int16"),
                   StorageCompatibility::Truncates(
                       IntegerTruncation::AllTargets));
    }

    #[test]
    fn integer_literal_truncation_uses_value_and_signedness() {
        let database = TypeDatabase::default();
        let actual = |name: &str|match resolved(&named_type(name), &database) {
            DMLResolvedType::Actual(typed) => typed,
            DMLResolvedType::Dummy(_) => unreachable!(),
        };
        let uint8 = actual("uint8");
        let long = actual("long");

        assert_eq!(uint8.integer_value_truncation(255), None);
        assert_eq!(uint8.integer_value_truncation(256),
                   Some(IntegerTruncation::AllTargets));
        assert_eq!(uint8.integer_value_truncation(-1),
                   Some(IntegerTruncation::AllTargets));
        assert_eq!(long.integer_value_truncation(i64::MAX.into()),
                   Some(IntegerTruncation::Windows));
    }

    #[test]
    fn storage_check_uses_integer_values_when_available() {
        let database = TypeDatabase::default();
        let actual = |name: &str|match resolved(&named_type(name), &database) {
            DMLResolvedType::Actual(typed) => typed,
            DMLResolvedType::Dummy(_) => unreachable!(),
        };
        let target = actual("uint8");
        let source = actual("uint64");

        assert_eq!(check_storage_compatibility(
            &target, &source, Some(StorageValue::Integer(255))),
            StorageCompatibility::Compatible);
        assert_eq!(check_storage_compatibility(
            &target, &source, Some(StorageValue::Integer(256))),
            StorageCompatibility::Truncates(IntegerTruncation::AllTargets));
        assert_eq!(check_storage_compatibility(&target, &source, None),
            StorageCompatibility::Truncates(IntegerTruncation::AllTargets));
    }

    #[test]
    fn can_store_is_transparent_through_typedefs() {
        let mut database = TypeDatabase::default();
        add_type(&mut database, "integer_t", Some(named_type("int")));

        assert!(resolved(&named_type("integer_t"), &database)
            .can_store(&resolved(&named_type("uint32"), &database)));
    }

    #[test]
    fn pointers_can_add_but_not_discard_pointee_const() {
        let database = TypeDatabase::default();
        let int_pointer = wrapped_type(
            DMLTypeKind::Pointer, named_type("int"));
        let const_int_pointer = wrapped_type(
            DMLTypeKind::Pointer,
            wrapped_type(DMLTypeKind::Const, named_type("int")));

        assert!(resolved(&const_int_pointer, &database)
            .can_store(&resolved(&int_pointer, &database)));
        assert!(!resolved(&int_pointer, &database)
            .can_store(&resolved(&const_int_pointer, &database)));
    }

    #[test]
    fn pointer_targets_accept_array_decay_and_void_pointers() {
        let database = TypeDatabase::default();
        let int_pointer = wrapped_type(DMLTypeKind::Pointer, named_type("int"));
        let int_array = array_type(named_type("int"), None);
        let void_pointer = wrapped_type(DMLTypeKind::Pointer, named_type("void"));

        assert!(resolved(&int_pointer, &database)
            .can_store(&resolved(&int_array, &database)));
        assert!(resolved(&void_pointer, &database)
            .can_store(&resolved(&int_pointer, &database)));
    }

    #[test]
    fn can_store_rejects_incompatible_wrappers() {
        let database = TypeDatabase::default();
        let pointer = wrapped_type(DMLTypeKind::Pointer, named_type("int"));
        let vector = wrapped_type(DMLTypeKind::Vector, named_type("int"));

        assert!(!resolved(&pointer, &database)
            .can_store(&resolved(&vector, &database)));
    }

    #[test]
    fn unresolved_types_do_not_cascade_storage_errors() {
        let database = TypeDatabase::default();
        let unknown = DMLType {
            kind: DMLTypeKind::Unknown,
            span: ZeroSpan::invalid("resolved-type-test.dml"),
        };

        assert!(resolved(&unknown, &database)
            .can_store(&resolved(&named_type("int"), &database)));
        assert!(resolved(&named_type("int"), &database)
            .can_store(&resolved(&unknown, &database)));
    }
}

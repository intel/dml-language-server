//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::collections::HashMap;
use std::convert::TryFrom;

use lsp_types::DiagnosticSeverity;

use crate::analysis::DMLError;
use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::expressions::{DMLString, ExpressionKind};
use crate::analysis::structure::objects::Typedef;
use crate::analysis::structure::types::{DMLType, DMLTypeKind, DMLTypeMember};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeUse {
    Declared(TypeId),
    // Struct, layout or bitfields type without a name, identified by its span
    Anonymous(ZeroSpan),
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
    pub fn report_unknown_types(&self,
                                typed: &DMLType,
                                report: &mut Vec<DMLError>) {
        match &typed.kind {
            DMLTypeKind::Named(name) =>
                if !is_builtin_type(&name.val)
                    && self.typedefs_named(&name.val).is_empty() {
                    report.push(DMLError {
                        span: name.span,
                        description: format!("Unknown type: '{}'", name.val),
                        related: vec![],
                        severity: Some(DiagnosticSeverity::ERROR),
                    });
                },
            DMLTypeKind::Struct(members) |
            DMLTypeKind::Layout { fields: members, .. } =>
                for member in members {
                    self.report_unknown_types(&member.typed, report);
                },
            DMLTypeKind::Bitfields { fields, .. } =>
                for field in fields {
                    self.report_unknown_types(&field.member.typed, report);
                },
            DMLTypeKind::Hook(arguments) =>
                for argument in arguments {
                    self.report_unknown_types(argument, report);
                },
            DMLTypeKind::Const(inner) |
            DMLTypeKind::Pointer(inner) |
            DMLTypeKind::Vector(inner) =>
                self.report_unknown_types(inner, report),
            DMLTypeKind::Array { element, .. } =>
                self.report_unknown_types(element, report),
            DMLTypeKind::Function { returns, parameters, .. } => {
                self.report_unknown_types(returns, report);
                for parameter in parameters {
                    self.report_unknown_types(parameter, report);
                }
            },
            DMLTypeKind::TypeOf(_) | DMLTypeKind::Sequence(_) |
            DMLTypeKind::Unknown => (),
        }
    }

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
        self.resolve_type_shape(typed).map(|(type_use, _)|type_use)
    }

    // The type use under 'typed', and how many pointer/array layers wrap it
    pub fn resolve_type_shape(&self, typed: &DMLType)
                              -> Option<(TypeUse, u32)> {
        match &typed.kind {
            DMLTypeKind::Named(name) => match self.typedefs_named(&name.val) {
                [type_id] => Some((TypeUse::Declared(*type_id), 0)),
                _ => None,
            },
            DMLTypeKind::Struct(_) |
            DMLTypeKind::Layout { .. } |
            DMLTypeKind::Bitfields { .. } =>
                Some((TypeUse::Anonymous(typed.span), 0)),
            DMLTypeKind::Const(inner) => self.resolve_type_shape(inner),
            DMLTypeKind::Pointer(inner) |
            DMLTypeKind::Vector(inner) |
            DMLTypeKind::Array { element: inner, .. } =>
                self.resolve_type_shape(inner)
                    .map(|(type_use, layers)|(type_use, layers + 1)),
            DMLTypeKind::Function { returns, .. } =>
                self.resolve_type_shape(returns),
            _ => None,
        }
    }

    // Whether the type under 'typed', as found by 'resolve_type_shape', is a
    // 'typeof' that we cannot resolve
    pub fn is_typeof_shape(&self, typed: &DMLType) -> bool {
        match &typed.kind {
            DMLTypeKind::TypeOf(_) => true,
            DMLTypeKind::Const(inner) |
            DMLTypeKind::Pointer(inner) |
            DMLTypeKind::Vector(inner) |
            DMLTypeKind::Array { element: inner, .. } |
            DMLTypeKind::Function { returns: inner, .. } =>
                self.is_typeof_shape(inner),
            _ => false,
        }
    }

    // Follows typedefs from 'type_use' to a struct-like type, counting the
    // pointer/array layers the typedefs add on the way
    pub fn struct_core(&self, type_use: TypeUse) -> Option<(TypeUse, u32)> {
        let mut current = type_use;
        let mut layers = 0;
        let mut visited = vec![];
        loop {
            let TypeUse::Declared(type_id) = current else {
                return Some((current, layers));
            };
            if visited.contains(&type_id) {
                return None;
            }
            visited.push(type_id);
            let underlying = self.declaration(type_id)?.underlying.as_ref()?;
            if direct_members(underlying).is_some() {
                return Some((current, layers));
            }
            let (next, extra) = self.resolve_type_shape(underlying)?;
            current = next;
            layers += extra;
        }
    }
}

pub fn direct_members(typed: &DMLType) -> Option<Vec<&DMLTypeMember>> {
    match &typed.kind {
        DMLTypeKind::Struct(members) |
        DMLTypeKind::Layout { fields: members, .. } =>
            Some(members.iter().collect()),
        DMLTypeKind::Bitfields { fields, .. } =>
            Some(fields.iter().map(|field|&field.member).collect()),
        _ => None,
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TypeComparisonMode {
    Strict,
    #[default]
    Fuzzy,
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
        self.storage_compatibility_with_mode(source, TypeComparisonMode::default())
    }

    pub fn storage_compatibility_with_mode(
        &self,
        source: &DMLConcreteType,
        mode: TypeComparisonMode,
    ) -> StorageCompatibility {
        use DMLConcreteTypeKind::*;

        if matches!(self.kind, Unknown | TypeOf)
            || matches!(source.kind, Unknown | TypeOf) {
            return StorageCompatibility::Compatible;
        }
        if let (Some(target), Some(source)) =
            (integer_storage_type(self), integer_storage_type(source)) {
            return target.storage_compatibility(source);
        }
        if matches!(self.kind, Bitfields { .. }) {
            return StorageCompatibility::Incompatible;
        }

        match (&self.kind, &source.kind) {
            (Typedef { underlying: Some(underlying), .. }, _) =>
                underlying.storage_compatibility_with_mode(source, mode),
            (_, Typedef { underlying: Some(underlying), .. }) =>
                self.storage_compatibility_with_mode(underlying, mode),
            (Const(target), _) =>
                target.storage_compatibility_with_mode(source, mode),
            (_, Const(source)) =>
                self.storage_compatibility_with_mode(source, mode),
            (Named(target), Named(source)) =>
                named_type_storage_compatibility(target, source),
            (Pointer(target), Pointer(source)) |
            (Pointer(target), Array { element: source, .. }) =>
                if pointer_target_can_store(target, source, mode) {
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
        use DMLConcreteTypeKind::{Bitfields, Const, Named, Typedef};

        match &self.kind {
            Typedef { underlying: Some(underlying), .. } |
            Const(underlying) => underlying.integer_value_truncation(value),
            Named(name) => integer_type(name)
                .and_then(|target|target.value_truncation(value)),
            Bitfields { width, .. } => width.as_deref()
                .and_then(bitfield_integer_type)
                .and_then(|target|target.value_truncation(value)),
            _ => None,
        }
    }

    fn equivalent(&self, other: &DMLConcreteType) -> bool {
        use DMLConcreteTypeKind::*;

        match (&self.kind, &other.kind) {
            (Unknown | TypeOf, _) | (_, Unknown | TypeOf) => true,
            (Typedef { id: left, .. }, Typedef { id: right, .. })
                if left == right => true,
            (Typedef { underlying: Some(underlying), .. }, _) =>
                underlying.equivalent(other),
            (_, Typedef { underlying: Some(underlying), .. }) =>
                self.equivalent(underlying),
            (Named(left), Named(right)) => left == right,
            (Template(left), Template(right)) => left == right,
            (Typedef { id: left, underlying: None },
             Typedef { id: right, underlying: None }) => left == right,
            (Struct { identity: left_identity, .. },
             Struct { identity: right_identity, .. }) =>
                left_identity == right_identity,
            (Layout {
                identity: left_identity,
                ..
            }, Layout {
                identity: right_identity,
                ..
            }) => left_identity == right_identity,
            (Bitfields {
                width: left_width,
                fields: left,
                ..
            }, Bitfields {
                width: right_width,
                fields: right,
                ..
            }) => left_width == right_width
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

    fn equivalent_fuzzy(&self, other: &DMLConcreteType) -> bool {
        fn unqualified(typed: &DMLConcreteType) -> &DMLConcreteType {
            match &typed.kind {
                DMLConcreteTypeKind::Typedef {
                    underlying: Some(underlying), ..
                } | DMLConcreteTypeKind::Const(underlying) =>
                    unqualified(underlying),
                _ => typed,
            }
        }

        let left = unqualified(self);
        let right = unqualified(other);
        use DMLConcreteTypeKind::*;
        match (&left.kind, &right.kind) {
            (Unknown | TypeOf, _) | (_, Unknown | TypeOf) => true,
            (Pointer(left), Pointer(right)) |
            (Pointer(left), Array { element: right, .. }) |
            (Array { element: left, .. }, Pointer(right)) |
            (Array { element: left, .. }, Array { element: right, .. }) =>
                matches!(&unqualified(left).kind, Named(name) if name == "void")
                    || matches!(&unqualified(right).kind,
                                Named(name) if name == "void")
                    || left.equivalent_fuzzy(right),
            (Vector(left), Vector(right)) => left.equivalent_fuzzy(right),
            (Hook(left), Hook(right)) => left.len() == right.len()
                && left.iter().zip(right).all(|(left, right)|
                    left.equivalent_fuzzy(right)),
            (Function {
                returns: left_returns,
                parameters: left_parameters,
                variadic: left_variadic,
            }, Function {
                returns: right_returns,
                parameters: right_parameters,
                variadic: right_variadic,
            }) => left_variadic == right_variadic
                && left_returns.equivalent_fuzzy(right_returns)
                && left_parameters.len() == right_parameters.len()
                && left_parameters.iter().zip(right_parameters)
                    .all(|(left, right)|left.equivalent_fuzzy(right)),
            _ => left.equivalent(right),
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

fn bitfield_integer_type(width: &str) -> Option<IntegerType> {
    width.parse::<u16>().ok()
        .filter(|width|*width > 0)
        .map(|width|IntegerType::fixed(width, false))
}

fn integer_storage_type(typed: &DMLConcreteType) -> Option<IntegerType> {
    match &typed.kind {
        DMLConcreteTypeKind::Named(name) => integer_type(name),
        DMLConcreteTypeKind::Bitfields { width, .. } =>
            width.as_deref().and_then(bitfield_integer_type),
        DMLConcreteTypeKind::Typedef {
            underlying: Some(underlying), ..
        } | DMLConcreteTypeKind::Const(underlying) =>
            integer_storage_type(underlying),
        _ => None,
    }
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

// Mirrors DMLC's parse_type and initial typedefs
fn is_builtin_type(name: &str) -> bool {
    integer_type(name).is_some() || is_float_type(name)
        || matches!(name, "bool" | "void" | "integer_t" | "uinteger_t")
}

fn pointer_target_can_store(target: &DMLConcreteType,
                            source: &DMLConcreteType,
                            mode: TypeComparisonMode) -> bool {
    use DMLConcreteTypeKind::{Const, Named, Typedef};

    if target.equivalent(source) {
        return true;
    }

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
    let target_unqualified = match &target.kind {
        Const(inner) => inner.as_ref(),
        _ => target,
    };
    let source_unqualified = match &source.kind {
        Const(inner) => inner.as_ref(),
        _ => source,
    };
    if matches!(&target_unqualified.kind, Named(name) if name == "void")
        || matches!(&source_unqualified.kind, Named(name) if name == "void") {
        return mode == TypeComparisonMode::Fuzzy
            || !source_const
            || target_const;
    }
    if source_const && !target_const {
        return false;
    }
    target_unqualified.equivalent(source_unqualified)
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
        self.equivalent_unqualified_with_mode(other, TypeComparisonMode::default())
    }

    pub fn equivalent_unqualified_with_mode(
        &self,
        other: &DMLResolvedType,
        mode: TypeComparisonMode,
    ) -> bool {
        fn unqualified(typ: &DMLConcreteType) -> &DMLConcreteType {
            match &typ.kind {
                DMLConcreteTypeKind::Typedef {
                    underlying: Some(underlying), ..
                } => unqualified(underlying),
                DMLConcreteTypeKind::Const(inner) => unqualified(inner),
                _ => typ,
            }
        }

        match (self, other) {
            (DMLResolvedType::Actual(left), DMLResolvedType::Actual(right)) => {
                let left = unqualified(left);
                let right = unqualified(right);
                match mode {
                    TypeComparisonMode::Strict => left.equivalent(right),
                    TypeComparisonMode::Fuzzy => left.equivalent_fuzzy(right),
                }
            },
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
        self.storage_compatibility_with_mode(source, TypeComparisonMode::default())
    }

    pub fn storage_compatibility_with_mode(
        &self,
        source: &DMLResolvedType,
        mode: TypeComparisonMode,
    ) -> StorageCompatibility {
        match (self, source) {
            (DMLResolvedType::Actual(target),
             DMLResolvedType::Actual(source)) =>
                target.storage_compatibility_with_mode(source, mode),
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
                   collect_anonymous: bool,
                   expression_type: &mut dyn FnMut(
                       &ExpressionKind) -> Option<DMLConcreteType>)
                   -> Vec<DMLResolvedMember> {
    members.iter().map(|member|DMLResolvedMember {
        name: member.name.as_ref().map(|name|name.val.clone()),
        typed: resolve_concrete_type(
            database, &member.typed, visited, anonymous, collect_anonymous,
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
            } else if is_builtin_type(&name.val) {
                Named(name.val.clone())
            } else {
                Unknown
            }
        },
        DMLTypeKind::Struct(members) => {
            if collect_anonymous {
                anonymous.push(DMLStructType { span: ast.span });
            }
            Struct {
                identity: ast.span,
                fields: resolve_members(
                    database, members, visited, anonymous,
                    collect_anonymous, expression_type),
            }
        },
        DMLTypeKind::Layout { byte_order, fields } => Layout {
            identity: ast.span,
            byte_order: byte_order.clone(),
            fields: resolve_members(
                database, fields, visited, anonymous,
                collect_anonymous, expression_type),
        },
        DMLTypeKind::Bitfields { width, fields } => Bitfields {
            identity: ast.span,
            width: width.clone(),
            fields: resolve_members(database, &fields.iter()
                .map(|field|field.member.clone()).collect::<Vec<_>>(),
                visited, anonymous, collect_anonymous, expression_type),
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
                database, argument, visited, anonymous, collect_anonymous,
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
                    database, parameter, visited, anonymous,
                    collect_anonymous,
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
    fn unknown_type_names_are_not_storage_checked() {
        let database = TypeDatabase::default();
        let (_, builtin) = eval_type_simple(&named_type("uint2"), &database);
        let (_, misspelled) = eval_type_simple(&named_type("uit2"), &database);
        assert!(matches!(misspelled.kind, DMLConcreteTypeKind::Unknown));
        assert!(misspelled.can_store(&builtin));
        assert!(builtin.can_store(&misspelled));

        let mut report = vec![];
        database.report_unknown_types(&named_type("uint2"), &mut report);
        database.report_unknown_types(
            &wrapped_type(DMLTypeKind::Pointer, named_type("uit2")),
            &mut report);
        assert_eq!(report.len(), 1);
        assert_eq!(report[0].description, "Unknown type: 'uit2'");
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
        let id = add_type(&mut database, "node_t", Some(wrapped_type(
            DMLTypeKind::Pointer, named_type("node_t"))));

        let node = resolved(&named_type("node_t"), &database);
        assert!(node.equivalent(&node));
        let recursion_marker = DMLResolvedType::Actual(DMLConcreteType {
            kind: DMLConcreteTypeKind::Typedef {
                id,
                underlying: None,
            },
            span: ZeroSpan::invalid("resolved-type-test.dml"),
        });
        assert!(node.equivalent(&recursion_marker));
        assert!(recursion_marker.equivalent(&node));
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
    fn struct_equivalence_uses_declaration_identity() {
        let span = ZeroSpan::invalid("same-struct.dml");
        let first = DMLConcreteType {
            kind: DMLConcreteTypeKind::Struct {
                identity: span,
                fields: vec![],
            },
            span,
        };
        let second = DMLConcreteType {
            kind: DMLConcreteTypeKind::Struct {
                identity: span,
                fields: vec![DMLResolvedMember {
                    name: Some("field".to_string()),
                    typed: DMLConcreteType {
                        kind: DMLConcreteTypeKind::Named("int".to_string()),
                        span,
                    },
                }],
            },
            span,
        };

        assert!(first.equivalent(&second));
        assert!(second.equivalent(&first));
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
    fn bitfields_store_integers_like_their_unsigned_backing_type() {
        let database = TypeDatabase::default();
        let span = ZeroSpan::invalid("bitfield-storage-test.dml");
        let bitfields = DMLResolvedType::Actual(DMLConcreteType {
            kind: DMLConcreteTypeKind::Bitfields {
                identity: span,
                width: Some("8".to_string()),
                fields: vec![],
            },
            span,
        });
        let uint8 = resolved(&named_type("uint8"), &database);
        let uint16 = resolved(&named_type("uint16"), &database);
        let floating = resolved(&named_type("double"), &database);

        assert_eq!(bitfields.storage_compatibility(&uint8),
                   StorageCompatibility::Compatible);
        assert_eq!(bitfields.storage_compatibility(&uint16),
                   StorageCompatibility::Truncates(
                       IntegerTruncation::AllTargets));
        assert_eq!(bitfields.storage_compatibility(&floating),
                   StorageCompatibility::Incompatible);
        assert_eq!(uint8.storage_compatibility(&bitfields),
                   StorageCompatibility::Compatible);
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
    fn comparison_modes_match_dmlc_pointer_semantics() {
        let database = TypeDatabase::default();
        let int_pointer = resolved(&wrapped_type(
            DMLTypeKind::Pointer, named_type("int")), &database);
        let int_array = resolved(&array_type(named_type("int"), Some(4)),
                                 &database);
        let const_int_pointer = resolved(&wrapped_type(
            DMLTypeKind::Pointer,
            wrapped_type(DMLTypeKind::Const, named_type("int"))),
            &database);
        let void_pointer = resolved(&wrapped_type(
            DMLTypeKind::Pointer, named_type("void")), &database);

        assert!(int_pointer.equivalent_unqualified(&int_array));
        assert!(!int_pointer.equivalent_unqualified_with_mode(
            &int_array, TypeComparisonMode::Strict));
        assert!(int_pointer.equivalent_unqualified(&const_int_pointer));
        assert!(!int_pointer.equivalent_unqualified_with_mode(
            &const_int_pointer, TypeComparisonMode::Strict));
        assert_eq!(void_pointer.storage_compatibility(&const_int_pointer),
                   StorageCompatibility::Compatible);
        assert_eq!(void_pointer.storage_compatibility_with_mode(
            &const_int_pointer, TypeComparisonMode::Strict),
            StorageCompatibility::Incompatible);
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

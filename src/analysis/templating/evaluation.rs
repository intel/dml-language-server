//  © 2026 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

// Defines traits and functions for evaluating expressions within an analysis

use lsp_types::DiagnosticSeverity;

use crate::analysis::{DMLError, DeclarationSpan};
use crate::analysis::structure::expressions::{ExpressionKind, FunctionCall,
                                              IndexExpression, MemberLiteral,
                                              MemberOp};
use crate::analysis::structure::types::DMLType;
use crate::analysis::templating::types::{DMLConcreteType,
                                        DMLConcreteTypeKind,
                                        DMLResolvedType};
use crate::analysis::parsing::tree::ZeroSpan;

use std::collections::HashSet;

/// Structure that will contain the information available at the point of
/// evaluation, can be extended with additional optional fields in the future
#[derive(Debug, Clone)]
pub struct EvaluationContext {
    reported_errors: HashSet<DMLError>,
}

#[allow(clippy::new_without_default)]
impl EvaluationContext {
    pub fn new() -> Self {
        EvaluationContext {
            reported_errors: HashSet::default(),
        }
    }

    pub fn report_if_new(&mut self, to_report: DMLError, report: &mut Vec<DMLError>) {
        if self.reported_errors.insert(to_report.clone()) {
            report.push(to_report);
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EvaluatedValue {
    // We'll use this for storing both signed, unsigned, and endian values
    // the disambiguation between them is done by the type in the
    // EvaluationResult struct
    Int(i128),
    Bool(bool),
    DMLObject(()), // TODO: How to specify this? Path? Arc<objectref>?
    // TODO: Other, more obtuse, types (floats, pointers, strings?)
}

impl EvaluatedValue {
    pub fn as_int(&self) -> Option<i128> {
        match self {
            EvaluatedValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            EvaluatedValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvaluationOrigin {
    pub identity: String,
    pub location: ZeroSpan,
}

#[derive(Debug, Clone)]
pub struct EvaluationSlot {
    pub value: Option<EvaluatedValue>,
    pub constant: Option<bool>,
    pub typed: Option<DMLResolvedType>,
}

impl EvaluationSlot {
    pub fn unknown() -> Self {
        Self {
            value: None,
            constant: None,
            typed: None,
        }
    }

    pub fn from_type(typed: DMLResolvedType) -> Self {
        Self {
            value: None,
            constant: None,
            typed: Some(typed),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EvaluationResult {
    Known {
        slots: Vec<EvaluationSlot>,
        origin: Option<EvaluationOrigin>,
    },
    UnknownArity,
}

impl EvaluationResult {
    pub fn empty() -> Self {
        Self::Known {
            slots: vec![EvaluationSlot::unknown()],
            origin: None,
        }
    }

    pub fn single(slot: EvaluationSlot) -> Self {
        Self::Known { slots: vec![slot], origin: None }
    }

    pub fn slot(&self) -> Option<&EvaluationSlot> {
        match self {
            Self::Known { slots, .. } if slots.len() == 1 => slots.first(),
            _ => None,
        }
    }

    pub fn origin(&self) -> Option<&EvaluationOrigin> {
        match self {
            Self::Known { origin, .. } => origin.as_ref(),
            Self::UnknownArity => None,
        }
    }

    pub fn as_int(&self) -> Option<i128> {
        self.slot()?.value.as_ref().and_then(EvaluatedValue::as_int)
    }

    pub fn as_bool(&self) -> Option<bool> {
        self.slot()?.value.as_ref().and_then(EvaluatedValue::as_bool)
    }

    pub fn constant(&self) -> Option<bool> {
        self.slot()?.constant
    }

    pub fn with_constant(mut self, constant: Option<bool>) -> Self {
        if let Self::Known { slots, .. } = &mut self {
            if let [slot] = slots.as_mut_slice() {
                slot.constant = constant;
            }
        }
        self
    }

    pub fn with_type(mut self, typed: DMLResolvedType) -> Self {
        if let Self::Known { slots, .. } = &mut self {
            if let [slot] = slots.as_mut_slice() {
                slot.typed = Some(typed);
            }
        }
        self
    }
}

pub trait ExpressionResolver {
    fn identifier_type(&mut self, _name: &str) -> Option<DMLResolvedType> {
        None
    }

    fn resolve_type(
        &mut self,
        _typed: &DMLType,
        _context: &mut EvaluationContext,
        _report: &mut Vec<DMLError>,
    ) -> Option<DMLResolvedType> {
        None
    }

    fn function_call(
        &mut self,
        _call: &FunctionCall,
        _context: &mut EvaluationContext,
        _report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        EvaluationResult::UnknownArity
    }
}

struct NoExpressionResolver;

impl ExpressionResolver for NoExpressionResolver {}

pub trait Evaluatable {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult;

    fn evaluate(&self, context: &mut EvaluationContext,
                report: &mut Vec<DMLError>) -> EvaluationResult {
        self.evaluate_with(context, &mut NoExpressionResolver, report)
    }
}

// TODO: As we make improvements, we can add Evaluatable trait impls
// for various types here, for now we allow a small sub-set only

// Note: we fully qualify the types here, as we may want to implement this trait for similarly-named
// types from different stages in the analysis
impl Evaluatable for ExpressionKind {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let span = *self.span();
        let named = |name: &str|DMLResolvedType::Actual(DMLConcreteType {
            kind: DMLConcreteTypeKind::Named(name.to_string()),
            span,
        });
        match self {
            ExpressionKind::TertiaryExpression(expression) =>
                expression.evaluate_with(context, resolver, report),
            ExpressionKind::BinaryExpression(expression) =>
                expression.evaluate_with(context, resolver, report),
            ExpressionKind::UnaryExpression(expression) =>
                expression.evaluate_with(context, resolver, report),
            ExpressionKind::Identifier(identifier) =>
                identifier.evaluate_with(context, resolver, report),
            ExpressionKind::IntegerLiteral(integer) =>
                EvaluationResult::single(EvaluationSlot {
                    value: Some(EvaluatedValue::Int(integer.value())),
                    constant: Some(true),
                    typed: Some(named(match integer {
                        crate::analysis::structure::expressions::IntegerLiteral::Signed(_) =>
                            "int64",
                        crate::analysis::structure::expressions::IntegerLiteral::Unsigned(_) =>
                            "uint64",
                    })),
                }),
            ExpressionKind::FloatLiteral(_) =>
                typed_slot(named("double")),
            ExpressionKind::CharLiteral(_) =>
                typed_slot(named("char")),
            ExpressionKind::StringLiteral(_) => EvaluationResult::single(
                EvaluationSlot {
                    value: None,
                    constant: Some(true),
                    typed: Some(DMLResolvedType::Actual(DMLConcreteType {
                        kind: DMLConcreteTypeKind::Pointer(Box::new(
                            DMLConcreteType {
                                kind: DMLConcreteTypeKind::Const(Box::new(
                                    DMLConcreteType {
                                        kind: DMLConcreteTypeKind::Named(
                                            "char".to_string()),
                                        span,
                                    })),
                                span,
                            })),
                        span,
                    })),
                }),
            ExpressionKind::FunctionCall(call) =>
                resolver.function_call(call, context, report),
            ExpressionKind::MemberLiteral(member) =>
                member.evaluate_with(context, resolver, report),
            ExpressionKind::IndexExpression(index) =>
                index.evaluate_with(context, resolver, report),
            ExpressionKind::CastExpression(cast) => {
                let _ = cast.from.evaluate_with(context, resolver, report);
                resolver.resolve_type(&cast.to, context, report)
                    .map(typed_slot)
                    .unwrap_or_else(EvaluationResult::empty)
            },
            _ => EvaluationResult::empty(),
        }
    }
}

fn unqualified_type(typed: &DMLConcreteType) -> &DMLConcreteType {
    match &typed.kind {
        DMLConcreteTypeKind::Typedef {
            underlying: Some(underlying), ..
        } | DMLConcreteTypeKind::Const(underlying) =>
            unqualified_type(underlying),
        _ => typed,
    }
}

impl Evaluatable for MemberLiteral {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let evaluated = self.left.evaluate_with(context, resolver, report);
        let Some(DMLResolvedType::Actual(left)) = evaluated.slot()
            .and_then(|slot|slot.typed.as_ref())
        else {
            return EvaluationResult::empty();
        };
        let aggregate = match self.operator {
            MemberOp::Dot => unqualified_type(left),
            MemberOp::Arrow => {
                let DMLConcreteTypeKind::Pointer(inner) =
                    &unqualified_type(left).kind
                else {
                    return EvaluationResult::empty();
                };
                unqualified_type(inner)
            },
        };
        let (fields, aggregate_name) = match &aggregate.kind {
            DMLConcreteTypeKind::Struct { fields, .. } => (fields, "Struct"),
            DMLConcreteTypeKind::Layout { fields, .. } => (fields, "Layout"),
            DMLConcreteTypeKind::Bitfields { fields, .. } =>
                (fields, "Bitfields"),
            _ => return EvaluationResult::empty(),
        };
        let Some(field) = fields.iter().find(|field|
            field.name.as_deref() == Some(self.right.val.as_str()))
        else {
            context.report_if_new(DMLError {
                span: self.right.span,
                description: format!(
                    "{} type has no field named '{}'",
                    aggregate_name, self.right.val),
                related: vec![],
                severity: Some(DiagnosticSeverity::ERROR),
            }, report);
            return EvaluationResult::empty();
        };
        EvaluationResult::single(EvaluationSlot::from_type(
            DMLResolvedType::Actual(field.typed.clone())))
    }
}

impl Evaluatable for IndexExpression {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let evaluated = self.array.evaluate_with(context, resolver, report);
        let _ = self.index.evaluate_with(context, resolver, report);
        let Some(DMLResolvedType::Actual(array)) = evaluated.slot()
            .and_then(|slot|slot.typed.as_ref())
        else {
            return EvaluationResult::empty();
        };
        let element = match &unqualified_type(array).kind {
            DMLConcreteTypeKind::Array { element, .. } |
            DMLConcreteTypeKind::Pointer(element) |
            DMLConcreteTypeKind::Vector(element) => element,
            _ => return EvaluationResult::empty(),
        };
        EvaluationResult::single(EvaluationSlot::from_type(
            DMLResolvedType::Actual((**element).clone())))
    }
}

impl Evaluatable for crate::analysis::structure::expressions::Expression {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        self.as_ref().evaluate_with(context, resolver, report)
    }
}

fn typed_slot(typed: DMLResolvedType) -> EvaluationResult {
    EvaluationResult::single(EvaluationSlot::from_type(typed))
}

fn not_operation(inner: EvaluationResult) -> EvaluationResult {
    // For now, we only support the not operator for boolean values, but ! is applicable to
    // other types as well
    // TODO: We don't have a type system yet, but when we do that should guide the semantics of this
    match inner.as_bool() {
        Some(value) => EvaluationResult::single(EvaluationSlot {
            value: Some(EvaluatedValue::Bool(!value)),
            constant: inner.constant(),
            typed: inner.slot().and_then(|slot|slot.typed.clone()),
        }),
        _ => EvaluationResult::empty(),
    }
}

impl Evaluatable for crate::analysis::structure::expressions::UnaryExpression {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let inner_evaluated = self.operand.evaluate_with(
            context, resolver, report);
        // Break out operations to subfunctions so as to not clutter this function too much
        match self.operator {
            crate::analysis::structure::expressions::UnaryOp::Not => not_operation(inner_evaluated),
            crate::analysis::structure::expressions::UnaryOp::PlusPlus => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::MinusMinus => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::Defined => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::Minus => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::BinNot => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::AddressOf => EvaluationResult::empty(),
            crate::analysis::structure::expressions::UnaryOp::Dereference => EvaluationResult::empty(),
        }
    }
}

impl Evaluatable for crate::analysis::structure::expressions::Identifier {
    fn evaluate_with(
        &self,
        _context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        _report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        // TODO: eventually we will do symbol lookups based on the evaluation context,
        // for now we hard-code some constants which are treated especially by DMLC
        let boolean_type = ||DMLResolvedType::Actual(DMLConcreteType {
            kind: DMLConcreteTypeKind::Named("bool".to_string()),
            span: self.name.span,
        });
        match self.name.val.as_str() {
            "true" => EvaluationResult::single(EvaluationSlot {
                value: Some(EvaluatedValue::Bool(true)),
                constant: Some(true),
                typed: Some(boolean_type()),
            }),
            "false" => EvaluationResult::single(EvaluationSlot {
                value: Some(EvaluatedValue::Bool(false)),
                constant: Some(true),
                typed: Some(boolean_type()),
            }),
            "dml_1_2" => EvaluationResult::single(EvaluationSlot {
                value: Some(EvaluatedValue::Bool(false)),
                constant: Some(true),
                typed: Some(boolean_type()),
            }),
            "dml_1_4" => EvaluationResult::single(EvaluationSlot {
                value: Some(EvaluatedValue::Bool(true)),
                constant: Some(true),
                typed: Some(boolean_type()),
            }),
            name => resolver.identifier_type(name)
                .map(typed_slot)
                .unwrap_or_else(EvaluationResult::empty),
        }
    }
}

impl Evaluatable for crate::analysis::structure::expressions::BinaryExpression {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let left_evaluated = self.left.evaluate_with(
            context, resolver, report);
        let right_evaluated = self.right.evaluate_with(
            context, resolver, report);
        let boolean_type = DMLResolvedType::Actual(DMLConcreteType {
            kind: DMLConcreteTypeKind::Named("bool".to_string()),
            span: self.span,
        });
        match &self.operator {
            crate::analysis::structure::expressions::BinOp::Math(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::BinOp::Comp(_) =>
                typed_slot(boolean_type),
            crate::analysis::structure::expressions::BinOp::Logic(logic_op) => logic_expression(
                left_evaluated, right_evaluated, logic_op, report,
            ).with_type(boolean_type),
        }
    }
}

// This is annoying enough to break out into its own function
fn combine_constants(constant_info: &[Option<bool>]) -> Option<bool> {
    for constant in constant_info {
        match constant {
            Some(true) => (),
            Some(false) => return Some(false),
            None => return None,
        }
    }
    Some(true)
}

// These are meaningful to evaluate already, since we have sub-expressions used for logic
fn logic_expression(left: EvaluationResult,
                    right: EvaluationResult,
                    logic_op: &crate::analysis::structure::expressions::LogicOp,
                    _report: &mut Vec<DMLError>) -> EvaluationResult {
    // TODO: we can verify types before matching on operator
    // NOTE: Short-cutting for indeterminate expressions let's us discard
    // indeterminate values in some cases
    // So A | B -> T if A or B is T, even if the other is indeterminate
    // similarly, A & B becomes F is either A or B is F
    let (constant, logic_result) = match logic_op {
        crate::analysis::structure::expressions::LogicOp::And => {
            match (left.as_bool(), right.as_bool()) {
                // TODO: Verify that DMLC short-circuiting actually short-circuits the constant property like this
                (Some(false), _) => (left.constant(), Some(false)),
                (_, Some(false)) => (right.constant(), Some(false)),
                (Some(b1), Some(b2)) => (combine_constants(
                    &[left.constant(), right.constant()]), Some(b1 && b2)),
                _ => (None, None),
            }
        },
        crate::analysis::structure::expressions::LogicOp::Or => {
            match (left.as_bool(), right.as_bool()) {
                (Some(false), Some(false)) => (combine_constants(
                    &[left.constant(), right.constant()]), Some(false)),
                (Some(true), _) => (left.constant(), Some(true)),
                (_, Some(true)) => (right.constant(), Some(true)),
                 _ => (None, None),
            }
        },
    };
    EvaluationResult::single(EvaluationSlot {
        value: logic_result.map(EvaluatedValue::Bool),
        constant,
        typed: None,
    })
}

impl Evaluatable for crate::analysis::structure::expressions::TertiaryExpression {
    fn evaluate_with(
        &self,
        context: &mut EvaluationContext,
        resolver: &mut dyn ExpressionResolver,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let left_evaluated = self.left.evaluate_with(
            context, resolver, report);
        // Patch in the constant-ness of the condition, as that will modify the constant-ness of the result
        let middle_evaluated = self.middle.evaluate_with(
            context, resolver, report);
        let right_evaluated = self.right.evaluate_with(
            context, resolver, report);
        let middle_constant = combine_constants(
            &[left_evaluated.constant(), middle_evaluated.constant()]);
        let right_constant = combine_constants(
            &[left_evaluated.constant(), right_evaluated.constant()]);
        match &self.operator {
            crate::analysis::structure::expressions::TertiaryOp::Cond => 
                if left_evaluated.as_bool() == Some(true) {
                    middle_evaluated.with_constant(middle_constant)
                } else if left_evaluated.as_bool() == Some(false) {
                    right_evaluated.with_constant(right_constant)
                } else {
                    EvaluationResult::empty()
                },
            crate::analysis::structure::expressions::TertiaryOp::HashCond => {
                if left_evaluated.constant() == Some(false) {
                    context.report_if_new(
                        DMLError {
                            span: *self.left.span(),
                            severity: Some(DiagnosticSeverity::ERROR),
                            description: "Condition in #if must be constant".to_string(),
                            related: vec![],
                        }, report);
                }
                if left_evaluated.as_bool() == Some(true) {
                    middle_evaluated.with_constant(middle_constant)
                } else if left_evaluated.as_bool() == Some(false) {
                    right_evaluated.with_constant(right_constant)
                } else {
                    EvaluationResult::empty()
                }
            },
        }
    }
}

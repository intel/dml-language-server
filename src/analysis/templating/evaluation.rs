//  © 2026 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

// Defines traits and functions for evaluating expressions within an analysis

use lsp_types::DiagnosticSeverity;

use crate::analysis::{DMLError, DeclarationSpan, templating::types::DMLResolvedType};

/// Structure that will contain the information available at the point of
/// evaluation, can be extended with additional optional fields in the future
#[derive(Debug, Clone)]
pub struct EvaluationContext {
}

#[allow(clippy::new_without_default)]
impl EvaluationContext {
    pub fn new() -> Self {
        EvaluationContext {
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EvaluatedValue {
    // We'll use this for storing both signed, unsigned, and endian values
    // the disambiguation between them is done by the type in the
    // EvaluationResult struct
    Int(i64),
    Bool(bool),
    DMLObject(()), // TODO: How to specify this? Path? Arc<objectref>?
    // TODO: Other, more obtuse, types (floats, pointers, strings?)
}

impl EvaluatedValue {
    pub fn as_int(&self) -> Option<i64> {
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

#[derive(Debug, Clone)]
pub struct EvaluationResult {
    pub value: Option<EvaluatedValue>,
    // Whether this value is 'true constant' and can be used to resolve #if's at compile time
    pub constant: Option<bool>,
    pub typed: Option<DMLResolvedType>,
}

impl EvaluationResult {
    pub fn empty() -> Self {
        EvaluationResult {
            value: None,
            constant: None,
            typed: None,
        }
    }
    pub fn as_int(&self) -> Option<i64> {
        self.value.as_ref().and_then(EvaluatedValue::as_int)
    }

    pub fn as_bool(&self) -> Option<bool> {
        self.value.as_ref().and_then(EvaluatedValue::as_bool)
    }
}

pub trait Evaluatable {
    fn evaluate(&self, context: &EvaluationContext, report: &mut Vec<DMLError>) -> EvaluationResult;
}

// TODO: As we make improvements, we can add Evaluatable trait impls
// for various types here, for now we allow a small sub-set only

// Note: we fully qualify the types here, as we may want to implement this trait for similarly-named
// types from different stages in the analysis
impl Evaluatable for crate::analysis::structure::expressions::Expression {
    fn evaluate(&self, context: &EvaluationContext, report: &mut Vec<DMLError>) -> EvaluationResult {
        match self.as_ref() {
            crate::analysis::structure::expressions::ExpressionKind::TertiaryExpression(texpr) => texpr.evaluate(context, report),
            crate::analysis::structure::expressions::ExpressionKind::BinaryExpression(binexpr) => binexpr.evaluate(context, report),
            crate::analysis::structure::expressions::ExpressionKind::UnaryExpression(uexpr) => uexpr.evaluate(context, report),
            crate::analysis::structure::expressions::ExpressionKind::IndexExpression(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::Slice(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::Identifier(ident) => ident.evaluate(context, report),
            crate::analysis::structure::expressions::ExpressionKind::IntegerLiteral(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::StringLiteral(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::CharLiteral(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::MemberLiteral(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::FloatLiteral(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::FunctionCall(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::CastExpression(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::NewExpression(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::SizeOf(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::SizeOfType(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::ConstantList(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::EachIn(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::AutoObjectRef(_, _) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::Undefined(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::ExpressionKind::Unknown(_) => EvaluationResult::empty(),
        }
    }
}

fn not_operation(inner: EvaluationResult) -> EvaluationResult {
    // For now, we only support the not operator for boolean values, but ! is applicable to
    // other types as well
    // TODO: We don't have a type system yet, but when we do that should guide the semantics of this
    match inner.value {
        Some(EvaluatedValue::Bool(b)) => EvaluationResult {
            value: Some(EvaluatedValue::Bool(!b)),
            constant: inner.constant,
            typed: inner.typed,
        },
        _ => EvaluationResult::empty(),
    }
}

impl Evaluatable for crate::analysis::structure::expressions::UnaryExpression {
    fn evaluate(&self, context: &EvaluationContext, report: &mut Vec<DMLError>) -> EvaluationResult {
        let inner_evaluated = self.operand.evaluate(context, report);
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
    fn evaluate(&self, _context: &EvaluationContext, _report: &mut Vec<DMLError>) -> EvaluationResult {
        // TODO: eventually we will do symbol lookups based on the evaluation context,
        // for now we hard-code some constants which are treated especially by DMLC
        match self.name.val.as_str() {
            "true" => EvaluationResult {
                value: Some(EvaluatedValue::Bool(true)),
                constant: Some(true),
                typed: None,
            },
            "false" => EvaluationResult {
                value: Some(EvaluatedValue::Bool(false)),
                constant: Some(true),
                typed: None,
            },
            "dml_1_2" => EvaluationResult {
                value: Some(EvaluatedValue::Bool(false)),
                constant: Some(true),
                typed: None,
            },
            "dml_1_4" => EvaluationResult {
                value: Some(EvaluatedValue::Bool(true)),
                constant: Some(true),
                typed: None,
            },
            _ => EvaluationResult::empty(),
        }
    }
}

impl Evaluatable for crate::analysis::structure::expressions::BinaryExpression {
    fn evaluate(&self, context: &EvaluationContext, report: &mut Vec<DMLError>) -> EvaluationResult {
        let left_evaluated = self.left.evaluate(context, report);
        let right_evaluated = self.right.evaluate(context, report);
        match &self.operator {
            crate::analysis::structure::expressions::BinOp::Math(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::BinOp::Comp(_) => EvaluationResult::empty(),
            crate::analysis::structure::expressions::BinOp::Logic(logic_op) => logic_expression(
                left_evaluated, right_evaluated, logic_op, report,
            ),
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
    // NOTE: Short-cutting for indeterminate expressions has some arbitrary heuristic decisions made, in essence we assume
    // that the indeterminate value does not exist. So A | B -> A if B is indeterminate, and A & B similarly becomes A
    let (constant, logic_result) = match logic_op {
        crate::analysis::structure::expressions::LogicOp::And => {
            match (left.as_bool(), right.as_bool()) {
                // TODO: Verify that DMLC short-circuiting actually short-circuits the constant property like this
                (Some(b), None) => (left.constant, Some(b)),
                (None, b) => (right.constant, b),
                (Some(b1), Some(b2)) => (combine_constants(&[left.constant, right.constant]), Some(b1 && b2)),
                
            }
        },
        crate::analysis::structure::expressions::LogicOp::Or => {
            match (left.as_bool(), right.as_bool()) {
                (Some(false), Some(false)) => (combine_constants(&[left.constant, right.constant]), Some(false)),
                (b, None) => (left.constant, b),
                (Some(true), _) => (left.constant, Some(true)),
                (_, b) => (right.constant, b),
            }
        },
    };
    EvaluationResult {
        value: logic_result.map(EvaluatedValue::Bool),
        constant,
        typed: None,
    }
}

impl Evaluatable for crate::analysis::structure::expressions::TertiaryExpression {
    fn evaluate(&self, context: &EvaluationContext, report: &mut Vec<DMLError>) -> EvaluationResult {
        let left_evaluated = self.left.evaluate(context, report);
        // Patch in the constant-ness of the condition, as that will modify the constant-ness of the result
        let mut middle_evaluated = self.middle.evaluate(context, report);
        let mut right_evaluated = self.right.evaluate(context, report);
        middle_evaluated.constant = combine_constants(&[left_evaluated.constant, middle_evaluated.constant]);
        right_evaluated.constant = combine_constants(&[left_evaluated.constant, right_evaluated.constant]);
        match &self.operator {
            crate::analysis::structure::expressions::TertiaryOp::Cond => 
                if left_evaluated.as_bool() == Some(true) {
                    middle_evaluated
                } else {
                    right_evaluated
                },
            crate::analysis::structure::expressions::TertiaryOp::HashCond => {
                if left_evaluated.constant == Some(false) {
                    report.push(DMLError {
                        span: *self.left.span(),
                        severity: Some(DiagnosticSeverity::ERROR),
                        description: "Condition in #if must be constant".to_string(),
                        related: vec![],
                    });
                }
                if left_evaluated.as_bool() == Some(true) {
                    middle_evaluated
                } else {
                    right_evaluated
                }
            },
        }
    }
}

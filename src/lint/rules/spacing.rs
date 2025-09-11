use itertools::izip;
use std::convert::TryInto;
use serde::{Deserialize, Serialize};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::misc::CDeclContent;
use crate::analysis::parsing::types::{BitfieldsContent, LayoutContent,
                                      StructTypeContent};
use crate::lint::{rules::{Rule, RuleType},
                  DMLStyleError};
use crate::analysis::parsing::tree::{LeafToken, TreeElement, ZeroRange};
use crate::analysis::parsing::expression::{BinaryExpressionContent,
                                           FunctionCallContent, IndexContent,
                                           PostUnaryExpressionContent,
                                           TertiaryExpressionContent,
                                           UnaryExpressionContent};
use crate::analysis::parsing::statement::{AfterContent, CompoundContent, ExpressionStmtContent,
                                          ForContent, IfContent, VariableDeclContent,
                                          WhileContent};
use crate::analysis::parsing::structure::{MethodContent,
                                          ObjectStatementsContent};

use crate::span::{ZeroIndexed, Range};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpReservedOptions {}

pub struct SpReservedRule {
    pub enabled: bool,
}

pub struct SpReservedArgs {
    before_range: Option<ZeroRange>,
    token_range: ZeroRange,
    after_range: ZeroRange,
}

impl SpReservedArgs {
    pub fn from_after_content(node: &AfterContent) -> Vec<SpReservedArgs> {
        let mut args_list = vec![];
        if let Some(timer) = &node.timer {
            args_list.push(SpReservedArgs {
                before_range: None,
                token_range: node.after.range(),
                after_range: timer.range(),
            });
        }
        args_list
    }
    pub fn from_if(node: &IfContent) -> Vec<SpReservedArgs> {
        let mut args_list = vec![];

        args_list.push(SpReservedArgs {
            before_range: None,
            token_range: node.iftok.range(),
            after_range: node.lparen.range(),
        });

        if let Some((else_tok, elsebranch)) = &node.elsebranch {
            args_list.push(SpReservedArgs {
                before_range: Some(node.truebranch.range()),
                token_range: else_tok.range(),
                after_range: elsebranch.range(),
            });
        }

        args_list
    }
    pub fn from_for(node: &ForContent) -> Vec<SpReservedArgs> {
        let args_list = vec![
            SpReservedArgs {
                before_range: None,
                token_range: node.fortok.range(),
                after_range: node.lparen.range(),
            }
        ];
        args_list
    }
    pub fn from_while(node: &WhileContent) -> Vec<SpReservedArgs> {
        let args_list = vec![
            SpReservedArgs {
                before_range: None,
                token_range: node.whiletok.range(),
                after_range: node.lparen.range(),
            }
        ];
        args_list
    }
}

impl SpReservedRule {
    pub fn check(&self,
                 args: Vec<SpReservedArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        for arg in args {
            if let Some(before_range) = &arg.before_range {
                if (before_range.row_end == arg.token_range.row_start)
                    && (before_range.col_end == arg.token_range.col_start) {
                    acc.push(
                        self.create_err(Range::combine(
                            *before_range, arg.token_range
                        ))
                    );
                }
            }
            if (arg.token_range.row_end == arg.after_range.row_start)
                && (arg.token_range.col_end == arg.after_range.col_start) {
                acc.push(
                    self.create_err(Range::combine(
                        arg.token_range, arg.after_range
                    ))
                );
            }
        }
    }
}

impl Rule for SpReservedRule {
    fn name() -> &'static str {
        "sp_reserved"
    }
    fn description() -> &'static str {
        "Missing space around reserved words"
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpReserved
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpBraceOptions {}

pub struct SpBracesRule {
    pub enabled: bool,
}

pub struct SpBracesArgs {
    body_start: ZeroRange,
    body_end: ZeroRange,
    lbrace: ZeroRange,
    rbrace: ZeroRange,
}

impl SpBracesArgs {
    pub fn from_compound(node: &CompoundContent) -> Option<SpBracesArgs> {
        if node.statements.is_empty() {
            return None;
        }
        Some(SpBracesArgs {
            body_start: node.statements.first().unwrap().range(),
            body_end: node.statements.last().unwrap().range(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
        })
    }
    pub fn from_obj_stmts(node: &ObjectStatementsContent)
                          -> Option<SpBracesArgs> {
        if let ObjectStatementsContent::List(l_brace,
                                             declarations,
                                             r_brace) = node {
            if declarations.is_empty() {
                return None;
            }
            Some(SpBracesArgs {
                body_start: declarations.first().unwrap().range(),
                body_end: declarations.last().unwrap().range(),
                lbrace: l_brace.range(),
                rbrace: r_brace.range(),
            })
        } else {
            None
        }
    }
    pub fn from_struct_type_content(node: &StructTypeContent)
                                    -> Option<SpBracesArgs> {
        if node.members.is_empty() {
            return None;
        }
        Some(SpBracesArgs {
            body_start: node.members.first().unwrap().range(),
            body_end: node.members.last().unwrap().range(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
        })
    }
    pub fn from_layout_content(node: &LayoutContent) -> Option<SpBracesArgs> {
        if node.fields.is_empty() {
            return None;
        }
        Some(SpBracesArgs {
            body_start: node.fields.first().unwrap().range(),
            body_end: node.fields.last().unwrap().range(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
        })
    }
    pub fn from_bitfields_content(node: &BitfieldsContent)
                                  -> Option<SpBracesArgs> {
        if node.fields.is_empty() {
            return None;
        }
        Some(SpBracesArgs {
            body_start: node.fields.first().unwrap().range(),
            body_end: node.fields.last().unwrap().range(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
        })
    }
}

impl SpBracesRule {
    pub fn check(&self,
                 ranges: Option<SpBracesArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(location) = ranges {
            if (location.lbrace.row_end == location.body_start.row_start)
                && (location.lbrace.col_end == location.body_start.col_start) {
                acc.push(self.create_err(location.lbrace));
            }
            if (location.rbrace.row_start == location.body_end.row_end)
                && (location.rbrace.col_start == location.body_end.col_end) {
                acc.push(self.create_err(location.rbrace));
            }
        }
    }
}

impl Rule for SpBracesRule {
    fn name() -> &'static str {
        "sp_brace"
    }
    fn description() -> &'static str {
        "Missing space around brace."
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpBraces
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpBinopOptions {}

pub struct SpBinopRule {
    pub enabled: bool,
}

pub struct SpBinopArgs {
    left: ZeroRange,
    operator:  ZeroRange,
    right: ZeroRange,
}

impl SpBinopArgs {
    pub fn from_binary_expression_content(node: &BinaryExpressionContent) -> Option<SpBinopArgs> {
        Some(SpBinopArgs {
            left: node.left.range(),
            operator: node.operation.range(),
            right: node.right.range(),
        })
    }
}

impl SpBinopRule {
    pub fn check(&self,
                 ranges: Option<SpBinopArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(location) = ranges {
            if (location.left.row_end == location.operator.row_start)
                && (location.left.col_end == location.operator.col_start) {
                acc.push(self.create_err(location.left));
            }
            if (location.right.row_start == location.operator.row_end)
                && (location.operator.col_end == location.right.col_start) {

                acc.push(self.create_err(location.right));
            }
        }
    }
}

impl Rule for SpBinopRule {
    fn name() -> &'static str {
        "sp_binop"
    }
    fn description() -> &'static str {
        "Missing space around binary operator"
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpBinop
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpTernaryOptions {}

pub struct SpTernaryRule {
    pub enabled: bool,
}

pub struct SpTernaryArgs {
    left: ZeroRange,
    left_op: ZeroRange,
    middle: ZeroRange,
    right_op: ZeroRange,
    right: ZeroRange,
}

impl SpTernaryArgs {
    pub fn from_tertiary_expression_content(node: &TertiaryExpressionContent) -> Option<SpTernaryArgs> {
        Some(SpTernaryArgs {
            left: node.left.range(),
            left_op: node.left_operation.range(),
            middle: node.middle.range(),
            right_op: node.right_operation.range(),
            right: node.right.range(),
        })
    }
}

fn no_gap(left: ZeroRange, right: ZeroRange) -> bool {
    left.row_end == right.row_start
        && left.col_end == right.col_start
}

impl SpTernaryRule {
    pub fn check(&self,
                 ranges: Option<SpTernaryArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(SpTernaryArgs { left, left_op, middle, right_op, right }) = ranges {
            if no_gap(left, left_op) {
                acc.push(self.create_err(
                    ZeroRange::combine(left, left_op)
                ));
            }
            if no_gap(left_op, middle) {
                acc.push(self.create_err(
                    ZeroRange::combine(left_op, middle)
                ));
            }
            if no_gap(middle, right_op) {
                acc.push(self.create_err(
                    ZeroRange::combine(middle, right_op)
                ));
            }
            if no_gap(right_op, right) {
                acc.push(self.create_err(
                    ZeroRange::combine(right_op, right)
                ));
            }
        }
    }
}

impl Rule for SpTernaryRule {
    fn name() -> &'static str {
        "sp_ternary"
    }
    fn description() -> &'static str {
        "Missing space around ? or : in conditional expression"
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpTernary
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpPunctOptions {}

pub struct SpPunctRule {
    pub enabled: bool,
}

pub struct SpPunctArgs {
    before_range_list: Vec<Range<ZeroIndexed>>,
    punct_range_list: Vec<Range<ZeroIndexed>>,
    after_range_list: Vec<Option<Range<ZeroIndexed>>>,
}

impl SpPunctArgs {
    pub fn from_method(node: &MethodContent) -> Option<SpPunctArgs> {
        let mut before_range_list = vec![];
        let mut punct_range_list = vec![];
        let mut after_range_list = vec![];
        let mut iterator = node.arguments.iter().peekable();

        while let Some((arg_decl, comma)) = iterator.next() {
            if let Some(comma_token) = comma {
                before_range_list.push(arg_decl.range());
                punct_range_list.push(comma_token.range());
                if let Some((next_arg_decl, _)) = iterator.peek() {
                    after_range_list.push(Some(next_arg_decl.range()));
                } else {
                    after_range_list.push(None);
                }
            }
        }

        Some(SpPunctArgs {
            before_range_list,
            punct_range_list,
            after_range_list,
        })
    }
    pub fn from_function_call(node: &FunctionCallContent)
                              -> Option<SpPunctArgs> {
        let mut before_range_list = vec![];
        let mut punct_range_list = vec![];
        let mut after_range_list = vec![];
        let mut iterator = node.arguments.iter().peekable();

        while let Some((expression, comma)) = iterator.next() {
            if let Some(comma_token) = comma {
                before_range_list.push(expression.range());
                punct_range_list.push(comma_token.range());
                if let Some((next_expression, _)) = iterator.peek() {
                    after_range_list.push(Some(next_expression.range()));
                } else {
                    after_range_list.push(None);
                }
            }
        }

        Some(SpPunctArgs {
            before_range_list,
            punct_range_list,
            after_range_list,
        })
    }
    pub fn from_expression_stmt(node: &ExpressionStmtContent)
                                -> Option<SpPunctArgs> {
        let mut before_range_list = vec![];
        let mut punct_range_list = vec![];
        let mut after_range_list = vec![];

        before_range_list.push(node.expression.range());
        punct_range_list.push(node.semi.range());
        after_range_list.push(None);

        Some(SpPunctArgs {
            before_range_list,
            punct_range_list,
            after_range_list,
        })
    }
    pub fn from_variable_decl(node: &VariableDeclContent)
                              -> Option<SpPunctArgs> {
        let mut before_range_list = vec![];
        let mut punct_range_list = vec![];
        let mut after_range_list = vec![];

        if let Some(initializer) = node.initializer.as_ref() {
            before_range_list.push(initializer.1.range());
        } else {
            before_range_list.push(node.decls.range());
        }
        punct_range_list.push(node.semi.range());
        after_range_list.push(None);

        Some(SpPunctArgs {
            before_range_list,
            punct_range_list,
            after_range_list,
        })
    }
}

impl SpPunctRule {
    pub fn check(&self,
                 ranges: Option<SpPunctArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(args) = ranges {
            for (before_range, punct_range, after_range) in
                izip!(args.before_range_list,
                      args.punct_range_list,
                      args.after_range_list) {
                if (before_range.row_end != punct_range.row_start)
                    || (before_range.col_end != punct_range.col_start) {
                    let error_range = Range::new(
                        before_range.row_end, punct_range.row_start,
                        before_range.col_end, punct_range.col_start
                    );
                    acc.push(self.create_err(error_range));
                }

                if after_range.is_none() {continue;}

                if (punct_range.row_end == after_range.unwrap().row_start)
                    && (punct_range.col_end == after_range.unwrap().col_start) {
                    let error_range = Range::new(
                        punct_range.row_start, after_range.unwrap().row_end,
                        punct_range.col_start, after_range.unwrap().col_end,
                    );
                    acc.push(self.create_err(error_range));
                }
            }
        }
    }
}

impl Rule for SpPunctRule {
    fn name() -> &'static str {
        "sp_punct"
    }
    fn description() -> &'static str {
        "Missing space after punctuation mark."
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpPunct
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct NspFunparOptions {}

pub struct NspFunparRule {
    pub enabled: bool,
}

// Single ZeroRange required as input for this rule
pub type NspFunparArgs = ZeroRange;

impl NspFunparArgs {
    fn found_gap(fn_name: &ZeroRange, lparen: &ZeroRange)
                 -> Option<NspFunparArgs> {
        if (fn_name.row_end != lparen.row_start)
        || (fn_name.col_end != lparen.col_start) {
            Some(NspFunparArgs {
                row_start: fn_name.row_end,
                row_end: lparen.row_start,
                col_start: fn_name.col_end,
                col_end: lparen.col_start,
            })
        } else { None }
    }
    pub fn from_method(node: &MethodContent) -> Option<NspFunparArgs> {
        Self::found_gap(&node.name.range(), &node.lparen.range())
    }
    pub fn from_function_call(node: &FunctionCallContent)
                              -> Option<NspFunparArgs> {
        Self::found_gap(&node.fun.range(), &node.lparen.range())
    }
}

impl NspFunparRule {
    pub fn check(&self,
                 range: Option<NspFunparArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(gap) = range {
            acc.push(self.create_err(gap));
        }
    }
}

impl Rule for NspFunparRule {
    fn name() -> &'static str {
        "nsp_funpar"
    }
    fn description() -> &'static str {
        "There should be no space between a method/function name and its opening parenthesis."
    }
    fn get_rule_type() -> RuleType {
        RuleType::NspFunpar
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct NspInparenOptions {}

pub struct NspInparenRule {
    pub enabled: bool,
}

pub struct NspInparenArgs {
    opening: ZeroRange,
    content_start: ZeroRange,
    content_end: ZeroRange,
    closing: ZeroRange,
}

impl NspInparenArgs {
    pub fn from_method(node: &MethodContent) -> Option<NspInparenArgs> {
        let content_start_range;
        let content_end_range;
        if node.arguments.is_empty() {
            content_start_range = node.rparen.range();
            content_end_range = node.lparen.range();
        } else {
            content_start_range = node.arguments.first().unwrap().range();
            content_end_range = node.arguments.last().unwrap().range();
        }
        Some(NspInparenArgs {
            opening: node.lparen.range(),
            content_start: content_start_range,
            content_end: content_end_range,
            closing: node.rparen.range(),
        })
    }
    pub fn from_function_call(node: &FunctionCallContent)
                              -> Option<NspInparenArgs> {
        let content_start_range;
        let content_end_range;
        if node.arguments.is_empty() {
            content_start_range = node.rparen.range();
            content_end_range = node.lparen.range();
        } else {
            content_start_range = node.arguments.first().unwrap().range();
            content_end_range = node.arguments.last().unwrap().range();
        }
        Some(NspInparenArgs {
            opening: node.lparen.range(),
            content_start: content_start_range,
            content_end: content_end_range,
            closing: node.rparen.range(),
        })
    }
    pub fn from_if(node: &IfContent) -> Option<NspInparenArgs> {
        Some(NspInparenArgs {
            opening: node.lparen.range(),
            content_start: node.cond.range(),
            content_end: node.cond.range(),
            closing: node.rparen.range(),
        })
    }
    pub fn from_index(node: &IndexContent) -> Option<NspInparenArgs> {
        Some(NspInparenArgs {
            opening: node.lbracket.range(),
            content_start: node.index.range(),
            content_end: node.index.range(),
            closing: node.rbracket.range(),
        })
    }
}

impl NspInparenRule {
    pub fn check(&self,
                 ranges: Option<NspInparenArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(location) =  ranges {
            if (location.opening.row_end == location.content_start.row_start)
                && (location.opening.col_end != location.content_start.col_start) {
                let mut gap = location.opening;
                gap.col_start = location.opening.col_end;
                gap.col_end = location.content_start.col_start;
                acc.push(self.create_err(gap));
            }
            if (location.closing.row_start == location.content_end.row_end)
                && (location.closing.col_start != location.content_end.col_end) {
                let mut gap = location.closing;
                gap.col_end = location.closing.col_start;
                gap.col_start = location.content_end.col_end;
                acc.push(self.create_err(gap));
            }
        }
    }
}

impl Rule for NspInparenRule {
    fn name() -> &'static str {
        "nsp_inparen"
    }
    fn description() -> &'static str {
        "There should be no space after opening or before closing () / []."
    }
    fn get_rule_type() -> RuleType {
        RuleType::NspInparen
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct NspUnaryOptions {}

pub struct NspUnaryRule {
    pub enabled: bool,
}

// Single ZeroRange required as input for this rule
pub type NspUnaryArgs = ZeroRange;

impl NspUnaryArgs {
    fn is_exception(node: &UnaryExpressionContent) -> bool {
        // Defined keyword counts as UnaryOp for DLS, but we allow space after it
        match node.operation.get_token() {
            Some(token) => match token.kind {
                TokenKind::Defined => true,
                _ => false,
            },
            None => false,
        }
    }

    pub fn from_unary_expr(node: &UnaryExpressionContent)
                           -> Option<NspUnaryArgs> {
        if Self::is_exception(node) { return None; }
        let mut gap = node.range();
        gap.col_start = node.operation.range().col_end;
        gap.col_end = node.expr.range().col_start;
        if gap.col_end != gap.col_start {
            Some(gap)
        } else { None }
    }
    pub fn from_postunary_expr(node: &PostUnaryExpressionContent)
                               -> Option<NspUnaryArgs> {
        let mut gap = node.range();
        gap.col_start = node.expr.range().col_end;
        gap.col_end = node.operation.range().col_start;
        if gap.col_end != gap.col_start {
            Some(gap)
        } else { None }
    }
}

impl NspUnaryRule {
    pub fn check(&self,
                 range: Option<NspUnaryArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(gap) = range {
            acc.push(self.create_err(gap));
        }
    }
}

impl Rule for NspUnaryRule {
    fn name() -> &'static str {
        "nsp_unary"
    }
    fn description() -> &'static str {
        "There should be no space between unary operator and its operand."
    }
    fn get_rule_type() -> RuleType {
        RuleType::NspUnary
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct NspTrailingOptions {}

pub struct NspTrailingRule {
    pub enabled: bool,
}

impl NspTrailingRule {
    pub fn check(&self, row: usize, line: &str, acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let len = line.len().try_into().unwrap();
        let row_u32 = row.try_into().unwrap();
        let tokens_end = line.trim_end().len().try_into().unwrap();
        if tokens_end < len {
            acc.push(self.create_err(Range::<ZeroIndexed>::from_u32(row_u32,
                                                        row_u32,
                                                        tokens_end,
                                                        len)));
        }
    }
}

impl Rule for NspTrailingRule {
    fn name() -> &'static str {
        "nsp_trailing"
    }
    fn description() -> &'static str {
        "Found trailing whitespace on row."
    }
    fn get_rule_type() -> RuleType {
        RuleType::NspTrailing
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct SpPtrDeclOptions {}

pub struct SpPtrDeclRule {
    pub enabled: bool,
}

pub struct SpPtrDeclArgs {
    type_name_range: ZeroRange,
    operator_ranges: Vec<ZeroRange>
}

fn extract_operator_ranges_from_cdecl(node: &CDeclContent) -> Vec<ZeroRange> {
    node.modifiers.iter()
            .filter_map(|m| {
                if let LeafToken::Actual(token) = m {
                    if token.kind == TokenKind::Multiply {
                        return Some(m.range());
                    }
                }
                None
            }).collect()
}

impl SpPtrDeclArgs {
    pub fn from_cdecl(node: &CDeclContent) -> Option<SpPtrDeclArgs> {
        // Check if node has a multiply token inside its modifiers
        let operator_ranges: Vec<ZeroRange> = extract_operator_ranges_from_cdecl(node);
        Some(SpPtrDeclArgs {
            type_name_range: node.base.range(),
            operator_ranges,
        })
    }
}

fn has_space_between(range_left: &ZeroRange,
    range_right: &ZeroRange) -> bool {
    !((range_left.row_end == range_right.row_start)
    && (range_left.col_end == range_right.col_start))
}

impl SpPtrDeclRule {
    pub fn check(&self,
                 ranges: Option<SpPtrDeclArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(ranges) = ranges {
            if ranges.operator_ranges.iter().any(|op_range| {
                !has_space_between(&ranges.type_name_range, op_range)
            }) {
                acc.push(self.create_err(ranges.type_name_range));
            }
        }
    }
}

impl Rule for SpPtrDeclRule {
    fn name() -> &'static str {
        "sp_ptrdecl"
    }
    fn description() -> &'static str {
        "There should be a space between type and * marking a pointer"
    }
    fn get_rule_type() -> RuleType {
        RuleType::SpPtrDecl
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct NspPtrDeclOptions {}

pub struct NspPtrDeclRule {
    pub enabled: bool,
}

pub struct NspPtrDeclArgs {
    rightmost_multiply: Option<ZeroRange>,
    identifier_range: ZeroRange
}

impl NspPtrDeclArgs {
    pub fn from_cdecl(node: &CDeclContent) -> Option<NspPtrDeclArgs> {
        // Check if node has a multiply token inside its modifiers
        let operator_ranges: Vec<ZeroRange> = extract_operator_ranges_from_cdecl(node);
        let rightmost_multiply: Option<ZeroRange> = operator_ranges.last().cloned();
        Some(NspPtrDeclArgs {
            rightmost_multiply,
            identifier_range: node.decl.range()
        })
    }
}

impl NspPtrDeclRule {
    pub fn check(&self,
                 ranges: Option<NspPtrDeclArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        if let Some(ranges) = ranges {
            if let Some(op_range) = ranges.rightmost_multiply {
                if has_space_between(&op_range, &ranges.identifier_range) {
                    acc.push(self.create_err(ranges.identifier_range));
                }
            }
        }
    }
}

impl Rule for NspPtrDeclRule {
    fn name() -> &'static str {
        "nsp_ptrdecl"
    }
    fn description() -> &'static str {
        "There should be no space after the * marking a pointer in a declaration"
    }
    fn get_rule_type() -> RuleType {
        RuleType::NspPtrDecl
    }
}

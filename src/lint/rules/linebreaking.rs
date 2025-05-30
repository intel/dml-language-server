use serde::{Deserialize, Serialize};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{ZeroRange, TreeElement};
use crate::analysis::parsing::{expression::{CastContent, FunctionCallContent}};
use crate::lint::{DMLStyleError, LintCfg, RuleType};
use crate::lint::rules::indentation::IndentParenExprArgs;
use super::Rule;

pub struct FuncCallBreakOnOpeningParen {
    pub enabled: bool,
}

pub struct FuncCallBreakOnOpeningParenArgs {
    pub member_ranges: Vec<ZeroRange>,
    pub expected_depth: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct FuncCallBreakOnOpeningParenOptions{}

// pub struct FunctionCallContent {
//     pub fun: Expression,
//     pub lparen: LeafToken,
//     pub arguments: Vec<(SingleInitializer, Option<LeafToken>)>,
//     pub rparen: LeafToken,
// }

impl FuncCallBreakOnOpeningParenArgs {
    pub fn from_function_call(node: &FunctionCallContent, depth: u32) -> Option<FuncCallBreakOnOpeningParenArgs> {
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        for (arg, _comma) in node.arguments.iter() {
            filtered_member_ranges.extend(
                IndentParenExprArgs::filter_out_parenthesized_tokens(arg.tokens())
                    .iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range)
            );
        }
        if filtered_member_ranges.is_empty()
            || ! IndentParenExprArgs::is_broken_after_lparen(
                node.lparen.range(),
                filtered_member_ranges.first()?.to_owned()) {
            return None
        }
        Some(FuncCallBreakOnOpeningParenArgs {
            member_ranges: filtered_member_ranges,
            expected_depth: depth,
        })
    }
}

impl FuncCallBreakOnOpeningParen {
    pub fn check(&self, acc: &mut Vec<DMLStyleError>, args: Option<FuncCallBreakOnOpeningParenArgs>) {
        if !self.enabled {
            return;
        } 
        let Some(args) = args else { return; };
        // TODO : enable shared access to indentation_spaces param for all related rules
        let expected_line_start = args.expected_depth * 4; // Assuming 4 spaces per indent level
        let mut last_row = args.member_ranges.first().unwrap().row_start.0;

        for member_range in args.member_ranges {
            if member_range.row_start.0 != last_row {
                last_row = member_range.row_start.0;
                if member_range.col_start.0 != expected_line_start {
                    acc.push(self.create_err(member_range));
                }
            }
        }
    }
}

impl Rule for FuncCallBreakOnOpeningParen {
    fn name() -> &'static str {
        "FUNC_CALL_BREAK_ON_OPENING_PAREN"
    }
    fn description() -> &'static str {
        "Function or method calls broken right after opening parenthesis should
        indent continuation lines one more level."
    }
    fn get_rule_type() -> RuleType {
        RuleType::LL6
    }
}


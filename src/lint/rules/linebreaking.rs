use serde::{Deserialize, Serialize};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{ZeroRange, TreeElement};
use crate::analysis::parsing::expression::FunctionCallContent;
use crate::lint::{DMLStyleError, RuleType};
use crate::lint::rules::indentation::IndentParenExprArgs;
use super::Rule;

pub const INDENTATION_LEVEL_DEFAULT: u32 = 4;

fn default_indentation_spaces() -> u32 {
    INDENTATION_LEVEL_DEFAULT
}

pub struct FuncCallBreakOnOpenParenRule {
    pub enabled: bool,
    indentation_spaces: u32
}

pub struct FuncCallBreakOnOpenParenArgs {
    pub member_ranges: Vec<ZeroRange>,
    pub expected_depth: u32,
    pub lparen: ZeroRange,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct FuncCallBreakOnOpenParenOptions{
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

impl Rule for FuncCallBreakOnOpenParenRule {
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

impl FuncCallBreakOnOpenParenRule {
    pub fn from_options(options: &Option<FuncCallBreakOnOpenParenOptions>) -> FuncCallBreakOnOpenParenRule {
        match options {
            Some(options) => FuncCallBreakOnOpenParenRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => FuncCallBreakOnOpenParenRule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }

    pub fn check(&self, acc: &mut Vec<DMLStyleError>, args: Option<FuncCallBreakOnOpenParenArgs>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.member_ranges.is_empty() { return; }
        let expected_line_start = self.indentation_spaces * (args.expected_depth + 1);
        let mut last_row = args.lparen.row_start.0;
        if last_row == args.member_ranges.first().unwrap().row_start.0 {
            // If the first member is on the same line as the lparen, we don't
            // need to check it.
            return;
        }
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

impl FuncCallBreakOnOpenParenArgs {
    pub fn from_function_call(node: &FunctionCallContent, depth: u32) -> Option<FuncCallBreakOnOpenParenArgs> {
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
        Some(FuncCallBreakOnOpenParenArgs {
            member_ranges: filtered_member_ranges,
            expected_depth: depth,
            lparen: node.lparen.range(),
        })
    }
}

use serde::{Deserialize, Serialize};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::Token;
use crate::analysis::parsing::structure::MethodContent;
use crate::analysis::parsing::tree::{TreeElement, TreeElementTokenIterator, ZeroRange};
use crate::analysis::parsing::expression::{CastContent, FunctionCallContent, ParenExpressionContent};
use crate::lint::{DMLStyleError, RuleType};
use super::indentation::IndentParenExprArgs;
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
    pub members_ranges: Vec<ZeroRange>,
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
        "FUNC_CALL_BREAK_ON_OPEN_PAREN"
    }
    fn description() -> &'static str {
        "Function or method calls broken right after opening parenthesis should
        indent continuation lines one more level."
    }
    fn get_rule_type() -> RuleType {
        RuleType::LL6
    }
}

impl FuncCallBreakOnOpenParenArgs {
    pub fn filter_out_parenthesized_tokens(expression_tokens: TreeElementTokenIterator) -> Vec<Token> {
        let mut token_list: Vec<Token> = vec![];
        let mut paren_depth = 0;
        // paren_depth is used to identify nested
        // parenthesized expressions within other expressions
        // and avoid double checking this type, given
        // ParenExpressionContent already checks indent_paren_expr on its own
        for token in expression_tokens {
            match token.kind {
                TokenKind::LParen => {
                    if paren_depth == 0 { token_list.push(token); }
                    paren_depth += 1;
                },
                TokenKind::RParen => {
                    paren_depth-=1;
                },
                TokenKind::LBrace => {
                    break;
                },
                _ => { 
                    if paren_depth == 0 { token_list.push(token); }
                }
            }
        }
        token_list
    }
    pub fn from_function_call(node: &FunctionCallContent, depth: u32) -> Option<FuncCallBreakOnOpenParenArgs> {
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        for (arg, _comma) in node.arguments.iter() {
            filtered_member_ranges.extend(
                Self::filter_out_parenthesized_tokens(arg.tokens())
                    .iter().map(|t| t.range)
            );
        }
        if filtered_member_ranges.is_empty()
            || ! IndentParenExprArgs::is_broken_after_lparen(
                node.lparen.range(),
                filtered_member_ranges.first()?.to_owned()) {
            return None
        }
        Some(FuncCallBreakOnOpenParenArgs {
            members_ranges: filtered_member_ranges,
            expected_depth: depth,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_paren_expression(node: &ParenExpressionContent, depth: u32) -> Option<FuncCallBreakOnOpenParenArgs> {
        Some(FuncCallBreakOnOpenParenArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.expr.tokens())
                .iter().map(|t| t.range).collect(),
            expected_depth: depth,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_method(node: &MethodContent, depth: u32) -> Option<FuncCallBreakOnOpenParenArgs> {
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        for (arg, _comma) in node.arguments.iter() {
            filtered_member_ranges.extend(
                Self::filter_out_parenthesized_tokens(arg.tokens())
                    .iter().map(|t| t.range));
        }
        Some(FuncCallBreakOnOpenParenArgs {
            members_ranges: filtered_member_ranges,
            expected_depth: depth,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_cast(node: &CastContent, depth: u32) -> Option<FuncCallBreakOnOpenParenArgs> {
        let mut cast_member_tokens = node.from.tokens();
        cast_member_tokens.append(&mut node.to.tokens());
        Some(FuncCallBreakOnOpenParenArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(cast_member_tokens)
                .iter().map(|t| t.range).collect(),
            expected_depth: depth,
            lparen: node.lparen.range(),
        })
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

    pub fn check(&self, args: Option<FuncCallBreakOnOpenParenArgs>, acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.members_ranges.is_empty() { return; }
        let expected_line_start = self.indentation_spaces * (args.expected_depth + 1);
        let mut last_row = args.lparen.row_start.0;
        if last_row == args.members_ranges.first().unwrap().row_start.0 {
            // If the first member is on the same line as the lparen, we don't
            // need to check it.
            return;
        }
        for member_range in args.members_ranges {
            if member_range.row_start.0 != last_row {
                last_row = member_range.row_start.0;
                if member_range.col_start.0 != expected_line_start {
                    acc.push(self.create_err(member_range));
                }
            }
        }
    }
}

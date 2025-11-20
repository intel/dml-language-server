//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::convert::TryInto;

use crate::analysis::parsing::{expression::{CastContent,
        FunctionCallContent,
        ParenExpressionContent},
    lexer::TokenKind,
    parser::Token,
    statement::{self,
        CompoundContent,
        DoContent,
        ForContent,
        ForeachContent,
        IfContent,
        StatementContent,
        SwitchCase,
        SwitchContent,
        WhileContent},
    structure::{DMLObjectContent,
        MethodContent,
        ObjectStatementsContent},
    tree::TreeElementTokenIterator,
    types::{BitfieldsContent,
        LayoutContent,
        StructTypeContent}};
use crate::span::{Range, ZeroIndexed};
use crate::analysis::parsing::tree::{ZeroRange, Content, TreeElement};
use serde::{Deserialize, Serialize};
use super::Rule;
use crate::lint::{LintCfg, DMLStyleError, RuleType};

pub const MAX_LENGTH_DEFAULT: u32 = 80;
pub const INDENTATION_LEVEL_DEFAULT: u32 = 4;

fn default_indentation_spaces() -> u32 {
    INDENTATION_LEVEL_DEFAULT
}

pub fn setup_indentation_size(cfg: &mut LintCfg) {
    let mut indentation_spaces = INDENTATION_LEVEL_DEFAULT;

    if let Some(size) = &cfg.indent_size {
        indentation_spaces = size.indentation_spaces;
    }
    if let Some(indent_code_block) = &mut cfg.indent_code_block {
        indent_code_block.indentation_spaces = indentation_spaces;
    }
    if let Some(indent_switch_case) = &mut cfg.indent_switch_case {
        indent_switch_case.indentation_spaces = indentation_spaces;
    }
    if let Some(indent_empty_loop) = &mut cfg.indent_empty_loop {
        indent_empty_loop.indentation_spaces = indentation_spaces;
    }
    if let Some(indent_continuation_line) = &mut cfg.indent_continuation_line {
        indent_continuation_line.indentation_spaces = indentation_spaces;
    }
}
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct LongLineOptions {
    pub max_length: u32,
}

pub struct LongLinesRule {
    pub enabled: bool,
    pub max_length: u32,
}

impl LongLinesRule {
    pub fn from_options(options: &Option<LongLineOptions>) -> LongLinesRule {
        match options {
            Some(long_lines) => LongLinesRule {
                enabled: true,
                max_length: long_lines.max_length,
            },
            None => LongLinesRule {
                enabled: false,
                max_length: MAX_LENGTH_DEFAULT,
            },
        }
    }
    pub fn check(&self, row: usize, line: &str, acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let len = line.len().try_into().unwrap();
        if len > self.max_length {
            let rowu32 = row.try_into().unwrap();
            acc.push(
                self.create_err(Range::<ZeroIndexed>::from_u32(rowu32,rowu32, self.max_length, len))
            );
        }
    }
}
impl Rule for LongLinesRule {
    fn name() -> &'static str {
        "long_lines"
    }
    fn description() -> &'static str {
        "Line length is above the threshold."
    }
    fn get_rule_type() -> RuleType {
        RuleType::LL1
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentSizeOptions {
    pub indentation_spaces: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentNoTabOptions {}

pub struct IndentNoTabRule {
    pub enabled: bool,
}

impl IndentNoTabRule {
    pub fn check(&self, row: usize, line: &str, acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let rowu32 = row.try_into().unwrap();

        for (col, _) in line.match_indices('\t') {
            let colu32 = col.try_into().unwrap();
            acc.push(
                self.create_err(Range::<ZeroIndexed>::from_u32(rowu32, rowu32, colu32, colu32 + 1))
            );
        }
    }
}
impl Rule for IndentNoTabRule {
    fn name() -> &'static str {
        "indent_no_tabs"
    }
    fn description() -> &'static str {
        "Tab characters (ASCII 9) should never be used to indent lines."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN2
    }
}

pub struct IndentCodeBlockRule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentCodeBlockOptions {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IndentCodeBlockArgs {
    members_ranges: Vec<ZeroRange>,
    lbrace: ZeroRange,
    rbrace: ZeroRange,
    expected_depth: u32,
}

impl IndentCodeBlockArgs {
    pub fn from_obj_stmts_content(node: &ObjectStatementsContent, depth: u32) -> Option<IndentCodeBlockArgs> {
        if let ObjectStatementsContent::List(lbrace, stmnts, rbrace) = node {
            Some(IndentCodeBlockArgs {
                members_ranges: stmnts.iter().map(|s| s.range()).collect(),
                lbrace: lbrace.range(),
                rbrace: rbrace.range(),
                expected_depth: depth,
            })
        } else {
            None
        }
    }
    pub fn from_struct_type_content(node: &StructTypeContent, depth: u32) -> Option<IndentCodeBlockArgs> {
        Some(IndentCodeBlockArgs {
            members_ranges: node.members.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_compound_content(node: &CompoundContent, depth: u32) -> Option<IndentCodeBlockArgs> {
        Some(IndentCodeBlockArgs {
            members_ranges: node.statements.iter().map(|s| s.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_layout_content(node: &LayoutContent, depth: u32) -> Option<IndentCodeBlockArgs> {
        Some(IndentCodeBlockArgs {
            members_ranges: node.fields.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_bitfields_content(node: &BitfieldsContent, depth: u32) -> Option<IndentCodeBlockArgs> {
        Some(IndentCodeBlockArgs {
            members_ranges: node.fields.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
}

impl IndentCodeBlockRule {
    pub fn from_options(options: &Option<IndentCodeBlockOptions>) -> IndentCodeBlockRule {
        match options {
            Some(options) => IndentCodeBlockRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IndentCodeBlockRule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self,
                 args: Option<IndentCodeBlockArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.members_ranges.is_empty() { return; }
        if args.lbrace.row_start == args.rbrace.row_start ||
            args.lbrace.row_start == args.members_ranges[0].row_start { return; }
        for member_range in args.members_ranges {
            if self.indentation_is_not_aligned(member_range, args.expected_depth) {
                acc.push(self.create_err(member_range));
            }
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        // Implicit IN1
        let expected_column = self.indentation_spaces * depth;
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IndentCodeBlockRule {
    fn name() -> &'static str {
        "indent_code_block"
    }
    fn description() -> &'static str {
        "Previous line contains an opening brace and current line is not one \
         level of indentation ahead of past line."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN3
    }
}

pub struct IndentClosingBraceRule {
    pub enabled: bool,
    pub indentation_spaces: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentClosingBraceOptions {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

impl Rule for IndentClosingBraceRule {
    fn name() -> &'static str {
        "indent_closing_brace"
    }
    fn description() -> &'static str {
        "Closing braces at the beginning of a line should be aligned to the corresponding \
        indentation level of the statement that started the code block. A closing brace should \
        only ever appear on the same line as the opening brace, or first on a line."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN4
    }
}

pub struct IndentClosingBraceArgs {
    expected_depth: u32,
    lbrace: ZeroRange,
    last_member: ZeroRange,
    rbrace: ZeroRange,
}

impl IndentClosingBraceArgs {
    pub fn from_compound_content(node: &CompoundContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        Some(IndentClosingBraceArgs {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.statements.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_obj_stmts_content(node: &ObjectStatementsContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        if let ObjectStatementsContent::List(lbrace, stmnts, rbrace) = node {
            Some(IndentClosingBraceArgs {
                expected_depth: depth.saturating_sub(1),
                lbrace: lbrace.range(),
                last_member: stmnts.last()?.range(),
                rbrace: rbrace.range(),
            })
        } else {
            None
        }
    }

    pub fn from_switch_content(node: &SwitchContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        Some(IndentClosingBraceArgs {
            // Switch content does not increase indentation level before this call
            // so there is no need to reduce it
            expected_depth: depth,
            lbrace: node.lbrace.range(),
            last_member: node.cases.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_struct_type_content(node: &StructTypeContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        Some(IndentClosingBraceArgs {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.members.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_layout_content(node: &LayoutContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        Some(IndentClosingBraceArgs {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.fields.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_bitfields_content(node: &BitfieldsContent, depth: u32) -> Option<IndentClosingBraceArgs> {
        Some(IndentClosingBraceArgs {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.fields.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

}

impl IndentClosingBraceRule {
    pub fn from_options(options: &Option<IndentClosingBraceOptions>) -> IndentClosingBraceRule {
        match options {
            Some(options) => IndentClosingBraceRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces,
            },
            None => IndentClosingBraceRule {
                enabled: false,
                indentation_spaces: 0,
            },
        }
    }

    pub fn check(&self,
                 args: Option<IndentClosingBraceArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };

        let lbrace_on_same_row_than_rbrace:bool = args.lbrace.row_start
            == args.rbrace.row_start;
        if lbrace_on_same_row_than_rbrace { return; }

        let last_member_on_same_row_than_rbrace:bool = args.last_member.row_end
            == args.rbrace.row_start;
        if last_member_on_same_row_than_rbrace {
            acc.push(self.create_err(args.rbrace));
            return;
        }

        let rbrace_on_same_ind_level_than_switchtok:bool = args.rbrace.col_start.0
            == args.expected_depth * self.indentation_spaces;
        if !rbrace_on_same_ind_level_than_switchtok {
            acc.push(self.create_err(args.rbrace));
        }
    }
}


pub struct IndentParenExprRule {
    pub enabled: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentParenExprOptions {}

pub struct IndentParenExprArgs {
    members_ranges: Vec<ZeroRange>,
    lparen: ZeroRange,
}

impl IndentParenExprArgs {
    pub fn is_broken_after_lparen(lparen_range: ZeroRange, next_token_range: ZeroRange) -> bool {
        lparen_range.row_start != next_token_range.row_start
    }
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
                    if paren_depth == 0 { token_list.push(token); }
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

    pub fn from_for(node: &ForContent) -> Option<IndentParenExprArgs> {
        // For loop has three parts within parentheses: pre, cond, and post
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        filtered_member_ranges.extend(&mut Self::filter_out_parenthesized_tokens(node.pre.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range));
        filtered_member_ranges.push(node.lsemi.range());
        filtered_member_ranges.extend(&mut Self::filter_out_parenthesized_tokens(node.cond.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range));
        filtered_member_ranges.push(node.rsemi.range());
        filtered_member_ranges.extend(&mut Self::filter_out_parenthesized_tokens(node.post.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range));

        Some(IndentParenExprArgs {
            members_ranges: filtered_member_ranges,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_foreach(node: &ForeachContent) -> Option<IndentParenExprArgs> {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.expression.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_function_call(node: &FunctionCallContent) -> Option<IndentParenExprArgs> {
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        for (arg, _comma) in node.arguments.iter() {
            filtered_member_ranges.extend(Self::filter_out_parenthesized_tokens(arg.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range));
        }
        if filtered_member_ranges.is_empty()
            || Self::is_broken_after_lparen(node.lparen.range(),
                filtered_member_ranges.first()?.to_owned()) {
            return None
        }
        Some(IndentParenExprArgs {
            members_ranges: filtered_member_ranges,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_paren_expression(node: &ParenExpressionContent)
            -> Option<IndentParenExprArgs> {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.expr.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_method(node: &MethodContent) -> Option<IndentParenExprArgs> {
        let mut filtered_member_ranges: Vec<ZeroRange> = vec![];
        for (arg, _comma) in node.arguments.iter() {
            filtered_member_ranges.extend(Self::filter_out_parenthesized_tokens(arg.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range));
        }
        Some(IndentParenExprArgs {
            members_ranges: filtered_member_ranges,
            lparen: node.lparen.range(),
        })
    }

    pub fn from_while(node: &WhileContent) -> Option<IndentParenExprArgs> {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.cond.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_do_while(node: &DoContent) -> Option<IndentParenExprArgs> {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.cond.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_if(node: &IfContent) -> Option<IndentParenExprArgs>  {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.cond.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_cast(node: &CastContent) -> Option<IndentParenExprArgs> {
        let mut cast_member_tokens = node.from.tokens();
        cast_member_tokens.append(&mut node.to.tokens());
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(cast_member_tokens).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }

    pub fn from_switch(node: &SwitchContent) -> Option<IndentParenExprArgs> {
        Some(IndentParenExprArgs {
            members_ranges: Self::filter_out_parenthesized_tokens(node.expr.tokens()).iter().filter(|t| t.kind != TokenKind::RParen).map(|t| t.range).collect(),
            lparen: node.lparen.range(),
        })
    }
}

impl IndentParenExprRule {
    pub fn check(&self,
                 args: Option<IndentParenExprArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.members_ranges.is_empty() { return; }
        let expected_line_start = args.lparen.col_start.0 + 1;
        let mut last_row = args.lparen.row_start.0;
        if last_row != args.members_ranges.first().unwrap().row_start.0 {
            // If the first member is not on the same line as the lparen,
            // then it is not a continuation line, so we do not check it.
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

impl Rule for IndentParenExprRule {
    fn name() -> &'static str {
        "indent_paren_expr"
    }
    fn description() -> &'static str {
        "Continuation line broken inside a parenthesized expression not \
         indented to line up with the corresponding parenthesis."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN5
    }
}


pub struct IndentSwitchCaseRule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentSwitchCaseOptions {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IndentSwitchCaseArgs {
    case_range: ZeroRange,
    expected_depth: u32,
}

impl IndentSwitchCaseArgs {
    pub fn from_switch_case(node: &SwitchCase, depth: u32) -> Option<IndentSwitchCaseArgs> {
        match node {
            SwitchCase::Case(_, _, _) |
            SwitchCase::Default(_, _) => {},
            SwitchCase::Statement(statement) => {
                if let Content::Some(statement::StatementContent::Compound(_)) = *statement.content {
                    return None;
                }
            },
            SwitchCase::HashIf(_) => {
                return None;
            }
        }

        Some(IndentSwitchCaseArgs {
            case_range: node.range(),
            expected_depth: depth
        })

    }
}

impl IndentSwitchCaseRule {
    pub fn from_options(options: &Option<IndentSwitchCaseOptions>) -> IndentSwitchCaseRule {
        match options {
            Some(options) => IndentSwitchCaseRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IndentSwitchCaseRule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self,
                 args: Option<IndentSwitchCaseArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if self.indentation_is_not_aligned(args.case_range, args.expected_depth) {
            acc.push(self.create_err(args.case_range));
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        // Implicit IN1
        let expected_column = self.indentation_spaces * depth;
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IndentSwitchCaseRule {
    fn name() -> &'static str {
        "indent_switch_case"
    }
    fn description() -> &'static str {
        "Case labels should be indented at the same level as the switch keyword, \
         statements should be indented one level deeper and not in the same line as the case label."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN9
    }
}

// IndentEmptyLoop: Indentation in empty loop
pub struct IndentEmptyLoopRule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentEmptyLoopOptions {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IndentEmptyLoopArgs {
    loop_keyword_range: ZeroRange,
    semicolon_range: ZeroRange,
    expected_depth: u32,
}

impl IndentEmptyLoopArgs {
    pub fn from_for_content(node: &ForContent, depth: u32) -> Option<IndentEmptyLoopArgs> {
        if let Content::Some(statement::StatementContent::Empty(semicolon)) = node.statement.content.as_ref() {
            return Some(IndentEmptyLoopArgs {
                loop_keyword_range: node.fortok.range(),
                semicolon_range: semicolon.range(),
                expected_depth: depth + 1
            });

        }
        None
    }

    pub fn from_while_content(node: &WhileContent, depth: u32) -> Option<IndentEmptyLoopArgs> {
        if let Content::Some(statement::StatementContent::Empty(semicolon)) = node.statement.content.as_ref() {
            return Some(IndentEmptyLoopArgs {
                loop_keyword_range: node.whiletok.range(),
                semicolon_range: semicolon.range(),
                expected_depth: depth + 1
            });

        }
        None
    }
}

impl IndentEmptyLoopRule {
    pub fn from_options(options: &Option<IndentEmptyLoopOptions>) -> IndentEmptyLoopRule {
        match options {
            Some(options) => IndentEmptyLoopRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IndentEmptyLoopRule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self,
                 args: Option<IndentEmptyLoopArgs>,
                 acc: &mut Vec<DMLStyleError>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if self.indentation_is_not_aligned(args.semicolon_range, args.expected_depth) ||
            args.loop_keyword_range.row_start == args.semicolon_range.row_start {
            acc.push(
                self.create_err(Range::combine(args.loop_keyword_range, args.semicolon_range))
            );
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        let expected_column = self.indentation_spaces * depth;
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IndentEmptyLoopRule {
    fn name() -> &'static str {
        "indent_empty_loop"
    }
    fn description() -> &'static str {
        "When the body of a while or for loop is left empty, \
        indent the semicolon to the appropriate statement level."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN10
    }
}

// in6
pub struct IndentContinuationLineRule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IndentContinuationLineOptions {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IndentContinuationLineArgs {
    token_list: Vec<Token>,
    expected_depth: u32,
}

impl IndentContinuationLineArgs {
    pub fn filter_out_last_semi_ranges(expression_tokens: &mut TreeElementTokenIterator) {
        // This function filters out the last semicolon in a list of tokens
        // as it is not part of the continuation line check.
        // expression_tokens.pop_if(|token| token.kind == TokenKind::SemiColon);
        if let Some(last_token) = expression_tokens.last() {
            if last_token.kind == TokenKind::SemiColon {
                expression_tokens.pop();
            }
        }
    }

    pub fn from_statement_content(node: &StatementContent, depth: u32) -> Option<IndentContinuationLineArgs> {
        match node {
            StatementContent::Compound(_) |
            StatementContent::If(_) |
            StatementContent::While(_) |
            StatementContent::Do(_) |
            StatementContent::For(_) |
            StatementContent::Try(_) |
            StatementContent::Foreach(_) |
            StatementContent::Throw(_) |
            StatementContent::Switch(_) => return None,
            _ => {}
        };
        let mut tokens = node.tokens();
        Self::filter_out_last_semi_ranges(&mut tokens);

        Some(IndentContinuationLineArgs {
            token_list: IndentParenExprArgs::filter_out_parenthesized_tokens(tokens),
            expected_depth: depth,
        })
    }

    pub fn from_dml_object_content(node: &DMLObjectContent, depth: u32) -> Option<IndentContinuationLineArgs> {
        match node {
            DMLObjectContent::Parameter(_) |
            DMLObjectContent::Hook(_) |
            DMLObjectContent::Import(_) |
            DMLObjectContent::InEach(_) |
            DMLObjectContent::Session(_) |
            DMLObjectContent::Typedef(_) => {
                let mut tokens = node.tokens();
                Self::filter_out_last_semi_ranges(&mut tokens);
                return Some(IndentContinuationLineArgs {
                    token_list: IndentParenExprArgs::filter_out_parenthesized_tokens(tokens),
                    expected_depth: depth,
            })},
            _ => return None,
        }
    }
}

impl IndentContinuationLineRule {
    pub fn from_options(options: &Option<IndentContinuationLineOptions>) -> IndentContinuationLineRule {
        match options {
            Some(options) => IndentContinuationLineRule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IndentContinuationLineRule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }

    pub fn check<'a> (&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IndentContinuationLineArgs>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.token_list.is_empty() { return; }
        let expected_line_start = self.indentation_spaces * (args.expected_depth + 1);
        let mut last_row = args.token_list.first().unwrap().range.row_start.0;

        for token in args.token_list {
            match token.kind {
                TokenKind::RParen => {
                    if token.range.row_start.0 != last_row {
                        last_row = token.range.row_start.0;
                    }
                }
                _ => {
                    if token.range.row_start.0 != last_row {
                        last_row = token.range.row_start.0;
                        if token.range.col_start.0 != expected_line_start {
                            acc.push(self.create_err(token.range))
                        }
                    }
                }
            }
        }
    }
}

impl Rule for IndentContinuationLineRule {
    fn name() -> &'static str {
        "INDENT_CONTINUATION_LINE"
    }
    fn description() -> &'static str {
        "A continuation line not broken inside a parenthesized expression is indented one level."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN6
    }
}

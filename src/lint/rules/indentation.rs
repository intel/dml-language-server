use std::convert::TryInto;

use crate::analysis::parsing::{statement::{self, CompoundContent, ForContent,
                               SwitchCase, WhileContent, SwitchContent},
                               structure::ObjectStatementsContent,
                               types::{BitfieldsContent, LayoutContent, StructTypeContent}};
use crate::span::{Range, ZeroIndexed};
use crate::analysis::LocalDMLError;
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

    if let Some(in1) = &cfg.in1 {
        indentation_spaces = in1.indentation_spaces;
    }
    if let Some(in3) = &mut cfg.in3 {
        in3.indentation_spaces = indentation_spaces;
    }
    if let Some(in9) = &mut cfg.in9 {
        in9.indentation_spaces = indentation_spaces;
    }
    if let Some(in10) = &mut cfg.in10 {
        in10.indentation_spaces = indentation_spaces;
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
    pub fn check(&self, acc: &mut Vec<DMLStyleError>, row: usize, line: &str) {
        if !self.enabled { return; }
        let len = line.len().try_into().unwrap();
        if len > self.max_length {
            let rowu32 = row.try_into().unwrap();
            let msg = LongLinesRule::description().to_owned()
                + format!(" of {} characters", self.max_length).as_str();
            let dmlerror = DMLStyleError {
                error: LocalDMLError {
                    range: Range::<ZeroIndexed>::from_u32(rowu32, rowu32, self.max_length, len),
                    description: msg,
                },
                rule_type: Self::get_rule_type(),
            };
            acc.push(dmlerror);
        }
    }
}
impl Rule for LongLinesRule {
    fn name() -> &'static str {
        "LONG_LINE"
    }
    fn description() -> &'static str {
        "Line length is above the threshold"
    }
    fn get_rule_type() -> RuleType {
        RuleType::LongLines
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN1Options {
    pub indentation_spaces: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN2Options {}

pub struct IN2Rule {
    pub enabled: bool,
}

impl IN2Rule {
    pub fn check(&self, acc: &mut Vec<DMLStyleError>, row: usize, line: &str) {
        if !self.enabled { return; }
        let rowu32 = row.try_into().unwrap();

        for (col, _) in line.match_indices('\t') {
            let colu32 = col.try_into().unwrap();
            let msg = IN2Rule::description().to_owned();
            let dmlerror = DMLStyleError {
                error: LocalDMLError {
                    range: Range::<ZeroIndexed>::from_u32(rowu32, rowu32, colu32, colu32 + 1),
                    description: msg,
                },
                rule_type: Self::get_rule_type(),
            };
            acc.push(dmlerror);
        }
    }
}
impl Rule for IN2Rule {
    fn name() -> &'static str {
        "IN2"
    }
    fn description() -> &'static str {
        "Tab characters (ASCII 9) should never be used to indent lines."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN2
    }
}

pub struct IN3Rule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN3Options {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IN3Args {
    members_ranges: Vec<ZeroRange>,
    lbrace: ZeroRange,
    rbrace: ZeroRange,
    expected_depth: u32,
}

impl IN3Args {
    pub fn from_obj_stmts_content(node: &ObjectStatementsContent, depth: u32) -> Option<IN3Args> {
        if let ObjectStatementsContent::List(lbrace, stmnts, rbrace) = node {
            Some(IN3Args {
                members_ranges: stmnts.iter().map(|s| s.range()).collect(),
                lbrace: lbrace.range(),
                rbrace: rbrace.range(),
                expected_depth: depth,
            })
        } else {
            None
        }
    }
    pub fn from_struct_type_content(node: &StructTypeContent, depth: u32) -> Option<IN3Args> {
        Some(IN3Args {
            members_ranges: node.members.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_compound_content(node: &CompoundContent, depth: u32) -> Option<IN3Args> {
        Some(IN3Args {
            members_ranges: node.statements.iter().map(|s| s.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_layout_content(node: &LayoutContent, depth: u32) -> Option<IN3Args> {
        Some(IN3Args {
            members_ranges: node.fields.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_bitfields_content(node: &BitfieldsContent, depth: u32) -> Option<IN3Args> {
        Some(IN3Args {
            members_ranges: node.fields.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
}

impl IN3Rule {
    pub fn from_options(options: &Option<IN3Options>) -> IN3Rule {
        match options {
            Some(options) => IN3Rule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IN3Rule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IN3Args>)
    {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.lbrace.row_start == args.rbrace.row_start ||
            args.lbrace.row_start == args.members_ranges[0].row_start { return; }
        for member_range in args.members_ranges {
            if self.indentation_is_not_aligned(member_range, args.expected_depth) {
                let dmlerror = DMLStyleError {
                    error: LocalDMLError {
                        range: member_range,
                        description: Self::description().to_string(),
                    },
                    rule_type: Self::get_rule_type(),
                };
                acc.push(dmlerror);
            }
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        // Implicit IN1
        let expected_column = self.indentation_spaces * depth;
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IN3Rule {
    fn name() -> &'static str {
        "IN3"
    }
    fn description() -> &'static str {
        "Previous line contains an openning brace and current line is not one\
         level of indentation ahead of past line"
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN3
    }
}

pub struct IN4Rule {
    pub enabled: bool,
    pub indentation_spaces: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN4Options {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

impl Rule for IN4Rule {
    fn name() -> &'static str {
        "IN4"
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

pub struct IN4Args {
    expected_depth: u32,
    lbrace: ZeroRange,
    last_member: ZeroRange,
    rbrace: ZeroRange,
}

impl IN4Args {
    pub fn from_compound_content(node: &CompoundContent, depth: u32) -> Option<IN4Args> {
        Some(IN4Args {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.statements.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_obj_stmts_content(node: &ObjectStatementsContent, depth: u32) -> Option<IN4Args> {
        if let ObjectStatementsContent::List(lbrace, stmnts, rbrace) = node {
            Some(IN4Args {
                expected_depth: depth.saturating_sub(1),
                lbrace: lbrace.range(),
                last_member: stmnts.last()?.range(),
                rbrace: rbrace.range(),
            })
        } else {
            None
        }
    }

    pub fn from_switch_content(node: &SwitchContent, depth: u32) -> Option<IN4Args> {
        Some(IN4Args {
            // Switch content does not increase indentation level before this call
            // so there is no need to reduce it
            expected_depth: depth,
            lbrace: node.lbrace.range(),
            last_member: node.cases.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_struct_type_content(node: &StructTypeContent, depth: u32) -> Option<IN4Args> {
        Some(IN4Args {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.members.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_layout_content(node: &LayoutContent, depth: u32) -> Option<IN4Args> {
        Some(IN4Args {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.fields.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

    pub fn from_bitfields_content(node: &BitfieldsContent, depth: u32) -> Option<IN4Args> {
        Some(IN4Args {
            expected_depth: depth.saturating_sub(1),
            lbrace: node.lbrace.range(),
            last_member: node.fields.last()?.range(),
            rbrace: node.rbrace.range(),
        })
    }

}

impl IN4Rule {
    pub fn from_options(options: &Option<IN4Options>) -> IN4Rule {
        match options {
            Some(options) => IN4Rule {
                enabled: true,
                indentation_spaces: options.indentation_spaces,
            },
            None => IN4Rule {
                enabled: false,
                indentation_spaces: 0,
            },
        }
    }

    pub fn check(&self, acc: &mut Vec<DMLStyleError>, args: Option<IN4Args>) {
        if !self.enabled { return; }
        let Some(args) = args else { return; };

        let lbrace_on_same_row_than_rbrace:bool = args.lbrace.row_start
            == args.rbrace.row_start;
        if lbrace_on_same_row_than_rbrace { return; }

        let last_member_on_same_row_than_rbrace:bool = args.last_member.row_end
            == args.rbrace.row_start;
        if last_member_on_same_row_than_rbrace {
            return self.push_err(acc, args.rbrace);
        }

        let rbrace_on_same_ind_level_than_switchtok:bool = args.rbrace.col_start.0
            == args.expected_depth * self.indentation_spaces;
        if !rbrace_on_same_ind_level_than_switchtok {
            return self.push_err(acc, args.rbrace);
        }
    }
}

pub struct IN9Rule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN9Options {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IN9Args {
    case_range: ZeroRange,
    expected_depth: u32,
}

impl IN9Args {
    pub fn from_switch_case(node: &SwitchCase, depth: u32) -> Option<IN9Args> {
        match node {
            SwitchCase::Case(_, _, _) |
            SwitchCase::Default(_, _) => {},
            SwitchCase::Statement(statement) => {
                if let Content::Some(ref content) = *statement.content {
                    if let statement::StatementContent::Compound(_) = content {
                        return None;
                    }
                }
            },
            SwitchCase::HashIf(_) => {
                return None;
            }
        }

        Some(IN9Args {
            case_range: node.range(),
            expected_depth: depth
        })

    }
}

impl IN9Rule {
    pub fn from_options(options: &Option<IN9Options>) -> IN9Rule {
        match options {
            Some(options) => IN9Rule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IN9Rule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IN9Args>)
    {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if self.indentation_is_not_aligned(args.case_range, args.expected_depth) {
            let dmlerror = DMLStyleError {
                error: LocalDMLError {
                    range: args.case_range,
                    description: Self::description().to_string(),
                },
                rule_type: Self::get_rule_type(),
            };
            acc.push(dmlerror);
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        // Implicit IN1
        let expected_column = self.indentation_spaces * depth;
        print!("{:#?}", expected_column);
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IN9Rule {
    fn name() -> &'static str {
        "IN9"
    }
    fn description() -> &'static str {
        "Case labels should be indented at the same level as the switch keyword, \
         statements should be indented one level deeper and not in the same line as the case label."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN9
    }
}

// IN10: Indentation in empty loop
pub struct IN10Rule {
    pub enabled: bool,
    indentation_spaces: u32
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN10Options {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IN10Args {
    loop_keyword_range: ZeroRange,
    semicolon_range: ZeroRange,
    expected_depth: u32,
}

impl IN10Args {
    pub fn from_for_content(node: &ForContent, depth: u32) -> Option<IN10Args> {
        if let Content::Some(statement::StatementContent::Empty(semicolon)) = node.statement.content.as_ref() {
            return Some(IN10Args {
                loop_keyword_range: node.fortok.range(),
                semicolon_range: semicolon.range(),
                expected_depth: depth + 1
            });
            
        }
        return None;
    }

    pub fn from_while_content(node: &WhileContent, depth: u32) -> Option<IN10Args> {
        if let Content::Some(statement::StatementContent::Empty(semicolon)) = node.statement.content.as_ref() {
            return Some(IN10Args {
                loop_keyword_range: node.whiletok.range(),
                semicolon_range: semicolon.range(),
                expected_depth: depth + 1
            });
            
        }
        return None;
    }
}

impl IN10Rule {
    pub fn from_options(options: &Option<IN10Options>) -> IN10Rule {
        match options {
            Some(options) => IN10Rule {
                enabled: true,
                indentation_spaces: options.indentation_spaces
            },
            None => IN10Rule {
                enabled: false,
                indentation_spaces: 0
            }
        }
    }
    pub fn check(&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IN10Args>)
    {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if self.indentation_is_not_aligned(args.semicolon_range, args.expected_depth) ||
            args.loop_keyword_range.row_start == args.semicolon_range.row_start {
            let dmlerror = DMLStyleError {
                error: LocalDMLError {
                    range: Range::combine(args.loop_keyword_range, args.semicolon_range),
                    description: Self::description().to_string(),
                },
                rule_type: Self::get_rule_type(),
            };
            acc.push(dmlerror);
        }
    }
    fn indentation_is_not_aligned(&self, member_range: ZeroRange, depth: u32) -> bool {
        let expected_column = self.indentation_spaces * depth;
        member_range.col_start.0 != expected_column
    }
}

impl Rule for IN10Rule {
    fn name() -> &'static str {
        "IN10_INDENTATION_EMPTY_LOOP"
    }
    fn description() -> &'static str {
        "When the body of a while or for loop is left empty, \
        indent the semicolon to the appropriate statement level"
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN10
    }
}
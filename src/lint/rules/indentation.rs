use std::convert::TryInto;

use crate::analysis::parsing::{statement::{self, CompoundContent, SwitchCase},
                               structure::ObjectStatementsContent,
                               types::{LayoutContent, StructTypeContent}};
use crate::span::{Range, ZeroIndexed, Row, Column};
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
    if let Some(in6) = &mut cfg.in6 {
        in6.indentation_spaces = indentation_spaces;
    }
    if let Some(in9) = &mut cfg.in9 {
        in9.indentation_spaces = indentation_spaces;
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

pub struct IN3Args<'a> {
    members_ranges: Vec<ZeroRange>,
    lbrace: ZeroRange,
    rbrace: ZeroRange,
    expected_depth: &'a mut u32,
}

impl IN3Args<'_> {
    pub fn from_obj_stmts_content<'a>(node: &ObjectStatementsContent, depth: &'a mut u32) -> Option<IN3Args<'a>> {
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
    pub fn from_struct_type_content<'a>(node: &StructTypeContent, depth: &'a mut u32) -> Option<IN3Args<'a>> {
        Some(IN3Args {
            members_ranges: node.members.iter().map(|m| m.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_compound_content<'a>(node: &CompoundContent, depth: &'a mut u32) -> Option<IN3Args<'a>> {
        Some(IN3Args {
            members_ranges: node.statements.iter().map(|s| s.range()).collect(),
            lbrace: node.lbrace.range(),
            rbrace: node.rbrace.range(),
            expected_depth: depth,
        })
    }
    pub fn from_layout_content<'a>(node: &LayoutContent, depth: &'a mut u32) -> Option<IN3Args<'a>> {
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
    pub fn check<'a>(&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IN3Args<'a>>)
    {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if args.lbrace.row_start == args.rbrace.row_start ||
            args.lbrace.row_start == args.members_ranges[0].row_start { return; }
        *args.expected_depth += 1;
        for member_range in args.members_ranges {
            if self.indentation_is_not_aligned(member_range, *args.expected_depth) {
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

// IN6: Continuation Line
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct IN6Options {
    #[serde(default = "default_indentation_spaces")]
    pub indentation_spaces: u32,
}

pub struct IN6Rule {
    pub enabled: bool,
    pub indentation_spaces: u32,
}

impl IN6Rule {
    pub fn from_options(options: &Option<IN6Options>) -> IN6Rule {
        match options {
            Some(in6) => IN6Rule {
                enabled: true,
                indentation_spaces: in6.indentation_spaces,
            },
            None => IN6Rule {
                enabled: false,
                indentation_spaces: 0,
            },
        }
    }

    pub fn check(&self, acc: &mut Vec<DMLStyleError>, lines: &[&str]) {
        if !self.enabled {
            return;
        }

        let arithmetic_operators = ["+", "-", "*", "/", "%", "="];
        let comparison_operators = ["==", "!=", "<", ">", "<=", ">="];
        let logical_operators = ["&&", "||"];
        let bitwise_operators = ["&", "|", "<<", ">>"];

        let operators = [
            &arithmetic_operators[..],
            &comparison_operators[..],
            &logical_operators[..],
            &bitwise_operators[..],
        ];

        for (i, line) in lines.iter().enumerate() {
            if let Some(last_char) = line.trim().chars().last() {
                if operators.iter().any(|ops| ops.contains(&last_char.to_string().as_str())) {
                    let next_line = lines.get(i + 1);
                    if let Some(next_line) = next_line {
                        let expected_indent = line.chars().take_while(|c| c.is_whitespace()).count() + self.indentation_spaces as usize;
                        let actual_indent = next_line.chars().take_while(|c| c.is_whitespace()).count();
                        if actual_indent != expected_indent {
                            let msg = IN6Rule::description().to_owned();
                            let dmlerror = DMLStyleError {
                                error: LocalDMLError {
                                    range: Range::new(
                                        Row::new_zero_indexed((i + 1) as u32),
                                        Row::new_zero_indexed((i + 1) as u32),
                                        Column::new_zero_indexed(0),
                                        Column::new_zero_indexed(next_line.len() as u32)
                                    ),
                                    description: msg,
                                },
                                rule_type: Self::get_rule_type(),
                            };
                            acc.push(dmlerror);
                        }
                    }
                }
            }
        }
    }
}

impl Rule for IN6Rule {
    fn name() -> &'static str {
        "IN6_CONTINUATION_LINE"
    }

    fn description() -> &'static str {
        "Continuation line not indented correctly."
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN6
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

pub struct IN9Args<'a> {
    case_range: ZeroRange,
    expected_depth: &'a mut u32,
}

impl IN9Args<'_> {
    pub fn from_switch_case<'a>(node: &SwitchCase, depth: &'a mut u32) -> Option<IN9Args<'a>> {
        match node {
            SwitchCase::Case(_, _, _) |
            SwitchCase::Default(_, _) => {},
            SwitchCase::Statement(statement) => {
                if let Content::Some(ref content) = *statement.content {
                    if let statement::StatementContent::Compound(_) = content {
                        return None;
                    }
                    *depth += 1;
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
    pub fn check<'a>(&self, acc: &mut Vec<DMLStyleError>,
        args: Option<IN9Args<'a>>)
    {
        if !self.enabled { return; }
        let Some(args) = args else { return; };
        if self.indentation_is_not_aligned(args.case_range, *args.expected_depth) {
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
        "Case labels are indented one level less than surrounding lines, \
         so that they are on the same level as the switch statement"
    }
    fn get_rule_type() -> RuleType {
        RuleType::IN9
    }
}

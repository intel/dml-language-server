pub mod spacing;
pub mod indentation;

#[cfg(test)]
pub mod tests;

use spacing::{SpBracesRule,
    SpPunctRule, NspFunparRule, NspInparenRule,
    NspUnaryRule, NspTrailingRule};
use indentation::{LongLinesRule, IdentNoTabRule, IndentCodeBlockRule, IndentClosingBraceRule, IndentParenExprRule, IndentSwitchCaseRule, IndentEmptyLoopRule};
use crate::lint::{LintCfg, DMLStyleError};
use crate::analysis::{LocalDMLError, parsing::tree::ZeroRange};

pub struct CurrentRules {
    pub sp_brace: SpBracesRule,
    pub sp_punct: SpPunctRule,
    pub nsp_funpar: NspFunparRule,
    pub nsp_inparen: NspInparenRule,
    pub nsp_unary: NspUnaryRule,
    pub nsp_trailing: NspTrailingRule,
    pub long_lines: LongLinesRule,
    pub indent_no_tabs: IdentNoTabRule,
    pub indent_code_block: IndentCodeBlockRule,
    pub indent_closing_brace: IndentClosingBraceRule,
    pub indent_paren_expr: IndentParenExprRule,
    pub indent_switch_case: IndentSwitchCaseRule,
    pub indent_empty_loop: IndentEmptyLoopRule
}

pub fn  instantiate_rules(cfg: &LintCfg) -> CurrentRules {
    CurrentRules {
        sp_brace: SpBracesRule { enabled: cfg.sp_brace.is_some() },
        sp_punct: SpPunctRule { enabled: cfg.sp_punct.is_some() },
        nsp_funpar: NspFunparRule { enabled: cfg.nsp_funpar.is_some() },
        nsp_inparen: NspInparenRule { enabled: cfg.nsp_inparen.is_some() },
        nsp_unary: NspUnaryRule { enabled: cfg.nsp_unary.is_some() },
        nsp_trailing: NspTrailingRule { enabled: cfg.nsp_trailing.is_some() },
        long_lines: LongLinesRule::from_options(&cfg.long_lines),
        indent_no_tabs: IdentNoTabRule { enabled: cfg.indent_no_tabs.is_some() },
        indent_code_block: IndentCodeBlockRule::from_options(&cfg.indent_code_block),
        indent_closing_brace: IndentClosingBraceRule::from_options(&cfg.indent_closing_brace),
        indent_paren_expr: IndentParenExprRule { enabled: cfg.indent_paren_expr.is_some() },
        indent_switch_case: IndentSwitchCaseRule::from_options(&cfg.indent_switch_case),
        indent_empty_loop: IndentEmptyLoopRule::from_options(&cfg.indent_empty_loop)
    }
}

// struct/trait generic_rule
pub trait Rule {
    fn name() -> &'static str;
    fn description() -> &'static str;
    fn get_rule_type() -> RuleType;
    fn push_err(&self, acc: &mut Vec<DMLStyleError>, range: ZeroRange) {
        let dmlerror = DMLStyleError {
            error: LocalDMLError {
                range,
                description: Self::description().to_string(),
            },
            rule_type: Self::get_rule_type(),
        };
        acc.push(dmlerror);
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum RuleType {
    SpBraces,
    SpPunct,
    NspFunpar,
    NspInparen,
    NspUnary,
    NspTrailing,
    LL1,
    IN2,
    IN3,
    IN4,
    IN5,
    IN6,
    IN9,
    IN10
}


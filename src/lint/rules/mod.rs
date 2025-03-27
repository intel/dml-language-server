pub mod spacing;
pub mod indentation;

#[cfg(test)]
pub mod tests;

use spacing::{SpBracesRule,
    SpPunctRule, NspFunparRule, NspInparenRule,
    NspUnaryRule, NspTrailingRule};
use indentation::{LongLinesRule, IN2Rule, IN3Rule, IN4Rule, IN9Rule, IN10Rule};
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
    pub in2: IN2Rule,
    pub in3: IN3Rule,
    pub in4: IN4Rule,
    pub in9: IN9Rule,
    pub in10: IN10Rule
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
        in2: IN2Rule { enabled: cfg.in2.is_some() },
        in3: IN3Rule::from_options(&cfg.in3),
        in4: IN4Rule::from_options(&cfg.in4),
        in9: IN9Rule::from_options(&cfg.in9),
        in10: IN10Rule::from_options(&cfg.in10)
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
                range: range,
                description: Self::description().to_string(),
            },
            rule_type: Self::get_rule_type(),
        };
        acc.push(dmlerror);
    }
}

#[derive(PartialEq)]
pub enum RuleType {
    SpBraces,
    SpPunct,
    NspFunpar,
    NspInparen,
    NspUnary,
    NspTrailing,
    LongLines,
    IN2,
    IN3,
    IN4,
    IN6,
    IN9,
    IN10
}


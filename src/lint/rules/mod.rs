pub mod spacing;
pub mod indentation;

#[cfg(test)]
pub mod tests;

use spacing::{SpBracesRule,
    SpPunctRule, NspFunparRule, NspInparenRule,
    NspUnaryRule, NspTrailingRule};
use indentation::{LongLinesRule, IN2Rule, IN3Rule, IN6Rule, IN9Rule};
use crate::lint::LintCfg;

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
    pub in6: IN6Rule,
    pub in9: IN9Rule
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
        in6: IN6Rule::from_options(&cfg.in6),
        in9: IN9Rule::from_options(&cfg.in9),
    }
}

// struct/trait generic_rule
pub trait Rule {
    fn name() -> &'static str;
    fn description() -> &'static str;
    fn get_rule_type() -> RuleType;
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
    IN6,
    IN9
}

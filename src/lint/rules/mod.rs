//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
pub mod spacing;
pub mod indentation;

#[cfg(test)]
pub mod tests;

use spacing::{NspFunparRule,
              NspInparenRule,
              NspTrailingRule,
              NspUnaryRule,
              SpBracesRule,
              SpBinopRule,
              SpTernaryRule,
              SpPtrDeclRule,
              NspPtrDeclRule,
              SpPunctRule,
              SpReservedRule};
use indentation::{LongLinesRule, IndentNoTabRule, IndentCodeBlockRule, IndentClosingBraceRule, IndentParenExprRule, IndentSwitchCaseRule, IndentEmptyLoopRule};
use crate::lint::{LintCfg, DMLStyleError};
use crate::analysis::{LocalDMLError, parsing::tree::ZeroRange};

pub struct CurrentRules {
    pub sp_reserved: SpReservedRule,
    pub sp_brace: SpBracesRule,
    pub sp_punct: SpPunctRule,
    pub sp_binop: SpBinopRule,
    pub sp_ternary: SpTernaryRule,
    pub sp_ptrdecl: SpPtrDeclRule,
    pub nsp_ptrdecl: NspPtrDeclRule,
    pub nsp_funpar: NspFunparRule,
    pub nsp_inparen: NspInparenRule,
    pub nsp_unary: NspUnaryRule,
    pub nsp_trailing: NspTrailingRule,
    pub long_lines: LongLinesRule,
    pub indent_no_tabs: IndentNoTabRule,
    pub indent_code_block: IndentCodeBlockRule,
    pub indent_closing_brace: IndentClosingBraceRule,
    pub indent_paren_expr: IndentParenExprRule,
    pub indent_switch_case: IndentSwitchCaseRule,
    pub indent_empty_loop: IndentEmptyLoopRule
}

pub fn  instantiate_rules(cfg: &LintCfg) -> CurrentRules {
    CurrentRules {
        sp_reserved: SpReservedRule { enabled: cfg.sp_reserved.is_some() },
        sp_brace: SpBracesRule { enabled: cfg.sp_brace.is_some() },
        sp_punct: SpPunctRule { enabled: cfg.sp_punct.is_some() },
        sp_binop: SpBinopRule { enabled: cfg.sp_binop.is_some() },
        sp_ternary: SpTernaryRule { enabled: cfg.sp_ternary.is_some() },
        sp_ptrdecl: SpPtrDeclRule { enabled: cfg.sp_ptrdecl.is_some() },
        nsp_ptrdecl: NspPtrDeclRule { enabled: cfg.nsp_ptrdecl.is_some() },
        nsp_funpar: NspFunparRule { enabled: cfg.nsp_funpar.is_some() },
        nsp_inparen: NspInparenRule { enabled: cfg.nsp_inparen.is_some() },
        nsp_unary: NspUnaryRule { enabled: cfg.nsp_unary.is_some() },
        nsp_trailing: NspTrailingRule { enabled: cfg.nsp_trailing.is_some() },
        long_lines: LongLinesRule::from_options(&cfg.long_lines),
        indent_no_tabs: IndentNoTabRule { enabled: cfg.indent_no_tabs.is_some() },
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
    fn create_err(&self, range: ZeroRange) -> DMLStyleError {
        DMLStyleError {
            error: LocalDMLError {
                range,
                description: Self::description().to_string(),
            },
            rule_ident: Self::name(),
            rule_type: Self::get_rule_type(),
        }
    }
}

#[derive(Copy, PartialEq, Debug, Clone, Eq, Hash)]
pub enum RuleType {
    SpReserved,
    SpBraces,
    SpPunct,
    SpBinop,
    SpTernary,
    SpPtrDecl,
    NspPtrDecl,
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
    IN10,
    Configuration,
}

impl std::str::FromStr for RuleType {
    // More descriptive error than this not needed
    type Err = ();
    fn from_str(val: &str) -> Result<RuleType, ()> {
        macro_rules! get_rule_type {
            ($v: expr, $rule_class: ty) => {
                if <$rule_class>::name() == $v {
                    Ok(<$rule_class>::get_rule_type())
                } else {
                    Err(())
                }
            };
            ($v: expr, $rule_class: ty, $($remain_classes:ty),+) => {
                if <$rule_class>::name() == $v {
                    Ok(<$rule_class>::get_rule_type())
                } else {
                    get_rule_type!($v, $($remain_classes),+)
                }
            };
        }
        get_rule_type!(val,
                       LongLinesRule,
                       IndentNoTabRule,
                       IndentCodeBlockRule,
                       IndentClosingBraceRule,
                       IndentParenExprRule,
                       IndentSwitchCaseRule,
                       IndentEmptyLoopRule,
                       SpBracesRule,
                       SpPunctRule,
                       NspFunparRule,
                       NspInparenRule,
                       NspUnaryRule,
                       NspTrailingRule)
    }
}

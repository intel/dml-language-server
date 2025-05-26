use crate::lint::{begin_style_check, DMLStyleError, LintCfg};
use crate::lint::rules::{instantiate_rules, CurrentRules, RuleType};
use crate::lint::tests::create_ast_from_snippet;
use crate::analysis::ZeroRange;
use crate::vfs::Error;
use std::collections::HashSet;

#[derive(Debug)]
pub struct ExpectedDMLStyleError {
    pub range: ZeroRange,
    pub rule_type: RuleType,
}

// Each test expects errors from a single RuleType
// A list of tuples is used to define the ZeroRange location
// of each expected error
#[macro_export]
macro_rules! define_expected_errors {
    ($rule_type:expr, $(($start_line:expr, $end_line:expr, $start_col:expr, $end_col:expr)),* $(,)?) => {
        vec![
            $(
                $crate::lint::rules::tests::common::ExpectedDMLStyleError {
                    range: $crate::analysis::ZeroRange::from_u32($start_line, $end_line, $start_col, $end_col),
                    rule_type: $rule_type,
                }
            ),*
        ]
    };
}

pub fn run_linter(source_code: &str, rules: &CurrentRules)
    -> Result<Vec<DMLStyleError>, Error>
{
    print!("\nSnippet to test on:\n{}\n", source_code);
    let ast = create_ast_from_snippet(source_code);
    print!("Resulting AST:\n{:#?}\n", ast);
    begin_style_check(ast, source_code.to_string(), rules)
}

pub fn assert_snippet(source_code: &str, expected_errors: usize, rules: &CurrentRules) {
    let lint_errors = run_linter(source_code, rules);
    assert!(lint_errors.is_ok());
    assert_eq!(lint_errors.clone().unwrap().len(), expected_errors,
               "{:#?}", lint_errors);
}

pub fn robust_assert_snippet(source_code: &str, expected_errors: Vec<ExpectedDMLStyleError>, rules: &CurrentRules) {
    let lint_errors = run_linter(source_code, rules).unwrap();
    assert_eq!(lint_errors.len(), expected_errors.len(), "{:#?}", lint_errors);

    let actual_errors: HashSet<_> = lint_errors.iter().map(|e| (e.error.range, e.rule_type.clone())).collect();
    let expected_errors: HashSet<_> = expected_errors.iter().map(|e| (e.range, e.rule_type.clone())).collect();

    assert_eq!(actual_errors, expected_errors, "Mismatch between actual and expected errors:\nActual: {:#?}\nExpected: {:#?}", actual_errors, expected_errors);
}

pub fn set_up() -> CurrentRules {
    let cfg = LintCfg::default();
    instantiate_rules(&cfg)
}

pub mod in2;
pub mod in3;
pub mod in6;
pub mod in9;

use crate::lint::rules::tests::common::{assert_snippet, run_linter};
use crate::lint::LintCfg;
use crate::lint::LongLineOptions;
use crate::lint::rules::{CurrentRules, instantiate_rules};
use crate::analysis::LocalDMLError;
use std::convert::TryInto;

pub fn assert_indentation(
    code: &str, expected_errors: usize, rules: CurrentRules)
{
    let lint_errors = run_linter(code, &rules);
    let Ok(ref lint_errors) = lint_errors else {
        panic!();
    };
    let mut indent_errors: Vec<&LocalDMLError> = vec!();
    for error in lint_errors {
        indent_errors.push(error);
    }
    assert_eq!(indent_errors.len(), expected_errors, "{:#?}", lint_errors);
}

//  Line length can be configured to a maximum
//(defaults to 80, feature disabled)
pub static LONG_LINE: &str = "
param some_parameter_name_in_this_device = some_long_name_bank.some_long_name_group.SOME_REGISTER_NAME;
";
#[test]
fn style_check_long_line() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    assert_snippet(LONG_LINE, 1, &rules);
    // Test rule disable
    cfg.long_lines = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(LONG_LINE, 0, &rules);
    // Test lower max_length
    cfg.long_lines = Some(LongLineOptions{
        max_length: (LONG_LINE.len()-3).try_into().unwrap()
    });
    rules = instantiate_rules(&cfg);
    assert_snippet(LONG_LINE, 1, &rules);
}

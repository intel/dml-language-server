//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
mod no_tabs;
mod code_block;
mod closing_brace;
mod paren_expr;
mod switch_case;
mod empty_loop;
mod continuation_line;

use crate::lint::rules::tests::common::assert_snippet;
use crate::lint::rules::RuleType;
use crate::lint::LintCfg;
use crate::lint::LongLineOptions;
use crate::lint::rules::instantiate_rules;
use std::convert::TryInto;

//  Line length can be configured to a maximum
//(defaults to 80, feature disabled)
static LINE_LENGTH_INCORRECT: &str = "
param some_parameter_name_in_this_device = some_long_name_bank.some_long_name_group.SOME_REGISTER_NAME;
";
#[test]
fn line_length_incorrect() {
    let mut cfg = LintCfg::default();
    let mut rules = instantiate_rules(&cfg);
    let expected_errors = define_expected_errors!(
        RuleType::LL1,
        (1, 1, 80, 103),
    );
    assert_snippet(LINE_LENGTH_INCORRECT, expected_errors, &rules);
    // Test rule disable
    cfg.long_lines = None;
    rules = instantiate_rules(&cfg);
    assert_snippet(LINE_LENGTH_INCORRECT, vec![], &rules);
    // Test lower max_length
    cfg.long_lines = Some(LongLineOptions{
        max_length: (LINE_LENGTH_INCORRECT.len()-3).try_into().unwrap()
    });
    rules = instantiate_rules(&cfg);
    let expected_errors = define_expected_errors!(
        RuleType::LL1,
        (1, 1, 102, 103),
    );
    assert_snippet(LINE_LENGTH_INCORRECT, expected_errors, &rules);
}

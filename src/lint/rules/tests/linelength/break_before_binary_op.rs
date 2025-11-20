//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

static PARAM_ASSIGN_LOGIC_OP_CORRECT: &str = "
param LOGIC_RESULT = initial_condition
    | second_condition;
";

#[test]
fn param_assign_logic_op_correct() {
    let rules = set_up();
    let expected_errors = vec![];
    assert_snippet(PARAM_ASSIGN_LOGIC_OP_CORRECT, expected_errors, &rules);
}

static PARAM_ASSIGN_LOGIC_OP_INCORRECT: &str = "
param LOGIC_RESULT = initial_condition |
    second_condition;
";

#[test]
fn param_assign_logic_op_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL2,
        (1, 1, 39, 40),
    );
    assert_snippet(PARAM_ASSIGN_LOGIC_OP_INCORRECT, expected_errors, &rules);
}

static ASSIGN_ARITHMETIC_OP_CORRECT: &str = "
session int variable
    = initial_condition
    + second_condition;
";

#[test]
fn assign_arithmetic_op_correct() {
    let rules = set_up();
    let expected_errors = vec![];
    assert_snippet(ASSIGN_ARITHMETIC_OP_CORRECT, expected_errors, &rules);
}

static ASSIGN_ARITHMETIC_OP_INCORRECT: &str = "
session int variable =
    initial_value +
    second_condition;
";

#[test]
fn assign_arithmetic_op_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL2,
        (2, 2, 18, 19),
    );
    assert_snippet(ASSIGN_ARITHMETIC_OP_INCORRECT, expected_errors, &rules);
}
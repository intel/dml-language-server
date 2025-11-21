//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

// LL6: Function and method invocations can be broken after the opening parenthesis,
//  with the continuation lines indented one level.

static AFTER_PAREN_EXCEPTION_CORRECT: &str = "
method effect() {
    callback(
        0xABC,
        identifier,
        false);
}
";
#[test]
fn after_paren_exception_correct() {
    let rules = set_up();
    assert_snippet(AFTER_PAREN_EXCEPTION_CORRECT, vec![], &rules);
}

static AFTER_PAREN_EXCEPTION_INCORRECT: &str = "
method effect() {
    callback(
        0xABC,
            identifier,
    false);
}
";
#[test]
fn after_paren_exception_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL6,
        (4, 4, 12, 22),
        (5, 5, 4, 9),
    );
    assert_snippet(AFTER_PAREN_EXCEPTION_INCORRECT, expected_errors, &rules);
}

static FIRST_LINE_INCORRECT: &str = "
method effect() {
    callback(
               0xABC,
        identifier,
        false);
}
";
#[test]
fn first_line_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL6,
        (3, 3, 15, 20)
    );
    assert_snippet(FIRST_LINE_INCORRECT, expected_errors, &rules);
}

static NESTED_PAREN_CORRECT: &str = "
param result = (
    (reg0.val
     * reg1.enable.val)
    & mask_reg
    + 1);
";

#[test]
fn nested_paren_correct(){
    let rules = set_up();
    assert_snippet(NESTED_PAREN_CORRECT, vec![], &rules);
}

static NESTED_PAREN_INCORRECT: &str = "
param result = (
    (reg0.val
     * reg1.enable.val)
                & mask_reg
                + 1);
";

#[test]
fn nested_paren_incorrect(){
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL6,
        (4, 4, 16, 17),
        (5, 5, 16, 17),
    );
    assert_snippet(NESTED_PAREN_INCORRECT, expected_errors, &rules);
}

static METHOD_CORRECT: &str = "
method some_method(
    int a,
    int b, bool c) {
    return c ? a + b : a - b;
}
";
#[test]
fn method_correct() {
    let rules = set_up();
    assert_snippet(METHOD_CORRECT, vec![], &rules);
}

static METHOD_INCORRECT: &str = "
method some_method(
    int a,
    int b,
        bool c) {
    return c ? a + b : a - b;
}
";
#[test]
fn method_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL6,
        (4, 4, 8, 12),
    );
    assert_snippet(METHOD_INCORRECT, expected_errors, &rules);
}

static CAST_CORRECT: &str = "
method example_cast() {
    local uint64 original_value = 42;
    local uint32 casted_value;

    casted_value = cast(original_value, uint32);

    log info: casted_value;
}
";
#[test]
fn cast_correct() {
    let rules = set_up();
    assert_snippet(CAST_CORRECT, vec![], &rules);
}

static CAST_INCORRECT: &str = "
method example_cast() {
    local uint64 original_value = 42;
    local uint32 casted_value;

    casted_value = cast(
            original_value,
        uint32);

    log info: casted_value;
}
";
#[test]
fn cast_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL6,
        (6, 6, 12, 26),
    );
    assert_snippet(CAST_INCORRECT, expected_errors, &rules);
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

// LL5: Break long method declarations with output parameters before the arrow.

static OUTPUT_CORRECT: &str = "
method inquiry_status(uint64 physical_address)
        -> (uint16 status) {
    return 0;
}

method inquiry_status(uint64 physical_address)
    -> (uint16 status) {
    return 0;
}

method inquiry_status(uint64 physical_address) -> (uint16 status) {
    return 0;
}

method other_method(uint64 arg1,
                    uint64 arg2)
        -> (uint16 status) {
    return 0;
}
";
#[test]
fn output_correct() {
    let rules = set_up();
    assert_snippet(OUTPUT_CORRECT, vec![], &rules);
}

static OUTPUT_INCORRECT: &str = "
method inquiry_status(uint64 physical_address) ->
    (uint16 status) {
    return 0;
}

method inquiry_status(uint64 physical_address) ->
        (uint16 status) {
    return 0;
}

method other_method(uint64 arg1,
                    uint64 arg2) ->
        (uint16 status) {
    return 0;
}
";
#[test]
fn output_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL5,
        (1, 1, 47, 49),
        (6, 6, 47, 49),
        (12, 12, 33, 35),
    );
    assert_snippet(OUTPUT_INCORRECT, expected_errors, &rules);
}
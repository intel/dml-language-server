//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

static USING_TAB_INDENT_INCORRECT: &str = "
bank BankA {
    group GroupB {
        param some_param = this.REG_C;
        register REG_C[osdml_reg_idx < ...] is (some_template) {
            param other_param = 0;
            field Field_D {
            	is osdml_write;
    		    method write_action(uint64 value) {
                    if (value == 1) {
                        log info, 3: \"Writing Field_D\";
                    }
                }
    	    }
            method is_even(int value) -> (uint32) {
                if (value % 2 == 0) {
                    return true;
	    	    } else
                    return false;
            }
        }
    }
}
";
#[test]
fn using_tab_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN2,
        (7, 7, 12, 13),
        (8, 8, 4, 5),
        (8, 8, 5, 6),
        (13, 13, 4, 5),
        (17, 17, 0, 1),
        (17, 17, 5, 6),
    );
    assert_snippet(USING_TAB_INDENT_INCORRECT, expected_errors, &rules);
}

static USING_SPACE_INDENT_CORRECT: &str = "
bank BankA {
    group GroupB {
        param some_param = this.REG_C;
        register REG_C[osdml_reg_idx < ...] is (some_template) {
            param other_param = 0;
            field Field_D {
                is osdml_write;
                method write_action(uint64 value) {
                    if (value == 1) {
                        log info, 3: \"Writing Field_D\";
                    }
                }
            }
            method is_even(int value) -> (uint32) {
                if (value % 2 == 0) {
                    return true;
                } else
                    return false;
            }
        }
    }
}
";
#[test]
fn using_space_indent_correct() {
    let rules = set_up();
    assert_snippet(USING_SPACE_INDENT_CORRECT, vec![], &rules);
}

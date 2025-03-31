use crate::lint::rules::tests::common::set_up;
use crate::lint::rules::tests::indentation::assert_snippet;

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
    assert_snippet(USING_TAB_INDENT_INCORRECT, 6, &rules);
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
    assert_snippet(USING_SPACE_INDENT_CORRECT, 0, &rules);
}
use crate::lint::rules::tests::common::set_up;
use crate::lint::rules::tests::indentation::assert_indentation;

pub static IN2_USING_TAB_INDENT: &str = "
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
fn in2_using_tab_indent() {
    let rules = set_up();
    assert_indentation(IN2_USING_TAB_INDENT, 6, rules);
}

pub static IN2_USING_SPACE_INDENT: &str = "
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
fn in2_using_space_indent() {
    let rules = set_up();
    assert_indentation(IN2_USING_SPACE_INDENT, 0, rules);
}
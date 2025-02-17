use crate::lint::rules::tests::common::set_up;
use crate::lint::rules::tests::indentation::assert_indentation;

pub static IN3_FUNCTION_CONTENTS_CORRECT_INDENT: &str = "
method some_function(int a) {
    return 0;
}
";
#[test]
fn in3_function_contents_should_indent() {
    let rules = set_up();
    assert_indentation(IN3_FUNCTION_CONTENTS_CORRECT_INDENT, 0, rules);
}

pub static IN3_ONE_LINE_NO_INDENT: &str = "
method some_function(int a) { return 0; }
";
#[test]
fn in3_one_line_no_indent() {
    let rules = set_up();
    assert_indentation(IN3_ONE_LINE_NO_INDENT, 0, rules);
}

pub static IN3_FUNCTION_CONTENTS_NO_INDENT: &str = "
method some_function(int a) {
return a;
}
";
#[test]
fn in3_function_contents_no_indent() {
    let rules = set_up();
    assert_indentation(IN3_FUNCTION_CONTENTS_NO_INDENT, 1, rules);
}

pub static IN3_FUNCTION_PARAMS_BREAKED_AND_NO_INDENT: &str = "
method some_function(int a,
                     int b) {
return a;
}
";
#[test]
fn in3_function_params_breaked_and_no_indent() {
    let rules = set_up();
    assert_indentation(IN3_FUNCTION_PARAMS_BREAKED_AND_NO_INDENT, 1, rules);
}

pub static IN3_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT: &str = "
method some_function(int a,
    int b)
{
return a;
}
";
#[test]
fn in3_function_params_badly_breaked_and_no_indent() {
    let rules = set_up();
    assert_indentation(IN3_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT, 1, rules);
}

pub static IN3_INLINE_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT: &str = "
inline method some_function(int a,
    int b)
{
return a;
}
";
#[test]
fn in3_inline_function_params_badly_breaked_and_no_indent() {
    let rules = set_up();
    assert_indentation(IN3_INLINE_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT, 1, rules);
}

pub static IN3_FULL_BANK_CORRECT_INDENT: &str = "
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
fn in3_full_bank_correct_indent() {
    let rules = set_up();
    assert_indentation(IN3_FULL_BANK_CORRECT_INDENT, 0, rules);
}

pub static IN3_STRUCTS_BAD_INDENT: &str = "
typedef struct {
     uint16 idx;
       uint16 qid_co;
} hqm_cq_list_release_ctx_t;

typedef layout \"little-endian\" {
     bitfields 8 {
        uint2 rsvd             @ [7:6];
        uint1 error_f          @ [5:5];
        uint1 int_arm          @ [4:4];
        uint1 qe_valid         @ [3:3];
        uint1 qe_frag          @ [2:2];
        uint1 qe_comp          @ [1:1];
        uint1 cq_token         @ [0:0];
    } byte;
} prod_qe_cmd_t;
";
#[test]
fn in3_structs_bad_indent() {
    let rules = set_up();
    assert_indentation(IN3_STRUCTS_BAD_INDENT, 3, rules);
}

pub static IN3_COND_STRUCTURE_BAD_INDENT: &str = "
method control_device() {
    if (control.start == 1) {
    log info, 2: 'Starting the device';
            status.enabled = 1;
    } else if (control.stop == 1) {
                log info, 2: 'Stopping the device';
        status.enabled = 0;
    } else {
    log info, 2: 'No control action taken';
    }
}
";

#[test]
fn in3_cond_structure_bad_indent() {
    let rules = set_up();
    assert_indentation(IN3_COND_STRUCTURE_BAD_INDENT, 4, rules);
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

static FUNCTION_CONTENTS_INDENT_CORRECT: &str = "
method some_function(int a) {
    return 0;
}
";
#[test]
fn function_contents_indent_correct() {
    let rules = set_up();
    assert_snippet(FUNCTION_CONTENTS_INDENT_CORRECT, vec![], &rules);
}

static ONE_LINE_NO_INDENT_CORRECT: &str = "
method some_function(int a) { return 0; }
";
#[test]
fn one_line_no_indent_correct() {
    let rules = set_up();
    assert_snippet(ONE_LINE_NO_INDENT_CORRECT, vec![], &rules);
}

static FUNCTION_CONTENTS_INDENT_INCORRECT: &str = "
method some_function(int a) {
return a;
}
";
#[test]
fn function_contents_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (2, 2, 0, 9),
    );
    assert_snippet(FUNCTION_CONTENTS_INDENT_INCORRECT, expected_errors, &rules);
}

static FUNCTION_PARAMS_BROKEN_AND_NO_INDENT: &str = "
method some_function(int a,
                     int b) {
return a;
}
";
#[test]
fn function_params_broken_and_no_indent() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (3, 3, 0, 9),
    );
    assert_snippet(FUNCTION_PARAMS_BROKEN_AND_NO_INDENT, expected_errors, &rules);
}

static FUNCTION_PARAMS_BADLY_BROKEN_AND_NO_INDENT: &str = "
method some_function(int a,
                     int b)
{
return a;
}
";
#[test]
fn function_params_badly_broken_and_no_indent() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (4, 4, 0, 9),
    );
    assert_snippet(FUNCTION_PARAMS_BADLY_BROKEN_AND_NO_INDENT, expected_errors, &rules);
}

static INLINE_FUNCTION_PARAMS_BADLY_BROKEN_AND_NO_INDENT: &str = "
inline method some_function(int a,
                            int b)
{
return a;
}
";
#[test]
fn inline_function_params_badly_broken_and_no_indent() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (4, 4, 0, 9),
    );
    assert_snippet(INLINE_FUNCTION_PARAMS_BADLY_BROKEN_AND_NO_INDENT, expected_errors, &rules);
}

static FULL_BANK_INDENT_CORRECT: &str = "
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
fn full_bank_indent_correct() {
    let rules = set_up();
    assert_snippet(FULL_BANK_INDENT_CORRECT, vec![], &rules);
}

static STRUCTS_INDENT_CORRECT: &str = "
typedef struct {
    uint16 idx;
    uint8 qid_co;
    uint8 pid;
} cq_list_release_ctx_t;

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
fn structs_indent_correct() {
    let rules = set_up();
    assert_snippet(STRUCTS_INDENT_CORRECT, vec![], &rules);
}

static STRUCTS_INDENT_INCORRECT: &str = "
typedef struct {
     uint16 idx;
       uint8 qid_co;
    uint8 pid;
} cq_list_release_ctx_t;

typedef layout \"little-endian\" {
     bitfields 8 {
        uint2 rsvd             @ [7:6];
        uint1 error_f          @ [5:5];
          uint1 int_arm        @ [4:4];
          uint1 qe_valid       @ [3:3];
        uint1 qe_frag          @ [2:2];
        uint1 qe_comp          @ [1:1];
        uint1 cq_token         @ [0:0];
    } byte;
} prod_qe_cmd_t;
";
#[test]
fn structs_bad_indent() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (2, 2, 5, 16),
        (3, 3, 7, 20),
        (8, 16, 5, 11),
        (11, 11, 10, 39),
        (12, 12, 10, 39),
    );
    assert_snippet(STRUCTS_INDENT_INCORRECT, expected_errors, &rules);
}

static COND_STRUCTURE_INDENT_INCORRECT: &str = "
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
fn cond_structure_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (3, 3, 4, 39),
        (4, 4, 12, 31),
        (6, 6, 16, 51),
        (9, 9, 4, 43),
    );
    assert_snippet(COND_STRUCTURE_INDENT_INCORRECT, expected_errors, &rules);
}

static EMBEDDED_INDENT_CORRECT: &str = "
bank pcie_config {
    register command {
        field mem {
            method pcie_write(uint64 value) {
                if (value != 0) {
                    value = value + 1;
                    callback();
                }
                default(value);
                map_memory_alt();
            }
        }
    }
}
";
#[test]
fn embedded_indent_correct() {
    let rules = set_up();
    assert_snippet(EMBEDDED_INDENT_CORRECT, vec![], &rules);
}

static EMBEDDED_INDENT_INCORRECT: &str = "
bank pcie_config {
    register command {
          field mem {
            method pcie_write(uint64 value) {
                if (value != 0) {
                value = value + 1;
                    callback();
                }
              default(value);
                map_memory_alt();
            }
        }
    }
}
";
#[test]
fn embedded_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (3, 12, 10, 9),
        (9, 9, 14, 29),
        (6, 6, 16, 34),
    );
    assert_snippet(EMBEDDED_INDENT_INCORRECT, expected_errors, &rules);
}

static TEMPLATE_CORRECT: &str = "
template template_name {
    is some_other_template;

    register some_reg size 8 @ ami_offset_0 + (0x0) {
        is osdml_reg_or_field;
        param some_param = SOME_CONSTANT;
        field some_field @ [63:0] is (some_template) {
            is some_other_template;
            param init_val = 0x200004000100460a;
        }
    }
}
";
#[test]
fn template_correct() {
    let rules = set_up();
    assert_snippet(TEMPLATE_CORRECT, vec![], &rules);
}

static TEMPLATE_INCORRECT: &str = "
template template_name {
      is some_other_template;

   register some_reg size 8 @ ami_offset_0 + (0x0) {
        is osdml_reg_or_field;
         param some_param = SOME_CONSTANT;
        field some_field @ [63:0] is (some_template) {
             is some_other_template;
            param init_val = 0x200004000100460a;
        }
    }
}
";
#[test]
fn template_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (2, 2, 6, 29),
        (4, 11, 3, 5),
        (6, 6, 9, 42),
        (8, 8, 13, 36),
    );
    assert_snippet(TEMPLATE_INCORRECT, expected_errors, &rules);
}

static TOPLEVEL_HASHIF_CORRECT: &str = "
constant FEATURE_SUPPORTED = false;

#if (!FEATURE_SUPPORTED) {
attribute target_sb_port is uint64_attr {}
} #else {
attribute target_sb_fid is uint64_attr {}
}

#if (!FEATURE_SUPPORTED) {
method signal_feature() {
    return 1;
}
} #else {
method signal_feature() { return 0; }
}
";

#[test]
fn toplevel_hashif_correct() {
    let rules = set_up();
    assert_snippet(TOPLEVEL_HASHIF_CORRECT, vec![], &rules);
}

static TOPLEVEL_HASHIF_INCORRECT: &str = "
constant FEATURE_SUPPORTED = false;

#if (!FEATURE_SUPPORTED) {
    attribute target_sb_port is uint64_attr {}
} #else {
    attribute target_sb_fid is uint64_attr {}
}

#if (!FEATURE_SUPPORTED) {
    method signal_feature() {
        return 1;
    }
} #else {
    method signal_feature() { return 0; }
}
";

#[test]
fn toplevel_hashif_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN3,
        (4, 4, 4, 46),
        (6, 6, 4, 45),
        (10, 12, 4, 5),
        (11, 11, 8, 17),
        (14, 14, 4, 41),
    );
    assert_snippet(TOPLEVEL_HASHIF_INCORRECT, expected_errors, &rules);
}
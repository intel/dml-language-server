use crate::lint::rules::tests::common::set_up;
use crate::lint::rules::tests::indentation::assert_snippet;

static FUNCTION_CONTENTS_INDENT_CORRECT: &str = "
method some_function(int a) {
    return 0;
}
";
#[test]
fn function_contents_indent_correct() {
    let rules = set_up();
    assert_snippet(FUNCTION_CONTENTS_INDENT_CORRECT, 0, &rules);
}

static ONE_LINE_NO_INDENT_CORRECT: &str = "
method some_function(int a) { return 0; }
";
#[test]
fn one_line_no_indent_correct() {
    let rules = set_up();
    assert_snippet(ONE_LINE_NO_INDENT_CORRECT, 0, &rules);
}

static FUNCTION_CONTENTS_INDENT_INCORRECT: &str = "
method some_function(int a) {
return a;
}
";
#[test]
fn function_contents_indent_incorrect() {
    let rules = set_up();
    assert_snippet(FUNCTION_CONTENTS_INDENT_INCORRECT, 1, &rules);
}

static FUNCTION_PARAMS_BREAKED_AND_NO_INDENT: &str = "
method some_function(int a,
                     int b) {
return a;
}
";
#[test]
fn function_params_breaked_and_no_indent() {
    let rules = set_up();
    assert_snippet(FUNCTION_PARAMS_BREAKED_AND_NO_INDENT, 1, &rules);
}

static FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT: &str = "
method some_function(int a,
    int b)
{
return a;
}
";
#[test]
fn function_params_badly_breaked_and_no_indent() {
    let rules = set_up();
    assert_snippet(FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT, 1, &rules);
}

static INLINE_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT: &str = "
inline method some_function(int a,
    int b)
{
return a;
}
";
#[test]
fn inline_function_params_badly_breaked_and_no_indent() {
    let rules = set_up();
    assert_snippet(INLINE_FUNCTION_PARAMS_BADLY_BREAKED_AND_NO_INDENT, 1, &rules);
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
    assert_snippet(FULL_BANK_INDENT_CORRECT, 0, &rules);
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
    assert_snippet(STRUCTS_INDENT_CORRECT, 0, &rules);
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
    assert_snippet(STRUCTS_INDENT_INCORRECT, 5, &rules);
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
    assert_snippet(COND_STRUCTURE_INDENT_INCORRECT, 4, &rules);
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
    assert_snippet(EMBEDDED_INDENT_CORRECT, 0, &rules);
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
    assert_snippet(EMBEDDED_INDENT_INCORRECT, 3, &rules);
}
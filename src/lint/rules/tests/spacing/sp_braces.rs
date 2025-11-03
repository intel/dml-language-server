//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  SP.braces around braces ({ and })
static SPACE_BRACES_METHOD_BANK_REGISTER_FIELD_INCORRECT: &str = "
method this_is_some_method() {return 0;}

method this_is_empty_method() { }

bank pcie_config {register command {field mem {
    method pcie_write(uint64 value) {
        if (value != 0) {value = value + 1;}
        default(value);
        map_memory_alt();
    }
}
}
}
";
#[test]
fn space_braces_method_bank_register_field_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpBraces,
        (1, 1, 29, 30),
        (1, 1, 39, 40),
        (5, 5, 17, 18),
        (5, 5, 35, 36),
        (7, 7, 24, 25),
        (7, 7, 43, 44),
    );
    assert_snippet(SPACE_BRACES_METHOD_BANK_REGISTER_FIELD_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_brace.enabled = false;
    assert_snippet(SPACE_BRACES_METHOD_BANK_REGISTER_FIELD_INCORRECT, vec![], &rules);
}

static SPACE_BRACES_METHOD_BANK_REGISTER_FIELD_CORRECT: &str = "
method this_is_some_method() { return 0; }

method this_is_empty_method() { }

bank pcie_config { register command { field mem {
    method pcie_write(uint64 value) {
        if (value != 0) { value = value + 1; }
        default(value);
        map_memory_alt();
    }
}
}
}
";
#[test]
fn space_braces_method_bank_register_field_correct() {
    let rules = set_up();
    assert_snippet(SPACE_BRACES_METHOD_BANK_REGISTER_FIELD_CORRECT, vec![], &rules);
}

static SPACE_BRACES_STRUCT_LAYOUT_BITF_INCORRECT: &str = "
typedef struct {uint16 idx;} hqm_cq_list_release_ctx_t;

typedef layout \"little-endian\" {bitfields 1 {uint1 cq @ [0:0];} byte;} q_t;
";
#[test]
fn space_braces_struct_layout_bitf_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpBraces,
        (1, 1, 15, 16),
        (1, 1, 27, 28),
        (3, 3, 31, 32),
        (3, 3, 69, 70),
        (3, 3, 44, 45),
        (3, 3, 62, 63),
    );
    assert_snippet(SPACE_BRACES_STRUCT_LAYOUT_BITF_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_brace.enabled = false;
    assert_snippet(SPACE_BRACES_STRUCT_LAYOUT_BITF_INCORRECT, vec![], &rules);
}

static SPACE_BRACES_STRUCT_LAYOUT_BITF_CORRECT: &str = "
typedef struct { uint16 idx; } hqm_cq_list_release_ctx_t;

typedef layout \"little-endian\" { bitfields 1 { uint1 cq @ [0:0]; } byte; } q_t;
";
#[test]
fn space_braces_struct_layout_bitf_correct() {
    let mut rules = set_up();
    assert_snippet(SPACE_BRACES_STRUCT_LAYOUT_BITF_CORRECT, vec![], &rules);
    // Test rule disable
    rules.sp_brace.enabled = false;
    assert_snippet(SPACE_BRACES_STRUCT_LAYOUT_BITF_CORRECT, vec![], &rules);
}

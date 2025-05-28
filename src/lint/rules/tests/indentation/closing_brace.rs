use crate::lint::rules::tests::common::{set_up, robust_assert_snippet as assert_snippet};
use crate::lint::rules::RuleType;

static BASIC_COMPOUND_CORRECT: &str = "
method my_method() {
    if (true) {
        return;
    }
}
";


#[test]
fn basic_compound_correct() {
    let rules = set_up();
    assert_snippet(BASIC_COMPOUND_CORRECT, vec![], &rules);
}

static CLOSING_BRACE_NOT_FIRST_IN_LINE_INCORRECT: &str = "
method my_method() {
    if (true) {
        return; }
}
";

#[test]
fn closing_brace_not_first_in_line_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (3, 3, 16, 17),
    );
    assert_snippet(CLOSING_BRACE_NOT_FIRST_IN_LINE_INCORRECT, expected_errors, &rules);
}

static CLOSING_AND_OPEN_BRACE_ON_SAME_LINE_CORRECT: &str = "
method my_method() {
    if (true) { return; }
}
";

#[test]
fn closing_and_open_brace_on_same_line_correct() {
    let rules = set_up();
    assert_snippet(CLOSING_AND_OPEN_BRACE_ON_SAME_LINE_CORRECT, vec![], &rules);
}

static CLOSING_BRACE_NOT_DEINDENTED_INCORRECT: &str = "
method my_method() {
    if (true) {
        return;
        }
}";

#[test]
fn closing_brace_not_deindented_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (4, 4, 8, 9),
    );
    assert_snippet(CLOSING_BRACE_NOT_DEINDENTED_INCORRECT, expected_errors, &rules);
}

static CLOSING_BRACE_OVERINDENTED_INCORRECT: &str = "
method my_method() {
    if (true) {
        return;
    }
        }
";

#[test]
fn closing_brace_overindented_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (5, 5, 8, 9),
    );
    assert_snippet(CLOSING_BRACE_OVERINDENTED_INCORRECT, expected_errors, &rules);
}

static CLOSING_BRACE_NOT_FIRST_SWITCH_INCORRECT: &str = "
method my_method() {
    switch (true) {
    case 1:
        return;
        break;
    case 2:
        return;
        break; }
}";

#[test]
fn closing_brace_not_first_switch_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (8, 8, 15, 16),
    );
    assert_snippet(CLOSING_BRACE_NOT_FIRST_SWITCH_INCORRECT, expected_errors, &rules);
}

static CLOSING_BRACE_NOT_FIRST_STRUCT_INCORRECT: &str = "
typedef struct {
    int x; } mystruct_t;
";

#[test]
fn closing_brace_not_first_struct_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (2, 2, 11, 12),
    );
    assert_snippet(CLOSING_BRACE_NOT_FIRST_STRUCT_INCORRECT, expected_errors, &rules);
}

static CLOSING_BRACE_NOT_FIRST_LAYOUT_INCORRECT: &str = "
typedef layout \"little-endian\" {
    uint8 cmd_code; } mylayout_t;
";

#[test]
fn closing_brace_not_first_layout_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (2, 2, 20, 21),
    );
    assert_snippet(CLOSING_BRACE_NOT_FIRST_LAYOUT_INCORRECT, expected_errors, &rules);
}

static CLOSING_BRACE_NOT_FIRST_BITFIELD_INCORRECT: &str = "
typedef layout \"little-endian\" {
    bitfields 8 {
        uint7 addr        @ [7:1];
        uint1 always_zero @ [0:0]; } dst_slave;
} mylayout_t;
";

#[test]
fn closing_brace_not_first_bitfield_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (4, 4, 35, 36),
    );
    assert_snippet(CLOSING_BRACE_NOT_FIRST_BITFIELD_INCORRECT, expected_errors, &rules);
}

static SWITCH_CASE_SAME_LINE_CORRECT: &str = "
method my_method() {
    switch (true) {
    case 1: { return; }
    case 2: { return; }
    }
}";

#[test]
fn switch_case_same_line_correct() {
    let rules = set_up();
    assert_snippet(SWITCH_CASE_SAME_LINE_CORRECT, vec![], &rules);
}

static COMPOSITE_INDENT_CORRECT: &str = "
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
fn composite_indent_correct() {
    let rules = set_up();
    assert_snippet(COMPOSITE_INDENT_CORRECT, vec![], &rules);
}

static COMPOSITE_INDENT_INCORRECT: &str = "
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
} } }
";
#[test]
fn composite_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN4,
        (11, 11, 16, 17),
        (12, 12, 0, 1),
        (12, 12, 2, 3),
        (12, 12, 4, 5),
    );
    assert_snippet(COMPOSITE_INDENT_INCORRECT, expected_errors, &rules);
}

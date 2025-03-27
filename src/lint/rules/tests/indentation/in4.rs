use crate::lint::rules::tests::common::{set_up, assert_snippet};

pub static BASIC_CORRECT: &str = "
method my_method() {
    if (true) {
        return;
    }
}
";


#[test]
fn test_basic_correct() {
    let rules = set_up();
    assert_snippet(BASIC_CORRECT, 0, &rules);
}

pub static CLOSING_BRACE_NOT_FIRST: &str = "
method my_method() {
    if (true) {
        return; }
}
";

#[test]
fn test_closing_brace_not_first() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_FIRST, 1, &rules);
}

pub static CLOSING_AND_OPEN_BRACE_ON_SAME_LINE: &str = "
method my_method() {
    if (true) { return; }
}
";

#[test]
fn test_closing_and_open_brace_on_same_line() {
    let rules = set_up();
    assert_snippet(CLOSING_AND_OPEN_BRACE_ON_SAME_LINE, 0, &rules);
}

pub static CLOSING_BRACE_NOT_DEINDENTED: &str = "
method my_method() {
    if (true) {
        return;
        }
}";

#[test]
fn test_closing_brace_not_deindented() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_DEINDENTED, 1, &rules);
}

pub static CLOSING_BRACE_OVERINDENTED: &str = "
method my_method() {
    if (true) {
        return;
    }
        }
";

#[test]
pub fn test_closing_brace_overindented() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_OVERINDENTED, 1, &rules);
}

pub static CLOSING_BRACE_NOT_FIRST_SWITCH: &str = "
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
pub fn test_closing_brace_not_first_switch() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_FIRST_SWITCH, 1, &rules);
}

pub static CLOSING_BRACE_NOT_FIRST_STRUCT: &str = "
typedef struct {
    int x; } mystruct_t;
";

#[test]
pub fn test_closing_brace_not_first_struct() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_FIRST_STRUCT, 1, &rules);
}

pub static CLOSING_BRACE_NOT_FIRST_LAYOUT: &str = "
typedef layout \"little-endian\" {
    uint8 cmd_code; } mylayout_t;
";

#[test]
pub fn test_closing_brace_not_first_layout() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_FIRST_LAYOUT, 1, &rules);
}

pub static CLOSING_BRACE_NOT_FIRST_BITFIELD: &str = "
typedef layout \"little-endian\" {
    bitfields 8 {
        uint7 addr        @ [7:1];
        uint1 always_zero @ [0:0]; } dst_slave;
} mylayout_t;
";

#[test]
pub fn test_closing_brace_not_first_bitfield() {
    let rules = set_up();
    assert_snippet(CLOSING_BRACE_NOT_FIRST_BITFIELD, 1, &rules);
}

pub static SWITCH_CASE_SAME_LINE: &str = "
method my_method() {
    switch (true) {
    case 1: { return; }
    case 2: { return; }
    }
}";

#[test]
pub fn test_switch_case_same_line() {
    let rules = set_up();
    assert_snippet(SWITCH_CASE_SAME_LINE, 0, &rules);
}

pub static IN4_COMPOSITE_CORRECT_INDENT: &str = "
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
fn in4_composite_correct_indent() {
    let rules = set_up();
    assert_snippet(IN4_COMPOSITE_CORRECT_INDENT, 0, &rules);
}

pub static IN4_COMPOSITE_INCORRECT_INDENT: &str = "
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
fn in4_composite_incorrect_indent() {
    let rules = set_up();
    assert_snippet(IN4_COMPOSITE_INCORRECT_INDENT, 4, &rules);
}

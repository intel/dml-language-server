use crate::lint::rules::tests::common::{set_up, robust_assert_snippet as assert_snippet};
use crate::lint::rules::RuleType;

static CONTINUATION_LINE_CORRECT: &str = "
method set_irq() {
    interrupt_enabled =
        irq_enabled(interrupt_device);
}
";
#[test]
fn continuation_line_correct() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_CORRECT, vec![], &rules);
}

static CONTINUATION_LINE_CORRECT_2: &str = "
method calculate_sum(uint64 a, uint64 b) -> (uint64) {
    return (a + b) * (a - b) +
        (a * b);
}
";
#[test]
fn continuation_line_correct_2() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_CORRECT_2, vec![], &rules);
}

static CONTINUATION_LINE_CORRECT_3: &str = "
bank regs {
    register example_register size 4 @ 0x00 {
        method read() -> (uint64) {
            local uint64 value = (this.val + 10) *
                (this.val - 5);
            return value;
        }
    }
}
";
#[test]
fn continuation_line_correct_3() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_CORRECT_3, vec![], &rules);
}

static CONTINUATION_LINE_INCORRECT: &str = "
method set_irq() {
    interrupt_enabled =
irq_enabled(interrupt_device);
}
";
#[test]
fn continuation_line_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (3, 3, 0, 11),
    );
    assert_snippet(CONTINUATION_LINE_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_INCORRECT_2: &str = "
method write(uint64 value) {
    local uint64 a = value;
    local uint64 result = a <<
                               2;
    log info: 'Writing to register, result after left shift = %x', result;
}
";
#[test]
fn continuation_line_incorrect_2() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (4, 4, 31, 32),
    );
    assert_snippet(CONTINUATION_LINE_INCORRECT_2, expected_errors, &rules);
}

pub static CONTINUATION_LINE_INCORRECT_3: &str = "
bank regs {
    register control size 4 @ 0x00 {
        field enable @ [0];
        field mode @ [2:1];
        field status @ [31:3] {
            param init_val = (1 << 2) |
                                            (1 << 1);
        }
    }
}
";
#[test]
fn continuation_line_incorrect_3() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (8, 8, 44, 52),
    );
    assert_snippet(CONTINUATION_LINE_INCORRECT_3, expected_errors, &rules);
}
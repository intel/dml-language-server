use crate::lint::rules::tests::common::{set_up, assert_snippet};

pub static IN6_CONTINUATION_LINE_INCORRECT: &str = "
method set_irq() {
    interrupt_enabled =
irq_enabled(interrupt_device);
}
";

pub static IN6_CONTINUATION_LINE_INCORRECT_2: &str = "
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

pub static IN6_CONTINUATION_LINE_INCORRECT_3: &str = "
method write(uint64 value) {
    local uint64 a = value;
    local uint64 result = a <<
                               2;
    log info: 'Writing to register, result after left shift = %x', result;
}
";

pub static IN6_CONTINUATION_LINE_OK: &str = "
method set_irq() {
    interrupt_enabled =
        irq_enabled(interrupt_device);
}
";

pub static IN6_CONTINUATION_LINE_OK_2: &str = "
method calculate_sum(uint64 a, uint64 b) -> (uint64) {
    return (a + b) * (a - b) +
        (a * b);
}
";

pub static IN6_CONTINUATION_LINE_OK_3: &str = "
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
fn in6_continuation_line() {
    let rules = set_up();

    assert_snippet(IN6_CONTINUATION_LINE_INCORRECT, 1, &rules);
    assert_snippet(IN6_CONTINUATION_LINE_INCORRECT_2, 1, &rules);
    assert_snippet(IN6_CONTINUATION_LINE_INCORRECT_3, 1, &rules);
    assert_snippet(IN6_CONTINUATION_LINE_OK, 0, &rules);
    assert_snippet(IN6_CONTINUATION_LINE_OK_2, 0, &rules);
    assert_snippet(IN6_CONTINUATION_LINE_OK_3, 0, &rules);
}

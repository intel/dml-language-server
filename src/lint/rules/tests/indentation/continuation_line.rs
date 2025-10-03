use crate::lint::rules::tests::common::{set_up, assert_snippet};
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

static CONTINUATION_LINE_RETURN_CORRECT: &str = "
method calculate_sum(uint64 a, uint64 b) -> (uint64) {
    return (a + b) * (a - b)
        + (a * b);
}
";
#[test]
fn continuation_line_return_correct() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_RETURN_CORRECT, vec![], &rules);
}

static CONTINUATION_LINE_LOCAL_DECLA_CORRECT: &str = "
bank regs {
    register example_register size 4 @ 0x00 {
        method read() -> (uint64) {
            local uint64 value = (this.val + 10)
                * (this.val - 5);
            return value;
        }
    }
}
";
#[test]
fn continuation_line_local_decla_correct() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_LOCAL_DECLA_CORRECT, vec![], &rules);
}

pub static CONTINUATION_LINE_FUNC_CALL_CORRECT: &str = "
method check_hashes(uint64 key, x_range *range) -> (int) {
    local const uint8 *digest = get_key(key);
    local bool same_hash = memcmp(range->hash_bytes,
                                  digest, SIZE) == 0;
    return (same_hash) ? CORRECT : INCORRECT;
}
";
#[test]
fn continuation_line_func_call_correct() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_FUNC_CALL_CORRECT, vec![], &rules);
}

pub static CONTINUATION_LINE_IN_EACH_CORRECT: &str ="
in each reg_read_write {
    param y_val_reg = CONST_X;
    is ip_disable;
}
";
#[test]
fn continuation_line_in_each_correct() {
    let rules = set_up();
    assert_snippet(CONTINUATION_LINE_IN_EACH_CORRECT, vec![], &rules);
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

pub static CONTINUATION_LINE_BITWISE_INCORRECT: &str = "
method write(uint64 value) {
    local uint64 a = value;
    local uint64 result = a
                               << 2;
    log info: 'Writing to register, result after left shift = %x', result;
}
";
#[test]
fn continuation_line_bitwise_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (4, 4, 31, 33),
    );
    assert_snippet(CONTINUATION_LINE_BITWISE_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_PARAM_INCORRECT: &str = "
bank regs {
    register control size 4 @ 0x00 {
        field status @ [31:3] {
            param init_val = 1 << 2
                                            | 1 << 1;
        }
    }
}
";
#[test]
fn continuation_line_param_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (5, 5, 44, 45),
    );
    assert_snippet(CONTINUATION_LINE_PARAM_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_HOOK_INCORRECT: &str = "
hook(int *, bool)
h3;

template hooktest {
    hook()
                h2;
}
";
#[test]
fn continuation_line_hook_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (2, 2, 0, 2),
        (6, 6, 16, 18),
    );
    assert_snippet(CONTINUATION_LINE_HOOK_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_IN_EACH_INCORRECT: &str ="
in each
reg_read_write {
    param y_val_reg = CONST_X;
    is ip_disable;
}
";
#[test]
fn continuation_line_in_each_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (2, 2, 0, 14),
    );
    assert_snippet(CONTINUATION_LINE_IN_EACH_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_SESSION_INCORRECT: &str ="
session
    int max_buffer_index = 1;
session
int last_index_sent = 0;
";
#[test]
fn continuation_line_session_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (4, 4, 0, 3),
    );
    assert_snippet(CONTINUATION_LINE_SESSION_INCORRECT, expected_errors, &rules);
}

pub static CONTINUATION_LINE_TYPEDEF_INCORRECT: &str ="
typedef
        struct {
    uint32 data;
    uint32 address;
}entry;
";
#[test]
fn continuation_line_typedef_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN6,
        (2, 2, 8, 14),
    );
    assert_snippet(CONTINUATION_LINE_TYPEDEF_INCORRECT, expected_errors, &rules);
}
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  NSP.unary between a unary operator and its operand
static NO_SPACE_UNARY_INCORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if (! dummy_obj)
        return;
    local uint64 p = & dummy_obj;
    p ++;
    -- p;
    local int64 neg = - 1;
}
";
#[test]
fn no_space_unary_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspUnary,
        (2, 2, 9, 10),
        (4, 4, 22, 23),
        (5, 5, 5, 6),
        (6, 6, 6, 7),
        (7, 7, 23, 24),
    );
    assert_snippet(NO_SPACE_UNARY_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_unary.enabled = false;
    assert_snippet(NO_SPACE_UNARY_INCORRECT, vec![], &rules);
}

static NO_SPACE_UNARY_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if (!dummy_obj)
        return;
    local uint64 p = &dummy_obj;
    p++;
    --p;
    local int64 neg = -1;
}
";
#[test]
fn no_space_unary_correct() {
    let rules = set_up();
    assert_snippet(NO_SPACE_UNARY_CORRECT, vec![], &rules);
}
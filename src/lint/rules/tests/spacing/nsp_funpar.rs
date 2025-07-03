use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

// There should be no space:
//  NSP.funpar between a function/method name and its opening parenthesis
static NO_SPACE_METHOD_FUNC_INCORRECT: &str = "
method this_is_some_method (conf_object_t *dummy_obj) {
    if(!dummy_obj)
        other_method_called ();
}
";
#[test]
fn no_space_method_func_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspFunpar,
        (1, 1, 26, 27),
        (3, 3, 27, 28),
    );
    assert_snippet(NO_SPACE_METHOD_FUNC_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_funpar.enabled = false;
    assert_snippet(NO_SPACE_METHOD_FUNC_INCORRECT, vec![], &rules);
}

static NO_SPACE_METHOD_FUNC_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if(!dummy_obj)
        other_method_called();
}
";
#[test]
fn no_space_method_func_correct() {
    let rules = set_up();
    assert_snippet(NO_SPACE_METHOD_FUNC_CORRECT, vec![], &rules);
}
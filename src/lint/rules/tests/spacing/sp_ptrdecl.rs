use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

#[allow(dead_code)]
static SP_PTRDECL_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if(!dummy_obj) {
        return;
    }
}";

#[allow(dead_code)]
static SP_PTRDECL_INCORRECT_PARAM: &str = "
method this_is_some_method(conf_object_t*dummy_obj) {
    if(!dummy_obj) {
        return;
    }
}";

#[allow(dead_code)]
static SP_PTRDECL_INCORRECT_STATEMENT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    local conf_object_t*conf = dummy_obj;
    if(!conf) {
        return;
    }
}";

#[allow(dead_code)]
static SP_MULTIPLE_POINTER_SYMBOLS: &str = "
method this_is_some_method(conf_object_t **dummy_obj) {
}";

#[test]
fn sp_ptrdecl_correct() {
    let mut rules = set_up();
    assert_snippet(SP_PTRDECL_CORRECT, vec![], &rules);
    // Test rule disable
    rules.sp_ptrdecl.enabled = false;
    assert_snippet(SP_PTRDECL_CORRECT, vec![], &rules);
}

#[test]
fn sp_ptrdecl_incorrect_param() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpPtrDecl,
        (1, 1, 27, 40),
    );
    assert_snippet(SP_PTRDECL_INCORRECT_PARAM, expected_errors, &rules);
    // Test rule disable
    rules.sp_ptrdecl.enabled = false;
    assert_snippet(SP_PTRDECL_INCORRECT_PARAM, vec![], &rules);
}
#[test]
fn sp_ptrdecl_incorrect_statement() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpPtrDecl,
        (2, 2, 10, 23),
    );
    assert_snippet(SP_PTRDECL_INCORRECT_STATEMENT, expected_errors, &rules);
    // Test rule disable
    rules.sp_ptrdecl.enabled = false;
    assert_snippet(SP_PTRDECL_INCORRECT_STATEMENT, vec![], &rules);
}

#[test]
fn sp_ptrdecl_multiple_symbols() {
    let rules = set_up();
    assert_snippet(SP_MULTIPLE_POINTER_SYMBOLS, vec![], &rules);
}
use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

static NSP_PTRDECL_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if (!dummy_obj) {
        return;
    }
}";
#[test]
fn nsp_ptrdecl_correct() {
    let mut rules = set_up();
    assert_snippet(NSP_PTRDECL_CORRECT, vec![], &rules);
    // Test rule disable
    rules.nsp_ptrdecl.enabled = false;
    assert_snippet(NSP_PTRDECL_CORRECT, vec![], &rules);
}

static NSP_PTRDECL_INCORRECT_PARAM: &str = "
method this_is_some_method(conf_object_t * dummy_obj) {
    if (!dummy_obj) {
        return;
    }
}";
#[test]
fn nsp_ptrdcl_incorrect_param() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspPtrDecl,
        (1, 1, 43, 52),
    );
    assert_snippet(NSP_PTRDECL_INCORRECT_PARAM, expected_errors, &rules);
    // Test rule disable
    rules.nsp_ptrdecl.enabled = false;
    assert_snippet(NSP_PTRDECL_INCORRECT_PARAM, vec![], &rules);
}

static NSP_PTRDECL_INCORRECT_STATEMENT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    local conf_object_t * conf = dummy_obj;
    if (!conf) {
        return;
    }
}";
#[test]
fn nsp_ptrdecl_incorrect_statement() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspPtrDecl,
        (2, 2, 26, 30),
    );
    assert_snippet(NSP_PTRDECL_INCORRECT_STATEMENT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_ptrdecl.enabled = false;
    assert_snippet(NSP_PTRDECL_INCORRECT_STATEMENT, vec![], &rules);
}

static NSP_PTRDECL_MULTIPLE_POINTER_SYMBOLS: &str = "
method this_is_some_method(conf_object_t **dummy_obj) {
    local conf_object_t ** conf = dummy_obj;
    if (!conf) {
        return;
    }
}";
#[test]
fn nsp_ptrdecl_multiple_symbols() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspPtrDecl,
        (2, 2, 27, 31),
    );
    assert_snippet(NSP_PTRDECL_MULTIPLE_POINTER_SYMBOLS, expected_errors, &rules);
    // Test rule disable
    rules.nsp_ptrdecl.enabled = false;
    assert_snippet(NSP_PTRDECL_MULTIPLE_POINTER_SYMBOLS, vec![], &rules);
}
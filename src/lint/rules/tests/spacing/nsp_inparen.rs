use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  NSP.inparen immediately inside parentheses or brackets
static NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT: &str = "
method this_is_some_method( conf_object_t *dummy_obj ) {
    if( !dummy_obj[ 0 ] )
        return;
}
";
#[test]
fn no_space_inparen_method_func_index_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspInparen,
        (1, 1, 27, 28),
        (1, 1, 52, 53),
        (2, 2, 7, 8),
        (2, 2, 23, 24),
        (2, 2, 19, 20),
        (2, 2, 21, 22),
    );
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_inparen.enabled = false;
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT, vec![], &rules);
}

//  NSP.inparen immediately inside parentheses or brackets
static NO_SPACE_INPAREN_METHOD_FUNC_INDEX_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if(!dummy_obj[0])
        return;
}
";
#[test]
fn no_space_inparen_method_func_index_correct() {
    let rules = set_up();
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_CORRECT, vec![], &rules);
}
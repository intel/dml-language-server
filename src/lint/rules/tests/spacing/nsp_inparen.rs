use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  NSP.inparen immediately inside parentheses or brackets
static METHOD_FUNC_INDEX_INCORRECT: &str = "
method this_is_some_method( conf_object_t *dummy_obj ) {
    if ( !dummy_obj[ 0 ] )
        return;
}
";
#[test]
fn method_func_index_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspInparen,
        (1, 1, 27, 28),
        (1, 1, 52, 53),
        (2, 2, 8, 9),
        (2, 2, 24, 25),
        (2, 2, 20, 21),
        (2, 2, 22, 23),
    );
    assert_snippet(METHOD_FUNC_INDEX_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_inparen.enabled = false;
    assert_snippet(METHOD_FUNC_INDEX_INCORRECT, vec![], &rules);
}

//  NSP.inparen immediately inside parentheses or brackets
static METHOD_FUNC_INDEX_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if (!dummy_obj[0])
        return;
}
";
#[test]
fn method_func_index_correct() {
    let rules = set_up();
    assert_snippet(METHOD_FUNC_INDEX_CORRECT, vec![], &rules);
}


pub static COMPOSITE_ARRAY_DECLARATION_AND_TEMPLATES_INCORRECT: &str = "
bank some_bank {
    group some_group[ i < ( GROUP_COUNT ) ] {
        register some_reg is ( some_template, another_template ) {
            param desc = \"Register description\";
        }
    }
}
";
#[test]
fn composite_array_declaration_and_templates_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspInparen,
        (2, 2, 21, 22),
        (2, 2, 27, 28),
        (2, 2, 39, 40),
        (2, 2, 41, 42),
        (3, 3, 30, 31),
        (3, 3, 62, 63),            
    );
    assert_snippet(COMPOSITE_ARRAY_DECLARATION_AND_TEMPLATES_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_inparen.enabled = false;
    assert_snippet(COMPOSITE_ARRAY_DECLARATION_AND_TEMPLATES_INCORRECT, vec![], &rules);
}

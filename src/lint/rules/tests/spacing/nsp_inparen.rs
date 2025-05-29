use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  NSP.inparen immediately inside parentheses or brackets
static NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT: &str = "
method this_is_some_method( conf_object_t *dummy_obj ) {
    if ( !dummy_obj[ 0 ] )
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
        (2, 2, 8, 9),
        (2, 2, 24, 25),
        (2, 2, 20, 21),
        (2, 2, 22, 23),
    );
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_inparen.enabled = false;
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_INCORRECT, vec![], &rules);
}

//  NSP.inparen immediately inside parentheses or brackets
static NO_SPACE_INPAREN_METHOD_FUNC_INDEX_CORRECT: &str = "
method this_is_some_method(conf_object_t *dummy_obj) {
    if (!dummy_obj[0])
        return;
}
";
#[test]
fn no_space_inparen_method_func_index_correct() {
    let rules = set_up();
    assert_snippet(NO_SPACE_INPAREN_METHOD_FUNC_INDEX_CORRECT, vec![], &rules);
}


pub static NSP_INPAREN_02: &str = "
bank some_bank {
    group some_group[ i < ( GROUP_COUNT ) ] {
        register some_reg is ( some_template, another_template ) {
            param desc = \"Register description\";
        }
    }
}
";
    #[test]
    fn style_check_nsp_inparen_02() {
        // let mut cfg = LintCfg::default();
        // let mut rules = instantiate_rules(&cfg);
        // assert_snippet(NSP_INPAREN_02, 6, &rules);
        // Test rule disable
        // cfg.nsp_inparen = None;
        // rules = instantiate_rules(&cfg);
        // assert_snippet(NSP_INPAREN_02, 0, &rules);

    }

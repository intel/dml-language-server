use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  SP.punct after but not before colon, semicolon and comma
static SP_PUNCT_INCORRECT: &str = "
method this_is_some_method(bool flag ,int8 var) {
    local int this_some_integer = 0x666 ;
    if(this_some_integer == 0x666)
        return;
    some_func(arg1 ,arg2 ,arg3 ,arg4);
}
";
#[test]
fn sp_punct_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpPunct,
        (1, 1, 36, 37),
        (1, 1, 37, 46),
        (2, 2, 39, 40),
        (5, 5, 18, 19),
        (5, 5, 19, 24),
        (5, 5, 24, 25),
        (5, 5, 25, 30),
        (5, 5, 30, 31),
        (5, 5, 31, 36),
    );
    assert_snippet(SP_PUNCT_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_punct.enabled = false;
    assert_snippet(SP_PUNCT_INCORRECT, vec![], &rules);
}

static SP_PUNCT_CORRECT: &str = "
method this_is_some_method(bool flag, int8 var) {
    local int this_some_integer = 0x666;
    if(this_some_integer == 0x666)
        return;
    some_func(arg1, arg2, arg3, arg4);
}
";
#[test]
fn sp_punct_correct() {
    let rules = set_up();
    assert_snippet(SP_PUNCT_CORRECT, vec![], &rules);
}
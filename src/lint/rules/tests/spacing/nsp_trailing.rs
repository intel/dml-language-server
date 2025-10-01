use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  Adding trailing whitespace removal to spacing rules:
//  no whitespaces should be left at the end of a line between the last token
//  and the newline \n
static TRAILING_WHITESPACE_INCORRECT: &str = "
method this_is_some_method(int64 num) {
    local int this_some_integer = 0x666;           
    if (this_some_integer == 0x666)       
        return;  
}   
";
#[test]
fn trailing_whitespace_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::NspTrailing,
        (2, 2, 40, 51),
        (3, 3, 35, 42),
        (4, 4, 15, 17),
        (5, 5, 1, 4),
    );
    assert_snippet(TRAILING_WHITESPACE_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.nsp_trailing.enabled = false;
    assert_snippet(TRAILING_WHITESPACE_INCORRECT, vec![], &rules);
}
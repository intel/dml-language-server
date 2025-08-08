use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;


//  SP.ternary around ? and : in the ternary ?: operator
static VARIABLE_CONDITIONAL_CORRECT: &str = "
method this_is_some_method(bool flag) {
    local int this_some_integer = (flag ? 5 : 7);
}
";
#[test]
fn variable_conditional_correct() {
	let rules = set_up();
	assert_snippet(VARIABLE_CONDITIONAL_CORRECT, vec![], &rules);
}

static VARIABLE_CONDITIONAL_INCORRECT: &str = "
method this_is_some_method(bool flag) {
    local int this_some_integer = (flag?5:7);
}
";
#[test]
fn variable_conditional_incorrect() {
	let rules = set_up();
    let expected_errors = define_expected_errors!(
		RuleType::SpTernary,
		(2, 2, 35, 39),
		(2, 2, 40, 41),
		(2, 2, 40, 41),
		(2, 2, 42, 43),
	);
	assert_snippet(VARIABLE_CONDITIONAL_INCORRECT, expected_errors, &rules);
}

static PARAM_CONDITIONAL_CORRECT: &str = "
param even_or_odd = odd_flag ? 'odd' : 'even';
";
#[test]
fn param_conditional_correct() {
	let rules = set_up();
	assert_snippet(PARAM_CONDITIONAL_CORRECT, vec![], &rules);
}

static PARAM_CONDITIONAL_INCORRECT: &str = "
param even_or_odd = odd_flag ?'odd' :'even';
";
#[test]
fn param_conditional_incorrect() {
	let rules = set_up();
	let expected_errors = define_expected_errors!(
		RuleType::SpTernary,
		(1, 1, 30, 35),
		(1, 1, 37, 43),
	);
	assert_snippet(PARAM_CONDITIONAL_INCORRECT, expected_errors, &rules);
}

#[test]
fn rule_disable() {
	let mut rules = set_up();
	let expected_errors = define_expected_errors!(
		RuleType::SpTernary,
		(1, 1, 30, 35),
		(1, 1, 37, 43),
	);
	assert_snippet(PARAM_CONDITIONAL_INCORRECT, expected_errors, &rules);
	rules.sp_ternary.enabled = false;
	assert_snippet(PARAM_CONDITIONAL_INCORRECT, vec![], &rules);
}

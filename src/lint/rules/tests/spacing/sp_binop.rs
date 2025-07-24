use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

//  SP.binop around binary operators except the dereferencing operators dot
//  (a.b) and arrow (a->b)
static ARTITHMETIC_OPERATOR_CORRECT: &str = "
method this_is_some_method() {
    local int this_some_integer = 5 + 6;
    if (this_some_integer == 0x666) {
        this_some_integer = this.val;
    }
}
";

#[test]
fn arithmetic_operator_correct() {
	let rules = set_up();
	assert_snippet(ARTITHMETIC_OPERATOR_CORRECT, vec![], &rules);
}
static ARTITHMETIC_OPERATOR_INCORRECT: &str = "
method this_is_some_method() {
    local int this_some_integer = 5+6;
    if (this_some_integer == 0x666) {
        this_some_integer = this.val;
    }
}
";

#[test]
fn arithmetic_operator_incorrect() {
	let rules = set_up();
    let expected_errors = define_expected_errors!(
		RuleType::SpBinop,
		(2, 2, 34, 35),
		(2, 2, 36, 37),
	);
	assert_snippet(ARTITHMETIC_OPERATOR_INCORRECT, expected_errors, &rules);
}

static CONDITIONAL_OPERATOR_INCORRECT: &str = "
method this_is_some_method() {
    local int this_some_integer = 5 + 6;
    if (this_some_integer==0x666) {
        this_some_integer = this.val;
    }
}
";
#[test]
fn conditional_operator_incorrect() {
	let rules = set_up();
    let expected_errors = define_expected_errors!(
		RuleType::SpBinop,
		(3, 3, 8, 25),
		(3, 3, 27, 32),
	);
	assert_snippet(CONDITIONAL_OPERATOR_INCORRECT, expected_errors, &rules);
}

static SP_BINOP: &str = "
method this_is_some_method() {
    local int this_some_integer = 5+6;
    if (this_some_integer == 0x666) {
        this_some_integer = this.val;
    }
}
";

#[test]
fn rule_disable() {
	let mut rules = set_up();
	let expected_errors = define_expected_errors!(
		RuleType::SpBinop,
		(2, 2, 34, 35),
		(2, 2, 36, 37),
	);
	assert_snippet(SP_BINOP, expected_errors, &rules);
	rules.sp_binop.enabled = false;
	assert_snippet(SP_BINOP, vec![], &rules);
}

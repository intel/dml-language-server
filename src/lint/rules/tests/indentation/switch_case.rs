use crate::lint::rules::tests::common::{set_up, robust_assert_snippet as assert_snippet};
use crate::lint::rules::RuleType;

static SWITCH_CASE_INDENT_CORRECT: &str = "
method some_switch(int arg) {
    switch(arg) {
    case ZERO:
    #if (asdd == 0) {
        some_call();
    }
        if (a) {
            return;
        }
        some_call();
        break;
    default: { return; }
    }
}
";
#[test]
// #[ignore]
fn switch_case_indent_correct() {
    let rules = set_up();
    assert_snippet(SWITCH_CASE_INDENT_CORRECT, vec![], &rules);
}

static SWITCH_CASE_INDENT_INCORRECT: &str = "
method some_switch(int arg) {
    switch(arg) {
      case ZERO:
        #if (asdd == 0) {
        some_call();
        }
          if (a) {
            return;
        }
        some_call();
      break;
    case ONE: {
        return;
    }
    default: { return; }
    }
}
";
#[test]
fn switch_case_indent_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN9,
        (3, 3, 6, 16),
        (7, 9, 10, 9),
        (11, 11, 6, 12),
    );
    assert_snippet(SWITCH_CASE_INDENT_INCORRECT, expected_errors, &rules);
}

use crate::lint::rules::tests::common::{set_up, assert_snippet};

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
    assert_snippet(SWITCH_CASE_INDENT_CORRECT, 0, &rules);
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
    assert_snippet(SWITCH_CASE_INDENT_INCORRECT, 3, &rules);
}

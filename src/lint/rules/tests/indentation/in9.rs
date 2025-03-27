use crate::lint::rules::tests::common::{set_up, assert_snippet};

pub static IN9_CORRECT_CASE_INDENT: &str = "
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
fn in9_correct_case_indent() {
    let rules = set_up();
    assert_snippet(IN9_CORRECT_CASE_INDENT, 0, &rules);
}

pub static IN9_INCORRECT_CASE_INDENT: &str = "
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
fn in9_incorrect_case_indent() {
    let rules = set_up();
    assert_snippet(IN9_INCORRECT_CASE_INDENT, 3, &rules);
}

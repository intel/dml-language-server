use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

// A continuation line that is broken inside a parenthesized expression 
// is indented to line up inside the corresponding parenthesis on the previous line

static OPERATION_PAREN_CORRECT: &str = "
param result = (((reg0.val * reg1.enable.val)
                 & mask_reg)
                + 1);
";
#[test]
fn operation_paren_correct() {
    let rules = set_up();
    assert_snippet(OPERATION_PAREN_CORRECT, vec![], &rules);
}

static OPERATION_PAREN_INCORRECT: &str = "
param result = (((reg0.val * reg1.enable.val)
                & mask_reg)
                 + 1);
";
#[test]
fn operation_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 17, 18),
        (2, 2, 16, 17),
    );
    assert_snippet(OPERATION_PAREN_INCORRECT, expected_errors, &rules);
}

static METHODARGS_PAREN_CORRECT: &str = "
method some_method(int a,
                   int b, bool c) {
    return c ? a + b : a - b;
}
";
#[test]
fn methodargs_paren_correct() {
    let rules = set_up();
    assert_snippet(METHODARGS_PAREN_CORRECT, vec![], &rules);
}

static METHODARGS_PAREN_INCORRECT: &str = "
method some_method(int a,
    int b, bool c) {
    return c ? a + b : a - b;
}
";
#[test]
fn methodargs_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (2, 2, 4, 7),
    );
    assert_snippet(METHODARGS_PAREN_INCORRECT, expected_errors, &rules);
}

static FUNCALL_PAREN_CORRECT: &str = "
method effect() {
    callback(0xABC,
             identifier,
             false);
}
";
#[test]
fn funcall_paren_correct() {
    let rules = set_up();
    assert_snippet(FUNCALL_PAREN_CORRECT, vec![], &rules);
}

static FUNCALL_PAREN_INCORRECT: &str = "
method effect() {
    callback(0xABC,
        identifier,
        false);
}
";
#[test]
fn funcall_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 8, 18),
        (4, 4, 8, 13),
    );
    assert_snippet(FUNCALL_PAREN_INCORRECT, expected_errors, &rules);
}

static FUNCALL_NESTED_PAREN_CORRECT: &str = "
method effect() {
    callback(another_cb(varname,
                        value),
             (1 + 2
              + 3 + 4),
             false);
}
";
#[test]
fn funcall_nested_paren_correct() {
    // This test checks that nested parentheses are correctly indented
    let rules = set_up();
    assert_snippet(FUNCALL_NESTED_PAREN_CORRECT, vec![], &rules);
}

static FUNCALL_NESTED_PAREN_INCORRECT: &str = "
method effect() {
    callback(another_cb(varname,
                    value),
        (1 + 2
         + 3 + 4),
        false);
}
";
#[test]
fn funcall_nested_paren_incorrect() {
    // This test checks that nested parentheses are incorrectly indented
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (4, 4, 8, 9),
        (6, 6, 8, 13),
        (3, 3, 20, 25),
    );
    assert_snippet(FUNCALL_NESTED_PAREN_INCORRECT, expected_errors, &rules);
}

static IF_PAREN_CORRECT: &str = "
method callback() {
    if (conditionX &&
        conditionY) {
        return;
    }
}
";
#[test]
fn if_paren_correct() {
    let rules = set_up();
    assert_snippet(IF_PAREN_CORRECT, vec![], &rules);
}

static IF_PAREN_INCORRECT: &str = "
method callback() {
    if (conditionX &&
          conditionY) {
        return;
    }
}
";
#[test]
fn if_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 10, 20),
    );
    assert_snippet(IF_PAREN_INCORRECT, expected_errors, &rules);
}

static WHILE_PAREN_CORRECT: &str = "
method callback() {
    while (conditionX
           || conditionY) {
        update_conditions();
    }
}
";
#[test]
fn while_paren_correct() {
    let rules = set_up();
    assert_snippet(WHILE_PAREN_CORRECT, vec![], &rules);
}

static WHILE_PAREN_INCORRECT: &str = "
method callback() {
    while (initial
        + offset
           <= size) {
        offset++;
    }
}
";
#[test]
fn while_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 8, 9),
    );
    assert_snippet(WHILE_PAREN_INCORRECT, expected_errors, &rules);
}

static DO_WHILE_PAREN_CORRECT: &str = "
method callback() {
    do {
        log info: 'some message';
    } while (conditionX
             || conditionY);
}
";
#[test]
fn do_while_paren_correct() {
    let rules = set_up();
    assert_snippet(DO_WHILE_PAREN_CORRECT, vec![], &rules);
}

static DO_WHILE_PAREN_INCORRECT: &str = "
method callback() {
    do {
        set_values();
    } while (conditionX
            || conditionY);
}
";
#[test]
fn do_while_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (5, 5, 12, 14),
    );
    assert_snippet(DO_WHILE_PAREN_INCORRECT, expected_errors, &rules);
}

static FOR_PAREN_CORRECT: &str = "
method callback() {
    for (local int i = 0;
         i < 10;
         i++) {
        log info: 'some message';
    }
}
";
#[test]
fn for_paren_correct() {
    let rules = set_up();
    assert_snippet(FOR_PAREN_CORRECT, vec![], &rules);
}

static FOR_PAREN_INCORRECT: &str = "
method callback() {
    for (local int i = 0;
           i < 10;
             i++) {
        log info: 'some message';
    }
}
";
#[test]
fn for_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 11, 12),
        (4, 4, 13, 14),
    );
    assert_snippet(FOR_PAREN_INCORRECT, expected_errors, &rules);
}

static FOREACH_PAREN_CORRECT: &str = "
method handle() {
    #foreach item in ([reg0, reg1,
                      reg2, reg3]) {
        item.update();
    }
}
";
#[test]
fn foreach_paren_correct() {
    let rules = set_up();
    assert_snippet(FOREACH_PAREN_CORRECT, vec![], &rules);
}

static FOREACH_PAREN_INCORRECT: &str = "
method handle() {
    #foreach item in ([reg0,
            reg1,
                reg2,
                    reg3]) {
        item.update();
    }
}
";
#[test]
fn foreach_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 12, 16),
        (4, 4, 16, 20),
        (5, 5, 20, 24),
    );
    assert_snippet(FOREACH_PAREN_INCORRECT, expected_errors, &rules);
}

static SWITCH_PAREN_CORRECT: &str = "
method select_from_result() {
    switch (a
            + b
            - c) {
    case 0:
        log info: 'Result is zero';
        break;
    case 1:
        log info: 'Result is one';
        break;
    default:
        log info: 'Unknown result';
    }
}
";
#[test]
fn switch_paren_correct() {
    let rules = set_up();
    assert_snippet(SWITCH_PAREN_CORRECT, vec![], &rules);
}

static SWITCH_PAREN_INCORRECT: &str = "
method select_from_result() {
    switch (a
              + b
                - c) {
    case 0:
        log info: 'Result is zero';
        break;
    case 1:
        log info: 'Result is one';
        break;
    default:
        log info: 'Unknown result';
    }
}
";
#[test]
fn switch_paren_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (3, 3, 14, 15),
        (4, 4, 16, 17),
    );
    assert_snippet(SWITCH_PAREN_INCORRECT, expected_errors, &rules);
}

static NESTED_PAREN_EXPR_CORRECT: &str = "
param result = (
                (reg0.val
                 * reg1.enable.val)
                &
                mask_reg
                +
                1);
";

#[test]
fn nested_paren_expr_correct(){
    let rules = set_up();
    assert_snippet(NESTED_PAREN_EXPR_CORRECT, vec![], &rules);
}

static NESTED_PAREN_EXPR_INCORRECT: &str = "
param result = (
    (reg0.val
     * reg1.enable.val)
                &
                 mask_reg
                +
                1);
";
#[test]
fn nested_paren_expr_incorrect(){
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN5,
        (2, 2, 4, 5),
        (5, 5, 17, 25),
    );
    assert_snippet(NESTED_PAREN_EXPR_INCORRECT, expected_errors, &rules);
}

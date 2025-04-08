use crate::lint::rules::tests::common::{set_up, assert_snippet};

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
    assert_snippet(OPERATION_PAREN_CORRECT, 0, &rules);
}

static OPERATION_PAREN_INCORRECT: &str = "
param result = (((reg0.val * reg1.enable.val)
                & mask_reg)
                 + 1);
";
#[test]
fn operation_paren_incorrect() {
    let rules = set_up();
    assert_snippet(OPERATION_PAREN_INCORRECT, 2, &rules);
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
    assert_snippet(METHODARGS_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(METHODARGS_PAREN_INCORRECT, 1, &rules);
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
    assert_snippet(FUNCALL_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(FUNCALL_PAREN_INCORRECT, 2, &rules);
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
    assert_snippet(FUNCALL_NESTED_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(FUNCALL_NESTED_PAREN_INCORRECT, 3, &rules);
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
    assert_snippet(IF_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(IF_PAREN_INCORRECT, 1, &rules);
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
    assert_snippet(WHILE_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(WHILE_PAREN_INCORRECT, 1, &rules);
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
    assert_snippet(DO_WHILE_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(DO_WHILE_PAREN_INCORRECT, 1, &rules);
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
    assert_snippet(FOR_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(FOR_PAREN_INCORRECT, 2, &rules);
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
    assert_snippet(FOREACH_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(FOREACH_PAREN_INCORRECT, 3, &rules);
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
    assert_snippet(SWITCH_PAREN_CORRECT, 0, &rules);
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
    assert_snippet(SWITCH_PAREN_INCORRECT, 2, &rules);
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
    assert_snippet(NESTED_PAREN_EXPR_CORRECT, 0, &rules);
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
    assert_snippet(NESTED_PAREN_EXPR_INCORRECT, 2, &rules);
}

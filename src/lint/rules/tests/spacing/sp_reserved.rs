use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;

static SP_RESERVED_IF_INCORRECT: &str = "
method this_is_some_method(bool flag) {
    local int this_some_integer = 0x666;
    if(this_some_integer == 0x666)
        return;

    if(this_some_integer == 0x667) {
        if(flag) {
            some_cal();
        }else{
            return;
        }
    }
}
";
#[test]
fn sp_reserved_if_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpReserved,
        (3, 3, 4, 7),
        (6, 6, 4, 7),
        (7, 7, 8, 11),
        (7, 9, 17, 13),
        (9, 11, 9, 9),
    );
    assert_snippet(SP_RESERVED_IF_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_reserved.enabled = false;
    assert_snippet(SP_RESERVED_IF_INCORRECT, vec![], &rules);
}

static SP_RESERVED_IF_CORRECT: &str = "
method this_is_some_method(bool flag) {
    local int this_some_integer = 0x666;
    if (this_some_integer == 0x666)
        return;

    if (this_some_integer == 0x667) {
        if (flag) {
            some_cal();
        } else {
            return;
        }
    }
}
";
#[test]
fn sp_reserved_if_correct() {
    let rules = set_up();
    assert_snippet(SP_RESERVED_IF_CORRECT, vec![], &rules);
}

static SP_RESERVED_FOR_INCORRECT: &str = "
method this_is_some_method() {
    for(local uint16 i = 0; i < SOME_CONST; i++) {
        local uint16 j;
        for(j = 0; j < OTHER_CONST; j++) {
            local uint64 offset = some_var[i][j];
            write(offset, 4, val);
        }
    }
}
";
#[test]
fn sp_reserved_for_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpReserved,
        (2, 2, 4, 8),
        (4, 4, 8, 12),
    );
    assert_snippet(SP_RESERVED_FOR_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_reserved.enabled = false;
    assert_snippet(SP_RESERVED_FOR_INCORRECT, vec![], &rules);
}

static SP_RESERVED_FOR_CORRECT: &str = "
method this_is_some_method() {
    for (local uint16 i = 0; i < SOME_CONST; i++) {
        local uint16 j;
        for (j = 0; j < OTHER_CONST; j++) {
            local uint64 offset = some_var[i][j];
            write(offset, 4, val);
        }
    }
}
";
#[test]
fn sp_reserved_for_correct() {
    let rules = set_up();
    assert_snippet(SP_RESERVED_FOR_CORRECT, vec![], &rules);
}

static SP_RESERVED_WHILE_INCORRECT: &str = "
method this_is_some_method() {
    local uint16 i = 0;
    while(i < SOMETHING) {
        some_fun();
        if (some_expression) {
            local uint16 j = 0;
            while(j < SOMETHING) {
                some_other_fun();
                j++;
            }
        } else {
            break;
        }
        i++;
    }
}
";
#[test]
fn sp_reserved_while_incorrect() {
    let mut rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::SpReserved,
        (3, 3, 4, 10),
        (7, 7, 12, 18),
    );
    assert_snippet(SP_RESERVED_WHILE_INCORRECT, expected_errors, &rules);
    // Test rule disable
    rules.sp_reserved.enabled = false;
    assert_snippet(SP_RESERVED_WHILE_INCORRECT, vec![], &rules);
}

static SP_RESERVED_WHILE_CORRECT: &str = "
method this_is_some_method() {
    local uint16 i = 0;
    while (i < SOMETHING) {
        some_fun();
        if (some_expression) {
            local uint16 j = 0;
            while (j < SOMETHING) {
                some_other_fun();
                j++;
            }
        } else {
            break;
        }
        i++;
    }
}
";
#[test]
fn sp_reserved_while_correct() {
    let rules = set_up();
    assert_snippet(SP_RESERVED_WHILE_CORRECT, vec![], &rules);
}

use crate::lint::rules::tests::common::{set_up, robust_assert_snippet as assert_snippet};
use crate::lint::rules::RuleType;

static EMPTY_LOOP_INCORRECT: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
    ;
}
";
#[test]
fn empty_loop_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN10,
        (2, 3, 4, 5),
    );
    assert_snippet(EMPTY_LOOP_INCORRECT, expected_errors, &rules);
}

static EMPTY_LOOP_INCORRECT_2: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
            ;
}
";
#[test]
fn empty_loop_incorrect_2() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN10,
        (2, 3, 4, 13),
    );
    assert_snippet(EMPTY_LOOP_INCORRECT_2, expected_errors, &rules);
}

static EMPTY_LOOP_INCORRECT_3: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
    ;
    s++;
}
";
#[test]
fn empty_loop_incorrect_3() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN10,
        (3, 4, 4, 5),
    );
    assert_snippet(EMPTY_LOOP_INCORRECT_3, expected_errors, &rules);
}

static EMPTY_LOOP_CORRECT: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
        ;
}
";
#[test]
fn empty_loop_correct() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_CORRECT, vec![], &rules);
}

static EMPTY_LOOP_CORRECT_2: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
        ;
    s++;
}
";
#[test]
fn empty_loop_correct_2() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_CORRECT_2, vec![], &rules);
}

static NESTED_LOOP_INCORRECT: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
                ;
    }
}
";
#[test]
fn nested_loop_incorrect() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::IN10,
        (3, 4, 8, 17),
    );
    assert_snippet(NESTED_LOOP_INCORRECT, expected_errors, &rules);
}

static NESTED_LOOP_CORRECT: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
            ;
    }
}
";
#[test]
fn nested_loop_correct() {
    let rules = set_up();
    assert_snippet(NESTED_LOOP_CORRECT, vec![], &rules);
}

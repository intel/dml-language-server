use crate::lint::rules::tests::common::{set_up, assert_snippet};

static EMPTY_LOOP_INCORRECT: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
    ;
}
";

static EMPTY_LOOP_INCORRECT_2: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
            ;
}
";

static EMPTY_LOOP_INCORRECT_3: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
    ;
    s++;
}
";

static EMPTY_LOOP_CORRECT: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
        ;
}
";

static EMPTY_LOOP_CORRECT_2: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
        ;
    s++;
}
";

static NESTED_LOOP_INCORRECT: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
                ;
    }
}
";

static NESTED_LOOP_CORRECT: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
            ;
    }
}
";

#[test]
fn empty_loop_incorrect() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_INCORRECT, 1, &rules);
}

#[test]
fn empty_loop_incorrect_2() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_INCORRECT_2, 1, &rules);
}

#[test]
fn empty_loop_incorrect_3() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_INCORRECT_3, 1, &rules);
}

#[test]
fn empty_loop_correct() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_CORRECT, 0, &rules);
}

#[test]
fn empty_loop_correct_2() {
    let rules = set_up();
    assert_snippet(EMPTY_LOOP_CORRECT_2, 0, &rules);
}

#[test]
fn nested_loop_incorrect() {
    let rules = set_up();
    assert_snippet(NESTED_LOOP_INCORRECT, 1, &rules);
}

#[test]
fn nested_loop_correct() {
    let rules = set_up();
    assert_snippet(NESTED_LOOP_CORRECT, 0, &rules);
}

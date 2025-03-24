use crate::lint::rules::tests::common::{set_up, assert_snippet};

pub static IN10_EMPTY_LOOP_INCORRECT: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
    ;
}
";

pub static IN10_EMPTY_LOOP_INCORRECT_2: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
            ;
}
";

pub static IN10_EMPTY_LOOP_INCORRECT_3: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
    ;
    s++;
}
";

pub static IN10_EMPTY_LOOP_OK: &str = "
method some_function() {
    for (s = 0; (1 << s) < x; s++)
        ;
}
";

pub static IN10_EMPTY_LOOP_OK_2: &str = "
method some_function(int x) {
    local uint64 s = 0;
    while (s < x)
        ;
    s++;
}
";

pub static IN10_NESTED_LOOP_INCORRECT: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
                ;
    }
}
";

pub static IN10_NESTED_LOOP_OK: &str = "
method some_function() {
    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++)
            ;
    }
}
";

#[test]
fn in10_empty_loop() {
    let rules = set_up();

    assert_snippet(IN10_EMPTY_LOOP_INCORRECT, 1, &rules);
    assert_snippet(IN10_EMPTY_LOOP_INCORRECT_2, 1, &rules);
    assert_snippet(IN10_EMPTY_LOOP_INCORRECT_3, 1, &rules);
    assert_snippet(IN10_EMPTY_LOOP_OK, 0, &rules);
    assert_snippet(IN10_EMPTY_LOOP_OK_2, 0, &rules);
    assert_snippet(IN10_NESTED_LOOP_INCORRECT, 1, &rules);
    assert_snippet(IN10_NESTED_LOOP_OK, 0, &rules);
}

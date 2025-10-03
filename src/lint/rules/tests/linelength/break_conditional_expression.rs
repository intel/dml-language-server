use crate::lint::rules::tests::common::{set_up, assert_snippet};
use crate::lint::rules::RuleType;


static BEFORE_QUESTION_MARK_CORRECT: &str = "
method bootprep_type_to_string(uint8 prep_type) -> (const char*) {
    return
        prep_type == PREP_GENERAL
        ? \"PrepGeneral\" : \"PrepEarly\";
}";
#[test]
fn before_question_mark_correct() {
    let rules = set_up();
    let expected_errors = vec![];
    assert_snippet(BEFORE_QUESTION_MARK_CORRECT, expected_errors, &rules);
}

static BEFORE_COLON_AND_QUESTION_MARK_CORRECT: &str = "
method bootprep_type_to_string(uint8 prep_type) -> (const char*) {
    return
        prep_type == PREP_GENERAL
        ? \"PrepGeneral\"
        : \"PrepEarly\";
}";
#[test]
fn after_colon_and_question_mark_correct() {
    let rules = set_up();
    let expected_errors = vec![];
    assert_snippet(BEFORE_COLON_AND_QUESTION_MARK_CORRECT, expected_errors, &rules);
}

static ONLY_BEFORE_COLON: &str = "
method harvest_resource() {
    harvest_resource(my->gas < GAS_THRESHOLD ? Rsrc_Gas
                     : (my->minerals < MINERAL_THRESHOLD
                        ? Rsrc_Minerals : nearest_resource()));
}";
#[test]
fn only_before_colon() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL3,
        (3, 3, 21, 22),
    );
    assert_snippet(ONLY_BEFORE_COLON, expected_errors, &rules);
}

static NESTED_CORRECT: &str = "
method bootprep_type_to_string(uint8 prep_type) -> (const char*) {
    return
        prep_type == PREP_GENERAL
        ? \"PrepGeneral\"
        : prep_type == PREP_EARLY
        ? \"PrepEarly\" : \"UnknownBootPrepType\";
}";
#[test]
fn nested_correct() {
    let rules = set_up();
    let expected_errors = vec![];
    assert_snippet(NESTED_CORRECT, expected_errors, &rules);
}

static AFTER_OPERATORS_NESTED: &str = "
method bootprep_type_to_string(uint8 prep_type) -> (const char*) {
    return
        prep_type == PREP_GENERAL ?
        \"PrepGeneral\" :
        prep_type == PREP_EARLY ? \"PrepEarly\" :
        \"UnknownBootPrepType\";
}";
#[test]
fn after_operators_nested() {
    let rules = set_up();
    let expected_errors = define_expected_errors!(
        RuleType::LL3,
        (3, 3, 34, 35),
        (4, 4, 22, 23),
        (5, 5, 46, 47),
    );
    assert_snippet(AFTER_OPERATORS_NESTED, expected_errors, &rules);
}


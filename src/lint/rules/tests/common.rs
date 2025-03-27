use crate::lint::{LintCfg, begin_style_check};
use crate::lint::rules::{CurrentRules, instantiate_rules};
use crate::lint::tests::create_ast_from_snippet;
use crate::analysis::LocalDMLError;
use crate::vfs::Error;

pub fn run_linter(source_code: &str, rules: &CurrentRules)
    -> Result<Vec<LocalDMLError>, Error>
{
    print!("\nSnippet to test on:\n{}\n", source_code);
    let ast = create_ast_from_snippet(source_code);
    print!("Resulting AST:\n{:#?}\n", ast);
    begin_style_check(ast, source_code.to_string(), rules)
}

pub fn assert_snippet(source_code: &str, expected_errors: usize, rules: &CurrentRules) {
    let lint_errors = run_linter(source_code, rules);
    assert!(lint_errors.is_ok());
    assert_eq!(lint_errors.clone().unwrap().len(), expected_errors,
               "{:#?}", lint_errors);
}

pub fn set_up() -> CurrentRules {
    let cfg = LintCfg::default();
    instantiate_rules(&cfg)
}

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use lazy_static::lazy_static;
use log::{debug, error, trace};
use serde::{Deserialize, Serialize};
use regex::Regex;
use rules::{instantiate_rules, CurrentRules, RuleType};
use rules::{spacing::{SpReservedOptions,
                      SpBraceOptions,
                      SpPunctOptions,
                      SpBinopOptions,
                      NspFunparOptions,
                      SpTernaryOptions,
                      SpPtrDeclOptions,
                      NspPtrDeclOptions,
                      NspInparenOptions,
                      NspUnaryOptions,
                      NspTrailingOptions},
            indentation::{LongLineOptions, IndentSizeOptions, IndentCodeBlockOptions,
                          IndentNoTabOptions, IndentClosingBraceOptions, IndentParenExprOptions,
                          IndentSwitchCaseOptions, IndentEmptyLoopOptions},
                    };
use crate::analysis::{DMLError, IsolatedAnalysis, LocalDMLError, ZeroRange};
use crate::analysis::parsing::tree::TreeElement;
use crate::file_management::CanonPath;
use crate::vfs::{Error, TextFile};
use crate::analysis::parsing::structure::TopAst;
use crate::lint::rules::indentation::{MAX_LENGTH_DEFAULT,
                                      INDENTATION_LEVEL_DEFAULT,
                                      setup_indentation_size
                                    };
use crate::server::{maybe_notify_unknown_lint_fields, Output};                                    

pub fn parse_lint_cfg(path: PathBuf) -> Result<(LintCfg, Vec<String>), String> {
    debug!("Reading Lint configuration from {:?}", path);
    let file_content = fs::read_to_string(path).map_err(|e| e.to_string())?;
    trace!("Content is {:?}", file_content);
    
    let val: serde_json::Value = serde_json::from_str(&file_content)
        .map_err(|e| e.to_string())?;
    
    let mut unknowns = Vec::new();
    let cfg = LintCfg::try_deserialize(&val, &mut unknowns)?;
    
    Ok((cfg, unknowns))
}

pub fn maybe_parse_lint_cfg<O: Output>(path: PathBuf, out: &O) -> Option<LintCfg> {
    match parse_lint_cfg(path) {
        Ok((mut cfg, unknowns)) => {
            // Send visible warning to client
            maybe_notify_unknown_lint_fields(out, &unknowns);
            setup_indentation_size(&mut cfg);
            Some(cfg)
        },
        Err(e) => {
            error!("Failed to parse linting CFG: {}", e);
            None
        }
    }
}



#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct LintCfg {
    #[serde(default)]
    pub sp_reserved: Option<SpReservedOptions>,
    #[serde(default)]
    pub sp_brace: Option<SpBraceOptions>,
    #[serde(default)]
    pub sp_punct: Option<SpPunctOptions>,
    #[serde(default)]
    pub sp_binop: Option<SpBinopOptions>,
    #[serde(default)]
    pub sp_ternary: Option<SpTernaryOptions>,
    #[serde(default)]
    pub sp_ptrdecl: Option<SpPtrDeclOptions>,
    #[serde(default)]
    pub nsp_ptrdecl: Option<NspPtrDeclOptions>,
    #[serde(default)]
    pub nsp_funpar: Option<NspFunparOptions>,
    #[serde(default)]
    pub nsp_inparen: Option<NspInparenOptions>,
    #[serde(default)]
    pub nsp_unary: Option<NspUnaryOptions>,
    #[serde(default)]
    pub nsp_trailing: Option<NspTrailingOptions>,
    #[serde(default)]
    pub long_lines: Option<LongLineOptions>,
    #[serde(default)]
    pub indent_size: Option<IndentSizeOptions>,
    #[serde(default)]
    pub indent_no_tabs: Option<IndentNoTabOptions>,
    #[serde(default)]
    pub indent_code_block: Option<IndentCodeBlockOptions>,
    #[serde(default)]
    pub indent_closing_brace: Option<IndentClosingBraceOptions>,
    #[serde(default)]
    pub indent_paren_expr: Option<IndentParenExprOptions>,
    #[serde(default)]
    pub indent_switch_case: Option<IndentSwitchCaseOptions>,
    #[serde(default)]
    pub indent_empty_loop: Option<IndentEmptyLoopOptions>,
    #[serde(default = "get_true")]
    pub annotate_lints: bool,
}

impl LintCfg {
    pub fn try_deserialize(
        val: &serde_json::Value,
        unknowns: &mut Vec<String>,
    ) -> Result<LintCfg, String> {
        // Use serde_ignored to automatically track unknown fields
        match serde_ignored::deserialize(val, |json_field| {
            unknowns.push(json_field.to_string());
        }) {
            Ok(cfg) => Ok(cfg),
            Err(e) => Err(e.to_string()),
        }
    }
}

fn get_true() -> bool {
    true
}

impl Default for LintCfg {
    fn default() -> LintCfg {
        LintCfg {
            sp_reserved: Some(SpReservedOptions{}),
            sp_brace: Some(SpBraceOptions{}),
            sp_punct: Some(SpPunctOptions{}),
            sp_binop: Some(SpBinopOptions{}),
            sp_ternary: Some(SpTernaryOptions{}),
            sp_ptrdecl: Some(SpPtrDeclOptions{}),
            nsp_ptrdecl: Some(NspPtrDeclOptions{}),
            nsp_funpar: Some(NspFunparOptions{}),
            nsp_inparen: Some(NspInparenOptions{}),
            nsp_unary: Some(NspUnaryOptions{}),
            nsp_trailing: Some(NspTrailingOptions{}),
            long_lines: Some(LongLineOptions{max_length: MAX_LENGTH_DEFAULT}),
            indent_size: Some(IndentSizeOptions{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            indent_no_tabs: Some(IndentNoTabOptions{}),
            indent_code_block: Some(IndentCodeBlockOptions{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            indent_closing_brace: Some(IndentClosingBraceOptions{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            indent_paren_expr: Some(IndentParenExprOptions{}),
            indent_switch_case: Some(IndentSwitchCaseOptions{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            indent_empty_loop: Some(IndentEmptyLoopOptions{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            annotate_lints: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DMLStyleError {
    pub error: LocalDMLError,
    pub rule_ident: &'static str,
    pub rule_type: RuleType,
}

#[derive(Debug, Clone)]
pub struct LinterAnalysis {
    pub path: CanonPath,
    pub errors: Vec<DMLError>,
}

impl fmt::Display for LinterAnalysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LinterAnalysis {{")?;
        writeln!(f, "\tpath: {}", self.path.as_str())?;
        writeln!(f, "\n}}")?;
        Ok(())
    }
}

impl LinterAnalysis {
    pub fn new(path: &Path, file: TextFile, cfg: LintCfg,  original_analysis: IsolatedAnalysis)
               -> Result<LinterAnalysis, Error> {
        debug!("local linting for: {:?}", path);
        let canonpath: CanonPath = path.into();
        let rules =  instantiate_rules(&cfg);
        let local_lint_errors = begin_style_check(original_analysis.ast, &file.text, &rules)?;
        let mut lint_errors = vec![];
        for entry in local_lint_errors {
            let ident = entry.rule_ident;
            let mut local_err = entry.error.warning_with_file(path);
            if cfg.annotate_lints {
                local_err.description = format!("{}: {}",
                                            ident,
                                                local_err.description);
            }
            lint_errors.push(local_err);
        }

        let res = LinterAnalysis {
            path: canonpath,
            errors: lint_errors,
        };
        debug!("Produced an isolated linter: {}", res);
        Ok(res)
    }
}

pub fn begin_style_check(ast: TopAst, file: &str, rules: &CurrentRules) -> Result<Vec<DMLStyleError>, Error> {
    let (mut invalid_lint_annot, lint_annot) = obtain_lint_annotations(file);
    let mut linting_errors: Vec<DMLStyleError> = vec![];
    ast.style_check(&mut linting_errors, rules, AuxParams { depth: 0 });

    // Per line checks
    let lines: Vec<&str> = file.lines().collect();
    for (row, line) in lines.iter().enumerate() {
        rules.indent_no_tabs.check(row, line, &mut linting_errors);
        rules.long_lines.check(row, line, &mut linting_errors);
        rules.nsp_trailing.check(row, line, &mut linting_errors);
    }

    // Do this _before_ post-process, since post-process may incorrectly
    // remove errors based on disabled lints
    remove_disabled_lints(&mut linting_errors, lint_annot);
    post_process_linting_errors(&mut linting_errors);

    linting_errors.append(&mut invalid_lint_annot);
    Ok(linting_errors)
}

// NOTE: this could in theory be expanded to allow us to settings for specific
// lints from specific lines/files
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum LintAnnotation {
    Allow(RuleType),
}

#[derive(Debug, Clone, Default, PartialEq)]
struct LintAnnotations {
    line_specific: HashMap<u32, HashSet<LintAnnotation>>,
    whole_file: HashSet<LintAnnotation>,
}

lazy_static! {
    // matches <non-comment?> // dml-lint: <OPERATION> = <IDENT>
    static ref LINT_ANNOTATION: Regex = Regex::new(
        r"^(.*)\/\/\s*dml-lint:\s+([a-z-A-Z]+)\s*=\s*([^\s]+)\s*$")
        .unwrap();
    static ref JUST_WHITESPACE: Regex = Regex::new(r"^\s*$").unwrap();
}

fn obtain_lint_annotations(file: &str) -> (Vec<DMLStyleError>,
                                           LintAnnotations) {
    let mut annotations = LintAnnotations::default();
    let mut incorrect_annotations = vec![];
    // In order to allow stacking of lint annotations, we store them
    // in a set, stacking them up until we find a line without a leading
    // annotation
    let mut unapplied_annotations: HashSet<LintAnnotation> = HashSet::default();
    enum Operation {
        Allow,
        AllowForFile,
    }
    fn apply_annotations(row: u32,
                         to_apply: &mut HashSet<LintAnnotation>,
                         insert_into: &mut HashMap<u32,
                                                   HashSet<LintAnnotation>>) {
        if to_apply.is_empty() {
            return;
        }
        let mut new_hashset = HashSet::default();
        std::mem::swap(&mut new_hashset, to_apply);
        insert_into.insert(row, new_hashset);
    }

    let mut last_line = 0;
    for (row, line) in file.lines().enumerate() {
        last_line = row;
        if let Some(capture) = LINT_ANNOTATION.captures(line) {
            let has_pre = capture.get(1)
                .map_or(false,
                        |m|!m.is_empty() &&
                        JUST_WHITESPACE.captures(m.as_str()).is_none());
            let op_capture = capture.get(2).unwrap();
            let operation = match op_capture.as_str() {
                "allow" => Operation::Allow,
                "allow-file" => Operation::AllowForFile,
                c => {
                    incorrect_annotations.push(DMLStyleError {
                        error: LocalDMLError {
                            range: ZeroRange::from_u32(
                                row as u32,
                                row as u32,
                                op_capture.start() as u32,
                                op_capture.end() as u32),
                            description: format!(
                                "Invalid command '{}' in dml-lint \
                                 annotation.", c),
                        },
                        rule_ident: "LintCfg",
                        rule_type: RuleType::Configuration,
                    });
                    continue;
                },
            };
            let target_capture = capture.get(3).unwrap();
            let Ok(target) = RuleType::from_str(target_capture.as_str())
            else {
                incorrect_annotations.push(DMLStyleError {
                    error: LocalDMLError {
                        range: ZeroRange::from_u32(
                            row as u32,
                            row as u32,
                            target_capture.start() as u32,
                            target_capture.end() as u32),
                        description: format!(
                            "Invalid lint rule target '{}'.",
                            target_capture.as_str()),
                    },
                    rule_ident: "LintCfg",
                    rule_type: RuleType::Configuration,
                });
                continue;
            };
            match operation {
                Operation::AllowForFile => {
                    annotations.whole_file.insert(
                        LintAnnotation::Allow(target));
                },
                Operation::Allow => {
                    unapplied_annotations.insert(
                        LintAnnotation::Allow(target));
                    if has_pre {
                        apply_annotations(row as u32,
                                          &mut unapplied_annotations,
                                          &mut annotations.line_specific);
                    }
                },
            }
        } else if JUST_WHITESPACE.captures(line).is_none() {
            apply_annotations(row as u32,
                              &mut unapplied_annotations,
                              &mut annotations.line_specific);
        }
    }
    if !unapplied_annotations.is_empty() {
        // TODO: the range of this warning could be improved to cover the actual
        // range of the unapplied annotations
        incorrect_annotations.push(DMLStyleError {
            error: LocalDMLError {
                range: ZeroRange::from_u32(
                    last_line as u32,
                    last_line as u32,
                    0, 0),
                description: "dml-lint annotations without effect at \
                              end of file."
                    .to_string(),
            },
            rule_ident: "LintCfg",
            rule_type: RuleType::Configuration,
        });
    }
    (incorrect_annotations, annotations)
}

fn post_process_linting_errors(errors: &mut Vec<DMLStyleError>) {
    // Collect indent_no_tabs ranges
    let indent_no_tabs_ranges: Vec<_> = errors.iter()
        .filter(|style_err| style_err.rule_type == RuleType::IN2)
        .map(|style_err| style_err.error.range)
        .collect();

    // Remove linting errors that are in indent_no_tabs rows
    errors.retain(|style_err| {
        !indent_no_tabs_ranges.iter().any(|range|
            (range.row_start == style_err.error.range.row_start || range.row_end == style_err.error.range.row_end)
            && style_err.rule_type != RuleType::IN2)
    });
}

fn remove_disabled_lints(errors: &mut Vec<DMLStyleError>,
                         annotations: LintAnnotations) {
    errors.retain(
        |error| {
            !annotations.whole_file.contains(
                &LintAnnotation::Allow(error.rule_type)) &&
                !annotations.line_specific.get(&error.error.range.row_start.0)
                .map_or(false, |annots|annots.contains(
                    &LintAnnotation::Allow(error.rule_type)))
        }
    );
}


// AuxParams is an extensible struct.
// It can be used for any data that needs
// to be passed down the tree nodes
// to where Rules can use such data.
#[derive(Copy, Clone)]
pub struct AuxParams {
    // depth is used by the indentation rules for calculating
    // the correct indentation level for a node in the AST.
    // Individual nodes update depth to affect level of their
    // nested TreeElements. See more in src/lint/README.md
    pub depth: u32,
}

pub mod rules;
pub mod tests {
    use std::path::Path;
    use std::str::FromStr;
    use crate::{analysis::{parsing::{parser::FileInfo, structure::{self, TopAst}}, FileSpec}, vfs::TextFile};

    pub fn create_ast_from_snippet(source: &str) -> TopAst {
        use logos::Logos;
        use crate::analysis::parsing::lexer::TokenKind;
        use crate::analysis::parsing::parser::FileParser;
        let lexer = TokenKind::lexer(source);
        let mut fparser = FileParser::new(lexer);
        let mut parse_state = FileInfo::default();
        let file_result =  &TextFile::from_str(source);
        assert!(file_result.is_ok());
        let file = file_result.clone().unwrap();
        let filespec = FileSpec {
            path: Path::new("test.txt"), file: &file
        };
        structure::parse_toplevel(&mut fparser, &mut parse_state, filespec)
    }

    // Tests both that the example Cfg parses, and that it is the default Cfg
    pub static EXAMPLE_CFG: &str = "/example_files/example_lint_cfg.json";
    #[test]
    fn test_example_lintcfg() {
        use crate::lint::{parse_lint_cfg, LintCfg};
        let example_path = format!("{}{}",
                                   env!("CARGO_MANIFEST_DIR"),
                                   EXAMPLE_CFG);
<<<<<<< HEAD
        let (example_cfg, unknowns) = parse_lint_cfg(example_path.into()).unwrap();
=======
        let example_cfg = parse_lint_cfg(example_path.into()).unwrap();
        println!("Example LintCfg: {:#?}", example_cfg);
        println!("LintCfg::default(): {:#?}", LintCfg::default());
>>>>>>> 736fdc7 (Add SP Reserved spacing rule (#21))
        assert_eq!(example_cfg, LintCfg::default());
        // Assert that there are no unknown fields in the example config:
        assert!(unknowns.is_empty(), "Example config should not have unknown fields: {:?}", unknowns);
    }

    #[test]
    fn test_unknown_fields_detection() {
        use crate::lint::LintCfg;
        
        // JSON with unknown fields
        let json_with_unknowns = r#"{
            "sp_brace": {},
            "unknown_field_1": true,
            "indent_size": {"indentation_spaces": 4},
            "another_unknown": "value"
        }"#;
        
        let val: serde_json::Value = serde_json::from_str(json_with_unknowns).unwrap();
        let mut unknowns = Vec::new();
        let result = LintCfg::try_deserialize(&val, &mut unknowns);
        
        assert!(result.is_ok());
        let cfg = result.unwrap();
        
        // Assert that unknown fields were detected
        assert_eq!(unknowns.len(), 2);
        assert!(unknowns.contains(&"unknown_field_1".to_string()));
        assert!(unknowns.contains(&"another_unknown".to_string()));
        
        // Assert the final LintCfg matches expected json (the known fields)
        let expected_json = r#"{
            "sp_brace": {},
            "indent_size": {"indentation_spaces": 4}
        }"#;
        let expected_val: serde_json::Value = serde_json::from_str(expected_json).unwrap();
        let mut expected_unknowns = Vec::new();
        let expected_cfg = LintCfg::try_deserialize(&expected_val, &mut expected_unknowns).unwrap();
        
        assert_eq!(cfg, expected_cfg);
        assert!(expected_unknowns.is_empty()); // No unknown fields in the expected config
    }

    #[test]
    fn test_annotation_parse() {
        use super::*;
        use crate::lint::rules::indentation::*;
        use crate::lint::rules::Rule;

        let source = "// dml-lint: allow=long_lines
                      // dml-lint: allow-file=indent_empty_loop
                      // dml-lint: allow=indent_switch_case
                      // dml-lint: allow=unknown_rule
                      // dml-lint: configure=not_valid
                      local int foo = 5;
                      local bool bar = 0; // dml-lint: allow=indent_paren_expr
                      // dml-lint: allow=long_lines";
        let (errs, mut annotations) = obtain_lint_annotations(source);
        let simplified_errs = errs.into_iter()
            .map(|err|
                 {
                     let DMLStyleError {
                         rule_ident,
                         rule_type,
                         error: LocalDMLError {
                             range,
                             description
                         },
                     } = err;
                     assert_eq!(rule_ident, "LintCfg");
                     assert_eq!(rule_type, RuleType::Configuration);
                     assert_eq!(range.row_start, range.row_end);
                     (range.row_start.0, description)
                 })
            .collect::<Vec<_>>();
        assert_eq!(simplified_errs,
                   vec![(3, "Invalid lint rule target 'unknown_rule'.".to_string()),
                        (4, "Invalid command 'configure' in dml-lint \
                             annotation.".to_string()),
                        (7, "dml-lint annotations without effect at \
                             end of file.".to_string())]);
        assert_eq!(annotations.whole_file.iter().collect::<Vec<_>>(),
                   vec![
                       &LintAnnotation::Allow(
                           IndentEmptyLoopRule::get_rule_type())
                   ]);
        let mut line5 = annotations.line_specific.remove(&5).unwrap();
        let mut line6 = annotations.line_specific.remove(&6).unwrap();
        assert!(annotations.line_specific.is_empty());
        for expected in [
            LintAnnotation::Allow(LongLinesRule::get_rule_type()),
            LintAnnotation::Allow(IndentSwitchCaseRule::get_rule_type())
        ] {
            assert!(line5.remove(&expected));
        }
        assert!(line5.is_empty());
        for expected in [
            LintAnnotation::Allow(IndentParenExprRule::get_rule_type()),
        ] {
            assert!(line6.remove(&expected));
        }
        assert!(line6.is_empty());
    }

    #[test]
    fn test_annotation_apply() {
        use crate::lint::rules::indentation::*;
        use crate::lint::rules::tests::common::{
            ExpectedDMLStyleError,
            set_up, assert_snippet
        };
        use crate::lint::rules::Rule;
        use crate::analysis::ZeroRange;

        env_logger::init();

        let source =
            "
dml 1.4;

// dml-lint: allow-file=nsp_unary

method my_method() {
    if (true ++) {
    // dml-lint: allow=indent_closing_brace
        return; }
    if (true) {
        return; } // dml-lint: allow=indent_closing_brace
    if (true) {
        return; }
}}
// dml-lint: allow=long_lines
method my_method() { /* now THIS is a long line. but we will allow it just this once*/ }
method my_method() { /* however, this long line will not be allowed even though its the same*/ }
";
        assert_snippet(source,
                       vec![
                           ExpectedDMLStyleError {
                               range: ZeroRange::from_u32(12,12,16,17),
                               rule_type: IndentClosingBraceRule::get_rule_type(),
                           },
                           ExpectedDMLStyleError {
                               range: ZeroRange::from_u32(16,16,80,96),
                               rule_type: LongLinesRule::get_rule_type(),
                           }],
                       &set_up()
        );
    }
}

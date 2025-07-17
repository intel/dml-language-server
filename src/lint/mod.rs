use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use log::{debug, error, trace};
use serde::{Deserialize, Serialize};
use rules::{instantiate_rules, CurrentRules, RuleType};
use rules::{spacing::{SpBraceOptions, SpPunctOptions, SpBinopOptions, NspFunparOptions,
                      SpTernaryOptions, SpPtrDeclOptions, NspPtrDeclOptions,
                      NspInparenOptions, NspUnaryOptions, NspTrailingOptions},
                      indentation::{LongLineOptions, IndentSizeOptions, IndentCodeBlockOptions,
                                    IndentNoTabOptions, IndentClosingBraceOptions, IndentParenExprOptions, IndentSwitchCaseOptions, IndentEmptyLoopOptions},
                    };
use crate::analysis::{DMLError, IsolatedAnalysis, LocalDMLError};
use crate::analysis::parsing::tree::TreeElement;
use crate::file_management::CanonPath;
use crate::vfs::{Error, TextFile};
use crate::analysis::parsing::structure::TopAst;
use crate::lint::rules::indentation::{MAX_LENGTH_DEFAULT,
                                      INDENTATION_LEVEL_DEFAULT,
                                      setup_indentation_size
                                    };

pub fn parse_lint_cfg(path: PathBuf) -> Result<LintCfg, String> {
    debug!("Reading Lint configuration from {:?}", path);
    let file_content = fs::read_to_string(path).map_err(
        |e|e.to_string())?;
    trace!("Content is {:?}", file_content);
    serde_json::from_str(&file_content)
        .map_err(|e|e.to_string())
}

pub fn maybe_parse_lint_cfg(path: PathBuf) -> Option<LintCfg> {
    match parse_lint_cfg(path) {
        Ok(mut cfg) => {
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
#[serde(deny_unknown_fields)]
pub struct LintCfg {
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

fn get_true() -> bool {
    true
}

impl Default for LintCfg {
    fn default() -> LintCfg {
        LintCfg {
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
        let local_lint_errors = begin_style_check(original_analysis.ast, file.text, &rules)?;
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

pub fn begin_style_check(ast: TopAst, file: String, rules: &CurrentRules) -> Result<Vec<DMLStyleError>, Error> {
    let mut linting_errors: Vec<DMLStyleError> = vec![];
    ast.style_check(&mut linting_errors, rules, AuxParams { depth: 0 });      

    // Per line checks
    let lines: Vec<&str> = file.lines().collect();
    for (row, line) in lines.iter().enumerate() {
        rules.indent_no_tabs.check(&mut linting_errors, row, line);
        rules.long_lines.check(&mut linting_errors, row, line);
        rules.nsp_trailing.check(&mut linting_errors, row, line);
    }

    post_process_linting_errors(&mut linting_errors);

    Ok(linting_errors)
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
        let example_cfg = parse_lint_cfg(example_path.into()).unwrap();
        assert_eq!(example_cfg, LintCfg::default());
    }
}

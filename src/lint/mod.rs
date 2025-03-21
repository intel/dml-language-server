use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use log::{debug, error, trace};
use serde::{Deserialize, Serialize};
use rules::{instantiate_rules, CurrentRules, RuleType};
use rules::{spacing::{SpBraceOptions, SpPunctOptions, NspFunparOptions,
                      NspInparenOptions, NspUnaryOptions, NspTrailingOptions},
                      indentation::{LongLineOptions, IN1Options, IN3Options,
                                    IN2Options, IN9Options, IN6Options},
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
    pub in1: Option<IN1Options>,
    #[serde(default)]
    pub in2: Option<IN2Options>,
    #[serde(default)]
    pub in3: Option<IN3Options>,
    #[serde(default)]
    pub in6: Option<IN6Options>,
    #[serde(default)]
    pub in9: Option<IN9Options>,
}

impl Default for LintCfg {
    fn default() -> LintCfg {
        LintCfg {
            sp_brace: Some(SpBraceOptions{}),
            sp_punct: Some(SpPunctOptions{}),
            nsp_funpar: Some(NspFunparOptions{}),
            nsp_inparen: Some(NspInparenOptions{}),
            nsp_unary: Some(NspUnaryOptions{}),
            nsp_trailing: Some(NspTrailingOptions{}),
            long_lines: Some(LongLineOptions{max_length: MAX_LENGTH_DEFAULT}),
            in1: Some(IN1Options{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            in2: Some(IN2Options{}),
            in3: Some(IN3Options{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            in6: Some(IN6Options{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
            in9: Some(IN9Options{indentation_spaces: INDENTATION_LEVEL_DEFAULT}),
        }
    }
}

pub struct DMLStyleError {
    pub error: LocalDMLError,
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
        for error in local_lint_errors {
            lint_errors.push(error.warning_with_file(path));
        }

        let res = LinterAnalysis {
            path: canonpath,
            errors: lint_errors,
        };
        debug!("Produced an isolated linter: {}", res);
        Ok(res)
    }
}

pub fn begin_style_check(ast: TopAst, file: String, rules: &CurrentRules) -> Result<Vec<LocalDMLError>, Error> {
    let mut linting_errors: Vec<DMLStyleError> = vec![];
    ast.style_check(&mut linting_errors, rules, AuxParams { depth: 0 });      

    // Per line checks
    let lines: Vec<&str> = file.lines().collect();
    for (row, line) in lines.iter().enumerate() {
        rules.in2.check(&mut linting_errors, row, line);
        rules.long_lines.check(&mut linting_errors, row, line);
        rules.nsp_trailing.check(&mut linting_errors, row, line);
    }

    // Continuation line check
    rules.in6.check(&mut linting_errors, &lines);

    post_process_linting_errors(&mut linting_errors);

    Ok(linting_errors.into_iter().map(|e| e.error).collect())
}

fn post_process_linting_errors(errors: &mut Vec<DMLStyleError>) {
    // Collect in2 ranges
    let in2_ranges: Vec<_> = errors.iter()
        .filter(|style_err| style_err.rule_type == RuleType::IN2)
        .map(|style_err| style_err.error.range)
        .collect();

    // Remove linting errors that are in in2 rows
    errors.retain(|style_err| {
        !in2_ranges.iter().any(|range|
            (range.row_start == style_err.error.range.row_start || range.row_end == style_err.error.range.row_end)
            && style_err.rule_type != RuleType::IN2)
    });
}

#[derive(Copy, Clone)]
pub struct AuxParams {
    pub depth: u32,
}


pub mod rules;
pub mod tests {
    use std::path::Path;
    use std::str::FromStr;
    use crate::{analysis::{parsing::{parser::FileInfo, structure::{self, TopAst}}, FileSpec}, vfs::TextFile};

    pub static SOURCE: &str = "
    dml 1.4;

    bank sb_cr {
        group monitor {    

            register MKTME_KEYID_MASK {
                method get() -> (uint64) {
                    local uint64 physical_address_mask = mse.srv10nm_mse_mktme.get_key_addr_mask();
                    this.Mask.set(physical_address_mask);
                    this.function_with_args('some_string',
                                    integer,
                                    floater);
                    return this.val;
                }
            }

            register TDX_KEYID_MASK {
                method get() -> (uint64) {
                    local uint64 tdx_keyid_mask = mse.srv10nm_mse_tdx.get_key_addr_mask();
                    local uint64 some_uint = (is_this_real) ? then_you_might_like_this_value : or_this_one;
                    this.Mask.set(tdx_keyid_mask);
                    return this.val;
                }
            }
        }
    }   

    /*
        This is ONEEEE VEEEEEERY LLOOOOOOONG COOOMMMEENTT ON A SINGLEEEE LINEEEEEEEEEEEEEE
        and ANOTHEEEER VEEEEEERY LLOOOOOOONG COOOMMMEENTT ON A SINGLEEEE LINEEEEEEEEEEEEEE
    */

    ";

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

    #[test]
    #[ignore]
    fn test_main() {
        use crate::lint::{begin_style_check, LintCfg};
        use crate::lint::rules:: instantiate_rules;
        let ast = create_ast_from_snippet(SOURCE);
        let cfg = LintCfg::default();
        let rules = instantiate_rules(&cfg);
        let _lint_errors = begin_style_check(ast, SOURCE.to_string(), &rules);
        assert!(_lint_errors.is_ok());
        assert!(!_lint_errors.unwrap().is_empty());
    }
}

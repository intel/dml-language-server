//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

#[cfg(test)]
macro_rules! assert_match_destruct {
    ($e: expr, $p: pat $(, $i: ident)*) => {{
        let &$p = &$e else {
            panic!("{:?} did not match {}", $e, stringify!($p));
        };
        #[allow(clippy::unused_unit)]
        ($($i),*)
    }}
}

#[cfg(test)]
pub mod test {
    use crate::analysis::{FileSpec, LocalDMLError, TokenKind};
    use crate::analysis::parsing::parser::{
        FileParser,
        FileInfo,
        ParseContext,
        Parse,
        doesnt_understand_tokens,
    };
    use crate::analysis::parsing::tree::TreeElement;
    use crate::vfs::TextFile;

    use logos::Logos;
    use std::path::Path;
    use std::str::FromStr;

    pub fn parse_to_structure<C, PT, F, T>(source: &str, reparse_fun: F)
        -> Option<(T, Vec<LocalDMLError>)>
        where C: PartialEq + Clone + TreeElement + std::fmt::Debug + 'static,
              PT: Parse<C>,
              F: Fn(&C,
                    &mut Vec<LocalDMLError>,
                    FileSpec<'_>) -> T
    {
        let lexer = TokenKind::lexer(source);
        let mut fileparse = FileParser::new(lexer);
        let top_context = ParseContext::new_context(doesnt_understand_tokens);
        let file_info = FileInfo::default();
        let mut ast_errors = vec![];
        let file = &TextFile::from_str(source).expect("Failed to create textfile");
        let maybe_ast = PT::parse(&top_context, &mut fileparse, &file_info);
        assert!(fileparse.skipped_tokens.is_empty(), "Parse error: skipped tokens {:#?}",
                fileparse.skipped_tokens);
        let parse_tree = maybe_ast.as_actual()?;
        let ast = reparse_fun(parse_tree, &mut ast_errors, FileSpec {
            path: Path::new("TestingInFile"), file,
        });
        Some((ast, ast_errors))
    }
}

pub mod types;
pub mod statements;
pub mod objects;
pub mod expressions;
pub mod toplevel;


//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use logos::Logos;

use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::{Token, doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::parsing::tree::{AstObject, Content, TreeElement,
                                     LeafToken, MissingToken,
                                     ZeroRange, ZeroPosition};

pub fn make_ast<T: PartialEq + Clone>(range: ZeroRange, t: T) -> AstObject<T> {
    AstObject::<T> {
        range,
        content: Box::new(Content::Some(t)),
    }
}

pub fn make_token(prefixrange: ZeroRange,
                  range: ZeroRange,
                  kind: TokenKind) -> Token {
    Token { kind, prefixrange, range }
}

pub fn make_leaf(prefixrange: ZeroRange,
                 range: ZeroRange,
                 kind: TokenKind) -> LeafToken {
    LeafToken::Actual(make_token(prefixrange, range, kind))
}

pub fn make_missing(position: ZeroPosition,
                    kind: TokenKind,
                    ended_by: Option<Token>) -> LeafToken {
    LeafToken::Missing(MissingToken {
        position,
        description: kind.description(),
        ended_by
    })
}

pub fn zero_range(rowstart: u32, rowend: u32, colstart: u32, colend: u32)
              -> ZeroRange{
    ZeroRange::from_u32(rowstart, rowend, colstart, colend)
}

pub fn zero_position(row: u32, col: u32) -> ZeroPosition {
    ZeroPosition::from_u32(row, col)
}

#[allow(clippy::ptr_arg)]
pub fn test_ast_tree<C: PartialEq + Clone + TreeElement + std::fmt::Debug,
                     PT: Parse<C>>(
    source: &str,
    expected_ast: &AstObject<C>,
    expected_skipped_tokens: &Vec<Token>) {
    let lexer = TokenKind::lexer(source);
    let mut fileparse = FileParser::new(lexer);
    let top_context = ParseContext::new_context(doesnt_understand_tokens);
    let file_info = FileInfo::default();
    let got = PT::parse(&top_context, &mut fileparse, &file_info);
    assert_eq!((&got, &fileparse.skipped_tokens.iter()
                .map(|(tok, _)|*tok).collect::<Vec<Token>>()),
               (expected_ast, expected_skipped_tokens));
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use logos::Lexer;
use regex::Regex;
use log::debug;
use lazy_static::lazy_static;

use crate::span::{Range, ZeroIndexed, Position};
use crate::analysis::LocalDMLError;
use crate::analysis::provisionals::ProvisionalsManager;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{AstObject, LeafToken, MissingToken};
use crate::vfs::TextFile;

// For storing in ASTs
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    // Technically, we could store this as a position and infer the range from
    // this + first position of range. But this is more handy
    pub prefixrange: Range<ZeroIndexed>,
    pub range: Range<ZeroIndexed>,
}

impl Token {
    fn new(kind: TokenKind,
           prefixstart: Position<ZeroIndexed>,
           start: Position<ZeroIndexed>,
           end: Position<ZeroIndexed>) -> Self {
        Token {
            kind,
            prefixrange: Range::from_positions(prefixstart, start),
            range: Range::from_positions(start, end),
        }
    }
    pub fn read_token(&self, file: &TextFile) -> String {
        // For now, assume this read works
        file.load_range(self.range).unwrap().to_string()
    }
}

pub type UnderstandsTokenFn = fn (TokenKind) -> bool;

pub fn doesnt_understand_tokens(_: TokenKind) -> bool {
    false
}

#[derive(Debug)]
pub struct ParseContext {
    // The position this context ended at
    end_position: Option<Position<ZeroIndexed>>,
    // The token this context was ended by, if any
    end_token: Option<Token>,
    understands_token: UnderstandsTokenFn,
    previous_context_understands: Vec<UnderstandsTokenFn>,
}

impl ParseContext {
    pub fn peek(&mut self, fileparser: &mut FileParser<'_>) -> Option<Token> {
        if self.end_position.is_some() {
            None
        } else {
            fileparser.peek()
        }
    }

    pub fn peek_kind(&mut self, fileparser: &mut FileParser<'_>)
                     -> Option<TokenKind> {
        if self.end_position.is_some() {
            None
        } else {
            Some(fileparser.peek()?.kind)
        }
    }

    pub fn next_if_filter(&mut self,
                          fileparser: &mut FileParser<'_>,
                          matcher: UnderstandsTokenFn)
                        -> Option<LeafToken> {
        if matcher(self.peek_kind(fileparser)?) {
            Some(self.next_leaf(fileparser))
        } else {
            None
        }
    }

    pub fn next_if_kind(&mut self, fileparser: &mut FileParser<'_>, kind: TokenKind)
                        -> Option<LeafToken> {
        if self.peek_kind(fileparser)? == kind {
            Some(self.next_leaf(fileparser))
        } else {
            None
        }
    }

    pub fn expect_peek(&mut self,
                       fileparser: &mut FileParser<'_>,
                       description: &'static str)
                       -> LeafToken {
        match self.end_position {
            Some(position) => LeafToken::Missing(MissingToken {
                position,
                description,
                ended_by: self.end_token,
            }),
            None => match fileparser.peek() {
                Some(result) => LeafToken::Actual(result),
                None =>  self.end_context(fileparser, description),
            }
        }
    }

    pub fn next(&mut self, fileparser: &mut FileParser<'_>) -> Option<Token> {
        if self.end_position.is_some() {
            None
        } else {
            fileparser.next_tok()
        }
    }

    // When you dont expect to miss, but want a leaftoken regardless
    pub fn next_leaf(&mut self, fileparser: &mut FileParser<'_>) -> LeafToken {
        self.expect_next(fileparser, "<Internal Error: expected token>")
    }

    pub fn end_context(&mut self,
                       fileparser: &mut FileParser<'_>,
                       description: &'static str) -> LeafToken {
        let peek = fileparser.peek();
        let (endpos, endtok) = match &peek {
            Some(tok) => {
                debug!("Ended context at {:?}: got {}, expected {}",
                       tok.range.end(), tok.kind.description(),
                       description);
                (Some(tok.range.start()), Some(*tok))
            },
            None => {
                debug!("Ended context at {:?}: got EOF, expected {}",
                       fileparser.get_position(), description);
                (Some(fileparser.get_position()), None)
            },
        };
        self.end_position = endpos;
        self.end_token = endtok;
        LeafToken::Missing(MissingToken {
            position: self.end_position.unwrap(),
            description,
            ended_by: peek,
        })
    }

    // Use when any token is fine, or when you have previously ensured
    // correctness of token (with peek)
    pub fn expect_next(&mut self,
                       fileparser: &mut FileParser<'_>,
                       description: &'static str) -> LeafToken {
        match self.end_position {
            Some(position) => LeafToken::Missing(MissingToken {
                position,
                description,
                ended_by: self.end_token,
            }),
            None => match fileparser.next_tok() {
                Some(token) => LeafToken::Actual(token),
                None => self.end_context(fileparser, description),
            }
        }
    }


    // Use when you want a particular token kind, and want to
    // specify the description of it
    pub fn expect_next_kind_custom(&mut self,
                                   fileparser: &mut FileParser<'_>,
                                   kind: TokenKind,
                                   description: &'static str) -> LeafToken {
        match self.end_position {
            Some(position) => LeafToken::Missing(MissingToken {
                position,
                description,
                ended_by: self.end_token,
            }),
            None => match self.peek(fileparser) {
                Some(token) =>
                    if token.kind == kind {
                        LeafToken::Actual(self.next(fileparser).unwrap())
                    } else {
                        // If some above context can handle this token,
                        // end current context, do not advance, and
                        // give a missing token
                        if self.previous_understands_token(token.kind) {
                            self.end_context(fileparser, description)
                        } else {
                            // If nobody could've handled this token,
                            // skip it and retry
                            debug!("Skipped token {} at {:?}, expected {}",
                                   token.kind.description(), token.range,
                                   description);
                            fileparser.skip(description);
                            self.expect_next_kind_custom(
                                fileparser, kind, description)
                        }
                    },
                None => self.end_context(fileparser, description),
            }
        }
    }

    // Use when you want a particular token kind
    pub fn expect_next_kind(&mut self,
                            fileparser: &mut FileParser<'_>,
                            kind: TokenKind) -> LeafToken {
        self.expect_next_kind_custom(fileparser, kind, kind.description())
    }

    // When the next token _should_ fit filter, but you dont want to
    // properly 'next' it yet. Producs a leaftoken unlike other peek
    // methods, as we want to be able to obtain position and reason
    // for failure
    pub fn expect_peek_filter(&mut self,
                              fileparser: &mut FileParser<'_>,
                              matcher: UnderstandsTokenFn,
                              description: &'static str)
                              -> LeafToken {
        match self.end_position {
            Some(position) => LeafToken::Missing(MissingToken {
                position,
                description,
                ended_by: self.end_token,
            }),
            None => match self.peek(fileparser) {
                Some(token) =>
                    if matcher(token.kind) {
                        LeafToken::Actual(token)
                    } else {
                        // If some above context can handle this token,
                        // end current context, do not advance, and
                        // give a missing token
                        if self.previous_understands_token(token.kind) {
                            self.end_context(fileparser, description)
                        } else {
                            // If nobody could've handled this token,
                            // skip it and retry
                            debug!("Skipped token {} at {:?}, expected {}",
                                   token.kind.description(), token.range,
                                   description);
                            fileparser.skip(description);
                            self.expect_peek_filter(
                                fileparser, matcher, description)
                        }
                    },
                None => self.end_context(fileparser, description),
            }
        }
    }

    // Use when you need arbitrary filter on next
    pub fn expect_next_filter(&mut self,
                              fileparser: &mut FileParser<'_>,
                              matcher: UnderstandsTokenFn,
                              description: &'static str) -> LeafToken {
        match self.end_position {
            Some(position) => LeafToken::Missing(MissingToken {
                position,
                description,
                ended_by: self.end_token,
            }),
            None => match self.peek(fileparser) {
                Some(token) =>
                    if matcher(token.kind) {
                        LeafToken::Actual(self.next(fileparser).unwrap())
                    } else {
                        // If some above context can handle this token,
                        // end current context, do not advance, and
                        // give a missing token
                        if self.previous_understands_token(token.kind) {
                            self.end_context(fileparser, description)
                        } else {
                            // If nobody could've handled this token,
                            // skip it and retry
                            debug!("Skipped token {} at {:?}, expected {}",
                                   token.kind.description(), token.range,
                                   description);
                            fileparser.skip(description);
                            self.expect_next_filter(
                                fileparser, matcher, description)
                        }
                    },
                None => self.end_context(fileparser, description),
            }
        }
    }

    pub fn enter_context(&self, understand: UnderstandsTokenFn) -> Self
    {
        let mut new_vec = self.previous_context_understands.clone();
        new_vec.push(self.understands_token);
        ParseContext {
            // Don't allow sub-contexts to 'revive' parsing
            end_position: self.end_position,
            end_token: self.end_token,
            understands_token: understand,
            previous_context_understands: new_vec,
        }
    }

    pub fn new_context(understand: UnderstandsTokenFn) -> Self
    {
        ParseContext {
            end_position: None,
            end_token: None,
            understands_token: understand,
            previous_context_understands: vec![],
        }
    }

    pub fn previous_understands_token(&self, token: TokenKind) -> bool {
        (self.previous_context_understands).iter().any(|f| f(token))
    }
}

pub struct FileParser<'a> {
    pub lexer: Lexer::<'a, TokenKind>,
    // Track the current exact position of the next unparsed token
    current_line: u32,
    current_column: u32,
    // Track the current exact end position of the previously provided
    // token
    previous_line: u32,
    previous_column: u32,
    // Buffer next here, for peeking
    next_token: Option<Token>,
    // (skipped token, expected description)
    pub skipped_tokens: Vec<(Token, &'static str)>,
}

impl <'a> FileParser<'a> {
    pub fn new(lexer: Lexer::<'a, TokenKind>) -> Self {
        let mut to_return = FileParser {
            lexer,
            current_column: 0,
            current_line: 0,
            previous_column: 0,
            previous_line: 0,
            next_token: None,
            skipped_tokens: vec![],
        };
        to_return.next_tok();
        to_return
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.next_token
    }

    fn advance(&mut self) {
        // This is how we skip whitespace and comments
        let next = self.lexer.next();
        if next.is_none() {
            self.next_token = None;
            return;
        }
        let next = next.unwrap();
        if let Ok(tokkind) = next {
            match tokkind {
                TokenKind::Newline | TokenKind::Comment => {
                    self.current_line += 1;
                    self.current_column = 0;
                    self.advance();
                },
                TokenKind::Whitespace => {
                    // TODO: this might not work correctly with tabs
                    self.current_column += self.lexer.slice().len() as u32;
                    self.advance();
                },
                TokenKind::MultilineComment => {
                    // Because we need to keep track of position through this,
                    // we need to analyze the content for newlines and ending
                    // column
                    lazy_static! {
                        static ref MULTILINE_END: Regex = Regex::new(
                            r"(\n(.*\*/))|(/\*[^\n]*\*/)").unwrap();
                    }
                    let content = self.lexer.slice();
                    self.current_line += (content.lines().count() - 1) as u32;
                    let captures = MULTILINE_END.captures(content).unwrap();
                    match (captures.get(2), captures.get(3)) {
                        (Some(case1), None) =>
                            self.current_column =
                              case1.as_str().encode_utf16().count() as u32,
                        (None, Some(case2)) =>
                            self.current_column +=
                              case2.as_str().encode_utf16().count() as u32,
                        _ => panic!("Internal Parser Error: \
                                     Broken multiline comment handling"),
                    }
                    self.advance();
                },
                TokenKind::CBlock => {
                    // Because we need to keep track of position through this,
                    // we need to analyze the content for newlines and ending
                    // column
                    lazy_static! {
                        static ref MULTILINE_END: Regex = Regex::new(
                            r"(\n(.*%\}))|(%\{[^\n]*%\})").unwrap();
                    }
                    let content = self.lexer.slice();
                    let lines = (content.lines().count() - 1) as u32;
                    let captures = MULTILINE_END.captures(content).unwrap();
                    let end_column = match (captures.get(2), captures.get(3)) {
                        (Some(case1), None) =>
                            case1.as_str().encode_utf16().count() as u32,
                        (None, Some(case2)) =>
                            self.current_column
                            + case2.as_str().encode_utf16().count() as u32,
                        _ => panic!("Internal Parser Error: \
                                     Broken cblock handling"),
                    };
                    self.next_token = Some(Token::new(
                        TokenKind::CBlock,
                        Position::<ZeroIndexed>::from_u32(self.previous_line,
                                                          self.previous_column),
                        Position::<ZeroIndexed>::from_u32(self.current_line,
                                                          self.current_column),
                        Position::<ZeroIndexed>::from_u32(self.current_line
                                                          + lines,
                                                          end_column)));
                    self.previous_line = self.current_line + lines;
                    self.current_line = self.previous_line;
                    self.previous_column = end_column;
                    self.current_column = self.previous_column;
                },
                kind => {
                    self.set_next_tok(kind);
                },
            }
        } else {
            self.set_next_tok(TokenKind::LexerError);
        }
    }

    fn set_next_tok(&mut self, kind: TokenKind) {
        let slice_len = self.lexer.slice().encode_utf16().count() as u32;
        self.next_token = Some(Token::new(
            kind,
            Position::<ZeroIndexed>::from_u32(self.previous_line,
                                              self.previous_column),
            Position::<ZeroIndexed>::from_u32(self.current_line,
                                              self.current_column),
            Position::<ZeroIndexed>::from_u32(
                self.current_line,
                self.current_column + slice_len)));
        self.previous_line = self.current_line;
        self.previous_column = self.current_column + slice_len;
        self.current_column = self.previous_column;
    }

    pub fn next_tok(&mut self) -> Option<Token> {
        let to_return = self.next_token;
        self.advance();
        to_return
    }

    pub fn skip(&mut self, expected: &'static str) {
        if let Some(to_skip) = self.next_tok() {
            self.skipped_tokens.push((to_skip, expected));
        }
    }

    pub fn get_position(&mut self) -> Position<ZeroIndexed> {
        Position::<ZeroIndexed>::from_u32(self.current_line,
                                          self.current_column)
    }

    pub fn report_skips(&self) -> Vec<LocalDMLError> {
        self.skipped_tokens.iter().map(
            |(tok, desc)|LocalDMLError {
                range: tok.range,
                description: format!("Unexpected token {}, expected {}",
                                     tok.kind.description(), desc),
            }).collect()
    }
}


#[derive(Clone, Debug, PartialEq, Default)]
pub struct FileInfo {
    pub provisionals: ProvisionalsManager,
}

// TODO: This trait seems to be of marginal convenience, we end up
// with quite a few auxiliary methods anyways
pub trait Parse<T: Clone + PartialEq> {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> AstObject<T>;
}


// TODO: the wish-fullfillment syntax would "let token = stream.expect_next()?"
// which would immediately return the correct none-filled ast object on
// missingtoken, and othewise bind the token.
// however this is, at the very least, nightly-only
macro_rules! handle_missing {
    ($leaf: expr, $followup: expr) => {
        match $leaf {
            LeafToken::Actual(token) => $followup(token),
            LeafToken::Missing(token) => {
                AstObject {
                    range: $leaf.range(),
                    content: Box::new(Content::Missing(MissingContent {
                        ended_by: token.ended_by,
                        description: token.description
                    })),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use logos::Logos;
    use crate::span::ZeroIndexed;
    use crate::analysis::parsing::tree::ZeroRange;

    macro_rules! check_parser_stream {
        ($input: expr, $ ( $e: expr),*) => {
            let lexer = TokenKind::lexer($input);
            let mut fileparse = FileParser::new(lexer);
            $(
                assert_eq!(fileparse.next_tok(), Some($e));
            )*
            assert_eq!(fileparse.next_tok(), None);
        };
    }

    #[test]
    fn simple_tokens() {
        check_parser_stream!(
            "new 588 ++ \t {",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 3),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::New,
            },
            Token {
                range: ZeroRange::from_u32(0, 0, 4, 7),
                prefixrange: ZeroRange::from_u32(0, 0, 3, 4),
                kind: TokenKind::IntConstant,
            },
            Token {
                range: ZeroRange::from_u32(0, 0, 8, 10),
                prefixrange: ZeroRange::from_u32(0, 0, 7, 8),
                kind: TokenKind::PlusPlus,
            },
            Token {
                range: ZeroRange::from_u32(0, 0, 13, 14),
                prefixrange: ZeroRange::from_u32(0, 0, 10, 13),
                kind: TokenKind::LBrace,
            }
        );
    }

    // NOTE: The server currently only supports UTF-16
    // position encoding (which is the default, and required)
    // Meaning larger-than-utf8 characters must be broken into
    // utf-16 pieces
    #[test]
    fn unicode_column_numbering() {
        // Here the 3-byte unicode char becomes 2 utf-16 chars,
        // meaning what would normally be a 8-char prefix is actually
        // 9
        check_parser_stream!(
            "/* \u{1F608} */ dml",
            Token {
                range: ZeroRange::from_u32(0, 0, 9, 12),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 9),
                kind: TokenKind::DML,
            });
    }

    #[test]
    fn string_constant() {
        check_parser_stream!(
            "\"string\" \"another\"",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 8),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::StringConstant,
            },
            Token {
                range: ZeroRange::from_u32(0, 0, 9, 18),
                prefixrange: ZeroRange::from_u32(0, 0, 8, 9),
                kind: TokenKind::StringConstant,
            }
        );
    }

    #[test]
    fn comments() {
        check_parser_stream!(
            "foo //comment\n bar",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 3),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::Identifier,
            },
            Token {
                range: ZeroRange::from_u32(1, 1, 1, 4),
                prefixrange: ZeroRange::from_u32(0, 1, 3, 1),
                kind: TokenKind::Identifier,
            }
        );
        check_parser_stream!(
            "foo /*comment*/ bar",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 3),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::Identifier,
            },
            Token {
                range: ZeroRange::from_u32(0, 0, 16, 19),
                prefixrange: ZeroRange::from_u32(0, 0, 3, 16),
                kind: TokenKind::Identifier,
            }
        );
        check_parser_stream!(
            "foo /*comment\n*/ bar",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 3),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::Identifier,
            },
            Token {
                range: ZeroRange::from_u32(1, 1, 3, 6),
                prefixrange: ZeroRange::from_u32(0, 1, 3, 3),
                kind: TokenKind::Identifier,
            }
        );
    }

    #[test]
    fn peek() {
        let lexer = TokenKind::lexer("5+");
        let mut fileparse = FileParser::new(lexer);
        assert_eq!(fileparse.peek(), Some(Token {
            range: ZeroRange::from_u32(0, 0, 0, 1),
            prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
            kind: TokenKind::IntConstant,
        }));
        assert_eq!(fileparse.next_tok(), Some(Token {
            range: ZeroRange::from_u32(0, 0, 0, 1),
            prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
            kind: TokenKind::IntConstant,
        }));
        assert_eq!(fileparse.peek(),  Some(Token {
            range: ZeroRange::from_u32(0, 0, 1, 2),
            prefixrange: ZeroRange::from_u32(0, 0, 1, 1),
            kind: TokenKind::Plus,
        }));
        assert_eq!(fileparse.next_tok(), Some(Token {
            range: ZeroRange::from_u32(0, 0, 1, 2),
            prefixrange: ZeroRange::from_u32(0, 0, 1, 1),
            kind: TokenKind::Plus,
        }));
        assert_eq!(fileparse.peek(), None);
    }

    #[test]
    fn lines() {
        check_parser_stream!(
            "foo\n(\n\t5\n)",
            Token {
                range: ZeroRange::from_u32(0, 0, 0, 3),
                prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                kind: TokenKind::Identifier,
            },
            Token {
                range: ZeroRange::from_u32(1, 1, 0, 1),
                prefixrange: ZeroRange::from_u32(0, 1, 3, 0),
                kind: TokenKind::LParen,
            },
            Token {
                range: ZeroRange::from_u32(2, 2, 1, 2),
                prefixrange: ZeroRange::from_u32(1, 2, 1, 1),
                kind: TokenKind::IntConstant,
            },
            Token {
                range: ZeroRange::from_u32(3, 3, 0, 1),
                prefixrange: ZeroRange::from_u32(2, 3, 2, 0),
                kind: TokenKind::RParen,
            }
        );
    }

    #[test]
    fn context() {
        let lexer = TokenKind::lexer("{ foo ? 5 }");
        let mut fileparse = FileParser::new(lexer);
        fn understands_rbrace(token: TokenKind) -> bool {
            token == TokenKind::RBrace
        }
        let mut context1 = ParseContext::new_context(understands_rbrace);
        assert_eq!(context1.expect_next_kind(&mut fileparse, TokenKind::LBrace),
                   LeafToken::Actual(
                       Token {
                           range: ZeroRange::from_u32(0, 0, 0, 1),
                           prefixrange: ZeroRange::from_u32(0, 0, 0, 0),
                           kind: TokenKind::LBrace,
                       }));
        let mut context2 = context1.enter_context(doesnt_understand_tokens);
        assert_eq!(context2.expect_next_kind(
            &mut fileparse, TokenKind::Identifier),
                   LeafToken::Actual(
                       Token {
                           range: ZeroRange::from_u32(0, 0, 2, 5),
                           prefixrange: ZeroRange::from_u32(0, 0, 1, 2),
                           kind: TokenKind::Identifier,
                       }));
        // The first unknown token will be transparently skipped,
        // since context1 does not understand it
        assert_eq!(context2.expect_next_kind(
            &mut fileparse, TokenKind::IntConstant),
                   LeafToken::Actual(
                       Token {
                           range: ZeroRange::from_u32(0, 0, 8, 9),
                           prefixrange: ZeroRange::from_u32(0, 0, 7, 8),
                           kind: TokenKind::IntConstant,
                       }));
        // The second unknown token will end the context because context1
        // understands it
        assert_eq!(context2.expect_next_kind(
            &mut fileparse, TokenKind::SemiColon),
                   LeafToken::Missing(
                       MissingToken {
                           position: Position::<ZeroIndexed>::from_u32(0, 10),
                           description: "';'",
                           ended_by: Some(Token {
                               range: ZeroRange::from_u32(0, 0, 10, 11),
                               prefixrange: ZeroRange::from_u32(0, 0, 9, 10),
                               kind: TokenKind::RBrace,
                           }),
                       }));
        assert_eq!(context2.next(&mut fileparse), None);
        // Verify that outer context next token is as expected
        assert_eq!(context1.next(&mut fileparse),
                   Some(Token {
                       range: ZeroRange::from_u32(0, 0, 10, 11),
                       prefixrange: ZeroRange::from_u32(0, 0, 9, 10),
                       kind: TokenKind::RBrace,
                   }));
        // Verify that skipped token is tracked
        assert_eq!(fileparse.skipped_tokens, vec![(Token {
            range: ZeroRange::from_u32(0, 0, 6, 7),
            prefixrange: ZeroRange::from_u32(0, 0, 5, 6),
            kind: TokenKind::CondOp,
        }, TokenKind::IntConstant.description())]);
    }

        // Sanity to check we never break simple parsing
    #[test]
    #[ignore]
    fn parity_test() {
        let test_root = std::env::var("PARITY_TEST_ROOT").unwrap();
        let mut file_count = 0;
        let failed_files: Vec<_> = walkdir::WalkDir::new(test_root)
            .into_iter()
            .filter_map(|r|r.ok())
            .filter(|e|!e.file_type().is_dir())
            .filter_map(|e|e.path().to_str().map(ToString::to_string))
            .filter(|f|!f.contains("errors")
                    && !f.contains("1.2")
                    && !f.contains("bugs")
                    && !f.contains("legacy")
                    && f.ends_with(".dml"))
            .filter_map(|f|{
                let read_file = std::fs::read_to_string(&f);
                if let Err(e) = read_file {
                    return Some(format!("Failed to read {}; {}", f, e));
                }
                let read_file = read_file.unwrap();
                let lexer = TokenKind::lexer(&read_file);
                let errors: Vec<_> = lexer.filter(
                    |tok|matches!(
                        tok,
                        Ok(TokenKind::LexerError) | Err(_))).collect();
                if !errors.is_empty() {
                    return Some(format!(
                        "Failed to parse {} with the following errors: {}",
                        f,
                        errors.into_iter().map(|e|format!("{:?}", e))
                            .collect::<Vec<_>>().join(", ")));
                }
                file_count += 1;
                None
            }).collect();
        assert!(failed_files.is_empty(),
                "{}",
                failed_files.join("\n"));
        println!("Successfully parsed {} files from PARITY_TEST_ROOT",
                 file_count);
    }
}

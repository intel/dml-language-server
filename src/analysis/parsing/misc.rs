use crate::lint::rules::spacing::{NspPtrDeclArgs, SpPtrDeclArgs};
use crate::lint::{rules::CurrentRules, AuxParams, DMLStyleError};
//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::span::Range;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::{Token, doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::parsing::tree::{AstObject, Content, LeafToken, TreeElement,
                                     TreeElements, ZeroRange};
use crate::analysis::parsing::expression::{Expression,
                                           ExpressionContent,
                                           ParenExpressionContent,
                                           maybe_parse_tertiary_expression};
use crate::analysis::parsing::types::{BaseType, typeident_filter};
use crate::analysis::LocalDMLError;

#[derive(Debug, Clone, PartialEq)]
pub struct InitializerStructElem {
    pub period: LeafToken,
    pub ident: LeafToken,
    pub assign: LeafToken,
    pub init: SingleInitializer,
}

impl TreeElement for InitializerStructElem {
    fn range(&self) -> ZeroRange {
        ZeroRange::combine(self.period.range(), self.init.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.period, &self.ident, &self.assign, &self.init)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SingleInitializerContent {
    Expression(Expression),
    List(LeafToken, Vec<(SingleInitializer, Option<LeafToken>)>, LeafToken),
    Structure(LeafToken,
              Vec<(InitializerStructElem,
                   Option<LeafToken>)>,
              Option<LeafToken>,
              LeafToken),
}

impl TreeElement for SingleInitializerContent {
    fn range(&self) -> ZeroRange {
        match &self {
            Self::Expression(content) => content.range(),
            Self::List(l, _, r) => Range::combine(l.range(), r.range()),
            Self::Structure(l, _, _, r) => Range::combine(l.range(), r.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Expression(expr) => create_subs!(expr),
            Self::List(left, middle, right) =>
                create_subs!(left, middle, right),
            Self::Structure(left, middle, ellipsis, right) =>
                create_subs!(left, middle, ellipsis, right),
        }
    }
}

pub type SingleInitializer = AstObject<SingleInitializerContent>;

impl Parse<SingleInitializerContent> for SingleInitializer {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> SingleInitializer {
        fn understands_rbrace(token: TokenKind) -> bool {
            token == TokenKind::RBrace
        }
        let mut new_context = context.enter_context(
            doesnt_understand_tokens);
        match new_context.peek_kind(stream) {
            Some(TokenKind::LBrace) => {
                let lbrace = new_context.next_leaf(stream);
                // If we peek a dot, we're parsing a structure.
                // Otherwise, a list
                let mut brace_context = new_context.enter_context(
                    understands_rbrace);
                let mut innermost_context = brace_context.enter_context(
                    doesnt_understand_tokens);
                match new_context.peek_kind(stream) {
                    Some(TokenKind::Dot) => {
                        let mut inits = vec![];
                        let mut ellipsis = None;
                        let mut end = false;
                        // designated struct lists are non-empty
                        // additionally, the ellipsis cannot appear before
                        // a field init
                        while !end {
                            // this means this will never be true in the first
                            // loop
                            if innermost_context.peek_kind(stream)
                                == Some(TokenKind::Ellipsis) {
                                    ellipsis = Some(innermost_context.next_leaf(
                                        stream));
                                    end = true;
                                    continue;
                            }
                            let period = innermost_context.expect_next_kind(
                                stream, TokenKind::Dot);
                            let ident = innermost_context.expect_next_filter(
                                stream, ident_filter, "field name");
                            let assign = innermost_context.expect_next_kind(
                                stream, TokenKind::Assign);
                            let init = SingleInitializer::parse(
                                &innermost_context, stream, file_info);
                            let comma = match innermost_context.peek_kind(
                                stream) {
                                Some(TokenKind::Comma) => {
                                    // Since we allow comma with no init after,
                                    // check for rbrace here in case we should stop
                                    Some(innermost_context.next_leaf(stream))
                                },
                                _ => {
                                    // Anything else and we should end loop
                                    end = true;
                                    None
                                }
                            };
                            inits.push((InitializerStructElem {
                                period, ident, assign, init}, comma));
                            // Cover the "....field = init,}" case
                            if brace_context.peek_kind(stream) == Some(
                                TokenKind::RBrace) {
                                end = true;
                            }
                        }
                        let rbrace = brace_context.expect_next_kind(
                            stream, TokenKind::RBrace);
                        SingleInitializerContent::Structure(
                            lbrace, inits, ellipsis, rbrace)
                    },
                    _ => {
                        let mut inits = vec![];
                        let mut end = false;
                        // initializer lists are non-empty
                        while !end {
                            let new_init = SingleInitializer::parse(
                                context, stream, file_info);
                            let comma = match new_context.peek_kind(stream) {
                                Some(TokenKind::Comma) => {
                                    Some(new_context.next_leaf(stream))
                                },
                                _ => {
                                    // Anything else and we should end loop
                                    end = true;
                                    None
                                }
                            };
                            inits.push((new_init, comma));
                            // Cover the "...init,}" case
                            if new_context.peek_kind(stream) == Some(
                                TokenKind::RBrace) {
                                end = true;
                            }
                        }
                        let rbrace = new_context.expect_next_kind(
                            stream, TokenKind::RBrace);
                        SingleInitializerContent::List(lbrace, inits, rbrace)
                    }
                }
            },
            _ => SingleInitializerContent::Expression(
                // We re-use the context here, as the parsing is completely
                // deferred (and we no longer want to understand '}')
                Expression::parse(context, stream, file_info))
        }.into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitializerContent {
    Single(SingleInitializer),
    Tuple(LeafToken, Vec<(SingleInitializer,
                          Option<LeafToken>)>,
          LeafToken)
}

impl TreeElement for InitializerContent {
    fn range(&self) -> ZeroRange {
        match &self {
            Self::Single(content) => content.range(),
            Self::Tuple(l, _, r) => Range::combine(l.range(), r.range()),
        }
    }

    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Single(content) => create_subs!(content),
            Self::Tuple(left, middle, right) =>
                create_subs!(left, middle, right),
        }
    }
}

impl Parse<InitializerContent> for Initializer {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>, file_info: &FileInfo) -> Initializer {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut new_context = context.enter_context(
            understands_rparen);
        match new_context.peek_kind(stream) {
            Some(TokenKind::LParen) => {
                // There is a parse conflict here, this can either be
                // a tuple initializer OR a paranthesis expression
                // Distinguish by the fact that tuple initializers
                // are non-empty

                // Initializer tuple case
                let lparen = new_context.next_leaf(stream);
                let mut inits = vec![];
                let mut end = false;
                // tuple initializers are non-empty
                while !end {
                    let single_init = SingleInitializer::parse(
                        &new_context, stream, file_info);
                    let comma = match new_context.peek_kind(stream) {
                        Some(TokenKind::Comma) => {
                            // Since we allow comma with no init after,
                            // check for rbrace here in case we should stop
                            Some(new_context.next_leaf(stream))
                        },
                        _ => {
                            // Anything else and we should end loop
                            end = true;
                            None
                        }
                    };
                    if new_context.peek_kind(stream) == Some(
                        TokenKind::RParen) {
                        end = true;
                    }
                    inits.push((single_init, comma));
                }
                let rparen = new_context.expect_next_kind(
                    stream, TokenKind::RParen);
                if let [
                    (AstObject {
                        content,
                        ..
                    }, None)] = &inits[..]
                {
                    if let Content::Some(
                        SingleInitializerContent::Expression(expr)) =
                        &**content {
                        // Hack together the paren expression
                        let parenexpr: Expression =
                            ExpressionContent::ParenExpression(
                                ParenExpressionContent {
                                    lparen,
                                    expr: expr.clone(),
                                    rparen
                                }).into();
                        // Finish parsing the expression
                        let fullexpr = maybe_parse_tertiary_expression(
                            Some(parenexpr), &new_context, stream, file_info);
                        // Hack together the initializer
                        InitializerContent::Single(
                            SingleInitializerContent::Expression(
                                fullexpr
                            ).into()
                        )
                    } else {
                        InitializerContent::Tuple(lparen, inits, rparen)
                    }
                } else {
                    InitializerContent::Tuple(lparen, inits, rparen)
                }
            },
            _ => {
                InitializerContent::Single(SingleInitializer::parse(
                    &new_context, stream, file_info))
            }
        }.into()
    }
}

pub type Initializer = AstObject<InitializerContent>;

// Corresponds to cdecl3 in grammar
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclContent {
    Ident(LeafToken),
    Array(TypeDecl, LeafToken, Expression, LeafToken),
    Fun(TypeDecl, LeafToken, Vec<(CDecl, Option<LeafToken>)>,
        Option<LeafToken>, LeafToken),
    Parens(LeafToken, Vec<LeafToken>, TypeDecl, LeafToken),
}

impl TreeElement for TypeDeclContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Ident(tok) => tok.range(),
            Self::Array(tdecl, lbracket, _, rbracket) => Range::combine(
                Range::combine(tdecl.range(), lbracket.range()),
                rbracket.range()),
            Self::Fun(tdecl, lparen, _, _, rparen) => Range::combine(
                Range::combine(tdecl.range(), lparen.range()),
                rparen.range()),
            Self::Parens(lparen, _, _, rparen) => Range::combine(
                lparen.range(), rparen.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Ident(tok) => create_subs!(tok),
            Self::Array(tdecl, left, middle, right) =>
                create_subs!(tdecl, left, middle, right),
            Self::Fun(tdecl, left, args, ellips, right) =>
                create_subs!(tdecl, left, args, ellips, right),
            Self::Parens(left, mods, tdecl, right) =>
                create_subs!(left, mods, tdecl, right),
        }
    }
}

// A TypeDecl can be empty, which we model as the None option value
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub content: Option<AstObject<TypeDeclContent>>,
}

impl TreeElement for TypeDecl {
    fn range(&self) -> ZeroRange {
        self.content.range()
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.content)
    }
}

impl TypeDecl {
    pub fn get_name(&self) -> Option<Token> {
        fn get_name_content(declcontent: &TypeDeclContent) -> Option<Token> {
            match declcontent {
                TypeDeclContent::Ident(leaftok) => {
                    leaftok.get_token()
                },
                TypeDeclContent::Array(decl, ..) => decl.get_name(),
                TypeDeclContent::Fun(decl, ..) => decl.get_name(),
                TypeDeclContent::Parens(_, _, decl, _) => decl.get_name(),
            }
        }
        match &self.content {
            None => None,
            Some(obj) => {
                // NOTE: The default value here is a bit superfluous, we should
                // not be able to aquire a missing typedeclcontent
                obj.with_content(get_name_content, None)
            }
        }
    }
}

// TODO: This is only used for method parameters, and should perhaps be moved
fn parse_decl_opt_ellipsis_aux(context: &ParseContext,
                               stream: &mut FileParser<'_>,
                               file_info: &FileInfo)
                               -> (Vec<(CDecl, Option<LeafToken>)>,
                                   Option<LeafToken>) {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    let mut end = false;
    let mut decls = vec![];
    let mut ellipsis = None;
    while !end {
        match new_context.peek_kind(stream) {
            Some(TokenKind::RParen) | None => {
                end = true;
            },
            Some(TokenKind::Ellipsis) => {
                end = true;
                ellipsis = Some(new_context.next_leaf(stream))
            },
            _ => {
                let cdecl = CDecl::parse(&new_context, stream, file_info);
                let comma = match new_context.peek_kind(stream) {
                    Some(TokenKind::Comma) =>
                        Some(new_context.next_leaf(stream)),
                    _ => { end = true; None },
                };
                decls.push((cdecl, comma));
            },
        }
    }
    (decls, ellipsis)
}

#[derive(Debug, Clone, PartialEq)]
pub struct CDeclList(Vec<(CDecl, Option<LeafToken>)>);

impl TreeElement for CDeclList {
    fn range(&self) -> ZeroRange {
        self.0.range()
    }
    fn subs(&self) -> TreeElements<'_> {
        self.0.subs()
    }
}

impl IntoIterator for CDeclList {
    type Item = (CDecl, Option<LeafToken>);
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl CDeclList {
    pub fn iter(&'_ self) ->
        std::slice::Iter<'_, (CDecl, Option<LeafToken>)> {
        self.0.iter()
    }
}

impl CDeclList {
    pub fn ensure_named(&self) -> Vec<LocalDMLError> {
        self.0.iter().flat_map(|(d, _)|d.ensure_named()).collect()
    }
    pub fn parse(context: &ParseContext,
                 stream: &mut FileParser<'_>,
                 file_info: &FileInfo)
                 -> CDeclList {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let mut decls = vec![];
        let empty = !new_context.peek_kind(stream).map_or(
            false, CDecl::first_token_matcher);
        // Note that trailing commas in these lists are NOT allowed,
        // which is why we only check for entry into the loop
        if !empty {
            while {
                let cdecl = CDecl::parse(&new_context, stream, file_info);
                let (comma, cont) = match new_context.peek_kind(stream) {
                    Some(TokenKind::Comma) =>
                        (Some(new_context.next_leaf(stream)), true),
                    _ => (None, false)
                };
                decls.push((cdecl, comma));
                cont
            } {}
        }
        CDeclList(decls)
    }
    pub fn parse_nonempty(context: &ParseContext,
                          stream: &mut FileParser<'_>,
                          file_info: &FileInfo)
                          -> CDeclList {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let mut decls = vec![];
        while {
            let cdecl = CDecl::parse(&new_context, stream, file_info);
            let (comma, cont) = match new_context.peek_kind(stream) {
                Some(TokenKind::Comma) =>
                    (Some(new_context.next_leaf(stream)), true),
                _ => (None, false)
            };
            decls.push((cdecl, comma));
            cont
        } {}
        CDeclList(decls)
    }
}

// Grammar allows cdecl3 to sart with cdecl3, thus we need to bind to the
// right and need an inner parse here
fn parse_typedecl_inner(context: &ParseContext,
                        stream: &mut FileParser<'_>,
                        _file_info: &FileInfo)
                        -> TypeDecl {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        None => TypeDecl { content : None },
        Some(tok) => if ident_filter(tok) {
            TypeDecl { content: Some(TypeDeclContent::Ident(
                new_context.next_leaf(stream)).into()) }
        } else {
            TypeDecl { content: None }
        }
    }
}

// NOTE: The meaning of TypeDecl here is changed. None value means no extension
// was made
fn parse_typedecl_outer(lead_typedecl: TypeDecl,
                        context: &ParseContext,
                        stream: &mut FileParser<'_>,
                        file_info: &FileInfo) -> TypeDecl {
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }
    fn understands_rbracket(token: TokenKind) -> bool {
        token == TokenKind::RBracket
    }
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        // NOTE: There is a parse conflict between
        // cdecl3: cdecl3 LPAREN ...
        // and
        // cdecl3: LPAREN cdecl2 ...
        // Disambiguate by checking if we parsed a cdecl3 (lead_typedecl)
        // (prioritizing LPAREN cdecl2)
        Some(TokenKind::LParen) => {
            let mut paren_context = new_context.enter_context(
                understands_rparen);
            let lparen = paren_context.next_leaf(stream);
            if lead_typedecl.content.is_some() {
                let (decls, ellipsis) = parse_decl_opt_ellipsis_aux(
                    &paren_context, stream, file_info);
                let rparen = paren_context.expect_next_kind(
                    stream, TokenKind::RParen);
                TypeDecl { content: Some(TypeDeclContent::Fun(
                    lead_typedecl, lparen, decls, ellipsis, rparen).into()) }
            } else {
                let (modifiers, cdecl) = parse_modified_typedecl(
                    &paren_context, stream, file_info);
                let rparen = paren_context.expect_next_kind(
                    stream, TokenKind::RParen);
                TypeDecl { content: Some(TypeDeclContent::Parens(
                    lparen, modifiers, cdecl, rparen).into()) }
            }
        },
        Some(TokenKind::LBracket) => {
            let mut bracket_context = new_context.enter_context(
                understands_rbracket);
            let lbracket = bracket_context.next_leaf(stream);
            let expr = Expression::parse(&bracket_context, stream, file_info);
            let rbracket = bracket_context.expect_next_kind(
                stream, TokenKind::RBracket);
            TypeDecl { content: Some(TypeDeclContent::Array(
                lead_typedecl, lbracket, expr, rbracket).into()) }
        }
        _ => TypeDecl { content: None},
    }
}

fn parse_typedecl_ext(lead_typedecl: TypeDecl,
                      context: &ParseContext,
                      stream: &mut FileParser<'_>,
                      file_info: &FileInfo) -> TypeDecl {
    match parse_typedecl_outer(
        lead_typedecl.clone(), context, stream, file_info).content {
        Some(tdecl) => parse_typedecl_ext(
            TypeDecl { content: Some(tdecl)}, context, stream, file_info),
        None => lead_typedecl,
    }
}

fn parse_typedecl(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> TypeDecl {
    let new_context = context.enter_context(doesnt_understand_tokens);
    let leftdecl = parse_typedecl_inner(&new_context, stream, file_info);
    parse_typedecl_ext(leftdecl, &new_context, stream, file_info)
}
// This is roughly equivalent to a cdecl2
fn parse_modified_typedecl(context: &ParseContext,
                           stream: &mut FileParser<'_>,
                           file_info: &FileInfo)
                           -> (Vec<LeafToken>, TypeDecl) {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    let mut modifiers = vec![];
    while matches!(new_context.peek_kind(stream),
                   Some(TokenKind::Const) | Some(TokenKind::Multiply) |
                   Some(TokenKind::Vect)) {
        modifiers.push(new_context.next_leaf(stream));
    }
    let decl = parse_typedecl(&new_context, stream, file_info);
    (modifiers, decl)
}

#[derive(Debug, Clone, PartialEq)]
pub struct CDeclContent {
    pub consttok: Option<LeafToken>,
    pub base: BaseType,
    pub modifiers: Vec<LeafToken>,
    pub decl: TypeDecl,
}

impl TreeElement for CDeclContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.consttok.range(),
            Range::combine(
                self.base.range(),
                Range::combine(
                    self.modifiers.range(),
                    self.decl.range())))
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.consttok, &self.base,
                     &self.modifiers, &self.decl)
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, _aux: AuxParams) {
        rules.sp_ptrdecl.check(acc, SpPtrDeclArgs::from_cdecl(self));
        rules.nsp_ptrdecl.check(acc, NspPtrDeclArgs::from_cdecl(self));
    }
}

// corresponds to cdecl in grammar
pub type CDecl = AstObject<CDeclContent>;

impl CDecl {
    pub fn check_named(&self) -> bool {
        self.get_name().is_some()
    }
    pub fn ensure_named(&self) -> Vec<LocalDMLError> {
        if !self.check_named() {
            vec![LocalDMLError {
                range: self.range(),
                description: "missing name in declaration".to_string(),
            }]
        } else {
            vec![]
        }
    }
    pub fn get_name(&self) -> Option<Token> {
        self.with_content(|content|content.decl.get_name(), None)
    }
}

impl Parse<CDeclContent> for CDecl {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> CDecl {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let consttok = new_context.next_if_kind(stream, TokenKind::Const);
        let base = BaseType::parse(&new_context, stream, file_info);
        let (modifiers, decl) = parse_modified_typedecl(&new_context,
                                                        stream,
                                                        file_info);
        CDeclContent {
            consttok, base, modifiers, decl
        }.into()
    }
}

impl CDecl {
    pub fn first_token_matcher(token: TokenKind) -> bool {
        BaseType::first_token_matcher(token) ||
            token == TokenKind::Const ||
            typeident_filter(token)
    }
}
pub fn ident_filter(token: TokenKind) -> bool {
    matches!(token,
             TokenKind::Identifier | TokenKind::Attribute | TokenKind::Bank |
             TokenKind::Connect | TokenKind::Constant | TokenKind::Data |
             TokenKind::Device | TokenKind:: Event | TokenKind::Field |
             TokenKind::Footer | TokenKind::Group | TokenKind::Header |
             TokenKind::Implement | TokenKind::Import | TokenKind::Independent |
             TokenKind::Interface | TokenKind::Loggroup | TokenKind::Method |
             TokenKind::Memoized | TokenKind::Port | TokenKind::Size |
             TokenKind::Throws | TokenKind::Param | TokenKind::Saved |
             TokenKind::Startup | TokenKind::Subdevice )
}

pub fn objident_filter(token: TokenKind) -> bool {
    ident_filter(token) || token == TokenKind::Register
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[test]
    fn initializer() {
        use crate::analysis::parsing::expression::ExpressionContent;
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "5",
            &make_ast(zero_range(0, 0, 0, 1),
                      SingleInitializerContent::Expression(
                          make_ast(
                              zero_range(0, 0, 0, 1),
                              ExpressionContent::Literal(make_leaf(
                                  zero_range(0, 0, 0, 0),
                                  zero_range(0, 0, 0, 1),
                                  TokenKind::IntConstant))))
            ),
            &vec![]);
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "{5}",
            &make_ast(zero_range(0, 0, 0, 3),
                      SingleInitializerContent::List(
                          make_leaf(zero_range(0, 0, 0, 0),
                                    zero_range(0, 0, 0, 1),
                                    TokenKind::LBrace),
                          vec![(make_ast(
                              zero_range(0, 0, 1, 2),
                              SingleInitializerContent::Expression(make_ast(
                                  zero_range(0, 0, 1, 2),
                                  ExpressionContent::Literal(
                                      make_leaf(zero_range(0, 0, 1, 1),
                                                zero_range(0, 0, 1, 2),
                                                TokenKind::IntConstant))))),
                                None)],
                          make_leaf(zero_range(0, 0, 2, 2),
                                    zero_range(0, 0, 2, 3),
                                    TokenKind::RBrace),
                      )),
            &vec![]);
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "{5, 4}",
            &make_ast(zero_range(0, 0, 0, 6),
                      SingleInitializerContent::List(
                          make_leaf(zero_range(0, 0, 0, 0),
                                    zero_range(0, 0, 0, 1),
                                    TokenKind::LBrace),
                          vec![
                              (make_ast(
                                  zero_range(0, 0, 1, 2),
                                  SingleInitializerContent::Expression(make_ast(
                                      zero_range(0, 0, 1, 2),
                                      ExpressionContent::Literal(
                                          make_leaf(
                                              zero_range(0, 0, 1, 1),
                                              zero_range(0, 0, 1, 2),
                                              TokenKind::IntConstant))))),
                               Some(make_leaf(zero_range(0, 0, 2, 2),
                                              zero_range(0, 0, 2, 3),
                                              TokenKind::Comma))),
                              (make_ast(
                                  zero_range(0, 0, 4, 5),
                                  SingleInitializerContent::Expression(make_ast(
                                      zero_range(0, 0, 4, 5),
                                      ExpressionContent::Literal(
                                          make_leaf(
                                              zero_range(0, 0, 3, 4),
                                              zero_range(0, 0, 4, 5),
                                              TokenKind::IntConstant))))),
                               None),
                          ],
                          make_leaf(zero_range(0, 0, 5, 5),
                                    zero_range(0, 0, 5, 6),
                                    TokenKind::RBrace),
                      )),
            &vec![]);
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "{5, 4,}",
            &make_ast(zero_range(0, 0, 0, 7),
                      SingleInitializerContent::List(
                          make_leaf(zero_range(0, 0, 0, 0),
                                    zero_range(0, 0, 0, 1),
                                    TokenKind::LBrace),
                          vec![
                              (make_ast(
                                  zero_range(0, 0, 1, 2),
                                  SingleInitializerContent::Expression(make_ast(
                                      zero_range(0, 0, 1, 2),
                                      ExpressionContent::Literal(
                                          make_leaf(
                                              zero_range(0, 0, 1, 1),
                                              zero_range(0, 0, 1, 2),
                                              TokenKind::IntConstant))))),
                               Some(make_leaf(zero_range(0, 0, 2, 2),
                                              zero_range(0, 0, 2, 3),
                                              TokenKind::Comma))),
                              (make_ast(
                                  zero_range(0, 0, 4, 5),
                                  SingleInitializerContent::Expression(make_ast(
                                      zero_range(0, 0, 4, 5),
                                      ExpressionContent::Literal(
                                          make_leaf(
                                              zero_range(0, 0, 3, 4),
                                              zero_range(0, 0, 4, 5),
                                              TokenKind::IntConstant))))),
                               Some(make_leaf(
                                   zero_range(0, 0, 5, 5),
                                   zero_range(0, 0, 5, 6),
                                   TokenKind::Comma))),
                          ],
                          make_leaf(zero_range(0, 0, 6, 6),
                                    zero_range(0, 0, 6, 7),
                                    TokenKind::RBrace),
                      )),
            &vec![]);
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "{.a = 5, .b = 4}",
            &make_ast(zero_range(0, 0, 0, 16),
                      SingleInitializerContent::Structure(
                          make_leaf(zero_range(0, 0, 0, 0),
                                    zero_range(0, 0, 0, 1),
                                    TokenKind::LBrace),
                          vec![
                              (InitializerStructElem {
                                  period: make_leaf(zero_range(0, 0, 1, 1),
                                                    zero_range(0, 0, 1, 2),
                                                    TokenKind::Dot),
                                  ident: make_leaf(zero_range(0, 0, 2, 2),
                                                   zero_range(0, 0, 2, 3),
                                                   TokenKind::Identifier),
                                  assign: make_leaf(zero_range(0, 0, 3, 4),
                                                    zero_range(0, 0, 4, 5),
                                                    TokenKind::Assign),
                                  init: make_ast(
                                      zero_range(0, 0, 6, 7),
                                      SingleInitializerContent::Expression(
                                          make_ast(
                                              zero_range(0, 0, 6, 7),
                                              ExpressionContent::Literal(
                                                  make_leaf(
                                                      zero_range(0, 0, 5, 6),
                                                      zero_range(0, 0, 6, 7),
                                                      TokenKind::IntConstant))))),
                              }, Some(make_leaf(zero_range(0, 0, 7, 7),
                                                zero_range(0, 0, 7, 8),
                                                TokenKind::Comma))),
                              (InitializerStructElem {
                                  period: make_leaf(zero_range(0, 0, 8, 9),
                                                    zero_range(0, 0, 9, 10),
                                                    TokenKind::Dot),
                                  ident: make_leaf(zero_range(0, 0, 10, 10),
                                                   zero_range(0, 0, 10, 11),
                                                   TokenKind::Identifier),
                                  assign: make_leaf(zero_range(0, 0, 11, 12),
                                                    zero_range(0, 0, 12, 13),
                                                    TokenKind::Assign),
                                  init: make_ast(
                                      zero_range(0, 0, 14, 15),
                                      SingleInitializerContent::Expression(make_ast(
                                          zero_range(0, 0, 14, 15),
                                          ExpressionContent::Literal(
                                              make_leaf(
                                                  zero_range(0, 0, 13, 14),
                                                  zero_range(0, 0, 14, 15),
                                                  TokenKind::IntConstant))))),
                              }, None),
                          ],
                          None,
                          make_leaf(zero_range(0, 0, 15, 15),
                                    zero_range(0, 0, 15, 16),
                                    TokenKind::RBrace),
                      )),
            &vec![]);
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "{.a = 5, ...}",
            &make_ast(zero_range(0, 0, 0, 13),
                      SingleInitializerContent::Structure(
                          make_leaf(zero_range(0, 0, 0, 0),
                                    zero_range(0, 0, 0, 1),
                                    TokenKind::LBrace),
                          vec![
                              (InitializerStructElem {
                                  period: make_leaf(zero_range(0, 0, 1, 1),
                                                    zero_range(0, 0, 1, 2),
                                                    TokenKind::Dot),
                                  ident: make_leaf(zero_range(0, 0, 2, 2),
                                                   zero_range(0, 0, 2, 3),
                                                   TokenKind::Identifier),
                                  assign: make_leaf(zero_range(0, 0, 3, 4),
                                                    zero_range(0, 0, 4, 5),
                                                    TokenKind::Assign),
                                  init: make_ast(
                                      zero_range(0, 0, 6, 7),
                                      SingleInitializerContent::Expression(
                                          make_ast(
                                              zero_range(0, 0, 6, 7),
                                              ExpressionContent::Literal(
                                                  make_leaf(
                                                      zero_range(0, 0, 5, 6),
                                                      zero_range(0, 0, 6, 7),
                                                      TokenKind::IntConstant))))),
                              }, Some(make_leaf(zero_range(0, 0, 7, 7),
                                                zero_range(0, 0, 7, 8),
                                                TokenKind::Comma))),
                          ],
                          Some(make_leaf(zero_range(0, 0, 8, 9),
                                         zero_range(0, 0, 9, 12),
                                         TokenKind::Ellipsis)),
                          make_leaf(zero_range(0, 0, 12, 12),
                                    zero_range(0, 0, 12, 13),
                                    TokenKind::RBrace),
                      )),
            &vec![]);
    }

    #[test]
    fn cdecl() {
        use crate::analysis::parsing::types::BaseTypeContent;
        use crate::analysis::parsing::expression::ExpressionContent;
        test_ast_tree::<CDeclContent, CDecl>(
            "int",
            &make_ast(zero_range(0, 0, 0, 3),
                      CDeclContent {
                          consttok: None,
                          base: make_ast(
                              zero_range(0, 0, 0, 3),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 0, 0),
                                            zero_range(0, 0, 0, 3),
                                            TokenKind::Int))),
                          modifiers: vec![],
                          decl: TypeDecl { content: None }
                      }),
            &vec![]);
        test_ast_tree::<CDeclContent, CDecl>(
            "int foo",
            &make_ast(zero_range(0, 0, 0, 7),
                      CDeclContent {
                          consttok: None,
                          base: make_ast(
                              zero_range(0, 0, 0, 3),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 0, 0),
                                            zero_range(0, 0, 0, 3),
                                            TokenKind::Int))),
                          modifiers: vec![],
                          decl: TypeDecl { content:
                              Some(make_ast(
                                  zero_range(0, 0, 4, 7),
                                  TypeDeclContent::Ident(
                                      make_leaf(zero_range(0, 0, 3, 4),
                                                zero_range(0, 0, 4, 7),
                                                TokenKind::Identifier))))}
                      }),
            &vec![]);
        test_ast_tree::<CDeclContent, CDecl>(
            "int **foo",
            &make_ast(zero_range(0, 0, 0, 9),
                      CDeclContent {
                          consttok: None,
                          base: make_ast(
                              zero_range(0, 0, 0, 3),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 0, 0),
                                            zero_range(0, 0, 0, 3),
                                            TokenKind::Int))),
                          modifiers: vec![
                              make_leaf(zero_range(0, 0, 3, 4),
                                        zero_range(0, 0, 4, 5),
                                        TokenKind::Multiply),
                              make_leaf(zero_range(0, 0, 5, 5),
                                        zero_range(0, 0, 5, 6),
                                        TokenKind::Multiply),
                          ],
                          decl: TypeDecl { content: Some(make_ast(
                              zero_range(0, 0, 6, 9),
                              TypeDeclContent::Ident(
                                  make_leaf(zero_range(0, 0, 6, 6),
                                            zero_range(0, 0, 6, 9),
                                            TokenKind::Identifier)))) }
                      }),
            &vec![]);
        let paren_cdecl = TypeDecl { content: Some(make_ast(
            zero_range(0, 0, 4, 10),
            TypeDeclContent::Parens(
                make_leaf(zero_range(0, 0, 3, 4),
                          zero_range(0, 0, 4, 5),
                          TokenKind::LParen),
                vec![make_leaf(zero_range(0, 0, 5, 5),
                               zero_range(0, 0, 5, 6),
                               TokenKind::Multiply)],
                TypeDecl { content: Some(make_ast(zero_range(0, 0, 6, 9),
                                       TypeDeclContent::Ident(
                                           make_leaf(zero_range(0, 0, 6, 6),
                                                     zero_range(0, 0, 6, 9),
                                                     TokenKind::Identifier))))},
                make_leaf(zero_range(0, 0, 9, 9),
                          zero_range(0, 0, 9, 10),
                          TokenKind::RParen))))};
        test_ast_tree::<CDeclContent, CDecl>(
            "int (*foo)()",
            &make_ast(zero_range(0, 0, 0, 12),
                      CDeclContent {
                          consttok: None,
                          base: make_ast(
                              zero_range(0, 0, 0, 3),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 0, 0),
                                            zero_range(0, 0, 0, 3),
                                            TokenKind::Int))),
                          modifiers: vec![],
                          decl: TypeDecl { content: Some(make_ast(
                              zero_range(0, 0, 4, 12),
                              TypeDeclContent::Fun(
                                  paren_cdecl,
                                  make_leaf(zero_range(0, 0, 10, 10),
                                            zero_range(0, 0, 10, 11),
                                            TokenKind::LParen),
                                  vec![], None,
                                  make_leaf(zero_range(0, 0, 11, 11),
                                            zero_range(0, 0, 11, 12),
                                            TokenKind::RParen))))},
                      }),
            &vec![]);
        test_ast_tree::<CDeclContent, CDecl>(
            "const int foo",
            &make_ast(zero_range(0, 0, 0, 13),
                      CDeclContent {
                          consttok: Some(make_leaf(
                              zero_range(0, 0, 0, 0),
                              zero_range(0, 0, 0, 5),
                              TokenKind::Const)),
                          base: make_ast(
                              zero_range(0, 0, 6, 9),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 5, 6),
                                            zero_range(0, 0, 6, 9),
                                            TokenKind::Int))),
                          modifiers: vec![],
                          decl: TypeDecl { content: Some(make_ast(
                              zero_range(0, 0, 10, 13),
                              TypeDeclContent::Ident(
                                  make_leaf(zero_range(0, 0, 9, 10),
                                            zero_range(0, 0, 10, 13),
                                            TokenKind::Identifier))))}
                      }),
            &vec![]);
        test_ast_tree::<CDeclContent, CDecl>(
            "int foo[2]",
            &make_ast(zero_range(0, 0, 0, 10),
                      CDeclContent {
                          consttok: None,
                          base: make_ast(
                              zero_range(0, 0, 0, 3),
                              BaseTypeContent::Ident(
                                  make_leaf(zero_range(0, 0, 0, 0),
                                            zero_range(0, 0, 0, 3),
                                            TokenKind::Int))),
                          modifiers: vec![],
                          decl: TypeDecl { content: Some(make_ast(
                              zero_range(0, 0, 4, 10),
                              TypeDeclContent::Array(
                                  TypeDecl { content: Some(make_ast(
                                      zero_range(0, 0, 4, 7),
                                      TypeDeclContent::Ident(
                                          make_leaf(zero_range(0, 0, 3, 4),
                                                    zero_range(0, 0, 4, 7),
                                                    TokenKind::Identifier
                                          ))))},
                                  make_leaf(zero_range(0, 0, 7, 7),
                                            zero_range(0, 0, 7, 8),
                                            TokenKind::LBracket),
                                  make_ast(
                                      zero_range(0, 0, 8, 9),
                                      ExpressionContent::Literal(
                                          make_leaf(zero_range(0, 0, 8, 8),
                                                    zero_range(0, 0, 8, 9),
                                                    TokenKind::IntConstant))),
                                  make_leaf(zero_range(0, 0, 9, 9),
                                            zero_range(0, 0, 9, 10),
                                            TokenKind::RBracket))))}
                      }),
            &vec![]);
    }
    #[test]
    fn initializer_paren_expression() {
        use crate::analysis::parsing::expression::*;
        use crate::analysis::parsing::misc::*;
        let i = make_ast(
            zero_range(0, 0, 1, 2),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 1, 1),
                zero_range(0, 0, 1, 2),
                TokenKind::Identifier
            )));
        let one = make_ast(
            zero_range(0, 0, 3, 4),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 3, 3),
                zero_range(0, 0, 3, 4),
                TokenKind::IntConstant
            )));
        let i_plus_one = make_ast(
            zero_range(0, 0, 1, 4),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: i,
                operation: make_leaf(
                    zero_range(0, 0, 2, 2),
                    zero_range(0, 0, 2, 3),
                    TokenKind::Plus
                ),
                right: one,
            }));
        let parens = make_ast(
            zero_range(0, 0, 0, 5),
            ExpressionContent::ParenExpression(ParenExpressionContent {
                lparen: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 1),
                    TokenKind::LParen
                ),
                expr: i_plus_one,
                rparen: make_leaf(
                    zero_range(0, 0, 4, 4),
                    zero_range(0, 0, 4, 5),
                    TokenKind::RParen
                ),
            }));
        let three = make_ast(
            zero_range(0, 0, 6, 7),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 6, 6),
                zero_range(0, 0, 6, 7),
                TokenKind::IntConstant
            )));
        let left_times_right = make_ast(
            zero_range(0, 0, 0, 7),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: parens,
                operation: make_leaf(
                    zero_range(0, 0, 5, 5),
                    zero_range(0, 0, 5, 6),
                    TokenKind::Multiply
                ),
                right: three,
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 7),
            SingleInitializerContent::Expression(left_times_right));
        test_ast_tree::<SingleInitializerContent, SingleInitializer>(
            "(i+1)*3",
            &expected,
            &vec![]);
    }
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::lint::{rules::{indentation::IN3Args,
                            spacing::SpBracesArgs,
                            CurrentRules},
                            AuxParams,
                            DMLStyleError};
use crate::span::Range;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::{doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::parsing::tree::{AstObject, TreeElement, TreeElements,
                            LeafToken, ZeroRange};
use crate::analysis::parsing::misc::{CDecl, ident_filter};
use crate::analysis::parsing::expression::Expression;
use crate::analysis::LocalDMLError;
use crate::vfs::TextFile;

pub fn typeident_filter(token: TokenKind) -> bool {
    match token {
        TokenKind::Identifier |
        TokenKind::Int | TokenKind::Float | TokenKind::Char |
        TokenKind::Double | TokenKind::Long | TokenKind::Short |
        TokenKind::Signed | TokenKind::Unsigned |
        TokenKind::Void | TokenKind::Register => true,
        _ => ident_filter(token)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StructTypeContent {
    pub structtok: LeafToken,
    pub lbrace: LeafToken,
    pub members: Vec<(CDecl, LeafToken)>,
    pub rbrace: LeafToken,
}

impl TreeElement for StructTypeContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.structtok.range(), self.rbrace.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.structtok,
                     &self.lbrace,
                     &self.members,
                     &self.rbrace)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        let mut errors = vec![];
        for (decl, _) in &self.members {
            errors.append(&mut decl.ensure_named());
        }
        errors
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, aux: &mut AuxParams) {
        rules.in3.check(acc, IN3Args::from_struct_type_content(self, &mut aux.depth));
        rules.sp_brace.check(acc, SpBracesArgs::from_struct_type_content(self));
    }
}

impl Parse<BaseTypeContent> for StructTypeContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        fn understands_rbrace_and_semi(token: TokenKind) -> bool {
            token == TokenKind::RBrace || token == TokenKind::SemiColon
        }
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let mut list_context = new_context.enter_context(
            understands_rbrace_and_semi);
        // Guaranteed by parser
        let structtok =  new_context.next_leaf(stream);
        let lbrace = new_context.expect_next_kind(stream, TokenKind::LBrace);
        let mut members = vec![];
        let mut cont = true;
        while cont {
            let next = new_context.peek_kind(stream);
            cont = false;
            if let Some(next_kind) = next {
                if CDecl::first_token_matcher(next_kind) {
                    let member = CDecl::parse(&list_context, stream, file_info);
                    let semi = list_context.expect_next_kind(
                        stream, TokenKind::SemiColon);
                    members.push((member, semi));
                    cont = true;
                }
            }
        }
        let rbrace = new_context.expect_next_kind(stream, TokenKind::RBrace);
        BaseTypeContent::StructType(StructTypeContent {
            structtok,
            lbrace,
            members,
            rbrace,
        }).into()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LayoutContent {
    pub layout: LeafToken,
    pub byteorder: LeafToken,
    pub lbrace: LeafToken,
    pub fields: Vec<(CDecl, LeafToken)>,
    pub rbrace: LeafToken,
}

impl TreeElement for LayoutContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.layout.range(), self.rbrace.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.layout,
                     &self.byteorder,
                     &self.lbrace,
                     &self.fields,
                     &self.rbrace)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        let mut errors = vec![];
        for (field, _) in &self.fields {
            errors.append(&mut field.ensure_named());
        }
        if let Some(byteorder) = self.byteorder.read_leaf(file) {
            if byteorder != r#""big-endian""# &&
                byteorder != r#""little-endian""# {
                errors.push(LocalDMLError {
                    range: self.byteorder.range(),
                    description: "Must be 'big-endian' or \
                                  'little-endian'".to_string(),
                });
            }
        }
        errors
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, aux: &mut AuxParams) {
        rules.in3.check(acc, IN3Args::from_layout_content(self, &mut aux.depth));
        rules.sp_brace.check(acc, SpBracesArgs::from_layout_content(self));
    }
}

impl Parse<BaseTypeContent> for LayoutContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        fn understands_rbrace_and_semi(token: TokenKind) -> bool {
            token == TokenKind::RBrace || token == TokenKind::SemiColon
        }
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let mut list_context = new_context.enter_context(
            understands_rbrace_and_semi);
        // Guaranteed by parser
        let layout = new_context.next_leaf(stream);
        let byteorder = new_context.expect_next_kind(
            stream, TokenKind::StringConstant);
        let lbrace = new_context.expect_next_kind(stream, TokenKind::LBrace);
        let mut fields = vec![];
        while match new_context.peek_kind(stream) {
            Some(TokenKind::RBrace) | None => false,
            Some(_) => {
                let field = CDecl::parse(&list_context, stream, file_info);
                let semi = list_context.expect_next_kind(
                    stream, TokenKind::SemiColon);
                fields.push((field, semi));
                true
            },
        } {}
        let rbrace = new_context.expect_next_kind(stream, TokenKind::RBrace);
        BaseTypeContent::Layout(LayoutContent {
            layout,
            byteorder,
            lbrace,
            fields,
            rbrace,
        }).into()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum BitfieldsRange {
    Expression(Expression),
    Range(Expression, LeafToken, Expression),
}

impl TreeElement for BitfieldsRange {
    fn range(&self) -> ZeroRange {
        match self {
            BitfieldsRange::Expression(expr) => expr.range(),
            BitfieldsRange::Range(l, _, r) => Range::combine(
                l.range(), r.range())
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            BitfieldsRange::Expression(expr) => create_subs!(expr),
            BitfieldsRange::Range(l, m, r) => create_subs!(
                l, m, r)
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BitfieldsDeclContent {
    pub cdecl: CDecl,
    pub at: LeafToken,
    pub lbracket: LeafToken,
    pub range: BitfieldsRange,
    pub rbracket: LeafToken,
    pub semi: LeafToken
}

impl TreeElement for BitfieldsDeclContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.cdecl.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.cdecl,
                     &self.at,
                     &self.lbracket,
                     &self.range,
                     &self.rbracket,
                     &self.semi)
    }
}

impl BitfieldsDeclContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> BitfieldsDeclContent {
        fn understands_semi(token: TokenKind) -> bool {
            token == TokenKind::SemiColon
        }
        fn understands_rbracket(token: TokenKind) -> bool {
            token == TokenKind::RBracket
        }
        let mut semi_context = context.enter_context(understands_semi);
        let mut rbracket_context = semi_context.enter_context(
            understands_rbracket);
        let cdecl = CDecl::parse(&semi_context, stream, file_info);
        let at = semi_context.expect_next_kind(stream, TokenKind::At);
        let lbracket = semi_context.expect_next_kind(
            stream, TokenKind::LBracket);
        let range = {
            let first_expr = Expression::parse(&rbracket_context, stream, file_info);
            if rbracket_context.peek_kind(stream) == Some(TokenKind::Colon) {
                let colon = rbracket_context.next_leaf(stream);
                let second_expr = Expression::parse(&rbracket_context, stream, file_info);
                BitfieldsRange::Range(first_expr, colon, second_expr)
            } else {
                BitfieldsRange::Expression(first_expr)
            }
        };
        let rbracket = rbracket_context.expect_next_kind(
            stream, TokenKind::RBracket);
        let semi = semi_context.expect_next_kind(stream, TokenKind::SemiColon);
        BitfieldsDeclContent {
            cdecl,
            at,
            lbracket,
            range,
            rbracket,
            semi,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BitfieldsContent {
    pub bitfields: LeafToken,
    pub iconst: LeafToken,
    pub lbrace: LeafToken,
    pub fields: Vec<BitfieldsDeclContent>,
    pub rbrace: LeafToken,
}

impl TreeElement for BitfieldsContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.bitfields.range(), self.rbrace.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.bitfields,
                     &self.iconst,
                     &self.lbrace,
                     &self.fields,
                     &self.rbrace)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        let mut used_names = vec![];
        let mut errors = vec![];
        for field in &self.fields {
            match field.cdecl.get_name() {
                None => errors.push(LocalDMLError {
                    range: field.cdecl.range(),
                    description: "missing name in declaration".to_string(),
                }),
                Some(tok) => {
                    let name = tok.read_token(file);
                    if used_names.contains(&name) {
                        errors.push(LocalDMLError {
                            range: field.cdecl.range(),
                            description: "The name is already in use in the \
                                          same scope".to_string(),
                        })
                    } else {
                        used_names.push(name)
                    }
                }
            }
        }
        errors
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.sp_brace.check(acc, SpBracesArgs::from_bitfields_content(self));
    }
}

impl Parse<BaseTypeContent> for BitfieldsContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        fn understands_rbrace(token: TokenKind) -> bool {
            token == TokenKind::RBrace
        }
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let list_context = new_context.enter_context(understands_rbrace);
        // Guaranteed by parser
        let bitfields = new_context.next_leaf(stream);
        let iconst = new_context.expect_next_kind(stream,
                                                  TokenKind::IntConstant);
        let lbrace = new_context.expect_next_kind(stream, TokenKind::LBrace);
        let mut fields = vec![];
        while new_context.peek_kind(stream).map_or(
            false,
            CDecl::first_token_matcher) {
            fields.push(BitfieldsDeclContent::parse(
                &list_context, stream, file_info));
        }
        let rbrace = new_context.expect_next_kind(stream, TokenKind::RBrace);
        BaseTypeContent::Bitfields(BitfieldsContent {
            bitfields,
            iconst,
            lbrace,
            fields,
            rbrace,
        }).into()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct TypeOfContent {
    pub typeoftok: LeafToken,
    pub of: Expression,
}

impl TreeElement for TypeOfContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.typeoftok.range(), self.of.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.typeoftok, &self.of)
    }
}

impl Parse<BaseTypeContent> for TypeOfContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        // Guaranteed by parser
        let typeoftok = new_context.next_leaf(stream);
        let of = Expression::parse(&new_context, stream, file_info);
        BaseTypeContent::TypeOf(TypeOfContent {
            typeoftok,
            of,
        }).into()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct SequenceContent {
    pub sequence: LeafToken,
    pub lparen: LeafToken,
    pub ident: LeafToken,
    pub rparen: LeafToken,
}

impl TreeElement for SequenceContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.sequence.range(), self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.sequence,
                     &self.lparen,
                     &self.ident,
                     &self.rparen)
    }
}

impl Parse<BaseTypeContent> for SequenceContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo) -> BaseType {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RBracket
        }
        let mut new_context = context.enter_context(understands_rparen);
        // Guaranteed by parser
        let sequence = new_context.next_leaf(stream);
        let lparen = new_context.expect_next_kind(stream, TokenKind::LParen);
        let ident = new_context.expect_next_filter
            (stream, typeident_filter, "type ident");
        let rparen = new_context.expect_next_kind(stream, TokenKind::RParen);
        BaseTypeContent::Sequence(SequenceContent {
            sequence, lparen, ident, rparen
        }).into()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct HookTypeContent {
    pub hook: LeafToken,
    pub lparen: LeafToken,
    pub args: Vec<(CDecl, Option<LeafToken>)>,
    pub rparen: LeafToken,
}

impl TreeElement for HookTypeContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.hook.range(), self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.hook,
                     &self.lparen,
                     &self.args,
                     &self.rparen)
    }
}

impl Parse<BaseTypeContent> for HookTypeContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RBracket
        }
        let mut new_context = context.enter_context(understands_rparen);
        // Guaranteed by parser
        let hook = new_context.next_leaf(stream);
        let lparen = new_context.expect_next_kind(stream, TokenKind::LParen);
        let mut args = vec![];
        while new_context.peek_kind(stream).map_or(
            false,
            CDecl::first_token_matcher) {
            let arg = CDecl::parse(&new_context, stream, file_info);
            let next_kind = new_context.peek_kind(stream);
            // This is a neat trick, by checking for a comma if we see one OR
            // if we would loop again, we can catch the case
            // "hook(typ1 typ2)" and correctly expect a comma between the cdecls
            let comma = if next_kind == Some(TokenKind::Comma)
                ||  new_context.peek_kind(stream).map_or(
                    false, CDecl::first_token_matcher)  {
                Some(new_context.expect_next_kind(stream, TokenKind::Comma))
            } else {
                None
            };
            args.push((arg, comma));
        }
        let rparen = new_context.expect_next_kind(stream, TokenKind::RParen);
        BaseTypeContent::HookType(HookTypeContent {
            hook, lparen, args, rparen
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseTypeContent {
    Ident(LeafToken),
    StructType(StructTypeContent),
    Layout(LayoutContent),
    Bitfields(BitfieldsContent),
    TypeOf(TypeOfContent),
    Sequence(SequenceContent),
    HookType(HookTypeContent),
}

impl TreeElement for BaseTypeContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Ident(content) => content.range(),
            Self::StructType(content) => content.range(),
            Self::Layout(content) => content.range(),
            Self::Bitfields(content) => content.range(),
            Self::TypeOf(content) => content.range(),
            Self::Sequence(content) => content.range(),
            Self::HookType(content) => content.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Ident(content) => create_subs![content],
            Self::StructType(content) => create_subs![content],
            Self::Layout(content) => create_subs![content],
            Self::Bitfields(content) => create_subs![content],
            Self::TypeOf(content) => create_subs![content],
            Self::Sequence(content) => create_subs![content],
            Self::HookType(content) => create_subs![content],
        }
    }
}

pub type BaseType = AstObject<BaseTypeContent>;

impl Parse<BaseTypeContent> for BaseType {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> BaseType {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        match new_context.expect_peek_filter(
            stream, BaseType::first_token_matcher, "basic type") {
            LeafToken::Actual(token) => {
                match token.kind {
                    TokenKind::Struct =>
                        StructTypeContent::parse(&new_context, stream, file_info),
                    TokenKind::Layout =>
                        LayoutContent::parse(&new_context, stream, file_info),
                    TokenKind::Bitfields =>
                        BitfieldsContent::parse(&new_context, stream, file_info),
                    TokenKind::TypeOf =>
                        TypeOfContent::parse(&new_context, stream, file_info),
                    TokenKind::Sequence =>
                        SequenceContent::parse(&new_context, stream, file_info),
                    TokenKind::Hook =>
                        HookTypeContent::parse(&new_context, stream, file_info),
                    _ => {
                        if typeident_filter(token.kind) {
                            BaseTypeContent::Ident(
                                new_context.next_leaf(stream)).into()
                        } else {
                            std::unreachable!("Should have been filtered")
                        }
                    },
                }
            },
            LeafToken::Missing(missing) => missing.into(),
        }
    }
}

impl BaseType {
    pub fn first_token_matcher(token: TokenKind) -> bool {
        match token {
            TokenKind::Struct | TokenKind::Layout | TokenKind::Bitfields |
            TokenKind::TypeOf | TokenKind::Sequence | TokenKind::Hook => true,
            _ => typeident_filter(token)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CTypeDeclSimpleContent {
    pub modifiers: Vec<LeafToken>,
    pub inner: Option<(LeafToken, CTypeDeclSimple, LeafToken)>,
}

impl TreeElement for CTypeDeclSimpleContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.modifiers.range(), self.inner.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.modifiers, &self.inner)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        // Check order of modifiers
        let mut errors = vec![];
        let mut previous_was_mult = false;
        // By invariant, these tokens cannot be missing leafs or
        // non-times/non-mult tokens
        for leaf in &self.modifiers {
            match leaf {
                LeafToken::Actual(token) => match token.kind {
                    TokenKind::Multiply => previous_was_mult = true,
                    TokenKind::Const => {
                        if !previous_was_mult {
                            errors.push(LocalDMLError {
                                range: token.range,
                                description:
                                "Expected '*' before 'const'".to_string()
                            })
                        }
                        previous_was_mult = false;
                    },
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        errors
    }
}

pub type CTypeDeclSimple = AstObject<CTypeDeclSimpleContent>;

impl Parse<CTypeDeclSimpleContent> for CTypeDeclSimple {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> CTypeDeclSimple {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let mut modifiers = vec![];
        while match new_context.peek_kind(stream) {
            Some(TokenKind::Multiply) | Some(TokenKind::Const) =>
            { modifiers.push(new_context.next_leaf(stream));
              true },
            _ => false,
        } {}
        let inner = match new_context.peek_kind(stream) {
            Some(TokenKind::LParen) => {
                let mut paren_context = new_context.enter_context(
                    understands_rparen);
                let lparen = paren_context.next_leaf(stream);
                let simple = CTypeDeclSimple::parse(&paren_context,
                                                    stream,
                                                    file_info);
                let rparen = paren_context.expect_next_kind(
                    stream, TokenKind::RParen);
                Some((lparen, simple, rparen))
            },
            _ => None,
        };
        CTypeDeclSimpleContent {
            modifiers, inner
        }.into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CTypeDeclContent {
    pub consttok: Option<LeafToken>,
    pub base: BaseType,
    pub simple: CTypeDeclSimple,
}

impl TreeElement for CTypeDeclContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.consttok.range(),
                       Range::combine(self.base.range(),
                                      self.simple.range()))
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.consttok, &self.base, &self.simple)
    }
}

pub type CTypeDecl = AstObject<CTypeDeclContent>;

impl Parse<CTypeDeclContent> for CTypeDecl {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> CTypeDecl {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        let consttok = match new_context.peek_kind(stream) {
            Some(TokenKind::Const) => Some(new_context.next_leaf(stream)),
            _ => None
        };
        let base = BaseType::parse(&new_context, stream, file_info);
        let simple = CTypeDeclSimple::parse(&new_context, stream, file_info);
        CTypeDeclContent {
            consttok,
            base,
            simple,
        }.into()
    }
}

// TODO: Expand unit tests
#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[test]
    fn ctypedecl() {
        test_ast_tree::<CTypeDeclContent, CTypeDecl>(
            "const foo**",
            &make_ast(zero_range(0, 0, 0, 11),
                      CTypeDeclContent {
                          consttok: Some(
                              make_leaf(zero_range(0, 0, 0, 0),
                                        zero_range(0, 0, 0, 5),
                                        TokenKind::Const)),
                          base: make_ast(zero_range(0, 0, 6, 9),
                                         BaseTypeContent::Ident(make_leaf(
                                             zero_range(0, 0, 5, 6),
                                             zero_range(0, 0, 6, 9),
                                             TokenKind::Identifier))),
                          simple: make_ast(
                              zero_range(0, 0, 9, 11),
                              CTypeDeclSimpleContent {
                                  modifiers: vec![
                                      make_leaf(zero_range(0, 0, 9, 9),
                                                zero_range(0, 0, 9, 10),
                                                TokenKind::Multiply),
                                      make_leaf(zero_range(0, 0, 10, 10),
                                                zero_range(0, 0, 10, 11),
                                                TokenKind::Multiply)],
                                  inner: None
                              }),
                      }),
            &vec![])
    }
}

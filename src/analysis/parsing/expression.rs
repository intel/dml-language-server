//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::span::Range;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::{Token, doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::parsing::tree::{AstObject, TreeElement,
                                     TreeElements, LeafToken,
                                     ZeroRange, ZeroSpan};
use crate::analysis::parsing::types::CTypeDecl;
use crate::analysis::parsing::misc::{objident_filter, SingleInitializer};
use crate::analysis::reference::{Reference, ReferenceKind,
                                 MaybeIsNodeRef, NodeRef};
use crate::analysis::FileSpec;
use crate::analysis::structure::expressions::DMLString;
use crate::analysis::{DeclarationSpan, LocalDMLError};


use crate::lint::{rules::{spacing::{NspFunparArgs,
                                    NspInparenArgs,
                                    NspUnaryArgs,
                                    SpPunctArgs},
                                    CurrentRules},
                                    AuxParams};

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpressionContent {
    pub operation: LeafToken,
    pub expr: Expression,
}

impl TreeElement for UnaryExpressionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.operation.range(), self.expr.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.operation, &self.expr)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_unary.check(acc, NspUnaryArgs::from_unary_expr(self));
    }
}

impl Parse<ExpressionContent> for UnaryExpressionContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> Expression {
        let mut new_context = context.enter_context(
            doesnt_understand_tokens);
        ExpressionContent::UnaryExpression(UnaryExpressionContent {
            operation: new_context.next_leaf(stream),
            expr: parse_expression_inner(None,
                                         &new_context,
                                         stream,
                                         file_info),
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PostUnaryExpressionContent {
    pub expr: Expression,
    pub operation: LeafToken,
}

impl TreeElement for PostUnaryExpressionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.expr.range(), self.operation.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.expr, &self.operation)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_unary.check(acc, NspUnaryArgs::from_postunary_expr(self));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpressionContent {
    pub operation: LeafToken,
    pub left: Expression,
    pub right: Expression,
}

impl TreeElement for BinaryExpressionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.left.range(), self.right.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.left, &self.operation, &self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberLiteralContent {
    pub operation: LeafToken,
    pub left: Expression,
    pub right: LeafToken,
}

impl TreeElement for MemberLiteralContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.left.range(), self.right.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.left, &self.operation, &self.right)
    }
    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        self.subs().into_iter()
            .for_each(|s|s.collect_references(accumulator, file));
        if let Some(refr) = self.maybe_noderef(file) {
            accumulator.push(Reference::from_noderef(
                refr, ReferenceKind::Variable));
        }
    }
}

impl MaybeIsNodeRef for MemberLiteralContent {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        let left_noderef = self.left.maybe_noderef(file)?;
        let right_node = DMLString::from_token(&self.right, file)?;
        let span = ZeroSpan::combine(
            *left_noderef.span(),
            *right_node.span());
        Some(NodeRef::Sub(
            Box::new(left_noderef),
            right_node,
            span))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TertiaryExpressionContent {
    pub left: Expression,
    pub left_operation: LeafToken,
    pub middle: Expression,
    pub right_operation: LeafToken,
    pub right: Expression,
}

impl TreeElement for TertiaryExpressionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.left.range(), self.right.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.left, &self.left_operation,
                     &self.middle, &self.right_operation, &self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParenExpressionContent {
    pub lparen: LeafToken,
    pub expr: Expression,
    pub rparen: LeafToken,
}

impl TreeElement for ParenExpressionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.lparen.range(), self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.lparen, &self.expr, &self.rparen)
    }
}

impl Parse<ExpressionContent> for ParenExpressionContent{
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut new_context = context.enter_context(understands_rparen);
        let lparen = new_context.next_leaf(stream);
        let expr = Expression::parse(&new_context, stream, file_info);
        let rparen = new_context.expect_next_kind(stream, TokenKind::RParen);
        ExpressionContent::ParenExpression(
            ParenExpressionContent {
                lparen,
                expr,
                rparen,
            }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallContent {
    pub fun: Expression,
    pub lparen: LeafToken,
    pub arguments: Vec<(SingleInitializer, Option<LeafToken>)>,
    pub rparen: LeafToken,
}

impl TreeElement for FunctionCallContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.fun.range(), self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.fun, &self.lparen, &self.arguments, &self.rparen)
    }
    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        self.default_references(accumulator, file);
        if let Some(noderef) = self.fun.maybe_noderef(file) {
            accumulator.push(Reference::from_noderef(
                noderef, ReferenceKind::Callable));
        }
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_funpar.check(acc, NspFunparArgs::from_function_call(self));
        rules.nsp_inparen.check(acc, NspInparenArgs::from_function_call(self));
        rules.sp_punct.check(acc, SpPunctArgs::from_function_call(self));
    }
}

pub fn parse_function_call(fun: Expression,
                           context: &ParseContext,
                           stream: &mut FileParser<'_>,
                           file_info: &FileInfo) -> Expression {
    fn understands_rparen_and_comma(token: TokenKind) -> bool {
        token == TokenKind::RParen || token == TokenKind::Comma
    }
    let mut new_context = context.enter_context(understands_rparen_and_comma);
    let lparen = new_context.next_leaf(stream);
    let mut arguments = vec![];
    match new_context.peek_kind(stream) {
        Some(TokenKind::RParen) => (),
        Some(_) => {
            let mut done = false;
            while !done {
                let new_argument = SingleInitializer::parse(&new_context,
                                                            stream,
                                                            file_info);
                match new_context.peek_kind(stream) {
                    // A comma token keeps us in loop, anything else takes us
                    // out
                    Some(TokenKind::Comma) => arguments.push(
                        (new_argument, Some(new_context.next_leaf(stream)))),
                    // EOF here is treated as missing ')', not as missing ','
                    _ => {
                        arguments.push((new_argument, None));
                        done = true;
                    },
                }
            };
        },
        None => (),
    };
    let rparen = new_context.expect_next_kind(stream, TokenKind::RParen);
    ExpressionContent::FunctionCall(
        FunctionCallContent { fun, lparen, arguments, rparen }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewContent {
    pub new: LeafToken,
    pub typed: CTypeDecl,
    pub array: Option<(LeafToken, Expression, LeafToken)>,
}

impl TreeElement for NewContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.new.range(),
                       match &self.array {
                           Some((_, _, r)) => r.range(),
                           None => self.typed.range(),
                       })
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.new, &self.typed, &self.array)
    }
}

impl Parse<ExpressionContent> for NewContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_lbracket(token: TokenKind) -> bool {
            token == TokenKind::LBracket
        }
        fn understands_rbracket(token: TokenKind) -> bool {
            token == TokenKind::RBracket
        }
        let mut pre_lbracket_context = context.enter_context(
            understands_lbracket);
        // Guaranteed by caller
        let new = pre_lbracket_context.next_leaf(stream);
        let typed = CTypeDecl::parse(&pre_lbracket_context, stream, file_info);
        // TODO: There is a parsing conflict here betwen
        // new X[5] (create array of 5 X)
        // new X[5] (create X and index into it with 5)
        let array = match pre_lbracket_context.peek_kind(stream) {
            Some(TokenKind::LBracket) => {
                let lbracket = pre_lbracket_context.next_leaf(stream);
                let mut pre_rbracket_context = context.enter_context(
                    understands_rbracket);
                let arraysize = Expression::parse(
                    &pre_rbracket_context, stream, file_info);
                let rbracket = pre_rbracket_context.expect_next_kind(
                    stream, TokenKind::RBracket);
                Some((lbracket, arraysize, rbracket))
            },
            _ => None,
        };
        ExpressionContent::New(NewContent { new, typed, array }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastContent {
    pub cast: LeafToken,
    pub lparen: LeafToken,
    pub from: Expression,
    pub comma: LeafToken,
    pub to: CTypeDecl,
    pub rparen: LeafToken,
}

impl TreeElement for CastContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.cast.range(),
                       self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.cast, &self.lparen, &self.from,
                     &self.comma, &self.to, &self.rparen)
    }
}

impl Parse<ExpressionContent> for CastContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        // Guaranteed by caller
        let cast = outer.next_leaf(stream);
        let lparen = outer.expect_next_kind(stream, TokenKind::LParen);
        let mut paren_context = context.enter_context(understands_rparen);
        let from = Expression::parse(&paren_context, stream, file_info);
        let comma = paren_context.expect_next_kind(stream, TokenKind::Comma);
        let to = CTypeDecl::parse(&paren_context, stream, file_info);
        let rparen = outer.expect_next_kind(stream, TokenKind::RParen);
        ExpressionContent::Cast(
            CastContent { cast, lparen, from, comma, to, rparen }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstListContent {
    pub lbracket: LeafToken,
    pub elements: Vec<(Expression, Option<LeafToken>)>,
    pub rbracket: LeafToken,
}

impl TreeElement for ConstListContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.lbracket.range(), self.rbracket.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.lbracket, &self.elements, &self.rbracket)
    }
}

impl Parse<ExpressionContent> for ConstListContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_rbracket_and_comma(token: TokenKind) -> bool {
            token == TokenKind::RBracket || token == TokenKind::Comma
        }
        let mut new_context = context.enter_context(
            understands_rbracket_and_comma);
        // lbracket guaranteed by caller
        let lbracket = new_context.next_leaf(stream);
        let mut elements = vec![];
        match new_context.peek_kind(stream) {
            Some(TokenKind::RBracket) => (),
            Some(_) => {
                let mut done = false;
                while !done {
                    let item = Expression::parse(&new_context, stream, file_info);
                    match new_context.peek_kind(stream) {
                        // A comma token keeps us in loop, anything else takes
                        // us out
                        Some(TokenKind::Comma) => elements.push(
                            (item, Some(new_context.next_leaf(stream)))),
                        // EOF here is treated as missing ']', not as missing
                        // ','
                        _ => {
                            elements.push((item, None));
                            done = true;
                        },
                    };
                };
            },
            None => (),
        };
        let rbracket = new_context.expect_next_kind(
            stream, TokenKind::RBracket);
        ExpressionContent::ConstList(ConstListContent {
            lbracket,
            elements,
            rbracket,
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexContent {
    pub array: Expression,
    pub lbracket: LeafToken,
    pub index: Expression,
    pub rbracket: LeafToken,
}

impl TreeElement for IndexContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.array.range(), self.rbracket.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.array, &self.lbracket, &self.index, &self.rbracket)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_inparen.check(acc, NspInparenArgs::from_index(self));
    }
}

impl MaybeIsNodeRef for IndexContent {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        self.array.maybe_noderef(file)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SliceContent {
    pub val: Expression,
    pub lbracket: LeafToken,
    pub leftslice: Expression,
    pub rightslice: Option<(LeafToken, Expression)>,
    pub bitorder: Option<(LeafToken, LeafToken)>,
    pub rbracket: LeafToken,
}

impl TreeElement for SliceContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.val.range(), self.rbracket.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.val,
                     &self.lbracket,
                     &self.leftslice,
                     &self.rightslice,
                     &self.bitorder,
                     &self.rbracket)
    }
}

pub fn parse_index_or_slice(array: Expression,
                            context: &ParseContext,
                            stream: &mut FileParser<'_>,
                            file_info: &FileInfo) -> Expression {
    fn understands_rbracket(token: TokenKind) -> bool {
        token == TokenKind::RBracket
    }
    fn understands_comma(token: TokenKind) -> bool {
        token == TokenKind::Comma
    }
    fn understands_colon(token: TokenKind) -> bool {
        token == TokenKind::Colon
    }
    // These are unwound as we find more of out expected symbols
    let mut third_context = context.enter_context(understands_rbracket);
    let mut second_context = third_context.enter_context(understands_comma);
    let mut first_context = second_context.enter_context(understands_colon);
    let lbracket = first_context.next_leaf(stream);
    let index = Expression::parse(&first_context, stream, file_info);
    let (rightslice, bitorder) = match first_context.peek_kind(stream) {
        Some(TokenKind::RBracket) => (None, None),
        // Verification that the bitorder is LE or BE is done is post-parse
        // walk of ast tree
        Some(TokenKind::Comma) => {
            let comma = second_context.next_leaf(stream);
            let bitorder = second_context.expect_next_kind(
                stream, TokenKind::Identifier);
            (None, Some((comma, bitorder)))
        },
        Some(TokenKind::Colon) => {
            let colon = first_context.next_leaf(stream);
            let rindex = Expression::parse(&second_context,
                                           stream,
                                           file_info);
            let bitorder = match second_context.peek_kind(stream) {
                Some(TokenKind::Comma) =>{
                    let comma = second_context.next_leaf(stream);
                    let bo = second_context.expect_next_kind(
                        stream, TokenKind::Identifier);
                    Some((comma, bo))
                },
                _ => None,
            };
            (Some((colon, rindex)), bitorder)
        },
        _ => (None, None),
    };
    let rbracket = third_context.expect_next_kind(stream, TokenKind::RBracket);
    match (&rightslice, &bitorder) {
        (None, None) => ExpressionContent::Index(IndexContent {
            array, lbracket, index, rbracket
        }).into(),
        _ => ExpressionContent::Slice(SliceContent {
            val: array,
            lbracket,
            leftslice: index,
            rightslice,
            bitorder,
            rbracket,
        }).into(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EachInContent {
    pub each: LeafToken,
    pub ident: LeafToken,
    pub intok: LeafToken,
    pub lparen: LeafToken,
    pub of: Expression,
    pub rparen: LeafToken,
}

impl TreeElement for EachInContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.each.range(), self.rparen.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.each, &self.ident, &self.intok,
                     &self.lparen, &self.of, &self.rparen)
    }
}

impl Parse<ExpressionContent> for EachInContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_in(token: TokenKind) -> bool {
            token == TokenKind::In
        }
        fn understands_lparen(token: TokenKind) -> bool {
            token == TokenKind::LParen
        }
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut third_context = context.enter_context(understands_rparen);
        let mut second_context = third_context.enter_context(
            understands_lparen);
        let mut first_context = second_context.enter_context(understands_in);
        // guaranteed by grammar
        let each = first_context.next_leaf(stream);
        let ident = first_context.expect_next_filter(
            stream, objident_filter, "identifier");
        let intok = first_context.expect_next_kind(stream, TokenKind::In);
        let lparen = second_context.expect_next_kind(stream, TokenKind::LParen);
        let of = Expression::parse(&third_context, stream, file_info);
        let rparen = third_context.expect_next_kind(stream, TokenKind::RParen);
        ExpressionContent::EachIn(EachInContent {
            each, ident, intok, lparen, of, rparen
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizeOfContent {
    pub sizeof: LeafToken,
    pub of: Expression,
}

impl TreeElement for SizeOfContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.sizeof.range(), self.of.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.sizeof, &self.of)
    }
}

impl Parse<ExpressionContent> for SizeOfContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        // guaranteed by caller
        let sizeof = new_context.next_leaf(stream);
        let of = Expression::parse(&new_context, stream, file_info);
        ExpressionContent::SizeOf(SizeOfContent {
            sizeof, of
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SizeOfTypeArg {
    Raw(CTypeDecl),
    Paren(LeafToken, CTypeDecl, LeafToken),
}

impl TreeElement for SizeOfTypeArg {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Raw(typed) => typed.range(),
            Self::Paren(left, _, right) => Range::combine(left.range(),
                                                          right.range())
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Raw(typed) => create_subs!(typed),
            Self::Paren(left, middle, right) =>
                create_subs!(left, middle, right),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct SizeOfTypeContent {
    pub sizeoftype: LeafToken,
    pub arg: SizeOfTypeArg,
}

impl TreeElement for SizeOfTypeContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.sizeoftype.range(),
                       self.arg.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.sizeoftype, &self.arg)
    }
}

impl Parse<ExpressionContent> for SizeOfTypeContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Expression {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut new_context = context.enter_context(understands_rparen);
        // guaranteed by caller
        let sizeoftype = new_context.next_leaf(stream);
        let arg = match new_context.peek_kind(stream) {
            Some(TokenKind::LParen) => {
                let lparen = new_context.next_leaf(stream);
                let mut paren_context = new_context.enter_context(
                    understands_rparen);
                let of = CTypeDecl::parse(&paren_context, stream, file_info);
                let rparen = paren_context.expect_next_kind(
                    stream, TokenKind::RParen);
                SizeOfTypeArg::Paren(lparen, of, rparen)
            },
            _ => SizeOfTypeArg::Raw(CTypeDecl::parse(&new_context, stream, file_info)),
        };
        ExpressionContent::SizeOfType(SizeOfTypeContent {
            sizeoftype, arg
        }).into()
    }
}

fn parse_special_function(context: &ParseContext,
                          stream: &mut FileParser<'_>,
                          file_info: &FileInfo)
                          -> Expression {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    // Guaranteed by parser
    let funname = ExpressionContent::Identifier(
        new_context.next_leaf(stream)).into();
    parse_function_call(funname, &new_context, stream, file_info)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionContent {
    BinaryExpression(BinaryExpressionContent),
    MemberLiteral(MemberLiteralContent),
    ConstList(ConstListContent),
    ParenExpression(ParenExpressionContent),
    EachIn(EachInContent),
    FunctionCall(FunctionCallContent),
    Identifier(LeafToken),
    Index(IndexContent),
    Undefined(LeafToken),
    Literal(LeafToken),
    New(NewContent),
    Cast(CastContent),
    UnaryExpression(UnaryExpressionContent),
    PostUnaryExpression(PostUnaryExpressionContent),
    SizeOf(SizeOfContent),
    SizeOfType(SizeOfTypeContent),
    Slice(SliceContent),
    TertiaryExpression(TertiaryExpressionContent),
}

impl TreeElement for ExpressionContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Identifier(token) => token.range(),
            Self::Literal(token) => token.range(),
            Self::UnaryExpression(content) => content.range(),
            Self::PostUnaryExpression(content) => content.range(),
            Self::BinaryExpression(content) => content.range(),
            Self::MemberLiteral(content) => content.range(),
            Self::TertiaryExpression(content) => content.range(),
            Self::ParenExpression(content) => content.range(),
            Self::FunctionCall(content) => content.range(),
            Self::New(content) => content.range(),
            Self::Cast(content) => content.range(),
            Self::Index(content) => content.range(),
            Self::Slice(content) => content.range(),
            Self::ConstList(content) => content.range(),
            Self::EachIn(content) => content.range(),
            Self::SizeOf(content) => content.range(),
            Self::SizeOfType(content) => content.range(),
            Self::Undefined(content) => content.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Identifier(token) => create_subs![token],
            Self::Literal(token) => create_subs![token],
            Self::UnaryExpression(content) => create_subs![content],
            Self::PostUnaryExpression(content) => create_subs![content],
            Self::BinaryExpression(content) => create_subs![content],
            Self::MemberLiteral(content) => create_subs![content],
            Self::TertiaryExpression(content) => create_subs![content],
            Self::ParenExpression(content) => create_subs![content],
            Self::FunctionCall(content) => create_subs![content],
            Self::New(content) => create_subs![content],
            Self::Cast(content) => create_subs![content],
            Self::Index(content) => create_subs![content],
            Self::Slice(content) => create_subs![content],
            Self::ConstList(content) => create_subs![content],
            Self::EachIn(content) => create_subs![content],
            Self::SizeOf(content) => create_subs![content],
            Self::SizeOfType(content) => create_subs![content],
            Self::Undefined(token) => create_subs![token],
        }
    }

    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        self.default_references(accumulator, file);
        if let Some(refr) = self.maybe_noderef(file) {
            accumulator.push(Reference::from_noderef(
                refr, ReferenceKind::Variable));
        }
    }
}

impl MaybeIsNodeRef for ExpressionContent {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        match self {
            Self::Index(expr) => expr.maybe_noderef(file),
            Self::MemberLiteral(content) => content.maybe_noderef(file),
            Self::Identifier(ident) => ident.maybe_noderef(file),
            _ => None,
        }
    }
}

pub type Expression = AstObject<ExpressionContent>;

// Expose this, it is used as lookahead in a few other places
pub fn dmlexpression_first_token_matcher(token: TokenKind) -> bool {
    match token {
        TokenKind::StringConstant | TokenKind::IntConstant |
        TokenKind::HexConstant | TokenKind::BinaryConstant |
        TokenKind::CharConstant | TokenKind::FloatConstant |
        TokenKind::LParen | TokenKind::LBracket | TokenKind::Minus |
        TokenKind::Not | TokenKind::BinNot | TokenKind::PlusPlus |
        TokenKind::MinusMinus | TokenKind::BinAnd | TokenKind::Defined |
        TokenKind::New | TokenKind::SizeOf | TokenKind::SizeOfType|
        TokenKind::Stringify | TokenKind::Cast | TokenKind::Each |
        TokenKind::Default | TokenKind::This | TokenKind::Undefined |
        TokenKind::Multiply => true,
        _ => objident_filter(token),
    }
}

fn maybe_parse_extended_expression(preset_left: Option<Expression>,
                                   context: &ParseContext,
                                   stream: &mut FileParser<'_>,
                                   file_info: &FileInfo) -> Expression {
    fn understands_continuations(token: TokenKind) -> bool {
        matches!(token, TokenKind::LParen | TokenKind::PlusPlus
                 | TokenKind::MinusMinus | TokenKind::LBracket
                 | TokenKind::Dot | TokenKind::Arrow)
    }
    let mut new_context = context.enter_context(understands_continuations);
    let mut left = parse_expression_inner(
        preset_left, &new_context, stream, file_info);
    while new_context.peek_kind(stream).map_or(
        false, |k|understands_continuations(k)) {
        match new_context.peek_kind(stream) {
            Some(TokenKind::LParen) => left = parse_function_call(
                left, &new_context, stream, file_info),
            Some(TokenKind::PlusPlus | TokenKind::MinusMinus) =>
                left = ExpressionContent::PostUnaryExpression(
                PostUnaryExpressionContent {
                    expr: left,
                    operation: new_context.next_leaf(stream),
                }).into(),
            Some(TokenKind::LBracket) =>
                left = parse_index_or_slice(
                    left, &new_context, stream, file_info),
            Some(TokenKind::Dot | TokenKind::Arrow) => {
                let operation = new_context.next_leaf(stream);
                let right = new_context.expect_next_filter(
                    stream, objident_filter, "identifier");
                left = ExpressionContent::MemberLiteral(MemberLiteralContent {
                    operation,
                    left,
                    right,
                }).into()
            },
            _=> ()
        }
    }
    left
}

fn maybe_parse_muldivmod_expression(preset_left: Option<Expression>,
                                    context: &ParseContext,
                                    stream: &mut FileParser<'_>,
                                    file_info: &FileInfo) -> Expression {
    fn understands_muldivmods(token: TokenKind) -> bool {
        matches!(token, TokenKind::Divide | TokenKind::Multiply
                 | TokenKind::Mod)
    }
    let mut new_context = context.enter_context(understands_muldivmods);
    let mut left = maybe_parse_extended_expression(preset_left,
                                                   &new_context,
                                                   stream, file_info);
    if new_context.peek_kind(stream).map_or(false, understands_muldivmods) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_muldivmod_expression(None,
                                                     context,
                                                     stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_addsub_expression(preset_left: Option<Expression>,
                                 context: &ParseContext,
                                 stream: &mut FileParser<'_>,
                                 file_info: &FileInfo) -> Expression {
    fn understands_addsubs(token: TokenKind) -> bool {
        matches!(token, TokenKind::Plus | TokenKind::Minus)
    }
    let mut new_context = context.enter_context(understands_addsubs);
    let mut left = maybe_parse_muldivmod_expression(preset_left,
                                                    &new_context,
                                                    stream, file_info);
    if new_context.peek_kind(stream).map_or(false, understands_addsubs) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_addsub_expression(None,
                                                  context,
                                                  stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_shift_expression(preset_left: Option<Expression>,
                                context: &ParseContext,
                                stream: &mut FileParser<'_>,
                                file_info: &FileInfo) -> Expression {
    fn understands_shifts(token: TokenKind) -> bool {
        matches!(token, TokenKind::LShift | TokenKind::RShift)
    }
    let mut new_context = context.enter_context(understands_shifts);
    let mut left = maybe_parse_addsub_expression(preset_left,
                                                 &new_context,
                                                 stream, file_info);
    if new_context.peek_kind(stream).map_or(false, understands_shifts) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_shift_expression(None,
                                                 context,
                                                 stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_comparison_expression(preset_left: Option<Expression>,
                                     context: &ParseContext,
                                     stream: &mut FileParser<'_>,
                                     file_info: &FileInfo) -> Expression {
    fn understands_comparisons(token: TokenKind) -> bool {
        matches!(token, TokenKind::GreaterThan | TokenKind::LessThan
                 | TokenKind::GEquals | TokenKind::LEquals)
    }
    let mut new_context = context.enter_context(understands_comparisons);
    let mut left = maybe_parse_shift_expression(preset_left,
                                                &new_context,
                                                stream, file_info);
    if new_context.peek_kind(stream).map_or(false, understands_comparisons) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_comparison_expression(None,
                                                      context,
                                                      stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_equality_expression(preset_left: Option<Expression>,
                                   context: &ParseContext,
                                   stream: &mut FileParser<'_>,
                                   file_info: &FileInfo) -> Expression {
    fn understands_equality(token: TokenKind) -> bool {
        matches!(token, TokenKind::Equals | TokenKind::NotEquals)
    }
    let mut new_context = context.enter_context(understands_equality);
    let mut left = maybe_parse_comparison_expression(preset_left,
                                                     &new_context,
                                                     stream,
                                                     file_info);
    if new_context.peek_kind(stream).map_or(false, understands_equality) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_equality_expression(None,
                                                    context,
                                                    stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

// This encompasses |, &, and ^, they are all associative so binding order can
// be hard left-associative
fn maybe_parse_binary_calc_expression(preset_left: Option<Expression>,
                                      context: &ParseContext,
                                      stream: &mut FileParser<'_>,
                                      file_info: &FileInfo) -> Expression {
    fn understands_binaries(token: TokenKind) -> bool {
        matches!(token, TokenKind::BinAnd | TokenKind::BinOr
                 | TokenKind::BinXor)
    }
    let mut new_context = context.enter_context(understands_binaries);
    let mut left = maybe_parse_equality_expression(preset_left,
                                                   &new_context,
                                                   stream, file_info);
    if new_context.peek_kind(stream).map_or(false, understands_binaries) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_binary_calc_expression(None,
                                                       context,
                                                       stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_logic_and_expression(preset_left: Option<Expression>,
                                    context: &ParseContext,
                                    stream: &mut FileParser<'_>,
                                    file_info: &FileInfo) -> Expression {
    fn understands_logand(token: TokenKind) -> bool {
        token == TokenKind::And
    }
    let mut new_context = context.enter_context(understands_logand);
    let mut left = maybe_parse_binary_calc_expression(preset_left,
                                                      &new_context,
                                                      stream,
                                                      file_info);
    if new_context.peek_kind(stream) == Some(TokenKind::And) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_logic_and_expression(None,
                                                     context,
                                                     stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

fn maybe_parse_logic_or_expression(preset_left: Option<Expression>,
                                   context: &ParseContext,
                                   stream: &mut FileParser<'_>,
                                   file_info: &FileInfo) -> Expression {
    fn understands_logor(token: TokenKind) -> bool {
        token == TokenKind::Or
    }
    let mut new_context = context.enter_context(understands_logor);
    let mut left = maybe_parse_logic_and_expression(preset_left,
                                                    &new_context,
                                                    stream, file_info);
    if new_context.peek_kind(stream) == Some(TokenKind::Or) {
        let operation = new_context.next_leaf(stream);
        let right = maybe_parse_logic_or_expression(None,
                                                    context,
                                                    stream, file_info);
        left = ExpressionContent::BinaryExpression(BinaryExpressionContent {
            operation,
            left,
            right,
        }).into();
    }
    left
}

pub fn maybe_parse_tertiary_expression(preset_left: Option<Expression>,
                                       context: &ParseContext,
                                       stream: &mut FileParser<'_>,
                                       file_info: &FileInfo) -> Expression {
    fn understands_tert_first_ops(token: TokenKind) -> bool {
        matches!(token, TokenKind::CondOp | TokenKind::HashCondOp)
    }
    fn understands_tert_second_ops(token: TokenKind) -> bool {
        matches!(token, TokenKind::Colon | TokenKind::HashColon)
    }
    let outer_context = context.enter_context(doesnt_understand_tokens);
    // TODO/NOTE: It is possible to improve this in cases like;
    // "expr : expr"
    // giving us;
    // <expr, <expected ?>, <expected expr>, :, expr>
    let mut pre_colon_context = outer_context.enter_context(
        understands_tert_second_ops);
    let mut pre_quest_context = outer_context.enter_context(
        understands_tert_first_ops);

    let mut left = maybe_parse_logic_or_expression(preset_left,
                                                   &pre_quest_context,
                                                   stream, file_info);
    if pre_quest_context.peek_kind(stream).map_or(
        false, |k|understands_tert_first_ops(k)) {
        let first_opr = pre_quest_context.next_leaf(stream);
        let opr_kind = match first_opr {
            LeafToken::Actual(token) => token.kind,
            _ => panic!("Unexpected EOF"),
        };
        let middle = Expression::parse(&pre_colon_context, stream, file_info);
        let middle_opr = pre_colon_context.expect_next_kind(
            stream, match opr_kind {
                TokenKind::CondOp => TokenKind::Colon,
                TokenKind::HashCondOp => TokenKind::HashColon,
                _ => panic!("Unexpected conditional OP kind"),
            });
        // This ends up being a left-associative recursive call
        let right = Expression::parse(&outer_context, stream, file_info);
        left = ExpressionContent::TertiaryExpression(
            TertiaryExpressionContent {
                left,
                left_operation: first_opr,
                middle,
                right_operation: middle_opr,
                right,
            }).into();
    }
    left
}

fn parse_expression_inner(preset_left: Option<Expression>,
                          context: &ParseContext,
                          stream: &mut FileParser<'_>,
                          file_info: &FileInfo) -> Expression {
    if let Some(left) = preset_left {
        return left;
    }
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.expect_peek_filter(
        stream, dmlexpression_first_token_matcher, "expression") {
        LeafToken::Actual(token) => match token.kind {
            TokenKind::StringConstant | TokenKind::IntConstant |
            TokenKind::HexConstant | TokenKind::BinaryConstant |
            TokenKind::CharConstant | TokenKind::FloatConstant =>
                ExpressionContent::Literal(
                    new_context.next_leaf(stream)).into(),
            TokenKind::LParen => ParenExpressionContent::parse(
                &new_context, stream, file_info),
            TokenKind::LBracket => ConstListContent::parse(
                &new_context, stream, file_info),
            TokenKind::Minus | TokenKind::Not | TokenKind::BinNot |
            TokenKind::PlusPlus | TokenKind::MinusMinus |
            TokenKind::BinAnd | TokenKind::Defined | TokenKind::Multiply =>
                UnaryExpressionContent::parse(
                    &new_context, stream, file_info),
            TokenKind::New => NewContent::parse(&new_context, stream, file_info),
            TokenKind::SizeOf => SizeOfContent::parse(&new_context, stream, file_info),
            TokenKind::SizeOfType => SizeOfTypeContent::parse(
                &new_context, stream, file_info),
            TokenKind::Stringify => parse_special_function(
                &new_context, stream, file_info),
            TokenKind::Cast => CastContent::parse(&new_context, stream, file_info),
            TokenKind::Each => EachInContent::parse(&new_context, stream, file_info),
            TokenKind::Default => ExpressionContent::Identifier(
                new_context.next_leaf(stream)).into(),
                TokenKind::This => ExpressionContent::Identifier(
                    new_context.next_leaf(stream)).into(),
            TokenKind::Undefined => ExpressionContent::Undefined(
                new_context.next_leaf(stream)).into(),
            _ => {
                if objident_filter(token.kind) {
                    ExpressionContent::Identifier(
                        new_context.next_leaf(stream)).into()
                } else {
                    unreachable!()
                }
            },
        },
        LeafToken::Missing(missing) => missing.into(),
    }
}

impl Parse<ExpressionContent> for ExpressionContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> Expression {
        maybe_parse_tertiary_expression(None, context, stream, file_info)
    }
}

impl Parse<ExpressionContent> for Expression {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> Expression {
        ExpressionContent::parse(context, stream, file_info)
    }
}

// Ensure an expression only consists of string constants, adds, or missing
// tokens
pub fn ensure_string_concatenation(expr: &Expression) -> Vec<LocalDMLError> {
    fn check_content(content: &ExpressionContent) -> Vec<LocalDMLError> {
        match &content {
            ExpressionContent::BinaryExpression(exprcontent) => {
                let mut errors = exprcontent.operation.with_actual(
                    |tok: &Token| if tok.kind != TokenKind::Plus {
                        vec![LocalDMLError{
                            range: tok.range,
                            description: format!(
                                "Can only compose string literals with '+', got {}",
                                tok.kind.description())}]
                    } else {
                        vec![]
                    }, vec![]);
                errors.append(
                    &mut ensure_string_concatenation(&exprcontent.left));
                errors.append(
                    &mut ensure_string_concatenation(&exprcontent.right));
                errors
            },
            ExpressionContent::Literal(literalleaf) => {
                literalleaf.with_actual(
                    |tok| if tok.kind != TokenKind::StringConstant {
                        vec![LocalDMLError {
                            range: tok.range,
                            description: format!(
                                "Expected string literal, got {}",
                                tok.kind.description())
                        }]} else {
                        vec![]
                    }, vec![])},
            ExpressionContent::ParenExpression(paren) => {
                ensure_string_concatenation(&paren.expr)
            },
            _ => vec![LocalDMLError {
                range: content.range(),
                description: "Expected composed string literal".to_string(),
            }],
        }
    }
    expr.with_content(check_content, vec![])
}

// TODO: expand tests
#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[allow(clippy::ptr_arg)]
    fn test_expression_tree(source: &str,
                            expected_ast: &Expression,
                            expected_skipped_tokens: &Vec<Token>) {
        test_ast_tree::<ExpressionContent, Expression>(source,
                                                       expected_ast,
                                                       expected_skipped_tokens)
    }

    #[test]
    fn simple_identifier() {
        test_expression_tree(
            "a",
            &make_ast(zero_range(0, 0, 0, 1),
                      ExpressionContent::Identifier(make_leaf(
                          zero_range(0, 0, 0, 0),
                          zero_range(0, 0, 0, 1),
                          TokenKind::Identifier
                      ))),
            &vec![]);
    }

    #[test]
    fn simple_expression() {
        let a = make_ast(
            zero_range(0, 0, 0, 1),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 1),
                TokenKind::Identifier
            )));
        let five = make_ast(
            zero_range(0, 0, 3, 4),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 3, 3),
                zero_range(0, 0, 3, 4),
                TokenKind::IntConstant
            )));
        let four = make_ast(
            zero_range(0, 0, 5, 6),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 5, 5),
                zero_range(0, 0, 5, 6),
                TokenKind::IntConstant
            )));
        let five_minus_four = make_ast(
            zero_range(0, 0, 3, 6),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: five,
                operation: make_leaf(
                    zero_range(0, 0, 4, 4),
                    zero_range(0, 0, 4, 5),
                    TokenKind::Minus
                ),
                right: four,
            }));
        let parens = make_ast(
            zero_range(0, 0, 2, 7),
            ExpressionContent::ParenExpression(ParenExpressionContent {
                lparen: make_leaf(
                    zero_range(0, 0, 2, 2),
                    zero_range(0, 0, 2, 3),
                    TokenKind::LParen
                ),
                expr: five_minus_four,
                rparen: make_leaf(
                    zero_range(0, 0, 6, 6),
                    zero_range(0, 0, 6, 7),
                    TokenKind::RParen
                ),
            }));
        let three = make_ast(
            zero_range(0, 0, 8, 9),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 8, 8),
                zero_range(0, 0, 8, 9),
                TokenKind::IntConstant
            )));
        let parens_plus_three = make_ast(
            zero_range(0, 0, 2, 9),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: parens,
                operation: make_leaf(
                    zero_range(0, 0, 7, 7),
                    zero_range(0, 0, 7, 8),
                    TokenKind::Plus
                ),
                right: three,
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 9),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: a,
                operation: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::Plus
                ),
                right: parens_plus_three,
            }));
        test_expression_tree(
            "a+(5-4)+3",
            &expected,
            &vec![]);
    }

    #[test]
    fn another_simple_expression() {
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
        let three = make_ast(
            zero_range(0, 0, 6, 7),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 6, 6),
                zero_range(0, 0, 6, 7),
                TokenKind::IntConstant
            )));
        let j = make_ast(
            zero_range(0, 0, 8, 9),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 8, 8),
                zero_range(0, 0, 8, 9),
                TokenKind::Identifier
            )));
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
        let parens_times_three = make_ast(
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
            zero_range(0, 0, 0, 9),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: parens_times_three,
                operation: make_leaf(
                    zero_range(0, 0, 7, 7),
                    zero_range(0, 0, 7, 8),
                    TokenKind::Plus
                ),
                right: j,
            }));
        test_expression_tree(
            "(i+1)*3+j",
            &expected,
            &vec![]);
    }

    #[test]
    fn function_call() {
        use crate::analysis::parsing::misc::SingleInitializerContent;
        let foo_name = make_ast(
            zero_range(0, 0, 0, 3),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 3),
                TokenKind::Identifier
            )));
        let five = make_ast(
            zero_range(0, 0, 4, 5),
            SingleInitializerContent::Expression(
                make_ast(
                    zero_range(0, 0, 4, 5),
                    ExpressionContent::Literal(make_leaf(
                        zero_range(0, 0, 4, 4),
                        zero_range(0, 0, 4, 5),
                        TokenKind::IntConstant
                    )))));
        let a = make_ast(
            zero_range(0, 0, 7, 8),
            SingleInitializerContent::Expression(
                make_ast(
                    zero_range(0, 0, 7, 8),
                    ExpressionContent::Identifier(make_leaf(
                        zero_range(0, 0, 6, 7),
                        zero_range(0, 0, 7, 8),
                        TokenKind::Identifier
                    )))));
        let expected = make_ast(
            zero_range(0, 0, 0, 9),
            ExpressionContent::FunctionCall(FunctionCallContent {
                fun: foo_name,
                lparen: make_leaf(
                    zero_range(0, 0, 3, 3),
                    zero_range(0, 0, 3, 4),
                    TokenKind::LParen
                ),
                arguments: vec![(five,
                                 Some(make_leaf(
                                     zero_range(0, 0, 5, 5),
                                     zero_range(0, 0, 5, 6),
                                     TokenKind::Comma))),
                                (a, None)],
                rparen: make_leaf(
                    zero_range(0, 0, 8, 8),
                    zero_range(0, 0, 8, 9),
                    TokenKind::RParen
                )
            }));
        test_expression_tree(
            "foo(5, a)",
            &expected,
            &vec![]);
    }

    #[test]
    fn cast() {
        use crate::analysis::parsing::types::{CTypeDeclContent, CTypeDeclSimpleContent,
                                     BaseTypeContent};
        let five = make_ast(
            zero_range(0, 0, 5, 6),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 5, 5),
                zero_range(0, 0, 5, 6),
                TokenKind::IntConstant
            )));
        let basetype = make_ast(
            zero_range(0, 0, 8, 14),
            BaseTypeContent::Ident(make_leaf(
                zero_range(0, 0, 7, 8),
                zero_range(0, 0, 8, 14),
                TokenKind::Identifier
            )));
        let uint64 = make_ast(
            zero_range(0, 0, 8, 14),
            CTypeDeclContent {
                consttok: None,
                base: basetype,
                simple: make_ast(
                    zero_range(0, 0, 0, 0),
                    CTypeDeclSimpleContent {
                        modifiers: vec![],
                        inner: None
                    })
            });
        let expected = make_ast(
            zero_range(0, 0, 0, 15),
            ExpressionContent::Cast(CastContent {
                cast: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 4),
                    TokenKind::Cast
                ),
                lparen: make_leaf(
                    zero_range(0, 0, 4, 4),
                    zero_range(0, 0, 4, 5),
                    TokenKind::LParen
                ),
                from: five,
                comma: make_leaf(
                    zero_range(0, 0, 6, 6),
                    zero_range(0, 0, 6, 7),
                    TokenKind::Comma
                ),
                to: uint64,
                rparen: make_leaf(
                    zero_range(0, 0, 14, 14),
                    zero_range(0, 0, 14, 15),
                    TokenKind::RParen
                )
            }));
        test_expression_tree(
            "cast(5, uint64)",
            &expected,
            &vec![]);
    }

    #[test]
    fn const_list() {
        let five = make_ast(
            zero_range(0, 0, 1, 2),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 1, 1),
                zero_range(0, 0, 1, 2),
                TokenKind::IntConstant
            )));
        let a = make_ast(
            zero_range(0, 0, 4, 5),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 3, 4),
                zero_range(0, 0, 4, 5),
                TokenKind::Identifier
            )));
        let expected = make_ast(
            zero_range(0, 0, 0, 6),
            ExpressionContent::ConstList(ConstListContent {
                lbracket: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 1),
                    TokenKind::LBracket
                ),
                elements: vec![(five,
                                Some(make_leaf(
                                    zero_range(0, 0, 2, 2),
                                    zero_range(0, 0, 2, 3),
                                    TokenKind::Comma))),
                               (a, None)],
                rbracket: make_leaf(
                    zero_range(0, 0, 5, 5),
                    zero_range(0, 0, 5, 6),
                    TokenKind::RBracket
                )
            }));
        test_expression_tree(
            "[5, a]",
            &expected,
            &vec![]);
    }

    #[test]
    fn index_chain() {
        let identifier = make_ast(
            zero_range(0, 0, 0, 3),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 3),
                TokenKind::Identifier
            )));
        let two = make_ast(
            zero_range(0, 0, 4, 5),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 4, 4),
                zero_range(0, 0, 4, 5),
                TokenKind::IntConstant
            )));
        let three = make_ast(
            zero_range(0, 0, 6, 7),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 6, 6),
                zero_range(0, 0, 6, 7),
                TokenKind::IntConstant
            )));
        let two_plus_three = make_ast(
            zero_range(0, 0, 4, 7),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: two,
                operation: make_leaf(
                    zero_range(0, 0, 5, 5),
                    zero_range(0, 0, 5, 6),
                    TokenKind::Plus
                ),
                right: three,
            }));
        let eleven = make_ast(
            zero_range(0, 0, 9, 11),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 9, 9),
                zero_range(0, 0, 9, 11),
                TokenKind::IntConstant
            )));
        let first_index = make_ast(
            zero_range(0, 0, 0, 8),
            ExpressionContent::Index(IndexContent {
                array: identifier,
                lbracket: make_leaf(
                    zero_range(0, 0, 3, 3),
                    zero_range(0, 0, 3, 4),
                    TokenKind::LBracket
                ),
                index: two_plus_three,
                rbracket: make_leaf(
                    zero_range(0, 0, 7, 7),
                    zero_range(0, 0, 7, 8),
                    TokenKind::RBracket
                ),
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 12),
            ExpressionContent::Index(IndexContent {
                array: first_index,
                lbracket: make_leaf(
                    zero_range(0, 0, 8, 8),
                    zero_range(0, 0, 8, 9),
                    TokenKind::LBracket
                ),
                index: eleven,
                rbracket: make_leaf(
                    zero_range(0, 0, 11, 11),
                    zero_range(0, 0, 11, 12),
                    TokenKind::RBracket
                ),
            }));
        test_expression_tree(
            "foo[2+3][11]",
            &expected,
            &vec![]);
    }

    #[test]
    fn slices() {
        let identifier = make_ast(
            zero_range(0, 0, 0, 1),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 1),
                TokenKind::Identifier
            )));
        let four = make_ast(
            zero_range(0, 0, 2, 3),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 2, 2),
                zero_range(0, 0, 2, 3),
                TokenKind::IntConstant
            )));
        let five = make_ast(
            zero_range(0, 0, 4, 5),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 4, 4),
                zero_range(0, 0, 4, 5),
                TokenKind::IntConstant
            )));
        let expected_1 = make_ast(
            zero_range(0, 0, 0, 8),
            ExpressionContent::Slice(SliceContent {
                val: identifier.clone(),
                lbracket: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::LBracket
                ),
                leftslice: four.clone(),
                rightslice: None,
                bitorder: Some((
                    make_leaf(
                        zero_range(0, 0, 3, 3),
                        zero_range(0, 0, 3, 4),
                        TokenKind::Comma),
                    make_leaf(
                        zero_range(0, 0, 4, 5),
                        zero_range(0, 0, 5, 7),
                        TokenKind::Identifier))
                ),
                rbracket: make_leaf(
                    zero_range(0, 0, 7, 7),
                    zero_range(0, 0, 7, 8),
                    TokenKind::RBracket
                ),
            }));
        test_expression_tree(
            "a[4, le]",
            &expected_1,
            &vec![]);
        let expected_2 = make_ast(
            zero_range(0, 0, 0, 10),
            ExpressionContent::Slice(SliceContent {
                val: identifier.clone(),
                lbracket: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::LBracket
                ),
                leftslice: four.clone(),
                rightslice: Some((
                    make_leaf(
                        zero_range(0, 0, 3, 3),
                        zero_range(0, 0, 3, 4),
                        TokenKind::Colon),
                    five.clone())),
                bitorder: Some((
                    make_leaf(
                        zero_range(0, 0, 5, 5),
                        zero_range(0, 0, 5, 6),
                        TokenKind::Comma),
                    make_leaf(
                        zero_range(0, 0, 6, 7),
                        zero_range(0, 0, 7, 9),
                        TokenKind::Identifier))),
                rbracket: make_leaf(
                    zero_range(0, 0, 9, 9),
                    zero_range(0, 0, 9, 10),
                    TokenKind::RBracket
                ),
            }));
        test_expression_tree(
            "a[4:5, be]",
            &expected_2,
            &vec![]);
        let expected_3 = make_ast(
            zero_range(0, 0, 0, 6),
            ExpressionContent::Slice(SliceContent {
                val: identifier,
                lbracket: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::LBracket
                ),
                leftslice: four,
                rightslice: Some((make_leaf(
                    zero_range(0, 0, 3, 3),
                    zero_range(0, 0, 3, 4),
                    TokenKind::Colon),
                                  five)),
                bitorder: None,
                rbracket: make_leaf(
                    zero_range(0, 0, 5, 5),
                    zero_range(0, 0, 5, 6),
                    TokenKind::RBracket
                ),
            }));
        test_expression_tree(
            "a[4:5]",
            &expected_3,
            &vec![]);
    }

    #[test]
    fn unary_expression() {
        let minus_ten = make_ast(
            zero_range(0, 0, 2, 5),
            ExpressionContent::UnaryExpression(UnaryExpressionContent {
                operation: make_leaf(
                    zero_range(0, 0, 2, 2),
                    zero_range(0, 0, 2, 3),
                    TokenKind::Minus
                ),
                expr: make_ast(
                    zero_range(0, 0, 3, 5),
                    ExpressionContent::Literal(make_leaf(
                        zero_range(0, 0, 3, 3),
                        zero_range(0, 0, 3, 5),
                        TokenKind::IntConstant
                    ))
                )},
            ));
        let five = make_ast(
            zero_range(0, 0, 0, 1),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 1),
                TokenKind::IntConstant
            )));
        let six = make_ast(
            zero_range(0, 0, 6, 7),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 6, 6),
                zero_range(0, 0, 6, 7),
                TokenKind::IntConstant
            )));
        let five_times_minus = make_ast(
            zero_range(0, 0, 0, 5),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: five,
                operation: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::Multiply
                ),
                right: minus_ten,
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 7),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: five_times_minus,
                operation: make_leaf(
                    zero_range(0, 0, 5, 5),
                    zero_range(0, 0, 5, 6),
                    TokenKind::Plus
                ),
                right: six,
            }));
        // Tests precedence also (bind as -10 and not -(10+6))
        test_expression_tree(
            "5*-10+6",
            &expected,
            &vec![],
        );
    }

    #[test]
    fn binary_expression() {
        let five = make_ast(
            zero_range(0, 0, 0, 1),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 1),
                TokenKind::IntConstant
            )));
        let ten = make_ast(
            zero_range(0, 0, 2, 4),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 2, 2),
                zero_range(0, 0, 2, 4),
                TokenKind::IntConstant
            )));
        let six = make_ast(
            zero_range(0, 0, 5, 6),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 5, 5),
                zero_range(0, 0, 5, 6),
                TokenKind::IntConstant
            )));
        let five_times_ten = make_ast(
            zero_range(0, 0, 0, 4),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: five,
                operation: make_leaf(
                    zero_range(0, 0, 1, 1),
                    zero_range(0, 0, 1, 2),
                    TokenKind::Multiply
                ),
                right: ten,
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 6),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: five_times_ten,
                operation: make_leaf(
                    zero_range(0, 0, 4, 4),
                    zero_range(0, 0, 4, 5),
                    TokenKind::Plus
                ),
                right: six,
            }));
        // Tests precedence also (bind as -10 and not -(10+6))
        test_expression_tree(
            "5*10+6",
            &expected,
            &vec![],
        );
    }

    #[test]
    fn tertiary_expression() {
        let true_ident = make_ast(
            zero_range(0, 0, 0, 4),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 0, 0),
                zero_range(0, 0, 0, 4),
                TokenKind::Identifier
            )));
        let five = make_ast(
            zero_range(0, 0, 5, 6),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 5, 5),
                zero_range(0, 0, 5, 6),
                TokenKind::IntConstant
            )));
        let one = make_ast(
            zero_range(0, 0, 7, 8),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 7, 7),
                zero_range(0, 0, 7, 8),
                TokenKind::IntConstant
            )));
        let two = make_ast(
            zero_range(0, 0, 9, 10),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 9, 9),
                zero_range(0, 0, 9, 10),
                TokenKind::IntConstant
            )));
        let one_plus_two = make_ast(
            zero_range(0, 0, 7, 10),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: one,
                operation: make_leaf(
                    zero_range(0, 0, 8, 8),
                    zero_range(0, 0, 8, 9),
                    TokenKind::Plus
                ),
                right: two,
            }));
        let expected = make_ast(
            zero_range(0, 0, 0, 10),
            ExpressionContent::TertiaryExpression(TertiaryExpressionContent {
                left: true_ident,
                left_operation: make_leaf(
                    zero_range(0, 0, 4, 4),
                    zero_range(0, 0, 4, 5),
                    TokenKind::CondOp
                ),
                middle: five,
                right_operation: make_leaf(
                    zero_range(0, 0, 6, 6),
                    zero_range(0, 0, 6, 7),
                    TokenKind::Colon
                ),
                right: one_plus_two
            }));
        // Tests precedence also (bind as -10 and not -(10+6))
        test_expression_tree(
            "true?5:1+2",
            &expected,
            &vec![],
        );
    }

    #[test]
    fn new_variants() {
        use crate::analysis::parsing::types::{CTypeDeclContent, CTypeDeclSimpleContent,
                                     BaseTypeContent};
        let new = make_leaf(
            zero_range(0, 0, 0, 0),
            zero_range(0, 0, 0, 3),
            TokenKind::New
        );
        let basetype = make_ast(
            zero_range(0, 0, 4, 7),
            BaseTypeContent::Ident(make_leaf(
                zero_range(0, 0, 3, 4),
                zero_range(0, 0, 4, 7),
                TokenKind::Identifier
            )));
        let typed = make_ast(
            zero_range(0, 0, 4, 7),
            CTypeDeclContent {
                consttok: None,
                base: basetype,
                simple: make_ast(
                    zero_range(0, 0, 0, 0),
                    CTypeDeclSimpleContent {
                        modifiers: vec![],
                        inner: None
                    })
            });
        let expected_1 = make_ast(
            zero_range(0, 0, 0, 7),
            ExpressionContent::New(NewContent {
                new: new.clone(),
                typed: typed.clone(),
                array: None,
            }));
        let expected_2 = make_ast(
            zero_range(0, 0, 0, 10),
            ExpressionContent::New(NewContent {
                new,
                typed,
                array: Some((
                    make_leaf(
                        zero_range(0, 0, 7, 7),
                        zero_range(0, 0, 7, 8),
                        TokenKind::LBracket),
                    make_ast(
                        zero_range(0, 0, 8, 9),
                        ExpressionContent::Literal(
                            make_leaf(
                                zero_range(0, 0, 8, 8),
                                zero_range(0, 0, 8, 9),
                                TokenKind::IntConstant
                            ))),
                    make_leaf(
                        zero_range(0, 0, 9, 9),
                        zero_range(0, 0, 9, 10),
                        TokenKind::RBracket
                    ))),
            }));
        test_expression_tree(
            "new foo",
            &expected_1,
            &vec![]);
        test_expression_tree(
            "new foo[4]",
            &expected_2,
            &vec![]);
    }

    #[test]
    fn each_in() {
        let bar = make_ast(
            zero_range(0, 0, 13, 16),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 13, 13),
                zero_range(0, 0, 13, 16),
                TokenKind::Identifier
            )));
        let expected = make_ast(
            zero_range(0, 0, 0, 17),
            ExpressionContent::EachIn(EachInContent {
                each: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 4),
                    TokenKind::Each
                ),
                ident: make_leaf(
                    zero_range(0, 0, 4, 5),
                    zero_range(0, 0, 5, 8),
                    TokenKind::Identifier
                ),
                intok: make_leaf(
                    zero_range(0, 0, 8, 9),
                    zero_range(0, 0, 9, 11),
                    TokenKind::In
                ),
                lparen: make_leaf(
                    zero_range(0, 0, 11, 12),
                    zero_range(0, 0, 12, 13),
                    TokenKind::LParen
                ),
                of: bar,
                rparen: make_leaf(
                    zero_range(0, 0, 16, 16),
                    zero_range(0, 0, 16, 17),
                    TokenKind::RParen
                ),
            }));
        test_expression_tree(
            "each foo in (bar)",
            &expected,
            &vec![]);
    }

    #[test]
    fn sizeof() {
        let five = make_ast(
            zero_range(0, 0, 7, 8),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 6, 7),
                zero_range(0, 0, 7, 8),
                TokenKind::IntConstant
            )));
        let expected = make_ast(
            zero_range(0, 0, 0, 8),
            ExpressionContent::SizeOf(SizeOfContent {
                sizeof: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 6),
                    TokenKind::SizeOf
                ),
                of: five,
            }));
        test_expression_tree(
            "sizeof 5",
            &expected,
            &vec![]);
    }

    #[test]
    fn sizeoftype() {
        use crate::analysis::parsing::types::{CTypeDeclContent, CTypeDeclSimpleContent,
                                     BaseTypeContent};
        let basetype = make_ast(
            zero_range(0, 0, 11, 14),
            BaseTypeContent::Ident(make_leaf(
                zero_range(0, 0, 11, 11),
                zero_range(0, 0, 11, 14),
                TokenKind::Identifier)));
        let foo_name = make_ast(
            zero_range(0, 0, 11, 14),
            CTypeDeclContent {
                consttok: None,
                base: basetype,
                simple: make_ast(
                    zero_range(0, 0, 0, 0),
                    CTypeDeclSimpleContent {
                        modifiers: vec![],
                        inner: None
                    }),
            });
        let expected = make_ast(
            zero_range(0, 0, 0, 15),
            ExpressionContent::SizeOfType(SizeOfTypeContent {
                sizeoftype: make_leaf(
                    zero_range(0, 0, 0, 0),
                    zero_range(0, 0, 0, 10),
                    TokenKind::SizeOfType
                ),
                arg: SizeOfTypeArg::Paren(
                    make_leaf(
                        zero_range(0, 0, 10, 10),
                        zero_range(0, 0, 10, 11),
                        TokenKind::LParen
                    ),
                    foo_name,
                    make_leaf(
                        zero_range(0, 0, 14, 14),
                        zero_range(0, 0, 14, 15),
                        TokenKind::RParen
                    )),
            }));
        test_expression_tree(
            "sizeoftype(foo)",
            &expected,
            &vec![]);
    }

    #[test]
    fn stringify() {
        use crate::analysis::parsing::misc::SingleInitializerContent;
        let five = make_ast(
            zero_range(0, 0, 10, 11),
            SingleInitializerContent::Expression(
                make_ast(
                    zero_range(0, 0, 10, 11),
                    ExpressionContent::Literal(make_leaf(
                        zero_range(0, 0, 10, 10),
                        zero_range(0, 0, 10, 11),
                        TokenKind::IntConstant
                    )))));
        let expected = make_ast(
            zero_range(0, 0, 0, 12),
            ExpressionContent::FunctionCall(FunctionCallContent {
                fun: make_ast(
                    zero_range(0, 0, 0, 9),
                    ExpressionContent::Identifier(
                        make_leaf(
                            zero_range(0, 0, 0, 0),
                            zero_range(0, 0, 0, 9),
                            TokenKind::Stringify))),
                lparen: make_leaf(
                    zero_range(0, 0, 9, 9),
                    zero_range(0, 0, 9, 10),
                    TokenKind::LParen),
                arguments: vec![(five, None)],
                rparen: make_leaf(
                    zero_range(0, 0, 11, 11),
                    zero_range(0, 0, 11, 12),
                    TokenKind::RParen
                )
            }));
        test_expression_tree(
            "stringify(5)",
            &expected,
            &vec![]);
    }
}

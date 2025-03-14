//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use log::error;

use crate::span::Range;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::parser::{Token, doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::parsing::tree::{AstObject, Content, TreeElement, TreeElements,
                                     LeafToken, ZeroRange, MissingContent};
use crate::analysis::parsing::expression::{Expression,
                                           dmlexpression_first_token_matcher,
                                           ensure_string_concatenation,
                                           maybe_parse_tertiary_expression,
                                           MemberLiteralContent,
                                           ExpressionContent,
                                           ParenExpressionContent};
use crate::analysis::parsing::misc::{Initializer, InitializerContent, CDecl,
                                     SingleInitializerContent,
                                     ident_filter, objident_filter};
use crate::analysis::parsing::structure::{parse_vardecl, VarDecl};
use crate::analysis::LocalDMLError;
use crate::lint::{rules::{CurrentRules,
                         indentation::{IN3Args, IN9Args},
                         spacing::{NspInparenArgs,
                                   SpBracesArgs,
                                   SpPunctArgs}},
                                   AuxParams};
use crate::vfs::TextFile;

fn statement_contexts(context: &ParseContext)
                      -> (ParseContext, ParseContext) {
    fn understands_semi(token: TokenKind) -> bool {
        token == TokenKind::SemiColon
    }
    let outer_context = context.enter_context(understands_semi);
    let inner_context = outer_context.enter_context(
        doesnt_understand_tokens);
    (outer_context, inner_context)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorContent {
    pub error: LeafToken,
    pub message: Option<Expression>,
    pub semi: LeafToken,
}

impl TreeElement for ErrorContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.error.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.error, &self.message, &self.semi)
    }
}

impl Parse<StatementContent> for ErrorContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let error = inner.expect_next_kind(stream, TokenKind::Error);
        let message = match inner.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(Expression::parse(&inner, stream, file_info)),
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Error(ErrorContent {
            error, message, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssertContent {
    pub assert: LeafToken,
    pub expression: Expression,
    pub semi: LeafToken,
}

impl TreeElement for AssertContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.assert.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.assert, &self.expression, &self.semi)
    }
}

impl Parse<StatementContent> for AssertContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let assert = inner.expect_next_kind(stream, TokenKind::Assert);
        let expression = Expression::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Assert(AssertContent {
            assert, expression, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThrowContent {
    pub throw: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for ThrowContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.throw.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.throw, &self.semi)
    }
}

impl Parse<StatementContent> for ThrowContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let throw = inner.expect_next_kind(stream, TokenKind::Throw);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Throw(ThrowContent {
            throw, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundContent {
    pub lbrace: LeafToken,
    pub statements: Vec<Statement>,
    pub rbrace: LeafToken,
}

impl TreeElement for CompoundContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.lbrace.range(), self.rbrace.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.lbrace, &self.statements, &self.rbrace)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, aux: &mut AuxParams) {
        rules.sp_brace.check(acc, SpBracesArgs::from_compound(self));
        rules.in3.check(acc, IN3Args::from_compound_content(self, &mut aux.depth));
    }
}

impl Parse<StatementContent> for CompoundContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_rbrace(token: TokenKind) -> bool {
            token == TokenKind::RBrace
        }
        let mut new_context = context.enter_context(understands_rbrace);
        let mut statement_context = new_context.enter_context(
            dmlstatement_first_token_matcher);
        let lbrace = statement_context.expect_next_kind(
            stream, TokenKind::LBrace);
        let mut statements = vec![];
        let mut cont = !matches!(statement_context.peek_kind(stream),
                                 Some(TokenKind::RBrace)
                                 | None);
        while cont {
            statements.push(Statement::parse(&statement_context,
                                             stream, file_info));
            cont = statement_context.peek_kind(stream)
                .map_or(false, dmlstatement_first_token_matcher);
        }
        let rbrace = new_context.expect_next_kind(stream, TokenKind::RBrace);
        StatementContent::Compound(CompoundContent {
            lbrace,
            statements,
            rbrace,
        }).into()
    }
}

// Either local, saved, or session
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclContent {
    pub kind: LeafToken,
    pub decls: VarDecl,
    pub initializer: Option<(LeafToken, Initializer)>,
    pub semi: LeafToken,
}

impl TreeElement for VariableDeclContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.kind.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.kind, &self.decls, &self.initializer, &self.semi)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        self.decls.ensure_named()
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.sp_punct.check(acc, SpPunctArgs::from_variable_decl(self));
    }
}

impl Parse<StatementContent> for VariableDeclContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        // Guaranteed by parser
        let kind = inner.next_leaf(stream);
        let decls = parse_vardecl(&inner, stream, file_info);
        let initializer = match inner.peek_kind(stream) {
            Some(TokenKind::Assign) => {
                let equals = inner.next_leaf(stream);
                let init = Initializer::parse(&inner, stream, file_info);
                Some((equals, init))
            },
            _ => None
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::VariableDecl(VariableDeclContent {
            kind, decls, initializer, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeleteContent {
    pub delete: LeafToken,
    pub expr: Expression,
    pub semi: LeafToken,
}

impl TreeElement for DeleteContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.delete.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.delete, &self.expr, &self.semi)
    }
}

impl Parse<StatementContent> for DeleteContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let delete = inner.expect_next_kind(stream, TokenKind::Delete);
        let expr = Expression::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Delete(DeleteContent {
            delete, expr, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignOpContent {
    pub assignee: Expression,
    pub opr: LeafToken,
    pub assign: Expression,
    pub semi: LeafToken,
}

impl TreeElement for AssignOpContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.assignee.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.assignee,
                     &self.opr,
                     &self.assign,
                     &self.semi)
    }
}

fn parse_assignop(assignee: Expression,
                  outer_context: &mut ParseContext,
                  inner_context: &mut ParseContext,
                  stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
    // Guaranteed by grammar
    let opr = inner_context.next_leaf(stream);
    let assign = Expression::parse(inner_context, stream, file_info);
    let semi = outer_context.expect_next_kind(stream, TokenKind::SemiColon);
    StatementContent::AssignOp(AssignOpContent {
        assignee,
        opr,
        assign,
        semi
    }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignTarget {
    One(Expression),
    Many(LeafToken, Vec<(Expression, Option<LeafToken>)>, LeafToken),
}

impl TreeElement for AssignTarget {
    fn range(&self) -> ZeroRange {
        match self {
            AssignTarget::One(expr) => expr.range(),
            AssignTarget::Many(lparen, _, rparen) =>
                Range::combine(lparen.range(), rparen.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            AssignTarget::One(expr) => create_subs!(expr),
            AssignTarget::Many(lparen, targets, rparen) =>
                create_subs!(lparen, targets, rparen),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assigner {
    Initializer(LeafToken, Initializer),
    Chain(Vec<(LeafToken, Expression)>),
}

impl TreeElement for Assigner {
    fn range(&self) -> ZeroRange {
        match self {
            Assigner::Initializer(assign, init) =>
                Range::combine(assign.range(),
                               init.range()),
            Assigner::Chain(chain) => chain.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Assigner::Initializer(assign, init) => create_subs!(assign, init),
            Assigner::Chain(chain) => create_subs!(chain),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignContent {
    pub target: AssignTarget,
    pub assigner: Assigner,
    pub semi: LeafToken,
}

impl TreeElement for AssignContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.target.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.target, &self.assigner, &self.semi)
    }
}

fn parse_assigner_chain(
    leading_assign: LeafToken,
    leading_expression: Expression,
    context: &mut ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
    -> Assigner {
    let mut chain = vec![(leading_assign, leading_expression)];
    while matches!(context.peek_kind(stream),
                   Some(TokenKind::Assign)) {
        let assign = context.expect_next_kind(
            stream, TokenKind::Assign);
        let expr = Expression::parse(context, stream, file_info);
        chain.push((assign, expr));
    }
    Assigner::Chain(chain)
}

fn parse_assigner(context: &mut ParseContext,
                  stream: &mut FileParser<'_>, file_info: &FileInfo) -> Assigner {
    let first_assign = context.expect_next_kind(
        stream, TokenKind::Assign);
    let first_init = Initializer::parse(context, stream, file_info);
    // If the initializer is a simple expression singleinitializer,
    // attempt to make a chain.
    if let Content::Some(InitializerContent::Single(singleinit))
        = &*first_init.content {
            if let Content::Some(SingleInitializerContent::Expression(expr))
                = &*singleinit.content {
                    parse_assigner_chain(first_assign,
                                         expr.clone(),
                                         context,
                                         stream,
                                         file_info)
                }
            else {
                Assigner::Initializer(first_assign, first_init)
            }
        } else {
            Assigner::Initializer(first_assign, first_init)
        }
}

fn parse_assign(target: AssignTarget,
                outer_context: &mut ParseContext,
                inner_context: &mut ParseContext,
                stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
    let assigner = parse_assigner(inner_context, stream, file_info);
    let semi = outer_context.expect_next_kind(stream, TokenKind::SemiColon);
    StatementContent::Assign(AssignContent {
        target, assigner, semi
    }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfContent {
    pub iftok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub truebranch: Statement,
    pub elsebranch: Option<(LeafToken, Statement)>,
}

impl TreeElement for IfContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.iftok.range(),
            match &self.elsebranch {
                Some((_, statement)) => statement.range(),
                None => self.truebranch.range(),
            },
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.iftok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.truebranch,
                     &self.elsebranch)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_inparen.check(acc, NspInparenArgs::from_if(self));
    }
}

impl Parse<StatementContent> for IfContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let paren_context = outer_context.enter_context(understands_rparen);
        let iftok = outer_context.expect_next_kind(stream, TokenKind::If);
        let lparen = outer_context.expect_next_kind(stream, TokenKind::LParen);
        let cond = Expression::parse(&paren_context, stream, file_info);
        let rparen = outer_context.expect_next_kind(stream, TokenKind::RParen);
        let truebranch = Statement::parse(&outer_context, stream, file_info);
        let elsebranch = match outer_context.peek_kind(stream) {
            Some(TokenKind::Else) => {
                let elsetok = outer_context.next_leaf(stream);
                let elsestmt = Statement::parse(&outer_context, stream, file_info);
                Some((elsetok, elsestmt))
            },
            _ => None
        };
        StatementContent::If(IfContent {
            iftok, lparen, cond, rparen, truebranch, elsebranch
        }).into()
    }
}

// We keep #if separate from if, so that we can see errors
// related to invalid # context easier in post-parse walk
#[derive(Debug, Clone, PartialEq)]
pub struct HashIfContent {
    pub iftok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub truebranch: Statement,
    pub elsebranch: Option<(LeafToken, Statement)>,
}

impl TreeElement for HashIfContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.iftok.range(),
            match &self.elsebranch {
                Some((_, statement)) => statement.range(),
                None => self.truebranch.range(),
            },
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.iftok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.truebranch,
                     &self.elsebranch)
    }
}

impl Parse<StatementContent> for HashIfContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let paren_context = outer_context.enter_context(understands_rparen);
        let iftok = outer_context.expect_next_kind(stream, TokenKind::HashIf);
        let lparen = outer_context.expect_next_kind(stream, TokenKind::LParen);
        let cond = Expression::parse(&paren_context, stream, file_info);
        let rparen = outer_context.expect_next_kind(stream, TokenKind::RParen);
        let truebranch = Statement::parse(&outer_context, stream, file_info);
        let elsebranch = match outer_context.peek_kind(stream) {
            Some(TokenKind::HashElse) => {
                let elsetok = outer_context.next_leaf(stream);
                let elsestmt = Statement::parse(&outer_context, stream, file_info);
                Some((elsetok, elsestmt))
            },
            _ => None
        };
        StatementContent::HashIf(HashIfContent {
            iftok, lparen, cond, rparen, truebranch, elsebranch
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileContent {
    pub whiletok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub statement: Statement,
}

impl TreeElement for WhileContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.whiletok.range(),
            self.statement.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.whiletok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.statement)
    }
}

impl Parse<StatementContent> for WhileContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let paren_context = outer_context.enter_context(understands_rparen);
        let whiletok = outer_context.expect_next_kind(stream, TokenKind::While);
        let lparen = outer_context.expect_next_kind(stream, TokenKind::LParen);
        let cond = Expression::parse(&paren_context, stream, file_info);
        let rparen = outer_context.expect_next_kind(stream, TokenKind::RParen);
        let statement = Statement::parse(&outer_context, stream, file_info);
        StatementContent::While(WhileContent {
            whiletok, lparen, cond, rparen, statement
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoContent {
    pub dotok: LeafToken,
    pub statement: Statement,
    pub whiletok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for DoContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.dotok.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.dotok,
                     &self.statement,
                     &self.whiletok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.semi)
    }
}

impl Parse<StatementContent> for DoContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        fn understands_while(token: TokenKind) -> bool {
            token == TokenKind::While
        }
        let mut prewhile_context = inner.enter_context(
            understands_while);
        let dotok = prewhile_context.expect_next_kind(stream, TokenKind::Do);
        let statement = Statement::parse(&prewhile_context, stream, file_info);
        let whiletok = inner.expect_next_kind(stream, TokenKind::While);
        let lparen = inner.expect_next_kind(stream, TokenKind::LParen);
        let paren_context = inner.enter_context(understands_rparen);
        let cond = Expression::parse(&paren_context, stream, file_info);
        let rparen = inner.expect_next_kind(stream, TokenKind::RParen);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Do(DoContent {
            dotok, statement, whiletok, lparen, cond, rparen, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForPostElement {
    Expression(Expression),
    Assign(AssignTarget, Assigner),
    AssignOp(Expression, LeafToken, Expression),
}

pub type ForPost = Vec<(ForPostElement, Option<LeafToken>)>;

impl TreeElement for ForPostElement {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Expression(expr) => expr.range(),
            Self::Assign(target, vec) => {
                Range::combine(target.range(), vec.range())
            },
            Self::AssignOp(target, _, expr) =>
                Range::combine(target.range(), expr.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Expression(expr) => create_subs!(expr),
            Self::Assign(target, vec) =>
                create_subs!(target, vec),
            Self::AssignOp(target, opr, expr) =>
                create_subs!(target, opr, expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForPre {
    Local(LeafToken, CDecl, Option<(LeafToken, Initializer)>),
    Post(ForPost),
}

impl TreeElement for ForPre {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Local(loc, cdecl, init) =>
                Range::combine(Range::combine(loc.range(), cdecl.range()),
                               init.range()),
            Self::Post(forpost) => forpost.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Local(loc, cdecl, init) =>
                create_subs!(loc, cdecl, init),
            Self::Post(forpost) => create_subs!(forpost),
        }
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        if let ForPre::Local(_, cdecl, _) = self {
            cdecl.ensure_named()
        } else {
            vec![]
        }
    }
}

enum TupleOrExpression {
    Tuple(LeafToken, Vec<(Expression, Option<LeafToken>)>, LeafToken),
    Expression(Expression)
}

// TODO: This code duplicates with code in expression_statement,
// parse_assign, and parse_expression_or_tuple, consider refactoring
fn parse_tuple_or_expression(context: &ParseContext,
                             stream: &mut FileParser<'_>, file_info: &FileInfo)
                             -> TupleOrExpression {
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }
    fn understands_comma(token: TokenKind) -> bool {
        token == TokenKind::Comma
    }
    let mut paren_context = context.enter_context(
        understands_rparen);
    let mut deeper_context = paren_context.enter_context(
        understands_comma);
    // Guaranteed by grammar
    let lparen = paren_context.next_leaf(stream);
    let first_expr = Expression::parse(&deeper_context, stream, file_info);
    if let Some(TokenKind::Comma) = deeper_context.peek_kind(stream) {
        // tuple literal case
        let mut elements = vec![(first_expr,
                                 Some(deeper_context.next_leaf(stream)))];
        while {
            let expr = Expression::parse(&deeper_context, stream, file_info);
            let maybe_comma = deeper_context.next_if_kind(
                stream, TokenKind::Comma);
            elements.push((expr, maybe_comma));
            elements.last().unwrap().1.is_some()
        } {}
        let rparen = paren_context.expect_next_kind(stream,
                                                    TokenKind::RParen);
        TupleOrExpression::Tuple(lparen, elements, rparen)
    } else {
        // Expression case
        let rparen = paren_context.expect_next_kind(stream,
                                                    TokenKind::RParen);
        let paren_expr: Expression =
            ExpressionContent::ParenExpression(
                ParenExpressionContent {
                    lparen,
                    expr: first_expr,
                    rparen
                }).into();
        TupleOrExpression::Expression(maybe_parse_tertiary_expression(
            Some(paren_expr), context, stream, file_info))
    }
}

fn parse_forpost_element(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                 -> ForPostElement {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    let expr = match new_context.peek_kind(stream) {
        Some(TokenKind::LParen) => {
            match parse_tuple_or_expression(&new_context, stream, file_info) {
                TupleOrExpression::Tuple(l, elems, r) => {
                    let assign_target = AssignTarget::Many(
                        l, elems, r);
                    // We cannot reuse the cases below here, because things
                    // like (a, b) += y isnt allowed
                    let assigner = parse_assigner(&mut new_context, stream, file_info);
                    return ForPostElement::Assign(assign_target, assigner)
                },
                TupleOrExpression::Expression(expr) => expr,
            }
        },
        _ => Expression::parse(&new_context, stream, file_info)
    };
    match new_context.peek_kind(stream) {
        Some(TokenKind::PlusAssign) | Some(TokenKind::MinusAssign) |
        Some(TokenKind::TimesAssign) | Some(TokenKind::DivideAssign) |
        Some(TokenKind::ModAssign) | Some(TokenKind::BOrAssign) |
        Some(TokenKind::BXorAssign) | Some(TokenKind::BAndAssign) |
        Some(TokenKind::LShiftAssign) | Some(TokenKind::RShiftAssign) => {
            let opr = new_context.next_leaf(stream);
            let assign = Expression::parse(&new_context, stream, file_info);
            ForPostElement::AssignOp(expr, opr, assign)
        }
        Some(TokenKind::Assign) => {
            let assigner = parse_assigner(&mut new_context, stream, file_info);
            ForPostElement::Assign(AssignTarget::One(expr),
                                   assigner)
        },
        _ => {
            ForPostElement::Expression(expr)
        }
    }
}

fn parse_forpost(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                 -> ForPost {
    // Technically, _we_ dont understand semi but the
    // ones above us do
    fn understands_semi_and_comma(token: TokenKind) -> bool {
        token == TokenKind::SemiColon || token == TokenKind::Comma
    }
    let mut outer = context.enter_context(understands_semi_and_comma);
    let mut forposts = vec![];
    let mut end = match outer.peek_kind(stream) {
        Some(tok) => !dmlexpression_first_token_matcher(tok) ||
            tok == TokenKind::LBrace,
        _ => true,
    };
    while !end {
        let element = parse_forpost_element(&outer, stream, file_info);
        let comma = match outer.peek_kind(stream) {
            Some(TokenKind::Comma) => Some(outer.next_leaf(stream)),
            _ => {
                end = true;
                None
            }
        };
        forposts.push((element, comma));
    }
    forposts
}

fn parse_forpre(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> ForPre {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        Some(TokenKind::Local) => {
            let local = new_context.expect_next_kind(stream, TokenKind::Local);
            let decl = CDecl::parse(&new_context, stream, file_info);
            let initializer = match new_context.peek_kind(stream) {
                Some(TokenKind::Assign) => {
                    let assign = new_context.next_leaf(stream);
                    let init = Initializer::parse(&new_context, stream, file_info);
                    Some((assign, init))
                },
                _ => None
            };
            ForPre::Local(local, decl, initializer)
        },
        _ => ForPre::Post(parse_forpost(&new_context, stream, file_info)),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForContent {
    pub fortok: LeafToken,
    pub lparen: LeafToken,
    pub pre: Option<ForPre>,
    pub lsemi: LeafToken,
    pub cond: Option<Expression>,
    pub rsemi: LeafToken,
    pub post: Option<ForPost>,
    pub rparen: LeafToken,
    pub statement: Statement,
}

impl TreeElement for ForContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.fortok.range(),
            self.statement.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.fortok,
                     &self.lparen,
                     &self.pre,
                     &self.lsemi,
                     &self.cond,
                     &self.rsemi,
                     &self.post,
                     &self.rparen,
                     &self.statement)
    }
}

impl Parse<StatementContent> for ForContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        fn understands_semi(token: TokenKind) -> bool {
            token == TokenKind::SemiColon
        }
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        let fortok = outer_context.expect_next_kind(stream, TokenKind::For);
        let lparen = outer_context.expect_next_kind(stream, TokenKind::LParen);
        let mut paren_context = outer_context.enter_context(understands_rparen);
        let mut semi_context = paren_context.enter_context(understands_semi);
        let pre = match semi_context.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(parse_forpre(&semi_context, stream, file_info)),
        };
        let lsemi = semi_context.expect_next_kind(
            stream, TokenKind::SemiColon);
        let cond = match semi_context.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(Expression::parse(&semi_context, stream, file_info)),
        };
        let rsemi = semi_context.expect_next_kind(
            stream, TokenKind::SemiColon);
        let post = match semi_context.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(parse_forpost(&semi_context, stream, file_info)),
        };
        let rparen = paren_context.expect_next_kind(stream, TokenKind::RParen);
        let statement = Statement::parse(&outer_context, stream, file_info);
        StatementContent::For(ForContent {
            fortok, lparen, pre, lsemi, cond, rsemi, post, rparen, statement
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchHashIf {
    pub iftok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub lbrace: LeafToken,
    pub truecases: Vec<SwitchCase>,
    pub rbrace: LeafToken,
    pub hashelse: Option<(LeafToken, LeafToken, Vec<SwitchCase>, LeafToken)>,
}

impl TreeElement for SwitchHashIf {
    fn range(&self) -> ZeroRange {
        Range::combine(self.iftok.range(),
                       match &self.hashelse {
                           Some((_, _, _, rbrace)) => rbrace.range(),
                           _ => self.rbrace.range()
                       })
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.iftok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.lbrace,
                     &self.truecases,
                     &self.rbrace,
                     &self.hashelse)
    }
}

fn parse_switchhashif(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                      -> SwitchHashIf {
    fn understands_hashelse(token: TokenKind) -> bool {
        token == TokenKind::HashElse
    }
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }
    fn understands_rbrace(token: TokenKind) -> bool {
        token == TokenKind::RBrace
    }
    let mut outer_context = context.enter_context(understands_hashelse);
    let iftok = outer_context.expect_next_kind(stream, TokenKind::HashIf);
    let lparen = outer_context.expect_next_kind(stream, TokenKind::LParen);
    let paren_context = outer_context.enter_context(understands_rparen);
    let cond = Expression::parse(&paren_context, stream, file_info);
    let rparen = outer_context.expect_next_kind(stream, TokenKind::RParen);
    let lbrace = outer_context.expect_next_kind(stream, TokenKind::LBrace);
    let mut rbrace_context = outer_context.enter_context(understands_rbrace);
    let mut truecases = vec![];
    while rbrace_context.peek_kind(stream).map_or(
        false, |t|matches!(t,
                           TokenKind::HashIf |
                           TokenKind::Case |
                           TokenKind::Default) ||
            dmlstatement_first_token_matcher(t)) {
        truecases.push(parse_switchcase(&rbrace_context, stream, file_info));
    }
    let rbrace = outer_context.expect_next_kind(stream, TokenKind::RBrace);
    let hashelse = match outer_context.peek_kind(stream) {
        Some(TokenKind::HashElse) => {
            let hashelse = outer_context.next_leaf(stream);
            let else_lbrace = outer_context.expect_next_kind(stream,
                                                             TokenKind::LBrace);
            let mut falsecases = vec![];
            let mut rbrace_context = outer_context.enter_context(
                understands_rbrace);
            while rbrace_context.peek_kind(stream).map_or(
                false, |t|matches!(t,
                                   TokenKind::HashIf |
                                   TokenKind::Case |
                                   TokenKind::Default) ||
                    dmlstatement_first_token_matcher(t)){
                falsecases.push(parse_switchcase(&rbrace_context, stream, file_info));
            }
            let else_rbrace = outer_context.expect_next_kind(stream,
                                                             TokenKind::RBrace);
            Some((hashelse, else_lbrace, falsecases, else_rbrace))
        },
        _ => None
    };
    SwitchHashIf {
        iftok, lparen, cond, rparen, lbrace, truecases, rbrace, hashelse
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCase {
    Statement(Statement),
    Case(LeafToken, Expression, LeafToken),
    HashIf(SwitchHashIf),
    Default(LeafToken, LeafToken),
}

impl TreeElement for SwitchCase {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Statement(content) => content.range(),
            Self::Case(case, _, colon) => Range::combine(case.range(),
                                                         colon.range()),
            Self::HashIf(content) => content.range(),
            Self::Default(default, colon) => Range::combine(
                default.range(),
                colon.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Statement(content) => create_subs!(content),
            Self::Case(case, expr, colon) => create_subs!(case, expr, colon),
            Self::HashIf(content) => create_subs!(content),
            Self::Default(default, colon) => create_subs!(default, colon),
        }
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, aux: &mut AuxParams) {
        rules.in9.check(acc, IN9Args::from_switch_case(self, &mut aux.depth));
    }
}

fn parse_switchcase(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                    -> SwitchCase {
    fn understands_colon(token: TokenKind) -> bool {
        token == TokenKind::Colon
    }
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        Some(TokenKind::HashIf) => SwitchCase::HashIf(
            parse_switchhashif(&new_context, stream, file_info)),
        Some(TokenKind::Case) => {
            let case = new_context.next_leaf(stream);
            let mut colon_context = new_context.enter_context(
                understands_colon);
            let expr = Expression::parse(&colon_context, stream, file_info);
            let colon = colon_context.expect_next_kind(
                stream, TokenKind::Colon);
            SwitchCase::Case(case, expr, colon)
        },
        Some(TokenKind::Default) => {
            let default = new_context.next_leaf(stream);
            // If next token is :, we are in the standard switchcase,
            // otherwise we are a statement expression
            // (likely call to 'default')
            if new_context.peek_kind(stream) == Some(TokenKind::Colon) {
                let colon = new_context.expect_next_kind(stream,
                                                         TokenKind::Colon);
                SwitchCase::Default(default, colon)
            } else {
                // HACK: pre-construct the 'default' identifier expression
                // and pass it directly into expression parsing
                let default_expr = ExpressionContent::Identifier(
                    default).into();
                let expr = maybe_parse_tertiary_expression(
                    Some(default_expr),
                    &new_context,
                    stream,
                    file_info);
                let semi = new_context.expect_next_kind(stream,
                                                        TokenKind::SemiColon);
                SwitchCase::Statement(
                    StatementContent::Expression(
                        ExpressionStmtContent {
                            expression: expr,
                            semi,
                        }).into())
            }
        },
        _ => SwitchCase::Statement(Statement::parse(&new_context, stream, file_info)),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchContent {
    pub switchtok: LeafToken,
    pub lparen: LeafToken,
    pub expr: Expression,
    pub rparen: LeafToken,
    pub lbrace: LeafToken,
    pub cases: Vec<SwitchCase>,
    pub rbrace: LeafToken,
}

impl TreeElement for SwitchContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.switchtok.range(),
            self.rbrace.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.switchtok,
                     &self.lparen,
                     &self.expr,
                     &self.rparen,
                     &self.lbrace,
                     &self.cases,
                     &self.rbrace)
    }
}

impl Parse<StatementContent> for SwitchContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_rbrace(token: TokenKind) -> bool {
            token == TokenKind::RBrace
        }
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        let switchtok = outer_context.expect_next_kind(
            stream, TokenKind::Switch);
        let lparen = outer_context.expect_next_kind(
            stream, TokenKind::LParen);
        let paren_context = outer_context.enter_context(understands_rparen);
        let expr = Expression::parse(&paren_context, stream, file_info);
        let rparen = outer_context.expect_next_kind(stream, TokenKind::RParen);
        let lbrace = outer_context.expect_next_kind(stream, TokenKind::LBrace);
        let mut rbrace_context = outer_context.enter_context(
            understands_rbrace);
        let mut cases = vec![];
        while rbrace_context.peek_kind(stream).map_or(
            false, |t|matches!(t,
                               TokenKind::HashIf |
                               TokenKind::Case |
                               TokenKind::Default) ||
                dmlstatement_first_token_matcher(t)) {
            cases.push(parse_switchcase(&rbrace_context, stream, file_info));
        }
        let rbrace = outer_context.expect_next_kind(stream, TokenKind::RBrace);
        StatementContent::Switch(SwitchContent {
            switchtok, lparen, expr, rparen, lbrace, cases, rbrace
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryContent {
    pub trytok: LeafToken,
    pub trystatement: Statement,
    pub catch: LeafToken,
    pub catchstatement: Statement,
}

impl TreeElement for TryContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.trytok.range(),
            self.catchstatement.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.trytok,
                     &self.trystatement,
                     &self.catch,
                     &self.catchstatement)
    }
}

impl Parse<StatementContent> for TryContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_catch(token: TokenKind) -> bool {
            token == TokenKind::Catch
        }
        let mut outer_context = context.enter_context(doesnt_understand_tokens);
        let trytok = outer_context.expect_next_kind(stream, TokenKind::Try);
        let mut catch_context = outer_context.enter_context(understands_catch);
        let trystatement = Statement::parse(&catch_context, stream, file_info);
        let catch = catch_context.expect_next_kind(stream, TokenKind::Catch);
        let catchstatement = Statement::parse(&catch_context, stream, file_info);
        StatementContent::Try(TryContent {
            trytok, trystatement, catch, catchstatement
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AfterTiming {
    Timer(Expression, LeafToken),
    HookNoParams(Expression),
    // expr -> ident
    HookBindOne(Expression, LeafToken, LeafToken),
    // expr -> ([expr ,?])
    HookBindList(Expression, LeafToken, LeafToken,
                 Vec<(LeafToken, Option<LeafToken>)>, LeafToken),
}

impl TreeElement for AfterTiming {
    fn range(&self) -> ZeroRange {
        match self {
            AfterTiming::Timer(expr, leaf) => Range::combine(
                expr.range(),
                leaf.range(),
            ),
            AfterTiming::HookNoParams(expr) => expr.range(),
            AfterTiming::HookBindOne(expr, _, bind) => Range::combine(
                expr.range(),
                bind.range(),
            ),
            AfterTiming::HookBindList(expr, _, _, _, rparen) => Range::combine(
                expr.range(),
                rparen.range(),
            ),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            AfterTiming::Timer(expr, leaf) =>
                create_subs!(expr,
                             leaf),
            AfterTiming::HookNoParams(expr) => create_subs!(expr),
            AfterTiming::HookBindOne(expr, arrow, bind) =>
                create_subs!(expr,
                             arrow,
                             bind),
            AfterTiming::HookBindList(expr, arrow, lparen,
                                      ident_list, rparen) =>
                create_subs!(expr,
                             arrow,
                             lparen,
                             ident_list,
                             rparen),
        }
    }
    #[allow(clippy::single_match)]
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        match self {
            AfterTiming::Timer(_, unit_tok) => {
                if let Some(unit) = unit_tok.read_leaf(file) {
                    if !matches!(unit.as_str(), "s" | "cycles" | "ps") {
                        return vec![LocalDMLError {
                            range: unit_tok.range(),
                            description: "Expected time unit ('s', 'cycles', 'ps')"
                                .to_string(),
                        }]
                    }
                }
            },
            _ => (),
        }
        vec![]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AfterContent {
    pub after: LeafToken,
    pub timer: Option<AfterTiming>,
    pub colon: LeafToken,
    pub callexpression: Expression,
    pub semi: LeafToken,
}

impl TreeElement for AfterContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.after.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.after,
                     &self.timer,
                     &self.colon,
                     &self.callexpression,
                     &self.semi)
    }
}

impl Parse<StatementContent> for AfterContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        fn understands_lparen(token: TokenKind) -> bool {
            token == TokenKind::LParen
        }
        let after = inner.expect_next_kind(stream, TokenKind::After);
        let timer = if inner.peek_kind(stream) != Some(TokenKind::Colon) {
            // NOTE: Hack here, there is a parse conflict between
            // expr -> ident // as a member expression
            // and
            // expr -> ident // as a hook bind
            // In addition,
            // expr -> (ident, ...
            // will fail to parse in the expression. As such, we pre-emptively
            // enter a context that will allow us to skip tokens up to the
            // lparen before parsing the expression, and then pick apart
            // the result.
            let pre_lparen = inner.enter_context(understands_lparen);
            let afterexpression = Expression::parse(&pre_lparen,
                                                    stream, file_info);
            if let Some(ExpressionContent::MemberLiteral(
                MemberLiteralContent {
                    operation: arrow @ LeafToken::Actual(Token {
                        kind: TokenKind::Arrow,
                        ..
                    }),
                    left,
                    right,
                })) = afterexpression.with_content(|con|Some(con.clone()),
                                                   None) {
                match right {
                    // If the rightside is missing, we are in the -> ( case
                    LeafToken::Missing(_) => {
                        let hook = left;
                        let lparen = inner.expect_next_kind(
                            stream, TokenKind::LParen);
                        let mut decls = vec![];
                        let mut cont = true;
                        while cont {
                            cont = false;
                            let bind = inner.expect_next_kind(
                                stream, TokenKind::Identifier);
                            let comma = if inner.peek_kind(stream) == Some(
                                TokenKind::Comma) {
                                cont = true;
                                Some(inner.next_leaf(stream))
                            } else {
                                None
                            };
                            decls.push((bind, comma));
                        }
                        let rparen = inner.expect_next_kind(stream,
                                                            TokenKind::RParen);
                        Some(AfterTiming::HookBindList(hook.clone(),
                                                       arrow.clone(),
                                                       lparen,
                                                       decls,
                                                       rparen))
                    },
                    // I the rightside is NOT missing, this is the
                    // hook -> ident case
                    leaf => {
                        // Sanity, the rightside of hook must be an identifier
                        // There is no good way to reconstruct parser state here
                        if leaf.get_token().map(|l|l.kind)
                            != Some(TokenKind::Identifier) {
                                error!("Internal Parser Error: Unexpectedly \
                                        got '-> {:?}'", leaf);
                            }
                        Some(AfterTiming::HookBindOne(
                            left.clone(), arrow.clone(), leaf.clone()))
                    }
                }
            } else {
                // Non-hack cases
                match inner.peek_kind(stream) {
                    // after <expr><unit>: case
                    Some(TokenKind::Identifier) => {
                        let unit = inner.next_leaf(stream);
                        Some(AfterTiming::Timer(afterexpression, unit))
                    },
                    // after <hook>: case
                    _ => {
                        Some(AfterTiming::HookNoParams(afterexpression))
                    },
                }
            }
        } else {
            // after: case
            None
        };

        let colon = inner.expect_next_kind(stream, TokenKind::Colon);
        let callexpression = Expression::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::After(AfterContent {
            after, timer, colon, callexpression, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogLevel {
    Simple(Expression),
    Subsequent(Expression, LeafToken, Expression),
}

impl TreeElement for LogLevel {
    fn range(&self) -> ZeroRange {
        match self {
            LogLevel::Simple(expr) => expr.range(),
            LogLevel::Subsequent(l, _, r) => ZeroRange::combine(l.range(),
                                                                r.range())
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            LogLevel::Simple(expr) => create_subs!(expr),
            LogLevel::Subsequent(l, c, r) => create_subs!(l, c, r)
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct LogContent {
    pub log: LeafToken,
    pub log_kind: LeafToken,
    pub level: Option<(LeafToken, LogLevel)>,
    pub flags: Option<(LeafToken, Expression)>,
    pub colon: LeafToken,
    pub message: Expression,
    pub args: Vec<(LeafToken, Expression)>,
    pub semi: LeafToken,
}

impl TreeElement for LogContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.log.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.log,
                     &self.log_kind,
                     &self.level,
                     &self.flags,
                     &self.colon,
                     &self.message,
                     &self.args,
                     &self.semi)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        ensure_string_concatenation(&self.message)
    }
}

impl Parse<StatementContent> for LogContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn expects_ident_or_error(token: TokenKind) -> bool {
            token == TokenKind::Error || token == TokenKind::Identifier
        }
        let (mut outer, mut inner) = statement_contexts(context);
        let log = inner.expect_next_kind(stream, TokenKind::Log);
        let log_kind = inner.expect_next_filter(stream, expects_ident_or_error,
                                                "Log Kind");
        let (level, flags) = match inner.peek_kind(stream) {
            Some(TokenKind::Comma) => {
                let level_comma = inner.next_leaf(stream);
                let level = {
                    let level_expr = Expression::parse(&inner, stream, file_info);
                    if let Some(TokenKind::Then) = inner.peek_kind(stream) {
                        let then = inner.next_leaf(stream);
                        let subsequent_log = Expression::parse(&inner, stream, file_info);
                        LogLevel::Subsequent(level_expr, then, subsequent_log)
                    } else {
                        LogLevel::Simple(level_expr)
                    }
                };
                (Some((level_comma, level)),
                 match inner.peek_kind(stream) {
                     Some(TokenKind::Comma) => {
                         let flags_comma = inner.next_leaf(stream);
                         let flags_expr = Expression::parse(&inner, stream, file_info);
                         Some((flags_comma, flags_expr))
                     },
                     _ => None,
                 })
            },
            _ => (None, None),
        };
        let colon = inner.expect_next_kind(stream, TokenKind::Colon);
        let message = Expression::parse(&inner, stream, file_info);
        let mut args = vec![];
        while matches!(inner.peek_kind(stream), Some(TokenKind::Comma)) {
            let arg_comma = inner.next_leaf(stream);
            let arg = Expression::parse(&inner, stream, file_info);
            args.push((arg_comma, arg));
        }
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Log(LogContent {
            log, log_kind, level, flags, colon, message, args, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashSelectContent {
    pub select: LeafToken,
    pub ident: LeafToken,
    pub intok: LeafToken,
    pub inlparen: LeafToken,
    pub inexpression: Expression,
    pub inrparen: LeafToken,
    pub wheretok: LeafToken,
    pub wherelparen: LeafToken,
    pub whereexpression: Expression,
    pub whererparen: LeafToken,
    pub selectstatement: Statement,
    pub elsetok: LeafToken,
    pub elsestatement: Statement,
}

impl TreeElement for HashSelectContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.select.range(),
            self.elsestatement.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.select,
                     &self.ident,
                     &self.intok,
                     &self.inlparen,
                     &self.inexpression,
                     &self.inrparen,
                     &self.wheretok,
                     &self.wherelparen,
                     &self.whereexpression,
                     &self.whererparen,
                     &self.selectstatement,
                     &self.elsetok,
                     &self.elsestatement)
    }
}

impl Parse<StatementContent> for HashSelectContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        fn understands_in(token: TokenKind) -> bool {
            token == TokenKind::In
        }
        fn understands_where(token: TokenKind) -> bool {
            token == TokenKind::Where
        }
        fn understands_hashelse(token: TokenKind) -> bool {
            token == TokenKind::HashElse
        }
        let mut pre_else = context.enter_context(understands_hashelse);
        let mut pre_where = pre_else.enter_context(understands_where);
        let mut pre_in = pre_where.enter_context(understands_in);
        let mut inner = pre_in.enter_context(doesnt_understand_tokens);
        let select = inner.expect_next_kind(stream, TokenKind::HashSelect);
        let ident = inner.expect_next_filter(
            stream, ident_filter, "identifier");
        let intok = pre_in.expect_next_kind(stream, TokenKind::In);
        let inlparen = pre_in.expect_next_kind(stream, TokenKind::LParen);
        let paren_context = pre_in.enter_context(understands_rparen);
        let inexpression = Expression::parse(&paren_context, stream, file_info);
        let inrparen = pre_in.expect_next_kind(stream, TokenKind::RParen);
        let wheretok = pre_where.expect_next_kind(stream, TokenKind::Where);
        let wherelparen = pre_where.expect_next_kind(stream, TokenKind::LParen);
        let paren_context_2 = pre_where.enter_context(understands_rparen);
        let whereexpression = Expression::parse(&paren_context_2, stream, file_info);
        let whererparen = pre_where.expect_next_kind(stream, TokenKind::RParen);
        let selectstatement = Statement::parse(&pre_where, stream, file_info);
        let elsetok = pre_else.expect_next_kind(stream, TokenKind::HashElse);
        let elsestatement = Statement::parse(&pre_else, stream, file_info);
        StatementContent::HashSelect(HashSelectContent {
            select, ident, intok, inlparen, inexpression, inrparen,
            wheretok, wherelparen, whereexpression, whererparen,
            selectstatement, elsetok, elsestatement
        }).into()
    }
}



#[derive(Debug, Clone, PartialEq)]
pub struct ForeachContent {
    pub foreach: LeafToken,
    pub ident: LeafToken,
    pub intok: LeafToken,
    pub lparen: LeafToken,
    pub expression: Expression,
    pub rparen: LeafToken,
    pub statement: Statement,
}

impl TreeElement for ForeachContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.foreach.range(),
            self.statement.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.foreach,
                     &self.ident,
                     &self.intok,
                     &self.lparen,
                     &self.expression,
                     &self.rparen,
                     &self.statement)
    }
}

impl Parse<StatementContent> for ForeachContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        fn understands_in(token: TokenKind) -> bool {
            token == TokenKind::In
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        let mut pre_in = outer.enter_context(understands_in);
        // Guarnteed by parser
        let foreach = pre_in.next_leaf(stream);
        let mut inner = pre_in.enter_context(doesnt_understand_tokens);
        let ident = inner.expect_next_filter(
            stream, objident_filter, "template name");
        let intok = pre_in.expect_next_kind(stream, TokenKind::In);
        let lparen = outer.expect_next_kind(stream, TokenKind::LParen);
        let mut paren_context = outer.enter_context(understands_rparen);
        let expression = Expression::parse(&paren_context, stream, file_info);
        let rparen = paren_context.expect_next_kind(stream, TokenKind::RParen);
        let statement = Statement::parse(&outer, stream, file_info);
        StatementContent::Foreach(ForeachContent {
            foreach, ident, intok, lparen, expression, rparen, statement
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueContent {
    pub continuetok: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for ContinueContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.continuetok.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.continuetok, &self.semi)
    }
}

impl Parse<StatementContent> for ContinueContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let continuetok = inner.expect_next_kind(stream, TokenKind::Continue);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Continue(ContinueContent {
            continuetok, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakContent {
    pub breaktok: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for BreakContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.breaktok.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.breaktok, &self.semi)
    }
}

impl Parse<StatementContent> for BreakContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let breaktok = inner.expect_next_kind(stream, TokenKind::Break);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Break(BreakContent {
            breaktok, semi
        }).into()
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ReturnContent {
    pub returntok: LeafToken,
    pub val: Option<Initializer>,
    pub semi: LeafToken,
}

impl TreeElement for ReturnContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.returntok.range(),
            self.semi.range(),
        )
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.returntok, &self.val, &self.semi)
    }
}

impl Parse<StatementContent> for ReturnContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let (mut outer, mut inner) = statement_contexts(context);
        let returntok = inner.expect_next_kind(stream, TokenKind::Return);
        let val = match inner.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(Initializer::parse(&inner, stream, file_info)),
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        StatementContent::Return(ReturnContent {
            returntok, val, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStmtContent {
    pub expression: Expression,
    pub semi: LeafToken,
}

impl TreeElement for ExpressionStmtContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.expression.range(),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.expression, &self.semi)
    }
    fn evaluate_rules(&self, acc: &mut Vec<LocalDMLError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.sp_punct.check(acc, SpPunctArgs::from_expression_stmt(self));
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum StatementContent {
    Empty(LeafToken),
    Error(ErrorContent),
    Assert(AssertContent),
    Compound(CompoundContent),
    VariableDecl(VariableDeclContent),
    Delete(DeleteContent),
    AssignOp(AssignOpContent),
    Assign(AssignContent),
    If(IfContent),
    HashIf(HashIfContent),
    Expression(ExpressionStmtContent),
    While(WhileContent),
    Do(DoContent),
    For(ForContent),
    Switch(SwitchContent),
    Try(TryContent),
    After(AfterContent),
    Log(LogContent),
    HashSelect(HashSelectContent),
    Foreach(ForeachContent),
    Throw(ThrowContent),
    Continue(ContinueContent),
    Break(BreakContent),
    Return(ReturnContent),
}

impl TreeElement for StatementContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Empty(content) => content.range(),
            Self::Error(content) => content.range(),
            Self::Assert(content) => content.range(),
            Self::Compound(content) => content.range(),
            Self::VariableDecl(content) => content.range(),
            Self::Delete(content) => content.range(),
            Self::AssignOp(content) => content.range(),
            Self::Assign(content) => content.range(),
            Self::If(content) => content.range(),
            Self::HashIf(content) => content.range(),
            Self::Expression(content) => content.range(),
            Self::While(content) => content.range(),
            Self::Do(content) => content.range(),
            Self::For(content) => content.range(),
            Self::Switch(content) => content.range(),
            Self::Try(content) => content.range(),
            Self::After(content) => content.range(),
            Self::Log(content) => content.range(),
            Self::HashSelect(content) => content.range(),
            Self::Foreach(content) => content.range(),
            Self::Throw(content) => content.range(),
            Self::Continue(content) => content.range(),
            Self::Break(content) => content.range(),
            Self::Return(content) => content.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Empty(content) => create_subs![content],
            Self::Error(content) => create_subs![content],
            Self::Assert(content) => create_subs![content],
            Self::Compound(content) => create_subs![content],
            Self::VariableDecl(content) => create_subs![content],
            Self::Delete(content) => create_subs![content],
            Self::AssignOp(content) => create_subs![content],
            Self::Assign(content) => create_subs![content],
            Self::If(content) => create_subs![content],
            Self::HashIf(content) => create_subs![content],
            Self::Expression(content) => create_subs![content],
            Self::While(content) => create_subs![content],
            Self::Do(content) => create_subs![content],
            Self::For(content) => create_subs![content],
            Self::Switch(content) => create_subs![content],
            Self::Try(content) => create_subs![content],
            Self::After(content) => create_subs![content],
            Self::Log(content) => create_subs![content],
            Self::HashSelect(content) => create_subs![content],
            Self::Foreach(content) => create_subs![content],
            Self::Throw(content) => create_subs![content],
            Self::Continue(content) => create_subs![content],
            Self::Break(content) => create_subs![content],
            Self::Return(content) => create_subs![content],
        }
    }
}

pub type Statement = AstObject<StatementContent>;

fn parse_expression_statement_ext(expression: Expression,
                                  outer: &mut ParseContext,
                                  inner: &mut ParseContext,
                                  stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
    match *expression.content {
        // We rarely do this, but convert a clean parse failure here into
        // a 'statement' miss rather than an 'expression' miss
        Content::Missing(ref prev) =>
            Statement::from_missing(expression.range(), prev, "statement"),
        _ => match inner.peek_kind(stream) {
            Some(TokenKind::PlusAssign) | Some(TokenKind::MinusAssign) |
            Some(TokenKind::TimesAssign) | Some(TokenKind::DivideAssign) |
            Some(TokenKind::ModAssign) | Some(TokenKind::BOrAssign) |
            Some(TokenKind::BXorAssign) | Some(TokenKind::BAndAssign) |
            Some(TokenKind::LShiftAssign) | Some(TokenKind::RShiftAssign) =>
                parse_assignop(expression, outer, inner, stream, file_info),
            Some(TokenKind::Assign) =>
                parse_assign(AssignTarget::One(expression),
                             outer, inner, stream, file_info),
            _ => {
                let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
                StatementContent::Expression(
                    ExpressionStmtContent{expression, semi}).into()
            },
        }
    }
}

// Since this might be an expression, an assignment, or an assignop, we cannot
// put this in any particular contents parse function
fn parse_expression_statement(context: &ParseContext,
                              stream: &mut FileParser<'_>, file_info: &FileInfo)
                              -> Statement {
    let (mut outer, mut inner) = statement_contexts(context);
    match inner.peek_kind(stream) {
        // We need special handling of lparen here, since it might either
        // be an expression or a multi-target assignment
        Some(TokenKind::LParen) => {
            match parse_tuple_or_expression(&inner, stream, file_info) {
                TupleOrExpression::Tuple(l, elems, r) =>
                    parse_assign(AssignTarget::Many(l, elems, r),
                                 &mut outer, &mut inner, stream, file_info),
                TupleOrExpression::Expression(expr) =>
                    parse_expression_statement_ext(
                        expr,
                        &mut outer, &mut inner, stream, file_info),
            }
        },
        _ => {
            let expression = Expression::parse(&inner, stream, file_info);
            parse_expression_statement_ext(
                expression, &mut outer, &mut inner, stream, file_info)
        }
    }
}

fn parse_expression_or_tuple_assignment(context: &ParseContext,
                                        stream: &mut FileParser<'_>, file_info: &FileInfo)
                                        -> Statement {
    let (mut outer_context, mut inner_context) = statement_contexts(context);
    match parse_tuple_or_expression(&inner_context, stream, file_info) {
        TupleOrExpression::Tuple(l, elems, r) =>
            parse_assign(AssignTarget::Many(l, elems, r),
                         &mut outer_context, &mut inner_context, stream, file_info),
        TupleOrExpression::Expression(expr) =>
            parse_expression_statement_ext(
                expr, &mut outer_context, &mut inner_context, stream, file_info)
    }
}

impl Parse<StatementContent> for Statement {
    #[allow(clippy::redundant_closure_call)]
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> Statement {
        let mut new_context = context.enter_context(doesnt_understand_tokens);
        handle_missing!(
            new_context.expect_peek(stream, "statement"),
            |token: Token| {
                match token.kind {
                    TokenKind::Error =>
                        ErrorContent::parse(&new_context, stream, file_info),
                    TokenKind::Assert =>
                        AssertContent::parse(&new_context, stream, file_info),
                    TokenKind::Throw =>
                        ThrowContent::parse(&new_context, stream, file_info),
                    TokenKind::LBrace =>
                        CompoundContent::parse(&new_context, stream, file_info),
                    TokenKind::Local | TokenKind::Session | TokenKind::Saved =>
                        VariableDeclContent::parse(&new_context, stream, file_info),
                    TokenKind::Delete =>
                        DeleteContent::parse(&new_context, stream, file_info),
                    TokenKind::If =>
                        IfContent::parse(&new_context, stream, file_info),
                    TokenKind::HashIf =>
                        HashIfContent::parse(&new_context, stream, file_info),
                    TokenKind::While =>
                        WhileContent::parse(&new_context, stream, file_info),
                    TokenKind::Do =>
                        DoContent::parse(&new_context, stream, file_info),
                    TokenKind::For =>
                        ForContent::parse(&new_context, stream, file_info),
                    TokenKind::Switch =>
                        SwitchContent::parse(&new_context, stream, file_info),
                    TokenKind::Try =>
                        TryContent::parse(&new_context, stream, file_info),
                    TokenKind::After =>
                        AfterContent::parse(&new_context, stream, file_info),
                    TokenKind::Log =>
                        LogContent::parse(&new_context, stream, file_info),
                    TokenKind::HashSelect =>
                        HashSelectContent::parse(&new_context, stream, file_info),
                    TokenKind::Foreach =>
                        ForeachContent::parse(&new_context, stream, file_info),
                    TokenKind::HashForeach =>
                        ForeachContent::parse(&new_context, stream, file_info),
                    TokenKind::Continue =>
                        ContinueContent::parse(&new_context, stream, file_info),
                    TokenKind::Break =>
                        BreakContent::parse(&new_context, stream, file_info),
                    TokenKind::Return =>
                        ReturnContent::parse(&new_context, stream, file_info),
                    TokenKind::SemiColon => StatementContent::Empty(
                        new_context.next_leaf(stream)).into(),
                    TokenKind::LParen => parse_expression_or_tuple_assignment(
                        &new_context, stream, file_info),
                    _ => parse_expression_statement(&new_context,
                                                    stream,
                                                    file_info)
                }
            })
    }
}

pub fn dmlstatement_first_token_matcher(token: TokenKind) -> bool {
    matches!(token,
             TokenKind::Error | TokenKind::Assert | TokenKind::Throw |
             TokenKind::LBrace | TokenKind::Local | TokenKind::Session |
             TokenKind::Saved | TokenKind::Delete | TokenKind::If |
             TokenKind::HashIf | TokenKind::While | TokenKind::Do |
             TokenKind::For | TokenKind::Switch | TokenKind::Try |
             TokenKind::After | TokenKind::Log | TokenKind::HashSelect |
             TokenKind::Foreach | TokenKind::HashForeach | TokenKind::Continue |
             TokenKind::Break | TokenKind::Return | TokenKind::SemiColon |
             TokenKind::LParen | TokenKind::Default) ||
        dmlexpression_first_token_matcher(token)
}

// TODO: expand unit tests
#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[allow(clippy::ptr_arg)]
    fn test_statement_tree(source: &str,
                           expected_ast: &Statement,
                           expected_skipped_tokens: &Vec<Token>) {
        test_ast_tree::<StatementContent, Statement>(source,
                                                     expected_ast,
                                                     expected_skipped_tokens)
    }

    #[test]
    fn expression_statement() {
        test_statement_tree(
            "foo;",
            &make_ast(zero_range(0, 0, 0, 4),
                      StatementContent::Expression(ExpressionStmtContent {
                          expression: make_ast(
                              zero_range(0, 0, 0, 3),
                              ExpressionContent::Identifier(make_leaf(
                                  zero_range(0, 0, 0, 0),
                                  zero_range(0, 0, 0, 3),
                                  TokenKind::Identifier
                              ))),
                          semi: make_leaf(
                              zero_range(0, 0, 3, 3),
                              zero_range(0, 0, 3, 4),
                              TokenKind::SemiColon),
                      })),
            &vec![]);
    }

    #[test]
    fn assignop_statement() {
        test_statement_tree(
            "foo += 2;",
            &make_ast(zero_range(0, 0, 0, 9),
                      StatementContent::AssignOp(AssignOpContent {
                          assignee: make_ast(
                              zero_range(0, 0, 0, 3),
                              ExpressionContent::Identifier(
                                  make_leaf(
                                      zero_range(0, 0, 0, 0),
                                      zero_range(0, 0, 0, 3),
                                      TokenKind::Identifier
                                  ))),
                          opr: make_leaf(
                              zero_range(0, 0, 3, 4),
                              zero_range(0, 0, 4, 6),
                              TokenKind::PlusAssign),
                          assign: make_ast(
                              zero_range(0, 0, 7, 8),
                              ExpressionContent::Literal(make_leaf(
                                  zero_range(0, 0, 6, 7),
                                  zero_range(0, 0, 7, 8),
                                  TokenKind::IntConstant))),
                          semi: make_leaf(
                              zero_range(0, 0, 8, 8),
                              zero_range(0, 0, 8, 9),
                              TokenKind::SemiColon),
                      })),
            &vec![]);
    }

    #[test]
    fn assign_statement() {
        test_statement_tree(
            "foo = 2;",
            &make_ast(zero_range(0, 0, 0, 8),
                      StatementContent::Assign(AssignContent {
                          target: AssignTarget::One(make_ast(
                              zero_range(0, 0, 0, 3),
                              ExpressionContent::Identifier(make_leaf(
                                  zero_range(0, 0, 0, 0),
                                  zero_range(0, 0, 0, 3),
                                  TokenKind::Identifier
                              )))),
                          assigner: Assigner::Chain(vec![(
                              make_leaf(zero_range(0, 0, 3, 4),
                                        zero_range(0, 0, 4, 5),
                                        TokenKind::Assign),
                              make_ast(
                                  zero_range(0, 0, 6, 7),
                                  ExpressionContent::Literal(
                                      make_leaf(
                                          zero_range(0, 0, 5, 6),
                                          zero_range(0, 0, 6, 7),
                                          TokenKind::IntConstant))))]),
                          semi: make_leaf(
                              zero_range(0, 0, 7, 7),
                              zero_range(0, 0, 7, 8),
                              TokenKind::SemiColon),
                      })),
            &vec![]);
        test_statement_tree(
            "foo = bar = 2;",
            &make_ast(zero_range(0, 0, 0, 14),
                      StatementContent::Assign(AssignContent {
                          target: AssignTarget::One(make_ast(
                              zero_range(0, 0, 0, 3),
                              ExpressionContent::Identifier(
                                  make_leaf(
                                      zero_range(0, 0, 0, 0),
                                      zero_range(0, 0, 0, 3),
                                      TokenKind::Identifier
                                  )))),
                          assigner: Assigner::Chain(vec![
                              (make_leaf(
                                  zero_range(0, 0, 3, 4),
                                  zero_range(0, 0, 4, 5),
                                  TokenKind::Assign),
                               make_ast(
                                   zero_range(0, 0, 6, 9),
                                   ExpressionContent::Identifier(
                                       make_leaf(
                                           zero_range(0, 0, 5, 6),
                                           zero_range(0, 0, 6, 9),
                                           TokenKind::Identifier)))),
                              (make_leaf(
                                  zero_range(0, 0, 9, 10),
                                  zero_range(0, 0, 10, 11),
                                  TokenKind::Assign),
                               make_ast(
                                   zero_range(0, 0, 12, 13),
                                   ExpressionContent::Literal(
                                       make_leaf(
                                           zero_range(0, 0, 11, 12),
                                           zero_range(0, 0, 12, 13),
                                           TokenKind::IntConstant)))
                              )]),
                          semi: make_leaf(
                              zero_range(0, 0, 13, 13),
                              zero_range(0, 0, 13, 14),
                              TokenKind::SemiColon),
                      })),
            &vec![]);
        test_statement_tree(
            "(foo, bar) = 2;",
            &make_ast(zero_range(0, 0, 0, 15),
                      StatementContent::Assign(AssignContent {
                          target: AssignTarget::Many(
                              make_leaf(
                                  zero_range(0, 0, 0, 0),
                                  zero_range(0, 0, 0, 1),
                                  TokenKind::LParen),
                              vec![(
                                  make_ast(
                                      zero_range(0, 0, 1, 4),
                                      ExpressionContent::Identifier(
                                          make_leaf(
                                              zero_range(0, 0, 1, 1),
                                              zero_range(0, 0, 1, 4),
                                              TokenKind::Identifier))),
                                  Some(make_leaf(
                                      zero_range(0, 0, 4, 4),
                                      zero_range(0, 0, 4, 5),
                                      TokenKind::Comma))),
                                   (make_ast(
                                       zero_range(0, 0, 6, 9),
                                       ExpressionContent::Identifier(
                                           make_leaf(
                                               zero_range(0, 0, 5, 6),
                                               zero_range(0, 0, 6, 9),
                                               TokenKind::Identifier))),
                                    None)],
                              make_leaf(
                                  zero_range(0, 0, 9, 9),
                                  zero_range(0, 0, 9, 10),
                                  TokenKind::RParen)),
                          assigner: Assigner::Chain(vec![(
                              make_leaf(
                                  zero_range(0, 0, 10, 11),
                                  zero_range(0, 0, 11, 12),
                                  TokenKind::Assign),
                              make_ast(
                                  zero_range(0, 0, 13, 14),
                                  ExpressionContent::Literal(
                                      make_leaf(
                                          zero_range(0, 0, 12, 13),
                                          zero_range(0, 0, 13, 14),
                                          TokenKind::IntConstant))))]),
                          semi: make_leaf(
                              zero_range(0, 0, 14, 14),
                              zero_range(0, 0, 14, 15),
                              TokenKind::SemiColon),
                      })),
            &vec![]);
    }

    #[test]
    fn if_statement() {
        let bar = make_ast(
            zero_range(0, 0, 9, 13),
            StatementContent::Expression(ExpressionStmtContent {
                expression: make_ast(
                    zero_range(0, 0, 9, 12),
                    ExpressionContent::Identifier(
                        make_leaf(
                            zero_range(0, 0, 8, 9),
                            zero_range(0, 0, 9, 12),
                            TokenKind::Identifier))),
                semi: make_leaf(
                    zero_range(0, 0, 12, 12),
                    zero_range(0, 0, 12, 13),
                    TokenKind::SemiColon)
            }));
        let foobar = make_ast(
            zero_range(0, 0, 19, 26),
            StatementContent::Expression(ExpressionStmtContent {
                expression: make_ast(
                    zero_range(0, 0, 19, 25),
                    ExpressionContent::Identifier(
                        make_leaf(
                            zero_range(0, 0, 18, 19),
                            zero_range(0, 0, 19, 25),
                            TokenKind::Identifier))),
                semi: make_leaf(
                    zero_range(0, 0, 25, 25),
                    zero_range(0, 0, 25, 26),
                    TokenKind::SemiColon)
            }));
        test_statement_tree(
            "if (foo) bar; else foobar;",
            &make_ast(zero_range(0, 0, 0, 26),
                      StatementContent::If(IfContent {
                          iftok: make_leaf(
                              zero_range(0, 0, 0, 0),
                              zero_range(0, 0, 0, 2),
                              TokenKind::If),
                          lparen: make_leaf(
                              zero_range(0, 0, 2, 3),
                              zero_range(0, 0, 3, 4),
                              TokenKind::LParen),
                          cond: make_ast(
                              zero_range(0, 0, 4, 7),
                              ExpressionContent::Identifier(
                                  make_leaf(
                                      zero_range(0, 0, 4, 4),
                                      zero_range(0, 0, 4, 7),
                                      TokenKind::Identifier))),
                          rparen: make_leaf(
                              zero_range(0, 0, 7, 7),
                              zero_range(0, 0, 7, 8),
                              TokenKind::RParen),
                          truebranch: bar,
                          elsebranch: Some(
                              (make_leaf(
                                  zero_range(0, 0, 13, 14),
                                  zero_range(0, 0, 14, 18),
                                  TokenKind::Else),
                               foobar)),
                      })),
            &vec![]);
    }

    #[test]
    fn switch_statement() {
        let hashif = SwitchHashIf {
            iftok: make_leaf(zero_range(0, 0, 22, 23),
                             zero_range(0, 0, 23, 26),
                             TokenKind::HashIf),
            lparen: make_leaf(zero_range(0, 0, 26, 27),
                              zero_range(0, 0, 27, 28),
                              TokenKind::LParen),
            cond: make_ast(zero_range(0, 0, 28, 29),
                           ExpressionContent::Literal(
                               make_leaf(zero_range(0, 0, 28, 28),
                                         zero_range(0, 0, 28, 29),
                                         TokenKind::IntConstant))),
            rparen: make_leaf(zero_range(0, 0, 29, 29),
                              zero_range(0, 0, 29, 30),
                              TokenKind::RParen),
            lbrace: make_leaf(zero_range(0, 0, 30, 31),
                              zero_range(0, 0, 31, 32),
                              TokenKind::LBrace),
            truecases: vec![SwitchCase::Case(
                make_leaf(zero_range(0, 0, 32, 33),
                          zero_range(0, 0, 33, 37),
                          TokenKind::Case),
                make_ast(zero_range(0, 0, 38, 39),
                         ExpressionContent::Literal(
                             make_leaf(zero_range(0, 0, 37, 38),
                                       zero_range(0, 0, 38, 39),
                                       TokenKind::IntConstant))),
                make_leaf(zero_range(0, 0, 39, 39),
                          zero_range(0, 0, 39, 40),
                          TokenKind::Colon))
            ],
            rbrace: make_leaf(zero_range(0, 0, 40, 41),
                              zero_range(0, 0, 41, 42),
                              TokenKind::RBrace),
            hashelse: None,
        };
        let cases = vec![
            SwitchCase::Case(
                make_leaf(zero_range(0, 0, 14, 15),
                          zero_range(0, 0, 15, 19),
                          TokenKind::Case),
                make_ast(zero_range(0, 0, 20, 21),
                         ExpressionContent::Literal(
                             make_leaf(zero_range(0, 0, 19, 20),
                                       zero_range(0, 0, 20, 21),
                                       TokenKind::IntConstant))),
                make_leaf(zero_range(0, 0, 21, 21),
                          zero_range(0, 0, 21, 22),
                          TokenKind::Colon)),
            SwitchCase::HashIf(hashif),
            SwitchCase::Statement(
                make_ast(
                    zero_range(0, 0, 43, 47),
                    StatementContent::Expression(ExpressionStmtContent {
                        expression: make_ast(
                            zero_range(0, 0, 43, 46),
                            ExpressionContent::Identifier(
                                make_leaf(zero_range(0, 0, 42, 43),
                                          zero_range(0, 0, 43, 46),
                                          TokenKind::Identifier))),
                        semi: make_leaf(zero_range(0, 0, 46, 46),
                                        zero_range(0, 0, 46, 47),
                                        TokenKind::SemiColon)
                    }))),
            SwitchCase::Default(make_leaf(zero_range(0, 0, 47, 48),
                                          zero_range(0, 0, 48, 55),
                                          TokenKind::Default),
                                make_leaf(zero_range(0, 0, 55, 55),
                                          zero_range(0, 0, 55, 56),
                                          TokenKind::Colon)),
            SwitchCase::Statement(
                make_ast(
                    zero_range(0, 0, 57, 61),
                    StatementContent::Expression(ExpressionStmtContent {
                        expression: make_ast(
                            zero_range(0, 0, 57, 60),
                            ExpressionContent::Identifier(
                                make_leaf(zero_range(0, 0, 56, 57),
                                          zero_range(0, 0, 57, 60),
                                          TokenKind::Identifier))),
                        semi: make_leaf(zero_range(0, 0, 60, 60),
                                        zero_range(0, 0, 60, 61),
                                        TokenKind::SemiColon)
                    }))),
            SwitchCase::Statement(
                make_ast(
                    zero_range(0, 0, 62, 69),
                    StatementContent::Expression(ExpressionStmtContent {
                        expression: make_ast(
                            zero_range(0, 0, 62, 68),
                            ExpressionContent::Identifier(
                                make_leaf(zero_range(0, 0, 61, 62),
                                          zero_range(0, 0, 62, 68),
                                          TokenKind::Identifier))),
                        semi: make_leaf(zero_range(0, 0, 68, 68),
                                        zero_range(0, 0, 68, 69),
                                        TokenKind::SemiColon)
                    }))),
        ];
        test_statement_tree(
            "switch (foo) { case 0: #if (1) { case 1: } foo; default: bar; foobar; }",
            &make_ast(zero_range(0, 0, 0, 71),
                      StatementContent::Switch(SwitchContent {
                          switchtok: make_leaf(zero_range(0, 0, 0, 0),
                                               zero_range(0, 0, 0, 6),
                                               TokenKind::Switch),
                          lparen: make_leaf(zero_range(0, 0, 6, 7),
                                            zero_range(0, 0, 7, 8),
                                            TokenKind::LParen),
                          expr: make_ast(
                              zero_range(0, 0, 8, 11),
                              ExpressionContent::Identifier(make_leaf(
                                  zero_range(0, 0, 8, 8),
                                  zero_range(0, 0, 8, 11),
                                  TokenKind::Identifier))),
                          rparen: make_leaf(zero_range(0, 0, 11, 11),
                                            zero_range(0, 0, 11, 12),
                                            TokenKind::RParen),
                          lbrace: make_leaf(zero_range(0, 0, 12, 13),
                                            zero_range(0, 0, 13, 14),
                                            TokenKind::LBrace),
                          cases,
                          rbrace: make_leaf(zero_range(0, 0, 69, 70),
                                            zero_range(0, 0, 70, 71),
                                            TokenKind::RBrace),
                      })),
            &vec![]);
    }

    #[test]
    fn log_statement() {
        use crate::analysis::parsing::expression::BinaryExpressionContent;
        test_statement_tree(
            "log info, 4: \"foo%d\" + \"bar\", 5;",
            &make_ast(zero_range(0, 0, 0, 32),
                      StatementContent::Log(LogContent {
                          log: make_leaf(zero_range(0, 0, 0, 0),
                                         zero_range(0, 0, 0, 3),
                                         TokenKind::Log),
                          log_kind: make_leaf(zero_range(0, 0, 3, 4),
                                              zero_range(0, 0, 4, 8),
                                              TokenKind::Identifier),
                          level: Some((
                              make_leaf(zero_range(0, 0, 8, 8),
                                        zero_range(0, 0, 8, 9),
                                        TokenKind::Comma),
                              LogLevel::Simple(
                                  make_ast(zero_range(0, 0, 10, 11),
                                           ExpressionContent::Literal(make_leaf(
                                               zero_range(0, 0, 9, 10),
                                               zero_range(0, 0, 10, 11),
                                               TokenKind::IntConstant)))))),
                          flags: None,
                          colon: make_leaf(zero_range(0, 0, 11, 11),
                                           zero_range(0, 0, 11, 12),
                                           TokenKind::Colon),
                          message: make_ast(
                              zero_range(0, 0, 13, 28),
                              ExpressionContent::BinaryExpression(
                                  BinaryExpressionContent {
                                      operation: make_leaf(
                                          zero_range(0, 0, 20, 21),
                                          zero_range(0, 0, 21, 22),
                                          TokenKind::Plus),
                                      left: make_ast(
                                          zero_range(0, 0, 13, 20),
                                          ExpressionContent::Literal(
                                              make_leaf(
                                                  zero_range(0, 0, 12, 13),
                                                  zero_range(0, 0, 13, 20),
                                                  TokenKind::StringConstant))),
                                      right: make_ast(
                                          zero_range(0, 0, 23, 28),
                                          ExpressionContent::Literal(
                                              make_leaf(
                                                  zero_range(0, 0, 22, 23),
                                                  zero_range(0, 0, 23, 28),
                                                  TokenKind::StringConstant)))
                                  })),
                          args: vec![
                              (make_leaf(zero_range(0, 0, 28, 28),
                                         zero_range(0, 0, 28, 29),
                                         TokenKind::Comma),
                               make_ast(zero_range(0, 0, 30, 31),
                                        ExpressionContent::Literal(make_leaf(
                                            zero_range(0, 0, 29, 30),
                                            zero_range(0, 0, 30, 31),
                                            TokenKind::IntConstant))))],
                          semi: make_leaf(zero_range(0, 0, 31, 31),
                                          zero_range(0, 0, 31, 32),
                                          TokenKind::SemiColon),
                      })),
            &vec![]);
    }

    #[test]
    fn after_statement() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let timer = AfterTiming::Timer(
            make_ast(zero_range(0,0,6,7),
                     ExpressionContent::Literal(
                         make_leaf(zero_range(0,0,5,6),
                                   zero_range(0,0,6,7),
                                   TokenKind::IntConstant))),
            make_leaf(zero_range(0,0,7,7),
                      zero_range(0,0,7,8),
                      TokenKind::Identifier));
        let fun = make_ast(zero_range(0,0,10,13),
                           ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,9,10),
                               zero_range(0,0,10,13),
                               TokenKind::Identifier)));
        let call = make_ast(zero_range(0,0,10,15),
                            ExpressionContent::FunctionCall(
                                FunctionCallContent {
                                    fun,
                                    lparen: make_leaf(zero_range(0,0,13,13),
                                                      zero_range(0,0,13,14),
                                                      TokenKind::LParen),
                                    arguments: vec![],
                                    rparen: make_leaf(zero_range(0,0,14,14),
                                                      zero_range(0,0,14,15),
                                                      TokenKind::RParen)
                                }));
        test_statement_tree(
            "after 4s: foo();",
            &make_ast(zero_range(0, 0, 0, 16),
                      StatementContent::After(AfterContent {
                          after: make_leaf(zero_range(0,0,0,0),
                                           zero_range(0,0,0,5),
                                           TokenKind::After),
                          timer: Some(timer),
                          colon: make_leaf(zero_range(0,0,8,8),
                                           zero_range(0,0,8,9),
                                           TokenKind::Colon),
                          callexpression: call,
                          semi: make_leaf(zero_range(0,0,15,15),
                                          zero_range(0,0,15,16),
                                          TokenKind::SemiColon)
                      })),
            &vec![]
        );
    }
    #[test]
    fn immediate_after_statement() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let fun = make_ast(zero_range(0,0,7,10),
                           ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,6,7),
                               zero_range(0,0,7,10),
                               TokenKind::Identifier)));
        let call = make_ast(zero_range(0,0,7,12),
                            ExpressionContent::FunctionCall(
                                FunctionCallContent {
                                    fun,
                                    lparen: make_leaf(zero_range(0,0,10,10),
                                                      zero_range(0,0,10,11),
                                                      TokenKind::LParen),
                                    arguments: vec![],
                                    rparen: make_leaf(zero_range(0,0,11,11),
                                                      zero_range(0,0,11,12),
                                                      TokenKind::RParen)
                                }));
        test_statement_tree(
            "after: foo();",
            &make_ast(zero_range(0, 0, 0, 13),
                      StatementContent::After(AfterContent {
                          after: make_leaf(zero_range(0,0,0,0),
                                           zero_range(0,0,0,5),
                                           TokenKind::After),
                          timer: None,
                          colon: make_leaf(zero_range(0,0,5,5),
                                           zero_range(0,0,5,6),
                                           TokenKind::Colon),
                          callexpression: call,
                          semi: make_leaf(zero_range(0,0,12,12),
                                          zero_range(0,0,12,13),
                                          TokenKind::SemiColon)
                      })),
            &vec![]
        );
    }

    #[test]
    fn after_hook_statement() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let hook = make_ast(zero_range(0, 0, 6, 9),
                            ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,5,6),
                               zero_range(0,0,6,9),
                               TokenKind::Identifier)));
        let fun = make_ast(zero_range(0,0,11,14),
                           ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,10,11),
                               zero_range(0,0,11,14),
                               TokenKind::Identifier)));
        let call = make_ast(zero_range(0,0,11,16),
                            ExpressionContent::FunctionCall(
                                FunctionCallContent {
                                    fun,
                                    lparen: make_leaf(zero_range(0,0,14,14),
                                                      zero_range(0,0,14,15),
                                                      TokenKind::LParen),
                                    arguments: vec![],
                                    rparen: make_leaf(zero_range(0,0,15,15),
                                                      zero_range(0,0,15,16),
                                                      TokenKind::RParen)
                                }));
        test_statement_tree(
            "after foo: bar();",
            &make_ast(zero_range(0, 0, 0, 17),
                      StatementContent::After(AfterContent {
                          after: make_leaf(zero_range(0,0,0,0),
                                           zero_range(0,0,0,5),
                                           TokenKind::After),
                          timer: Some(AfterTiming::HookNoParams(hook)),
                          colon: make_leaf(zero_range(0,0,9,9),
                                           zero_range(0,0,9,10),
                                           TokenKind::Colon),
                          callexpression: call,
                          semi: make_leaf(zero_range(0,0,16,16),
                                          zero_range(0,0,16,17),
                                          TokenKind::SemiColon)
                      })),
            &vec![]
        );
    }

    #[test]
    fn after_hook_binds_statement() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let hook = make_ast(zero_range(0, 0, 6, 9),
                            ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,5,6),
                               zero_range(0,0,6,9),
                                TokenKind::Identifier)));
        let bind = AfterTiming::HookBindList(
            hook,
            make_leaf(zero_range(0,0,9,10),
                      zero_range(0,0,10,12),
                      TokenKind::Arrow),
            make_leaf(zero_range(0,0,12,13),
                      zero_range(0,0,13,14),
                      TokenKind::LParen),
            vec![(make_leaf(zero_range(0,0,14,14),
                            zero_range(0,0,14,15),
                            TokenKind::Identifier),
                  Some(make_leaf(zero_range(0,0,15,15),
                                 zero_range(0,0,15,16),
                                 TokenKind::Comma))),
                 (make_leaf(zero_range(0,0,16,17),
                            zero_range(0,0,17,18),
                            TokenKind::Identifier),
                  None)],
            make_leaf(zero_range(0,0,18,18),
                      zero_range(0,0,18,19),
                      TokenKind::RParen));
        let fun = make_ast(zero_range(0,0,21,24),
                           ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,20,21),
                               zero_range(0,0,21,24),
                               TokenKind::Identifier)));
        let call = make_ast(zero_range(0,0,21,26),
                            ExpressionContent::FunctionCall(
                                FunctionCallContent {
                                    fun,
                                    lparen: make_leaf(zero_range(0,0,24,24),
                                                      zero_range(0,0,24,25),
                                                      TokenKind::LParen),
                                    arguments: vec![],
                                    rparen: make_leaf(zero_range(0,0,25,25),
                                                      zero_range(0,0,25,26),
                                                      TokenKind::RParen)
                                }));
        test_statement_tree(
            "after foo -> (x, y): bar();",
            &make_ast(zero_range(0, 0, 0, 27),
                      StatementContent::After(AfterContent {
                          after: make_leaf(zero_range(0,0,0,0),
                                           zero_range(0,0,0,5),
                                           TokenKind::After),
                          timer: Some(bind),
                          colon: make_leaf(zero_range(0,0,19,19),
                                           zero_range(0,0,19,20),
                                           TokenKind::Colon),
                          callexpression: call,
                          semi: make_leaf(zero_range(0,0,26,26),
                                          zero_range(0,0,26,27),
                                          TokenKind::SemiColon)
                      })),
            &vec![]
        );
    }

    #[test]
    fn after_hook_bind_statement() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let hook = make_ast(zero_range(0, 0, 6, 9),
                            ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,5,6),
                               zero_range(0,0,6,9),
                                TokenKind::Identifier)));
        let bind = AfterTiming::HookBindOne(
            hook,
            make_leaf(zero_range(0,0,9,10),
                      zero_range(0,0,10,12),
                      TokenKind::Arrow),
            make_leaf(zero_range(0,0,12,13),
                      zero_range(0,0,13,14),
                      TokenKind::Identifier));
        let fun = make_ast(zero_range(0,0,16,19),
                           ExpressionContent::Identifier(make_leaf(
                               zero_range(0,0,15,16),
                               zero_range(0,0,16,19),
                               TokenKind::Identifier)));
        let call = make_ast(zero_range(0,0,16,21),
                            ExpressionContent::FunctionCall(
                                FunctionCallContent {
                                    fun,
                                    lparen: make_leaf(zero_range(0,0,19,19),
                                                      zero_range(0,0,19,20),
                                                      TokenKind::LParen),
                                    arguments: vec![],
                                    rparen: make_leaf(zero_range(0,0,20,20),
                                                      zero_range(0,0,20,21),
                                                      TokenKind::RParen)
                                }));
        test_statement_tree(
            "after foo -> x: bar();",
            &make_ast(zero_range(0, 0, 0, 22),
                      StatementContent::After(AfterContent {
                          after: make_leaf(zero_range(0,0,0,0),
                                           zero_range(0,0,0,5),
                                           TokenKind::After),
                          timer: Some(bind),
                          colon: make_leaf(zero_range(0,0,14,14),
                                           zero_range(0,0,14,15),
                                           TokenKind::Colon),
                          callexpression: call,
                          semi: make_leaf(zero_range(0,0,21,21),
                                          zero_range(0,0,21,22),
                                          TokenKind::SemiColon)
                      })),
            &vec![]
        );
    }

    #[test]
    fn default_statement_in_default_case() {
        use crate::analysis::parsing::expression::FunctionCallContent;
        let default_call = ExpressionContent::FunctionCall(
            FunctionCallContent {
                fun: ExpressionContent::Identifier(
                    make_leaf(zero_range(0, 0, 21, 22),
                              zero_range(0, 0, 22, 29),
                              TokenKind::Default)).into(),
                lparen: make_leaf(zero_range(0, 0, 29, 29),
                                  zero_range(0, 0, 29, 30),
                                  TokenKind::LParen),
                arguments: vec![],
                rparen: make_leaf(zero_range(0, 0, 30, 30),
                                  zero_range(0, 0, 30, 31),
                                  TokenKind::RParen),
            });
        let default_case = vec![
            SwitchCase::Default(
                make_leaf(zero_range(0, 0, 12, 13),
                          zero_range(0, 0, 13, 20),
                          TokenKind::Default),
                make_leaf(zero_range(0, 0, 20, 20),
                          zero_range(0, 0, 20, 21),
                          TokenKind::Colon)),
            SwitchCase::Statement(
                make_ast(
                    zero_range(0, 0, 22, 32),
                    StatementContent::Expression(ExpressionStmtContent {
                        expression: default_call.into(),
                        semi: make_leaf(zero_range(0, 0, 31, 31),
                                        zero_range(0, 0, 31, 32),
                                        TokenKind::SemiColon)
                    }))),
        ];
        test_statement_tree(
            "switch (a) { default: default(); }",
            &make_ast(zero_range(0, 0, 0, 34),
                      StatementContent::Switch(SwitchContent {
                          switchtok: make_leaf(zero_range(0, 0, 0, 0),
                                               zero_range(0, 0, 0, 6),
                                               TokenKind::Switch),
                          lparen: make_leaf(zero_range(0, 0, 6, 7),
                                            zero_range(0, 0, 7, 8),
                                            TokenKind::LParen),
                          expr: make_ast(
                              zero_range(0, 0, 8, 9),
                              ExpressionContent::Identifier(make_leaf(
                                  zero_range(0, 0, 8, 8),
                                  zero_range(0, 0, 8, 9),
                                  TokenKind::Identifier))),
                          rparen: make_leaf(zero_range(0, 0, 9, 9),
                                            zero_range(0, 0, 9, 10),
                                            TokenKind::RParen),
                          lbrace: make_leaf(zero_range(0, 0, 10, 11),
                                            zero_range(0, 0, 11, 12),
                                            TokenKind::LBrace),
                          cases: default_case,
                          rbrace: make_leaf(zero_range(0, 0, 32, 33),
                                            zero_range(0, 0, 33, 34),
                                            TokenKind::RBrace),
                      })),
            &vec![]);
    }
}

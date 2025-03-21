//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
// Types, traits, and structs for the structure of a DML file
use log::{trace};

use crate::analysis::parsing::expression::{Expression,
                                           ensure_string_concatenation};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::misc::{CDecl, CDeclList, Initializer,
                                     ident_filter, objident_filter};
use crate::analysis::parsing::statement::Statement;
use crate::analysis::parsing::tree::{AstObject, TreeElement, TreeElements,
                                     LeafToken, ZeroRange};
use crate::analysis::parsing::types::CTypeDecl;
use crate::analysis::parsing::parser::{doesnt_understand_tokens,
                                       FileParser, Parse, ParseContext,
                                       FileInfo};
use crate::analysis::LocalDMLError;
use crate::lint::rules::spacing::{SpBracesArgs,
                                  NspInparenArgs,
                                  NspFunparArgs,
                                  SpPunctArgs};
use crate::lint::rules::indentation::{IN3Args};
use crate::lint::{rules::CurrentRules, AuxParams, DMLStyleError};
use crate::analysis::reference::{Reference, ReferenceKind};
use crate::analysis::FileSpec;
use crate::span::Range;
use crate::vfs::TextFile;

fn object_contexts(context: &ParseContext)
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
pub enum ArgumentDecl {
    Typed(CDecl),
    Inline(LeafToken, LeafToken),
}

impl TreeElement for ArgumentDecl {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Typed(content) => content.range(),
            Self::Inline(inline, name) => Range::combine(inline.range(),
                                                         name.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Typed(cdecl) => create_subs!(cdecl),
            Self::Inline(inline, name) => create_subs!(inline, name),
        }
    }
    // TODO: typerefs in cdecl
}

fn parse_argumentdecl(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                      -> ArgumentDecl {
    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        Some(TokenKind::Inline) => {
            let inline = new_context.next_leaf(stream);
            let name = new_context.expect_next_filter(
                stream, ident_filter, "argument name");
            ArgumentDecl::Inline(inline, name)
        },
        _ => ArgumentDecl::Typed(CDecl::parse(&new_context, stream, file_info)),
    }
}

type ReturnContent = (LeafToken, LeafToken,
                      Vec<(CTypeDecl, Option<LeafToken>)>, LeafToken);

#[derive(Debug, Clone, PartialEq)]
pub struct MethodContent {
    // inline/shared
    pub modifier: Option<LeafToken>,
    pub independent: Option<LeafToken>,
    pub startup: Option<LeafToken>,
    pub memoized: Option<LeafToken>,
    pub method: LeafToken,
    pub name: LeafToken,
    pub lparen: LeafToken,
    pub arguments: Vec<(ArgumentDecl, Option<LeafToken>)>,
    pub rparen: LeafToken,
    pub returns: Option<ReturnContent>,
    pub throws: Option<LeafToken>,
    pub default: Option<LeafToken>,
    pub statements: Statement,
}

impl TreeElement for MethodContent {
    fn range(&self) -> ZeroRange {
        let first_tok = self.modifier.as_ref().or(self.independent.as_ref())
            .or(self.startup.as_ref()).or(self.memoized.as_ref()).unwrap_or(
                &self.method);
        Range::combine(first_tok.range(),
                       self.statements.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.modifier,
                     &self.independent,
                     &self.startup,
                     &self.memoized,
                     &self.method,
                     &self.name,
                     &self.lparen,
                     &self.arguments,
                     &self.rparen,
                     &self.returns,
                     &self.throws,
                     &self.default,
                     &self.statements)
    }

    fn references<'a>(&self,
                      _accumulator: &mut Vec<Reference>,
                      _file: FileSpec<'a>) {}

    #[allow(clippy::unnecessary_unwrap)]
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        let mut errors = vec![];
        let inline = self.modifier.as_ref().and_then(
            |t| t.get_token()).and_then(
            |t| if t.kind == TokenKind::Inline {
                Some(t)
            } else {
                None
            });
        if self.startup.is_some() {
            if !self.arguments.is_empty() {
                errors.push(LocalDMLError {
                    range: self.arguments.range(),
                    description: "a method declared as startup \
                                  cannot have input parameters".to_string(),
                });
            }
            if self.memoized.is_some()
                && self.throws.is_none()
                && self.returns.is_none() {
                    errors.push(LocalDMLError {
                        range: self.memoized.range(),
                        description: "a method declared as memoized \
                                      must either throw or have return \
                                      types".to_string(),
                    });
                }
            if self.memoized.is_none() {
                if self.throws.is_some() {
                    errors.push(LocalDMLError {
                        range: self.throws.range(),
                        description: "a startup method not declared as \
                                      memoized cannot throw".to_string(),
                    });
                }
                if self.returns.is_some() {
                    errors.push(LocalDMLError {
                        range: self.returns.range(),
                        description: "a startup method not declared as \
                                      memoized cannot have return \
                                      values".to_string(),
                    });
                }
            }
            if self.default.is_some() {
                errors.push(LocalDMLError {
                    range: self.default.range(),
                    description: "a startup method cannot be declared \
                                  as default".to_string(),
                });
            }
        }

        if self.independent.is_none() {
            if self.startup.is_some() {
                errors.push(LocalDMLError {
                            range: self.startup.range(),
                            description: "a method declared as startup \
                                          must also be declared \
                                          independent".to_string(),
                });
            }
            if self.memoized.is_some() {
                errors.push(LocalDMLError {
                            range: self.startup.range(),
                            description: "a method declared as memoized \
                                          must also be declared \
                                          independent".to_string(),
                });
            }
        }

        if self.memoized.is_some() && self.startup.is_none() {
            errors.push(LocalDMLError {
                range: self.startup.range(),
                description: "a method declared as memoized \
                              must also be declared \
                              startup".to_string(),
            });
        }

        let mut has_inline_arg = false;
        for (arg, _) in &self.arguments {
            match arg {
                ArgumentDecl::Typed(cdecl) => {
                    errors.append(&mut cdecl.ensure_named());
                },
                ArgumentDecl::Inline(inl, _) => {
                    has_inline_arg = true;
                    if inline.is_none() {
                        errors.push(LocalDMLError {
                            range: inl.range(),
                            description: "inline arguments require method to \
                                          be declared as inline".to_string(),
                        });
                    }
                }
            }
        }
        if !has_inline_arg && inline.is_some() {
            errors.push(LocalDMLError {
                // There is a more rustic way to do this, but its either less
                // concise or unstable
                range: inline.unwrap().range,
                description: "only use inline if there are \
                              untyped arguments".to_string(),
            });
        }
        errors
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, _aux: &mut AuxParams) {
        rules.nsp_funpar.check(acc, NspFunparArgs::from_method(self));
        rules.nsp_inparen.check(acc, NspInparenArgs::from_method(self));
        rules.sp_punct.check(acc, SpPunctArgs::from_method(self));
    }
}

fn parse_method(modifier: Option<LeafToken>,
                context: &ParseContext,
                stream: &mut FileParser<'_>,
                file_info: &FileInfo) -> DMLObject {
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }
    fn understands_lbrace_or_semi(token: TokenKind) -> bool {
        token == TokenKind::LBrace || token == TokenKind::SemiColon
    }
    fn understands_comma(token: TokenKind) -> bool {
        token == TokenKind::Comma
    }
    let outer = context.enter_context(understands_lbrace_or_semi);
    let mut pre_statements_context = outer.enter_context(
        doesnt_understand_tokens);
    let independent = pre_statements_context.next_if_kind(
        stream, TokenKind::Independent);
    let startup = pre_statements_context.next_if_kind(
        stream, TokenKind::Startup);
    let memoized = pre_statements_context.next_if_kind(
        stream, TokenKind::Memoized);
    let method = pre_statements_context.expect_next_kind(
        stream, TokenKind::Method);
    let name = pre_statements_context.expect_next_filter(
        stream, objident_filter, "method name");
    let lparen = pre_statements_context.expect_next_kind(
        stream, TokenKind::LParen);
    let mut paren_context = pre_statements_context.enter_context(
        understands_rparen);
    let mut end = matches!(paren_context.peek_kind(stream),
                           Some(TokenKind::RParen));
    let mut arguments = vec![];
    while !end {
        let mut comma_context = paren_context.enter_context(
            understands_comma);
        let arg = parse_argumentdecl(&comma_context, stream, file_info);
        let comma = match comma_context.peek_kind(stream) {
            Some(TokenKind::Comma) => Some(comma_context.next_leaf(stream)),
            _ => {
                end = true;
                None
            }
        };
        arguments.push((arg, comma));
    }
    let rparen = pre_statements_context.expect_next_kind(
        stream, TokenKind::RParen);
    let returns = match pre_statements_context.peek_kind(stream) {
        Some(TokenKind::Arrow) => {
            let arrow = pre_statements_context.next_leaf(stream);
            let lparen = pre_statements_context.expect_next_kind(
                stream, TokenKind::LParen);
            let mut paren_context = pre_statements_context.enter_context(
                understands_rparen);
            let mut types = vec![];
            // if return is declared, it cannot be empty
                let mut end = false;
            while !end {
                let typed = CTypeDecl::parse(&paren_context, stream, file_info);
                let comma = match paren_context.peek_kind(stream) {
                    Some(TokenKind::Comma) =>
                        Some(paren_context.next_leaf(stream)),
                    _ => {
                        end = true;
                        None
                    },
                };
                types.push((typed, comma));
            }
            let rparen = pre_statements_context.expect_next_kind(
                stream, TokenKind::RParen);
            Some((arrow, lparen, types, rparen))
        },
        _ => None,
    };
    let throws = pre_statements_context.next_if_kind(
        stream, TokenKind::Throws);
    let default = pre_statements_context.next_if_kind(
        stream, TokenKind::Default);
    let statements = Statement::parse(&outer, stream, file_info);
    DMLObjectContent::Method(MethodContent {
        modifier, independent, startup, memoized, method, name, lparen,
        arguments, rparen, returns, throws, default, statements
    }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParamDef {
    // Either default or =
    Set(LeafToken, Expression),
    Auto(LeafToken),
    Typed(LeafToken, CTypeDecl),
    Empty,
}

impl TreeElement for ParamDef {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Set(kw, expr) => Range::combine(kw.range(), expr.range()),
            Self::Auto(tok) => tok.range(),
            Self::Typed(colon, typedecl) =>
                Range::combine(colon.range(), typedecl.range()),
            Self::Empty => ZeroRange::invalid(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Set(kw, expr) => create_subs!(kw, expr),
            Self::Auto(tok) => create_subs!(tok),
            Self::Typed(colon, typedecl) =>
                create_subs!(colon, typedecl),
            Self::Empty => vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterContent {
    pub param: LeafToken,
    pub name: LeafToken,
    pub def: ParamDef,
    pub semi: LeafToken,
}

impl TreeElement for ParameterContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.param.range(),
            self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.param,
                     &self.name,
                     &self.def,
                     &self.semi)
    }
}

impl Parse<DMLObjectContent> for ParameterContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let param = inner.expect_next_kind(stream, TokenKind::Param);
        let name = inner.expect_next_filter(
            stream, objident_filter, "parameter name");
        let def = match inner.peek_kind(stream) {
            Some(TokenKind::Assign) | Some(TokenKind::Default) => {
                let assign = inner.next_leaf(stream);
                let value = Expression::parse(&inner, stream, file_info);
                ParamDef::Set(assign, value)
            },
            Some(TokenKind::Auto) => ParamDef::Auto(inner.next_leaf(stream)),
            Some(TokenKind::Colon) => {
                let colon = inner.next_leaf(stream);
                let typed = CTypeDecl::parse(&inner, stream, file_info);
                ParamDef::Typed(colon, typed)
            },
            _ => ParamDef::Empty,
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Parameter(ParameterContent {
            param, name, def, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDecl {
    One(CDecl),
    Many(LeafToken, CDeclList, LeafToken),
}

impl TreeElement for VarDecl {
    fn range(&self) -> ZeroRange {
        match self {
            Self::One(decl) => decl.range(),
            Self::Many(l, _, r) => ZeroRange::combine(l.range(), r.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::One(decl) => decl.subs(),
            Self::Many(l, decls, r) => create_subs!(l, decls, r),
        }
    }
}

impl VarDecl {
    pub fn ensure_named(&self) -> Vec<LocalDMLError> {
        match &self {
            VarDecl::One(decl) => decl.ensure_named(),
            VarDecl::Many(_, cdecls, _) => cdecls.ensure_named(),
        }
    }
}

pub fn parse_vardecl(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
                     -> VarDecl {
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }

    let mut new_context = context.enter_context(doesnt_understand_tokens);
    match new_context.peek_kind(stream) {
        Some(TokenKind::LParen) => {
            let mut parencontext = new_context.enter_context(
                understands_rparen);
            let lparen = parencontext.next_leaf(stream);
            let cdecls = CDeclList::parse_nonempty(&parencontext, stream, file_info);
            let rparen = parencontext.expect_next_kind(
                stream, TokenKind::RParen);
            VarDecl::Many(lparen, cdecls, rparen)
        },
        _ => VarDecl::One(CDecl::parse(&new_context, stream, file_info))
    }
}

// session, saved, and extern
#[derive(Debug, Clone, PartialEq)]
pub struct VariableContent {
    pub kind: LeafToken,
    pub cdecl: VarDecl,
    pub initializer: Option<(LeafToken, Initializer)>,
    pub semi: LeafToken,
}

impl TreeElement for VariableContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.kind.range(),
            self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.kind,
                     &self.cdecl,
                     &self.initializer,
                     &self.semi)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        self.cdecl.ensure_named()
    }
}

impl Parse<DMLObjectContent> for VariableContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        // Guaranteed by parser
        let variable_kind = inner.peek_kind(stream).unwrap();
        let kind = inner.next_leaf(stream);
        let cdecl = parse_vardecl(&inner, stream, file_info);
        let initializer = match inner.peek_kind(stream) {
            Some(TokenKind::Assign) => {
                let assign = inner.next_leaf(stream);
                let initializer = Initializer::parse(&inner, stream, file_info);
                Some((assign, initializer))
            },
            _ => None,
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        let variablecontent = VariableContent {
            kind, cdecl, initializer, semi
        };
        match variable_kind {
            TokenKind::Session => DMLObjectContent::Session(variablecontent),
            TokenKind::Saved => DMLObjectContent::Saved(variablecontent),
            _ => panic!("Internal Parser Error: \
                         Unexpected variable object kind"),
        }.into()
    }
}

// Because we need to distinguish between 'extern cdecl' and 'extern typedef'
// the externtok is parsed one step above
fn parse_extern(
    externtok: LeafToken, context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
    -> DMLObject {
    let (mut outer, mut inner) = object_contexts(context);
    let cdecl = VarDecl::One(CDecl::parse(&inner, stream, file_info));
    let initializer = match inner.peek_kind(stream) {
        Some(TokenKind::Assign) => {
            let assign = inner.next_leaf(stream);
            let initializer = Initializer::parse(&inner, stream, file_info);
            Some((assign, initializer))
        },
        _ => None,
    };
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    DMLObjectContent::Extern(VariableContent {
        kind: externtok, cdecl, initializer, semi
    }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantContent {
    pub constant: LeafToken,
    pub name: LeafToken,
    pub equals: LeafToken,
    pub expression: Expression,
    pub semi: LeafToken,
}

impl TreeElement for ConstantContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            self.constant.range(),
            self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.constant,
                     &self.name,
                     &self.equals,
                     &self.expression,
                     &self.semi)
    }
}

impl Parse<DMLObjectContent> for ConstantContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        // Guaranteed by parser
        let constant = inner.expect_next_kind(stream, TokenKind::Constant);
        let name = inner.expect_next_filter(
            stream, ident_filter, "constant name");
        let equals = inner.expect_next_kind(stream, TokenKind::Assign);
        let expression = Expression::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Constant(ConstantContent {
            constant, name, equals, expression, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instantiation {
    One(LeafToken),
    Many(LeafToken, Vec<(LeafToken, Option<LeafToken>)>, LeafToken),
}

impl TreeElement for Instantiation {
    fn range(&self) -> ZeroRange {
        match self {
            Self::One(token) => token.range(),
            Self::Many(left, _, right) => Range::combine(left.range(),
                                                         right.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::One(token) => create_subs!(token),
            Self::Many(left, vect, right) =>
                create_subs!(left, vect, right),
        }
    }
    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        let toks = match self {
            Instantiation::One(tok) => vec![tok],
            Instantiation::Many(_, toks, _) => toks.iter().map(
                |(tok,_)|tok).collect(),
        };

        let refs = toks.into_iter().filter_map(
            |tok|Reference::global_from_token(
                tok, file, ReferenceKind::Template));
        accumulator.extend(refs);
    }
}

fn parse_instantiation(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo)
                       -> Instantiation {
    fn understands_rparen(token: TokenKind) -> bool {
        token == TokenKind::RParen
    }
    let mut outer = context.enter_context(doesnt_understand_tokens);
    match outer.peek_kind(stream) {
        Some(TokenKind::LParen) => {
            let lparen = outer.next_leaf(stream);
            let mut paren_context = outer.enter_context(understands_rparen);
            let mut instantiations = vec![];
            // At least one instantiation is required
            let first_instantiation = paren_context.expect_next_filter(
                stream, objident_filter, "template name");
            let mut end = true;
            let first_comma = match paren_context.peek_kind(stream) {
                Some(TokenKind::Comma) => {
                    end = false;
                    Some(paren_context.next_leaf(stream))
                },
                _ => None
            };
            instantiations.push((first_instantiation, first_comma));
            while !end {
                let instantiation = paren_context.expect_next_filter(
                    stream, objident_filter, "template name");
                let comma = match paren_context.peek_kind(stream) {
                    Some(TokenKind::Comma) =>
                        Some(paren_context.next_leaf(stream)),
                    _ => {
                        end = true;
                        None
                    }
                };
                instantiations.push((instantiation, comma));
            }
            let rparen = outer.expect_next_kind(stream, TokenKind::RParen);
            Instantiation::Many(lparen, instantiations, rparen)
        },
        _ => Instantiation::One(outer.expect_next_filter(
            stream, objident_filter, "template name"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstantiationContent {
    pub is: LeafToken,
    pub instantiation: Instantiation,
    pub semi: LeafToken,
}

impl TreeElement for InstantiationContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.is.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.is, &self.instantiation, &self.semi)
    }
}

impl Parse<DMLObjectContent> for InstantiationContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo) -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let is = inner.expect_next_kind(stream, TokenKind::Is);
        let instantiation = parse_instantiation(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Instantiation(InstantiationContent {
            is, instantiation, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectStatementsContent {
    Empty(LeafToken),
    List(LeafToken, Vec<DMLObject>, LeafToken),
}

impl TreeElement for ObjectStatementsContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Empty(token) => token.range(),
            Self::List(left, _, right) => Range::combine(left.range(),
                                                         right.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Empty(token) => create_subs!(token),
            Self::List(left, vect, right) =>
                create_subs!(left, vect, right)
        }
    }
    fn evaluate_rules(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, aux: &mut AuxParams) {
        rules.sp_brace.check(acc, SpBracesArgs::from_obj_stmts(self));
        rules.in3.check(acc, IN3Args::from_obj_stmts_content(self, &mut aux.depth));
    }
}

fn check_dmlobject_kind(obj: &DMLObjectContent, _file: &TextFile) ->
    Vec<LocalDMLError> {
        trace!("Checking kind restriction on {:?}", obj);
        match obj {
            DMLObjectContent::Parameter(paramcontent) => {
                if let ParamDef::Typed(_, _) = &paramcontent.def {
                    return vec![LocalDMLError {
                        range: obj.range(),
                        description: "Typed parameter declaration only \
                                      permitted in top level template \
                                      block".to_string(),
                    }];
                }
            },
            DMLObjectContent::Method(methodcontent) => {
                if methodcontent.modifier.as_ref().and_then(|m|m.get_token())
                    .map_or(false, |s|s.kind == TokenKind::Shared) {
                        return vec![LocalDMLError {
                            range: obj.range(),
                            description: "Shared method \
                                          declaration only \
                                          permitted in top level \
                                          template \
                                          block".to_string(),
                        }]
                    }
            },
            _ => {}
        }
        vec![]
    }

impl ObjectStatementsContent {
    fn check_template_only_stmnts(&self, file: &TextFile) -> Vec<LocalDMLError> {
        match self {
            ObjectStatementsContent::Empty(_) => vec![],
            ObjectStatementsContent::List(_, list, _) => {
                let mut errors = vec![];
                for object in list {
                    errors.append(&mut object.with_content(
                        |obj|check_dmlobject_kind(obj, file), vec![]));
                }
                errors
            },
        }
    }
}

pub type ObjectStatements = AstObject<ObjectStatementsContent>;

fn objectstatements_first_token_matcher(token: TokenKind) -> bool {
    matches!(token, TokenKind::LBrace | TokenKind::SemiColon)
}

impl Parse<ObjectStatementsContent> for ObjectStatements {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> ObjectStatements {
        fn understands_rbrace(token: TokenKind) -> bool {
        token == TokenKind::RBrace
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        match outer.expect_peek_filter(
            stream, objectstatements_first_token_matcher, "object statements") {
            LeafToken::Actual(token) => match token.kind {
                TokenKind::LBrace => {
                    let lbrace = outer.next_leaf(stream);
                    let mut bracecontext = outer.enter_context(
                        understands_rbrace);
                    let mut statements = vec![];
                    while bracecontext.peek_kind(stream).map_or(
                        false, |t|dmlobject_first_token_matcher(t)) {
                        statements.push(
                            DMLObject::parse(&bracecontext, stream, file_info));
                    }
                    let rbrace = bracecontext.expect_next_kind(
                        stream, TokenKind::RBrace);
                    ObjectStatementsContent::List(
                        lbrace, statements, rbrace).into()
                },
                TokenKind::SemiColon =>
                    ObjectStatementsContent::Empty(
                        outer.next_leaf(stream)).into(),
                _ => unreachable!(),
            },
            LeafToken::Missing(missing) => missing.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeObjectContent {
    pub kind: LeafToken,
    pub name: LeafToken,
    pub dimensions: Vec<(LeafToken,
                         LeafToken, LeafToken, ArraySize,
                         LeafToken)>,
    pub instantiation: Option<(LeafToken, Instantiation)>,
    pub documentation: Option<Expression>,
    pub statements: ObjectStatements,
}

impl TreeElement for CompositeObjectContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.kind.range(),
                       self.statements.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.kind,
                     &self.name,
                     &self.dimensions,
                     &self.instantiation,
                     &self.documentation,
                     &self.statements)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        let mut errors = vec![];
        if let Some(doc) = &self.documentation {
            errors.append(&mut ensure_string_concatenation(doc));
        }
        errors.append(&mut self.statements.with_content(
            |stmnts|stmnts.check_template_only_stmnts(file), vec![]));
        errors
    }
    fn references<'a>(&self, _accumulator: &mut Vec<Reference>, _file: FileSpec<'a>) {}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArraySize {
    Defined(Expression),
    Implicit(LeafToken),
}

impl TreeElement for ArraySize {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Defined(expr) => expr.range(),
            Self::Implicit(tok) => tok.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Defined(expr) => create_subs!(expr),
            Self::Implicit(tok) => create_subs!(tok),
        }
    }
}

// [ident < expression|...|] *
pub type ArraySizeDef = Vec<(LeafToken, LeafToken, LeafToken,
                             ArraySize, LeafToken)>;

fn parse_composite_object_name(context: &ParseContext,
                               stream: &mut FileParser<'_>, file_info: &FileInfo)
                               -> (LeafToken, ArraySizeDef)
{
    fn understands_rbracket(token: TokenKind) -> bool {
        token == TokenKind::RBracket
    }
    fn understands_lessthan(token: TokenKind) -> bool {
        token == TokenKind::LessThan
    }
    fn understands_ellipsis(token: TokenKind) -> bool {
        token == TokenKind::Ellipsis
    }
    let mut outer = context.enter_context(doesnt_understand_tokens);
    let name = outer.expect_next_filter(stream, objident_filter, "object name");
    let mut dimensions = vec![];
    while matches!(outer.peek_kind(stream), Some(TokenKind::LBracket)) {
        let lbracket = outer.next_leaf(stream);
        let mut bracket_context = outer.enter_context(understands_rbracket);
        let mut ellipsis_context = bracket_context.enter_context(
            understands_ellipsis);
        let mut lessthan_context = ellipsis_context.enter_context(
            understands_lessthan);
        let indexvar = lessthan_context.expect_next_kind(
            stream, TokenKind::Identifier);
        let lessthan = ellipsis_context.expect_next_kind(
            stream, TokenKind::LessThan);
        let size = match bracket_context.peek_kind(stream) {
            Some(TokenKind::Ellipsis) => ArraySize::Implicit(
                bracket_context.next_leaf(stream)),
            _ => ArraySize::Defined(
                Expression::parse(&bracket_context, stream, file_info)),
        };
        let rbracket = outer.expect_next_kind(stream, TokenKind::RBracket);
        dimensions.push((lbracket, indexvar, lessthan, size, rbracket));
    }
    (name, dimensions)
}

fn parse_composite_object_remain(context: &ParseContext,
                                 stream: &mut FileParser<'_>, file_info: &FileInfo)
                                 -> (Option<(LeafToken, Instantiation)>,
                                     Option<Expression>)
{
    let mut outer = context.enter_context(doesnt_understand_tokens);
    let instantiation = match outer.peek_kind(stream) {
        Some(TokenKind::Is) => {
            let is = outer.next_leaf(stream);
            let instantiation = parse_instantiation(&outer, stream, file_info);
            Some((is, instantiation))
        },
        _ => None,
    };
    let documentation = match outer.peek_kind(stream) {
        Some(TokenKind::SemiColon) | Some(TokenKind::LBrace) | None => None,
        _ => Some(Expression::parse(&outer, stream, file_info)),
    };
    (instantiation, documentation)
}

impl Parse<DMLObjectContent> for CompositeObjectContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        fn understands_lbrace_or_semi(token: TokenKind) -> bool {
            token == TokenKind::LBrace || token == TokenKind::SemiColon
        }
        let outer = context.enter_context(doesnt_understand_tokens);
        let mut pre_statements_context = outer.enter_context(
            understands_lbrace_or_semi);
        // Guaranteed by parser
        let object_kind = pre_statements_context.peek_kind(stream).unwrap();
        let kind = pre_statements_context.next_leaf(stream);
        let (name, dimensions) = parse_composite_object_name(
            &pre_statements_context, stream, file_info);
        let (instantiation, documentation) =
            parse_composite_object_remain(&pre_statements_context, stream, file_info);
        let statements = ObjectStatements::parse(&outer, stream, file_info);
        let content = CompositeObjectContent {
            kind, name, dimensions,
            instantiation, documentation, statements,
        };
        match object_kind {
            TokenKind::Attribute => DMLObjectContent::Attribute(content),
            TokenKind::Bank => DMLObjectContent::Bank(content),
            TokenKind::Connect => DMLObjectContent::Connection(content),
            TokenKind::Event => DMLObjectContent::Event(content),
            TokenKind::Group => DMLObjectContent::Group(content),
            TokenKind::Implement => DMLObjectContent::Implement(content),
            TokenKind::Interface => DMLObjectContent::Interface(content),
            TokenKind::Port => DMLObjectContent::Port(content),
            TokenKind::Subdevice => DMLObjectContent::Subdevice(content),
            _ => panic!("Internal Parser Error: \
                         Unexpected composite object kind {:?}", object_kind)
        }.into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateContent {
    pub template: LeafToken,
    pub name: LeafToken,
    pub instantiation: Option<(LeafToken, Instantiation)>,
    pub statements: ObjectStatements,
}

impl TreeElement for TemplateContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.template.range(),
                       self.statements.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.template,
                     &self.name,
                     &self.instantiation,
                     &self.statements)
    }
    fn references<'a>(&self, _accumulator: &mut Vec<Reference>, _file: FileSpec<'a>) {}
}

impl Parse<DMLObjectContent> for TemplateContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        fn understands_lbrace_or_semi(token: TokenKind) -> bool {
            token == TokenKind::LBrace || token == TokenKind::SemiColon
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        let mut pre_statements_context = outer.enter_context(
            understands_lbrace_or_semi);
        // Guaranteed by parser
        let template = pre_statements_context.next_leaf(stream);
        let name = pre_statements_context.expect_next_filter(
            stream, objident_filter, "template name");
        let instantiation = match pre_statements_context.peek_kind(stream) {
            Some(TokenKind::Is) => {
                let is = outer.next_leaf(stream);
                let instantiation = parse_instantiation(
                    &pre_statements_context, stream, file_info);
                Some((is, instantiation))
            },
            _ => None,
        };
        let statements = ObjectStatements::parse(&outer, stream, file_info);
        DMLObjectContent::Template(TemplateContent {
            template, name, instantiation, statements,
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegisterContent {
    pub obj: CompositeObjectContent,
    pub size: Option<(LeafToken, Expression)>,
    pub offset: Option<(LeafToken, Expression)>,
}

impl TreeElement for RegisterContent {
    fn range(&self) -> ZeroRange {
        self.obj.range()
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.obj, &self.size, &self.offset)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        if let Some(doc) = &self.obj.documentation {
            ensure_string_concatenation(doc)
        } else {
            vec![]
        }
    }
    fn references<'a>(&self, _accumulator: &mut Vec<Reference>, _file: FileSpec<'a>) {}
}

impl Parse<DMLObjectContent> for RegisterContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        fn understands_lbrace_or_semi(token: TokenKind) -> bool {
            token == TokenKind::LBrace || token == TokenKind::SemiColon
        }
        let outer = context.enter_context(understands_lbrace_or_semi);
        let mut pre_statements_context = outer.enter_context(
            doesnt_understand_tokens);
        let kind = pre_statements_context.expect_next_kind(
            stream, TokenKind::Register);
        let (name, dimensions) = parse_composite_object_name(
            &pre_statements_context, stream, file_info);
        let size = match pre_statements_context.peek_kind(stream) {
            Some(TokenKind::Size) => {
                let sizetok = pre_statements_context.next_leaf(stream);
                let sizeval = Expression::parse(
                    &pre_statements_context, stream, file_info);
                Some((sizetok, sizeval))
            },
            _ => None
        };
        let offset = match pre_statements_context.peek_kind(stream) {
            Some(TokenKind::At) => {
                let at = pre_statements_context.next_leaf(stream);
                let addr = Expression::parse(&pre_statements_context, stream, file_info);
                Some((at, addr))
            },
            _ => None
        };
        let (instantiation, documentation) =
            parse_composite_object_remain(&pre_statements_context, stream, file_info);
        let statements = ObjectStatements::parse(&outer, stream, file_info);
        let obj = CompositeObjectContent {
            kind, name, dimensions,
            instantiation, documentation, statements,
        };
        DMLObjectContent::Register(RegisterContent {
            obj, size, offset
        }).into()
    }
}

pub type BitRangeContent = (LeafToken, LeafToken, Expression,
                            Option<(LeafToken, Expression)>, LeafToken);

#[derive(Debug, Clone, PartialEq)]
pub struct FieldContent {
    pub obj: CompositeObjectContent,
    pub bitrange: Option<BitRangeContent>,
}

impl TreeElement for FieldContent {
    fn range(&self) -> ZeroRange {
        self.obj.range()
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.obj, &self.bitrange)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        if let Some(doc) = &self.obj.documentation {
            ensure_string_concatenation(doc)
        } else {
            vec![]
        }
    }
    fn references<'a>(&self, _accumulator: &mut Vec<Reference>, _file: FileSpec<'a>) {}
}

impl Parse<DMLObjectContent> for FieldContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        fn understands_lbrace_or_semi(token: TokenKind) -> bool {
            token == TokenKind::LBrace || token == TokenKind::SemiColon
        }
        fn understands_rbracket(token: TokenKind) -> bool {
            token == TokenKind::RBracket
        }
        let outer = context.enter_context(understands_lbrace_or_semi);
        let mut pre_statements_context = outer.enter_context(
            doesnt_understand_tokens);
        let kind = pre_statements_context.expect_next_kind(
            stream, TokenKind::Field);
        let (name, dimensions) = parse_composite_object_name(
            &pre_statements_context, stream, file_info);
        let bitrange = match pre_statements_context.peek_kind(stream) {
            Some(TokenKind::At) => {
                let at = pre_statements_context.next_leaf(stream);
                let lbracket = pre_statements_context.expect_next_kind(
                    stream, TokenKind::LBracket);
                let mut bracket_context = pre_statements_context.enter_context(
                    understands_rbracket);
                let lside = Expression::parse(&pre_statements_context, stream, file_info);
                let rside = match bracket_context.peek_kind(stream) {
                    Some(TokenKind::Colon) => {
                        let colon = bracket_context.next_leaf(stream);
                        let rside_expr = Expression::parse(
                            &bracket_context, stream, file_info);
                        Some((colon, rside_expr))
                    },
                    _ => None,
                };
                let rbracket = bracket_context.expect_next_kind(
                    stream, TokenKind::RBracket);
                Some((at, lbracket, lside, rside, rbracket))
            },
            _ => None
        };
        let (instantiation, documentation) =
            parse_composite_object_remain(&pre_statements_context, stream, file_info);
        let statements = ObjectStatements::parse(&outer, stream, file_info);
        let obj = CompositeObjectContent {
            kind, name, dimensions,
            instantiation, documentation, statements,
        };
        DMLObjectContent::Field(FieldContent {
            obj, bitrange
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DMLVersionContent {
    pub dml: LeafToken,
    pub version: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for DMLVersionContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.dml.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.dml, &self.version, &self.semi)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        match self.version.read_leaf(file) {
            Some(ver) => {
                if ver != "1.4" {
                    vec![LocalDMLError {
                        range: self.version.range(),
                        description:
                        "The language server only supports DML 1.4 files"
                            .to_string(),
                    }]
                } else {
                    vec![]
                }
            },
            None => vec![]
        }
    }
}

fn dmlversion_parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo)
                   -> DMLVersionContent {
    let (mut outer, mut inner) = object_contexts(context);
    let dml = inner.expect_next_kind(stream, TokenKind::DML);
    // Version X.Y is parsed as a floatconstant, work untill we want sub-sub
    // versions
    let version = inner.expect_next_kind_custom(
        stream, TokenKind::FloatConstant, "version number (1.4)");
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    DMLVersionContent {
        dml, version, semi
    }
}

impl Parse<DMLObjectContent> for DMLVersionContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        DMLObjectContent::DMLVersion(dmlversion_parse(context, stream, file_info)).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportContent {
    pub import: LeafToken,
    pub file: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for ImportContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.import.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.import, &self.file, &self.semi)
    }
}

impl Parse<DMLObjectContent> for ImportContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo)
             -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let import = inner.expect_next_kind(stream, TokenKind::Import);
        let file = inner.expect_next_kind(stream, TokenKind::StringConstant);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Import(ImportContent {
            import, file, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BitorderContent {
    pub bitorder: LeafToken,
    pub bitorderdesc: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for BitorderContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.bitorder.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.bitorder, &self.bitorderdesc, &self.semi)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        match self.bitorderdesc.read_leaf(file) {
            Some(desc) => if desc != "le" && desc != "be" {
                vec![LocalDMLError {
                    range: self.bitorderdesc.range(),
                    description: "bitorder must be 'le' or 'be'".to_string(),
                }]
            } else { vec![] },
            None => vec![]
        }
    }
}

fn bitorder_parse(context: &ParseContext,
                  stream: &mut FileParser<'_>,
                  _file_info: &FileInfo)
                  -> BitorderContent {
    let (mut outer, mut inner) = object_contexts(context);
    let bitorder = inner.expect_next_kind(stream, TokenKind::Bitorder);
    let bitorderdesc = inner.expect_next_kind(
        stream, TokenKind::Identifier);
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    BitorderContent {
        bitorder, bitorderdesc, semi
    }
}

impl Parse<DMLObjectContent> for BitorderContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> DMLObject {
        DMLObjectContent::Bitorder(
            bitorder_parse(context, stream, file_info)).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HashElse {
    HashIf(DMLObject),
    Statements(ObjectStatements),
}

impl TreeElement for HashElse {
    fn range(&self) -> ZeroRange {
        match self {
            HashElse::HashIf(hif) => hif.range(),
            HashElse::Statements(stmnts) => stmnts.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            HashElse::HashIf(hif) => create_subs!(hif),
            HashElse::Statements(stmnts) => create_subs!(stmnts),
        }
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        match self {
            HashElse::HashIf(heif) =>
                heif.with_content(|hif|hif.post_parse_sanity(file), vec![]),
            HashElse::Statements(hestmnts) =>
                hestmnts.with_content(
                    |stmnts|stmnts.check_template_only_stmnts(file), vec![]),
        }
    }
}

// This name does not conflict with the expression, since that is stored as a
// TertiaryExpressionContent
#[derive(Debug, Clone, PartialEq)]
pub struct HashIfContent {
    pub iftok: LeafToken,
    pub lparen: LeafToken,
    pub cond: Expression,
    pub rparen: LeafToken,
    pub truestatements: ObjectStatements,
    pub elsebranch: Option<(LeafToken, HashElse)>,
}

impl TreeElement for HashIfContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.iftok.range(),
                       match &self.elsebranch {
                           Some((_, statements)) => statements.range(),
                           None => self.truestatements.range()
                       })
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.iftok,
                     &self.lparen,
                     &self.cond,
                     &self.rparen,
                     &self.truestatements,
                     &self.elsebranch)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        let mut errors = self.truestatements.with_content(
            |stmnts|stmnts.check_template_only_stmnts(file), vec![]);
        if let Some(helse) = &self.elsebranch {
            errors.append(&mut helse.post_parse_sanity(file));
        }
        errors
    }
}

impl Parse<DMLObjectContent> for HashIfContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> DMLObject {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        let iftok = outer.expect_next_kind(stream, TokenKind::HashIf);
        let lparen = outer.expect_next_kind(stream, TokenKind::LParen);
        let paren_context = outer.enter_context(understands_rparen);
        let cond = Expression::parse(&paren_context, stream, file_info);
        let rparen = outer.expect_next_kind(stream, TokenKind::RParen);
        let truestatements = ObjectStatements::parse(&outer, stream,
                                                     file_info);
        let elsebranch = match outer.peek_kind(stream) {
            Some(TokenKind::HashElse) => {
                let hashelsetok = outer.next_leaf(stream);
                let hashelse = match outer.peek_kind(stream) {
                    Some(TokenKind::HashIf) =>
                        HashElse::HashIf(HashIfContent::parse(&outer, stream,
                                                              file_info)),
                    _ => HashElse::Statements(
                        ObjectStatements::parse(&outer, stream, file_info)),
                };
                Some((hashelsetok, hashelse))
            },
            _ => None,
        };
        DMLObjectContent::HashIf(HashIfContent {
            iftok, lparen, cond, rparen, truestatements, elsebranch
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportContent {
    pub export: LeafToken,
    pub target: Expression,
    pub astok: LeafToken,
    pub name: Expression,
    pub semi: LeafToken,
}

impl TreeElement for ExportContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.export.range(),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.export, &self.target, &self.astok, &self.name, &self.semi)
    }
}

impl Parse<DMLObjectContent> for ExportContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let export = inner.expect_next_kind(stream, TokenKind::Export);
        let target = Expression::parse(&inner, stream, file_info);
        let astok = inner.expect_next_kind(stream, TokenKind::As);
        let name = Expression::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Export(ExportContent {
            export, target, astok, name, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CBlockContent {
    pub kind: LeafToken,
    // Currently, C headers/footers are opaque boxes
    pub cblock: LeafToken,
}

impl TreeElement for CBlockContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.kind.range(),
                       self.cblock.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.kind, &self.cblock)
    }
}

impl Parse<DMLObjectContent> for CBlockContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             _file_info: &FileInfo)
             -> DMLObject {
        let mut outer = context.enter_context(doesnt_understand_tokens);
        // guaranteed by parser
        let token_kind = outer.peek_kind(stream).unwrap();
        let kind = outer.next_leaf(stream);
        let cblock = outer.expect_next_kind(stream, TokenKind::CBlock);
        let content = CBlockContent {
            kind, cblock
        };
        match token_kind {
            TokenKind::Header => DMLObjectContent::Header(content),
            TokenKind::Footer => DMLObjectContent::Footer(content),
            _ => panic!("Internal Parser Error: \
                         Unexpected CBlock kind")
        }.into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HookContent {
    pub shared: Option<LeafToken>,
    pub hook: LeafToken,
    pub lparen: LeafToken,
    pub arg_types: CDeclList,
    pub rparen: LeafToken,
    pub ident: LeafToken,
    // TODO: Hook arrays are internally-experimental atm
    pub semi: LeafToken,
}

impl TreeElement for HookContent {
    fn range(&self) -> ZeroRange {
        Range::combine(Range::combine(self.shared.range(),
                                      self.hook.range()),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.shared,
                     &self.hook,
                     &self.lparen,
                     &self.arg_types,
                     &self.rparen,
                     &self.ident,
                     &self.semi)
    }
}

fn parse_hook(shared: Option<LeafToken>,
              context: &ParseContext,
              stream: &mut FileParser<'_>,
              file_info: &FileInfo)
              -> DMLObject {
    let mut outer = context.enter_context(doesnt_understand_tokens);
    let hook = outer.next_leaf(stream);
    let lparen = outer.expect_next_kind(stream, TokenKind::LParen);
    let arg_types = CDeclList::parse(context, stream, file_info);
        let rparen = outer.expect_next_kind(stream, TokenKind::RParen);
    let ident = outer.expect_next_kind(stream, TokenKind::Identifier);
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    DMLObjectContent::Hook(HookContent {
        shared,
        hook,
        lparen,
        arg_types,
        rparen,
        ident,
        semi,
    }).into()
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoggroupContent {
    pub loggroup: LeafToken,
    pub ident: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for LoggroupContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.loggroup.range(),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.loggroup, &self.ident, &self.semi)
    }
}

impl Parse<DMLObjectContent> for LoggroupContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, _file_info: &FileInfo)
             -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let loggroup = inner.expect_next_kind(stream, TokenKind::Loggroup);
        let ident = inner.expect_next_kind(stream, TokenKind::Identifier);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Loggroup(LoggroupContent {
            loggroup, ident, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedefContent {
    pub externtok: Option<LeafToken>,
    pub typedef: LeafToken,
    pub decl: CDecl,
    pub semi: LeafToken,
}

impl TreeElement for TypedefContent {
    fn range(&self) -> ZeroRange {
        Range::combine(
            match &self.externtok {
                Some(tok) => tok.range(),
                None => self.typedef.range(),
            },
            self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.externtok, &self.typedef, &self.decl, &self.semi)
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        self.decl.ensure_named()
    }
}

// Special case, to distinguish between extern cdecl and extern typedef, the
// grammar needs to parse the extern token one step above
impl TypedefContent {
    fn parse(externtok: Option<LeafToken>,
             context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let typedef = inner.expect_next_kind(stream, TokenKind::Typedef);
        let decl = CDecl::parse(&inner, stream, file_info);
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::Typedef(TypedefContent {
            externtok, typedef, decl, semi
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InEachSpec {
    One(LeafToken),
    List(LeafToken, Vec<(LeafToken, Option<LeafToken>)>, LeafToken)
}

impl TreeElement for InEachSpec {
    fn range(&self) -> ZeroRange {
        match self {
            Self::One(tok) => tok.range(),
            Self::List(left, _, right) => Range::combine(
                left.range(), right.range()),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::One(tok) => create_subs!(tok),
            Self::List(left, vect, right) =>
                create_subs!(left, vect, right)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InEachContent {
    pub intok: LeafToken,
    pub each: LeafToken,
    pub spec: InEachSpec,
    pub statements: ObjectStatements,
}

impl TreeElement for InEachContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.intok.range(), self.statements.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.intok, &self.each, &self.spec, &self.statements)
    }
    fn post_parse_sanity(&self, file: &TextFile) -> Vec<LocalDMLError> {
        fn object_statement_empty(content: &ObjectStatementsContent) ->
            Vec<LocalDMLError> {
                match content {
                    ObjectStatementsContent::Empty(tok) => vec![LocalDMLError {
                        range: tok.range(),
                        description:
                        "'in each' declaration needs a compound declaration".to_string(),
                    }],
                    _ => vec![],
                }
            }
        let mut errors = self.statements.with_content(
            object_statement_empty, vec![]);
        errors.append(&mut self.statements.with_content(
            |stmnts|stmnts.check_template_only_stmnts(file), vec![]));
        errors
    }
    fn references<'a>(&self, _accumulator: &mut Vec<Reference>, _file: FileSpec<'a>) {}
}

impl Parse<DMLObjectContent> for InEachContent {
    fn parse(context: &ParseContext, stream: &mut FileParser<'_>, file_info: &FileInfo)
             -> DMLObject {
        fn understands_rparen(token: TokenKind) -> bool {
            token == TokenKind::RParen
        }
        let mut outer = context.enter_context(doesnt_understand_tokens);
        let intok = outer.expect_next_kind(stream, TokenKind::In);
        let each = outer.expect_next_kind(stream, TokenKind::Each);
        let spec = match outer.peek_kind(stream) {
            Some(TokenKind::LParen) => {
                let lparen = outer.next_leaf(stream);
                let mut paren_context = outer.enter_context(understands_rparen);
                let mut spec = vec![];
                // Require at least one
                let first_objident = paren_context.expect_next_filter(
                    stream, objident_filter, "template name");
                let mut end = true;
                let first_comma = match outer.peek_kind(stream) {
                    Some(TokenKind::Comma) => {
                        end = false;
                        Some(outer.next_leaf(stream))
                    },
                    _ => None,
                };
                spec.push((first_objident, first_comma));
                while !end {
                    let objident = paren_context.expect_next_filter(
                        stream, objident_filter, "template name");
                    let comma = match paren_context.peek_kind(stream) {
                        Some(TokenKind::Comma) =>
                            Some(paren_context.next_leaf(stream)),
                        _ => {
                            end = true;
                            None
                        },
                    };
                    spec.push((objident, comma));
                }
                let rparen = outer.expect_next_kind(stream, TokenKind::RParen);
                InEachSpec::List(lparen, spec, rparen)
            },
            _ => InEachSpec::One(outer.expect_next_filter(
                stream, objident_filter, "template name"))
        };
        let statements = ObjectStatements::parse(&outer, stream, file_info);
        DMLObjectContent::InEach(InEachContent {
            intok, each, spec, statements
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeviceContent {
    pub device: LeafToken,
    pub name: LeafToken,
    pub semi: LeafToken,
}

impl TreeElement for DeviceContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.device.range(),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.device, &self.name, &self.semi)
    }
    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        if let Some(refr) = Reference::global_from_token(
            &self.device, file, ReferenceKind::Template) {
            accumulator.push(refr);
        }
    }
}

fn device_parse(context: &ParseContext,
                stream: &mut FileParser<'_>,
                _file_info: &FileInfo) -> DeviceContent {
    let (mut outer, mut inner) = object_contexts(context);
    let device = inner.expect_next_kind(stream, TokenKind::Device);
    let name = inner.expect_next_kind(stream, TokenKind::Identifier);
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    DeviceContent {
        device, name, semi
    }
}

impl Parse<DMLObjectContent> for DeviceContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> DMLObject {
        DMLObjectContent::Device(device_parse(context,
                                              stream,
                                              file_info)).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorObjectContent {
    pub error: LeafToken,
    pub message: Option<Expression>,
    pub semi: LeafToken,
}

impl TreeElement for ErrorObjectContent {
    fn range(&self) -> ZeroRange {
        Range::combine(self.error.range(),
                       self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.error, &self.message, &self.semi)
    }
}

impl Parse<DMLObjectContent> for ErrorObjectContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo)
             -> DMLObject {
        let (mut outer, mut inner) = object_contexts(context);
        let error = inner.expect_next_kind(stream, TokenKind::Error);
        let message = match inner.peek_kind(stream) {
            Some(TokenKind::SemiColon) | None => None,
            _ => Some(Expression::parse(&inner, stream, file_info)),
        };
        let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
        DMLObjectContent::ErrorObject(ErrorObjectContent {
            error, message, semi
        }).into()
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum DMLObjectContent {
    Attribute(CompositeObjectContent),
    Bank(CompositeObjectContent),
    Bitorder(BitorderContent),
    Connection(CompositeObjectContent),
    Constant(ConstantContent),
    Device(DeviceContent),
    DMLVersion(DMLVersionContent),
    ErrorObject(ErrorObjectContent),
    Export(ExportContent),
    Extern(VariableContent),
    Event(CompositeObjectContent),
    Field(FieldContent),
    Footer(CBlockContent),
    Group(CompositeObjectContent),
    HashIf(HashIfContent),
    Header(CBlockContent),
    Hook(HookContent),
    Implement(CompositeObjectContent),
    Import(ImportContent),
    InEach(InEachContent),
    Instantiation(InstantiationContent),
    Interface(CompositeObjectContent),
    Loggroup(LoggroupContent),
    Method(MethodContent),
    Parameter(ParameterContent),
    Port(CompositeObjectContent),
    Provisional(ProvisionalContent),
    Register(RegisterContent),
    Saved(VariableContent),
    Session(VariableContent),
    Subdevice(CompositeObjectContent),
    Template(TemplateContent),
    Typedef(TypedefContent),
}

impl TreeElement for DMLObjectContent {
    fn range(&self) -> ZeroRange {
        match self {
            Self::Attribute(content) => content.range(),
            Self::Bank(content) => content.range(),
            Self::Bitorder(content) => content.range(),
            Self::Connection(content) => content.range(),
            Self::Constant(content) => content.range(),
            Self::Device(content) => content.range(),
            Self::DMLVersion(content) => content.range(),
            Self::ErrorObject(content) => content.range(),
            Self::Export(content) => content.range(),
            Self::Extern(content) => content.range(),
            Self::Typedef(content) => content.range(),
            Self::Event(content) => content.range(),
            Self::Field(content) => content.range(),
            Self::Footer(content) => content.range(),
            Self::Group(content) => content.range(),
            Self::HashIf(content) => content.range(),
            Self::Header(content) => content.range(),
            Self::Hook(content) => content.range(),
            Self::Implement(content) => content.range(),
            Self::Import(content) => content.range(),
            Self::InEach(content) => content.range(),
            Self::Instantiation(content) => content.range(),
            Self::Interface(content) => content.range(),
            Self::Loggroup(content) => content.range(),
            Self::Method(content) => content.range(),
            Self::Parameter(content) => content.range(),
            Self::Port(content) => content.range(),
            Self::Provisional(content) => content.range(),
            Self::Register(content) => content.range(),
            Self::Saved(content) => content.range(),
            Self::Session(content) => content.range(),
            Self::Subdevice(content) => content.range(),
            Self::Template(content) => content.range(),
        }
    }
    fn subs(&self) -> TreeElements<'_> {
        match self {
            Self::Attribute(content) => create_subs![content],
            Self::Bank(content) => create_subs![content],
            Self::Bitorder(content) => create_subs![content],
            Self::Connection(content) => create_subs![content],
            Self::Constant(content) => create_subs![content],
            Self::Device(content) => create_subs![content],
            Self::DMLVersion(content) => create_subs![content],
            Self::ErrorObject(content) => create_subs![content],
            Self::Export(content) => create_subs![content],
            Self::Extern(content) => create_subs![content],
            Self::Typedef(content) => create_subs![content],
            Self::Event(content) => create_subs![content],
            Self::Field(content) => create_subs![content],
            Self::Footer(content) => create_subs![content],
            Self::Group(content) => create_subs![content],
            Self::HashIf(content) => create_subs![content],
            Self::Header(content) => create_subs![content],
            Self::Hook(content) =>create_subs![content],
            Self::Implement(content) => create_subs![content],
            Self::Import(content) => create_subs![content],
            Self::InEach(content) => create_subs![content],
            Self::Instantiation(content) => create_subs![content],
            Self::Interface(content) => create_subs![content],
            Self::Loggroup(content) => create_subs![content],
            Self::Method(content) => create_subs![content],
            Self::Parameter(content) => create_subs![content],
            Self::Port(content) => create_subs![content],
            Self::Provisional(content) => create_subs![content],
            Self::Register(content) => create_subs![content],
            Self::Saved(content) => create_subs![content],
            Self::Session(content) => create_subs![content],
            Self::Subdevice(content) => create_subs![content],
            Self::Template(content) => create_subs![content],
        }
    }
}

pub type DMLObject = AstObject<DMLObjectContent>;

pub fn dmlobject_first_token_matcher(token: TokenKind) -> bool {
    matches!(token,
             TokenKind::Attribute | TokenKind::Bank | TokenKind::Bitorder |
             TokenKind::Connect | TokenKind::Constant | TokenKind::Device |
             TokenKind::DML | TokenKind::Error | TokenKind::Export |
             TokenKind::Extern | TokenKind::Typedef | TokenKind::Event |
             TokenKind::Field | TokenKind::Footer | TokenKind::Group |
             TokenKind::HashIf | TokenKind::Header | TokenKind::Hook |
             TokenKind::Implement |
             TokenKind::Import | TokenKind::In | TokenKind::Independent |
             TokenKind::Inline | TokenKind::Is | TokenKind::Interface |
             TokenKind::Loggroup | TokenKind::Memoized | TokenKind::Method |
             TokenKind::Param | TokenKind::Port | TokenKind::Register |
             TokenKind::Saved | TokenKind::Session | TokenKind::Subdevice |
             TokenKind::Shared | TokenKind::Startup | TokenKind::Template |
             TokenKind::Provisional)
}

impl Parse<DMLObjectContent> for DMLObject {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> DMLObject {
        let mut outer = context.enter_context(doesnt_understand_tokens);
        match outer.expect_peek_filter(
            stream, dmlobject_first_token_matcher, "DML object") {
            LeafToken::Actual(token) =>
                match token.kind {
                    TokenKind::Attribute =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Bank =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Bitorder =>
                        BitorderContent::parse(&outer, stream, file_info),
                    TokenKind::Connect =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Constant =>
                        ConstantContent::parse(&outer, stream, file_info),
                    TokenKind::Device =>
                        DeviceContent::parse(&outer, stream, file_info),
                    TokenKind::DML =>
                        DMLVersionContent::parse(&outer, stream, file_info),
                    TokenKind::Error =>
                        ErrorObjectContent::parse(&outer, stream, file_info),
                    TokenKind::Export =>
                        ExportContent::parse(&outer, stream, file_info),
                    TokenKind::Extern => {
                        let externtok = outer.next_leaf(stream);
                        match outer.peek_kind(stream) {
                            Some(TokenKind::Typedef) => TypedefContent::parse(
                                Some(externtok), &outer, stream, file_info),
                            _ => parse_extern(externtok, &outer, stream,
                                              file_info),
                        }
                    },
                    TokenKind::Typedef =>
                        TypedefContent::parse(None, &outer, stream,
                                              file_info),
                    TokenKind::Event =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Field =>
                        FieldContent::parse(&outer, stream, file_info),
                    TokenKind::Footer =>
                        CBlockContent::parse(&outer, stream, file_info),
                    TokenKind::Group =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::HashIf =>
                        HashIfContent::parse(&outer, stream, file_info),
                    TokenKind::Header =>
                        CBlockContent::parse(&outer, stream, file_info),
                    TokenKind::Hook =>
                        parse_hook(None, &outer, stream, file_info),
                    TokenKind::Implement =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Import =>
                        ImportContent::parse(&outer, stream, file_info),
                    TokenKind::In =>
                        InEachContent::parse(&outer, stream, file_info),
                    TokenKind::Interface =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Is =>
                        InstantiationContent::parse(&outer, stream,
                                                    file_info),
                    TokenKind::Loggroup =>
                        LoggroupContent::parse(&outer, stream, file_info),
                    TokenKind::Shared => {
                        let sharedtok = outer.next_leaf(stream);
                        match outer.peek_kind(stream) {
                            Some(TokenKind::Hook) => parse_hook(
                                Some(sharedtok), &outer, stream, file_info),
                            _ => parse_method(
                                Some(sharedtok), &outer, stream, file_info),
                        }
                    },
                    TokenKind::Inline =>
                        parse_method(Some(outer.next_leaf(stream)),
                                     &outer,
                                     stream,
                                     file_info),
                    TokenKind::Method | TokenKind::Independent
                        | TokenKind::Startup | TokenKind::Memoized =>
                        parse_method(None, &outer, stream, file_info),
                    TokenKind::Param =>
                        ParameterContent::parse(&outer, stream, file_info),
                    TokenKind::Port =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Provisional =>
                        ProvisionalContent::parse(&outer, stream, file_info),
                    TokenKind::Register =>
                        RegisterContent::parse(&outer, stream, file_info),
                    TokenKind::Saved =>
                        VariableContent::parse(&outer, stream, file_info),
                    TokenKind::Session =>
                        VariableContent::parse(&outer, stream, file_info),
                    TokenKind::Subdevice =>
                        CompositeObjectContent::parse(&outer, stream,
                                                      file_info),
                    TokenKind::Template =>
                        TemplateContent::parse(&outer, stream, file_info),
                    _ => unreachable!(),
                },
            LeafToken::Missing(missing) => missing.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProvisionalContent {
    pub provisional: LeafToken,
    pub activations: Vec<(LeafToken, Option<LeafToken>)>,
    pub semi: LeafToken
}

impl TreeElement for ProvisionalContent {
    fn range(&self) -> ZeroRange {
        ZeroRange::combine(self.provisional.range(), self.semi.range())
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.provisional, &self.activations, &self.semi)
    }
}

impl Parse<DMLObjectContent> for ProvisionalContent {
    fn parse(context: &ParseContext,
             stream: &mut FileParser<'_>,
             file_info: &FileInfo) -> DMLObject {
        DMLObjectContent::Provisional(
            provisionals_parse(context, stream, file_info)).into()
    }
}

fn provisionals_parse(context: &ParseContext,
                      stream: &mut FileParser<'_>,
                      _file_info: &FileInfo)
                      -> ProvisionalContent {
    let (mut outer, mut inner) = object_contexts(context);
    let provisional = inner.expect_next_kind(stream, TokenKind::Provisional);
    let mut cont = true;
    let mut activations = vec![];
    while cont {
        cont = false;
        let activate = inner.expect_next_kind(stream, TokenKind::Identifier);
        let comma = if inner.peek_kind(stream) == Some(TokenKind::Comma) {
            cont = true;
            Some(inner.next_leaf(stream))
        } else {
            None
        };
        activations.push((activate, comma));
    }
    let semi = outer.expect_next_kind(stream, TokenKind::SemiColon);
    ProvisionalContent {
        provisional,
        activations,
        semi,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopAst {
    pub version : DMLVersionContent,
    pub provisionals : Option<ProvisionalContent>,
    pub device: Option<DeviceContent>,
    pub bitorder: Option<BitorderContent>,
    pub declarations: Vec<DMLObject>,
}

impl TreeElement for TopAst {
    fn range(&self) -> ZeroRange {
        let second_range = if !self.declarations.is_empty() {
            self.declarations.range()
        } else if let Some(order) = self.bitorder.as_ref() {
            order.range()
        } else if let Some(device) = self.device.as_ref() {
            device.range()
        } else {
            self.version.range()
        };
        Range::combine(self.version.range(),
                       second_range)
    }
    fn subs(&self) -> TreeElements<'_> {
        create_subs!(&self.version, &self.provisionals, &self.device,
                     &self.bitorder, &self.declarations)
    }
}

pub fn parse_toplevel(stream: &mut FileParser<'_>,
                      file_info: &mut FileInfo,
                      file: FileSpec<'_>)
                      -> TopAst {
    let mut top_context = ParseContext::new_context(
        dmlobject_first_token_matcher);
    let version = dmlversion_parse(&top_context, stream, file_info);
    let provisionals = if top_context.peek_kind(stream) == Some(TokenKind::Provisional) {
        Some(provisionals_parse(&top_context, stream, file_info))
    } else {
        None
    };

    if let Some(provisionals) = provisionals.as_ref() {
        file_info.provisionals.add_provisionals(
            &provisionals.activations, file);
    }

    let device = if top_context.peek_kind(stream) == Some(TokenKind::Device) {
        Some(device_parse(&top_context, stream, file_info))
    } else {
        None
    };
    let bitorder = if top_context.peek_kind(stream) == Some(TokenKind::Bitorder) {
        Some(bitorder_parse(&top_context, stream, file_info))
    } else {
        None
    };

    let mut declarations = vec![];
    while top_context.peek(stream).is_some() {
        declarations.push(DMLObject::parse(&top_context, stream, file_info));
    }
    TopAst {
        version, provisionals, device, bitorder, declarations
    }
}

#[allow(clippy::ptr_arg)]
pub fn post_parse_toplevel(toplevel: &TopAst,
                           file: &TextFile,
                           errors: &mut Vec<LocalDMLError>) {
    trace!("Doing post-parse sanity check on toplevel");
    for object in &toplevel.declarations {
        trace!("Checking toplevel restrictions on {:?}", object);
        let mut thing = object.with_content(
            |obj|check_dmlobject_kind(obj, file), vec![]);
        errors.append(&mut thing);
    }
    toplevel.post_parse_sanity_walk(errors, file);
}

// TODO: expand tests
#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    // discovered a particular case while running towards test cases
    #[test]
    fn test_saved_statement_with_initializer() {
        use crate::analysis::parsing::expression::*;
        use crate::analysis::parsing::misc::*;
        use crate::analysis::parsing::types::*;

        let kind = make_leaf(zero_range(0, 0, 0, 0),
                             zero_range(0, 0, 0, 5),
                             TokenKind::Saved);
        let cdecl = VarDecl::One(make_ast(zero_range(0, 0, 6, 13),
                             CDeclContent {
                                 consttok: None,
                                 base: make_ast(
                                     zero_range(0, 0, 6, 9),
                                     BaseTypeContent::Ident(
                                         make_leaf(zero_range(0, 0, 5, 6),
                                                   zero_range(0, 0, 6, 9),
                                                   TokenKind::Int))),
                                 modifiers: vec![],
                                 decl: TypeDecl { content:
                                                  Some(make_ast(
                                                      zero_range(0, 0, 10, 13),
                                  TypeDeclContent::Ident(
                                      make_leaf(zero_range(0, 0, 9, 10),
                                                zero_range(0, 0, 10, 13),
                                                TokenKind::Identifier))))}
                             }));
        let i = make_ast(
            zero_range(0, 0, 17, 18),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 17, 17),
                zero_range(0, 0, 17, 18),
                TokenKind::Identifier
            )));
        let one = make_ast(
            zero_range(0, 0, 19, 20),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 19, 19),
                zero_range(0, 0, 19, 20),
                TokenKind::IntConstant
            )));
        let i_plus_one = make_ast(
            zero_range(0, 0, 17, 20),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: i,
                operation: make_leaf(
                    zero_range(0, 0, 18, 18),
                    zero_range(0, 0, 18, 19),
                    TokenKind::Plus
                ),
                right: one,
            }));
        let parens = make_ast(
            zero_range(0, 0, 16, 21),
            ExpressionContent::ParenExpression(ParenExpressionContent {
                lparen: make_leaf(
                    zero_range(0, 0, 15, 16),
                    zero_range(0, 0, 16, 17),
                    TokenKind::LParen
                ),
                expr: i_plus_one,
                rparen: make_leaf(
                    zero_range(0, 0, 20, 20),
                    zero_range(0, 0, 20, 21),
                    TokenKind::RParen
                ),
            }));
        let three = make_ast(
            zero_range(0, 0, 22, 23),
            ExpressionContent::Literal(make_leaf(
                zero_range(0, 0, 22, 22),
                zero_range(0, 0, 22, 23),
                TokenKind::IntConstant
            )));
        let parens_times_three = make_ast(
            zero_range(0, 0, 16, 23),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: parens,
                operation: make_leaf(
                    zero_range(0, 0, 21, 21),
                    zero_range(0, 0, 21, 22),
                    TokenKind::Multiply
                ),
                right: three,
            }));
        let j = make_ast(
            zero_range(0, 0, 24, 25),
            ExpressionContent::Identifier(make_leaf(
                zero_range(0, 0, 24, 24),
                zero_range(0, 0, 24, 25),
                TokenKind::Identifier
            )));
        let expr = make_ast(
            zero_range(0, 0, 16, 25),
            ExpressionContent::BinaryExpression(BinaryExpressionContent {
                left: parens_times_three,
                operation: make_leaf(
                    zero_range(0, 0, 23, 23),
                    zero_range(0, 0, 23, 24),
                    TokenKind::Plus
                ),
                right: j,
            }));
        let initializer = Some(
            (make_leaf(zero_range(0, 0, 13, 14),
                       zero_range(0, 0, 14, 15),
                       TokenKind::Assign),
             make_ast(zero_range(0, 0, 16, 25),
                      InitializerContent::Single(
                          make_ast(zero_range(0, 0, 16, 25),
                                   SingleInitializerContent::Expression(expr))))));
        let semi = make_leaf(zero_range(0, 0, 25, 25),
                             zero_range(0, 0, 25, 26),
                             TokenKind::SemiColon);
        let expected = make_ast(zero_range(0, 0, 0, 26),
                                DMLObjectContent::Saved(
                                    VariableContent {
                                        kind,
                                        cdecl,
                                        initializer,
                                        semi,
                                    }));
        test_ast_tree::<DMLObjectContent, DMLObject>(
            "saved int foo = (i+1)*3+j;",
            &expected,
            &vec![]);
    }

    #[test]
    fn test_empty_provisional() {
        let expected = make_ast(zero_range(0, 0, 0, 12),
                                DMLObjectContent::Provisional(
                                    ProvisionalContent {
                                        provisional: make_leaf(
                                            zero_range(0,0,0,0),
                                            zero_range(0,0,0,11),
                                            TokenKind::Provisional),
                                        activations: vec![
                                            (make_missing(
                                                zero_position(0,11),
                                                TokenKind::Identifier,
                                                Some(make_token(
                                                    zero_range(0,0,11,11),
                                                    zero_range(0,0,11,12),
                                                    TokenKind::SemiColon)
                                                )),
                                             None)],
                                        semi: make_leaf(
                                            zero_range(0,0,11,11),
                                            zero_range(0,0,11,12),
                                            TokenKind::SemiColon),
                                    }));
        test_ast_tree::<DMLObjectContent, DMLObject>(
            "provisional;",
            &expected,
            &vec![]);
    }
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::logging::{error};

use std::num::{IntErrorKind, ParseIntError};

use crate::analysis::parsing::expression;
use crate::analysis::parsing::misc;
use crate::analysis::parsing::expression::ExpressionContent;
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{LeafToken, ZeroRange,
                                     ZeroSpan, TreeElement};
use crate::analysis::FileSpec;
use crate::analysis::structure::types;
use crate::analysis::structure::types::DMLType;

use crate::analysis::{DeclarationSpan, LocalDMLError};
use crate::analysis::templating::objects::StructureKey;

// A DML value that corresponds directly to a rust value, with its associated
// position in source
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Value<T> {
    pub val: T,
    pub span: ZeroSpan,
}

impl <T> DeclarationSpan for Value<T> {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum MathOp {
    Plus, Minus, Multiply, Divide,
    Mod, BinOr, BinAnd, BinXor,
    LShift, RShift,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum CompOp {
    Equals, NotEquals,
    GreaterThan, LessThan,
    GreaterEquals, LessEquals,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum LogicOp {
    Or, And
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum MemberOp {
    Dot, Arrow,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum TertiaryOp {
    Cond, HashCond,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum UnaryOp {
    Not, PlusPlus, MinusMinus, Defined, Minus,
    BinNot, AddressOf, Dereference,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum BinOp {
    Math(MathOp),
    Comp(CompOp),
    Logic(LogicOp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BinaryExpression {
    pub span: ZeroSpan,
    pub left: Expression,
    pub right: Expression,
    pub operator: BinOp,
}

impl BinaryExpression {
    pub fn to_expression<'a>(content: &expression::BinaryExpressionContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        // This should be unable to fail, as the parser will not
        // construct a binary expression without seeing a valid
        // binary operation token first
        let operator = match content.operation.get_token().unwrap().kind {
            TokenKind::Plus => BinOp::Math(MathOp::Plus),
            TokenKind::Minus => BinOp::Math(MathOp::Minus),
            TokenKind::Multiply => BinOp::Math(MathOp::Multiply),
            TokenKind::Divide => BinOp::Math(MathOp::Divide),
            TokenKind::Mod => BinOp::Math(MathOp::Mod),
            TokenKind::BinOr => BinOp::Math(MathOp::BinOr),
            TokenKind::BinAnd => BinOp::Math(MathOp::BinAnd),
            TokenKind::BinXor => BinOp::Math(MathOp::BinXor),
            TokenKind::LShift => BinOp::Math(MathOp::LShift),
            TokenKind::RShift => BinOp::Math(MathOp::RShift),
            TokenKind::Equals => BinOp::Comp(CompOp::Equals),
            TokenKind::GreaterThan => BinOp::Comp(CompOp::GreaterThan),
            TokenKind::LessThan => BinOp::Comp(CompOp::LessThan),
            TokenKind::NotEquals => BinOp::Comp(CompOp::NotEquals),
            TokenKind::GEquals => BinOp::Comp(CompOp::GreaterEquals),
            TokenKind::LEquals => BinOp::Comp(CompOp::LessEquals),
            TokenKind::And => BinOp::Logic(LogicOp::And),
            TokenKind::Or => BinOp::Logic(LogicOp::Or),
            e => {
                error!("Unexpected binary operation token kind: {:?}", e);
                return None;
            }
        };
        let left = ExpressionKind::to_expression(
            &content.left, report, file)?;
        let right = ExpressionKind::to_expression(
            &content.right, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::BinaryExpression(BinaryExpression {
            span, left, right, operator,
        }).into()
    }
}

impl DeclarationSpan for BinaryExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MemberLiteral {
    pub span: ZeroSpan,
    pub left: Expression,
    pub right: DMLString,
    pub operator: MemberOp,
}

impl MemberLiteral {
    pub fn to_expression<'a>(content: &expression::MemberLiteralContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let left = ExpressionKind::to_expression(&content.left, report, file)?;
        // Guaranteed by parser
        let operator = match content.operation.get_token().unwrap().kind {
            TokenKind::Dot => MemberOp::Dot,
            TokenKind::Arrow => MemberOp::Arrow,
            e => {
                error!("Unexpected member operation token kind: {:?}", e);
                return None;
            }
        };
        let right = DMLString::from_token(&content.right, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::MemberLiteral(MemberLiteral {
            span,
            left,
            operator,
            right,
        }).into()
    }
}

impl DeclarationSpan for MemberLiteral {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TertiaryExpression {
    pub span: ZeroSpan,
    pub left: Expression,
    pub middle: Expression,
    pub right: Expression,
    pub operator: TertiaryOp,
}

impl DeclarationSpan for TertiaryExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl TertiaryExpression {
    pub fn to_expression<'a>(content: &expression::TertiaryExpressionContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        // This should be unable to fail, as the parser will not
        // construct a tertiary expression without seeing a valid
        // tertiary operation token first
        let operator = match content.left_operation.get_token().unwrap().kind {
            TokenKind::CondOp => TertiaryOp::HashCond,
            TokenKind::HashCondOp => TertiaryOp::HashCond,
            e => {
                error!("Unexpected tertiary operation token kind: {:?} at {:?}",
                       e, content.left_operation.range());
                return None;
            }
        };
        let left = ExpressionKind::to_expression(
            &content.left, report, file)?;
        let middle = ExpressionKind::to_expression(
            &content.middle, report, file)?;
        let right = ExpressionKind::to_expression(
            &content.right, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::TertiaryExpression(TertiaryExpression {
            span,
            left,
            middle,
            right,
            operator,
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UnaryExpression {
    pub span: ZeroSpan,
    pub operand: Expression,
    pub operator: UnaryOp,
}

impl DeclarationSpan for UnaryExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

pub fn to_unary_expression<'a>(operation: &LeafToken,
                               expression: &expression::Expression,
                               range: ZeroRange,
                               report: &mut Vec<LocalDMLError>,
                               file: FileSpec<'a>) -> Option<Expression> {
    // This should be unable to fail, as the parser will not
    // construct a unary expression without seeing a valid
    // unary operation token first (or
    let operator = match operation.get_token().unwrap().kind {
        TokenKind::PlusPlus => UnaryOp::PlusPlus,
        TokenKind::MinusMinus => UnaryOp::MinusMinus,
        TokenKind::Not => UnaryOp::Not,
        TokenKind::Defined => UnaryOp::Defined,
        TokenKind::Minus => UnaryOp::Minus,
        TokenKind::BinNot => UnaryOp::BinNot,
        TokenKind::BinAnd => UnaryOp::AddressOf,
        TokenKind::Multiply => UnaryOp::Dereference,
        e => {
            error!("Unexpected unary operation token kind: {:?} at {:?} in {:?}",
                   e, operation.range(), file.path);
            return None;
        }
    };
    let operand = ExpressionKind::to_expression(expression, report, file)?;
    let span = ZeroSpan::from_range(range, file.path);
    ExpressionKind::UnaryExpression(UnaryExpression {
        span,
        operand,
        operator,
    }).into()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SliceExpression {
    pub span: ZeroSpan,
    pub target: Expression,
    pub minind: Expression,
    pub maxind: Expression,
}

impl SliceExpression {
    pub fn to_expression<'a>(content: &expression::SliceContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let target = ExpressionKind::to_expression(&content.val, report, file)?;
        let minind = ExpressionKind::to_expression(
            &content.leftslice, report, file)?;
        let maxind = content.rightslice.as_ref()
            .and_then(|(_, e)|ExpressionKind::to_expression(e, report, file))?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::Slice(SliceExpression {
            span,
            target, minind, maxind
        }).into()
    }
}

impl DeclarationSpan for SliceExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier {
    pub name: DMLString,
}

impl Identifier {
    #[allow(clippy::ptr_arg)]
    fn to_expression<'a>(content: &LeafToken,
                         _report: &mut Vec<LocalDMLError>,
                         file: FileSpec<'a>) -> Option<Expression> {
        ExpressionKind::Identifier(
            Identifier {
                name: DMLString::from_token(content, file)?,
            }
        ).into()
    }
}

impl DeclarationSpan for Identifier {
    fn span(&self) -> &ZeroSpan {
        self.name.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionCall {
    pub span: ZeroSpan,
    pub function: Expression,
    pub arguments: Vec<SingleInitializer>,
}

impl FunctionCall {
    pub fn to_expression<'a>(content: &expression::FunctionCallContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let function = ExpressionKind::to_expression(&content.fun, report, file)?;
        let arguments = content.arguments.iter()
            .filter_map(|(arg, _)|SingleInitializer::to_single(arg, report, file)).collect();
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::FunctionCall(FunctionCall {
            span,
            function,
            arguments,
        }).into()
    }
}

impl DeclarationSpan for FunctionCall {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CastExpression {
    pub span: ZeroSpan,
    pub from: Expression,
    pub to: DMLType,
}

impl CastExpression {
    pub fn to_expression<'a>(content: &expression::CastContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let from = ExpressionKind::to_expression(&content.from, report, file)?;
        let to = types::to_type(&content.to, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::CastExpression(CastExpression {
            span,
            to,
            from,
        }).into()
    }
}

impl DeclarationSpan for CastExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IndexExpression {
    pub span: ZeroSpan,
    pub array: Expression,
    pub index: Expression,
}

impl IndexExpression {
    pub fn to_expression<'a>(content: &expression::IndexContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let array = ExpressionKind::to_expression(
            &content.array, report, file)?;
        let index = ExpressionKind::to_expression(
            &content.index, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::IndexExpression(IndexExpression {
            array, index , span
        }).into()
    }
}

impl DeclarationSpan for IndexExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NewExpression {
    pub span: ZeroSpan,
    pub valuetype: DMLType,
    pub amount: Option<Expression>,
}

impl NewExpression {
    pub fn to_expression<'a>(content: &expression::NewContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let amount = content.array.as_ref().and_then(
            |(_, e, _)|ExpressionKind::to_expression(e, report, file));
        let valuetype = types::to_type(&content.typed, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::NewExpression(NewExpression {
            valuetype, amount, span
        }).into()
    }
}

impl DeclarationSpan for NewExpression {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SizeOf {
    pub span: ZeroSpan,
    pub sizeof: Expression,
}

impl SizeOf {
    pub fn to_expression<'a>(content: &expression::SizeOfContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let sizeof = ExpressionKind::to_expression(&content.of, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::SizeOf(SizeOf {
            sizeof, span
        }).into()
    }
}

impl DeclarationSpan for SizeOf {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SizeOfType {
    pub span: ZeroSpan,
    pub typed: DMLType,
}

impl SizeOfType {
    pub fn to_expression<'a>(content: &expression::SizeOfTypeContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let typed = match &content.arg {
            expression::SizeOfTypeArg::Raw(decl) |
            expression::SizeOfTypeArg::Paren(_, decl, _) =>
                types::to_type(decl, report, file)?
        };
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::SizeOfType(SizeOfType {
            typed, span
        }).into()
    }
}

impl DeclarationSpan for SizeOfType {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstantList {
    pub span: ZeroSpan,
    pub list: Vec<Expression>
}

impl ConstantList {
    pub fn to_expression<'a>(content: &expression::ConstListContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let list = content.elements.iter()
            .filter_map(|(e,_)|ExpressionKind::to_expression(
                e, report, file)).collect();
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::ConstantList(ConstantList {
            list, span
        }).into()
    }
}

impl DeclarationSpan for ConstantList {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EachIn {
    pub span: ZeroSpan,
    pub templ: DMLString,
    pub inexpr: Expression,
}

impl EachIn {
    pub fn to_expression<'a>(content: &expression::EachInContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        let templ = DMLString::from_token(&content.ident, file)?;
        let inexpr = ExpressionKind::to_expression(&content.of, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        ExpressionKind::EachIn(EachIn {
            templ, inexpr, span
        }).into()
    }
}

impl DeclarationSpan for EachIn {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

// Here, we have discarded some information about the order of operations,
// since this is implicit from the tree structure
pub type Expression = Box<ExpressionKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntegerLiteral {
    Signed(Value<i64>),
    Unsigned(Value<u64>),
}

impl DeclarationSpan for IntegerLiteral {
    fn span(&self) -> &ZeroSpan {
        match self {
            IntegerLiteral::Signed(val) => val.span(),
            IntegerLiteral::Unsigned(val) => val.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ExpressionKind {
    TertiaryExpression(TertiaryExpression),
    BinaryExpression(BinaryExpression),
    // NOTE: This covers both pre- and post-fix op
    UnaryExpression(UnaryExpression),
    IndexExpression(IndexExpression),
    Slice(SliceExpression),
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(Value<String>),
    CharLiteral(Value<String>),
    MemberLiteral(MemberLiteral),
    // TODO: f64 is annoying, as it does not implement Eq or Ord
    // likely we have to use another, safer, float-like struct
    // perhaps a custom-made one
    FloatLiteral(DMLString),
    FunctionCall(FunctionCall),
    CastExpression(CastExpression),
    NewExpression(NewExpression),
    SizeOf(SizeOf),
    SizeOfType(SizeOfType),
    ConstantList(ConstantList),
    EachIn(EachIn),
    // Used for internally-created auto-params.
    // NOTE: The span here will refer to the containing objects
    // declloc, which is incorrect
    AutoObjectRef(StructureKey, ZeroSpan),
    // Undefined is literal 'undefined' in source. 'unknown' is in-analysis for
    // expressions we cannot determine what they are
    Undefined(ZeroSpan),
    Unknown(ZeroSpan),
}

impl DeclarationSpan for ExpressionKind {
    fn span(&self) -> &ZeroSpan {
        match self {
            Self::TertiaryExpression(op) => op.span(),
            Self::BinaryExpression(op) => op.span(),
            Self::UnaryExpression(op) => op.span(),
            Self::IndexExpression(op) => op.span(),
            Self::Slice(slice) => slice.span(),
            Self::Identifier(ident) => ident.span(),
            Self::IntegerLiteral(int) => int.span(),
            Self::StringLiteral(string) => string.span(),
            Self::CharLiteral(string) => string.span(),
            Self::MemberLiteral(lit) => lit.span(),
            Self::FloatLiteral(float) => float.span(),
            Self::FunctionCall(call) => call.span(),
            Self::CastExpression(cast) => cast.span(),
            Self::NewExpression(new) => new.span(),
            Self::SizeOf(siz) => siz.span(),
            Self::SizeOfType(siz) => siz.span(),
            Self::ConstantList(list) => list.span(),
            Self::EachIn(eachin) => eachin.span(),
            Self::Undefined(span) => span,
            Self::Unknown(span) => span,
            Self::AutoObjectRef(_, span) => span,
        }
    }
}

impl From<ExpressionKind> for Option<Expression> {
    fn from(kind: ExpressionKind) -> Option<Expression> {
        Some(Box::new(kind))
    }
}

fn report_or_return<T>(range: &ZeroRange,
                       res: Result<T, ParseIntError>,
                       report: &mut Vec<LocalDMLError>)
                       -> Option<T> {
    res.map_or_else(|err|{
        match err.kind() {
            IntErrorKind::PosOverflow => report.push(
                LocalDMLError {
                    range: *range,
                    description:
                    "Too large integer constant".to_string(),
                }),
            IntErrorKind::NegOverflow => report.push(
                LocalDMLError {
                    range: *range,
                    description:
                    "Too small integer constant".to_string(),
                }),
            e => error!("Unexpected failure in int parse at {:?}: {:?}",
                        range, e),
        }
        None
    },
                    |res|Some(res))
}

#[allow(clippy::ptr_arg)]
fn ast_to_literal<'a>(ast: &LeafToken,
                      report: &mut Vec<LocalDMLError>,
                      file: FileSpec<'a>) -> Option<Expression> {
    let tok = ast.get_token()?;
    let mut tok_str = tok.read_token(file.file);
    let span = ZeroSpan::from_range(tok.range, file.path);
    match tok.kind {
        TokenKind::StringConstant => {
            // Remove quotes
            tok_str.pop();
            tok_str.remove(0);
            ExpressionKind::StringLiteral(
                Value {
                    val: tok_str,
                    span,
                })
        },
        TokenKind::IntConstant => match tok_str.as_bytes()[0] {
            b'-' => {
                let raw_val = tok_str.replace(['_'],"");
                ExpressionKind::IntegerLiteral(
                    IntegerLiteral::Signed(
                    Value {
                        val: report_or_return(&tok.range,
                                              raw_val.parse::<i64>(),
                                              report)?,
                        span,
                    }))
            },
            _ => {
                let raw_val = tok_str.replace(['_'],"");
                ExpressionKind::IntegerLiteral(
                    IntegerLiteral::Unsigned(
                        Value {
                            val: report_or_return(&tok.range,
                                                  raw_val.parse::<u64>(),
                                                  report)?,
                            span,
                        }))
            },
        },
        TokenKind::HexConstant => {
            // Parse everything after 0x as hexadecimal, stripping out '_'
            let raw_hex = tok_str[2..].replace(['_'],"");
            ExpressionKind::IntegerLiteral(
                IntegerLiteral::Unsigned(
                    Value {
                        val: report_or_return(&tok.range,
                                              u64::from_str_radix(&raw_hex, 16),
                                              report)?,
                        span,
                    }))
        },
        TokenKind::BinaryConstant => {
            let raw_bin = tok_str[2..].replace(['_'],"");
            ExpressionKind::IntegerLiteral(
                IntegerLiteral::Unsigned(
                    Value {
                        // Parse everything after 0b as binary
                        val: report_or_return(&tok.range,
                                              u64::from_str_radix(&raw_bin, 2),
                                              report)?,
                        span,
                    }))
        },
        TokenKind::CharConstant => {
            // Remove quotes
            tok_str.pop();
            tok_str.remove(0);
            ExpressionKind::CharLiteral(
                Value {
                    val: tok_str,
                    span,
                })
        },
        TokenKind::FloatConstant => {
            // TODO: Placeholder, we will need a good way to
            // store floats internally. We do not want to store
            // them as f64
            ExpressionKind::FloatLiteral(
                Value {
                    val: tok_str,
                    span,
                })
        },
        e => {
            error!("Invalid tokenkind for literal constant: {:?}", e);
            return None;
        }
    }.into()
}

impl ExpressionKind {
    pub fn to_expression<'a>(expr: &expression::Expression,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>) -> Option<Expression> {
        expr.with_content(
            |con|
            match con {
                ExpressionContent::BinaryExpression(bin)
                    => BinaryExpression::to_expression(
                        bin, report, file),
                ExpressionContent::MemberLiteral(memb)
                    => MemberLiteral::to_expression(
                        memb, report, file),
                ExpressionContent::TertiaryExpression(ter)
                    => TertiaryExpression::to_expression(ter, report, file),
                ExpressionContent::UnaryExpression(unaryexprcontent)
                    => to_unary_expression(
                        &unaryexprcontent.operation,
                        &unaryexprcontent.expr,
                        unaryexprcontent.range(), report, file),
                ExpressionContent::PostUnaryExpression(unaryexprcontent)
                    => to_unary_expression(
                        &unaryexprcontent.operation,
                        &unaryexprcontent.expr,
                        unaryexprcontent.range(), report, file),
                ExpressionContent::Index(index)
                    => IndexExpression::to_expression(index, report, file),
                ExpressionContent::Literal(lit)
                    => ast_to_literal(lit, report, file),
                ExpressionContent::Identifier(ident)
                    => Identifier::to_expression(ident, report, file),
                ExpressionContent::FunctionCall(call)
                    => FunctionCall::to_expression(call, report, file),
                ExpressionContent::Cast(cast)
                    => CastExpression::to_expression(cast, report, file),
                ExpressionContent::New(new)
                    => NewExpression::to_expression(new, report, file),
                ExpressionContent::SizeOf(sizeof)
                    => SizeOf::to_expression(sizeof, report, file),
                ExpressionContent::SizeOfType(typed)
                    => SizeOfType::to_expression(typed, report, file),
                ExpressionContent::Slice(slice)
                    => SliceExpression::to_expression(slice, report, file),
                ExpressionContent::ConstList(list)
                    => ConstantList::to_expression(list, report, file),
                ExpressionContent::ParenExpression(paren)
                    => ExpressionKind::to_expression(&paren.expr, report, file),
                ExpressionContent::EachIn(eachin)
                    => EachIn::to_expression(eachin, report, file),
                ExpressionContent::Undefined(undefined)
                    => Some(ExpressionKind::Undefined(
                        ZeroSpan::from_range(undefined.range(),
                                             file.path)
                    ).into()),
            },
            None)
    }
}

pub type DMLString = Value<String>;

impl DMLString {
    pub fn from_token(token: &LeafToken,
                      file: FileSpec<'_>) -> Option<DMLString> {
        Some(DMLString {
            val: token.read_leaf(file.file)?,
            span: ZeroSpan::from_range(token.range(), file.path),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InitializerStruct {
    initializer_fields: Vec<(DMLString,
                             Box<SingleInitializer>)>,
    span: ZeroSpan,
}

impl InitializerStruct {
    fn to_single<'a>(content: &Vec<(misc::InitializerStructElem,
                                    Option<LeafToken>)>,
                     report: &mut Vec<LocalDMLError>,
                     file: FileSpec<'a>) -> Option<SingleInitializer> {
        let initializer_fields: Vec<(DMLString,
                                     Box<SingleInitializer>)>
            = content.iter().filter_map(
                |(field,_)|match (DMLString::from_token(&field.ident, file),
                                  SingleInitializer::to_single(&field.init,
                                                               report,
                                                               file).map(
                                      |si|Box::new(si))) {
                    (Some(a), Some(b)) => Some((a,b)),
                    _ => None,
                }).collect();
        if initializer_fields.is_empty() {
            None
        } else {
            Some(SingleInitializer::Structure(InitializerStruct {
                initializer_fields,
                span: ZeroSpan::from_range(content.range(), file.path),
            }))
        }
    }
}

impl DeclarationSpan for InitializerStruct {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InitializerList {
    list: Vec<SingleInitializer>,
    span: ZeroSpan,
}

impl InitializerList {
    fn to_single<'a>(content: &Vec<(misc::SingleInitializer,
                                    Option<LeafToken>)>,
                     report: &mut Vec<LocalDMLError>,
                     file: FileSpec<'a>) -> Option<SingleInitializer> {
        let list: Vec<SingleInitializer> = content.iter().filter_map(
            |(single, _)|SingleInitializer::to_single(single, report, file))
            .collect();
        if list.is_empty() {
            None
        } else {
            Some(SingleInitializer::List(InitializerList {
                list,
                span: ZeroSpan::from_range(content.range(), file.path),
            }))
        }
    }
}

impl DeclarationSpan for InitializerList {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SingleInitializer {
    Expression(Expression),
    List(InitializerList),
    Structure(InitializerStruct),
}

impl SingleInitializer {
    fn to_single<'a>(content: &misc::SingleInitializer,
                     report: &mut Vec<LocalDMLError>,
                     file: FileSpec<'a>) -> Option<SingleInitializer> {
        content.with_content(
            |con|
            match con {
                misc::SingleInitializerContent::Expression(expr) =>
                    Some(SingleInitializer::Expression(
                        ExpressionKind::to_expression(expr, report, file)?)),
                misc::SingleInitializerContent::List(_, list, _) =>
                    InitializerList::to_single(list, report, file),
                misc::SingleInitializerContent::Structure(_, fields,
                                                          _, _) =>
                    InitializerStruct::to_single(fields, report, file),
            },
        None)
    }

    fn to_initializer<'a>(content: &misc::SingleInitializer,
                          report: &mut Vec<LocalDMLError>,
                          file: FileSpec<'a>) -> Option<Initializer> {
        SingleInitializer::to_single(content, report, file)
            .map(|si|Initializer::Single(si))
    }
}

impl DeclarationSpan for SingleInitializer {
    fn span(&self) -> &ZeroSpan {
        match self {
            SingleInitializer::Expression(expr) => expr.span(),
            SingleInitializer::List(list) => list.span(),
            SingleInitializer::Structure(struc) => struc.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InitializerTuple {
    pub list: Vec<SingleInitializer>,
    pub span: ZeroSpan,
}

impl InitializerTuple {
    fn to_initializer<'a>(content: &Vec<(misc::SingleInitializer,
                                         Option<LeafToken>)>,
                          report: &mut Vec<LocalDMLError>,
                          file: FileSpec<'a>) -> Option<Initializer> {
        let list: Vec<SingleInitializer> = content.iter().filter_map(
            |(single, _)|SingleInitializer::to_single(single, report, file))
            .collect();
        if list.is_empty() {
            None
        } else {
            Some(Initializer::Tuple(InitializerTuple {
                list,
                span: ZeroSpan::from_range(content.range(), file.path),
            }))
        }
    }
}

impl DeclarationSpan for InitializerTuple {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Initializer {
    Single(SingleInitializer),
    Tuple(InitializerTuple),
}

impl DeclarationSpan for Initializer {
    fn span(&self) -> &ZeroSpan {
        match self {
            Initializer::Single(single) => single.span(),
            Initializer::Tuple(tuple) => tuple.span(),
        }
    }
}

impl Initializer {
    pub fn to_initializer<'a>(content: &misc::Initializer,
                              report: &mut Vec<LocalDMLError>,
                              file: FileSpec<'a>) -> Option<Initializer> {
        content.with_content(
            |con|
            match con {
                misc::InitializerContent::Single(single) =>
                    SingleInitializer::to_initializer(single,
                                                      report,
                                                      file),
                misc::InitializerContent::Tuple(_, initializers, _) =>
                    InitializerTuple::to_initializer(initializers,
                                                     report,
                                                     file),
            },
            None)
    }
}

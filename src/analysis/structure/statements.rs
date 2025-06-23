//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use log::error;

use crate::analysis::{DeclarationSpan,
                      LocalDMLError};


use crate::analysis::symbols::{StructureSymbol,
                               SymbolContainer};
use crate::analysis::structure::objects::{Variable,
                                          VariableDeclKind,
                                          to_variable_structure};
use crate::analysis::structure::expressions::{DMLString, ExpressionKind,
                                              Expression, Initializer};
use crate::analysis::parsing::{structure, statement, misc};
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{LeafToken, ZeroSpan, TreeElement};
use crate::analysis::FileSpec;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForEach {
    pub identifier: DMLString,
    pub inexpr: Expression,
    pub body: Statement,
    pub span: ZeroSpan,
}

impl ForEach {
    fn to_statement<'a>(content: &statement::ForeachContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let identifier = DMLString::from_token(&content.ident, file)?;
        let inexpr = ExpressionKind::to_expression(
            &content.expression, report, file)?;
        let body = StatementKind::to_statement(
            &content.statement, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::ForEach(ForEach {
            identifier, inexpr, body, span
        }).into()
    }
}

impl DeclarationSpan for ForEach {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct If {
    pub condition: Expression,
    pub ifbody: Statement,
    pub elsebody: Option<Statement>,
    pub span: ZeroSpan,
}

impl If {
    fn to_statement<'a>(content: &statement::IfContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let condition = ExpressionKind::to_expression(
            &content.cond, report, file)?;
        let ifbody = StatementKind::to_statement(
            &content.truebranch,
            report,
            file)?;
        let elsebody = content.elsebranch.as_ref().and_then(
            |(_, elbody)|StatementKind::to_statement(elbody, report, file));
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::If(If {
            condition, ifbody, elsebody, span
        }).into()
    }
}

impl DeclarationSpan for If {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashIf {
    pub condition: Expression,
    pub ifbody: Statement,
    pub elsebody: Option<Statement>,
    pub span: ZeroSpan,
}

impl HashIf {
    fn to_statement<'a>(content: &statement::HashIfContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let condition = ExpressionKind::to_expression(
            &content.cond, report, file)?;
        let ifbody = StatementKind::to_statement(&content.truebranch,
                                                report,
                                                file)?;
        let elsebody = content.elsebranch.as_ref().and_then(
            |(_, content)|StatementKind::to_statement(content, report, file));
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::HashIf(HashIf {
            condition, ifbody, elsebody, span,
        }).into()
    }
}

impl DeclarationSpan for HashIf {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashIfCase {
    pub condition: Expression,
    pub truecases: Vec<SwitchCase>,
    pub falsecases: Vec<SwitchCase>,
    pub span: ZeroSpan,
}

impl HashIfCase {
    fn to_hif_case<'a>(content: &statement::SwitchHashIf,
                       report: &mut Vec<LocalDMLError>,
                       file: FileSpec<'a>) -> Option<Box<HashIfCase>> {
        let condition = ExpressionKind::to_expression(
            &content.cond, report, file)?;
        let truecases = content.truecases.iter().filter_map(
            |case|SwitchCase::to_case(case, report, file)).collect();
        let falsecases = content.hashelse.as_ref().map_or_else(
            ||vec![],
            |(_,_,vec,_)|vec.iter().filter_map(
                |case|SwitchCase::to_case(case, report, file)).collect());
        let span = ZeroSpan::from_range(content.range(), file.path);
        Some(HashIfCase {
            condition, truecases, falsecases, span,
        }.into())
    }
}

impl DeclarationSpan for HashIfCase {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SwitchCase {
    Statement(Statement),
    Case(Expression),
    HashIf(Box<HashIfCase>),
    Default(ZeroSpan),
}

impl SwitchCase {
    fn to_case<'a>(content: &statement::SwitchCase,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'a>) -> Option<SwitchCase> {
        Some(match content {
            statement::SwitchCase::Statement(stmnt) =>
                SwitchCase::Statement(
                    StatementKind::to_statement(stmnt, report, file)?),
            statement::SwitchCase::Case(_, expr, _) =>
                SwitchCase::Case(
                    ExpressionKind::to_expression(expr, report, file)?),
            statement::SwitchCase::HashIf(hif) =>
                SwitchCase::HashIf(HashIfCase::to_hif_case(
                    hif, report, file)?),
            con @ statement::SwitchCase::Default(_, _) =>
                SwitchCase::Default(
                    ZeroSpan::from_range(con.range(), file.path)),
        })
    }
}

impl DeclarationSpan for SwitchCase {
    fn span(&self) -> &ZeroSpan {
        match self {
            SwitchCase::Statement(stmnt) => stmnt.span(),
            SwitchCase::Case(expr) => expr.span(),
            SwitchCase::HashIf(ifcase) => ifcase.span(),
            SwitchCase::Default(span) => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Switch {
    pub expr: Expression,
    pub cases: Vec<SwitchCase>,
    pub span: ZeroSpan,
}

impl Switch {
    fn to_statement<'a>(content: &statement::SwitchContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let expr =  ExpressionKind::to_expression(&content.expr, report, file)?;
        let cases = content.cases.iter().filter_map(
            |case|SwitchCase::to_case(case, report, file)).collect();
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Switch(Switch {
            expr, cases, span
        }).into()
    }
}

impl DeclarationSpan for Switch {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While {
    pub cond: Expression,
    pub body: Statement,
    pub span: ZeroSpan,
}

impl While {
    fn to_statement<'a>(content: &statement::WhileContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let cond = ExpressionKind::to_expression(&content.cond, report, file)?;
        let body = StatementKind::to_statement(
            &content.statement, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::While(While {
            cond, body, span,
        }).into()
    }
}

impl DeclarationSpan for While {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DoWhile {
    pub cond: Expression,
    pub body: Statement,
    pub span: ZeroSpan,
}

impl DoWhile {
    fn to_statement<'a>(content: &statement::DoContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let cond = ExpressionKind::to_expression(&content.cond, report, file)?;
        let body = StatementKind::to_statement(
            &content.statement, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::DoWhile(DoWhile {
            cond, body, span,
        }).into()
    }
}

impl DeclarationSpan for DoWhile {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ForPostElement {
    Expression(Expression),
    Assign(Vec<Expression>, Assigner),
    AssignOp(Expression, AssignOp, Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ForPre {
    Declaration(Variable),
    Post(Vec<ForPostElement>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct For {
    pub pre: Option<ForPre>,
    pub cond: Option<Expression>,
    pub post: Vec<ForPostElement>,
    pub body: Statement,
    pub span: ZeroSpan,
}

fn to_forpost<'a>(content: &statement::ForPost,
                  report: &mut Vec<LocalDMLError>,
                  file: FileSpec<'a>) -> Vec<ForPostElement> {
    content.iter().filter_map(|(elem,_)|match elem {
        statement::ForPostElement::Expression(expr) =>
            ExpressionKind::to_expression(expr, report, file).map(
                |e|ForPostElement::Expression(e)),
        statement::ForPostElement::Assign(t, a) =>
            if let Some(assigner) = assigner_to_assigner(a, report, file) {
                let target  = target_to_vec(t, report, file);
                if target.is_empty() {
                    None
                } else {
                    Some(ForPostElement::Assign(target, assigner))
                }
            } else {
                None
            },
        statement::ForPostElement::AssignOp(expr, op, expr2) => {
            match (ExpressionKind::to_expression(expr, report, file),
                   ExpressionKind::to_expression(expr2, report, file)) {
                (Some(e1), Some(e2)) => {
                    // TODO: Check if this is guaranteed by parser
                    let opr = tok_to_assignop(op.get_token().unwrap().kind)
                        .unwrap();
                    Some(ForPostElement::AssignOp(e1, opr, e2))
                },
                _ => None,
            }
        },
    }).collect()
}

impl For {
    fn to_statement<'a>(content: &statement::ForContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let cond = content.cond.as_ref().and_then(
            |exp|ExpressionKind::to_expression(exp, report, file));
        let post = content.post.as_ref().map_or_else(
            ||vec![],
            |post|to_forpost(post, report, file));
        let pre = content.pre.as_ref().and_then(|pre|match pre {
            statement::ForPre::Declaration(kind, vardecls, maybe_init) =>
                to_variable(kind, vardecls,
                            maybe_init.as_ref().map(|(_, init)|init),
                            report, file)
                .map(ForPre::Declaration),
            statement::ForPre::Post(p) => Some(ForPre::Post(
                to_forpost(p, report, file))),
        });
        let body = StatementKind::to_statement(
            &content.statement, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::For(For {
            cond, pre, post, body, span,
        }).into()
    }
}

impl DeclarationSpan for For {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AfterExpression {
    Timer(Expression),
    // Note: We allow None here in order to not double-report
    // cases where a non-identifier has been providied as a binding
    // (it would report both wrong number of bindings and a parser error)
    Hook(Expression, Vec<Option<String>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct After {
    pub after: Option<AfterExpression>,
    pub call: Expression,
    pub span: ZeroSpan,
}

impl After {
    fn to_statement<'a>(content: &statement::AfterContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let after = content.timer.as_ref().and_then(
            |t|Some(match t {
                statement::AfterTiming::Timer(expr, _) =>
                    AfterExpression::Timer(
                        ExpressionKind::to_expression(
                            expr, report, file)?),
                statement::AfterTiming::HookNoParams(hook) =>
                    AfterExpression::Hook(
                        ExpressionKind::to_expression(
                            hook, report, file)?,
                        vec![]),
                statement::AfterTiming::HookBindOne(hook, _, bind) =>
                    AfterExpression::Hook(
                        ExpressionKind::to_expression(
                            hook, report, file)?,
                        vec![bind.read_leaf(file.file)]),
                statement::AfterTiming::HookBindList(hook, _, _, binds, _) =>
                    AfterExpression::Hook(
                        ExpressionKind::to_expression(
                            hook, report, file)?,
                        binds.iter()
                            .map(|(bind, _)|bind.read_leaf(file.file))
                            .collect()),
            }));
        let call = ExpressionKind::to_expression(
            &content.callexpression, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::After(After {
            after, call, span,
        }).into()
    }
}

impl DeclarationSpan for After {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    pub ret: Option<Initializer>,
    pub span: ZeroSpan,
}

impl Return {
    fn to_statement<'a>(content: &statement::ReturnContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let ret = content.val.as_ref().and_then(
            |ret|Initializer::to_initializer(ret, report, file));
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Return(Return {
            ret, span,
        }).into()
    }
}

impl DeclarationSpan for Return {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue {
    pub span: ZeroSpan,
}

impl Continue {
    #[allow(clippy::ptr_arg)]
    fn to_statement<'a>(content: &statement::ContinueContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Continue(Continue {
            span,
        }).into()
    }
}

impl DeclarationSpan for Continue {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break {
    pub span: ZeroSpan,
}

impl DeclarationSpan for Break {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl Break {
    #[allow(clippy::ptr_arg)]
    fn to_statement<'a>(content: &statement::BreakContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Break(Break {
            span,
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TryCatch {
    pub tryblock: Statement,
    pub catchblock: Statement,
    pub span: ZeroSpan,
}

impl TryCatch {
    fn to_statement<'a>(content: &statement::TryContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let tryblock = StatementKind::to_statement(&content.trystatement,
                                                   report, file)?;
        let catchblock = StatementKind::to_statement(&content.catchstatement,
                                                     report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::TryCatch(TryCatch {
            tryblock, catchblock, span,
        }).into()
    }
}

impl DeclarationSpan for TryCatch {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Throw {
    pub span: ZeroSpan,
}

impl Throw {
    #[allow(clippy::ptr_arg)]
    fn to_statement<'a>(content: &statement::ThrowContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Throw(Throw {
            span,
        }).into()
    }
}

impl DeclarationSpan for Throw {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogLevel {
    Simple(Expression),
    Subsequent(Expression, Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogKind {
    SpecViol,
    Info,
    Critical,
    Unimpl,
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Log {
    pub kind: LogKind,
    pub level: Option<LogLevel>,
    pub flags: Option<Expression>,
    pub message: Expression,
    pub args: Vec<Expression>,
    pub span: ZeroSpan,
}

impl Log {
    fn to_statement<'a>(content: &statement::LogContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        // This is actually NOT filtered on a parser level, so check valid log
        // kinds here
        let kind = content.log_kind.get_token().map(
            |kindtok|(kindtok.range,
                      kindtok.read_token(file.file))).and_then(
            |(range, id)|match id.as_str() {
                "error" => Some(LogKind::Error),
		"warning" => Some(LogKind::Warning),
                "info" => Some(LogKind::Info),
                "unimpl" => Some(LogKind::Unimpl),
                "critical" => Some(LogKind::Critical),
                "spec_viol" => Some(LogKind::SpecViol),
                _ => {
                    report.push(LocalDMLError {
                        range,
                        description: "Invalid log kind, valid log kinds are \
                                      \"error\", \"warning\", \"info\", \"unimpl\", \
                                      \"critical\", and \"spec_viol\"."
                            .to_string(),
                    });
                    None
                },
            })?;
        let level = content.level.as_ref().and_then(
            |(_,lev)|match lev {
                statement::LogLevel::Simple(expr) =>
                    ExpressionKind::to_expression(expr, report, file)
                    .map(|expr|LogLevel::Simple(expr)),
                statement::LogLevel::Subsequent(l, _, r) =>
                    match (ExpressionKind::to_expression(l, report, file),
                           ExpressionKind::to_expression(r, report, file)) {
                        (Some(l), Some(r)) => Some(LogLevel::Subsequent(l, r)),
                        _ => None
                    }
            }
        );
        let flags = content.flags.as_ref().and_then(
            |(_,expr)|ExpressionKind::to_expression(expr, report, file));
        let message = ExpressionKind::to_expression(
            &content.message, report, file)?;
        let args = content.args.iter().filter_map(
            |(_, expr)|ExpressionKind::to_expression(
                expr, report, file)).collect();

        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Log(Log {
            kind, level, flags, message, args,
            span,
        }).into()
    }
}

impl DeclarationSpan for Log {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Assert {
    pub expression: Expression,
    pub span: ZeroSpan,
}

impl Assert {
    fn to_statement<'a>(content: &statement::AssertContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let expression = ExpressionKind::to_expression(
            &content.expression, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Assert(Assert {
            expression,
            span,
        }).into()
    }
}

impl DeclarationSpan for Assert {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delete {
    pub expression: Expression,
    pub span: ZeroSpan,
}

impl Delete {
    fn to_statement<'a>(content: &statement::DeleteContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let expression = ExpressionKind::to_expression(
            &content.expr, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Delete(Delete {
            expression,
            span,
        }).into()
    }
}

impl DeclarationSpan for Delete {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    pub message: Option<Expression>,
    pub span: ZeroSpan,
}

impl Error {
    fn to_statement<'a>(content: &statement::ErrorContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let message = content.message.as_ref().and_then(
            |expr|ExpressionKind::to_expression(
                expr, report, file));
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Error(Error {
            message,
            span,
        }).into()
    }
}

impl DeclarationSpan for Error {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

fn to_variable<'a>(leaf_for_kind: &LeafToken,
                   decls: &structure::VarDecl,
                   inits: Option<&misc::Initializer>,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'a>) -> Option<Variable> {
    let kind = if let Some(kind_str) = leaf_for_kind.read_leaf(file.file) {
        match kind_str.as_str() {
            "session" => VariableDeclKind::Session,
            "saved" => VariableDeclKind::Saved,
            "local" => VariableDeclKind::Local,
            e => {
                error!("Internal error: Unexpected declaration kind {}", e);
                return None;
            },
        }
    } else {
        // Normally this is cleanly unexpected
        error!("Internal error: Could not read declaration kind");
        return None;
    };
    to_variable_structure(decls, inits, kind, report, file)
}

fn to_statement_variable_decl<'a>(content: &statement::VariableDeclContent,
                                  report: &mut Vec<LocalDMLError>,
                                  file: FileSpec<'a>) -> Option<Statement> {
    StatementKind::VariableDecl(to_variable(
        &content.kind,
        &content.decls,
        content.initializer.as_ref().map(|(_,i)|i),
        report,
        file)?).into()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AssignOp {
    Plus, Minus, Times, Divide, Mod,
    BOr, BAnd, BXor,
    LShift, RShift,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignOpStatement {
    pub assignee: Expression,
    pub operation: AssignOp,
    pub assigner: Expression,
    pub span: ZeroSpan,
}

impl DeclarationSpan for AssignOpStatement {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

fn tok_to_assignop(tok: TokenKind) -> Option<AssignOp> {
    Some(match tok {
        TokenKind::PlusAssign => AssignOp::Plus,
        TokenKind::MinusAssign => AssignOp::Minus,
        TokenKind::TimesAssign => AssignOp::Times,
        TokenKind::DivideAssign => AssignOp::Divide,
        TokenKind::ModAssign => AssignOp::Mod,
        TokenKind::BOrAssign => AssignOp::BOr,
        TokenKind::BAndAssign => AssignOp::BAnd,
        TokenKind::BXorAssign => AssignOp::BXor,
        TokenKind::LShiftAssign => AssignOp::LShift,
        TokenKind::RShiftAssign => AssignOp::RShift,
        e => {
            error!("Unexpected assignop token kind: {:?}", e);
            return None;
        },
    })
}
impl AssignOpStatement {
    fn to_statement<'a>(content: &statement::AssignOpContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let assignee = ExpressionKind::to_expression(
            &content.assignee, report, file)?;
        let assigner = ExpressionKind::to_expression(
            &content.assign, report, file)?;
        // Guaranteed by parser
        let operation = tok_to_assignop(
            content.opr.get_token().unwrap().kind)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::AssignOp(AssignOpStatement {
            operation,
            assignee,
            assigner,
            span,
        }).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Assigner {
    Initializer(Initializer),
    Chain(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignStatement {
    pub assignees: Vec<Expression>,
    pub assigner: Assigner,
    pub span: ZeroSpan,
}

fn target_to_vec<'a>(content: &statement::AssignTarget,
                     report: &mut Vec<LocalDMLError>,
                     file: FileSpec<'a>) -> Vec<Expression> {
    match content {
        statement::AssignTarget::One(expr) =>
            ExpressionKind::to_expression(expr, report, file)
            .into_iter().collect(),
        statement::AssignTarget::Many(_, exprs, _) => exprs.iter()
            .filter_map(|(expr,_)|
                        ExpressionKind::to_expression(expr, report, file))
            .collect(),
    }
}

fn assigner_to_assigner<'a>(content: &statement::Assigner,
                            report: &mut Vec<LocalDMLError>,
                            file: FileSpec<'a>) -> Option<Assigner> {
    match content {
        statement::Assigner::Initializer(_, init) =>
            Initializer::to_initializer(init, report, file)
            .map(|init|Assigner::Initializer(init)),
        statement::Assigner::Chain(chain) => {
            let expr_chain: Vec<Expression> = chain.iter().filter_map(
                |(_, e)|ExpressionKind::to_expression(e, report, file))
                .collect();
            if expr_chain.is_empty() {
                None
            } else {
                Some(Assigner::Chain(expr_chain))
            }
        },
    }
}

impl AssignStatement {
    fn to_statement<'a>(content: &statement::AssignContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let assignees = target_to_vec(
            &content.target, report, file);
        if assignees.is_empty() {
            return None;
        }
        let assigner = assigner_to_assigner(
            &content.assigner, report, file)?;

        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Assign(AssignStatement {
            assignees,
            assigner,
            span,
        }).into()
    }
}

impl DeclarationSpan for AssignStatement {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashSelect {
    pub ident: DMLString,
    pub inexpr: Expression,
    pub whereexpr: Expression,
    pub selectbranch: Statement,
    pub elsebranch: Statement,
    pub span: ZeroSpan,
}

impl HashSelect {
    fn to_statement<'a>(content: &statement::HashSelectContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let ident = DMLString::from_token(&content.ident, file)?;
        let inexpr = ExpressionKind::to_expression(
            &content.inexpression, report, file)?;
        let whereexpr = ExpressionKind::to_expression(
            &content.whereexpression, report, file)?;
        let selectbranch = StatementKind::to_statement(
            &content.selectstatement, report, file)?;
        let elsebranch = StatementKind::to_statement(
            &content.elsestatement, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::HashSelect(HashSelect {
            ident,
            inexpr,
            whereexpr,
            selectbranch,
            elsebranch,
            span,
        }).into()
    }
}

impl DeclarationSpan for HashSelect {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompoundStatement {
    pub statements: Vec<Statement>,
    pub span: ZeroSpan,
}

impl CompoundStatement {
    fn to_statement<'a>(content: &statement::CompoundContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let statements: Vec<Statement> = content.statements.iter()
            .filter_map(|s|StatementKind::to_statement(s, report, file))
            .collect();

        Some(Box::new(StatementKind::Compound(CompoundStatement {
            statements,
            span: ZeroSpan::from_range(content.range(), file.path),
        })))
    }
}

impl DeclarationSpan for CompoundStatement {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub span: ZeroSpan,
}

impl ExpressionStatement {
    fn to_statement<'a>(content: &statement::ExpressionStmtContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Statement> {
        let expression = ExpressionKind::to_expression(
            &content.expression, report, file)?;
        let span = ZeroSpan::from_range(content.range(), file.path);
        StatementKind::Expression(ExpressionStatement {
            expression,
            span,
        }).into()
    }
}

impl DeclarationSpan for ExpressionStatement {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

pub type Statement = Box<StatementKind>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StatementKind {
    ForEach(ForEach),
    HashIf(HashIf),
    If(If),
    Switch(Switch),
    While(While),
    For(For),
    DoWhile(DoWhile),
    HashSelect(HashSelect),
    After(After),
    Return(Return),
    Continue(Continue),
    TryCatch(TryCatch),
    Throw(Throw),
    Break(Break),
    Log(Log),
    Assert(Assert),
    Delete(Delete),
    Error(Error),
    VariableDecl(Variable),
    Assign(AssignStatement),
    AssignOp(AssignOpStatement),
    Expression(ExpressionStatement),
    Compound(CompoundStatement),
    Empty(ZeroSpan),
}

impl DeclarationSpan for StatementKind {
    fn span(&self) -> &ZeroSpan {
        match self {
            StatementKind::ForEach(stmnt) => stmnt.span(),
            StatementKind::HashIf(stmnt) => stmnt.span(),
            StatementKind::If(stmnt) => stmnt.span(),
            StatementKind::Switch(stmnt) => stmnt.span(),
            StatementKind::While(stmnt) => stmnt.span(),
            StatementKind::For(stmnt) => stmnt.span(),
            StatementKind::DoWhile(stmnt) => stmnt.span(),
            StatementKind::HashSelect(stmnt) => stmnt.span(),
            StatementKind::After(stmnt) => stmnt.span(),
            StatementKind::Return(stmnt) => stmnt.span(),
            StatementKind::Continue(stmnt) => stmnt.span(),
            StatementKind::TryCatch(stmnt) => stmnt.span(),
            StatementKind::Throw(stmnt) => stmnt.span(),
            StatementKind::Break(stmnt) => stmnt.span(),
            StatementKind::Log(stmnt) => stmnt.span(),
            StatementKind::Assert(stmnt) => stmnt.span(),
            StatementKind::Error(stmnt) => stmnt.span(),
            StatementKind::Delete(stmnt) => stmnt.span(),
            StatementKind::VariableDecl(stmnt) => stmnt.span(),
            StatementKind::Assign(stmnt) => stmnt.span(),
            StatementKind::AssignOp(stmnt) => stmnt.span(),
            StatementKind::Expression(stmnt) => stmnt.span(),
            StatementKind::Compound(stmnt) => stmnt.span(),
            StatementKind::Empty(span) => span,
        }
    }
}

impl SymbolContainer for StatementKind {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        vec![]
    }
}

impl StatementKind {
    #[allow(clippy::ptr_arg)]
    pub fn to_statement<'a>(stmnt: &statement::Statement,
                            report: &mut Vec<LocalDMLError>,
                            file: FileSpec<'a>) -> Option<Statement> {
        let stmntspan = ZeroSpan::from_range(stmnt.range(), file.path);
        stmnt.with_content(|con|match con {
            statement::StatementContent::Empty(_) =>
                Some(Box::new(StatementKind::Empty(stmntspan))),
            statement::StatementContent::Compound(compcont) =>
                CompoundStatement::to_statement(compcont, report, file),
            statement::StatementContent::Expression(exprstmt) =>
                ExpressionStatement::to_statement(exprstmt, report, file),
            statement::StatementContent::Assert(assert) =>
                Assert::to_statement(assert, report, file),
            statement::StatementContent::VariableDecl(decl) =>
                to_statement_variable_decl(decl, report, file),
            statement::StatementContent::Delete(delete) =>
                Delete::to_statement(delete, report, file),
            statement::StatementContent::AssignOp(assignop) =>
                AssignOpStatement::to_statement(assignop, report, file),
            statement::StatementContent::Assign(assign) =>
                AssignStatement::to_statement(assign, report, file),
            statement::StatementContent::Error(err) =>
                Error::to_statement(err, report, file),
            statement::StatementContent::If(ifcontent) =>
                If::to_statement(ifcontent, report, file),
            statement::StatementContent::Switch(switch) =>
                Switch::to_statement(switch, report, file),
            statement::StatementContent::HashIf(hashifcontent) =>
                HashIf::to_statement(hashifcontent, report, file),
            statement::StatementContent::While(whilecontent) =>
                While::to_statement(whilecontent, report, file),
            statement::StatementContent::Do(docontent) =>
                DoWhile::to_statement(docontent, report, file),
            statement::StatementContent::For(forcontent) =>
                For::to_statement(forcontent, report, file),
            statement::StatementContent::Try(trycontent) =>
                TryCatch::to_statement(trycontent, report, file),
            statement::StatementContent::After(after) =>
                After::to_statement(after, report, file),
            statement::StatementContent::Log(log) =>
                Log::to_statement(log, report, file),
            statement::StatementContent::HashSelect(select) =>
                HashSelect::to_statement(select, report, file),
            statement::StatementContent::Foreach(foreach) =>
                ForEach::to_statement(foreach, report, file),
            statement::StatementContent::Throw(throw) =>
                Throw::to_statement(throw, report, file),
            statement::StatementContent::Continue(cont) =>
                Continue::to_statement(cont, report, file),
            statement::StatementContent::Break(brk) =>
                Break::to_statement(brk, report, file),
            statement::StatementContent::Return(ret) =>
                Return::to_statement(ret, report, file),
        }, None)
    }
}

impl From<StatementKind> for Option<Statement> {
    fn from(kind: StatementKind) -> Option<Statement> {
        Some(Box::new(kind))
    }
}

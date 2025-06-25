//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::sync::Arc;
use lsp_types::DiagnosticSeverity;

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::symbols::{DMLSymbolKind, MakeSymbolContainer,
                               StructureSymbol, SymbolContainer};
use crate::analysis::structure::expressions::DMLString;
use crate::analysis::structure::types::DMLType;
use crate::analysis::structure::objects::{MaybeAbstract, MethodArgument,
                                          MethodModifier, Method};
use crate::analysis::structure::statements::{Statement, StatementKind};
use crate::analysis::{DeclarationSpan, DMLNamed, DMLError};
use crate::analysis::templating::Declaration;
use crate::analysis::templating::objects::DMLNamedMember;
use crate::analysis::templating::types::{eval_type_simple, DMLResolvedType};
use crate::analysis::templating::traits::{DMLTrait};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DMLMethodArg {
    Typed(Declaration),
    Inline(DMLString),
}

impl StructureSymbol for DMLMethodArg {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::MethodArg
    }
}

impl DMLNamed for DMLMethodArg {
    fn name(&self) -> &DMLString {
        match self {
            DMLMethodArg::Inline(inl) => inl,
            DMLMethodArg::Typed(decl) => &decl.name,
        }
    }
}

impl DMLMethodArg {
    pub fn is_inline(&self) -> bool {
        matches!(self, DMLMethodArg::Inline(_))
    }
    pub fn equivalent(&self, other: &DMLMethodArg) -> bool {
        match (self, other) {
            (DMLMethodArg::Inline(_), DMLMethodArg::Inline(_)) =>
                true,
            (DMLMethodArg::Typed(decl1), DMLMethodArg::Typed(decl2)) =>
                decl1.type_ref.equivalent(&decl2.type_ref),
            _ => false
        }
    }
    pub fn span(&self) -> &ZeroSpan {
        match self {
            DMLMethodArg::Inline(inl) => &inl.span,
            DMLMethodArg::Typed(decl) => &decl.name.span,
        }
    }
}

pub fn eval_method_args(args: &[MethodArgument], report: &mut Vec<DMLError>)
                    -> Vec<DMLMethodArg> {
    args.iter().map(|arg|
                    match arg {
                        MethodArgument::Typed(name, typed) => {
                            let (structs, type_ref) = eval_type_simple(
                                typed, (), ());
                            for s in &structs {
                                report.push(
                                    DMLError {
                                        span: *s.span(),
                                        description:
                                            "Cannot use anonymous".to_string() +
                                            " struct type in argument type",
                                        related: vec![],
                                        severity: Some(DiagnosticSeverity::ERROR),
                                    });
                            }
                            DMLMethodArg::Typed(Declaration {
                                type_ref: if structs.is_empty() {
                                    type_ref.into()
                                } else {
                                    DMLResolvedType::Dummy(
                                        *type_ref.span())
                                },
                                name: name.clone()
                            })
                        },
                        MethodArgument::Inline(name) =>
                            DMLMethodArg::Inline(name.clone())
                    }).collect()
}

pub fn eval_method_returns(returns: &[DMLType], report: &mut Vec<DMLError>)
                       -> Vec<DMLResolvedType> {
    returns.iter().map(|ret| {
        let (structs, type_ref) = eval_type_simple(ret, (), ());
        for s in &structs {
            report.push(
                DMLError {
                    span: *s.span(),
                    description:
                    "Cannot use anonymous struct type in return type".into(),
                    related: vec![],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
        }
        if structs.is_empty() {
            type_ref.into()
        } else {
            DMLResolvedType::Dummy(*type_ref.span())
        }
    }).collect()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodDecl {
    pub name: DMLString,
    pub modifier: MethodModifier,
    pub independent: bool,
    pub default: bool,
    pub throws: bool,
    pub method_args: Vec<DMLMethodArg>,
    pub return_types: Vec<DMLResolvedType>,
    pub body: Statement,
    pub span: ZeroSpan,
}

impl SymbolContainer for MethodDecl {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.method_args.to_symbols();
        symbols.append(&mut self.body.symbols());
        symbols
    }
}

impl MaybeAbstract for MethodDecl {
    fn is_abstract(&self) -> bool {
        matches!(self.body.as_ref(), StatementKind::Empty(_))
    }
}

impl DMLNamedMember for MethodDecl {
    fn identity(&self) -> &str {
        &self.name.val
    }

    fn kind_name(&self) -> &'static str {
        "method"
    }

    fn location(&self) -> &ZeroSpan {
        &self.name.span
    }
}

impl DeclarationSpan for MethodDecl {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

pub trait MethodDeclaration : DMLNamedMember + MaybeAbstract {
    fn is_shared(&self) -> bool;
    fn throws(&self) -> bool;
    fn fully_typed(&self) -> bool {
        for arg in self.args() {
            if matches!(arg, DMLMethodArg::Inline(_)) {
                return false;
            }
        }
        true
    }
    fn args(&self) -> &Vec<DMLMethodArg>;
    fn returns(&self) -> &Vec<DMLResolvedType>;

    fn check_override<T>(&self,
                         overridden: &T,
                         report: &mut Vec<DMLError>)
    where
        T : MethodDeclaration,
    {
        if self.throws() && !overridden.throws() {
            report.push(DMLError {
                span: *self.location(),
                description: "Throwing method cannot override \
                              non-throwing method".to_string(),
                related: vec![(
                    *overridden.location(),
                    "Non-throwing method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        } else if !self.throws() && overridden.throws() {
            report.push(DMLError {
                span: *self.location(),
                description: "Non-throwing method cannot override \
                              throwing method".to_string(),
                related: vec![(*overridden.location(),
                               "Throwing method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        if self.args().len() != overridden.args().len() {
            report.push(DMLError {
                span: *self.location(),
                description: "Wrong number of arguments in method override"
                    .to_string(),
                related: vec![(*overridden.location(),
                               "Overridden method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        for (arg1, arg2) in self.args().iter().zip(overridden.args()) {
            if !arg1.equivalent(arg2) {
                report.push(DMLError {
                    span: *arg1.span(),
                    description: "Mismatching argument type in \
                                  method override".to_string(),
                    related: vec![(*arg2.span(),
                                   "Corresponding argument declared here"
                                   .to_string())],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
        }

        if self.returns().len() != overridden.returns().len() {
            report.push(DMLError {
                span: *self.location(),
                description: "Wrong number of return types in method override"
                    .to_string(),
                related: vec![(
                    *overridden.location(),
                    "Overridden method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        for (type1, type2) in self.returns().iter().zip(overridden.returns()) {
            if !type1.equivalent(type2) {
                report.push(DMLError {
                    span: *type1.span(),
                    description: "Mismatching return type in \
                                  method override".to_string(),
                    related: vec![(*type2.span(),
                                   "Corresponding return type declared here"
                                   .to_string())],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
        }
    }
}

impl MethodDeclaration for MethodDecl {
    fn is_shared(&self) -> bool {
        matches!(self.modifier, MethodModifier::Shared)
    }

    fn throws(&self) -> bool {
        self.throws
    }

    fn args(&self) -> &Vec<DMLMethodArg> {
        &self.method_args
    }

    fn returns(&self) -> &Vec<DMLResolvedType> {
        &self.return_types
    }
}

impl MethodDecl {
    pub fn from_content(content: &Method, report: &mut Vec<DMLError>)
                        -> MethodDecl {
        MethodDecl {
            name: content.object.name.clone(),
            modifier: content.modifier,
            independent: content.independent,
            default: content.default,
            body: content.body.clone(),
            throws: content.throws,
            method_args: eval_method_args(&content.arguments, report),
            return_types: eval_method_returns(&content.returns, report),
            span: *content.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DMLMethodRef {
    TraitMethod(Arc<DMLTrait>, MethodDecl),
    ConcreteMethod(DMLConcreteMethod),
}

impl DMLMethodRef {
    pub fn get_decl(&self) -> &MethodDecl {
        match self {
            Self::TraitMethod(_, decl) => decl,
            Self::ConcreteMethod(conc) => &conc.decl,
        }
    }

    pub fn get_default(&self) -> Option<Arc<DMLMethodRef>> {
        match self {
            Self::TraitMethod(_, _) => None,
            Self::ConcreteMethod(decl) => decl.default_call
                .as_ref().map(Arc::clone),
        }
    }

    pub fn get_all_defs(&self) -> Vec<ZeroSpan> {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => if !decl.is_abstract() {
                vec![*decl.location()]
            } else {
                vec![]
            },
            DMLMethodRef::ConcreteMethod(concmeth) => concmeth.get_all_defs(),
        }
    }
    pub fn get_all_decls(&self) -> Vec<ZeroSpan> {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => if decl.is_abstract() {
                vec![*decl.location()]
            } else {
                vec![]
            },
            DMLMethodRef::ConcreteMethod(concmeth) => concmeth.get_all_defs(),
        }
    }
    pub fn get_base(&self) -> MethodDecl {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => {
                decl.clone()
            },
            DMLMethodRef::ConcreteMethod(meth) => {
                if let Some(default) = &meth.default_call {
                    default.get_base()
                } else {
                    meth.decl.clone()
                }
            }
        }
    }
}

impl MaybeAbstract for DMLMethodRef {
    fn is_abstract(&self) -> bool {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.is_abstract(),
            DMLMethodRef::ConcreteMethod(decl) => decl.is_abstract(),
        }
    }
}

impl MethodDeclaration for DMLMethodRef {
    fn is_shared(&self) -> bool {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.is_shared(),
            DMLMethodRef::ConcreteMethod(decl) => decl.is_shared(),
        }
    }

    fn throws(&self) -> bool {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.throws(),
            DMLMethodRef::ConcreteMethod(decl) => decl.throws(),
        }
    }

    fn args(&self) -> &Vec<DMLMethodArg> {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.args(),
            DMLMethodRef::ConcreteMethod(decl) => decl.args(),
        }
    }

    fn returns(&self) -> &Vec<DMLResolvedType> {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.returns(),
            DMLMethodRef::ConcreteMethod(decl) => decl.returns(),
        }
    }
}

impl DMLNamedMember for DMLMethodRef {
    fn identity(&self) -> &str {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.identity(),
            DMLMethodRef::ConcreteMethod(decl) => decl.identity(),
        }
    }

    fn kind_name(&self) -> &'static str {
        match self {
            DMLMethodRef::TraitMethod(_, _) => "shared method",
            DMLMethodRef::ConcreteMethod(_) => "method",
        }
    }

    fn location(&self) -> &ZeroSpan {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.location(),
            DMLMethodRef::ConcreteMethod(decl) => decl.location(),
        }
    }
}

impl DeclarationSpan for DMLMethodRef {
    fn span(&self) -> &ZeroSpan {
        match self {
            DMLMethodRef::TraitMethod(_, decl) => decl.span(),
            DMLMethodRef::ConcreteMethod(decl) => decl.span(),
        }
    }
 }

// This is roughly equivalent with a non-codegenned method in DMLC
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLConcreteMethod {
    pub decl: MethodDecl,
    // where the default call goes (None if invalid)
    pub default_call: Option<Arc<DMLMethodRef>>,
}

impl DMLConcreteMethod {
    pub fn get_all_defs(&self) -> Vec<ZeroSpan> {
        let mut defs = self.default_call.as_ref().map_or_else(
            ||vec![],
            |default|default.get_all_defs());
        if !self.decl.is_abstract() {
            defs.push(*self.decl.location());
        }
        defs
    }
    pub fn get_all_decls(&self) -> Vec<ZeroSpan> {
        let mut decls = self.default_call.as_ref().map_or_else(
            ||vec![],
            |default|default.get_all_decls());
        if self.decl.is_abstract() {
            decls.push(*self.decl.location());
        }
        decls
    }
}

impl DMLNamedMember for DMLConcreteMethod {
    fn identity(&self) -> &str {
        self.decl.identity()
    }

    fn kind_name(&self) -> &'static str {
        self.decl.kind_name()
    }

    fn location(&self) -> &ZeroSpan {
        self.decl.location()
    }
}

impl MethodDeclaration for DMLConcreteMethod {
    fn is_shared(&self) -> bool {
        self.decl.is_shared()
    }

    fn throws(&self) -> bool {
        self.decl.throws()
    }

    fn args(&self) -> &Vec<DMLMethodArg> {
        self.decl.args()
    }

    fn returns(&self) -> &Vec<DMLResolvedType> {
        self.decl.returns()
    }
}

impl MaybeAbstract for DMLConcreteMethod {
    fn is_abstract(&self) -> bool {
        self.decl.is_abstract()
    }
}

impl DeclarationSpan for DMLConcreteMethod {
    fn span(&self) -> &ZeroSpan {
        self.decl.span()
    }
}

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
    fn is_default(&self) -> bool;
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

    // TODO: Let this take multiple overridden methods, as there are allowed cases
    // of that and we can provide better (fewer, more compact) reports, bundling
    // together similar ones
    fn check_override<T>(&self,
                         overridden: &T,
                         report: &mut Vec<DMLError>)
    where
        T : MethodDeclaration,
    {
        if self.is_shared() && !overridden.is_shared() {
            report.push(DMLError {
                span: *self.location(),
                description:
                    "Shared method cannot override non-shared method".to_string(),
                related: vec![(
                    *overridden.location(),
                    "Overridden method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }

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

    fn is_default(&self) -> bool {
        self.default
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

// Note: A method can have a template ref even if not shared, it just means
// it was declared top-level within a template
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DMLMethodRef {
    pub template_ref: Option<Arc<DMLTrait>>,
    pub concrete_decl: DMLConcreteMethod,
}

impl DMLMethodRef {
    pub fn get_disambiguation_name(&self) -> Option<String> {
        self.template_ref.as_ref().map(
            |trt|format!("templates.{}.{}",
                         trt.name, self.identity()))
    }

    pub fn get_decl(&self) -> &MethodDecl {
        &self.concrete_decl.decl
    }

    pub fn get_default(&self) -> Option<&DefaultCallReference> {
        self.concrete_decl.default_call.as_ref()
    }

    pub fn get_all_defs(&self) -> Vec<ZeroSpan> {
        self.concrete_decl.get_all_defs()
    }
    pub fn get_all_decls(&self) -> Vec<ZeroSpan> {
        self.concrete_decl.get_all_decls()
    }
    // Invariant: Return is non-empty
    pub fn get_bases(&self) -> Vec<MethodDecl> {
        let to_return = if let Some(default_refs) = &self.concrete_decl.default_call {
            default_refs.flat_refs().into_iter().flat_map(|m|m.get_bases()).collect()
        } else {
            vec![self.concrete_decl.decl.clone()]
        };
        assert!(!to_return.is_empty());
        to_return
    }
}

impl MaybeAbstract for DMLMethodRef {
    fn is_abstract(&self) -> bool {
        self.concrete_decl.is_abstract()
    }
}

impl MethodDeclaration for DMLMethodRef {
    fn is_shared(&self) -> bool {
        self.concrete_decl.is_shared()
    }

    fn is_default(&self) -> bool {
        self.concrete_decl.is_default()
    }

    fn throws(&self) -> bool {
        self.concrete_decl.throws()
    }

    fn args(&self) -> &Vec<DMLMethodArg> {
        self.concrete_decl.args()
    }

    fn returns(&self) -> &Vec<DMLResolvedType> {
        self.concrete_decl.returns()
    }
}

impl DMLNamedMember for DMLMethodRef {
    fn identity(&self) -> &str {
        self.concrete_decl.identity()
    }

    fn kind_name(&self) -> &'static str {
        self.concrete_decl.kind_name()
    }

    fn location(&self) -> &ZeroSpan {
        self.concrete_decl.location()
    }
}

impl DeclarationSpan for DMLMethodRef {
    fn span(&self) -> &ZeroSpan {
        self.concrete_decl.span()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefaultCallReference {
    Abstract(MethodDecl),
    Valid(Arc<DMLMethodRef>),
    Ambiguous(Vec<Arc<DMLMethodRef>>),
}

impl DefaultCallReference {
    pub fn get_bases(&self) -> Vec<MethodDecl> {
        match self {
            DefaultCallReference::Abstract(method) => vec![method.clone()],
            DefaultCallReference::Valid(method) => method.get_bases(),
            DefaultCallReference::Ambiguous(methods) => 
                methods.iter().flat_map(|m|m.get_bases()).collect(),
        }
    }

    pub fn get_all_decls(&self) -> Vec<ZeroSpan> {
        match self {
            DefaultCallReference::Abstract(method) => vec![*method.location()],
            DefaultCallReference::Valid(method) => method.get_all_decls(),
            DefaultCallReference::Ambiguous(methods) =>
                methods.iter().flat_map(|m|m.get_all_decls()).collect(),
        }
    }

    pub fn get_all_defs(&self) -> Vec<ZeroSpan> {
        match self {
            DefaultCallReference::Abstract(_) => vec![],
            DefaultCallReference::Valid(method) => method.get_all_defs(),
            DefaultCallReference::Ambiguous(methods) =>
                methods.iter().flat_map(|m|m.get_all_defs()).collect(),
        }
    }

    pub fn as_valid(&self) -> Option<&Arc<DMLMethodRef>> {
        match self {
            DefaultCallReference::Valid(method) => Some(method),
            DefaultCallReference::Ambiguous(_) => None,
            DefaultCallReference::Abstract(_) => None,
        }
    }

    pub fn flat_refs(&self) -> Vec<&Arc<DMLMethodRef>> {
        match self {
            DefaultCallReference::Abstract(_) => vec![],
            DefaultCallReference::Valid(method) => vec![method],
            DefaultCallReference::Ambiguous(methods) =>
                methods.iter().collect()
        }
    }
}

// This is roughly equivalent with a non-codegenned method in DMLC
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLConcreteMethod {
    pub decl: MethodDecl,
    // where the default call goes (None if nothing was overriding)
    pub default_call: Option<DefaultCallReference>,
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

    fn is_default(&self) -> bool {
        self.decl.default
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

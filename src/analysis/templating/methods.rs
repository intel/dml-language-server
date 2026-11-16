//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::sync::Arc;
use lsp_types::DiagnosticSeverity;
use std::collections::HashMap;

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::symbols::{DMLSymbolKind, MakeSymbolContainer,
                               StructureSymbol, SymbolContainer};
use crate::analysis::structure::expressions::{DMLString, ExpressionKind,
                                              FunctionCall, Initializer,
                                              SingleInitializer};
use crate::analysis::structure::types::DMLType;
use crate::analysis::structure::objects::{MaybeAbstract, MethodArgument,
                                          MethodModifier, Method,
                                          VariableDeclKind};
use crate::analysis::structure::statements::{Assigner, ForPre, Statement,
                                             StatementKind, SwitchCase};
use crate::analysis::{DeclarationSpan, DMLNamed, DMLError};
use crate::analysis::templating::Declaration;
use crate::analysis::templating::evaluation::{Evaluatable, EvaluatedValue,
                                              EvaluationContext,
                                              EvaluationOrigin,
                                              EvaluationResult,
                                              EvaluationSlot,
                                              ExpressionResolver};
use crate::analysis::templating::objects::{DMLCompositeObject, DMLNamedMember};
use crate::analysis::templating::types::{check_storage_compatibility,
                                        eval_type_simple,
                                        eval_type_with_expression,
                                        DMLConcreteType,
                                        DMLStructType,
                                        DMLResolvedType,
                                        StorageCompatibility, StorageValue,
                                        TypeDatabase};
use crate::analysis::templating::traits::{lookup_impl, DMLTrait};

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
                decl1.type_ref.equivalent_unqualified(&decl2.type_ref),
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

pub fn eval_method_args(args: &[MethodArgument], types: &TypeDatabase,
                        report: &mut Vec<DMLError>)
                    -> Vec<DMLMethodArg> {
    args.iter().map(|arg|
                    match arg {
                        MethodArgument::Typed(name, typed) => {
                            types.report_unknown_types(typed, report);
                            let (structs, type_ref) = eval_type_simple(
                                typed, types);
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
                                type_syntax: typed.clone(),
                                name: name.clone()
                            })
                        },
                        MethodArgument::Inline(name) =>
                            DMLMethodArg::Inline(name.clone())
                    }).collect()
}

pub fn eval_method_returns(returns: &[DMLType], types: &TypeDatabase,
                           report: &mut Vec<DMLError>)
                       -> Vec<DMLResolvedType> {
    returns.iter().map(|ret| {
        types.report_unknown_types(ret, report);
        let (structs, type_ref) = eval_type_simple(ret, types);
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

#[derive(Clone)]
struct MethodBinding {
    typed: Option<DMLConcreteType>,
    declaration_span: ZeroSpan,
    writable: bool,
}

type MethodEnvironment = HashMap<String, MethodBinding>;

fn evaluated_storage_value(slot: &EvaluationSlot) -> Option<StorageValue> {
    slot.value.as_ref().and_then(|value|match value {
        EvaluatedValue::Int(value) => Some(StorageValue::Integer(*value)),
        _ => None,
    })
}

struct MethodExpressionResolver<'a> {
    environment: &'a MethodEnvironment,
    method: &'a DMLMethodRef,
    object: &'a DMLCompositeObject,
    types: &'a TypeDatabase,
}

impl MethodExpressionResolver<'_> {
    fn resolve_type_with(
        &mut self,
        typed: &DMLType,
        context: &mut EvaluationContext,
        report: &mut Vec<DMLError>,
    ) -> (Vec<DMLStructType>, DMLConcreteType) {
        eval_type_with_expression(typed, self.types, |expression| {
            let evaluated = expression.evaluate_with(context, self, report);
            match evaluated.slot().and_then(|slot|slot.typed.as_ref()) {
                Some(DMLResolvedType::Actual(typed)) => Some(typed.clone()),
                _ => None,
            }
        })
    }

    fn resolve_call(&self, call: &FunctionCall) -> Option<Arc<DMLMethodRef>> {
        if let ExpressionKind::MemberLiteral(member) = call.function.as_ref() {
            return self.object.resolved_method_calls
                .get(&member.right.span).map(Arc::clone);
        }
        let ExpressionKind::Identifier(identifier) = call.function.as_ref()
        else { return None; };
        if identifier.name.val == "default" {
            return self.method.get_default()
                .and_then(DefaultCallReference::as_valid)
                .map(Arc::clone);
        }
        self.object.get_method(identifier.name.val.as_str())
            .or_else(|| {
                let lexical_trait = self.method.template_ref.as_ref()?;
                let implementation = lookup_impl(
                    lexical_trait, &identifier.name.val)?;
                let concrete = implementation.get_defined_method(
                    &identifier.name.val)?;
                Some(Arc::new(DMLMethodRef {
                    template_ref: Some(Arc::clone(implementation)),
                    concrete_decl: concrete.clone(),
                }))
            })
    }

    fn check_call_arguments(
        &mut self,
        call: &FunctionCall,
        called: &DMLMethodRef,
        context: &mut EvaluationContext,
        report: &mut Vec<DMLError>,
    ) {
        if call.arguments.len() != called.args().len() {
            report.push(DMLError {
                span: call.span,
                description: format!(
                    "Method '{}' expects {} arguments but call provides {}",
                    called.identity(), called.args().len(), call.arguments.len()),
                related: vec![(*called.location(),
                    "Method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        for (initializer, argument) in call.arguments.iter().zip(called.args()) {
            let (SingleInitializer::Expression(expression),
                 DMLMethodArg::Typed(declaration)) = (initializer, argument)
            else {
                continue;
            };
            let DMLResolvedType::Actual(target) = &declaration.type_ref else {
                continue;
            };
            let evaluated = expression.evaluate_with(context, self, report);
            let Some(slot) = evaluated.slot() else {
                continue;
            };
            let Some(DMLResolvedType::Actual(source)) = &slot.typed else {
                continue;
            };
            let value = evaluated_storage_value(slot);
            let compatibility = check_storage_compatibility(
                target, source, value);
            let target_name = target.description(self.types);
            let source_name = source.description(self.types);
            let description = match compatibility {
                StorageCompatibility::Incompatible => format!(
                    "Cannot pass value of type '{}' to argument '{}' of method \
                     '{}' with type '{}'",
                    source_name, declaration.name.val, called.identity(),
                    target_name),
                StorageCompatibility::Truncates(truncation) => format!(
                    "Value of type '{}' passed to argument '{}' of method '{}' \
                     may be truncated when stored in type '{}'{}",
                    source_name, declaration.name.val, called.identity(),
                    target_name, truncation.target_suffix()),
                StorageCompatibility::Compatible => continue,
            };
            let error = DMLError {
                span: *expression.span(),
                description,
                related: vec![(declaration.name.span,
                    "Argument declared here".to_string())],
                severity: Some(match compatibility {
                    StorageCompatibility::Incompatible =>
                        DiagnosticSeverity::ERROR,
                    _ => DiagnosticSeverity::WARNING,
                }),
            };
            match compatibility {
                StorageCompatibility::Truncates(_) => if false {
                    report.push(error);
                },
                _ => report.push(error),
            }
        }
    }
}

impl ExpressionResolver for MethodExpressionResolver<'_> {
    fn identifier_type(&mut self, name: &str) -> Option<DMLResolvedType> {
        self.environment.get(name)
            .and_then(|binding|binding.typed.clone())
            .map(DMLResolvedType::Actual)
    }

    fn resolve_type(
        &mut self,
        typed: &DMLType,
        context: &mut EvaluationContext,
        report: &mut Vec<DMLError>,
    ) -> Option<DMLResolvedType> {
        let (anonymous, resolved) = self.resolve_type_with(
            typed, context, report);
        anonymous.is_empty().then_some(DMLResolvedType::Actual(resolved))
    }

    fn function_call(
        &mut self,
        call: &FunctionCall,
        context: &mut EvaluationContext,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let Some(called) = self.resolve_call(call) else {
            return EvaluationResult::UnknownArity;
        };
        self.check_call_arguments(call, &called, context, report);
        let slots = called.returns().iter().cloned()
            .map(EvaluationSlot::from_type).collect();
        EvaluationResult::Known {
            slots,
            origin: Some(EvaluationOrigin {
                identity: called.identity().to_string(),
                location: *called.location(),
            }),
        }
    }
}

fn check_local_variable_types(statement: &Statement,
                              arguments: &[DMLMethodArg],
                              method: &DMLMethodRef,
                              object: &DMLCompositeObject,
                              types: &TypeDatabase,
                              report: &mut Vec<DMLError>) {
    struct ExpressionStorage {
        source: DMLConcreteType,
        value: Option<StorageValue>,
        compatibility: StorageCompatibility,
    }

    struct ReturnSource {
        span: ZeroSpan,
        slot: EvaluationSlot,
    }

    fn assignment_binding<'a>(
        identifier: &crate::analysis::structure::expressions::Identifier,
        environment: &'a MethodEnvironment,
        report: &mut Vec<DMLError>,
    ) -> Option<&'a MethodBinding> {
        let binding = environment.get(&identifier.name.val)?;
        if !binding.writable {
            report.push(DMLError {
                span: identifier.name.span,
                description: format!(
                    "Cannot assign to inline method argument '{}'",
                    identifier.name.val),
                related: vec![(binding.declaration_span,
                    "Inline argument declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
            return None;
        }
        Some(binding)
    }

    fn evaluate_expression(
        expression: &ExpressionKind,
        environment: &MethodEnvironment,
        method: &DMLMethodRef,
        object: &DMLCompositeObject,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) -> EvaluationResult {
        let mut context = EvaluationContext::new();
        let mut resolver = MethodExpressionResolver {
            environment, method, object, types,
        };
        expression.evaluate_with(&mut context, &mut resolver, report)
    }

    fn evaluate_type(
        typed: &DMLType,
        environment: &MethodEnvironment,
        method: &DMLMethodRef,
        object: &DMLCompositeObject,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) -> (Vec<DMLStructType>, DMLConcreteType) {
        let mut context = EvaluationContext::new();
        let mut resolver = MethodExpressionResolver {
            environment, method, object, types,
        };
        resolver.resolve_type_with(typed, &mut context, report)
    }

    fn slot_storage(slot: &EvaluationSlot)
                    -> Option<(DMLConcreteType, Option<StorageValue>)> {
        let Some(DMLResolvedType::Actual(source)) = &slot.typed else {
            return None;
        };
        Some((source.clone(), evaluated_storage_value(slot)))
    }

    fn check_expression_storage(
        target: &DMLConcreteType,
        expression: &ExpressionKind,
        environment: &MethodEnvironment,
        method: &DMLMethodRef,
        object: &DMLCompositeObject,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) -> Option<ExpressionStorage> {
        let evaluated = evaluate_expression(
            expression, environment, method, object, types, report);
        let (source, value) = slot_storage(evaluated.slot()?)?;
        let compatibility = check_storage_compatibility(
            target, &source, value);
        Some(ExpressionStorage { source, value, compatibility })
    }

    fn expression_sources(
        expression: &ExpressionKind,
        object: &DMLCompositeObject,
        method: &DMLMethodRef,
        environment: &MethodEnvironment,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) -> Option<Vec<ReturnSource>> {
        match evaluate_expression(
            expression, environment, method, object, types, report)
        {
            EvaluationResult::Known { slots, .. } => Some(slots.into_iter()
                .map(|slot|ReturnSource {
                    span: *expression.span(), slot,
                }).collect()),
            EvaluationResult::UnknownArity => None,
        }
    }

    fn check_return(
        return_statement: &crate::analysis::structure::statements::Return,
        method: &DMLMethodRef,
        object: &DMLCompositeObject,
        environment: &MethodEnvironment,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) {
        let sources = if let Some(initializer) = &return_statement.ret {
            match initializer {
                Initializer::Single(SingleInitializer::Expression(expression)) => {
                    let Some(sources) = expression_sources(
                        expression, object, method, environment, types, report)
                    else {
                        return;
                    };
                    sources
                },
                Initializer::Tuple(tuple) => {
                    let mut sources = vec![];
                    for initializer in &tuple.list {
                        match initializer {
                            SingleInitializer::Expression(expression) => {
                                let Some(mut inferred) = expression_sources(
                                    expression, object, method,
                                    environment, types, report)
                                else {
                                    return;
                                };
                                sources.append(&mut inferred);
                            },
                            _ => sources.push(ReturnSource {
                                span: *initializer.span(),
                                slot: EvaluationSlot::unknown(),
                            }),
                        }
                    }
                    sources
                },
                Initializer::Single(initializer) =>
                    vec![ReturnSource {
                        span: *initializer.span(),
                        slot: EvaluationSlot::unknown(),
                    }],
            }
        } else {
            vec![]
        };
        let declaration = method.get_decl();
        if sources.len() != declaration.return_types.len() {
            report.push(DMLError {
                span: return_statement.span,
                description: format!(
                    "Method '{}' declares {} return values but return statement \
                     provides {}",
                    method.identity(), declaration.return_types.len(),
                    sources.len()),
                related: vec![(declaration.name.span,
                    "Method declared here".to_string())],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        for (source, target) in
            sources.into_iter().zip(&declaration.return_types)
        {
            let Some((source_type, value)) = slot_storage(&source.slot) else {
                continue;
            };
            let DMLResolvedType::Actual(target) = target else {
                continue;
            };
            let compatibility = check_storage_compatibility(
                target, &source_type, value);
            let target_name = target.description(types);
            let source_name = source_type.description(types);
            let description = match compatibility {
                StorageCompatibility::Incompatible => format!(
                    "Cannot return value of type '{}' from method '{}' with \
                     return type '{}'",
                    source_name, method.identity(), target_name),
                StorageCompatibility::Truncates(truncation) => format!(
                    "Value of type '{}' returned from method '{}' may be \
                     truncated when stored in return type '{}'{}",
                    source_name, method.identity(), target_name,
                    truncation.target_suffix()),
                StorageCompatibility::Compatible => continue,
            };
            let error = DMLError {
                span: source.span,
                description,
                related: vec![(*target.span(),
                    "Return type declared here".to_string())],
                severity: Some(match compatibility {
                    StorageCompatibility::Incompatible =>
                        DiagnosticSeverity::ERROR,
                    _ => DiagnosticSeverity::WARNING,
                }),
            };
            match compatibility {
                StorageCompatibility::Truncates(_) => if false {
                    report.push(error);
                },
                _ => report.push(error),
            }
        }
    }

    fn storage_diagnostic(
        target: &DMLConcreteType,
        expression: &ExpressionKind,
        storage: ExpressionStorage,
        variable_name: &str,
        declaration_span: ZeroSpan,
        initializer: bool,
        types: &TypeDatabase,
    ) -> Option<DMLError> {
        let target_name = target.description(types);
        let source_name = storage.source.description(types);
        let related = vec![(declaration_span,
            format!("Local variable '{}' declared as '{}' here",
                    variable_name, target_name))];

        match (storage.compatibility, storage.value) {
            (StorageCompatibility::Incompatible, _) => Some(DMLError {
                span: *expression.span(),
                description: if initializer {
                    format!("Cannot initialize local variable '{}' of type '{}' \
                             with value of type '{}'",
                            variable_name, target_name, source_name)
                } else {
                    format!("Cannot assign value of type '{}' to local variable \
                             '{}' of type '{}'",
                            source_name, variable_name, target_name)
                },
                related,
                severity: Some(DiagnosticSeverity::ERROR),
            }),
            (StorageCompatibility::Truncates(truncation), Some(_)) => {
                if false {
                    Some(DMLError {
                        span: *expression.span(),
                        description: if initializer {
                            format!("Initializer for local variable '{}' does not fit in \
                                     type '{}'{} and will be truncated",
                                    variable_name, target_name,
                                    truncation.target_suffix())
                        } else {
                            format!("Value assigned to local variable '{}' does not fit in \
                                     type '{}'{} and will be truncated",
                                    variable_name, target_name,
                                    truncation.target_suffix())
                        },
                        related,
                        severity: Some(DiagnosticSeverity::WARNING),
                    })
                } else {
                    None
                }
            },
            (StorageCompatibility::Truncates(truncation), None) =>
                if false {
                    Some(DMLError {
                        span: *expression.span(),
                        description: if initializer {
                            format!("Initializer for local variable '{}' of type '{}' \
                                     may be truncated when stored in type '{}'{}",
                                    variable_name, source_name, target_name,
                                    truncation.target_suffix())
                        } else {
                            format!("Assignment to local variable '{}' from type '{}' \
                                     may be truncated when stored in type '{}'{}",
                                    variable_name, source_name, target_name,
                                    truncation.target_suffix())
                        },
                        related,
                        severity: Some(DiagnosticSeverity::WARNING),
                    })
                } else {
                    None
                },
            (StorageCompatibility::Compatible, _) => None,
        }
    }

    fn return_binding_diagnostic(
        target: &DMLConcreteType,
        source: &DMLConcreteType,
        expression: &ExpressionKind,
        origin: &EvaluationOrigin,
        variable_name: &str,
        declaration_span: ZeroSpan,
        initializer: bool,
        types: &TypeDatabase,
    ) -> Option<DMLError> {
        let compatibility = check_storage_compatibility(target, source, None);
        let target_name = target.description(types);
        let source_name = source.description(types);
        let description = match compatibility {
            StorageCompatibility::Incompatible => if initializer {
                format!("Cannot initialize local variable '{}' of type '{}' with \
                         return value of type '{}' from method '{}'",
                        variable_name, target_name, source_name,
                        origin.identity)
            } else {
                format!("Cannot assign return value of type '{}' from method '{}' \
                         to local variable '{}' of type '{}'",
                        source_name, origin.identity, variable_name,
                        target_name)
            },
            StorageCompatibility::Truncates(truncation) => if initializer {
                format!("Return value of type '{}' from method '{}' may be \
                         truncated when stored in local variable '{}' of type \
                         '{}'{}",
                        source_name, origin.identity, variable_name,
                        target_name, truncation.target_suffix())
            } else {
                format!("Return value of type '{}' from method '{}' may be \
                         truncated when assigned to local variable '{}' of type \
                         '{}'{}",
                        source_name, origin.identity, variable_name,
                        target_name, truncation.target_suffix())
            },
            StorageCompatibility::Compatible => return None,
        };
        let error = DMLError {
            span: *expression.span(),
            description,
            related: vec![(declaration_span,
                format!("Local variable '{}' declared here", variable_name))],
            severity: Some(match compatibility {
                StorageCompatibility::Incompatible => DiagnosticSeverity::ERROR,
                _ => DiagnosticSeverity::WARNING,
            }),
        };
        match compatibility {
            StorageCompatibility::Truncates(_) => if false {
                Some(error)
            } else {
                None
            },
            _ => Some(error),
        }
    }

    fn array_storage_diagnostic(
        element_type: &DMLConcreteType,
        array_type: &DMLConcreteType,
        expression: &ExpressionKind,
        storage: ExpressionStorage,
        variable_name: &str,
        element_index: usize,
        declaration_span: ZeroSpan,
        types: &TypeDatabase,
    ) -> Option<DMLError> {
        let element_name = element_type.description(types);
        let source_name = storage.source.description(types);
        let related = vec![(declaration_span,
            format!("Local array '{}' declared as '{}' here",
                    variable_name, array_type.description(types)))];
        let description = match (storage.compatibility, storage.value) {
            (StorageCompatibility::Incompatible, _) => format!(
                "Cannot initialize element {} of local array '{}' of type '{}' \
                 with value of type '{}'",
                element_index, variable_name, element_name, source_name),
            (StorageCompatibility::Truncates(truncation), Some(_)) => format!(
                "Initializer for element {} of local array '{}' does not fit in \
                 type '{}'{} and will be truncated",
                element_index, variable_name, element_name,
                truncation.target_suffix()),
            (StorageCompatibility::Truncates(truncation), None) => format!(
                "Initializer for element {} of local array '{}' of type '{}' may \
                 be truncated when stored in type '{}'{}",
                element_index, variable_name, source_name, element_name,
                truncation.target_suffix()),
            (StorageCompatibility::Compatible, _) => return None,
        };
        let error = DMLError {
            span: *expression.span(),
            description,
            related,
            severity: Some(match storage.compatibility {
                StorageCompatibility::Incompatible => DiagnosticSeverity::ERROR,
                _ => DiagnosticSeverity::WARNING,
            }),
        };
        match storage.compatibility {
            StorageCompatibility::Truncates(_) => if false {
                Some(error)
            } else {
                None
            },
            _ => Some(error),
        }
    }

    fn check_variable(variable: &crate::analysis::structure::objects::Variable,
                      environment: &mut MethodEnvironment,
                      method: &DMLMethodRef,
                      object: &DMLCompositeObject,
                      types: &TypeDatabase,
                      report: &mut Vec<DMLError>) {
        let call_binding = if let [initializer] = variable.values.as_slice() {
            initializer.expression().and_then(|expression| {
                if !matches!(expression.as_ref(), ExpressionKind::FunctionCall(_)) {
                    return None;
                }
                match evaluate_expression(
                    expression, environment, method, object, types, report)
                {
                    EvaluationResult::Known {
                        slots,
                        origin: Some(origin),
                    } => Some((expression, origin, slots)),
                    _ => None,
                }
            })
        } else {
            None
        };
        if let Some((expression, origin, slots)) = &call_binding {
            if slots.len() != variable.vars.len() {
                report.push(DMLError {
                    span: *expression.span(),
                    description: format!(
                        "Method '{}' returns {} values but declaration binds {}",
                        origin.identity, slots.len(), variable.vars.len()),
                    related: vec![(origin.location,
                        "Method declared here".to_string())],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
        }
        for (index, declaration) in variable.vars.iter().enumerate() {
            if declaration.kind != VariableDeclKind::Local {
                continue;
            }
            types.report_unknown_types(&declaration.typed, report);
            let (anonymous, target) = evaluate_type(
                &declaration.typed, environment, method, object, types, report);
            if anonymous.is_empty() {
                if let Some((expression, origin, slots)) = &call_binding {
                    if let Some(source) = slots.get(index).and_then(|slot|
                        match &slot.typed {
                            Some(DMLResolvedType::Actual(source)) => Some(source),
                            _ => None,
                        })
                    {
                        if let Some(error) = return_binding_diagnostic(
                            &target, source, expression, origin,
                            &declaration.object.name.val,
                            declaration.object.name.span, true, types)
                        {
                            report.push(error);
                        }
                    }
                } else if let Some(initializer) = variable.values.get(index) {
                    let variable_name = &declaration.object.name.val;
                    if let Some(expression) = initializer.expression() {
                        if let Some(storage) = check_expression_storage(
                            &target, expression, environment, method, object,
                            types, report)
                        {
                            if let Some(error) = storage_diagnostic(
                                &target, expression, storage, variable_name,
                                declaration.object.name.span, true, types)
                            {
                                report.push(error);
                            }
                        }
                    } else if let (Some(element_type), Some(elements)) =
                        (target.array_element(), initializer.list())
                    {
                        if let Some(length) = target.array_length() {
                            if length != elements.len() as u64 {
                                report.push(DMLError {
                                    span: *initializer.span(),
                                    description: format!(
                                        "Local array '{}' has length {} but its \
                                         initializer has {} elements",
                                        variable_name, length, elements.len()),
                                    related: vec![(declaration.object.name.span,
                                        format!("Local array '{}' declared here",
                                                variable_name))],
                                    severity: Some(DiagnosticSeverity::ERROR),
                                });
                            }
                        }
                        for (element_index, initializer) in
                            elements.iter().enumerate()
                        {
                            let Some(expression) = initializer.expression() else {
                                continue;
                            };
                            let Some(storage) = check_expression_storage(
                                element_type, expression, environment, method,
                                object, types, report)
                            else {
                                continue;
                            };
                            if let Some(error) = array_storage_diagnostic(
                                element_type, &target, expression, storage,
                                variable_name, element_index,
                                declaration.object.name.span, types)
                            {
                                report.push(error);
                            }
                        }
                    }
                }
                environment.insert(
                    declaration.object.name.val.clone(),
                    MethodBinding {
                        typed: Some(target),
                        declaration_span: declaration.object.name.span,
                        writable: true,
                    });
            }
        }
    }

    fn check_assignment(
        assignment: &crate::analysis::structure::statements::AssignStatement,
        environment: &MethodEnvironment,
        method: &DMLMethodRef,
        object: &DMLCompositeObject,
        types: &TypeDatabase,
        report: &mut Vec<DMLError>,
    ) {
        fn check_pair(
            assignee: &ExpressionKind,
            expression: &ExpressionKind,
            environment: &MethodEnvironment,
            method: &DMLMethodRef,
            object: &DMLCompositeObject,
            types: &TypeDatabase,
            report: &mut Vec<DMLError>,
        ) {
            let ExpressionKind::Identifier(identifier) = assignee else {
                return;
            };
            let Some(binding) = assignment_binding(
                identifier, environment, report)
            else {
                return;
            };
            let Some(target) = binding.typed.as_ref() else {
                return;
            };
            let Some(storage) = check_expression_storage(
                target, expression, environment, method, object, types, report)
            else {
                return;
            };
            if let Some(error) = storage_diagnostic(
                target, expression, storage, &identifier.name.val,
                binding.declaration_span, false, types)
            {
                report.push(error);
            }
        }

        if let Assigner::Chain(expressions) = &assignment.assigner {
            if let [expression] = expressions.as_slice() {
                let call_binding = matches!(
                    expression.as_ref(), ExpressionKind::FunctionCall(_))
                    .then(||evaluate_expression(
                        expression, environment, method, object, types, report));
                if let Some(EvaluationResult::Known {
                    slots,
                    origin: Some(origin),
                }) = call_binding {
                    if assignment.assignees.len() != slots.len() {
                        report.push(DMLError {
                            span: *expression.span(),
                            description: format!(
                                "Method '{}' returns {} values but assignment \
                                 has {} targets",
                                origin.identity, slots.len(),
                                assignment.assignees.len()),
                            related: vec![(origin.location,
                                "Method declared here".to_string())],
                            severity: Some(DiagnosticSeverity::ERROR),
                        });
                    }
                    for (assignee, source) in
                        assignment.assignees.iter().zip(&slots)
                    {
                        let ExpressionKind::Identifier(identifier) =
                            assignee.as_ref()
                        else {
                            continue;
                        };
                        let Some(binding) = assignment_binding(
                            identifier, environment, report)
                        else {
                            continue;
                        };
                        let Some(target) = binding.typed.as_ref() else {
                            continue;
                        };
                        let Some(DMLResolvedType::Actual(source)) =
                            source.typed.as_ref()
                        else {
                            continue;
                        };
                        if let Some(error) = return_binding_diagnostic(
                            target, source, expression, &origin,
                            &identifier.name.val, binding.declaration_span,
                            false, types)
                        {
                            report.push(error);
                        }
                    }
                    return;
                }
            }
        }

        match &assignment.assigner {
            Assigner::Initializer(Initializer::Single(
                SingleInitializer::Expression(expression))) => {
                if let [assignee] = assignment.assignees.as_slice() {
                    check_pair(assignee, expression, environment, method,
                               object, types, report);
                }
            },
            Assigner::Initializer(Initializer::Tuple(tuple)) => {
                if assignment.assignees.len() != tuple.list.len() {
                    report.push(DMLError {
                        span: assignment.span,
                        description: format!(
                            "Simultaneous assignment has {} targets but \
                             initializer provides {} values",
                            assignment.assignees.len(), tuple.list.len()),
                        related: vec![],
                        severity: Some(DiagnosticSeverity::ERROR),
                    });
                    return;
                }
                for (assignee, initializer) in
                    assignment.assignees.iter().zip(&tuple.list)
                {
                    if let SingleInitializer::Expression(expression) = initializer {
                        check_pair(
                            assignee, expression, environment, method, object,
                            types, report);
                    }
                }
            },
            Assigner::Chain(expressions) => {
                let Some((last, targets)) = expressions.split_last() else {
                    return;
                };
                let evaluated = evaluate_expression(
                    last, environment, method, object, types, report);
                if assignment.assignees.len() > 1 && targets.is_empty()
                    && evaluated.slot().is_some()
                {
                    report.push(DMLError {
                        span: assignment.span,
                        description: format!(
                            "Simultaneous assignment has {} targets but \
                             initializer provides 1 value",
                            assignment.assignees.len()),
                        related: vec![],
                        severity: Some(DiagnosticSeverity::ERROR),
                    });
                    return;
                }
                let Some((mut source, mut value)) = evaluated.slot()
                    .and_then(slot_storage)
                else {
                    return;
                };
                let mut source_expression = last.as_ref();
                for target_expression in targets.iter().rev()
                    .chain(assignment.assignees.iter().rev())
                {
                    let ExpressionKind::Identifier(identifier) =
                        target_expression.as_ref()
                    else {
                        return;
                    };
                    let Some(binding) = assignment_binding(
                        identifier, environment, report)
                    else {
                        return;
                    };
                    let Some(target) = binding.typed.as_ref() else {
                        return;
                    };
                    let compatibility = check_storage_compatibility(
                        target, &source, value);
                    let storage = ExpressionStorage {
                        source: source.clone(), value, compatibility,
                    };
                    if let Some(error) = storage_diagnostic(
                        target, source_expression, storage,
                        &identifier.name.val, binding.declaration_span,
                        false, types)
                    {
                        report.push(error);
                    }
                    source = target.clone();
                    value = None;
                    source_expression = target_expression;
                }
            },
            _ => (),
        }
    }

    fn check_switch_cases(cases: &[SwitchCase],
                          environment: &mut MethodEnvironment,
                          method: &DMLMethodRef,
                          object: &DMLCompositeObject,
                          types: &TypeDatabase,
                          report: &mut Vec<DMLError>) {
        for case in cases {
            match case {
                SwitchCase::Statement(statement) =>
                    check_statement(statement, environment,
                                    method, object, types, report),
                SwitchCase::HashIf(hash_if) => {
                    let mut true_environment = environment.clone();
                    check_switch_cases(&hash_if.truecases, &mut true_environment,
                                       method, object, types, report);
                    let mut false_environment = environment.clone();
                    check_switch_cases(&hash_if.falsecases, &mut false_environment,
                                       method, object, types, report);
                },
                _ => (),
            }
        }
    }

    fn check_statement(statement: &Statement,
                       environment: &mut MethodEnvironment,
                       method: &DMLMethodRef,
                       object: &DMLCompositeObject,
                       types: &TypeDatabase,
                       report: &mut Vec<DMLError>) {
        match statement.as_ref() {
            StatementKind::Compound(compound) => {
                let mut block_environment = environment.clone();
                for statement in &compound.statements {
                    check_statement(statement, &mut block_environment,
                                    method, object, types, report);
                }
            },
            StatementKind::VariableDecl(variable) =>
                check_variable(
                    variable, environment, method, object, types, report),
            StatementKind::Assign(assignment) =>
                check_assignment(assignment, environment, method,
                                 object, types, report),
            StatementKind::Return(return_statement) =>
                check_return(return_statement, method, object,
                             environment, types, report),
            StatementKind::Expression(statement) => {
                let _ = evaluate_expression(
                    &statement.expression, environment, method, object,
                    types, report);
            },
            StatementKind::Switch(statement) =>
                check_switch_cases(&statement.cases, environment,
                                   method, object, types, report),
            StatementKind::If(statement) => {
                let mut branch_environment = environment.clone();
                check_statement(&statement.ifbody, &mut branch_environment,
                                method, object, types, report);
                if let Some(elsebody) = &statement.elsebody {
                    let mut branch_environment = environment.clone();
                    check_statement(elsebody, &mut branch_environment,
                                    method, object, types, report);
                }
            },
            StatementKind::HashIf(statement) => {
                let mut branch_environment = environment.clone();
                check_statement(&statement.ifbody, &mut branch_environment,
                                method, object, types, report);
                if let Some(elsebody) = &statement.elsebody {
                    let mut branch_environment = environment.clone();
                    check_statement(elsebody, &mut branch_environment,
                                    method, object, types, report);
                }
            },
            StatementKind::While(statement) => {
                let mut body_environment = environment.clone();
                check_statement(&statement.body, &mut body_environment,
                                method, object, types, report);
            },
            StatementKind::DoWhile(statement) => {
                let mut body_environment = environment.clone();
                check_statement(&statement.body, &mut body_environment,
                                method, object, types, report);
            },
            StatementKind::For(statement) => {
                let mut loop_environment = environment.clone();
                if let Some(ForPre::Declaration(variable)) = &statement.pre {
                    check_variable(variable, &mut loop_environment,
                                   method, object, types, report);
                }
                check_statement(&statement.body, &mut loop_environment,
                                method, object, types, report);
            },
            StatementKind::ForEach(statement) => {
                let mut body_environment = environment.clone();
                check_statement(&statement.body, &mut body_environment,
                                method, object, types, report);
            },
            StatementKind::TryCatch(statement) => {
                let mut try_environment = environment.clone();
                check_statement(&statement.tryblock, &mut try_environment,
                                method, object, types, report);
                let mut catch_environment = environment.clone();
                check_statement(&statement.catchblock, &mut catch_environment,
                                method, object, types, report);
            },
            StatementKind::HashSelect(statement) => {
                let mut select_environment = environment.clone();
                check_statement(&statement.selectbranch, &mut select_environment,
                                method, object, types, report);
                let mut else_environment = environment.clone();
                check_statement(&statement.elsebranch, &mut else_environment,
                                method, object, types, report);
            },
            _ => (),
        }
    }

    let mut environment = HashMap::new();
    for argument in arguments {
        match argument {
            DMLMethodArg::Typed(declaration) => {
                environment.insert(
                    declaration.name.val.clone(), MethodBinding {
                        typed: match &declaration.type_ref {
                            DMLResolvedType::Actual(resolved) =>
                                Some(resolved.clone()),
                            DMLResolvedType::Dummy(_) => None,
                        },
                        declaration_span: declaration.name.span,
                        writable: true,
                    });
            },
            DMLMethodArg::Inline(name) => {
                environment.insert(name.val.clone(), MethodBinding {
                    typed: None,
                    declaration_span: name.span,
                    writable: false,
                });
            },
        }
    }
    check_statement(
        statement, &mut environment, method, object, types, report);
}

pub fn check_method_body_types(method: &DMLMethodRef,
                               object: &DMLCompositeObject,
                               types: &TypeDatabase,
                               report: &mut Vec<DMLError>) {
    let declaration = method.get_decl();
    report_error_statements(&declaration.body, report);
    check_local_variable_types(
        &declaration.body, &declaration.method_args,
        method, object, types, report);
}

// Mirrors DMLC's EERRSTMT; only '#if' (not 'if') can exclude an error
fn report_error_statements(statement: &Statement,
                           report: &mut Vec<DMLError>) {
    let children: Vec<&Statement> = match statement.as_ref() {
        StatementKind::Error(error) => {
            report.push(DMLError {
                span: error.span,
                description: match error.message.as_deref() {
                    Some(ExpressionKind::StringLiteral(message)) =>
                        message.val.clone(),
                    _ => "forced compilation error in source code"
                        .to_string(),
                },
                related: vec![],
                severity: Some(DiagnosticSeverity::ERROR),
            });
            vec![]
        },
        StatementKind::Compound(compound) =>
            compound.statements.iter().collect(),
        StatementKind::If(statement) => std::iter::once(&statement.ifbody)
            .chain(statement.elsebody.as_ref()).collect(),
        StatementKind::While(statement) => vec![&statement.body],
        StatementKind::DoWhile(statement) => vec![&statement.body],
        StatementKind::For(statement) => vec![&statement.body],
        StatementKind::ForEach(statement) => vec![&statement.body],
        StatementKind::TryCatch(statement) =>
            vec![&statement.tryblock, &statement.catchblock],
        StatementKind::HashSelect(statement) =>
            vec![&statement.selectbranch, &statement.elsebranch],
        StatementKind::Switch(statement) => statement.cases.iter()
            .filter_map(|case|match case {
                SwitchCase::Statement(statement) => Some(statement),
                _ => None,
            }).collect(),
        _ => vec![],
    };
    for child in children {
        report_error_statements(child, report);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodDecl {
    pub name: DMLString,
    pub modifier: MethodModifier,
    pub independent: bool,
    pub startup: bool,
    pub memoized: bool,
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
    // (qualifier name, whether present) for independent, startup and memoized
    fn qualifiers(&self) -> [(&'static str, bool); 3];
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
        for ((qualifier, mine), (_, theirs)) in
            self.qualifiers().iter().zip(overridden.qualifiers().iter()) {
            if mine != theirs {
                report.push(DMLError {
                    span: *self.location(),
                    description: format!(
                        "One declaration is qualified as {}, but the other \
                         is not", qualifier),
                    related: vec![(*overridden.location(),
                                   "Overridden method declared here"
                                   .to_string())],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
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
            if !type1.equivalent_unqualified(type2) {
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

    fn qualifiers(&self) -> [(&'static str, bool); 3] {
        [("independent", self.independent),
         ("startup", self.startup),
         ("memoized", self.memoized)]
    }

    fn args(&self) -> &Vec<DMLMethodArg> {
        &self.method_args
    }

    fn returns(&self) -> &Vec<DMLResolvedType> {
        &self.return_types
    }
}

impl MethodDecl {
    pub fn from_content(content: &Method, types: &TypeDatabase,
                        report: &mut Vec<DMLError>)
                        -> MethodDecl {
        MethodDecl {
            name: content.object.name.clone(),
            modifier: content.modifier,
            independent: content.independent,
            startup: content.startup,
            memoized: content.memoized,
            default: content.default,
            body: content.body.clone(),
            throws: content.throws,
            method_args: eval_method_args(&content.arguments, types, report),
            return_types: eval_method_returns(&content.returns, types, report),
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
        log::trace!("Getting bases of {:?}", self);
        let to_return = if let Some(default_refs) = &self.concrete_decl.default_call {
            default_refs.get_bases()
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

    fn qualifiers(&self) -> [(&'static str, bool); 3] {
        self.concrete_decl.qualifiers()
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
    Abstract(Arc<DMLMethodRef>),
    Valid(Arc<DMLMethodRef>),
    Ambiguous(Vec<Arc<DMLMethodRef>>),
}

impl DefaultCallReference {
    pub fn get_bases(&self) -> Vec<MethodDecl> {
        match self {
            DefaultCallReference::Abstract(method) |
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
            DefaultCallReference::Abstract(method) |
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

    fn qualifiers(&self) -> [(&'static str, bool); 3] {
        self.decl.qualifiers()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::structure::types::{DMLType, DMLTypeKind};
    use crate::analysis::templating::types::{DMLConcreteType,
                                             DMLConcreteTypeKind};

    struct TestMethod {
        location: ZeroSpan,
        arguments: Vec<DMLMethodArg>,
        returns: Vec<DMLResolvedType>,
    }

    impl DMLNamedMember for TestMethod {
        fn identity(&self) -> &str { "test_method" }
        fn kind_name(&self) -> &'static str { "method" }
        fn location(&self) -> &ZeroSpan { &self.location }
    }

    impl MaybeAbstract for TestMethod {
        fn is_abstract(&self) -> bool { false }
    }

    impl MethodDeclaration for TestMethod {
        fn is_shared(&self) -> bool { false }
        fn is_default(&self) -> bool { false }
        fn throws(&self) -> bool { false }
        fn qualifiers(&self) -> [(&'static str, bool); 3] {
            [("independent", false), ("startup", false), ("memoized", false)]
        }
        fn args(&self) -> &Vec<DMLMethodArg> { &self.arguments }
        fn returns(&self) -> &Vec<DMLResolvedType> { &self.returns }
    }

    fn typed_argument(name: &str, type_name: &str) -> DMLMethodArg {
        let span = ZeroSpan::invalid("method-type-test.dml");
        let type_syntax = DMLType {
            kind: DMLTypeKind::Named(DMLString {
                val: type_name.to_string(),
                span,
            }),
            span,
        };
        DMLMethodArg::Typed(Declaration {
            name: DMLString { val: name.to_string(), span },
            type_ref: DMLConcreteType {
                kind: DMLConcreteTypeKind::Named(type_name.to_string()),
                span,
            }.into(),
            type_syntax,
        })
    }

    fn unresolved_argument(name: &str) -> DMLMethodArg {
        let span = ZeroSpan::invalid("method-type-test.dml");
        DMLMethodArg::Typed(Declaration {
            name: DMLString { val: name.to_string(), span },
            type_ref: DMLResolvedType::Dummy(span),
            type_syntax: DMLType {
                kind: DMLTypeKind::Unknown,
                span,
            },
        })
    }

    fn wrapped_argument(name: &str,
                        kind: impl FnOnce(Box<DMLConcreteType>)
                            -> DMLConcreteTypeKind,
                        inner: DMLConcreteType) -> DMLMethodArg {
        let span = ZeroSpan::invalid("method-type-test.dml");
        DMLMethodArg::Typed(Declaration {
            name: DMLString { val: name.to_string(), span },
            type_ref: DMLConcreteType {
                kind: kind(Box::new(inner)),
                span,
            }.into(),
            type_syntax: DMLType {
                kind: DMLTypeKind::Unknown,
                span,
            },
        })
    }

    fn concrete_named_type(name: &str) -> DMLConcreteType {
        DMLConcreteType {
            kind: DMLConcreteTypeKind::Named(name.to_string()),
            span: ZeroSpan::invalid("method-type-test.dml"),
        }
    }

    #[test]
    fn mismatching_argument_types_are_reported() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let method = TestMethod {
            location,
            arguments: vec![typed_argument("value", "int")],
            returns: vec![],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![typed_argument("value", "packet_t")],
            returns: vec![],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert!(report.iter().any(|error|
            error.description == "Mismatching argument type in method override"));
    }

    #[test]
    fn matching_argument_types_are_accepted() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let method = TestMethod {
            location,
            arguments: vec![typed_argument("value", "int")],
            returns: vec![],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![typed_argument("value", "int")],
            returns: vec![],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert!(report.is_empty());
    }

    #[test]
    fn numeric_conversion_argument_types_are_reported() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let method = TestMethod {
            location,
            arguments: vec![typed_argument("value", "float")],
            returns: vec![],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![typed_argument("value", "int")],
            returns: vec![],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert!(report.iter().any(|error|
            error.description == "Mismatching argument type in method override"));
    }

    #[test]
    fn override_argument_compatibility_ignores_pointee_const_by_default() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let int_type = concrete_named_type("int");
        let const_int_type = DMLConcreteType {
            kind: DMLConcreteTypeKind::Const(Box::new(int_type.clone())),
            span: location,
        };
        let int_pointer = wrapped_argument(
            "value", DMLConcreteTypeKind::Pointer, int_type);
        let const_int_pointer = wrapped_argument(
            "value", DMLConcreteTypeKind::Pointer, const_int_type);
        let accepting_override = TestMethod {
            location,
            arguments: vec![const_int_pointer.clone()],
            returns: vec![],
        };
        let base = TestMethod {
            location,
            arguments: vec![int_pointer.clone()],
            returns: vec![],
        };
        let rejecting_override = TestMethod {
            location,
            arguments: vec![int_pointer],
            returns: vec![],
        };
        let const_base = TestMethod {
            location,
            arguments: vec![const_int_pointer],
            returns: vec![],
        };
        let mut accepted_report = vec![];
        let mut rejected_report = vec![];

        accepting_override.check_override(&base, &mut accepted_report);
        rejecting_override.check_override(&const_base, &mut rejected_report);

        assert!(accepted_report.is_empty());
        assert!(rejected_report.is_empty());
    }

    #[test]
    fn override_argument_compatibility_ignores_top_level_const() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let const_int = wrapped_argument(
            "value", DMLConcreteTypeKind::Const, concrete_named_type("int"));
        let method = TestMethod {
            location,
            arguments: vec![const_int],
            returns: vec![],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![typed_argument("value", "int")],
            returns: vec![],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert!(report.is_empty());
    }

    #[test]
    fn override_return_types_remain_exact() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let method = TestMethod {
            location,
            arguments: vec![],
            returns: vec![concrete_named_type("float").into()],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![],
            returns: vec![concrete_named_type("int").into()],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert!(report.iter().any(|error|
            error.description == "Mismatching return type in method override"));
    }

    #[test]
    fn partially_known_signatures_compare_known_slots() {
        let location = ZeroSpan::invalid("method-type-test.dml");
        let method = TestMethod {
            location,
            arguments: vec![
                unresolved_argument("unknown"),
                typed_argument("known", "int"),
            ],
            returns: vec![
                DMLResolvedType::Dummy(location),
                concrete_named_type("int").into(),
            ],
        };
        let overridden = TestMethod {
            location,
            arguments: vec![
                typed_argument("unknown", "packet_t"),
                typed_argument("known", "float"),
            ],
            returns: vec![
                concrete_named_type("packet_t").into(),
                concrete_named_type("float").into(),
            ],
        };
        let mut report = vec![];

        method.check_override(&overridden, &mut report);

        assert_eq!(report.iter().filter(|error|
            error.description == "Mismatching argument type in method override")
            .count(), 1);
        assert_eq!(report.iter().filter(|error|
            error.description == "Mismatching return type in method override")
            .count(), 1);
    }

    #[test]
    fn unresolved_return_types_remain_present_in_call_slots() {
        let span = ZeroSpan::invalid("method-type-test.dml");

        let slot = EvaluationSlot::from_type(DMLResolvedType::Dummy(span));

        assert!(matches!(slot.typed, Some(DMLResolvedType::Dummy(found))
            if found == span));
    }
}

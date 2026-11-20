//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::tree::{LeafToken, TreeElement, ZeroSpan};
use crate::analysis::parsing::{misc, types};
use crate::analysis::structure::expressions::{DMLString, Expression, ExpressionKind};
use crate::analysis::symbols::{DMLSymbolKind, StructureSymbol};
use crate::analysis::{FileSpec, LocalDMLError, LocationSpan, Named};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLType {
    pub kind: DMLTypeKind,
    pub span: ZeroSpan,
}

impl DMLType {
    fn new(kind: DMLTypeKind, span: ZeroSpan) -> Self {
        Self { kind, span }
    }

    pub fn unknown(span: ZeroSpan) -> Self {
        Self::new(DMLTypeKind::Unknown, span)
    }

    pub fn span(&self) -> &ZeroSpan {
        &self.span
    }

    pub fn declaration_members(&self) -> Vec<&DMLTypeMember> {
        let members = match &self.kind {
            DMLTypeKind::Struct(members) |
            DMLTypeKind::Layout { fields: members, .. } => members,
            DMLTypeKind::Bitfields { fields, .. } =>
                return fields.iter().flat_map(|field|
                    std::iter::once(&field.member).chain(
                        field.member.typed.declaration_members()))
                    .collect(),
            DMLTypeKind::Const(inner) |
            DMLTypeKind::Pointer(inner) |
            DMLTypeKind::Vector(inner) =>
                return inner.declaration_members(),
            DMLTypeKind::Array { element, .. } =>
                return element.declaration_members(),
            DMLTypeKind::Function { returns, .. } =>
                return returns.declaration_members(),
            _ => return vec![],
        };
        members.iter().flat_map(|member|
            std::iter::once(member).chain(
                member.typed.declaration_members()))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLTypeKind {
    Named(DMLString),
    Struct(Vec<DMLTypeMember>),
    Layout {
        byte_order: Option<String>,
        fields: Vec<DMLTypeMember>,
    },
    Bitfields {
        width: Option<String>,
        fields: Vec<DMLBitfieldMember>,
    },
    TypeOf(Expression),
    Sequence(DMLString),
    Hook(Vec<DMLType>),
    Const(Box<DMLType>),
    Pointer(Box<DMLType>),
    Vector(Box<DMLType>),
    Array {
        element: Box<DMLType>,
        size: Option<Expression>,
    },
    Function {
        returns: Box<DMLType>,
        parameters: Vec<DMLType>,
        variadic: bool,
    },
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLTypeMember {
    pub name: Option<DMLString>,
    pub typed: DMLType,
    pub span: ZeroSpan,
}

impl Named for DMLTypeMember {
    fn get_name(&self) -> String {
        self.name.as_ref().map(|name|name.val.clone()).unwrap_or_default()
    }
}

impl LocationSpan for DMLTypeMember {
    fn loc_span(&self) -> &ZeroSpan {
        self.name.as_ref().map(|name|&name.span).unwrap_or(&self.span)
    }
}

impl StructureSymbol for DMLTypeMember {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Field
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLBitfieldMember {
    pub member: DMLTypeMember,
    pub range: DMLBitfieldRange,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLBitfieldRange {
    Single(Expression),
    Range(Expression, Expression),
    Unknown(ZeroSpan),
}

fn span_of(element: &impl TreeElement, file: FileSpec<'_>) -> ZeroSpan {
    ZeroSpan::from_range(element.range(), file.path)
}

fn unknown_type(element: &impl TreeElement, file: FileSpec<'_>) -> DMLType {
    DMLType::new(DMLTypeKind::Unknown, span_of(element, file))
}

fn wrap_type(kind: impl FnOnce(Box<DMLType>) -> DMLTypeKind,
             typed: DMLType,
             span: ZeroSpan) -> DMLType {
    DMLType::new(kind(Box::new(typed)), span)
}

fn apply_modifiers(modifiers: &[LeafToken],
                   mut typed: DMLType,
                   file: FileSpec<'_>) -> DMLType {
    for modifier in modifiers {
        let span = ZeroSpan::from_range(
            crate::span::Range::combine(typed.span.range, modifier.range()),
            file.path);
        typed = match modifier.get_token().map(|token|token.kind) {
            Some(TokenKind::Const) =>
                wrap_type(DMLTypeKind::Const, typed, span),
            Some(TokenKind::Multiply) =>
                wrap_type(DMLTypeKind::Pointer, typed, span),
            Some(TokenKind::Vect) =>
                wrap_type(DMLTypeKind::Vector, typed, span),
            _ => DMLType::new(DMLTypeKind::Unknown, span),
        };
    }
    typed
}

fn lower_member(decl: &misc::CDecl,
                report: &mut Vec<LocalDMLError>,
                file: FileSpec<'_>) -> DMLTypeMember {
    let span = span_of(decl, file);
    let (name, typed) = decl.with_content(
        |content|deconstruct_cdecl(content, report, file),
        (None, None));
    DMLTypeMember {
        name,
        typed: typed.unwrap_or_else(||unknown_type(decl, file)),
        span,
    }
}

fn lower_base_type(content: &types::BaseTypeContent,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'_>) -> DMLType {
    let span = span_of(content, file);
    let kind = match content {
        types::BaseTypeContent::Ident(token) =>
            DMLString::from_token(token, file).map(DMLTypeKind::Named),
        types::BaseTypeContent::StructType(struct_type) => Some(
            DMLTypeKind::Struct(struct_type.members.iter()
                .map(|(decl, _)|lower_member(decl, report, file))
                .collect())),
        types::BaseTypeContent::Layout(layout) => Some(DMLTypeKind::Layout {
            byte_order: layout.byteorder.read_leaf(file.file),
            fields: layout.fields.iter()
                .map(|(decl, _)|lower_member(decl, report, file))
                .collect(),
        }),
        types::BaseTypeContent::Bitfields(bitfields) => Some(
            DMLTypeKind::Bitfields {
                // Invalid widths are reported when parsing
                width: bitfields.iconst.read_leaf(file.file)
                    .map(|width|width.replace('_', ""))
                    .filter(|width|width.parse::<u8>()
                            .is_ok_and(|width|(1..=64).contains(&width))),
                fields: bitfields.fields.iter().map(|field| {
                    let range = match &field.range {
                        types::BitfieldsRange::Expression(expression) =>
                            ExpressionKind::to_expression(
                                expression, report, file)
                                .map(DMLBitfieldRange::Single),
                        types::BitfieldsRange::Range(first, _, last) =>
                            ExpressionKind::to_expression(first, report, file)
                                .zip(ExpressionKind::to_expression(
                                    last, report, file))
                                .map(|(first, last)|
                                    DMLBitfieldRange::Range(first, last)),
                    }.unwrap_or_else(||DMLBitfieldRange::Unknown(
                        span_of(&field.range, file)));
                    DMLBitfieldMember {
                        member: lower_member(&field.cdecl, report, file),
                        range,
                    }
                }).collect(),
            }),
        types::BaseTypeContent::TypeOf(type_of) =>
            ExpressionKind::to_expression(&type_of.of, report, file)
                .map(DMLTypeKind::TypeOf),
        types::BaseTypeContent::Sequence(sequence) =>
            DMLString::from_token(&sequence.ident, file)
                .map(DMLTypeKind::Sequence),
        types::BaseTypeContent::HookType(hook) => Some(DMLTypeKind::Hook(
            hook.args.iter().map(|(decl, _)| {
                decl.with_content(
                    |content|deconstruct_cdecl(content, report, file).1,
                    None)
                    .unwrap_or_else(||unknown_type(decl, file))
            }).collect())),
    };
    DMLType::new(kind.unwrap_or(DMLTypeKind::Unknown), span)
}

fn lower_base(base: &types::BaseType,
              report: &mut Vec<LocalDMLError>,
              file: FileSpec<'_>) -> DMLType {
    base.with_content(
        |content|lower_base_type(content, report, file),
        unknown_type(base, file))
}

fn lower_type_decl(content: &misc::TypeDeclContent,
                   typed: DMLType,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'_>) -> (Option<DMLString>, DMLType) {
    match content {
        misc::TypeDeclContent::Ident(token) =>
            (DMLString::from_token(token, file), typed),
        misc::TypeDeclContent::Array(inner, _, size, _) => {
            let span = span_of(content, file);
            let size = ExpressionKind::to_expression(size, report, file);
            let array = DMLType::new(DMLTypeKind::Array {
                element: Box::new(typed),
                size,
            }, span);
            lower_optional_type_decl(inner, array, report, file)
        },
        misc::TypeDeclContent::Fun(inner, _, parameters, ellipsis, _) => {
            let span = span_of(content, file);
            let parameters = parameters.iter().map(|(decl, _)| {
                decl.with_content(
                    |content|deconstruct_cdecl(content, report, file).1,
                    None)
                    .unwrap_or_else(||unknown_type(decl, file))
            }).collect();
            let function = DMLType::new(DMLTypeKind::Function {
                returns: Box::new(typed),
                parameters,
                variadic: ellipsis.is_some(),
            }, span);
            lower_optional_type_decl(inner, function, report, file)
        },
        misc::TypeDeclContent::Parens(_, modifiers, inner, _) => {
            let modified = apply_modifiers(modifiers, typed, file);
            lower_optional_type_decl(inner, modified, report, file)
        },
    }
}

fn lower_optional_type_decl(decl: &misc::TypeDecl,
                            typed: DMLType,
                            report: &mut Vec<LocalDMLError>,
                            file: FileSpec<'_>) -> (Option<DMLString>, DMLType) {
    match &decl.content {
        Some(content) => content.with_content(
            |content|lower_type_decl(content, typed.clone(), report, file),
            (None, typed.clone())),
        None => (None, typed),
    }
}

pub fn deconstruct_cdecl<'a>(content: &misc::CDeclContent,
                             report: &mut Vec<LocalDMLError>,
                             file: FileSpec<'a>)
                             -> (Option<DMLString>, Option<DMLType>) {
    let mut typed = lower_base(&content.base, report, file);
    if content.consttok.is_some() {
        let span = span_of(content, file);
        typed = wrap_type(DMLTypeKind::Const, typed, span);
    }
    typed = apply_modifiers(&content.modifiers, typed, file);
    let (name, mut typed) = lower_optional_type_decl(
        &content.decl, typed, report, file);
    typed.span = span_of(content, file);
    (name, Some(typed))
}

fn lower_simple_ctype(simple: &types::CTypeDeclSimple,
                      typed: DMLType,
                      file: FileSpec<'_>) -> DMLType {
    simple.with_content(|content| {
        let modified = apply_modifiers(&content.modifiers, typed.clone(), file);
        content.inner.as_ref().map_or(modified.clone(), |(_, inner, _)|
            lower_simple_ctype(inner, modified, file))
    }, typed.clone())
}

#[allow(clippy::ptr_arg)]
pub fn to_type<'a>(content: &types::CTypeDecl,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'a>) -> Option<DMLType> {
    content.with_content(|content| {
        let mut typed = lower_base(&content.base, report, file);
        if content.consttok.is_some() {
            typed = wrap_type(DMLTypeKind::Const, typed, span_of(content, file));
        }
        let mut typed = lower_simple_ctype(&content.simple, typed, file);
        typed.span = span_of(content, file);
        Some(typed)
    }, None)
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::str::FromStr;

    use logos::Logos;

    use super::*;
    use crate::analysis::parsing::lexer::TokenKind;
    use crate::analysis::parsing::parser::{
        doesnt_understand_tokens, FileInfo, FileParser, Parse, ParseContext,
    };
    use crate::vfs::TextFile;

    fn lower_cdecl(source: &str) -> (Option<DMLString>, DMLType) {
        let lexer = TokenKind::lexer(source);
        let mut parser = FileParser::new(lexer);
        let context = ParseContext::new_context(doesnt_understand_tokens);
        let ast = misc::CDecl::parse(&context, &mut parser, &FileInfo::default());
        assert!(parser.skipped_tokens.is_empty(),
                "unexpected skipped tokens: {:?}", parser.skipped_tokens);

        let text = TextFile::from_str(source).unwrap();
        let path = Path::new("type-test.dml");
        let file = FileSpec { path, file: &text };
        let mut errors = vec![];
        let lowered = ast.with_content(
            |content|deconstruct_cdecl(content, &mut errors, file),
            (None, None));
        assert!(errors.is_empty(), "unexpected lowering errors: {:?}", errors);
        (lowered.0, lowered.1.expect("declaration should have a type"))
    }

    fn assert_named(typed: &DMLType, expected: &str) {
        match &typed.kind {
            DMLTypeKind::Named(name) => assert_eq!(name.val, expected),
            other => panic!("expected named type, got {:?}", other),
        }
    }

    #[test]
    fn lowers_array_of_pointers_with_correct_precedence() {
        let (name, typed) = lower_cdecl("uint32 *items[4]");
        assert_eq!(name.unwrap().val, "items");
        match typed.kind {
            DMLTypeKind::Array { element, size: Some(_) } => match element.kind {
                DMLTypeKind::Pointer(inner) => assert_named(&inner, "uint32"),
                other => panic!("expected pointer element, got {:?}", other),
            },
            other => panic!("expected array type, got {:?}", other),
        }
    }

    #[test]
    fn lowers_pointer_to_array_with_correct_precedence() {
        let (name, typed) = lower_cdecl("uint32 (*items)[4]");
        assert_eq!(name.unwrap().val, "items");
        match typed.kind {
            DMLTypeKind::Pointer(inner) => match inner.kind {
                DMLTypeKind::Array { element, size: Some(_) } =>
                    assert_named(&element, "uint32"),
                other => panic!("expected array target, got {:?}", other),
            },
            other => panic!("expected pointer type, got {:?}", other),
        }
    }

    #[test]
    fn retains_anonymous_struct_fields() {
        let (name, typed) = lower_cdecl(
            "struct { uint32 addr; uint8 data[16]; } packet");
        assert_eq!(name.unwrap().val, "packet");
        let DMLTypeKind::Struct(fields) = typed.kind else {
            panic!("expected struct type, got {:?}", typed.kind);
        };
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name.as_ref().unwrap().val, "addr");
        assert_named(&fields[0].typed, "uint32");
        assert_eq!(fields[1].name.as_ref().unwrap().val, "data");
        assert!(matches!(fields[1].typed.kind, DMLTypeKind::Array { .. }));
    }
}

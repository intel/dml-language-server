//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::parsing::lexer::TokenKind;
use crate::analysis::parsing::{misc, structure};
use crate::analysis::parsing::tree::{LeafToken, ZeroRange,
                                     ZeroSpan, TreeElement};
use crate::analysis::structure::expressions::{Expression, Value, DMLString,
                                              ExpressionKind};
use crate::analysis::structure::statements::{Statement, StatementKind};
use crate::analysis::structure::types::{DMLType, deconstruct_cdecl, to_type};
use crate::analysis::FileSpec;
use crate::analysis::{LocalDMLError, DeclarationSpan, LocationSpan, DMLNamed,
                      Named};
use crate::analysis::reference::{ReferenceKind, Reference,
                                 ReferenceContainer};
use crate::analysis::symbols::{StructureSymbol, SimpleSymbol,
                               DMLSymbolKind,
                               SymbolContainer, MakeSymbolContainer};
use crate::analysis::scope::{Scope, ScopeContainer,
                             MakeScopeContainer, ContextKey};

// This defines the plain structure of the file, that is, before
// any template instantiations have been done

// Here is where incompletly parsed nodes are rejected, after we
// have done as much structural analysis as possible

// Note that, in general, missing elements do not need to be reported
// as errors here since they are already reported by the
// post-parse walk

pub trait ToStructure<F> {
    fn to_structure<'a>(content: &F,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Self> where Self: Sized;
    fn allowed_sub(&self, _sub: &DMLObject) -> bool {
        false
    }
}

pub trait MaybeAbstract {
    fn is_abstract(&self) -> bool;
    fn is_concrete(&self) -> bool {
        !self.is_abstract()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLBitorder {
    BE, LE,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bitorder {
    pub span: ZeroSpan,
    pub order: Value<DMLBitorder>,
}

impl DeclarationSpan for Bitorder {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl ToStructure<structure::BitorderContent> for Bitorder {
    #[allow(clippy::ptr_arg)]
    fn to_structure<'a>(content: &structure::BitorderContent,
                    _report: &mut Vec<LocalDMLError>,
                    file: FileSpec<'a>) -> Option<Bitorder> {
        Some(Bitorder {
            span: ZeroSpan::from_range(content.range(), file.path),
            order: Value::<DMLBitorder> {
                val: match content.bitorderdesc.read_leaf(file.file)?.as_str() {
                    "be" => DMLBitorder::BE,
                    "le" => DMLBitorder::LE,
                    // This is already reported as a syntax error
                    _ => None?,
                },
                span: ZeroSpan::from_range(content.bitorderdesc.range(),
                                           file.path),
            },
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Device {
    pub decl: CompObjectKindDecl,
    pub name: DMLString,
    pub span: ZeroSpan,
}

impl ToStructure<structure::DeviceContent> for Device {
    fn to_structure<'a>(content: &structure::DeviceContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Device> {
        Some(Device {
            decl: CompObjectKindDecl {
                span: ZeroSpan::from_range(content.device.range(), file.path),
                kind: CompObjectKind::Device,
            },
            name: DMLString::from_token(&content.name, file)?,
            span: ZeroSpan::from_range(content.range(), file.path),
        })
    }
}

impl DeclarationSpan for Device {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl DMLNamed for Device {
    fn name(&self) -> &DMLString {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version {
    pub span: ZeroSpan,
    pub major_version: Value<u8>,
    pub minor_version: Value<u8>,
}

impl ToStructure<structure::DMLVersionContent> for Version {
    fn to_structure<'a>(content: &structure::DMLVersionContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Version> {
        let (minor_version, major_version) =
            match content.version.read_leaf(file.file) {
                Some(string) => {
                    let versions: Vec<&str> = string.split('.').collect();
                    // Guaranteed by parser
                    assert!(versions.len() == 2);
                    let minor = match versions[1].parse() {
                        Ok(val) => Some(Value::<u8> {
                            val,
                            span: ZeroSpan::from_range(content.version.range(), file.path)
                        }),
                        _ => {
                            report.push(
                                LocalDMLError {
                                    range: content.version.range(),
                                    description:
                                    "Invalid DML minor version".to_string(),
                                });
                            None
                        }
                    };
                    let major = match versions[0].parse() {
                        Ok(val) => Some(Value::<u8> {
                            val,
                            span: ZeroSpan::from_range(content.version.range(), file.path)
                        }),
                        _ => {
                            report.push(
                                LocalDMLError {
                                    range: content.version.range(),
                                    description:
                                    "Invalid DML major version".to_string(),
                                });
                            None
                        }
                    };
                    (minor, major)
                },
                // Already reported by parser
                None => (None, None)
            };
        Some(Version {
            span: ZeroSpan::from_range(content.range(), file.path),
            major_version: major_version?,
            minor_version: minor_version?,
        })
    }
}

impl DeclarationSpan for Version {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Export {
    pub span: ZeroSpan,
    pub target: Expression,
    pub name: Expression,
}

impl ToStructure<structure::ExportContent> for Export {
    fn to_structure<'a>(content: &structure::ExportContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Export> {
        let target = ExpressionKind::to_expression(
            &content.target, report, file);
        let name = ExpressionKind::to_expression(
            &content.name, report, file);
        Some(Export {
            span: ZeroSpan::from_range(content.range(), file.path),
            target: target?,
            name: name?,
        })
    }
}

impl DeclarationSpan for Export {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CBlock {
    pub span: ZeroSpan,
    pub cblock: String,
}

impl ToStructure<structure::CBlockContent> for CBlock {
    fn to_structure<'a>(content: &structure::CBlockContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<CBlock> {
        Some(CBlock {
            span: ZeroSpan::from_range(content.range(), file.path),
            cblock: content.cblock.read_leaf(file.file)?,
        })
    }
}

impl DeclarationSpan for CBlock {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

// TODO: We are missing a fair bit of error reporting for hooks:
// Check uses of hooks (both "send_now" and after bindings) for argument number
// Check that shared hooks are only declared in traits
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Hook {
    pub shared: bool,
    pub object: DMLObjectCommon,
    pub args: Vec<DMLType>,
}

impl ToStructure<structure::HookContent> for Hook {
    fn to_structure<'a>(content: &structure::HookContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Hook> {
        let mut args = vec![];
        for (cdecl, _) in content.arg_types.iter() {
            // Name is non-semantic for hooks
            if let (_, Some(typed)) = cdecl.with_content(
                |con|deconstruct_cdecl(con, report, file),
                (None, None)) {
                args.push(typed);
            }
        }
        Some(Hook {
            shared: content.shared.is_some(),
            args,
            // Safe to discard hooks with no name
            object: DMLObjectCommon {
                name: DMLString::from_token(&content.ident, file)?,
                span: ZeroSpan::from_range(content.range(), file.path),
            }
        })
    }
}

impl DeclarationSpan for Hook {
    fn span(&self) -> &ZeroSpan {
        &self.object.span
    }
}

impl DMLNamed for Hook {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl StructureSymbol for Hook {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Hook
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Import {
    pub span: ZeroSpan,
    pub name: DMLString,
}

impl DMLNamed for Import {
    fn name(&self) -> &DMLString {
        &self.name
    }
}

impl DeclarationSpan for Import {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl ToStructure<structure::ImportContent> for Import {
    fn to_structure<'a>(content: &structure::ImportContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Import> {
        Some(Import {
            span: ZeroSpan::from_range(content.range(), file.path),
            name: DMLString::from_token(&content.file, file)?,
        })
    }
}

impl Import {
    pub fn imported_name(&self) -> &str {
        &self.name.val[1 .. self.name.val.len() - 1]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InEach {
    pub span: ZeroSpan,
    pub spec: Vec<DMLString>,
    pub statements: Statements,
    pub references: Vec<Reference>,
}

impl DeclarationSpan for InEach {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl Named for InEach {
    fn get_name(&self) -> String {
        format!("InEach({})",
                self.spec.iter().map(|name|name.val.to_string())
                .collect::<Vec<String>>().join(", "))
    }
}

impl Scope for InEach {
    fn create_context(&self) -> ContextKey {
        ContextKey::AllWithTemplate(
            self.spec.iter().map(|s|s.span)
                .reduce(|acc, e|ZeroSpan::combine(acc, e))
            // This is a bit of a hack, when we don't have any
            // template specifier, we still want some reasonable
            // place to point to for the context definition. So we faux the
            // start of it from the declaration span
                .unwrap_or_else(
                    ||ZeroSpan::from_positions(
                        self.span.start_position().position,
                        self.span.start_position().position,
                        self.span.path())),
            self.spec.iter().map(|s|s.val.to_string()).collect()
        )
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.statements.symbols()
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        self.statements.scopes()
    }
    fn defined_references(&self) -> &Vec<Reference> {
        &self.references
    }
}

impl ToStructure<structure::InEachContent> for InEach {
    fn to_structure<'a>(content: &structure::InEachContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<InEach> {
        let mut spec = vec![];
        match &content.spec {
            structure::InEachSpec::One(tok) =>
                if let Some(dmlstr) = DMLString::from_token(tok, file) {
                    spec.push(dmlstr)
                },
            structure::InEachSpec::List(_, list, _) => {
                for (tok, _) in list {
                    if let Some(dmlstr) = DMLString::from_token(tok, file) {
                        spec.push(dmlstr);
                    };
                }
            }
        };
        let span = ZeroSpan::from_range(content.range(), file.path);
        let statements = content.statements.with_content(
            |con|to_objectstatements(con, report, file),
            Statements::empty());
        let mut references = vec![];
        content.spec.references(&mut references, file);
        content.statements.references(&mut references, file);
        Some(InEach {
            span,
            spec,
            statements,
            references,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loggroup {
    pub span: ZeroSpan,
    pub name: DMLString,
}

impl ToStructure<structure::LoggroupContent> for Loggroup {
    fn to_structure<'a>(content: &structure::LoggroupContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Loggroup> {
        Some(Loggroup {
            span: ZeroSpan::from_range(content.range(), file.path),
            name: DMLString::from_token(&content.ident, file)?,
        })
    }
}

impl DeclarationSpan for Loggroup {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl DMLNamed for Loggroup {
    fn name(&self) -> &DMLString {
        &self.name
    }
}

impl StructureSymbol for Loggroup {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Loggroup
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instantiation {
    pub span: ZeroSpan,
    pub names: Vec<DMLString>,
}

impl ToStructure<structure::InstantiationContent> for Instantiation {
    fn to_structure<'a>(content: &structure::InstantiationContent,
                        _report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Instantiation> {
        let mut names = vec![];
        match &content.instantiation {
            structure::Instantiation::One(tok) => {
                if let Some(dmlstr) = DMLString::from_token(tok, file) {
                    names.push(dmlstr);
                }
            },
            structure::Instantiation::Many(_, toks, _) => {
                for (tok, _) in toks {
                    if let Some(dmlstr) = DMLString::from_token(tok, file) {
                        names.push(dmlstr);
                    }
                }
            }
        }
        Some(Instantiation {
            span: ZeroSpan::from_range(content.range(), file.path),
            names,
        })
    }
}

impl DeclarationSpan for Instantiation {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    pub span: ZeroSpan,
    // No need to track exact location of the message
    pub message: Option<Expression>,
}

impl ToStructure<structure::ErrorObjectContent> for Error {
    fn to_structure<'a>(content: &structure::ErrorObjectContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Error> {
        Some(
            Error {
                span: ZeroSpan::from_range(content.range(), file.path),
                message: match &content.message {
                    Some(expr) => ExpressionKind::to_expression(
                        expr, report, file),
                    None => None,
                },
            })
    }
}

impl DeclarationSpan for Error {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

// We store instantiations and errors seperately, since
// their semantics are special, but storing them is common
// enough that we want a struct for it
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statements {
    pub instantiations: Vec<Instantiation>,
    pub errors: Vec<Error>,
    pub statements: Vec<DMLStatement>,
    pub ineachs: Vec<InEach>
}

impl Statements {
    pub fn empty() -> Statements {
        Statements {
            instantiations: vec![],
            errors: vec![],
            statements: vec![],
            ineachs: vec![],
        }
    }
}

impl ScopeContainer for Statements {
    fn scopes(&self) -> Vec<&dyn Scope> {
        let mut scopes = self.statements.scopes();
        scopes.append(&mut self.ineachs.to_scopes());
        scopes
    }
}

impl SymbolContainer for Statements {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.statements.symbols()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum DMLStatementKind {
    Error(Error),
    Instantiation(Instantiation),
    Statement(DMLStatement),
    InEach(InEach),
}

impl SymbolContainer for DMLStatementKind {
    fn symbols(&self) -> Vec<&dyn StructureSymbol>{
        if let Self::Statement(statement) = self {
            statement.symbols()
        } else {
            vec![]
        }
    }
}

pub fn make_statements<'a>(ast: &[structure::DMLObject],
                           report: &mut Vec<LocalDMLError>,
                           file: FileSpec<'a>) -> Statements {
    let mut instantiations = vec![];
    let mut errors = vec![];
    let mut statements = vec![];
    let mut ineachs = vec![];
    for obj in ast {
        match obj.with_content(
            |objcon|to_objectstatement(objcon, report, file), None) {
            None => (),
            Some(DMLStatementKind::Error(err)) => errors.push(err),
            Some(DMLStatementKind::Instantiation(inst)) =>
                instantiations.push(inst),
            Some(DMLStatementKind::Statement(stmt)) =>
                statements.push(stmt),
            Some(DMLStatementKind::InEach(ie)) =>
                ineachs.push(ie),
        }
    };
    Statements { instantiations, errors, statements, ineachs }
}

fn to_objectstatements<'a>(list: &structure::ObjectStatementsContent,
                           report: &mut Vec<LocalDMLError>,
                           file: FileSpec<'a>) -> Statements
{
    match list {
        structure::ObjectStatementsContent::Empty(_) => Statements::empty(),
        structure::ObjectStatementsContent::List(_, objects, _) => {
            make_statements(objects, report, file)
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLStatement {
    Object(DMLObject),
    HashIf(HashIf),
}

impl ScopeContainer for DMLStatement {
    fn scopes(&self) -> Vec<&dyn Scope> {
        match self {
            DMLStatement::Object(DMLObject::CompositeObject(obj)) =>
                vec![obj as &dyn Scope],
            DMLStatement::Object(DMLObject::Method(obj)) =>
                vec![obj as &dyn Scope],
            DMLStatement::Object(DMLObject::Template(obj)) =>
                vec![obj as &dyn Scope],
            DMLStatement::HashIf(hashif) => hashif.scopes(),
            _ => vec![],
        }
    }
}

impl SymbolContainer for DMLStatement {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        match self {
            DMLStatement::Object(DMLObject::CompositeObject(obj)) =>
                vec![obj as &dyn StructureSymbol],
            DMLStatement::Object(DMLObject::Method(obj)) =>
                vec![obj as &dyn StructureSymbol],
            DMLStatement::Object(DMLObject::Template(obj)) =>
                vec![obj as &dyn StructureSymbol],
            DMLStatement::Object(DMLObject::Hook(hook)) =>
                vec![hook as &dyn StructureSymbol],
            DMLStatement::Object(DMLObject::Saved(var)) |
            DMLStatement::Object(DMLObject::Session(var)) => var.symbols(),
            // TODO: Are these actually just at toplevel?
            DMLStatement::Object(DMLObject::Constant(con)) =>
                vec![con as &dyn StructureSymbol],
            DMLStatement::Object(DMLObject::Parameter(param)) =>
                vec![param as &dyn StructureSymbol],
            DMLStatement::HashIf(hashif) => hashif.symbols(),
            _ => vec![]
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Template {
    pub object: DMLObjectCommon,
    pub statements: Statements,
    pub references: Vec<Reference>,
}

impl StructureSymbol for Template {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Template
    }
}

impl Scope for Template {
    fn create_context(&self) -> ContextKey {
        ContextKey::Template(SimpleSymbol::make(self, self.kind()))
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.statements.symbols()
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        self.statements.scopes()
    }
    fn defined_references(&self) -> &Vec<Reference> {
        &self.references
    }
}

impl DeclarationSpan for Template {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for Template {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl ToStructure<structure::TemplateContent> for Template {
    fn to_structure<'a>(content: &structure::TemplateContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Template> {
        // Analyze statements first
        let mut statements = content.statements.with_content(
            |con|to_objectstatements(con, report, file), Statements::empty());
        if let Some((_, instantiations)) = &content.instantiation {
            let mut names = vec![];
            match instantiations {
                structure::Instantiation::One(tok) =>
                    if let Some(dmlstr) = DMLString::from_token(tok, file) {
                        names.push(dmlstr)
                    },
                structure::Instantiation::Many(_, insts, _) =>
                    for (tok, _) in insts {
                        if let Some(dmlstr) = DMLString::from_token(tok, file) {
                            names.push(dmlstr)
                        }
                    },
            }
            statements.instantiations.push(Instantiation {
                span: ZeroSpan::from_range(content.instantiation.range(), file.path),
                names,
            })
        }
        let mut references = vec![];
        content.instantiation.references(&mut references, file);
        content.statements.references(&mut references, file);
        Some(Template {
            object: DMLObjectCommon {
                name: DMLString::from_token(&content.name, file)?,
                span: ZeroSpan::from_range(content.range(), file.path),
            },
            // At this point, if the name of the template is missing
            // we can discard the entire construct
            statements,
            references,
        })
    }
}

// A special case,
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HashIf {
    pub span: ZeroSpan,
    pub condition: Expression,
    pub truebranch: Statements,
    pub falsebranch: Statements,
}

impl ScopeContainer for HashIf {
    fn scopes(&self) -> Vec<&dyn Scope> {
        let mut scopes = self.truebranch.scopes();
        scopes.append(&mut self.falsebranch.scopes());
        scopes
    }
}

impl SymbolContainer for HashIf {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.truebranch.symbols();
        symbols.append(&mut self.falsebranch.symbols());
        symbols
    }
}

impl ToStructure<structure::HashIfContent> for HashIf {
    fn to_structure<'a>(content: &structure::HashIfContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<HashIf> {
        let condition = ExpressionKind::to_expression(
            &content.cond, report, file);
        let truestatements = content.truestatements.with_content(
            |con|to_objectstatements(con, report, file), Statements::empty());
        let falsestatements = match &content.elsebranch {
            Some((_, structure::HashElse::Statements(stmnts))) =>
                stmnts.with_content(
                    |con|to_objectstatements(con, report, file),
                    Statements::empty()),
            Some((_,  structure::HashElse::HashIf(hif))) => {
                let mut stmnts = Statements::empty();
                if let Some(hi) = hif.with_content(
                    |h|to_objectstatement(h, report, file),
                    None) {
                    match hi {
                        DMLStatementKind::Statement(dmlhi) =>
                            stmnts.statements.push(dmlhi),
                        _ => unreachable!("HashElseIf invalid structure"),
                    }
                }
                stmnts
            },
            None => Statements::empty(),
        };
        Some(HashIf {
            span: ZeroSpan::from_range(content.range(), file.path),
            condition: condition?,
            truebranch: truestatements,
            falsebranch: falsestatements,
        })
    }
}

impl DeclarationSpan for HashIf {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DMLObjectCommon {
    pub name: DMLString,
    pub span: ZeroSpan,
}

impl DeclarationSpan for DMLObjectCommon {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl DMLNamed for DMLObjectCommon {
    fn name(&self) -> &DMLString {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrayDim {
    pub span: ZeroSpan,
    pub indexvar: DMLString,
    // Here, None means an implicit size
    pub size: Option<Expression>,
}

impl DeclarationSpan for ArrayDim {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl DMLNamed for ArrayDim {
    fn name(&self) -> &DMLString {
        &self.indexvar
    }
}

impl StructureSymbol for ArrayDim {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Parameter
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompObjectKind {
    Attribute, Bank, Connect, Device, Event, Field, Group,
    Implement, Interface, Port, Register, Subdevice
}

impl CompObjectKind {
    pub fn kind_name(&self) -> &'static str {
        match self {
            CompObjectKind::Attribute => "attribute",
            CompObjectKind::Bank => "bank",
            CompObjectKind::Connect => "connect",
            CompObjectKind::Device => "device",
            CompObjectKind::Event => "event",
            CompObjectKind::Field => "field",
            CompObjectKind::Group => "group",
            CompObjectKind::Implement => "implement",
            CompObjectKind::Interface => "interface",
            CompObjectKind::Port => "port",
            CompObjectKind::Register => "register",
            CompObjectKind::Subdevice => "subdevice",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompObjectKindDecl {
    pub kind: CompObjectKind,
    pub span: ZeroSpan,
}

impl DeclarationSpan for CompObjectKindDecl {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl LocationSpan for CompObjectKindDecl {
    fn loc_span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl Named for CompObjectKindDecl {
    fn get_name(&self) -> String {
        self.kind_name().to_string()
    }
}

impl CompObjectKindDecl {
    pub fn kind_name(&self) -> &'static str {
        self.kind.kind_name()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompositeObject {
    pub kind: CompObjectKindDecl,
    pub object: DMLObjectCommon,
    pub dims: Vec<ArrayDim>,
    pub doc: Option<Expression>,
    pub statements: Statements,
    pub references: Vec<Reference>,
}

impl DeclarationSpan for CompositeObject {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for CompositeObject {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl CompositeObject {
    pub fn comp_kind(&self) -> CompObjectKind {
        self.kind.kind
    }
    pub fn kind_name(&self) -> &'static str {
        self.kind.kind_name()
    }
}

impl StructureSymbol for CompositeObject {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::CompObject(self.comp_kind())
    }
}

impl Scope for CompositeObject {
    fn create_context(&self) -> ContextKey {
        ContextKey::Structure(SimpleSymbol::make(self, self.kind()))
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        self.statements.scopes()
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.dims.to_symbols();
        symbols.append(&mut self.statements.symbols());
        symbols
    }
    fn defined_references(&self) -> &Vec<Reference> {
        &self.references
    }
}

impl SymbolContainer for CompositeObject {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> { vec![] }
}

impl ScopeContainer for CompositeObject {
    fn scopes(&self) -> Vec<&dyn Scope> { vec![] }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub enum MethodModifier {
    None, Shared, Inline
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MethodArgument {
    Typed(DMLString, DMLType),
    Inline(DMLString),
}

impl DMLNamed for MethodArgument {
    fn name(&self) -> &DMLString {
        match self {
            MethodArgument::Typed(name, _) |
            MethodArgument::Inline(name) => name
        }
    }
}

impl StructureSymbol for MethodArgument {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::MethodArg
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Method {
    pub object: DMLObjectCommon,
    pub arguments: Vec<MethodArgument>,
    pub returns: Vec<DMLType>,
    pub modifier: MethodModifier,
    pub independent: bool,
    // TODO: we could track startup here, but is there any
    // reason to?
    pub memoized: bool,
    pub default: bool,
    pub throws: bool,
    pub body: Statement,
    pub references: Vec<Reference>,
}

impl DeclarationSpan for Method {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for Method {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl StructureSymbol for Method {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Method
    }
}

impl Scope for Method {
    fn create_context(&self) -> ContextKey {
        ContextKey::Method(SimpleSymbol::make(self, self.kind()))
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.arguments.to_symbols();
        symbols.append(&mut self.body.symbols());
        symbols
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        vec![]
    }
    fn defined_references(&self) -> &Vec<Reference> {
        &self.references
    }
}

impl ToStructure<structure::MethodContent> for Method {
    fn to_structure<'a>(content: &structure::MethodContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Method> {
        let mut arguments = vec![];
        for (arg, _) in &content.arguments {
            match arg {
                structure::ArgumentDecl::Typed(ast_decl) =>
                    if let (Some(name), Some(typed)) = ast_decl.with_content(
                        |con|deconstruct_cdecl(con, report, file),
                        (None, None)) {
                        arguments.push(MethodArgument::Typed(name, typed));
                    },
                structure::ArgumentDecl::Inline(_, ast_name) =>
                    if let Some(name) = DMLString::from_token(ast_name, file) {
                        arguments.push(MethodArgument::Inline(name));
                    },
            }
        }
        let mut returns = vec![];
        if let Some((_, _, ast_returns, _)) = &content.returns {
            for (typedecl, _) in ast_returns {
                // TODO: It might be reasonable to insert a dummy type here
                if let Some(ty) = to_type(typedecl, report, file) {
                    returns.push(ty);
                }
            }
        }
        let object = DMLObjectCommon {
            name: DMLString::from_token(&content.name, file)?,
            span: ZeroSpan::from_range(content.range(), file.path),
        };
        let body = StatementKind::to_statement(
            &content.statements, report, file)
            .unwrap_or_else(
                ||StatementKind::Empty(ZeroSpan::from_range(
                    content.statements.range(), file.path)).into());
        let throws = content.throws.is_some();
        let default = content.default.is_some();
        let independent = content.independent.is_some();
        let memoized = content.memoized.is_some();
        let modifier = if let Some(tok) = &content.modifier {
            // This token shouldnt actually be able to be missing
            match tok.read_leaf(file.file).unwrap().as_ref() {
                "inline" => MethodModifier::Inline,
                "shared" => MethodModifier::Shared,
                // Syntax error already handled in parser
                _ => MethodModifier::None,
            }
        } else {
            MethodModifier::None
        };
        let mut references = vec![];
        content.arguments.collect_references(&mut references, file);
        content.returns.collect_references(&mut references, file);
        content.statements.collect_references(&mut references, file);
        Some(Method {
            object, arguments, returns,
            independent, memoized,
            modifier, default, throws, body,
            references,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamValue {
    Set(Expression),
    Auto(ZeroSpan),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub object: DMLObjectCommon,
    pub is_default: bool,
    pub value: Option<ParamValue>,
    pub typed: Option<DMLType>,
}

impl DeclarationSpan for Parameter {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for Parameter {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl StructureSymbol for Parameter {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Parameter
    }
}

impl ToStructure<structure::ParameterContent> for Parameter {
    fn to_structure<'a>(content: &structure::ParameterContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Parameter> {
        // TODO: track whether this was a defining-declaration without type
        // ("param foo := 1;") for purposes of checking when the provisional
        // is activated
        // TODO: We could consider warning here if ":=" syntax is used without
        // provisional active?
        let typed = content.typing.as_ref()
            .and_then(|t|to_type(t, report, file));
        let (is_default, value) = content.def.as_ref()
            .map_or((false, None), |d| match d {
                structure::ParamDef::Set(assigntok, expr) => {
                    (assigntok.get_token()
                     .map_or(false, |rt|rt.kind == TokenKind::Default),
                     ExpressionKind::to_expression(expr, report, file).map(
                         |e|ParamValue::Set(e)))
                },
                structure::ParamDef::Auto(tok) =>
                    (false,
                     Some(ParamValue::Auto(ZeroSpan::from_range(tok.range(),
                                                                file.path)))),
            });
        let object = DMLObjectCommon {
            name: DMLString::from_token(&content.name, file)?,
            span: ZeroSpan::from_range(content.range(), file.path),
        };
        Some(Parameter {
            object, is_default, value, typed,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Initializer {
    span: ZeroSpan,
    kind: InitializerKind
}

impl DeclarationSpan for Initializer {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl From<InitializerKind> for Initializer {
    fn from(from: InitializerKind) -> Initializer {
        let span = match &from {
            InitializerKind::One(expr) => *expr.span(),
            InitializerKind::List(inits) =>
                ZeroSpan::combine(
                    *inits.first().unwrap().span(),
                    *inits.last().unwrap().span()),
            InitializerKind::Struct(fields, ellipsis) => ZeroSpan::combine(
                fields.first().unwrap().0.span,
                if let Some(ellipsis_span) = ellipsis {
                    *ellipsis_span
                } else {
                    fields.last().unwrap().1.span
                }),
        };
        Initializer {
            span,
            kind: from,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InitializerKind {
    One(Expression),
    List(Vec<Box<Initializer>>),
    Struct(Vec<(DMLString, Box<Initializer>)>, Option<ZeroSpan>),
}

fn to_single_initializer<'a>(
    init_content: &misc::SingleInitializerContent,
    report: &mut Vec<LocalDMLError>,
    file: FileSpec<'a>) -> Option<Initializer> {
    Some(match init_content {
        misc::SingleInitializerContent::Expression(expr) =>
            InitializerKind::One(ExpressionKind::to_expression(
                expr, report, file)?).into(),
        misc::SingleInitializerContent::List(_, init_list, _) => {
            let mut inits = vec![];
            for (init_ast, _) in init_list {
                if let Some(init) = init_ast.with_content(|con|to_single_initializer(
                    con, report, file), None) {
                        inits.push(Box::new(init));
                    }
            }
            InitializerKind::List(inits).into()
        },
        misc::SingleInitializerContent::Structure(_, init_list, ellipsis, _)
            => {
                let mut inits = vec![];
                for (assign_ast, _) in init_list {
                    let misc::InitializerStructElem {
                        ident: ident_ast,
                        init: init_ast,
                        ..
                    } = assign_ast;

                    let ident = DMLString::from_token(ident_ast, file);
                    let init = init_ast.with_content(|con|to_single_initializer(
                        con, report, file), None);
                    // Dont store if neither side of the assignment exists
                    if let (Some(id), Some(int)) = (ident, init) {
                        inits.push((id, Box::new(int)));
                    };
                }
                InitializerKind::Struct(inits, ellipsis.as_ref().map(
                    |lt|ZeroSpan::from_range(lt.range(), file.path))).into()
            }
    })
}

fn to_initializer<'a>(content: &misc::InitializerContent,
                      report: &mut Vec<LocalDMLError>,
                      file: FileSpec<'a>) -> Vec<Initializer> {
    match content {
        misc::InitializerContent::Single(single_content) =>
            single_content.with_content(|con|to_single_initializer(
                con, report, file), None).map_or(vec![], |i|vec![i]),
        misc::InitializerContent::Tuple(_, init_list, _) => {
            let mut inits = vec![];
            for (init_ast, _) in init_list {
                if let Some(init) = init_ast.with_content(|con|to_single_initializer(
                    con, report, file), None) {
                        inits.push(init);
                }
            }
            inits
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VariableDeclKind {
    Extern,
    Saved,
    Session,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDecl {
    // While-span is hidden away inside object
    pub object: DMLObjectCommon,
    // This is redundant in structure, but becomes highly useful later
    // when we flatten variables into their decls
    pub kind: VariableDeclKind,
    pub typed: DMLType,
}

impl DeclarationSpan for VariableDecl {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for VariableDecl {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl StructureSymbol for VariableDecl {
    fn kind(&self) -> DMLSymbolKind {
        match self.kind {
            VariableDeclKind::Session => DMLSymbolKind::Session,
            VariableDeclKind::Saved => DMLSymbolKind::Saved,
            VariableDeclKind::Extern => DMLSymbolKind::Extern,
            VariableDeclKind::Local => DMLSymbolKind::Local,
        }
    }
}

fn to_variable_decl_structure<'a>(content: &misc::CDecl,
                                  kind: VariableDeclKind,
                                  report: &mut Vec<LocalDMLError>,
                                  file: FileSpec<'a>) -> Option<VariableDecl> {
    let (name, typed) = content.with_content(
        |con|deconstruct_cdecl(con, report, file),
        (None, None));
    // Fair to discard a variable declaration if it doesnt have a name
    let object = DMLObjectCommon {
        name: name?,
        span: ZeroSpan::from_range(content.range(), file.path),
    };
    Some(VariableDecl {
        object,
        kind,
        typed: typed?,
    })
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    pub vars: Vec<VariableDecl>,
    pub values: Vec<Initializer>,
    pub span: ZeroSpan,
}

impl SymbolContainer for Variable {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.vars.to_symbols()
    }
}

impl DeclarationSpan for Variable {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

pub fn to_variable_structure<'a>(decls: &structure::VarDecl,
                                 initializer: Option<&misc::Initializer>,
                                 kind: VariableDeclKind,
                                 report: &mut Vec<LocalDMLError>,
                                 file: FileSpec<'a>) -> Option<Variable> {
    let values = if let Some(init_ast) = initializer {
        init_ast.with_content(
            |con|to_initializer(con, report, file),
            vec![])
    } else {
        vec![]
    };
    // I _think_ we will always have a real content here
    let vars = match decls {
        structure::VarDecl::One(decl) => {
            vec![to_variable_decl_structure(decl, kind, report, file)?]
        },
        structure::VarDecl::Many(_, decls, _) => {
            decls.iter().filter_map(
                |(d, _)|to_variable_decl_structure(d, kind.clone(),
                                                   report, file)
            ).collect()
        }
    };

    if let Some(init) = initializer {
        // NOTE: this is the half of the initializer arity that
        // can be performed in an isolated context. Checking that
        // the method would return the right number of values
        // is performed in device analysis
        // TODO: if we ever update to latest rust, we can merge this if-case
        // with the next
        let mut is_single_fun = false;

        if let [val] = values.as_slice() {
            if let InitializerKind::One(expr) = &val.kind {
                is_single_fun = matches!(
                    expr.as_ref(), ExpressionKind::FunctionCall(_));
            }
        }
        if !is_single_fun && vars.len() != values.len(){
            report.push(LocalDMLError {
                range: init.range(),
                description: "Wrong number of \
                              initializers in declaration".to_string(),
            });
        }
    }

    Some(Variable {
        vars,
        values,
        span: ZeroSpan::from_range(
            initializer.map_or_else(
                ||decls.range(),
                |i|ZeroRange::combine(decls.range(), i.range())),
            file.path),
    })
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    pub object: DMLObjectCommon,
    pub value: Expression,
}

impl DeclarationSpan for Constant {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for Constant {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

impl StructureSymbol for Constant {
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::Constant
    }
}

impl ToStructure<structure::ConstantContent> for Constant {
    fn to_structure<'a>(content: &structure::ConstantContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Constant> {
        let value = ExpressionKind::to_expression(
            &content.expression, report, file)?;
        let name = DMLString::from_token(&content.name, file);
        let object = DMLObjectCommon {
            name: name?,
            span: ZeroSpan::from_range(content.range(), file.path),
        };
        Some(Constant {
            object,
            value,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Typedef {
    pub is_extern: bool,
    pub object: DMLObjectCommon,
    pub typed: DMLType,
}

impl ToStructure<structure::TypedefContent> for Typedef {
    fn to_structure<'a>(content: &structure::TypedefContent,
                        report: &mut Vec<LocalDMLError>,
                        file: FileSpec<'a>) -> Option<Typedef> {
        let is_extern = content.externtok.is_some();
        let (name, typed) = content.decl.with_content(
            |con|deconstruct_cdecl(con, report, file),
            (None, None));
        let object = DMLObjectCommon {
            name: name?,
            span: ZeroSpan::from_range(content.range(), file.path),
        };
        Some(Typedef {
            is_extern,
            object,
            // TODO: We need to be able to store partially resolved objects
            // TODO/NOTE: Applies at least everywhere there is a deconstruct_cdecl
            typed: typed?,
        })
    }
}

impl DeclarationSpan for Typedef {
    fn span(&self) -> &ZeroSpan {
        self.object.span()
    }
}

impl DMLNamed for Typedef {
    fn name(&self) -> &DMLString {
        self.object.name()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLObject {
    Bitorder(Bitorder),
    CompositeObject(CompositeObject),
    Constant(Constant),
    Device(Device),
    Version(Version),
    Export(Export),
    Extern(Variable),
    Footer(CBlock),
    Header(CBlock),
    Hook(Hook),
    Import(Import),
    Loggroup(Loggroup),
    Method(Method),
    Parameter(Parameter),
    Saved(Variable),
    Session(Variable),
    Template(Template),
    Typedef(Typedef),
}

impl DMLObject {
    pub fn kind_name(&self) -> &'static str {
        match self {
            Self::CompositeObject(s) => s.kind.kind_name(),
            Self::Bitorder(_) => "bitorder",
            Self::Constant(_) => "constant",
            Self::Device(_) => "device",
            Self::Version(_) => "version",
            Self::Export(_) => "export",
            Self::Extern(_) => "extern",
            Self::Footer(_) => "footer",
            Self::Header(_) => "header",
            Self::Hook(_) => "hook",
            Self::Import(_) => "import",
            Self::Loggroup(_) => "loggroup",
            Self::Method(_) => "method",
            Self::Parameter(_) => "parameter",
            Self::Saved(_) => "saved",
            Self::Session(_) => "session",
            Self::Template(_) => "template",
            Self::Typedef(_) => "typedef",
        }
    }
}

impl DeclarationSpan for DMLObject {
    fn span(&self) -> &ZeroSpan {
        match self {
            Self::CompositeObject(c) => c.span(),
            Self::Bitorder(c) => c.span(),
            Self::Constant(c) => c.span(),
            Self::Device(c) => c.span(),
            Self::Version(c) => c.span(),
            Self::Export(c) => c.span(),
            Self::Extern(c) => c.span(),
            Self::Footer(c) => c.span(),
            Self::Header(c) => c.span(),
            Self::Hook(c) => c.span(),
            Self::Import(c) => c.span(),
            Self::Loggroup(c) => c.span(),
            Self::Method(c) => c.span(),
            Self::Parameter(c) => c.span(),
            Self::Saved(c) => c.span(),
            Self::Session(c) => c.span(),
            Self::Template(c) => c.span(),
            Self::Typedef(c) => c.span(),
        }
    }
}

fn to_objectstatement<'a>(content: &structure::DMLObjectContent,
                          report: &mut Vec<LocalDMLError>,
                          file: FileSpec<'a>) -> Option<DMLStatementKind> {
    Some(match content {
        structure::DMLObjectContent::Attribute(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Bank(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Bitorder(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Bitorder(
                Bitorder::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Connection(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Constant(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Constant(
                Constant::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Device(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Device(
                Device::to_structure(con, report, file)?))),
        structure::DMLObjectContent::DMLVersion(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Version(
                Version::to_structure(con, report, file)?))),
        structure::DMLObjectContent::ErrorObject(con) =>
            DMLStatementKind::Error(
                Error::to_structure(con, report, file)?),
        structure::DMLObjectContent::Export(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Export(
                Export::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Extern(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Extern(
                to_variable_structure(&con.cdecl,
                                      con.initializer.as_ref().map(|(_,i)|i),
                                      VariableDeclKind::Extern,
                                      report, file)?))),
        structure::DMLObjectContent::Event(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Field(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_field(con, report, file)?))),
        structure::DMLObjectContent::Footer(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Footer(
                CBlock::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Group(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::HashIf(con) =>
            DMLStatementKind::Statement(DMLStatement::HashIf(
                HashIf::to_structure(con, report, file)?)),
        structure::DMLObjectContent::Header(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Header(
                CBlock::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Hook(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Hook(
                Hook::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Implement(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Import(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Import(
                Import::to_structure(con, report, file)?))),
        structure::DMLObjectContent::InEach(con) =>
            DMLStatementKind::InEach(InEach::to_structure(con, report, file)?),
        structure::DMLObjectContent::Instantiation(con) =>
            DMLStatementKind::Instantiation(
                Instantiation::to_structure(con, report, file)?),
        structure::DMLObjectContent::Interface(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Loggroup(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Loggroup(
                Loggroup::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Method(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Method(
                Method::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Parameter(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Parameter(
                Parameter::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Port(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Provisional(con) => {
            // TODO: There is an incongruity here with how this is handled vs
            // version, device, and bitorder. Consider letting this pass through
            // structure and then never be used
            report.push(LocalDMLError {
                range: con.range(),
                description: "Provisional declaration must immediately follow \
                              the version declaration".to_string(),
            });
            return None;
        },
        structure::DMLObjectContent::Register(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_register(con, report, file)?))),
        structure::DMLObjectContent::Saved(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Saved(
                to_variable_structure(&con.cdecl,
                                      con.initializer.as_ref().map(|(_,i)|i),
                                      VariableDeclKind::Saved,
                                      report, file)?))),
        structure::DMLObjectContent::Session(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Session(
                to_variable_structure(&con.cdecl,
                                      con.initializer.as_ref().map(|(_,i)|i),
                                      VariableDeclKind::Session,
                                      report, file)?))),
        structure::DMLObjectContent::Subdevice(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(
                DMLObject::CompositeObject(
                    to_composite_object(con, report, file)?))),
        structure::DMLObjectContent::Template(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Template(
                Template::to_structure(con, report, file)?))),
        structure::DMLObjectContent::Typedef(con) =>
            DMLStatementKind::Statement(DMLStatement::Object(DMLObject::Typedef(
                Typedef::to_structure(con, report, file)?))),
    })
}

fn interpret_kind(tok: &LeafToken, file: FileSpec<'_>)
                  -> CompObjectKindDecl {
    // I am _pretty_ sure this cannot actually fail, as the parser has no reason
    // to generate a compobject unless it sees one of these
    let realtok = tok.get_token().unwrap();
    CompObjectKindDecl {
        span: ZeroSpan::from_range(
            realtok.range, file.path),
        kind: match realtok.kind {
            TokenKind::Attribute => CompObjectKind::Attribute,
            TokenKind::Bank => CompObjectKind::Bank,
            TokenKind::Connect => CompObjectKind::Connect,
            TokenKind::Event => CompObjectKind::Event,
            TokenKind::Field => CompObjectKind::Field,
            TokenKind::Group => CompObjectKind::Group,
            TokenKind::Implement => CompObjectKind::Implement,
            TokenKind::Interface => CompObjectKind::Interface,
            TokenKind::Port => CompObjectKind::Port,
            TokenKind::Register => CompObjectKind::Register,
            TokenKind::Subdevice => CompObjectKind::Subdevice,
            _ => unreachable!("Should have been invalidated by parser"),
        }
    }
}

fn to_composite_object<'a>(content: &structure::CompositeObjectContent,
                           report: &mut Vec<LocalDMLError>,
                           file: FileSpec<'a>) -> Option<CompositeObject> {
    let mut statements = content.statements.with_content(
        |con|to_objectstatements(con, report, file), Statements::empty());
    let doc = match &content.documentation {
        Some(expr) => ExpressionKind::to_expression(expr, report, file),
        None => None,
    };
    if let Some((_, instantiations)) = &content.instantiation {
        let mut names = vec![];
        match instantiations {
            structure::Instantiation::One(tok) =>
                if let Some(dmlstr) = DMLString::from_token(tok, file) {
                    names.push(dmlstr)
                },
            structure::Instantiation::Many(_, insts, _) =>
                for (tok, _) in insts {
                    if let Some(dmlstr) = DMLString::from_token(tok, file) {
                        names.push(dmlstr)
                    }
                },
        }
        statements.instantiations.push(Instantiation {
            span: ZeroSpan::from_range(
                content.instantiation.range(), file.path),
            names,
        })
    }
    let mut dims = vec![];
    for (start, indexname, _, sizedecl, end) in &content.dimensions {
        let name = DMLString::from_token(indexname, file);
        let size = match &sizedecl {
            structure::ArraySize::Defined(expr) =>
                Some(ExpressionKind::to_expression(expr, report, file)?),
            structure::ArraySize::Implicit(_tok) =>
                None,
        };
        let span = ZeroSpan::from_range(
            ZeroRange::combine(start.range(), end.range()),
            file.path);
        dims.push(ArrayDim {
            span, indexvar: name?, size,
        });
    }
    let object = DMLObjectCommon {
        name: DMLString::from_token(&content.name, file)?,
        span: ZeroSpan::from_range(content.range(), file.path),
    };
    let mut references = vec![];
    content.dimensions.references(&mut references, file);
    content.instantiation.references(&mut references, file);
    content.statements.references(&mut references, file);
    let kind = interpret_kind(&content.kind, file);
    references.push(Reference::global_from_string(
        kind.kind_name().to_string(),
        ZeroSpan::from_range(content.kind.range(),
                             file.path),
        ReferenceKind::Template));
    Some(CompositeObject {
        kind,
        object,
        doc,
        dims,
        statements,
        references,
    })
}

fn to_register<'a>(content: &structure::RegisterContent,
                   report: &mut Vec<LocalDMLError>,
                   file: FileSpec<'a>) -> Option<CompositeObject> {
    let mut mb_obj = to_composite_object(&content.obj, report, file);
    if let Some(obj) = &mut mb_obj {
        if let Some((sizetok, sizeexpr)) = &content.size {
            if let Some(actualsize) = ExpressionKind::to_expression(
                sizeexpr, report, file) {
                if let Some(name) = DMLString::from_token(sizetok, file) {
                    obj.statements.statements.push(
                        DMLStatement::Object(DMLObject::Parameter(Parameter {
                            object: DMLObjectCommon {
                                name,
                                span: ZeroSpan::from_range(
                                    sizetok.range(), file.path),
                            },
                            is_default: false,
                            value: Some(ParamValue::Set(actualsize)),
                            typed: None,
                        })));
                }
            }
        }
        if let Some((addrtok, offsexpr)) = &content.offset {
            if let Some(actualoffset) = ExpressionKind::to_expression(
                offsexpr, report, file) {
                obj.statements.statements.push(
                    DMLStatement::Object(DMLObject::Parameter(Parameter {
                        object: DMLObjectCommon {
                            name: Value::<String> {
                                val: "offset".to_string(),
                                span: ZeroSpan::from_range(
                                    addrtok.range(), file.path),
                            },
                            span: ZeroSpan::from_range(
                                content.offset.range(), file.path),
                        },
                        is_default: false,
                        value: Some(ParamValue::Set(actualoffset)),
                        typed: None,
                    })));
            }
        }
        content.size.references(&mut obj.references, file);
        content.offset.references(&mut obj.references, file);
    }
    mb_obj
}

fn to_field<'a>(content: &structure::FieldContent,
                report: &mut Vec<LocalDMLError>,
                file: FileSpec<'a>) -> Option<CompositeObject> {
    let mut mb_obj = to_composite_object(&content.obj, report, file);
    if let Some(obj) = &mut mb_obj {
        if let Some(
            (at, _, msb_spec, lsb_spec, _)) = &content.bitrange {
            let msb_expr = ExpressionKind::to_expression(
                msb_spec, report, file);
            let lsb_expr = if let Some((_, lsb_ispec)) = lsb_spec {
                ExpressionKind::to_expression(lsb_ispec, report, file)
            } else {
                msb_expr.clone()
            };
            if let Some(msb) = msb_expr {
                obj.statements.statements.push(
                    DMLStatement::Object(DMLObject::Parameter(Parameter {
                        object: DMLObjectCommon {
                            name: Value::<String> {
                                val: "msb".to_string(),
                                span: ZeroSpan::from_range(at.range(), file.path),
                            },
                            span: ZeroSpan::from_range(
                                content.bitrange.range(), file.path),
                        },
                        is_default: false,
                        value: Some(ParamValue::Set(msb)),
                        typed: None,
                    })));
            }
            if let Some(lsb) = lsb_expr {
                obj.statements.statements.push(
                    DMLStatement::Object(DMLObject::Parameter(Parameter {
                        object: DMLObjectCommon {
                            name: Value::<String> {
                                val: "lsb".to_string(),
                                span: ZeroSpan::from_range(
                                    at.range(), file.path),
                            },
                            span: ZeroSpan::from_range(
                                content.bitrange.range(), file.path),
                        },
                        is_default: false,
                        value: Some(ParamValue::Set(lsb)),
                        typed: None,
                    })));
            }
        }
        content.bitrange.references(&mut obj.references, file);
    }
    mb_obj
}

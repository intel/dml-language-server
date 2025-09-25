//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::fmt::{Display, Formatter, self as fmt};
use std::path::PathBuf;

use log::trace;

use crate::analysis::structure::objects::{Bitorder, CBlock, CompositeObject,
                                          CompObjectKind, Constant, Device,
                                          DMLObject, DMLStatement, Error,
                                          make_statements, Hook,
                                          Loggroup, Import, InEach,
                                          Instantiation, Method, MethodModifier,
                                          Export, Parameter, Statements,
                                          Template, Typedef,
                                          Variable, Version, ToStructure};
use crate::analysis::structure::expressions::{Expression};
use crate::analysis::FileSpec;
use crate::analysis::parsing::tree::{ZeroRange, ZeroSpan, TreeElement};
use crate::analysis::parsing::structure;
use crate::analysis::{DeclarationSpan, LocationSpan, Named,
                      DMLNamed, LocalDMLError};
use crate::analysis::symbols::{DMLSymbolKind, StructureSymbol,
                               SimpleSymbol, SymbolContainer,
                               MakeSymbolContainer};
use crate::analysis::reference::{Reference};
use crate::analysis::scope::{Scope, ScopeContainer, MakeScopeContainer,
                             ContextKey};


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExistCondition {
    Always,
    // The bool here is whether the expression should be reversed, as the
    // thing existing is in an else branch
    // Note: Since Expression contains location info, we can use it to identify
    // which hashif branch this is
    // Note: Right now we are implicitly assuming this has some ordering, such
    // that two objectdecls within the same hashif block will have the same
    // existcondition
    Conditional(Vec<(bool, Expression)>),
}

impl ExistCondition {
    pub fn guaranteed_overlaps(&self, other: &ExistCondition) -> bool {
        match (self, other) {
            (ExistCondition::Always, ExistCondition::Always) => true,
            (ExistCondition::Conditional(selfvec),
             ExistCondition::Conditional(othervec)) => {
                // This becomes equivalent to checking if they are in the
                // same nested hashifs
                // As it turns out, collision is guaranted regardless of
                // which branch they are in
                for ((_, cond1),
                     (_, cond2)) in selfvec.iter().zip(othervec.iter()) {
                    if cond1 != cond2 {
                        return false;
                    }
                }
                true
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StatementContext {
    Object,
    HashIfTrue,
    HashIfElse,
    InEach,
    Template,
}

impl Display for StatementContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f,
               "{}",
               match self {
                   StatementContext::HashIfTrue => "'#if'",
                   StatementContext::HashIfElse => "'#else'",
                   StatementContext::Object => "an object declaration",
                   StatementContext::InEach => "an 'in each' declaration",
                   StatementContext::Template => "a 'template' definition",
               })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectDecl<T> where T: DeclarationSpan
{
    pub cond: ExistCondition,
    // True backing object
    // TODO: I wish we didn't have to own this. If memory footprinting
    // shows this to be a real issue, refactor this to be
    // a box or something so that structure and toplevel can
    // share data
    pub obj: T,
    pub spec: StatementSpec,
}

impl <T: Named> Named for ObjectDecl<T>
where T: DeclarationSpan {
    fn get_name(&self) -> String {
        self.obj.get_name()
    }
}

impl <T: LocationSpan> LocationSpan for ObjectDecl<T>
where T: DeclarationSpan {
    fn loc_span(&self) -> &ZeroSpan {
        self.obj.loc_span()
    }
}

impl <T> DeclarationSpan for ObjectDecl<T>
where T: DeclarationSpan {
    fn span(&self) -> &ZeroSpan {
        self.obj.span()
    }
}

impl <T: ScopeContainer> ScopeContainer for ObjectDecl<T>
where T: DeclarationSpan {
    fn scopes(&self) -> Vec<&dyn Scope> {
        self.spec.scopes()
    }
}

impl <T: SymbolContainer> SymbolContainer for ObjectDecl<T>
where T: DeclarationSpan {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.spec.symbols();
        symbols.append(&mut self.obj.symbols());
        symbols
    }
}

impl <T: StructureSymbol + DMLNamed> StructureSymbol for ObjectDecl<T>
where T: DeclarationSpan {
    fn kind(&self) -> DMLSymbolKind {
        self.obj.kind()
    }
}

impl <T: Scope> Scope for ObjectDecl<T>
where T: DeclarationSpan {
    fn create_context(&self) -> ContextKey {
        self.obj.create_context()
    }
    fn defined_references(&self) -> &Vec<Reference> {
        self.obj.defined_references()
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        self.obj.defined_scopes()
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.obj.defined_symbols()
    }
}

impl <T: Clone> ObjectDecl<T>
where T: DeclarationSpan {
    pub fn depending_on_context(obj: &T,
                                context: StatementContext,
                                conds: &[(bool, Expression)])
    -> ObjectDecl<T> {
        match context {
            StatementContext::HashIfTrue |
            StatementContext::HashIfElse =>
                ObjectDecl::conditional(obj, conds),
            _ => ObjectDecl::always(obj),
        }
    }

    pub fn always(obj: &T) -> ObjectDecl<T> {
        ObjectDecl {
            cond: ExistCondition::Always,
            obj: obj.clone(),
            spec: StatementSpec::empty(),
        }
    }
    pub fn conditional(obj: &T,
                       conds: &[(bool, Expression)])
                       -> ObjectDecl<T> {
        ObjectDecl {
            cond: ExistCondition::Conditional(conds.to_vec()),
            obj: obj.clone(),
            spec: StatementSpec::empty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StatementSpecStatement<'t> {
    Object(&'t ObjectDecl<CompositeObject>),
    Instantiation(&'t ObjectDecl<Instantiation>),
    Import(&'t ObjectDecl<Import>),
    Parameter(&'t ObjectDecl<Parameter>),
    Saved(&'t ObjectDecl<Variable>),
    Session(&'t ObjectDecl<Variable>),
    Error(&'t ObjectDecl<Error>),
    Method(&'t ObjectDecl<Method>),
}

impl StatementSpec {
    pub fn all_statements(&self) -> Vec<StatementSpecStatement<'_>> {
        self.objects.iter().map(|o|StatementSpecStatement::Object(o))
            .chain(self.instantiations.iter()
                   .map(|i|StatementSpecStatement::Instantiation(i)))
            .chain(self.imports.iter()
                   .map(|i|StatementSpecStatement::Import(i)))
            .chain(self.params.iter()
                   .map(|p|StatementSpecStatement::Parameter(p)))
            .chain(self.saveds.iter()
                   .map(|s|StatementSpecStatement::Saved(s)))
            .chain(self.sessions.iter()
                   .map(|s|StatementSpecStatement::Session(s)))
            .chain(self.errors.iter()
                   .map(|e|StatementSpecStatement::Error(e)))
            .chain(self.methods.iter()
                   .map(|m|StatementSpecStatement::Method(m)))
            .chain(self.objects.iter().flat_map(
                |s|s.spec.all_statements().into_iter()))
            .chain(self.ineachs.iter().flat_map(
                |ie|ie.spec.all_statements().into_iter()))
            .collect()
    }
}

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementSpec {
    pub objects: Vec<ObjectDecl<CompositeObject>>,
    pub sessions: Vec<ObjectDecl<Variable>>,
    pub saveds: Vec<ObjectDecl<Variable>>,
    pub params: Vec<ObjectDecl<Parameter>>,
    pub methods: Vec<ObjectDecl<Method>>,
    pub ineachs: Vec<ObjectDecl<InEach>>,
    pub exports: Vec<ObjectDecl<Export>>,
    pub hooks: Vec<ObjectDecl<Hook>>,
    pub instantiations: Vec<ObjectDecl<Instantiation>>,
    // It is a bit strange to keep this here, but we need this
    // in the contexts where we only have a spec. It is empty everywhere
    // except toplevel
    pub imports: Vec<ObjectDecl<Import>>,
    pub errors: Vec<ObjectDecl<Error>>,
    pub constants: Vec<ObjectDecl<Constant>>,
}

impl SymbolContainer for StatementSpec {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.sessions.symbols();
        symbols.append(&mut self.saveds.symbols());
        symbols.append(&mut self.params.to_symbols());
        symbols.append(&mut self.objects.to_symbols());
        symbols.append(&mut self.methods.to_symbols());
        symbols.append(&mut self.constants.to_symbols());
        symbols.append(&mut self.hooks.to_symbols());
        symbols
    }
}

impl ScopeContainer for StatementSpec {
    fn scopes(&self) -> Vec<&dyn Scope> {
        let mut scopes = self.methods.to_scopes();
        scopes.append(&mut self.objects.to_scopes());
        scopes.append(&mut self.ineachs.to_scopes());
        scopes
    }
}

impl StatementSpec {
    pub fn empty() -> StatementSpec {
        StatementSpec {
            objects: vec![],
            sessions: vec![],
            saveds: vec![],
            params: vec![],
            constants: vec![],
            methods: vec![],
            hooks: vec![],
            instantiations: vec![],
            errors: vec![],
            ineachs: vec![],
            exports: vec![],
            imports: vec![],
        }
    }
    pub fn consume(mut self,
                   objects: &mut Vec<ObjectDecl<CompositeObject>>,
                   sessions: &mut Vec<ObjectDecl<Variable>>,
                   saveds: &mut Vec<ObjectDecl<Variable>>,
                   params: &mut Vec<ObjectDecl<Parameter>>,
                   constants: &mut Vec<ObjectDecl<Constant>>,
                   methods: &mut Vec<ObjectDecl<Method>>,
                   instantiations: &mut Vec<ObjectDecl<Instantiation>>,
                   ineachs: &mut Vec<ObjectDecl<InEach>>,
                   exports: &mut Vec<ObjectDecl<Export>>,
                   errors: &mut Vec<ObjectDecl<Error>>,
                   imports: &mut Vec<ObjectDecl<Import>>) {
        objects.append(&mut self.objects);
        constants.append(&mut self.constants);
        sessions.append(&mut self.sessions);
        saveds.append(&mut self.saveds);
        params.append(&mut self.params);
        methods.append(&mut self.methods);
        instantiations.append(&mut self.instantiations);
        ineachs.append(&mut self.ineachs);
        exports.append(&mut self.exports);
        errors.append(&mut self.errors);
        imports.append(&mut self.imports);
    }
}

// Flattens hashifs
fn flatten_hashif_branch(context: StatementContext,
                         stmnts: &Statements,
                         conds: Vec<(bool, Expression)>,
                         report: &mut Vec<LocalDMLError>)
                         -> StatementSpec {
    let mut objects = vec![];
    let mut sessions = vec![];
    let mut saveds = vec![];
    let mut constants = vec![];
    let mut methods = vec![];
    let mut params = vec![];
    let mut instantiations = vec![];
    let mut imports = vec![];
    let mut ineachs = vec![];
    let mut exports = vec![];
    let mut hooks = vec![];
    let mut errors = vec![];
    for inst in &stmnts.instantiations {
        instantiations.push(ObjectDecl::depending_on_context(
            inst, context, &conds));
    }
    for err in &stmnts.errors {
        errors.push(ObjectDecl::depending_on_context(
            err, context, &conds));
    }
    for ineach in stmnts.ineachs.iter() {
        let spec = flatten_hashif_branch(StatementContext::InEach,
                                         &ineach.statements,
                                         vec![], report);
        ineachs.push(
            ObjectDecl {
                cond: match context {
                    StatementContext::HashIfTrue |
                    StatementContext::HashIfElse =>
                        ExistCondition::Conditional(
                            conds.clone()),
                    _ => ExistCondition::Always,
                },
                obj: ineach.clone(),
                spec,
            });
    }

    for stmnt in stmnts.statements.iter() {
        match stmnt {
            DMLStatement::Object(obj) => {
                match obj {
                    DMLObject::Version(ver) =>
                        report.push(LocalDMLError {
                            range: ver.span.range,
                            description: "Version declaration must be first statement in file".to_string(),
                        }),
                    DMLObject::Device(dev) =>
                        report.push(LocalDMLError {
                            range: dev.span.range,
                            description: "Device declaration must be second statement in file".to_string(),
                        }),
                    DMLObject::Bitorder(bit) =>
                        report.push(LocalDMLError {
                            range: bit.span.range,
                            description: "Bitorder declaration must follow a device declaration".to_string(),
                        }),
                    DMLObject::Constant(con) =>
                        report.push(LocalDMLError {
                            range: con.object.name.span.range,
                            description: format!(
                                "Constant declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::Extern(ext) =>
                        report.push(LocalDMLError {
                            range:
                            ext.vars.first().unwrap().object.name.span.range,
                            description: format!(
                                "Extern declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::Template(tmpl) =>
                        report.push(LocalDMLError {
                            range: tmpl.object.name.span.range,
                            description: format!(
                                "Template declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::Header(block) =>
                        report.push(LocalDMLError {
                            range: block.span.range,
                            description: format!(
                                "Header declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::Footer(block) =>
                        report.push(LocalDMLError {
                            range: block.span.range,
                            description: format!(
                                "Footer declaration not allowed in {}",
                                context),
                        }),
                    // TODO: Verify that conditions are constants
                    // or builtin parameters
                    DMLObject::Import(import) =>
                        imports.push(ObjectDecl::depending_on_context(
                            import, context, &conds)),
                    DMLObject::Loggroup(loggroup) =>
                        report.push(LocalDMLError {
                            range: loggroup.span.range,
                            description: format!(
                                "Loggroup declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::Typedef(typdef) =>
                        report.push(LocalDMLError {
                            range: typdef.object.span.range,
                            description: format!(
                                "Typedef declaration not allowed in {}",
                                context),
                        }),
                    DMLObject::CompositeObject(compobj) => {
                        let subspec = flatten_hashif_branch(
                            StatementContext::Object,
                            &compobj.statements,
                            vec![], report);
                        objects.push(
                            ObjectDecl {
                                cond: match context {
                                    StatementContext::HashIfTrue |
                                    StatementContext::HashIfElse =>
                                        ExistCondition::Conditional(
                                            conds.clone()),
                                    _ => ExistCondition::Always,
                                },
                                obj: compobj.clone(),
                                spec: subspec,
                            });
                    },
                    DMLObject::Export(exp) =>
                        exports.push(ObjectDecl::depending_on_context(
                            exp, context, &conds)),
                    DMLObject::Hook(hook)=>
                        hooks.push(ObjectDecl::depending_on_context(
                            hook, context, &conds)),
                    DMLObject::Method(meth) =>
                    // This error has already been reported, but
                    // we also need to clear our and pretend
                    // that the method isnt saved to avoid
                    // errors in the analysis logic
                        if meth.modifier == MethodModifier::Shared &&
                        context != StatementContext::Template {
                            trace!("Set {:?} to be unshared", meth);
                            let mut modified_method = meth.clone();
                            modified_method.modifier
                                = MethodModifier::None;
                            methods.push(
                                ObjectDecl::depending_on_context(
                                    &modified_method, context, &conds))
                        } else {
                            methods.push(
                                ObjectDecl::depending_on_context(
                                    meth, context, &conds))
                        },
                    DMLObject::Saved(saved) =>
                        saveds.push(ObjectDecl::depending_on_context(
                            saved, context, &conds)),
                    DMLObject::Session(sess) =>
                        sessions.push(ObjectDecl::depending_on_context(
                            sess, context, &conds)),
                    DMLObject::Parameter(param) =>
                            params.push(ObjectDecl::depending_on_context(
                                param, context, &conds)),
                }
            },
            DMLStatement::HashIf(hi) => {
                let mut true_conds = conds.clone();
                true_conds.push(
                    (true, hi.condition.clone()));
                let mut false_conds = conds.clone();
                false_conds.push(
                    (false, hi.condition.clone()));
                let truebranchspec = flatten_hashif_branch(
                    StatementContext::HashIfTrue,
                    &hi.truebranch,
                    true_conds,
                    report);
                let falsebranchspec = flatten_hashif_branch(
                    StatementContext::HashIfElse,
                    &hi.falsebranch,
                    false_conds,
                    report);
                truebranchspec.consume(&mut objects,
                                       &mut sessions,
                                       &mut saveds,
                                       &mut params,
                                       &mut constants,
                                       &mut methods,
                                       &mut instantiations,
                                       &mut ineachs,
                                       &mut exports,
                                       &mut errors,
                                       &mut vec![]);
                falsebranchspec.consume(&mut objects,
                                        &mut sessions,
                                        &mut saveds,
                                        &mut params,
                                        &mut constants,
                                        &mut methods,
                                        &mut instantiations,
                                        &mut ineachs,
                                        &mut exports,
                                        &mut errors,
                                        &mut vec![]);
            },
        }
    }
    StatementSpec {
        objects,
        sessions,
        saveds,
        methods,
        params,
        constants,
        ineachs,
        exports,
        hooks,
        instantiations,
        errors,
        imports: vec![],
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TopLevel {
    pub start_of_file: ZeroSpan,
    pub filespan: ZeroSpan,
    // This duplicates some info from filespan but with nicer lifetimes
    pub filename: PathBuf,
    pub version: Option<Version>,
    pub device: Option<Device>,
    // These are all outside regular namespace, so makes sense to isolate them
    pub bitorder: Option<Bitorder>,
    pub loggroups: Vec<Loggroup>,
    pub cblocks: Vec<CBlock>,
    pub externs: Vec<Variable>,
    pub typedefs: Vec<Typedef>,
    pub templates: Vec<ObjectDecl<Template>>,
    pub references: Vec<Reference>,
    pub spec: StatementSpec,
    // The philosophy for duplicate templates is to track them,
    // analyze within them, but not actually instantiate them. So keep
    // track of what not to instantiate here
}

impl Named for TopLevel {
    fn get_name(&self) -> String {
        self.filespan.path().display().to_string()
    }
}

impl LocationSpan for TopLevel {
    fn loc_span(&self) -> &ZeroSpan {
        &self.start_of_file
    }
}

impl DeclarationSpan for TopLevel {
    fn span(&self) -> &ZeroSpan {
        &self.filespan
    }
}

impl Scope for TopLevel {
    fn create_context(&self) -> ContextKey {
        ContextKey::Structure(SimpleSymbol::make(
            self, DMLSymbolKind::CompObject(CompObjectKind::Device)))
    }
    fn defined_references(&self) -> &Vec<Reference> {
        &self.references
    }
    fn defined_scopes(&self) -> Vec<&dyn Scope> {
        let mut scopes = self.spec.scopes();
        scopes.append(&mut self.templates.to_scopes());
        scopes
    }
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol> {
        let mut symbols = self.spec.symbols();
        symbols.append(&mut self.templates.to_symbols());
        symbols
    }
}

impl fmt::Display for TopLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        if let Some(ver) = &self.version {
            writeln!(f, "\tversion: {}.{}",
                     ver.major_version.val,
                     ver.minor_version.val)?;
        }
        if let Some(dev) = &self.device {
            writeln!(f, "\tdevice: {}", dev.name.val)?;
        }
        write!(f, "\ttemplates: [")?;
        for template in &self.templates {
            write!(f, "{}, ", template.obj.object.name.val)?;
        }
        writeln!(f, "]")?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

// Rudimentary checks for toplevel only
impl TopLevel {
    pub fn from_ast(ast: &structure::TopAst,
                    report: &mut Vec<LocalDMLError>,
                    filespec: FileSpec<'_>)
                    -> TopLevel {

        let version = Version::to_structure(&ast.version,
                                            report,
                                            filespec);

        if version.is_none() {
            report.push(LocalDMLError {
                range: ZeroRange::invalid(),
                description:
                "First statement must be version declaration ('dml 1.4;')"
                    .to_string(),
            });
        };

        let device = ast.device.as_ref().and_then(
            |devcon|Device::to_structure(devcon,
                                         report,
                                         filespec));
        let bitorder = ast.bitorder.as_ref().and_then(
            |bitcon|Bitorder::to_structure(bitcon,
                                           report,
                                           filespec));

        let statements = make_statements(&ast.declarations, report, filespec);
        let stmnts = statements.statements;
        let mut instantiations = statements.instantiations.iter().map(
            |stmnt|ObjectDecl::always(stmnt)).collect();
        let mut errors = statements.errors.iter().map(
            |stmnt|ObjectDecl::always(stmnt)).collect();
        let mut ineachs = vec![];

        for ineach in &statements.ineachs {
            let spec = flatten_hashif_branch(StatementContext::Object,
                                             &ineach.statements,
                                             vec![], report);
            ineachs.push(
                ObjectDecl {
                    cond: ExistCondition::Always,
                    obj: ineach.clone(),
                    spec,
                });
        }

        let mut templates = vec![];
        let mut loggroups = vec![];
        let mut constants = vec![];
        let mut externs = vec![];
        let mut typedefs = vec![];
        let mut cblocks = vec![];
        let mut imports = vec![];
        let mut objects = vec![];
        let mut sessions = vec![];
        let mut saveds = vec![];
        let mut methods = vec![];
        let mut params = vec![];
        let mut exports = vec![];
        let mut hooks = vec![];

        for maybeobj in &stmnts {
            match maybeobj {
                DMLStatement::Object(obj) => {
                    match obj {
                        DMLObject::Version(ver) =>
                            report.push(LocalDMLError {
                                range: ver.span.range,
                                description: "Version declaration must be first statement in file".to_string(),
                            }),
                        DMLObject::Device(dev) =>
                            report.push(LocalDMLError {
                                range: dev.span.range,
                                description: "Device declaration must be second statement in file".to_string(),
                            }),
                        DMLObject::Bitorder(bit) =>
                            report.push(LocalDMLError {
                                range: bit.span.range,
                                description: "Bitorder declaration must follow a device declaration".to_string(),
                            }),
                        DMLObject::Loggroup(log) =>
                            loggroups.push(log.clone()),
                        DMLObject::Constant(con) =>
                            constants.push(ObjectDecl::always(con)),
                        DMLObject::Extern(ext) =>
                            externs.push(ext.clone()),
                        DMLObject::Template(tmpl) => {
                            let subspec = flatten_hashif_branch(
                                StatementContext::Template,
                                &tmpl.statements,
                                vec![],
                                report);
                            templates.push(
                                ObjectDecl {
                                    cond: ExistCondition::Always,
                                    obj: tmpl.clone(),
                                    spec: subspec,
                                });
                        },
                        DMLObject::Header(block) |
                        DMLObject::Footer(block) =>
                            cblocks.push(block.clone()),
                        DMLObject::Import(import) =>
                            imports.push(ObjectDecl::always(import)),
                        DMLObject::Typedef(typedef) =>
                            typedefs.push(typedef.clone()),
                        DMLObject::CompositeObject(compobj) => {
                            let subspec = flatten_hashif_branch(
                                StatementContext::Object,
                                &compobj.statements,
                                vec![], report);
                            objects.push(
                                ObjectDecl {
                                    cond: ExistCondition::Always,
                                    obj: compobj.clone(),
                                    spec: subspec,
                                });
                        },
                        DMLObject::Export(exp) =>
                            exports.push(ObjectDecl::always(exp)),
                        DMLObject::Hook(hook)=>
                            hooks.push(ObjectDecl::always(hook)),
                        DMLObject::Method(meth) =>
                            if meth.modifier == MethodModifier::Shared {
                                trace!("Set {:?} to be unshared", meth);
                                let mut modified_method = meth.clone();
                                modified_method.modifier
                                    = MethodModifier::None;
                                methods.push(
                                    ObjectDecl::always(&modified_method));
                            } else {
                                methods.push(ObjectDecl::always(meth));
                            },
                        DMLObject::Saved(sav) =>
                            saveds.push(ObjectDecl::always(sav)),
                        DMLObject::Session(sess) =>
                            sessions.push(ObjectDecl::always(sess)),
                        DMLObject::Parameter(par) =>
                            params.push(ObjectDecl::always(par)),

                    }
                },
                DMLStatement::HashIf(hi) => {
                    // First element of the cond tuple is whether the
                    // condition is NOT in an elsebranch
                    let true_conds = vec![(true, hi.condition.clone())];
                    let false_conds = vec![(false, hi.condition.clone())];
                    let truebranchspec = flatten_hashif_branch(
                        StatementContext::HashIfTrue,
                        &hi.truebranch,
                        true_conds,
                        report);
                    let falsebranchspec = flatten_hashif_branch(
                        StatementContext::HashIfElse,
                        &hi.falsebranch,
                        false_conds,
                        report);
                    truebranchspec.consume(&mut objects,
                                           &mut sessions,
                                           &mut saveds,
                                           &mut params,
                                           &mut constants,
                                           &mut methods,
                                           &mut instantiations,
                                           &mut ineachs,
                                           &mut exports,
                                           &mut errors,
                                           &mut vec![]);
                    falsebranchspec.consume(&mut objects,
                                            &mut sessions,
                                            &mut saveds,
                                            &mut params,
                                            &mut constants,
                                            &mut methods,
                                            &mut instantiations,
                                            &mut ineachs,
                                            &mut exports,
                                            &mut errors,
                                            &mut vec![]);
                },
            }
        }

        let spec = StatementSpec {
            objects, instantiations, ineachs, errors, imports,
            saveds, sessions, params, constants, methods, exports, hooks
        };
        let mut references = vec![];
        ast.references(&mut references, filespec);

        TopLevel {
            start_of_file: ZeroSpan::from_range(
                ZeroRange::from_u32(0, 0, 0, 1), filespec.path),
            // Hack, we need a span that covers entire file no matter what
            // We SHOULD be getting a proper declaration span based on
            // statements but this is at the very least not wrong
            // for checking if a range is within a file (it will be if the
            // files match)
            filespan: ZeroSpan::from_range(ZeroRange::from_u32(
                0,0,u32::MAX,u32::MAX), filespec.path),
            filename: filespec.path.to_path_buf(),
            version,
            device,
            cblocks,
            externs,
            typedefs,
            bitorder,
            templates,
            loggroups,
            spec,
            references,
        }
    }
}

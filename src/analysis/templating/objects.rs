//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::collections::{HashMap, HashSet};
use std::iter;
use std::sync::Arc;

use log::{debug, trace, error};
use lsp_types::DiagnosticSeverity;
use slotmap::{DefaultKey, Key, SlotMap};

use crate::utility::{partial_sort_by_key, partial_sort_by_key_in_place};
use crate::analysis::{DMLNamed, DMLError};
use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::objects::{ArrayDim, CompObjectKind,
                                          CompObjectKindDecl,
                                          CompositeObject,
                                          Constant,
                                          DMLObjectCommon, Error, Hook,
                                          Import, InEach, Initializer,
                                          Instantiation, MaybeAbstract, Method,
                                          Parameter, ParamValue,
                                          Variable,
                                          VariableDecl};
use crate::analysis::structure::expressions::{DMLString, Value, ExpressionKind,
                                              Identifier, ConstantList};
use crate::analysis::structure::toplevel::{ExistCondition, ObjectDecl,
                                           StatementSpec, TopLevel};
use crate::analysis::symbols::{DMLSymbolKind};
use crate::analysis::templating::Declaration;
use crate::analysis::templating::types::eval_type;
use crate::analysis::templating::methods::{DMLConcreteMethod, DMLMethodRef, DefaultCallReference, MethodDecl, MethodDeclaration};
use crate::analysis::templating::topology::{InEachStruct, InferiorVariant,
                                            Rank, RankDesc, RankDescKind,
                                            RankMaker, topsort};
use crate::analysis::templating::traits::{DMLTemplate, DMLTrait,
                                          get_impls, TraitMemberKind, TemplateTraitInfo};
use crate::analysis::{LocationSpan, DeclarationSpan, combine_vec_of_decls};

type InEachSpec = HashMap<String, Vec<(Vec<String>, (ZeroSpan, Arc<ObjectSpec>))>>;
pub type StructureKey = DefaultKey;
pub type StructureContainer = SlotMap<StructureKey, DMLCompositeObject>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectSpec {
    pub condition: ExistCondition,
    pub loc: ZeroSpan,
    pub span: ZeroSpan,
    pub rank: Rank,

    pub subobjs: HashMap<ObjectDecl<CompositeObject>,
                          Arc<ObjectSpec>>,
    pub instantiations: HashMap<ObjectDecl<Instantiation>,
                                 Vec<Arc<DMLTemplate>>>,
    pub imports: HashMap<ObjectDecl<Import>,
                          Arc<DMLTemplate>>,
    pub in_eachs: InEachSpec,
    pub params: Vec<ObjectDecl<Parameter>>,
    pub constants: Vec<ObjectDecl<Constant>>,
    pub saveds: Vec<ObjectDecl<Variable>>,
    pub sessions: Vec<ObjectDecl<Variable>>,
    pub errors: Vec<ObjectDecl<Error>>,
    pub methods: Vec<ObjectDecl<Method>>,
    pub hooks: Vec<ObjectDecl<Hook>>,
}

impl DeclarationSpan for ObjectSpec {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl LocationSpan for ObjectSpec {
    fn loc_span(&self) -> &ZeroSpan {
        &self.loc
    }
}

pub trait Spec {
    fn get_rank(&self) -> &Rank;
    fn defined_symbols(&self) -> HashSet<&DMLString>;
}

impl Spec for ObjectSpec {
    fn get_rank(&self) -> &Rank {
        &self.rank
    }
    fn defined_symbols(&self) -> HashSet<&DMLString> {
        let subobj_names = self.subobjs.keys().map(|o|&o.obj.object.name);
        let param_names = self.params.iter().map(|o|&o.obj.object.name);
        let constant_names = self.constants.iter().map(|o|&o.obj.object.name);
        let method_names = self.methods.iter().map(|o|&o.obj.object.name);
        let hook_names = self.hooks.iter().map(|o|&o.obj.object.name);
        let var_names = self.saveds.iter()
            .chain(self.sessions.iter())
            .flat_map(|o|o.obj.vars.iter().map(|v|&v.object.name));
        subobj_names.chain(param_names)
            .chain(constant_names)
            .chain(method_names)
            .chain(hook_names)
            .chain(var_names)
            .collect()
    }
}

fn create_spec<'t>(loc: ZeroSpan,
                   span: ZeroSpan,
                   maybe_kind: Option<&CompObjectKindDecl>,
                   spec: &'t StatementSpec,
                   cond: &ExistCondition,
                   in_each_specs: &HashMap<&'t ObjectDecl<InEach>,
                                           Arc<ObjectSpec>>,
                   invalid_isimps: &HashMap<InferiorVariant<'t>, Vec<&'t str>>,
                   imp_map: &HashMap<Import, String>,
                   templates: &HashMap<String, Arc<DMLTemplate>>,
                   rank: &Rank,
                   _report: &mut Vec<DMLError>)
                   -> Arc<ObjectSpec> {
    trace!("Making a spec for something at {:?}", loc);
    trace!("Guarded by existcond {:?}", cond);
    trace!("With rank {:?}", rank);
    trace!("Known in-eachs are: {:?}", in_each_specs);
    trace!("from spec {:?}", spec);

    // Map objectdecl to templates it instantiates
    let mut instantiations = HashMap::default();
    // Add implicit instantiations to object template
    if let Some(kind) = maybe_kind {
        // Only add this if we know about the template
        let templ_name = kind.kind_name();
        if let Some(templ) = templates.get(templ_name) {
            instantiations.insert(
                ObjectDecl::always(&Instantiation {
                    span: loc,
                    names: vec![Value::<String> {
                        span: loc,
                        val: templ_name.to_string(),
                    }],
                }),
                vec![Arc::clone(templ)]);
        }
    }
    for inst in &spec.instantiations {
        instantiations.insert(
            inst.clone(),
            // If an instantiation has been marked as invalid, filter it out
            // from tracked instantiations
            if let Some(invalid_names) = invalid_isimps.get(
                &InferiorVariant::Is(inst)) {
                inst.obj.names.iter().filter_map(
                    |name| if !invalid_names.contains(&name.val.as_str()) {
                        // TODO: consider warning for double instantiations here
                        Some(Arc::clone(
                            templates.get(name.val.as_str()).unwrap()))
                    } else {
                        None
                    }).collect()
            } else {
                inst.obj.names.iter().filter_map(
                    |name|templates.get(name.val.as_str())).cloned().collect()
            });
    }
    let mut imports = HashMap::default();
    for inst in &spec.imports {
        if let Some(invalid_names) = invalid_isimps.get(
            &InferiorVariant::Import(inst)) {
            assert!(invalid_names.len() == 1);
        } else {
            imports.insert(
                inst.clone(),
                templates.get(
                    imp_map.get(&inst.obj)
                        .map_or_else(||inst.obj.imported_name(),
                                     |s|s.as_str()))
                    .cloned().unwrap());
        };
    }

    let mut in_eachs = InEachSpec::default();
    for ineach in &spec.ineachs {
        // TODO: I feel like we could filter out nonexistant templates here
        if let Some((first, rest)) = ineach.obj.spec.split_first() {
            if let Some(in_each_spec) = in_each_specs.get(ineach) {
                let to_add = (rest.iter().map(|t|t.val.clone()).collect(),
                              (ineach.obj.loc, Arc::clone(in_each_spec)));
                if let Some(e) = in_eachs.get_mut(&first.val) {
                    e.push(to_add);
                } else {
                    in_eachs.insert(first.val.clone(), vec![to_add]);
                }
            } else {
                error!("Expected {:?} to exist in {:?}, but it didnt",
                       ineach, in_each_specs);
            }
        }
    }
    let mut subobjs = HashMap::default();
    for obj in &spec.objects {
        subobjs.insert(obj.clone(),
                       create_spec(*obj.loc_span(),
                                   *obj.span(),
                                   Some(&obj.obj.kind),
                                   &obj.spec, &obj.cond, in_each_specs,
                                   invalid_isimps, imp_map, templates,
                                   rank, _report));
    }
    ObjectSpec {
        loc,
        span,
        rank: rank.clone(),
        condition: cond.clone(),
        subobjs,
        instantiations,
        imports,
        in_eachs,
        params: spec.params.to_vec(),
        constants: spec.constants.to_vec(),
        saveds: spec.saveds.to_vec(),
        sessions: spec.sessions.to_vec(),
        errors: spec.errors.to_vec(),
        methods: spec.methods.to_vec(),
        hooks: spec.hooks.to_vec(),
    }.into()
}

pub fn create_objectspec<'t>(loc: ZeroSpan,
                             range: ZeroSpan,
                             kind: Option<&CompObjectKindDecl>,
                             spec: &'t StatementSpec,
                             cond: &'t ExistCondition,
                             rankdesc: RankDesc,
                             in_each_struct: &InEachStruct<'t>,
                             invalid_isimps: &HashMap<InferiorVariant<'t>,
                                                      Vec<&'t str>>,
                             imp_map: &HashMap<Import, String>,
                             templates: &HashMap<String, Arc<DMLTemplate>>,
                             rankmaker: &mut RankMaker,
                             report: &mut Vec<DMLError>) -> Arc<ObjectSpec> {
    let mut in_each_specs = HashMap::default();
    for (decl, structinfo) in &in_each_struct.in_eachs {
        in_each_specs.insert(
            *decl, create_objectspec(
                *decl.span(),
                *decl.span(),
                None,
                &decl.spec,
                &decl.cond,
                rankdesc.extended_names(
                    decl.obj.spec.iter().map(|name|name.val.as_str())
                        .collect()),
                structinfo,
                invalid_isimps, imp_map, templates, rankmaker, report));
    }

    let inferior_ranks = in_each_struct.inferior.iter().filter(
        |(name, kind)|invalid_isimps.get(kind)
            .map_or(true,
                    |container|!container.contains(name))).map(
        |(name, _)|templates.get(*name).unwrap_or_else(||panic!(
            "Internal Error: \
             Unexpectedly missing def for template '{:?}'", name))
            .get_rank());
    let inferior_ranks = inferior_ranks.chain(in_each_specs.values().map(
        |s|s.get_rank()));
    let rank = rankmaker.new_rank(rankdesc, inferior_ranks.collect());

    create_spec(loc, range, kind, spec, cond, &in_each_specs, invalid_isimps,
                imp_map, templates, &rank, report)
}

pub fn make_device<'t>(path: &str,
                       toplevel: &TopLevel,
                       tt_info: &TemplateTraitInfo,
                       mut imp_map: HashMap<Import, String>,
                       container: &'t mut StructureContainer,
                       rankmaker: &mut RankMaker,
                       report: &mut Vec<DMLError>) -> &'t DMLCompositeObject {
    debug!("Creating a device for {}", path);
    // create the faux spec for the device toplevel, importing the device file
    // and specifying commandline parameters (TODO)
    let mut faux_stmts = StatementSpec::empty();
    let base_import = Import {
        span: ZeroSpan::invalid(path),
        name: DMLString {
            // We need to quote the path here, so as to adhere to all other
            // imports that are quoted
            val: format!("\"{}\"", path),
            span: ZeroSpan::invalid(path),
        },
    };
    faux_stmts.imports.push(ObjectDecl::always(&base_import));
    // Sneak the faux statement into the imp_map
    imp_map.insert(base_import, path.to_string());
    // Guaranteed, we do not dispatch device analysis on things
    // without device declarations
    let device_decl = toplevel.device.as_ref().unwrap();
    let device_kind_decl = CompObjectKindDecl {
        kind: CompObjectKind::Device,
        span: *device_decl.span(),
    };
    let faux_spec = create_objectspec(*device_kind_decl.span(),
                                      ZeroSpan::invalid(
                                          toplevel.filespan.path()),
                                      Some(&device_kind_decl),
                                      &faux_stmts,
                                      &ExistCondition::Always,
                                      RankDesc {
                                          kind: RankDescKind::File,
                                          desc: format!(
                                              "{} toplevel",
                                              device_decl.name.val),
                                          in_eachs: vec![],
                                      },
                                      &InEachStruct::default(),
                                      &HashMap::default(),
                                      &imp_map,
                                      &tt_info.templates,
                                      rankmaker,
                                      report);

    let obj_key = make_object(
        toplevel.start_of_file,
        &device_decl.name,
        device_decl.decl.kind,
        vec![],
        vec![],
        vec![faux_spec],
        &InEachSpec::default(),
        None,
        container,
        report);
    let device_obj = container.get(obj_key).unwrap();
    trace!("Device components are: {:?}", device_obj.components);
    export_invariants(toplevel, device_obj, container, report);
    device_obj
}

pub trait DMLNamedMember {
    fn identity(&self) -> &str;
    fn kind_name(&self) -> &'static str;
    fn location(&self) -> &ZeroSpan;
}

pub trait DMLHierarchyMember : DMLNamedMember {
    fn parent(&self) -> Option<StructureKey>;
    fn kind(&self) -> DMLSymbolKind;
    fn key(&self) -> Option<StructureKey> {
        None
    }

    fn qualified_name(&self, container: &StructureContainer) -> String {
        let parent_name = self.parent().and_then(
            |par|container.get(par).map(|o|o.qualified_name(container)));
        if let Some(qname) = parent_name {
            qname + "." + self.identity()
        } else {
            self.identity().to_owned()
        }
    }
}

// Notes on DML object resolution;
// When object definitions are within #if clauses, we may not be able to
// resolve which one is actually used. Thus we need to store all
// definition/declaration points that could possibly be relevant.
// This is _not_ the case for composite object, where we instead have the
// underlying objects carry their own multiple definitions
// This means we might miss some cases where we refer to a composite object
// that does not actually exist
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DMLObject {
    // This is a key to be used in the structure dictionary for
    // current device analysis
    CompObject(StructureKey),
    ShallowObject(DMLShallowObject),
}

impl DMLObject {
    pub fn is_comp(&self) -> bool {
        matches!(self, DMLObject::CompObject(_))
    }
    pub fn is_shallow(&self) -> bool {
        !self.is_comp()
    }
    pub fn as_shallow(&self) -> Option<&DMLShallowObject> {
        match self {
            Self::ShallowObject(obj) => Some(obj),
            _ => None,
        }
    }
    // NOTE: Used during symbol tracking to discard duplicate
    // symbols
    pub fn equivalent(&self, other: &DMLObject) -> bool {
        match (self, other) {
            (DMLObject::ShallowObject(shallow1),
             DMLObject::ShallowObject(shallow2)) =>
                shallow1.variant == shallow2.variant,
            // NOTE: here's part of the payoff, we ignore the structurekey
            (_, _) => true
        }
    }
}

pub trait CoarseObjectKind {
    fn coarse_kind(&self)  -> DMLCoarseObjectKind;
    fn same_kind<T: CoarseObjectKind>(&self, other: &T) -> bool {
        self.coarse_kind() == other.coarse_kind()
    }
}

impl CoarseObjectKind for DMLObject {
    fn coarse_kind(&self) -> DMLCoarseObjectKind {
        match self {
            DMLObject::CompObject(_) => DMLCoarseObjectKind::CompositeObject,
            DMLObject::ShallowObject(s) => s.coarse_kind(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DMLCoarseObjectKind {
    CompositeObject,
    Method,
    Session,
    Saved,
    Parameter,
    Hook,
    Constant,
}

impl DMLObject {
    pub fn resolve<'t, 'c>(&'t self, container: &'c StructureContainer)
                       -> DMLResolvedObject<'t, 'c> {
        match self {
            DMLObject::CompObject(key) =>
                DMLResolvedObject::CompObject(container.get(*key).unwrap()),
            DMLObject::ShallowObject(obj) =>
                DMLResolvedObject::ShallowObject(obj),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DMLResolvedObject<'t, 'c> {
    CompObject(&'c DMLCompositeObject),
    ShallowObject(&'t DMLShallowObject),
}

impl <'t, 'c> CoarseObjectKind for DMLResolvedObject<'t, 'c> {
    fn coarse_kind(&self) -> DMLCoarseObjectKind {
        match self {
            DMLResolvedObject::CompObject(_) =>
                DMLCoarseObjectKind::CompositeObject,
            DMLResolvedObject::ShallowObject(s) => s.coarse_kind(),
        }
    }
}

impl <'t, 'c> DMLNamedMember for DMLResolvedObject<'t, 'c> {
    fn identity(&self) -> &str {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.identity(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.identity(),
        }
    }
    fn kind_name(&self) -> &'static str {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.kind_name(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.kind_name(),
        }
    }
    fn location(&self) -> &ZeroSpan {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.location(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.location(),
        }
    }
}

impl <'t, 'c> DMLResolvedObject<'t, 'c> {
    pub fn as_comp(&self) -> Option<&'c DMLCompositeObject> {
        match self {
            DMLResolvedObject::CompObject(comp) => Some(comp),
            _ => None
        }
    }
    pub fn as_shallow(&self) -> Option<&'t DMLShallowObject> {
        match self {
            DMLResolvedObject::ShallowObject(shallow) => Some(shallow),
            _ => None
        }
    }
}

impl <'t, 'c> DMLHierarchyMember for DMLResolvedObject<'t, 'c> {
    fn parent(&self) -> Option<StructureKey> {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.parent(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.parent(),
        }
    }

    fn key(&self) -> Option<StructureKey> {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.key(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.key(),
        }
    }

    fn kind(&self) -> DMLSymbolKind {
        match self {
            DMLResolvedObject::CompObject(comp) => comp.kind(),
            DMLResolvedObject::ShallowObject(shallow) => shallow.kind(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLShallowObject {
    pub parent: StructureKey,
    pub variant: DMLShallowObjectVariant,
}

impl CoarseObjectKind for DMLShallowObject {
    fn coarse_kind(&self) -> DMLCoarseObjectKind {
        self.variant.coarse_kind()
    }
}

impl DMLNamedMember for DMLShallowObject {
    fn identity(&self) -> &str {
        self.variant.identity()
    }

    fn kind_name(&self) -> &'static str {
        self.variant.kind_name()
    }

    fn location(&self) -> &ZeroSpan {
        self.variant.location()
    }
}

impl DMLHierarchyMember for DMLShallowObject {
    fn parent(&self) -> Option<StructureKey> {
        Some(self.parent)
    }
    fn kind(&self) -> DMLSymbolKind {
        match &self.variant {
            DMLShallowObjectVariant::Method(_) =>
                DMLSymbolKind::Method,
            DMLShallowObjectVariant::Session(_) =>
                DMLSymbolKind::Session,
            DMLShallowObjectVariant::Saved(_) =>
                DMLSymbolKind::Saved,
            DMLShallowObjectVariant::Parameter(_) =>
                DMLSymbolKind::Parameter,
            DMLShallowObjectVariant::Constant(_) =>
                DMLSymbolKind::Constant,
            DMLShallowObjectVariant::Hook(_) =>
                DMLSymbolKind::Hook,
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DMLShallowObjectVariant {
    Method(Arc<DMLMethodRef>),
    Session(DMLVariable),
    Saved(DMLVariable),
    Parameter(DMLParameter),
    Constant(Constant),
    Hook(ObjectDecl<Hook>),
}

impl DMLShallowObjectVariant {
    pub fn as_method(&self) -> Option<&Arc<DMLMethodRef>> {
        match self {
            Self::Method(meth) => Some(meth),
            _ => None,
        }
    }
    pub fn as_session_or_saved(&self) -> Option<&DMLVariable> {
        match self {
            Self::Session(var) | Self::Saved(var) => Some(var),
            _ => None,
        }
    }
    pub fn as_parameter(&self) -> Option<&DMLParameter> {
        match self {
            Self::Parameter(param) => Some(param),
            _ => None,
        }
    }
    pub fn as_constant(&self) -> Option<&Constant> {
        match self {
            Self::Constant(con) => Some(con),
            _ => None,
        }
    }
    pub fn as_hook(&self) -> Option<&ObjectDecl<Hook>> {
        match self {
            Self::Hook(hook) => Some(hook),
            _ => None,
        }
    }
}

impl CoarseObjectKind for DMLShallowObjectVariant {
    fn coarse_kind(&self) -> DMLCoarseObjectKind {
        match self {
            DMLShallowObjectVariant::Method(_) => DMLCoarseObjectKind::Method,
            DMLShallowObjectVariant::Session(_) => DMLCoarseObjectKind::Session,
            DMLShallowObjectVariant::Saved(_) => DMLCoarseObjectKind::Saved,
            DMLShallowObjectVariant::Parameter(_) =>
                DMLCoarseObjectKind::Parameter,
            DMLShallowObjectVariant::Hook(_) =>
                DMLCoarseObjectKind::Hook,
            DMLShallowObjectVariant::Constant(_) =>
                DMLCoarseObjectKind::Constant,
        }
    }
}

impl DMLNamedMember for DMLShallowObjectVariant {
    fn identity(&self) -> &str {
        match self {
            // TODO: probably these should themselve implement
            // DMLNamedMember
            DMLShallowObjectVariant::Method(m) => m.identity(),
            DMLShallowObjectVariant::Session(d) |
            DMLShallowObjectVariant::Saved(d) => &d.declaration.name.val,
            DMLShallowObjectVariant::Parameter(p) => &p.identity,
            DMLShallowObjectVariant::Constant(c) => &c.name().val,
            DMLShallowObjectVariant::Hook(h) => &h.obj.name().val,
        }
    }

    fn kind_name(&self) -> &'static str {
        match self {
            DMLShallowObjectVariant::Method(m) => m.kind_name(),
            DMLShallowObjectVariant::Session(_d) => "session",
            DMLShallowObjectVariant::Saved(_d) => "saved",
            DMLShallowObjectVariant::Parameter(_) => "parameter",
            DMLShallowObjectVariant::Constant(_) => "constant",
            DMLShallowObjectVariant::Hook(_) => "hook",
        }
    }

    fn location(&self) -> &ZeroSpan {
        match self {
            DMLShallowObjectVariant::Method(m) => m.location(),
            DMLShallowObjectVariant::Session(d) |
            DMLShallowObjectVariant::Saved(d) => &d.declaration.name.span,
            DMLShallowObjectVariant::Parameter(p) =>
                p.get_likely_definition().loc_span(),
            DMLShallowObjectVariant::Constant(c) =>
                c.loc_span(),
            DMLShallowObjectVariant::Hook(h) =>
                h.loc_span(),
        }
    }
}

impl DMLNamedMember for DMLCompositeObject {
    fn identity(&self) -> &str {
        &self.identity.val
    }
    fn kind_name(&self) -> &'static str {
        self.kind.kind_name()
    }
    fn location(&self) -> &ZeroSpan {
        &self.declloc
    }
}

impl DMLHierarchyMember for DMLCompositeObject {
    fn parent(&self) -> Option<StructureKey> {
        self.parent
    }
    fn key(&self) -> Option<StructureKey> {
        Some(self.key)
    }
    fn kind(&self) -> DMLSymbolKind {
        DMLSymbolKind::CompObject(self.kind)
    }
}

impl DMLCompositeObject {
    fn set_parent(&mut self, key: StructureKey) {
        assert!(self.parent.is_none());
        self.parent = Some(key);
    }
}

// This is _not_ used for DML composite objects, which may have
// split definition points anyway
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLAmbiguousDef<T: std::fmt::Debug + Clone + PartialEq> {
    pub identity: String,
    // Possible definition sites, guarded by which conditions need
    // to be true for them to be used (from the context of the closest
    // objectspec)
    // Note: If the definitions exist here; we have, in general not
    // been able to rule them out and as such they must be considered.
    // Note': This list should be sorted, with the most likely candidate first
    pub used_definitions: Vec<(ExistCondition, T)>,
    // These are overridden definitions
    pub definitions: Vec<(ExistCondition, T)>,
    // Similar, but for declaration sites
    pub declarations: Vec<(ExistCondition, T)>,
}

impl <T: std::fmt::Debug + Clone + PartialEq> DMLAmbiguousDef<T> {
    pub fn new_simple(identity: String,
                      definition: T) -> DMLAmbiguousDef<T> {
        DMLAmbiguousDef::new(
            identity,
            vec![(ExistCondition::Always, definition)],
            vec![],
            vec![])
    }
    pub fn new(identity: String,
               used_definitions: Vec<(ExistCondition, T)>,
               definitions: Vec<(ExistCondition, T)>,
               declarations: Vec<(ExistCondition, T)>)
               -> DMLAmbiguousDef<T> {
        assert!(!used_definitions.is_empty() || !declarations.is_empty(),
                "Internal error; attempted to create ambiguous def with\
                 no used definitions and no declarations");
        DMLAmbiguousDef {
            identity,
            used_definitions,
            definitions,
            declarations,
        }
    }

    pub fn get_likely_definition(&self) -> &T {
        if let Some(def) = self.used_definitions.first() {
            &def.1
        } else if let Some(def) = self.definitions.first() {
            &def.1
        } else {
            &self.declarations.first().unwrap().1
        }
    }

    pub fn get_likely_declaration(&self) -> &T {
        if let Some(def) = self.declarations.first() {
            &def.1
        } else {
            &self.used_definitions.first().unwrap().1
        }
    }
    pub fn get_unambiguous_def(&self) -> Option<&T> {
        if self.used_definitions.len() == 1 {
            self.used_definitions.first().map(|t|&t.1)
        } else {
            None
        }
    }
    pub fn get_unambiguous_decl(&self) -> Option<&T> {
        if self.declarations.len() == 1 {
            self.declarations.first().map(|t|&t.1)
        } else {
            None
        }
    }
    pub fn is_unambiguous(&self) -> bool {
        self.declarations.len() == 1 && self.used_definitions.len() == 1
    }
}

impl <T: std::fmt::Debug + Clone + PartialEq> DMLNamed for DMLAmbiguousDef<T>
where T: DMLNamed {
    fn name(&self) -> &DMLString {
        self.get_likely_definition().name()
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLVariable {
    pub declaration: Declaration,
    pub init: Option<Initializer>,
}

impl MaybeAbstract for DMLVariable {
    fn is_abstract(&self) -> bool {
        self.init.is_none()
    }
}

impl DMLNamed for DMLVariable {
    fn name(&self) -> &DMLString {
        self.declaration.name()
    }
}

pub type DMLParameter = DMLAmbiguousDef<Parameter>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLCompositeObject {
    pub declloc: ZeroSpan,
    // These are the ranges of the objectspecs declared with the
    // objects name
    pub all_decls: Vec<Arc<ObjectSpec>>,
    pub identity: DMLString,
    // Reference to self, let's us pass obj refs rather than
    // keys to functions unless necessary
    pub key: StructureKey,
    pub kind: CompObjectKind,
    // These are the full objectspecs used to define this object, non-semantic
    pub definitions: Vec<Arc<ObjectSpec>>,
    // These are the immediate parents
    pub templates: HashMap<String, Arc<DMLTemplate>>,
    pub parent: Option<StructureKey>,
    // None: 'none' for size in arraydim here means an _unknown_ size
    pub arraydimvars: Vec<ArrayDim>,
    pub components: HashMap<String, DMLObject>,
    // Used for goto-implementation on composite objects
    pub used_ineach_locs: Vec<ZeroSpan>,
}

impl DMLCompositeObject {
    pub fn get_object<T>(&self, name: T) -> Option<&DMLObject>
    where T: std::borrow::Borrow<str> + Sized {
        self.components.get(name.borrow())
    }
    pub fn get_param<T>(&self, name: T) -> Option<&DMLParameter>
    where T: std::borrow::Borrow<str> + Sized {
        if let Some(DMLObject::ShallowObject(
            DMLShallowObject {
                variant: DMLShallowObjectVariant::Parameter(param),
                ..
            })) = self.get_object(name) {
            Some(param)
        } else {
            None
        }
    }

    pub fn get_method<T>(&self, name: T) -> Option<Arc<DMLMethodRef>>
    where T: std::borrow::Borrow<str> + Sized {
        if let Some(DMLObject::ShallowObject(
            DMLShallowObject {
                variant: DMLShallowObjectVariant::Method(meth),
                ..
            })) = self.get_object(name) {
            Some(Arc::clone(meth))
        } else {
            None
        }
    }

    pub fn dimensions(&self, container: &StructureContainer) -> usize {
        if let Some(parent) = &self.parent {
            container.get(*parent).unwrap().dimensions(container)
                + self.local_dimensions()
        } else {
            self.local_dimensions()
        }
    }

    pub fn local_dimensions(&self) -> usize {
        self.arraydimvars.len()
    }

    pub fn parameters<'t>(&'t self)
                          -> impl Iterator<Item=&'t DMLParameter> + 't {
        self.components.values().filter_map(
            |c| if let DMLObject::ShallowObject(
                DMLShallowObject {
                    variant: DMLShallowObjectVariant::Parameter(p),
                    ..
                }) = c {
                Some(p)
            } else {
                None
            })
    }

    pub fn add_composite_component(&mut self,
                                   child: &mut DMLCompositeObject) {
        if self.components.contains_key(child.identity()) {
            error!("Invalid internal state: Attempted to add component {:?} to \
                    {:?}. But the name {} was already occupied",
                   child, self, child.identity());
        } else {
            child.set_parent(self.key);
            self.components.insert(child.identity.val.clone(),
                                   DMLObject::CompObject(child.key));
        }
    }

    pub fn add_shallow_component(&mut self, child: DMLShallowObjectVariant) {
        if let Some(collision) = self.components.get(child.identity()) {
            error!("Invalid internal state: Attempted to add component {:?} to \
                    {:?}. But the name {} was already occupied by {:?}",
                   child, self, child.identity(),
                   match collision {
                       DMLObject::ShallowObject(s) => format!("{:?}", s),
                       DMLObject::CompObject(_) =>
                           "some composite object (cannot resolve)".to_string(),
                   });
        } else {
            self.components.insert(child.identity().to_string(),
                                   DMLObject::ShallowObject(DMLShallowObject {
                                       parent: self.key,
                                       variant: child,
                                   }));
        }
    }
}

// Adds the new objectspecs to add, based on templates instantiated
// (also considers templates instantiated by templates, and in_each statements
// we need to add)
// NOTE: new objectspecs are pathed with the full condition chain required
// to instantiate them
// Returns all spans of final actually used ineachspecs, needed for
// implementations tracking for composite objects
fn add_template_specs(obj_specs: &mut Vec<Arc<ObjectSpec>>,
                      source_each_stmts: &InEachSpec) -> Vec<ZeroSpan>{
    let mut each_stmts = source_each_stmts.clone();
    // TODO: We need to handle conditional imports and is-es here, as these are
    // allowed in _some_ cases. For now, we merely pretend the conditions do not
    // exist. In practice, I think this causes no problems _right now_ because
    // we do not support dml12 compat in language server anyway

    // Queue is a pair of flattened (condition, templ) based on templates
    // instantiated by spec
    let mut queue: Vec<(ExistCondition, Arc<DMLTemplate>)> =
        obj_specs.iter().flat_map(
            |s|s.instantiations.iter()
                .flat_map(|(d, v)|v.iter()
                          .map(move |i|(d.cond.clone(),
                                        Arc::clone(i)))))
        .collect();
    queue.extend(
        obj_specs.iter().flat_map(|s|s.imports.iter()
                             .map(|(d, v)|(d.cond.clone(),
                                           Arc::clone(v)))));

    // TODO: When conditional instantiation is available, we will need to
    // handle existence conditions here somehow. Perhaps
    // only track used templates for things whose conditions we can evaluate to
    // true in whatever context it would work in
    let mut used_templates = HashSet::<String>::default();

    let mut used_ineach_spans = vec![];

    while let Some((_, tpl)) = queue.pop() {
        // TODO: here we would have to consider cond when conditional
        // is/imports exist
        if used_templates.contains(&tpl.name) {
            continue;
        }
        debug!("Handling instantiation/import of {:?}",
               tpl.name);
        used_templates.insert(tpl.name.to_string());
        let mut modifications = vec![];
        {
            if let Some(templ_specs) = each_stmts.get(&tpl.name) {
                for (needed_templates, (loc, spec)) in templ_specs {
                    let mut can_add = true;
                    for templ in needed_templates {
                        if !used_templates.contains(templ.as_str()) {
                            // We will need to re-add a check for this template,
                            // in case it will appear later in the queue
                            modifications.push(
                                (templ.clone(),
                                needed_templates.clone(),
                                (*loc, Arc::clone(spec))));
                            can_add = false;
                            break;
                        }
                    }
                    if can_add {
                        queue.extend(spec.instantiations.values()
                        .flat_map(|v|v.iter())
                        .map(|t|(spec.condition.clone(),
                        Arc::clone(t))));
                        obj_specs.push(Arc::clone(spec));
                        used_ineach_spans.push(*loc);
                    }
                }
            }
        }
        for (name, templs, (loc, spec)) in modifications {
            let to_insert = (templs, (loc, spec));
            if let Some(e) = each_stmts.get_mut(&name) {
                e.push(to_insert);
            } else {
                each_stmts.insert(name, vec![to_insert]);
            }
        }
        obj_specs.push(Arc::clone(&tpl.spec));
        for (d, v) in &tpl.spec.instantiations {
            queue.extend(v.iter().map(|i|(d.cond.clone(),
                                          Arc::clone(i))));
        }
        for (d, v) in &tpl.spec.imports {
            queue.push((d.cond.clone(), Arc::clone(v)));
        }
    }

    used_ineach_spans
}

// Simply adds the in_eachs specified in object specs to the ineachspec
fn add_template_ineachs(obj_specs: &[Arc<ObjectSpec>],
                        in_eachs: &mut InEachSpec) {
    for spec in obj_specs {
        for (first_needed_template, further_each_specs) in &spec.in_eachs {
            in_eachs.entry(first_needed_template.clone())
                .and_modify(|e|e.extend(further_each_specs.clone()))
                .or_insert_with(||further_each_specs.clone());
        }
    }
}

fn add_templates(obj: &mut DMLCompositeObject,
                 obj_specs: &[Arc<ObjectSpec>]) {
    for spec in obj_specs {
        for templs in spec.instantiations.values() {
            obj.templates.extend(templs.clone().into_iter()
            .map(|templ|(templ.name.clone(), templ)));
        }
    }
}

fn add_parameters(obj: &mut DMLCompositeObject,
                  parameters: Vec<DMLParameter>) {
    for param in parameters {
        obj.add_shallow_component(DMLShallowObjectVariant::Parameter(param))
    }
}

fn add_hooks(obj: &mut DMLCompositeObject,
             hooks: HookMapping) {
    for (_, (used, mut hook_decls)) in hooks {
        if used {
            let (_, hook) = hook_decls.swap_remove(0);
            obj.add_shallow_component(
                DMLShallowObjectVariant::Hook(hook));
        }
    }
}

fn add_constants(obj: &mut DMLCompositeObject,
                 constants: Vec<Constant>) {
    for constant in constants {
        obj.add_shallow_component(DMLShallowObjectVariant::Constant(constant));
    }
}

enum VariableKind {
    Session,
    Saved,
}

fn add_sessions(obj: &mut DMLCompositeObject,
                sessions: SessionMapping) {
    for (_, (used, mut decls)) in sessions {
        if used {
            let (_, (s, i)) = decls.swap_remove(0);
            handle_variable(obj, s, i, VariableKind::Session)
        }
    }
}

fn add_saveds(obj: &mut DMLCompositeObject,
              saveds: SavedMapping) {
    for (_, (used, mut decls)) in saveds {
        if used {
            let (_, (s, i)) = decls.swap_remove(0);
            handle_variable(obj, s, i, VariableKind::Saved)
        }
    }
}

fn add_subobjs(obj_key: StructureKey,
               objects: Vec<StructureKey>,
               container: &mut StructureContainer) {
    for key in objects {
        let [obj, subobj] = container.get_disjoint_mut([obj_key, key]).unwrap();
        obj.add_composite_component(subobj);
    }
}

fn make_objectdecl_auto_param(name: &str,
                              span: &ZeroSpan,
                              value: Option<ParamValue>)
                              -> ObjectDecl<Parameter> {
    ObjectDecl::always(&Parameter {
        object: DMLObjectCommon {
            name: Value::<String> {
                val: name.to_string(),
                span: *span,
            },
            span: *span,
        },
        is_default: false,
        value,
        typed: None,
    })
}

fn gather_parameters<'t>(obj_loc: &ZeroSpan,
                         specs: &'t [Arc<ObjectSpec>],
                         index_info: &[ArrayDim],
                         auto_parameters: HashMap<String,
                                                  ObjectDecl<Parameter>>,
                         report: &mut Vec<DMLError>) -> Vec<DMLParameter> {
    let mut parameters: HashMap<&str, Vec<(ObjectDecl<Parameter>, Rank)>> =
        HashMap::default();

    // It is fine to create an implicit rank like this, the batch will be
    // different from all other ranks, and thus the ranks will be incomparable
    let implicit_rank = RankMaker::new().new_rank(RankDesc {
        kind: RankDescKind::Template,
        desc: "Implicit parameters".to_string(),
        in_eachs: vec![],
    }, vec![]);

    // Add array index variable
    for arraydim in index_info {
        let new_decl = (make_objectdecl_auto_param(
            &arraydim.indexvar.val,
            &arraydim.indexvar.span,
            // TODO: Actual parameter value
            arraydim.size.as_ref().map(|s|ParamValue::Set(s.clone()))),
            implicit_rank.clone());
        if let Some(e) = parameters.get_mut(arraydim.indexvar.val.as_str()) {
            e.push(new_decl)
        } else {
                parameters.insert(arraydim.indexvar.val.as_str(),
                                  vec![new_decl]);
        }
    }

    // Add code-decl parameters
    for spec in specs {
        for param in &spec.params {
            let new_decl = (param.clone(), spec.rank.clone());
            if let Some(e) = parameters.get_mut(
                param.obj.object.name.val.as_str()) {
                e.push(new_decl)
            } else {
                parameters.insert(param.obj.object.name.val.as_str(),
                                  vec![new_decl]);
            }
        }
    }
    parameters.into_iter().map(
        |(name, decls)|
        resolve_parameter(obj_loc, name, decls,
                          &auto_parameters, report)).collect()
}

// Partitions parameter decls, with rank,  according to a predicate,
// sorts each group by rank, and then return a vec with the first from the
fn sort_and_partition_parameters<F>(
    decls: Vec<(ObjectDecl<Parameter>, Rank)>,
    partitioner: F)
    -> Vec<(ObjectDecl<Parameter>, Rank)>
where F: FnMut(&(ObjectDecl<Parameter>, Rank)) -> bool {
    let (group1, group2) : (Vec<_>, Vec<_>)
        = decls.into_iter().partition(partitioner);
    // sort each by rank by converting them to a binary heap
    let sorted_group1: Vec<(ObjectDecl<Parameter>, Rank)>
        = partial_sort_by_key(group1, |(_, r)|r);
    let sorted_group2: Vec<(ObjectDecl<Parameter>, Rank)>
        = partial_sort_by_key(group2, |(_, r)|r);
    // TODO: There is no good way to pick a "preferred" declaration, if we
    // have rank-unrelated ones.
    // NOTE: Parallel declarations are allowed as long as they are not both
    // typed, but that is caught in the trait system. Parallel definitions
    // caught later
    let mut sorted_group1_iter = sorted_group1.into_iter();
    let mut sorted_group2_iter = sorted_group2.into_iter();
    let mut to_return = vec![];
    if let Some(elem) = sorted_group1_iter.next() {
        to_return.push(elem);
    }
    if let Some(elem) = sorted_group2_iter.next() {
        to_return.push(elem);
    }
    to_return.extend(sorted_group1_iter);
    to_return.extend(sorted_group2_iter);
    to_return
}

// TODO: Right now, this does not consider existconditions at all. Needs
// to try to resolve them at this point in order to non report conflicts
// between mutually exclusive branches
fn resolve_parameter(obj_loc: &ZeroSpan,
                     name: &str,
                     decls: Vec<(ObjectDecl<Parameter>, Rank)>,
                     auto_parameters: &HashMap<String, ObjectDecl<Parameter>>,
                     report: &mut Vec<DMLError>) -> DMLParameter {
    trace!("Making parameter {}", name);
    let (declarations, definitions): (Vec<_>, Vec<_>)
        = decls.into_iter().partition(|(decl, _)|decl.obj.value.is_none());
    // Roughly order objectdecls in order of importance
    // For declarations;
    // typed declaration (highest rank) > untyped declarations (highest rank)
    // > other typeds > other untypeds
    // For definitions:
    // non-default declarations (highest rank) > default (highest rank) >
    // non-defaults (sorted by rank) > defaults (sorted by ranks)
    let sorted_declarations: Vec<(ObjectDecl<Parameter>, Rank)>
        = sort_and_partition_parameters(
            declarations, |(decl, _)|decl.obj.typed.is_some());
    let sorted_definitions: Vec<(ObjectDecl<Parameter>, Rank)>
        = sort_and_partition_parameters(
            definitions, |(def, _)|!def.obj.is_default);

    if sorted_definitions.is_empty() {
        report.push(DMLError {
            span: *obj_loc,
            description: format!("No assignment to parameter '{}'", name),
            related: vec![(*sorted_declarations
                           .first().unwrap().0.span(),
                           "Declared here".to_string())],
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }

    // find if we're an auto param
    for (def, _) in &sorted_definitions {
        if def.obj.value.as_ref().map_or(
            false, |v|matches!(v, ParamValue::Auto(_))) {
            if sorted_definitions.len() > 1 {
                report.push(DMLError {
                    span: *def.obj.span(),
                    description: format!(
                        "Invalid assignment to auto-parameter '{}'",
                        name),
                    related: sorted_definitions.iter().filter_map(
                        |(def2, _)| if def != def2 {
                            Some((*def2.obj.span(),
                                  "Conflicting assignment here".to_string())
                            ) } else {
                            None
                        }).collect(),
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
            let def = auto_parameters.get(name).unwrap_or(
                &sorted_definitions.first().unwrap().0);
            return DMLAmbiguousDef {
                    identity: name.to_string(),
                    used_definitions: vec![(def.cond.clone(), def.obj.clone())],
                    definitions: vec![(def.cond.clone(), def.obj.clone())],
                    declarations: sorted_declarations.into_iter()
                    .map(|(ObjectDecl { cond, obj, ..},_)
                         |(cond, obj)).collect(),
            };
        }
    }

    let ranks : Vec<&Rank> = sorted_definitions.iter().map(
        |(_, r)|r).collect();
    trace!("Ranks for {} are {:?}", name, ranks);
    // find all ranks that are overridden
    let all_inferior_ranks: Vec<_> = sorted_definitions.iter().flat_map(
        |(_, r)|r.inferior.iter()).collect();
    trace!("Inferior ranks for {} are {:?}",
           name, all_inferior_ranks);
    let overriding_defs: Vec<&(ObjectDecl<Parameter>, Rank)> =
        sorted_definitions.iter()
        .filter(|decl_rank|!all_inferior_ranks.contains(&&decl_rank.1.id))
        .collect();
    trace!("Top defs for {} are {:?}", name, overriding_defs);
    if overriding_defs.len() > 1 {
        debug!("Conflicting assignment for {:?}, {:?}",
               name, overriding_defs);
        // in dmlc, there is a preference for blaming default declarations.
        // we will merely blame all of them, starting at the preferred def
        // TODO: This can be improved for specific cases where a name
        // collision error is more appropriate
        let mut rest = overriding_defs.iter();
        let (first, _) = rest.next().unwrap();
        report.push(DMLError {
            span: *first.obj.span(),
            description: format!(
                "Conflicting assignments to parameter '{}'", name),
            related: rest.map(
                |(def2, _)| (*def2.obj.span(),
                             "Conflicting assignment here"
                             .to_string())).collect(),
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }

    let bad_overrides: Vec<&(ObjectDecl<Parameter>, Rank)>
        = sorted_definitions.iter().filter(
            |(def, _)|!def.obj.is_default && !overriding_defs.iter().any(
                |(def2, _)|def2 == def)).collect();
    if !bad_overrides.is_empty() {
        report.push(DMLError {
            span: *sorted_definitions.first().unwrap().0.obj.span(),
            description: "Definition overrides non-default parameter"
                .to_string(),
            related: bad_overrides.iter().map(
                |(bad, _)| (*bad.obj.span(),
                            "Non-default declaration here"
                            .to_string())).collect(),
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }
    DMLAmbiguousDef::new(
        name.to_string(),
        overriding_defs.iter().map(
            |(def, _)|(def.cond.clone(), def.obj.clone())).collect(),
        sorted_definitions.iter().filter(
            |o|!overriding_defs.contains(o)).map(
            |(def, _)|(def.cond.clone(), def.obj.clone())).collect(),
        sorted_declarations.iter().map(
            |(def, _)|(def.cond.clone(), def.obj.clone())).collect(),
    )
}

fn create_object_instance(loc: Option<ZeroSpan>,
                          all_decls: Vec<Arc<ObjectSpec>>,
                          identity: &DMLString,
                          kind: CompObjectKind,
                          array_info: &[ArrayDim],
                          container: &mut StructureContainer)
                          -> StructureKey {
    // TODO: handle array_info properly
    match kind {
        CompObjectKind::Device | CompObjectKind::Implement |
        CompObjectKind::Interface =>
            if !array_info.is_empty() {
                error!("Invalid internal state: {:?} has array information \
                        despite being a flat kind", identity);
            },
        _ => (),
    }
    debug!("Making obj {:?} with decl ranges {:?}",
           identity,
           all_decls);
    let obj = DMLCompositeObject {
        declloc: loc.unwrap_or(identity.span),
        all_decls,
        identity: identity.clone(),
        used_ineach_locs: vec![],
        key: StructureKey::null(),
        kind,
        definitions: vec![],
        templates: HashMap::default(),
        parent: None,
        arraydimvars: array_info.to_vec(),
        components: HashMap::default(),
    };
    let key = container.insert(obj);
    container.get_mut(key).unwrap().key = key;
    key
}

fn handle_variable(obj: &mut DMLCompositeObject,
                   vardecl: VariableDecl,
                   init: Option<Initializer>,
                   kind: VariableKind) {
    let (_, typed) = eval_type(&vardecl.typed, (), (),
                               false, None, false);
    let var = DMLVariable {
        declaration: Declaration {
            name: vardecl.object.name,
            type_ref: typed.into(),
        },
        init,
    };
    obj.add_shallow_component(
        match kind {
            VariableKind::Session =>
                DMLShallowObjectVariant::Session(var),
            VariableKind::Saved =>
                DMLShallowObjectVariant::Saved(var),
        });
}

// Maps name to (used, definitions), where definitions is a vector of
//    (Rank, Method) tuples
type MethodMapping = HashMap<String, (bool, Vec<(Rank, MethodDecl)>)>;
// Maps name to (used, definitions), where definitions is a vector of
//    (Rank, Decl, Spec) tuples
type ObjectMapping = HashMap<String, (bool, Vec<(Rank,
                                                 ObjectDecl<CompositeObject>,
                                                 Arc<ObjectSpec>)>)>;
// Maps name to (used, definitions), similar to methodmapping
type SavedMapping = HashMap<String,
                            (bool,
                             Vec<(Rank,
                                  (VariableDecl, Option<Initializer>))>)>;
type SessionMapping = HashMap<String,
                              (bool,
                               Vec<(Rank,
                                    (VariableDecl, Option<Initializer>))>)>;
type HookMapping = HashMap<String, (bool, Vec<(Rank, ObjectDecl<Hook>)>)>;

// Figure out symbol mappings for these specs
// reports unguarded error
// the hashmap is the symbol mapping
type CollectedSymbols = (HashMap<String, (ZeroSpan, Vec<ZeroSpan>)>,
                         Vec<Constant>,
                         SavedMapping, SessionMapping,
                         MethodMapping, HookMapping, ObjectMapping);
fn collect_symbols(parameters: &[DMLParameter],
                   obj_specs: &[Arc<ObjectSpec>],
                   report: &mut Vec<DMLError>) -> CollectedSymbols
{
    // We will report all name collisions _after_ we have sorted and collected
    // this, based on the ambiguousdefs we get.
    // This makes the error locations better in some cases, and reports fewer
    // diagnostics overall
    // Saved/Sessions are added directly to object here, so we don't need to
    // store 'used' for them
    let mut saveds = SavedMapping::default();
    let mut sessions = SessionMapping::default();

    let mut methods = MethodMapping::default();
    let mut subobjs = ObjectMapping::default();
    let mut hooks = HookMapping::default();
    let mut constants: Vec<Constant> = vec![];

    for spec in obj_specs {
        for error in &spec.errors {
            // TODO: evaluate conditions
            // for now, conservatively only report the errors for
            // things not behind hashifs
            if error.cond == ExistCondition::Always {
                report.push(DMLError {
                    span: *error.span(),
                    // TODO: early-evaluate the error message, somehow
                    description: "unguarded error statement".to_string(),
                    related: vec![],
                    severity: Some(DiagnosticSeverity::ERROR),
                });
            }
        }
        // TODO: How to handle extra initializers here?
        // Its a bit to early to eagerly evaluate them, but discarding
        // them completely isn't ideal either
        // (extra vars is not a problem, since they just become un-inited
        //  declarations)
        for constant in &spec.constants {
            constants.push(constant.obj.clone());
        }
        for saved_objectdecl in &spec.saveds {
            // Pad initializers with 'None' values, so that we handle all the
            // variables
            let saved = &saved_objectdecl.obj;
            for (var, init) in saved.vars.iter().zip(
                saved.values.iter().map(|e|Some(e)).chain(iter::repeat(None))) {
                // TODO: verify serializability of type
                let to_insert = (spec.rank.clone(),
                                 (var.clone(), init.cloned()));
                let name = var.object.name.val.clone();
                if let Some((_, e)) = saveds.get_mut(&name) {
                    e.push(to_insert);
                } else {
                    saveds.insert(name, (false, vec![to_insert]));
                }
            }
        }
        for session_objectdecl in &spec.sessions {
            let session = &session_objectdecl.obj;
            for (var, init) in session.vars.iter().zip(
                session.values.iter().map(|e|Some(e))
                    .chain(iter::repeat(None))) {
                let to_insert = (spec.rank.clone(),
                                 (var.clone(), init.cloned()));
                let name = var.object.name.val.clone();
                if let Some((_,e)) = sessions.get_mut(&name) {
                    e.push(to_insert);
                } else {
                    sessions.insert(name, (false, vec![to_insert]));
                }
            }
        }
        for method in &spec.methods {
            let to_insert = (spec.rank.clone(),
                             MethodDecl::from_content(&method.obj, report));
            let name = method.obj.object.name.val.clone();
            if let Some((_, e)) = methods.get_mut(&name) {
                e.push(to_insert);
            } else {
                methods.insert(name, (false, vec![to_insert]));
            }
        }
        for hook in &spec.hooks {
            let to_insert = (spec.rank.clone(), hook.clone());
            let name = &hook.obj.name().val;
            if let Some((_, e)) = hooks.get_mut(name) {
                e.push(to_insert);
            } else {
                hooks.insert(name.to_string(), (false, vec![to_insert]));
            }
        }

        for (subobj, spec) in &spec.subobjs {
            let to_insert = (spec.rank.clone(),
                             subobj.clone(), Arc::clone(spec));
            let name = subobj.obj.object.name.val.clone();
            if let Some((_, e)) = subobjs.get_mut(&name) {
                e.push(to_insert);
            } else {
                subobjs.insert(name, (false, vec![to_insert]));
            }
        }
    }

    // TODO: the sorting is insertion sort anyway, we could
    // be sorting this while inserting
    for (_, decls) in saveds.values_mut() {
        partial_sort_by_key_in_place(decls, |(r, _)|r);
    }
    for (_, decls) in sessions.values_mut() {
        partial_sort_by_key_in_place(decls, |(r, _)|r);
    }
    for (_, decls) in methods.values_mut() {
        partial_sort_by_key_in_place(decls, |(r, _)|r);
    }
    for (_, decls) in hooks.values_mut() {
        partial_sort_by_key_in_place(decls, |(r, _)|r);
    }
    for (_, decls) in subobjs.values_mut() {
        partial_sort_by_key_in_place(decls, |(r, _,  _)|r);
    }

    // Map names to most relevant declaration and colliding declarations
    // This creates an order for symbol definition precedence
    // constant > parameter > subobj > method > saved > session
    // Grab the most-relevant decl from each ambiguousdecl, if they have
    // parameter-to-parameter collisions then that has already been reported
    let mut symbols: HashMap<String, (ZeroSpan, Vec<ZeroSpan>)>
        = HashMap::new();

    // NOTE: constants are top-level only, so ordering doesn't really matter
    for constant in &constants {
        if let Some((_, rest)) = symbols.get_mut(constant.name().val.as_str()) {
            rest.push(*constant.loc_span());
        } else {
            symbols.insert(constant.name().val.clone(),
                           (*constant.loc_span(), vec![]));
        }
    }

    for parameter in parameters {
        let maybe_auth_decl_span = *parameter.get_likely_definition()
            .object.loc_span();
        let name = parameter.get_likely_definition().object.name.val.clone();
        if let Some((_, rest)) = symbols.get_mut(&name) {
            rest.push(maybe_auth_decl_span);
        } else {
            symbols.insert(name, (maybe_auth_decl_span, vec![]));
        }
    }

    for (name, (used, objs)) in &mut subobjs {
        let maybe_auth_decl_span = objs.first().unwrap()
            .1.obj.object.name.span;
        if let Some((_, rest)) = symbols.get_mut(name) {
            rest.push(maybe_auth_decl_span);
        } else {
            *used = true;
            symbols.insert(name.to_string(), (maybe_auth_decl_span, vec![]));
        }
    }

    for (name, (used, methods)) in &mut methods {
        let maybe_auth_decl_span = methods.first().unwrap()
            .1.name.span;
        if let Some((_, rest)) = symbols.get_mut(name) {
            rest.push(maybe_auth_decl_span);
        } else {
            *used = true;
            symbols.insert(name.to_string(), (maybe_auth_decl_span, vec![]));
        }
    }

    for (name, (used, decls)) in &mut saveds {
        for (_, (var, _)) in decls {
            let maybe_auth_decl_span = var.object.name.span;
            if let Some((_, rest)) = symbols.get_mut(name) {
                rest.push(maybe_auth_decl_span);
            } else {
                *used = true;
                symbols.insert(name.to_string(),
                               (maybe_auth_decl_span, vec![]));
            }
        }
    }
    for (name, (used, decls)) in &mut sessions {
        for (_, (var, _)) in decls {
            let maybe_auth_decl_span = var.object.name.span;
            if let Some((_, rest)) = symbols.get_mut(name) {
                rest.push(maybe_auth_decl_span);
            } else {
                *used = true;
                symbols.insert(name.to_string(),
                               (maybe_auth_decl_span, vec![]));
            }
        }
    }

    for (name, (used, decls)) in &mut hooks {
        for (_, hook) in decls {
            let maybe_auth_decl_span = hook.obj.name().span;
            if let Some((_, rest)) = symbols.get_mut(name) {
                rest.push(maybe_auth_decl_span);
            } else {
                *used = true;
                symbols.insert(name.to_string(),
                               (maybe_auth_decl_span, vec![]));
            }
        }
    }

    for (name, (auth, others)) in &symbols {
        if !others.is_empty() {
            report.push(DMLError {
                span: (*auth),
                description: format!("Name collision in declaration on '{}'",
                                     name),
                related: others.iter().map(
                    |s|(*s,
                        "also declared here".to_string())).collect(),
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
    }
    (symbols, constants, saveds, sessions, methods, hooks, subobjs)
}

fn merge_composite_subobj<'c>(name: String,
                              parent_each_stmts: &InEachSpec,
                              specs: Vec<(ObjectDecl<CompositeObject>,
                                          Arc<ObjectSpec>)>,
                              parent_key: Option<StructureKey>,
                              container: &'c mut StructureContainer,
                              report: &mut Vec<DMLError>) -> StructureKey {
    debug!("Merging a composite subobj for {}", name);
    let (auth_obj, auth_spec) = &specs.first().unwrap();
    // Grab the first spec objectdecl for the auth kind
    let auth_kind = &auth_obj.obj.kind.kind;
    // similar, first spec gives auth loc
    let auth_loc = &auth_spec.loc;
    let kind_collisions: Vec<&ZeroSpan> = specs.iter()
        .filter_map(|(o, _)|if &o.obj.kind.kind != auth_kind {
            Some(&o.obj.object.name.span)
        } else {
            None
        }).collect();
    if !kind_collisions.is_empty() {
        report.push(DMLError {
            span: *auth_loc,
            description: format!("Inconsistent object type for {}", name),
            severity: Some(DiagnosticSeverity::ERROR),
            related: kind_collisions.iter().map(
                |l|(*(*l),
                    "mismatching type here".to_string())).collect(),
        });
    }

    // Vec of (auth, mismatching name) array declarations
    // TODO: How to actually evaluate expressions? We might need to defer
    // this to later and more closely mimic how DMLC does this
    let mut array_info: Vec<(ArrayDim, Vec<&ZeroSpan>)> =
        auth_obj.obj.dims.iter()
        .map(|d|(d.clone(), vec![])).collect();
    for (decl, _) in &specs {
        if decl.obj.dims.len() != array_info.len() {
            // When an object with no array decl conflicts with an object
            // with one, blame the object decl
            let error_span = if !decl.obj.dims.is_empty() {
                combine_vec_of_decls(&decl.obj.dims)
            } else {
                *decl.obj.span()
            };
            let related = if !auth_obj.obj.dims.is_empty() {
                vec![(combine_vec_of_decls(&auth_obj.obj.dims),
                      "expected this dimensionality".to_string())]
            } else {
                vec![(*auth_obj.obj.span(),
                      "no dimensions declared here".to_string())]
            };

            report.push(DMLError {
                span: error_span,
                description: "Mismatching number of dimensions \
                              in object declaration".to_string(),
                related,
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        for ((auth_decl, mismatches), other_decl) in
            array_info.iter_mut().zip(decl.obj.dims.iter()) {
                if auth_decl.indexvar.val != other_decl.indexvar.val {
                    mismatches.push(&other_decl.indexvar.span)
                }
                if auth_decl.size.is_none() {
                    auth_decl.size = other_decl.size.clone();
                }
                // TODO: also check auth size if its not none
            }
    }

    for (auth_decl, mismatches) in &array_info {
        if auth_decl.size.is_none() {
            report.push(DMLError {
                span: auth_decl.indexvar.span,
                description: "Missing array size specification \
                       for this array variable".to_string(),
                related: vec![],
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
        if !mismatches.is_empty() {
            report.push(DMLError {
                span: auth_decl.indexvar.span,
                description: "Other object declarations mismatch this \
                       array dimensions name".to_string(),
                related: mismatches.iter().map(
                    |s|(*(*s),
                        "mismatch here".to_string())).collect(),
                severity: Some(DiagnosticSeverity::ERROR),
            });
        }
    }

    let object_specs = specs.iter().map(|(_, s)|Arc::clone(s)).collect();

    make_object(auth_obj.obj.object.name.span,
                &auth_obj.obj.object.name,
                *auth_kind,
                array_info.iter().map(|(d, _)|d).cloned().collect(),
                specs.iter().map(|(_, s)|Arc::clone(s)).collect(),
                object_specs,
                parent_each_stmts,
                parent_key,
                container,
                report)
}

fn merge_composite_subobjs<'c>(parent_each_stmts: &InEachSpec,
                               subobjs: ObjectMapping,
                               parent_key: Option<StructureKey>,
                               container: &'c mut StructureContainer,
                               report: &mut Vec<DMLError>)
                               -> Vec<StructureKey> {
    debug!("Merging composite subobjects");
    subobjs.into_iter().filter_map(
        |(name, (used, subobj_specs))|
        if used {
            Some(merge_composite_subobj(name,
                                        parent_each_stmts,
                                        subobj_specs
                                        .into_iter()
                                        .map(|(_, o, s)|(o, s))
                                        .collect(),
                                        parent_key,
                                        container,
                                        report))
        } else {
            None
        }).collect()
    //obj.add_composite_component(new_obj_key, container);
    // TODO: Check collisions of 'name' parameters
}

// return a tuple (default_map, order)
// where 'default_map' maps method declarations to declarations they directly
//     override
// and 'order' is a order of methods, with lowest rank being last
fn sort_method_decls<'t>(decls: &[(&Rank, &'t MethodDecl)]) ->
    (HashMap<&'t MethodDecl, HashSet<&'t MethodDecl>>, Vec<&'t MethodDecl>) {
        debug!("Sorting method decls {:?}", decls);
        let mut rank_to_method: HashMap<&Rank, (Vec<&MethodDecl>, Vec<&MethodDecl>)>
            = HashMap::default();

        for (rank, decl) in decls {
            let (ref mut defs, ref mut decls) = rank_to_method.entry(rank).or_default();
            if decl.is_abstract() {
                decls.push(*decl);
            } else {
                defs.push(*decl);
            }
        }
        trace!("rank-to-method is: {:?}", rank_to_method);
        let ranks: HashSet<&Rank> = rank_to_method.keys()
            .cloned().collect();
        let mut ancestry: HashMap<&Rank, Vec<&Rank>> = ranks.iter()
            .map(|r|(*r, ranks.iter().filter(
                |r2|r.inferior.contains(&r2.id)).cloned().collect()))
            .collect();

        // Insert implicit special ancestry ordering to resolve unrelated ranks
        // where a non-abstract method can override an abstract method
        trace!("Initial ancestry is {:?}", ancestry);
        let top_ancestry: HashSet<&Rank> = ranks.iter()
            .filter(|a|!ancestry.values().any(|ra|ra.contains(*a)))
            .cloned()
            .collect();
        trace!("Top ancestry ranks are {:?}", top_ancestry);
        let new_edges = {
            let mut top_defs = vec![];
            let mut top_decls = vec![];
            for r in top_ancestry {
                let (defs, decls) = rank_to_method.get(r).unwrap();
                top_defs.extend(defs.iter().map(|d|(r, *d)));
                top_decls.extend(decls.iter().map(|d|(r, *d)));
            }
            if top_defs.len() == 1 {
                let (top_rank, top_def) = &top_defs[0];
                if !top_def.is_abstract() {
                    top_decls.iter().filter_map(|(subrank, subdecl)|
                        // In-rank case is handled in method sorting
                        if subdecl.is_abstract() && top_rank != subrank {
                            Some((top_rank, subrank))
                        } else {
                            None
                        })
                        .map(|(from, to)|(*from, *to))
                        .collect()
                } else {
                    vec![]
                }
            } else {
                vec![]
            }
        };

        for (from, to) in new_edges {
            trace!("Inserted implicit ancestry edge from {:?} to {:?}", from, to);
            ancestry.entry(from).or_default().push(to);
        }
        trace!("Ancestry is {:?}", ancestry);
        let mut minimal_ancestry: HashMap<&Rank, Vec<&Rank>>
            = HashMap::default();
        // Get ancestors that are not the ancestor of any ancestor
        for (rank, ancestors) in &ancestry {
            let mut minimal_ancestors = vec![];
            for ancestor in ancestors {
                if !ancestors.iter().flat_map(|a|ancestry.get(a).unwrap())
                    .any(|r|r == ancestor) {
                    minimal_ancestors.push(*ancestor);
                }
            }
            minimal_ancestry.insert(rank, minimal_ancestors);
        }

        // Map method declaration to those that they directly override
        let mut method_map: HashMap<&'t MethodDecl, HashSet<&'t MethodDecl>>
            = HashMap::default();
        for (r, (defs, decls)) in &rank_to_method {
            for def in defs {
                // Ensure the entry exists for later unwraps
                method_map.entry(def).or_default();
                for subrank in &minimal_ancestry[r] {
                    let (subdefs, subdecls) = rank_to_method.get(subrank).unwrap();
                    if subdefs.is_empty() {
                        method_map.get_mut(def).unwrap()
                            .extend(subdecls.clone());
                    } else {
                        method_map.get_mut(def).unwrap()
                            .extend(subdefs.clone());
                    }
                }
                method_map.get_mut(def).unwrap()
                    .extend(decls.iter().cloned());
            }
            for decl in decls {
                method_map.entry(decl).or_default();
            }
        }
            
        trace!("Default map is {:?}", method_map);
        let method_order = topsort(&method_map).unwrap_sorted();
        trace!("Method order is {:?}", method_order);
        (method_map, method_order)
}

fn add_methods(obj: &mut DMLCompositeObject,
               methods: MethodMapping,
               decl_to_trait_map: &HashMap<MethodDecl, Arc<DMLTrait>>,
               report: &mut Vec<DMLError>) {
    debug!("Adding methods to {}", obj.identity());

    for (name, (used, regular_decls)) in methods {
        if !used {
            trace!("Skipped {} name, declaration unused", name);
            continue;
        }
        trace!("Handling method {}, which is declared by {:?}",
               name, regular_decls);
        let all_decls: Vec<(&Rank, &MethodDecl)>
            = regular_decls.iter().map(|(a, b)|(a, b)).collect();

        let (default_map, method_order) = sort_method_decls(&all_decls);
        let mut decl_to_method: HashMap<MethodDecl, Arc<DMLMethodRef>>
            = HashMap::default();
        for method in method_order.iter().rev() {
            trace!("Handling overrides of {:?}", method);
            // Guaranteed by topsort
            let defaults = default_map.get(method).unwrap();
            let default = match defaults.len() {
                1 => {
                    let decl = defaults.iter().next().unwrap();
                    trace!("Default call decl is {:?}", decl);
                    trace!("And the current map is {:?}", decl_to_method);
                    if let Some(m) = decl_to_method.get(decl) {
                        Some(DefaultCallReference::Valid(Arc::clone(m)))
                    } else {
                        Some(DefaultCallReference::Abstract((*decl).clone()))
                    }
                },
                0 => None,
                _ => Some(DefaultCallReference::Ambiguous(
                    defaults.iter().map(|d| Arc::clone(decl_to_method.get(d).unwrap())).collect())),
            };
            trace!("Default call would be {:?}", default);
            for decl in defaults {
                // TODO: I suspect we could improve this error message in cases
                // where several similar incorrect overrides are made over one
                // method
                // We need to store it in a wider context and collect
                // it after this to report at most one diagnostic per
                // declaration

                if !decl.default && !decl.is_abstract() {
                    report.push(DMLError {
                        span: decl.name.span,
                        description:
                        "Cannot override non-default method".into(),
                        related: vec![(method.name.span,
                                       "overridden here".to_string())],
                        severity: Some(DiagnosticSeverity::ERROR),
                    });
                }

                 method.check_override(*decl, report);
            }

            let template_ref = decl_to_trait_map
                .get(method)
                .map(Arc::clone);

            let new_method = Arc::new(
                DMLMethodRef {
                    template_ref,
                    concrete_decl: DMLConcreteMethod {
                        decl: (*method).clone(),
                        default_call: default,
                    },
                }
            );
            trace!("Inserted dependent methoddecl {:?}", new_method);
            decl_to_method.insert((*method).clone(), new_method);
        }
        trace!("Complete decl-to-method map is: {:?}", decl_to_method);
        let to_add = decl_to_method.get(method_order.first().unwrap()).unwrap();
        debug!("Added {:?}", to_add.identity());
        obj.add_shallow_component(DMLShallowObjectVariant::Method(
            Arc::clone(to_add)));
    }
}

fn check_trait_overrides(obj: &DMLCompositeObject,
                         container: &StructureContainer,
                         report: &mut Vec<DMLError>) {
    debug!("Checking traits overrides on {:?}", obj.identity);
    trace!("all symbols are: {:?}",
           obj.components.keys().collect::<Vec<&String>>());
    fn report_collision(name: &str,
                        srcloc: ZeroSpan,
                        coll_loc: ZeroSpan,
                        report: &mut Vec<DMLError>) {
        report.push(DMLError {
            span: srcloc,
            description: format!("Name collision on '{}'", name),
            severity: Some(DiagnosticSeverity::ERROR),
            related: vec![(coll_loc, "Previously defined here".to_string())],
        });
    }
    for templ in obj.templates.values() {
        let traitspec = &templ.traitspec;
        for (name, impl_trait) in get_impls(traitspec) {
            trace!("Checking for existence of {}", name);
            let maybe_collision = obj.components.get(&name)
                .map(|c|c.resolve(container));
            // Some members are invisible, so getting none here is not
            // entirely unexpected
            if let Some(member) = impl_trait.get_member(&name) {
                if let Some(collision) = maybe_collision {
                    // Sessions and saveds cannot be overridden
                    // in dmlc this is handled in vtables, we
                    // currently do not emulate that
                    // so report it here unless the declaration we got
                    // is the same as the one in the trait
                    if let TraitMemberKind::Session(data) = member {
                        let mut ok = false;
                        if let DMLResolvedObject::ShallowObject(
                            DMLShallowObject {
                                variant: DMLShallowObjectVariant::Session(s),
                                ..
                            }) = collision {
                            ok = &s.declaration == data;
                        }
                        if !ok {
                            report_collision(&name,
                                             *collision.location(),
                                             data.name.span,
                                             report);
                        }
                    } else if let TraitMemberKind::Saved(data) = member {
                        let mut ok = false;
                        if let DMLResolvedObject::ShallowObject(
                            DMLShallowObject {
                                variant: DMLShallowObjectVariant::Saved(s),
                                ..
                            }) = collision {
                            ok = &s.declaration == data;
                        }
                        if !ok {
                            report_collision(&name,
                                             *collision.location(),
                                             data.name.span,
                                             report);
                        }
                    } else if !member.same_kind(&collision) {
                        report_collision(&name,
                                         *collision.location(),
                                         *member.location(),
                                         report);
                    }
                    if let Some(meth) = member.as_method() {
                        if let DMLResolvedObject::ShallowObject(
                            DMLShallowObject {
                                variant: DMLShallowObjectVariant::Method(m),
                                ..
                            }) = collision {
                                // Special case, we are allowed conflict with
                                // an abstract method if we can override it
                                if m.is_abstract() {
                                    meth.check_override(m.get_decl(), report);
                                } else {
                                    m.check_override(meth, report);
                                }
                            }
                    }
                } else if let TraitMemberKind::Method(meth) = member {
                    if meth.is_abstract() {
                        // TODO: find the actual instantiation to blame
                        report.push(DMLError {
                            span: *obj.location(),
                            description: format!(
                                "Instantiating template {} \
                                 requires method '{}' to be \
                                 defined", templ.name, name),
                            related: vec![(
                                // This is guaranteed to be safe, since
                                // the trait is non-empty
                                impl_trait.loc.unwrap(),
                                "Template defined here".to_string()
                            )],
                            severity: Some(DiagnosticSeverity::ERROR),
                        });
                    }
                }
            }
        }
    }
}

#[allow(clippy::ptr_arg)]
fn obj_invariants(obj: &mut DMLCompositeObject,
                  _report: &mut Vec<DMLError>) {
    match obj.kind {
        // Note: in DMLC for some reason the bank invariants
        // are checked from the perspective of the device
        CompObjectKind::Bank => {
            // TODO: check register overlap
        },
        CompObjectKind::Register => {
            // TODO: check that fields are not
            // overlapping and within register
            // TODO: check register storage sanity versus
            // set/get methods existence
        },
        CompObjectKind::Field => {
            // TODO: check that bit ranges are not inverse
        },
        CompObjectKind::Interface => {
            // TODO: check interface type
        },
        _ => (),
    }
}


#[allow(clippy::ptr_arg)]
fn export_invariants<'c>(_toplevel: &TopLevel,
                         _obj: &DMLCompositeObject,
                         _container: &'c StructureContainer,
                         _report: &mut Vec<DMLError>) {
    // TODO: check existence, validity, name collisions etc.
}

fn make_auto_params(identity: &DMLString,
                    declloc: &ZeroSpan,
                    kind: CompObjectKind,
                    index_info: &[ArrayDim],
                    parent_key: Option<StructureKey>) ->
    HashMap<String, ObjectDecl<Parameter>> {
        // Add auto parameter definitions
        let mut auto_parameters = HashMap::default();

        auto_parameters.insert(
            "_ident".to_string(),
            make_objectdecl_auto_param(
                "_ident",
                &identity.span,
                Some(ParamValue::Set(Box::new(
                    ExpressionKind::StringLiteral(identity.clone()))))));

        let mut indices = vec![];
        for arraydim in index_info {
            let new_decl = make_objectdecl_auto_param(
                &arraydim.indexvar.val, &arraydim.indexvar.span,
                arraydim.size.as_ref().map(|e|ParamValue::Set(e.clone())));
            auto_parameters.insert(arraydim.indexvar.val.to_string(), new_decl);
            // Note: the location of this identifier is fake and will point to
            // the index declaration (this is probably fine)
            indices.push(Box::new(ExpressionKind::Identifier(Identifier {
                name: arraydim.indexvar.clone(),
            })));
        }

        auto_parameters.insert(
            "indices".to_string(),
            make_objectdecl_auto_param(
                "indices", declloc,
                Some(ParamValue::Set(Box::new(ExpressionKind::ConstantList(
                    ConstantList {
                        // Note: This declaration location is very fake, and
                        // will point to the objects auth decl.
                        span: *declloc,
                        list: indices,
                    }))))));
        match kind {
            CompObjectKind::Device => {
                // device-level implicit parameters are reasonably defined
                // at the 'device' decl
                auto_parameters.insert(
                    "obj".to_string(),
                    make_objectdecl_auto_param(
                        "obj",
                        declloc,
                        Some(ParamValue::Set(Box::new(
                            ExpressionKind::Unknown(*declloc))))));
                auto_parameters.insert(
                    "simics_api_version".to_string(),
                    make_objectdecl_auto_param(
                        "simics_api_version",
                        declloc,
                        Some(ParamValue::Set(Box::new(
                            ExpressionKind::Unknown(*declloc))))));
            },
            CompObjectKind::Event => {
                auto_parameters.insert(
                    "evclass".to_string(),
                    make_objectdecl_auto_param(
                        "evclass",
                        declloc,
                        Some(ParamValue::Set(Box::new(
                            ExpressionKind::Unknown(*declloc))))));
            },
            _ => (),
        }
        auto_parameters.insert(
            "qname".to_string(),
            make_objectdecl_auto_param(
                "qname", &identity.span,
                Some(ParamValue::Set(
                    // TODO: Calculate real qname
                    Box::new(ExpressionKind::StringLiteral(
                        identity.clone()))))));

        auto_parameters.insert(
            "parent".to_string(),
            make_objectdecl_auto_param(
                "parent", declloc,
                Some(ParamValue::Set(
                    Box::new(
                        if let Some(key) = parent_key {
                            ExpressionKind::AutoObjectRef(key,
                                                          *declloc)
                        } else {
                            ExpressionKind::Undefined(*declloc)
                        }
                    )))));
        auto_parameters
    }

pub fn make_object(loc: ZeroSpan,
                   identity: &DMLString,
                   kind: CompObjectKind,
                   array_info: Vec<ArrayDim>,
                   merged_obj_specs: Vec<Arc<ObjectSpec>>,
                   mut obj_specs: Vec<Arc<ObjectSpec>>,
                   parent_each_stmts: &InEachSpec,
                   parent_key: Option<StructureKey>,
                   container: &mut StructureContainer,
                   report: &mut Vec<DMLError>) -> StructureKey {
    debug!("Making object {}", identity.val);

    let mut each_stmts = parent_each_stmts.clone();
    let used_ineach_locs = add_template_specs(&mut obj_specs, &each_stmts);
    add_template_ineachs(&obj_specs, &mut each_stmts);

    trace!("Has specs at {:?}", obj_specs.iter().map(|rc|rc.loc)
          .collect::<Vec<ZeroSpan>>());

    trace!("Complete specs are: {:?}",
           obj_specs.iter().map(|rc|rc.as_ref()).collect::<Vec<&ObjectSpec>>());
    trace!("Complete each_stmt structure is: {:?}",
           each_stmts);
    let auto_params = make_auto_params(identity, &loc, kind, &array_info,
                                       parent_key);
    let parameters = gather_parameters(&loc, &obj_specs, &array_info,
                                       auto_params, report);

    trace!("Parameters are: {:?}", parameters);

    let (symbols, constants,
         saveds, sessions,
         methods, hooks,
         subobjs) = collect_symbols(&parameters, &obj_specs, report);
    trace!("All local symbols are: {:?}",
           symbols.keys().map(|k|k.as_str()).collect::<Vec<&str>>());

    let new_obj_key = create_object_instance(Some(loc),
                                             merged_obj_specs,
                                             identity, kind,
                                             &array_info, container);

    let subobj_keys =
        merge_composite_subobjs(&each_stmts, subobjs,
                                Some(new_obj_key), container, report);

    {
        let new_obj = container.get_mut(new_obj_key).unwrap();
        new_obj.definitions = obj_specs.clone();
        new_obj.used_ineach_locs = used_ineach_locs;
        add_templates(new_obj, &obj_specs);
        add_constants(new_obj, constants);
        add_parameters(new_obj, parameters);
        add_hooks(new_obj, hooks);
        add_saveds(new_obj, saveds);
        add_sessions(new_obj, sessions);
    }
    // We need to drop the new_obj reference here, so that we can
    // access multiple keys simultaneously from the storage
    add_subobjs(new_obj_key, subobj_keys, container);
    {
        let new_obj = container.get_mut(new_obj_key).unwrap();
        let trait_method_map = new_obj.templates.values()
            .flat_map(|t|t.traitspec.methods.values()
                      .map(|m|(m.decl.clone(), Arc::clone(&t.traitspec)))
                      .collect::<Vec<_>>())
            .collect();

        add_methods(new_obj, methods, &trait_method_map, report);
    }
    check_trait_overrides(container.get(new_obj_key).unwrap(),
                          container, report);
    obj_invariants(container.get_mut(new_obj_key).unwrap(), report);
    new_obj_key
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
// Figures out the ranks of DML declarations
use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::iter::{self, Iterator};
use std::sync::Arc;
use std::sync::Mutex;

use itertools::Itertools;
use lazy_static::lazy_static;
use log::{debug, error, trace};
use lsp_types::DiagnosticSeverity;

use crate::analysis::DeclarationSpan;
use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::objects::{CompObjectKindDecl, CompositeObject, Import, InEach, Instantiation, Template};
use crate::analysis::structure::toplevel::{ExistCondition, ObjectDecl,
                                           StatementSpecStatement,
                                           StatementSpec, TopLevel};
use crate::analysis::DMLError;
use crate::analysis::templating::objects::{create_objectspec};
use crate::analysis::templating::traits::{TemplateTraitInfo,
                                          DMLTemplate,
                                          DMLTrait};

lazy_static! {
    static ref RANKMAKER_ID: Mutex<u64> = Mutex::new(0);
}

#[derive(Debug, Clone, Hash)]
pub struct RankMaker {
    id: u64,
    next_rank_id: u64,
}

impl Default for RankMaker {
    fn default() -> Self {
        Self::new()
    }
}

impl RankMaker {
    pub fn new() -> RankMaker {
        let mut id_ref = RANKMAKER_ID.lock().unwrap();
        let id = *id_ref;
        *id_ref += 1;
        RankMaker {
            id,
            next_rank_id: 0,
        }
    }
    pub fn new_rank(&mut self,
                    desc: RankDesc,
                    inferior: Vec<&Rank>) -> Rank {
        let mut inferior_rank_ids = HashSet::new();
        for rank in inferior {
            if rank.rank_batch == self.id {
                inferior_rank_ids.insert(rank.id);
                inferior_rank_ids.extend(rank.inferior.iter().cloned());
            } else {
                error!("Inferior rank {:?} did not share rank \
                        batch ID with {:?}",
                       rank, self);
            }
        }
        let to_return = Rank {
            inferior: inferior_rank_ids.into_iter().collect(),
            id: self.next_rank_id,
            desc,
            rank_batch: self.id,
        };
        self.next_rank_id += 1;
        to_return
    }
}

#[derive(Debug, Clone)]
pub struct Rank {
    pub inferior: Vec<u64>,
    pub desc: RankDesc,
    pub id: u64,
    rank_batch: u64,
}

impl PartialEq for Rank {
    fn eq(&self, other: &Self) -> bool {
        self.rank_batch == other.rank_batch && self.id == other.id
    }
}

impl Hash for Rank {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rank_batch.hash(state);
        self.id.hash(state);
    }
}

impl Eq for Rank {}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if other.rank_batch != self.rank_batch {
            return None;
        }
        if other.id == self.id {
            Some(Ordering::Equal)
        } else if other.inferior.iter().any(|rc|&self.id == rc) {
            Some(Ordering::Less)
        } else if self.inferior.iter().any(|rc|&other.id == rc) {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RankDescKind {
    File,
    Template
}

impl RankDescKind {
    fn as_str(&self) -> &'static str {
        match self {
            RankDescKind::File => "file",
            RankDescKind::Template => "template",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RankDesc {
    pub kind: RankDescKind,
    pub desc: String,
    pub in_eachs: Vec<Vec<String>>,
}

impl RankDesc {
    pub fn format(&self) -> String {
        let mut frm = format!("{} {}", self.kind.as_str(), self.desc);
        for ineachnames in &self.in_eachs {
            frm = format!("'in each ({})' block from {}",
                          ineachnames.join(", "), frm);
        }
        frm
    }
    pub fn extended_names(&self, names: Vec<&str>) -> RankDesc {
        RankDesc {
            kind: self.kind,
            desc: self.desc.clone(),
            in_eachs: {
                let mut new_in_eachs: Vec<Vec<String>>
                    = self.in_eachs.to_vec();
                new_in_eachs.insert(0, names.iter().map(|s|s.to_string())
                                    .collect());
                new_in_eachs
            },
        }
    }
}

pub fn create_templates_traits<'t>(
    device_span: &ZeroSpan,
    rankmaker: &mut RankMaker,
    template_specs: HashMap<&'t str, TemplateRef<'t>>,
    order: Vec<&'t str>,
    invalid_isimps: HashMap<InferiorVariant<'t>, Vec<&'t str>>,
    imp_map: &'t HashMap<Import, String>,
    rank_struct: HashMap<&'t str, InEachStruct<'t>>,
    report: &mut Vec<DMLError>)
    -> TemplateTraitInfo {
    let mut templates = HashMap::default();
    let mut traits = HashMap::default();
    debug!("template order is: {:?}", order);
    for templname in order {
        let templ = &template_specs[templname];
        let rankinf = &rank_struct[templname];
        let spec = create_objectspec(*templ.get_def_loc()
                                     .unwrap_or(device_span),
                                     *templ.span(),
                                     None,
                                     templ.get_spec(),
                                     &ExistCondition::Always,
                                     templ.get_rankdesc(),
                                     rankinf,
                                     &invalid_isimps,
                                     imp_map,
                                     &templates,
                                     rankmaker,
                                     report);
        // In DMLC, we will only create a trait for templates
        // that contain 'trait statements', for now I am here
        // also creating traits in any case, just with nothing
        // in them
        let traitspec = DMLTrait::process(templname, templ.get_def_loc(),
                                          &spec, &traits, report);
        traits.insert(templname.to_string(), Arc::clone(&traitspec));
        let template = DMLTemplate::make(
            templname,
            templ.get_def_loc(),
            spec, traitspec);
        templates.insert(templname.to_string(), template);
    }

    TemplateTraitInfo {
        templates,
        traits,
    }
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum InferiorVariant<'t> {
    Is(&'t ObjectDecl<Instantiation>),
    ImplicitIs(&'t CompObjectKindDecl),
    Object(&'t ObjectDecl<CompositeObject>),
    InEach(&'t ObjectDecl<InEach>),
    Import(&'t ObjectDecl<Import>),
}

impl <'t> InferiorVariant<'t> {
    // We use this for reporting only, so we can faux the span a bit to point
    // to the exact name
    pub fn span(&self) -> &'t ZeroSpan {
        match self {
            InferiorVariant::Is(objdecl) => &objdecl.obj.span,
            InferiorVariant::ImplicitIs(kinddecl) => &kinddecl.span,
            InferiorVariant::Object(objdecl) => &objdecl.obj.kind.span,
            InferiorVariant::InEach(objdecl) => &objdecl.obj.span,
            InferiorVariant::Import(objdecl) => &objdecl.obj.span,
        }
    }
}

pub type Inferiors<'t> = HashMap<&'t str, InferiorVariant<'t>>;

pub type InEachStructMap<'t> = HashMap::<&'t ObjectDecl<InEach>,
                                         InEachStruct<'t>>;

#[derive(PartialEq, Debug, Default, Clone)]
pub struct InEachStruct<'t> {
    pub inferior: Inferiors<'t>,
    pub in_eachs: InEachStructMap<'t>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct DependencyInfo<'t> {
    pub inferior: Inferiors<'t>,
    // We do not need to track name->site here, since we can always look that up
    // in inferior
    pub unconditional_references: HashSet<&'t str>,
    pub in_eachs: InEachStructMap<'t>,
}

pub fn dependencies<'t>(statements: &'t StatementSpec,
                        imp_map: &'t HashMap<Import, String>)
                        -> DependencyInfo<'t> {
    let mut queue = vec![];
    let mut inferior = Inferiors::new();
    let mut unconditional_references: HashSet<&'t str> = HashSet::new();
    let mut in_eachs = InEachStructMap::new();

    for inst in &statements.instantiations {
        queue.push(InferiorVariant::Is(inst));
    }
    for ineach in &statements.ineachs {
        queue.push(InferiorVariant::InEach(ineach));
    }
    for objstmnt in &statements.objects {
        queue.push(InferiorVariant::Object(objstmnt));
        queue.push(InferiorVariant::ImplicitIs(
            &objstmnt.obj.kind));
    }
    for import in &statements.imports {
        queue.push(InferiorVariant::Import(import));
    }

    while let Some(decl) = queue.pop() {
        match decl {
            InferiorVariant::Object(obj) => {
                queue.extend(obj.spec.objects.iter().map(
                    |o|InferiorVariant::Object(o)));
                queue.extend(obj.spec.ineachs.iter().map(
                    |o|InferiorVariant::InEach(o)));
                queue.extend(obj.spec.instantiations.iter().map(
                    |o|InferiorVariant::Is(o)));
                // There should not be any imports to add here
                if !obj.spec.imports.is_empty() {
                    error!("Unexpectedly allowed imports in object declaration \
                            {:?}", obj);
                    queue.extend(obj.spec.imports.iter().map(
                        |o|InferiorVariant::Import(o)));
                }
            },
            InferiorVariant::Is(is) => {
                for name in &is.obj.names {
                    inferior.insert(&name.val, InferiorVariant::Is(is));
                    if is.cond == ExistCondition::Always {
                        unconditional_references.insert(&name.val);
                    }
                }
            },
            InferiorVariant::ImplicitIs(kind) => {
                inferior.insert(kind.kind_name(),
                                InferiorVariant::ImplicitIs(kind));
            },
            InferiorVariant::Import(imp) => {
                let name = imp_map.get(&imp.obj)
                    .map_or_else(||imp.obj.imported_name(), |s|s.as_str());
                debug!("Mapped {:?} to {:?}", imp.obj, name);
                inferior.insert(name,
                                InferiorVariant::Import(imp));
                if imp.cond == ExistCondition::Always {
                    unconditional_references.insert(name);
                }
            },
            InferiorVariant::InEach(ineach) => {
                let DependencyInfo {
                    inferior: mut inf_inferior,
                    in_eachs: inf_in_eachs,
                    unconditional_references: inf_unconditional_references,
                } = dependencies(&ineach.spec, imp_map);

                for name in &ineach.obj.spec {
                    inf_inferior.insert(&name.val,
                                        InferiorVariant::InEach(ineach));
                }
                for (key, value) in &inf_inferior {
                    inferior.insert(key, value.clone());
                }
                in_eachs.insert(ineach, InEachStruct {
                    inferior: inf_inferior,
                    in_eachs: inf_in_eachs,
                });
                if ineach.cond == ExistCondition::Always {
                    for name in &ineach.obj.spec {
                        unconditional_references.insert(&name.val);
                    }
                    for name in &inf_unconditional_references {
                        unconditional_references.insert(name);
                    }
                }
            }
        }
    }
    DependencyInfo {
        inferior,
        unconditional_references,
        in_eachs,
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct MissingTemplate<'t> {
    pub name: &'t str,
    pub location: &'t ZeroSpan,
    pub unconditional: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TemplateRefKind<'t> {
    Template(&'t ObjectDecl<Template>),
    File(&'t TopLevel),
}

impl <'t> TemplateRefKind<'t> {
    pub fn get_spec(&self) -> &'t StatementSpec {
        match self {
            TemplateRefKind::Template(objspec) => &objspec.spec,
            TemplateRefKind::File(toplev) => &toplev.spec,
        }
    }

    pub fn get_name(&self) -> &'t str {
        match self {
            TemplateRefKind::Template(objspec) => &objspec.obj.object.name.val,
            TemplateRefKind::File(toplev) =>
                toplev.filename.to_str().unwrap(),
        }
    }

    fn get_def_loc(&self) -> &'t ZeroSpan {
        match self {
            TemplateRefKind::Template(objspec) => &objspec.obj.object.name.span,
            TemplateRefKind::File(file) => &file.start_of_file,
        }
    }

    pub fn span(&self) -> &'t ZeroSpan {
        match self {
            TemplateRefKind::Template(objspec) => &objspec.obj.object.name.span,
            TemplateRefKind::File(file) => &file.filespan,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TemplateRef<'t> {
    Exists(TemplateRefKind<'t>),
    Missing(MissingTemplate<'t>),
}

impl <'t> TemplateRef<'t> {
    fn get_name(&self) -> &'t str {
        match self {
            TemplateRef::Exists(template) => template.get_name(),
            TemplateRef::Missing(missing) => missing.name,
        }
    }

    fn get_def_loc(&self) -> Option<&'t ZeroSpan> {
        match self {
            TemplateRef::Exists(template) => Some(template.get_def_loc()),
            TemplateRef::Missing(_) => None,
        }
    }

    fn get_spec(&self) -> &'t StatementSpec {
        match self {
            TemplateRef::Exists(template) => template.get_spec(),
            TemplateRef::Missing(_) => &EMPTY_SPEC,
        }
    }
    fn span(&self) -> &'t ZeroSpan {
        match self {
            TemplateRef::Exists(templ) => templ.span(),
            TemplateRef::Missing(missing) => missing.location,
        }
    }
    pub fn get_rankdesc(&self) -> RankDesc {
        match self {
            TemplateRef::Exists(r) =>
                match r {
                    TemplateRefKind::Template(t) => {
                        RankDesc {
                            kind: RankDescKind::Template,
                            desc: t.obj.object.name.val.to_string(),
                            in_eachs: vec![],
                        }
                    },
                    TemplateRefKind::File(f) => {
                        RankDesc {
                            kind: RankDescKind::File,
                            desc: f.filespan.path().file_name().unwrap()
                                .to_str().unwrap().to_string(),
                            in_eachs: vec![],
                        }
                    },
                },
            TemplateRef::Missing(r) => {
                RankDesc {
                    kind: RankDescKind::Template,
                    desc: r.name.to_string(),
                    in_eachs: vec![],
                }
            }
        }
    }
}

lazy_static! {
    static ref EMPTY_SPEC: StatementSpec = StatementSpec::empty();
}

// (name -> template,
//  order,
//  invalid is-es or imports,
//  ineach map)
type RankedTemplates<'t> = (HashMap<&'t str, TemplateRef<'t>>,
                            Vec<&'t str>,
                            HashMap<InferiorVariant<'t>,
                                    Vec<&'t str>>,
                            HashMap<&'t str, InEachStruct<'t>>);

pub fn rank_templates<'t>(real_templates: &HashMap<&'t str,
                                                   &'t ObjectDecl<Template>>,
                          files: &HashMap<&'t str, &'t TopLevel>,
                          imp_map: &'t HashMap<Import, String>,
                          report: &mut Vec<DMLError>)
                          -> RankedTemplates<'t>
{
    let mut temp_templates: HashMap<&'t str, TemplateRef<'t>> =
        real_templates.iter().map(
            |(k,t)|(*k, TemplateRef::Exists(
                TemplateRefKind::Template(t)))).collect();
    temp_templates.extend(
        files.iter()
            .map(|(k,t)|
                 (*k, TemplateRef::Exists(
                     TemplateRefKind::File(t)))));
    rank_templates_aux(temp_templates, imp_map, HashMap::default(), report)
}

// We do not output errors for these, as they are confusing and the warning
// from the server is more interesting
// We do not ignore templates prefaced with _, as these are considered internal
pub const BUILTIN_TEMPLATES: [&str; 47] = ["name",
                                           "desc",
                                           "shown_desc",
                                           "documentation",
                                           "limitations",
                                           "init",
                                           "post_init",
                                           "object",
                                           "group",
                                           "attribute",
                                           "bool_attr",
                                           "uint64_attr",
                                           "int64_attr",
                                           "double_attr",
                                           "pseudo_attr",
                                           "read_only_attr",
                                           "write_only_attr",
                                           "connect",
                                           "init_as_subobj",
                                           "interface",
                                           "port",
                                           "subdevice",
                                           "implement",
                                           "bank_io_memory",
                                           "bank_transaction",
                                           "bank",
                                           "register",
                                           "type",
                                           "field",
                                           "get_val",
                                           "set_val",
                                           "get",
                                           "set",
                                           "read_register",
                                           "write_register",
                                           "read_field",
                                           "write_field",
                                           "read",
                                           "write",
                                           "init_val",
                                           "event",
                                           "simple_time_event",
                                           "simple_cycle_event",
                                           "custom_time_event",
                                           "custom_cycle_event",
                                           "uint64_time_event",
                                           "uint64_cycle_event"];

pub fn rank_templates_aux<'t>(mut templates: HashMap<&'t str,
                                                     TemplateRef<'t>>,
                              imp_map: &'t HashMap<Import, String>,
                              mut invalid_isimps: HashMap<InferiorVariant<'t>,
                                                          Vec<&'t str>>,
                              report: &mut Vec<DMLError>)
                              -> RankedTemplates<'t>
{
    debug!("Sorting templates {}", templates.keys().cloned().join(", "));
    let mut required_templates
        = HashMap::<&'t str, HashSet<&'t str>>::new();
    let mut rank_struct = HashMap::<&'t str, InEachStruct<'t>>::new();
    for template in templates.values() {
        let DependencyInfo::<'t> {
            inferior,
            in_eachs,
            unconditional_references,
        } = dependencies(template.get_spec(), imp_map);
        let referenced: HashSet<&'t str>
            = inferior.keys().filter(|s|!invalid_isimps.values().flatten().
                                     any(|s2|&s2 == s))
            .cloned().collect();
        trace!("Template {:?} requires {:?}", template.get_name(), referenced);
        required_templates.insert(template.get_name(), referenced.clone());
        let all_missing: HashSet<&'t str> = referenced.difference(
            &templates.keys().cloned().collect()).cloned().collect();
        trace!("Missing templates are {:?}", all_missing);
        if !all_missing.is_empty() {
            for missing_template_name in &all_missing {
                match &inferior[missing_template_name] {
                    InferiorVariant::Import(inf) => {
                        report.push(
                            DMLError {
                                span: *inf.span(),
                                description: format!(
                                    "Could not find file; '{}'",
                                    missing_template_name),
                                related: vec![],
                                severity: Some(DiagnosticSeverity::ERROR),
                            });
                    },
                    InferiorVariant::Is(inf) => {
                        if !BUILTIN_TEMPLATES.iter().any(
                            |name|name==missing_template_name) {
                            // The 'is' may contain more names than we
                            // are looking for, find the particular name
                            // that matches and use that span
                            // Because the same name could be used multiple
                            // times, but this only causes inferior binding
                            // report one error per such span

                            let spans: Vec<_> = inf.obj.names.iter()
                                .filter(|dmlname|
                                        &dmlname.val == missing_template_name)
                                .map(|dmlname|dmlname.span())
                                .collect();
                            if spans.is_empty() {
                                internal_error!("Unexpectedly no name \
                                                 matching missing template \
                                                 in {:?} (wanted {})",
                                                inf, missing_template_name);
                                continue;
                            }
                            for span in spans {
                                report.push(
                                    DMLError {
                                        span: *span,
                                        description: format!(
                                            "No template; '{}'",
                                            missing_template_name),
                                        related: vec![],
                                        severity: Some(
                                            DiagnosticSeverity::ERROR),
                                    });
                            }
                        }
                    },
                    // All implicit inferior references are object templates, and thus do not
                    // need to be reported
                    InferiorVariant::ImplicitIs(_) => {
                        debug!("Implicit import of '{}' missing and intentionally ignored",
                               missing_template_name);
                    },
                    inf => {
                        internal_error!(
                            "Unexpected template dependency through {:?}",
                            inf);
                    },
                }
                debug!("Added dummy missing template {}",
                       missing_template_name);
                templates.insert(
                    missing_template_name,
                    TemplateRef::Missing(
                        MissingTemplate {
                            name: missing_template_name,
                            location: inferior[missing_template_name].span(),
                            unconditional: unconditional_references
                                .contains(
                                    missing_template_name),
                        }));
            }
            trace!("Recursed template ranking");
            return rank_templates_aux(templates, imp_map,
                                      invalid_isimps, report);
        }
        rank_struct.insert(template.get_name(),
                           InEachStruct {
                               inferior,
                               in_eachs,
                           });
    }
    match bottomsort(&required_templates) {
        SortResult::Cycle(cycle) => {
            let mut is_or_imp_sites: Vec<&'t ZeroSpan> = vec![];
            // At this point, duplicate template names are solved
            let mapped_cycle: Vec<&TemplateRef<'t>> = cycle.iter().map(
                |n|&templates[*n]).collect();
            let mut unrotated_cycle = mapped_cycle.iter();
            let firstelem = unrotated_cycle.next().unwrap();
            let rotated_cycle = unrotated_cycle.chain(
                iter::once(firstelem));
            debug!("Found template cycle {:?}",
                   mapped_cycle.iter()
                   .map(|tr|tr.get_name())
                   .collect::<Vec<&str>>());
            let mut is_import_cycle = false;
            for (templ1, templ2) in mapped_cycle.iter().zip(rotated_cycle) {
                // Find the instantiation for this template name
                let spec = templ1.get_spec();
                for stmnt in spec.all_statements() {
                    match stmnt {
                        StatementSpecStatement::Import(imp) => {
                            if imp_map.get(&imp.obj).map_or(
                                false,
                                |iname|iname == templ2.get_name()) {
                                is_import_cycle = true;
                                is_or_imp_sites.push(imp.span());
                            }
                        },
                        StatementSpecStatement::Instantiation(inst) => {
                            for name in &inst.obj.names {
                                if name.val == templ2.get_name() {
                                    is_or_imp_sites.push(&name.span);
                                }
                            }
                        },
                        _ => (),
                    }
                }
            }
            let (firsts, rests) = cycle.split_at(1);
            let first = firsts.first().unwrap();
            let second = rests.first().unwrap_or(first);
            let firsttempl = &templates[*first];

            report.push(DMLError {
                span: *firsttempl.span(),
                description: if is_import_cycle {
                    "DML file imports itself, \
                     either directly or indirectly".to_string()
                } else {
                    "Template inherits from itself, \
                     either directly or indirectly".to_string()
                },
                severity: Some(DiagnosticSeverity::ERROR),
                related: is_or_imp_sites.into_iter().map(|i|(
                    *i, "Imported through here".to_string())).collect(),
            });

            // Break the cycle by removing the is-es (or imports) from the
            // first template (or file) that instantiates (or imports)
            // the second. We accomplish this by marking them
            // and later just not using them as instantiations to
            // be handled
            let mut removed_one = false;
            debug!("Wants to break cycle between template {:?} and {}",
                   firsttempl, second);
            for stmnt in firsttempl.get_spec().all_statements() {
                match stmnt {
                    StatementSpecStatement::Instantiation(inst) => {
                        if inst.obj.names.iter()
                            .map(|name|&name.val)
                            .any(|name|name.as_str() == *second) {
                                trace!("Set to disregard invalid 'is' {:?}",
                                       inst);
                                removed_one = true;
                                invalid_isimps.entry(InferiorVariant::Is(inst))
                                    .and_modify(|v|v.push(second))
                                    .or_default();
                            }
                    },
                    StatementSpecStatement::Import(imp) => {
                        debug!("considering {:?}", stmnt);

                        // TODO: Look over if this is actually always safe
                        if &imp_map[&imp.obj] == second {
                            trace!("Set to disregard invalid 'import' {:?}",
                                   imp);
                            removed_one = true;
                            invalid_isimps.entry(InferiorVariant::Import(imp))
                                .and_modify(|v|v.push(second))
                                .or_default();
                        }
                    }
                    _ => (),
                }
            }
            if !removed_one {
                // TODO: This is a hard failure state that shouldnt happen,
                // consider panicking instead
                error!("Failed to break cycle around {}", second);
                return (HashMap::new(), vec![], HashMap::new(), HashMap::new());
            }
            rank_templates_aux(templates, imp_map, invalid_isimps, report)
        },
        SortResult::Sorted(order) => (templates,
                                      order,
                                      invalid_isimps,
                                      rank_struct),
    }
}

#[allow(clippy::ptr_arg)]
fn traverse<'t, T>(graph: &HashMap<&'t T, HashSet<&'t T>>,
                   currnode: &'t T,
                   path: Vec<&'t T>,
                   visited: &mut HashSet<&'t T>,
                   result: &mut Vec<&'t T>) -> Option<Vec<&'t T>>
where
    T : std::hash::Hash + std::cmp::Eq + ?Sized
{
    if path.contains(&currnode) {
        let (_, rest) = path.split_at(path.iter().position(
            |prevnode|*prevnode == currnode).unwrap());
        Some(rest.into())
    } else {
        for nextnode in &graph[currnode] {
            if !visited.contains(nextnode) {
                let mut newpath = path.clone();
                newpath.push(currnode);
                if let Some(cycle) = traverse(graph, nextnode,
                                              newpath, visited, result) {
                    return Some(cycle);
                }
            }
        }
        visited.insert(currnode);
        result.push(currnode);
        None
    }
}

pub enum SortResult<T> {
    Sorted(Vec<T>),
    Cycle(Vec<T>),
}

impl <T> SortResult<T> {
    pub fn unwrap_sorted(self) -> Vec<T> {
        if let SortResult::Sorted(order) = self {
            order
        } else {
            panic!("Internal Error: Unexpectedly unsorted result");
        }
    }
}

pub fn bottomsort<'t, T>(graph: &HashMap<&'t T, HashSet<&'t T>>)
                      -> SortResult<&'t T>
where
    T : std::hash::Hash + std::cmp::Eq + ?Sized
{
    let mut visited = HashSet::new();
    let mut result = vec![];
    for startnode in graph.keys() {
        if !visited.contains(startnode) {
            if let Some(cycle) = traverse(graph, startnode, vec![],
                                          &mut visited, &mut result) {
                return SortResult::Cycle(cycle);
            }
        }
    }
    SortResult::Sorted(result)
}

pub fn topsort<'t, T>(graph: &HashMap<&'t T, HashSet<&'t T>>)
                      -> SortResult<&'t T>
where
    T : std::hash::Hash + std::cmp::Eq + ?Sized
{
    match bottomsort(graph) {
        SortResult::Sorted(mut result) => {
            result.reverse();
            SortResult::Sorted(result)
        },
        cycle => cycle
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn topsort_order() {
        let a = "a";
        let b = "b";
        let c = "c";
        let d = "d";
        let e = "e";
        let f = "f";
        let g = "g";

        let graph: HashMap<&str, HashSet<&str>> =
            [(a, vec![b, c]),
             (b, vec![c, d]),
             (c, vec![e]),
             (d, vec![]),
             (e, vec![f, g]),
             (f, vec![]),
             (g, vec![])].iter().cloned()
                .map(|(k, v)|(k, v.into_iter().collect()))
                .collect();
        match topsort(&graph) {
            SortResult::Sorted(sort) => {
                let mut possible_next: HashSet<&str> = HashSet::default();
                let mut already_visited: HashSet<&str> = HashSet::default();
                possible_next.insert("a");
                for next in sort {
                    assert!(possible_next.contains(&next),
                            "incorrect method order");
                    assert!(!already_visited.contains(&next),
                            "double node in method order");
                    possible_next.extend(&graph[&next]);
                    already_visited.insert(next);
                }
            },
            SortResult::Cycle(_) => {
                panic!("Internal Error: Expected sorted, got cycle");
            }
        }
    }
}

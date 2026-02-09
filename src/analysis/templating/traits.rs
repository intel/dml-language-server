//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::borrow::Borrow;
use std::collections::{HashSet, HashMap};
use std::fmt::Write;
use std::iter;
use std::sync::Arc;

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::objects::{MaybeAbstract};
use crate::analysis::structure::expressions::DMLString;
use crate::analysis::structure::toplevel::{ObjectDecl, ExistCondition};

use crate::analysis::DMLError;
use crate::analysis::templating::methods::{DMLConcreteMethod, DMLMethodRef, DefaultCallReference, MethodDecl, MethodDeclaration};
use crate::analysis::templating::objects::{CoarseObjectKind,
                                           DMLNamedMember,
                                           DMLCoarseObjectKind,
                                           ObjectSpec, Spec};
use crate::analysis::templating::Declaration;
use crate::analysis::templating::types::eval_type_simple;
use crate::analysis::templating::topology::Rank;

use crate::analysis::DeclarationSpan;

use log::{debug, error, trace};
use lsp_types::DiagnosticSeverity;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLTrait {
    pub name: String,
    pub rank: Rank,
    // Missing templates have no loc. Realistically, they shouldn't need much
    // reported from them
    pub loc: Option<ZeroSpan>,
    // Strictly speaking, we do not need to store the names here
    // (and if we could, we would use a HashSet instead), however
    // since we cannot hash a value with hashmaps in it, the
    // indirection of (known to be unique) names works in place
    // of a hashset of dmltraits
    pub parents: HashMap<String, Arc<DMLTrait>>,
    pub ancestors: HashMap<String, Arc<DMLTrait>>,
    // This is the final specification of a trait in analysis. So
    // lock in the objects here and move them to owned
    // for further analysis later. Note that these are
    // _not_ objectdecls as conditional declarations are not
    // part of template type (this is IMO weird but what can you do)

    pub sessions: HashMap<String, Declaration>,
    pub saveds: HashMap<String, Declaration>,
    pub params: HashMap<String, Declaration>,

    pub abstract_methods: HashMap<String, MethodDecl>,
    // These are now concrete method declarations, since we need to be able
    // to handle them as such for purposes of TQMIC resolutions or default calls
    pub methods: HashMap<String, DMLConcreteMethod>,

    // Maps a symbol name to the ancestor implementing it
    // does NOT include symbols defined in this trait
    pub ancestor_map: HashMap<String, Arc<DMLTrait>>,

    // These symbols are reserved but not defined within this trait
    pub reserved_symbols: HashMap<String, ZeroSpan>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TraitMemberKind<'t> {
    Session(&'t Declaration),
    Saved(&'t Declaration),
    Parameter(&'t Declaration),
    Method(&'t MethodDecl),
}

impl <'t> DMLNamedMember for TraitMemberKind<'t> {
    fn location(&self) -> &'t ZeroSpan {
        match self {
            TraitMemberKind::Session(d) |
            TraitMemberKind::Saved(d) |
            TraitMemberKind::Parameter(d) => &d.name.span,
            TraitMemberKind::Method(m) => m.location(),
        }
    }

    fn identity(&self) -> &'t str {
        match self {
            TraitMemberKind::Session(d) |
            TraitMemberKind::Saved(d) |
            TraitMemberKind::Parameter(d) => &d.name.val,
            TraitMemberKind::Method(m) => m.identity()
        }
    }

    fn kind_name(&self) -> &'static str {
        match self {
            TraitMemberKind::Session(_) => "session",
            TraitMemberKind::Saved(_) => "saved",
            TraitMemberKind::Parameter(_) => "parameter",
            TraitMemberKind::Method(_) => "method",
        }
    }
}

impl <'t> TraitMemberKind<'t> {
    pub fn as_method(&self) -> Option<&'t MethodDecl> {
        match self {
            TraitMemberKind::Method(m) => Some(m),
            _ => None,
        }
    }
}

impl <'t> CoarseObjectKind for TraitMemberKind<'t> {
    fn coarse_kind(&self) -> DMLCoarseObjectKind {
        match self {
            TraitMemberKind::Session(_) => DMLCoarseObjectKind::Session,
            TraitMemberKind::Saved(_) => DMLCoarseObjectKind::Saved,
            TraitMemberKind::Parameter(_) => DMLCoarseObjectKind::Parameter,
            TraitMemberKind::Method(_) => DMLCoarseObjectKind::Method,
        }
    }
}

impl <'t> MaybeAbstract for TraitMemberKind<'t> {
    fn is_abstract(&self) -> bool {
        match self {
            TraitMemberKind::Parameter(p) => p.is_abstract(),
            TraitMemberKind::Method(m) => m.is_abstract(),
            _ => false,
        }
    }
}

// Because this always returns an Arc, we cannot fold this
// into the generic ref impl below
// Finds the RC trait containing the implementation of member_name in or under
// "me"
pub fn lookup_impl<'t, T>(me: &'t Arc<DMLTrait>,
                          member_name: &T)
                          -> Option<&'t Arc<DMLTrait>>
where T: Borrow<str> {
    if me.get_member(member_name).is_some() {
        Some(me)
    } else {
        me.ancestor_map.get(member_name.borrow())
    }
}

pub fn get_impls(me: &Arc<DMLTrait>) -> HashMap<String, Arc<DMLTrait>> {
    trace!("Getting all impls of {:?}", me.name);
    me.sessions.keys()
        .chain(me.saveds.keys())
        .chain(me.params.keys())
        .chain(me.methods.keys())
        .chain(me.abstract_methods.keys())
        .map(|name|(name.to_string(), Arc::clone(me)))
        .chain(me.ancestor_map.iter().map(|(n, rc)|(n.clone(), Arc::clone(rc))))
        .collect()
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplementRelation {
    Unrelated,
    Implements,
    ImplementedBy,
    Same,
}

impl DMLTrait {
    pub fn get_defined_method<'t, T>(&'t self, method_name: &T)
                             -> Option<&'t DMLConcreteMethod>
    where T: Borrow<str> + ?Sized  {
        self.methods.get(method_name.borrow())
    }

    pub fn get_method<'t, T>(&'t self, method_name: &T)
                             -> Option<&'t MethodDecl>
    where T: Borrow<str> + ?Sized  {
        self.get_member(method_name)?.as_method()
    }

    pub fn implement_relation<T>(&self, other: &T) -> ImplementRelation
    where T: Borrow<DMLTrait>
    {
        if self.rank == other.borrow().rank {
            return ImplementRelation::Same;
        }
        match (self.ancestors.contains_key(&other.borrow().name),
               other.borrow().ancestors.contains_key(&self.name)) {
            (false, false) => ImplementRelation::Unrelated,
            (true, false) => ImplementRelation::Implements,
            (false, true) => ImplementRelation::ImplementedBy,
            (true, true) => {
                error!("Invalid internal state: cyclic ancestory around \
                        at {} and {}",
                       self.name, other.borrow().name);
                ImplementRelation::Unrelated
            }
        }
    }

    // Lookup a definiton or declaration of a member in THIS trait, does not
    // examine ancestors
    pub fn get_member<'t, T>(&'t self, member_name: &T)
                             -> Option<TraitMemberKind<'t>>
    where T: Borrow<str> + ?Sized {
        trace!("Getting member {} of trait {}",
               member_name.borrow(), self.name);
        if let Some(session) = self.sessions.get(member_name.borrow()) {
            trace!("Got as {:?}", session);
            return Some(TraitMemberKind::Session(session));
        }
        if let Some(saved) = self.saveds.get(member_name.borrow()) {
            trace!("Got as {:?}", saved);
            return Some(TraitMemberKind::Saved(saved));
        }
        if let Some(param) = self.params.get(member_name.borrow()) {
            trace!("Got as {:?}", param);
            return Some(TraitMemberKind::Parameter(param));
        }
        if let Some(method) = self.methods.get(member_name.borrow()) {
            trace!("Got as {:?}", method);
            return Some(TraitMemberKind::Method(&method.decl));
        }
        if let Some(method) = self.abstract_methods.get(member_name.borrow()) {
            trace!("Got as {:?}", method);
            return Some(TraitMemberKind::Method(method));
        }
        trace!("Didn't exist");
        None
    }

    // Lookup a definition in this trait or parents
    pub fn lookup<'t, T: Borrow<str>>(&'t self, member_name: &T)
                                      -> Option<TraitMemberKind<'t>> {
        if let Some(mem) = self.get_member(member_name.borrow()) {
            if self.ancestor_map.contains_key(member_name.borrow()) {
                error!("Invalid internal state: trait {} has both an \
                        owned and inherited definition of {}",
                       self.name,
                       member_name.borrow());
            }
            Some(mem)
        } else if let Some(mem) = self.ancestor_map.get(member_name.borrow())?
            .get_member(member_name.borrow()) {
                Some(mem)
            } else {
                error!("Invalid internal state: trait {} was indicated \
                        as defining {} by {}, but didn't",
                       self.ancestor_map.get(member_name.borrow())
                       .unwrap().name,
                       member_name.borrow(),
                       self.name);
                None
            }
    }

    pub fn process(name: &str,
                   maybeloc: Option<&ZeroSpan>,
                   spec: &Arc<ObjectSpec>,
                   traits: &HashMap<String, Arc<DMLTrait>>,
                   report: &mut Vec<DMLError>)
                   -> Arc<DMLTrait> {
        debug!("Processing trait for {}", name);
        trace!("other traits are {:?}", traits.keys().fold(
                   "".to_string(),
                   |mut s,k|{write!(s, "{}, ", k)
                             .expect("write string error");
                             s}));
        // Skip analysis for dummy trait
        if maybeloc.is_none() {
            trace!("Dummy trait, skipped");
            return Arc::new(DMLTrait {
                name: name.to_string(),
                rank: spec.rank.clone(),
                loc: None,
                parents: HashMap::default(),
                ancestors: HashMap::default(),
                sessions: HashMap::default(),
                saveds: HashMap::default(),
                params: HashMap::default(),
                abstract_methods: HashMap::default(),
                methods: HashMap::default(),
                ancestor_map: HashMap::default(),
                reserved_symbols: HashMap::default(),
            });
        }

        let loc = maybeloc.unwrap();

        // TODO: Consider warning for redundant 'is'-es here
        let parents: HashMap<String, Arc<DMLTrait>> =
            spec.instantiations.values()
            .flatten()
            .map(|t|(t.name.clone(), Arc::clone(&t.traitspec)))
            .collect();
        let ancestors: HashMap<String, Arc<DMLTrait>> = parents.values()
            .flat_map(|par|par.ancestors.iter()
                      .map(|(name, anc)|(name.clone(), Arc::clone(anc)))
                      .chain(iter::once((par.name.clone(), Arc::clone(par)))))
            .collect();

        trace!("Parents are: {}", parents.keys().fold(
            "".to_string(),
            |mut s, t|{write!(s, "{}, ", t).expect("write string error");
                       s}));
        trace!("Ancestors are: {}", ancestors.keys().fold(
            "".to_string(),
            |mut s, t|{write!(s, "{}, ", t).expect("write string error");
                       s}));

        // Check that unification of ancestors implementations are sound
        let impl_map = merge_impl_maps(name, loc, parents.values(), report);

        // Check that our implementations do not collide
        let mut used_names = HashMap::default();
        fn maybe_report_collision(used: &mut HashMap<String, ZeroSpan>,
                                  name: String,
                                  span: ZeroSpan,
                                  report: &mut Vec<DMLError>) -> bool {
            if used.contains_key(name.as_str()) {
                report.push(DMLError {
                    span,
                    description: format!("Name collision on '{}'", name),
                    severity: Some(DiagnosticSeverity::ERROR),
                    related: vec![(used[&name],
                                   "Previously defined here".into())],
                });
                false
            } else {
                used.insert(name, span);
                true
            }
        }

        fn discard_conditional<T: Clone + DeclarationSpan>(
            decl: &ObjectDecl<T>) -> Option<T> {
            if matches!(decl.cond, ExistCondition::Always) {
                Some(decl.obj.clone())
            } else {
                None
            }
        }

        trace!("Gathering members of {}", name);

        // Flatten sessions and saveds into non-colliding data declarations
        // with resolved types
        let sessions: HashMap<String, Declaration> =
            spec.sessions.iter().filter_map(discard_conditional)
            .flat_map(|sess|sess.vars)
            .map(|decl|Declaration {
                name: decl.object.name.clone(),
                type_ref: {
                    // TODO: How to actually evaluate this?
                    // Needs global scope
                    let (_, typ) = eval_type_simple(
                        &decl.typed, (), ());
                    typ.into()
                }
            }).filter(|d|maybe_report_collision(
                &mut used_names,
                d.name.val.clone(),
                d.name.span,
                report)).map(|d|(d.name.val.clone(), d)).collect();
        trace!("sessions are: {:?}", sessions);

        let saveds: HashMap<String, Declaration> =
            spec.saveds.iter().filter_map(discard_conditional)
            .flat_map(|sav|sav.vars)
            .map(|decl|Declaration {
                name: decl.object.name.clone(),
                type_ref: {
                    // TODO: How to actually evaluate this?
                    // Needs global scope
                    let (_, typ) = eval_type_simple(
                        &decl.typed, (), ());
                    typ.into()
                }
            }).filter(|d|maybe_report_collision(
                &mut used_names,
                d.name.val.clone(),
                d.name.span,
                report)).map(|d|(d.name.val.clone(), d)).collect();
        trace!("saveds are: {:?}", saveds);

        // Map typed parameters into non-colliding declarations
        let params: HashMap<String, Declaration> = spec.params.iter()
            .filter_map(discard_conditional)
            .filter(|param|param.typed.is_some())
            .map(|param|Declaration {
                name: param.object.name.clone(),
                type_ref: {
                    // TODO: How to actually evaluate this?
                    // Needs global scope
                    let (_, typ) = eval_type_simple(
                        &param.typed.unwrap(), (), ());
                    typ.into()
                }
            }).filter(|d|maybe_report_collision(
                &mut used_names,
                d.name.val.clone(),
                d.name.span,
                report)).map(|d|(d.name.val.clone(), d)).collect();
        trace!("params are: {:?}", params);

        // Map methods into non-colliding method declarations
        let abstract_methods: HashMap<String, MethodDecl> = spec.methods.iter()
            .filter_map(discard_conditional)
            .filter(|method|method.is_abstract())
            .filter_map(|m| {
                let new_m = MethodDecl::from_content(&m, report);
                if maybe_report_collision(
                    &mut used_names,
                    new_m.identity().to_string(),
                    *new_m.location(),
                    report) {
                    Some(new_m)
                } else {
                    None
                }}).map(|m|(m.name.val.clone(), m)).collect();
        trace!("abstract methods are: {:?}", abstract_methods);
        
        let mut defined_methods: HashMap<String, MethodDecl> = HashMap::default();
        for method in spec.methods.iter()
            .filter_map(discard_conditional)
            .filter(|method|!method.is_abstract()) {
            let new_m = MethodDecl::from_content(&method, report);
            // special case, methods are allowed to conflict 
            // with abstract methods
            // NOTE: Right now this means we'd report the collision
            // with non-method members from abstract decl only
            // when the definition also collides, which is probably fine
            if abstract_methods.contains_key(new_m.name.val.as_str())
            || (maybe_report_collision(
                &mut used_names,
                new_m.identity().to_string(),
                *new_m.location(),
                report)) {
                    // Need one more check here, if we have multiple
                    // non-shared decls and one or more shared decls,
                    // we'd miss the extra non-shared ones otherwise
                    if defined_methods.contains_key(new_m.identity()) {
                        report.push(DMLError {
                            span: *new_m.location(),
                            description:
                            format!("Name collision on '{}'",
                            new_m.identity()),
                            severity: Some(DiagnosticSeverity::ERROR),
                            related: vec![
                            (*defined_methods[new_m.identity()].location(),
                            "Previously defined here".to_string())],
                        });
                    } else {
                        defined_methods.insert(
                            new_m.identity().to_string(), new_m.clone());
                        }
                    }
        }
        
        trace!("defined methods are: {:?}", defined_methods);

        let mut methods: HashMap<String, DMLConcreteMethod> = HashMap::default();

        // Check that our overrides are sound and construct our concrete methods
        // NOTE: Override order is already checked in merge_impl_maps, so we can use
        // impl_map to get the next in order
        for meth in defined_methods.values().chain(abstract_methods.values()) {
            // TODO: Fairly sure this does not handle ambiguous overrides correctly.
            // Odds are: they are reported correctly from merge_impl_maps, but
            // we do not collect all of them here
            let default_call = if let Some(ancestor) = impl_map.get(&meth.name.val) {
                if let Some(coll) = ancestor.get_member(
                    meth.name.val.as_str()) {
                    match coll {
                        TraitMemberKind::Method(undermeth) => {
                            if meth.is_abstract() {
                                report.push(DMLError {
                                    span: meth.name.span,
                                    description:
                                    "Abstract method cannot \
                                     override another method".into(),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    related: vec![
                                        (*undermeth.location(),
                                         "Overridden definition here".into())],
                                });
                            }
                            if !undermeth.is_default() && !undermeth.is_abstract()  {
                                report.push(DMLError {
                                    span: meth.name.span,
                                    description:
                                    "This method attempts to override \
                                     a non-default method".into(),
                                    related: vec![
                                        (*undermeth.location(),
                                         "Attempted to override this \
                                          declaration".into())],
                                    severity: Some(DiagnosticSeverity::ERROR),
                                });
                            }
                            meth.check_override(undermeth, report);
                            ancestor.get_defined_method(meth.identity())
                                .map(|dmeth|DefaultCallReference::Valid(
                                    Arc::new(DMLMethodRef {
                                        template_ref: Some(Arc::clone(ancestor)),
                                        concrete_decl: dmeth.clone(),
                                    })))
                        },
                        TraitMemberKind::Saved(invalid) |
                        TraitMemberKind::Session(invalid) |
                        TraitMemberKind::Parameter(invalid) => {
                            report.push(DMLError {
                                span: meth.name.span,
                                description:
                                format!("Name collision on '{}'",
                                        meth.name.val.clone()),
                                severity: Some(DiagnosticSeverity::ERROR),
                                related: vec![
                                    (invalid.name.span,
                                     "Previously defined here".into())],
                            });
                            None
                        }
                    }
                } else {
                    internal_error!("trait {} was indicated \
                        as defining {} by {}, but didn't",
                        ancestor.name,
                        meth.name.val,
                        name);
                    None
                }
            } else {
                None
            };
            if !meth.is_abstract() {
                methods.insert(
                    meth.name.val.clone(),
                    DMLConcreteMethod {
                        // TODO: This clone is slightly inefficient
                        decl: meth.clone(),
                        default_call,       
                    }
                );
            }
        }

        // Check that we do not try to override non-overrideables
        for decl in params.values()
            .chain(sessions.values())
            .chain(saveds.values()) {
                if let Some(tr) = impl_map.get(&decl.name.val) {
                    report.push(DMLError {
                        span: decl.name.span,
                        description:
                        format!("Name collision on '{}'",
                                decl.name.val.clone()),
                        severity: Some(DiagnosticSeverity::ERROR),
                        related: vec![(*tr.get_member(&decl.name.val).unwrap()
                                       .location(),
                                       "Previously defined here".into())],
                    });
                }
            }

        // Reserve symbols defined in the template but not in the trait
        let reserved_symbols = spec.defined_symbols().iter()
            .filter(|decl| {
                let name = &decl.val;
                !sessions.contains_key(name) &&
                    !saveds.contains_key(name) &&
                    !params.contains_key(name) &&
                    !methods.contains_key(name)
            }).map(|decl|(decl.val.clone(), decl.span))
            .collect();

        Arc::new(DMLTrait {
            name: name.to_string(),
            rank: spec.rank.clone(),
            loc: Some(*loc),
            parents,
            ancestors,
            sessions,
            saveds,
            params,
            abstract_methods,
            methods,
            ancestor_map: impl_map,
            reserved_symbols,
        })
    }
}

// Produces a map from name to the trait in parents whose
// implementation should be used
pub fn merge_impl_maps<'t, I>(name: &str,
                              loc: &ZeroSpan,
                              parents_iter: I,
                              report: &mut Vec<DMLError>)
                              -> HashMap<String, Arc<DMLTrait>>
where
    I: IntoIterator<Item = &'t Arc<DMLTrait>>
{
    trace!("Merging implementation maps for {} at {:?}", name, loc);
    let parents: Vec<&'t Arc<DMLTrait>> = parents_iter.into_iter().collect();
    trace!("Parents are {}", parents.iter().fold(
        "".to_string(),
        |mut s, t|{write!(s, "{}, ", t.name).expect("write string error");
                   s}));

    let mut map: HashMap<String, Arc<DMLTrait>> = HashMap::default();
    // Maps name to traits from which we have ambiguous
    // implementations of a declaration
    let mut conflicting_defs: HashMap::<String, Vec<Arc<DMLTrait>>>
        = HashMap::default();
    let mut ambiguous_defs: HashMap::<String, Vec<Arc<DMLTrait>>>
        = HashMap::default();
    for parent in parents {
        trace!("Handling parent {}", parent.name);
        trace!("Current map {:?}", map);
        for (name, implr) in get_impls(parent) {
            let decl = implr.get_member(&name).unwrap();
            if let Some(mimplr) = map.remove(&name) {
                let relation = implr.implement_relation(&mimplr);
                if relation == ImplementRelation::Same {
                    map.insert(name, mimplr);
                    continue;
                }
                let edecl = mimplr.get_member(&name).unwrap();
                // Methods can override each other on trait level,
                // other declarations cannot
                match (decl, edecl) {
                    (TraitMemberKind::Method(m1), TraitMemberKind::Method(m2))
                        => match relation {
                            ImplementRelation::Implements => {
                                map.insert(name, implr);
                            },
                            ImplementRelation::ImplementedBy => {
                                map.insert(name, mimplr);
                            },
                            // TODO/NOTE: This methodology means we pick
                            // whichever def is found first for further
                            // analysis.
                            // A minor improvement would be to pick both
                            ImplementRelation::Unrelated => {
                                // This conflict is allowed as long as exactly 1
                                // is abstract
                                if m1.is_abstract() && !m2.is_abstract() {
                                    map.insert(name, mimplr);
                                    
                                } else if !m1.is_abstract() && m2.is_abstract() {
                                    map.insert(name, implr);
                                } else {
                                    // Both abstract or both non-abstract
                                    // ambiguous
                                    map.insert(name.clone(), Arc::clone(&implr));
                                    ambiguous_defs.entry(name)
                                        .or_default()
                                        .push(mimplr);
                                }
                            },
                            _ => unreachable!("should be covered by if clause"),
                        },
                    _ => {
                        if let Some(confldef) =
                            conflicting_defs.get_mut(&name) {
                                confldef.push(implr);
                            }
                        else {
                            conflicting_defs.insert(name, vec![mimplr, implr]);
                        }
                    },
                }
            } else{
                trace!("No conflict");
                map.insert(name, implr);
            };
        }
    }
    trace!("Ambiguous defs {:?}", ambiguous_defs.keys());
    for (ambiguous_name, traits) in &ambiguous_defs {
        // TODO: in some cases we can here report an obvious suggestion
        // example: templ A contains a default decl and templ B contains
        // a non-default one, we can then suggest B to implement A
        report.push(DMLError {
            span: *loc,
            // TODO: This can be improved. Realistically it is only
            // an uncertain order if either method is default
            description: format!(
                "Conflicting definitions of method '{}' in {}. \
                 This might be due to uncertain inheritance order.",
                ambiguous_name, name),
            related: traits.iter().map(
                |t|{
                    let decl = t.get_method(ambiguous_name).unwrap();
                    (*decl.location(),
                     format!("Conflicting definition here in {}",
                             t.name))
                }).collect(),
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }
    trace!("Conflicting defs {:?}", conflicting_defs);
    for (conflict_name, traits) in &conflicting_defs {
        // TODO: in some cases we can here report an obvious suggestion
        // example: templ A contains a default decl and templ B contains
        // a non-default one, we can then suggest B to implement A
        report.push(DMLError {
            span: *loc,
            description: format!(
                "Conflicting definitions of '{}' in {}.",
                conflict_name, name),
            related: traits.iter().map(|t|{
                let decl = t.get_member(conflict_name).unwrap();
                (*decl.location(),
                 format!("Conflicting definition here in {}",
                         t.name))
            }).collect(),
            severity: Some(DiagnosticSeverity::ERROR),
        });
    }
    trace!("Resulting map is {:?}", map);
    map
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DMLTemplate {
    pub name: String,
    pub location: Option<ZeroSpan>,
    pub spec: Arc<ObjectSpec>,
    pub traitspec: Arc<DMLTrait>,
}

impl Spec for DMLTemplate {
    fn get_rank(&self) -> &Rank {
        self.spec.get_rank()
    }
    fn defined_symbols(&self) -> HashSet<&DMLString> {
        self.spec.defined_symbols()
    }
}

impl DMLTemplate {
    pub fn make(name: &str,
                location: Option<&ZeroSpan>,
                spec: Arc<ObjectSpec>,
                traitspec: Arc<DMLTrait>) -> Arc<DMLTemplate> {
        DMLTemplate {
            name: name.to_string(),
            location: location.cloned(),
            spec,
            traitspec,
        }.into()
    }
}

#[derive(Default, Debug, Clone)]
pub struct TemplateTraitInfo {
    pub templates: HashMap<String, Arc<DMLTemplate>>,
    pub traits: HashMap<String, Arc<DMLTrait>>,
}

impl TemplateTraitInfo {
    pub fn new() -> TemplateTraitInfo {
        TemplateTraitInfo  {
            templates: HashMap::new(),
            traits: HashMap::new(),
        }
    }

    pub fn get_template<'t>(&'t self, name: &str) -> Option<(&'t DMLTemplate,
                                                             &'t DMLTrait)> {
        let templ = self.templates.get(name)?;
        Some((templ.as_ref(), templ.traitspec.as_ref()))
    }
}

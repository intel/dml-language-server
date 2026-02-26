//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::{DeclarationSpan, LocationSpan, Named,
                      ZeroPosition, ZeroFilePosition, ZeroSpan};
use crate::analysis::symbols::{DMLSymbolKind, SimpleSymbol,
                               StructureSymbol};
use crate::analysis::reference::{Reference};

use crate::analysis::structure::objects::Method;

use crate::logging::{debug, trace};

pub trait Scope : DeclarationSpan + Named {
    // Need this to create default mappings
    fn as_method(&self) -> Option<Method> { None }
    fn create_context(&self) -> ContextKey;
    // NOTE: the defined scopes, and symbols here are _similar_
    // to the container traits, but not the same. This is because
    // the things scopes defined are not to be flattened to outside
    // the scope through collections
    fn defined_scopes(&self) -> Vec<&dyn Scope>;
    fn defined_symbols(&self) -> Vec<&dyn StructureSymbol>;
    fn defined_references(&self) -> &Vec<Reference>;

    fn reference_at_pos_inside(&self, pos: ZeroPosition) -> Option<&Reference> {
        self.defined_references().iter()
            .find(|r|{trace!("Considering {:?}", r);
                      r.loc_span().range.contains_pos(pos)})
    }

    fn reference_at_pos(&self, pos: &ZeroFilePosition) -> Option<&Reference> {
        trace!("Searching for references inside {:?}", self.create_context());
        if let Some(our_ref) = self.reference_at_pos_inside(pos.position) {
            trace!("Got {:?}", our_ref);
            Some(our_ref)
        } else {
            trace!("Recursed");
            for scope in &self.defined_scopes() {
                if scope.span().contains_pos(pos) {
                    return scope.reference_at_pos(pos);
                }
            }
            None
        }
    }

    fn to_context(&self) -> SymbolContext {
        let mut subsymbols: Vec<SubSymbol> = self.defined_symbols().into_iter()
            .map(|sym| {
                let kind = sym.kind();
                SubSymbol::Simple(SimpleSymbol::make(sym, kind))
            })
            .collect();
        subsymbols.extend(self.defined_scopes().into_iter().map(
            |scope|SubSymbol::Context(Box::new(scope.to_context()))));
        SymbolContext {
            context: self.create_context(),
            span: *self.span(),
            subsymbols,
        }
    }
}

// We use two traits here, so that we can implement on both T: Scope
// and T: ScopeContainer
pub trait ScopeContainer {
    fn scopes(&self) -> Vec<&dyn Scope>;
}

pub trait MakeScopeContainer {
    fn to_scopes(&self) -> Vec<&dyn Scope>;
}

impl <T: Scope> MakeScopeContainer for Vec<T> {
    fn to_scopes(&self) -> Vec<&dyn Scope> {
        self.iter().map(|s|s as &dyn Scope).collect()
    }
}

impl <T: Scope> MakeScopeContainer for Option<T> {
    fn to_scopes(&self) -> Vec<&dyn Scope> {
        self.iter().map(|s|s as &dyn Scope).collect()
    }
}

impl <T: ScopeContainer> ScopeContainer for Vec<T> {
    fn scopes(&self) -> Vec<&dyn Scope> {
        self.iter().flat_map(|s|s.scopes().into_iter()).collect()
    }
}

impl <T: ScopeContainer> ScopeContainer for Option<T> {
    fn scopes(&self) -> Vec<&dyn Scope> {
        self.iter().flat_map(|s|s.scopes().into_iter()).collect()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ContextKey {
    Structure(SimpleSymbol),
    Method(SimpleSymbol),
    Template(SimpleSymbol),
    AllWithTemplate(ZeroSpan, Vec<String>),
}

impl ContextKey {
    pub fn kind(&self) -> Option<DMLSymbolKind> {
        match self {
            Self::Structure(s) => Some(s.kind),
            Self::Method(s) => Some(s.kind),
            Self::Template(s) => Some(s.kind),
            Self::AllWithTemplate(_, _) => None,
        }
    }
}

impl Named for ContextKey {
    fn get_name(&self) -> String {
        match self {
            ContextKey::Template(sym) |
            ContextKey::Method(sym) |
            ContextKey::Structure(sym) => sym.get_name(),
            ContextKey::AllWithTemplate(_, templs) => format!(
                "InEach ({})", templs.join(", ")),
        }
    }
}

impl LocationSpan for ContextKey {
    fn loc_span(&self) -> &ZeroSpan {
        match self {
            ContextKey::Template(sym) |
            ContextKey::Method(sym) |
            ContextKey::Structure(sym) => sym.loc_span(),
            ContextKey::AllWithTemplate(span, _) => span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum SubSymbol {
    Context(Box<SymbolContext>),
    Simple(SimpleSymbol),
}

impl LocationSpan for SubSymbol {
    fn loc_span(&self) -> &ZeroSpan {
        match self {
            SubSymbol::Context(sub) => sub.loc_span(),
            SubSymbol::Simple(simple) => simple.loc_span(),
        }
    }
}

impl SubSymbol {
    fn contains_pos(&self, pos: &ZeroFilePosition) -> bool {
        match self {
            SubSymbol::Context(context) => context.span().contains_pos(pos),
            SubSymbol::Simple(simple) => simple.loc_span().contains_pos(pos),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolContext {
    pub context: ContextKey,
    pub span: ZeroSpan,
    pub subsymbols: Vec<SubSymbol>,
}

impl Named for SymbolContext {
    fn get_name(&self) -> String {
        self.context.get_name()
    }
}

impl DeclarationSpan for SymbolContext {
    fn span(&self) -> &ZeroSpan {
        &self.span
    }
}

impl LocationSpan for SymbolContext {
    fn loc_span(&self) -> &ZeroSpan {
        self.context.loc_span()
    }
}

#[derive(Debug, Clone)]
pub struct ContextedSymbol<'t> {
    pub symbol: &'t SimpleSymbol,
    pub contexts: Vec<&'t ContextKey>,
}

impl <'t> Named for ContextedSymbol<'t> {
    fn get_name(&self) -> String {
        self.symbol.get_name()
    }
}

impl <'t> LocationSpan for ContextedSymbol<'t> {
    fn loc_span(&self) -> &'t ZeroSpan {
        self.symbol.loc_span()
    }
}

impl <'t> StructureSymbol for ContextedSymbol<'t> {
    fn kind(&self) -> DMLSymbolKind {
        self.symbol.kind()
    }
}

impl <'t> ContextedSymbol<'t> {
    pub fn remove_head_context(&mut self) {
        self.contexts.remove(0);
    }
}

impl SymbolContext {
    pub fn lookup_symbol<'t>(&'t self, pos: &ZeroFilePosition)
                             -> Option<ContextedSymbol<'t>> {
        debug!("Starting file-position lookup in {:?}", self.get_name());
        let mut acc = vec![];
        self.lookup_symbol_aux(pos, &mut acc).map(
            |sub|ContextedSymbol {
                symbol: sub,
                contexts: acc,
            })
    }

    fn lookup_symbol_aux<'t>(&'t self,
                             pos: &ZeroFilePosition,
                             acc: &mut Vec<&'t ContextKey>)
                             -> Option<&'t SimpleSymbol> {
        acc.push(&self.context);
        self.subsymbols.iter()
            .find(|sub|{trace!("Considering {:?}, contains pos? {:?}",
                               sub,
                               sub.contains_pos(pos));
                        sub.contains_pos(pos)})
            .and_then(|sub|match sub {
                SubSymbol::Simple(simp) => Some(simp),
                SubSymbol::Context(sub) => sub.lookup_symbol_aux(pos, acc),
            })
    }
}

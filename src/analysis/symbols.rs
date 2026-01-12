//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::sync::Arc;

use std::collections::HashSet;
use std::sync::Mutex;

use crate::analysis::{Named, LocationSpan};

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::objects::{CompObjectKind};

use crate::analysis::structure::expressions::DMLString;
use crate::analysis::templating::objects::{DMLObject, DMLNamedMember, StructureKey};
use crate::analysis::templating::methods::{DMLMethodRef};
use crate::analysis::templating::types::DMLResolvedType;
use crate::analysis::templating::traits::DMLTemplate;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum DMLSymbolKind {
    CompObject(CompObjectKind),
    Parameter,
    Constant,
    Extern,
    Hook,
    Local,
    Loggroup,
    Method,
    MethodArg,
    Saved,
    Session,
    Template,
    Typedef,
}

pub trait StructureSymbol : Named + LocationSpan {
    fn kind(&self) -> DMLSymbolKind;
}

pub trait SymbolContainer {
    fn symbols(&self) -> Vec<&dyn StructureSymbol>;
}

pub trait MakeSymbolContainer {
    fn to_symbols(&self) -> Vec<&dyn StructureSymbol>;
}

impl <T: StructureSymbol> MakeSymbolContainer for Vec<T> {
    fn to_symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.iter().map(|s|s as &dyn StructureSymbol).collect()
    }
}

impl <T: SymbolContainer> SymbolContainer for Vec<T> {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.iter().flat_map(|s|s.symbols().into_iter()).collect()
    }
}

impl <T: SymbolContainer> SymbolContainer for Option<T> {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.iter().flat_map(|s|s.symbols().into_iter()).collect()
    }
}

impl <T: SymbolContainer> SymbolContainer for Box<T> {
    fn symbols(&self) -> Vec<&dyn StructureSymbol> {
        self.as_ref().symbols()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SimpleSymbol {
    pub name: String,
    pub loc: ZeroSpan,
    pub kind: DMLSymbolKind,
}

impl Named for SimpleSymbol {
    fn get_name(&self) -> String {
        self.name.clone()
    }
}

impl LocationSpan for SimpleSymbol {
    fn loc_span(&self) -> &ZeroSpan {
        &self.loc
    }
}

impl StructureSymbol for SimpleSymbol {
    fn kind(&self) -> DMLSymbolKind {
        self.kind
    }
}

impl SimpleSymbol {
    pub fn make<T: Named + LocationSpan + ?Sized>(
        source: &T, kind: DMLSymbolKind)
        -> SimpleSymbol {
        SimpleSymbol {
            name: source.get_name(),
            loc: *source.loc_span(),
            kind,
        }
    }
    pub fn name_ref(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolSource {
    DMLObject(DMLObject),
    // (key of containing object, method ref)
    // TODO: same principle for shared methods? likely not, there is
    // no object for the topmost method
    Method(StructureKey, Arc<DMLMethodRef>),
    MethodArg(Arc<DMLMethodRef>, DMLString),
    MethodLocal(Arc<DMLMethodRef>, DMLString),
    // TODO: RC this if it's expensive
    Type(DMLResolvedType),
    Template(Arc<DMLTemplate>),
}

impl SymbolSource {
    pub fn as_object(&self) -> Option<&DMLObject> {
        match self {
            Self::DMLObject(obj) => Some(obj),
            _ => None
        }
    }
    pub fn as_metharg(&self) -> Option<(&Arc<DMLMethodRef>, &DMLString)> {
        match self {
            Self::MethodArg(arg, name) => Some((arg, name)),
            _ => None
        }
    }
    pub fn as_type(&self) -> Option<&DMLResolvedType> {
        match self {
            Self::Type(typ) => Some(typ),
            _ => None
        }
    }
    pub fn as_template(&self) -> Option<&Arc<DMLTemplate>> {
        match self {
            Self::Template(templ) => Some(templ),
            _ => None
        }
    }
    // NOTE: Used during symbol tracking to discard duplicate
    // symbols
    pub fn equivalent(&self, other: &SymbolSource) -> bool {
        match (self, other) {
            (SymbolSource::DMLObject(obj1),
             SymbolSource::DMLObject(obj2)) => obj1.equivalent(obj2),
            (_, _) => self == other
        }
    }

    pub fn maybe_name(&self) -> Option<&str> {
        match self {
            SymbolSource::DMLObject(obj) => obj.as_shallow().map(|o| o.identity()),
            SymbolSource::Method(_, methref) => Some(methref.identity()),
            SymbolSource::MethodArg(_, name) => Some(&name.val),
            SymbolSource::MethodLocal(_, name) => Some(&name.val),
            SymbolSource::Type(_) => None,
            SymbolSource::Template(templ) => Some(&templ.name),
        }
    }
}

// Intentionally omitted implementation of clone, should not be done
// Instead, clone the symbolref
#[derive(Debug)]
pub struct Symbol {
    pub id: SymbolID,
    pub loc: ZeroSpan,
    pub kind: DMLSymbolKind,
    // Bases are the topmost declarations of this symbol
    pub bases: Vec<ZeroSpan>,
    // Definitions are the sites which define at least part of this symbol
    pub definitions: Vec<ZeroSpan>,
    // Declarations are the sites which declare this symbol
    pub declarations: Vec<ZeroSpan>,
    // References are all locations that may refer to this symbol
    pub references: HashSet<ZeroSpan>,
    // NOTE: The meaning of 'implementation' varies with symbol kind
    // - For interfaces this is straightforward
    // - For templates it will give all direct instantiations of the template
    // - For methods, it gives all methods that directly override this method,
    //   note that when presented to the user, we give both direct and indirect overrides
    // TODO: For objects, we could give the locations of in-eachs that affect it
    // there is no way to go to these specs without finding definitions of
    // some param or method set in them
    pub implementations: HashSet<ZeroSpan>,
    pub source: SymbolSource,
    // TODO: RC or box this if it's expensive
    pub typed: Option<DMLResolvedType>,
}

// We do this often enough to warrant a reusable pattern,
// slightly better as a macro due to variable arguments
macro_rules! symbol_ref {
    ($maker: expr, $loc: expr, $kind: expr, $source: expr,
     $($set: ident = $to: expr),*) => {
        {
            let symbol = $maker.new_symbol($loc, $kind, $source);
            {
                let mut symbol_lock = symbol.lock().unwrap();
                $(symbol_lock.$set = $to;)*
            }
            symbol
        }
    }
}

impl Symbol {
    // NOTE: Used during symbol tracking to discard duplicate
    // symbols
    pub fn equivalent(&self, other: &Symbol) -> bool {
        self.loc == other.loc
            && self.kind == other.kind
            && self.definitions == other.definitions
            && self.declarations == other.declarations
            && self.source.equivalent(&other.source)
    }
}

pub type SymbolID = u64;

#[derive(Debug, Default)]
pub struct SymbolMaker {
    next_id: std::sync::atomic::AtomicU64,
}

impl SymbolMaker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_symbol(
        &self,
        loc: ZeroSpan,
        kind: DMLSymbolKind,
        source: SymbolSource) -> SymbolRef {
        let next_id = self.next_id.fetch_add(
            1, std::sync::atomic::Ordering::Relaxed);
        SymbolRefInner::new_ref(next_id, loc, kind, source)
    }
}

#[derive(Debug)]
pub struct SymbolRefInner {
    pub id: SymbolID,
    pub symbol: Mutex<Symbol>,
}

pub type SymbolRef = Arc<SymbolRefInner>;

impl SymbolRefInner {
    pub fn new_ref(id: SymbolID,
                   loc: ZeroSpan,
                   kind: DMLSymbolKind,
                   source: SymbolSource) -> SymbolRef {
        Arc::new(SymbolRefInner {
            id,
            symbol: Mutex::new(Symbol::new(id, loc, kind, source))
        })
    }
}

impl Eq for SymbolRefInner {}
impl PartialEq for SymbolRefInner {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::hash::Hash for SymbolRefInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl SymbolRefInner {
    pub fn lock(&self)
                -> std::sync::LockResult<std::sync::MutexGuard<'_, Symbol>> {
        self.symbol.lock()
    }
}

impl Symbol {
    pub fn new(id: SymbolID,
               loc: ZeroSpan,
               kind: DMLSymbolKind,
               source: SymbolSource) -> Self {
        Symbol {
            id,
            loc,
            kind,
            source,
            definitions: Vec::default(),
            declarations: Vec::default(),
            references: HashSet::default(),
            implementations: HashSet::default(),
            bases: Vec::default(),
            typed: None,
        }
    }

    // Prints appropriate info about a method without clogging the output
    // with minutiae for symbols that may contain lots of sub-data (methods)
    pub fn medium_debug_info(&self) -> String {
        format!(
            "Symbol({}, {:?} at loc: {:?})",
            self.source.maybe_name()
                .map_or(format!("id={}", self.id),
                        |name| format!("{} (id={})", name, self.id)),
            self.kind,
            self.loc)
    }
}

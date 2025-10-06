//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use std::sync::Arc;

use std::collections::{HashMap, HashSet};

use crate::analysis::{Named, LocationSpan};

use crate::analysis::parsing::tree::ZeroSpan;
use crate::analysis::structure::objects::{CompObjectKind};

use crate::analysis::structure::expressions::DMLString;
use crate::analysis::templating::objects::DMLObject;
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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub loc: ZeroSpan,
    pub kind: DMLSymbolKind,
    pub definitions: Vec<ZeroSpan>,
    pub declarations: Vec<ZeroSpan>,
    pub references: HashSet<ZeroSpan>,
    // NOTE: The meaning of 'implementation' varies with symbol kind
    // For methods and interfaces, this is straightforward
    // For templates, it will give all declarations for all objects that
    // directly or indirectly implement the template
    // TODO: For objects, we could give the locations of in-eachs that affect it
    // there is no way to go to these specs without finding definitions of
    // some param or method set in them
    pub implementations: Vec<ZeroSpan>,
    pub bases: Vec<ZeroSpan>,
    pub source: SymbolSource,
    // Used for method symbols only, maps default references
    // to the method_decl they should resolve to
    pub default_mappings: HashMap<ZeroSpan, ZeroSpan>,
    // TODO: RC or box this if it's expensive
    pub typed: Option<DMLResolvedType>,
}

impl Symbol {
    pub fn new(loc: ZeroSpan,
               kind: DMLSymbolKind,
               source: SymbolSource) -> Self {
        Symbol {
            loc,
            kind,
            source,
            definitions: Vec::default(),
            declarations: Vec::default(),
            references: HashSet::default(),
            implementations: Vec::default(),
            bases: Vec::default(),
            default_mappings: HashMap::default(),
            typed: None,
        }
    }
}

// We do this often enough to warrant a reusable pattern,
// slightly better as a macro due to variable arguments
macro_rules! symbol_ref {
    ($loc: expr, $kind: expr, $source: expr, $($set: ident = $to: expr),*) => {
        {
            let mut symbol = Symbol::new($loc, $kind, $source);
            $(symbol.$set = $to;)*
            Arc::new(Mutex::new(symbol))
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

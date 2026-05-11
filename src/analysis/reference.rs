//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::{DeclarationSpan, LocationSpan, DMLNamed, ZeroSpan};
use crate::analysis::parsing::tree::{LeafToken, TreeElement};
use crate::analysis::structure::expressions::{DMLString};
use crate::analysis::FileSpec;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeRef {
    Simple(DMLString),
    //// Ignore index here
    Sub(Box<NodeRef>, DMLString, ZeroSpan),
}

impl std::fmt::Display for NodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)
           -> Result<(), std::fmt::Error> {
        match self {
            NodeRef::Simple(name) => name.val.fmt(f),
            NodeRef::Sub(sub, name, _) => write!(f,
                                                 "{}.{}",
                                                 sub,
                                                 &name.val),
        }
    }
}

impl DMLNamed for NodeRef {
    fn name(&self) -> &DMLString {
        match self {
            Self::Simple(simple) => simple,
            Self::Sub(_, sub, _) => sub,
        }
    }
}
impl DeclarationSpan for NodeRef {
    fn span(&self) -> &ZeroSpan {
        match self {
            Self::Simple(simple) => &simple.span,
            Self::Sub(_, _, span) => span,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableReference {
    pub reference: NodeRef,
    pub kind: ReferenceKind,
}

impl std::fmt::Display for VariableReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)
           -> Result<(), std::fmt::Error> {
        self.reference.fmt(f)
    }
}

impl DeclarationSpan for VariableReference {
    fn span(&self) -> &ZeroSpan {
        self.reference.span()
    }
}

impl LocationSpan for VariableReference {
    fn loc_span(&self) -> &ZeroSpan {
        self.reference.loc_span()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalReference {
    pub name: String,
    pub loc: ZeroSpan,
    pub kind: ReferenceKind,
}

impl std::fmt::Display for GlobalReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.name.fmt(f)
    }
}

// NOTE: For global references, decl and loc spans are the same
// since they are just one name
impl LocationSpan for GlobalReference {
    fn loc_span(&self) -> &ZeroSpan {
        &self.loc
    }
}
impl DeclarationSpan for GlobalReference {
    fn span(&self) -> &ZeroSpan {
        &self.loc
    }
}

#[derive(Copy, Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReferenceKind {
    Template,
    Type,
    Variable,
    Callable,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReferenceVariant {
    Variable(VariableReference),
    Global(GlobalReference),
}

// NOTE: Since this information is likely to be sparse, use
// boxes for any non-trivially small fields. If fields get to be more
// than a few, the entire info should probably be boxed instead
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReferenceInfo {
    pub was_instantiation: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CodeReference {
    pub variant: ReferenceVariant,
    pub extra_info: ReferenceInfo,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reference {
    CodeReference(CodeReference),
    FileReference(DMLString),
}

impl Reference {
    pub fn file_ref_from_token<'a>(token: &LeafToken, file: FileSpec<'a>) -> Option<Reference> {
        DMLString::from_token(token, file).map(Reference::FileReference)
    }
}

impl LocationSpan for Reference {
    fn loc_span(&self) -> &ZeroSpan {
        match self {
            Reference::CodeReference(code_ref) => code_ref.loc_span(),
            Reference::FileReference(import) => &import.span,
        }
    }
}
impl DeclarationSpan for Reference {
    fn span(&self) -> &ZeroSpan {
        match self {
            Reference::CodeReference(code_ref) => code_ref.span(),
            Reference::FileReference(import) => &import.span,
        }
    }
}

impl Reference  {
    pub fn as_code_ref(&self) -> Option<&CodeReference> {
        match self {
            Reference::CodeReference(code_ref) => Some(code_ref),
            _ => None,
        }
    }
    pub fn as_file_ref(&self) -> Option<&DMLString> {
        match self {
            Reference::FileReference(file_ref) => Some(file_ref),
            _ => None,
        }
    }
}

impl From<CodeReference> for Reference {
    fn from(code_ref: CodeReference) -> Self {
        Reference::CodeReference(code_ref)
    }
}

// NOTE: The locationspan of a reference is the actual source range its
// considered to be selectable at
// For example:
// The full noderef:
// a.f[3].r
// would have the locationspan covering 'r'
// and the declarationspan covering the full reference
impl LocationSpan for CodeReference {
    fn loc_span(&self) -> &ZeroSpan {
        match &self.variant {
            ReferenceVariant::Variable(var) => var.loc_span(),
            ReferenceVariant::Global(glob) => glob.loc_span(),
        }
    }
}
impl DeclarationSpan for CodeReference {
    fn span(&self) -> &ZeroSpan {
        match &self.variant {
            ReferenceVariant::Variable(var) => var.span(),
            ReferenceVariant::Global(glob) => glob.span(),
        }
    }
}

impl std::fmt::Display for CodeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>)
           -> Result<(), std::fmt::Error> {
        match &self.variant {
            ReferenceVariant::Variable(var) => var.fmt(f),
            ReferenceVariant::Global(glob) => glob.fmt(f),
        }
    }
}

impl CodeReference {
    pub fn as_variable_ref(&self) -> Option<&VariableReference> {
        match &self.variant {
            ReferenceVariant::Variable(var) => Some(var),
            _ => None,
        }
    }

    pub fn from_noderef(node: NodeRef, kind: ReferenceKind) -> CodeReference {
        CodeReference {
            variant: ReferenceVariant::Variable(VariableReference {
                reference: node,
                kind,
            }),
            extra_info: ReferenceInfo::default(),
        }
    }

    pub fn global_from_string(name: String,
                              loc: ZeroSpan,
                              kind: ReferenceKind) -> CodeReference {
        CodeReference {
            variant: ReferenceVariant::Global(GlobalReference {
                name,
                loc,
                kind,
            }),
            extra_info: ReferenceInfo::default(),
        }
    }
    pub fn global_from_token<'a>(token: &LeafToken,
                                 file: FileSpec<'a>,
                                 kind: ReferenceKind) -> Option<CodeReference> {
        token.read_leaf(file.file).map(
            |string|CodeReference::global_from_string(
                string,
                ZeroSpan::from_range(token.range(),
                                     file.path),
                kind))
    }
    pub fn reference_kind(&self) -> ReferenceKind {
        match &self.variant {
            ReferenceVariant::Variable(r) => r.kind,
            ReferenceVariant::Global(r) => r.kind,
        }
    }
}

pub trait ReferenceContainer {
    fn collect_references<'a>(&self,
                              accumulator: &mut Vec<Reference>,
                              file: FileSpec<'a>);
}

pub trait MaybeIsNodeRef {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef>;
}

impl <T: MaybeIsNodeRef> MaybeIsNodeRef for Option<T> {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        self.as_ref().and_then(|inner|inner.maybe_noderef(file))
    }
}

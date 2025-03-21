//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use crate::analysis::parsing::parser::Token;
use crate::analysis::{LocalDMLError, make_error_from_missing_content};
use crate::analysis::reference::{Reference, NodeRef, MaybeIsNodeRef,
                                 ReferenceContainer};
use crate::analysis::FileSpec;
use crate::analysis::structure::expressions::DMLString;

use crate::lint::{rules::CurrentRules, AuxParams, DMLStyleError};
use crate::span::{Range, Span, ZeroIndexed, Position, FilePosition};
use crate::vfs::{Vfs as GenVfs, TextFile};

pub type ZeroRange = Range::<ZeroIndexed>;
pub type ZeroSpan = Span::<ZeroIndexed>;
pub type ZeroPosition = Position::<ZeroIndexed>;
pub type ZeroFilePosition = FilePosition::<ZeroIndexed>;

// Marker trait, let's us implement things on things containing
// treeelements without specifying the long list of trai
pub trait TreeElementMember
    : TreeElement + ReferenceContainer + std::fmt::Debug {}
impl <T: TreeElement + ReferenceContainer + std::fmt::Debug>
    TreeElementMember for T {}

pub type TreeElements<'t> = Vec<&'t dyn TreeElementMember>;
// TODO: Might be worth moving this specialization somewhere else
pub type Vfs = GenVfs<()>;

// TODO: Consider moving extraction of range into its own trait
// - one for &ZeroRange and one for ZeroRange
pub trait TreeElement {
    fn range(&self) -> ZeroRange;
    // True if position is within range()
    fn contains_pos(&self, pos: ZeroPosition) -> bool {
        self.range().contains_pos(pos)
    }
    fn get_leaf(&self, pos: ZeroPosition) -> Option<LeafToken> {
        if !self.range().contains_pos(pos) {
            None
        } else {
            for sub in self.subs() {
                let sub_lookup = sub.get_leaf(pos);
                if sub_lookup.is_some() {
                    return sub_lookup
                }
            };
            None
        }
    }
    fn subs<'a>(&'a self) -> TreeElements<'a>;

    fn post_parse_sanity_walk(&self,
                              collect: &mut Vec<LocalDMLError>,
                              file: &TextFile) {
        collect.append(&mut self.post_parse_sanity(file));
        for sub in self.subs() {
            sub.post_parse_sanity_walk(collect, file);
        }
    }
    fn post_parse_sanity(&self, _file: &TextFile) -> Vec<LocalDMLError> {
        // Default behaviour is noop
        vec![]
    }

    // This is the default fallback for all 'content' types. Meaning if we get
    // here we are not missing (but our subs might be)
    fn report_missing(&self) -> Vec<LocalDMLError> {
        let mut accumulate = vec![];
        // By default, report only the _first_ missing sub
        for sub in self.subs() {
            accumulate.append(&mut sub.report_missing());
        }
        accumulate
    }

    // NOTE: The way this works is: references will automatically collect all
    // references throughout subs(), so at points where scopes are defined and
    // we do not want references to be collect we will re-define this to no-op
    // NOTE: default_references ends up being useful in cases where we want to
    // inject noderefs as references
    fn default_references<'a>(&self,
                              accumulator: &mut Vec<Reference>,
                              file: FileSpec<'a>) {
        self.subs().into_iter()
            .for_each(|s|s.collect_references(accumulator, file));
    }
    fn references<'a>(&self,
                      accumulator: &mut Vec<Reference>,
                      file: FileSpec<'a>) {
        self.default_references(accumulator, file);
    }

    fn style_check(&self, acc: &mut Vec<DMLStyleError>, rules: &CurrentRules, mut aux: AuxParams) {
        self.evaluate_rules(acc, rules, &mut aux);
        for sub in self.subs() {
            sub.style_check(acc, rules, aux);
        }
    }
    fn evaluate_rules(&self, _acc: &mut Vec<DMLStyleError>, _rules: &CurrentRules, _aux: &mut AuxParams) {} // default NOOP
}

impl <T: ?Sized + TreeElement> ReferenceContainer for T {
    fn collect_references<'a>(&self,
                              accumulator: &mut Vec<Reference>,
                              file: FileSpec<'a>) {
        self.references(accumulator, file);
    }
}

macro_rules! create_subs {
    ( ) => { vec![] };
    ( $lead:expr $(, $sub_list: expr)* ) => {
        vec![$lead $(, $sub_list)*]
    };
}

impl <T: TreeElementMember> TreeElement for Vec<T> {
    fn range(&self) -> ZeroRange {
        if self.is_empty() {
            Range::invalid()
        } else {
            Range::combine(self.first().unwrap().range(),
                           self.last().unwrap().range())
        }
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        self.iter().map(|e|e as &dyn TreeElementMember).collect()
    }
}

impl TreeElement for Box<dyn TreeElementMember> {
    fn range(&self) -> ZeroRange {
        self.as_ref().range()
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        self.as_ref().subs()
    }
}

impl <T: TreeElementMember + Clone> TreeElement for Option<T> {
    fn range(&self) -> ZeroRange {
        if let Some(e) = self {
            e.range()
        } else {
            ZeroRange::invalid()
        }

    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        match self {
            Some(inner) => vec![inner],
            _ => vec![]
        }
    }
}

// Its not feasible to automate this, even with macros.
// Add more tuple lengths as needed
impl <T: TreeElementMember + Clone + 'static> TreeElement for (T,) {
    fn range(&self) -> ZeroRange {
        self.0.range()
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        create_subs!(&self.0)
    }
}

impl <A: TreeElementMember + Clone + 'static,
      B: TreeElementMember + Clone + 'static> TreeElement for (A, B) {
    fn range(&self) -> ZeroRange {
        Range::combine(self.0.range(), self.1.range())
    }
    fn subs<'a>(&self) -> TreeElements<'_> {
        create_subs!(&self.0, &self.1)
    }
}

impl <A: TreeElementMember + Clone + 'static,
      B: TreeElementMember + Clone + 'static,
      C: TreeElementMember + Clone + 'static> TreeElement for (A, B, C) {
    fn range(&self) -> ZeroRange {
        Range::combine(self.0.range(), self.2.range())
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        create_subs!(&self.0, &self.1,  &self.2)
    }
}

impl <A: TreeElementMember + Clone + 'static,
      B: TreeElementMember + Clone + 'static,
      C: TreeElementMember + Clone + 'static,
      D: TreeElementMember + Clone + 'static> TreeElement for (A, B, C, D) {
    fn range(&self) -> ZeroRange {
        Range::combine(self.0.range(), self.3.range())
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        create_subs!(&self.0, &self.1,  &self.2, &self.3)
    }
}

impl <A: TreeElementMember + Clone + 'static,
      B: TreeElementMember + Clone + 'static,
      C: TreeElementMember + Clone + 'static,
      D: TreeElementMember + Clone + 'static,
      E: TreeElementMember + Clone + 'static> TreeElement for (A, B, C, D, E) {
    fn range(&self) -> ZeroRange {
        Range::combine(self.0.range(), self.4.range())
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        create_subs!(&self.0, &self.1,  &self.2, &self.3, &self.4)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct MissingToken {
    pub position: Position<ZeroIndexed>,
    // Describes the missing token
    pub description: &'static str,
    pub ended_by: Option<Token>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LeafToken {
    Actual(Token),
    Missing(MissingToken),
}

impl MaybeIsNodeRef for LeafToken {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        DMLString::from_token(self, file).map(|dmlstr|NodeRef::Simple(dmlstr))
    }
}

impl TreeElement for LeafToken {
    fn range(&self) -> ZeroRange {
        match self {
            LeafToken::Actual(token) => token.range,
            LeafToken::Missing(token) => Range::from_positions(
                token.position, token.position
            ),
        }
    }
    fn get_leaf(&self, pos: ZeroPosition) -> Option<LeafToken> {
        if self.range().contains_pos(pos) {
            Some(self.clone())
        } else {
            None
        }
    }
    fn subs<'a>(&'a self) -> TreeElements<'a> {
        vec![]
    }
    fn report_missing(&self) -> Vec<LocalDMLError> {
        match self {
            Self::Actual(_) => vec![],
            Self::Missing(missingtok) => vec![missingtok.into()],
        }
    }
}

impl LeafToken {
    pub fn with_actual<R, F>(&self, check: F, default: R) -> R where
        F : FnOnce (&Token) -> R {
        match self {
            Self::Actual(token) => check(token),
            _ => default,
        }
    }
    pub fn read_leaf(&self, file: &TextFile) -> Option<String> {
        self.with_actual(|tok|Some(tok.read_token(file)), None)
    }
    pub fn get_token(&self) -> Option<Token> {
        match self {
            Self::Actual(tok) => Some(*tok),
            Self::Missing(_) => None,
        }
    }
    pub fn is_actual(&self) -> bool {
        matches!(self, LeafToken::Actual(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MissingContent {
    pub description: &'static str,
    pub ended_by: Option<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Content<T: Clone + PartialEq> {
    Some(T),
    Missing(MissingContent),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstObject<T: Clone + PartialEq> {
    // Entire range of content
    pub range: ZeroRange,
    // Boxing the content here saves us headaches from recursive types
    // in other places
    pub content: Box<Content<T>>,
}

impl <T: MaybeIsNodeRef + Clone + PartialEq + 'static> MaybeIsNodeRef for AstObject<T> {
    fn maybe_noderef<'a>(&self, file: FileSpec<'a>) -> Option<NodeRef> {
        self.with_content(|content|content.maybe_noderef(file), None)
    }
}

impl <T: TreeElement + Clone + PartialEq + 'static> From<T> for AstObject<T> {
    fn from(content: T) -> Self {
        AstObject::<T> {
            range: content.range(),
            content: Box::new(Content::Some(content)),
        }
    }
}

impl <T: TreeElementMember + Clone + PartialEq + 'static> TreeElement
    for AstObject<T> {
        fn range(&self) -> ZeroRange {
            self.range
        }
        fn subs<'a>(&'a self) -> TreeElements<'a> {
            match &*self.content {
                Content::Some(content) => vec![
                    content as &dyn TreeElementMember],
                Content::Missing(_) => vec![],
            }
        }
        fn report_missing(&self) -> Vec<LocalDMLError> {
            match &*self.content {
                Content::Some(content) => content.report_missing(),
                Content::Missing(missing) => vec![
                make_error_from_missing_content(self.range, missing)],
            }
        }
    }

impl <T: Clone + PartialEq> From<MissingToken> for AstObject<T> {
    fn from(miss: MissingToken) -> Self {
        AstObject::<T>{
            range: Range::from_positions(miss.position, miss.position),
            content: Box::new(Content::Missing(
                MissingContent {
                    description: miss.description,
                    ended_by: miss.ended_by
                })),
        }
    }
}

impl <T: Clone + PartialEq> AstObject<T> {
    pub fn from_missing(range: ZeroRange,
                        missing: &MissingContent,
                        new_desc: &'static str) -> Self {
        AstObject::<T>{
            range,
            content: Box::new(Content::Missing(MissingContent {
                description: new_desc,
                ended_by: missing.ended_by
            })),
        }
    }
    pub fn with_content<R, F>(&self, mut check: F, default: R) -> R where
        F : FnMut (&T) -> R {
        match &*self.content {
            Content::Some(content) => check(content),
            _ => default,
        }
    }
}

//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use lazy_static::lazy_static;
use slotmap::{DefaultKey, SlotMap};

use serde::{Deserialize, Serialize};

use std::marker::PhantomData;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Mutex;

use std::cmp::{Ordering, Ord, PartialOrd};
use std::fmt::{Debug, Formatter};

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Column<I: Indexed>(pub u32, PhantomData<I>);

impl <I: Indexed> Debug for Column<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Col<{}I>({})",
               I::MINIMUM_VALUE,
               &self.0)
    }
}

impl<I: Indexed> Column<I> {
    fn new(c: u32) -> Column<I> {
        Column(c, PhantomData)
    }
}

impl<I: Indexed> Clone for Column<I> {
    fn clone(&self) -> Column<I> {
        *self
    }
}

impl<I: Indexed> Copy for Column<I> {}

impl<I: Indexed> Column<I> {
    pub fn next(self) -> Column<I> {
        Column::new(self.0 + 1)
    }
    pub fn previous(self) -> Option<Column<I>> {
        if self.0 > I::MINIMUM_VALUE {
            Some(Column(self.0 - 1, PhantomData))
        } else {
            None
        }
    }
}

impl<I: Indexed> Serialize for Column<I> {
    fn serialize<S: serde::Serializer>(
        &self,
        s: S,
    ) -> Result<<S as serde::Serializer>::Ok, <S as serde::Serializer>::Error> {
        s.serialize_u32(self.0)
    }
}

impl<'dt, I: Indexed> Deserialize<'dt> for Column<I> {
    fn deserialize<D: serde::Deserializer<'dt>>(
        d: D,
    ) -> std::result::Result<Self, <D as serde::Deserializer<'dt>>::Error> {
        <u32 as Deserialize>::deserialize(d).map(Column::new)
    }
}

impl Column<OneIndexed> {
    pub fn new_one_indexed(c: u32) -> Column<OneIndexed> {
        Column(c, PhantomData)
    }

    pub fn zero_indexed(self) -> Column<ZeroIndexed> {
        Column(self.0 - 1, PhantomData)
    }
}

impl Column<ZeroIndexed> {
    pub fn new_zero_indexed(c: u32) -> Column<ZeroIndexed> {
        Column(c, PhantomData)
    }

    pub fn one_indexed(self) -> Column<OneIndexed> {
        Column(self.0 + 1, PhantomData)
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Row<I: Indexed>(pub u32, PhantomData<I>);

impl <I: Indexed> Debug for Row<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Row<{}I>({})",
               I::MINIMUM_VALUE,
               &self.0)
    }
}

impl<I: Indexed> Row<I> {
    fn new(c: u32) -> Row<I> {
        Row(c, PhantomData)
    }
}

impl<I: Indexed> Clone for Row<I> {
    fn clone(&self) -> Row<I> {
        *self
    }
}

impl<I: Indexed> Copy for Row<I> {}

impl<I: Indexed> Row<I> {
    pub fn next(self) -> Row<I> {
        Row::new(self.0 + 1)
    }
    pub fn previous(self) -> Option<Row<I>> {
        if self.0 > I::MINIMUM_VALUE {
            Some(Row(self.0 - 1, PhantomData))
        } else {
            None
        }
    }
}

impl<I: Indexed> serde::Serialize for Row<I> {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_u32(self.0)
    }
}

impl<'dt, I: Indexed> serde::Deserialize<'dt> for Row<I> {
    fn deserialize<D: serde::Deserializer<'dt>>(d: D) -> std::result::Result<Self, D::Error> {
        <u32 as Deserialize>::deserialize(d).map(Row::new)
    }
}

impl Row<OneIndexed> {
    pub fn new_one_indexed(c: u32) -> Row<OneIndexed> {
        Row(c, PhantomData)
    }

    pub fn zero_indexed(self) -> Row<ZeroIndexed> {
        Row(self.0 - 1, PhantomData)
    }
}

impl Row<ZeroIndexed> {
    pub fn new_zero_indexed(c: u32) -> Row<ZeroIndexed> {
        Row(c, PhantomData)
    }

    pub fn one_indexed(self) -> Row<OneIndexed> {
        Row(self.0 + 1, PhantomData)
    }
}

#[derive(Debug, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position<I: Indexed> {
    pub row: Row<I>,
    pub col: Column<I>,
}

impl<I: Indexed> Position<I> {
    pub fn new(row: Row<I>, col: Column<I>) -> Position<I> {
        Position { row, col }
    }
}

impl<I: Indexed> Clone for Position<I> {
    fn clone(&self) -> Position<I> {
        *self
    }
}

impl<I: Indexed> Copy for Position<I> {}

impl Position<OneIndexed> {
    pub fn zero_indexed(self) -> Position<ZeroIndexed> {
        Position { row: self.row.zero_indexed(), col: self.col.zero_indexed() }
    }
    pub fn from_u32(row: u32, col: u32) ->Position<OneIndexed> {
        Position::<OneIndexed>::new(
            Row::<OneIndexed>::new_one_indexed(row),
            Column::<OneIndexed>::new_one_indexed(col)
        )
    }
}

impl Position<ZeroIndexed> {
    pub fn one_indexed(self) -> Position<OneIndexed> {
        Position { row: self.row.one_indexed(), col: self.col.one_indexed() }
    }
    pub fn from_u32(row: u32, col: u32) ->Position<ZeroIndexed> {
        Position::<ZeroIndexed>::new(
            Row::<ZeroIndexed>::new_zero_indexed(row),
            Column::<ZeroIndexed>::new_zero_indexed(col)
        )
    }
}

// We end up storing _a lot_ of file-aware position info, resulting in a lot
// of duplicating PathBufs. Here we attempt to store each pathbuf at most twice
// (once for index->path, once for path->index)
#[derive(Copy, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct PathBufKey(DefaultKey);

impl std::fmt::Debug for PathBufKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        get_path(*self).fmt(f)
    }
}

// Note: This is similar in storage size as a bimap
lazy_static! {
    static ref PATHBUF_STORAGE: Mutex<SlotMap<DefaultKey, PathBuf>> =
        Mutex::default();
    static ref PATHBUF_INDEXES: Mutex<HashMap<PathBuf, DefaultKey>> =
        Mutex::default();
}

fn track_path(path: &PathBuf) -> PathBufKey {
    {
        if let Some(index) = PATHBUF_INDEXES.lock().unwrap().get(path) {
            return PathBufKey(*index);
        }
    }
    let index = PATHBUF_STORAGE.lock().unwrap().insert(path.clone());
    PATHBUF_INDEXES.lock().unwrap().insert(path.clone(), index);
    PathBufKey(index)
}

fn get_path(index: PathBufKey) -> PathBuf {
    // Guaranteed, because we never clear out the map
    PATHBUF_STORAGE.lock().unwrap().get(index.0).unwrap().clone()
}

#[derive(Copy, Clone, Debug, Hash,
         PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePosition<I: Indexed> {
    pub file: PathBufKey,
    pub position: Position<I>,
}

impl<I: Indexed> FilePosition<I> {
    pub fn new<F: Into<PathBuf>>(position: Position<I>,
                                 file: F) -> FilePosition<I> {
        let index = track_path(&file.into());
        FilePosition { position, file: index }
    }
}

impl <I: Indexed> FilePosition<I> {
    pub fn path(&self) -> PathBuf {
        get_path(self.file)
    }
}

#[derive(Debug, Deserialize, Serialize, Hash, PartialEq, Eq)]
pub struct Range<I: Indexed> {
    pub row_start: Row<I>,
    pub row_end: Row<I>,
    pub col_start: Column<I>,
    pub col_end: Column<I>,
}

impl <I: Indexed> PartialOrd for Range<I> {
    fn partial_cmp(&self, other: &Range<I>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl <I: Indexed> Ord for Range<I> {
    fn cmp(&self, other: &Range<I>) -> Ordering {
        if self == other {
            Ordering::Equal
        } else if self.start() < other.start()
            || (self.start() == other.start() && self.end() < other.end()) {
                Ordering::Less
            } else {
                Ordering::Greater
            }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum ContainsResult {
    Equal,
    Contains,
    Contained,
    Partial,
    Disjoint,
}

impl<I: Indexed> Range<I> {
    pub fn new(
        row_start: Row<I>,
        row_end: Row<I>,
        col_start: Column<I>,
        col_end: Column<I>,
    ) -> Range<I> {
        Range { row_start, row_end, col_start, col_end }
    }

    pub fn from_positions(start: Position<I>, end: Position<I>) -> Range<I> {
        assert!(start <= end);
        Range { row_start: start.row, row_end: end.row,
                col_start: start.col, col_end: end.col }
    }

    pub fn start(self) -> Position<I> {
        Position { row: self.row_start, col: self.col_start }
    }

    pub fn end(self) -> Position<I> {
        Position { row: self.row_end, col: self.col_end }
    }

    pub fn combine(first: Range<I>, second: Range<I>) -> Self{
        // Check if either range is invalid, and if so ignore it
        if first == Range::<I>::invalid() {
            second
        } else if second == Range::<I>::invalid() {
            first
        } else if first < second {
            Self::from_positions(first.start(), second.end())
        } else {
            Self::from_positions(second.start(), first.end())
        }
    }

    pub fn contains(&self, other: Range<I>) -> ContainsResult {
        match (other.contains_pos(self.start()),
               self.contains_pos(other.start()),
               other.contains_pos(self.end()),
               self.contains_pos(other.end())) {
            (true, true, false, false) => ContainsResult::Equal,
            (true, _, true, _)   => ContainsResult::Contained,
            (_, true, false, _) => ContainsResult::Contains,
            (false, false, _, _) => ContainsResult::Disjoint,
            _ => ContainsResult::Partial,
        }
    }

    pub fn contains_pos(&self, pos: Position<I>) -> bool {
        (self.start() <= pos) && (self.end() > pos)
    }

    pub fn overlaps(&self, range: Range<I>) -> bool {
        range.contains_pos(self.start())
            || self.contains_pos(range.start())
    }

    pub fn invalid() -> Range<I> {
        // TODO: we should probably not have this, very un-rustic.
        //       better to wrap range in an option or an enum
        // This is as invalid as a range can get
        // It will contain no positions at least
        Self::new(
            Row::<I>::new(0),
            Row::<I>::new(0),
            Column::<I>::new(0),
            Column::<I>::new(0))
    }
}

impl<I: Indexed> Clone for Range<I> {
    fn clone(&self) -> Range<I> {
        *self
    }
}

impl<I: Indexed> Copy for Range<I> {}

impl Range<OneIndexed> {
    pub fn zero_indexed(self) -> Range<ZeroIndexed> {
        Range {
            row_start: self.row_start.zero_indexed(),
            row_end: self.row_end.zero_indexed(),
            col_start: self.col_start.zero_indexed(),
            col_end: self.col_end.zero_indexed(),
        }
    }
    pub fn from_u32(row_start: u32,
                    row_end: u32,
                    col_start: u32,
                    col_end: u32) -> Range<OneIndexed> {
        Self::from_positions(
            Position::<OneIndexed>::new(
                Row::<OneIndexed>::new_one_indexed(row_start),
                Column::<OneIndexed>::new_one_indexed(col_start),
            ),
            Position::<OneIndexed>::new(
                Row::<OneIndexed>::new_one_indexed(row_end),
                Column::<OneIndexed>::new_one_indexed(col_end),
            ),
        )
    }
}

impl Range<ZeroIndexed> {
    pub fn one_indexed(self) -> Range<OneIndexed> {
        Range {
            row_start: self.row_start.one_indexed(),
            row_end: self.row_end.one_indexed(),
            col_start: self.col_start.one_indexed(),
            col_end: self.col_end.one_indexed(),
        }
    }
    pub fn from_u32(row_start: u32,
                    row_end: u32,
                    col_start: u32,
                    col_end: u32) -> Range<ZeroIndexed> {
        Self::from_positions(
            Position::<ZeroIndexed>::new(
                Row::<ZeroIndexed>::new_zero_indexed(row_start),
                Column::<ZeroIndexed>::new_zero_indexed(col_start),
            ),
            Position::<ZeroIndexed>::new(
                Row::<ZeroIndexed>::new_zero_indexed(row_end),
                Column::<ZeroIndexed>::new_zero_indexed(col_end),
            ),
        )
    }
}

#[derive(Debug, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location<I: Indexed> {
    pub file: PathBuf,
    pub position: Position<I>,
}

impl<I: Indexed> Location<I> {
    pub fn new<F: Into<PathBuf>>(row: Row<I>, col: Column<I>, file: F) -> Location<I> {
        Location { position: Position { row, col }, file: file.into() }
    }

    pub fn from_position<F: Into<PathBuf>>(position: Position<I>, file: F) -> Location<I> {
        Location { position, file: file.into() }
    }
}

impl<I: Indexed> Clone for Location<I> {
    fn clone(&self) -> Location<I> {
        Location { position: self.position, file: self.file.clone() }
    }
}

impl Location<OneIndexed> {
    pub fn zero_indexed(&self) -> Location<ZeroIndexed> {
        Location { position: self.position.zero_indexed(), file: self.file.clone() }
    }
}

impl Location<ZeroIndexed> {
    pub fn one_indexed(&self) -> Location<OneIndexed> {
        Location { position: self.position.one_indexed(), file: self.file.clone() }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span<I: Indexed> {
    pub file: PathBufKey,
    pub range: Range<I>,
}

impl <I: Indexed> Span<I> {
    pub fn path(&self) -> PathBuf {
        get_path(self.file)
    }
}

impl<I: Indexed> Span<I> {
    pub fn new<F: Into<PathBuf>>(
        row_start: Row<I>,
        row_end: Row<I>,
        col_start: Column<I>,
        col_end: Column<I>,
        file: F,
    ) -> Span<I> {
        Span::from_range(Range { row_start, row_end, col_start, col_end },
                         file)
    }

    pub fn from_range<F: Into<PathBuf>>(range: Range<I>, file: F) -> Span<I> {
        let index = track_path(&file.into());
        Span { range, file: index }
    }

    pub fn from_positions<F: Into<PathBuf>>(
        start: Position<I>,
        end: Position<I>,
        file: F,
    ) -> Span<I> {
        Span::from_range(Range::from_positions(start, end), file)
    }

    pub fn combine(first: Span<I>, second: Span<I>) -> Self {
        // It is a hard error to combine spans from different files
        assert!(first.file == second.file);
        let Span{file, range: first_range} = first;
        let second_range = second.range;
        Span::from_range(Range::combine(first_range, second_range),
                         get_path(file))
    }

    pub fn start_position(&self) -> FilePosition<I> {
        FilePosition::new(self.range.start(), self.path())
    }

    pub fn end_position(&self) -> FilePosition<I> {
        FilePosition::new(self.range.end(), self.path())
    }
}

impl<I: Indexed> Span<I> {
    pub fn invalid<F: Into<PathBuf>>(file: F) -> Span<I> {
        // TODO: we should probably not have this, very un-rustic.
        //       better to wrap range in an option or an enum
        // This is as invalid as a range can get
        // It will contain no positions at least
        Span::from_range(Range::invalid(), file)
    }
}

impl <I: Indexed> Span<I> {
    pub fn contains_pos(&self, pos: &FilePosition<I>) -> bool {
        self.range.contains_pos(pos.position)
    }
}

impl Span<OneIndexed> {
    pub fn zero_indexed(&self) -> Span<ZeroIndexed> {
        Span { range: self.range.zero_indexed(), file: self.file }
    }
    pub fn from_u32<F: Into<PathBuf>>(row_start: u32,
                                      row_end: u32,
                                      col_start: u32,
                                      col_end: u32,
                                      file: F) -> Span<OneIndexed> {
        Self::from_positions(
            Position::<OneIndexed>::new(
                Row::<OneIndexed>::new_one_indexed(row_start),
                Column::<OneIndexed>::new_one_indexed(col_start),
            ),
            Position::<OneIndexed>::new(
                Row::<OneIndexed>::new_one_indexed(row_end),
                Column::<OneIndexed>::new_one_indexed(col_end),
            ),
            file,
        )
    }
}

impl Span<ZeroIndexed> {
    pub fn one_indexed(&self) -> Span<OneIndexed> {
        Span { range: self.range.one_indexed(), file: self.file }
    }
    pub fn from_u32<F: Into<PathBuf>>(row_start: u32,
                                      row_end: u32,
                                      col_start: u32,
                                      col_end: u32,
                                      file: F) -> Span<ZeroIndexed> {
        Self::from_positions(
            Position::<ZeroIndexed>::new(
                Row::<ZeroIndexed>::new_zero_indexed(row_start),
                Column::<ZeroIndexed>::new_zero_indexed(col_start),
            ),
            Position::<ZeroIndexed>::new(
                Row::<ZeroIndexed>::new_zero_indexed(row_end),
                Column::<ZeroIndexed>::new_zero_indexed(col_end),
            ),
            file,
        )
    }
}

pub trait Indexed: PartialOrd + Ord {
    const MINIMUM_VALUE: u32;
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize,
         Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ZeroIndexed;
impl Indexed for ZeroIndexed {
    const MINIMUM_VALUE: u32 = 0;
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize,
         Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OneIndexed;
impl Indexed for OneIndexed {
    const MINIMUM_VALUE: u32 = 1;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_row_zero_indexed() {
        let row_0 = Row::<ZeroIndexed>::new(0);
        let row_1 = Row::<ZeroIndexed>::new(1);
        assert!(row_0 < row_1);
        assert!(row_1 > row_0);
        assert!(row_0 == row_0.clone());
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_row_one_indexed() {
        let row_1 = Row::<ZeroIndexed>::new(1);
        let row_2 = Row::<ZeroIndexed>::new(2);
        assert!(row_1 < row_2);
        assert!(row_2 > row_1);
        assert_eq!(row_1, row_1.clone());
    }

    #[test]
    fn test_row_conversion() {
        let row_0_zero = Row::<ZeroIndexed>::new(0);
        let row_1_one  = Row::<OneIndexed>::new(1);
        assert_eq!(row_0_zero.one_indexed(), row_1_one);
        assert_eq!(row_1_one.zero_indexed(), row_0_zero);
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_column_zero_indexed() {
        let column_0 = Column::<ZeroIndexed>::new(0);
        let column_1 = Column::<ZeroIndexed>::new(1);
        assert!(column_0 < column_1);
        assert!(column_1 > column_0);
        assert!(column_0 == column_0.clone());
    }

    #[test]
    #[allow(clippy::clone_on_copy)]
    fn test_column_one_indexed() {
        let column_1 = Column::<ZeroIndexed>::new(1);
        let column_2 = Column::<ZeroIndexed>::new(2);
        assert!(column_1 < column_2);
        assert!(column_2 > column_1);
        assert_eq!(column_1, column_1.clone());
    }

    #[test]
    fn test_column_conversion() {
        let column_0_zero = Column::<ZeroIndexed>::new(0);
        let column_1_one  = Column::<OneIndexed>::new(1);
        assert_eq!(column_0_zero.one_indexed(), column_1_one);
        assert_eq!(column_1_one.zero_indexed(), column_0_zero);
    }

    #[test]
    fn test_position() {
        assert!(Position::<ZeroIndexed>::from_u32(0, 1) >
                Position::<ZeroIndexed>::from_u32(0, 0),
                "0,1 > 0,0");

        let positions: Vec<Position<ZeroIndexed>>
            = (0..2).flat_map(
                move |i|(0..2).map(
                    move |j|Position::<ZeroIndexed>::from_u32(i, j)))
            .collect();
        for (i, pos) in positions.iter().enumerate() {
            for (j, other_pos) in positions.iter().enumerate() {
                match i.cmp(&j) {
                    Ordering::Equal =>
                        assert_eq!(pos, other_pos,
                                   "{:?} == {:?}",
                                   pos, other_pos),
                    Ordering::Less =>
                        assert!(pos < other_pos, "{:?} < {:?}", pos, other_pos),
                    Ordering::Greater  =>
                        assert!(pos > other_pos, "{:?} > {:?}", pos, other_pos),
                }
            }
        }
    }

    #[test]
    fn test_range() {
        let range = Range::<ZeroIndexed>::from_u32(0, 1, 2, 3);
        assert!(range.start() < range.end());
        let range_2 = Range::<ZeroIndexed>::from_u32(1, 1, 1, 1);
        assert!(range < range_2);
        let range_3 = Range::<ZeroIndexed>::from_u32(1, 1, 0, 2);
        assert!(range < range_3);
        assert!(range_2 > range_3);
    }

    #[test]
    fn test_range_contains_pos() {
        let range = Range::<ZeroIndexed>::from_u32(0, 1, 0, 0);
        assert!(range.contains_pos(Position::<ZeroIndexed>::from_u32(0, 0)));
        assert!(range.contains_pos(Position::<ZeroIndexed>::from_u32(0, 1000)));
        assert!(!range.contains_pos(Position::<ZeroIndexed>::from_u32(1, 1)));
        assert!(!range.contains_pos(Position::<ZeroIndexed>::from_u32(1, 0)));
        assert!(range.contains_pos(range.start()),
                "Range contains .start()");
        assert!(!range.contains_pos(range.end()),
                "Range does not contain .end()");
    }

    #[test]
    fn test_range_overlap() {
        let range0 = Range::<ZeroIndexed>::from_u32(0, 1, 0, 0);
        let range1 = Range::<ZeroIndexed>::from_u32(0, 2, 0, 0);
        let range2 = Range::<ZeroIndexed>::from_u32(1, 3, 0, 0);
        assert!(range0.overlaps(range1), "(0,1) overlaps (0,2)");
        assert!(range1.overlaps(range0), "(0,2) overlaps (0,1)");
        assert!(range1.overlaps(range2), "(0,2) overlaps (1,3)");
        assert!(range2.overlaps(range1), "(1,3) overlaps (0,2)");
        assert!(!range0.overlaps(range2), "(0,1) not overlaps (1,3)");
        assert!(!range2.overlaps(range0), "(1,3) not overlaps (0,1)");
    }

    #[test]
    fn test_range_contains() {
        let range0 = Range::<ZeroIndexed>::from_u32(0, 1, 0, 0);
        let range1 = Range::<ZeroIndexed>::from_u32(0, 2, 0, 0);
        let range2 = Range::<ZeroIndexed>::from_u32(1, 3, 0, 0);
        let range3 = Range::<ZeroIndexed>::from_u32(0, 4, 0, 0);
        assert!(range0.contains(range0) == ContainsResult::Equal);
        assert!(range0.contains(range1) == ContainsResult::Contained);
        assert!(range1.contains(range0) == ContainsResult::Contains);
        assert!(range0.contains(range2) == ContainsResult::Disjoint);
        assert!(range1.contains(range2) == ContainsResult::Partial);
        assert!(range2.contains(range1) == ContainsResult::Partial);
        assert!(range3.contains(range2) == ContainsResult::Contains);
        assert!(range2.contains(range3) == ContainsResult::Contained);
    }

    #[test]
    fn test_range_sort() {
        let range0 = Range::<ZeroIndexed>::from_u32(0, 4, 0, 0);
        let range1 = Range::<ZeroIndexed>::from_u32(1, 3, 0, 0);
        let range2 = Range::<ZeroIndexed>::from_u32(2, 2, 0, 0);
        let range3 = Range::<ZeroIndexed>::from_u32(1, 5, 0, 0);
        let mut vec = vec![range0, range1, range2, range3];
        vec.sort();
        assert_eq!(vec,
                   vec![range0, range1, range3, range2])
    }
}

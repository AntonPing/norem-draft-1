use std::default::Default;
use std::fmt::{self, Debug};
use std::hash::Hash;

/// module `position`
/// In this module we define structure `Position`,
/// structure `Span` and trait `Spanned`

/// A `Position` is a location in the source code.
/// The `abs` field is the absolute index of the character,
/// while `row` and `col` fields are line number and column number
/// 
/// # Example
/// 
/// ```text
/// Hello, world!
/// This is an
/// example source code
/// ```
/// Here the letter 'i' in word "This" has Position
/// Position { row: 2, col: 3, abs: 16 }

#[derive(Clone, Copy, Default, Hash)]
pub struct Position {
    pub row: usize,
    pub col: usize,
    pub abs: usize,
}

impl PartialEq for Position {
    // Only comparing field `abs` is enough. Same in `PartialOrd` and `Ord`
    fn eq(&self, other: &Self) -> bool {
        self.abs == other.abs
    }
}

impl Eq for Position {}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.abs.partial_cmp(&other.abs)
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.abs.cmp(&other.abs)
    }
}

impl Position {
    pub fn new(row: usize, col: usize, abs: usize) -> Position {
        Position { row, col, abs }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}({})", self.row, self.col, self.abs)
    }
}

/// A `Span` is a structure of two position.
/// It marks the `start` and the `end` of a slice in source code.
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }
    pub fn merge(lhs: &Span, rhs: &Span) -> Span {
        let start = lhs.start.min(rhs.start);
        let end = lhs.end.max(rhs.end);
        Span { start, end }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?}..{:?}]", self.start, self.end)
    }
}

/// A `Spanned` structure is a structure in which contains a `Span`
pub trait Spanned {
    fn span(&self) -> &Span;
    fn span_mut(&mut self) -> &mut Span;
}
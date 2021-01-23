//! Various diff (longest common subsequence) algorithms.
//!
//! The implementations of the algorithms in this module are relatively low
//! level and expose the most generic bounds possible for the algorithm.  To
//! use them you would typically use the higher level API if possible but
//! direct access to these algorithms can be useful in some cases.
//!
//! All these algorithms provide a `diff` function which takes two indexable
//! objects (for instance slices) and a [`DiffHook`].  As the diff is generated
//! the diff hook is invoked.  Note that the diff hook does not get access to
//! the actual values but only the indexes.  This is why the diff hook is not
//! used outside of the raw algorithm implementations as for most situations
//! access to the values is useful of required.
//!
//! Most of the crate operates on the [`Algorithm`] enum which abstracts over
//! the different algorithms.

// general traits and utilities
mod capture;
mod hook;
mod replace;

use std::hash::Hash;
use std::ops::{Index, Range};

pub use capture::*;
pub use hook::*;
pub use replace::*;

// actual diffing algorithms
pub mod myers;
pub mod patience;

/// An enum representing a diffing algorithm.
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Algorithm {
    Myers,
    Patience,
}

impl Default for Algorithm {
    /// Returns the default algorithm ([`Algorithm::Myers`]).
    fn default() -> Algorithm {
        Algorithm::Myers
    }
}

/// Creates a diff between old and new with the given algorithm.
///
/// Diffs `old`, between indices `old_range` and `new` between indices `new_range`.
pub fn diff<Old, New, D>(
    alg: Algorithm,
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq + Ord,
    New::Output: PartialEq<Old::Output> + Hash + Eq + Ord,
{
    match alg {
        Algorithm::Myers => myers::diff(d, old, old_range, new, new_range),
        Algorithm::Patience => patience::diff(d, old, old_range, new, new_range),
    }
}

/// Shortcut for diffing slices with a specific algorithm.
pub fn diff_slices<D, T>(alg: Algorithm, d: &mut D, old: &[T], new: &[T]) -> Result<(), D::Error>
where
    D: DiffHook,
    T: Eq + Hash + Ord,
{
    diff(alg, d, old, 0..old.len(), new, 0..new.len())
}

/// Creates a diff between old and new with the given algorithm capturing the ops.
///
/// This is like [`diff`] but instead of using an arbitrary hook this will
/// always use [`Replace`] + [`Capture`] and return the captured [`DiffOp`]s.
pub fn capture_diff<Old, New>(
    alg: Algorithm,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> Vec<DiffOp>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq + Ord,
    New::Output: PartialEq<Old::Output> + Hash + Eq + Ord,
{
    let mut d = Replace::new(Capture::new());
    diff(alg, &mut d, old, old_range, new, new_range).unwrap();
    d.into_inner().into_ops()
}

/// Creates a diff between old and new with the given algorithm capturing the ops.
pub fn capture_diff_slices<D, T>(alg: Algorithm, old: &[T], new: &[T]) -> Vec<DiffOp>
where
    T: Eq + Hash + Ord,
{
    let mut d = Replace::new(Capture::new());
    diff_slices(alg, &mut d, old, new).unwrap();
    d.into_inner().into_ops()
}

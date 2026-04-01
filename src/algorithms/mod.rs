//! Various diff (longest common subsequence) algorithms.
//!
//! The implementations of the algorithms in this module are relatively low
//! level and expose the most generic bounds possible for the algorithm.  To
//! use them you would typically use the higher level API if possible but
//! direct access to these algorithms can be useful in some cases.
//!
//! All these algorithms provide a `diff` function which takes two indexable
//! objects (for instance slices) and a [`DiffHook`].  As the
//! diff is generated the diff hook is invoked.  Note that the diff hook does
//! not get access to the actual values but only the indexes.  This is why the
//! diff hook is not used outside of the raw algorithm implementations as for
//! most situations access to the values is useful of required.
//!
//! The algorithms module really is the most low-level module in similar and
//! generally not the place to start.
//!
//! # Heuristics
//!
//! Algorithm entrypoints in this module (`diff` / `diff_deadline`) are
//! heuristic-enabled.  In practice that means they use practical shortcuts to
//! keep difficult inputs fast while still producing useful diff scripts.
//!
//! At a high level, the current heuristics are:
//!
//! - **Shared disjoint-range fast path** (all algorithms):
//!   if two large ranges appear to have no common items, we skip expensive
//!   search and emit a straight delete+insert replacement.
//! - **Prefix/suffix trimming** (used widely):
//!   matching runs at the beginning/end are emitted immediately so each
//!   algorithm only works on the changed middle.
//! - **Deadline-aware fallback behavior**:
//!   when a deadline is provided, algorithms periodically check it and may fall
//!   back to a simpler script instead of running too long.
//! - **Algorithm-local anchor strategies**:
//!   - **Patience** anchors on items unique in both sides.
//!   - **Histogram** prefers low-frequency anchors and avoids very noisy lines.
//!   - **Hunt** uses match lists and longest-increasing anchor chains.
//! - **Myers-specific safeguards**:
//!   beyond classic middle-snake recursion, Myers adds a front-anchor peel for
//!   heavily unbalanced shifts and an exact small-side fallback when one side
//!   is tiny and the other is large.
//!
//! Some heuristic-enabled entrypoints may require stricter trait bounds than
//! their raw counterpart (for example, shared heuristics that build hash-based
//! lookups require `Hash + Eq`).  In those cases, `diff_deadline_raw` is the
//! compatibility path with minimal bounds.
//!
//! If you want to skip shared heuristics, each algorithm module provides
//! `diff_deadline_raw`, which keeps that algorithm's minimal intrinsic bounds.
//!
//! The top-level dispatcher [`diff_deadline`] always calls the
//! heuristic-enabled entrypoints and never calls raw variants.
//!
//! # Example
//!
//! This is a simple example that shows how you can calculate the difference
//! between two sequences and capture the ops into a vector.
//!
//! ```rust
//! use similar::algorithms::{Algorithm, Replace, Capture, diff_slices};
//!
//! let a = vec![1, 2, 3, 4, 5];
//! let b = vec![1, 2, 3, 4, 7];
//! let mut d = Replace::new(Capture::new());
//! diff_slices(Algorithm::Myers, &mut d, &a, &b).unwrap();
//! let ops = d.into_inner().into_ops();
//! ```
//!
//! The above example is equivalent to using
//! [`capture_diff_slices`](crate::capture_diff_slices).

mod capture;
mod compact;
mod hook;
mod preflight;
mod replace;
pub(crate) mod utils;

use std::hash::Hash;
use std::ops::{Index, Range};

use crate::deadline_support::Instant;
pub use capture::Capture;
pub use compact::Compact;
pub use hook::{DiffHook, NoFinishHook};
pub use replace::Replace;
pub use utils::IdentifyDistinct;

#[doc(no_inline)]
pub use crate::Algorithm;

pub mod histogram;
pub mod hunt;
pub mod lcs;
pub mod myers;
pub mod patience;

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
    diff_deadline(alg, d, old, old_range, new, new_range, None)
}

/// Creates a diff between old and new with the given algorithm with deadline.
///
/// Diffs `old`, between indices `old_range` and `new` between indices `new_range`.
///
/// This diff is done with an optional deadline that defines the maximal
/// execution time permitted before it bails and falls back to an approximation.
/// Note that not all algorithms behave well if they reach the deadline (LCS
/// for instance produces a very simplistic diff when the deadline is reached
/// in all cases).
pub fn diff_deadline<Old, New, D>(
    alg: Algorithm,
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq + Ord,
    New::Output: PartialEq<Old::Output> + Hash + Eq + Ord,
{
    match alg {
        Algorithm::Myers => myers::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Patience => patience::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Lcs => lcs::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Hunt => hunt::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Histogram => {
            histogram::diff_deadline(d, old, old_range, new, new_range, deadline)
        }
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

/// Shortcut for diffing slices with a specific algorithm.
pub fn diff_slices_deadline<D, T>(
    alg: Algorithm,
    d: &mut D,
    old: &[T],
    new: &[T],
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    D: DiffHook,
    T: Eq + Hash + Ord,
{
    diff_deadline(alg, d, old, 0..old.len(), new, 0..new.len(), deadline)
}

#[test]
fn test_disjoint_fast_path_cross_type_guard() {
    use crate::DiffOp;
    use std::hash::{Hash, Hasher};

    #[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
    struct A(u32);

    impl Hash for A {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.0.hash(state);
        }
    }

    #[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
    struct B(u32);

    impl Hash for B {
        fn hash<H: Hasher>(&self, state: &mut H) {
            (self.0.wrapping_add(1_000_000)).hash(state);
        }
    }

    impl PartialEq<A> for B {
        fn eq(&self, other: &A) -> bool {
            self.0 == other.0
        }
    }

    let n = 512u32;
    let old: Vec<A> = (0..n).map(A).collect();
    let mut new: Vec<B> = (0..n).map(B).collect();
    new.rotate_left(1);

    let ops = crate::capture_diff(Algorithm::Myers, &old, 0..old.len(), &new, 0..new.len());
    let equal_len = ops
        .iter()
        .map(|op| match op {
            DiffOp::Equal { len, .. } => *len,
            _ => 0,
        })
        .sum::<usize>();

    assert_eq!(equal_len, n as usize - 1);
}

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
mod replace;
pub(crate) mod utils;

use std::any::type_name;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::{Index, Range};

use crate::deadline_support::{Instant, deadline_exceeded};
pub use capture::Capture;
pub use compact::Compact;
pub use hook::{DiffHook, NoFinishHook};
pub use replace::Replace;
pub use utils::IdentifyDistinct;

#[doc(no_inline)]
pub use crate::Algorithm;

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
    if maybe_emit_disjoint_fast_path(d, old, old_range.clone(), new, new_range.clone(), deadline)? {
        return Ok(());
    }

    match alg {
        Algorithm::Myers => myers::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Patience => patience::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Lcs => lcs::diff_deadline(d, old, old_range, new, new_range, deadline),
        Algorithm::Hunt => hunt::diff_deadline(d, old, old_range, new, new_range, deadline),
    }
}

const DISJOINT_FAST_PATH_MIN_LEN: usize = 512;
const DISJOINT_FAST_PATH_MIN_WORK: usize = 128 * 1024;
const DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL: usize = 1024;

fn maybe_emit_disjoint_fast_path<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<bool, D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq + Ord,
    New::Output: PartialEq<Old::Output> + Hash + Eq + Ord,
{
    if deadline_exceeded(deadline) {
        return Ok(false);
    }

    let old_len = old_range.len();
    let new_len = new_range.len();

    if old_len < DISJOINT_FAST_PATH_MIN_LEN
        || new_len < DISJOINT_FAST_PATH_MIN_LEN
        || old_len.saturating_mul(new_len) < DISJOINT_FAST_PATH_MIN_WORK
    {
        return Ok(false);
    }

    // This fast-path relies on hashing values from both sides into the same
    // map. Restrict it to apparent same-output types to avoid cross-type hash
    // compatibility pitfalls.
    if type_name::<Old::Output>() != type_name::<New::Output>() {
        return Ok(false);
    }

    if new[new_range.start] == old[old_range.start]
        || new[new_range.end - 1] == old[old_range.end - 1]
    {
        return Ok(false);
    }

    let has_common_item =
        match has_common_item(old, old_range.clone(), new, new_range.clone(), deadline) {
            Some(value) => value,
            None => return Ok(false),
        };

    if has_common_item {
        return Ok(false);
    }

    d.delete(old_range.start, old_len, new_range.start)?;
    d.insert(old_range.start, new_range.start, new_len)?;
    d.finish()?;
    Ok(true)
}

fn has_common_item<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<bool>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash,
    New::Output: PartialEq<Old::Output> + Hash,
{
    #[inline(always)]
    fn hash_value<T: Hash + ?Sized>(value: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    let mut by_hash = HashMap::<u64, Vec<usize>>::new();
    for (idx, old_idx) in old_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        by_hash
            .entry(hash_value(&old[old_idx]))
            .or_default()
            .push(old_idx);
    }

    for (idx, new_idx) in new_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        if let Some(candidates) = by_hash.get(&hash_value(&new[new_idx])) {
            let new_item = &new[new_idx];
            if candidates.iter().any(|&old_idx| new_item == &old[old_idx]) {
                return Some(true);
            }
        }
    }

    Some(false)
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
fn test_has_common_item() {
    assert_eq!(
        has_common_item(&[1, 2, 3], 0..3, &[9, 3, 10], 0..3, None),
        Some(true)
    );
    assert_eq!(
        has_common_item(&[1, 2, 3], 0..3, &[9, 8, 10], 0..3, None),
        Some(false)
    );
}

#[test]
fn test_has_common_item_hash_collisions() {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Collide(u32);

    impl Hash for Collide {
        fn hash<H: Hasher>(&self, state: &mut H) {
            0u8.hash(state);
        }
    }

    assert_eq!(
        has_common_item(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(4)],
            0..2,
            None
        ),
        Some(false)
    );
    assert_eq!(
        has_common_item(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(2)],
            0..2,
            None
        ),
        Some(true)
    );
}

#[test]
fn test_disjoint_fast_path_cross_type_guard() {
    use crate::DiffOp;

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

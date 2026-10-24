//! Anchored diffing.
//!
//! An anchored diff keeps selected items aligned and runs a regular diff
//! algorithm on the stretches in between.  This follows the idea behind
//! `git diff --anchored`: an anchor that occurs exactly once on both sides is
//! never shown as deleted or inserted, no matter what the underlying algorithm
//! would have picked.
use alloc::vec::Vec;
use core::hash::Hash;
use core::ops::{Index, Range};

use crate::Algorithm;
use crate::algorithms::utils::{HashBucket, stable_hash};
use crate::algorithms::{DiffHook, NoFinishHook, diff_deadline};
use crate::deadline_support::Instant;
use crate::types::int_key_map_with_capacity;

/// Creates a diff between old and new that keeps anchor items aligned.
///
/// See [`diff_anchored_deadline`] for details.
pub fn diff_anchored<Old, New, D, F>(
    alg: Algorithm,
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    is_anchor: F,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    F: FnMut(&Old::Output) -> bool,
{
    diff_anchored_deadline(alg, d, old, old_range, new, new_range, is_anchor, None)
}

/// Creates a diff between old and new that keeps anchor items aligned, with
/// an optional deadline.
///
/// An item of `old` for which `is_anchor` returns `true` becomes an anchor if
/// its value occurs exactly once in `old_range` and exactly once in
/// `new_range`.  Anchors are always reported as equal, and `alg` only diffs
/// the ranges between them.  This mirrors `git diff --anchored=<text>`, which
/// is useful to keep a known line (such as a function signature) from being
/// shown as removed and re-added when code around it moves.
///
/// Anchors whose order differs between the two sides cannot all be kept.  In
/// that case the largest set of anchors that appear in the same order on both
/// sides is used.
///
/// `is_anchor` is only invoked for items of `old` and must give the same
/// answer for equal values.
///
/// ```rust
/// use similar::algorithms::{diff_anchored, Algorithm, Capture, Replace};
/// use similar::DiffTag;
///
/// let old = ["a", "b", "c", "anchor", "d"];
/// let new = ["anchor", "a", "b", "c", "d"];
/// let mut d = Replace::new(Capture::new());
/// diff_anchored(
///     Algorithm::Myers,
///     &mut d,
///     &old,
///     0..old.len(),
///     &new,
///     0..new.len(),
///     |item| *item == "anchor",
/// )
/// .unwrap();
/// let ops = d.into_inner().into_ops();
/// // the anchor stays aligned, "a", "b", "c" are moved around it instead
/// assert_eq!(ops[0].tag(), DiffTag::Delete);
/// assert_eq!(ops[1].tag(), DiffTag::Equal);
/// assert_eq!(ops[1].old_range(), 3..4);
/// ```
#[allow(clippy::too_many_arguments)]
pub fn diff_anchored_deadline<Old, New, D, F>(
    alg: Algorithm,
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    is_anchor: F,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    F: FnMut(&Old::Output) -> bool,
{
    let anchors = find_anchors(old, old_range.clone(), new, new_range.clone(), is_anchor);
    diff_with_anchors(alg, d, old, old_range, new, new_range, &anchors, deadline)
}

/// Finds the anchor pairs `(old_index, new_index)` for an anchored diff.
///
/// The result is ordered and strictly increasing on both sides.
pub(crate) fn find_anchors<Old, New, F>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    mut is_anchor: F,
) -> Vec<(usize, usize)>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    F: FnMut(&Old::Output) -> bool,
{
    struct Candidate {
        old_index: usize,
        old_count: u8,
        new_index: usize,
        new_count: u8,
    }

    let mut candidates: Vec<Candidate> = Vec::new();
    let mut by_hash = int_key_map_with_capacity::<u64, HashBucket<usize>>(0);
    for old_index in old_range.clone() {
        let value = &old[old_index];
        if !is_anchor(value) {
            continue;
        }
        let hash = stable_hash(value);
        if let Some(bucket) = by_hash.get_mut(&hash) {
            if bucket
                .iter()
                .any(|&idx| old[candidates[idx].old_index] == *value)
            {
                continue;
            }
            bucket.push(candidates.len());
        } else {
            by_hash.insert(hash, HashBucket::new(candidates.len()));
        }
        candidates.push(Candidate {
            old_index,
            old_count: 0,
            new_index: 0,
            new_count: 0,
        });
    }
    if candidates.is_empty() {
        return Vec::new();
    }

    // Count over all items rather than only the ones accepted by
    // `is_anchor`.  This keeps the uniqueness check exact even if equal
    // values do not agree on being an anchor.
    for old_index in old_range {
        let value = &old[old_index];
        if let Some(bucket) = by_hash.get(&stable_hash(value)) {
            if let Some(&idx) = bucket
                .iter()
                .find(|&&idx| old[candidates[idx].old_index] == *value)
            {
                let candidate = &mut candidates[idx];
                candidate.old_count = candidate.old_count.saturating_add(1);
            }
        }
    }
    for new_index in new_range {
        let value = &new[new_index];
        if let Some(bucket) = by_hash.get(&stable_hash(value)) {
            if let Some(&idx) = bucket
                .iter()
                .find(|&&idx| *value == old[candidates[idx].old_index])
            {
                let candidate = &mut candidates[idx];
                candidate.new_count = candidate.new_count.saturating_add(1);
                candidate.new_index = new_index;
            }
        }
    }

    // Candidates were collected in old order.
    let pairs = candidates
        .into_iter()
        .filter(|c| c.old_count == 1 && c.new_count == 1)
        .map(|c| (c.old_index, c.new_index))
        .collect::<Vec<_>>();
    longest_increasing_run(&pairs)
}

/// Returns the longest subsequence of `pairs` (sorted by old index) whose new
/// indexes are strictly increasing.
fn longest_increasing_run(pairs: &[(usize, usize)]) -> Vec<(usize, usize)> {
    if pairs.len() <= 1 {
        return pairs.to_vec();
    }
    // `tails[k]` is the index into `pairs` of the smallest tail of an
    // increasing run of length `k + 1`.
    let mut tails: Vec<usize> = Vec::new();
    let mut prev = Vec::with_capacity(pairs.len());
    for (idx, &(_, new_index)) in pairs.iter().enumerate() {
        let pos = tails.partition_point(|&tail| pairs[tail].1 < new_index);
        prev.push(if pos > 0 { Some(tails[pos - 1]) } else { None });
        if pos == tails.len() {
            tails.push(idx);
        } else {
            tails[pos] = idx;
        }
    }
    let mut rv = Vec::with_capacity(tails.len());
    let mut cursor = tails.last().copied();
    while let Some(idx) = cursor {
        rv.push(pairs[idx]);
        cursor = prev[idx];
    }
    rv.reverse();
    rv
}

/// Diffs the ranges between the given anchors with `alg`.
///
/// `anchors` must be strictly increasing on both sides, lie within the
/// ranges and refer to equal items.
#[allow(clippy::too_many_arguments)]
pub(crate) fn diff_with_anchors<Old, New, D>(
    alg: Algorithm,
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    anchors: &[(usize, usize)],
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    let mut old_pos = old_range.start;
    let mut new_pos = new_range.start;
    let mut iter = anchors.iter().copied().peekable();
    while let Some((old_anchor, new_anchor)) = iter.next() {
        debug_assert!(old_anchor >= old_pos && new_anchor >= new_pos);
        if old_anchor > old_pos || new_anchor > new_pos {
            diff_deadline(
                alg,
                &mut NoFinishHook::new(&mut *d),
                old,
                old_pos..old_anchor,
                new,
                new_pos..new_anchor,
                deadline,
            )?;
        }
        // Coalesce directly adjacent anchors into one equal run.
        let mut len = 1;
        while let Some(&(next_old, next_new)) = iter.peek() {
            if next_old != old_anchor + len || next_new != new_anchor + len {
                break;
            }
            len += 1;
            iter.next();
        }
        d.equal(old_anchor, new_anchor, len)?;
        old_pos = old_anchor + len;
        new_pos = new_anchor + len;
    }
    diff_deadline(
        alg,
        d,
        old,
        old_pos..old_range.end,
        new,
        new_pos..new_range.end,
        deadline,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::algorithms::{Capture, Replace};
    use crate::{DiffOp, DiffTag};
    use alloc::vec;

    fn anchored(alg: Algorithm, old: &[&str], new: &[&str], anchor: &str) -> Vec<DiffOp> {
        let mut d = Replace::new(Capture::new());
        diff_anchored(
            alg,
            &mut d,
            old,
            0..old.len(),
            new,
            0..new.len(),
            |item: &&str| item.starts_with(anchor),
        )
        .unwrap();
        d.into_inner().into_ops()
    }

    fn equal_pairs(ops: &[DiffOp]) -> Vec<(usize, usize)> {
        ops.iter()
            .filter(|op| op.tag() == DiffTag::Equal)
            .flat_map(|op| op.old_range().zip(op.new_range()))
            .collect()
    }

    #[test]
    fn test_anchor_is_kept() {
        let old = ["a", "b", "c", "fn x", "d"];
        let new = ["fn x", "a", "b", "c", "d"];
        for alg in [
            Algorithm::Myers,
            Algorithm::RawMyers,
            Algorithm::Patience,
            Algorithm::Histogram,
            Algorithm::Hunt,
            Algorithm::Lcs,
        ] {
            let ops = anchored(alg, &old, &new, "fn ");
            let pairs = equal_pairs(&ops);
            assert!(pairs.contains(&(3, 0)), "{alg:?}: {ops:?}");
            assert!(pairs.contains(&(4, 4)), "{alg:?}: {ops:?}");
        }
    }

    #[test]
    fn test_non_unique_anchor_is_ignored() {
        let old = ["a", "b", "c", "fn x", "fn x", "d"];
        let new = ["fn x", "a", "b", "c", "d"];
        let ops = anchored(Algorithm::Myers, &old, &new, "fn ");
        let mut plain = Replace::new(Capture::new());
        crate::algorithms::diff_slices(Algorithm::Myers, &mut plain, &old, &new).unwrap();
        assert_eq!(ops, plain.into_inner().into_ops());
    }

    #[test]
    fn test_crossing_anchors_keep_longest_run() {
        let old = ["fn a", "x", "fn b", "y", "fn c"];
        let new = ["fn c", "fn a", "x", "fn b", "y"];
        assert_eq!(
            find_anchors(&old, 0..5, &new, 0..5, |x: &&str| x.starts_with("fn ")),
            vec![(0, 1), (2, 3)]
        );
    }

    #[test]
    fn test_adjacent_anchors_coalesce() {
        let old = ["x", "fn a", "fn b", "y"];
        let new = ["fn a", "fn b", "z"];
        let ops = anchored(Algorithm::Myers, &old, &new, "fn ");
        insta::assert_debug_snapshot!(ops, @r#"
        [
            Delete {
                old_index: 0,
                old_len: 1,
                new_index: 0,
            },
            Equal {
                old_index: 1,
                new_index: 0,
                len: 2,
            },
            Replace {
                old_index: 3,
                old_len: 1,
                new_index: 2,
                new_len: 1,
            },
        ]
        "#);
    }

    #[test]
    fn test_finish_called_once() {
        struct CountFinish(usize);
        impl DiffHook for CountFinish {
            type Error = ();
            fn finish(&mut self) -> Result<(), ()> {
                self.0 += 1;
                Ok(())
            }
        }
        let old = ["a", "fn x", "b", "fn y", "c"];
        let new = ["b", "fn x", "c", "fn y", "a"];
        let mut d = CountFinish(0);
        diff_anchored(
            Algorithm::Patience,
            &mut d,
            &old,
            0..old.len(),
            &new,
            0..new.len(),
            |x: &&str| x.starts_with("fn "),
        )
        .unwrap();
        assert_eq!(d.0, 1);
    }
}

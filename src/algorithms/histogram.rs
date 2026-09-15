//! Histogram diff algorithm.
//!
//! This implementation follows the idea behind Git's histogram diff: find a
//! common region anchored on low-frequency elements and recurse around it.
//! Compared to plain Myers this tends to prefer structurally meaningful anchors
//! over very frequent lines.
//!
//! * time: input-dependent (typically close to Hunt/Patience-style behavior);
//!   worst-case falls back to Myers (`O((N+M)D)`)
//! * space: `O(N + M)` plus match-list/index overhead
//!
//! # Heuristics
//!
//! See [`crate::algorithms`] for shared heuristics and the
//! `diff_deadline_raw` API.

use alloc::vec;
use alloc::vec::Vec;
use core::hash::Hash;
use core::ops::{Index, Range};

use crate::algorithms::utils::{common_prefix_len, common_suffix_len, is_empty_range};
use crate::algorithms::{DiffHook, IdentifyDistinct, NoFinishHook, myers, preflight};
use crate::deadline_support::{Instant, deadline_exceeded};
use crate::types::IntKeyMap;

const MAX_CHAIN_LENGTH: usize = 64;

#[derive(Clone, Copy)]
struct DiffOptions {
    run_preflight: bool,
    use_raw_myers: bool,
}

const HEURISTIC_OPTIONS: DiffOptions = DiffOptions {
    run_preflight: true,
    use_raw_myers: false,
};
const RAW_OPTIONS: DiffOptions = DiffOptions {
    run_preflight: false,
    use_raw_myers: true,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Anchor {
    old_start: usize,
    new_start: usize,
    len: usize,
}

enum SearchResult {
    Anchor(Anchor),
    None,
    Fallback,
}

const NONE: usize = usize::MAX;

/// A flat diagonal table costs eight bytes per item of both sides.  When
/// one side is much larger than the other, the number of diagonals that
/// actually see a match is bounded by the smaller side (times the chain
/// limit), so a sparse map is far cheaper than a table spanning the sum.
const DENSE_COVERED_MAX_RATIO: usize = 8;

/// Furthest new index (plus one) whose equal run was already extended, per
/// diagonal.
///
/// Diagonals are identified by `(old_idx - old_base) + (new_end - new_idx)`.
/// This is purely an optimization: skipping a candidate never changes the
/// chosen anchor because its run was fully extended when first seen.
enum Covered {
    Dense(Vec<usize>),
    Sparse(IntKeyMap<usize, usize>),
}

/// Flat lookup tables shared by every anchor search of one histogram diff.
///
/// The values handed to the search are dense identifiers produced by
/// [`IdentifyDistinct`], so occurrence lists and diagonal coverage can live
/// in plain vectors instead of hash maps.  Each search indexes its old range
/// into the tables and clears exactly the entries it touched afterwards, so
/// the cost per search stays proportional to the searched range.
///
/// Only the old side is indexed, and old values are numbered first by
/// [`IdentifyDistinct`], so the per-value tables are sized by the number of
/// distinct old values rather than the whole domain.
struct Scratch {
    /// First old index holding each old value in the currently indexed range.
    head: Vec<usize>,
    /// Number of occurrences of each old value in the currently indexed range.
    count: Vec<usize>,
    /// Next old index holding the same value, indexed by `old_idx - old_base`.
    next: Vec<usize>,
    covered: Covered,
    touched_diagonals: Vec<usize>,
    old_base: usize,
    new_end: usize,
}

impl Scratch {
    fn new(old_distinct: usize, old_range: &Range<usize>, new_range: &Range<usize>) -> Self {
        debug_assert!(!old_range.is_empty() && !new_range.is_empty());
        let diagonals = old_range.len() + new_range.len() + 1;
        let dense = diagonals <= DENSE_COVERED_MAX_RATIO * old_range.len().min(new_range.len());
        Scratch::with_covered(old_distinct, old_range, new_range, dense)
    }

    fn with_covered(
        old_distinct: usize,
        old_range: &Range<usize>,
        new_range: &Range<usize>,
        dense: bool,
    ) -> Self {
        let covered = if dense {
            Covered::Dense(vec![0; old_range.len() + new_range.len() + 1])
        } else {
            Covered::Sparse(IntKeyMap::default())
        };
        Scratch {
            head: vec![NONE; old_distinct],
            count: vec![0; old_distinct],
            next: vec![NONE; old_range.len()],
            covered,
            touched_diagonals: Vec::new(),
            old_base: old_range.start,
            new_end: new_range.end,
        }
    }

    #[inline(always)]
    fn diagonal(&self, old_idx: usize, new_idx: usize) -> usize {
        (old_idx - self.old_base) + (self.new_end - new_idx)
    }

    #[inline(always)]
    fn covered_end(&self, diagonal: usize) -> usize {
        match &self.covered {
            Covered::Dense(table) => table[diagonal],
            Covered::Sparse(map) => map.get(&diagonal).copied().unwrap_or(0),
        }
    }

    #[inline(always)]
    fn cover(&mut self, diagonal: usize, new_end: usize) {
        let slot = match &mut self.covered {
            Covered::Dense(table) => &mut table[diagonal],
            Covered::Sparse(map) => map.entry(diagonal).or_insert(0),
        };
        if *slot == 0 {
            self.touched_diagonals.push(diagonal);
        }
        *slot = (*slot).max(new_end + 1);
    }

    fn clear_covered(&mut self) {
        match &mut self.covered {
            Covered::Dense(table) => {
                for diagonal in self.touched_diagonals.drain(..) {
                    table[diagonal] = 0;
                }
            }
            Covered::Sparse(map) => {
                for diagonal in self.touched_diagonals.drain(..) {
                    map.remove(&diagonal);
                }
            }
        }
    }
}

/// Histogram diff algorithm.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
pub fn diff<Old, New, D>(
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    diff_deadline(d, old, old_range, new, new_range, None)
}

/// Histogram diff algorithm with deadline.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
///
/// This diff is done with an optional deadline that defines the maximal
/// execution time permitted before it bails and falls back to Myers.
pub fn diff_deadline<Old, New, D>(
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    diff_deadline_impl(
        d,
        old,
        old_range,
        new,
        new_range,
        deadline,
        HEURISTIC_OPTIONS,
    )
}

/// Raw histogram diff algorithm with deadline and without shared heuristics.
pub fn diff_deadline_raw<Old, New, D>(
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, RAW_OPTIONS)
}

fn diff_deadline_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    options: DiffOptions,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    if options.run_preflight
        && preflight::maybe_emit_replace_fast_path(
            d,
            old,
            old_range.clone(),
            new,
            new_range.clone(),
            deadline,
        )?
    {
        return Ok(());
    }

    // Trim before building the shared integer domain. Identical inputs then
    // need no allocation, and sparse edits only remap their changed middle.
    let prefix = common_prefix_len(old, old_range.clone(), new, new_range.clone());
    let old_after_prefix = old_range.start + prefix..old_range.end;
    let new_after_prefix = new_range.start + prefix..new_range.end;
    let suffix = common_suffix_len(old, old_after_prefix.clone(), new, new_after_prefix.clone());
    let old_middle = old_after_prefix.start..old_after_prefix.end - suffix;
    let new_middle = new_after_prefix.start..new_after_prefix.end - suffix;

    if prefix > 0 {
        d.equal(old_range.start, new_range.start, prefix)?;
    }

    // A one sided middle needs neither the integer domain nor the anchor
    // tables; they would be proportional to the remaining side for nothing.
    if old_middle.is_empty() {
        if !new_middle.is_empty() {
            d.insert(old_middle.start, new_middle.start, new_middle.len())?;
        }
    } else if new_middle.is_empty() {
        d.delete(old_middle.start, old_middle.len(), new_middle.start)?;
    } else {
        // Build a shared integer domain so we can use a compact key type while
        // still supporting differing old/new output types.
        let h = IdentifyDistinct::<usize>::new_matching_old(old, old_middle, new, new_middle);
        let mut no_finish_d = NoFinishHook::new(&mut *d);
        diff_deadline_int(
            &mut no_finish_d,
            h.old_lookup(),
            h.old_range(),
            h.new_lookup(),
            h.new_range(),
            h.old_distinct_count(),
            deadline,
            options.use_raw_myers,
        )?;
    }

    if suffix > 0 {
        d.equal(old_range.end - suffix, new_range.end - suffix, suffix)?;
    }

    d.finish()
}

/// Diffs dense integer identifiers where every old value is below
/// `old_distinct`.
#[allow(clippy::too_many_arguments)]
fn diff_deadline_int<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    old_distinct: usize,
    deadline: Option<Instant>,
    use_raw_myers: bool,
) -> Result<(), D::Error>
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
    D: DiffHook,
{
    let mut scratch = Scratch::new(old_distinct, &old_range, &new_range);
    let mut no_finish_d = NoFinishHook::new(d);
    diff_impl(
        &mut no_finish_d,
        old,
        old_range,
        new,
        new_range,
        deadline,
        use_raw_myers,
        &mut scratch,
    )?;
    no_finish_d.into_inner().finish()
}

#[allow(clippy::too_many_arguments)]
fn diff_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    mut old_range: Range<usize>,
    new: &New,
    mut new_range: Range<usize>,
    deadline: Option<Instant>,
    use_raw_myers: bool,
    scratch: &mut Scratch,
) -> Result<(), D::Error>
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
    D: DiffHook,
{
    if is_empty_range(&old_range) && is_empty_range(&new_range) {
        return Ok(());
    }
    if is_empty_range(&old_range) {
        d.insert(old_range.start, new_range.start, new_range.len())?;
        return Ok(());
    }
    if is_empty_range(&new_range) {
        d.delete(old_range.start, old_range.len(), new_range.start)?;
        return Ok(());
    }

    let prefix = common_prefix_len(old, old_range.clone(), new, new_range.clone());
    if prefix > 0 {
        d.equal(old_range.start, new_range.start, prefix)?;
        old_range.start += prefix;
        new_range.start += prefix;
    }

    let suffix = common_suffix_len(old, old_range.clone(), new, new_range.clone());
    let suffix_old_start = old_range.end - suffix;
    let suffix_new_start = new_range.end - suffix;
    old_range.end -= suffix;
    new_range.end -= suffix;

    if is_empty_range(&old_range) {
        if !is_empty_range(&new_range) {
            d.insert(old_range.start, new_range.start, new_range.len())?;
        }
    } else if is_empty_range(&new_range) {
        d.delete(old_range.start, old_range.len(), new_range.start)?;
    } else {
        match find_anchor(
            old,
            old_range.clone(),
            new,
            new_range.clone(),
            deadline,
            scratch,
        ) {
            SearchResult::Anchor(anchor) => {
                let left_old = old_range.start..anchor.old_start;
                let left_new = new_range.start..anchor.new_start;
                diff_impl(
                    d,
                    old,
                    left_old,
                    new,
                    left_new,
                    deadline,
                    use_raw_myers,
                    scratch,
                )?;

                d.equal(anchor.old_start, anchor.new_start, anchor.len)?;

                let right_old = (anchor.old_start + anchor.len)..old_range.end;
                let right_new = (anchor.new_start + anchor.len)..new_range.end;
                diff_impl(
                    d,
                    old,
                    right_old,
                    new,
                    right_new,
                    deadline,
                    use_raw_myers,
                    scratch,
                )?;
            }
            SearchResult::None => {
                d.delete(old_range.start, old_range.len(), new_range.start)?;
                d.insert(old_range.end, new_range.start, new_range.len())?;
            }
            SearchResult::Fallback => {
                let mut myers_hook = NoFinishHook::new(&mut *d);
                if use_raw_myers {
                    myers::diff_deadline_raw(
                        &mut myers_hook,
                        old,
                        old_range.clone(),
                        new,
                        new_range.clone(),
                        deadline,
                    )?;
                } else {
                    myers::diff_deadline(
                        &mut myers_hook,
                        old,
                        old_range.clone(),
                        new,
                        new_range.clone(),
                        deadline,
                    )?;
                }
            }
        }
    }

    if suffix > 0 {
        d.equal(suffix_old_start, suffix_new_start, suffix)?;
    }

    Ok(())
}

fn find_anchor<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    scratch: &mut Scratch,
) -> SearchResult
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
{
    if deadline_exceeded(deadline) {
        return SearchResult::Fallback;
    }

    // Index the old range.  Walking backwards keeps every occurrence chain in
    // increasing index order, matching the insertion order of a map based
    // bucket list.
    for old_idx in old_range.clone().rev() {
        let value = old[old_idx];
        scratch.next[old_idx - scratch.old_base] = scratch.head[value];
        scratch.head[value] = old_idx;
        scratch.count[value] += 1;
    }

    let result = find_anchor_indexed(old, &old_range, new, &new_range, deadline, scratch);

    for old_idx in old_range {
        let value = old[old_idx];
        scratch.head[value] = NONE;
        scratch.count[value] = 0;
    }
    scratch.clear_covered();

    result
}

fn find_anchor_indexed<Old, New>(
    old: &Old,
    old_range: &Range<usize>,
    new: &New,
    new_range: &Range<usize>,
    deadline: Option<Instant>,
    scratch: &mut Scratch,
) -> SearchResult
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
{
    let mut has_common = false;
    let mut best: Option<Anchor> = None;
    let mut best_count = usize::MAX;

    for new_idx in new_range.clone() {
        if deadline_exceeded(deadline) {
            return SearchResult::Fallback;
        }

        let value = new[new_idx];
        // Values that only occur in the new range have identifiers beyond
        // the old tables.
        let Some(&first) = scratch.head.get(value) else {
            continue;
        };
        if first == NONE {
            continue;
        }

        has_common = true;

        // Similar to git's safety valve for overly frequent lines.
        let candidates_len = scratch.count[value];
        if candidates_len > MAX_CHAIN_LENGTH {
            continue;
        }

        let mut old_idx = first;
        while old_idx != NONE {
            let candidate_old_idx = old_idx;
            old_idx = scratch.next[candidate_old_idx - scratch.old_base];

            // Extending every matching item across the same equal run makes
            // nearly identical inputs quadratic.  Remember the furthest
            // covered new index on each diagonal so every equal run is
            // extended only once.
            let diagonal = scratch.diagonal(candidate_old_idx, new_idx);
            if scratch.covered_end(diagonal) > new_idx {
                continue;
            }

            let mut old_start = candidate_old_idx;
            let mut new_start = new_idx;
            let mut old_end = candidate_old_idx;
            let mut new_end = new_idx;
            let mut min_count = candidates_len;

            while old_start > old_range.start
                && new_start > new_range.start
                && old[old_start - 1] == new[new_start - 1]
            {
                old_start -= 1;
                new_start -= 1;
                if (new_start & 1023 == 0) && deadline_exceeded(deadline) {
                    return SearchResult::Fallback;
                }
                let cnt = scratch.count[old[old_start]];
                if cnt < min_count {
                    min_count = cnt;
                }
            }

            while old_end + 1 < old_range.end
                && new_end + 1 < new_range.end
                && old[old_end + 1] == new[new_end + 1]
            {
                old_end += 1;
                new_end += 1;
                if (new_end & 1023 == 0) && deadline_exceeded(deadline) {
                    return SearchResult::Fallback;
                }
                let cnt = scratch.count[old[old_end]];
                if cnt < min_count {
                    min_count = cnt;
                }
            }

            scratch.cover(diagonal, new_end);

            let len = old_end - old_start + 1;
            let candidate = Anchor {
                old_start,
                new_start,
                len,
            };

            let is_better = match best {
                None => true,
                Some(current) => {
                    candidate.len > current.len
                        || (candidate.len == current.len
                            && (min_count < best_count
                                || (min_count == best_count
                                    && (candidate.old_start < current.old_start
                                        || (candidate.old_start == current.old_start
                                            && candidate.new_start < current.new_start)))))
                }
            };

            if is_better {
                best = Some(candidate);
                best_count = min_count;
            }
        }
    }

    if let Some(anchor) = best {
        return SearchResult::Anchor(anchor);
    }

    if has_common {
        SearchResult::Fallback
    } else {
        SearchResult::None
    }
}

#[test]
fn test_scratch_sizing_follows_old_side() {
    // Comparable sides use the flat diagonal table.
    let scratch = Scratch::new(100, &(0..100), &(0..150));
    assert!(matches!(scratch.covered, Covered::Dense(ref table) if table.len() == 251));
    assert_eq!(scratch.head.len(), 100);

    // A tiny old side against a huge new side must not allocate anything
    // proportional to the new side: per-value tables follow the distinct
    // old values and diagonal coverage becomes sparse.
    let scratch = Scratch::new(3, &(0..3), &(0..100_000));
    assert!(matches!(scratch.covered, Covered::Sparse(_)));
    assert_eq!(scratch.head.len(), 3);
    assert_eq!(scratch.count.len(), 3);
    assert_eq!(scratch.next.len(), 3);
}

#[test]
fn test_sparse_and_dense_coverage_agree() {
    use crate::algorithms::Capture;

    let mut seed = 0x1234_5678u64;
    let mut next = || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        seed as usize
    };

    for case in 0..200 {
        // Old values are dense identifiers below `old_distinct`; some new
        // values are new-only and get identifiers beyond that.
        let old_distinct = 1 + case % 12;
        let old_len = 1 + next() % 40;
        let new_len = 1 + next() % if case % 2 == 0 { 40 } else { 400 };
        let old: Vec<usize> = (0..old_len).map(|_| next() % old_distinct).collect();
        let new: Vec<usize> = (0..new_len).map(|_| next() % (old_distinct + 3)).collect();

        let mut results = Vec::new();
        for dense in [true, false] {
            let mut scratch =
                Scratch::with_covered(old_distinct, &(0..old_len), &(0..new_len), dense);
            let mut capture = Capture::new();
            diff_impl(
                &mut capture,
                &old,
                0..old_len,
                &new,
                0..new_len,
                None,
                false,
                &mut scratch,
            )
            .unwrap();
            results.push(capture.into_ops());
        }
        assert_eq!(results[0], results[1], "case {case}");
    }
}

#[test]
fn test_one_sided_middle_skips_indexing() {
    use core::cell::Cell;
    use core::hash::Hasher;

    struct Counted<'a> {
        value: u32,
        hashes: &'a Cell<usize>,
    }

    impl Hash for Counted<'_> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.hashes.set(self.hashes.get() + 1);
            self.value.hash(state);
        }
    }

    impl PartialEq for Counted<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.value == other.value
        }
    }

    impl Eq for Counted<'_> {}

    let hashes = Cell::new(0usize);
    let make = |values: Vec<u32>| -> Vec<Counted<'_>> {
        values
            .into_iter()
            .map(|value| Counted {
                value,
                hashes: &hashes,
            })
            .collect()
    };

    // Pure insertion after a shared prefix, and a pure deletion: after
    // trimming, one side is empty and nothing needs to be hashed.
    let old = make((0..10).collect());
    let new = make((0..10).chain(100..10_100).collect());
    for algorithm in [crate::Algorithm::Histogram, crate::Algorithm::Hunt] {
        hashes.set(0);
        let ops = crate::capture_diff(algorithm, &old, 0..old.len(), &new, 0..new.len());
        assert_eq!(hashes.get(), 0, "{algorithm:?} hashed for an insertion");
        assert_eq!(ops.len(), 2);

        hashes.set(0);
        let ops = crate::capture_diff(algorithm, &new, 0..new.len(), &old, 0..old.len());
        assert_eq!(hashes.get(), 0, "{algorithm:?} hashed for a deletion");
        assert_eq!(ops.len(), 2);
    }
}

#[test]
fn test_diff() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 9, 4];

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, a, b),
        vec![
            DiffOp::Equal {
                old_index: 0,
                new_index: 0,
                len: 3,
            },
            DiffOp::Replace {
                old_index: 3,
                old_len: 1,
                new_index: 3,
                new_len: 1,
            },
            DiffOp::Equal {
                old_index: 4,
                new_index: 4,
                len: 1,
            }
        ]
    );
}

#[test]
fn test_issue44_swapped_regression() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let a: &[usize] = &[0, 1, 4, 5, 8, 9];
    let b: &[usize] = &[0, 1, 3, 4, 5];

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, a, b),
        vec![
            DiffOp::Equal {
                old_index: 0,
                new_index: 0,
                len: 2,
            },
            DiffOp::Insert {
                old_index: 2,
                new_index: 2,
                new_len: 1,
            },
            DiffOp::Equal {
                old_index: 2,
                new_index: 3,
                len: 2,
            },
            DiffOp::Delete {
                old_index: 4,
                old_len: 2,
                new_index: 5,
            },
        ]
    );
}

#[test]
fn test_prefers_low_frequency_anchor_over_noise() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    // The "42" line is a stable rare anchor, while "1" is frequent noise.
    let old = [1, 1, 1, 42, 2, 2, 2];
    let new = [1, 1, 42, 2, 2, 2, 2];

    let ops = capture_diff_slices(Algorithm::Histogram, &old, &new);
    let equal_len = ops
        .iter()
        .map(|op| match *op {
            DiffOp::Equal { len, .. } => len,
            _ => 0,
        })
        .sum::<usize>();

    assert_eq!(equal_len, 6);
}

#[test]
fn test_finish_called() {
    struct HasRunFinish(bool);

    impl DiffHook for HasRunFinish {
        type Error = ();
        fn finish(&mut self) -> Result<(), Self::Error> {
            self.0 = true;
            Ok(())
        }
    }

    let mut d = HasRunFinish(false);
    let slice = &[1, 2];
    let slice2 = &[1, 2, 3];
    diff(&mut d, slice, 0..slice.len(), slice2, 0..slice2.len()).unwrap();
    assert!(d.0);

    let mut d = HasRunFinish(false);
    let slice = &[1, 2];
    diff(&mut d, slice, 0..slice.len(), slice, 0..slice.len()).unwrap();
    assert!(d.0);

    let mut d = HasRunFinish(false);
    let slice: &[u8] = &[];
    diff(&mut d, slice, 0..slice.len(), slice, 0..slice.len()).unwrap();
    assert!(d.0);
}

#[test]
fn test_subrange_regression() {
    use crate::DiffOp;
    use crate::algorithms::Capture;

    let a: &[usize] = &[99, 0, 1, 4, 5, 8, 9, 88];
    let b: &[usize] = &[77, 0, 1, 3, 4, 5, 66];

    let mut d = Capture::new();
    diff(&mut d, a, 1..7, b, 1..6).unwrap();
    assert_eq!(
        d.into_ops(),
        vec![
            DiffOp::Equal {
                old_index: 1,
                new_index: 1,
                len: 2,
            },
            DiffOp::Insert {
                old_index: 3,
                new_index: 3,
                new_len: 1,
            },
            DiffOp::Equal {
                old_index: 3,
                new_index: 4,
                len: 2,
            },
            DiffOp::Delete {
                old_index: 5,
                old_len: 2,
                new_index: 6,
            },
        ]
    );
}

#[test]
fn test_empty_sides() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, &[] as &[u8], &[1u8, 2]),
        vec![DiffOp::Insert {
            old_index: 0,
            new_index: 0,
            new_len: 2,
        }]
    );

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, &[1u8, 2], &[] as &[u8]),
        vec![DiffOp::Delete {
            old_index: 0,
            old_len: 2,
            new_index: 0,
        }]
    );

    assert!(capture_diff_slices(Algorithm::Histogram, &[] as &[u8], &[] as &[u8]).is_empty());
}

#[test]
fn test_identical_input() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, &[1, 2, 3], &[1, 2, 3]),
        vec![DiffOp::Equal {
            old_index: 0,
            new_index: 0,
            len: 3,
        }]
    );
}

#[test]
fn test_no_common_elements() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    assert_eq!(
        capture_diff_slices(Algorithm::Histogram, &[1, 2, 3], &[4, 5]),
        vec![DiffOp::Replace {
            old_index: 0,
            old_len: 3,
            new_index: 0,
            new_len: 2,
        }]
    );
}

#[cfg(feature = "std")]
#[test]
fn test_deadline_fallback() {
    use std::ops::Index;
    use std::time::Duration;

    use crate::algorithms::{Capture, Replace};

    let a = (0..64).collect::<Vec<_>>();
    let mut b = a.clone();
    b[10] = 999;
    b[40] = 999;

    struct SlowIndex<'a>(&'a [usize]);

    impl Index<usize> for SlowIndex<'_> {
        type Output = usize;

        fn index(&self, index: usize) -> &Self::Output {
            std::thread::sleep(Duration::from_millis(1));
            &self.0[index]
        }
    }

    let slow_a = SlowIndex(&a);
    let slow_b = SlowIndex(&b);

    let mut d = Replace::new(Capture::new());
    diff_deadline(
        &mut d,
        &slow_a,
        0..a.len(),
        &slow_b,
        0..b.len(),
        Some(Instant::now() + Duration::from_millis(5)),
    )
    .unwrap();

    let ops = d.into_inner().into_ops();
    assert!(!ops.is_empty());
}

#[test]
fn test_cross_type_lookup_compatibility() {
    use crate::{Algorithm, DiffOp, capture_diff};
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
            self.0.hash(state);
        }
    }

    impl PartialEq<A> for B {
        fn eq(&self, other: &A) -> bool {
            self.0 == other.0
        }
    }

    let old = [A(1), A(2), A(3), A(4)];
    let new = [B(1), B(2), B(9), B(4)];

    let ops = capture_diff(Algorithm::Histogram, &old, 0..old.len(), &new, 0..new.len());
    let equal_len = ops
        .iter()
        .map(|op| match op {
            DiffOp::Equal { len, .. } => *len,
            _ => 0,
        })
        .sum::<usize>();

    assert_eq!(equal_len, 3);
}

#[test]
fn test_anchor_search_scans_equal_runs_once() {
    use core::cell::Cell;

    struct CountingLookup<'a> {
        values: &'a [usize],
        lookups: Cell<usize>,
    }

    impl Index<usize> for CountingLookup<'_> {
        type Output = usize;

        fn index(&self, index: usize) -> &Self::Output {
            self.lookups.set(self.lookups.get() + 1);
            &self.values[index]
        }
    }

    let size = 2048;
    let old_values = (0..size).collect::<Vec<_>>();
    let mut new_values = old_values.clone();
    new_values[size / 4] = size + 1;
    new_values[size / 2] = size + 2;
    new_values[size * 3 / 4] = size + 3;
    let old = CountingLookup {
        values: &old_values,
        lookups: Cell::new(0),
    };
    let new = CountingLookup {
        values: &new_values,
        lookups: Cell::new(0),
    };

    let mut scratch = Scratch::new(size, &(0..size), &(0..size));
    assert!(matches!(
        find_anchor(&old, 0..size, &new, 0..size, None, &mut scratch),
        SearchResult::Anchor(_)
    ));

    // Re-extending the run from every matching item used to require millions
    // of lookups for this input. The diagonal coverage cache keeps it linear.
    assert!(old.lookups.get() + new.lookups.get() < size * 50);
}

#[test]
fn test_deterministic_tie_breaking() {
    use crate::{Algorithm, capture_diff_slices};

    let old = [1, 2, 1, 2, 3, 4, 3, 4];
    let new = [1, 1, 2, 2, 3, 3, 4, 4];

    let first = capture_diff_slices(Algorithm::Histogram, &old, &new);
    for _ in 0..8 {
        let next = capture_diff_slices(Algorithm::Histogram, &old, &new);
        assert_eq!(first, next);
    }
}

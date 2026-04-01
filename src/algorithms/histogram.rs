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

use std::collections::HashMap;
use std::ops::{Index, Range};

use crate::algorithms::utils::{common_prefix_len, common_suffix_len, is_empty_range};
use crate::algorithms::{DiffHook, IdentifyDistinct, NoFinishHook, myers, preflight};
use crate::deadline_support::{Instant, deadline_exceeded};

const MAX_CHAIN_LENGTH: usize = 64;

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
    Old::Output: std::hash::Hash + Eq,
    New::Output: PartialEq<Old::Output> + std::hash::Hash + Eq,
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
    Old::Output: std::hash::Hash + Eq,
    New::Output: PartialEq<Old::Output> + std::hash::Hash + Eq,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, true, false)
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
    Old::Output: std::hash::Hash + Eq,
    New::Output: PartialEq<Old::Output> + std::hash::Hash + Eq,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, false, true)
}

fn diff_deadline_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    run_preflight: bool,
    use_raw_myers: bool,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: std::hash::Hash + Eq,
    New::Output: PartialEq<Old::Output> + std::hash::Hash + Eq,
{
    if run_preflight
        && preflight::maybe_emit_disjoint_fast_path(
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

    // Build a shared integer domain so we can use a compact key type while
    // still supporting differing old/new output types.
    let h = IdentifyDistinct::<usize>::new(old, old_range, new, new_range);
    diff_deadline_int(
        d,
        h.old_lookup(),
        h.old_range(),
        h.new_lookup(),
        h.new_range(),
        deadline,
        use_raw_myers,
    )
}

fn diff_deadline_int<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    use_raw_myers: bool,
) -> Result<(), D::Error>
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
    D: DiffHook,
{
    let mut no_finish_d = NoFinishHook::new(d);
    diff_impl(
        &mut no_finish_d,
        old,
        old_range,
        new,
        new_range,
        deadline,
        use_raw_myers,
    )?;
    no_finish_d.into_inner().finish()
}

fn diff_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    mut old_range: Range<usize>,
    new: &New,
    mut new_range: Range<usize>,
    deadline: Option<Instant>,
    use_raw_myers: bool,
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
        match find_anchor(old, old_range.clone(), new, new_range.clone(), deadline) {
            SearchResult::Anchor(anchor) => {
                let left_old = old_range.start..anchor.old_start;
                let left_new = new_range.start..anchor.new_start;
                diff_impl(d, old, left_old, new, left_new, deadline, use_raw_myers)?;

                d.equal(anchor.old_start, anchor.new_start, anchor.len)?;

                let right_old = (anchor.old_start + anchor.len)..old_range.end;
                let right_new = (anchor.new_start + anchor.len)..new_range.end;
                diff_impl(d, old, right_old, new, right_new, deadline, use_raw_myers)?;
            }
            SearchResult::None => {
                d.delete(old_range.start, old_range.len(), new_range.start)?;
                d.insert(old_range.start, new_range.start, new_range.len())?;
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
) -> SearchResult
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
{
    if deadline_exceeded(deadline) {
        return SearchResult::Fallback;
    }

    let mut old_positions = HashMap::<usize, Vec<usize>>::new();
    let mut old_counts = HashMap::<usize, usize>::new();

    for old_idx in old_range.clone() {
        let value = old[old_idx];
        old_positions.entry(value).or_default().push(old_idx);
        *old_counts.entry(value).or_insert(0) += 1;
    }

    let mut has_common = false;
    let mut best: Option<Anchor> = None;
    let mut best_count = usize::MAX;

    for new_idx in new_range.clone() {
        if deadline_exceeded(deadline) {
            return SearchResult::Fallback;
        }

        let value = new[new_idx];
        let Some(candidates) = old_positions.get(&value) else {
            continue;
        };

        has_common = true;

        // Similar to git's safety valve for overly frequent lines.
        if candidates.len() > MAX_CHAIN_LENGTH {
            continue;
        }

        for &old_idx in candidates {
            let mut old_start = old_idx;
            let mut new_start = new_idx;
            let mut old_end = old_idx;
            let mut new_end = new_idx;
            let mut min_count = candidates.len();

            while old_start > old_range.start
                && new_start > new_range.start
                && old[old_start - 1] == new[new_start - 1]
            {
                old_start -= 1;
                new_start -= 1;
                let cnt = old_counts[&old[old_start]];
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
                let cnt = old_counts[&old[old_end]];
                if cnt < min_count {
                    min_count = cnt;
                }
            }

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

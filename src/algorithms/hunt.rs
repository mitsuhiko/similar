//! Hunt-style diff algorithm.
//!
//! This implementation follows the classic Hunt family approach by building
//! match lists for the new sequence and then tracking k-candidates with a
//! threshold array and backpointers.
//!
//! In practice this is close to the Hunt–Szymanski variant (descending match
//! list processing).
//!
//! * time: `O((N + R) log L)` where `R` is the number of matching pairs
//! * space: `O(N + M + R)`

use std::collections::HashMap;
use std::ops::{Index, Range};

use crate::algorithms::utils::{common_prefix_len, common_suffix_len, is_empty_range};
use crate::algorithms::{myers, DiffHook, IdentifyDistinct, NoFinishHook};
use crate::deadline_support::{deadline_exceeded, Instant};

#[derive(Clone, Copy)]
struct Candidate {
    old_index: usize,
    new_index: usize,
    prev: Option<usize>,
}

/// Hunt-style diff algorithm.
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

/// Hunt-style diff algorithm with deadline.
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
    // Build a shared integer domain first so we can use a compact key type for
    // match lists while still supporting old/new lookups of different types.
    let h = IdentifyDistinct::<usize>::new(old, old_range, new, new_range);
    diff_deadline_int(
        d,
        h.old_lookup(),
        h.old_range(),
        h.new_lookup(),
        h.new_range(),
        deadline,
    )
}

fn diff_deadline_int<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize, Output = usize> + ?Sized,
    New: Index<usize, Output = usize> + ?Sized,
    D: DiffHook,
{
    if is_empty_range(&new_range) {
        d.delete(old_range.start, old_range.len(), new_range.start)?;
        d.finish()?;
        return Ok(());
    } else if is_empty_range(&old_range) {
        d.insert(old_range.start, new_range.start, new_range.len())?;
        d.finish()?;
        return Ok(());
    }

    let common_prefix_len = common_prefix_len(old, old_range.clone(), new, new_range.clone());
    let common_suffix_len = common_suffix_len(
        old,
        old_range.start + common_prefix_len..old_range.end,
        new,
        new_range.start + common_prefix_len..new_range.end,
    );

    if common_prefix_len == old_range.len() && (old_range.len() == new_range.len()) {
        d.equal(old_range.start, new_range.start, old_range.len())?;
        d.finish()?;
        return Ok(());
    }

    let old_mid_range =
        (old_range.start + common_prefix_len)..(old_range.end.saturating_sub(common_suffix_len));
    let new_mid_range =
        (new_range.start + common_prefix_len)..(new_range.end.saturating_sub(common_suffix_len));

    if common_prefix_len > 0 {
        d.equal(old_range.start, new_range.start, common_prefix_len)?;
    }

    if is_empty_range(&old_mid_range) {
        if !is_empty_range(&new_mid_range) {
            d.insert(
                old_mid_range.start,
                new_mid_range.start,
                new_mid_range.len(),
            )?;
        }
    } else if is_empty_range(&new_mid_range) {
        d.delete(
            old_mid_range.start,
            old_mid_range.len(),
            new_mid_range.start,
        )?;
    } else if let Some(match_list) = build_match_list(new, new_mid_range.clone(), deadline) {
        if let Some(anchors) = hunt_anchors(old, old_mid_range.clone(), &match_list, deadline) {
            emit_anchored_script(
                d,
                old_mid_range.start,
                old_mid_range.end,
                new_mid_range.start,
                new_mid_range.end,
                &anchors,
            )?;
        } else {
            let mut no_finish_d = NoFinishHook::new(&mut *d);
            myers::diff_deadline(
                &mut no_finish_d,
                old,
                old_mid_range,
                new,
                new_mid_range,
                deadline,
            )?;
        }
    } else {
        let mut no_finish_d = NoFinishHook::new(&mut *d);
        myers::diff_deadline(
            &mut no_finish_d,
            old,
            old_mid_range,
            new,
            new_mid_range,
            deadline,
        )?;
    }

    if common_suffix_len > 0 {
        d.equal(
            old_range.end - common_suffix_len,
            new_range.end - common_suffix_len,
            common_suffix_len,
        )?;
    }

    d.finish()
}

fn build_match_list<New>(
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<HashMap<usize, Vec<usize>>>
where
    New: Index<usize, Output = usize> + ?Sized,
{
    let mut rv = HashMap::new();
    for new_index in new_range {
        if deadline_exceeded(deadline) {
            return None;
        }
        rv.entry(new[new_index])
            .or_insert_with(Vec::new)
            .push(new_index);
    }
    Some(rv)
}

fn lower_bound(slice: &[usize], value: usize) -> usize {
    let mut lo = 0;
    let mut hi = slice.len();
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if slice[mid] < value {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    lo
}

fn hunt_anchors<Old>(
    old: &Old,
    old_range: Range<usize>,
    match_list: &HashMap<usize, Vec<usize>>,
    deadline: Option<Instant>,
) -> Option<Vec<(usize, usize)>>
where
    Old: Index<usize, Output = usize> + ?Sized,
{
    let mut thresh = Vec::new();
    let mut links = Vec::new();
    let mut candidates = Vec::new();

    for old_index in old_range {
        if deadline_exceeded(deadline) {
            return None;
        }

        if let Some(new_indexes) = match_list.get(&old[old_index]) {
            for &new_index in new_indexes.iter().rev() {
                let k = lower_bound(&thresh, new_index);

                if k == thresh.len() || new_index < thresh[k] {
                    let prev = if k > 0 { Some(links[k - 1]) } else { None };
                    let candidate_index = candidates.len();
                    candidates.push(Candidate {
                        old_index,
                        new_index,
                        prev,
                    });

                    if k == thresh.len() {
                        thresh.push(new_index);
                        links.push(candidate_index);
                    } else {
                        thresh[k] = new_index;
                        links[k] = candidate_index;
                    }
                }
            }
        }
    }

    let mut anchors = Vec::with_capacity(links.len());
    let mut ptr = if let Some(ptr) = links.last().copied() {
        ptr
    } else {
        return Some(anchors);
    };

    loop {
        let c = candidates[ptr];
        anchors.push((c.old_index, c.new_index));
        if let Some(prev) = c.prev {
            ptr = prev;
        } else {
            break;
        }
    }

    anchors.reverse();
    Some(anchors)
}

fn emit_anchored_script<D>(
    d: &mut D,
    old_start: usize,
    old_end: usize,
    new_start: usize,
    new_end: usize,
    anchors: &[(usize, usize)],
) -> Result<(), D::Error>
where
    D: DiffHook,
{
    let mut old_index = old_start;
    let mut new_index = new_start;
    let mut anchor_ptr = 0;

    while anchor_ptr < anchors.len() {
        let run_old_start = anchors[anchor_ptr].0;
        let run_new_start = anchors[anchor_ptr].1;

        if old_index < run_old_start {
            d.delete(old_index, run_old_start - old_index, new_index)?;
            old_index = run_old_start;
        }

        if new_index < run_new_start {
            d.insert(old_index, new_index, run_new_start - new_index)?;
        }

        let mut run_len = 1;
        anchor_ptr += 1;
        while anchor_ptr < anchors.len()
            && anchors[anchor_ptr].0 == run_old_start + run_len
            && anchors[anchor_ptr].1 == run_new_start + run_len
        {
            run_len += 1;
            anchor_ptr += 1;
        }

        d.equal(run_old_start, run_new_start, run_len)?;
        old_index = run_old_start + run_len;
        new_index = run_new_start + run_len;
    }

    if old_index < old_end {
        d.delete(old_index, old_end - old_index, new_index)?;
        old_index = old_end;
    }

    if new_index < new_end {
        d.insert(old_index, new_index, new_end - new_index)?;
    }

    Ok(())
}

#[test]
fn test_diff() {
    use crate::{capture_diff_slices, Algorithm, DiffOp};

    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 9, 4];

    assert_eq!(
        capture_diff_slices(Algorithm::Hunt, a, b),
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
    use crate::{capture_diff_slices, Algorithm, DiffOp};

    let a: &[usize] = &[0, 1, 4, 5, 8, 9];
    let b: &[usize] = &[0, 1, 3, 4, 5];

    assert_eq!(
        capture_diff_slices(Algorithm::Hunt, a, b),
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
fn test_subrange_regression() {
    use crate::algorithms::Capture;
    use crate::DiffOp;

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
fn test_lcs_length_matches_classic_lcs() {
    use crate::{capture_diff_slices, Algorithm, DiffOp};

    fn equal_len(ops: &[DiffOp]) -> usize {
        ops.iter()
            .map(|op| match *op {
                DiffOp::Equal { len, .. } => len,
                _ => 0,
            })
            .sum()
    }

    let cases = [
        (vec![1, 2, 3, 4, 5], vec![1, 2, 9, 4, 5]),
        (vec![1, 2, 1, 2, 1, 2], vec![2, 1, 2, 1, 2, 1]),
        (vec![0, 1, 4, 5, 8, 9], vec![0, 1, 3, 4, 5]),
        (vec![5, 1, 2, 3, 4, 5, 6], vec![1, 2, 7, 3, 4, 8, 6]),
    ];

    for (a, b) in cases {
        let hunt = capture_diff_slices(Algorithm::Hunt, &a, &b);
        let lcs = capture_diff_slices(Algorithm::Lcs, &a, &b);
        assert_eq!(equal_len(&hunt), equal_len(&lcs));
    }
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

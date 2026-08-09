//! Classic LCS table diff algorithm.
//!
//! This implementation builds an LCS table for the compared ranges and then
//! walks it forward to emit operations.
//! * time: `O(N*M)`
//! * space `O(N*M)`
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
use crate::algorithms::{DiffHook, preflight};
use crate::deadline_support::{Instant, deadline_exceeded};

/// Classic LCS table diff algorithm.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
///
/// This diff is done with an optional deadline that defines the maximal
/// execution time permitted before it bails and falls back to an very bad
/// approximation.  Deadlines with LCS do not make a lot of sense and should
/// not be used.
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

/// Classic LCS table diff algorithm.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
///
/// This diff is done with an optional deadline that defines the maximal
/// execution time permitted before it bails and falls back to an approximation.
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
    if preflight::maybe_emit_replace_fast_path(
        d,
        old,
        old_range.clone(),
        new,
        new_range.clone(),
        deadline,
    )? {
        return Ok(());
    }

    diff_deadline_impl(d, old, old_range, new, new_range, deadline)
}

/// Raw classic LCS table diff algorithm with deadline and without shared
/// heuristics.
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
    New::Output: PartialEq<Old::Output>,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline)
}

fn diff_deadline_impl<Old, New, D>(
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
    New::Output: PartialEq<Old::Output>,
{
    if is_empty_range(&old_range) && is_empty_range(&new_range) {
        d.finish()?;
        return Ok(());
    } else if is_empty_range(&new_range) {
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

    // If the sequences are not different then we're done
    if common_prefix_len == old_range.len() && (old_range.len() == new_range.len()) {
        d.equal(old_range.start, new_range.start, old_range.len())?;
        d.finish()?;
        return Ok(());
    }

    let maybe_table = make_table(
        old,
        (old_range.start + common_prefix_len)..(old_range.end - common_suffix_len),
        new,
        (new_range.start + common_prefix_len)..(new_range.end - common_suffix_len),
        deadline,
    );
    let mut old_idx = 0;
    let mut new_idx = 0;
    let new_len = new_range.len() - common_prefix_len - common_suffix_len;
    let old_len = old_range.len() - common_prefix_len - common_suffix_len;

    if common_prefix_len > 0 {
        d.equal(old_range.start, new_range.start, common_prefix_len)?;
    }

    if let Some(table) = maybe_table {
        while new_idx < new_len && old_idx < old_len {
            let old_orig_idx = old_range.start + common_prefix_len + old_idx;
            let new_orig_idx = new_range.start + common_prefix_len + new_idx;

            if new[new_orig_idx] == old[old_orig_idx] {
                d.equal(old_orig_idx, new_orig_idx, 1)?;
                old_idx += 1;
                new_idx += 1;
            } else if table.get(new_idx, old_idx + 1) >= table.get(new_idx + 1, old_idx) {
                d.delete(old_orig_idx, 1, new_orig_idx)?;
                old_idx += 1;
            } else {
                d.insert(old_orig_idx, new_orig_idx, 1)?;
                new_idx += 1;
            }
        }
    }

    // Emit any unconsumed tail. If table construction exceeded the deadline,
    // both cursors are still zero and this becomes the fallback edit script.
    if old_idx < old_len {
        d.delete(
            old_range.start + common_prefix_len + old_idx,
            old_len - old_idx,
            new_range.start + common_prefix_len + new_idx,
        )?;
        old_idx += old_len - old_idx;
    }

    if new_idx < new_len {
        d.insert(
            old_range.start + common_prefix_len + old_idx,
            new_range.start + common_prefix_len + new_idx,
            new_len - new_idx,
        )?;
    }

    if common_suffix_len > 0 {
        d.equal(
            old_range.start + old_len + common_prefix_len,
            new_range.start + new_len + common_prefix_len,
            common_suffix_len,
        )?;
    }

    d.finish()
}

#[derive(Clone, Copy)]
enum LcsTableLayout {
    Forward,
    ReverseRows {
        old_len: usize,
        new_len: usize,
        transposed: bool,
    },
}

struct LcsTable {
    width: usize,
    values: Vec<u32>,
    layout: LcsTableLayout,
}

impl LcsTable {
    #[inline(always)]
    fn get(&self, new_index: usize, old_index: usize) -> u32 {
        let index = match self.layout {
            LcsTableLayout::Forward => new_index * self.width + old_index,
            LcsTableLayout::ReverseRows {
                new_len,
                transposed: false,
                ..
            } => (new_len - new_index) * self.width + old_index,
            LcsTableLayout::ReverseRows {
                old_len,
                transposed: true,
                ..
            } => (old_len - old_index) * self.width + new_index,
        };
        self.values[index]
    }
}

const TABLE_DEADLINE_CHECK_INTERVAL: usize = 1024;

fn append_zeroed_row(values: &mut Vec<u32>, width: usize) -> usize {
    let row = values.len();
    let new_len = row.checked_add(width).expect("LCS table capacity overflow");
    // Keep allocation failure behavior consistent with normal Vec growth.
    // A deadline controls computation time; it must not hide allocation errors.
    values.resize(new_len, 0);
    row
}

fn make_table_incremental<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<LcsTable>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let old_len = old_range.len();
    let new_len = new_range.len();
    // Keep each incremental row on the shorter axis. This bounds the amount
    // allocated and zeroed between deadline checks for unbalanced inputs.
    let transposed = new_len < old_len;
    let column_len = if transposed { new_len } else { old_len };
    let row_len = if transposed { old_len } else { new_len };
    let width = column_len.checked_add(1).expect("LCS table width overflow");
    let mut values = Vec::new();
    append_zeroed_row(&mut values, width);

    for row_index in (0..row_len).rev() {
        if deadline_exceeded(deadline) {
            return None;
        }

        let next_row = values.len() - width;
        let row = append_zeroed_row(&mut values, width);
        for column_index in (0..column_len).rev() {
            if (column_index & (TABLE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                && deadline_exceeded(deadline)
            {
                return None;
            }

            let equal = if transposed {
                new[new_range.start + column_index] == old[old_range.start + row_index]
            } else {
                new[new_range.start + row_index] == old[old_range.start + column_index]
            };
            values[row + column_index] = if equal {
                values[next_row + column_index + 1] + 1
            } else {
                values[next_row + column_index].max(values[row + column_index + 1])
            };
        }
    }

    Some(LcsTable {
        width,
        values,
        layout: LcsTableLayout::ReverseRows {
            old_len,
            new_len,
            transposed,
        },
    })
}

fn make_table<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<LcsTable>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if deadline_exceeded(deadline) {
        return None;
    }

    let old_len = old_range.len();
    let new_len = new_range.len();
    if old_len == 0 || new_len == 0 {
        return None;
    }

    if deadline.is_some() {
        return make_table_incremental(old, old_range, new, new_range, deadline);
    }

    let width = old_len.checked_add(1).expect("LCS table width overflow");
    let height = new_len.checked_add(1).expect("LCS table height overflow");
    let cell_count = width
        .checked_mul(height)
        .expect("LCS table capacity overflow");
    let mut values = vec![0u32; cell_count];

    for i in (0..new_len).rev() {
        let row = i * width;
        let next_row = (i + 1) * width;
        for j in (0..old_len).rev() {
            values[row + j] = if new[new_range.start + i] == old[old_range.start + j] {
                values[next_row + j + 1] + 1
            } else {
                values[next_row + j].max(values[row + j + 1])
            };
        }
    }

    Some(LcsTable {
        width,
        values,
        layout: LcsTableLayout::Forward,
    })
}

#[test]
fn test_empty_table_dimension_does_not_allocate() {
    let values = [0u8];
    assert!(make_table(&values, 0..0, &values, 0..usize::MAX, None).is_none());
    assert!(make_table(&values, 0..usize::MAX, &values, 0..0, None).is_none());
}

#[test]
#[should_panic(expected = "LCS table capacity overflow")]
fn test_table_size_overflow_panics() {
    let values = [0u8];
    let old_len = usize::MAX / 2;
    let _ = make_table(&values, 0..old_len, &values, 0..2, None);
}

#[test]
fn test_table() {
    let old = vec![2, 3];
    let new = vec![0, 1, 2];
    let table = make_table(&old, 0..old.len(), &new, 0..new.len(), None).unwrap();
    assert_eq!(table.width, 3);
    assert_eq!(table.get(0, 0), 1);
    assert_eq!(table.get(1, 0), 1);
    assert_eq!(table.get(2, 0), 1);
    assert_eq!(table.get(3, 0), 0);
}

#[cfg(feature = "std")]
#[test]
fn test_incremental_table_matches_contiguous_in_both_orientations() {
    use core::time::Duration;

    for (old, new) in [
        (vec![1, 2, 3], vec![0, 1, 3, 4, 5]),
        (vec![0, 1, 3, 4, 5], vec![1, 2, 3]),
    ] {
        let contiguous = make_table(&old, 0..old.len(), &new, 0..new.len(), None).unwrap();
        let incremental = make_table(
            &old,
            0..old.len(),
            &new,
            0..new.len(),
            Some(Instant::now() + Duration::from_secs(1)),
        )
        .unwrap();

        for new_index in 0..=new.len() {
            for old_index in 0..=old.len() {
                assert_eq!(
                    incremental.get(new_index, old_index),
                    contiguous.get(new_index, old_index)
                );
            }
        }
    }
}

#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
#[test]
fn test_deadline_table_allocation_is_incremental() {
    use core::time::Duration;

    let old = (0..511u32).collect::<Vec<_>>();
    let new = (0..1_000_000u32).map(|value| value + 1).collect::<Vec<_>>();
    let table = make_table(
        &old,
        0..old.len(),
        &new,
        0..new.len(),
        Some(Instant::now() + Duration::from_millis(1)),
    );

    assert!(table.is_none());
}

#[test]
fn test_diff() {
    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 9, 4];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_raw_accepts_partialeq_only_values() {
    let old = [1.0f32, 2.0, 3.0];
    let new = [1.0f32, 4.0, 3.0];

    let mut d = crate::algorithms::Capture::new();
    diff_deadline_raw(&mut d, &old, 0..old.len(), &new, 0..new.len(), None).unwrap();

    assert!(!d.ops().is_empty());
}

#[cfg(feature = "std")]
#[test]
fn test_deadline_fallback_issue_97() {
    use core::time::Duration;

    use crate::DiffOp;
    use crate::algorithms::Capture;

    let old = [1u32, 2];
    let new = [3u32, 4];
    let deadline = Instant::now()
        .checked_sub(Duration::from_millis(1))
        .unwrap();
    let mut d = Capture::new();

    diff_deadline_raw(
        &mut d,
        &old,
        0..old.len(),
        &new,
        0..new.len(),
        Some(deadline),
    )
    .unwrap();

    assert_eq!(
        d.into_ops(),
        vec![
            DiffOp::Delete {
                old_index: 0,
                old_len: 2,
                new_index: 0,
            },
            DiffOp::Insert {
                old_index: 2,
                new_index: 0,
                new_len: 2,
            },
        ]
    );
}

#[test]
fn test_contiguous() {
    let a: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];
    let b: &[usize] = &[0, 1, 2, 8, 9, 4, 4, 7];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_pat() {
    let a: &[usize] = &[0, 1, 3, 4, 5];
    let b: &[usize] = &[0, 1, 4, 5, 8, 9];

    let mut d = crate::algorithms::Capture::new();
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.ops());
}

#[test]
fn test_issue44_swapped_regression() {
    use crate::DiffOp;

    let a: &[usize] = &[0, 1, 4, 5, 8, 9];
    let b: &[usize] = &[0, 1, 3, 4, 5];

    let mut d = crate::algorithms::Capture::new();
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    assert_eq!(
        d.into_ops(),
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
                len: 1,
            },
            DiffOp::Equal {
                old_index: 3,
                new_index: 4,
                len: 1,
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
    use crate::DiffOp;

    let a: &[usize] = &[99, 0, 1, 4, 5, 8, 9, 88];
    let b: &[usize] = &[77, 0, 1, 3, 4, 5, 66];

    let mut d = crate::algorithms::Capture::new();
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
                len: 1,
            },
            DiffOp::Equal {
                old_index: 4,
                new_index: 5,
                len: 1,
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
fn test_identical_subrange_issue_98() {
    use crate::{Algorithm, ChangeTag, DiffOp, capture_diff};

    let old = ["a", "b", "c"];
    let new = ["z", "b", "c"];
    let ops = capture_diff(Algorithm::Lcs, &old, 1..3, &new, 1..3);

    assert_eq!(
        ops,
        vec![DiffOp::Equal {
            old_index: 1,
            new_index: 1,
            len: 2,
        }]
    );
    assert_eq!(
        ops.iter()
            .flat_map(|op| op.iter_changes(&old, &new))
            .map(|change| (change.tag(), change.value()))
            .collect::<Vec<_>>(),
        vec![(ChangeTag::Equal, "b"), (ChangeTag::Equal, "c")]
    );
}

#[test]
fn test_same() {
    let a: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];
    let b: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];

    let mut d = crate::algorithms::Capture::new();
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.ops());
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
fn test_bad_range_regression() {
    use crate::DiffOp;
    use crate::algorithms::Capture;
    let mut d = Capture::new();
    diff(&mut d, &[0], 0..1, &[0, 0], 0..2).unwrap();
    assert_eq!(
        d.into_ops(),
        vec![
            DiffOp::Equal {
                old_index: 0,
                new_index: 0,
                len: 1
            },
            DiffOp::Insert {
                old_index: 1,
                new_index: 1,
                new_len: 1
            }
        ]
    );
}

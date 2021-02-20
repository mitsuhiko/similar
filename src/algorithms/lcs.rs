//! Hunt–McIlroy / Hunt–Szymanski LCS diff algorithm.
//!
//! * time: `O((NM)D log (M)D)`
//! * space `O(MN)`
use std::collections::BTreeMap;
use std::ops::{Index, Range};
use std::time::Instant;

use crate::algorithms::DiffHook;

/// Hunt–McIlroy / Hunt–Szymanski LCS diff algorithm.
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
    New::Output: PartialEq<Old::Output>,
{
    diff_deadline(d, old, old_range, new, new_range, None)
}

/// Hunt–McIlroy / Hunt–Szymanski LCS diff algorithm.
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
    New::Output: PartialEq<Old::Output>,
{
    if new_range.start >= new_range.end {
        d.delete(old_range.start, old_range.len(), new_range.start)?;
        return Ok(());
    } else if old_range.start >= old_range.end {
        d.insert(old_range.start, new_range.start, new_range.len())?;
        return Ok(());
    }

    let prefix_len = old_range
        .clone()
        .zip(new_range.clone())
        .take_while(|x| new[x.1] == old[x.0])
        .count();
    let suffix_len = old_range
        .clone()
        .rev()
        .zip(new_range.clone().rev())
        .take(old_range.len().min(new_range.len()) - prefix_len)
        .take_while(|x| new[x.1] == old[x.0])
        .count();

    let maybe_table = make_table(
        old,
        prefix_len..(old_range.len() - suffix_len),
        new,
        prefix_len..(new_range.len() - suffix_len),
        deadline,
    );
    let mut old_idx = 0;
    let mut new_idx = 0;
    let new_len = new_range.len() - prefix_len - suffix_len;
    let old_len = old_range.len() - prefix_len - suffix_len;

    if prefix_len > 0 {
        d.equal(old_range.start, new_range.start, prefix_len)?;
    }

    if let Some(table) = maybe_table {
        while new_idx < new_len && old_idx < old_len {
            let old_orig_idx = old_range.start + prefix_len + old_idx;
            let new_orig_idx = new_range.start + prefix_len + new_idx;

            if new[new_orig_idx] == old[old_orig_idx] {
                d.equal(old_orig_idx, new_orig_idx, 1)?;
                old_idx += 1;
                new_idx += 1;
            } else if table.get(&(new_idx, old_idx + 1)).map_or(0, |&x| x)
                >= table.get(&(new_idx + 1, old_idx)).map_or(0, |&x| x)
            {
                d.delete(old_orig_idx, 1, new_orig_idx)?;
                old_idx += 1;
            } else {
                d.insert(old_orig_idx, new_orig_idx, 1)?;
                new_idx += 1;
            }
        }
    } else {
        let old_orig_idx = old_range.start + prefix_len + old_idx;
        let new_orig_idx = new_range.start + prefix_len + new_idx;
        d.delete(old_orig_idx, old_len, new_orig_idx)?;
        d.insert(old_orig_idx, new_orig_idx, new_len)?;
    }

    if old_idx < old_len {
        d.delete(
            old_range.start + prefix_len + old_idx,
            old_len - old_idx,
            new_range.start + prefix_len + new_idx,
        )?;
        old_idx += old_len - old_idx;
    }

    if new_idx < new_len {
        d.insert(
            old_range.start + prefix_len + old_idx,
            new_range.start + prefix_len + new_idx,
            new_len - new_idx,
        )?;
    }

    if suffix_len > 0 {
        d.equal(
            old_range.start + old_len + prefix_len,
            new_range.start + new_len + prefix_len,
            suffix_len,
        )?;
    }

    d.finish()
}

/// Shortcut for diffing slices.
#[deprecated(
    since = "1.4.0",
    note = "slice utility function is now only available via similar::algorithms::diff_slices"
)]
pub fn diff_slices<D, T>(d: &mut D, old: &[T], new: &[T]) -> Result<(), D::Error>
where
    D: DiffHook,
    T: PartialEq,
{
    diff(d, old, 0..old.len(), new, 0..new.len())
}

fn make_table<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<BTreeMap<(usize, usize), u32>>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let old_len = old_range.len();
    let new_len = new_range.len();
    let mut table = BTreeMap::new();

    for i in (0..new_len).rev() {
        // are we running for too long?  give up on the table
        if let Some(deadline) = deadline {
            if Instant::now() > deadline {
                return None;
            }
        }

        for j in (0..old_len).rev() {
            let val = if new[i] == old[j] {
                table.get(&(i + 1, j + 1)).map_or(0, |&x| x) + 1
            } else {
                table
                    .get(&(i + 1, j))
                    .map_or(0, |&x| x)
                    .max(table.get(&(i, j + 1)).map_or(0, |&x| x))
            };
            if val > 0 {
                table.insert((i, j), val);
            }
        }
    }

    Some(table)
}

#[test]
fn test_table() {
    let table = make_table(&vec![2, 3], 0..2, &vec![0, 1, 2], 0..3, None).unwrap();
    let expected = {
        let mut m = BTreeMap::new();
        m.insert((1, 0), 1);
        m.insert((0, 0), 1);
        m.insert((2, 0), 1);
        m
    };
    assert_eq!(table, expected);
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

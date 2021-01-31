//! Myers' diff algorithm.
//!
//! * time: `O((N+M)D)`
//! * space `O(N+M)`
//!
//! See [the original article by Eugene W. Myers](http://www.xmailserver.org/diff2.pdf)
//! describing it.

use std::cmp::{max, min};
use std::ops::{Index, Range};

use crate::algorithms::DiffHook;

/// Myers' diff algorithm.
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
    New::Output: PartialEq<Old::Output>,
{
    diff_offsets(
        d,
        old,
        old_range.start,
        old_range.end,
        new,
        new_range.start,
        new_range.end,
    )?;
    d.finish()
}

/// Shortcut for diffing slices.
pub fn diff_slices<D, T>(d: &mut D, old: &[T], new: &[T]) -> Result<(), D::Error>
where
    D: DiffHook,
    T: PartialEq,
{
    diff(d, old, 0..old.len(), new, 0..new.len())
}

fn modulo(a: isize, b: usize) -> usize {
    a.rem_euclid(b as isize) as usize
}

pub(crate) fn diff_offsets<D, Old, New>(
    diff: &mut D,
    old: &Old,
    old_current: usize,
    old_end: usize,
    new: &New,
    new_current: usize,
    new_end: usize,
) -> Result<(), D::Error>
where
    D: DiffHook + ?Sized,
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    #![allow(clippy::many_single_char_names)]
    if old_end > old_current && new_end > new_current {
        let old_span = old_end - old_current;
        let new_span = new_end - new_current;
        let total_span = (old_span + new_span) as isize;
        let vec_size = (2 * min(old_span, new_span) + 2) as usize;
        let w = old_span as isize - new_span as isize;
        let mut vec_down = vec![0; vec_size as usize];
        let mut vec_up = vec![0; vec_size as usize];
        for i in 0..=(total_span / 2 + total_span % 2) {
            for &inverse in &[true, false][..] {
                let (v1, v2) = if inverse {
                    (&mut vec_down, &mut vec_up)
                } else {
                    (&mut vec_up, &mut vec_down)
                };
                let j_start = -(i - 2 * max(0, i - new_span as isize));
                let j_end = i - 2 * max(0, i - old_span as isize) + 1;
                for j in (j_start..j_end).step_by(2) {
                    let mut a: usize = if j == -i
                        || j != i && v1[modulo(j - 1, vec_size)] < v1[modulo(j + 1, vec_size)]
                    {
                        v1[modulo(j + 1, vec_size)]
                    } else {
                        v1[modulo(j - 1, vec_size)] + 1
                    };
                    let mut b = (a as isize - j) as usize;
                    let (s, t) = (a, b);
                    while a < old_span && b < new_span && {
                        let (e_i, f_i) = if inverse {
                            (a, b)
                        } else {
                            (old_span - a - 1, new_span - b - 1)
                        };
                        new[new_current + f_i] == old[old_current + e_i]
                    } {
                        a += 1;
                        b += 1;
                    }
                    v1[modulo(j, vec_size)] = a;
                    let bound = if inverse { i - 1 } else { i };
                    if (total_span % 2 == 1) == inverse
                        && w - j >= -bound
                        && w - j <= bound
                        && v1[modulo(j, vec_size)] + v2[modulo(w - j, vec_size)] >= old_span
                    {
                        let (x, y, u, v) = if inverse {
                            (s, t, a, b)
                        } else {
                            (old_span - a, new_span - b, old_span - s, new_span - t)
                        };
                        if i + bound > 1 || (x != u && y != v) {
                            diff_offsets(
                                diff,
                                old,
                                old_current,
                                old_current + x,
                                new,
                                new_current,
                                new_current + y,
                            )?;
                            if x != u {
                                diff.equal(old_current + x, new_current + y, u - x)?;
                            }
                            diff_offsets(
                                diff,
                                old,
                                old_current + u,
                                old_end,
                                new,
                                new_current + v,
                                new_end,
                            )?;
                            return Ok(());
                        } else if new_span > old_span {
                            diff.equal(old_current, new_current, old_span)?;
                            diff.insert(
                                old_current + old_span,
                                new_current + old_span,
                                new_span - old_span,
                            )?;
                            return Ok(());
                        } else if new_span < old_span {
                            diff.equal(old_current, new_current, new_span)?;
                            diff.delete(
                                old_current + new_span,
                                old_span - new_span,
                                new_current + new_span,
                            )?;
                            return Ok(());
                        } else {
                            return Ok(());
                        }
                    }
                }
            }
        }
    } else if old_end > old_current {
        diff.delete(old_current, old_end - old_current, new_current)?
    } else if new_end > new_current {
        diff.insert(old_current, new_current, new_end - new_current)?
    }
    Ok(())
}

#[test]
fn test_modulo() {
    assert_eq!(modulo(-11, 10), 9);
    assert_eq!(modulo(23, 7), 2);
    assert_eq!(modulo(-12, 6), 0);
}

#[test]
fn test_diff() {
    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 9, 4];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff_slices(&mut d, a, b).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_contiguous() {
    let a: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];
    let b: &[usize] = &[0, 1, 2, 8, 9, 4, 4, 7];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff_slices(&mut d, a, b).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_pat() {
    let a: &[usize] = &[0, 1, 3, 4, 5];
    let b: &[usize] = &[0, 1, 4, 5, 8, 9];

    let mut d = crate::algorithms::Capture::new();
    diff_slices(&mut d, a, b).unwrap();
    insta::assert_debug_snapshot!(d.ops());
}

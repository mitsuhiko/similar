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

fn modulo(a: isize, b: usize) -> usize {
    a.rem_euclid(b as isize) as usize
}

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

pub(crate) fn diff_offsets<D, Old, New>(
    diff: &mut D,
    old: &Old,
    i: usize,
    i_: usize,
    new: &New,
    j: usize,
    j_: usize,
) -> Result<(), D::Error>
where
    D: DiffHook + ?Sized,
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if i_ > i && j_ > j {
        let n = i_ - i;
        let m = j_ - j;
        let l = (n + m) as isize;
        let z = (2 * min(n, m) + 2) as usize;
        let w = n as isize - m as isize;
        let mut g = vec![0; z as usize];
        let mut p = vec![0; z as usize];
        for h in 0..=(l / 2 + l % 2) {
            for &inverse in &[true, false][..] {
                let (dollar_c, dollar_d) = if inverse {
                    (&mut g, &mut p)
                } else {
                    (&mut p, &mut g)
                };
                let (k0, k1) = {
                    let (m, n) = (m as isize, n as isize);
                    (-(h - 2 * max(0, h - m)), h - 2 * max(0, h - n) + 1)
                };
                for k in (k0..k1).step_by(2) {
                    let mut a: usize = if k == -h
                        || k != h && dollar_c[modulo(k - 1, z)] < dollar_c[modulo(k + 1, z)]
                    {
                        dollar_c[modulo(k + 1, z)]
                    } else {
                        dollar_c[modulo(k - 1, z)] + 1
                    };
                    let mut b = (a as isize - k) as usize;
                    let (s, t) = (a, b);
                    while a < n && b < m && {
                        let (e_i, f_i) = if inverse {
                            (a, b)
                        } else {
                            (n - a - 1, m - b - 1)
                        };
                        new[j + f_i] == old[i + e_i]
                    } {
                        a += 1;
                        b += 1;
                    }
                    dollar_c[modulo(k, z)] = a;
                    let bound = if inverse { h - 1 } else { h };
                    if (l % 2 == 1) == inverse
                        && w - k >= -bound
                        && w - k <= bound
                        && dollar_c[modulo(k, z)] + dollar_d[modulo(w - k, z)] >= n
                    {
                        let (x, y, u, v) = if inverse {
                            (s, t, a, b)
                        } else {
                            (n - a, m - b, n - s, m - t)
                        };
                        if h + bound > 1 || (x != u && y != v) {
                            diff_offsets(diff, old, i, i + x, new, j, j + y)?;
                            if x != u {
                                diff.equal(i + x, j + y, u - x)?;
                            }
                            diff_offsets(diff, old, i + u, i_, new, j + v, j_)?;
                            return Ok(());
                        } else if m > n {
                            diff.equal(i, j, n)?;
                            diff.insert(i + n, j + n, m - n)?;
                            return Ok(());
                        } else if m < n {
                            diff.equal(i, j, m)?;
                            diff.delete(i + m, n - m, j + m)?;
                            return Ok(());
                        } else {
                            return Ok(());
                        }
                    }
                }
            }
        }
    } else if i_ > i {
        diff.delete(i, i_ - i, j)?
    } else if j_ > j {
        diff.insert(i, j, j_ - j)?
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

    struct D;
    impl DiffHook for D {
        type Error = ();
        fn delete(&mut self, o: usize, len: usize, new: usize) -> Result<(), ()> {
            assert_eq!(o, 3);
            assert_eq!(len, 1);
            assert_eq!(new, 3);
            println!("delete");
            Ok(())
        }
        fn insert(&mut self, o: usize, n: usize, len: usize) -> Result<(), ()> {
            assert_eq!(o, 3);
            assert_eq!(n, 3);
            assert_eq!(len, 1);
            println!("insert");
            Ok(())
        }
    }

    let mut d = crate::algorithms::Replace::new(D);
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap()
}

#[test]
fn test_contiguous() {
    let a: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];
    let b: &[usize] = &[0, 1, 2, 8, 9, 4, 4, 7];
    struct D;

    impl DiffHook for D {
        type Error = ();
        fn delete(&mut self, _o: usize, _len: usize, _new: usize) -> Result<(), ()> {
            panic!("Should not delete")
        }
        fn insert(&mut self, _o: usize, _n: usize, _len: usize) -> Result<(), ()> {
            panic!("Should not insert")
        }
        fn replace(&mut self, o: usize, l: usize, n: usize, nl: usize) -> Result<(), ()> {
            assert!(o != 3 || (l == 2 && nl == 2));
            assert!(o != 7 || (l == 1 && nl == 1));
            println!("replace {:?} {:?} {:?} {:?}", o, l, n, nl);
            Ok(())
        }
    }

    let mut d = crate::algorithms::Replace::new(D);
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
}

#[test]
fn test_replace() {
    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 7, 8, 9];

    struct D;
    impl DiffHook for D {
        type Error = ();
        fn delete(&mut self, _o: usize, _len: usize, _new: usize) -> Result<(), ()> {
            panic!("should not delete")
        }
        fn insert(&mut self, _o: usize, _n: usize, _len: usize) -> Result<(), ()> {
            panic!("should not insert")
        }
        fn replace(&mut self, _o: usize, _l: usize, _n: usize, _nl: usize) -> Result<(), ()> {
            Ok(())
        }
    }
    let mut d = crate::algorithms::Replace::new(D);
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
}

#[test]
fn test_pat() {
    let a: &[usize] = &[0, 1, 3, 4, 5];
    let b: &[usize] = &[0, 1, 4, 5, 8, 9];

    struct D;
    impl DiffHook for D {
        type Error = ();
        fn delete(&mut self, _o: usize, _len: usize, _new: usize) -> Result<(), ()> {
            println!("delete {:?} {:?} {:?}", _o, _len, _new);
            Ok(())
        }
        fn insert(&mut self, _o: usize, _n: usize, _len: usize) -> Result<(), ()> {
            println!("insert {:?} {:?} {:?}", _o, _n, _len);
            Ok(())
        }
        fn replace(&mut self, _o: usize, _l: usize, _n: usize, _nl: usize) -> Result<(), ()> {
            println!("replace {:?} {:?} {:?} {:?}", _o, _l, _n, _nl);
            Ok(())
        }
    }

    let mut d = crate::algorithms::Replace::new(D);
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
}

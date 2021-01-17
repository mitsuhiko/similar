//! Patience diff algorithm.
//!
//! * time: `O(N log N + M log M + (N+M)D)`
//! * space: `O(N+M)`
//!
//! Tends to give more human-readable outputs. See [Bram Cohen's blog
//! post](https://bramcohen.livejournal.com/73318.html) describing it.
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Index, Range};

use crate::algorithms::{myers, DiffHook, Replace};

struct Indexable<'a, T: ?Sized> {
    p: &'a T,
    i: usize,
}

impl<'a, T: Index<usize> + 'a> std::fmt::Debug for Indexable<'a, T>
where
    T::Output: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:?}", &self.p[self.i])
    }
}

impl<'a, 'b, A, B> PartialEq<Indexable<'a, A>> for Indexable<'b, B>
where
    A: Index<usize> + 'b + ?Sized,
    B: Index<usize> + 'b + ?Sized,
    B::Output: PartialEq<A::Output>,
{
    fn eq(&self, b: &Indexable<'a, A>) -> bool {
        self.p[self.i] == b.p[b.i]
    }
}

fn unique<T>(p: &T, e0: usize, e1: usize) -> Vec<Indexable<T>>
where
    T: Index<usize> + ?Sized,
    T::Output: Hash + Eq,
{
    let mut aa = HashMap::new();
    for i in e0..e1 {
        match aa.entry(&p[i]) {
            Entry::Vacant(e) => {
                e.insert(Some(i));
            }
            Entry::Occupied(mut e) => {
                let e = e.get_mut();
                if e.is_some() {
                    *e = None
                }
            }
        }
    }
    let mut v: Vec<_> = aa
        .into_iter()
        .filter_map(|(_, x)| x)
        .map(|i| Indexable { p, i })
        .collect();
    v.sort_by(|a, b| a.i.cmp(&b.i));
    v
}

/// Patience diff algorithm.
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    D: DiffHook,
{
    let au = unique(old, old_range.start, old_range.end);
    let bu = unique(new, old_range.start, old_range.end);

    struct Patience<
        'a,
        'b,
        'd,
        S: 'a + Index<usize> + ?Sized,
        T: 'b + Index<usize> + ?Sized,
        D: DiffHook + 'd,
    > {
        current_a: usize,
        current_b: usize,
        a1: usize,
        b1: usize,
        a: &'a S,
        b: &'b T,
        d: &'d mut D,
        au: &'a [Indexable<'a, S>],
        bu: &'b [Indexable<'b, T>],
    }
    impl<
            'a,
            'b,
            'd,
            S: 'a + Index<usize> + ?Sized,
            T: 'b + Index<usize> + ?Sized,
            D: DiffHook + 'd,
        > DiffHook for Patience<'a, 'b, 'd, S, T, D>
    where
        T::Output: PartialEq<S::Output>,
    {
        type Error = D::Error;
        fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), D::Error> {
            for (old, new) in (old..old + len).zip(new..new + len) {
                let a0 = self.current_a;
                let b0 = self.current_b;
                while self.current_a < self.au[old].i
                    && self.current_b < self.bu[new].i
                    && self.b[self.current_b] == self.a[self.current_a]
                {
                    self.current_a += 1;
                    self.current_b += 1;
                }
                if self.current_a > a0 {
                    self.d.equal(a0, b0, self.current_a - a0)?
                }
                myers::diff_offsets(
                    self.d,
                    self.a,
                    self.current_a,
                    self.au[old].i,
                    self.b,
                    self.current_b,
                    self.bu[new].i,
                )?;
                self.current_a = self.au[old].i;
                self.current_b = self.bu[new].i;
            }
            Ok(())
        }

        fn finish(&mut self) -> Result<(), D::Error> {
            myers::diff(
                self.d,
                self.a,
                self.current_a..self.a1,
                self.b,
                self.current_b..self.b1,
            )
        }
    }
    let mut d = Replace::new(Patience {
        current_a: old_range.start,
        current_b: new_range.start,
        a: old,
        a1: old_range.end,
        b: new,
        b1: new_range.end,
        d,
        au: &au,
        bu: &bu,
    });
    myers::diff(&mut d, &au, 0..au.len(), &bu, 0..bu.len())?;
    Ok(())
}

/// Shortcut for diffing slices.
pub fn diff_slices<D, T>(d: &mut D, old: &[T], new: &[T]) -> Result<(), D::Error>
where
    D: DiffHook,
    T: Eq + Hash,
{
    diff(d, old, 0..old.len(), new, 0..new.len())
}

#[test]
fn test_patience() {
    let a: &[usize] = &[11, 1, 2, 2, 3, 4, 4, 4, 5, 47, 19];
    let b: &[usize] = &[10, 1, 2, 2, 8, 9, 4, 4, 7, 47, 18];

    struct D(Vec<(usize, usize, usize, usize)>);
    impl DiffHook for D {
        type Error = ();
        fn delete(&mut self, o: usize, len: usize, new: usize) -> Result<(), ()> {
            self.0.push((o, len, new, 0));
            Ok(())
        }
        fn insert(&mut self, o: usize, n: usize, len: usize) -> Result<(), ()> {
            self.0.push((o, 0, n, len));
            Ok(())
        }
        fn replace(&mut self, o: usize, l: usize, n: usize, nl: usize) -> Result<(), ()> {
            self.0.push((o, l, n, nl));
            Ok(())
        }
    }
    let mut d = Replace::new(D(Vec::new()));
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    let d: D = d.into_inner();

    insta::assert_json_snapshot!(&d.0.as_slice(), @r###"
    [
      [
        0,
        1,
        0,
        1
      ],
      [
        4,
        2,
        4,
        2
      ],
      [
        8,
        1,
        8,
        1
      ],
      [
        10,
        1,
        10,
        1
      ]
    ]
    "###);
}

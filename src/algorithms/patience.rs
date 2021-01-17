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
    value: &'a T,
    index: usize,
}

impl<'a, T: Index<usize> + 'a> std::fmt::Debug for Indexable<'a, T>
where
    T::Output: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:?}", &self.value[self.index])
    }
}

impl<'a, 'b, A, B> PartialEq<Indexable<'a, A>> for Indexable<'b, B>
where
    A: Index<usize> + 'b + ?Sized,
    B: Index<usize> + 'b + ?Sized,
    B::Output: PartialEq<A::Output>,
{
    fn eq(&self, b: &Indexable<'a, A>) -> bool {
        self.value[self.index] == b.value[b.index]
    }
}

fn unique<T>(seq: &T, lower: usize, upper: usize) -> Vec<Indexable<T>>
where
    T: Index<usize> + ?Sized,
    T::Output: Hash + Eq,
{
    let mut by_item = HashMap::new();
    for index in lower..upper {
        match by_item.entry(&seq[index]) {
            Entry::Vacant(entry) => {
                entry.insert(Some(index));
            }
            Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                if entry.is_some() {
                    *entry = None
                }
            }
        }
    }
    let mut rv = by_item
        .into_iter()
        .filter_map(|(_, x)| x)
        .map(|index| Indexable { value: seq, index })
        .collect::<Vec<_>>();
    rv.sort_by(|a, b| a.index.cmp(&b.index));
    rv
}

struct Patience<'old, 'new, 'd, Old: ?Sized, New: ?Sized, D> {
    d: &'d mut D,
    old: &'old Old,
    new: &'new New,
    current_old: usize,
    end_old: usize,
    current_new: usize,
    end_new: usize,
    indexes_old: &'old [Indexable<'old, Old>],
    indexes_new: &'new [Indexable<'new, New>],
}

impl<'old, 'new, 'd, Old, New, D> DiffHook for Patience<'old, 'new, 'd, Old, New, D>
where
    D: DiffHook + 'd,
    Old: Index<usize> + ?Sized + 'old,
    New: Index<usize> + ?Sized + 'new,
    New::Output: PartialEq<Old::Output>,
{
    type Error = D::Error;
    fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), D::Error> {
        for (old, new) in (old..old + len).zip(new..new + len) {
            let a0 = self.current_old;
            let b0 = self.current_new;
            while self.current_old < self.indexes_old[old].index
                && self.current_new < self.indexes_new[new].index
                && self.new[self.current_new] == self.old[self.current_old]
            {
                self.current_old += 1;
                self.current_new += 1;
            }
            if self.current_old > a0 {
                self.d.equal(a0, b0, self.current_old - a0)?
            }
            myers::diff_offsets(
                self.d,
                self.old,
                self.current_old,
                self.indexes_old[old].index,
                self.new,
                self.current_new,
                self.indexes_new[new].index,
            )?;
            self.current_old = self.indexes_old[old].index;
            self.current_new = self.indexes_new[new].index;
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<(), D::Error> {
        myers::diff(
            self.d,
            self.old,
            self.current_old..self.end_old,
            self.new,
            self.current_new..self.end_new,
        )
    }
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
    let indexes_old = unique(old, old_range.start, old_range.end);
    let indexes_new = unique(new, old_range.start, old_range.end);

    let mut d = Replace::new(Patience {
        d,
        old,
        new,
        current_old: old_range.start,
        end_old: old_range.end,
        current_new: new_range.start,
        end_new: new_range.end,
        indexes_old: &indexes_old,
        indexes_new: &indexes_new,
    });
    myers::diff(
        &mut d,
        &indexes_old,
        0..indexes_old.len(),
        &indexes_new,
        0..indexes_new.len(),
    )?;
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

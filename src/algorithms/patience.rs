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
    let old_indexes = unique(old, old_range.start, old_range.end);
    let new_indexes = unique(new, new_range.start, new_range.end);

    let mut d = Replace::new(Patience {
        d,
        old,
        old_current: old_range.start,
        old_end: old_range.end,
        old_indexes: &old_indexes,
        new,
        new_current: new_range.start,
        new_end: new_range.end,
        new_indexes: &new_indexes,
    });
    myers::diff(
        &mut d,
        &old_indexes,
        0..old_indexes.len(),
        &new_indexes,
        0..new_indexes.len(),
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
    old_current: usize,
    old_end: usize,
    old_indexes: &'old [Indexable<'old, Old>],
    new: &'new New,
    new_current: usize,
    new_end: usize,
    new_indexes: &'new [Indexable<'new, New>],
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
            let a0 = self.old_current;
            let b0 = self.new_current;
            while self.old_current < self.old_indexes[old].index
                && self.new_current < self.new_indexes[new].index
                && self.new[self.new_current] == self.old[self.old_current]
            {
                self.old_current += 1;
                self.new_current += 1;
            }
            if self.old_current > a0 {
                self.d.equal(a0, b0, self.old_current - a0)?
            }
            myers::diff_offsets(
                self.d,
                self.old,
                self.old_current,
                self.old_indexes[old].index,
                self.new,
                self.new_current,
                self.new_indexes[new].index,
            )?;
            self.old_current = self.old_indexes[old].index;
            self.new_current = self.new_indexes[new].index;
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<(), D::Error> {
        myers::diff(
            self.d,
            self.old,
            self.old_current..self.old_end,
            self.new,
            self.new_current..self.new_end,
        )
    }
}

#[test]
fn test_patience() {
    let a: &[usize] = &[11, 1, 2, 2, 3, 4, 4, 4, 5, 47, 19];
    let b: &[usize] = &[10, 1, 2, 2, 8, 9, 4, 4, 7, 47, 18];

    let mut d = Replace::new(crate::algorithms::Capture::new());
    diff_slices(&mut d, a, b).unwrap();

    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_patience_out_of_bounds_bug() {
    // this used to be a bug
    let a: &[usize] = &[1, 2, 3, 4];
    let b: &[usize] = &[1, 2, 3];

    let mut d = Replace::new(crate::algorithms::Capture::new());
    diff_slices(&mut d, a, b).unwrap();

    insta::assert_debug_snapshot!(d.into_inner().ops());
}

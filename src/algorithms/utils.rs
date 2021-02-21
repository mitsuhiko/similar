use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Index, Range};

/// Utility function to check if a range is empty that works on older rust versions
#[inline(always)]
pub fn is_empty_range<T: PartialOrd>(range: &Range<T>) -> bool {
    !(range.start < range.end)
}
pub struct Indexable<'a, Idx: ?Sized> {
    lookup: &'a Idx,
    index: usize,
}

impl<'a, Idx: ?Sized> Indexable<'a, Idx> {
    /// Returns the index.
    pub fn index(&self) -> usize {
        self.index
    }
}

impl<'a, Idx: Index<usize> + 'a> std::fmt::Debug for Indexable<'a, Idx>
where
    Idx::Output: std::fmt::Debug,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:?}", &self.lookup[self.index])
    }
}

impl<'a, 'b, A, B> PartialEq<Indexable<'a, A>> for Indexable<'b, B>
where
    A: Index<usize> + 'b + ?Sized,
    B: Index<usize> + 'b + ?Sized,
    B::Output: PartialEq<A::Output>,
{
    fn eq(&self, b: &Indexable<'a, A>) -> bool {
        self.lookup[self.index] == b.lookup[b.index]
    }
}

pub fn unique<Idx>(seq: &Idx, range: Range<usize>) -> Vec<Indexable<Idx>>
where
    Idx: Index<usize> + ?Sized,
    Idx::Output: Hash + Eq,
{
    let mut by_item = HashMap::new();
    for index in range {
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
        .map(|index| Indexable { lookup: seq, index })
        .collect::<Vec<_>>();
    rv.sort_by(|a, b| a.index.cmp(&b.index));
    rv
}

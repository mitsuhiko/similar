use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, Range};

/// Utility function to check if a range is empty that works on older rust versions
#[inline(always)]
#[allow(clippy::neg_cmp_op_on_partial_ord)]
pub fn is_empty_range<T: PartialOrd<T>>(range: &Range<T>) -> bool {
    !(range.start < range.end)
}

/// Represents an item in the vector returend by [`unique`].
///
/// It compares like the underlying item does it was created from but
/// carries the index it was originally created from.
pub struct UniqueItem<'a, Idx: ?Sized> {
    lookup: &'a Idx,
    index: usize,
}

impl<'a, Idx: ?Sized> UniqueItem<'a, Idx>
where
    Idx: Index<usize>,
{
    /// Returns the value.
    #[inline(always)]
    pub fn value(&self) -> &Idx::Output {
        &self.lookup[self.index]
    }

    /// Returns the original index.
    #[inline(always)]
    pub fn original_index(&self) -> usize {
        self.index
    }
}

impl<'a, Idx: Index<usize> + 'a> Debug for UniqueItem<'a, Idx>
where
    Idx::Output: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("UniqueItem")
            .field("value", &self.value())
            .field("original_index", &self.original_index())
            .finish()
    }
}

impl<'a, 'b, A, B> PartialEq<UniqueItem<'a, A>> for UniqueItem<'b, B>
where
    A: Index<usize> + 'b + ?Sized,
    B: Index<usize> + 'b + ?Sized,
    B::Output: PartialEq<A::Output>,
{
    #[inline(always)]
    fn eq(&self, other: &UniqueItem<'a, A>) -> bool {
        self.value() == other.value()
    }
}

/// Returns only unique items in the sequence as vector.
///
/// Each item is wrapped in a [`UniqueItem`] so that both the value and the
/// index can be extracted.
pub fn unique<Idx>(lookup: &Idx, range: Range<usize>) -> Vec<UniqueItem<Idx>>
where
    Idx: Index<usize> + ?Sized,
    Idx::Output: Hash + Eq,
{
    let mut by_item = HashMap::new();
    for index in range {
        match by_item.entry(&lookup[index]) {
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
        .map(|index| UniqueItem { lookup, index })
        .collect::<Vec<_>>();
    rv.sort_by_key(|a| a.original_index());
    rv
}

/// Given two lookups and ranges calculates the length of the common prefix.
pub fn common_prefix_len<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if is_empty_range(&old_range) || is_empty_range(&new_range) {
        return 0;
    }
    new_range
        .zip(old_range)
        .take_while(
            #[inline(always)]
            |x| new[x.0] == old[x.1],
        )
        .count()
}

/// Given two lookups and ranges calculates the length of common suffix.
pub fn common_suffix_len<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if is_empty_range(&old_range) || is_empty_range(&new_range) {
        return 0;
    }
    new_range
        .rev()
        .zip(old_range.rev())
        .take_while(
            #[inline(always)]
            |x| new[x.0] == old[x.1],
        )
        .count()
}

#[test]
fn test_unique() {
    let u = unique(&vec!['a', 'b', 'c', 'd', 'd', 'b'], 0..6)
        .into_iter()
        .map(|x| (*x.value(), x.original_index()))
        .collect::<Vec<_>>();
    assert_eq!(u, vec![('a', 0), ('c', 2)]);
}

#[test]
fn test_common_prefix_len() {
    assert_eq!(
        common_prefix_len("".as_bytes(), 0..0, "".as_bytes(), 0..0),
        0
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 0..9, "foobarblah".as_bytes(), 0..10),
        7
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 0..9, "blablabla".as_bytes(), 0..9),
        0
    );
    assert_eq!(
        common_prefix_len("foobarbaz".as_bytes(), 3..9, "foobarblah".as_bytes(), 3..10),
        4
    );
}

#[test]
fn test_common_suffix_len() {
    assert_eq!(
        common_suffix_len("".as_bytes(), 0..0, "".as_bytes(), 0..0),
        0
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 0..4, "X0001234".as_bytes(), 0..8),
        4
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 0..4, "Xxxx".as_bytes(), 0..4),
        0
    );
    assert_eq!(
        common_suffix_len("1234".as_bytes(), 2..4, "01234".as_bytes(), 2..5),
        2
    );
}

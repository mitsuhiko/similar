use alloc::vec::Vec;
use core::cell::OnceCell;
use core::ops::Index;

/// Adapts a computed random-access sequence to an indexable lookup by caching
/// each value on first access.
///
/// This is useful for virtual or lazily computed sequences that cannot return a
/// stable reference directly.  The producer is called at most once per index;
/// afterwards the cached value is borrowed from internal storage.
///
/// ```rust
/// use similar::CachedLookup;
///
/// let lookup = CachedLookup::new(3, |idx| format!("item-{idx}"));
/// assert_eq!(&lookup[1], "item-1");
/// assert_eq!(&lookup[1], "item-1");
/// ```
pub struct CachedLookup<T, F> {
    slots: Vec<OnceCell<T>>,
    make_value: F,
}

impl<T, F> CachedLookup<T, F> {
    /// Creates a cached lookup with `len` elements produced by `make_value`.
    pub fn new(len: usize, make_value: F) -> Self {
        CachedLookup {
            slots: core::iter::repeat_with(OnceCell::new).take(len).collect(),
            make_value,
        }
    }

    /// Returns the number of items in the lookup.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Returns `true` if the lookup is empty.
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }
}

impl<T, F> Index<usize> for CachedLookup<T, F>
where
    F: Fn(usize) -> T,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.slots[index].get_or_init(|| (self.make_value)(index))
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use alloc::vec;
    use alloc::vec::Vec;
    use core::cell::Cell;

    use super::CachedLookup;
    use crate::{Algorithm, ChangeTag, capture_diff};

    #[test]
    fn caches_values_once_per_index() {
        let hits = Cell::new(0);
        let lookup = CachedLookup::new(3, |idx| {
            hits.set(hits.get() + 1);
            idx * 2
        });

        assert_eq!(lookup[1], 2);
        assert_eq!(lookup[1], 2);
        assert_eq!(lookup[2], 4);
        assert_eq!(hits.get(), 2);
    }

    #[test]
    fn works_with_diff_algorithms() {
        let old = CachedLookup::new(3, |idx: usize| ["foo", "bar", "baz"][idx].to_string());
        let new = CachedLookup::new(3, |idx: usize| ["foo", "blah", "baz"][idx].to_string());
        let ops = capture_diff(Algorithm::Myers, &old, 0..old.len(), &new, 0..new.len());
        let changes = ops
            .iter()
            .flat_map(|op| op.iter_changes(&old, &new))
            .map(|change| (change.tag(), change.value()))
            .collect::<Vec<_>>();

        assert_eq!(
            changes,
            vec![
                (ChangeTag::Equal, "foo".to_string()),
                (ChangeTag::Delete, "bar".to_string()),
                (ChangeTag::Insert, "blah".to_string()),
                (ChangeTag::Equal, "baz".to_string()),
            ]
        );
    }
}

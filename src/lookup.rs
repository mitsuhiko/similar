use alloc::vec::Vec;
use core::cell::OnceCell;
use core::ops::Index;

/// Caches computed values for indexed lookup.
///
/// This is useful for virtual or lazily computed sequences that cannot return a
/// stable reference directly. The producer is called at most once per index.
/// For eager remapping to dense integer IDs, see
/// [`IdentityDistinct`](crate::algorithms::IdentifyDistinct).
///
/// ```rust
/// use similar::algorithms::CachedLookup;
///
/// let lookup = CachedLookup::new(3, |idx| format!("item-{idx}"));
/// assert_eq!(&lookup[1], "item-1");
/// assert_eq!(&lookup[1], "item-1");
/// ```
///
/// You can also diff by a derived key instead of the original value:
///
/// ```rust
/// use similar::{Algorithm, capture_diff};
/// use similar::algorithms::CachedLookup;
///
/// #[derive(Debug)]
/// struct User {
///     id: u32,
///     name: &'static str,
/// }
///
/// let old = [
///     User { id: 1, name: "Alice" },
///     User { id: 2, name: "Bob" },
/// ];
/// let new = [
///     User { id: 1, name: "Alice Smith" },
///     User { id: 3, name: "Carol" },
/// ];
///
/// let old_ids = CachedLookup::new(old.len(), |idx: usize| old[idx].id);
/// let new_ids = CachedLookup::new(new.len(), |idx: usize| new[idx].id);
///
/// let ops = capture_diff(
///     Algorithm::Myers,
///     &old_ids,
///     0..old_ids.len(),
///     &new_ids,
///     0..new_ids.len(),
/// );
///
/// assert!(ops.iter().any(|op| matches!(op, similar::DiffOp::Equal { len: 1, .. })));
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

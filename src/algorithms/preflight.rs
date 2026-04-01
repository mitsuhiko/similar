use std::any::type_name;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::{Index, Range};

use crate::algorithms::DiffHook;
use crate::deadline_support::{Instant, deadline_exceeded};

const DISJOINT_FAST_PATH_MIN_LEN: usize = 512;
const DISJOINT_FAST_PATH_MIN_WORK: usize = 128 * 1024;
const DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL: usize = 1024;

pub(crate) fn maybe_emit_disjoint_fast_path<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<bool, D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    if deadline_exceeded(deadline) {
        return Ok(false);
    }

    let old_len = old_range.len();
    let new_len = new_range.len();

    if old_len < DISJOINT_FAST_PATH_MIN_LEN
        || new_len < DISJOINT_FAST_PATH_MIN_LEN
        || old_len.saturating_mul(new_len) < DISJOINT_FAST_PATH_MIN_WORK
    {
        return Ok(false);
    }

    // This fast-path relies on hashing values from both sides into the same
    // map. Restrict it to apparent same-output types to avoid cross-type hash
    // compatibility pitfalls.
    if type_name::<Old::Output>() != type_name::<New::Output>() {
        return Ok(false);
    }

    if new[new_range.start] == old[old_range.start]
        || new[new_range.end - 1] == old[old_range.end - 1]
    {
        return Ok(false);
    }

    let has_common_item =
        match has_common_item(old, old_range.clone(), new, new_range.clone(), deadline) {
            Some(value) => value,
            None => return Ok(false),
        };

    if has_common_item {
        return Ok(false);
    }

    d.delete(old_range.start, old_len, new_range.start)?;
    d.insert(old_range.start, new_range.start, new_len)?;
    d.finish()?;
    Ok(true)
}

fn has_common_item<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<bool>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash,
    New::Output: PartialEq<Old::Output> + Hash,
{
    #[inline(always)]
    fn hash_value<T: Hash + ?Sized>(value: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    let mut by_hash = HashMap::<u64, Vec<usize>>::new();
    for (idx, old_idx) in old_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        by_hash
            .entry(hash_value(&old[old_idx]))
            .or_default()
            .push(old_idx);
    }

    for (idx, new_idx) in new_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        if let Some(candidates) = by_hash.get(&hash_value(&new[new_idx])) {
            let new_item = &new[new_idx];
            if candidates.iter().any(|&old_idx| new_item == &old[old_idx]) {
                return Some(true);
            }
        }
    }

    Some(false)
}

#[test]
fn test_has_common_item() {
    assert_eq!(
        has_common_item(&[1, 2, 3], 0..3, &[9, 3, 10], 0..3, None),
        Some(true)
    );
    assert_eq!(
        has_common_item(&[1, 2, 3], 0..3, &[9, 8, 10], 0..3, None),
        Some(false)
    );
}

#[test]
fn test_has_common_item_hash_collisions() {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Collide(u32);

    impl Hash for Collide {
        fn hash<H: Hasher>(&self, state: &mut H) {
            0u8.hash(state);
        }
    }

    assert_eq!(
        has_common_item(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(4)],
            0..2,
            None
        ),
        Some(false)
    );
    assert_eq!(
        has_common_item(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(2)],
            0..2,
            None
        ),
        Some(true)
    );
}

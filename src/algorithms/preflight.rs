use core::any::type_name;
use core::hash::Hash;
use core::ops::{Index, Range};

use crate::algorithms::DiffHook;
use crate::algorithms::utils::{HashBucket, stable_hash};
use crate::deadline_support::{Instant, deadline_exceeded};
use crate::types::MapType;

const DISJOINT_FAST_PATH_MIN_LEN: usize = 512;
const DISJOINT_FAST_PATH_MIN_WORK: usize = 128 * 1024;
const DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL: usize = 1024;
const DISJOINT_FAST_PATH_BOUNDARY_PROBE: usize = 8;
const NEAR_DISJOINT_COMMON_ITEM_DIVISOR: usize = 1024;
const NEAR_DISJOINT_MAX_COMMON_ITEMS: usize = 64;

pub(crate) fn maybe_emit_replace_fast_path<Old, New, D>(
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

    // Cheaply recognize a small insertion/deletion at either edge.  Without
    // this probe, an inserted header makes otherwise identical large files pay
    // for a full hash index merely to prove that they are not disjoint.
    let probe_len = old_len
        .min(new_len)
        .min(DISJOINT_FAST_PATH_BOUNDARY_PROBE + 1);
    for skip in 1..probe_len {
        if new[new_range.start + skip] == old[old_range.start]
            || new[new_range.start] == old[old_range.start + skip]
            || new[new_range.end - 1 - skip] == old[old_range.end - 1]
            || new[new_range.end - 1] == old[old_range.end - 1 - skip]
        {
            return Ok(false);
        }
    }

    // A handful of matching items does not justify an expensive exact search
    // through otherwise unrelated ranges. In particular, stopping this scan at
    // the first match lets one coincidental line defeat the disjoint fast path
    // and send Myers/LCS into near-worst-case work. Scale the tolerated overlap
    // with the shorter input, but cap it so meaningful sparse anchors still go
    // to the selected algorithm.
    let common_item_budget = (old_len.min(new_len) / NEAR_DISJOINT_COMMON_ITEM_DIVISOR)
        .clamp(1, NEAR_DISJOINT_MAX_COMMON_ITEMS);
    let exceeds_budget = match has_more_than_common_items(
        old,
        old_range.clone(),
        new,
        new_range.clone(),
        common_item_budget,
        deadline,
    ) {
        Some(value) => value,
        None => return Ok(false),
    };

    if exceeds_budget {
        return Ok(false);
    }

    d.delete(old_range.start, old_len, new_range.start)?;
    d.insert(old_range.start, new_range.start, new_len)?;
    d.finish()?;
    Ok(true)
}

fn has_more_than_common_items<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    common_item_budget: usize,
    deadline: Option<Instant>,
) -> Option<bool>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash,
    New::Output: PartialEq<Old::Output> + Hash,
{
    let mut by_hash = MapType::<u64, HashBucket<usize>>::new();
    for (idx, old_idx) in old_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        let hash = stable_hash(&old[old_idx]);
        if let Some(bucket) = by_hash.get_mut(&hash) {
            bucket.push(old_idx);
        } else {
            by_hash.insert(hash, HashBucket::new(old_idx));
        }
    }

    let mut common_items = 0usize;
    for (idx, new_idx) in new_range.enumerate() {
        if (idx & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        if let Some(candidates) = by_hash.get(&stable_hash(&new[new_idx])) {
            let new_item = &new[new_idx];
            if candidates.iter().any(|&old_idx| new_item == &old[old_idx]) {
                common_items += 1;
                if common_items > common_item_budget {
                    return Some(true);
                }
            }
        }
    }

    Some(false)
}

#[test]
fn test_common_item_budget() {
    let old = &[1, 2, 3];
    let new = &[9, 3, 2];
    assert_eq!(
        has_more_than_common_items(old, 0..3, new, 0..3, 0, None),
        Some(true)
    );
    assert_eq!(
        has_more_than_common_items(old, 0..3, new, 0..3, 1, None),
        Some(true)
    );
    assert_eq!(
        has_more_than_common_items(old, 0..3, new, 0..3, 2, None),
        Some(false)
    );
    assert_eq!(
        has_more_than_common_items(old, 0..3, &[9, 8, 10], 0..3, 0, None),
        Some(false)
    );
}

#[test]
fn test_nearly_disjoint_ranges_use_replace_fast_path() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let old = (0..4096u32).collect::<Vec<_>>();
    let mut new = (10_000..14_096u32).collect::<Vec<_>>();
    new[2048] = old[1024];

    assert_eq!(
        capture_diff_slices(Algorithm::Myers, &old, &new),
        vec![DiffOp::Replace {
            old_index: 0,
            old_len: old.len(),
            new_index: 0,
            new_len: new.len(),
        }]
    );
}

#[test]
fn test_common_item_budget_with_hash_collisions() {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Collide(u32);

    impl Hash for Collide {
        fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
            0u8.hash(state);
        }
    }

    assert_eq!(
        has_more_than_common_items(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(4)],
            0..2,
            0,
            None,
        ),
        Some(false)
    );
    assert_eq!(
        has_more_than_common_items(
            &[Collide(1), Collide(2)],
            0..2,
            &[Collide(3), Collide(2)],
            0..2,
            0,
            None,
        ),
        Some(true)
    );
}

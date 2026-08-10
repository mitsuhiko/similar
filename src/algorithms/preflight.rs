use alloc::vec::Vec;
use core::any::type_name;
use core::hash::Hash;
use core::ops::{Index, Range};

use crate::algorithms::DiffHook;
use crate::algorithms::utils::{HashBucket, common_prefix_len, common_suffix_len, stable_hash};
use crate::deadline_support::{Instant, deadline_exceeded};
use crate::types::MapType;

const DISJOINT_FAST_PATH_MIN_LEN: usize = 512;
const DISJOINT_FAST_PATH_MIN_WORK: usize = 128 * 1024;
const DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL: usize = 1024;
const DISJOINT_FAST_PATH_BOUNDARY_PROBE: usize = 8;
const NEAR_DISJOINT_COMMON_ITEM_DIVISOR: usize = 1024;
const NEAR_DISJOINT_MAX_COMMON_ITEMS: usize = 64;
const CONFUSING_RECORD_SCAN_WINDOW: usize = 100;
const CONFUSING_RECORD_RATIO: usize = 4;
const MAX_MATCH_FREQUENCY_THRESHOLD: usize = 1024;
const NO_MATCH_RECORD_ID: usize = usize::MAX;

pub(crate) struct TrimmedDiffInput {
    pub(crate) old_range: Range<usize>,
    pub(crate) new_range: Range<usize>,
    pub(crate) original_old_range: Range<usize>,
    pub(crate) original_new_range: Range<usize>,
    pub(crate) prefix_len: usize,
    pub(crate) suffix_len: usize,
}

pub(crate) struct ReducedDiffInput {
    pub(crate) old_values: Vec<usize>,
    pub(crate) old_indices: Vec<usize>,
    pub(crate) new_values: Vec<usize>,
    pub(crate) new_indices: Vec<usize>,
    pub(crate) trimmed: TrimmedDiffInput,
}

pub(crate) enum MyersPreflight {
    Trimmed(TrimmedDiffInput),
    Reduced(ReducedDiffInput),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RecordAction {
    Discard,
    Keep,
    Investigate,
}

fn should_scan<Old, New>(
    old: &Old,
    old_range: &Range<usize>,
    new: &New,
    new_range: &Range<usize>,
    deadline: Option<Instant>,
) -> bool
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if deadline_exceeded(deadline) {
        return false;
    }

    let old_len = old_range.len();
    let new_len = new_range.len();
    if old_len < DISJOINT_FAST_PATH_MIN_LEN
        || new_len < DISJOINT_FAST_PATH_MIN_LEN
        || old_len.saturating_mul(new_len) < DISJOINT_FAST_PATH_MIN_WORK
    {
        return false;
    }

    // This fast-path relies on hashing values from both sides into the same
    // map. Restrict it to apparent same-output types to avoid cross-type hash
    // compatibility pitfalls.
    if type_name::<Old::Output>() != type_name::<New::Output>() {
        return false;
    }

    if new[new_range.start] == old[old_range.start]
        || new[new_range.end - 1] == old[old_range.end - 1]
    {
        return false;
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
            return false;
        }
    }

    true
}

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
    if !should_scan(old, &old_range, new, &new_range, deadline) {
        return Ok(false);
    }

    let old_len = old_range.len();
    let new_len = new_range.len();

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

fn record_actions(ids: &[usize], opposite_counts: &[usize]) -> Vec<RecordAction> {
    let frequency_limit = ids.len().isqrt().clamp(1, MAX_MATCH_FREQUENCY_THRESHOLD);
    ids.iter()
        .map(|&id| {
            if id == NO_MATCH_RECORD_ID {
                RecordAction::Discard
            } else {
                match opposite_counts[id] {
                    0 => RecordAction::Discard,
                    count if count < frequency_limit => RecordAction::Keep,
                    _ => RecordAction::Investigate,
                }
            }
        })
        .collect()
}

fn scan_confusing_run(
    actions: &[RecordAction],
    indexes: impl Iterator<Item = usize>,
) -> (usize, usize) {
    let mut discards = 0usize;
    let mut investigates = 0usize;
    for index in indexes.take(CONFUSING_RECORD_SCAN_WINDOW) {
        match actions[index] {
            RecordAction::Discard => discards += 1,
            RecordAction::Investigate => investigates += 1,
            RecordAction::Keep => break,
        }
    }
    (discards, investigates)
}

fn discard_confusing_record(actions: &[RecordAction], index: usize) -> bool {
    let (left_discards, left_investigates) = scan_confusing_run(actions, (0..index).rev());
    if left_discards == 0 {
        return false;
    }

    let (right_discards, right_investigates) =
        scan_confusing_run(actions, index + 1..actions.len());
    if right_discards == 0 {
        return false;
    }

    let discards = left_discards + right_discards;
    let investigates = left_investigates + right_investigates + 1;
    investigates.saturating_mul(CONFUSING_RECORD_RATIO) < investigates + discards
}

fn keep_record(actions: &[RecordAction], index: usize) -> bool {
    match actions[index] {
        RecordAction::Discard => false,
        RecordAction::Keep => true,
        RecordAction::Investigate => !discard_confusing_record(actions, index),
    }
}

pub(crate) fn reduce_for_myers<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Option<MyersPreflight>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    // Let the normal Myers path perform its one required trim when there is no
    // time left for preflight work.
    if deadline_exceeded(deadline) {
        return None;
    }

    let original_old_range = old_range;
    let original_new_range = new_range;
    let prefix_len = common_prefix_len(
        old,
        original_old_range.clone(),
        new,
        original_new_range.clone(),
    );
    let old_after_prefix = original_old_range.start + prefix_len..original_old_range.end;
    let new_after_prefix = original_new_range.start + prefix_len..original_new_range.end;
    let suffix_len =
        common_suffix_len(old, old_after_prefix.clone(), new, new_after_prefix.clone());
    let old_range = old_after_prefix.start..old_after_prefix.end - suffix_len;
    let new_range = new_after_prefix.start..new_after_prefix.end - suffix_len;
    let trimmed = TrimmedDiffInput {
        old_range: old_range.clone(),
        new_range: new_range.clone(),
        original_old_range,
        original_new_range,
        prefix_len,
        suffix_len,
    };

    if !should_scan(old, &old_range, new, &new_range, deadline) {
        return Some(MyersPreflight::Trimmed(trimmed));
    }

    let mut by_hash = MapType::<u64, HashBucket<(usize, usize)>>::new();
    let mut old_values = Vec::with_capacity(old_range.len());
    let mut new_values = Vec::with_capacity(new_range.len());
    let mut old_counts = Vec::<usize>::new();
    let mut new_counts = Vec::<usize>::new();

    for (offset, old_index) in old_range.clone().enumerate() {
        if (offset & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Some(MyersPreflight::Trimmed(trimmed));
        }

        let hash = stable_hash(&old[old_index]);
        let id = if let Some(bucket) = by_hash.get_mut(&hash) {
            if let Some((_, id)) = bucket
                .iter()
                .find(|(index, _)| old[old_index] == old[*index])
            {
                *id
            } else {
                let id = old_counts.len();
                bucket.push((old_index, id));
                old_counts.push(0);
                new_counts.push(0);
                id
            }
        } else {
            let id = old_counts.len();
            by_hash.insert(hash, HashBucket::new((old_index, id)));
            old_counts.push(0);
            new_counts.push(0);
            id
        };
        old_counts[id] = old_counts[id].saturating_add(1);
        old_values.push(id);
    }

    for (offset, new_index) in new_range.clone().enumerate() {
        if (offset & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Some(MyersPreflight::Trimmed(trimmed));
        }

        let hash = stable_hash(&new[new_index]);
        let id = by_hash
            .get(&hash)
            .and_then(|bucket| {
                bucket
                    .iter()
                    .find(|(index, _)| new[new_index] == old[*index])
            })
            .map(|(_, id)| *id)
            .unwrap_or(NO_MATCH_RECORD_ID);
        if id != NO_MATCH_RECORD_ID {
            new_counts[id] = new_counts[id].saturating_add(1);
        }
        new_values.push(id);
    }
    drop(by_hash);

    let old_actions = record_actions(&old_values, &new_counts);
    let mut reduced_old_values = Vec::new();
    let mut old_indices = Vec::new();
    for (offset, &id) in old_values.iter().enumerate() {
        if (offset & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Some(MyersPreflight::Trimmed(trimmed));
        }
        if keep_record(&old_actions, offset) {
            reduced_old_values.push(id);
            old_indices.push(old_range.start + offset);
        }
    }
    drop(old_actions);
    drop(old_values);
    drop(new_counts);

    let new_actions = record_actions(&new_values, &old_counts);
    let mut reduced_new_values = Vec::new();
    let mut new_indices = Vec::new();
    for (offset, &id) in new_values.iter().enumerate() {
        if (offset & (DISJOINT_FAST_PATH_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Some(MyersPreflight::Trimmed(trimmed));
        }
        if keep_record(&new_actions, offset) {
            reduced_new_values.push(id);
            new_indices.push(new_range.start + offset);
        }
    }

    if old_indices.len() == old_range.len() && new_indices.len() == new_range.len() {
        return Some(MyersPreflight::Trimmed(trimmed));
    }

    Some(MyersPreflight::Reduced(ReducedDiffInput {
        old_values: reduced_old_values,
        old_indices,
        new_values: reduced_new_values,
        new_indices,
        trimmed,
    }))
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
fn test_myers_reduction_preserves_sparse_match() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let old = (0..4096u32).collect::<Vec<_>>();
    let mut new = (10_000..14_096u32).collect::<Vec<_>>();
    new[2048] = old[1024];

    assert_eq!(
        capture_diff_slices(Algorithm::Myers, &old, &new),
        vec![
            DiffOp::Replace {
                old_index: 0,
                old_len: 1024,
                new_index: 0,
                new_len: 2048,
            },
            DiffOp::Equal {
                old_index: 1024,
                new_index: 2048,
                len: 1,
            },
            DiffOp::Replace {
                old_index: 1025,
                old_len: 3071,
                new_index: 2049,
                new_len: 2047,
            },
        ]
    );
}

#[test]
fn test_confusing_record_cleanup_is_context_sensitive() {
    let mut surrounded = vec![RecordAction::Discard; 17];
    surrounded[8] = RecordAction::Investigate;
    assert!(discard_confusing_record(&surrounded, 8));

    let repetitive_run = vec![RecordAction::Investigate; 17];
    assert!(!discard_confusing_record(&repetitive_run, 8));

    let mut next_to_anchor = surrounded;
    next_to_anchor[7] = RecordAction::Keep;
    assert!(!discard_confusing_record(&next_to_anchor, 8));
}

#[test]
fn test_myers_reduction_suppresses_scattered_popular_records() {
    let old = (0..512u32).collect::<Vec<_>>();
    let mut new = (10_000..10_512u32).collect::<Vec<_>>();
    let mut old = old;
    for index in (10..510).step_by(20) {
        old[index] = 99_999;
        new[index + 1] = 99_999;
    }

    let Some(MyersPreflight::Reduced(reduced)) =
        reduce_for_myers(&old, 0..old.len(), &new, 0..new.len(), None)
    else {
        panic!("expected Myers reduction");
    };
    assert!(reduced.old_values.is_empty());
    assert!(reduced.new_values.is_empty());
}

#[test]
fn test_myers_reduction_preserves_sparse_anchor_chain_with_common_edges() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let old = (0..4096u32).collect::<Vec<_>>();
    let mut new = (10_000..14_096u32).collect::<Vec<_>>();
    new[0] = old[0];
    new[4095] = old[4095];
    for (old_index, new_index) in [
        (100, 200),
        (500, 700),
        (900, 1200),
        (1500, 1800),
        (2400, 2700),
        (3500, 3600),
    ] {
        new[new_index] = old[old_index];
    }

    let equal_len = capture_diff_slices(Algorithm::Myers, &old, &new)
        .iter()
        .map(|op| match op {
            DiffOp::Equal { len, .. } => *len,
            _ => 0,
        })
        .sum::<usize>();
    assert_eq!(equal_len, 8);
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

    let old = (0..512u32).map(Collide).collect::<Vec<_>>();
    let mut new = (1000..1512u32).map(Collide).collect::<Vec<_>>();
    new[256] = old[128];
    let equal_len = crate::capture_diff_slices(crate::Algorithm::Myers, &old, &new)
        .iter()
        .map(|op| match op {
            crate::DiffOp::Equal { len, .. } => *len,
            _ => 0,
        })
        .sum::<usize>();
    assert_eq!(equal_len, 1);
}

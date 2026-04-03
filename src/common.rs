use alloc::vec;
use alloc::vec::Vec;
use core::hash::Hash;
use core::ops::{Index, Range};

use crate::algorithms::{Capture, Compact, Replace, diff_deadline};
use crate::deadline_support::Instant;
use crate::{Algorithm, DiffOp};

/// Creates a diff between old and new with the given algorithm capturing the ops.
///
/// This is like [`diff`](crate::algorithms::diff) but instead of using an
/// arbitrary hook this will always use [`Compact`] + [`Replace`] + [`Capture`]
/// and return the captured [`DiffOp`]s. For lazily computed values or diffing
/// by a derived key, see [`crate::algorithms::CachedLookup`].
pub fn capture_diff<Old, New>(
    alg: Algorithm,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> Vec<DiffOp>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    capture_diff_deadline(alg, old, old_range, new, new_range, None)
}

/// Creates a diff between old and new with the given algorithm capturing the ops.
///
/// Works like [`capture_diff`] but with an optional deadline.
pub fn capture_diff_deadline<Old, New>(
    alg: Algorithm,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Vec<DiffOp>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    let mut d = Compact::new(Replace::new(Capture::new()), old, new);
    diff_deadline(alg, &mut d, old, old_range, new, new_range, deadline).unwrap();
    d.into_inner().into_inner().into_ops()
}

/// Creates a diff between old and new with the given algorithm capturing the ops.
///
/// For lazily computed values or diffing by a derived key, see
/// [`crate::algorithms::CachedLookup`].
pub fn capture_diff_slices<T>(alg: Algorithm, old: &[T], new: &[T]) -> Vec<DiffOp>
where
    T: Eq + Hash,
{
    capture_diff_slices_deadline(alg, old, new, None)
}

/// Creates a diff between old and new with the given algorithm capturing the ops.
///
/// Works like [`capture_diff_slices`] but with an optional deadline.
pub fn capture_diff_slices_deadline<T>(
    alg: Algorithm,
    old: &[T],
    new: &[T],
    deadline: Option<Instant>,
) -> Vec<DiffOp>
where
    T: Eq + Hash,
{
    capture_diff_deadline(alg, old, 0..old.len(), new, 0..new.len(), deadline)
}

/// Creates a diff between old and new slices using a derived key.
///
/// This eagerly computes the derived keys for both slices and then diffs the
/// resulting key vectors. For lazily computed or virtual sequences, use
/// [`crate::algorithms::CachedLookup`] directly.
pub fn capture_diff_slices_by_key<T, K>(
    alg: Algorithm,
    old: &[T],
    new: &[T],
    key: impl Fn(&T) -> K,
) -> Vec<DiffOp>
where
    K: Eq + Hash,
{
    capture_diff_slices_by_key_deadline(alg, old, new, key, None)
}

/// Creates a diff between old and new slices using a derived key.
///
/// Works like [`capture_diff_slices_by_key`] but with an optional deadline.
pub fn capture_diff_slices_by_key_deadline<T, K>(
    alg: Algorithm,
    old: &[T],
    new: &[T],
    key: impl Fn(&T) -> K,
    deadline: Option<Instant>,
) -> Vec<DiffOp>
where
    K: Eq + Hash,
{
    let old_keys = old.iter().map(&key).collect::<Vec<_>>();
    let new_keys = new.iter().map(key).collect::<Vec<_>>();
    capture_diff_deadline(
        alg,
        &old_keys,
        0..old_keys.len(),
        &new_keys,
        0..new_keys.len(),
        deadline,
    )
}

/// Return a measure of similarity in the range `0..=1`.
///
/// A ratio of `1.0` means the two sequences are a complete match, a
/// ratio of `0.0` would indicate completely distinct sequences.  The input
/// is the sequence of diff operations and the length of the old and new
/// sequence.
pub fn diff_ratio(ops: &[DiffOp], old_len: usize, new_len: usize) -> f32 {
    let matches = ops
        .iter()
        .map(|op| {
            if let DiffOp::Equal { len, .. } = *op {
                len
            } else {
                0
            }
        })
        .sum::<usize>();
    let len = old_len + new_len;
    if len == 0 {
        1.0
    } else {
        2.0 * matches as f32 / len as f32
    }
}

/// Isolate change clusters by eliminating ranges with no changes.
///
/// This will leave holes behind in long periods of equal ranges so that
/// you can build things like unified diffs.
pub fn group_diff_ops(mut ops: Vec<DiffOp>, n: usize) -> Vec<Vec<DiffOp>> {
    if ops.is_empty() {
        return vec![];
    }

    let mut pending_group = Vec::new();
    let mut rv = Vec::new();

    if let Some(DiffOp::Equal {
        old_index,
        new_index,
        len,
    }) = ops.first_mut()
    {
        let offset = (*len).saturating_sub(n);
        *old_index += offset;
        *new_index += offset;
        *len -= offset;
    }

    if let Some(DiffOp::Equal { len, .. }) = ops.last_mut() {
        *len -= (*len).saturating_sub(n);
    }

    for op in ops.into_iter() {
        if let DiffOp::Equal {
            old_index,
            new_index,
            len,
        } = op
        {
            // End the current group and start a new one whenever
            // there is a large range with no changes.
            if len > n * 2 {
                pending_group.push(DiffOp::Equal {
                    old_index,
                    new_index,
                    len: n,
                });
                rv.push(pending_group);
                let offset = len.saturating_sub(n);
                pending_group = vec![DiffOp::Equal {
                    old_index: old_index + offset,
                    new_index: new_index + offset,
                    len: len - offset,
                }];
                continue;
            }
        }
        pending_group.push(op);
    }

    match &pending_group[..] {
        &[] | &[DiffOp::Equal { .. }] => {}
        _ => rv.push(pending_group),
    }

    rv
}

#[test]
fn test_non_string_iter_change() {
    use crate::ChangeTag;

    let old = vec![1, 2, 3];
    let new = vec![1, 2, 4];
    let ops = capture_diff_slices(Algorithm::Myers, &old, &new);
    let changes: Vec<_> = ops
        .iter()
        .flat_map(|x| x.iter_changes(&old, &new))
        .map(|x| (x.tag(), x.value()))
        .collect();

    assert_eq!(
        changes,
        vec![
            (ChangeTag::Equal, 1),
            (ChangeTag::Equal, 2),
            (ChangeTag::Delete, 3),
            (ChangeTag::Insert, 4),
        ]
    );
}

#[test]
fn test_compact_keeps_diffop_cursors_contiguous() {
    let a = vec![1, 1, 0, 2];
    let b = vec![2, 1, 0, 0, 0, 0];
    let ops = capture_diff_slices(Algorithm::Myers, &a, &b);

    for i in 1..ops.len() {
        let prev = &ops[i - 1];
        let op = &ops[i];
        assert_eq!(
            op.old_range().start,
            prev.old_range().end,
            "old_index gap at op {i}: {prev:?} -> {op:?}"
        );
        assert_eq!(
            op.new_range().start,
            prev.new_range().end,
            "new_index gap at op {i}: {prev:?} -> {op:?}"
        );
    }
}

#[test]
fn test_capture_diff_slices_by_key() {
    #[derive(Debug, Clone)]
    struct Record {
        id: u32,
        _value: &'static str,
    }

    let old = vec![
        Record { id: 1, _value: "a" },
        Record { id: 2, _value: "b" },
        Record { id: 3, _value: "c" },
    ];
    let new = vec![
        Record { id: 1, _value: "x" },
        Record { id: 4, _value: "d" },
        Record { id: 3, _value: "z" },
    ];

    let ops = capture_diff_slices_by_key(Algorithm::Myers, &old, &new, |record| record.id);
    let changes: Vec<_> = ops
        .iter()
        .flat_map(|op| op.iter_changes(&old, &new))
        .map(|change| (change.tag(), change.value().id))
        .collect();

    use crate::ChangeTag;
    assert_eq!(
        changes,
        vec![
            (ChangeTag::Equal, 1),
            (ChangeTag::Delete, 2),
            (ChangeTag::Insert, 4),
            (ChangeTag::Equal, 3),
        ]
    );
}

#[test]
fn test_capture_diff_slices_by_key_all_algorithms() {
    let old = vec![(1, "a"), (2, "b"), (3, "c")];
    let new = vec![(1, "x"), (3, "y"), (4, "z")];

    for alg in [
        Algorithm::Myers,
        Algorithm::Patience,
        Algorithm::Lcs,
        Algorithm::Hunt,
        Algorithm::Histogram,
    ] {
        let ops = capture_diff_slices_by_key(alg, &old, &new, |value| value.0);
        let changes: Vec<_> = ops
            .iter()
            .flat_map(|op| op.iter_changes(&old, &new))
            .map(|change| (change.tag(), change.value().0))
            .collect();

        use crate::ChangeTag;
        assert_eq!(
            changes,
            vec![
                (ChangeTag::Equal, 1),
                (ChangeTag::Delete, 2),
                (ChangeTag::Equal, 3),
                (ChangeTag::Insert, 4),
            ],
            "failed for {alg:?}"
        );
    }
}

#[test]
fn test_capture_diff_slices_by_key_deadline() {
    use core::time::Duration;

    let old = vec![(1, "a"), (2, "b")];
    let new = vec![(1, "x"), (3, "c")];

    let ops = capture_diff_slices_by_key_deadline(
        Algorithm::Myers,
        &old,
        &new,
        |value| value.0,
        Some(Instant::now() + Duration::from_secs(1)),
    );

    assert!(matches!(ops[0], DiffOp::Equal { len: 1, .. }));
    assert!(ops.iter().any(|op| matches!(op, DiffOp::Replace { .. })));
}

#[test]
fn test_myers_compacts_adjacent_deletes_issue_80() {
    let a: Vec<u8> = vec![0, 1, 0, 0, 0, 1, 2];
    let b: Vec<u8> = vec![1, 0, 1];

    let ops = capture_diff_slices(Algorithm::Myers, &a, &b);

    let delete_lengths = ops
        .iter()
        .filter_map(|op| match op {
            DiffOp::Delete { old_len, .. } => Some(*old_len),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(delete_lengths, vec![1, 2, 1]);
}

#[test]
fn test_myers_unbalanced_regressions() {
    {
        let mut old = (0..3_000u32).collect::<Vec<_>>();
        let mut new = (0..2_999u32).collect::<Vec<_>>();

        old[2_999] = 1_000_000;
        new.push(2_000_000);
        new.extend((0..100_000u32).map(|i| 3_000_000 + i));

        let ops = capture_diff_slices(Algorithm::Myers, &old, &new);

        assert_eq!(
            ops,
            vec![
                DiffOp::Equal {
                    old_index: 0,
                    new_index: 0,
                    len: 2_999,
                },
                DiffOp::Replace {
                    old_index: 2_999,
                    old_len: 1,
                    new_index: 2_999,
                    new_len: 100_001,
                },
            ]
        );
    }

    {
        let mut old = (0..3_008u32).collect::<Vec<_>>();
        let mut new = (0..3_000u32).collect::<Vec<_>>();

        // Make the old tail distinct from the new tail except for a single
        // sparse overlap far into the new side.
        for i in 0..8 {
            old[3_000 + i] = 1_000_000 + i as u32;
        }

        new.extend((0..100_000u32).map(|i| 2_000_000 + i));
        new[3_000 + 50_000] = 1_000_000;

        let ops = capture_diff_slices(Algorithm::Myers, &old, &new);

        // Ensure the sparse overlap is preserved (do not collapse into one
        // large replace due to pathological fallback behavior).
        assert!(ops.iter().any(|op| {
            matches!(
                op,
                DiffOp::Equal {
                    old_index: 3_000,
                    new_index: 53_000,
                    len: 1,
                }
            )
        }));
    }
}

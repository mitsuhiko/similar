use crate::algorithms::hook::DiffHook;
use std::convert::Infallible;
use std::ops::Range;

/// Utility enum to capture a diff operation.
///
/// This is used by [`Capture`](crate::algorithms::Capture).
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum DiffOp {
    /// A segment is equal (see [`DiffHook::equal`])
    Equal {
        /// The starting index in the old sequence.
        old_index: usize,
        /// The starting index in the new sequence.
        new_index: usize,
        /// The length of the segment.
        len: usize,
    },
    /// A segment was deleted (see [`DiffHook::delete`])
    Delete {
        /// The starting index in the old sequence.
        old_index: usize,
        /// The length of the old segment.
        old_len: usize,
        /// The starting index in the new sequence.
        new_index: usize,
    },
    /// A segment was inserted (see [`DiffHook::insert`])
    Insert {
        /// The starting index in the old sequence.
        old_index: usize,
        /// The starting index in the new sequence.
        new_index: usize,
        /// The length of the new segment.
        new_len: usize,
    },
    /// A segment was replaced (see [`DiffHook::replace`])
    Replace {
        /// The starting index in the old sequence.
        old_index: usize,
        /// The length of the old segment.
        old_len: usize,
        /// The starting index in the new sequence.
        new_index: usize,
        /// The length of the new segment.
        new_len: usize,
    },
}

/// The tag of a diff operation.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Ord, PartialOrd)]
pub enum DiffTag {
    /// The diff op encodes an equal segment.
    Equal,
    /// The diff op encodes a deleted segment.
    Delete,
    /// The diff op encodes an inserted segment.
    Insert,
    /// The diff op encodes a replaced segment.
    Replace,
}

impl DiffOp {
    /// Returns the tag of the operation.
    pub fn tag(self) -> DiffTag {
        self.as_tag_tuple().0
    }

    /// Returns the old range.
    pub fn old_range(&self) -> Range<usize> {
        self.as_tag_tuple().1
    }

    /// Returns the new range.
    pub fn new_range(&self) -> Range<usize> {
        self.as_tag_tuple().2
    }

    /// Transform the op into a tuple of diff tag and ranges.
    ///
    /// This is useful when operating on slices.  The returned format is
    /// `(tag, i1..i2, j1..j2)`:
    ///
    /// * `Replace`: `a[i1..i2]` should be replaced by `b[j1..j2]`
    /// * `Delete`: `a[i1..i2]` should be deleted (`j1 == j2` in this case).
    /// * `Insert`: `b[j1..j2]` should be inserted at `a[i1..i2]` (`i1 == i2` in this case).
    /// * `Equal`: `a[i1..i2]` is equal to `b[j1..j2]`.
    pub fn as_tag_tuple(&self) -> (DiffTag, Range<usize>, Range<usize>) {
        match *self {
            DiffOp::Equal {
                old_index,
                new_index,
                len,
            } => (
                DiffTag::Equal,
                old_index..old_index + len,
                new_index..new_index + len,
            ),
            DiffOp::Delete {
                old_index,
                new_index,
                old_len,
            } => (
                DiffTag::Delete,
                old_index..old_index + old_len,
                new_index..new_index,
            ),
            DiffOp::Insert {
                old_index,
                new_index,
                new_len,
            } => (
                DiffTag::Insert,
                old_index..old_index,
                new_index..new_index + new_len,
            ),
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => (
                DiffTag::Replace,
                old_index..old_index + old_len,
                new_index..new_index + new_len,
            ),
        }
    }

    /// Apply this operation to a diff hook.
    pub fn apply_to_hook<D: DiffHook>(&self, d: &mut D) -> Result<(), D::Error> {
        match *self {
            DiffOp::Equal {
                old_index,
                new_index,
                len,
            } => d.equal(old_index, new_index, len),
            DiffOp::Delete {
                old_index,
                old_len,
                new_index,
            } => d.delete(old_index, old_len, new_index),
            DiffOp::Insert {
                old_index,
                new_index,
                new_len,
            } => d.insert(old_index, new_index, new_len),
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => d.replace(old_index, old_len, new_index, new_len),
        }
    }
}

/// A [`DiffHook`] that captures all diff operations.
#[derive(Default, Clone)]
pub struct Capture(Vec<DiffOp>);

impl Capture {
    /// Creates a new capture hook.
    pub fn new() -> Capture {
        Capture::default()
    }

    /// Converts the capture hook into a vector of ops.
    pub fn into_ops(self) -> Vec<DiffOp> {
        self.0
    }

    /// Isolate change clusters by eliminating ranges with no changes.
    ///
    /// This is equivalent to calling [`group_diff_ops`] on [`Capture::into_ops`].
    pub fn into_grouped_ops(self, n: usize) -> Vec<Vec<DiffOp>> {
        group_diff_ops(self.into_ops(), n)
    }

    /// Accesses the captured operations.
    pub fn ops(&self) -> &[DiffOp] {
        &self.0
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

/// Return a measure of similarity in the range `0..=1`.
///
/// A ratio of `1.0` means the two sequences are a complete match, a
/// ratio of `0.0` would indicate completely distinct sequences.  The input
/// is the sequence of diff operations and the length of the old and new
/// sequence.
pub fn get_diff_ratio(ops: &[DiffOp], old_len: usize, new_len: usize) -> f32 {
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

impl DiffHook for Capture {
    type Error = Infallible;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Equal {
            old_index,
            new_index,
            len,
        });
        Ok(())
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Delete {
            old_index,
            old_len,
            new_index,
        });
        Ok(())
    }

    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Insert {
            old_index,
            new_index,
            new_len,
        });
        Ok(())
    }

    fn replace(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Replace {
            old_index,
            old_len,
            new_index,
            new_len,
        });
        Ok(())
    }
}

#[test]
fn test_capture_hook_grouping() {
    use crate::algorithms::{myers, Replace};

    let rng = (1..100).collect::<Vec<_>>();
    let mut rng_new = rng.clone();
    rng_new[10] = 1000;
    rng_new[13] = 1000;
    rng_new[16] = 1000;
    rng_new[34] = 1000;

    let mut d = Replace::new(Capture::new());
    myers::diff_slices(&mut d, &rng, &rng_new).unwrap();

    let ops = d.into_inner().into_grouped_ops(3);
    let tags = ops
        .iter()
        .map(|group| group.iter().map(|x| x.as_tag_tuple()).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    insta::assert_debug_snapshot!(ops);
    insta::assert_debug_snapshot!(tags);
}

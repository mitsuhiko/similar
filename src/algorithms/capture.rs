use std::convert::Infallible;

use crate::algorithms::hook::DiffHook;
use crate::DiffOp;

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

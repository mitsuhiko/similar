//! Implements basic compacting.  This is based on the compaction logic from
//! diffy by Brandon Williams.
use alloc::vec::Vec;
use core::ops::Index;

use crate::{DiffOp, DiffTag};

use super::DiffHook;
use super::utils::{common_prefix_len, common_suffix_len};

/// Performs semantic cleanup operations on a diff.
///
/// This merges similar ops together but also tries to move hunks up and
/// down the diff with the desire to connect as many hunks as possible.
/// It still needs to be combined with [`Replace`](crate::algorithms::Replace)
/// to get actual replace diff ops out.
#[derive(Debug)]
pub struct Compact<'old, 'new, Old: ?Sized, New: ?Sized, D> {
    d: D,
    ops: Vec<DiffOp>,
    old: &'old Old,
    new: &'new New,
}

impl<'old, 'new, Old, New, D> Compact<'old, 'new, Old, New, D>
where
    D: DiffHook,
    Old: Index<usize> + ?Sized + 'old,
    New: Index<usize> + ?Sized + 'new,
    New::Output: PartialEq<Old::Output>,
{
    /// Creates a new compact hook wrapping another hook.
    pub fn new(d: D, old: &'old Old, new: &'new New) -> Self {
        Compact {
            d,
            ops: Vec::new(),
            old,
            new,
        }
    }

    /// Extracts the inner hook.
    pub fn into_inner(self) -> D {
        self.d
    }
}

impl<Old: ?Sized, New: ?Sized, D: DiffHook> AsRef<D> for Compact<'_, '_, Old, New, D> {
    fn as_ref(&self) -> &D {
        &self.d
    }
}

impl<Old: ?Sized, New: ?Sized, D: DiffHook> AsMut<D> for Compact<'_, '_, Old, New, D> {
    fn as_mut(&mut self) -> &mut D {
        &mut self.d
    }
}

impl<'old, 'new, Old, New, D> DiffHook for Compact<'old, 'new, Old, New, D>
where
    D: DiffHook,
    Old: Index<usize> + ?Sized + 'old,
    New: Index<usize> + ?Sized + 'new,
    New::Output: PartialEq<Old::Output>,
{
    type Error = D::Error;

    #[inline(always)]
    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        self.ops.push(DiffOp::Equal {
            old_index,
            new_index,
            len,
        });
        Ok(())
    }

    #[inline(always)]
    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        self.ops.push(DiffOp::Delete {
            old_index,
            old_len,
            new_index,
        });
        Ok(())
    }

    #[inline(always)]
    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.ops.push(DiffOp::Insert {
            old_index,
            new_index,
            new_len,
        });
        Ok(())
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        cleanup_diff_ops(self.old, self.new, &mut self.ops);
        for op in &self.ops {
            op.apply_to_hook(&mut self.d)?;
        }
        self.d.finish()
    }
}

/// Storage the compaction passes operate on.
///
/// Edits happen right next to the cursor, so a gap buffer keeps them O(1).
/// Small op lists stay in the plain vector to avoid the extra allocation.
trait OpsStore {
    fn len(&self) -> usize;
    fn get(&self, index: usize) -> Option<&DiffOp>;
    fn index(&self, index: usize) -> &DiffOp;
    fn index_mut(&mut self, index: usize) -> &mut DiffOp;
    fn insert(&mut self, index: usize, op: DiffOp);
    fn remove(&mut self, index: usize);
    fn swap(&mut self, left: usize, right: usize);
    /// Hint that the cursor moved to `index`.
    fn seek(&mut self, index: usize);
}

impl OpsStore for Vec<DiffOp> {
    #[inline]
    fn len(&self) -> usize {
        Vec::len(self)
    }

    #[inline]
    fn get(&self, index: usize) -> Option<&DiffOp> {
        <[DiffOp]>::get(self, index)
    }

    #[inline]
    fn index(&self, index: usize) -> &DiffOp {
        &self[index]
    }

    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut DiffOp {
        &mut self[index]
    }

    #[inline]
    fn insert(&mut self, index: usize, op: DiffOp) {
        Vec::insert(self, index, op);
    }

    #[inline]
    fn remove(&mut self, index: usize) {
        Vec::remove(self, index);
    }

    #[inline]
    fn swap(&mut self, left: usize, right: usize) {
        <[DiffOp]>::swap(self, left, right);
    }

    #[inline]
    fn seek(&mut self, _index: usize) {}
}

/// A gap buffer of diff ops.
///
/// The compaction passes walk the ops from left to right and only ever
/// insert or remove ops right next to their cursor.  Keeping the ops in two
/// stacks that meet at the cursor turns those edits into pushes and pops
/// instead of shifting the tail of a vector on every edit, which made the
/// cleanup quadratic on diffs with many ops.
struct OpsBuffer {
    front: Vec<DiffOp>,
    /// Ops after the gap in reverse order, so the op right after the gap is
    /// the last element.
    back: Vec<DiffOp>,
}

impl OpsBuffer {
    fn new(ops: &mut Vec<DiffOp>) -> Self {
        let mut back = core::mem::take(ops);
        back.reverse();
        OpsBuffer {
            front: Vec::with_capacity(back.len()),
            back,
        }
    }

    fn into_vec(mut self, ops: &mut Vec<DiffOp>) {
        self.front.extend(self.back.drain(..).rev());
        *ops = self.front;
    }

    #[inline]
    fn move_gap(&mut self, index: usize) {
        while self.front.len() > index {
            let op = self.front.pop().unwrap();
            self.back.push(op);
        }
        while self.front.len() < index {
            let op = self.back.pop().expect("index out of bounds");
            self.front.push(op);
        }
    }
}

impl OpsStore for OpsBuffer {
    #[inline]
    fn len(&self) -> usize {
        self.front.len() + self.back.len()
    }

    #[inline]
    fn get(&self, index: usize) -> Option<&DiffOp> {
        if index < self.front.len() {
            self.front.get(index)
        } else {
            let offset = index - self.front.len();
            if offset < self.back.len() {
                self.back.get(self.back.len() - 1 - offset)
            } else {
                None
            }
        }
    }

    #[inline]
    fn index(&self, index: usize) -> &DiffOp {
        self.get(index).expect("index out of bounds")
    }

    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut DiffOp {
        if index < self.front.len() {
            &mut self.front[index]
        } else {
            let offset = index - self.front.len();
            let back_len = self.back.len();
            &mut self.back[back_len - 1 - offset]
        }
    }

    fn insert(&mut self, index: usize, op: DiffOp) {
        self.move_gap(index);
        self.front.push(op);
    }

    fn remove(&mut self, index: usize) {
        self.move_gap(index);
        self.back.pop().expect("index out of bounds");
    }

    fn swap(&mut self, left: usize, right: usize) {
        let tmp = *self.index(left);
        *self.index_mut(left) = *self.index(right);
        *self.index_mut(right) = tmp;
    }

    #[inline]
    fn seek(&mut self, index: usize) {
        self.move_gap(index.min(self.len()));
    }
}

/// Op lists up to this size are compacted in place in the vector; the
/// shifting cost is negligible there and it avoids allocating a gap buffer.
const IN_PLACE_COMPACT_MAX_OPS: usize = 64;

#[test]
fn test_gap_buffer_matches_vector_compaction() {
    let mut seed = 42u64;
    let mut random = || {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        seed as usize
    };
    let mut large_cases = 0;
    for len in 0..512 {
        let old: Vec<_> = (0..len).map(|_| random() % 8).collect();
        let new: Vec<_> = (0..len).map(|_| random() % 8).collect();
        let mut capture = crate::algorithms::Capture::new();
        crate::algorithms::myers::diff(&mut capture, &old, 0..len, &new, 0..len).unwrap();
        let mut actual = capture.into_ops();
        large_cases += (actual.len() > IN_PLACE_COMPACT_MAX_OPS) as usize;
        let mut expected = actual.clone();
        shift_all_diff_ops(&mut expected, &old, &new);
        normalize_diff_op_cursors(&mut expected);
        let mut buffer = OpsBuffer::new(&mut actual);
        shift_all_diff_ops(&mut buffer, &old, &new);
        buffer.into_vec(&mut actual);
        normalize_diff_op_cursors(&mut actual);
        assert_eq!(actual, expected, "length {len}");
    }
    assert!(large_cases > 100);
}

// Walks through all edits and shifts them up and then down, trying to see if
// they run into similar edits which can be merged.
pub fn cleanup_diff_ops<Old, New>(old: &Old, new: &New, ops: &mut Vec<DiffOp>)
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    if ops.len() <= IN_PLACE_COMPACT_MAX_OPS {
        shift_all_diff_ops(ops, old, new);
    } else {
        let mut buffer = OpsBuffer::new(ops);
        shift_all_diff_ops(&mut buffer, old, new);
        buffer.into_vec(ops);
    }
    normalize_diff_op_cursors(ops);
}

fn shift_all_diff_ops<S, Old, New>(ops: &mut S, old: &Old, new: &New)
where
    S: OpsStore,
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    // First attempt to compact all Deletions
    let mut pointer = 0;
    while let Some(&op) = ops.get(pointer) {
        if let DiffTag::Delete = op.tag() {
            pointer = shift_diff_ops_up(ops, old, new, pointer);
            pointer = shift_diff_ops_down(ops, old, new, pointer);
        }
        pointer += 1;
        ops.seek(pointer);
    }

    // Then attempt to compact all Insertions
    let mut pointer = 0;
    ops.seek(0);
    while let Some(&op) = ops.get(pointer) {
        if let DiffTag::Insert = op.tag() {
            pointer = shift_diff_ops_up(ops, old, new, pointer);
            pointer = shift_diff_ops_down(ops, old, new, pointer);
        }
        pointer += 1;
        ops.seek(pointer);
    }
}

fn normalize_diff_op_cursors(ops: &mut [DiffOp]) {
    let mut old_cursor = 0;
    let mut new_cursor = 0;

    if let Some(op) = ops.first() {
        old_cursor = op.old_range().start;
        new_cursor = op.new_range().start;
    }

    for op in ops.iter_mut() {
        match op {
            DiffOp::Equal {
                old_index,
                new_index,
                len,
            } => {
                old_cursor = *old_index + *len;
                new_cursor = *new_index + *len;
            }
            DiffOp::Delete {
                old_index,
                old_len,
                new_index,
            } => {
                *new_index = new_cursor;
                old_cursor = *old_index + *old_len;
            }
            DiffOp::Insert {
                old_index,
                new_index,
                new_len,
            } => {
                *old_index = old_cursor;
                new_cursor = *new_index + *new_len;
            }
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                old_cursor = *old_index + *old_len;
                new_cursor = *new_index + *new_len;
            }
        }
    }
}

fn swap_adjacent_insert_delete<S: OpsStore>(ops: &mut S, left: usize) {
    let right = left + 1;
    debug_assert!(matches!(
        (ops.index(left).tag(), ops.index(right).tag()),
        (DiffTag::Insert, DiffTag::Delete) | (DiffTag::Delete, DiffTag::Insert)
    ));

    let old_start = ops.index(left).old_range().start;
    let old_end = ops.index(right).old_range().end;
    let new_start = ops.index(left).new_range().start;
    let new_end = ops.index(right).new_range().end;

    ops.swap(left, right);

    match ops.index_mut(left) {
        DiffOp::Insert { old_index, .. } => *old_index = old_start,
        DiffOp::Delete { new_index, .. } => *new_index = new_start,
        _ => unreachable!("expected insert/delete pair"),
    }
    match ops.index_mut(right) {
        DiffOp::Insert { old_index, .. } => *old_index = old_end,
        DiffOp::Delete { new_index, .. } => *new_index = new_end,
        _ => unreachable!("expected insert/delete pair"),
    }
}

fn shift_diff_ops_up<S, Old, New>(ops: &mut S, old: &Old, new: &New, mut pointer: usize) -> usize
where
    S: OpsStore,
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    while let Some(&prev_op) = pointer.checked_sub(1).and_then(|idx| ops.get(idx)) {
        let this_op = *ops.index(pointer);
        match (this_op.tag(), prev_op.tag()) {
            // Shift Inserts Upwards
            (DiffTag::Insert, DiffTag::Equal) => {
                let suffix_len =
                    common_suffix_len(old, prev_op.old_range(), new, this_op.new_range());
                if suffix_len > 0 {
                    if let Some(DiffTag::Equal) = ops.get(pointer + 1).map(|x| x.tag()) {
                        ops.index_mut(pointer + 1).grow_left(suffix_len);
                    } else {
                        ops.insert(
                            pointer + 1,
                            DiffOp::Equal {
                                old_index: prev_op.old_range().end - suffix_len,
                                new_index: this_op.new_range().end - suffix_len,
                                len: suffix_len,
                            },
                        );
                    }
                    ops.index_mut(pointer).shift_left(suffix_len);
                    ops.index_mut(pointer - 1).shrink_left(suffix_len);

                    if ops.index(pointer - 1).is_empty() {
                        ops.remove(pointer - 1);
                        pointer -= 1;
                    }
                } else if ops.index(pointer - 1).is_empty() {
                    ops.remove(pointer - 1);
                    pointer -= 1;
                } else {
                    // We can't shift upwards anymore
                    break;
                }
            }
            // Shift Deletions Upwards
            (DiffTag::Delete, DiffTag::Equal) => {
                let can_merge_with_previous_delete = pointer
                    .checked_sub(2)
                    .and_then(|idx| ops.get(idx))
                    .map(|op| op.tag() == DiffTag::Delete)
                    .unwrap_or(false);
                if !can_merge_with_previous_delete {
                    break;
                }

                // check common suffix for the amount we can shift
                let suffix_len =
                    common_suffix_len(old, this_op.old_range(), new, prev_op.new_range());
                if suffix_len != 0 {
                    if let Some(DiffTag::Equal) = ops.get(pointer + 1).map(|x| x.tag()) {
                        ops.index_mut(pointer + 1).grow_left(suffix_len);
                    } else {
                        let old_range = this_op.old_range();
                        let new_range = prev_op.new_range();
                        ops.insert(
                            pointer + 1,
                            DiffOp::Equal {
                                old_index: old_range.end - suffix_len,
                                new_index: new_range.end - suffix_len,
                                len: suffix_len,
                            },
                        );
                    }
                    ops.index_mut(pointer).shift_left(suffix_len);
                    ops.index_mut(pointer - 1).shrink_left(suffix_len);

                    if ops.index(pointer - 1).is_empty() {
                        ops.remove(pointer - 1);
                        pointer -= 1;
                    }
                } else if ops.index(pointer - 1).is_empty() {
                    ops.remove(pointer - 1);
                    pointer -= 1;
                } else {
                    // We can't shift upwards anymore
                    break;
                }
            }
            // Swap the Delete and Insert.  Since the two ops consume input on
            // different sides, their cursor positions need to be patched up to
            // keep the stream contiguous after swapping.
            (DiffTag::Insert, DiffTag::Delete) | (DiffTag::Delete, DiffTag::Insert) => {
                swap_adjacent_insert_delete(ops, pointer - 1);
                pointer -= 1;
            }
            // Merge the two ranges
            (DiffTag::Insert, DiffTag::Insert) => {
                ops.index_mut(pointer - 1)
                    .grow_right(this_op.new_range().len());
                ops.remove(pointer);
                pointer -= 1;
            }
            (DiffTag::Delete, DiffTag::Delete) => {
                ops.index_mut(pointer - 1)
                    .grow_right(this_op.old_range().len());
                ops.remove(pointer);
                pointer -= 1;
            }
            _ => unreachable!("unexpected tag"),
        }
    }
    pointer
}

fn shift_diff_ops_down<S, Old, New>(ops: &mut S, old: &Old, new: &New, mut pointer: usize) -> usize
where
    S: OpsStore,
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    while let Some(&next_op) = pointer.checked_add(1).and_then(|idx| ops.get(idx)) {
        let this_op = *ops.index(pointer);
        match (this_op.tag(), next_op.tag()) {
            // Shift Inserts Downwards
            (DiffTag::Insert, DiffTag::Equal) => {
                let prefix_len =
                    common_prefix_len(old, next_op.old_range(), new, this_op.new_range());
                if prefix_len > 0 {
                    if let Some(DiffTag::Equal) = pointer
                        .checked_sub(1)
                        .and_then(|x| ops.get(x))
                        .map(|x| x.tag())
                    {
                        ops.index_mut(pointer - 1).grow_right(prefix_len);
                    } else {
                        ops.insert(
                            pointer,
                            DiffOp::Equal {
                                old_index: next_op.old_range().start,
                                new_index: this_op.new_range().start,
                                len: prefix_len,
                            },
                        );
                        pointer += 1;
                    }
                    ops.index_mut(pointer).shift_right(prefix_len);
                    ops.index_mut(pointer + 1).shrink_right(prefix_len);

                    if ops.index(pointer + 1).is_empty() {
                        ops.remove(pointer + 1);
                    }
                } else if ops.index(pointer + 1).is_empty() {
                    ops.remove(pointer + 1);
                } else {
                    // We can't shift upwards anymore
                    break;
                }
            }
            // Shift Deletions Downwards
            (DiffTag::Delete, DiffTag::Equal) => {
                let can_merge_with_next_delete = ops
                    .get(pointer + 2)
                    .map(|op| op.tag() == DiffTag::Delete)
                    .unwrap_or(false);
                if !can_merge_with_next_delete {
                    break;
                }

                // check common prefix for the amount we can shift
                let prefix_len =
                    common_prefix_len(old, this_op.old_range(), new, next_op.new_range());
                if prefix_len > 0 {
                    if let Some(DiffTag::Equal) = pointer
                        .checked_sub(1)
                        .and_then(|x| ops.get(x))
                        .map(|x| x.tag())
                    {
                        ops.index_mut(pointer - 1).grow_right(prefix_len);
                    } else {
                        ops.insert(
                            pointer,
                            DiffOp::Equal {
                                old_index: this_op.old_range().start,
                                new_index: this_op.new_range().start,
                                len: prefix_len,
                            },
                        );
                        pointer += 1;
                    }
                    ops.index_mut(pointer).shift_right(prefix_len);
                    ops.index_mut(pointer + 1).shrink_right(prefix_len);

                    if ops.index(pointer + 1).is_empty() {
                        ops.remove(pointer + 1);
                    }
                } else if ops.index(pointer + 1).is_empty() {
                    ops.remove(pointer + 1);
                } else {
                    // We can't shift downwards anymore
                    break;
                }
            }
            // Swap the Delete and Insert.  Since the two ops consume input on
            // different sides, their cursor positions need to be patched up to
            // keep the stream contiguous after swapping.
            (DiffTag::Insert, DiffTag::Delete) | (DiffTag::Delete, DiffTag::Insert) => {
                swap_adjacent_insert_delete(ops, pointer);
                pointer += 1;
            }
            // Merge the two ranges
            (DiffTag::Insert, DiffTag::Insert) => {
                ops.index_mut(pointer).grow_right(next_op.new_range().len());
                ops.remove(pointer + 1);
            }
            (DiffTag::Delete, DiffTag::Delete) => {
                ops.index_mut(pointer).grow_right(next_op.old_range().len());
                ops.remove(pointer + 1);
            }
            _ => unreachable!("unexpected tag"),
        }
    }
    pointer
}

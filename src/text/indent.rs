//! Indentation based positioning of ambiguous hunks.
//!
//! When a group of inserted or deleted lines is surrounded by matching lines
//! it can often be shown at several positions that are all equally valid.
//! For instance adding a function to a file where every function ends with
//! the same closing line can be shown as inserting the new function before
//! or after that closing line.  This module implements the "indent
//! heuristic" from git (`git diff --indent-heuristic`, the default since git
//! 2.14) which scores the candidate positions based on blank lines and
//! indentation of the surrounding lines and picks the one that reads best.
//!
//! The weights are the ones git uses.  They were determined empirically by
//! Michael Haggerty, see <https://github.com/mhagger/diff-slider-tools>.
use alloc::vec;
use alloc::vec::Vec;
use core::ops::{Index, Range};

use crate::DiffOp;
use crate::algorithms::{Capture, DiffHook, Replace};

/// Indentation is clamped to this value.
const MAX_INDENT: i32 = 200;

/// Runs of blank lines are only counted up to this length.
const MAX_BLANKS: i32 = 20;

/// Penalty if there are no non-blank lines before the split.
const START_OF_FILE_PENALTY: i32 = 1;
/// Penalty if there are no non-blank lines after the split.
const END_OF_FILE_PENALTY: i32 = 21;
/// Multiplier for the number of blank lines around the split.
const TOTAL_BLANK_WEIGHT: i32 = -30;
/// Multiplier for the number of blank lines after the split.
const POST_BLANK_WEIGHT: i32 = 6;
/// Penalties if the line is indented more than its predecessor.
const RELATIVE_INDENT_PENALTY: i32 = -4;
const RELATIVE_INDENT_WITH_BLANK_PENALTY: i32 = 10;
/// Penalties if the line is indented less than its predecessor and its
/// successor.
const RELATIVE_OUTDENT_PENALTY: i32 = 24;
const RELATIVE_OUTDENT_WITH_BLANK_PENALTY: i32 = 17;
/// Penalties if the line is indented less than its predecessor but not less
/// than its successor.
const RELATIVE_DEDENT_PENALTY: i32 = 23;
const RELATIVE_DEDENT_WITH_BLANK_PENALTY: i32 = 17;
/// Weight of the comparison of the effective indents of two positions.
const INDENT_WEIGHT: i32 = 60;
/// Hunks are only scored over this many candidate positions.
const MAX_SLIDING: usize = 100;

/// Returns the indentation of a line with tabs expanded to multiples of 8, or
/// `-1` if the line is blank.
pub(crate) fn line_indent(line: &[u8]) -> i32 {
    let mut indent = 0;
    for &c in line {
        match c {
            b' ' => indent += 1,
            b'\t' => indent += 8 - indent % 8,
            b'\n' | b'\r' | b'\x0b' | b'\x0c' => {}
            _ => return indent,
        }
        if indent >= MAX_INDENT {
            return MAX_INDENT;
        }
    }
    -1
}

/// Moves slidable hunks in `ops` to the positions preferred by the indent
/// heuristic.
///
/// `old_indent` and `new_indent` return the indentation of a line (as
/// computed by [`line_indent`]) for an absolute index.
pub(crate) fn apply_indent_heuristic<Old, New, OldIndent, NewIndent>(
    ops: &mut Vec<DiffOp>,
    old: &Old,
    new: &New,
    old_indent: OldIndent,
    new_indent: NewIndent,
) where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: PartialEq,
    New::Output: PartialEq,
    OldIndent: Fn(usize) -> i32,
    NewIndent: Fn(usize) -> i32,
{
    let (Some(first), Some(last)) = (ops.first(), ops.last()) else {
        return;
    };
    if ops.iter().all(|op| matches!(op, DiffOp::Equal { .. })) {
        return;
    }
    let old_range = first.old_range().start..last.old_range().end;
    let new_range = first.new_range().start..last.new_range().end;

    let mut old_changed = Changed::new(old_range.len());
    let mut new_changed = Changed::new(new_range.len());
    for op in ops.iter() {
        if !matches!(op, DiffOp::Equal { .. }) {
            for idx in op.old_range() {
                old_changed.set(idx - old_range.start, true);
            }
            for idx in op.new_range() {
                new_changed.set(idx - new_range.start, true);
            }
        }
    }

    let old_base = old_range.start;
    let new_base = new_range.start;
    compact(
        &mut old_changed,
        &new_changed,
        |a, b| old[old_base + a] == old[old_base + b],
        |idx| old_indent(old_base + idx),
    );
    compact(
        &mut new_changed,
        &old_changed,
        |a, b| new[new_base + a] == new[new_base + b],
        |idx| new_indent(new_base + idx),
    );

    *ops = rebuild_ops(&old_changed, old_range, &new_changed, new_range);
}

/// Per line change flags with a sentinel on either end.
struct Changed {
    flags: Vec<bool>,
}

impl Changed {
    fn new(len: usize) -> Changed {
        Changed {
            flags: vec![false; len + 2],
        }
    }

    #[inline]
    fn len(&self) -> usize {
        self.flags.len() - 2
    }

    #[inline]
    fn get(&self, idx: usize) -> bool {
        self.flags[idx + 1]
    }

    /// Like `get` but for the line before `idx`, `false` before the start.
    #[inline]
    fn get_before(&self, idx: usize) -> bool {
        self.flags[idx]
    }

    #[inline]
    fn set(&mut self, idx: usize, value: bool) {
        self.flags[idx + 1] = value;
    }
}

/// A group of changed lines.  Empty groups sit between two unchanged lines
/// (or the start or end of the file).  Groups on both sides correspond to
/// each other one to one as they are separated by the same unchanged lines.
#[derive(Clone, Copy, PartialEq, Eq)]
struct Group {
    start: usize,
    end: usize,
}

impl Group {
    fn first(changed: &Changed) -> Group {
        let mut end = 0;
        while changed.get(end) {
            end += 1;
        }
        Group { start: 0, end }
    }

    fn is_empty(&self) -> bool {
        self.start == self.end
    }

    fn next(&mut self, changed: &Changed) -> bool {
        if self.end == changed.len() {
            return false;
        }
        self.start = self.end + 1;
        self.end = self.start;
        while changed.get(self.end) {
            self.end += 1;
        }
        true
    }

    fn previous(&mut self, changed: &Changed) -> bool {
        if self.start == 0 {
            return false;
        }
        self.end = self.start - 1;
        self.start = self.end;
        while changed.get_before(self.start) {
            self.start -= 1;
        }
        true
    }

    /// Slides the group one line down, merging with a following group it
    /// runs into.
    fn slide_down(&mut self, changed: &mut Changed, eq: &impl Fn(usize, usize) -> bool) -> bool {
        if self.end < changed.len() && eq(self.start, self.end) {
            changed.set(self.start, false);
            changed.set(self.end, true);
            self.start += 1;
            self.end += 1;
            while changed.get(self.end) {
                self.end += 1;
            }
            true
        } else {
            false
        }
    }

    /// Slides the group one line up, merging with a preceding group it runs
    /// into.
    fn slide_up(&mut self, changed: &mut Changed, eq: &impl Fn(usize, usize) -> bool) -> bool {
        if self.start > 0 && eq(self.start - 1, self.end - 1) {
            self.start -= 1;
            self.end -= 1;
            changed.set(self.start, true);
            changed.set(self.end, false);
            while changed.get_before(self.start) {
                self.start -= 1;
            }
            true
        } else {
            false
        }
    }
}

/// Port of git's `xdl_change_compact` with the indent heuristic enabled.
///
/// Every group is merged with the groups it can be slid into.  A group that
/// can be aligned with a change on the other side is moved there so that the
/// change is shown as a replacement, otherwise the indent heuristic picks the
/// position.
fn compact(
    changed: &mut Changed,
    other: &Changed,
    eq: impl Fn(usize, usize) -> bool,
    indent: impl Fn(usize) -> i32,
) {
    let mut g = Group::first(changed);
    let mut go = Group::first(other);

    loop {
        if !g.is_empty() {
            let mut group_size;
            let mut earliest_end;
            let mut end_matching_other;

            loop {
                group_size = g.end - g.start;
                end_matching_other = None;

                while g.slide_up(changed, &eq) {
                    let moved = go.previous(other);
                    debug_assert!(moved, "group sync broken sliding up");
                }
                earliest_end = g.end;
                if !go.is_empty() {
                    end_matching_other = Some(g.end);
                }

                while g.slide_down(changed, &eq) {
                    let moved = go.next(other);
                    debug_assert!(moved, "group sync broken sliding down");
                    if !go.is_empty() {
                        end_matching_other = Some(g.end);
                    }
                }

                if group_size == g.end - g.start {
                    break;
                }
            }

            // The group is now as far down as possible, the code below only
            // ever needs to slide it back up.
            if g.end == earliest_end {
                // the group cannot be moved
            } else if end_matching_other.is_some() {
                while go.is_empty() {
                    let moved = g.slide_up(changed, &eq);
                    debug_assert!(moved, "match disappeared");
                    let moved = go.previous(other);
                    debug_assert!(moved, "group sync broken sliding to match");
                }
            } else {
                let mut shift = earliest_end
                    .max(g.end.saturating_sub(group_size + 1))
                    .max(g.end.saturating_sub(MAX_SLIDING));
                let mut best: Option<(usize, Score)> = None;
                while shift <= g.end {
                    let mut score = Score::default();
                    score.add_split(&measure_split(changed.len(), shift, &indent));
                    score.add_split(&measure_split(changed.len(), shift - group_size, &indent));
                    if best.is_none_or(|(_, best_score)| score.cmp(&best_score) <= 0) {
                        best = Some((shift, score));
                    }
                    shift += 1;
                }
                if let Some((best_shift, _)) = best {
                    while g.end > best_shift {
                        let moved = g.slide_up(changed, &eq);
                        debug_assert!(moved, "best shift unreached");
                        let moved = go.previous(other);
                        debug_assert!(moved, "group sync broken sliding to best shift");
                    }
                }
            }
        }

        if !g.next(changed) {
            break;
        }
        let moved = go.next(other);
        debug_assert!(moved, "group sync broken moving to next group");
    }
}

/// Characteristics of a split (a position between two lines).
struct SplitMeasurement {
    /// The split is at the end of the file.
    end_of_file: bool,
    /// Indent of the line after the split, `-1` if blank.
    indent: i32,
    /// Number of blank lines directly above the split.
    pre_blank: i32,
    /// Indent of the nearest non-blank line above the split or `-1`.
    pre_indent: i32,
    /// Number of blank lines after the line following the split.
    post_blank: i32,
    /// Indent of the nearest non-blank line after the line following the
    /// split or `-1`.
    post_indent: i32,
}

/// Measures a split directly above line `split`.
fn measure_split(len: usize, split: usize, indent: &impl Fn(usize) -> i32) -> SplitMeasurement {
    let (end_of_file, line_indent) = if split >= len {
        (true, -1)
    } else {
        (false, indent(split))
    };

    let mut pre_blank = 0;
    let mut pre_indent = -1;
    for idx in (0..split).rev() {
        pre_indent = indent(idx);
        if pre_indent != -1 {
            break;
        }
        pre_blank += 1;
        if pre_blank == MAX_BLANKS {
            pre_indent = 0;
            break;
        }
    }

    let mut post_blank = 0;
    let mut post_indent = -1;
    for idx in split.saturating_add(1)..len {
        post_indent = indent(idx);
        if post_indent != -1 {
            break;
        }
        post_blank += 1;
        if post_blank == MAX_BLANKS {
            post_indent = 0;
            break;
        }
    }

    SplitMeasurement {
        end_of_file,
        indent: line_indent,
        pre_blank,
        pre_indent,
        post_blank,
        post_indent,
    }
}

/// The badness of a hunk position.  Smaller is better.
#[derive(Default, Clone, Copy)]
struct Score {
    effective_indent: i32,
    penalty: i32,
}

impl Score {
    fn add_split(&mut self, m: &SplitMeasurement) {
        if m.pre_indent == -1 && m.pre_blank == 0 {
            self.penalty += START_OF_FILE_PENALTY;
        }
        if m.end_of_file {
            self.penalty += END_OF_FILE_PENALTY;
        }

        let post_blank = if m.indent == -1 { 1 + m.post_blank } else { 0 };
        let total_blank = m.pre_blank + post_blank;
        self.penalty += TOTAL_BLANK_WEIGHT * total_blank;
        self.penalty += POST_BLANK_WEIGHT * post_blank;

        let indent = if m.indent != -1 {
            m.indent
        } else {
            m.post_indent
        };
        let any_blanks = total_blank != 0;
        self.effective_indent += indent;

        if indent == -1 || m.pre_indent == -1 {
            // no adjustments
        } else if indent > m.pre_indent {
            self.penalty += if any_blanks {
                RELATIVE_INDENT_WITH_BLANK_PENALTY
            } else {
                RELATIVE_INDENT_PENALTY
            };
        } else if indent == m.pre_indent {
            // same indentation as the predecessor
        } else if m.post_indent != -1 && m.post_indent > indent {
            // probably the start of a block
            self.penalty += if any_blanks {
                RELATIVE_OUTDENT_WITH_BLANK_PENALTY
            } else {
                RELATIVE_OUTDENT_PENALTY
            };
        } else {
            // probably the end of a block
            self.penalty += if any_blanks {
                RELATIVE_DEDENT_WITH_BLANK_PENALTY
            } else {
                RELATIVE_DEDENT_PENALTY
            };
        }
    }

    fn cmp(&self, other: &Score) -> i32 {
        let cmp_indents = (self.effective_indent > other.effective_indent) as i32
            - (self.effective_indent < other.effective_indent) as i32;
        INDENT_WEIGHT * cmp_indents + (self.penalty - other.penalty)
    }
}

/// Converts the change flags back into diff ops.
fn rebuild_ops(
    old_changed: &Changed,
    old_range: Range<usize>,
    new_changed: &Changed,
    new_range: Range<usize>,
) -> Vec<DiffOp> {
    let mut d = Replace::new(Capture::new());
    let old_len = old_changed.len();
    let new_len = new_changed.len();
    let (mut i, mut j) = (0, 0);
    while i < old_len || j < new_len {
        let equal_start = (i, j);
        while i < old_len && j < new_len && !old_changed.get(i) && !new_changed.get(j) {
            i += 1;
            j += 1;
        }
        if i > equal_start.0 {
            d.equal(
                old_range.start + equal_start.0,
                new_range.start + equal_start.1,
                i - equal_start.0,
            )
            .unwrap();
        }
        let del_start = i;
        while i < old_len && old_changed.get(i) {
            i += 1;
        }
        if i > del_start {
            d.delete(
                old_range.start + del_start,
                i - del_start,
                new_range.start + j,
            )
            .unwrap();
        }
        let ins_start = j;
        while j < new_len && new_changed.get(j) {
            j += 1;
        }
        if j > ins_start {
            d.insert(
                old_range.start + i,
                new_range.start + ins_start,
                j - ins_start,
            )
            .unwrap();
        }
        debug_assert!(
            i > equal_start.0 || j > equal_start.1,
            "change flags out of sync"
        );
        if i == equal_start.0 && j == equal_start.1 {
            break;
        }
    }
    d.finish().unwrap();
    d.into_inner().into_ops()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capture_diff_slices;

    fn diff_with_heuristic(old: &str, new: &str) -> String {
        let old_lines = old.lines().collect::<Vec<_>>();
        let new_lines = new.lines().collect::<Vec<_>>();
        let mut ops = capture_diff_slices(crate::Algorithm::Myers, &old_lines, &new_lines);
        apply_indent_heuristic(
            &mut ops,
            &old_lines[..],
            &new_lines[..],
            |idx| line_indent(old_lines[idx].as_bytes()),
            |idx| line_indent(new_lines[idx].as_bytes()),
        );
        let mut rv = String::new();
        for op in &ops {
            for change in op.iter_changes(&old_lines[..], &new_lines[..]) {
                rv.push_str(match change.tag() {
                    crate::ChangeTag::Equal => " ",
                    crate::ChangeTag::Delete => "-",
                    crate::ChangeTag::Insert => "+",
                });
                rv.push_str(change.value());
                rv.push('\n');
            }
        }
        rv
    }

    #[test]
    fn test_line_indent() {
        assert_eq!(line_indent(b"foo\n"), 0);
        assert_eq!(line_indent(b"  foo\n"), 2);
        assert_eq!(line_indent(b"\tfoo\n"), 8);
        assert_eq!(line_indent(b"  \tfoo\n"), 8);
        assert_eq!(line_indent(b"   \n"), -1);
        assert_eq!(line_indent(b""), -1);
        assert_eq!(line_indent(&[b' '; 300]), MAX_INDENT);
    }

    #[test]
    fn test_insert_attributed_function() {
        let old = "\
fn test_a() {
    a();
}

#[test]
fn test_c() {
    c();
}
";
        let new = "\
fn test_a() {
    a();
}

#[test]
fn test_b() {
    b();
}

#[test]
fn test_c() {
    c();
}
";
        // without the heuristic the insertion starts after `#[test]`
        insta::assert_snapshot!(diff_with_heuristic(old, new), @r"
         fn test_a() {
             a();
         }
         
        +#[test]
        +fn test_b() {
        +    b();
        +}
        +
         #[test]
         fn test_c() {
             c();
         }
        ");
    }

    #[test]
    fn test_insert_nested_block() {
        let old = "\
if x {
    a();
}
";
        let new = "\
if x {
    if y {
        b();
    }
    a();
}
";
        insta::assert_snapshot!(diff_with_heuristic(old, new), @r"
         if x {
        +    if y {
        +        b();
        +    }
             a();
         }
        ");
    }

    #[test]
    fn test_random_inputs_stay_valid() {
        let mut seed = 0x2545_f491_4f6c_dd1du64;
        let mut random = move |n: usize| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % n as u64) as usize
        };
        let vocabulary = ["", "  ", "x", "  x", "    x", "}", "  }", "\ty"];
        for _ in 0..2000 {
            let old = (0..random(30))
                .map(|_| vocabulary[random(vocabulary.len())])
                .collect::<Vec<_>>();
            let new = (0..random(30))
                .map(|_| vocabulary[random(vocabulary.len())])
                .collect::<Vec<_>>();
            for alg in [crate::Algorithm::Myers, crate::Algorithm::Histogram] {
                let before = capture_diff_slices(alg, &old, &new);
                let mut ops = before.clone();
                apply_indent_heuristic(
                    &mut ops,
                    &old[..],
                    &new[..],
                    |idx| line_indent(old[idx].as_bytes()),
                    |idx| line_indent(new[idx].as_bytes()),
                );

                let count = |ops: &[DiffOp]| {
                    ops.iter()
                        .filter(|op| !matches!(op, DiffOp::Equal { .. }))
                        .map(|op| (op.old_range().len(), op.new_range().len()))
                        .fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1))
                };
                assert_eq!(count(&before), count(&ops));

                let (mut old_pos, mut new_pos) = (0, 0);
                let mut rebuilt = Vec::new();
                for op in &ops {
                    assert_eq!(op.old_range().start, old_pos, "{ops:?}");
                    assert_eq!(op.new_range().start, new_pos, "{ops:?}");
                    if let DiffOp::Equal { .. } = op {
                        for (o, n) in op.old_range().zip(op.new_range()) {
                            assert_eq!(old[o], new[n]);
                        }
                    }
                    rebuilt.extend(op.new_range().map(|idx| new[idx]));
                    old_pos = op.old_range().end;
                    new_pos = op.new_range().end;
                }
                assert_eq!(old_pos, old.len());
                assert_eq!(rebuilt, new);
            }
        }
    }

    #[test]
    fn test_unchanged_ops_are_preserved() {
        let old = ["a", "b", "c"];
        let new = ["a", "x", "c"];
        let mut ops = capture_diff_slices(crate::Algorithm::Myers, &old, &new);
        let expected = ops.clone();
        apply_indent_heuristic(&mut ops, &old[..], &new[..], |_| 0, |_| 0);
        assert_eq!(ops, expected);
    }
}

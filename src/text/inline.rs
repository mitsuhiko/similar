#![cfg(feature = "inline")]
use std::borrow::Cow;
use std::fmt;

use crate::algorithms::{capture_diff, get_diff_ratio, Algorithm, DiffOp, DiffTag};
use crate::text::{Change, ChangeTag, DiffableStr, TextDiff};

use std::ops::Index;

struct MultiLookup<'bufs, 's, T: DiffableStr + ?Sized> {
    strings: &'bufs [&'s T],
    seqs: Vec<(&'s T, usize, usize)>,
}

impl<'bufs, 's, T: DiffableStr + ?Sized> MultiLookup<'bufs, 's, T> {
    fn new(strings: &'bufs [&'s T]) -> MultiLookup<'bufs, 's, T> {
        let mut seqs = Vec::new();
        for (string_idx, string) in strings.iter().enumerate() {
            let mut offset = 0;
            for word in string.split_unicode_words() {
                seqs.push((word, string_idx, offset));
                offset += word.len();
            }
        }
        MultiLookup { strings, seqs }
    }

    pub fn len(&self) -> usize {
        self.seqs.len()
    }

    fn get_original_slices(&self, idx: usize, len: usize) -> Vec<(usize, &'s T)> {
        let mut last = None;
        let mut rv = Vec::new();

        for offset in 0..len {
            let (s, str_idx, char_idx) = self.seqs[idx + offset];
            last = match last {
                None => Some((str_idx, char_idx, s.len())),
                Some((last_str_idx, start_char_idx, last_len)) => {
                    if last_str_idx == str_idx {
                        Some((str_idx, start_char_idx, last_len + s.len()))
                    } else {
                        rv.push((
                            last_str_idx,
                            self.strings[last_str_idx]
                                .slice(start_char_idx..start_char_idx + last_len),
                        ));
                        Some((str_idx, char_idx, s.len()))
                    }
                }
            };
        }

        if let Some((str_idx, start_char_idx, len)) = last {
            rv.push((
                str_idx,
                self.strings[str_idx].slice(start_char_idx..start_char_idx + len),
            ));
        }

        rv
    }
}

impl<'bufs, 's, T: DiffableStr + ?Sized> Index<usize> for MultiLookup<'bufs, 's, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.seqs[index].0
    }
}

fn push_values<'s, T: DiffableStr + ?Sized>(
    v: &mut Vec<Vec<(bool, &'s T)>>,
    idx: usize,
    emphasized: bool,
    s: &'s T,
) {
    v.resize_with(v.len().max(idx + 1), Vec::new);
    // newlines cause all kinds of wacky stuff if they end up highlighted.
    // because of this we want to unemphasize all newlines we encounter.
    if emphasized {
        for seg in s.split_lines_and_newlines() {
            v[idx].push((!seg.ends_with_newline(), seg));
        }
    } else {
        v[idx].push((false, s));
    }
}

/// Represents the expanded textual change with inline highlights.
///
/// This is like [`Change`] but with inline highlight info.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Ord, PartialOrd)]
pub struct InlineChange<'s, T: DiffableStr + ?Sized> {
    tag: ChangeTag,
    old_index: Option<usize>,
    new_index: Option<usize>,
    values: Vec<(bool, &'s T)>,
    missing_newline: bool,
}

impl<'s, T: DiffableStr + ?Sized> InlineChange<'s, T> {
    /// Returns the change tag.
    pub fn tag(&self) -> ChangeTag {
        self.tag
    }

    /// Returns the old index if available.
    pub fn old_index(&self) -> Option<usize> {
        self.old_index
    }

    /// Returns the new index if available.
    pub fn new_index(&self) -> Option<usize> {
        self.new_index
    }

    /// Returns the changed values.
    pub fn raw_values(&self) -> &[(bool, &'s T)] {
        &self.values
    }

    /// Iterates over all utf-8 decoded values.
    pub fn iter_values(&self) -> impl Iterator<Item = (bool, Cow<'_, str>)> {
        self.raw_values()
            .iter()
            .map(|(emphasized, raw_value)| (*emphasized, raw_value.as_str_lossy()))
    }

    /// Returns `true` if this change needs to be followed up by a
    /// missing newline.
    pub fn missing_newline(&self) -> bool {
        self.missing_newline
    }
}

impl<'s, T: DiffableStr + ?Sized> From<Change<'s, T>> for InlineChange<'s, T> {
    fn from(change: Change<'s, T>) -> InlineChange<'s, T> {
        InlineChange {
            tag: change.tag(),
            old_index: change.old_index(),
            new_index: change.new_index(),
            values: vec![(false, change.raw_value())],
            missing_newline: change.missing_newline(),
        }
    }
}

impl<'s, T: DiffableStr + ?Sized> fmt::Display for InlineChange<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (emphasized, value) in self.iter_values() {
            let marker = match (emphasized, self.tag) {
                (false, _) | (true, ChangeTag::Equal) => "",
                (true, ChangeTag::Delete) => "-",
                (true, ChangeTag::Insert) => "+",
            };
            write!(f, "{}{}{}", marker, value, marker)?;
        }
        if self.missing_newline {
            writeln!(f)?;
        }
        Ok(())
    }
}

pub(crate) fn iter_inline_changes<'diff, T>(
    diff: &'diff TextDiff<'_, '_, '_, T>,
    op: &DiffOp,
) -> impl Iterator<Item = InlineChange<'diff, T>>
where
    T: DiffableStr + ?Sized,
{
    let newline_terminated = diff.newline_terminated;
    let (tag, old_range, new_range) = op.as_tag_tuple();

    if let DiffTag::Equal | DiffTag::Insert | DiffTag::Delete = tag {
        return Box::new(diff.iter_changes(op).map(|x| x.into())) as Box<dyn Iterator<Item = _>>;
    }

    let mut old_index = old_range.start;
    let mut new_index = new_range.start;
    let old_slices = &diff.old_slices()[old_range];
    let new_slices = &diff.new_slices()[new_range];
    let old_lookup = MultiLookup::new(old_slices);
    let new_lookup = MultiLookup::new(new_slices);

    let ops = capture_diff(
        Algorithm::Patience,
        &old_lookup,
        0..old_lookup.len(),
        &new_lookup,
        0..new_lookup.len(),
    );

    if get_diff_ratio(&ops, old_lookup.len(), new_lookup.len()) < 0.5 {
        return Box::new(diff.iter_changes(op).map(|x| x.into())) as Box<dyn Iterator<Item = _>>;
    }

    let mut old_values = Vec::<Vec<_>>::new();
    let mut new_values = Vec::<Vec<_>>::new();

    for op in ops {
        match op {
            DiffOp::Equal {
                old_index,
                len,
                new_index,
            } => {
                for (idx, slice) in old_lookup.get_original_slices(old_index, len) {
                    push_values(&mut old_values, idx, false, slice);
                }
                for (idx, slice) in new_lookup.get_original_slices(new_index, len) {
                    push_values(&mut new_values, idx, false, slice);
                }
            }
            DiffOp::Delete {
                old_index, old_len, ..
            } => {
                for (idx, slice) in old_lookup.get_original_slices(old_index, old_len) {
                    push_values(&mut old_values, idx, true, slice);
                }
            }
            DiffOp::Insert {
                new_index, new_len, ..
            } => {
                for (idx, slice) in new_lookup.get_original_slices(new_index, new_len) {
                    push_values(&mut new_values, idx, true, slice);
                }
            }
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                for (idx, slice) in old_lookup.get_original_slices(old_index, old_len) {
                    push_values(&mut old_values, idx, true, slice);
                }
                for (idx, slice) in new_lookup.get_original_slices(new_index, new_len) {
                    push_values(&mut new_values, idx, true, slice);
                }
            }
        }
    }

    let mut rv = Vec::new();

    for values in old_values {
        rv.push(InlineChange {
            tag: ChangeTag::Delete,
            old_index: Some(old_index),
            new_index: None,
            values,
            missing_newline: false,
        });
        old_index += 1;
    }

    if newline_terminated
        && !old_slices.is_empty()
        && !old_slices[old_slices.len() - 1].ends_with_newline()
    {
        if let Some(last) = rv.last_mut() {
            last.missing_newline = true;
        }
    }

    for values in new_values {
        rv.push(InlineChange {
            tag: ChangeTag::Insert,
            old_index: None,
            new_index: Some(new_index),
            values,
            missing_newline: false,
        });
        new_index += 1;
    }

    if newline_terminated
        && !new_slices.is_empty()
        && !new_slices[new_slices.len() - 1].ends_with_newline()
    {
        if let Some(last) = rv.last_mut() {
            last.missing_newline = true;
        }
    }

    Box::new(rv.into_iter()) as Box<dyn Iterator<Item = _>>
}

#[test]
fn test_line_ops_inline() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert_eq!(diff.newline_terminated(), true);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
}

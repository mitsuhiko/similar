#![cfg(feature = "inline")]
use std::fmt;

use crate::algorithms::{capture_diff, get_diff_ratio, Algorithm, DiffOp, DiffTag};
use crate::text::{Change, ChangeTag, TextDiff};

use super::split_unicode_words;

use std::ops::Index;

struct MultiLookup<'bufs, 's> {
    strings: &'bufs [&'s str],
    seqs: Vec<(&'s str, usize, usize)>,
}

impl<'bufs, 's> MultiLookup<'bufs, 's> {
    fn new(strings: &'bufs [&'s str]) -> MultiLookup<'bufs, 's> {
        let mut seqs = Vec::new();
        for (string_idx, string) in strings.iter().enumerate() {
            let mut offset = 0;
            for word in split_unicode_words(string) {
                seqs.push((word, string_idx, offset));
                offset += word.len();
            }
        }
        MultiLookup { strings, seqs }
    }

    pub fn len(&self) -> usize {
        self.seqs.len()
    }

    fn get_original_slices(&self, idx: usize, len: usize) -> Vec<(usize, &'s str)> {
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
                            &self.strings[last_str_idx][start_char_idx..start_char_idx + last_len],
                        ));
                        Some((str_idx, char_idx, s.len()))
                    }
                }
            };
        }

        if let Some((str_idx, start_char_idx, len)) = last {
            rv.push((
                str_idx,
                &self.strings[str_idx][start_char_idx..start_char_idx + len],
            ));
        }

        rv
    }
}

impl<'bufs, 's> Index<usize> for MultiLookup<'bufs, 's> {
    type Output = str;

    fn index(&self, index: usize) -> &Self::Output {
        &self.seqs[index].0
    }
}

fn partition_newlines(s: &str) -> impl Iterator<Item = (&str, bool)> {
    let mut iter = s.char_indices().peekable();

    std::iter::from_fn(move || {
        if let Some((idx, c)) = iter.next() {
            let is_newline = c == '\r' || c == '\n';
            let start = idx;
            let mut end = idx + c.len_utf8();
            while let Some(&(_, next_char)) = iter.peek() {
                if (next_char == '\r' || next_char == '\n') != is_newline {
                    break;
                }
                iter.next();
                end += next_char.len_utf8();
            }
            Some((&s[start..end], is_newline))
        } else {
            None
        }
    })
}

fn push_values<'s>(v: &mut Vec<Vec<(bool, &'s str)>>, idx: usize, emphasized: bool, s: &'s str) {
    v.resize_with(v.len().max(idx + 1), Vec::new);
    // newlines cause all kinds of wacky stuff if they end up highlighted.
    // because of this we want to unemphasize all newlines we encounter.
    if emphasized {
        for (seg, is_nl) in partition_newlines(s) {
            v[idx].push((!is_nl, seg));
        }
    } else {
        v[idx].push((false, s));
    }
}

/// Represents the expanded textual change with inline highlights.
///
/// This is like [`Change`] but with inline highlight info.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Ord, PartialOrd)]
pub struct InlineChange<'s> {
    tag: ChangeTag,
    old_index: Option<usize>,
    new_index: Option<usize>,
    values: Vec<(bool, &'s str)>,
    missing_newline: bool,
}

impl<'s> InlineChange<'s> {
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
    pub fn values(&self) -> &[(bool, &'s str)] {
        &self.values
    }

    /// Returns `true` if this change needs to be followed up by a
    /// missing newline.
    pub fn missing_newline(&self) -> bool {
        self.missing_newline
    }
}

impl<'s> From<Change<'s>> for InlineChange<'s> {
    fn from(change: Change<'s>) -> InlineChange<'s> {
        InlineChange {
            tag: change.tag(),
            old_index: change.old_index(),
            new_index: change.new_index(),
            values: vec![(false, change.value())],
            missing_newline: change.missing_newline(),
        }
    }
}

impl<'s> fmt::Display for InlineChange<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &(emphasized, value) in &self.values {
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

pub(crate) fn iter_inline_changes<'diff>(
    diff: &'diff TextDiff,
    op: &DiffOp,
) -> impl Iterator<Item = InlineChange<'diff>> {
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
        && !old_slices[old_slices.len() - 1].ends_with(&['\r', '\n'][..])
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
        && !new_slices[new_slices.len() - 1].ends_with(&['\r', '\n'][..])
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

#![cfg(feature = "inline")]
use std::{fmt, iter};

use crate::algorithms::{Algorithm, DiffOp, DiffTag};
use crate::text::{Change, ChangeTag, TextDiff};

use super::split_unicode_words;

use std::ops::Range;

struct MultiIndex<'a, 's> {
    seq: &'a [&'s str],
    value: &'s str,
}

impl<'a, 's> MultiIndex<'a, 's> {
    pub fn new(seq: &'a [&'s str], value: &'s str) -> MultiIndex<'a, 's> {
        MultiIndex { seq, value }
    }

    pub fn get_slice(&self, rng: Range<usize>) -> &'s str {
        let mut start = 0;
        for &sseq in &self.seq[..rng.start] {
            start += sseq.len();
        }
        let mut end = start;
        for &sseq in &self.seq[rng.start..rng.end] {
            end += sseq.len();
        }
        &self.value[start..end]
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
    pub fn is_missing_newline(&self) -> bool {
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
    let mut change_iter = diff.iter_changes(op).peekable();
    let mut skip_next = false;
    let newline_terminated = diff.newline_terminated;

    iter::from_fn(move || {
        if skip_next {
            change_iter.next();
            skip_next = false;
        }
        if let Some(change) = change_iter.next() {
            let next_change = change_iter.peek();
            match (change.tag, next_change.map(|x| x.tag())) {
                (ChangeTag::Delete, Some(ChangeTag::Insert)) => {
                    let old_value = change.value();
                    let new_value = next_change.unwrap().value();
                    let old_chars = split_unicode_words(&old_value).collect::<Vec<_>>();
                    let new_chars = split_unicode_words(&new_value).collect::<Vec<_>>();
                    let old_mindex = MultiIndex::new(&old_chars, old_value);
                    let new_mindex = MultiIndex::new(&new_chars, new_value);
                    let inline_diff = TextDiff::configure()
                        .algorithm(Algorithm::Patience)
                        .diff_slices(&old_chars, &new_chars);

                    if inline_diff.ratio() < 0.5 {
                        return Some(None.into_iter().chain(Some(change.into()).into_iter()));
                    }

                    // skip the next element as we handle it here
                    skip_next = true;

                    let mut old_values = vec![];
                    let mut new_values = vec![];
                    for op in inline_diff.ops() {
                        match op.tag() {
                            DiffTag::Equal => {
                                old_values.push((false, old_mindex.get_slice(op.old_range())));
                                new_values.push((false, old_mindex.get_slice(op.old_range())));
                            }
                            DiffTag::Delete => {
                                old_values.push((true, old_mindex.get_slice(op.old_range())));
                            }
                            DiffTag::Insert => {
                                new_values.push((true, new_mindex.get_slice(op.new_range())));
                            }
                            DiffTag::Replace => {
                                old_values.push((true, old_mindex.get_slice(op.old_range())));
                                new_values.push((true, new_mindex.get_slice(op.new_range())));
                            }
                        }
                    }

                    Some(
                        Some(InlineChange {
                            tag: ChangeTag::Delete,
                            old_index: change.old_index(),
                            new_index: None,
                            values: old_values,
                            missing_newline: newline_terminated
                                && !old_value.ends_with(&['\r', '\n'][..]),
                        })
                        .into_iter()
                        .chain(
                            Some(InlineChange {
                                tag: ChangeTag::Insert,
                                old_index: None,
                                new_index: next_change.unwrap().new_index(),
                                values: new_values,
                                missing_newline: newline_terminated
                                    && !new_value.ends_with(&['\r', '\n'][..]),
                            })
                            .into_iter(),
                        ),
                    )
                }
                _ => Some(None.into_iter().chain(Some(change.into()).into_iter())),
            }
        } else {
            None
        }
    })
    .flatten()
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

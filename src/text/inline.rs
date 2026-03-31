use std::borrow::Cow;
use std::fmt;

use crate::deadline_support::Instant;
use crate::text::{DiffableStr, TextDiff};
use crate::types::{Algorithm, Change, ChangeTag, DiffOp, DiffTag};
use crate::{capture_diff_deadline, get_diff_ratio};

use std::ops::Index;

use super::utils::upper_seq_ratio;

/// Controls how replacement regions are tokenized for inline refinement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum InlineChangeMode {
    /// Use the crate default tokenization mode.
    ///
    /// This resolves to unicode words when the `unicode` feature is enabled,
    /// and to whitespace based words otherwise.
    Auto,
    /// Tokenize by whitespace runs and non-whitespace runs.
    Words,
    /// Tokenize by characters.
    Chars,
    /// Tokenize by unicode words.
    #[cfg(feature = "unicode")]
    UnicodeWords,
    /// Tokenize by unicode grapheme clusters.
    #[cfg(feature = "unicode")]
    Graphemes,
}

/// Configuration for inline refinement in [`TextDiff::iter_inline_changes`](crate::TextDiff::iter_inline_changes).
#[derive(Debug, Clone, Copy)]
pub struct InlineChangeOptions {
    algorithm: Algorithm,
    mode: InlineChangeMode,
    min_ratio: f32,
}

impl Default for InlineChangeOptions {
    fn default() -> Self {
        InlineChangeOptions {
            algorithm: Algorithm::Patience,
            mode: InlineChangeMode::Auto,
            min_ratio: 0.5,
        }
    }
}

impl InlineChangeOptions {
    /// Creates default inline change options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the algorithm used for second-level refinement inside replaced ranges.
    pub fn algorithm(&mut self, alg: Algorithm) -> &mut Self {
        self.algorithm = alg;
        self
    }

    /// Sets the tokenization mode used for second-level refinement.
    pub fn mode(&mut self, mode: InlineChangeMode) -> &mut Self {
        self.mode = mode;
        self
    }

    /// Sets the minimum ratio required to apply inline refinement.
    ///
    /// If the initial rough ratio or refined ratio drops below this threshold,
    /// the operation falls back to non-emphasized line changes.
    pub fn min_ratio(&mut self, min_ratio: f32) -> &mut Self {
        self.min_ratio = min_ratio;
        self
    }

    /// Returns the configured algorithm.
    pub fn get_algorithm(&self) -> Algorithm {
        self.algorithm
    }

    /// Returns the configured tokenization mode.
    pub fn get_mode(&self) -> InlineChangeMode {
        self.mode
    }

    /// Returns the configured minimum ratio threshold.
    pub fn get_min_ratio(&self) -> f32 {
        self.min_ratio
    }
}

fn tokenize_inline<'s, T: DiffableStr + ?Sized>(
    string: &'s T,
    mode: InlineChangeMode,
) -> Vec<&'s T> {
    match mode {
        InlineChangeMode::Auto => {
            #[cfg(feature = "unicode")]
            {
                string.tokenize_unicode_words()
            }
            #[cfg(not(feature = "unicode"))]
            {
                string.tokenize_words()
            }
        }
        InlineChangeMode::Words => string.tokenize_words(),
        InlineChangeMode::Chars => string.tokenize_chars(),
        #[cfg(feature = "unicode")]
        InlineChangeMode::UnicodeWords => string.tokenize_unicode_words(),
        #[cfg(feature = "unicode")]
        InlineChangeMode::Graphemes => string.tokenize_graphemes(),
        #[allow(unreachable_patterns)]
        _ => string.tokenize_words(),
    }
}

struct MultiLookup<'bufs, 's, T: DiffableStr + ?Sized> {
    strings: &'bufs [&'s T],
    seqs: Vec<(&'s T, usize, usize)>,
}

impl<'bufs, 's, T: DiffableStr + ?Sized> MultiLookup<'bufs, 's, T> {
    fn new(strings: &'bufs [&'s T], mode: InlineChangeMode) -> MultiLookup<'bufs, 's, T> {
        let mut seqs = Vec::new();
        for (string_idx, string) in strings.iter().enumerate() {
            let mut offset = 0;
            for token in tokenize_inline(*string, mode) {
                seqs.push((token, string_idx, offset));
                offset += token.len();
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

impl<T: DiffableStr + ?Sized> Index<usize> for MultiLookup<'_, '_, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.seqs[index].0
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
        for seg in s.tokenize_lines_and_newlines() {
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
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct InlineChange<'s, T: DiffableStr + ?Sized> {
    tag: ChangeTag,
    old_index: Option<usize>,
    new_index: Option<usize>,
    values: Vec<(bool, &'s T)>,
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
    ///
    /// Each item is a tuple in the form `(emphasized, value)` where `emphasized`
    /// is true if it should be highlighted as an inline diff.
    ///
    /// Depending on the type of the underlying [`DiffableStr`] this value is
    /// more or less useful.  If you always want to have a utf-8 string it's
    /// better to use the [`InlineChange::iter_strings_lossy`] method.
    pub fn values(&self) -> &[(bool, &'s T)] {
        &self.values
    }

    /// Iterates over all (potentially lossy) utf-8 decoded values.
    ///
    /// Each item is a tuple in the form `(emphasized, value)` where `emphasized`
    /// is true if it should be highlighted as an inline diff.
    ///
    /// Tokenization depends on the [`InlineChangeOptions`] used when this
    /// value was produced.
    pub fn iter_strings_lossy(&self) -> impl Iterator<Item = (bool, Cow<'_, str>)> {
        self.values()
            .iter()
            .map(|(emphasized, raw_value)| (*emphasized, raw_value.to_string_lossy()))
    }

    /// Returns `true` if this change does not end in a newline and must be
    /// followed up by one if line based diffs are used.
    pub fn missing_newline(&self) -> bool {
        !self.values.last().map_or(true, |x| x.1.ends_with_newline())
    }
}

impl<'s, T: DiffableStr + ?Sized> From<Change<&'s T>> for InlineChange<'s, T> {
    fn from(change: Change<&'s T>) -> InlineChange<'s, T> {
        InlineChange {
            tag: change.tag(),
            old_index: change.old_index(),
            new_index: change.new_index(),
            values: vec![(false, change.value())],
        }
    }
}

impl<T: DiffableStr + ?Sized> fmt::Display for InlineChange<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (emphasized, value) in self.iter_strings_lossy() {
            let marker = match (emphasized, self.tag) {
                (false, _) | (true, ChangeTag::Equal) => "",
                (true, ChangeTag::Delete) => "-",
                (true, ChangeTag::Insert) => "+",
            };
            write!(f, "{}{}{}", marker, value, marker)?;
        }
        if self.missing_newline() {
            writeln!(f)?;
        }
        Ok(())
    }
}

pub(crate) fn iter_inline_changes<'x, 'diff, 'old, 'new, 'bufs, T>(
    diff: &'diff TextDiff<'old, 'new, 'bufs, T>,
    op: &DiffOp,
    deadline: Option<Instant>,
    options: InlineChangeOptions,
) -> impl Iterator<Item = InlineChange<'x, T>> + 'diff
where
    T: DiffableStr + ?Sized,
    'x: 'diff,
    'old: 'x,
    'new: 'x,
{
    let (tag, old_range, new_range) = op.as_tag_tuple();

    if let DiffTag::Equal | DiffTag::Insert | DiffTag::Delete = tag {
        return Box::new(diff.iter_changes(op).map(|x| x.into())) as Box<dyn Iterator<Item = _>>;
    }

    let mut old_index = old_range.start;
    let mut new_index = new_range.start;
    let old_slices = &diff.old_slices()[old_range];
    let new_slices = &diff.new_slices()[new_range];
    let min_ratio = options.get_min_ratio();

    if upper_seq_ratio(old_slices, new_slices) < min_ratio {
        return Box::new(diff.iter_changes(op).map(|x| x.into())) as Box<dyn Iterator<Item = _>>;
    }

    let old_lookup = MultiLookup::new(old_slices, options.get_mode());
    let new_lookup = MultiLookup::new(new_slices, options.get_mode());

    let ops = capture_diff_deadline(
        options.get_algorithm(),
        &old_lookup,
        0..old_lookup.len(),
        &new_lookup,
        0..new_lookup.len(),
        deadline,
    );

    if get_diff_ratio(&ops, old_lookup.len(), new_lookup.len()) < min_ratio {
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
        });
        old_index += 1;
    }

    for values in new_values {
        rv.push(InlineChange {
            tag: ChangeTag::Insert,
            old_index: None,
            new_index: Some(new_index),
            values,
        });
        new_index += 1;
    }

    Box::new(rv.into_iter()) as Box<dyn Iterator<Item = _>>
}

#[test]
fn test_line_ops_inline() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert!(diff.newline_terminated());
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
}

#[test]
fn test_line_ops_inline_chars() {
    let diff = TextDiff::from_lines("abcde\n", "abXYZ\n");
    let mut options = InlineChangeOptions::new();
    options.mode(InlineChangeMode::Chars);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();

    assert_eq!(changes.len(), 2);
    assert_eq!(changes[0].tag(), ChangeTag::Delete);
    assert_eq!(changes[1].tag(), ChangeTag::Insert);
    assert_eq!(
        changes[0].iter_strings_lossy().collect::<Vec<_>>(),
        vec![
            (false, Cow::Borrowed("ab")),
            (true, Cow::Borrowed("cde")),
            (false, Cow::Borrowed("\n")),
        ]
    );
}

#[test]
#[cfg(feature = "serde")]
fn test_serde() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert!(diff.newline_terminated());
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes(op))
        .collect::<Vec<_>>();
    let json = serde_json::to_string_pretty(&changes).unwrap();
    insta::assert_snapshot!(&json);
}

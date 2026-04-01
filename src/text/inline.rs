use std::borrow::Cow;
use std::fmt;

use crate::deadline_support::Instant;
use crate::text::{DiffableStr, TextDiff};
use crate::types::{Algorithm, Change, ChangeTag, DiffOp, DiffTag};
use crate::{capture_diff_deadline, diff_ratio};

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
///
/// Defaults:
///
/// - algorithm: [`Algorithm::Patience`]
/// - mode: [`InlineChangeMode::Auto`]
/// - min ratio: `0.5`
/// - semantic cleanup: disabled
#[derive(Debug, Clone, Copy)]
pub struct InlineChangeOptions {
    algorithm: Algorithm,
    mode: InlineChangeMode,
    min_ratio: f32,
    semantic_cleanup: bool,
}

impl Default for InlineChangeOptions {
    fn default() -> Self {
        InlineChangeOptions {
            algorithm: Algorithm::Patience,
            mode: InlineChangeMode::Auto,
            min_ratio: 0.5,
            semantic_cleanup: false,
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

    /// Enables a semantic cleanup pass for refined inline ops.
    ///
    /// This performs extra boundary shifting and overlap extraction intended to
    /// improve human readability of intraline highlights.
    ///
    /// Disabled by default.
    pub fn semantic_cleanup(&mut self, yes: bool) -> &mut Self {
        self.semantic_cleanup = yes;
        self
    }

    /// Returns the algorithm used for second-level refinement.
    pub fn refinement_algorithm(&self) -> Algorithm {
        self.algorithm
    }

    /// Returns the tokenization mode used for second-level refinement.
    pub fn refinement_mode(&self) -> InlineChangeMode {
        self.mode
    }

    /// Returns the minimum ratio threshold required for inline refinement.
    pub fn minimum_ratio(&self) -> f32 {
        self.min_ratio
    }

    /// Returns `true` if semantic cleanup is enabled.
    pub fn semantic_cleanup_enabled(&self) -> bool {
        self.semantic_cleanup
    }

    /// Returns the configured algorithm.
    #[deprecated(note = "use refinement_algorithm()")]
    pub fn get_algorithm(&self) -> Algorithm {
        self.refinement_algorithm()
    }

    /// Returns the configured tokenization mode.
    #[deprecated(note = "use refinement_mode()")]
    pub fn get_mode(&self) -> InlineChangeMode {
        self.refinement_mode()
    }

    /// Returns the configured minimum ratio threshold.
    #[deprecated(note = "use minimum_ratio()")]
    pub fn get_min_ratio(&self) -> f32 {
        self.minimum_ratio()
    }

    /// Returns if semantic cleanup is enabled.
    #[deprecated(note = "use semantic_cleanup_enabled()")]
    pub fn get_semantic_cleanup(&self) -> bool {
        self.semantic_cleanup_enabled()
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
        !self.values.last().is_none_or(|x| x.1.ends_with_newline())
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

fn expand_replace_ops(ops: Vec<DiffOp>) -> Vec<DiffOp> {
    let mut rv = Vec::with_capacity(ops.len() + 4);
    for op in ops {
        match op {
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                if old_len > 0 {
                    rv.push(DiffOp::Delete {
                        old_index,
                        old_len,
                        new_index,
                    });
                }
                if new_len > 0 {
                    rv.push(DiffOp::Insert {
                        old_index,
                        new_index,
                        new_len,
                    });
                }
            }
            other => rv.push(other),
        }
    }
    rv
}

fn merge_inline_ops(ops: &mut Vec<DiffOp>) {
    fn can_merge(a: &DiffOp, b: &DiffOp) -> bool {
        match (*a, *b) {
            (
                DiffOp::Equal {
                    old_index: a_old,
                    new_index: a_new,
                    len: a_len,
                },
                DiffOp::Equal {
                    old_index: b_old,
                    new_index: b_new,
                    ..
                },
            ) => a_old + a_len == b_old && a_new + a_len == b_new,
            (
                DiffOp::Delete {
                    old_index: a_old,
                    old_len: a_len,
                    new_index: a_new,
                },
                DiffOp::Delete {
                    old_index: b_old,
                    new_index: b_new,
                    ..
                },
            ) => a_old + a_len == b_old && a_new == b_new,
            (
                DiffOp::Insert {
                    old_index: a_old,
                    new_index: a_new,
                    new_len: a_len,
                },
                DiffOp::Insert {
                    old_index: b_old,
                    new_index: b_new,
                    ..
                },
            ) => a_old == b_old && a_new + a_len == b_new,
            _ => false,
        }
    }

    fn merge(a: &mut DiffOp, b: &DiffOp) {
        match (*a, *b) {
            (DiffOp::Equal { .. }, DiffOp::Equal { len, .. }) => a.grow_right(len),
            (DiffOp::Delete { .. }, DiffOp::Delete { old_len, .. }) => a.grow_right(old_len),
            (DiffOp::Insert { .. }, DiffOp::Insert { new_len, .. }) => a.grow_right(new_len),
            _ => {}
        }
    }

    let mut merged = Vec::with_capacity(ops.len());
    for op in ops.drain(..) {
        if op.is_empty() {
            continue;
        }
        if let Some(last) = merged.last_mut() {
            if can_merge(last, &op) {
                merge(last, &op);
                continue;
            }
        }
        merged.push(op);
    }
    *ops = merged;
}

fn common_prefix_len_lookup<Old, New, T>(
    old: &Old,
    old_start: usize,
    old_len: usize,
    new: &New,
    new_start: usize,
    new_len: usize,
) -> usize
where
    T: ?Sized,
    Old: Index<usize, Output = T> + ?Sized,
    New: Index<usize, Output = T> + ?Sized,
    T: PartialEq,
{
    let mut matched = 0;
    let max = old_len.min(new_len);
    while matched < max && old[old_start + matched] == new[new_start + matched] {
        matched += 1;
    }
    matched
}

fn common_suffix_len_lookup<Old, New, T>(
    old: &Old,
    old_start: usize,
    old_len: usize,
    new: &New,
    new_start: usize,
    new_len: usize,
) -> usize
where
    T: ?Sized,
    Old: Index<usize, Output = T> + ?Sized,
    New: Index<usize, Output = T> + ?Sized,
    T: PartialEq,
{
    let mut matched = 0;
    let max = old_len.min(new_len);
    while matched < max
        && old[old_start + old_len - 1 - matched] == new[new_start + new_len - 1 - matched]
    {
        matched += 1;
    }
    matched
}

fn token_first_char<T: DiffableStr + ?Sized>(token: &T) -> Option<char> {
    token
        .as_str()
        .and_then(|x| x.chars().next())
        .or_else(|| token.as_bytes().first().map(|x| *x as char))
}

fn token_last_char<T: DiffableStr + ?Sized>(token: &T) -> Option<char> {
    token
        .as_str()
        .and_then(|x| x.chars().next_back())
        .or_else(|| token.as_bytes().last().map(|x| *x as char))
}

fn semantic_boundary_score<T, Seq>(
    seq: &Seq,
    left_start: usize,
    left_len: usize,
    right_start: usize,
    right_len: usize,
) -> usize
where
    T: DiffableStr + PartialEq + ?Sized,
    Seq: Index<usize, Output = T> + ?Sized,
{
    if left_len == 0 || right_len == 0 {
        return 6;
    }

    let Some(char1) = token_last_char::<T>(&seq[left_start + left_len - 1]) else {
        return 0;
    };
    let Some(char2) = token_first_char::<T>(&seq[right_start]) else {
        return 0;
    };

    let non_alphanumeric1 = !char1.is_ascii_alphanumeric();
    let non_alphanumeric2 = !char2.is_ascii_alphanumeric();
    let whitespace1 = non_alphanumeric1 && char1.is_ascii_whitespace();
    let whitespace2 = non_alphanumeric2 && char2.is_ascii_whitespace();
    let line_break1 = whitespace1 && char1.is_ascii_control();
    let line_break2 = whitespace2 && char2.is_ascii_control();

    if line_break1 || line_break2 {
        4
    } else if non_alphanumeric1 && !whitespace1 && whitespace2 {
        3
    } else if whitespace1 || whitespace2 {
        2
    } else if non_alphanumeric1 || non_alphanumeric2 {
        1
    } else {
        0
    }
}

fn cleanup_semantic_lossless<Old, New, T>(old: &Old, new: &New, ops: &mut Vec<DiffOp>)
where
    T: DiffableStr + PartialEq + ?Sized,
    Old: Index<usize, Output = T> + ?Sized,
    New: Index<usize, Output = T> + ?Sized,
{
    let mut pointer = 1;
    while pointer + 1 < ops.len() {
        let mut prev = ops[pointer - 1];
        let mut edit = ops[pointer];
        let mut next = ops[pointer + 1];

        let changed = match (prev, edit, next) {
            (DiffOp::Equal { .. }, DiffOp::Insert { .. }, DiffOp::Equal { .. }) => {
                let original = (prev, edit, next);
                let prev_new = prev.new_range();
                let edit_new = edit.new_range();

                let common = common_suffix_len_lookup::<_, _, T>(
                    new,
                    prev_new.start,
                    prev_new.len(),
                    new,
                    edit_new.start,
                    edit_new.len(),
                );
                if common > 0 {
                    prev.shrink_left(common);
                    edit.shift_left(common);
                    next.grow_left(common);
                }

                let mut best_prev = prev;
                let mut best_edit = edit;
                let mut best_next = next;
                let mut best_score = semantic_boundary_score::<T, _>(
                    new,
                    prev.new_range().start,
                    prev.new_range().len(),
                    edit.new_range().start,
                    edit.new_range().len(),
                ) + semantic_boundary_score::<T, _>(
                    new,
                    edit.new_range().start,
                    edit.new_range().len(),
                    next.new_range().start,
                    next.new_range().len(),
                );

                while edit.new_range().len() > 0
                    && next.new_range().len() > 0
                    && new[edit.new_range().start] == new[next.new_range().start]
                {
                    prev.grow_right(1);
                    edit.shift_right(1);
                    next.shift_right(1);
                    next.shrink_left(1);
                    let score = semantic_boundary_score::<T, _>(
                        new,
                        prev.new_range().start,
                        prev.new_range().len(),
                        edit.new_range().start,
                        edit.new_range().len(),
                    ) + semantic_boundary_score::<T, _>(
                        new,
                        edit.new_range().start,
                        edit.new_range().len(),
                        next.new_range().start,
                        next.new_range().len(),
                    );
                    if score >= best_score {
                        best_score = score;
                        best_prev = prev;
                        best_edit = edit;
                        best_next = next;
                    }
                }

                ops[pointer - 1] = best_prev;
                ops[pointer] = best_edit;
                ops[pointer + 1] = best_next;
                (best_prev, best_edit, best_next) != original
            }
            (DiffOp::Equal { .. }, DiffOp::Delete { .. }, DiffOp::Equal { .. }) => {
                let original = (prev, edit, next);
                let prev_old = prev.old_range();
                let edit_old = edit.old_range();

                let common = common_suffix_len_lookup::<_, _, T>(
                    old,
                    prev_old.start,
                    prev_old.len(),
                    old,
                    edit_old.start,
                    edit_old.len(),
                );
                if common > 0 {
                    prev.shrink_left(common);
                    edit.shift_left(common);
                    next.grow_left(common);
                }

                let mut best_prev = prev;
                let mut best_edit = edit;
                let mut best_next = next;
                let mut best_score = semantic_boundary_score::<T, _>(
                    old,
                    prev.old_range().start,
                    prev.old_range().len(),
                    edit.old_range().start,
                    edit.old_range().len(),
                ) + semantic_boundary_score::<T, _>(
                    old,
                    edit.old_range().start,
                    edit.old_range().len(),
                    next.old_range().start,
                    next.old_range().len(),
                );

                while edit.old_range().len() > 0
                    && next.old_range().len() > 0
                    && old[edit.old_range().start] == old[next.old_range().start]
                {
                    prev.grow_right(1);
                    edit.shift_right(1);
                    next.shift_right(1);
                    next.shrink_left(1);
                    let score = semantic_boundary_score::<T, _>(
                        old,
                        prev.old_range().start,
                        prev.old_range().len(),
                        edit.old_range().start,
                        edit.old_range().len(),
                    ) + semantic_boundary_score::<T, _>(
                        old,
                        edit.old_range().start,
                        edit.old_range().len(),
                        next.old_range().start,
                        next.old_range().len(),
                    );
                    if score >= best_score {
                        best_score = score;
                        best_prev = prev;
                        best_edit = edit;
                        best_next = next;
                    }
                }

                ops[pointer - 1] = best_prev;
                ops[pointer] = best_edit;
                ops[pointer + 1] = best_next;
                (best_prev, best_edit, best_next) != original
            }
            _ => false,
        };

        if changed {
            merge_inline_ops(ops);
            if pointer > 1 {
                pointer -= 1;
            }
        } else {
            pointer += 1;
        }
    }
}

fn cleanup_inline_overlaps<Old, New, T>(old: &Old, new: &New, ops: &mut Vec<DiffOp>)
where
    T: DiffableStr + PartialEq + ?Sized,
    Old: Index<usize, Output = T> + ?Sized,
    New: Index<usize, Output = T> + ?Sized,
{
    let mut pointer = 0;
    while pointer + 1 < ops.len() {
        let (DiffOp::Delete { .. }, DiffOp::Insert { .. }) = (ops[pointer], ops[pointer + 1])
        else {
            pointer += 1;
            continue;
        };

        let mut del = ops[pointer];
        let mut ins = ops[pointer + 1];

        let del_old = del.old_range();
        let ins_new = ins.new_range();
        let prefix = common_prefix_len_lookup::<_, _, T>(
            old,
            del_old.start,
            del_old.len(),
            new,
            ins_new.start,
            ins_new.len(),
        );

        if prefix > 0 {
            let old_start = del.old_range().start;
            let new_start = ins.new_range().start;
            del.shift_right(prefix);
            del.shrink_left(prefix);
            ins.shift_right(prefix);
            ins.shrink_left(prefix);

            let eq = DiffOp::Equal {
                old_index: old_start,
                new_index: new_start,
                len: prefix,
            };

            if pointer > 0 {
                if let DiffOp::Equal { .. } = ops[pointer - 1] {
                    if {
                        let prev = ops[pointer - 1];
                        let prev_old = prev.old_range();
                        let prev_new = prev.new_range();
                        prev_old.end == old_start && prev_new.end == new_start
                    } {
                        ops[pointer - 1].grow_right(prefix);
                    } else {
                        ops.insert(pointer, eq);
                        pointer += 1;
                    }
                } else {
                    ops.insert(pointer, eq);
                    pointer += 1;
                }
            } else {
                ops.insert(pointer, eq);
                pointer += 1;
            }
            ops[pointer] = del;
            ops[pointer + 1] = ins;
        }

        let del_old = ops[pointer].old_range();
        let ins_new = ops[pointer + 1].new_range();
        let suffix = common_suffix_len_lookup::<_, _, T>(
            old,
            del_old.start,
            del_old.len(),
            new,
            ins_new.start,
            ins_new.len(),
        );
        if suffix > 0 {
            ops[pointer].shrink_left(suffix);
            ops[pointer + 1].shrink_left(suffix);
            let old_start = ops[pointer].old_range().end;
            let new_start = ops[pointer + 1].new_range().end;
            ops.insert(
                pointer + 2,
                DiffOp::Equal {
                    old_index: old_start,
                    new_index: new_start,
                    len: suffix,
                },
            );
        }

        pointer += 1;
    }

    merge_inline_ops(ops);
}

fn cleanup_inline_semantic<Old, New, T>(old: &Old, new: &New, ops: &mut Vec<DiffOp>)
where
    T: DiffableStr + PartialEq + ?Sized,
    Old: Index<usize, Output = T> + ?Sized,
    New: Index<usize, Output = T> + ?Sized,
{
    let mut expanded = expand_replace_ops(std::mem::take(ops));
    merge_inline_ops(&mut expanded);
    cleanup_inline_overlaps::<_, _, T>(old, new, &mut expanded);
    cleanup_semantic_lossless::<_, _, T>(old, new, &mut expanded);
    merge_inline_ops(&mut expanded);
    *ops = expanded;
}

pub(crate) fn iter_inline_changes<'diff, 'old, 'new, T>(
    diff: &'diff TextDiff<'old, 'new, T>,
    op: &DiffOp,
    deadline: Option<Instant>,
    options: InlineChangeOptions,
) -> impl Iterator<Item = InlineChange<'diff, T>> + 'diff
where
    T: DiffableStr + ?Sized,
{
    let (tag, old_range, new_range) = op.as_tag_tuple();

    if let DiffTag::Equal | DiffTag::Insert | DiffTag::Delete = tag {
        return Box::new(diff.iter_changes(op).map(InlineChange::from))
            as Box<dyn Iterator<Item = _>>;
    }

    let mut old_index = old_range.start;
    let mut new_index = new_range.start;
    let old_slices = old_range
        .clone()
        .map(|idx| diff.old_slice(idx).expect("slice out of bounds"))
        .collect::<Vec<_>>();
    let new_slices = new_range
        .clone()
        .map(|idx| diff.new_slice(idx).expect("slice out of bounds"))
        .collect::<Vec<_>>();
    let min_ratio = options.minimum_ratio();

    if upper_seq_ratio(&old_slices, &new_slices) < min_ratio {
        return Box::new(diff.iter_changes(op).map(InlineChange::from))
            as Box<dyn Iterator<Item = _>>;
    }

    let old_lookup = MultiLookup::new(&old_slices, options.refinement_mode());
    let new_lookup = MultiLookup::new(&new_slices, options.refinement_mode());

    let mut ops = capture_diff_deadline(
        options.refinement_algorithm(),
        &old_lookup,
        0..old_lookup.len(),
        &new_lookup,
        0..new_lookup.len(),
        deadline,
    );

    if diff_ratio(&ops, old_lookup.len(), new_lookup.len()) < min_ratio {
        return Box::new(diff.iter_changes(op).map(InlineChange::from))
            as Box<dyn Iterator<Item = _>>;
    }

    if options.semantic_cleanup_enabled() {
        cleanup_inline_semantic::<_, _, T>(&old_lookup, &new_lookup, &mut ops);
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
fn test_iter_all_inline_changes_helpers_match_manual_iteration() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );

    let expected_default = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes(op))
        .collect::<Vec<_>>();
    assert_eq!(
        diff.iter_all_inline_changes().collect::<Vec<_>>(),
        expected_default
    );

    let expected_deadline = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_deadline(op, None))
        .collect::<Vec<_>>();
    assert_eq!(
        diff.iter_all_inline_changes_deadline(None)
            .collect::<Vec<_>>(),
        expected_deadline
    );

    let mut options = InlineChangeOptions::new();
    options
        .mode(InlineChangeMode::Chars)
        .semantic_cleanup(true)
        .min_ratio(0.25);

    let expected_options = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();
    assert_eq!(
        diff.iter_all_inline_changes_with_options(options)
            .collect::<Vec<_>>(),
        expected_options
    );

    let expected_options_deadline = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options_deadline(op, options, None))
        .collect::<Vec<_>>();
    assert_eq!(
        diff.iter_all_inline_changes_with_options_deadline(options, None)
            .collect::<Vec<_>>(),
        expected_options_deadline
    );
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
fn test_line_ops_inline_semantic_cleanup() {
    let diff = TextDiff::from_lines("The came.\n", "The cat came.\n");

    let mut options = InlineChangeOptions::new();
    options.mode(InlineChangeMode::Chars);

    let plain = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();

    options.semantic_cleanup(true);
    let cleaned = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();

    let plain_insert = plain
        .iter()
        .find(|x| x.tag() == ChangeTag::Insert)
        .unwrap()
        .iter_strings_lossy()
        .collect::<Vec<_>>();
    let cleaned_insert = cleaned
        .iter()
        .find(|x| x.tag() == ChangeTag::Insert)
        .unwrap()
        .iter_strings_lossy()
        .collect::<Vec<_>>();

    assert_ne!(plain_insert, cleaned_insert);
    assert_eq!(
        cleaned_insert,
        vec![
            (false, Cow::Borrowed("The ")),
            (true, Cow::Borrowed("cat ")),
            (false, Cow::Borrowed("came.\n")),
        ]
    );
}

#[test]
fn test_line_ops_inline_issue_84_chars() {
    let diff = TextDiff::from_lines("f(x) y\n", "f(z) y\n");
    let mut options = InlineChangeOptions::new();
    options.mode(InlineChangeMode::Chars);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();

    assert_eq!(changes.len(), 2);
    assert_eq!(
        changes[0].iter_strings_lossy().collect::<Vec<_>>(),
        vec![
            (false, Cow::Borrowed("f(")),
            (true, Cow::Borrowed("x")),
            (false, Cow::Borrowed(") y\n")),
        ]
    );
    assert_eq!(
        changes[1].iter_strings_lossy().collect::<Vec<_>>(),
        vec![
            (false, Cow::Borrowed("f(")),
            (true, Cow::Borrowed("z")),
            (false, Cow::Borrowed(") y\n")),
        ]
    );
}

#[test]
fn test_line_ops_inline_semantic_snapshot() {
    let diff = TextDiff::from_lines("The came.\n", "The cat came.\n");
    let mut options = InlineChangeOptions::new();
    options.mode(InlineChangeMode::Chars).semantic_cleanup(true);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_inline_changes_with_options(op, options))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
}

#[cfg(test)]
fn assert_semantic_cleanup_no_panic(old_lines: &[&str], new_lines: &[&str], mut ops: Vec<DiffOp>) {
    let old_lookup = MultiLookup::new(old_lines, InlineChangeMode::Chars);
    let new_lookup = MultiLookup::new(new_lines, InlineChangeMode::Chars);
    cleanup_semantic_lossless::<_, _, str>(&old_lookup, &new_lookup, &mut ops);
    assert!(!ops.is_empty());
}

#[test]
fn test_semantic_cleanup_handles_trailing_single_token_equal() {
    assert_semantic_cleanup_no_panic(
        &["Xa"],
        &["Xaba"],
        vec![
            DiffOp::Equal {
                old_index: 0,
                new_index: 0,
                len: 1,
            },
            DiffOp::Insert {
                old_index: 1,
                new_index: 1,
                new_len: 2,
            },
            DiffOp::Equal {
                old_index: 1,
                new_index: 3,
                len: 1,
            },
        ],
    );
}

#[test]
fn test_semantic_cleanup_handles_leading_single_token_equal() {
    assert_semantic_cleanup_no_panic(
        &["Xaba"],
        &["Xa"],
        vec![
            DiffOp::Equal {
                old_index: 0,
                new_index: 0,
                len: 1,
            },
            DiffOp::Delete {
                old_index: 1,
                old_len: 2,
                new_index: 1,
            },
            DiffOp::Equal {
                old_index: 3,
                new_index: 1,
                len: 1,
            },
        ],
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

//! Text diffing utilities.
//!
//! This provides helpful utilities for text (and more specifically line) diff
//! operations.
use std::borrow::Cow;

use crate::algorithms::{capture_diff_slices, group_diff_ops, Algorithm, DiffOp};

#[derive(Clone, Debug)]
pub struct TextDiffBuilder {
    algorithm: Algorithm,
}

impl Default for TextDiffBuilder {
    fn default() -> TextDiffBuilder {
        TextDiffBuilder {
            algorithm: Algorithm::default(),
        }
    }
}

impl TextDiffBuilder {
    pub fn diff_lines<'old, 'new>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'static> {
        self.diff(
            Cow::Owned(split_lines(old).collect()),
            Cow::Owned(split_lines(new).collect()),
        )
    }

    pub fn diff_words<'old, 'new>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'static> {
        self.diff(
            Cow::Owned(split_words(old).collect()),
            Cow::Owned(split_words(new).collect()),
        )
    }

    pub fn diff_slices<'old, 'new, 'bufs>(
        &self,
        old: &'bufs [&'old str],
        new: &'bufs [&'new str],
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(Cow::Borrowed(old), Cow::Borrowed(new))
    }

    fn diff<'old, 'new, 'bufs>(
        &self,
        old: Cow<'bufs, [&'old str]>,
        new: Cow<'bufs, [&'new str]>,
    ) -> TextDiff<'old, 'new, 'bufs> {
        let ops = capture_diff_slices(self.algorithm, &old, &new);
        TextDiff {
            old,
            new,
            ops,
            algorithm: self.algorithm,
        }
    }
}

/// Captures diff op codes for textual diffs
pub struct TextDiff<'old, 'new, 'bufs> {
    old: Cow<'bufs, [&'old str]>,
    new: Cow<'bufs, [&'new str]>,
    ops: Vec<DiffOp>,
    algorithm: Algorithm,
}

impl<'old, 'new, 'bufs> TextDiff<'old, 'new, 'bufs> {
    /// The name of the algorithm that created the diff.
    pub fn algorithm(&self) -> Algorithm {
        self.algorithm
    }

    /// Returns the old slices.
    pub fn old_slices(&self) -> &[&'old str] {
        &self.old
    }

    /// Returns the new slices.
    pub fn new_slices(&self) -> &[&'new str] {
        &self.new
    }

    /// Returns the captured diff ops.
    pub fn ops(&self) -> &[DiffOp] {
        &self.ops
    }

    /// Returns the ops grouped.
    pub fn grouped_ops(&self, n: usize) -> Vec<Vec<DiffOp>> {
        group_diff_ops(self.ops().to_vec(), n)
    }
}

/// Given a string splits it into lines.
///
/// This operation will preserve the newline separation character at the end.
/// It supports all common newline sequences (`\r\n`, `\n` as well as `\r`).
fn split_lines(s: &str) -> impl Iterator<Item = &str> {
    let mut iter = s.char_indices().peekable();
    let mut last_pos = 0;

    std::iter::from_fn(move || {
        if let Some((idx, c)) = iter.next() {
            let mut rv = None;
            if c == '\r' {
                if iter.peek().map_or(false, |x| x.1 == '\n') {
                    rv = Some(&s[last_pos..=idx + 1]);
                    iter.next();
                    last_pos = idx + 2;
                } else {
                    rv = Some(&s[last_pos..=idx]);
                    last_pos = idx + 1;
                }
            } else if c == '\n' {
                rv = Some(&s[last_pos..=idx]);
                last_pos = idx + 1;
            }
            Some(rv)
        } else if last_pos < s.len() {
            let tmp = &s[last_pos..];
            last_pos = s.len();
            Some(Some(tmp))
        } else {
            None
        }
    })
    .flatten()
}

/// Splits text into words with whitespace attached.
pub fn split_words(s: &str) -> impl Iterator<Item = &str> {
    let mut iter = s.char_indices().peekable();
    let mut last_pos = 0;

    std::iter::from_fn(move || {
        if let Some((idx, c)) = iter.next() {
            let mut rv = None;
            if c.is_whitespace() {
                let mut last = (idx, c);
                while let Some(&(next_idx, next_char)) = iter.peek() {
                    if !next_char.is_whitespace() {
                        break;
                    }
                    iter.next();
                    last = (next_idx, next_char);
                }
                let whitespace_end = last.0 + last.1.len_utf8();
                rv = Some(&s[last_pos..whitespace_end]);
                last_pos = whitespace_end;
            }
            Some(rv)
        } else if last_pos < s.len() {
            let tmp = &s[last_pos..];
            last_pos = s.len();
            Some(Some(tmp))
        } else {
            None
        }
    })
    .flatten()
}

#[test]
fn test_split_lines() {
    assert_eq!(
        split_lines("first\nsecond\rthird\r\nfourth\nlast").collect::<Vec<_>>(),
        vec!["first\n", "second\r", "third\r\n", "fourth\n", "last"]
    );
    assert_eq!(split_lines("\n\n").collect::<Vec<_>>(), vec!["\n", "\n"]);
    assert_eq!(split_lines("\n").collect::<Vec<_>>(), vec!["\n"]);
    assert!(split_lines("").collect::<Vec<_>>().is_empty());
}

#[test]
fn test_split_words() {
    assert_eq!(
        split_words("foo    bar baz\n\n  aha").collect::<Vec<_>>(),
        ["foo    ", "bar ", "baz\n\n  ", "aha"]
    );
}

//! Text diffing utilities.
//!
//! This provides helpful utilities for text (and more specifically line) diff
//! operations.  The main type you want to work with is [`TextDiff`] which
//! uses the underlying diff algorithms to expose a convenient API to work with
//! texts.
//!
//! It can produce a unified diff and also let you iterate over the changeset
//! directly if you want.
//!
//! ## Examples
//!
//! A super simple example for how to generate a unified diff with three lines
//! off context around the changes:
//!
//! ```rust
//! # use similar::text::TextDiff;
//! # let old_text = "";
//! # let new_text = "";
//! let diff = TextDiff::from_lines(old_text, new_text);
//! let unified_diff = diff.unified_diff(3, Some(("old_file", "new_file")));
//! ```
//!
//! This is another example that iterates over the actual changes:
//!
//! ```rust
//! # use similar::text::TextDiff;
//! # let old_text = "";
//! # let new_text = "";
//! let diff = TextDiff::from_lines(old_text, new_text);
//! for op in diff.ops() {
//!     for change in diff.iter_changes(op) {
//!         println!("{:?}", change);
//!     }
//! }
//! ```
//!
//! ## Ops vs Changes
//!
//! Because very commonly two compared sequences will largely match this module
//! splits it's functionality into two layers.  The first is inherited from the
//! general [`algorithms`](crate::algorithms) module: changes are encoded as
//! [diff operations](crate::algorithms::DiffOp).  These are ranges of the
//! differences by index in the source sequence.  Because this can be cumbersome
//! to work with a separate method [`TextDiff::iter_changes`] is provided which
//! expands all the changes on an item by item level encoded in an operation.
//!
//! Because the [`TextDiff::grouped_ops`] method can isolate clusters of changes
//! this even works for very long files if paired with this method.
use std::borrow::Cow;
use std::fmt;
use std::io;
use std::ops::Range;

use crate::algorithms::{capture_diff_slices, group_diff_ops, Algorithm, DiffOp, DiffTag};

/// A builder type config for more complex uses of [`TextDiff`].
#[derive(Clone, Debug)]
pub struct TextDiffConfig {
    algorithm: Algorithm,
    newline_terminated: Option<bool>,
}

impl Default for TextDiffConfig {
    fn default() -> TextDiffConfig {
        TextDiffConfig {
            algorithm: Algorithm::default(),
            newline_terminated: None,
        }
    }
}

impl TextDiffConfig {
    /// Changes the algorithm.
    ///
    /// The default algorithm is [`Algorithm::Myers`].
    pub fn algorithm(&mut self, alg: Algorithm) -> &mut Self {
        self.algorithm = alg;
        self
    }

    /// Changes the newlnine termination flag.
    ///
    /// The default is automatic based on input.  This flag controls the
    /// behavior of the [`TextDiff::write_unified_diff`] method with regards
    /// to newlines.  When the flag is set to `false` (which is the default)
    /// then newlines are added.  Otherwise the newlines from the source
    /// sequences are reused.
    pub fn newline_terminated(&mut self, yes: bool) -> &mut Self {
        self.newline_terminated = Some(yes);
        self
    }

    /// Creates a diff of lines.
    ///
    /// This splits the text `old` and `new` into lines preserving newlines
    /// in the input.
    pub fn diff_lines<'old, 'new, 'bufs>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(
            Cow::Owned(split_lines(old).collect()),
            Cow::Owned(split_lines(new).collect()),
            true,
        )
    }

    /// Creates a diff of words.
    pub fn diff_words<'old, 'new, 'bufs>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(
            Cow::Owned(split_words(old).collect()),
            Cow::Owned(split_words(new).collect()),
            false,
        )
    }

    /// Creates a diff of characters.
    pub fn diff_chars<'old, 'new, 'bufs>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(
            Cow::Owned(split_chars(old).collect()),
            Cow::Owned(split_chars(new).collect()),
            false,
        )
    }

    /// Creates a diff of graphemes.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn diff_graphemes<'old, 'new, 'bufs>(
        &self,
        old: &'old str,
        new: &'new str,
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(
            Cow::Owned(split_graphemes(old).collect()),
            Cow::Owned(split_graphemes(new).collect()),
            false,
        )
    }

    /// Creates a diff of arbitrary slices.
    pub fn diff_slices<'old, 'new, 'bufs>(
        &self,
        old: &'bufs [&'old str],
        new: &'bufs [&'new str],
    ) -> TextDiff<'old, 'new, 'bufs> {
        self.diff(Cow::Borrowed(old), Cow::Borrowed(new), false)
    }

    fn diff<'old, 'new, 'bufs>(
        &self,
        old: Cow<'bufs, [&'old str]>,
        new: Cow<'bufs, [&'new str]>,
        newline_terminated: bool,
    ) -> TextDiff<'old, 'new, 'bufs> {
        let ops = capture_diff_slices(self.algorithm, &old, &new);
        TextDiff {
            old,
            new,
            ops,
            newline_terminated: self.newline_terminated.unwrap_or(newline_terminated),
            algorithm: self.algorithm,
        }
    }
}

/// Captures diff op codes for textual diffs
pub struct TextDiff<'old, 'new, 'bufs> {
    old: Cow<'bufs, [&'old str]>,
    new: Cow<'bufs, [&'new str]>,
    ops: Vec<DiffOp>,
    newline_terminated: bool,
    algorithm: Algorithm,
}

/// The tag of a change.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Ord, PartialOrd)]
pub enum ChangeTag {
    Equal,
    Delete,
    Insert,
}

/// Represents the expanded textual change.
///
/// This type is returned from the [`TextDiff::iter_changes`] method.  It
/// exists so that it's more convenient to work with textual differences as
/// the underlying [`DiffOp`] does not know anything about strings.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Ord, PartialOrd)]
pub struct Change<'s> {
    tag: ChangeTag,
    old_index: Option<usize>,
    new_index: Option<usize>,
    value: &'s str,
}

impl<'s> Change<'s> {
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

    /// Returns the changed value.
    pub fn value(&self) -> &'s str {
        self.value
    }
}

impl ChangeTag {
    /// Returns the unified sign of this change.
    ///
    /// This is the prefix rendered into a unified diff:
    ///
    /// * `Equal`: an empty space (` `)
    /// * `Delete: a minus sign (`-`)
    /// * `Insert: a plus sign (`+`)
    pub fn unified_sign(self) -> char {
        match self {
            ChangeTag::Equal => ' ',
            ChangeTag::Delete => '-',
            ChangeTag::Insert => '+',
        }
    }
}

impl<'old, 'new, 'bufs> TextDiff<'old, 'new, 'bufs> {
    /// Configures a text differ before diffing.
    pub fn configure() -> TextDiffConfig {
        TextDiffConfig::default()
    }

    /// Creates a diff of lines.
    pub fn from_lines(old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_lines(old, new)
    }

    /// Creates a diff of words.
    pub fn from_words(&self, old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_words(old, new)
    }

    /// Creates a diff of chars.
    pub fn from_chars(&self, old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_chars(old, new)
    }

    /// Creates a diff of graphemes.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_graphemes(&self, old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_graphemes(old, new)
    }

    /// Creates a diff of arbitrary slices.
    pub fn from_slices(
        &self,
        old: &'bufs [&'old str],
        new: &'bufs [&'new str],
    ) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_slices(old, new)
    }

    /// The name of the algorithm that created the diff.
    pub fn algorithm(&self) -> Algorithm {
        self.algorithm
    }

    /// Returns `true` if items in the slice are newline terminated.
    pub fn newline_terminated(&self) -> bool {
        self.newline_terminated
    }

    /// Returns all old slices.
    pub fn old_slices(&self) -> &[&'old str] {
        &self.old
    }

    /// Returns all new slices.
    pub fn new_slices(&self) -> &[&'new str] {
        &self.new
    }

    /// Iterates over the changes the op expands to.
    ///
    /// This method is a convenient way to automatically resolve the different
    /// ways in which a change could be encoded (insert/delete vs replace), look
    /// up the value from the appropriate slice and also handle correct index
    /// handling.
    pub fn iter_changes(&self, op: &DiffOp) -> impl Iterator<Item = Change> {
        let (tag, old_range, new_range) = op.as_tag_tuple();
        let mut old_index = old_range.start;
        let mut new_index = new_range.start;
        let mut old_slices = &self.old_slices()[op.old_range()];
        let mut new_slices = &self.new_slices()[op.new_range()];

        std::iter::from_fn(move || match tag {
            DiffTag::Equal => {
                if let Some((&first, rest)) = old_slices.split_first() {
                    old_slices = rest;
                    old_index += 1;
                    new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Equal,
                        old_index: Some(old_index - 1),
                        new_index: Some(new_index - 1),
                        value: first,
                    })
                } else {
                    None
                }
            }
            DiffTag::Delete => {
                if let Some((&first, rest)) = old_slices.split_first() {
                    old_slices = rest;
                    old_index += 1;
                    Some(Change {
                        tag: ChangeTag::Delete,
                        old_index: Some(old_index - 1),
                        new_index: None,
                        value: first,
                    })
                } else {
                    None
                }
            }
            DiffTag::Insert => {
                if let Some((&first, rest)) = new_slices.split_first() {
                    new_slices = rest;
                    new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Insert,
                        old_index: None,
                        new_index: Some(new_index - 1),
                        value: first,
                    })
                } else {
                    None
                }
            }
            DiffTag::Replace => {
                if let Some((&first, rest)) = old_slices.split_first() {
                    old_slices = rest;
                    old_index += 1;
                    Some(Change {
                        tag: ChangeTag::Delete,
                        old_index: Some(old_index - 1),
                        new_index: None,
                        value: first,
                    })
                } else if let Some((&first, rest)) = new_slices.split_first() {
                    new_slices = rest;
                    new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Insert,
                        old_index: None,
                        new_index: Some(new_index - 1),
                        value: first,
                    })
                } else {
                    None
                }
            }
        })
    }

    /// Returns the captured diff ops.
    pub fn ops(&self) -> &[DiffOp] {
        &self.ops
    }

    /// Isolate change clusters by eliminating ranges with no changes.
    ///
    /// This is equivalent to calling [`group_diff_ops`] on [`TextDiff::ops`].
    pub fn grouped_ops(&self, n: usize) -> Vec<Vec<DiffOp>> {
        group_diff_ops(self.ops().to_vec(), n)
    }

    /// Format a unified diff as string.
    ///
    /// This is more or less equivalent to using [`TextDiff::write_unified_diff`] just
    /// that a string is produced.  Additionally if line diffs are printed
    /// a single trailing newline is removed automatically.
    pub fn unified_diff(&self, n: usize, header: Option<(&str, &str)>) -> String {
        let mut rv = Vec::<u8>::new();
        self.write_unified_diff(&mut rv, n, header).unwrap();
        if self.newline_terminated && rv.last() == Some(&b'\n') {
            rv.truncate(rv.len() - 1);
        }
        unsafe { String::from_utf8_unchecked(rv) }
    }

    /// Write a unified diff.
    ///
    /// This takes a writer `w` and the number of context lines `n` which should
    /// be shown around changes.  Optionally a `header` can be provided which
    /// will be written.  The header should be two file names.
    pub fn write_unified_diff<W: io::Write>(
        &self,
        mut w: W,
        n: usize,
        mut header: Option<(&str, &str)>,
    ) -> Result<(), io::Error> {
        struct UnifiedRange(Range<usize>);

        impl fmt::Display for UnifiedRange {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let mut beginning = self.0.start;
                let len = self.0.end - self.0.start;
                if len == 1 {
                    write!(f, "{}", beginning)
                } else {
                    if len == 0 {
                        // empty ranges begin at line just before the range
                        beginning -= 1;
                    }
                    write!(f, "{},{}", beginning, len)
                }
            }
        }

        let nl = if self.newline_terminated { "" } else { "\n" };

        for group in self.grouped_ops(n) {
            if let Some((old_file, new_file)) = header.take() {
                writeln!(&mut w, "--- {}", old_file)?;
                writeln!(&mut w, "+++ {}", new_file)?;
            }
            writeln!(
                &mut w,
                "@@ -{} +{} @@",
                UnifiedRange(group[0].old_range()),
                UnifiedRange(group[group.len() - 1].new_range()),
            )?;
            for op in group {
                for change in self.iter_changes(&op) {
                    write!(
                        &mut w,
                        "{}{}{}",
                        change.tag().unified_sign(),
                        change.value(),
                        nl
                    )?;
                }
            }
        }
        Ok(())
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
fn split_words(s: &str) -> impl Iterator<Item = &str> {
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

/// Splits text into characters.
fn split_chars(s: &str) -> impl Iterator<Item = &str> {
    s.char_indices().map(move |(i, c)| &s[i..i + c.len_utf8()])
}

/// Splits text into graphemes.
#[cfg(feature = "unicode")]
fn split_graphemes(s: &str) -> impl Iterator<Item = &str> {
    unicode_segmentation::UnicodeSegmentation::graphemes(s, true)
}

/// Quick way to get a unified diff as string.
pub fn unified_diff<'old, 'new>(
    alg: Algorithm,
    old: &'old str,
    new: &'new str,
    n: usize,
    header: Option<(&str, &str)>,
) -> String {
    TextDiff::configure()
        .algorithm(alg)
        .diff_lines(old, new)
        .unified_diff(n, header)
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

#[test]
fn test_split_chars() {
    assert_eq!(
        split_chars("abcfö❄️").collect::<Vec<_>>(),
        vec!["a", "b", "c", "f", "ö", "❄", "\u{fe0f}"]
    );
}

#[test]
#[cfg(feature = "unicode")]
fn test_split_graphemes() {
    assert_eq!(
        split_graphemes("abcfö❄️").collect::<Vec<_>>(),
        vec!["a", "b", "c", "f", "ö", "❄️"]
    );
}

#[test]
fn test_captured_ops() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    insta::assert_debug_snapshot!(&diff.ops(), @r###"
    [
        Equal {
            old_index: 0,
            new_index: 0,
            len: 1,
        },
        Replace {
            old_index: 1,
            old_len: 1,
            new_index: 1,
            new_len: 1,
        },
        Equal {
            old_index: 2,
            new_index: 2,
            len: 1,
        },
    ]
    "###);
}

#[test]
fn test_unified_diff() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert_eq!(diff.newline_terminated(), true);
    insta::assert_snapshot!(&diff.unified_diff(3, Some(("old", "new"))), @r###"
    --- old
    +++ new
    @@ -0 +2 @@
     Hello World
    -some stuff here
    +some amazing stuff here
     some more stuff here
    "###);
}

#[test]
fn test_line_ops() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert_eq!(diff.newline_terminated(), true);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes, @r###"
    [
        Change {
            tag: Equal,
            old_index: Some(
                0,
            ),
            new_index: Some(
                0,
            ),
            value: "Hello World\n",
        },
        Change {
            tag: Delete,
            old_index: Some(
                1,
            ),
            new_index: None,
            value: "some stuff here\n",
        },
        Change {
            tag: Insert,
            old_index: None,
            new_index: Some(
                1,
            ),
            value: "some amazing stuff here\n",
        },
        Change {
            tag: Equal,
            old_index: Some(
                2,
            ),
            new_index: Some(
                2,
            ),
            value: "some more stuff here\n",
        },
    ]
    "###);
}

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
//! Text diffing is available by default but can be disabled by turning off the
//! default features.  The feature to enable to get it back is `text`.
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
//! let unified_diff = diff.unified_diff().header("old_file", "new_file").to_string();
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
#![cfg(feature = "text")]
use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

use crate::algorithms::{capture_diff_slices, group_diff_ops, Algorithm, DiffOp, DiffTag};
use crate::udiff::UnifiedDiff;

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

    /// Changes the newline termination flag.
    ///
    /// The default is automatic based on input.  This flag controls the
    /// behavior of [`TextDiff::iter_changes`] and unified diff generation
    /// with regards to newlines.  When the flag is set to `false` (which
    /// is the default) then newlines are added.  Otherwise the newlines
    /// from the source sequences are reused.
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

    /// Returns `true` for virtual changes.
    ///
    /// Virtual changes are changes that do not exist in either diff but are
    /// necessary for a consistent user experience.  This currently only
    /// applies to changes related to newline handling.  If lines are passed
    /// to the [`TextDiff`] the [`TextDiff::newline_terminated`] flag is set
    /// in which case newlines of the input are included in the changes.  However
    /// if the trailing newline is missing it would mess up processing greatly.
    /// Because of this a trailing virtual newline is automatically added for a
    /// more consistent user experience.  This virtual newline can be detected
    /// by explicitly checking for this flag.
    pub fn is_virtual(&self) -> bool {
        self.old_index.is_none() && self.new_index.is_none()
    }
}

const VIRTUAL_NEWLINE_CHANGE: Change<'static> = Change {
    tag: ChangeTag::Equal,
    old_index: None,
    new_index: None,
    value: "\n",
};

impl<'old, 'new, 'bufs> TextDiff<'old, 'new, 'bufs> {
    /// Configures a text differ before diffing.
    pub fn configure() -> TextDiffConfig {
        TextDiffConfig::default()
    }

    /// Creates a diff of lines.
    ///
    /// Equivalent to `TextDiff::configure().diff_lines(old, new)`.
    pub fn from_lines(old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_lines(old, new)
    }

    /// Creates a diff of words.
    ///
    /// Equivalent to `TextDiff::configure().diff_words(old, new)`.
    pub fn from_words(old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_words(old, new)
    }

    /// Creates a diff of chars.
    ///
    /// Equivalent to `TextDiff::configure().diff_chars(old, new)`.
    pub fn from_chars(old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_chars(old, new)
    }

    /// Creates a diff of graphemes.
    ///
    /// Equivalent to `TextDiff::configure().diff_graphemes(old, new)`.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_graphemes(old: &'old str, new: &'new str) -> TextDiff<'old, 'new, 'bufs> {
        Self::configure().diff_graphemes(old, new)
    }

    /// Creates a diff of arbitrary slices.
    ///
    /// Equivalent to `TextDiff::configure().diff_slices(old, new)`.
    pub fn from_slices(
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
    ///
    /// This flag is used by the unified diff writer to determine if extra
    /// newlines have to be added.
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

    /// Return a measure of the sequences' similarity in the range `0..=1`.
    ///
    /// A ratio of `1.0` means the two sequences are a complete match, a
    /// ratio of `0.0` would indicate completely distinct sequences.
    ///
    /// ```rust
    /// # use similar::text::TextDiff;
    /// let diff = TextDiff::from_chars("abcd", "bcde");
    /// assert_eq!(diff.ratio(), 0.75);
    /// ```
    pub fn ratio(&self) -> f32 {
        let matches = self
            .ops()
            .iter()
            .map(|op| {
                if let DiffOp::Equal { len, .. } = *op {
                    len
                } else {
                    0
                }
            })
            .sum::<usize>();
        let len = self.old.len() + self.new.len();
        if len == 0 {
            1.0
        } else {
            2.0 * matches as f32 / len as f32
        }
    }

    /// Iterates over the changes the op expands to.
    ///
    /// This method is a convenient way to automatically resolve the different
    /// ways in which a change could be encoded (insert/delete vs replace), look
    /// up the value from the appropriate slice and also handle correct index
    /// handling.
    ///
    /// In addition it has some custom handling to insert "virtual" newlines
    /// for diffs where [`TextDiff::newline_terminated`] is `true` but the
    /// diff does not end in newlines in the right places.  For more information
    /// see [`Change::is_virtual`].
    pub fn iter_changes(&self, op: &DiffOp) -> impl Iterator<Item = Change> {
        let newline_terminated = self.newline_terminated;
        let (tag, old_range, new_range) = op.as_tag_tuple();
        let mut old_index = old_range.start;
        let mut new_index = new_range.start;
        let mut old_slices = &self.old_slices()[op.old_range()];
        let mut new_slices = &self.new_slices()[op.new_range()];

        // figure out if a virtual newline has to be inserted
        let mut virtual_newline = if newline_terminated {
            let last_element = match tag {
                DiffTag::Equal | DiffTag::Delete | DiffTag::Replace => old_slices.last(),
                DiffTag::Insert => new_slices.last(),
            };
            if !last_element.map_or(false, |x| x.ends_with(&['\r', '\n'][..])) {
                Some(VIRTUAL_NEWLINE_CHANGE)
            } else {
                None
            }
        } else {
            None
        };

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
                    virtual_newline.take()
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
                    virtual_newline.take()
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
                    virtual_newline.take()
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
                } else if let Some(virtual_newline) = virtual_newline.take() {
                    Some(virtual_newline)
                } else if let Some((&first, rest)) = new_slices.split_first() {
                    new_slices = rest;
                    new_index += 1;
                    // check for another virtual newline
                    if newline_terminated && rest.is_empty() && !first.ends_with(&['\r', '\n'][..])
                    {
                        virtual_newline = Some(VIRTUAL_NEWLINE_CHANGE);
                    }
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

    /// Utility to return a unified diff formatter.
    pub fn unified_diff<'diff>(&'diff self) -> UnifiedDiff<'diff, 'old, 'new, 'bufs> {
        UnifiedDiff::from_text_diff(self)
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

// quick and dirty way to get an upper sequence ratio.
fn upper_seq_ratio<T: PartialEq>(seq1: &[T], seq2: &[T]) -> f32 {
    let n = seq1.len() + seq2.len();
    if n == 0 {
        1.0
    } else {
        2.0 * seq1.len().min(seq2.len()) as f32 / n as f32
    }
}

/// Internal utility to calculate an upper bound for a ratio for
/// [`get_close_matches`].  This is based on Python's difflib approach
/// of considering the two sets to be multisets.
///
/// It counts the number of matches without regard to order, which is an
/// obvious upper bound.
struct QuickSeqRatio<'a>(HashMap<&'a str, i32>);

impl<'a> QuickSeqRatio<'a> {
    pub fn new(seq: &[&'a str]) -> QuickSeqRatio<'a> {
        let mut counts = HashMap::new();
        for &word in seq {
            *counts.entry(word).or_insert(0) += 1;
        }
        QuickSeqRatio(counts)
    }

    pub fn calc(&self, seq: &[&str]) -> f32 {
        let n = self.0.len() + seq.len();
        if n == 0 {
            return 1.0;
        }

        let mut available = HashMap::new();
        let mut matches = 0;
        for &word in seq {
            let x = if let Some(count) = available.get(&word) {
                *count
            } else {
                self.0.get(&word).copied().unwrap_or(0)
            };
            available.insert(word, x - 1);
            if x > 0 {
                matches += 1;
            }
        }

        2.0 * matches as f32 / n as f32
    }
}

/// Use the text differ to find `n` close matches.
///
/// `cutoff` defines the threshold which needs to be reached for a word
/// to be considered similar.  See [`TextDiff::ratio`] for more information.
///
/// ```
/// # use similar::text::get_close_matches;
/// let matches = get_close_matches(
///     "appel",
///     &["ape", "apple", "peach", "puppy"][..],
///     3,
///     0.6
/// );
/// assert_eq!(matches, vec!["apple", "ape"]);
/// ```
pub fn get_close_matches<'a>(
    word: &str,
    possibilities: &[&'a str],
    n: usize,
    cutoff: f32,
) -> Vec<&'a str> {
    let mut matches = BinaryHeap::new();
    let seq1 = split_chars(word).collect::<Vec<_>>();
    let quick_ratio = QuickSeqRatio::new(&seq1);

    for &possibility in possibilities {
        let seq2 = split_chars(possibility).collect::<Vec<_>>();

        if upper_seq_ratio(&seq1, &seq2) < cutoff || quick_ratio.calc(&seq2) < cutoff {
            continue;
        }

        let diff = TextDiff::from_slices(&seq1, &seq2);
        let ratio = diff.ratio();
        if ratio >= cutoff {
            // we're putting the word iself in reverse in so that matches with
            // the same ratio are ordered lexicographically.
            matches.push(((ratio * u32::MAX as f32) as u32, Reverse(possibility)));
        }
    }

    let mut rv = vec![];
    for _ in 0..n {
        if let Some((_, elt)) = matches.pop() {
            rv.push(elt.0);
        } else {
            break;
        }
    }

    rv
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
    insta::assert_debug_snapshot!(&diff.ops());
}

#[test]
fn test_unified_diff() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    assert_eq!(diff.newline_terminated(), true);
    insta::assert_snapshot!(&diff
        .unified_diff()
        .context_radius(3)
        .header("old", "new")
        .to_string());
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
    insta::assert_debug_snapshot!(&changes);
}

#[test]
fn test_virtual_newlines() {
    let diff = TextDiff::from_lines("a\nb", "a\nc\n");
    assert_eq!(diff.newline_terminated(), true);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
}

#[test]
fn test_char_diff() {
    let diff = TextDiff::from_chars("Hello World", "Hallo Welt");
    insta::assert_debug_snapshot!(diff.ops());
}

#[test]
fn test_ratio() {
    let diff = TextDiff::from_chars("abcd", "bcde");
    assert_eq!(diff.ratio(), 0.75);
    let diff = TextDiff::from_chars("", "");
    assert_eq!(diff.ratio(), 1.0);
}

#[test]
fn test_get_close_matches() {
    let matches = get_close_matches("appel", &["ape", "apple", "peach", "puppy"][..], 3, 0.6);
    assert_eq!(matches, vec!["apple", "ape"]);
    let matches = get_close_matches(
        "hulo",
        &[
            "hi", "hulu", "hali", "hoho", "amaz", "zulo", "blah", "hopp", "uulo", "aulo",
        ][..],
        5,
        0.7,
    );
    assert_eq!(matches, vec!["aulo", "hulu", "uulo", "zulo"]);
}

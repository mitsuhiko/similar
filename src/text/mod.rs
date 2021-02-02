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
//! # Examples
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
//! # Ops vs Changes
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
//!
//! # Trailing Newlines
//!
//! When working with line diffs (and unified diffs in general) there are two
//! "philosophies" to look at lines.  One is to diff lines without their newline
//! character, the other is to diff with the newline character.  Typically the
//! latter is done because text files do not _have_ to end in a newline character.
//! As a result there is a difference between `foo\n` and `foo` as far as diffs
//! are concerned.
//!
//! In similar this is handled on the [`Change`] or [`InlineChange`] level.  If
//! a diff was created via [`TextDiff::from_lines`] the text diffing system is
//! instructed to check if there are missing newlines encountered.  If that is
//! the case the [`Change`] object will return true from the
//! [`Change::missing_newline`] method so the caller knows to handle this by
//! either rendering a virtual newline at that position or to indicate it in
//! different ways.  For instance the unified diff code will render the special
//! `\ No newline at end of file` marker.
//!
//! # Bytes vs Unicode
//!
//! This module concerns itself with a loser definition of "text" than you would
//! normally see in Rust.  While by default it can only operate on [`str`] types
//! by enabling the `bytes` feature it gains support for byte slices with some
//! caveats.
//!
//! A lot of text diff functionality assumes that what is being diffed constiutes
//! text, but in the real world it can often be challenging to ensure that this is
//! all valid utf-8.  Because of this the crate is built so that most functinality
//! also still works with bytes for as long as they are roughtly ASCII compatible.
//!
//! This means you will be successful in creating a unified diff from latin1
//! encoded bytes but if you try to do the same with EBCDIC encoded bytes you
//! will only get garbage.
#![cfg(feature = "text")]
use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fmt;
use std::hash::Hash;

mod abstraction;
#[cfg(feature = "inline")]
mod inline;
mod udiff;
mod utils;

pub use self::abstraction::{DiffableStr, DiffableStrRef};
#[cfg(feature = "inline")]
pub use self::inline::InlineChange;
pub use self::udiff::{unified_diff, UnifiedDiff, UnifiedDiffHunk, UnifiedHunkHeader};

use self::utils::{upper_seq_ratio, QuickSeqRatio};
use crate::algorithms::{
    capture_diff_slices, get_diff_ratio, group_diff_ops, Algorithm, DiffOp, DiffTag,
};

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
    pub fn diff_lines<'old, 'new, 'bufs, T: DiffableStrRef + ?Sized>(
        &self,
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        self.diff(
            Cow::Owned(old.as_diffable_str().split_lines()),
            Cow::Owned(new.as_diffable_str().split_lines()),
            true,
        )
    }

    /// Creates a diff of words.
    ///
    /// This splits the text into words and whitespace.
    pub fn diff_words<'old, 'new, 'bufs, T: DiffableStrRef + ?Sized>(
        &self,
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        self.diff(
            Cow::Owned(old.as_diffable_str().split_words()),
            Cow::Owned(new.as_diffable_str().split_words()),
            false,
        )
    }

    /// Creates a diff of characters.
    pub fn diff_chars<'old, 'new, 'bufs, T: DiffableStrRef + ?Sized>(
        &self,
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        self.diff(
            Cow::Owned(old.as_diffable_str().split_chars()),
            Cow::Owned(new.as_diffable_str().split_chars()),
            false,
        )
    }

    /// Creates a diff of unicode words.
    ///
    /// This splits the text into words according to unicode rules.  This is
    /// generally recommended over [`TextDiffConfig::diff_words`] but
    /// requires a dependency.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn diff_unicode_words<'old, 'new, 'bufs, T: DiffableStrRef + ?Sized>(
        &self,
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        self.diff(
            Cow::Owned(old.as_diffable_str().split_unicode_words()),
            Cow::Owned(new.as_diffable_str().split_unicode_words()),
            false,
        )
    }

    /// Creates a diff of graphemes.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn diff_graphemes<'old, 'new, 'bufs, T: DiffableStrRef + ?Sized>(
        &self,
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        self.diff(
            Cow::Owned(old.as_diffable_str().split_graphemes()),
            Cow::Owned(new.as_diffable_str().split_graphemes()),
            false,
        )
    }

    /// Creates a diff of arbitrary slices.
    pub fn diff_slices<'old, 'new, 'bufs, T: DiffableStr + ?Sized>(
        &self,
        old: &'bufs [&'old T],
        new: &'bufs [&'new T],
    ) -> TextDiff<'old, 'new, 'bufs, T> {
        self.diff(Cow::Borrowed(old), Cow::Borrowed(new), false)
    }

    fn diff<'old, 'new, 'bufs, T: DiffableStr + ?Sized>(
        &self,
        old: Cow<'bufs, [&'old T]>,
        new: Cow<'bufs, [&'new T]>,
        newline_terminated: bool,
    ) -> TextDiff<'old, 'new, 'bufs, T> {
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

/// The tag of a change.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Ord, PartialOrd)]
pub enum ChangeTag {
    /// The change indicates equality (not a change)
    Equal,
    /// The change indicates deleted text.
    Delete,
    /// The change indicates inserted text.
    Insert,
}

impl fmt::Display for ChangeTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                ChangeTag::Equal => ' ',
                ChangeTag::Delete => '-',
                ChangeTag::Insert => '+',
            }
        )
    }
}

/// Represents the expanded textual change.
///
/// This type is returned from the [`TextDiff::iter_changes`] method.  It
/// exists so that it's more convenient to work with textual differences as
/// the underlying [`DiffOp`] does not know anything about strings.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Ord, PartialOrd)]
pub struct Change<'s, T: DiffableStr + ?Sized> {
    tag: ChangeTag,
    old_index: Option<usize>,
    new_index: Option<usize>,
    value: &'s T,
    missing_newline: bool,
}

impl<'s, T: DiffableStr + ?Sized> fmt::Display for Change<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.to_string_lossy(),
            if self.missing_newline { "\n" } else { "" }
        )
    }
}

impl<'s, T: DiffableStr + ?Sized> Change<'s, T> {
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

    /// Returns the underlying changed value.
    ///
    /// Depending on the type of the underlying [`DiffableStr`] this value is
    /// more or less useful.  If you always want to have a utf-8 string it's
    /// best to use the [`Change::as_str`] and [`Change::to_string_lossy`] methods.
    pub fn value(&self) -> &'s T {
        self.value
    }

    /// Returns the value as string if it is utf-8.
    pub fn as_str(&self) -> Option<&'s str> {
        T::as_str(self.value)
    }

    /// Returns the value (lossy) decoded as utf-8 string.
    pub fn to_string_lossy(&self) -> Cow<'s, str> {
        T::to_string_lossy(self.value)
    }

    /// Returns `true` if this change needs to be followed up by a
    /// missing newline.
    ///
    /// The [`std::fmt::Display`] implementation of [`Change`] will automatically
    /// insert a newline after the value if this is true.
    pub fn missing_newline(&self) -> bool {
        self.missing_newline
    }
}

/// Captures diff op codes for textual diffs
pub struct TextDiff<'old, 'new, 'bufs, T: DiffableStr + ?Sized> {
    old: Cow<'bufs, [&'old T]>,
    new: Cow<'bufs, [&'new T]>,
    ops: Vec<DiffOp>,
    newline_terminated: bool,
    algorithm: Algorithm,
}

impl<'old, 'new, 'bufs> TextDiff<'old, 'new, 'bufs, str> {
    /// Configures a text differ before diffing.
    pub fn configure() -> TextDiffConfig {
        TextDiffConfig::default()
    }

    /// Creates a diff of lines.
    ///
    /// Equivalent to `TextDiff::configure().diff_lines(old, new)`.
    pub fn from_lines<T: DiffableStrRef + ?Sized>(
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        TextDiff::configure().diff_lines(old, new)
    }

    /// Creates a diff of words.
    ///
    /// Equivalent to `TextDiff::configure().diff_words(old, new)`.
    pub fn from_words<T: DiffableStrRef + ?Sized>(
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        TextDiff::configure().diff_words(old, new)
    }

    /// Creates a diff of chars.
    ///
    /// Equivalent to `TextDiff::configure().diff_chars(old, new)`.
    pub fn from_chars<T: DiffableStrRef + ?Sized>(
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        TextDiff::configure().diff_chars(old, new)
    }

    /// Creates a diff of unicode words.
    ///
    /// Equivalent to `TextDiff::configure().diff_unicode_words(old, new)`.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_unicode_words<T: DiffableStrRef + ?Sized>(
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        TextDiff::configure().diff_unicode_words(old, new)
    }

    /// Creates a diff of graphemes.
    ///
    /// Equivalent to `TextDiff::configure().diff_graphemes(old, new)`.
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_graphemes<T: DiffableStrRef + ?Sized>(
        old: &'old T,
        new: &'new T,
    ) -> TextDiff<'old, 'new, 'bufs, T::Output> {
        TextDiff::configure().diff_graphemes(old, new)
    }
}

impl<'old, 'new, 'bufs, T: DiffableStr + ?Sized + 'old + 'new> TextDiff<'old, 'new, 'bufs, T> {
    /// Creates a diff of arbitrary slices.
    ///
    /// Equivalent to `TextDiff::configure().diff_slices(old, new)`.
    pub fn from_slices(
        old: &'bufs [&'old T],
        new: &'bufs [&'new T],
    ) -> TextDiff<'old, 'new, 'bufs, T> {
        TextDiff::configure().diff_slices(old, new)
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
    pub fn old_slices(&self) -> &[&'old T] {
        &self.old
    }

    /// Returns all new slices.
    pub fn new_slices(&self) -> &[&'new T] {
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
        get_diff_ratio(self.ops(), self.old.len(), self.new.len())
    }

    /// Iterates over the changes the op expands to.
    ///
    /// This method is a convenient way to automatically resolve the different
    /// ways in which a change could be encoded (insert/delete vs replace), look
    /// up the value from the appropriate slice and also handle correct index
    /// handling.
    pub fn iter_changes(&self, op: &DiffOp) -> impl Iterator<Item = Change<'_, T>> {
        let newline_terminated = self.newline_terminated;
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
                        missing_newline: newline_terminated
                            && rest.is_empty()
                            && !first.ends_with_newline(),
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
                        missing_newline: newline_terminated
                            && rest.is_empty()
                            && !first.ends_with_newline(),
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
                        missing_newline: newline_terminated
                            && rest.is_empty()
                            && !first.ends_with_newline(),
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
                        missing_newline: newline_terminated
                            && rest.is_empty()
                            && !first.ends_with_newline(),
                    })
                } else if let Some((&first, rest)) = new_slices.split_first() {
                    new_slices = rest;
                    new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Insert,
                        old_index: None,
                        new_index: Some(new_index - 1),
                        value: first,
                        missing_newline: newline_terminated
                            && rest.is_empty()
                            && !first.ends_with_newline(),
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
    pub fn unified_diff<'diff>(&'diff self) -> UnifiedDiff<'diff, 'old, 'new, 'bufs, T> {
        UnifiedDiff::from_text_diff(self)
    }

    /// Iterates over the changes the op expands to with inline emphasis.
    ///
    /// This is very similar to [`TextDiff::iter_changes`] but it performs a second
    /// level diff on adjacent line replacements.  The exact behavior of
    /// this function with regards to how it detects those inline changes
    /// is currently not defined and will likely change over time.
    #[cfg(feature = "inline")]
    pub fn iter_inline_changes(&self, op: &DiffOp) -> impl Iterator<Item = InlineChange<'_, T>> {
        inline::iter_inline_changes(self, op)
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
pub fn get_close_matches<'a, T: DiffableStr + ?Sized>(
    word: &T,
    possibilities: &[&'a T],
    n: usize,
    cutoff: f32,
) -> Vec<&'a T> {
    let mut matches = BinaryHeap::new();
    let seq1 = word.split_chars();
    let quick_ratio = QuickSeqRatio::new(&seq1);

    for &possibility in possibilities {
        let seq2 = possibility.split_chars();

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
fn test_captured_ops() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    insta::assert_debug_snapshot!(&diff.ops());
}

#[test]
fn test_captured_word_ops() {
    let diff = TextDiff::from_words(
        "Hello World\nsome stuff here\nsome more stuff here\n",
        "Hello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
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
    let a = "Hello World\nsome stuff here\nsome more stuff here\n";
    let b = "Hello World\nsome amazing stuff here\nsome more stuff here\n";
    let diff = TextDiff::from_lines(a, b);
    assert_eq!(diff.newline_terminated(), true);
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_changes(op))
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);

    #[cfg(feature = "bytes")]
    {
        let byte_diff = TextDiff::from_lines(a.as_bytes(), b.as_bytes());
        let byte_changes = byte_diff
            .ops()
            .iter()
            .flat_map(|op| byte_diff.iter_changes(op))
            .collect::<Vec<_>>();
        for (change, byte_change) in changes.iter().zip(byte_changes.iter()) {
            assert_eq!(change.to_string_lossy(), byte_change.to_string_lossy());
        }
    }
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

    #[cfg(feature = "bytes")]
    {
        let byte_diff = TextDiff::from_chars("Hello World".as_bytes(), "Hallo Welt".as_bytes());
        assert_eq!(diff.ops(), byte_diff.ops());
    }
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

//! Text diffing utilities.
use std::borrow::Borrow;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::ops::{Index, Range};
use std::time::Duration;

mod abstraction;
#[cfg(feature = "inline")]
mod inline;
mod utils;

pub use self::abstraction::{DiffInput, DiffableStr, DiffableStrRef, IntoDiffInput};
#[cfg(feature = "inline")]
pub use self::inline::{InlineChange, InlineChangeMode, InlineChangeOptions};

use self::utils::{QuickSeqRatio, upper_seq_ratio};
use crate::algorithms::IdentifyDistinct;
use crate::deadline_support::{Instant, duration_to_deadline};
use crate::udiff::UnifiedDiff;
use crate::{
    Algorithm, Change, ChangeTag, DiffOp, DiffTag, capture_diff_deadline, diff_ratio,
    group_diff_ops,
};

#[derive(Debug, Clone, Copy)]
enum Deadline {
    Absolute(Instant),
    Relative(Duration),
}

impl Deadline {
    fn into_instant(self) -> Option<Instant> {
        match self {
            Deadline::Absolute(instant) => Some(instant),
            Deadline::Relative(duration) => duration_to_deadline(duration),
        }
    }
}

pub(crate) enum TextDiffSide<'a, T: DiffableStr + ?Sized> {
    BorrowedTokens(Vec<&'a T>),
    OwnedTokens(Vec<<T as ToOwned>::Owned>),
}

impl<'a, T: DiffableStr + ?Sized> TextDiffSide<'a, T> {
    fn from_tokenized(input: DiffInput<'a, T>, tokenize: impl FnOnce(&T) -> Vec<&T>) -> Self {
        match input {
            DiffInput::Borrowed(value) => TextDiffSide::BorrowedTokens(tokenize(value)),
            DiffInput::Owned(value) => {
                let tokens = tokenize(value.borrow())
                    .into_iter()
                    .map(ToOwned::to_owned)
                    .collect();
                TextDiffSide::OwnedTokens(tokens)
            }
        }
    }

    fn from_slices(slices: &[&'a T]) -> Self {
        TextDiffSide::BorrowedTokens(slices.to_vec())
    }

    fn len(&self) -> usize {
        match self {
            TextDiffSide::BorrowedTokens(slices) => slices.len(),
            TextDiffSide::OwnedTokens(slices) => slices.len(),
        }
    }

    fn get(&self, index: usize) -> Option<&T> {
        match self {
            TextDiffSide::BorrowedTokens(slices) => slices.get(index).copied(),
            TextDiffSide::OwnedTokens(slices) => slices.get(index).map(Borrow::borrow),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        (0..self.len()).map(|idx| self.get(idx).expect("slice out of bounds"))
    }
}

impl<T: DiffableStr + ?Sized> Index<usize> for TextDiffSide<'_, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("slice out of bounds")
    }
}

/// Iterator for [`TextDiff::iter_changes`].
pub struct TextChangesIter<'diff, 'old, 'new, T: DiffableStr + ?Sized> {
    diff: &'diff TextDiff<'old, 'new, T>,
    old_range: Range<usize>,
    new_range: Range<usize>,
    old_index: usize,
    new_index: usize,
    old_i: usize,
    new_i: usize,
    tag: DiffTag,
}

impl<'diff, 'old, 'new, T: DiffableStr + ?Sized> TextChangesIter<'diff, 'old, 'new, T> {
    fn new(diff: &'diff TextDiff<'old, 'new, T>, op: &DiffOp) -> Self {
        let (tag, old_range, new_range) = op.as_tag_tuple();
        let old_index = old_range.start;
        let new_index = new_range.start;
        let old_i = old_range.start;
        let new_i = new_range.start;
        TextChangesIter {
            diff,
            old_range,
            new_range,
            old_index,
            new_index,
            old_i,
            new_i,
            tag,
        }
    }
}

impl<'diff, 'old, 'new, T: DiffableStr + ?Sized> Iterator
    for TextChangesIter<'diff, 'old, 'new, T>
{
    type Item = Change<&'diff T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tag {
            DiffTag::Equal => {
                if self.old_i < self.old_range.end {
                    let value = self
                        .diff
                        .old_side()
                        .get(self.old_i)
                        .expect("diff operation old range out of bounds");
                    self.old_i += 1;
                    self.old_index += 1;
                    self.new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Equal,
                        old_index: Some(self.old_index - 1),
                        new_index: Some(self.new_index - 1),
                        value,
                    })
                } else {
                    None
                }
            }
            DiffTag::Delete => {
                if self.old_i < self.old_range.end {
                    let value = self
                        .diff
                        .old_side()
                        .get(self.old_i)
                        .expect("diff operation old range out of bounds");
                    self.old_i += 1;
                    self.old_index += 1;
                    Some(Change {
                        tag: ChangeTag::Delete,
                        old_index: Some(self.old_index - 1),
                        new_index: None,
                        value,
                    })
                } else {
                    None
                }
            }
            DiffTag::Insert => {
                if self.new_i < self.new_range.end {
                    let value = self
                        .diff
                        .new_side()
                        .get(self.new_i)
                        .expect("diff operation new range out of bounds");
                    self.new_i += 1;
                    self.new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Insert,
                        old_index: None,
                        new_index: Some(self.new_index - 1),
                        value,
                    })
                } else {
                    None
                }
            }
            DiffTag::Replace => {
                if self.old_i < self.old_range.end {
                    let value = self
                        .diff
                        .old_side()
                        .get(self.old_i)
                        .expect("diff operation old range out of bounds");
                    self.old_i += 1;
                    self.old_index += 1;
                    Some(Change {
                        tag: ChangeTag::Delete,
                        old_index: Some(self.old_index - 1),
                        new_index: None,
                        value,
                    })
                } else if self.new_i < self.new_range.end {
                    let value = self
                        .diff
                        .new_side()
                        .get(self.new_i)
                        .expect("diff operation new range out of bounds");
                    self.new_i += 1;
                    self.new_index += 1;
                    Some(Change {
                        tag: ChangeTag::Insert,
                        old_index: None,
                        new_index: Some(self.new_index - 1),
                        value,
                    })
                } else {
                    None
                }
            }
        }
    }
}

/// A builder type config for more complex uses of [`TextDiff`].
///
/// Requires the `text` feature.
#[derive(Clone, Debug, Default)]
pub struct TextDiffConfig {
    algorithm: Algorithm,
    newline_terminated: Option<bool>,
    deadline: Option<Deadline>,
}

impl TextDiffConfig {
    /// Changes the algorithm.
    ///
    /// The default algorithm is [`Algorithm::Myers`].
    pub fn algorithm(&mut self, alg: Algorithm) -> &mut Self {
        self.algorithm = alg;
        self
    }

    /// Sets a deadline for the diff operation.
    ///
    /// By default a diff will take as long as it takes.  For certain diff
    /// algorithms like Myers', Patience and Hunt a maximum running time can be
    /// defined after which the algorithm gives up and approximates.
    pub fn deadline(&mut self, deadline: Instant) -> &mut Self {
        self.deadline = Some(Deadline::Absolute(deadline));
        self
    }

    /// Sets a timeout for thediff operation.
    ///
    /// This is like [`deadline`](Self::deadline) but accepts a duration.
    pub fn timeout(&mut self, timeout: Duration) -> &mut Self {
        self.deadline = Some(Deadline::Relative(timeout));
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
    /// in the input.  Line diffs are very common and because of that enjoy
    /// special handling in similar.  When a line diff is created with this
    /// method the `newline_terminated` flag is flipped to `true` and will
    /// influence the behavior of unified diff generation.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let diff = TextDiff::configure().diff_lines("a\nb\nc", "a\nb\nC");
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "a\n"),
    ///    (ChangeTag::Equal, "b\n"),
    ///    (ChangeTag::Delete, "c"),
    ///    (ChangeTag::Insert, "C"),
    /// ]);
    /// ```
    pub fn diff_lines<'old, 'new, Old, New, T>(&self, old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        self.diff(
            TextDiffSide::from_tokenized(old.into_diff_input(), DiffableStr::tokenize_lines),
            TextDiffSide::from_tokenized(new.into_diff_input(), DiffableStr::tokenize_lines),
            true,
        )
    }

    /// Creates a diff of words.
    ///
    /// This splits the text into words and whitespace.
    ///
    /// Note on word diffs: because the text differ will tokenize the strings
    /// into small segments it can be inconvenient to work with the results
    /// depending on the use case.  You might also want to combine word level
    /// diffs with the [`TextDiffRemapper`](crate::utils::TextDiffRemapper)
    /// which lets you remap the diffs back to the original input strings.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let diff = TextDiff::configure().diff_words("foo bar baz", "foo BAR baz");
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "foo"),
    ///    (ChangeTag::Equal, " "),
    ///    (ChangeTag::Delete, "bar"),
    ///    (ChangeTag::Insert, "BAR"),
    ///    (ChangeTag::Equal, " "),
    ///    (ChangeTag::Equal, "baz"),
    /// ]);
    /// ```
    pub fn diff_words<'old, 'new, Old, New, T>(&self, old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        self.diff(
            TextDiffSide::from_tokenized(old.into_diff_input(), DiffableStr::tokenize_words),
            TextDiffSide::from_tokenized(new.into_diff_input(), DiffableStr::tokenize_words),
            false,
        )
    }

    /// Creates a diff of characters.
    ///
    /// Note on character diffs: because the text differ will tokenize the strings
    /// into small segments it can be inconvenient to work with the results
    /// depending on the use case.  You might also want to combine word level
    /// diffs with the [`TextDiffRemapper`](crate::utils::TextDiffRemapper)
    /// which lets you remap the diffs back to the original input strings.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let diff = TextDiff::configure().diff_chars("abcdef", "abcDDf");
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "a"),
    ///    (ChangeTag::Equal, "b"),
    ///    (ChangeTag::Equal, "c"),
    ///    (ChangeTag::Delete, "d"),
    ///    (ChangeTag::Delete, "e"),
    ///    (ChangeTag::Insert, "D"),
    ///    (ChangeTag::Insert, "D"),
    ///    (ChangeTag::Equal, "f"),
    /// ]);
    /// ```
    pub fn diff_chars<'old, 'new, Old, New, T>(&self, old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        self.diff(
            TextDiffSide::from_tokenized(old.into_diff_input(), DiffableStr::tokenize_chars),
            TextDiffSide::from_tokenized(new.into_diff_input(), DiffableStr::tokenize_chars),
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
    ///
    /// Note on word diffs: because the text differ will tokenize the strings
    /// into small segments it can be inconvenient to work with the results
    /// depending on the use case.  You might also want to combine word level
    /// diffs with the [`TextDiffRemapper`](crate::utils::TextDiffRemapper)
    /// which lets you remap the diffs back to the original input strings.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let diff = TextDiff::configure().diff_unicode_words("ah(be)ce", "ah(ah)ce");
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "ah"),
    ///    (ChangeTag::Equal, "("),
    ///    (ChangeTag::Delete, "be"),
    ///    (ChangeTag::Insert, "ah"),
    ///    (ChangeTag::Equal, ")"),
    ///    (ChangeTag::Equal, "ce"),
    /// ]);
    /// ```
    #[cfg(feature = "unicode")]
    pub fn diff_unicode_words<'old, 'new, Old, New, T>(
        &self,
        old: Old,
        new: New,
    ) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        self.diff(
            TextDiffSide::from_tokenized(
                old.into_diff_input(),
                DiffableStr::tokenize_unicode_words,
            ),
            TextDiffSide::from_tokenized(
                new.into_diff_input(),
                DiffableStr::tokenize_unicode_words,
            ),
            false,
        )
    }

    /// Creates a diff of graphemes.
    ///
    /// This requires the `unicode` feature.
    ///
    /// Note on grapheme diffs: because the text differ will tokenize the strings
    /// into small segments it can be inconvenient to work with the results
    /// depending on the use case.  You might also want to combine word level
    /// diffs with the [`TextDiffRemapper`](crate::utils::TextDiffRemapper)
    /// which lets you remap the diffs back to the original input strings.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let diff = TextDiff::configure().diff_graphemes("💩🇦🇹🦠", "💩🇦🇱❄️");
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "💩"),
    ///    (ChangeTag::Delete, "🇦🇹"),
    ///    (ChangeTag::Delete, "🦠"),
    ///    (ChangeTag::Insert, "🇦🇱"),
    ///    (ChangeTag::Insert, "❄️"),
    /// ]);
    /// ```
    #[cfg(feature = "unicode")]
    pub fn diff_graphemes<'old, 'new, Old, New, T>(
        &self,
        old: Old,
        new: New,
    ) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        self.diff(
            TextDiffSide::from_tokenized(old.into_diff_input(), DiffableStr::tokenize_graphemes),
            TextDiffSide::from_tokenized(new.into_diff_input(), DiffableStr::tokenize_graphemes),
            false,
        )
    }

    /// Creates a diff of arbitrary slices.
    ///
    /// ```rust
    /// use similar::{TextDiff, ChangeTag};
    ///
    /// let old = &["foo", "bar", "baz"];
    /// let new = &["foo", "BAR", "baz"];
    /// let diff = TextDiff::configure().diff_slices(old, new);
    /// let changes: Vec<_> = diff
    ///     .iter_all_changes()
    ///     .map(|x| (x.tag(), x.value()))
    ///     .collect();
    ///
    /// assert_eq!(changes, vec![
    ///    (ChangeTag::Equal, "foo"),
    ///    (ChangeTag::Delete, "bar"),
    ///    (ChangeTag::Insert, "BAR"),
    ///    (ChangeTag::Equal, "baz"),
    /// ]);
    /// ```
    pub fn diff_slices<'old, 'new, T: DiffableStr + ?Sized>(
        &self,
        old: &[&'old T],
        new: &[&'new T],
    ) -> TextDiff<'old, 'new, T> {
        self.diff(
            TextDiffSide::from_slices(old),
            TextDiffSide::from_slices(new),
            false,
        )
    }

    fn diff<'old, 'new, T: DiffableStr + ?Sized>(
        &self,
        old: TextDiffSide<'old, T>,
        new: TextDiffSide<'new, T>,
        newline_terminated: bool,
    ) -> TextDiff<'old, 'new, T> {
        let deadline = self.deadline.and_then(|x| x.into_instant());
        let ops = if old.len() > 100 || new.len() > 100 {
            let ih = IdentifyDistinct::<u32>::new(&old, 0..old.len(), &new, 0..new.len());
            capture_diff_deadline(
                self.algorithm,
                ih.old_lookup(),
                ih.old_range(),
                ih.new_lookup(),
                ih.new_range(),
                deadline,
            )
        } else {
            capture_diff_deadline(
                self.algorithm,
                &old,
                0..old.len(),
                &new,
                0..new.len(),
                deadline,
            )
        };
        TextDiff {
            old,
            new,
            ops,
            newline_terminated: self.newline_terminated.unwrap_or(newline_terminated),
            algorithm: self.algorithm,
        }
    }
}

/// Captures diff op codes for textual diffs.
///
/// The exact diff behavior is depending on the underlying [`DiffableStr`].
/// For instance diffs on bytes and strings are slightly different.  You can
/// create a text diff from constructors such as [`TextDiff::from_lines`] or
/// the [`TextDiffConfig`] created by [`TextDiff::configure`].
///
/// Requires the `text` feature.
pub struct TextDiff<'old, 'new, T: DiffableStr + ?Sized> {
    old: TextDiffSide<'old, T>,
    new: TextDiffSide<'new, T>,
    ops: Vec<DiffOp>,
    newline_terminated: bool,
    algorithm: Algorithm,
}

impl<'old, 'new> TextDiff<'old, 'new, str> {
    /// Configures a text differ before diffing.
    pub fn configure() -> TextDiffConfig {
        TextDiffConfig::default()
    }

    /// Creates a diff of lines.
    ///
    /// For more information see [`TextDiffConfig::diff_lines`].
    pub fn from_lines<Old, New, T>(old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextDiff::configure().diff_lines(old, new)
    }

    /// Creates a diff of words.
    ///
    /// For more information see [`TextDiffConfig::diff_words`].
    pub fn from_words<Old, New, T>(old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextDiff::configure().diff_words(old, new)
    }

    /// Creates a diff of chars.
    ///
    /// For more information see [`TextDiffConfig::diff_chars`].
    pub fn from_chars<Old, New, T>(old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextDiff::configure().diff_chars(old, new)
    }

    /// Creates a diff of unicode words.
    ///
    /// For more information see [`TextDiffConfig::diff_unicode_words`].
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_unicode_words<Old, New, T>(old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextDiff::configure().diff_unicode_words(old, new)
    }

    /// Creates a diff of graphemes.
    ///
    /// For more information see [`TextDiffConfig::diff_graphemes`].
    ///
    /// This requires the `unicode` feature.
    #[cfg(feature = "unicode")]
    pub fn from_graphemes<Old, New, T>(old: Old, new: New) -> TextDiff<'old, 'new, T>
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextDiff::configure().diff_graphemes(old, new)
    }
}

impl<'old, 'new, T: DiffableStr + ?Sized> TextDiff<'old, 'new, T> {
    /// Creates a diff of arbitrary slices.
    ///
    /// For more information see [`TextDiffConfig::diff_slices`].
    pub fn from_slices(old: &[&'old T], new: &[&'new T]) -> TextDiff<'old, 'new, T> {
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

    /// Returns the number of old side slices.
    pub fn old_len(&self) -> usize {
        self.old.len()
    }

    /// Returns the number of new side slices.
    pub fn new_len(&self) -> usize {
        self.new.len()
    }

    /// Returns a specific old side slice.
    pub fn old_slice(&self, index: usize) -> Option<&T> {
        self.old.get(index)
    }

    /// Returns a specific new side slice.
    pub fn new_slice(&self, index: usize) -> Option<&T> {
        self.new.get(index)
    }

    /// Iterates all old side slices.
    pub fn iter_old_slices(&self) -> impl Iterator<Item = &T> {
        self.old.iter()
    }

    /// Iterates all new side slices.
    pub fn iter_new_slices(&self) -> impl Iterator<Item = &T> {
        self.new.iter()
    }

    /// Returns an indexable lookup for the old side slices.
    pub fn old_lookup(&self) -> &impl Index<usize, Output = T> {
        &self.old
    }

    /// Returns an indexable lookup for the new side slices.
    pub fn new_lookup(&self) -> &impl Index<usize, Output = T> {
        &self.new
    }

    pub(crate) fn old_side(&self) -> &TextDiffSide<'old, T> {
        &self.old
    }

    pub(crate) fn new_side(&self) -> &TextDiffSide<'new, T> {
        &self.new
    }

    /// Return a measure of the sequences' similarity in the range `0..=1`.
    ///
    /// A ratio of `1.0` means the two sequences are a complete match, a
    /// ratio of `0.0` would indicate completely distinct sequences.
    ///
    /// ```rust
    /// # use similar::TextDiff;
    /// let diff = TextDiff::from_chars("abcd", "bcde");
    /// assert_eq!(diff.ratio(), 0.75);
    /// ```
    pub fn ratio(&self) -> f32 {
        diff_ratio(self.ops(), self.old_len(), self.new_len())
    }

    /// Iterates over the changes the op expands to.
    ///
    /// This method is a convenient way to automatically resolve the different
    /// ways in which a change could be encoded (insert/delete vs replace), look
    /// up the value from the appropriate slice and also handle correct index
    /// handling.
    ///
    /// # Panics
    ///
    /// Panics if the passed [`DiffOp`] contains indexes that are out of bounds
    /// for this diff's old/new side.
    pub fn iter_changes(&self, op: &DiffOp) -> TextChangesIter<'_, 'old, 'new, T> {
        TextChangesIter::new(self, op)
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

    /// Flattens out the diff into all changes.
    ///
    /// This is a shortcut for combining [`TextDiff::ops`] with
    /// [`TextDiff::iter_changes`].
    pub fn iter_all_changes(&self) -> impl Iterator<Item = Change<&T>> + '_ {
        self.ops().iter().flat_map(|op| self.iter_changes(op))
    }

    /// Flattens out the diff into all inline changes.
    ///
    /// This is a shortcut for combining [`TextDiff::ops`] with
    /// [`TextDiff::iter_inline_changes`].
    ///
    /// Requires the `inline` feature.
    #[cfg(feature = "inline")]
    pub fn iter_all_inline_changes(&self) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        self.ops()
            .iter()
            .flat_map(move |op| self.iter_inline_changes(op))
    }

    /// Flattens out the diff into all inline changes with an explicit deadline.
    ///
    /// This is a shortcut for combining [`TextDiff::ops`] with
    /// [`TextDiff::iter_inline_changes_deadline`].
    ///
    /// Requires the `inline` feature.
    #[cfg(feature = "inline")]
    pub fn iter_all_inline_changes_deadline(
        &self,
        deadline: Option<Instant>,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        self.ops()
            .iter()
            .flat_map(move |op| self.iter_inline_changes_deadline(op, deadline))
    }

    /// Flattens out the diff into all inline changes with custom options.
    ///
    /// This is a shortcut for combining [`TextDiff::ops`] with
    /// [`TextDiff::iter_inline_changes_with_options`].
    ///
    /// Requires the `inline` feature.
    #[cfg(feature = "inline")]
    pub fn iter_all_inline_changes_with_options(
        &self,
        options: InlineChangeOptions,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        self.ops()
            .iter()
            .flat_map(move |op| self.iter_inline_changes_with_options(op, options))
    }

    /// Flattens out the diff into all inline changes with custom options and deadline.
    ///
    /// This is a shortcut for combining [`TextDiff::ops`] with
    /// [`TextDiff::iter_inline_changes_with_options_deadline`].
    ///
    /// Requires the `inline` feature.
    #[cfg(feature = "inline")]
    pub fn iter_all_inline_changes_with_options_deadline(
        &self,
        options: InlineChangeOptions,
        deadline: Option<Instant>,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        self.ops().iter().flat_map(move |op| {
            self.iter_inline_changes_with_options_deadline(op, options, deadline)
        })
    }

    /// Utility to return a unified diff formatter.
    pub fn unified_diff<'diff>(&'diff self) -> UnifiedDiff<'diff, 'old, 'new, T> {
        UnifiedDiff::from_text_diff(self)
    }

    /// Iterates over the changes the op expands to with inline emphasis.
    ///
    /// This is very similar to [`TextDiff::iter_changes`] but it performs a second
    /// level diff on adjacent line replacements.  The exact behavior of
    /// this function with regards to how it detects those inline changes
    /// is currently not defined and will likely change over time.
    ///
    /// This method has a hardcoded 500ms deadline which is often not ideal.  For
    /// fine tuning use [`iter_inline_changes_deadline`](Self::iter_inline_changes_deadline).
    ///
    /// For full control over tokenization mode, algorithm and heuristics use
    /// [`iter_inline_changes_with_options`](Self::iter_inline_changes_with_options)
    /// or its deadline variant.
    ///
    /// Requires the `inline` feature.
    #[cfg(feature = "inline")]
    pub fn iter_inline_changes(
        &self,
        op: &DiffOp,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        use crate::deadline_support::duration_to_deadline;

        inline::iter_inline_changes(
            self,
            op,
            duration_to_deadline(Duration::from_millis(500)),
            InlineChangeOptions::default(),
        )
    }

    /// Iterates over the changes the op expands to with inline emphasis with a deadline.
    ///
    /// Like [`iter_inline_changes`](Self::iter_inline_changes) but with an explicit deadline.
    #[cfg(feature = "inline")]
    pub fn iter_inline_changes_deadline(
        &self,
        op: &DiffOp,
        deadline: Option<Instant>,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        inline::iter_inline_changes(self, op, deadline, InlineChangeOptions::default())
    }

    /// Iterates over the changes the op expands to with inline emphasis and options.
    ///
    /// Like [`iter_inline_changes`](Self::iter_inline_changes) but with custom
    /// inline refinement options.  For improved human readability of intraline
    /// edits you can enable semantic cleanup via
    /// [`InlineChangeOptions::semantic_cleanup`].
    #[cfg(feature = "inline")]
    pub fn iter_inline_changes_with_options(
        &self,
        op: &DiffOp,
        options: InlineChangeOptions,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        use crate::deadline_support::duration_to_deadline;

        inline::iter_inline_changes(
            self,
            op,
            duration_to_deadline(Duration::from_millis(500)),
            options,
        )
    }

    /// Iterates over the changes the op expands to with inline emphasis, options and deadline.
    #[cfg(feature = "inline")]
    pub fn iter_inline_changes_with_options_deadline(
        &self,
        op: &DiffOp,
        options: InlineChangeOptions,
        deadline: Option<Instant>,
    ) -> impl Iterator<Item = InlineChange<'_, T>> + '_ {
        inline::iter_inline_changes(self, op, deadline, options)
    }
}

/// Use the text differ to find `n` close matches.
///
/// `cutoff` defines the threshold which needs to be reached for a word
/// to be considered similar.  See [`TextDiff::ratio`] for more information.
///
/// ```
/// # use similar::get_close_matches;
/// let matches = get_close_matches(
///     "appel",
///     &["ape", "apple", "peach", "puppy"][..],
///     3,
///     0.6
/// );
/// assert_eq!(matches, vec!["apple", "ape"]);
/// ```
///
/// Requires the `text` feature.
pub fn get_close_matches<'a, T: DiffableStr + ?Sized>(
    word: &T,
    possibilities: &[&'a T],
    n: usize,
    cutoff: f32,
) -> Vec<&'a T> {
    let mut matches = BinaryHeap::new();
    let seq1 = word.tokenize_chars();
    let quick_ratio = QuickSeqRatio::new(&seq1);

    for &possibility in possibilities {
        let seq2 = possibility.tokenize_chars();

        if upper_seq_ratio(&seq1, &seq2) < cutoff || quick_ratio.calc(&seq2) < cutoff {
            continue;
        }

        let diff = TextDiff::from_slices(&seq1, &seq2);
        let ratio = diff.ratio();
        if ratio >= cutoff {
            // we're putting the word itself in reverse in so that matches with
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
    assert!(diff.newline_terminated());
    insta::assert_snapshot!(
        &diff
            .unified_diff()
            .context_radius(3)
            .header("old", "new")
            .to_string()
    );
}

#[test]
fn test_line_ops() {
    let a = "Hello World\nsome stuff here\nsome more stuff here\n";
    let b = "Hello World\nsome amazing stuff here\nsome more stuff here\n";
    let diff = TextDiff::from_lines(a, b);
    assert!(diff.newline_terminated());
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

#[cfg(test)]
fn build_owned_diff() -> TextDiff<'static, 'static, str> {
    TextDiff::from_lines("a\nb\n".to_string(), "a\nc\n".to_string())
}

#[test]
fn test_owned_inputs() {
    let diff = build_owned_diff();
    let changes = diff
        .iter_all_changes()
        .map(|x| (x.tag(), x.value().to_string_lossy().into_owned()))
        .collect::<Vec<_>>();
    assert_eq!(
        changes,
        vec![
            (ChangeTag::Equal, "a\n".into()),
            (ChangeTag::Delete, "b\n".into()),
            (ChangeTag::Insert, "c\n".into()),
        ]
    );
}

#[test]
#[should_panic(expected = "old range out of bounds")]
fn test_iter_changes_panics_on_invalid_ops() {
    let diff = TextDiff::from_lines("a\n", "a\n");
    let invalid = DiffOp::Delete {
        old_index: 99,
        old_len: 1,
        new_index: 0,
    };
    let _ = diff.iter_changes(&invalid).next();
}

#[test]
fn test_virtual_newlines() {
    let diff = TextDiff::from_lines("a\nb", "a\nc\n");
    assert!(diff.newline_terminated());
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

#[test]
fn test_lifetimes_on_iter() {
    let a = "1\n2\n3\n".to_string();
    let b = "1\n99\n3\n".to_string();
    let diff = TextDiff::from_lines(&a, &b);
    let changes = diff.iter_all_changes().collect::<Vec<_>>();
    insta::assert_debug_snapshot!(&changes);
}

#[test]
#[cfg(feature = "serde")]
fn test_serde() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    let changes = diff
        .ops()
        .iter()
        .flat_map(|op| diff.iter_changes(op))
        .collect::<Vec<_>>();
    let json = serde_json::to_string_pretty(&changes).unwrap();
    insta::assert_snapshot!(&json);
}

#[test]
#[cfg(feature = "serde")]
fn test_serde_ops() {
    let diff = TextDiff::from_lines(
        "Hello World\nsome stuff here\nsome more stuff here\n\nAha stuff here\nand more stuff",
        "Stuff\nHello World\nsome amazing stuff here\nsome more stuff here\n",
    );
    let changes = diff.ops();
    let json = serde_json::to_string_pretty(&changes).unwrap();
    insta::assert_snapshot!(&json);
}

#[test]
fn test_regression_issue_37() {
    let config = TextDiffConfig::default();
    let diff = config.diff_lines("\u{18}\n\n", "\n\n\r");
    let mut output = diff.unified_diff();
    assert_eq!(
        output.context_radius(0).to_string(),
        "@@ -1 +1,0 @@\n-\u{18}\n@@ -2,0 +2,2 @@\n+\n+\r"
    );
}

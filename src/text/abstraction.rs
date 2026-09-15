use alloc::borrow::{Borrow, Cow, ToOwned};
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use core::hash::Hash;
use core::ops::Range;

/// Reference to a [`DiffableStr`].
///
/// This type exists because while the library only really provides ways to
/// work with `&str` and `&[u8]` there are types that deref into those string
/// slices such as `String` and `Vec<u8>`.
///
/// This trait is used in the library whenever it's nice to be able to pass
/// strings of different types in.
///
/// Requires the `text` feature.
pub trait DiffableStrRef {
    /// The type of the resolved [`DiffableStr`].
    type Output: DiffableStr + ?Sized;

    /// Resolves the reference.
    fn as_diffable_str(&self) -> &Self::Output;
}

impl<T: DiffableStr + ?Sized> DiffableStrRef for T {
    type Output = T;

    fn as_diffable_str(&self) -> &T {
        self
    }
}

impl DiffableStrRef for String {
    type Output = str;

    fn as_diffable_str(&self) -> &str {
        self.as_str()
    }
}

impl<T: DiffableStr + ?Sized> DiffableStrRef for Cow<'_, T> {
    type Output = T;

    fn as_diffable_str(&self) -> &T {
        self
    }
}

/// Input type for text diffing.
///
/// This enum allows the text diffing APIs to accept both borrowed and owned
/// input values.
pub enum DiffInput<'a, T: DiffableStr + ?Sized> {
    /// A borrowed input value.
    Borrowed(&'a T),
    /// An owned input value.
    Owned(<T as ToOwned>::Owned),
}

impl<T: DiffableStr + ?Sized> DiffInput<'_, T> {
    /// Returns the input as [`DiffableStr`] reference.
    pub fn as_diffable_str(&self) -> &T {
        match self {
            DiffInput::Borrowed(value) => value,
            DiffInput::Owned(value) => value.borrow(),
        }
    }
}

/// Converts supported text values into [`DiffInput`].
///
/// This trait is implemented for both borrowed and owned input types used by
/// text diff APIs.
pub trait IntoDiffInput<'a> {
    /// The resolved [`DiffableStr`] output type.
    type Output: DiffableStr + ?Sized;

    /// Converts the input value into a [`DiffInput`].
    fn into_diff_input(self) -> DiffInput<'a, Self::Output>;
}

impl<'a, T: DiffableStrRef + ?Sized> IntoDiffInput<'a> for &'a T {
    type Output = T::Output;

    fn into_diff_input(self) -> DiffInput<'a, Self::Output> {
        DiffInput::Borrowed(self.as_diffable_str())
    }
}

impl<'a> IntoDiffInput<'a> for String {
    type Output = str;

    fn into_diff_input(self) -> DiffInput<'a, Self::Output> {
        DiffInput::Owned(self)
    }
}

impl<'a> IntoDiffInput<'a> for Cow<'a, str> {
    type Output = str;

    fn into_diff_input(self) -> DiffInput<'a, Self::Output> {
        match self {
            Cow::Borrowed(value) => DiffInput::Borrowed(value),
            Cow::Owned(value) => DiffInput::Owned(value),
        }
    }
}

/// All supported diffable strings.
///
/// The text module can work with different types of strings depending
/// on how the crate is compiled.  Out of the box `&str` is always supported
/// but with the `bytes` feature one can also work with `[u8]` slices for
/// as long as they are ASCII compatible.
///
/// Requires the `text` feature.
pub trait DiffableStr: Hash + PartialEq + PartialOrd + Ord + Eq + ToOwned {
    /// Splits the value into newlines with newlines attached.
    fn tokenize_lines(&self) -> Vec<&Self>;

    /// Splits the value into newlines with newlines separated.
    fn tokenize_lines_and_newlines(&self) -> Vec<&Self>;

    /// Tokenizes into words.
    fn tokenize_words(&self) -> Vec<&Self>;

    /// Tokenizes the input into characters.
    fn tokenize_chars(&self) -> Vec<&Self>;

    /// Tokenizes into unicode words.
    #[cfg(feature = "unicode")]
    fn tokenize_unicode_words(&self) -> Vec<&Self>;

    /// Tokenizes into unicode graphemes.
    #[cfg(feature = "unicode")]
    fn tokenize_graphemes(&self) -> Vec<&Self>;

    /// Decodes the string (potentially) lossy.
    fn as_str(&self) -> Option<&str>;

    /// Decodes the string (potentially) lossy.
    fn to_string_lossy(&self) -> Cow<'_, str>;

    /// Checks if the string ends in a newline.
    fn ends_with_newline(&self) -> bool;

    /// The length of the string.
    fn len(&self) -> usize;

    /// Slices the string.
    fn slice(&self, rng: Range<usize>) -> &Self;

    /// Returns the string as slice of raw bytes.
    fn as_bytes(&self) -> &[u8];

    /// Checks if the string is empty.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Upper bound for the number of tokens reserved up front by the tokenizers.
///
/// The estimates below are proportional to the input length, which is a good
/// guess for typical text but would reserve hundreds of megabytes for a huge
/// input that consists of a single line or word.  Beyond this many tokens the
/// vectors simply grow as needed.
const MAX_INITIAL_TOKEN_CAPACITY: usize = 16 * 1024;

/// Returns a mask with the high bit set in every byte of `word` that is zero.
///
/// Unlike the shorter `(x - LO) & !x & HI` trick this is exact for every
/// byte, not just the lowest zero byte, which matters because a single word
/// can contain several line terminators.
#[inline(always)]
fn zero_bytes(word: u64) -> u64 {
    const HI: u64 = 0x8080_8080_8080_8080;
    let t = ((word & !HI).wrapping_add(!HI)) | word;
    !t & HI
}

/// Collects the byte offsets of every `\r` and `\n` in `bytes`.
///
/// Line terminators are ASCII, so byte scanning never lands inside a
/// multi-byte UTF-8 sequence.  The scan is eight bytes at a time and, more
/// importantly, free of data dependent branches in the common case: it
/// unconditionally pushes the first candidate of a word and then truncates
/// the vector back when the word contained no terminator.  Line lengths are
/// effectively random, so a branch per word would mispredict roughly once
/// per line and dominate the tokenizer.
#[inline]
fn line_terminator_offsets(bytes: &[u8]) -> Vec<usize> {
    const LO: u64 = 0x0101_0101_0101_0101;
    const CR: u64 = LO * b'\r' as u64;
    const LF: u64 = LO * b'\n' as u64;

    // Real world text averages a few dozen bytes per line; a modest
    // estimate avoids most of the regrowth without over-allocating on
    // inputs with very long lines.  The constant keeps small inputs (which
    // are common for inline diffs) at a single allocation.
    let mut offsets = Vec::with_capacity((bytes.len() / 32 + 16).min(MAX_INITIAL_TOKEN_CAPACITY));
    let mut pos = 0usize;
    let mut chunks = bytes.chunks_exact(8);
    for chunk in &mut chunks {
        let word = u64::from_le_bytes(chunk.try_into().unwrap());
        let found = zero_bytes(word ^ CR) | zero_bytes(word ^ LF);
        let len = offsets.len();
        offsets.push(pos + (found.trailing_zeros() >> 3) as usize);
        offsets.truncate(len + (found != 0) as usize);
        // Further terminators in the same word are rare (blank lines and
        // CRLF pairs); handle them in a loop that is usually not entered.
        let mut rest = found & found.wrapping_sub(1);
        while rest != 0 {
            offsets.push(pos + (rest.trailing_zeros() >> 3) as usize);
            rest &= rest - 1;
        }
        pos += 8;
    }
    for &byte in chunks.remainder() {
        if byte == b'\n' || byte == b'\r' {
            offsets.push(pos);
        }
        pos += 1;
    }
    offsets
}

/// Splits the byte string into lines keeping the terminators attached.
///
/// `slice` converts a byte range into the token type.  `\r\n` is treated as a
/// single terminator.
#[inline]
pub(crate) fn tokenize_lines_by_bytes<'a, T: ?Sized>(
    bytes: &[u8],
    slice: impl Fn(Range<usize>) -> &'a T,
) -> Vec<&'a T> {
    let offsets = line_terminator_offsets(bytes);
    let mut lines = Vec::with_capacity(offsets.len() + 1);
    let mut last_pos = 0;
    let mut index = 0;
    while index < offsets.len() {
        let idx = offsets[index];
        index += 1;
        let end = if bytes[idx] == b'\r' && bytes.get(idx + 1) == Some(&b'\n') {
            // Skip the offset recorded for the `\n` of this pair.
            index += 1;
            idx + 2
        } else {
            idx + 1
        };
        lines.push(slice(last_pos..end));
        last_pos = end;
    }
    if last_pos < bytes.len() {
        lines.push(slice(last_pos..bytes.len()));
    }
    lines
}

impl DiffableStr for str {
    fn tokenize_lines(&self) -> Vec<&Self> {
        tokenize_lines_by_bytes(self.as_bytes(), |range| &self[range])
    }

    fn tokenize_lines_and_newlines(&self) -> Vec<&Self> {
        let mut rv = vec![];
        let mut iter = self.char_indices().peekable();

        while let Some((idx, c)) = iter.next() {
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
            rv.push(&self[start..end]);
        }

        rv
    }

    fn tokenize_words(&self) -> Vec<&Self> {
        let bytes = self.as_bytes();
        // Words and the whitespace runs between them are usually short; the
        // cap keeps a single long word from reserving four times its size.
        let mut rv = Vec::with_capacity((bytes.len() / 4 + 1).min(MAX_INITIAL_TOKEN_CAPACITY));
        let mut start = 0;
        let mut pos = 0;
        let mut current_is_whitespace = false;

        while pos < bytes.len() {
            let byte = bytes[pos];
            // ASCII fast path: `char::is_whitespace` is true for exactly
            // U+0009..=U+000D and U+0020 below U+0080.
            let (is_whitespace, width) = if byte < 0x80 {
                (matches!(byte, b'\t'..=b'\r' | b' '), 1)
            } else {
                let c = self[pos..].chars().next().unwrap();
                (c.is_whitespace(), c.len_utf8())
            };
            if is_whitespace != current_is_whitespace {
                if pos > start {
                    rv.push(&self[start..pos]);
                }
                start = pos;
                current_is_whitespace = is_whitespace;
            }
            pos += width;
        }

        if start < bytes.len() {
            rv.push(&self[start..]);
        }

        rv
    }

    fn tokenize_chars(&self) -> Vec<&Self> {
        self.char_indices()
            .map(move |(i, c)| &self[i..i + c.len_utf8()])
            .collect()
    }

    #[cfg(feature = "unicode")]
    fn tokenize_unicode_words(&self) -> Vec<&Self> {
        unicode_segmentation::UnicodeSegmentation::split_word_bounds(self).collect()
    }

    #[cfg(feature = "unicode")]
    fn tokenize_graphemes(&self) -> Vec<&Self> {
        unicode_segmentation::UnicodeSegmentation::graphemes(self, true).collect()
    }

    fn as_str(&self) -> Option<&str> {
        Some(self)
    }

    fn to_string_lossy(&self) -> Cow<'_, str> {
        Cow::Borrowed(self)
    }

    fn ends_with_newline(&self) -> bool {
        self.ends_with(&['\r', '\n'][..])
    }

    fn len(&self) -> usize {
        str::len(self)
    }

    fn slice(&self, rng: Range<usize>) -> &Self {
        &self[rng]
    }

    fn as_bytes(&self) -> &[u8] {
        str::as_bytes(self)
    }
}

#[cfg(feature = "bytes")]
mod bytes_support {
    use super::*;

    use alloc::vec;
    use bstr::ByteSlice;

    impl DiffableStrRef for Vec<u8> {
        type Output = [u8];

        fn as_diffable_str(&self) -> &[u8] {
            self.as_slice()
        }
    }

    impl<'a> IntoDiffInput<'a> for Vec<u8> {
        type Output = [u8];

        fn into_diff_input(self) -> DiffInput<'a, Self::Output> {
            DiffInput::Owned(self)
        }
    }

    impl<'a> IntoDiffInput<'a> for Cow<'a, [u8]> {
        type Output = [u8];

        fn into_diff_input(self) -> DiffInput<'a, Self::Output> {
            match self {
                Cow::Borrowed(value) => DiffInput::Borrowed(value),
                Cow::Owned(value) => DiffInput::Owned(value),
            }
        }
    }

    /// Allows viewing ASCII compatible byte slices as strings.
    ///
    /// Requires the `bytes` feature.
    impl DiffableStr for [u8] {
        fn tokenize_lines(&self) -> Vec<&Self> {
            // Line terminators are ASCII bytes which lossy UTF-8 decoding
            // never merges into a replacement character, so scanning bytes
            // matches the char based behavior exactly.
            tokenize_lines_by_bytes(self, |range| &self[range])
        }

        fn tokenize_lines_and_newlines(&self) -> Vec<&Self> {
            let mut rv = vec![];
            let mut iter = self.char_indices().peekable();

            while let Some((start, mut end, c)) = iter.next() {
                let is_newline = c == '\r' || c == '\n';
                while let Some(&(_, new_end, next_char)) = iter.peek() {
                    if (next_char == '\r' || next_char == '\n') != is_newline {
                        break;
                    }
                    iter.next();
                    end = new_end;
                }
                rv.push(&self[start..end]);
            }

            rv
        }

        fn tokenize_words(&self) -> Vec<&Self> {
            let mut iter = self.char_indices().peekable();
            let mut rv = vec![];

            while let Some((start, mut end, c)) = iter.next() {
                let is_whitespace = c.is_whitespace();
                while let Some(&(_, new_end, next_char)) = iter.peek() {
                    if next_char.is_whitespace() != is_whitespace {
                        break;
                    }
                    iter.next();
                    end = new_end;
                }
                rv.push(&self[start..end]);
            }

            rv
        }

        #[cfg(feature = "unicode")]
        fn tokenize_unicode_words(&self) -> Vec<&Self> {
            self.words_with_breaks().map(|x| x.as_bytes()).collect()
        }

        #[cfg(feature = "unicode")]
        fn tokenize_graphemes(&self) -> Vec<&Self> {
            self.graphemes().map(|x| x.as_bytes()).collect()
        }

        fn tokenize_chars(&self) -> Vec<&Self> {
            self.char_indices()
                .map(move |(start, end, _)| &self[start..end])
                .collect()
        }

        fn as_str(&self) -> Option<&str> {
            core::str::from_utf8(self).ok()
        }

        fn to_string_lossy(&self) -> Cow<'_, str> {
            String::from_utf8_lossy(self)
        }

        fn ends_with_newline(&self) -> bool {
            matches!(self.last_byte(), Some(b'\r') | Some(b'\n'))
        }

        fn len(&self) -> usize {
            <[u8]>::len(self)
        }

        fn slice(&self, rng: Range<usize>) -> &Self {
            &self[rng]
        }

        fn as_bytes(&self) -> &[u8] {
            self
        }
    }
}

#[test]
fn test_split_lines() {
    assert_eq!(
        DiffableStr::tokenize_lines("first\nsecond\rthird\r\nfourth\nlast"),
        vec!["first\n", "second\r", "third\r\n", "fourth\n", "last"]
    );
    assert_eq!(DiffableStr::tokenize_lines("\n\n"), vec!["\n", "\n"]);
    assert_eq!(DiffableStr::tokenize_lines("\n"), vec!["\n"]);
    assert!(DiffableStr::tokenize_lines("").is_empty());
}

#[test]
fn test_split_lines_word_boundaries() {
    // Reference implementation: the original char based tokenizer.
    fn reference(s: &str) -> Vec<&str> {
        let mut iter = s.char_indices().peekable();
        let mut last_pos = 0;
        let mut lines = vec![];
        while let Some((idx, c)) = iter.next() {
            if c == '\r' {
                if iter.peek().is_some_and(|x| x.1 == '\n') {
                    lines.push(&s[last_pos..=idx + 1]);
                    iter.next();
                    last_pos = idx + 2;
                } else {
                    lines.push(&s[last_pos..=idx]);
                    last_pos = idx + 1;
                }
            } else if c == '\n' {
                lines.push(&s[last_pos..=idx]);
                last_pos = idx + 1;
            }
        }
        if last_pos < s.len() {
            lines.push(&s[last_pos..]);
        }
        lines
    }

    // Exercise terminators at every offset relative to the eight byte scan
    // words, including CRLF pairs that straddle a word boundary, several
    // terminators inside one word, and multi-byte characters.
    let pieces = ["a", "\n", "\r", "\r\n", "ö", "xyz", "\n\n", "\r\r\n", "❄️"];
    let mut input = String::new();
    for round in 0..64 {
        for (index, piece) in pieces.iter().enumerate() {
            if (round + index) % 3 != 0 {
                input.push_str(piece);
            }
            for prefix_len in [0, 1, 7, 8, 9, 15, 16, 17] {
                let padded = format!("{}{}", "p".repeat(prefix_len), input);
                assert_eq!(
                    DiffableStr::tokenize_lines(padded.as_str()),
                    reference(&padded)
                );
            }
        }
    }

    let long_line = "x".repeat(1000);
    let input = format!("{long_line}\r\n{long_line}\r{long_line}\n{long_line}");
    assert_eq!(
        DiffableStr::tokenize_lines(input.as_str()),
        reference(&input)
    );
    let input = "\r\n".repeat(50);
    assert_eq!(
        DiffableStr::tokenize_lines(input.as_str()),
        reference(&input)
    );
    let input = "\r".repeat(17);
    assert_eq!(
        DiffableStr::tokenize_lines(input.as_str()),
        reference(&input)
    );
}

#[test]
fn test_long_token_capacity() {
    let input = "x".repeat(1024 * 1024);
    let words = input.tokenize_words();
    assert_eq!(words, [input.as_str()]);
    assert!(words.capacity() <= 16 * 1024);
    let offsets = line_terminator_offsets(input.as_bytes());
    assert!(offsets.is_empty());
    assert!(offsets.capacity() <= 16 * 1024);
}

#[test]
fn test_tokenizer_capacity_is_bounded() {
    // A large input that produces a single token must not reserve a token
    // vector proportional to its byte length.
    let input = "x".repeat(4 * 1024 * 1024);
    let words = DiffableStr::tokenize_words(input.as_str());
    assert_eq!(words.len(), 1);
    assert!(words.capacity() <= MAX_INITIAL_TOKEN_CAPACITY);
    let lines = DiffableStr::tokenize_lines(input.as_str());
    assert_eq!(lines.len(), 1);
    assert!(lines.capacity() <= MAX_INITIAL_TOKEN_CAPACITY);
}

#[test]
fn test_split_words() {
    assert_eq!(
        DiffableStr::tokenize_words("foo    bar baz\n\n  aha"),
        ["foo", "    ", "bar", " ", "baz", "\n\n  ", "aha"]
    );
}

#[test]
fn test_split_chars() {
    assert_eq!(
        DiffableStr::tokenize_chars("abcfö❄️"),
        vec!["a", "b", "c", "f", "ö", "❄", "\u{fe0f}"]
    );
}

#[test]
#[cfg(feature = "unicode")]
fn test_split_graphemes() {
    assert_eq!(
        DiffableStr::tokenize_graphemes("abcfö❄️"),
        vec!["a", "b", "c", "f", "ö", "❄️"]
    );
}

#[test]
#[cfg(feature = "bytes")]
fn test_split_lines_bytes() {
    assert_eq!(
        DiffableStr::tokenize_lines("first\nsecond\rthird\r\nfourth\nlast".as_bytes()),
        vec![
            "first\n".as_bytes(),
            "second\r".as_bytes(),
            "third\r\n".as_bytes(),
            "fourth\n".as_bytes(),
            "last".as_bytes()
        ]
    );
    assert_eq!(
        DiffableStr::tokenize_lines("\n\n".as_bytes()),
        vec!["\n".as_bytes(), "\n".as_bytes()]
    );
    assert_eq!(
        DiffableStr::tokenize_lines("\n".as_bytes()),
        vec!["\n".as_bytes()]
    );
    assert!(DiffableStr::tokenize_lines("".as_bytes()).is_empty());
}

#[test]
#[cfg(feature = "bytes")]
fn test_split_words_bytes() {
    assert_eq!(
        DiffableStr::tokenize_words("foo    bar baz\n\n  aha".as_bytes()),
        [
            &b"foo"[..],
            &b"    "[..],
            &b"bar"[..],
            &b" "[..],
            &b"baz"[..],
            &b"\n\n  "[..],
            &b"aha"[..]
        ]
    );
}

#[test]
#[cfg(feature = "bytes")]
fn test_split_chars_bytes() {
    assert_eq!(
        DiffableStr::tokenize_chars("abcfö❄️".as_bytes()),
        vec![
            &b"a"[..],
            &b"b"[..],
            &b"c"[..],
            &b"f"[..],
            "ö".as_bytes(),
            "❄".as_bytes(),
            "\u{fe0f}".as_bytes()
        ]
    );
}

#[test]
#[cfg(all(feature = "bytes", feature = "unicode"))]
fn test_split_graphemes_bytes() {
    assert_eq!(
        DiffableStr::tokenize_graphemes("abcfö❄️".as_bytes()),
        vec![
            &b"a"[..],
            &b"b"[..],
            &b"c"[..],
            &b"f"[..],
            "ö".as_bytes(),
            "❄️".as_bytes()
        ]
    );
}

//! Text diffing utilities.
//!
//! This provides helpful utilities for text (and more specifically line) diff
//! operations.
use std::borrow::Cow;
use std::collections::VecDeque;
use std::convert::Infallible;
use std::fmt;

use crate::algorithms::{diff_slices, Algorithm, DiffHook, Replace};

/// A text diff operation.
#[derive(Debug, Clone)]
pub enum DiffOp<'old, 'new> {
    /// Old and new text are matching
    Equal {
        old_index: usize,
        new_index: usize,
        value: &'old str,
    },
    /// New text was inserted
    Insert { new_index: usize, value: &'new str },
    /// Old text was deleted
    Delete { old_index: usize, value: &'old str },
}

impl<'old, 'new> fmt::Display for DiffOp<'old, 'new> {
    /// Stringifies a diff op.
    ///
    /// This is mostly for debugging.  It prepends a diff marker to the beginning
    /// of the value (`+`, `-` or a space) and trims of trailing spaces.  If the
    /// space trimming is not wanted the alternative rendering mode disables that.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            match *self {
                DiffOp::Equal { .. } => " ",
                DiffOp::Insert { .. } => "+",
                DiffOp::Delete { .. } => "-",
            },
            if f.alternate() {
                self.as_str()
            } else {
                self.as_str().trim_end()
            }
        )
    }
}

impl<'old, 'new> DiffOp<'old, 'new> {
    /// Returns the old and new index based on availability.
    pub fn indices(&self) -> (Option<usize>, Option<usize>) {
        match *self {
            DiffOp::Equal {
                old_index,
                new_index,
                ..
            } => (Some(old_index), Some(new_index)),
            DiffOp::Insert { new_index, .. } => (None, Some(new_index)),
            DiffOp::Delete { old_index, .. } => (Some(old_index), None),
        }
    }

    /// Extracts the embedded string value.
    pub fn as_str(&self) -> &str {
        match *self {
            DiffOp::Equal { value, .. } => value,
            DiffOp::Insert { value, .. } => value,
            DiffOp::Delete { value, .. } => value,
        }
    }
}

struct ContextState<'old, 'new> {
    radius: usize,
    ops: VecDeque<DiffOp<'old, 'new>>,
    after_modification: bool,
}

/// Captures diff op codes for textual diffs
pub struct TextDiffer<'old, 'new, 'bufs> {
    old: Cow<'bufs, [&'old str]>,
    new: Cow<'bufs, [&'new str]>,
    ops: Vec<DiffOp<'old, 'new>>,
    algorithm: Algorithm,
    use_replace: bool,
    context_state: Option<ContextState<'old, 'new>>,
}

impl<'old, 'new, 'bufs> TextDiffer<'old, 'new, 'bufs> {
    /// Creates a new line based text differ from two strings.
    pub fn new_from_lines(old: &'old str, new: &'new str) -> TextDiffer<'old, 'new, 'bufs> {
        TextDiffer {
            old: Cow::Owned(split_lines(old).collect()),
            new: Cow::Owned(split_lines(new).collect()),
            ops: Vec::new(),
            algorithm: Algorithm::default(),
            use_replace: true,
            context_state: None,
        }
    }

    /// Creates a new word based text differ from two strings.
    pub fn new_from_words(old: &'old str, new: &'new str) -> TextDiffer<'old, 'new, 'bufs> {
        TextDiffer {
            old: Cow::Owned(split_words(old).collect()),
            new: Cow::Owned(split_words(new).collect()),
            ops: Vec::new(),
            algorithm: Algorithm::default(),
            use_replace: true,
            context_state: None,
        }
    }

    /// Creates a new text differ from two slices.
    pub fn new_from_slices(
        old: &'bufs [&'old str],
        new: &'bufs [&'new str],
    ) -> TextDiffer<'old, 'new, 'bufs> {
        TextDiffer {
            old: Cow::Borrowed(old),
            new: Cow::Borrowed(new),
            ops: Vec::new(),
            algorithm: Algorithm::default(),
            use_replace: true,
            context_state: None,
        }
    }

    /// Sets a context radius.
    ///
    /// By default no radius is set.  If a radius is set then `n` number of
    /// equal chunks around modifications are retained, others are discarded.
    pub fn set_context_radius(&mut self, n: Option<usize>) {
        self.context_state = n.map(|radius| ContextState {
            radius,
            ops: VecDeque::new(),
            after_modification: false,
        });
    }

    /// Sets a different diffing algorithm.
    ///
    /// If not explicitly configured the default algorithm is
    /// [`Algorithm::default`].
    pub fn set_algorithm(&mut self, alg: Algorithm) {
        self.algorithm = alg;
    }

    /// Controls if the [`Replace`] hook should be used automatically.
    ///
    /// By default the replacer is always used.
    pub fn set_use_replace(&mut self, yes: bool) {
        self.use_replace = yes;
    }

    /// Diffs the text with the given algorithm returning the ops.
    ///
    /// This is a shortcut for running a diff operation with the text differ
    /// as hook and converting it into the captured operaitons.
    pub fn diff(mut self) -> Vec<DiffOp<'old, 'new>> {
        // this requires some explanation: because the text differ can
        // hold owned buffers (from `TextDiffer::from_lines`) we cannot directly
        // use it can't fulfill the lifetime requirements.  For the way this
        // diff function works though we can get around this by making a new
        // differ that borrows the stack local buffers as nothing bound to
        // `bufs needs to outlive this stack frame.
        //
        // TODO: consider changing `TextDiffer` into a builder instead.
        let (old, new) = (&self.old[..], &self.new[..]);
        let mut d = TextDiffer {
            old: Cow::Borrowed(old),
            new: Cow::Borrowed(new),
            ops: self.ops,
            algorithm: self.algorithm,
            use_replace: self.use_replace,
            context_state: self.context_state.take(),
        };
        if d.use_replace {
            let mut d = Replace::new(d);
            diff_slices(self.algorithm, &mut d, old, new).unwrap();
            d.into_inner().into_ops()
        } else {
            diff_slices(self.algorithm, &mut d, old, new).unwrap();
            d.into_ops()
        }
    }

    /// Returns the captured ops.
    pub fn into_ops(self) -> Vec<DiffOp<'old, 'new>> {
        self.ops
    }

    fn push_op(&mut self, op: DiffOp<'old, 'new>) {
        match self.context_state {
            None => self.ops.push(op),
            Some(ref mut context_state) => {
                if let DiffOp::Equal { .. } = op {
                    if context_state.ops.len() >= context_state.radius {
                        if context_state.after_modification {
                            context_state.after_modification = false;
                            self.ops.extend(context_state.ops.drain(..));
                        }
                        context_state.ops.pop_front();
                    }
                    context_state.ops.push_back(op);
                } else {
                    context_state.after_modification = true;
                    self.ops.extend(context_state.ops.drain(..));
                    self.ops.push(op);
                }
            }
        }
    }
}

impl<'old, 'new, 'bufs> DiffHook for TextDiffer<'old, 'new, 'bufs> {
    type Error = Infallible;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        for off in 0..len {
            self.push_op(DiffOp::Equal {
                old_index: old_index + off,
                new_index: new_index + off,
                value: self.old[old_index + off],
            });
        }
        Ok(())
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        _new_index: usize,
    ) -> Result<(), Self::Error> {
        for off in 0..old_len {
            self.push_op(DiffOp::Delete {
                old_index: old_index + off,
                value: self.old[old_index + off],
            });
        }
        Ok(())
    }

    fn insert(
        &mut self,
        _old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        for off in 0..new_len {
            self.push_op(DiffOp::Insert {
                new_index: new_index + off,
                value: self.new[new_index + off],
            });
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        if let Some(context_state) = self.context_state.take() {
            self.ops.extend(context_state.ops);
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

#[test]
fn test_line_diff() {
    let differ = TextDiffer::new_from_lines("foo\nbar\nbaz", "foo\nblah\nbaz");
    insta::assert_debug_snapshot!(differ.diff(), @r###"
    [
        Equal {
            old_index: 0,
            new_index: 0,
            value: "foo\n",
        },
        Delete {
            old_index: 1,
            value: "bar\n",
        },
        Insert {
            new_index: 1,
            value: "blah\n",
        },
        Equal {
            old_index: 2,
            new_index: 2,
            value: "baz",
        },
    ]
    "###);
}

#[test]
fn test_context_diff() {
    let old_text = vec![
        "1\n", "2\n", "3\n", "4\n", "5\n", "6\n", "7\n", "8\n", "9\n", "0\n", "1\n", "2\n", "3\n",
        "4\n", "5\n", "6\n", "7\n", "8\n", "9\n", "0\n",
    ];
    let mut new_text = old_text.clone();
    new_text[9] = "a\n";
    new_text[11] = "b\n";
    new_text[19] = "c\n";
    let mut differ = TextDiffer::new_from_slices(&old_text, &new_text);
    differ.set_context_radius(Some(2));
    insta::assert_debug_snapshot!(differ.diff(), @r###"
    [
        Equal {
            old_index: 7,
            new_index: 7,
            value: "8\n",
        },
        Equal {
            old_index: 8,
            new_index: 8,
            value: "9\n",
        },
        Delete {
            old_index: 9,
            value: "0\n",
        },
        Insert {
            new_index: 9,
            value: "a\n",
        },
        Equal {
            old_index: 10,
            new_index: 10,
            value: "1\n",
        },
        Delete {
            old_index: 11,
            value: "2\n",
        },
        Insert {
            new_index: 11,
            value: "b\n",
        },
        Equal {
            old_index: 12,
            new_index: 12,
            value: "3\n",
        },
        Equal {
            old_index: 13,
            new_index: 13,
            value: "4\n",
        },
        Equal {
            old_index: 17,
            new_index: 17,
            value: "8\n",
        },
        Equal {
            old_index: 18,
            new_index: 18,
            value: "9\n",
        },
        Delete {
            old_index: 19,
            value: "0\n",
        },
        Insert {
            new_index: 19,
            value: "c\n",
        },
    ]
    "###);
}

#[test]
fn test_display() {
    let old_text = vec![
        "1\n", "2\n", "3\n", "4\n", "5\n", "6\n", "7\n", "8\n", "9\n",
    ];
    let mut new_text = old_text.clone();
    new_text[3] = "a\n";
    new_text[7] = "b\n";
    let mut differ = TextDiffer::new_from_slices(&old_text, &new_text);
    differ.set_context_radius(Some(2));
    let ops = differ
        .diff()
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>();
    insta::assert_debug_snapshot!(ops, @r###"
    [
        " 2",
        " 3",
        "-4",
        "+a",
        " 5",
        " 6",
        " 7",
        "-8",
        "+b",
        " 9",
    ]
    "###);
}

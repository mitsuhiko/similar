//! This module provides unified diff functionality.
//!
//! This module is available for as long as the `text` feature is enabled which
//! is enabled by default.
//!
//! ```rust
//! use similar::text::TextDiff;
//! # let old_text = "";
//! # let new_text = "";
//! let text_diff = TextDiff::from_lines(old_text, new_text);
//! print!("{}", text_diff
//!     .unified_diff()
//!     .context_radius(10)
//!     .header("old_file", "new_file"));
//! ```
//!
//! # Unicode vs Bytes
//!
//! The [`UnifiedDiff`] type supports both unicode and byte diffs for all
//! types compatible with [`DiffableStr`].  You can pick between the two
//! versions by using [`UnifiedDiff.to_string`] or [`UnifiedDiff.to_writer`].
//! The former uses [`DiffableStr::as_str_lossy`], the latter uses
//! [`DiffableStr::as_bytes`] for each line.

use std::ops::Range;
use std::{fmt, io};

use crate::algorithms::{Algorithm, DiffOp};
use crate::text::{Change, ChangeTag, TextDiff};

use super::DiffableStr;

#[derive(Copy, Clone, Debug)]
struct UnifiedDiffHunkRange(usize, usize);

impl UnifiedDiffHunkRange {
    fn new(range: Range<usize>) -> UnifiedDiffHunkRange {
        UnifiedDiffHunkRange(range.start, range.end)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn end(&self) -> usize {
        self.1
    }
}

impl fmt::Display for UnifiedDiffHunkRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut beginning = self.start();
        let len = self.end() - self.start();
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

/// Unified diff hunk header formatter.
pub struct UnifiedHunkHeader {
    old_range: UnifiedDiffHunkRange,
    new_range: UnifiedDiffHunkRange,
}

impl UnifiedHunkHeader {
    /// Creates a hunk header from a (non empty) slice of diff ops.
    pub fn new(ops: &[DiffOp]) -> UnifiedHunkHeader {
        UnifiedHunkHeader {
            old_range: UnifiedDiffHunkRange::new(ops[0].old_range()),
            new_range: UnifiedDiffHunkRange::new(ops[ops.len() - 1].new_range()),
        }
    }
}

impl fmt::Display for UnifiedHunkHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@@ -{} +{} @@", &self.old_range, &self.new_range)
    }
}

/// Unified diff formatter.
///
/// ```rust
/// use similar::text::TextDiff;
/// # let old_text = "";
/// # let new_text = "";
/// let text_diff = TextDiff::from_lines(old_text, new_text);
/// print!("{}", text_diff
///     .unified_diff()
///     .context_radius(10)
///     .header("old_file", "new_file"));
/// ```
///
/// ## Unicode vs Bytes
///
/// The [`UnifiedDiff`] type supports both unicode and byte diffs for all
/// types compatible with [`DiffableStr`].  You can pick between the two
/// versions by using [`UnifiedDiff.to_string`] or [`UnifiedDiff.to_writer`].
/// The former uses [`DiffableStr::as_str_lossy`], the latter uses
/// [`DiffableStr::as_bytes`] for each line.
pub struct UnifiedDiff<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized> {
    diff: &'diff TextDiff<'old, 'new, 'bufs, T>,
    context_radius: usize,
    missing_newline_hint: bool,
    header: Option<(String, String)>,
}

impl<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized> UnifiedDiff<'diff, 'old, 'new, 'bufs, T> {
    /// Creates a formatter from a text diff object.
    pub fn from_text_diff(diff: &'diff TextDiff<'old, 'new, 'bufs, T>) -> Self {
        UnifiedDiff {
            diff,
            context_radius: 3,
            missing_newline_hint: true,
            header: None,
        }
    }

    /// Changes the context radius.
    ///
    /// The context radius is the number of lines between changes that should
    /// be emitted.  This defaults to `3`.
    pub fn context_radius(&mut self, n: usize) -> &mut Self {
        self.context_radius = n;
        self
    }

    /// Sets a header to the diff.
    ///
    /// `a` and `b` are the file names that are added to the top of the unified
    /// file format.  The names are accepted verbaitim which lets you encode
    /// a timestamp into it when separated by a tab (`\t`).  For more information
    /// see [the unified diff format specification](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/diff.html#tag_20_34_10_07)
    pub fn header(&mut self, a: &str, b: &str) -> &mut Self {
        self.header = Some((a.to_string(), b.to_string()));
        self
    }

    /// Controls the missing newline hint.
    ///
    /// By default a special `\ No newline at end of file` marker is added to
    /// the output when a file is not terminated with a final newline.  This can
    /// be disabled with this flag.
    pub fn missing_newline_hint(&mut self, yes: bool) -> &mut Self {
        self.missing_newline_hint = yes;
        self
    }

    /// Iterates over all hunks as configured.
    pub fn iter_hunks(&self) -> impl Iterator<Item = UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, T>> {
        let diff = self.diff;
        let missing_newline_hint = self.missing_newline_hint;
        self.diff
            .grouped_ops(self.context_radius)
            .into_iter()
            .filter(|ops| !ops.is_empty())
            .map(move |ops| UnifiedDiffHunk::new(ops, diff, missing_newline_hint))
    }

    /// Write the unified diff as bytes to the output stream.
    pub fn to_writer<W: io::Write>(&self, mut w: W) -> Result<(), io::Error> {
        let mut header = self.header.as_ref();
        for hunk in self.iter_hunks() {
            if let Some((old_file, new_file)) = header.take() {
                writeln!(w, "--- {}", old_file)?;
                writeln!(w, "+++ {}", new_file)?;
            }
            write!(w, "{}", hunk)?;
        }
        Ok(())
    }

    fn header_opt(&mut self, header: Option<(&str, &str)>) -> &mut Self {
        if let Some((a, b)) = header {
            self.header(a, b);
        }
        self
    }
}

/// Unified diff hunk formatter.
///
/// The `Display` this renders out a single unified diff's hunk.
pub struct UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized> {
    diff: &'diff TextDiff<'old, 'new, 'bufs, T>,
    ops: Vec<DiffOp>,
    missing_newline_hint: bool,
}

impl<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized>
    UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, T>
{
    /// Creates a new hunk for some operations.
    pub fn new(
        ops: Vec<DiffOp>,
        diff: &'diff TextDiff<'old, 'new, 'bufs, T>,
        missing_newline_hint: bool,
    ) -> UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, T> {
        UnifiedDiffHunk {
            diff,
            ops,
            missing_newline_hint,
        }
    }

    /// Returns the header for the hunk.
    pub fn header(&self) -> UnifiedHunkHeader {
        UnifiedHunkHeader::new(&self.ops)
    }

    /// Returns all operations in the hunk.
    pub fn ops(&self) -> &[DiffOp] {
        &self.ops
    }

    /// Returns the value of the `missing_newline_hint` flag.
    pub fn missing_newline_hint(&self) -> bool {
        self.missing_newline_hint
    }

    /// Iterates over all changes in a hunk.
    pub fn iter_changes(&self) -> impl Iterator<Item = Change<'_, T>> + '_ {
        // unclear why this needs Box::new here.  It seems to infer some really
        // odd lifetimes I can't figure out how to work with.
        (Box::new(
            self.ops()
                .iter()
                .flat_map(move |op| self.diff.iter_changes(op)),
        )) as Box<dyn Iterator<Item = _>>
    }

    /// Write the hunk as bytes to the output stream.
    pub fn to_writer<W: io::Write>(&self, mut w: W) -> Result<(), io::Error> {
        let mut wrote_header = false;
        for change in self.iter_changes() {
            if !wrote_header {
                writeln!(w, "{}", self.header())?;
                wrote_header = true;
            }
            write!(
                w,
                "{}",
                match change.tag() {
                    ChangeTag::Equal => ' ',
                    ChangeTag::Delete => '-',
                    ChangeTag::Insert => '+',
                },
            )?;
            w.write_all(change.value().as_bytes())?;
            if self.diff.newline_terminated() {
                write!(w, "\n")?;
            }
            if change.missing_newline() {
                if self.missing_newline_hint {
                    writeln!(w, "\n\\ No newline at end of file")?;
                } else {
                    writeln!(w)?;
                }
            }
        }
        Ok(())
    }
}

impl<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized> fmt::Display
    for UnifiedDiffHunk<'diff, 'old, 'new, 'bufs, T>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let nl = if self.diff.newline_terminated() {
            ""
        } else {
            "\n"
        };
        let mut wrote_header = false;
        for change in self.iter_changes() {
            if !wrote_header {
                writeln!(f, "{}", self.header())?;
                wrote_header = true;
            }
            write!(
                f,
                "{}{}{}",
                match change.tag() {
                    ChangeTag::Equal => ' ',
                    ChangeTag::Delete => '-',
                    ChangeTag::Insert => '+',
                },
                change.as_str_lossy(),
                nl
            )?;
            if change.missing_newline() {
                if self.missing_newline_hint {
                    writeln!(f, "\n\\ No newline at end of file")?;
                } else {
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

impl<'diff, 'old, 'new, 'bufs, T: DiffableStr + ?Sized> fmt::Display
    for UnifiedDiff<'diff, 'old, 'new, 'bufs, T>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut header = self.header.as_ref();
        for hunk in self.iter_hunks() {
            if let Some((old_file, new_file)) = header.take() {
                writeln!(f, "--- {}", old_file)?;
                writeln!(f, "+++ {}", new_file)?;
            }
            write!(f, "{}", hunk)?;
        }
        Ok(())
    }
}

/// Quick way to get a unified diff as string.
///
/// `n` configures [`UnifiedDiff::context_radius`] and
/// `header` configures [`UnifiedDiff::header`] when not `None`.
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
        .unified_diff()
        .context_radius(n)
        .header_opt(header)
        .to_string()
}

#[test]
fn test_unified_diff() {
    let diff = TextDiff::from_lines(
        "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\nl\nm\nn\no\np\nq\nr\ns\nt\nu\nv\nw\nx\ny\nz\nA\nB\nC\nD\nE\nF\nG\nH\nI\nJ\nK\nL\nM\nN\nO\nP\nQ\nR\nS\nT\nU\nV\nW\nX\nY\nZ",
        "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\nl\nm\nn\no\np\nq\nr\nS\nt\nu\nv\nw\nx\ny\nz\nA\nB\nC\nD\nE\nF\nG\nH\nI\nJ\nK\nL\nM\nN\no\nP\nQ\nR\nS\nT\nU\nV\nW\nX\nY\nZ",
    );
    insta::assert_snapshot!(&diff.unified_diff().header("a.txt", "b.txt").to_string());
}
#[test]
fn test_empty_unified_diff() {
    let diff = TextDiff::from_lines("abc", "abc");
    assert_eq!(diff.unified_diff().header("a.txt", "b.txt").to_string(), "");
}

#[test]
fn test_unified_diff_newline_hint() {
    let diff = TextDiff::from_lines("a\n", "b");
    insta::assert_snapshot!(&diff.unified_diff().header("a.txt", "b.txt").to_string());
    insta::assert_snapshot!(&diff
        .unified_diff()
        .missing_newline_hint(false)
        .header("a.txt", "b.txt")
        .to_string());
}

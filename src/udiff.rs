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
#![cfg(feature = "text")]

use std::fmt;
use std::ops::Range;

use crate::algorithms::{Algorithm, DiffOp};
use crate::text::{Change, ChangeTag, TextDiff};

/// Represents a range of a unified diff hunk.
#[derive(Copy, Clone, Debug)]
struct HunkRange(usize, usize);

impl HunkRange {
    fn new(range: Range<usize>) -> HunkRange {
        HunkRange(range.start, range.end)
    }

    fn start(&self) -> usize {
        self.0
    }

    fn end(&self) -> usize {
        self.1
    }
}

impl fmt::Display for HunkRange {
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

/// Formats a unified diff hunk header for a group of diff operations.
pub struct HunkHeader {
    old_range: HunkRange,
    new_range: HunkRange,
}

impl HunkHeader {
    /// Creates a hunk header from a (non empty) slice of diff ops.
    pub fn new(ops: &[DiffOp]) -> HunkHeader {
        HunkHeader {
            old_range: HunkRange::new(ops[0].old_range()),
            new_range: HunkRange::new(ops[ops.len() - 1].new_range()),
        }
    }
}

impl fmt::Display for HunkHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@@ -{} +{} @@", &self.old_range, &self.new_range)
    }
}

/// A unified diff formatter.
///
/// The `Display` implementation renders a unified diff.
pub struct UnifiedDiff<'diff, 'old, 'new, 'bufs> {
    diff: &'diff TextDiff<'old, 'new, 'bufs>,
    context_radius: usize,
    header: Option<(String, String)>,
}

impl<'diff, 'old, 'new, 'bufs> UnifiedDiff<'diff, 'old, 'new, 'bufs> {
    /// Creates a formatter from a text diff object.
    pub fn from_text_diff(diff: &'diff TextDiff<'old, 'new, 'bufs>) -> Self {
        UnifiedDiff {
            diff,
            context_radius: 5,
            header: None,
        }
    }

    /// Changes the context radius.  Defaults to `5`.
    pub fn context_radius(&mut self, n: usize) -> &mut Self {
        self.context_radius = n;
        self
    }

    /// Sets a header to the diff.
    pub fn header(&mut self, a: &str, b: &str) -> &mut Self {
        self.header = Some((a.to_string(), b.to_string()));
        self
    }

    /// Iterates over all hunks as configured.
    pub fn iter_hunks(&self) -> impl Iterator<Item = Hunk<'diff, 'old, 'new, 'bufs>> {
        let diff = self.diff;
        self.diff
            .grouped_ops(self.context_radius)
            .into_iter()
            .filter(|ops| !ops.is_empty())
            .map(move |ops| Hunk::new(ops, diff))
    }

    fn header_opt(&mut self, header: Option<(&str, &str)>) -> &mut Self {
        if let Some((a, b)) = header {
            self.header(a, b);
        }
        self
    }
}

/// Represents a single hunk in a unified diff.
///
/// When formatted with `Display` this renders out a single unified diff's
/// hunk.
pub struct Hunk<'diff, 'old, 'new, 'bufs> {
    diff: &'diff TextDiff<'old, 'new, 'bufs>,
    ops: Vec<DiffOp>,
}

impl<'diff, 'old, 'new, 'bufs> Hunk<'diff, 'old, 'new, 'bufs> {
    /// Creates a new hunk for some operations.
    pub fn new(
        ops: Vec<DiffOp>,
        diff: &'diff TextDiff<'old, 'new, 'bufs>,
    ) -> Hunk<'diff, 'old, 'new, 'bufs> {
        Hunk { diff, ops }
    }

    /// Returns the header for the hunk.
    pub fn header(&self) -> HunkHeader {
        HunkHeader::new(&self.ops)
    }

    /// Returns all operations in the hunk.
    pub fn ops(&self) -> &[DiffOp] {
        &self.ops
    }

    /// Iterates over all changes in a hunk.
    pub fn iter_changes(&self) -> impl Iterator<Item = Change<'_>> {
        // unclear why this needs Box::new here.  It seems to infer some really
        // odd lifetimes I can't figure out how to work with.
        (Box::new(
            self.ops()
                .iter()
                .flat_map(move |op| self.diff.iter_changes(op)),
        )) as Box<dyn Iterator<Item = _>>
    }
}

impl<'diff, 'old, 'new, 'bufs> fmt::Display for Hunk<'diff, 'old, 'new, 'bufs> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let nl = if self.diff.newline_terminated() {
            ""
        } else {
            "\n"
        };
        writeln!(f, "{}", self.header())?;
        for change in self.iter_changes() {
            write!(
                f,
                "{}{}{}",
                match change.tag() {
                    ChangeTag::Equal => ' ',
                    ChangeTag::Delete => '-',
                    ChangeTag::Insert => '+',
                },
                change.value(),
                nl
            )?;
        }
        Ok(())
    }
}

impl<'diff, 'old, 'new, 'bufs> fmt::Display for UnifiedDiff<'diff, 'old, 'new, 'bufs> {
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

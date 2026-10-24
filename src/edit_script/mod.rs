//! Block-aware edit scripts for line diffs.
//!
//! A regular line diff only knows two kinds of edits: lines are deleted or
//! inserted.  When code is moved, copied, re-indented or a line is reflowed
//! into several lines, this produces large and hard to read hunks.  An
//! [`EditScript`] takes a line [`TextDiff`] and explains its changes with a
//! richer vocabulary of [`EditAction`]s:
//!
//! * lines that were [deleted](EditAction::Delete) or
//!   [inserted](EditAction::Insert),
//! * lines that were [updated](EditAction::Update) in place,
//! * a line that was [split](EditAction::Split) into several lines or
//!   several lines that were [merged](EditAction::Merge) into one,
//! * blocks of lines that were [moved](EditAction::Move) (optionally to a
//!   different indentation level) or [copied](EditAction::Copy).
//!
//! ```rust
//! use similar::{EditAction, EditScript};
//!
//! let old = "\
//! def helper():
//!     work()
//!     more_work()
//!
//! def main():
//!     run()
//! ";
//! let new = "\
//! def main():
//!     run()
//!
//! def helper():
//!     work()
//!     more_work()
//! ";
//! let script = EditScript::from_lines(old, new);
//! assert!(script.iter().any(|action| matches!(
//!     action,
//!     EditAction::Move { len: 2, indent: 0, .. }
//! )));
//! for action in script.iter() {
//!     println!("{action}");
//! }
//! ```
//!
//! # Algorithm
//!
//! The analysis follows the approach described in "BDiff: Block-aware and
//! Accurate Text-based Code Differencing" by Lu Yao et al.
//! ([arXiv:2510.21094](https://arxiv.org/abs/2510.21094)).  It was
//! implemented from the description in the paper:
//!
//! 1. The deleted and inserted lines come from the underlying [`TextDiff`].
//!    Within every hunk, lines that were split or merged without any other
//!    change are identified first.
//! 2. Within every hunk, deleted and inserted lines are paired up as line
//!    updates if their combined content and context similarity is high
//!    enough.  Pairs that cross each other are removed, starting with the
//!    pair with the most crossings.
//! 3. Runs of deleted lines are compared with runs of inserted lines
//!    anywhere in the file to find moved blocks, and all lines of the old
//!    text are compared with runs of inserted lines to find copied blocks.
//!    The lines of a block keep their relative indentation, matched lines
//!    may differ slightly (these are reported as updates within the block).
//! 4. The candidates conflict with each other.  They are resolved by
//!    repeatedly computing a minimum cost bipartite matching (Kuhn-Munkres)
//!    between the old and new sides of the candidates, where overlapping
//!    sides are merged into one vertex.  Parts of unselected candidates that
//!    no longer conflict are considered again in the next round.
//!
//! Some details are not fully specified in the paper or did not work well
//! and differ here:
//!
//! * The matching maximizes the number of lines explained by the selected
//!   candidates and uses the paper's weights (which prefer few edits per
//!   line, similar context and short distances) as a penalty.  A plain
//!   minimum weight matching prefers several weak candidates over one good
//!   block.
//! * Line updates also need a content similarity of at least 0.4, otherwise
//!   unrelated lines with identical surroundings are reported as updates.
//! * Candidate blocks are seeded by lines that are equal after removing
//!   indentation.  The minimum block size only counts such identical lines,
//!   changed lines may only appear between them and may not outnumber them.
//!   Without this, blocks of structurally similar but unrelated code were
//!   reported as copies.
//! * The distance between blocks is normalized by the length of the diff.
//! * To bound the running time, very large hunks only compare lines near
//!   the same relative position, the number of inexact line comparisons
//!   while growing blocks is limited, and very large conflict groups (or
//!   whatever is left after 32 matching rounds) are resolved greedily.
//!
//! The result depends on the underlying diff.  The paper found
//! [`Algorithm::Histogram`] to work best, which
//! is what [`EditScript::from_lines`] uses.
//!
//! Requires the `text` feature.
use alloc::collections::{BTreeMap, BinaryHeap};
use alloc::vec;
use alloc::vec::Vec;
use core::fmt;
use core::ops::Range;

use crate::algorithms::utils::{HashBucket, stable_hash};
use crate::text::{DiffableStr, IntoDiffInput, TextDiff};
use crate::types::int_key_map_with_capacity;
use crate::{Algorithm, DiffOp};

mod assignment;
mod similarity;

use self::similarity::{indel_ratio, max_indel_ratio, sequence_ratio};

/// Lines with more than this many identical seed lines are not used to seed
/// block candidates.
const MAX_SEED_OCCURRENCES: usize = 16;
/// Hunks with more line pairs than this only compare nearby lines.
const MAX_HUNK_PAIRS: usize = 10_000;
/// The number of positions considered around the expected position in large
/// hunks.
const HUNK_WINDOW: usize = 32;
/// The number of update candidates kept per deleted line.
const MAX_UPDATES_PER_LINE: usize = 4;
/// Update candidates in a hunk above which crossings are removed by
/// computing the best chain instead of the quadratic paper heuristic.
const MAX_CROSSING_REMOVAL: usize = 2_000;
/// Fuzzy line comparisons allowed per inserted line while growing blocks.
/// Once used up, blocks only grow through identical lines.
const FUZZY_BUDGET_PER_LINE: usize = 64;
/// After this many matching rounds the remaining candidates are resolved
/// greedily.
const MAX_MATCHING_ROUNDS: usize = 32;
/// Conflict groups larger than this are resolved greedily.
const MAX_ASSIGNMENT_SIZE: usize = 400;
/// Weight of the content similarity for line updates (the rest is context).
const LINE_SIMILARITY_WEIGHT: f64 = 0.6;
/// Minimum combined similarity for a line update.
const UPDATE_THRESHOLD: f64 = 0.5;
/// Minimum content similarity for a line update.  Without this, unrelated
/// lines with identical surroundings would be paired.
const MIN_UPDATE_CONTENT: f64 = 0.4;
/// Minimum similarity for lines to match inside a moved or copied block.
const BLOCK_LINE_THRESHOLD: f64 = 0.6;

/// A single action in an [`EditScript`].
///
/// All indexes are line indexes into the old and new side of the
/// [`TextDiff`] the script was created from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case", tag = "op"))]
pub enum EditAction {
    /// Lines `old_index..old_index + old_len` were deleted.
    Delete {
        /// The first deleted line.
        old_index: usize,
        /// The number of deleted lines.
        old_len: usize,
    },
    /// Lines `new_index..new_index + new_len` were inserted.
    Insert {
        /// The first inserted line.
        new_index: usize,
        /// The number of inserted lines.
        new_len: usize,
    },
    /// The old line `old_index` was changed into the new line `new_index`.
    ///
    /// Updates are also reported for the lines that changed within a moved
    /// or copied block.
    Update {
        /// The old line.
        old_index: usize,
        /// The new line.
        new_index: usize,
    },
    /// The old line `old_index` was split into the new lines
    /// `new_index..new_index + new_len` without other changes.
    Split {
        /// The old line.
        old_index: usize,
        /// The first new line.
        new_index: usize,
        /// The number of new lines.
        new_len: usize,
    },
    /// The old lines `old_index..old_index + old_len` were merged into the
    /// new line `new_index` without other changes.
    Merge {
        /// The first old line.
        old_index: usize,
        /// The number of old lines.
        old_len: usize,
        /// The new line.
        new_index: usize,
    },
    /// A block of `len` lines was moved from `old_index` to `new_index`.
    Move {
        /// The first line of the block in the old text.
        old_index: usize,
        /// The first line of the block in the new text.
        new_index: usize,
        /// The number of lines in the block.
        len: usize,
        /// How many columns the block was indented (positive) or dedented
        /// (negative).
        indent: isize,
    },
    /// A block of `len` lines starting at `old_index` was copied to
    /// `new_index`.  The source lines are not affected by this action.
    Copy {
        /// The first line of the source block in the old text.
        old_index: usize,
        /// The first line of the copy in the new text.
        new_index: usize,
        /// The number of lines in the block.
        len: usize,
        /// How many columns the copy was indented (positive) or dedented
        /// (negative) relative to the source.
        indent: isize,
    },
}

impl EditAction {
    /// The range of old lines this action refers to.
    ///
    /// This is empty for inserts.
    pub fn old_range(&self) -> Range<usize> {
        match *self {
            EditAction::Delete { old_index, old_len } => old_index..old_index + old_len,
            EditAction::Insert { .. } => 0..0,
            EditAction::Update { old_index, .. } | EditAction::Split { old_index, .. } => {
                old_index..old_index + 1
            }
            EditAction::Merge {
                old_index, old_len, ..
            } => old_index..old_index + old_len,
            EditAction::Move { old_index, len, .. } | EditAction::Copy { old_index, len, .. } => {
                old_index..old_index + len
            }
        }
    }

    /// The range of new lines this action refers to.
    ///
    /// This is empty for deletes.
    pub fn new_range(&self) -> Range<usize> {
        match *self {
            EditAction::Delete { .. } => 0..0,
            EditAction::Insert { new_index, new_len }
            | EditAction::Split {
                new_index, new_len, ..
            } => new_index..new_index + new_len,
            EditAction::Update { new_index, .. } | EditAction::Merge { new_index, .. } => {
                new_index..new_index + 1
            }
            EditAction::Move { new_index, len, .. } | EditAction::Copy { new_index, len, .. } => {
                new_index..new_index + len
            }
        }
    }
}

fn fmt_lines(f: &mut fmt::Formatter<'_>, range: Range<usize>) -> fmt::Result {
    if range.len() == 1 {
        write!(f, "line {}", range.start + 1)
    } else {
        write!(f, "lines {}-{}", range.start + 1, range.end)
    }
}

fn fmt_indent(f: &mut fmt::Formatter<'_>, indent: isize) -> fmt::Result {
    match indent {
        0 => Ok(()),
        1.. => write!(f, ", indented by {indent}"),
        _ => write!(f, ", dedented by {}", -indent),
    }
}

/// Formats the action as a human readable sentence with 1-based line
/// numbers, for instance "move 3 lines from line 5 to line 12".
impl fmt::Display for EditAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            EditAction::Delete { .. } => {
                f.write_str("delete ")?;
                fmt_lines(f, self.old_range())
            }
            EditAction::Insert { .. } => {
                f.write_str("insert ")?;
                fmt_lines(f, self.new_range())
            }
            EditAction::Update {
                old_index,
                new_index,
            } => write!(f, "update line {} to line {}", old_index + 1, new_index + 1),
            EditAction::Split { old_index, .. } => {
                write!(f, "split line {} into ", old_index + 1)?;
                fmt_lines(f, self.new_range())
            }
            EditAction::Merge { new_index, .. } => {
                f.write_str("merge ")?;
                fmt_lines(f, self.old_range())?;
                write!(f, " into line {}", new_index + 1)
            }
            EditAction::Move {
                old_index,
                new_index,
                len,
                indent,
            }
            | EditAction::Copy {
                old_index,
                new_index,
                len,
                indent,
            } => {
                let verb = if matches!(self, EditAction::Move { .. }) {
                    "move"
                } else {
                    "copy"
                };
                write!(
                    f,
                    "{verb} {len} line{} from line {} to line {}",
                    if len == 1 { "" } else { "s" },
                    old_index + 1,
                    new_index + 1
                )?;
                fmt_indent(f, indent)
            }
        }
    }
}

/// Configures how an [`EditScript`] is computed.
///
/// ```rust
/// use similar::{EditScriptConfig, TextDiff};
///
/// let diff = TextDiff::from_lines("a\nb\n", "b\na\n");
/// let script = EditScriptConfig::new()
///     .copies(false)
///     .min_move_lines(3)
///     .build(&diff);
/// ```
#[derive(Clone, Debug)]
pub struct EditScriptConfig {
    moves: bool,
    copies: bool,
    updates: bool,
    splits: bool,
    block_updates: bool,
    min_move_lines: usize,
    min_copy_lines: usize,
    max_split_lines: usize,
    context_lines: usize,
    tab_width: usize,
}

impl Default for EditScriptConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl EditScriptConfig {
    /// Creates the default configuration.
    pub const fn new() -> EditScriptConfig {
        EditScriptConfig {
            moves: true,
            copies: true,
            updates: true,
            splits: true,
            block_updates: true,
            min_move_lines: 2,
            min_copy_lines: 3,
            max_split_lines: 8,
            context_lines: 4,
            tab_width: 4,
        }
    }

    /// Enables or disables the detection of moved blocks.  Default: on.
    pub fn moves(&mut self, yes: bool) -> &mut Self {
        self.moves = yes;
        self
    }

    /// Enables or disables the detection of copied blocks.  Default: on.
    pub fn copies(&mut self, yes: bool) -> &mut Self {
        self.copies = yes;
        self
    }

    /// Enables or disables the detection of single line updates.  Default:
    /// on.
    pub fn updates(&mut self, yes: bool) -> &mut Self {
        self.updates = yes;
        self
    }

    /// Enables or disables the detection of split and merged lines.
    /// Default: on.
    pub fn splits(&mut self, yes: bool) -> &mut Self {
        self.splits = yes;
        self
    }

    /// Allows lines within moved and copied blocks to differ slightly.
    /// Default: on.
    ///
    /// The changed lines are reported as [`EditAction::Update`]s.
    pub fn block_updates(&mut self, yes: bool) -> &mut Self {
        self.block_updates = yes;
        self
    }

    /// The minimum number of significant lines in a moved block.  Default: 2.
    ///
    /// Only lines that are identical apart from indentation count.  Blank
    /// lines and lines without any letters or digits (like a lone `}`) do
    /// not count.
    pub fn min_move_lines(&mut self, lines: usize) -> &mut Self {
        self.min_move_lines = lines.max(1);
        self
    }

    /// The minimum number of significant lines in a copied block.  Default: 3.
    ///
    /// Only lines that are identical apart from indentation count.  Blank
    /// lines and lines without any letters or digits (like a lone `}`) do
    /// not count.
    pub fn min_copy_lines(&mut self, lines: usize) -> &mut Self {
        self.min_copy_lines = lines.max(1);
        self
    }

    /// The maximum number of lines considered for a split or merge.  Default: 8.
    pub fn max_split_lines(&mut self, lines: usize) -> &mut Self {
        self.max_split_lines = lines;
        self
    }

    /// The number of lines above and below that are compared to measure how
    /// similar the surroundings of two lines or blocks are.  Default: 4.
    pub fn context_lines(&mut self, lines: usize) -> &mut Self {
        self.context_lines = lines;
        self
    }

    /// The width of a tab when measuring indentation.  Default: 4.
    pub fn tab_width(&mut self, width: usize) -> &mut Self {
        self.tab_width = width.max(1);
        self
    }

    /// Computes the edit script for a line diff.
    ///
    /// The diff should be created with [`TextDiff::from_lines`] or
    /// [`TextDiffConfig::diff_lines`](crate::TextDiffConfig::diff_lines).
    pub fn build<T: DiffableStr + ?Sized>(&self, diff: &TextDiff<'_, '_, T>) -> EditScript {
        let old = diff
            .iter_old_slices()
            .map(|line| LineInfo::new(line.as_bytes(), self.tab_width))
            .collect::<Vec<_>>();
        let mut new = diff
            .iter_new_slices()
            .map(|line| LineInfo::new(line.as_bytes(), self.tab_width))
            .collect::<Vec<_>>();
        let mut old = old;
        let mut ids =
            int_key_map_with_capacity::<u64, HashBucket<(&[u8], u32)>>(old.len() + new.len());
        let mut next_id = 0;
        for line in old.iter_mut().chain(new.iter_mut()) {
            let hash = stable_hash(line.content);
            let bucket = ids.entry(hash).or_insert_with(|| {
                next_id += 1;
                HashBucket::new((line.content, next_id - 1))
            });
            let found = bucket
                .iter()
                .find(|(content, _)| *content == line.content)
                .map(|&(_, id)| id);
            line.id = found.unwrap_or_else(|| {
                next_id += 1;
                bucket.push((line.content, next_id - 1));
                next_id - 1
            });
        }
        EditScript {
            actions: Analysis::new(self, old, new, diff.ops()).run(),
        }
    }

    /// Diffs the lines of two texts with
    /// [`Algorithm::Histogram`] and computes the
    /// edit script.
    pub fn diff_lines<'old, 'new, Old, New, T>(&self, old: Old, new: New) -> EditScript
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized + 'old + 'new,
    {
        let diff = TextDiff::configure()
            .algorithm(Algorithm::Histogram)
            .diff_lines(old, new);
        self.build(&diff)
    }
}

/// A block-aware edit script for a line diff.
///
/// See the [module documentation](crate::edit_script) for an overview.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct EditScript {
    actions: Vec<EditAction>,
}

impl EditScript {
    /// Creates a configuration for edit scripts.
    pub const fn configure() -> EditScriptConfig {
        EditScriptConfig::new()
    }

    /// Computes the edit script for a line diff with the default
    /// configuration.
    pub fn new<T: DiffableStr + ?Sized>(diff: &TextDiff<'_, '_, T>) -> EditScript {
        EditScriptConfig::new().build(diff)
    }

    /// Diffs the lines of two texts and computes the edit script with the
    /// default configuration.
    ///
    /// See [`EditScriptConfig::diff_lines`].
    pub fn from_lines<'old, 'new, Old, New, T>(old: Old, new: New) -> EditScript
    where
        Old: IntoDiffInput<'old, Output = T>,
        New: IntoDiffInput<'new, Output = T>,
        T: DiffableStr + ?Sized + 'old + 'new,
    {
        EditScriptConfig::new().diff_lines(old, new)
    }

    /// Returns the actions in the order of their position in the diff.
    pub fn actions(&self) -> &[EditAction] {
        &self.actions
    }

    /// Iterates over the actions.
    pub fn iter(&self) -> core::slice::Iter<'_, EditAction> {
        self.actions.iter()
    }

    /// The number of actions.  This is the size of the edit script.
    pub fn len(&self) -> usize {
        self.actions.len()
    }

    /// Returns `true` if there are no changes.
    pub fn is_empty(&self) -> bool {
        self.actions.is_empty()
    }

    /// Converts the script into its actions.
    pub fn into_actions(self) -> Vec<EditAction> {
        self.actions
    }
}

impl<'a> IntoIterator for &'a EditScript {
    type Item = &'a EditAction;
    type IntoIter = core::slice::Iter<'a, EditAction>;

    fn into_iter(self) -> Self::IntoIter {
        self.actions.iter()
    }
}

/// Precomputed information about a line.
struct LineInfo<'a> {
    /// The line without indentation and trailing whitespace.
    content: &'a [u8],
    /// The indentation in columns.
    indent: usize,
    /// The line contains letters or digits.
    significant: bool,
    /// Identifies the content, equal contents have equal ids.
    id: u32,
}

impl<'a> LineInfo<'a> {
    fn new(raw: &'a [u8], tab_width: usize) -> LineInfo<'a> {
        let mut indent = 0;
        let mut start = 0;
        for &c in raw {
            match c {
                b' ' => indent += 1,
                b'\t' => indent += tab_width - indent % tab_width,
                _ => break,
            }
            start += 1;
        }
        let content = trim_end(&raw[start..]);
        LineInfo {
            content,
            indent,
            significant: content
                .iter()
                .any(|c| c.is_ascii_alphanumeric() || *c >= 0x80),
            id: 0,
        }
    }

    fn is_blank(&self) -> bool {
        self.content.is_empty()
    }
}

fn is_space(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\r' | b'\n' | b'\x0b' | b'\x0c')
}

fn trim_end(s: &[u8]) -> &[u8] {
    let end = s.iter().rposition(|&c| !is_space(c)).map_or(0, |x| x + 1);
    &s[..end]
}

fn trim_start(s: &[u8]) -> &[u8] {
    let start = s.iter().position(|&c| !is_space(c)).unwrap_or(s.len());
    &s[start..]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Kind {
    Update,
    Move,
    Copy,
}

/// A possible mapping of old lines to new lines.
#[derive(Debug, Clone)]
struct Candidate {
    kind: Kind,
    old_start: usize,
    new_start: usize,
    len: usize,
    indent: isize,
    /// The cost of selecting this candidate.  This is always negative.
    cost: f64,
}

impl Candidate {
    fn old_range(&self) -> Range<usize> {
        self.old_start..self.old_start + self.len
    }

    fn new_range(&self) -> Range<usize> {
        self.new_start..self.new_start + self.len
    }

    /// Copies do not consume their source lines.
    fn consumes_old(&self) -> bool {
        self.kind != Kind::Copy
    }
}

/// Converts the weight of a candidate (as defined in the paper) into the
/// cost used for the matching.
///
/// The matching maximizes the number of lines explained by the selected
/// candidates, the weight acts as a penalty.  Candidates that do not explain
/// more lines than their weight are not worth selecting and return `None`.
fn candidate_cost(weight: f64, explained_lines: usize) -> Option<f64> {
    let cost = weight - explained_lines as f64;
    (cost < 0.0).then_some(cost)
}

struct Hunk {
    old: Range<usize>,
    new: Range<usize>,
}

struct Analysis<'c, 'a> {
    config: &'c EditScriptConfig,
    old: Vec<LineInfo<'a>>,
    new: Vec<LineInfo<'a>>,
    old_deleted: Vec<bool>,
    new_added: Vec<bool>,
    /// Old lines consumed by an accepted action (other than a copy).
    old_used: Vec<bool>,
    /// New lines consumed by an accepted action.
    new_used: Vec<bool>,
    /// Position of every line in the aligned diff, used for distances.
    old_row: Vec<usize>,
    new_row: Vec<usize>,
    total_rows: usize,
    hunks: Vec<Hunk>,
    actions: Vec<EditAction>,
}

impl<'c, 'a> Analysis<'c, 'a> {
    fn new(
        config: &'c EditScriptConfig,
        old: Vec<LineInfo<'a>>,
        new: Vec<LineInfo<'a>>,
        ops: &[DiffOp],
    ) -> Self {
        let mut old_deleted = vec![false; old.len()];
        let mut new_added = vec![false; new.len()];
        let mut old_row = vec![0; old.len()];
        let mut new_row = vec![0; new.len()];
        let mut hunks = Vec::new();
        let mut row = 0;
        let mut idx = 0;
        while idx < ops.len() {
            if let DiffOp::Equal {
                old_index,
                new_index,
                len,
            } = ops[idx]
            {
                for offset in 0..len {
                    old_row[old_index + offset] = row + offset;
                    new_row[new_index + offset] = row + offset;
                }
                row += len;
                idx += 1;
                continue;
            }
            // Consecutive changes form a single hunk.
            let first = idx;
            while idx < ops.len() && !matches!(ops[idx], DiffOp::Equal { .. }) {
                idx += 1;
            }
            let hunk = Hunk {
                old: ops[first].old_range().start..ops[idx - 1].old_range().end,
                new: ops[first].new_range().start..ops[idx - 1].new_range().end,
            };
            for (offset, l) in hunk.old.clone().enumerate() {
                old_deleted[l] = true;
                old_row[l] = row + offset;
            }
            for (offset, r) in hunk.new.clone().enumerate() {
                new_added[r] = true;
                new_row[r] = row + offset;
            }
            row += hunk.old.len().max(hunk.new.len());
            hunks.push(hunk);
        }

        Analysis {
            config,
            old_used: vec![false; old.len()],
            new_used: vec![false; new.len()],
            old,
            new,
            old_deleted,
            new_added,
            old_row,
            new_row,
            total_rows: row,
            hunks,
            actions: Vec::new(),
        }
    }

    fn run(mut self) -> Vec<EditAction> {
        if self.hunks.is_empty() {
            return Vec::new();
        }
        if self.config.splits {
            self.find_splits_and_merges();
        }
        let mut candidates = Vec::new();
        if self.config.updates {
            self.find_updates(&mut candidates);
        }
        if self.config.moves {
            self.find_blocks(Kind::Move, &mut candidates);
        }
        if self.config.copies {
            self.find_blocks(Kind::Copy, &mut candidates);
        }
        let accepted = self.resolve(candidates);
        self.emit(accepted)
    }

    // -- splits and merges --------------------------------------------------

    fn find_splits_and_merges(&mut self) {
        for hunk_idx in 0..self.hunks.len() {
            let old = self.hunks[hunk_idx].old.clone();
            let new = self.hunks[hunk_idx].new.clone();
            if old.is_empty() || new.is_empty() {
                continue;
            }
            for l in old.clone() {
                if self.old_used[l] || self.old[l].is_blank() {
                    continue;
                }
                for r in hunk_window(l - old.start, old.len(), new.clone()) {
                    if let Some(end) = self.try_split(l, r, new.end) {
                        self.old_used[l] = true;
                        self.new_used[r..end].fill(true);
                        self.actions.push(EditAction::Split {
                            old_index: l,
                            new_index: r,
                            new_len: end - r,
                        });
                        break;
                    }
                }
            }
            for r in new.clone() {
                if self.new_used[r] || self.new[r].is_blank() {
                    continue;
                }
                for l in hunk_window(r - new.start, new.len(), old.clone()) {
                    if let Some(end) = self.try_merge(r, l, old.end) {
                        self.new_used[r] = true;
                        self.old_used[l..end].fill(true);
                        self.actions.push(EditAction::Merge {
                            old_index: l,
                            old_len: end - l,
                            new_index: r,
                        });
                        break;
                    }
                }
            }
        }
    }

    /// Checks if old line `l` equals the concatenation of the new lines
    /// starting at `r`.  Returns the end of the new lines.
    fn try_split(&self, l: usize, r: usize, end: usize) -> Option<usize> {
        split_match(
            self.old[l].content,
            r,
            end,
            self.config.max_split_lines,
            |k| (!self.new_used[k]).then(|| self.new[k].content),
        )
    }

    /// Checks if new line `r` equals the concatenation of the old lines
    /// starting at `l`.  Returns the end of the old lines.
    fn try_merge(&self, r: usize, l: usize, end: usize) -> Option<usize> {
        split_match(
            self.new[r].content,
            l,
            end,
            self.config.max_split_lines,
            |k| (!self.old_used[k]).then(|| self.old[k].content),
        )
    }

    // -- line updates -------------------------------------------------------

    fn find_updates(&self, candidates: &mut Vec<Candidate>) {
        for hunk in &self.hunks {
            let dels = hunk
                .old
                .clone()
                .filter(|&l| !self.old_used[l] && !self.old[l].is_blank())
                .collect::<Vec<_>>();
            let adds = hunk
                .new
                .clone()
                .filter(|&r| !self.new_used[r] && !self.new[r].is_blank())
                .collect::<Vec<_>>();
            if dels.is_empty() || adds.is_empty() {
                continue;
            }
            let del_histograms = dels
                .iter()
                .map(|&l| byte_histogram(self.old[l].content))
                .collect::<Vec<_>>();
            let add_histograms = adds
                .iter()
                .map(|&r| byte_histogram(self.new[r].content))
                .collect::<Vec<_>>();
            let n = self.config.context_lines;
            let add_contexts = adds
                .iter()
                .map(|&r| context_lines(&self.new, r, r + 1, n))
                .collect::<Vec<_>>();
            let mut pairs = Vec::new();
            let mut line_pairs = Vec::new();
            for (i, &l) in dels.iter().enumerate() {
                let range = hunk_window(i, dels.len(), 0..adds.len());
                line_pairs.clear();
                let del_context = context_lines(&self.old, l, l + 1, n);
                for j in range {
                    let r = adds[j];
                    let (a, b) = (self.old[l].content, self.new[r].content);
                    let best_case = LINE_SIMILARITY_WEIGHT * max_indel_ratio(a.len(), b.len())
                        + (1.0 - LINE_SIMILARITY_WEIGHT);
                    if best_case < UPDATE_THRESHOLD {
                        continue;
                    }
                    if max_indel_ratio(a.len(), b.len()) < MIN_UPDATE_CONTENT
                        || shared_bytes_ratio(
                            &del_histograms[i],
                            &add_histograms[j],
                            a.len() + b.len(),
                        ) < MIN_UPDATE_CONTENT
                    {
                        continue;
                    }
                    let content = indel_ratio(a, b);
                    if content < MIN_UPDATE_CONTENT
                        || LINE_SIMILARITY_WEIGHT * content + (1.0 - LINE_SIMILARITY_WEIGHT)
                            < UPDATE_THRESHOLD
                    {
                        continue;
                    }
                    let context = sequence_ratio(&del_context, &add_contexts[j]);
                    let score =
                        LINE_SIMILARITY_WEIGHT * content + (1.0 - LINE_SIMILARITY_WEIGHT) * context;
                    if score >= UPDATE_THRESHOLD {
                        line_pairs.push((l, r, score));
                    }
                }
                line_pairs.sort_by(|a, b| b.2.total_cmp(&a.2).then(a.1.cmp(&b.1)));
                pairs.extend(line_pairs.iter().take(MAX_UPDATES_PER_LINE));
            }
            remove_crossings(&mut pairs);
            candidates.extend(pairs.into_iter().filter_map(|(l, r, score)| {
                Some(Candidate {
                    kind: Kind::Update,
                    old_start: l,
                    new_start: r,
                    len: 1,
                    indent: 0,
                    cost: candidate_cost(1.0 + (1.0 - score) / 10.0, 2)?,
                })
            }));
        }
    }

    // -- moved and copied blocks --------------------------------------------

    fn find_blocks(&self, kind: Kind, candidates: &mut Vec<Candidate>) {
        let fuzzy_budget = core::cell::Cell::new(
            FUZZY_BUDGET_PER_LINE * self.new_added.iter().filter(|&&x| x).count(),
        );
        let in_pool = |l: usize| kind == Kind::Copy || (self.old_deleted[l] && !self.old_used[l]);
        let mut seeds = BTreeMap::<&[u8], Vec<usize>>::new();
        for (l, line) in self.old.iter().enumerate() {
            if line.significant && in_pool(l) {
                seeds.entry(line.content).or_default().push(l);
            }
        }
        // Found blocks per diagonal (old - new), as ranges of new lines.
        let mut covered = BTreeMap::<isize, Vec<Range<usize>>>::new();
        for r in 0..self.new.len() {
            if !self.new_added[r] || self.new_used[r] || !self.new[r].significant {
                continue;
            }
            let Some(olds) = seeds.get(self.new[r].content) else {
                continue;
            };
            if olds.len() > MAX_SEED_OCCURRENCES {
                continue;
            }
            for &l in olds {
                let diagonal = l as isize - r as isize;
                if covered
                    .get(&diagonal)
                    .is_some_and(|ranges| ranges.iter().any(|range| range.contains(&r)))
                {
                    continue;
                }
                let indent = self.new[r].indent as isize - self.old[l].indent as isize;
                let mut before = 0;
                while before < l.min(r)
                    && self.block_pair_matches(
                        kind,
                        l - before - 1,
                        r - before - 1,
                        indent,
                        &fuzzy_budget,
                    )
                {
                    before += 1;
                }
                let mut after = 1;
                while self.block_pair_matches(kind, l + after, r + after, indent, &fuzzy_budget) {
                    after += 1;
                }
                let block = (l - before, r - before, before + after);
                covered
                    .entry(diagonal)
                    .or_default()
                    .push(block.1..block.1 + block.2);
                if let Some(candidate) = self.make_block(kind, block.0, block.1, block.2, indent) {
                    candidates.push(candidate);
                }
            }
        }
    }

    /// Checks if old line `l` and new line `r` can be part of the same block.
    ///
    /// Comparing lines that are not identical uses up `fuzzy_budget`.
    fn block_pair_matches(
        &self,
        kind: Kind,
        l: usize,
        r: usize,
        indent: isize,
        fuzzy_budget: &core::cell::Cell<usize>,
    ) -> bool {
        if l >= self.old.len() || r >= self.new.len() {
            return false;
        }
        if !self.new_added[r] || self.new_used[r] {
            return false;
        }
        if kind == Kind::Move && (!self.old_deleted[l] || self.old_used[l]) {
            return false;
        }
        let (a, b) = (&self.old[l], &self.new[r]);
        match (a.is_blank(), b.is_blank()) {
            (true, true) => true,
            (false, false) => {
                if b.indent as isize - a.indent as isize != indent {
                    return false;
                }
                if a.id == b.id {
                    return true;
                }
                if !self.config.block_updates
                    || !a.significant
                    || !b.significant
                    || fuzzy_budget.get() == 0
                {
                    return false;
                }
                fuzzy_budget.set(fuzzy_budget.get() - 1);
                max_indel_ratio(a.content.len(), b.content.len()) > BLOCK_LINE_THRESHOLD
                    && indel_ratio(a.content, b.content) > BLOCK_LINE_THRESHOLD
            }
            _ => false,
        }
    }

    /// Creates a block candidate if it is large enough.
    ///
    /// A block needs at least the configured number of significant lines that are identical
    /// (apart from indentation), and at most as many changed lines as
    /// identical ones.
    fn make_block(
        &self,
        kind: Kind,
        old_start: usize,
        new_start: usize,
        len: usize,
        indent: isize,
    ) -> Option<Candidate> {
        let is_changed = |l: usize, r: usize| self.old[l].id != self.new[r].id;
        let min_lines = if kind == Kind::Move {
            self.config.min_move_lines
        } else {
            self.config.min_copy_lines
        };
        let mut identical = 0;
        let mut changed = 0;
        for k in 0..len {
            let (l, r) = (old_start + k, new_start + k);
            if is_changed(l, r) {
                changed += 1;
            } else if self.new[r].significant {
                identical += 1;
            }
        }
        if identical < min_lines || changed > identical {
            return None;
        }
        let edits = match kind {
            Kind::Move => 2.0,
            _ => 3.0,
        } + (indent != 0) as u8 as f64
            + (changed > 0) as u8 as f64;
        let context = self.context_similarity(old_start, len, new_start, len);
        let distance = self.old_row[old_start].abs_diff(self.new_row[new_start]) as f64
            / self.total_rows.max(1) as f64;
        let weight = edits / len as f64 + (1.0 - context) / 10.0 + distance / 100.0;
        // Every changed line is reported as an additional update.
        let explained = if kind == Kind::Move { 2 * len } else { len } - changed;
        Some(Candidate {
            kind,
            old_start,
            new_start,
            len,
            indent,
            cost: candidate_cost(weight, explained)?,
        })
    }

    /// Compares the non-blank lines around an old range with the ones
    /// around a new range.
    fn context_similarity(
        &self,
        old_start: usize,
        old_len: usize,
        new_start: usize,
        new_len: usize,
    ) -> f64 {
        let n = self.config.context_lines;
        let old_context = context_lines(&self.old, old_start, old_start + old_len, n);
        let new_context = context_lines(&self.new, new_start, new_start + new_len, n);
        sequence_ratio(&old_context, &new_context)
    }

    // -- conflict resolution ------------------------------------------------

    /// Picks a conflict free set of candidates.
    ///
    /// Every round computes a minimum cost matching between the old and new
    /// sides of the candidates, where overlapping sides are merged into a
    /// single vertex.  The parts of unselected candidates that do not
    /// overlap with selected ones are considered again in the next round.
    fn resolve(&mut self, mut candidates: Vec<Candidate>) -> Vec<Candidate> {
        let mut accepted = Vec::new();
        let mut rounds = 0;
        while !candidates.is_empty() {
            if rounds == MAX_MATCHING_ROUNDS {
                self.resolve_greedy(candidates, &mut accepted);
                break;
            }
            rounds += 1;
            let selected = self.match_round(&candidates);
            debug_assert!(!selected.is_empty());
            let mut is_selected = vec![false; candidates.len()];
            for &idx in &selected {
                is_selected[idx] = true;
                let candidate = &candidates[idx];
                self.new_used[candidate.new_range()].fill(true);
                if candidate.consumes_old() {
                    self.old_used[candidate.old_range()].fill(true);
                }
            }
            let mut next = Vec::new();
            let mut seen = BTreeMap::new();
            for (idx, candidate) in candidates.iter().enumerate() {
                if is_selected[idx] {
                    accepted.push(candidate.clone());
                    continue;
                }
                for piece in self.remaining_pieces(candidate) {
                    let key = (piece.kind, piece.old_start, piece.new_start, piece.len);
                    if seen.insert(key, ()).is_none() {
                        next.push(piece);
                    }
                }
            }
            candidates = next;
        }
        accepted
    }

    /// Accepts the cheapest candidate that does not conflict with accepted
    /// ones until none are left.  Candidates that became partially
    /// conflicting are trimmed and considered again.
    fn resolve_greedy(&mut self, candidates: Vec<Candidate>, accepted: &mut Vec<Candidate>) {
        let mut heap = candidates
            .into_iter()
            .map(ByCost)
            .collect::<BinaryHeap<_>>();
        while let Some(ByCost(candidate)) = heap.pop() {
            let mut pieces = self.remaining_pieces(&candidate);
            if pieces.len() == 1
                && pieces[0].old_start == candidate.old_start
                && pieces[0].new_start == candidate.new_start
                && pieces[0].len == candidate.len
            {
                self.new_used[candidate.new_range()].fill(true);
                if candidate.consumes_old() {
                    self.old_used[candidate.old_range()].fill(true);
                }
                accepted.push(candidate);
            } else {
                heap.extend(pieces.drain(..).map(ByCost));
            }
        }
    }

    /// The parts of a candidate that do not use consumed lines.
    fn remaining_pieces(&self, candidate: &Candidate) -> Vec<Candidate> {
        let is_free = |k: usize| {
            !self.new_used[candidate.new_start + k]
                && (!candidate.consumes_old() || !self.old_used[candidate.old_start + k])
        };
        if (0..candidate.len).all(is_free) {
            return vec![candidate.clone()];
        }
        if candidate.kind == Kind::Update {
            return Vec::new();
        }
        let mut rv = Vec::new();
        let mut k = 0;
        while k < candidate.len {
            if !is_free(k) {
                k += 1;
                continue;
            }
            let start = k;
            while k < candidate.len && is_free(k) {
                k += 1;
            }
            rv.extend(self.make_block(
                candidate.kind,
                candidate.old_start + start,
                candidate.new_start + start,
                k - start,
                candidate.indent,
            ));
        }
        rv
    }

    /// Selects a set of non-conflicting candidates.
    fn match_round(&self, candidates: &[Candidate]) -> Vec<usize> {
        // Merge candidates with overlapping sides into shared vertices.
        let mut left = UnionFind::new(candidates.len());
        let mut right = UnionFind::new(candidates.len());
        let mut old_owner = vec![usize::MAX; self.old.len()];
        let mut new_owner = vec![usize::MAX; self.new.len()];
        for (idx, candidate) in candidates.iter().enumerate() {
            if candidate.consumes_old() {
                for l in candidate.old_range() {
                    if old_owner[l] == usize::MAX {
                        old_owner[l] = idx;
                    } else {
                        left.union(old_owner[l], idx);
                    }
                }
            }
            for r in candidate.new_range() {
                if new_owner[r] == usize::MAX {
                    new_owner[r] = idx;
                } else {
                    right.union(new_owner[r], idx);
                }
            }
        }

        // The cheapest candidate for every pair of vertices.
        let mut edges = BTreeMap::<(usize, usize), usize>::new();
        for (idx, candidate) in candidates.iter().enumerate() {
            let key = (left.find(idx), right.find(idx));
            let best = edges.entry(key).or_insert(idx);
            if candidate.cost < candidates[*best].cost {
                *best = idx;
            }
        }

        // Split the graph into independent components.  Left vertex `v` is
        // `v`, right vertex `v` is `candidates.len() + v`.
        let mut components = UnionFind::new(candidates.len() * 2);
        for &(l, r) in edges.keys() {
            components.union(l, candidates.len() + r);
        }
        let mut grouped = BTreeMap::<usize, Vec<((usize, usize), usize)>>::new();
        for (&key, &idx) in &edges {
            grouped
                .entry(components.find(key.0))
                .or_default()
                .push((key, idx));
        }

        let mut selected = Vec::new();
        for edges in grouped.values() {
            if edges.len() == 1 {
                selected.push(edges[0].1);
            } else {
                select_component(edges, candidates, &mut selected);
            }
        }
        selected
    }

    // -- output -------------------------------------------------------------

    fn emit(mut self, accepted: Vec<Candidate>) -> Vec<EditAction> {
        for candidate in accepted {
            match candidate.kind {
                Kind::Update => self.actions.push(EditAction::Update {
                    old_index: candidate.old_start,
                    new_index: candidate.new_start,
                }),
                Kind::Move | Kind::Copy => {
                    self.actions.push(if candidate.kind == Kind::Move {
                        EditAction::Move {
                            old_index: candidate.old_start,
                            new_index: candidate.new_start,
                            len: candidate.len,
                            indent: candidate.indent,
                        }
                    } else {
                        EditAction::Copy {
                            old_index: candidate.old_start,
                            new_index: candidate.new_start,
                            len: candidate.len,
                            indent: candidate.indent,
                        }
                    });
                    for k in 0..candidate.len {
                        let (l, r) = (candidate.old_start + k, candidate.new_start + k);
                        if self.old[l].id != self.new[r].id {
                            self.actions.push(EditAction::Update {
                                old_index: l,
                                new_index: r,
                            });
                        }
                    }
                }
            }
        }

        let mut l = 0;
        while l < self.old.len() {
            if self.old_deleted[l] && !self.old_used[l] {
                let start = l;
                while l < self.old.len() && self.old_deleted[l] && !self.old_used[l] {
                    l += 1;
                }
                self.actions.push(EditAction::Delete {
                    old_index: start,
                    old_len: l - start,
                });
            } else {
                l += 1;
            }
        }
        let mut r = 0;
        while r < self.new.len() {
            if self.new_added[r] && !self.new_used[r] {
                let start = r;
                while r < self.new.len() && self.new_added[r] && !self.new_used[r] {
                    r += 1;
                }
                self.actions.push(EditAction::Insert {
                    new_index: start,
                    new_len: r - start,
                });
            } else {
                r += 1;
            }
        }

        let (old_row, new_row) = (&self.old_row, &self.new_row);
        self.actions.sort_by_key(|action| {
            let row = match action {
                EditAction::Delete { old_index, .. } => old_row[*old_index],
                _ => new_row[action.new_range().start],
            };
            // Updates within a block follow the block.
            let order = match action {
                EditAction::Delete { .. } => 0,
                EditAction::Move { .. } | EditAction::Copy { .. } => 1,
                EditAction::Insert { .. } => 3,
                _ => 2,
            };
            (row, order, *action)
        });
        self.actions
    }
}

/// The lines of `other` to compare with the line at `pos` of a side with
/// `len` lines.  Large hunks only compare lines near the same relative
/// position.
fn hunk_window(pos: usize, len: usize, other: Range<usize>) -> Range<usize> {
    if len * other.len() <= MAX_HUNK_PAIRS {
        return other;
    }
    let center = other.start + pos * other.len() / len;
    center.saturating_sub(HUNK_WINDOW).max(other.start)..(center + HUNK_WINDOW + 1).min(other.end)
}

/// Matches `content` against the concatenation of the lines produced by
/// `line` starting at `start`.
///
/// Whitespace between the parts is ignored and blank lines are skipped.  At
/// least two non-blank lines must be involved.  Returns the end of the
/// consumed lines.
fn split_match<'a>(
    content: &[u8],
    start: usize,
    end: usize,
    max_lines: usize,
    line: impl Fn(usize) -> Option<&'a [u8]>,
) -> Option<usize> {
    let mut rest = content;
    let mut parts = 0;
    let mut k = start;
    while k < end && parts < max_lines {
        let part = line(k)?;
        if part.is_empty() {
            if k == start {
                return None;
            }
            k += 1;
            continue;
        }
        if rest.first() != part.first() {
            return None;
        }
        parts += 1;
        if rest == part {
            return (parts >= 2).then_some(k + 1);
        }
        rest = trim_start(rest.strip_prefix(part)?);
        k += 1;
    }
    None
}

/// Removes update pairs that cross each other, starting with the pair that
/// crosses the most other pairs (ties remove the lower score first).
///
/// For very large sets the highest scoring set of pairs without crossings is
/// kept instead, as the paper's heuristic is quadratic.
fn remove_crossings(pairs: &mut Vec<(usize, usize, f64)>) {
    if pairs.len() > MAX_CROSSING_REMOVAL {
        keep_best_chain(pairs);
        return;
    }
    let crosses = |a: &(usize, usize, f64), b: &(usize, usize, f64)| {
        (a.0 < b.0 && a.1 > b.1) || (a.0 > b.0 && a.1 < b.1)
    };
    let mut counts = pairs
        .iter()
        .map(|a| pairs.iter().filter(|b| crosses(a, b)).count())
        .collect::<Vec<_>>();
    let mut alive = vec![true; pairs.len()];
    loop {
        let mut worst: Option<usize> = None;
        for idx in 0..pairs.len() {
            if !alive[idx] || counts[idx] == 0 {
                continue;
            }
            worst = match worst {
                Some(w)
                    if counts[w] > counts[idx]
                        || (counts[w] == counts[idx] && pairs[w].2 <= pairs[idx].2) =>
                {
                    Some(w)
                }
                _ => Some(idx),
            };
        }
        let Some(worst) = worst else { break };
        alive[worst] = false;
        for idx in 0..pairs.len() {
            if alive[idx] && crosses(&pairs[worst], &pairs[idx]) {
                counts[idx] -= 1;
            }
        }
    }
    let mut idx = 0;
    pairs.retain(|_| {
        idx += 1;
        alive[idx - 1]
    });
}

/// Keeps the highest scoring subset of pairs in which no two pairs cross.
///
/// Pairs that share a line do not cross, so this is the heaviest chain that
/// is non-decreasing on both sides.
fn keep_best_chain(pairs: &mut Vec<(usize, usize, f64)>) {
    pairs.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
    let mut columns = pairs.iter().map(|p| p.1).collect::<Vec<_>>();
    columns.sort_unstable();
    columns.dedup();
    // Fenwick tree over the new lines holding the best chain ending at or
    // before a line, together with the pair that ends it.
    let mut tree = vec![(0.0f64, usize::MAX); columns.len() + 1];
    let mut prev = vec![usize::MAX; pairs.len()];
    let mut best = (0.0f64, usize::MAX);
    for (idx, pair) in pairs.iter().enumerate() {
        let col = columns.binary_search(&pair.1).unwrap() + 1;
        let mut query = (0.0f64, usize::MAX);
        let mut i = col;
        while i > 0 {
            if tree[i].0 > query.0 {
                query = tree[i];
            }
            i &= i - 1;
        }
        prev[idx] = query.1;
        let total = (query.0 + pair.2, idx);
        if total.0 > best.0 {
            best = total;
        }
        let mut i = col;
        while i < tree.len() {
            if total.0 > tree[i].0 {
                tree[i] = total;
            }
            i += i & i.wrapping_neg();
        }
    }
    let mut keep = vec![false; pairs.len()];
    let mut cursor = best.1;
    while cursor != usize::MAX {
        keep[cursor] = true;
        cursor = prev[cursor];
    }
    let mut idx = 0;
    pairs.retain(|_| {
        idx += 1;
        keep[idx - 1]
    });
}

/// A coarse byte histogram, used to bound the similarity of two lines.
type Histogram = [u16; 32];

fn byte_histogram(s: &[u8]) -> Histogram {
    let mut rv = [0u16; 32];
    for &c in s {
        let bucket = &mut rv[(c % 32) as usize];
        *bucket = bucket.saturating_add(1);
    }
    rv
}

/// An upper bound for [`indel_ratio`]: the LCS cannot be longer than the
/// number of bytes both sides have in common.
fn shared_bytes_ratio(a: &Histogram, b: &Histogram, total_len: usize) -> f64 {
    if total_len == 0 {
        return 1.0;
    }
    let shared = a
        .iter()
        .zip(b)
        .map(|(x, y)| (*x).min(*y) as usize)
        .sum::<usize>();
    2.0 * shared as f64 / total_len as f64
}

/// Returns up to `n` non-blank lines before `start` and after `end`.
fn context_lines(lines: &[LineInfo<'_>], start: usize, end: usize, n: usize) -> Vec<u32> {
    let mut rv = lines[..start]
        .iter()
        .rev()
        .filter(|line| !line.is_blank())
        .take(n)
        .map(|line| line.id)
        .collect::<Vec<_>>();
    rv.reverse();
    rv.extend(
        lines[end..]
            .iter()
            .filter(|line| !line.is_blank())
            .take(n)
            .map(|line| line.id),
    );
    rv
}

/// Picks the candidates for one connected group of vertices.
fn select_component(
    edges: &[((usize, usize), usize)],
    candidates: &[Candidate],
    selected: &mut Vec<usize>,
) {
    let mut lefts = edges.iter().map(|e| e.0.0).collect::<Vec<_>>();
    let mut rights = edges.iter().map(|e| e.0.1).collect::<Vec<_>>();
    lefts.sort_unstable();
    lefts.dedup();
    rights.sort_unstable();
    rights.dedup();

    if lefts.len().max(rights.len()) > MAX_ASSIGNMENT_SIZE {
        let mut order = edges.to_vec();
        order.sort_by(|a, b| candidates[a.1].cost.total_cmp(&candidates[b.1].cost));
        let mut left_used = BTreeMap::new();
        let mut right_used = BTreeMap::new();
        for ((l, r), idx) in order {
            if !left_used.contains_key(&l) && !right_used.contains_key(&r) {
                left_used.insert(l, ());
                right_used.insert(r, ());
                selected.push(idx);
            }
        }
        return;
    }

    // The assignment solver needs rows <= columns.
    let transpose = lefts.len() > rights.len();
    let (rows, cols) = if transpose {
        (&rights, &lefts)
    } else {
        (&lefts, &rights)
    };
    // Non-edges cost nothing, which is the same as leaving a vertex
    // unmatched.  All edges have a negative cost.
    let mut cost = vec![0.0; rows.len() * cols.len()];
    let mut edge_at = vec![None; rows.len() * cols.len()];
    for &((l, r), idx) in edges {
        let (row_vertex, col_vertex) = if transpose { (r, l) } else { (l, r) };
        let row = rows.binary_search(&row_vertex).unwrap();
        let col = cols.binary_search(&col_vertex).unwrap();
        cost[row * cols.len() + col] = candidates[idx].cost;
        edge_at[row * cols.len() + col] = Some(idx);
    }
    for (row, col) in assignment::solve(&cost, rows.len(), cols.len())
        .into_iter()
        .enumerate()
    {
        if let Some(idx) = edge_at[row * cols.len() + col] {
            selected.push(idx);
        }
    }
}

/// Orders candidates so that a [`BinaryHeap`] pops the cheapest first.
struct ByCost(Candidate);

impl PartialEq for ByCost {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == core::cmp::Ordering::Equal
    }
}

impl Eq for ByCost {}

impl PartialOrd for ByCost {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ByCost {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        let key = |c: &Candidate| (c.kind, c.old_start, c.new_start, c.len);
        other
            .0
            .cost
            .total_cmp(&self.0.cost)
            .then_with(|| key(&other.0).cmp(&key(&self.0)))
    }
}

struct UnionFind {
    parent: Vec<usize>,
}

impl UnionFind {
    fn new(len: usize) -> UnionFind {
        UnionFind {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, mut idx: usize) -> usize {
        while self.parent[idx] != idx {
            self.parent[idx] = self.parent[self.parent[idx]];
            idx = self.parent[idx];
        }
        idx
    }

    fn union(&mut self, a: usize, b: usize) {
        let (a, b) = (self.find(a), self.find(b));
        if a != b {
            self.parent[a.max(b)] = a.min(b);
        }
    }
}

#[cfg(test)]
mod tests;

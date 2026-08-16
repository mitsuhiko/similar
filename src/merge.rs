//! Three-way, line-oriented text merging.
//!
//! A merge compares `base` to two descendants, conventionally called `ours`
//! and `theirs`. Non-overlapping edits are combined automatically while
//! overlapping, non-identical edits are retained as structured conflicts.
//!
//! ```rust
//! use similar::{MergeResolution, TextMerge};
//!
//! let merge = TextMerge::from_lines(
//!     "one\ntwo\nthree\n",
//!     "ONE\ntwo\nthree\n",
//!     "one\ntwo\nTHREE\n",
//! );
//!
//! assert!(!merge.is_conflicted());
//! assert_eq!(merge.to_string(), "ONE\ntwo\nTHREE\n");
//! assert!(merge.regions().iter().any(|region| {
//!     region.resolution() == MergeResolution::Ours
//! }));
//! assert!(merge.regions().iter().any(|region| {
//!     region.resolution() == MergeResolution::Theirs
//! }));
//! ```

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use core::ops::Range;

#[cfg(feature = "std")]
use std::io;

use crate::text::{DiffableStr, IntoDiffInput, TextDiffSide, WhitespaceKey, WhitespaceMode};
use crate::{Algorithm, DiffOp, capture_diff};

/// Describes how a region of a three-way merge was resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum MergeResolution {
    /// None of the three inputs changed in this region.
    Unchanged,
    /// Only the `ours` input changed this region.
    Ours,
    /// Only the `theirs` input changed this region.
    Theirs,
    /// Both inputs made an equivalent change.
    Both,
    /// The inputs made incompatible changes.
    Conflict,
}

/// A region in a three-way merge.
///
/// The ranges refer to line indices in the respective original inputs. The
/// inputs themselves are retained by [`TextMerge`], so no line data is copied
/// into a region.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeRegion {
    resolution: MergeResolution,
    base_range: Range<usize>,
    ours_range: Range<usize>,
    theirs_range: Range<usize>,
}

impl MergeRegion {
    /// Returns how this region was resolved.
    pub fn resolution(&self) -> MergeResolution {
        self.resolution
    }

    /// Returns the corresponding range in the common ancestor.
    pub fn base_range(&self) -> Range<usize> {
        self.base_range.clone()
    }

    /// Returns the corresponding range in `ours`.
    pub fn ours_range(&self) -> Range<usize> {
        self.ours_range.clone()
    }

    /// Returns the corresponding range in `theirs`.
    pub fn theirs_range(&self) -> Range<usize> {
        self.theirs_range.clone()
    }

    /// Returns `true` if this region is a conflict.
    pub fn is_conflict(&self) -> bool {
        self.resolution == MergeResolution::Conflict
    }
}

/// Controls how conflicts are rendered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[non_exhaustive]
pub enum ConflictStyle {
    /// Render the two descendants, separated by conflict markers.
    #[default]
    Merge,
    /// Also render the common ancestor between `|||||||` and `=======`.
    Diff3,
}

/// Configuration for a line-oriented three-way merge.
#[derive(Clone, Debug, Default)]
pub struct TextMergeConfig {
    algorithm: Algorithm,
    whitespace_mode: WhitespaceMode,
}

impl TextMergeConfig {
    /// Creates a default merge configuration.
    pub const fn new() -> Self {
        TextMergeConfig {
            algorithm: Algorithm::Myers,
            whitespace_mode: WhitespaceMode::Exact,
        }
    }

    /// Changes the algorithm used for the two base-to-descendant diffs.
    pub fn algorithm(&mut self, algorithm: Algorithm) -> &mut Self {
        self.algorithm = algorithm;
        self
    }

    /// Changes how whitespace is compared in lines.
    ///
    /// Original lines are retained for output. This has the same semantics as
    /// [`crate::TextDiffConfig::whitespace_mode`].
    pub fn whitespace_mode(&mut self, mode: WhitespaceMode) -> &mut Self {
        self.whitespace_mode = mode;
        self
    }

    /// Merges three inputs after splitting them into newline-terminated lines.
    pub fn merge_lines<'base, 'ours, 'theirs, Base, Ours, Theirs, T>(
        &self,
        base: Base,
        ours: Ours,
        theirs: Theirs,
    ) -> TextMerge<'base, 'ours, 'theirs, T>
    where
        Base: IntoDiffInput<'base, Output = T>,
        Ours: IntoDiffInput<'ours, Output = T>,
        Theirs: IntoDiffInput<'theirs, Output = T>,
        T: DiffableStr + ?Sized,
    {
        let base =
            TextDiffSide::from_tokenized(base.into_diff_input(), DiffableStr::tokenize_lines);
        let ours =
            TextDiffSide::from_tokenized(ours.into_diff_input(), DiffableStr::tokenize_lines);
        let theirs =
            TextDiffSide::from_tokenized(theirs.into_diff_input(), DiffableStr::tokenize_lines);
        let regions = build_regions(self.algorithm, self.whitespace_mode, &base, &ours, &theirs);

        TextMerge {
            base,
            ours,
            theirs,
            regions,
            algorithm: self.algorithm,
            whitespace_mode: self.whitespace_mode,
            conflict_style: ConflictStyle::Merge,
            marker_size: 7,
            ancestor_label: "base".to_string(),
            ours_label: "ours".to_string(),
            theirs_label: "theirs".to_string(),
        }
    }
}

/// The result of a line-oriented three-way merge.
///
/// The result retains all three inputs and represents merged regions as ranges
/// into them. Formatting the value produces the merged text and inserts
/// conflict markers for unresolved regions.
pub struct TextMerge<'base, 'ours, 'theirs, T: DiffableStr + ?Sized> {
    base: TextDiffSide<'base, T>,
    ours: TextDiffSide<'ours, T>,
    theirs: TextDiffSide<'theirs, T>,
    regions: Vec<MergeRegion>,
    algorithm: Algorithm,
    whitespace_mode: WhitespaceMode,
    conflict_style: ConflictStyle,
    marker_size: usize,
    ancestor_label: String,
    ours_label: String,
    theirs_label: String,
}

impl TextMerge<'_, '_, '_, str> {
    /// Creates a merge configuration.
    pub const fn configure() -> TextMergeConfig {
        TextMergeConfig::new()
    }

    /// Merges three line-oriented inputs using the default configuration.
    pub fn from_lines<'base, 'ours, 'theirs, Base, Ours, Theirs, T>(
        base: Base,
        ours: Ours,
        theirs: Theirs,
    ) -> TextMerge<'base, 'ours, 'theirs, T>
    where
        Base: IntoDiffInput<'base, Output = T>,
        Ours: IntoDiffInput<'ours, Output = T>,
        Theirs: IntoDiffInput<'theirs, Output = T>,
        T: DiffableStr + ?Sized,
    {
        TextMergeConfig::new().merge_lines(base, ours, theirs)
    }
}

impl<'base, 'ours, 'theirs, T: DiffableStr + ?Sized> TextMerge<'base, 'ours, 'theirs, T> {
    /// Returns all structured merge regions.
    pub fn regions(&self) -> &[MergeRegion] {
        &self.regions
    }

    /// Iterates over unresolved regions.
    pub fn conflicts(&self) -> impl Iterator<Item = &MergeRegion> {
        self.regions.iter().filter(|region| region.is_conflict())
    }

    /// Returns the number of unresolved regions.
    pub fn conflict_count(&self) -> usize {
        self.conflicts().count()
    }

    /// Returns `true` if the merge contains at least one conflict.
    pub fn is_conflicted(&self) -> bool {
        self.conflicts().next().is_some()
    }

    /// Returns the algorithm used for the base-to-descendant diffs.
    pub fn algorithm(&self) -> Algorithm {
        self.algorithm
    }

    /// Returns the whitespace comparison mode used for this merge.
    pub fn whitespace_mode(&self) -> WhitespaceMode {
        self.whitespace_mode
    }

    /// Changes the conflict output style.
    pub fn conflict_style(&mut self, style: ConflictStyle) -> &mut Self {
        self.conflict_style = style;
        self
    }

    /// Changes the number of characters in each conflict marker.
    ///
    /// A size of zero resets the marker size to the default of seven.
    pub fn conflict_marker_size(&mut self, size: usize) -> &mut Self {
        self.marker_size = if size == 0 { 7 } else { size };
        self
    }

    /// Changes the labels shown in conflict markers.
    pub fn labels(&mut self, ancestor: &str, ours: &str, theirs: &str) -> &mut Self {
        self.ancestor_label = ancestor.to_string();
        self.ours_label = ours.to_string();
        self.theirs_label = theirs.to_string();
        self
    }

    /// Returns the number of lines in the common ancestor.
    pub fn base_len(&self) -> usize {
        self.base.len()
    }

    /// Returns the number of lines in `ours`.
    pub fn ours_len(&self) -> usize {
        self.ours.len()
    }

    /// Returns the number of lines in `theirs`.
    pub fn theirs_len(&self) -> usize {
        self.theirs.len()
    }

    /// Returns a line from the common ancestor.
    pub fn base_line(&self, index: usize) -> Option<&T> {
        self.base.get(index)
    }

    /// Returns a line from `ours`.
    pub fn ours_line(&self, index: usize) -> Option<&T> {
        self.ours.get(index)
    }

    /// Returns a line from `theirs`.
    pub fn theirs_line(&self, index: usize) -> Option<&T> {
        self.theirs.get(index)
    }

    /// Writes the merged result as bytes.
    #[cfg(feature = "std")]
    pub fn to_writer<W: io::Write>(&self, mut writer: W) -> Result<(), io::Error> {
        let mut at_line_start = true;
        for region in &self.regions {
            match region.resolution {
                MergeResolution::Unchanged | MergeResolution::Ours | MergeResolution::Both => {
                    write_range_bytes(
                        &mut writer,
                        &self.ours,
                        region.ours_range.clone(),
                        &mut at_line_start,
                    )?;
                }
                MergeResolution::Theirs => {
                    write_range_bytes(
                        &mut writer,
                        &self.theirs,
                        region.theirs_range.clone(),
                        &mut at_line_start,
                    )?;
                }
                MergeResolution::Conflict => {
                    if !at_line_start {
                        writer.write_all(b"\n")?;
                    }
                    write_marker_bytes(
                        &mut writer,
                        b'<',
                        self.marker_size,
                        Some(&self.ours_label),
                    )?;
                    at_line_start = true;
                    write_range_bytes(
                        &mut writer,
                        &self.ours,
                        region.ours_range.clone(),
                        &mut at_line_start,
                    )?;
                    terminate_bytes(&mut writer, &mut at_line_start)?;

                    if self.conflict_style == ConflictStyle::Diff3 {
                        write_marker_bytes(
                            &mut writer,
                            b'|',
                            self.marker_size,
                            Some(&self.ancestor_label),
                        )?;
                        write_range_bytes(
                            &mut writer,
                            &self.base,
                            region.base_range.clone(),
                            &mut at_line_start,
                        )?;
                        terminate_bytes(&mut writer, &mut at_line_start)?;
                    }

                    write_marker_bytes(&mut writer, b'=', self.marker_size, None)?;
                    write_range_bytes(
                        &mut writer,
                        &self.theirs,
                        region.theirs_range.clone(),
                        &mut at_line_start,
                    )?;
                    terminate_bytes(&mut writer, &mut at_line_start)?;
                    write_marker_bytes(
                        &mut writer,
                        b'>',
                        self.marker_size,
                        Some(&self.theirs_label),
                    )?;
                    at_line_start = true;
                }
            }
        }
        Ok(())
    }
}

impl<T: DiffableStr + ?Sized> fmt::Display for TextMerge<'_, '_, '_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut at_line_start = true;
        for region in &self.regions {
            match region.resolution {
                MergeResolution::Unchanged | MergeResolution::Ours | MergeResolution::Both => {
                    fmt_range(f, &self.ours, region.ours_range.clone(), &mut at_line_start)?;
                }
                MergeResolution::Theirs => {
                    fmt_range(
                        f,
                        &self.theirs,
                        region.theirs_range.clone(),
                        &mut at_line_start,
                    )?;
                }
                MergeResolution::Conflict => {
                    if !at_line_start {
                        writeln!(f)?;
                    }
                    fmt_marker(f, '<', self.marker_size, Some(&self.ours_label))?;
                    at_line_start = true;
                    fmt_range(f, &self.ours, region.ours_range.clone(), &mut at_line_start)?;
                    terminate_fmt(f, &mut at_line_start)?;

                    if self.conflict_style == ConflictStyle::Diff3 {
                        fmt_marker(f, '|', self.marker_size, Some(&self.ancestor_label))?;
                        fmt_range(f, &self.base, region.base_range.clone(), &mut at_line_start)?;
                        terminate_fmt(f, &mut at_line_start)?;
                    }

                    fmt_marker(f, '=', self.marker_size, None)?;
                    fmt_range(
                        f,
                        &self.theirs,
                        region.theirs_range.clone(),
                        &mut at_line_start,
                    )?;
                    terminate_fmt(f, &mut at_line_start)?;
                    fmt_marker(f, '>', self.marker_size, Some(&self.theirs_label))?;
                    at_line_start = true;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct SideEdit {
    base: Range<usize>,
    side: Range<usize>,
}

fn capture_edits<T: DiffableStr + ?Sized>(
    algorithm: Algorithm,
    base: &TextDiffSide<'_, T>,
    side: &TextDiffSide<'_, T>,
    whitespace_mode: WhitespaceMode,
) -> Vec<SideEdit> {
    let ops = if whitespace_mode == WhitespaceMode::Exact {
        capture_diff(algorithm, base, 0..base.len(), side, 0..side.len())
    } else {
        let base_keys = base
            .iter()
            .map(|line| WhitespaceKey {
                bytes: line.as_bytes(),
                mode: whitespace_mode,
            })
            .collect::<Vec<_>>();
        let side_keys = side
            .iter()
            .map(|line| WhitespaceKey {
                bytes: line.as_bytes(),
                mode: whitespace_mode,
            })
            .collect::<Vec<_>>();
        capture_diff(
            algorithm,
            &base_keys,
            0..base_keys.len(),
            &side_keys,
            0..side_keys.len(),
        )
    };

    let mut edits: Vec<SideEdit> = Vec::new();
    for op in ops {
        if matches!(op, DiffOp::Equal { .. }) {
            continue;
        }
        let edit = SideEdit {
            base: op.old_range(),
            side: op.new_range(),
        };
        let joins_previous = edits.last().is_some_and(|previous| {
            previous.base.end == edit.base.start && previous.side.end == edit.side.start
        });
        if joins_previous {
            let previous = edits.last_mut().expect("edit disappeared");
            previous.base.end = edit.base.end;
            previous.side.end = edit.side.end;
        } else {
            edits.push(edit);
        }
    }
    edits
}

fn build_regions<T: DiffableStr + ?Sized>(
    algorithm: Algorithm,
    whitespace_mode: WhitespaceMode,
    base: &TextDiffSide<'_, T>,
    ours: &TextDiffSide<'_, T>,
    theirs: &TextDiffSide<'_, T>,
) -> Vec<MergeRegion> {
    let ours_edits = capture_edits(algorithm, base, ours, whitespace_mode);
    let theirs_edits = capture_edits(algorithm, base, theirs, whitespace_mode);
    let mut regions = Vec::new();
    let mut ours_index = 0;
    let mut theirs_index = 0;
    let mut base_cursor = 0;

    while ours_index < ours_edits.len() || theirs_index < theirs_edits.len() {
        match (ours_edits.get(ours_index), theirs_edits.get(theirs_index)) {
            (Some(ours_edit), Some(theirs_edit)) if edits_overlap(ours_edit, theirs_edit) => {
                push_unchanged(
                    &mut regions,
                    base_cursor,
                    ours_edit.base.start.min(theirs_edit.base.start),
                    &ours_edits,
                    &theirs_edits,
                );

                let ours_begin = ours_index;
                let theirs_begin = theirs_index;
                let mut cluster_start = ours_edit.base.start.min(theirs_edit.base.start);
                let mut cluster_end = ours_edit.base.end.max(theirs_edit.base.end);
                ours_index += 1;
                theirs_index += 1;

                loop {
                    let mut changed = false;
                    if let Some(edit) = ours_edits
                        .get(ours_index)
                        .filter(|edit| overlaps_cluster(edit, cluster_start, cluster_end))
                    {
                        cluster_start = cluster_start.min(edit.base.start);
                        cluster_end = cluster_end.max(edit.base.end);
                        ours_index += 1;
                        changed = true;
                    }
                    if let Some(edit) = theirs_edits
                        .get(theirs_index)
                        .filter(|edit| overlaps_cluster(edit, cluster_start, cluster_end))
                    {
                        cluster_start = cluster_start.min(edit.base.start);
                        cluster_end = cluster_end.max(edit.base.end);
                        theirs_index += 1;
                        changed = true;
                    }
                    if !changed {
                        break;
                    }
                }

                let base_range = cluster_start..cluster_end;
                let ours_range =
                    cluster_side_range(&ours_edits, ours_begin..ours_index, base_range.clone());
                let theirs_range = cluster_side_range(
                    &theirs_edits,
                    theirs_begin..theirs_index,
                    base_range.clone(),
                );
                let resolution = if ranges_equal(
                    ours,
                    ours_range.clone(),
                    theirs,
                    theirs_range.clone(),
                    whitespace_mode,
                ) {
                    MergeResolution::Both
                } else {
                    MergeResolution::Conflict
                };
                regions.push(MergeRegion {
                    resolution,
                    base_range: base_range.clone(),
                    ours_range,
                    theirs_range,
                });
                base_cursor = base_cursor.max(base_range.end);
            }
            (Some(ours_edit), Some(theirs_edit)) => {
                let take_ours = edit_is_before(ours_edit, theirs_edit);
                if take_ours {
                    push_single_region(
                        &mut regions,
                        MergeResolution::Ours,
                        ours_edit,
                        base_cursor,
                        &ours_edits,
                        &theirs_edits,
                    );
                    base_cursor = base_cursor.max(ours_edit.base.end);
                    ours_index += 1;
                } else {
                    push_single_region(
                        &mut regions,
                        MergeResolution::Theirs,
                        theirs_edit,
                        base_cursor,
                        &ours_edits,
                        &theirs_edits,
                    );
                    base_cursor = base_cursor.max(theirs_edit.base.end);
                    theirs_index += 1;
                }
            }
            (Some(ours_edit), None) => {
                push_single_region(
                    &mut regions,
                    MergeResolution::Ours,
                    ours_edit,
                    base_cursor,
                    &ours_edits,
                    &theirs_edits,
                );
                base_cursor = base_cursor.max(ours_edit.base.end);
                ours_index += 1;
            }
            (None, Some(theirs_edit)) => {
                push_single_region(
                    &mut regions,
                    MergeResolution::Theirs,
                    theirs_edit,
                    base_cursor,
                    &ours_edits,
                    &theirs_edits,
                );
                base_cursor = base_cursor.max(theirs_edit.base.end);
                theirs_index += 1;
            }
            (None, None) => break,
        }
    }

    push_unchanged(
        &mut regions,
        base_cursor,
        base.len(),
        &ours_edits,
        &theirs_edits,
    );
    regions
}

fn push_single_region(
    regions: &mut Vec<MergeRegion>,
    resolution: MergeResolution,
    edit: &SideEdit,
    base_cursor: usize,
    ours_edits: &[SideEdit],
    theirs_edits: &[SideEdit],
) {
    push_unchanged(
        regions,
        base_cursor,
        edit.base.start,
        ours_edits,
        theirs_edits,
    );
    let base_range = edit.base.clone();
    let ours_range = if resolution == MergeResolution::Ours {
        edit.side.clone()
    } else {
        project_range(ours_edits, base_range.clone())
    };
    let theirs_range = if resolution == MergeResolution::Theirs {
        edit.side.clone()
    } else {
        project_range(theirs_edits, base_range.clone())
    };
    regions.push(MergeRegion {
        resolution,
        base_range,
        ours_range,
        theirs_range,
    });
}

fn push_unchanged(
    regions: &mut Vec<MergeRegion>,
    start: usize,
    end: usize,
    ours_edits: &[SideEdit],
    theirs_edits: &[SideEdit],
) {
    if start >= end {
        return;
    }
    regions.push(MergeRegion {
        resolution: MergeResolution::Unchanged,
        base_range: start..end,
        ours_range: project_range(ours_edits, start..end),
        theirs_range: project_range(theirs_edits, start..end),
    });
}

fn edits_overlap(a: &SideEdit, b: &SideEdit) -> bool {
    // Like xdiff, changes that merely touch at a base boundary belong to the
    // same merge region. This is important for an insertion immediately next
    // to a replacement: there is no unchanged base line separating them.
    a.base.start <= b.base.end && b.base.start <= a.base.end
}

fn overlaps_cluster(edit: &SideEdit, start: usize, end: usize) -> bool {
    edit.base.start <= end && start <= edit.base.end
}

fn edit_is_before(a: &SideEdit, b: &SideEdit) -> bool {
    if a.base.start != b.base.start {
        a.base.start < b.base.start
    } else {
        // At the same boundary an insertion precedes an edit that consumes
        // base lines. Two insertions at the same boundary overlap.
        a.base.is_empty() && !b.base.is_empty()
    }
}

fn boundary_before(edits: &[SideEdit], position: usize) -> usize {
    let mut base_cursor = 0;
    let mut side_cursor = 0;
    for edit in edits {
        if position < edit.base.start {
            return side_cursor + position - base_cursor;
        }
        let side_at_start = side_cursor + edit.base.start - base_cursor;
        if position == edit.base.start {
            return side_at_start;
        }
        if position < edit.base.end {
            return edit.side.start;
        }
        base_cursor = edit.base.end;
        side_cursor = edit.side.end;
    }
    side_cursor + position - base_cursor
}

fn boundary_after_insertions(edits: &[SideEdit], position: usize) -> usize {
    let mut mapped = boundary_before(edits, position);
    for edit in edits {
        if edit.base.start > position {
            break;
        }
        if edit.base.is_empty() && edit.base.start == position {
            mapped = mapped.max(edit.side.end);
        }
    }
    mapped
}

fn project_range(edits: &[SideEdit], range: Range<usize>) -> Range<usize> {
    if range.is_empty() {
        let position = boundary_before(edits, range.start);
        position..position
    } else {
        boundary_after_insertions(edits, range.start)..boundary_before(edits, range.end)
    }
}

fn cluster_side_range(
    edits: &[SideEdit],
    selected: Range<usize>,
    base_range: Range<usize>,
) -> Range<usize> {
    if !base_range.is_empty() {
        // Boundary insertions are part of an overlapping cluster, unlike the
        // unchanged ranges on either side of it.
        return boundary_before(edits, base_range.start)
            ..boundary_after_insertions(edits, base_range.end);
    }
    let selected = &edits[selected];
    selected
        .first()
        .zip(selected.last())
        .map(|(first, last)| first.side.start..last.side.end)
        .unwrap_or_else(|| {
            let position = boundary_before(edits, base_range.start);
            position..position
        })
}

fn ranges_equal<T: DiffableStr + ?Sized>(
    ours: &TextDiffSide<'_, T>,
    ours_range: Range<usize>,
    theirs: &TextDiffSide<'_, T>,
    theirs_range: Range<usize>,
    whitespace_mode: WhitespaceMode,
) -> bool {
    if ours_range.len() != theirs_range.len() {
        return false;
    }
    ours_range
        .zip(theirs_range)
        .all(|(ours_index, theirs_index)| {
            let ours = ours.get(ours_index).expect("merge range out of bounds");
            let theirs = theirs.get(theirs_index).expect("merge range out of bounds");
            if whitespace_mode == WhitespaceMode::Exact {
                ours == theirs
            } else {
                WhitespaceKey {
                    bytes: ours.as_bytes(),
                    mode: whitespace_mode,
                } == WhitespaceKey {
                    bytes: theirs.as_bytes(),
                    mode: whitespace_mode,
                }
            }
        })
}

fn fmt_range<T: DiffableStr + ?Sized>(
    f: &mut fmt::Formatter<'_>,
    side: &TextDiffSide<'_, T>,
    range: Range<usize>,
    at_line_start: &mut bool,
) -> fmt::Result {
    for index in range {
        let line = side.get(index).expect("merge range out of bounds");
        f.write_str(&line.to_string_lossy())?;
        *at_line_start = line.ends_with_newline();
    }
    Ok(())
}

fn fmt_marker(
    f: &mut fmt::Formatter<'_>,
    character: char,
    marker_size: usize,
    label: Option<&str>,
) -> fmt::Result {
    for _ in 0..marker_size {
        write!(f, "{character}")?;
    }
    if let Some(label) = label {
        write!(f, " {label}")?;
    }
    writeln!(f)
}

fn terminate_fmt(f: &mut fmt::Formatter<'_>, at_line_start: &mut bool) -> fmt::Result {
    if !*at_line_start {
        writeln!(f)?;
        *at_line_start = true;
    }
    Ok(())
}

#[cfg(feature = "std")]
fn write_range_bytes<T: DiffableStr + ?Sized, W: io::Write>(
    writer: &mut W,
    side: &TextDiffSide<'_, T>,
    range: Range<usize>,
    at_line_start: &mut bool,
) -> Result<(), io::Error> {
    for index in range {
        let line = side.get(index).expect("merge range out of bounds");
        writer.write_all(line.as_bytes())?;
        *at_line_start = line.ends_with_newline();
    }
    Ok(())
}

#[cfg(feature = "std")]
fn write_marker_bytes<W: io::Write>(
    writer: &mut W,
    character: u8,
    marker_size: usize,
    label: Option<&str>,
) -> Result<(), io::Error> {
    for _ in 0..marker_size {
        writer.write_all(&[character])?;
    }
    if let Some(label) = label {
        writer.write_all(b" ")?;
        writer.write_all(label.as_bytes())?;
    }
    writer.write_all(b"\n")
}

#[cfg(feature = "std")]
fn terminate_bytes<W: io::Write>(
    writer: &mut W,
    at_line_start: &mut bool,
) -> Result<(), io::Error> {
    if !*at_line_start {
        writer.write_all(b"\n")?;
        *at_line_start = true;
    }
    Ok(())
}

#[test]
fn test_non_overlapping_changes_merge_cleanly() {
    let merge = TextMerge::from_lines(
        "one\ntwo\nthree\nfour\n",
        "ONE\ntwo\nthree\nfour\n",
        "one\ntwo\nthree\nFOUR\n",
    );
    assert!(!merge.is_conflicted());
    assert_eq!(merge.to_string(), "ONE\ntwo\nthree\nFOUR\n");
}

#[test]
fn test_identical_changes_merge_cleanly() {
    let merge = TextMerge::from_lines("one\ntwo\n", "one\nTWO\n", "one\nTWO\n");
    assert!(!merge.is_conflicted());
    assert!(
        merge
            .regions()
            .iter()
            .any(|region| region.resolution() == MergeResolution::Both)
    );
    assert_eq!(merge.to_string(), "one\nTWO\n");
}

#[test]
fn test_conflicting_changes_are_structured_and_rendered() {
    let mut merge = TextMerge::from_lines("one\ntwo\n", "one\nours\n", "one\ntheirs\n");
    assert_eq!(merge.conflict_count(), 1);
    let conflict = merge.conflicts().next().unwrap();
    assert_eq!(conflict.base_range(), 1..2);
    assert_eq!(conflict.ours_range(), 1..2);
    assert_eq!(conflict.theirs_range(), 1..2);
    assert_eq!(
        merge.to_string(),
        "one\n<<<<<<< ours\nours\n=======\ntheirs\n>>>>>>> theirs\n"
    );

    merge
        .conflict_style(ConflictStyle::Diff3)
        .labels("ancestor", "left", "right");
    assert_eq!(
        merge.to_string(),
        "one\n<<<<<<< left\nours\n||||||| ancestor\ntwo\n=======\ntheirs\n>>>>>>> right\n"
    );
}

#[test]
fn test_conflicting_insertions() {
    let merge = TextMerge::from_lines("one\n", "ours\none\n", "theirs\none\n");
    assert!(merge.is_conflicted());
    assert_eq!(merge.conflicts().next().unwrap().base_range(), 0..0);
}

#[test]
fn test_insert_at_changed_range_boundary_conflicts() {
    let merge = TextMerge::from_lines("one\ntwo\n", "inserted\none\ntwo\n", "ONE\ntwo\n");
    assert!(merge.is_conflicted());
    assert_eq!(
        merge.to_string(),
        "<<<<<<< ours\ninserted\none\n=======\nONE\n>>>>>>> theirs\ntwo\n"
    );
}

#[test]
fn test_owned_inputs() {
    let merge = TextMerge::from_lines(
        String::from("one\ntwo\nthree\n"),
        String::from("ONE\ntwo\nthree\n"),
        String::from("one\ntwo\nTHREE\n"),
    );
    assert_eq!(merge.to_string(), "ONE\ntwo\nTHREE\n");
}

#[test]
fn test_whitespace_insensitive_merge() {
    let merge = TextMerge::configure()
        .whitespace_mode(WhitespaceMode::IgnoreChanges)
        .merge_lines("one\n", "  one\n", "one\n");
    assert!(!merge.is_conflicted());
    assert_eq!(merge.to_string(), "  one\n");
}

#[test]
fn test_conflict_adds_newlines_only_around_markers() {
    let merge = TextMerge::from_lines("base", "ours", "theirs");
    assert_eq!(
        merge.to_string(),
        "<<<<<<< ours\nours\n=======\ntheirs\n>>>>>>> theirs\n"
    );
}

#[test]
fn test_merge_identity_and_range_invariants() {
    fn inputs(max_len: usize) -> Vec<String> {
        let mut rv = Vec::new();
        for len in 0..=max_len {
            for bits in 0..(1usize << len) {
                let mut value = String::new();
                for index in 0..len {
                    value.push(if bits & (1 << index) == 0 { 'a' } else { 'b' });
                    value.push('\n');
                }
                rv.push(value);
            }
        }
        rv
    }

    let inputs = inputs(3);
    for base in &inputs {
        for ours in &inputs {
            for theirs in &inputs {
                let merge = TextMerge::from_lines(base, ours, theirs);
                for region in merge.regions() {
                    assert!(region.base_range().end <= merge.base_len());
                    assert!(region.ours_range().end <= merge.ours_len());
                    assert!(region.theirs_range().end <= merge.theirs_len());
                }

                let rendered = merge.to_string();
                if ours == base {
                    assert!(!merge.is_conflicted());
                    assert_eq!(&rendered, theirs);
                }
                if theirs == base || ours == theirs {
                    assert!(!merge.is_conflicted());
                    assert_eq!(&rendered, ours);
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "bytes")]
fn test_byte_merge_writer() {
    let merge = TextMerge::from_lines(
        &b"one\ntwo\nthree\n"[..],
        &b"ONE\ntwo\nthree\n"[..],
        &b"one\ntwo\nTHREE\n"[..],
    );
    let mut output = Vec::new();
    merge.to_writer(&mut output).unwrap();
    assert_eq!(output, b"ONE\ntwo\nTHREE\n");
}

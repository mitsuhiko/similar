//! Myers' diff algorithm.
//!
//! * time: `O((N+M)D)`
//! * space `O(N+M)`
//!
//! See [the original article by Eugene W. Myers](http://www.xmailserver.org/diff2.pdf)
//! describing it.
//!
//! The implementation of this algorithm is based on the implementation by
//! Brandon Williams.
//!
//! # Heuristics
//!
//! See [`crate::algorithms`] for the shared heuristics policy and the
//! `diff_deadline_raw` API.
//!
//! Before the search, the default entrypoints remove unmatched/confusing
//! records.
//!
//! The default entrypoints use a Git-style bounded middle-snake search. On
//! difficult inputs it accepts a good non-optimal split, or the best split
//! reached after a size-dependent search limit. This keeps Myers useful as a
//! general fallback at the cost of occasionally producing a non-minimal edit
//! script. [`diff_deadline_raw`] retains the raw shortest-path search.
//!
//! Both modes additionally apply exactness-preserving local shortcuts inside
//! the core recursion (prefix/suffix trimming, front-anchor peeling, and a
//! small-side exact fallback), with the optional deadline as a final safeguard.

use alloc::vec;
use alloc::vec::Vec;
use core::hash::Hash;
use core::ops::{Index, IndexMut, Range};

use crate::algorithms::utils::{common_prefix_len, common_suffix_len, is_empty_range};
use crate::algorithms::{DiffHook, preflight};
use crate::deadline_support::{Instant, deadline_exceeded};

/// Myers' diff algorithm.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
pub fn diff<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    diff_deadline(d, old, old_range, new, new_range, None)
}

/// Myers' diff algorithm with deadline.
///
/// Diff `old`, between indices `old_range` and `new` between indices `new_range`.
///
/// This diff is done with an optional deadline that defines the maximal
/// execution time permitted before it bails and falls back to an approximation.
pub fn diff_deadline<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    match preflight::reduce_for_myers(old, old_range.clone(), new, new_range.clone(), deadline) {
        Some(preflight::MyersPreflight::Reduced(reduced)) => diff_reduced(d, &reduced, deadline),
        Some(preflight::MyersPreflight::Trimmed(trimmed)) => {
            diff_deadline_pretrimmed(d, old, new, &trimmed, deadline, MyersMode::Heuristic)
        }
        None => diff_deadline_impl(
            d,
            old,
            old_range,
            new,
            new_range,
            deadline,
            MyersMode::Heuristic,
        ),
    }
}

/// Raw Myers diff algorithm with deadline and without heuristics that can
/// change the resulting edit script.
///
/// This preserves the historical bound profile and accepts non-hashable
/// value types as long as cross-type equality is available.
pub fn diff_deadline_raw<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, MyersMode::Raw)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MyersMode {
    Heuristic,
    Raw,
}

#[derive(Clone, Copy, Debug)]
struct MyersSearch {
    deadline: Option<Instant>,
    mode: MyersMode,
}

impl MyersSearch {
    fn with_mode(self, mode: MyersMode) -> Self {
        Self { mode, ..self }
    }
}

struct ReducedDiffHook<'a, D> {
    d: &'a mut D,
    old_indices: &'a [usize],
    new_indices: &'a [usize],
    old_cursor: usize,
    new_cursor: usize,
    old_end: usize,
    new_end: usize,
}

impl<D: DiffHook> ReducedDiffHook<'_, D> {
    fn emit_gaps(&mut self, old_anchor: usize, new_anchor: usize) -> Result<(), D::Error> {
        if self.old_cursor < old_anchor {
            self.d.delete(
                self.old_cursor,
                old_anchor - self.old_cursor,
                self.new_cursor,
            )?;
            self.old_cursor = old_anchor;
        }
        if self.new_cursor < new_anchor {
            self.d.insert(
                self.old_cursor,
                self.new_cursor,
                new_anchor - self.new_cursor,
            )?;
            self.new_cursor = new_anchor;
        }
        Ok(())
    }
}

impl<D: DiffHook> DiffHook for ReducedDiffHook<'_, D> {
    type Error = D::Error;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        let mut offset = 0usize;
        while offset < len {
            let old_anchor = self.old_indices[old_index + offset];
            let new_anchor = self.new_indices[new_index + offset];
            self.emit_gaps(old_anchor, new_anchor)?;

            let mut run_len = 1usize;
            while offset + run_len < len
                && self.old_indices[old_index + offset + run_len] == old_anchor + run_len
                && self.new_indices[new_index + offset + run_len] == new_anchor + run_len
            {
                run_len += 1;
            }
            self.d.equal(old_anchor, new_anchor, run_len)?;
            self.old_cursor = old_anchor + run_len;
            self.new_cursor = new_anchor + run_len;
            offset += run_len;
        }
        Ok(())
    }

    // Only equalities in the reduced sequence are anchors. Items deleted or
    // inserted there remain part of the original gaps emitted around the next
    // anchor (or by `finish`).
    fn delete(
        &mut self,
        _old_index: usize,
        _old_len: usize,
        _new_index: usize,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn insert(
        &mut self,
        _old_index: usize,
        _new_index: usize,
        _new_len: usize,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        self.emit_gaps(self.old_end, self.new_end)
    }
}

fn diff_reduced<D: DiffHook>(
    d: &mut D,
    reduced: &preflight::ReducedDiffInput,
    deadline: Option<Instant>,
) -> Result<(), D::Error> {
    let trimmed = &reduced.trimmed;
    if trimmed.prefix_len > 0 {
        d.equal(
            trimmed.original_old_range.start,
            trimmed.original_new_range.start,
            trimmed.prefix_len,
        )?;
    }

    let mut hook = ReducedDiffHook {
        d,
        old_indices: &reduced.old_indices,
        new_indices: &reduced.new_indices,
        old_cursor: trimmed.old_range.start,
        new_cursor: trimmed.new_range.start,
        old_end: trimmed.old_range.end,
        new_end: trimmed.new_range.end,
    };
    let old_len = reduced.old_values.len();
    let new_len = reduced.new_values.len();
    diff_deadline_impl(
        &mut hook,
        &reduced.old_values,
        0..old_len,
        &reduced.new_values,
        0..new_len,
        deadline,
        MyersMode::Heuristic,
    )?;

    if trimmed.suffix_len > 0 {
        d.equal(
            trimmed.original_old_range.end - trimmed.suffix_len,
            trimmed.original_new_range.end - trimmed.suffix_len,
            trimmed.suffix_len,
        )?;
    }
    d.finish()
}

fn diff_deadline_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    mode: MyersMode,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    // Size the working vectors for the changed middle, not the full inputs.
    // This is particularly important for large files with sparse edits: the
    // Myers vectors are linear in the ranges they are sized from, while a long
    // common prefix or suffix needs no search storage at all.
    let common_prefix_len = common_prefix_len(old, old_range.clone(), new, new_range.clone());
    let after_prefix_old = old_range.start + common_prefix_len..old_range.end;
    let after_prefix_new = new_range.start + common_prefix_len..new_range.end;
    let common_suffix_len =
        common_suffix_len(old, after_prefix_old.clone(), new, after_prefix_new.clone());
    let trimmed = preflight::TrimmedDiffInput {
        old_range: after_prefix_old.start..after_prefix_old.end - common_suffix_len,
        new_range: after_prefix_new.start..after_prefix_new.end - common_suffix_len,
        original_old_range: old_range,
        original_new_range: new_range,
        prefix_len: common_prefix_len,
        suffix_len: common_suffix_len,
    };
    diff_deadline_pretrimmed(d, old, new, &trimmed, deadline, mode)
}

fn diff_deadline_pretrimmed<Old, New, D>(
    d: &mut D,
    old: &Old,
    new: &New,
    trimmed: &preflight::TrimmedDiffInput,
    deadline: Option<Instant>,
    mode: MyersMode,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    if trimmed.prefix_len > 0 {
        d.equal(
            trimmed.original_old_range.start,
            trimmed.original_new_range.start,
            trimmed.prefix_len,
        )?;
    }

    if trimmed.old_range.is_empty() && trimmed.new_range.is_empty() {
        // Nothing to emit.
    } else if trimmed.old_range.is_empty() {
        d.insert(
            trimmed.old_range.start,
            trimmed.new_range.start,
            trimmed.new_range.len(),
        )?;
    } else if trimmed.new_range.is_empty() {
        d.delete(
            trimmed.old_range.start,
            trimmed.old_range.len(),
            trimmed.new_range.start,
        )?;
    } else {
        let max_d = max_d(trimmed.old_range.len(), trimmed.new_range.len());
        let mut vb = V::new(max_d);
        let mut vf = V::new(max_d);
        conquer(
            d,
            old,
            trimmed.old_range.clone(),
            new,
            trimmed.new_range.clone(),
            &mut vf,
            &mut vb,
            MyersSearch { deadline, mode },
        )?;
    }

    if trimmed.suffix_len > 0 {
        d.equal(
            trimmed.original_old_range.end - trimmed.suffix_len,
            trimmed.original_new_range.end - trimmed.suffix_len,
            trimmed.suffix_len,
        )?;
    }

    d.finish()
}

// A D-path is a path which starts at (0,0) that has exactly D non-diagonal
// edges. All D-paths consist of a (D - 1)-path followed by a non-diagonal edge
// and then a possibly empty sequence of diagonal edges called a snake.

/// `V` contains the endpoints of the furthest reaching `D-paths`. For each
/// recorded endpoint `(x,y)` in diagonal `k`, we only need to retain `x` because
/// `y` can be computed from `x - k`. In other words, `V` is an array of integers
/// where `V[k]` contains the row index of the endpoint of the furthest reaching
/// path in diagonal `k`.
///
/// We can't use a traditional Vec to represent `V` since we use `k` as an index
/// and it can take on negative values. So instead `V` is represented as a
/// light-weight wrapper around a Vec plus an `offset` which is the maximum value
/// `k` can take on in order to map negative `k`'s back to a value >= 0.
#[derive(Debug)]
struct V {
    offset: isize,
    v: Vec<usize>, // Look into initializing this to -1 and storing isize
}

impl V {
    fn new(max_d: usize) -> Self {
        Self {
            offset: max_d as isize,
            v: vec![0; 2 * max_d],
        }
    }

    fn len(&self) -> usize {
        self.v.len()
    }
}

impl Index<isize> for V {
    type Output = usize;

    fn index(&self, index: isize) -> &Self::Output {
        &self.v[(index + self.offset) as usize]
    }
}

impl IndexMut<isize> for V {
    fn index_mut(&mut self, index: isize) -> &mut Self::Output {
        &mut self.v[(index + self.offset) as usize]
    }
}

fn max_d(len1: usize, len2: usize) -> usize {
    // XXX look into reducing the need to have the additional '+ 1'
    (len1 + len2).div_ceil(2) + 1
}

const SMALL_SIDE_EXACT_MAX: usize = 64;
const SMALL_SIDE_EXACT_MIN_LARGE: usize = 512;
const SMALL_SIDE_EXACT_MAX_WORK: usize = 64_000_000;
const SMALL_SIDE_DEADLINE_CHECK_INTERVAL: usize = 1024;
const FRONT_ANCHOR_DEADLINE_CHECK_INTERVAL: usize = 1024;

// These follow the shape of Git/xdiff's practical Myers policy. Once the
// search has become expensive, a long diagonal is a useful split even when it
// is not known to lie on a shortest edit path. A separate size-dependent cap
// ensures that inputs without such a diagonal cannot make the search
// quadratic in their total length.
const HEURISTIC_MIN_COST: usize = 256;
const HEURISTIC_MIN_SNAKE: usize = 20;
const HEURISTIC_MIN_MAX_COST: usize = 256;
const HEURISTIC_PROGRESS_FACTOR: usize = 4;

#[inline(always)]
fn common_prefix_len_at<Old, New>(
    old: &Old,
    old_start: usize,
    old_end: usize,
    new: &New,
    new_start: usize,
    new_end: usize,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let max_len = (old_end - old_start).min(new_end - new_start);
    let mut matched = 0;
    while matched < max_len && new[new_start + matched] == old[old_start + matched] {
        matched += 1;
    }
    matched
}

#[inline(always)]
fn common_prefix_len_at_deadline<Old, New>(
    old: &Old,
    old_start: usize,
    old_end: usize,
    new: &New,
    new_start: usize,
    new_end: usize,
    deadline: Option<Instant>,
) -> Option<usize>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let max_len = (old_end - old_start).min(new_end - new_start);
    let mut matched = 0;
    while matched < max_len {
        if (matched & (FRONT_ANCHOR_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return None;
        }
        if new[new_start + matched] != old[old_start + matched] {
            break;
        }
        matched += 1;
    }
    Some(matched)
}

#[inline(always)]
fn common_suffix_len_at<Old, New>(
    old: &Old,
    old_start: usize,
    old_end: usize,
    new: &New,
    new_start: usize,
    new_end: usize,
) -> usize
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let max_len = (old_end - old_start).min(new_end - new_start);
    let mut matched = 0;
    while matched < max_len && new[new_end - 1 - matched] == old[old_end - 1 - matched] {
        matched += 1;
    }
    matched
}

#[inline(always)]
fn split_at(range: Range<usize>, at: usize) -> (Range<usize>, Range<usize>) {
    (range.start..at, at..range.end)
}

#[inline(always)]
fn maybe_emit_small_side_exact<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<bool, D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    if deadline_exceeded(deadline) {
        return Ok(false);
    }

    let old_len = old_range.len();
    let new_len = new_range.len();
    let small = old_len.min(new_len);
    let large = old_len.max(new_len);
    let work = old_len.saturating_mul(new_len);

    if small == 0
        || small > SMALL_SIDE_EXACT_MAX
        || large < SMALL_SIDE_EXACT_MIN_LARGE
        || work > SMALL_SIDE_EXACT_MAX_WORK
    {
        return Ok(false);
    }

    if old_len <= new_len {
        emit_small_old_exact(d, old, old_range, new, new_range, deadline)
    } else {
        emit_small_new_exact(d, old, old_range, new, new_range, deadline)
    }
}

fn emit_small_old_exact<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<bool, D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    if deadline_exceeded(deadline) {
        return Ok(false);
    }

    let n = old_range.len();
    let m = new_range.len();
    let width = m + 1;

    // dp[i][j] = LCS length for old[i..] vs new[j..]
    let mut dp = vec![0u8; (n + 1) * width];
    for i in (0..n).rev() {
        if deadline_exceeded(deadline) {
            return Ok(false);
        }

        let row = i * width;
        let next_row = (i + 1) * width;
        for j in (0..m).rev() {
            if (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0) && deadline_exceeded(deadline) {
                return Ok(false);
            }

            dp[row + j] = if new[new_range.start + j] == old[old_range.start + i] {
                dp[next_row + j + 1] + 1
            } else {
                dp[next_row + j].max(dp[row + j + 1])
            };
        }
    }

    let mut emitted_any = false;
    let mut i = 0;
    let mut j = 0;
    while i < n && j < m {
        if !emitted_any
            && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Ok(false);
        }

        let row = i * width;
        let old_idx = old_range.start + i;
        let new_idx = new_range.start + j;

        if new[new_idx] == old[old_idx] && dp[row + j] == dp[(i + 1) * width + j + 1] + 1 {
            let start_i = i;
            let start_j = j;
            while i < n && j < m {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                let row = i * width;
                let old_idx = old_range.start + i;
                let new_idx = new_range.start + j;
                if new[new_idx] == old[old_idx] && dp[row + j] == dp[(i + 1) * width + j + 1] + 1 {
                    i += 1;
                    j += 1;
                } else {
                    break;
                }
            }
            d.equal(
                old_range.start + start_i,
                new_range.start + start_j,
                i - start_i,
            )?;
            emitted_any = true;
        } else if dp[(i + 1) * width + j] >= dp[row + j + 1] {
            let start_i = i;
            let new_idx = new_range.start + j;
            while i < n {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                if j >= m {
                    i = n;
                    break;
                }
                let row = i * width;
                let old_idx = old_range.start + i;
                let new_idx_curr = new_range.start + j;
                if new[new_idx_curr] == old[old_idx]
                    && dp[row + j] == dp[(i + 1) * width + j + 1] + 1
                {
                    break;
                }
                if dp[(i + 1) * width + j] >= dp[row + j + 1] {
                    i += 1;
                } else {
                    break;
                }
            }
            d.delete(old_range.start + start_i, i - start_i, new_idx)?;
            emitted_any = true;
        } else {
            let start_j = j;
            let old_idx = old_range.start + i;
            while j < m {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                if i >= n {
                    j = m;
                    break;
                }
                let row = i * width;
                let old_idx_curr = old_range.start + i;
                let new_idx = new_range.start + j;
                if new[new_idx] == old[old_idx_curr]
                    && dp[row + j] == dp[(i + 1) * width + j + 1] + 1
                {
                    break;
                }
                if dp[(i + 1) * width + j] < dp[row + j + 1] {
                    j += 1;
                } else {
                    break;
                }
            }
            d.insert(old_idx, new_range.start + start_j, j - start_j)?;
            emitted_any = true;
        }
    }

    if i < n {
        d.delete(old_range.start + i, n - i, new_range.start + j)?;
    }
    if j < m {
        d.insert(old_range.start + i, new_range.start + j, m - j)?;
    }

    Ok(true)
}

fn emit_small_new_exact<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
) -> Result<bool, D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    if deadline_exceeded(deadline) {
        return Ok(false);
    }

    let n = old_range.len();
    let m = new_range.len();
    let width = n + 1;

    // dp[i][j] = LCS length for new[i..] vs old[j..]
    let mut dp = vec![0u8; (m + 1) * width];
    for i in (0..m).rev() {
        if deadline_exceeded(deadline) {
            return Ok(false);
        }

        let row = i * width;
        let next_row = (i + 1) * width;
        for j in (0..n).rev() {
            if (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0) && deadline_exceeded(deadline) {
                return Ok(false);
            }

            dp[row + j] = if new[new_range.start + i] == old[old_range.start + j] {
                dp[next_row + j + 1] + 1
            } else {
                dp[next_row + j].max(dp[row + j + 1])
            };
        }
    }

    let mut emitted_any = false;
    let mut i = 0;
    let mut j = 0;
    while i < m && j < n {
        if !emitted_any
            && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
            && deadline_exceeded(deadline)
        {
            return Ok(false);
        }

        let row = i * width;
        let old_idx = old_range.start + j;
        let new_idx = new_range.start + i;

        if new[new_idx] == old[old_idx] && dp[row + j] == dp[(i + 1) * width + j + 1] + 1 {
            let start_i = i;
            let start_j = j;
            while i < m && j < n {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                let row = i * width;
                let old_idx = old_range.start + j;
                let new_idx = new_range.start + i;
                if new[new_idx] == old[old_idx] && dp[row + j] == dp[(i + 1) * width + j + 1] + 1 {
                    i += 1;
                    j += 1;
                } else {
                    break;
                }
            }
            d.equal(
                old_range.start + start_j,
                new_range.start + start_i,
                j - start_j,
            )?;
            emitted_any = true;
        } else if dp[(i + 1) * width + j] >= dp[row + j + 1] {
            let start_i = i;
            let old_idx = old_range.start + j;
            while i < m {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                if j >= n {
                    i = m;
                    break;
                }
                let row = i * width;
                let old_idx_curr = old_range.start + j;
                let new_idx = new_range.start + i;
                if new[new_idx] == old[old_idx_curr]
                    && dp[row + j] == dp[(i + 1) * width + j + 1] + 1
                {
                    break;
                }
                if dp[(i + 1) * width + j] >= dp[row + j + 1] {
                    i += 1;
                } else {
                    break;
                }
            }
            d.insert(old_idx, new_range.start + start_i, i - start_i)?;
            emitted_any = true;
        } else {
            let start_j = j;
            let new_idx = new_range.start + i;
            while j < n {
                if !emitted_any
                    && (j & (SMALL_SIDE_DEADLINE_CHECK_INTERVAL - 1) == 0)
                    && deadline_exceeded(deadline)
                {
                    return Ok(false);
                }
                if i >= m {
                    j = n;
                    break;
                }
                let row = i * width;
                let old_idx = old_range.start + j;
                let new_idx_curr = new_range.start + i;
                if new[new_idx_curr] == old[old_idx]
                    && dp[row + j] == dp[(i + 1) * width + j + 1] + 1
                {
                    break;
                }
                if dp[(i + 1) * width + j] < dp[row + j + 1] {
                    j += 1;
                } else {
                    break;
                }
            }
            d.delete(old_range.start + start_j, j - start_j, new_idx)?;
            emitted_any = true;
        }
    }

    if j < n {
        d.delete(old_range.start + j, n - j, new_range.start + i)?;
    }
    if i < m {
        d.insert(old_range.start + j, new_range.start + i, m - i)?;
    }

    Ok(true)
}

#[inline(always)]
fn try_emit_front_anchor<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: &mut Range<usize>,
    new: &New,
    new_range: &mut Range<usize>,
    deadline: Option<Instant>,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    const MAX_SKIP: usize = 4;
    const MIN_ANCHOR_COMMON: usize = 96;

    let old_len = old_range.len();
    let new_len = new_range.len();

    if old_len <= MIN_ANCHOR_COMMON || new_len <= MIN_ANCHOR_COMMON {
        return Ok(());
    }

    // This anchor search is most profitable when one side is substantially
    // larger than the other (large append/prepend shifts).  On similarly sized
    // ranges the extra scan overhead can outweigh the benefit.
    let small = old_len.min(new_len);
    let large = old_len.max(new_len);
    if large < small.saturating_mul(2) {
        return Ok(());
    }

    let max_old_skip = old_len.saturating_sub(1).min(MAX_SKIP);
    let max_new_skip = new_len.saturating_sub(1).min(MAX_SKIP);

    let prefer_insert_anchor = new_len > old_len;
    let prefer_delete_anchor = old_len > new_len;

    let mut best: Option<(usize, usize, usize)> = None;
    'search: for old_skip in 0..=max_old_skip {
        for new_skip in 0..=max_new_skip {
            if deadline_exceeded(deadline) {
                return Ok(());
            }

            if old_skip == 0 && new_skip == 0 {
                continue;
            }

            // Keep this heuristic exactness-preserving by only peeling one-sided
            // shifts, and only from the larger side.  On repetitive inputs,
            // deleting from the smaller side (or inserting into it) can commit
            // to a locally long but globally suboptimal alignment.
            if old_skip != 0 && new_skip != 0 {
                continue;
            }
            if prefer_insert_anchor && old_skip != 0 {
                continue;
            }
            if prefer_delete_anchor && new_skip != 0 {
                continue;
            }

            if new[new_range.start + new_skip] != old[old_range.start + old_skip] {
                continue;
            }

            let Some(common) = common_prefix_len_at_deadline(
                old,
                old_range.start + old_skip,
                old_range.end,
                new,
                new_range.start + new_skip,
                new_range.end,
                deadline,
            ) else {
                return Ok(());
            };
            if common >= MIN_ANCHOR_COMMON {
                if common >= MIN_ANCHOR_COMMON * 8 {
                    best = Some((old_skip, new_skip, common));
                    break 'search;
                }

                let replace_span = old_skip.max(new_skip);
                let candidate = (common, core::cmp::Reverse(replace_span), old_skip, new_skip);
                let better = best
                    .map(|(best_old_skip, best_new_skip, best_common)| {
                        candidate
                            > (
                                best_common,
                                core::cmp::Reverse(best_old_skip.max(best_new_skip)),
                                best_old_skip,
                                best_new_skip,
                            )
                    })
                    .unwrap_or(true);
                if better {
                    best = Some((old_skip, new_skip, common));
                }
            }
        }
    }

    let Some((old_skip, new_skip, common)) = best else {
        return Ok(());
    };

    match (old_skip, new_skip) {
        (0, 0) => {}
        (0, new_skip) => d.insert(old_range.start, new_range.start, new_skip)?,
        (old_skip, 0) => d.delete(old_range.start, old_skip, new_range.start)?,
        (old_skip, new_skip) => d.replace(old_range.start, old_skip, new_range.start, new_skip)?,
    }

    d.equal(
        old_range.start + old_skip,
        new_range.start + new_skip,
        common,
    )?;
    old_range.start += old_skip + common;
    new_range.start += new_skip + common;

    Ok(())
}

/// A `Snake` is a sequence of diagonal edges in the edit graph.  Normally
/// a snake has a start end end point (and it is possible for a snake to have
/// a length of zero, meaning the start and end points are the same) however
/// we do not need the end point which is why it's not implemented here.
///
/// The divide part of a divide-and-conquer strategy. A D-path has D+1 snakes
/// some of which may be empty. The divide step requires finding the ceil(D/2) +
/// 1 or middle snake of an optimal D-path. The idea for doing so is to
/// simultaneously run the basic algorithm in both the forward and reverse
/// directions until furthest reaching forward and reverse paths starting at
/// opposing corners 'overlap'.
#[derive(Clone, Copy, Debug)]
enum SplitDirection {
    Forward,
    Backward,
}

#[derive(Clone, Copy, Debug)]
struct HeuristicSplit {
    old_index: usize,
    new_index: usize,
    progress: usize,
    direction: SplitDirection,
}

#[derive(Clone, Copy, Debug)]
struct MiddleSnakeSplit {
    old_index: usize,
    new_index: usize,
    left_mode: MyersMode,
    right_mode: MyersMode,
}

impl MiddleSnakeSplit {
    fn exact(old_index: usize, new_index: usize) -> Self {
        Self {
            old_index,
            new_index,
            left_mode: MyersMode::Raw,
            right_mode: MyersMode::Raw,
        }
    }

    fn heuristic(split: HeuristicSplit, old_start: usize, new_start: usize) -> Self {
        let (left_mode, right_mode) = match split.direction {
            SplitDirection::Forward => (MyersMode::Raw, MyersMode::Heuristic),
            SplitDirection::Backward => (MyersMode::Heuristic, MyersMode::Raw),
        };
        Self {
            old_index: old_start + split.old_index,
            new_index: new_start + split.new_index,
            left_mode,
            right_mode,
        }
    }
}

fn update_best_split(best: &mut Option<HeuristicSplit>, candidate: HeuristicSplit) {
    if best
        .as_ref()
        .is_none_or(|current| candidate.progress > current.progress)
    {
        *best = Some(candidate);
    }
}

fn is_interior_split(old_index: usize, new_index: usize, old_len: usize, new_len: usize) -> bool {
    old_index <= old_len
        && new_index <= new_len
        && (old_index != 0 || new_index != 0)
        && (old_index != old_len || new_index != new_len)
}

fn clamp_diagonal_point(
    old_index: usize,
    diagonal: isize,
    old_len: usize,
    new_len: usize,
) -> Option<(usize, usize)> {
    if diagonal < 0 && diagonal.unsigned_abs() > new_len {
        return None;
    }

    let min_old = diagonal.max(0) as usize;
    let max_old = if diagonal >= 0 {
        old_len.min(new_len.saturating_add(diagonal as usize))
    } else {
        old_len.min(new_len.saturating_sub(diagonal.unsigned_abs()))
    };
    if min_old > max_old {
        return None;
    }

    let old_index = old_index.clamp(min_old, max_old);
    Some((old_index, (old_index as isize - diagonal) as usize))
}

fn heuristic_max_cost(old_len: usize, new_len: usize) -> usize {
    old_len
        .saturating_add(new_len)
        .isqrt()
        .max(HEURISTIC_MIN_MAX_COST)
}

fn find_middle_snake<Old, New>(
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    vf: &mut V,
    vb: &mut V,
    search: MyersSearch,
) -> Option<MiddleSnakeSplit>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    New::Output: PartialEq<Old::Output>,
{
    let n = old_range.len();
    let m = new_range.len();

    // By Lemma 1 in the paper, the optimal edit script length is odd or even as
    // `delta` is odd or even.
    let delta = n as isize - m as isize;
    let odd = delta & 1 == 1;

    // The initial point at (0, -1)
    vf[1] = 0;
    // The initial point at (N, M+1)
    vb[1] = 0;

    // We only need to explore ceil(D/2) + 1.
    let d_max = max_d(n, m);
    assert!(vf.len() >= d_max);
    assert!(vb.len() >= d_max);
    let max_cost = match search.mode {
        MyersMode::Heuristic => heuristic_max_cost(n, m),
        MyersMode::Raw => usize::MAX,
    };

    for d in 0..d_max as isize {
        if deadline_exceeded(search.deadline) {
            break;
        }

        let mut good_split = None;
        let mut forward_split = None;
        let mut backward_split = None;

        // Forward path
        for k in (-d..=d).rev().step_by(2) {
            let mut x = if k == -d || (k != d && vf[k - 1] < vf[k + 1]) {
                vf[k + 1]
            } else {
                vf[k - 1] + 1
            };
            let y = (x as isize - k) as usize;

            // The coordinate of the start of a snake
            let (x0, y0) = (x, y);
            if x < n && y < m {
                let advance = common_prefix_len_at(
                    old,
                    old_range.start + x,
                    old_range.end,
                    new,
                    new_range.start + y,
                    new_range.end,
                );
                x += advance;
            }
            let snake_len = x - x0;

            // This is the new best x value
            vf[k] = x;

            // Only check for connections from the forward search when N - M is
            // odd and when there is a reciprocal k line coming from the other
            // direction.
            if odd && (k - delta).abs() <= (d - 1) && vf[k] + vb[-(k - delta)] >= n {
                return Some(MiddleSnakeSplit::exact(
                    x0 + old_range.start,
                    y0 + new_range.start,
                ));
            }

            if search.mode == MyersMode::Heuristic {
                if let Some((split_old, split_new)) = clamp_diagonal_point(x, k, n, m) {
                    if is_interior_split(split_old, split_new, n, m) {
                        update_best_split(
                            &mut forward_split,
                            HeuristicSplit {
                                old_index: split_old,
                                new_index: split_new,
                                progress: split_old.saturating_add(split_new),
                                direction: SplitDirection::Forward,
                            },
                        );
                    }
                }

                let useful_progress = x.saturating_add(y).saturating_sub(k.unsigned_abs());
                if d as usize > HEURISTIC_MIN_COST
                    && snake_len >= HEURISTIC_MIN_SNAKE
                    && x < n
                    && y < m
                    && useful_progress > HEURISTIC_PROGRESS_FACTOR.saturating_mul(d as usize)
                    && is_interior_split(x0, y0, n, m)
                {
                    update_best_split(
                        &mut good_split,
                        HeuristicSplit {
                            old_index: x0,
                            new_index: y0,
                            progress: useful_progress,
                            direction: SplitDirection::Forward,
                        },
                    );
                }
            }
        }

        // Backward path
        for k in (-d..=d).rev().step_by(2) {
            let mut x = if k == -d || (k != d && vb[k - 1] < vb[k + 1]) {
                vb[k + 1]
            } else {
                vb[k - 1] + 1
            };
            let mut y = (x as isize - k) as usize;
            let x0 = x;

            if x < n && y < m {
                let advance = common_suffix_len_at(
                    old,
                    old_range.start,
                    old_range.start + n - x,
                    new,
                    new_range.start,
                    new_range.start + m - y,
                );
                x += advance;
                y += advance;
            }
            let snake_len = x - x0;

            // This is the new best x value
            vb[k] = x;

            if !odd && (k - delta).abs() <= d && vb[k] + vf[-(k - delta)] >= n {
                return Some(MiddleSnakeSplit::exact(
                    n - x + old_range.start,
                    m - y + new_range.start,
                ));
            }

            if search.mode == MyersMode::Heuristic {
                if let Some((distance_old, distance_new)) = clamp_diagonal_point(x, k, n, m) {
                    let split_old = n - distance_old;
                    let split_new = m - distance_new;
                    if is_interior_split(split_old, split_new, n, m) {
                        update_best_split(
                            &mut backward_split,
                            HeuristicSplit {
                                old_index: split_old,
                                new_index: split_new,
                                progress: distance_old.saturating_add(distance_new),
                                direction: SplitDirection::Backward,
                            },
                        );
                    }
                }

                if x <= n && y <= m {
                    let split_old = n - x;
                    let split_new = m - y;
                    let useful_progress = x.saturating_add(y).saturating_sub(k.unsigned_abs());
                    if d as usize > HEURISTIC_MIN_COST
                        && snake_len >= HEURISTIC_MIN_SNAKE
                        && split_old > 0
                        && split_new > 0
                        && useful_progress > HEURISTIC_PROGRESS_FACTOR.saturating_mul(d as usize)
                    {
                        update_best_split(
                            &mut good_split,
                            HeuristicSplit {
                                old_index: split_old,
                                new_index: split_new,
                                progress: useful_progress,
                                direction: SplitDirection::Backward,
                            },
                        );
                    }
                }
            }
        }

        if let Some(split) = good_split {
            return Some(MiddleSnakeSplit::heuristic(
                split,
                old_range.start,
                new_range.start,
            ));
        }

        if d as usize >= max_cost {
            let split = match (forward_split, backward_split) {
                (Some(forward), Some(backward)) => {
                    if forward.progress > backward.progress {
                        forward
                    } else {
                        backward
                    }
                }
                (Some(split), None) | (None, Some(split)) => split,
                (None, None) => break,
            };
            return Some(MiddleSnakeSplit::heuristic(
                split,
                old_range.start,
                new_range.start,
            ));
        }
    }

    // The deadline was reached before an exact or heuristic split was found.
    None
}

#[allow(clippy::too_many_arguments)]
fn conquer<Old, New, D>(
    d: &mut D,
    old: &Old,
    mut old_range: Range<usize>,
    new: &New,
    mut new_range: Range<usize>,
    vf: &mut V,
    vb: &mut V,
    search: MyersSearch,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    D: DiffHook,
    New::Output: PartialEq<Old::Output>,
{
    // Check for common prefix
    let common_prefix_len = common_prefix_len(old, old_range.clone(), new, new_range.clone());
    if common_prefix_len > 0 {
        d.equal(old_range.start, new_range.start, common_prefix_len)?;
    }
    old_range.start += common_prefix_len;
    new_range.start += common_prefix_len;

    // Check for common suffix
    let common_suffix_len = common_suffix_len(old, old_range.clone(), new, new_range.clone());
    let common_suffix = (
        old_range.end - common_suffix_len,
        new_range.end - common_suffix_len,
    );
    old_range.end -= common_suffix_len;
    new_range.end -= common_suffix_len;

    while old_range.start < old_range.end && new_range.start < new_range.end {
        let old_start_before = old_range.start;
        let new_start_before = new_range.start;
        try_emit_front_anchor(d, old, &mut old_range, new, &mut new_range, search.deadline)?;
        if old_range.start == old_start_before && new_range.start == new_start_before {
            break;
        }
    }

    if is_empty_range(&old_range) && is_empty_range(&new_range) {
        // Do nothing
    } else if is_empty_range(&new_range) {
        d.delete(old_range.start, old_range.len(), new_range.start)?;
    } else if is_empty_range(&old_range) {
        d.insert(old_range.start, new_range.start, new_range.len())?;
    } else if maybe_emit_small_side_exact(
        d,
        old,
        old_range.clone(),
        new,
        new_range.clone(),
        search.deadline,
    )? {
        // exact fallback emitted the script
    } else if let Some(split) = find_middle_snake(
        old,
        old_range.clone(),
        new,
        new_range.clone(),
        vf,
        vb,
        search,
    ) {
        let (old_a, old_b) = split_at(old_range, split.old_index);
        let (new_a, new_b) = split_at(new_range, split.new_index);
        conquer(
            d,
            old,
            old_a,
            new,
            new_a,
            vf,
            vb,
            search.with_mode(split.left_mode),
        )?;
        conquer(
            d,
            old,
            old_b,
            new,
            new_b,
            vf,
            vb,
            search.with_mode(split.right_mode),
        )?;
    } else {
        d.delete(
            old_range.start,
            old_range.end - old_range.start,
            new_range.start,
        )?;
        d.insert(
            old_range.start,
            new_range.start,
            new_range.end - new_range.start,
        )?;
    }

    if common_suffix_len > 0 {
        d.equal(common_suffix.0, common_suffix.1, common_suffix_len)?;
    }

    Ok(())
}

#[test]
fn test_find_middle_snake() {
    let a = &b"ABCABBA"[..];
    let b = &b"CBABAC"[..];
    let max_d = max_d(a.len(), b.len());
    let mut vf = V::new(max_d);
    let mut vb = V::new(max_d);
    let split = find_middle_snake(
        a,
        0..a.len(),
        b,
        0..b.len(),
        &mut vf,
        &mut vb,
        MyersSearch {
            deadline: None,
            mode: MyersMode::Raw,
        },
    )
    .unwrap();
    assert_eq!(split.old_index, 4);
    assert_eq!(split.new_index, 1);
}

#[test]
fn test_heuristic_middle_snake_has_a_work_limit() {
    let old = (0..1024u32).collect::<Vec<_>>();
    let new = (2048..3072u32).collect::<Vec<_>>();
    let max_d = max_d(old.len(), new.len());
    let mut vf = V::new(max_d);
    let mut vb = V::new(max_d);

    let split = find_middle_snake(
        &old,
        0..old.len(),
        &new,
        0..new.len(),
        &mut vf,
        &mut vb,
        MyersSearch {
            deadline: None,
            mode: MyersMode::Heuristic,
        },
    )
    .unwrap();

    assert!(split.left_mode == MyersMode::Heuristic || split.right_mode == MyersMode::Heuristic);
    assert!(is_interior_split(
        split.old_index,
        split.new_index,
        old.len(),
        new.len()
    ));
}

#[test]
fn test_diff() {
    let a: &[usize] = &[0, 1, 2, 3, 4];
    let b: &[usize] = &[0, 1, 2, 9, 4];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_raw_accepts_partialeq_only_values() {
    let old = [1.0f32, 2.0, 3.0];
    let new = [1.0f32, 4.0, 3.0];

    let mut d = crate::algorithms::Capture::new();
    diff_deadline_raw(&mut d, &old, 0..old.len(), &new, 0..new.len(), None).unwrap();

    assert!(!d.ops().is_empty());
}

#[test]
fn test_myers_preflight_reuses_common_context_scan() {
    use core::cell::Cell;
    use core::hash::{Hash, Hasher};

    struct Counted<'a> {
        value: u32,
        comparisons: &'a Cell<usize>,
    }

    impl Hash for Counted<'_> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.value.hash(state);
        }
    }

    impl PartialEq for Counted<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.comparisons.set(self.comparisons.get() + 1);
            self.value == other.value
        }
    }

    impl Eq for Counted<'_> {}

    let comparisons = Cell::new(0);
    let old = (0..2048)
        .map(|value| Counted {
            value,
            comparisons: &comparisons,
        })
        .collect::<Vec<_>>();
    let new = (0..2048)
        .map(|value| Counted {
            value,
            comparisons: &comparisons,
        })
        .collect::<Vec<_>>();
    let mut d = crate::algorithms::Capture::new();

    diff(&mut d, &old, 0..old.len(), &new, 0..new.len()).unwrap();
    assert_eq!(comparisons.get(), old.len());

    #[cfg(feature = "std")]
    {
        use core::time::Duration;

        comparisons.set(0);
        let deadline = Instant::now()
            .checked_sub(Duration::from_millis(1))
            .unwrap();
        let mut d = crate::algorithms::Capture::new();
        diff_deadline(
            &mut d,
            &old,
            0..old.len(),
            &new,
            0..new.len(),
            Some(deadline),
        )
        .unwrap();
        assert_eq!(comparisons.get(), old.len());
    }
}

#[test]
fn test_myers_reduction_and_raw_myers_preserve_sparse_match() {
    use crate::{Algorithm, DiffOp, capture_diff_slices};

    let old = (0..1024u32).collect::<Vec<_>>();
    let mut new = (10_000..11_024u32).collect::<Vec<_>>();
    new[512] = old[256];

    let equal_len = |ops: &[DiffOp]| {
        ops.iter()
            .map(|op| match op {
                DiffOp::Equal { len, .. } => *len,
                _ => 0,
            })
            .sum::<usize>()
    };
    let heuristic = capture_diff_slices(Algorithm::Myers, &old, &new);
    let raw = capture_diff_slices(Algorithm::RawMyers, &old, &new);

    assert_eq!(equal_len(&heuristic), 1);
    assert_eq!(equal_len(&raw), 1);
}

#[test]
fn test_myers_reduction_preserves_subrange_offsets() {
    use crate::{Algorithm, DiffOp, capture_diff};

    let mut old = vec![99_999];
    old.extend(0..1024u32);
    old.push(88_888);
    let mut new = vec![77_777];
    new.extend(10_000..11_024u32);
    new.push(66_666);
    new[513] = old[257];

    assert_eq!(
        capture_diff(Algorithm::Myers, &old, 1..1025, &new, 1..1025),
        vec![
            DiffOp::Replace {
                old_index: 1,
                old_len: 256,
                new_index: 1,
                new_len: 512,
            },
            DiffOp::Equal {
                old_index: 257,
                new_index: 513,
                len: 1,
            },
            DiffOp::Replace {
                old_index: 258,
                old_len: 767,
                new_index: 514,
                new_len: 511,
            },
        ]
    );
}

#[test]
fn test_contiguous() {
    let a: &[usize] = &[0, 1, 2, 3, 4, 4, 4, 5];
    let b: &[usize] = &[0, 1, 2, 8, 9, 4, 4, 7];

    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_pat() {
    let a: &[usize] = &[0, 1, 3, 4, 5];
    let b: &[usize] = &[0, 1, 4, 5, 8, 9];

    let mut d = crate::algorithms::Capture::new();
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();
    insta::assert_debug_snapshot!(d.ops());
}

#[test]
fn test_small_side_exact_variants() {
    {
        // fully disjoint, tiny old vs large new
        let old = &[1u32][..];
        let new = (0..1000u32).map(|x| x + 10).collect::<Vec<_>>();

        let mut d = crate::algorithms::Capture::new();
        let used = maybe_emit_small_side_exact(&mut d, old, 0..old.len(), &new, 0..new.len(), None)
            .unwrap();

        assert!(used);
        assert_eq!(
            d.ops(),
            &[
                crate::DiffOp::Delete {
                    old_index: 0,
                    old_len: 1,
                    new_index: 0,
                },
                crate::DiffOp::Insert {
                    old_index: 1,
                    new_index: 0,
                    new_len: 1000,
                },
            ]
        );
    }

    {
        // sparse overlap far into the larger side must be preserved
        let old = (0..8u32).collect::<Vec<_>>();
        let mut new = (1000..2000u32).collect::<Vec<_>>();
        new[500] = 0;

        let mut d = crate::algorithms::Capture::new();
        let used =
            maybe_emit_small_side_exact(&mut d, &old, 0..old.len(), &new, 0..new.len(), None)
                .unwrap();

        assert!(used);
        assert!(d.ops().iter().any(|op| {
            matches!(
                op,
                crate::DiffOp::Equal {
                    old_index: 0,
                    new_index: 500,
                    len: 1
                }
            )
        }));
    }

    {
        // mirrored direction: tiny new vs large old
        let old = (0..1000u32).collect::<Vec<_>>();
        let new = vec![500u32];

        let mut d = crate::algorithms::Capture::new();
        let used =
            maybe_emit_small_side_exact(&mut d, &old, 0..old.len(), &new, 0..new.len(), None)
                .unwrap();

        assert!(used);

        let mut total_deleted = 0usize;
        let mut insert_count = 0usize;
        let mut saw_expected_equal = false;

        for op in d.ops() {
            match *op {
                crate::DiffOp::Delete { old_len, .. } => total_deleted += old_len,
                crate::DiffOp::Insert { .. } => insert_count += 1,
                crate::DiffOp::Equal {
                    old_index,
                    new_index,
                    len,
                } => {
                    if old_index == 500 && new_index == 0 && len == 1 {
                        saw_expected_equal = true;
                    }
                }
                crate::DiffOp::Replace { .. } => unreachable!("capture hook does not emit replace"),
            }
        }

        assert_eq!(insert_count, 0);
        assert_eq!(total_deleted, 999);
        assert!(saw_expected_equal);
    }
}

#[cfg(test)]
struct SleepOnFirstEmit {
    inner: crate::algorithms::Capture,
    slept: bool,
}

#[cfg(test)]
impl SleepOnFirstEmit {
    fn new() -> Self {
        Self {
            inner: crate::algorithms::Capture::new(),
            slept: false,
        }
    }

    fn maybe_sleep(&mut self) {
        if !self.slept {
            std::thread::sleep(std::time::Duration::from_millis(40));
            self.slept = true;
        }
    }

    fn ops(&self) -> &[crate::DiffOp] {
        self.inner.ops()
    }
}

#[cfg(test)]
impl DiffHook for SleepOnFirstEmit {
    type Error = std::convert::Infallible;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        self.inner.equal(old_index, new_index, len)?;
        self.maybe_sleep();
        Ok(())
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        self.inner.delete(old_index, old_len, new_index)?;
        self.maybe_sleep();
        Ok(())
    }

    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.inner.insert(old_index, new_index, new_len)?;
        self.maybe_sleep();
        Ok(())
    }
}

#[cfg(feature = "std")]
#[test]
fn test_small_side_exact_commits_after_first_emit() {
    use std::time::Duration;

    {
        let old = vec![1u32, 2u32];
        let mut new = vec![0u32; 1300];
        new[0] = 1;
        new[1299] = 2;

        let mut d = SleepOnFirstEmit::new();
        let used = emit_small_old_exact(
            &mut d,
            &old,
            0..old.len(),
            &new,
            0..new.len(),
            Some(Instant::now() + Duration::from_millis(20)),
        )
        .unwrap();

        assert!(used);
        assert!(d.ops().len() >= 2);
    }

    {
        let mut old = vec![0u32; 1300];
        old[0] = 1;
        old[1299] = 2;
        let new = vec![1u32, 2u32];

        let mut d = SleepOnFirstEmit::new();
        let used = emit_small_new_exact(
            &mut d,
            &old,
            0..old.len(),
            &new,
            0..new.len(),
            Some(Instant::now() + Duration::from_millis(20)),
        )
        .unwrap();

        assert!(used);
        assert!(d.ops().len() >= 2);
    }
}

#[cfg(feature = "std")]
#[test]
fn test_deadline_reached() {
    use std::ops::Index;
    use std::time::Duration;

    let a = (0..100).collect::<Vec<_>>();
    let mut b = (0..100).collect::<Vec<_>>();
    b[10] = 99;
    b[50] = 99;
    b[25] = 99;

    struct SlowIndex<'a>(&'a [usize]);

    impl Index<usize> for SlowIndex<'_> {
        type Output = usize;

        fn index(&self, index: usize) -> &Self::Output {
            std::thread::sleep(Duration::from_millis(1));
            &self.0[index]
        }
    }

    let slow_a = SlowIndex(&a);
    let slow_b = SlowIndex(&b);

    // don't give it enough time to do anything interesting
    let mut d = crate::algorithms::Replace::new(crate::algorithms::Capture::new());
    diff_deadline(
        &mut d,
        &slow_a,
        0..a.len(),
        &slow_b,
        0..b.len(),
        Some(Instant::now() + Duration::from_millis(50)),
    )
    .unwrap();
    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[cfg(test)]
fn edit_cost(ops: &[crate::DiffOp]) -> usize {
    ops.iter()
        .map(|op| match *op {
            crate::DiffOp::Equal { .. } => 0,
            crate::DiffOp::Delete { old_len, .. } => old_len,
            crate::DiffOp::Insert { new_len, .. } => new_len,
            crate::DiffOp::Replace {
                old_len, new_len, ..
            } => old_len + new_len,
        })
        .sum()
}

#[test]
fn test_front_anchor_regressions_stay_exact() {
    {
        let mut old = vec![0u32, 1u32];
        old.extend(10..106u32);

        let mut new = vec![1u32, 2u32];
        new.extend(10..106u32);
        new.extend(1000..1098u32);

        let mut d = crate::algorithms::Capture::new();
        diff(&mut d, &old, 0..old.len(), &new, 0..new.len()).unwrap();

        let equal_len = d
            .ops()
            .iter()
            .filter_map(|op| match *op {
                crate::DiffOp::Equal { len, .. } => Some(len),
                _ => None,
            })
            .sum::<usize>();

        // n + m - 2 * LCS = 98 + 196 - 2 * 97 = 100
        assert_eq!(equal_len, 97);
        assert_eq!(edit_cost(d.ops()), 100);
    }

    {
        let old: Vec<u8> = (0..99).map(|i| (i % 2) as u8).collect();
        let new = [old[1..].to_vec(), old.clone(), vec![0]].concat();

        let mut myers = crate::algorithms::Capture::new();
        diff(&mut myers, &old, 0..old.len(), &new, 0..new.len()).unwrap();

        let mut lcs = crate::algorithms::Capture::new();
        crate::algorithms::lcs::diff(&mut lcs, &old, 0..old.len(), &new, 0..new.len()).unwrap();

        let myers_cost = edit_cost(myers.ops());
        let lcs_cost = edit_cost(lcs.ops());

        assert_eq!(lcs_cost, 99);
        assert_eq!(myers_cost, lcs_cost);
    }
}

#[cfg(feature = "std")]
#[test]
fn test_heuristic_deadline_guards() {
    use std::cell::Cell;
    use std::ops::Index;
    use std::time::Duration;

    struct CountingIndex<'a> {
        values: &'a [u8],
        hits: Cell<usize>,
    }

    impl Index<usize> for CountingIndex<'_> {
        type Output = u8;

        fn index(&self, index: usize) -> &Self::Output {
            self.hits.set(self.hits.get() + 1);
            &self.values[index]
        }
    }

    {
        let old_data = vec![1u8; SMALL_SIDE_EXACT_MAX];
        let new_data = vec![1u8; SMALL_SIDE_EXACT_MAX_WORK / SMALL_SIDE_EXACT_MAX];

        let old = CountingIndex {
            values: &old_data,
            hits: Cell::new(0),
        };
        let new = CountingIndex {
            values: &new_data,
            hits: Cell::new(0),
        };

        let mut d = crate::algorithms::Capture::new();
        let used = maybe_emit_small_side_exact(
            &mut d,
            &old,
            0..old_data.len(),
            &new,
            0..new_data.len(),
            Some(Instant::now() - Duration::from_secs(1)),
        )
        .unwrap();

        assert!(!used);
        assert!(d.ops().is_empty());
        assert_eq!(old.hits.get(), 0);
        assert_eq!(new.hits.get(), 0);
    }

    {
        // Keep ranges intentionally unbalanced so the anchor-search path is
        // eligible (large >= small * 2).
        let old_data = vec![1u8; 4096];
        let new_data = vec![1u8; 8200];
        let old = CountingIndex {
            values: &old_data,
            hits: Cell::new(0),
        };
        let new = CountingIndex {
            values: &new_data,
            hits: Cell::new(0),
        };

        let mut old_range = 0..old_data.len();
        let mut new_range = 0..new_data.len();
        let mut d = crate::algorithms::Capture::new();
        try_emit_front_anchor(
            &mut d,
            &old,
            &mut old_range,
            &new,
            &mut new_range,
            Some(Instant::now() - Duration::from_secs(1)),
        )
        .unwrap();

        assert_eq!(old.hits.get(), 0);
        assert_eq!(new.hits.get(), 0);
        assert_eq!(old_range, 0..old_data.len());
        assert_eq!(new_range, 0..new_data.len());

        // Sanity-check that the same inputs do hit the anchor scan when the
        // deadline is live.
        old.hits.set(0);
        new.hits.set(0);
        let mut old_range_live = 0..old_data.len();
        let mut new_range_live = 0..new_data.len();
        let mut d_live = crate::algorithms::Capture::new();
        try_emit_front_anchor(
            &mut d_live,
            &old,
            &mut old_range_live,
            &new,
            &mut new_range_live,
            None,
        )
        .unwrap();

        assert!(old.hits.get() > 0);
        assert!(new.hits.get() > 0);
    }
}

#[test]
fn test_finish_called() {
    struct HasRunFinish(bool);

    impl DiffHook for HasRunFinish {
        type Error = ();
        fn finish(&mut self) -> Result<(), Self::Error> {
            self.0 = true;
            Ok(())
        }
    }

    let mut d = HasRunFinish(false);
    let slice = &[1, 2];
    let slice2 = &[1, 2, 3];
    diff(&mut d, slice, 0..slice.len(), slice2, 0..slice2.len()).unwrap();
    assert!(d.0);

    let mut d = HasRunFinish(false);
    let slice = &[1, 2];
    diff(&mut d, slice, 0..slice.len(), slice, 0..slice.len()).unwrap();
    assert!(d.0);

    let mut d = HasRunFinish(false);
    let slice: &[u8] = &[];
    diff(&mut d, slice, 0..slice.len(), slice, 0..slice.len()).unwrap();
    assert!(d.0);
}

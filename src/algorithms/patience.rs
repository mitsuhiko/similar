//! Patience diff algorithm.
//!
//! * time: `O(N log N + M log M + (N+M)D)`
//! * space: `O(N+M)`
//!
//! Tends to give more human-readable outputs. See [Bram Cohen's blog
//! post](https://bramcohen.livejournal.com/73318.html) describing it.
//!
//! This is based on the patience implementation of [pijul](https://pijul.org/)
//! by Pierre-Étienne Meunier.
//!
//! # Heuristics
//!
//! See [`crate::algorithms`] for shared heuristics and the
//! `diff_deadline_raw` API.
use std::hash::Hash;
use std::ops::{Index, Range};

use crate::algorithms::{DiffHook, NoFinishHook, Replace, myers, preflight};
use crate::deadline_support::Instant;

use super::utils::{UniqueItem, unique};

/// Patience diff algorithm.
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    D: DiffHook,
{
    diff_deadline(d, old, old_range, new, new_range, None)
}

/// Patience diff algorithm with deadline.
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    D: DiffHook,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, true, false)
}

/// Raw patience diff algorithm with deadline and without shared heuristics.
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
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    D: DiffHook,
{
    diff_deadline_impl(d, old, old_range, new, new_range, deadline, false, true)
}

fn diff_deadline_impl<Old, New, D>(
    d: &mut D,
    old: &Old,
    old_range: Range<usize>,
    new: &New,
    new_range: Range<usize>,
    deadline: Option<Instant>,
    run_preflight: bool,
    use_raw_myers: bool,
) -> Result<(), D::Error>
where
    Old: Index<usize> + ?Sized,
    New: Index<usize> + ?Sized,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
    D: DiffHook,
{
    if run_preflight
        && preflight::maybe_emit_disjoint_fast_path(
            d,
            old,
            old_range.clone(),
            new,
            new_range.clone(),
            deadline,
        )?
    {
        return Ok(());
    }

    let old_indexes = unique(old, old_range.clone());
    let new_indexes = unique(new, new_range.clone());

    let mut d = Replace::new(Patience {
        d,
        old,
        old_current: old_range.start,
        old_end: old_range.end,
        old_indexes: &old_indexes,
        new,
        new_current: new_range.start,
        new_end: new_range.end,
        new_indexes: &new_indexes,
        deadline,
        use_raw_myers,
    });

    if use_raw_myers {
        myers::diff_deadline_raw(
            &mut d,
            &old_indexes,
            0..old_indexes.len(),
            &new_indexes,
            0..new_indexes.len(),
            deadline,
        )?;
    } else {
        myers::diff_deadline(
            &mut d,
            &old_indexes,
            0..old_indexes.len(),
            &new_indexes,
            0..new_indexes.len(),
            deadline,
        )?;
    }
    Ok(())
}

struct Patience<'old, 'new, 'd, Old: ?Sized, New: ?Sized, D> {
    d: &'d mut D,
    old: &'old Old,
    old_current: usize,
    old_end: usize,
    old_indexes: &'old [UniqueItem<'old, Old>],
    new: &'new New,
    new_current: usize,
    new_end: usize,
    new_indexes: &'new [UniqueItem<'new, New>],
    deadline: Option<Instant>,
    use_raw_myers: bool,
}

impl<'old, 'new, 'd, Old, New, D> DiffHook for Patience<'old, 'new, 'd, Old, New, D>
where
    D: DiffHook + 'd,
    Old: Index<usize> + ?Sized + 'old,
    New: Index<usize> + ?Sized + 'new,
    Old::Output: Hash + Eq,
    New::Output: PartialEq<Old::Output> + Hash + Eq,
{
    type Error = D::Error;
    fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), D::Error> {
        for (old, new) in (old..old + len).zip(new..new + len) {
            let a0 = self.old_current;
            let b0 = self.new_current;
            while self.old_current < self.old_indexes[old].original_index()
                && self.new_current < self.new_indexes[new].original_index()
                && self.new[self.new_current] == self.old[self.old_current]
            {
                self.old_current += 1;
                self.new_current += 1;
            }
            if self.old_current > a0 {
                self.d.equal(a0, b0, self.old_current - a0)?;
            }
            let mut no_finish_d = NoFinishHook::new(&mut self.d);
            if self.use_raw_myers {
                myers::diff_deadline_raw(
                    &mut no_finish_d,
                    self.old,
                    self.old_current..self.old_indexes[old].original_index(),
                    self.new,
                    self.new_current..self.new_indexes[new].original_index(),
                    self.deadline,
                )?;
            } else {
                myers::diff_deadline(
                    &mut no_finish_d,
                    self.old,
                    self.old_current..self.old_indexes[old].original_index(),
                    self.new,
                    self.new_current..self.new_indexes[new].original_index(),
                    self.deadline,
                )?;
            }
            self.old_current = self.old_indexes[old].original_index();
            self.new_current = self.new_indexes[new].original_index();
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<(), D::Error> {
        if self.use_raw_myers {
            myers::diff_deadline_raw(
                self.d,
                self.old,
                self.old_current..self.old_end,
                self.new,
                self.new_current..self.new_end,
                self.deadline,
            )
        } else {
            myers::diff_deadline(
                self.d,
                self.old,
                self.old_current..self.old_end,
                self.new,
                self.new_current..self.new_end,
                self.deadline,
            )
        }
    }
}

#[test]
fn test_patience() {
    let a: &[usize] = &[11, 1, 2, 2, 3, 4, 4, 4, 5, 47, 19];
    let b: &[usize] = &[10, 1, 2, 2, 8, 9, 4, 4, 7, 47, 18];

    let mut d = Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();

    insta::assert_debug_snapshot!(d.into_inner().ops());
}

#[test]
fn test_patience_out_of_bounds_bug() {
    // this used to be a bug
    let a: &[usize] = &[1, 2, 3, 4];
    let b: &[usize] = &[1, 2, 3];

    let mut d = Replace::new(crate::algorithms::Capture::new());
    diff(&mut d, a, 0..a.len(), b, 0..b.len()).unwrap();

    insta::assert_debug_snapshot!(d.into_inner().ops());
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

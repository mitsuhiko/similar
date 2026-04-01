use alloc::vec::Vec;
use core::hash::Hash;

use crate::algorithms::utils::stable_hash;
use crate::types::MapType;

use super::DiffableStrRef;

// quick and dirty way to get an upper sequence ratio.
pub fn upper_seq_ratio<T: PartialEq>(seq1: &[T], seq2: &[T]) -> f32 {
    let n = seq1.len() + seq2.len();
    if n == 0 {
        1.0
    } else {
        2.0 * seq1.len().min(seq2.len()) as f32 / n as f32
    }
}

/// Internal utility to calculate an upper bound for a ratio for
/// [`get_close_matches`].  This is based on Python's difflib approach
/// of considering the two sets to be multisets.
///
/// It counts the number of matches without regard to order, which is an
/// obvious upper bound.
pub struct QuickSeqRatio<'a, T: DiffableStrRef + ?Sized> {
    counts: MapType<u64, Vec<(&'a T, i32)>>,
    unique_count: usize,
}

impl<'a, T: DiffableStrRef + Hash + Eq + ?Sized> QuickSeqRatio<'a, T> {
    pub fn new(seq: &[&'a T]) -> QuickSeqRatio<'a, T> {
        let mut counts = MapType::<u64, Vec<(&T, i32)>>::new();
        let mut unique_count = 0;
        for &word in seq {
            let bucket = counts.entry(stable_hash(word)).or_default();
            if let Some((_, count)) = bucket.iter_mut().find(|(candidate, _)| *candidate == word) {
                *count += 1;
            } else {
                bucket.push((word, 1));
                unique_count += 1;
            }
        }
        QuickSeqRatio {
            counts,
            unique_count,
        }
    }

    pub fn calc(&self, seq: &[&T]) -> f32 {
        let n = self.unique_count + seq.len();
        if n == 0 {
            return 1.0;
        }

        let mut available = MapType::<u64, Vec<(&T, i32)>>::new();
        let mut matches = 0;
        for &word in seq {
            let hash = stable_hash(word);
            let bucket = available.entry(hash).or_default();

            let x = if let Some((_, count)) =
                bucket.iter_mut().find(|(candidate, _)| *candidate == word)
            {
                *count
            } else {
                let initial = self
                    .counts
                    .get(&hash)
                    .and_then(|source_bucket| {
                        source_bucket
                            .iter()
                            .find(|(candidate, _)| *candidate == word)
                            .map(|(_, count)| *count)
                    })
                    .unwrap_or(0);
                bucket.push((word, initial));
                initial
            };

            if let Some((_, count)) = bucket.iter_mut().find(|(candidate, _)| *candidate == word) {
                *count = x - 1;
            }

            if x > 0 {
                matches += 1;
            }
        }

        2.0 * matches as f32 / n as f32
    }
}

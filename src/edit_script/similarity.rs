//! Similarity measures used by the edit script analysis.
use alloc::vec;

use crate::{Algorithm, DiffOp, capture_diff_slices};

/// Above this many DP cells the LCS length is approximated with Myers.
const MAX_DP_CELLS: usize = 1 << 16;

/// Returns the normalized indel similarity `2 * LCS / (len(a) + len(b))`.
///
/// This is the similarity commonly called the "Levenshtein ratio" in diff
/// literature: `1.0` for equal inputs and `0.0` if nothing is shared.
pub(crate) fn indel_ratio(a: &[u8], b: &[u8]) -> f64 {
    let total = a.len() + b.len();
    if total == 0 {
        return 1.0;
    }
    2.0 * lcs_len(a, b) as f64 / total as f64
}

/// The largest possible [`indel_ratio`] for inputs of these lengths.
pub(crate) fn max_indel_ratio(a_len: usize, b_len: usize) -> f64 {
    let total = a_len + b_len;
    if total == 0 {
        1.0
    } else {
        2.0 * a_len.min(b_len) as f64 / total as f64
    }
}

fn lcs_len(a: &[u8], b: &[u8]) -> usize {
    let prefix = a.iter().zip(b).take_while(|(x, y)| x == y).count();
    let (a, b) = (&a[prefix..], &b[prefix..]);
    let suffix = a
        .iter()
        .rev()
        .zip(b.iter().rev())
        .take_while(|(x, y)| x == y)
        .count();
    let (a, b) = (&a[..a.len() - suffix], &b[..b.len() - suffix]);
    let common = prefix + suffix;
    if a.is_empty() || b.is_empty() {
        return common;
    }
    let (a, b) = if a.len() < b.len() { (b, a) } else { (a, b) };
    if b.len() <= 64 {
        return common + lcs_bit_parallel(a, b);
    }
    if a.len() * b.len() > MAX_DP_CELLS {
        let equal = capture_diff_slices(Algorithm::Myers, a, b)
            .iter()
            .map(|op| match *op {
                DiffOp::Equal { len, .. } => len,
                _ => 0,
            })
            .sum::<usize>();
        return common + equal;
    }
    common + lcs_dp(a, b)
}

/// Bit-parallel LCS length (Allison-Dix / Hyyrö) for `b` of at most 64
/// bytes.  Processes one byte of `a` per step in `O(len(a))`.
fn lcs_bit_parallel(a: &[u8], b: &[u8]) -> usize {
    debug_assert!(!b.is_empty() && b.len() <= 64);
    let mut masks = [0u64; 256];
    for (idx, &c) in b.iter().enumerate() {
        masks[c as usize] |= 1 << idx;
    }
    let width_mask = if b.len() == 64 {
        u64::MAX
    } else {
        (1u64 << b.len()) - 1
    };
    let mut v = u64::MAX;
    for &c in a {
        let u = v & masks[c as usize];
        v = v.wrapping_add(u) | v.wrapping_sub(u);
    }
    (!v & width_mask).count_ones() as usize
}

/// Classic LCS dynamic program over a single row.
pub(crate) fn lcs_dp<T: PartialEq>(a: &[T], b: &[T]) -> usize {
    let mut row = vec![0u32; b.len() + 1];
    for x in a {
        let mut diagonal = 0;
        for (j, y) in b.iter().enumerate() {
            let above = row[j + 1];
            row[j + 1] = if x == y {
                diagonal + 1
            } else {
                above.max(row[j])
            };
            diagonal = above;
        }
    }
    row[b.len()] as usize
}

/// Similarity of two sequences of lines: `2 * LCS / (len(a) + len(b))`.
///
/// Two empty sequences are considered equal.
pub(crate) fn sequence_ratio<T: PartialEq>(a: &[T], b: &[T]) -> f64 {
    let total = a.len() + b.len();
    if total == 0 {
        return 1.0;
    }
    2.0 * lcs_dp(a, b) as f64 / total as f64
}

#[test]
fn test_indel_ratio() {
    assert_eq!(indel_ratio(b"", b""), 1.0);
    assert_eq!(indel_ratio(b"abc", b"abc"), 1.0);
    assert_eq!(indel_ratio(b"abc", b"xyz"), 0.0);
    assert_eq!(indel_ratio(b"abcd", b"abxd"), 0.75);
    let long_a = b"x".repeat(400);
    let mut long_b = long_a.clone();
    long_b[200] = b'y';
    assert!((indel_ratio(&long_a, &long_b) - 399.0 / 400.0).abs() < 1e-9);
    assert_eq!(max_indel_ratio(1, 3), 0.5);
}

#[test]
fn test_bit_parallel_matches_dp() {
    let mut seed = 7u64;
    let mut random = move |n: usize| {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        (seed % n as u64) as usize
    };
    for _ in 0..2000 {
        let a = (0..random(100))
            .map(|_| b"abcd"[random(4)])
            .collect::<alloc::vec::Vec<_>>();
        let b = (0..1 + random(64))
            .map(|_| b"abcd"[random(4)])
            .collect::<alloc::vec::Vec<_>>();
        assert_eq!(lcs_bit_parallel(&a, &b), lcs_dp(&a, &b));
    }
}

#[test]
fn test_sequence_ratio() {
    assert_eq!(sequence_ratio::<u8>(&[], &[]), 1.0);
    assert_eq!(sequence_ratio(&[1, 2, 3], &[1, 3]), 0.8);
}

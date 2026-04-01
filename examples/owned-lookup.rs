//! Demonstrates diffing a virtual/computed sequence by materializing it through
//! [`similar::algorithms::CachedLookup`].
//!
//! This addresses <https://github.com/mitsuhiko/similar/issues/33> without
//! changing the core diff input model: values are computed on demand, cached on
//! first access, and then borrowed through normal indexing.
//!
//! The example flattens a `Vec<Vec<u8>>` into a virtual token stream and diffs
//! the two sides without first building a flat `Vec<Token>` eagerly.

use similar::algorithms::CachedLookup;
use similar::{Algorithm, ChangeTag, capture_diff};

/// A single element in the flattened representation of `Vec<Vec<u8>>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum Token {
    /// Marks the start of an inner vector.
    GroupStart,
    /// An actual element.
    Elem(u8),
}

/// A virtual, flattened view over `Vec<Vec<u8>>`.
///
/// Each inner vec contributes one `GroupStart` token followed by its elements.
/// The total length is `sum(1 + inner.len() for inner in data)`.
struct FlatView<'a> {
    data: &'a [Vec<u8>],
    /// Precomputed prefix sums so lookup is O(log n) instead of O(n).
    offsets: Vec<usize>,
    len: usize,
}

impl<'a> FlatView<'a> {
    fn new(data: &'a [Vec<u8>]) -> Self {
        let mut offsets = Vec::with_capacity(data.len());
        let mut cumulative = 0;
        for inner in data {
            offsets.push(cumulative);
            cumulative += 1 + inner.len();
        }
        FlatView {
            data,
            offsets,
            len: cumulative,
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn token(&self, index: usize) -> Token {
        let group = match self.offsets.binary_search(&index) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let local = index - self.offsets[group];
        if local == 0 {
            Token::GroupStart
        } else {
            Token::Elem(self.data[group][local - 1])
        }
    }
}

fn main() {
    let old_data: Vec<Vec<u8>> = vec![vec![1, 2], vec![3, 4, 5]];
    let new_data: Vec<Vec<u8>> = vec![vec![1, 2, 3], vec![3, 4, 5]];

    let old_view = FlatView::new(&old_data);
    let new_view = FlatView::new(&new_data);
    let old = CachedLookup::new(old_view.len(), |index| old_view.token(index));
    let new = CachedLookup::new(new_view.len(), |index| new_view.token(index));

    let ops = capture_diff(Algorithm::Myers, &old, 0..old.len(), &new, 0..new.len());

    println!("Diff ops:");
    for op in &ops {
        let (tag, old_range, new_range) = op.as_tag_tuple();
        match tag {
            similar::DiffTag::Equal => {
                for i in old_range {
                    println!("  = {:?}", old[i]);
                }
            }
            similar::DiffTag::Delete => {
                for i in old_range {
                    println!("  - {:?}", old[i]);
                }
            }
            similar::DiffTag::Insert => {
                for i in new_range {
                    println!("  + {:?}", new[i]);
                }
            }
            similar::DiffTag::Replace => {
                for i in old_range {
                    println!("  - {:?}", old[i]);
                }
                for i in new_range {
                    println!("  + {:?}", new[i]);
                }
            }
        }
    }

    println!("\nAll changes:");
    for op in &ops {
        for change in op.iter_changes(&old, &new) {
            let sign = match change.tag() {
                ChangeTag::Equal => " ",
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
            };
            println!("{} {:?}", sign, change.value_ref());
        }
    }
}

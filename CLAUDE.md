# Likewise - Floating Point Diff Implementation

## Codebase Structure

```
src/
├── lib.rs              # Main entry, re-exports
├── types.rs            # DiffOp, Change, ChangeTag, Algorithm enums
├── common.rs           # capture_diff_slices(), get_diff_ratio()
├── algorithms/         # myers.rs, patience.rs, lcs.rs, hook.rs, utils.rs
├── text/              # TextDiff, DiffableStr trait
├── udiff.rs           # Unified diff output
└── utils.rs           # Text utilities
```

## Current Diff Flow

1. `capture_diff_slices(alg, &old, &new)` → `Vec<DiffOp>`
2. `DiffOp::iter_changes(&old, &new)` → `Iterator<Change<T>>`
3. Requires `T: Hash + Eq + Ord`

## Key Types

```rust
pub enum DiffOp {
    Equal { old_index: usize, new_index: usize, len: usize },
    Delete { old_index: usize, old_len: usize, new_index: usize },
    Insert { old_index: usize, new_index: usize, new_len: usize },
    Replace { old_index: usize, old_len: usize, new_index: usize, new_len: usize },
}

pub struct Change<T> {
    pub(crate) tag: ChangeTag,
    pub(crate) old_index: Option<usize>,
    pub(crate) new_index: Option<usize>, 
    pub(crate) value: T,
}
```

## Algorithm Trait

```rust
pub trait DiffHook {
    type Error;
    fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), Self::Error>;
    fn delete(&mut self, old: usize, old_len: usize, new: usize) -> Result<(), Self::Error>;
    fn insert(&mut self, old: usize, new: usize, new_len: usize) -> Result<(), Self::Error>;
    fn replace(&mut self, old: usize, old_len: usize, new: usize, new_len: usize) -> Result<(), Self::Error>;
}
```

## Task: FP Comparison

Add eps/ulps floating point comparison for f32/f64 slices.

### Implementation Points

1. **New comparison trait** alongside existing `PartialEq` requirement
2. **Algorithm modification** to use FP-aware comparison in core loops
3. **API surface** for specifying epsilon/ulps parameters  
4. **Testing** edge cases: NaN, infinity, subnormals, precision boundaries

### Files to Modify

- `src/algorithms/myers.rs` - Core comparison logic
- `src/algorithms/patience.rs` - Core comparison logic  
- `src/algorithms/lcs.rs` - Core comparison logic
- `src/common.rs` - Add FP-aware `capture_diff_slices_fp()`
- `examples/floating_point.rs` - Usage demonstration

### Example Usage Goal

```rust
let old = vec![1.0, 2.0, 3.0];
let new = vec![1.000001, 2.0, 3.1];

// Absolute epsilon
let ops = capture_diff_slices_fp_eps(Algorithm::Myers, &old, &new, 0.0001);

// Relative ulps  
let ops = capture_diff_slices_fp_ulps(Algorithm::Myers, &old, &new, 4);
```

### Testing Strategy

Use existing `insta` snapshot testing framework. Test matrices:
- Normal values with various epsilon/ulps settings
- Edge cases: ±0.0, NaN, ±infinity
- Precision boundaries and subnormal values
- Mixed normal/edge case arrays

### Status

- [x] CI fixed (Rust 1.66 MSRV)
- [x] Codebase analysis complete
- [ ] Design FP comparison API
- [ ] Implement core FP comparison logic
- [ ] Add comprehensive tests
- [ ] Create usage examples
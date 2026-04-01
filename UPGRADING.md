# Upgrading From Similar 2.7 to 3.0

This guide covers migration from similar `2.7.x` to similar `3.0` on `main`.
Most code will keep working.  However, there are a few important breaking
changes.

## In Short

- Update your Rust toolchain to `1.85` or newer.
- `std` is now an explicit default feature. For `no_std`, disable default features.
- Replace `TextDiff::old_slices()` and `TextDiff::new_slices()` with the new accessor methods.
- Audit any code that passes custom or external `DiffOp` values into `TextDiff::iter_changes`.
- Prefer `diff_ratio` over `get_diff_ratio`.
- If you wrote explicit `TextDiff` type annotations, update them for the new lifetime shape.

## `std` Is Now a Feature (Enabled by Default)

In 3.0, the crate explicitly models standard library support as a feature:

- default: `features = ["std", "text"]`
- `no_std`: `default-features = false`

`no_std` map backends:

- default: `alloc::collections::BTreeMap`
- opt-in: `features = ["hashbrown"]`

## `TextDiff` Slice Access Changed

In 2.7, `TextDiff` exposed `old_slices()` and `new_slices()`.  In 3.0, these are
removed.  Use the accessor APIs instead.

### Before (2.7)

```rust
let old_item = diff.old_slices()[idx];
let new_item = diff.new_slices()[idx];

for item in diff.old_slices() {
    // ...
}
```

### After (3.0)

```rust
let old_item = diff.old_slice(idx);
let new_item = diff.new_slice(idx);

for item in diff.iter_old_slices() {
    // ...
}

let old_lookup = diff.old_lookup();
let first = &old_lookup[0];
```

Useful replacements:

- `diff.old_len()` and `diff.new_len()`
- `diff.old_slice(i)` and `diff.new_slice(i)`
- `diff.iter_old_slices()` and `diff.iter_new_slices()`
- `diff.old_lookup()` and `diff.new_lookup()`

## `TextDiff::iter_changes` Is Stricter

`TextDiff::iter_changes` now panics if a `DiffOp` range is out of bounds.  In
2.7, invalid ranges could be silently truncated.

If you only iterate `diff.ops()` from the same `TextDiff`, you are already safe.
If you deserialize, transform, or manually construct `DiffOp` values, validate
them before iteration.

```rust
use similar::{DiffOp, TextDiff};

fn op_in_bounds(diff: &TextDiff<'_, '_, str>, op: &DiffOp) -> bool {
    let (_, old, new) = op.as_tag_tuple();
    old.end <= diff.old_len() && new.end <= diff.new_len()
}
```

## `TextDiff` Type Parameters Changed

If you wrote explicit type annotations for `TextDiff`, update them.

### Before (2.7)

```rust
// 2.7 shape
similar::TextDiff<'old, 'new, 'bufs, str>
```

### After (3.0)

```rust
// 3.0 shape
similar::TextDiff<'old, 'new, str>
```

## More Constructors Are `const`

The following constructors are now usable in const contexts:

- `similar::algorithms::Capture::new`
- `similar::algorithms::Replace::new`
- `similar::algorithms::NoFinishHook::new`
- `similar::InlineChangeOptions::new` (with `inline` feature)
- `similar::TextDiff::configure` / `TextDiffConfig::new` (with `text` feature)

## `get_diff_ratio` is now `diff_ratio`

Use `diff_ratio` going forward as this is the new name:

### Before

```rust
let r = similar::get_diff_ratio(ops, old_len, new_len);
```

### After

```rust
let r = similar::diff_ratio(ops, old_len, new_len);
```

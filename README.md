# similar

[![Build Status](https://github.com/mitsuhiko/similar/workflows/Tests/badge.svg?branch=master)](https://github.com/mitsuhiko/similar/actions?query=workflow%3ATests)
[![Crates.io](https://img.shields.io/crates/d/similar.svg)](https://crates.io/crates/similar)
[![License](https://img.shields.io/github/license/mitsuhiko/similar)](https://github.com/mitsuhiko/similar/blob/master/LICENSE)
[![Documentation](https://docs.rs/similar/badge.svg)](https://docs.rs/similar)

Similar is a dependency free crate for Rust that implements different diffing
algorithms and high level interfaces for it.

It provides both low level implementations of Myer's and the Patience diff
algorithm as well as high level text diffing utilities (such as the ability
to generate unified diffs).

```rust
use similar::algorithms::Algorithm;
use similar::text::unified_diff;

let unified_diff = unified_diff(
    Algorithm::Patience,
    old_text,
    new_text,
    3,
    Some(("old.txt", "new.text"))
);
println!("{}", unified_diff);
```

## License and Links

- [Documentation](https://docs.rs/similar/)
- [Issue Tracker](https://github.com/mitsuhiko/similar/issues)
- License: [Apache-2.0](https://github.com/mitsuhiko/similar/blob/master/LICENSE)


# Similar: A Diffing Library

[![Crates.io](https://img.shields.io/crates/d/similar.svg)](https://crates.io/crates/similar)
[![License](https://img.shields.io/github/license/mitsuhiko/similar)](https://github.com/mitsuhiko/similar/blob/main/LICENSE)
[![Documentation](https://docs.rs/similar/badge.svg)](https://docs.rs/similar)

Similar is a dependency free crate for Rust that implements different diffing
algorithms and high level interfaces for it. It is based on the
[pijul](https://pijul.org/) implementation of the Patience algorithm and
inherits some ideas from there. It also incorporates the Myers' diff
algorithm which was largely written by Brandon Williams.  This library was
built for the [insta snapshot testing library](https://insta.rs).

```rust
use similar::{ChangeTag, TextDiff};

fn main() {
    let diff = TextDiff::from_lines(
        "Hello World\nThis is the second line.\nThis is the third.",
        "Hallo Welt\nThis is the second line.\nThis is life.\nMoar and more",
    );

    for change in diff.iter_all_changes() {
        let sign = match change.tag() {
            ChangeTag::Delete => "-",
            ChangeTag::Insert => "+",
            ChangeTag::Equal => " ",
        };
        print!("{}{}", sign, change);
    }
}
```

## Screenshot

![terminal highlighting](https://raw.githubusercontent.com/mitsuhiko/similar/main/assets/terminal-inline.png)

## What's in the box?

* Myers' diff
* Patience diff
* Hunt-style diff
* Histogram diff
* Classic LCS table diff
* Diffing on arbitrary comparable sequences
* Line, word, character and grapheme level diffing
* Text and Byte diffing
* Unified diff generation

## no_std Support

`similar` now enables `std` by default.

For `no_std + alloc` usage:

```toml
[dependencies]
similar = { version = "3", default-features = false }
```

Backend selection in `no_std` mode:

- default (`default-features = false`): `alloc::collections::BTreeMap`
- `default-features = false, features = ["hashbrown"]`: `hashbrown::HashMap`

## Related Projects

* [insta](https://insta.rs) snapshot testing library
* [similar-asserts](https://github.com/mitsuhiko/similar-asserts) assertion library

## License and Links

* [Documentation](https://docs.rs/similar/)
* [Upgrading Guide (2.7 to 3.0)](UPGRADING.md)
* [Issue Tracker](https://github.com/mitsuhiko/similar/issues)
* [Examples](https://github.com/mitsuhiko/similar/tree/main/examples)
* License: [Apache-2.0](https://github.com/mitsuhiko/similar/blob/main/LICENSE)

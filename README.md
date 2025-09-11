# Likewise: A Diffing Library

> This crate is a fork of [similar](https://github.com/mitsuhiko/similar) library, which, as of 11/9/25, is rather inactive.

[![Crates.io](https://img.shields.io/crates/d/likewise.svg)](https://crates.io/crates/likewise)
[![License](https://img.shields.io/github/license/frozen/likewise)](https://github.com/frozen/likewise/blob/main/LICENSE)
[![rustc 1.60.0](https://img.shields.io/badge/rust-1.60%2B-orange.svg)](https://img.shields.io/badge/rust-1.60%2B-orange.svg)
[![Documentation](https://docs.rs/likewise/badge.svg)](https://docs.rs/likewise)

Likewise is a dependency free crate for Rust that implements different diffing
algorithms and high level interfaces for it. It is based on the
[pijul](https://pijul.org/) implementation of the Patience algorithm and
inherits some ideas from there. It also incorporates the Myers' diff
algorithm which was largely written by Brandon Williams.  This library was
built for the [insta snapshot testing library](https://insta.rs).

```rust
use likewise::{ChangeTag, TextDiff};

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
* Hunt–McIlroy / Hunt–Szymanski LCS diff
* Diffing on arbitrary comparable sequences
* Line, word, character and grapheme level diffing
* Text and Byte diffing
* Unified diff generation

## Related Projects

* [similar](https://github.com/mitsuhiko/similar)
* [insta](https://insta.rs) snapshot testing library
* [similar-asserts](https://github.com/mitsuhiko/similar-asserts) assertion library

## License and Links

* [Documentation](https://docs.rs/likewise/)
* [Issue Tracker](https://github.com/barrett-ruth/likewise/issues)
* [Examples](https://github.com/barrett-ruth/likewise/tree/main/examples)
* License: [Apache-2.0](https://github.com/barrett-ruth/likewise/blob/main/LICENSE)

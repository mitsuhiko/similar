//! This crate implements diffing utilities.  It attempts to provide an abstraction
//! interface over different types of diffing algorithms.  It's based on the
//! the diff algorithm implementations of [pijul](https://pijul.org/).
//!
//! ```rust
//! # #[cfg(feature = "text")] {
//! use similar::ChangeTag;
//! use similar::text::TextDiff;
//!
//! let diff = TextDiff::from_lines(
//!     "Hello World\nThis is the second line.\nThis is the third.",
//!     "Hallo Welt\nThis is the second line.\nThis is life.\nMoar and more",
//! );
//!
//! for op in diff.ops() {
//!     for change in diff.iter_changes(op) {
//!         let sign = match change.tag() {
//!             ChangeTag::Delete => "-",
//!             ChangeTag::Insert => "+",
//!             ChangeTag::Equal => " ",
//!         };
//!         print!("{}{}", sign, change);
//!     }
//! }
//! # }
//! ```
//!
//! ## Functionality
//!
//! * [`algorithms`]: This implements the different types of diffing algorithms.
//!   It provides both low level access to the algorithms with the minimal
//!   trait bounds necessary, as well as a generic interface.
//! * [`text`]: This extends the general diffing functionality to text (and more
//!   specifically line) based diff operations.
//!
//! ## Features
//!
//! The crate by default does not have any dependencies however for some use
//! cases it's useful to pull in extra functionality.  Likewise you can turn
//! off some functionality.
//!
//! * `text`: this feature is enabled by default and enables the [`text`] module.
//!   If the crate is used without default features it's removed.
//! * `unicode`: when this feature is enabled the text diffing functionality
//!   gains the ability to diff on a grapheme instead of character level.  This
//!   is particularly useful when working with text containing emojis.  This
//!   pulls in some relatively complex dependencies for working with the unicode
//!   database.
//! * `bytes`: this feature adds support for working with byte slices in the
//!   [`text`] module in addition to unicode strings.  This pulls in the
//!   [`bstr`] dependency.
//! * `inline`: this feature gives access to additional functionality of the
//!   [`text`] module to provide inline information about which values changed
//!   in a line diff.  This currently also enables the `unicode` feature.
#![warn(missing_docs)]
pub mod algorithms;
pub mod text;

mod types;
pub use self::types::*;

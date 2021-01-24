//! This crate implements diffing utilities.  It attempts to provide an abstraction
//! interface over different types of diffing algorithms.  It's based on the
//! the diff algorithm implementations of [pijul](https://pijul.org/).
//! ```rust
//! # #[cfg(feature = "text")] {
//! use similar::text::TextDiff;
//! # let old_text = "";
//! # let new_text = "";
//! let diff = TextDiff::from_chars("Hello World", "Hallo Welt");
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
//! * `unicode`: when this feature is enabled the text diffing functionality
//!   gains the ability to diff on a grapheme instead of character level.  This
//!   is particularly useful when working with text containing emojis.
//! * `text`: this feature is enabled by default and enables the [`text`] module.
//!   If the crate is used without default features it's removed.
pub mod algorithms;
pub mod text;

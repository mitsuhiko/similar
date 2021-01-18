//! This crate implements diffing utilities.  It attempts to provide an abstraction
//! interface over different types of diffing algorithms.
//!
//! # Example
//!
//! ```rust
//! use similar::{text::diff_lines, algorithms::Algorithm};
//! let old = "Hello World!\nMore stuff here.";
//! let new = "Oh well World!\nMore stuff here.";
//! for op in diff_lines(Algorithm::Myers, old, new) {
//!     println!("{}", op);
//! }
//! ```
//!
//! # Components
//!
//! The crate is split into two components:
//!
//! * [`algorithms`]: This implements the different types of diffing algorithms.
//!   It provides both low level access to the algorithms with the minimal
//!   trait bounds necessary, as well as a generic interface.
//! * [`text`]: This extends the general diffing functionality to text (and more
//!   specifically line) based diff operations.
pub mod algorithms;
pub mod text;

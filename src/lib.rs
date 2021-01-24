//! This crate implements diffing utilities.  It attempts to provide an abstraction
//! interface over different types of diffing algorithms.  It's based on the
//! the diff algorithm implementations of [pijul](https://pijul.org/).
//!
//! The crate is split into two levels:
//!
//! * [`algorithms`]: This implements the different types of diffing algorithms.
//!   It provides both low level access to the algorithms with the minimal
//!   trait bounds necessary, as well as a generic interface.
//! * [`text`]: This extends the general diffing functionality to text (and more
//!   specifically line) based diff operations.
pub mod algorithms;
pub mod text;

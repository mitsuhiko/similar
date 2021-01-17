//! Various diff (longest common subsequence) algorithms.
//!
//! The implementations of the algorithms in this module are relatively low
//! level and expose the most generic bounds possible for the algorithm.  To
//! use them you would typically use the higher level API if possible but
//! direct access to these algorithms can be useful in some cases.
//!
//! All these algorithms provide a `diff` function which takes two indexable
//! objects (for instance slices) and a [`DiffHook`].  As the diff is generated
//! the diff hook is invoked.  Note that the diff hook does not get access to
//! the actual values but only the indexes.  This is why the diff hook is not
//! used outside of the raw algorithm implementations as for most situations
//! access to the values is useful of required.

mod hook;
mod replace;

pub use hook::*;
pub use replace::*;

pub mod myers;
pub mod patience;

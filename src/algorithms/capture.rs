use crate::algorithms::hook::DiffHook;
use std::convert::Infallible;

/// Utility enum to capture a diff operation.
///
/// This is used by [`Capture`](crate::algorithms::Capture).
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum DiffOp {
    /// A segment is equal (see [`DiffHook::equal`])
    Equal {
        old_index: usize,
        new_index: usize,
        len: usize,
    },
    /// A segment was deleted (see [`DiffHook::delete`])
    Delete {
        old_index: usize,
        old_len: usize,
        new_index: usize,
    },
    /// A segment was inserted (see [`DiffHook::insert`])
    Insert {
        old_index: usize,
        new_index: usize,
        new_len: usize,
    },
    /// A segment was replaced (see [`DiffHook::replace`])
    Replace {
        old_index: usize,
        old_len: usize,
        new_index: usize,
        new_len: usize,
    },
}

/// A [`DiffHook`] that captures all diff operations.
#[derive(Default, Clone)]
pub struct Capture(Vec<DiffOp>);

impl Capture {
    /// Creates a new capture hook.
    pub fn new() -> Capture {
        Capture::default()
    }

    /// Converts the capture hook into a vector.
    pub fn into_vec(self) -> Vec<DiffOp> {
        self.0
    }

    /// Accesses the captured operations.
    pub fn ops(&self) -> &[DiffOp] {
        &self.0
    }
}

impl DiffHook for Capture {
    type Error = Infallible;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Equal {
            old_index,
            new_index,
            len,
        });
        Ok(())
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Delete {
            old_index,
            old_len,
            new_index,
        });
        Ok(())
    }

    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Insert {
            old_index,
            new_index,
            new_len,
        });
        Ok(())
    }

    fn replace(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.0.push(DiffOp::Replace {
            old_index,
            old_len,
            new_index,
            new_len,
        });
        Ok(())
    }
}

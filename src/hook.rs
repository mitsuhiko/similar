/// A trait for reacting to an edit script from the "old" version to
/// the "new" version.
pub trait DiffHook: Sized {
    /// The error produced from the hook methods.
    type Error;

    /// Called when lines with indices `old_index` (in the old version) and
    /// `new_index` (in the new version) start an section equal in both
    /// versions, of length `len`.
    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        let _ = old_index;
        let _ = new_index;
        let _ = len;
        Ok(())
    }

    /// Called when a section of length `old_len`, starting at `old_index`,
    /// needs to be deleted from the old version.
    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        let _ = old_index;
        let _ = old_len;
        let _ = new_index;
        Ok(())
    }

    /// Called when a section of the new version, of length `new_len`
    /// and starting at `new_index`, needs to be inserted at position `old_index'.
    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        let _ = old_index;
        let _ = new_index;
        let _ = new_len;
        Ok(())
    }

    /// Called when a section of the old version, starting at index
    /// `old_index` and of length `old_len`, needs to be replaced with a
    /// section of length `new_len`, starting at `new_index`, of the new
    /// version.
    ///
    /// The default implementations invokes `delete` and `insert`.
    ///
    /// You can use the [`Replace`](crate::algorithms::Replace) hook to
    /// automatically generate these.
    fn replace(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.delete(old_index, old_len, new_index)?;
        self.insert(old_index, new_index, new_len)
    }

    /// Always called at the end of the algorithm.
    fn finish(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<'a, D: DiffHook + 'a> DiffHook for &'a mut D {
    type Error = D::Error;

    fn equal(&mut self, old_index: usize, new_index: usize, len: usize) -> Result<(), Self::Error> {
        (*self).equal(old_index, new_index, len)
    }

    fn delete(
        &mut self,
        old_index: usize,
        old_len: usize,
        new_index: usize,
    ) -> Result<(), Self::Error> {
        (*self).delete(old_index, old_len, new_index)
    }

    fn insert(
        &mut self,
        old_index: usize,
        new_index: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        (*self).insert(old_index, new_index, new_len)
    }

    fn replace(
        &mut self,
        old: usize,
        old_len: usize,
        new: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        (*self).replace(old, old_len, new, new_len)
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        (*self).finish()
    }
}

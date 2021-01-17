/// A trait for reacting to an edit script from the "old" version to
/// the "new" version.
pub trait DiffHook: Sized {
    type Error;

    /// Called when lines with indices `old` (in the old version) and
    /// `new` (in the new version) start an section equal in both
    /// versions, of length `len`.
    fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), Self::Error> {
        let _old = old;
        let _new = new;
        let _len = len;
        Ok(())
    }

    /// Called when a section of length `len`, starting at `old`,
    /// needs to be deleted from the old version.
    fn delete(&mut self, old: usize, old_len: usize, new: usize) -> Result<(), Self::Error> {
        let _old = old;
        let _old_len = old_len;
        let _new = new;
        Ok(())
    }

    /// Called when a section of the new version, of length `new_len`
    /// and starting at `new`, needs to be inserted at position `old'.
    fn insert(&mut self, old: usize, new: usize, new_len: usize) -> Result<(), Self::Error> {
        let _old = old;
        let _new = new;
        let _new_len = new_len;
        Ok(())
    }

    /// Called when a section of the old version, starting at index
    /// `old` and of length `old_len`, needs to be replaced with a
    /// section of length `new_len`, starting at `new`, of the new
    /// version.
    ///
    /// The default implementations invokes `delete` and `insert`.
    fn replace(
        &mut self,
        old: usize,
        old_len: usize,
        new: usize,
        new_len: usize,
    ) -> Result<(), Self::Error> {
        self.delete(old, old_len, new)?;
        self.insert(old, new, new_len)
    }

    /// Always called at the end of the algorithm.
    fn finish(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<'a, D: DiffHook + 'a> DiffHook for &'a mut D {
    type Error = D::Error;

    fn equal(&mut self, old: usize, new: usize, len: usize) -> Result<(), Self::Error> {
        (*self).equal(old, new, len)
    }

    fn delete(&mut self, old: usize, len: usize, new: usize) -> Result<(), Self::Error> {
        (*self).delete(old, len, new)
    }

    fn insert(&mut self, old: usize, new: usize, new_len: usize) -> Result<(), Self::Error> {
        (*self).insert(old, new, new_len)
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

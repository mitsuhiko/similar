use std::ops::Range;

/// Utility function to check if a range is empty that works on older rust versions
#[inline(always)]
pub(crate) fn is_empty_range<T: PartialOrd>(range: &Range<T>) -> bool {
    !(range.start < range.end)
}

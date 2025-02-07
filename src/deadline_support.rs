use core::time::Duration;

#[cfg(all(feature = "std", not(feature = "wasm32_web_time")))]
pub use std::time::Instant;

/// WASM (browser) specific instant type.
///
/// This type is only available when the `wasm32_web_time` feature is enabled.  In that
/// case this is an alias for [`web_time::Instant`].
#[cfg(all(not(feature = "std"), feature = "wasm32_web_time"))]
pub use web_time::Instant;

#[cfg(not(any(feature = "std", feature = "wasm32_web_time")))]
pub type Instant = ();

/// Checks if a deadline was exceeded.
pub fn deadline_exceeded(deadline: Option<Instant>) -> bool {
    match deadline {
        #[allow(unused)]
        Some(deadline) => {
            #[cfg(not(any(feature = "std", feature = "wasm32_web_time")))]
            {
                false
            }
            #[cfg(any(feature = "std", feature = "wasm32_web_time"))]
            {
                Instant::now() > deadline
            }
        }
        None => false,
    }
}

/// Converts a duration into a deadline.
#[allow(unused)]
pub fn duration_to_deadline(add: Duration) -> Option<Instant> {
    #[cfg(not(any(feature = "std", feature = "wasm32_web_time")))]
    {
        None
    }
    #[cfg(any(feature = "std", feature = "wasm32_web_time"))]
    {
        Instant::now().checked_add(add)
    }
}

//! Shared utility functions for memory alignment, formatting, and platform queries.

use mmtk::util::Address;
use std::mem::MaybeUninit;

pub struct FormattedSize(pub usize);

impl std::fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        const UNITS: [&str; 6] = ["B", "KiB", "MiB", "GiB", "TiB", "PiB"];

        if self.0 < 1024 {
            return write!(f, "{} B", self.0);
        }

        let mut size = self.0 as f64;
        let mut unit = 0;
        while size >= 1024.0 && unit + 1 < UNITS.len() {
            size /= 1024.0;
            unit += 1;
        }

        write!(f, "{size:.2}{}", UNITS[unit])
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize(size)
}

/// Fixedpoint function to find a fixed point of a function `f` applied to an initial value `start`.
/// It will iterate until either the maximum number of iterations is reached or the function reports
/// that it made no further progress.
///
/// Stabilized value is the last value returned with `changed == false`.
pub fn fixedpoint<T, F>(start: T, max: Option<usize>) -> impl FnOnce(F) -> T
where
    F: FnMut(T) -> (T, bool),
{
    move |mut f: F| {
        let max_iterations = max.unwrap_or(usize::MAX);

        let mut current = start;

        for _ in 0..max_iterations {
            let (next, changed) = f(current);
            if !changed {
                return next;
            }
            current = next;
        }

        current
    }
}

#[derive(Debug, Clone)]
pub struct DlInfo {
    pub fname: Option<String>,
    pub fbase: Address,
    pub sname: Option<String>,
    pub saddr: Address,
}

pub fn dladdr(addr: Address) -> std::io::Result<DlInfo> {
    unsafe {
        let mut dl_info = MaybeUninit::<libc::Dl_info>::uninit();
        let result = libc::dladdr(addr.to_mut_ptr(), dl_info.as_mut_ptr());
        if result == 0 {
            return Err(std::io::Error::other("dladdr failed"));
        }

        let dl_info = dl_info.assume_init();
        let fname = if dl_info.dli_fname.is_null() {
            None
        } else {
            Some(
                std::ffi::CStr::from_ptr(dl_info.dli_fname)
                    .to_string_lossy()
                    .into_owned(),
            )
        };
        let sname = if dl_info.dli_sname.is_null() {
            None
        } else {
            Some(
                std::ffi::CStr::from_ptr(dl_info.dli_sname)
                    .to_string_lossy()
                    .into_owned(),
            )
        };
        Ok(DlInfo {
            fname,
            fbase: Address::from_ptr(dl_info.dli_fbase),
            sname,
            saddr: Address::from_ptr(dl_info.dli_saddr),
        })
    }
}

pub fn align_allocation_no_fill(region: Address, alignment: usize, offset: usize) -> Address {
    let mask = (alignment - 1) as isize;
    let neg_off: isize = -(offset as isize);
    let delta = neg_off.wrapping_sub(region.as_usize() as isize) & mask;

    region + delta
}

pub mod easy_bitfield;
pub mod generic_static;
pub mod pass_profile;

use crate::rsgc::{Gc, Trace, alloc::array::Array};
use crate::runtime::value::Value;
use mmtk::util::Address;
use std::{mem::MaybeUninit, rc::Rc};

pub struct FormattedSize {
    size: usize,
}

impl std::fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}

/// Compare two trees for equality.
pub trait TreeEq {
    fn tree_eq(&self, other: &Self) -> bool;
}

/// Fixedpoint function to find a fixed point of a function `f` applied to an initial value `start`.
/// It will iterate until either the maximum number of iterations is reached or the value stabilizes.
///
/// Stabilized value is the last value that did not change after applying `f`.
pub fn fixedpoint<T, F>(start: T, max: Option<usize>) -> impl FnOnce(F) -> T
where
    T: TreeEq + Clone,
    F: Fn(&T) -> T,
{
    move |f: F| {
        let max_iterations = max.unwrap_or(usize::MAX);

        let mut current = start;

        for _ in 0..max_iterations {
            let next = f(&current);
            if next.tree_eq(&current) {
                return current;
            }
            current = next;
        }

        current
    }
}

impl<T: TreeEq> TreeEq for Rc<T> {
    fn tree_eq(&self, other: &Self) -> bool {
        self.as_ref().tree_eq(other.as_ref())
    }
}
impl<T: TreeEq> TreeEq for Vec<T> {
    fn tree_eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.iter().zip(other.iter()).all(|(a, b)| a.tree_eq(b))
    }
}

impl<'gc, T: TreeEq> TreeEq for Gc<'gc, T> {
    fn tree_eq(&self, other: &Self) -> bool {
        (**self).tree_eq(&**other)
    }
}

impl<'gc> TreeEq for Value<'gc> {
    fn tree_eq(&self, other: &Self) -> bool {
        self.r5rs_equal(*other)
    }
}

impl<T: TreeEq + Trace> TreeEq for Array<T> {
    fn tree_eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.iter().zip(other.iter()).all(|(a, b)| a.tree_eq(b))
    }
}

impl<T: TreeEq> TreeEq for Option<T> {
    fn tree_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.tree_eq(b),
            (None, None) => true,
            _ => false,
        }
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
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "dladdr failed",
            ));
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
pub mod graph;
#[cfg(test)]
mod tests {
    use super::*;

    // Helper implementation for tests
    impl TreeEq for i32 {
        fn tree_eq(&self, other: &Self) -> bool {
            self == other
        }
    }

    #[test]
    fn test_fixedpoint_stabilizes() {
        let start = 0;
        let f = |x: &i32| if *x < 5 { *x + 1 } else { *x };
        let result = fixedpoint(start, Some(10))(f);
        assert_eq!(result, 5);
    }

    #[test]
    fn test_fixedpoint_reaches_max_iterations() {
        let start = 0;
        let f = |x: &i32| *x + 1;
        let result = fixedpoint(start, Some(10))(f);
        assert_eq!(result, 10);
    }

    #[test]
    fn test_fixedpoint_no_max_iterations() {
        let start = 0;
        let f = |x: &i32| if *x < 100 { *x + 1 } else { *x };
        let result = fixedpoint(start, None)(f);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_fixedpoint_immediate_stabilization() {
        let start = 5;
        let f = |x: &i32| if *x < 5 { *x + 1 } else { *x };
        let result = fixedpoint(start, Some(10))(f);
        assert_eq!(result, 5);
    }

    #[test]
    fn test_fixedpoint_with_vec() {
        let start = vec![1, 2, 3];
        let f = |v: &Vec<i32>| {
            if v.len() < 5 {
                let mut next_v = v.clone();
                next_v.push(v.len() as i32 + 1);
                next_v
            } else {
                v.clone()
            }
        };
        let result = fixedpoint(start, Some(10))(f);
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }
}

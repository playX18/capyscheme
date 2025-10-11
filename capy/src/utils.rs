use std::rc::Rc;

use rsgc::{Gc, Trace, alloc::array::Array};

use crate::runtime::value::Value;

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

use std::rc::Rc;


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

pub mod graph;
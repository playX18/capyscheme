//! Continuation-Passing Style (CPS) IR and compiler to machine code.

pub mod builder;
pub mod closure;
pub mod contify;
pub mod fold;
pub mod free_vars;
pub mod optimizer;
pub mod peval;
pub mod pretty;
pub mod reify;
pub mod reify_primitives;
//pub mod sexp;
pub mod term;

use std::hash::BuildHasherDefault;

use hashlink::{LinkedHashMap, LinkedHashSet};
pub use optimizer::rewrite_func;
pub use reify::{ReifyInfo, reify};
use simplehash::Fnv1aHasher64;

/*
    Use fnv1a64 in compiler because we mainly hash by pointer
    which is just simple integer
*/
pub type Set<K> = LinkedHashSet<K, BuildHasherDefault<Fnv1aHasher64>>;
pub type Map<K, V> = LinkedHashMap<K, V, BuildHasherDefault<Fnv1aHasher64>>;

pub enum SingleValueSet<K> {
    Bottom,
    Singleton(K),
    Top,
}

impl<K> SingleValueSet<K> {
    pub fn join(self, other: Self) -> Self
    where
        K: PartialEq,
    {
        match (self, other) {
            (Self::Top, _) | (_, Self::Top) => Self::Top,
            (Self::Singleton(e1), Self::Singleton(e2)) if e1 == e2 => Self::Singleton(e1),
            (Self::Singleton(_), Self::Singleton(_)) => Self::Top,
            (Self::Bottom, other) | (other, Self::Bottom) => other,
        }
    }
}

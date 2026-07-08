//! Graphical CPS from "Compiling with Continuations, Continued".

pub mod clone;
pub mod convert;
pub mod fold;
mod dom_contify;
pub mod graph;
pub mod linear;
pub mod linear_pretty;
pub mod optimize;
pub mod reify;
mod scc_contify;
pub mod worklist;

use std::hash::BuildHasherDefault;
use simplehash::Fnv1aHasher64;

pub type Set<K> = im::HashSet<K, BuildHasherDefault<Fnv1aHasher64>>;
pub type Map<K, V> = im::HashMap<K, V, BuildHasherDefault<Fnv1aHasher64>>;

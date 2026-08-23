//! Graphical CPS from "Compiling with Continuations, Continued".

pub mod analysis;
pub mod clone;
pub mod convert;
mod dom_contify;
pub mod fixmerge;
pub mod flow;
pub mod fold;
pub mod graph;
pub mod optimize;
pub mod pretty;
pub mod reify;
pub mod share;
mod scc_contify;
pub mod worklist;

use simplehash::Fnv1aHasher64;
use std::hash::BuildHasherDefault;

pub type Set<K> = im::HashSet<K, BuildHasherDefault<Fnv1aHasher64>>;
pub type Map<K, V> = im::HashMap<K, V, BuildHasherDefault<Fnv1aHasher64>>;

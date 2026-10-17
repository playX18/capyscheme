//! Graphical CPS from "Compiling with Continuations, Continued".
//!
//! Pass order, driven by [`optimize::optimize_graph_func_to_ssa`]:
//!
//! 1. [`graph`]: the mutable graph both the builder
//!    (`expander::compile_cps`) and the passes share.
//! 2. [`optimize`]: the worklist optimizer ([`optimize::optimize_graph`]):
//!    collect redexes, then simplify within a gas budget, eta
//!    reduction, constant folding ([`fold`]), branch simplification,
//!    singleton inlining, contification (`scc_contify`/`dom_contify`,
//!    picked by `ContifyMode`).
//! 3. [`reify`]: reify continuations (`reify_graph`).
//! 4. [`fixmerge`]: merge nested `Fix`/`Letk` groups.
//! 5. [`analysis`] (stages), [`flow`] (0-CFA), [`share`] (shared
//!    environments, currently disabled) feed closure lowering.
//! 6. `cfg::lower::lower_graph` converts to SSA (outside this module).
//!
//! Supporting cast: [`clone`] (subterm cloning), [`convert`] (result
//! types), [`worklist`] (queues), [`pretty`] (dumps). [`Set`]/[`Map`] are
//! the persistent collections passes share.

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
mod scc_contify;
pub mod share;
pub mod worklist;

use simplehash::Fnv1aHasher64;
use std::hash::BuildHasherDefault;

pub type Set<K> = im::HashSet<K, BuildHasherDefault<Fnv1aHasher64>>;
pub type Map<K, V> = im::HashMap<K, V, BuildHasherDefault<Fnv1aHasher64>>;

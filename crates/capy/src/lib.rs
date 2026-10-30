#![allow(
    incomplete_features,
    internal_features,
    dead_code,
    path_statements,
    unused_must_use,
    unused_mut,
    unused_unsafe
)]
#![feature(
    thread_local,
    arbitrary_self_types,
    try_trait_v2,
    link_llvm_intrinsics,
    min_specialization,
    const_type_name,
    try_trait_v2_residual,
    explicit_tail_calls,
    rust_preserve_none_cc
)]

//! CapyScheme: R6RS/R7RS Scheme compiler and runtime.

pub mod api;
pub mod bytecode;
pub mod compiler;
pub mod expander;
pub mod frontend;
pub mod heap;
pub mod runtime;
pub mod utils;

pub use capy_derive::__unelide_lifetimes;
pub use capy_derive::Trace;
pub use runtime::sched;

// Curated GC surface. Prefer `runtime::MutationContext` for mutator work.
pub use heap::Global as HeapGlobal;
pub use heap::traits::Trace;
#[doc(hidden)]
pub use heap::{__DynRootable, Mutation, Root, Rootable, Visitor};
pub use heap::{
    ALLOWED_GC_PLAN_NAMES, Ephemeron, Frame, GarbageCollector, Gc, IndexWrite, NarrowGc, Pin,
    Scope, Space, Weak, WeakProcessor, Write, add_marking_constraint, is_allowed_plan,
    set_parallel_constraints, validate_plan,
};

pub mod prelude {
    pub use crate::global;
    pub use crate::heap::{Ephemeron, Frame, Gc, Pin, Scope, Space, Trace, Weak, Write, barrier};
    pub use crate::list;
    pub use crate::runtime::global::Global;
    pub use crate::runtime::prelude::*;
    pub use crate::runtime::vm::Call;
    pub use crate::vector;
    pub use capy_derive::scheme;
    pub use capy_derive::scm_match;
}

pub use capy_derive::scheme;
pub use capy_derive::scm_match;

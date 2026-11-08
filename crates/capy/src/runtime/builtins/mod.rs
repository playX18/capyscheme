//! Scheme-visible primitives, grouped by data domain.
//!
//! These modules contain the `#[scheme]` registration surface: arithmetic,
//! strings, vectors, lists, hashing, persistent collections, evaluation,
//! expansion, debugging, and memoization. Engine services that interpreters
//! and compiled code call directly stay in [`crate::runtime::vm`]; the
//! internal primitive ABI stays in [`crate::runtime::prims`].

pub mod arith;
pub mod debug;
pub mod eval;
pub mod expand;
pub mod format;
pub mod gc;
pub mod hash;
pub mod list;
pub mod memoize;
pub mod persistent_map;
pub mod persistent_set;
pub mod strings;
pub mod vector;

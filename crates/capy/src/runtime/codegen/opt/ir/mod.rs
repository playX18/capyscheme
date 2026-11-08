//! The SSA IR of the Tier 2 optimizer.
//!
//! A [`Func`](func::Func) is a control-flow graph of blocks with parameters,
//! in SSA form. Block parameters carry the values that join at a merge point
//! in place of phi nodes. Each block ends in a terminator. An instruction that
//! can deopt, and every call, names a [`FrameState`](frame_state::FrameState)
//! for the bytecode frames it must rebuild.
//!
//! Layout:
//!
//! - [`types`]: the type lattice and code and class references.
//! - [`inst`]: operations, terminators, and the effect model.
//! - [`func`]: blocks, values, the arenas, and dominators.
//! - [`frame_state`]: frame states with parent links for inlined frames.
//! - [`verify`]: the structural checks run under `CAPY_OPT_VERIFY=1`.
//! - [`print`]: the textual form for `CAPY_OPT_DUMP`.

// cranelift-entity's `entity_impl!` expands to `std::u32::MAX`.
#![allow(deprecated)]

pub mod frame_state;
pub mod func;
pub mod inst;
pub mod print;
pub mod types;
pub mod verify;

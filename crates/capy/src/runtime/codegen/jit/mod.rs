//! The JIT: bytecode procedures compiled to machine code, one image each.
//!
//! The baseline tier ([`compile`]) emits the interpreter's templates for a
//! whole procedure, with operands as immediates and control as jumps between
//! instructions. Its template context is
//! [`CompiledContext`](context::CompiledContext), with
//! [`ProcTargets`](context::ProcTargets) for the procedure and
//! [`OriginExits`](context::OriginExits) for an arithmetic stub. Registers are
//! frame slots (`[SFP + 8r]`), constants and metadata are absolute addresses
//! (code buffers never move), and a call stores the address of a local label
//! with a return-point header. Slow paths are the shared ones. One that
//! resumes a frame continues in compiled code when it has the target
//! instruction ([`resume`]), else in the interpreter. Frames are the bytecode
//! frames, so a long loop transfers into compiled code at a `loop_hint`.
//!
//!
//! A block's tier-up state is its atomic [`Promotion`](crate::runtime::value::Promotion):
//! `Interpreted`, `Submitted` (one winner of the compare-and-swap builds or
//! queues it), `Installed`, or `Failed`. A worker's finished unit rides on
//! the block's `CodeBuffer`, and a failed build sets `build_failed`, so a
//! result never outlives its block.
//!
//! Pipeline, lazy edges, template contexts and tiers are described in
//! `docs/JIT.md`. The lazy machinery is in `lazy`, and each compiled
//! procedure is a `CodeUnit` (`unit.rs`).
//!
//! Layout:
//!
//! - [`registry`]: each unit's resume table and byte counters, and
//!   lock-free [`resume`] of a slow path into compiled code.
//! - [`context`]: the compiled template context, parameterized by its
//!   `Targets` and a spec source.
//! - [`entry`]: entering a callee, shared by compiled code.
//! - [`compile`]: the baseline compiler, and the claim, build, attach and
//!   install steps of publication.
//! - [`promotion`]: the promotion implementation ([`request_tier_up`](promotion::request_tier_up))
//!   and its [`HotnessPolicy`](promotion::HotnessPolicy).
//! - [`worker`]: the async pipeline (a rooted submit queue and the worker
//!   threads that build and attach units).
//! - [`arith_site`]: arithmetic sites, the [`Lazy`](super::lazy::Lazy)
//!   implementation for numeric fast paths.
//! - [`dump`]: listings and dumps of compiled code, for `%jit-disassembly`
//!   and `CAPY_JIT_DUMP_DIR`.

pub(crate) mod arith_site;
mod compile;
pub(crate) mod config;
mod context;
mod dump;
mod entry;
pub(crate) mod promotion;
pub(crate) mod registry;
pub(crate) mod version;
pub(crate) mod worker;

pub use compile::compile;
pub(crate) use compile::install;
pub use config::maybe_report_jit_stats;
pub(crate) use config::{SLOW_CONTROL, SLOW_HELPER};
pub use dump::listing_for;
pub(crate) use entry::{emit_enter_callee, emit_interpreter_entry};
pub(crate) use promotion::{HotnessPolicy, ThresholdPolicy, request_tier_up};
pub use registry::{compiled_count, resume};
pub use worker::JobQueue;

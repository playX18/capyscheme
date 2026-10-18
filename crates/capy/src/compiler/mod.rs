//! Compilation pipeline: expander IR -> graph CPS -> CFG/SSA -> bytecode -> FASL.
//!
//! [`lower_to_cps`] (in the private `pipeline` module) is the frontend
//! driver. It runs the expander-level passes, primitive resolution,
//! free-variable annotation, letrec rectification, recursive-binding
//! rewriting, well-known-procedure expansion, assignment elimination
//! (skipped when the self-hosted Scheme pipeline already ran them),
//! lowers to graph CPS, optimizes and converts to SSA
//! ([`cps::optimize::optimize_graph_func_to_ssa`]), and returns a
//! [`LoweredProgram`]. The private `object` module is the backend driver:
//! SSA to bytecode ([`bytecode::compile_program`]), then the FASL image.
//!
//! - [`tree`]: the IR node types, re-exported for the expander.
//! - [`cps`]: graph CPS: the mutable graph, the worklist optimizer, and
//!   the analyses to run closure lowering.
//! - [`cfg`]: SSA-form CFG plus the finishing passes (rest args, dead
//!   code, switch inference, cache ops).
//! - [`bytecode`]: the backend: CFG procedures to assembled code blocks.
//! - [`prims::def`](crate::runtime::prims::def): the `prim-call` operations.

mod bootstrap;
pub mod bytecode;
pub mod cfg;
pub mod cps;
mod dump;
mod object;
mod pipeline;
pub mod tree;

pub use bootstrap::compile_file;
pub use object::{BackendDumpOptions, CompilationOptions};
pub use pipeline::lower_to_cps;

pub(crate) use dump::{
    begin_compilation_artifact, merge_compile_dump_options, resolve_artifact_dump_path,
};
pub(crate) use object::compile_lowered_to_fasl_bytes;
pub(crate) use pipeline::{
    DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, lower_expanded_to_cps,
};

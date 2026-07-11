//! Static Basic Block Versioning (SBBV) on the SSA IR.

mod config;
pub(crate) mod dump;
mod expand;
mod infer;
mod liveness;
mod merge;
mod specialize;
mod types;

pub use specialize::BlockAnnotation;

use crate::compiler::ssa::Procedure;

/// Runs the SBBV pipeline on a procedure when enabled by configuration.
pub(crate) fn run<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    if !config::enabled() {
        return procedure;
    }
    dump::maybe_dump_procedure("pre-expand", &procedure, None);
    let expanded = expand::expand_procedure(procedure);
    dump::maybe_dump_procedure("post-expand", &expanded, None);
    // Promote dominated free live-ins to explicit block params before
    // versioning. Specialization reuses block versions across predecessors;
    // free uses remapped from one reach would otherwise fail Cranelift
    // dominance when another edge enters the same version.
    let threaded = liveness::thread_live_ins(expanded);
    dump::maybe_dump_procedure("post-thread", &threaded, None);
    let (specialized, annotations) =
        specialize::specialize_procedure(threaded, config::version_limit());
    dump::maybe_dump_procedure("post-specialize", &specialized, Some(&annotations));
    specialized
}

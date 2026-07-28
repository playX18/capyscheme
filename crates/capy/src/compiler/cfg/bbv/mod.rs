//! Static Basic Block Versioning (SBBV) on the mutable-uvar IR.

mod config;
pub(crate) mod dump;
mod expand;
mod infer;
mod liveness;
mod merge;
mod specialize;
mod types;

pub use specialize::BlockAnnotation;

use crate::compiler::cfg::Procedure;

/// Runs the SBBV pipeline on a procedure when enabled by configuration.
pub(crate) fn run<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    if !config::enabled() {
        return procedure;
    }
    dump::maybe_dump_procedure("pre-expand", &procedure, None);
    let expanded = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.bbv.expand");
        expand::expand_procedure(procedure)
    };
    dump::maybe_dump_procedure("post-expand", &expanded, None);
    let (specialized, annotations) = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.bbv.specialize");
        specialize::specialize_procedure(expanded, config::version_limit())
    };
    dump::maybe_dump_procedure("post-specialize", &specialized, Some(&annotations));
    specialized
}

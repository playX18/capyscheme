pub mod bbv;
mod cache;
mod constant;
mod graph;
mod ir;
pub(crate) mod lower;
mod pretty;
mod rest;
mod switch;

use cache::lower_cache_operations;
use constant::hoist_constants;
pub use ir::*;
pub use pretty::{render_program, render_program_with_annotations};
use rest::lower_rest_arguments;
pub(crate) use switch::infer_switches;

pub(crate) fn finish_procedure<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    // Rest lowering must run before SBBV expand. Otherwise `car`/`cdr` on a rest
    // formal become `pair?` + `car/unchecked` (and a raise that mentions rest),
    // which `lower_rest_arguments` treats as incompatible and falls back to
    // RestToList — defeating RestLength/RestRef for `(lambda args (case (length
    // args) ...))` and similar shapes.
    let after_rest = lower_rest_arguments(procedure);
    let after_sbbv = bbv::run(after_rest);
    let finished = hoist_constants(lower_cache_operations(infer_switches(after_sbbv)));
    // Dump the IR that actually reaches Cranelift (after all SSA finish passes).
    if crate::compiler::dump::sbbv_dump_stage_enabled("post-finish")
        || crate::compiler::dump::sbbv_dump_stage_enabled("all")
        || std::env::var_os("CAPY_SBBV_DUMP")
            .is_some_and(|v| matches!(v.to_str(), Some("1" | "on" | "true" | "all")))
    {
        bbv::dump::maybe_dump_procedure("post-finish", &finished, None);
    }
    finished
}

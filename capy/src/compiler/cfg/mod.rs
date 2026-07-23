pub mod bbv;
mod cache;
mod constant;
pub mod effects;
mod graph;
mod ir;
pub(crate) mod lower;
mod pretty;
mod rest;
mod switch;

use cache::lower_cache_operations;
use constant::hoist_constants;
use effects::eliminate_dead_effect_free_instructions;
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
    let after_rest = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.rest");
        lower_rest_arguments(procedure)
    };
    let after_effects = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.dce1");
        eliminate_dead_effect_free_instructions(after_rest)
    };
    let after_sbbv = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.sbbv");
        bbv::run(after_effects)
    };
    let after_sbbv_effects = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.dce2");
        eliminate_dead_effect_free_instructions(after_sbbv)
    };
    let after_switch = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.switch");
        infer_switches(after_sbbv_effects)
    };
    let after_cache = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.cache");
        lower_cache_operations(after_switch)
    };
    let finished = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.hoist");
        hoist_constants(after_cache)
    };
    // Dump the IR that actually reaches Cranelift (after all CFG finish passes).
    if crate::compiler::dump::sbbv_dump_stage_enabled("post-finish")
        || crate::compiler::dump::sbbv_dump_stage_enabled("all")
        || std::env::var_os("CAPY_SBBV_DUMP")
            .is_some_and(|v| matches!(v.to_str(), Some("1" | "on" | "true" | "all")))
    {
        bbv::dump::maybe_dump_procedure("post-finish", &finished, None);
    }
    finished
}

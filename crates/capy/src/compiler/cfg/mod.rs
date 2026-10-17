mod cache;
pub mod effects;
mod graph;
mod ir;
pub(crate) mod lower;
mod pretty;
mod rest;
mod switch;

use cache::lower_cache_operations;
use effects::eliminate_dead_instructions;
pub use ir::*;
pub use pretty::render_program;
use rest::lower_rest_arguments;
pub(crate) use switch::infer_switches;

pub(crate) fn finish_procedure<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    let after_rest = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.rest");
        lower_rest_arguments(procedure)
    };
    let after_effects = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.dce");
        eliminate_dead_instructions(after_rest)
    };
    let after_switch = {
        let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.switch");
        infer_switches(after_effects)
    };
    // Constants stay operands: the bytecode backend loads each one where it
    // is used, into the register that needs it.
    let _p = crate::utils::pass_profile::ProfileScope::new("cfg.finish.cache");
    lower_cache_operations(after_switch)
}

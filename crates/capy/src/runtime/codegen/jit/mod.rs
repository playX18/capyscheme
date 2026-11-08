//! The JIT

pub(crate) mod arith_site;
pub(crate) mod compile;
pub(crate) mod config;
pub(crate) mod context;
mod dump;
pub(crate) mod entry;
pub(crate) mod promotion;
pub(crate) mod registry;
pub(crate) mod version;
pub(crate) mod worker;

pub use compile::compile;
pub(crate) use compile::install;
pub use config::maybe_report_jit_stats;
pub(crate) use config::{SLOW_CONTROL, SLOW_HELPER};
pub use dump::listing_for;
pub(crate) use entry::{emit_enter_callee, emit_interpreter_entry, emit_jump_compiled_entry};
pub(crate) use promotion::{HotnessPolicy, ThresholdPolicy, heal_to_entry, request_tier_up};
pub(crate) use registry::osr_entry;
pub use registry::{compiled_count, resume};
pub use worker::JobQueue;

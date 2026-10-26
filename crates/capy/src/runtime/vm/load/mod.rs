use crate::runtime::MutationContext;

pub(crate) mod artifact;
mod compile;
mod paths;
pub(crate) mod policy;
mod scheme;

pub use compile::load_thunk_in_vicinity;
pub(crate) use compile::{ExpandedJob, SourceJob, run_source_job};
pub use paths::{compiled_is_fresh, find_path_to, init_load_path};
pub(crate) use scheme::run_expanded_job;

pub fn init_load<'gc>(ctx: MutationContext<'gc>) {
    scheme::load_ops::register(ctx);
}

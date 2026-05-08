use crate::runtime::Context;

pub(crate) mod artifact;
mod compile;
mod lock;
mod paths;
pub(crate) mod policy;
mod scheme;

pub use compile::load_thunk_in_vicinity;
pub use paths::{compiled_is_fresh, find_path_to, init_load_path};

pub fn init_load<'gc>(ctx: Context<'gc>) {
    scheme::load_ops::register(ctx);
}

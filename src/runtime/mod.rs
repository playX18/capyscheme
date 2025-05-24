use std::sync::Once;

use rsgc::context::Mutation;

pub mod errors;
pub mod value;
pub mod vmthread;
pub mod thread;
pub mod vm;


static ONCE: Once = Once::new();

pub fn init<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        value::weak_set::init_weak_sets(mc);
        value::strings::init_strings(mc);
        value::symbols::init_symbols(mc);

        let _ = &*vmthread::VM_THREAD;
    });
}

pub use thread::{Context, Scheme, State};

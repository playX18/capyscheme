use std::sync::Once;

use rsgc::context::Mutation;

pub mod errors;
pub mod thread;
pub mod value;
pub mod vm;
pub mod vmthread;

static ONCE: Once = Once::new();

pub fn init<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        value::weak_table::init_weak_tables(mc);
        value::weak_set::init_weak_sets(mc);
        value::strings::init_strings(mc);
        value::symbols::init_symbols(mc);

        let _ = &*vmthread::VM_THREAD;
    });
}

pub use thread::{Context, Scheme, State};

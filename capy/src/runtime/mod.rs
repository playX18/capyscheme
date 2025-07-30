use rsgc::Mutation;

pub mod fluids;
pub mod modules;
pub mod thread;
pub mod value;
pub mod vmthread;

pub(crate) fn init<'gc>(mc: &Mutation<'gc>) {
    init_weak_sets(mc);
    init_weak_tables(mc);
    init_symbols(mc);
}

pub use thread::*;

use crate::runtime::value::{init_symbols, init_weak_sets, init_weak_tables};

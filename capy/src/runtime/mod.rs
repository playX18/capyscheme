pub mod fasl;
pub mod fluids;
pub mod modules;
pub mod thread;
pub mod value;
pub mod vm;
pub mod vmthread;

pub(crate) fn init<'gc>(mc: Context<'gc>) {
    let _ = &*VM_THREAD;
    modules::current_module(mc).set(mc, (*modules::root_module(mc)).into());

    vm::load::init_load_path(mc);
    vm::load::init(mc);
    let _ = crate::expander::primitives::primitives(mc);
}

pub use thread::*;

use crate::runtime::vmthread::VM_THREAD;

pub mod prelude {
    pub use super::thread::Context;
    pub use super::value::*;
    pub use super::vm::*;
}

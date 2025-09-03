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
    vm::arith::init(mc);
    runtime::value::init_structs(mc);
    vm::strings::init_strings(mc);
    vm::vector::init_vectors(mc);
    let _ = crate::expander::primitives::primitives(mc);
}

pub use thread::*;

use crate::runtime::{self, vmthread::VM_THREAD};

pub mod prelude {
    use crate::runtime::modules::Variable;

    pub use super::thread::Context;
    pub use super::value::*;
    pub use super::vm::*;
    use rsgc::Gc;
    pub use rsgc::Rootable;
    pub use rsgc::Trace;
    pub use rsgc::global::Global;
    pub type VariableRef<'gc> = Gc<'gc, Variable<'gc>>;
}

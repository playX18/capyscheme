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

    fluids::init_fluids(mc);
    vm::load::init_load_path(mc);
    vm::load::init_load(mc);
    vm::arith::init(mc);
    modules::init_modules(mc);
    runtime::value::init_structs(mc);
    vm::strings::init_strings(mc);
    vm::vector::init_vectors(mc);
    vm::list::init_lists(mc);
    vm::debug::init_debug(mc);
    vm::eval::init_eval(mc);
    vm::base::init_base(mc);
    vm::io::init_io(mc);
    vm::hash::init_hash(mc);
    vm::syntax::init_syntax(mc);
    vm::memoize::init_memoize(mc);
    let _ = crate::expander::primitives::primitives(mc);
}

pub use thread::*;

use crate::runtime::{self, vmthread::VM_THREAD};

#[allow(ambiguous_glob_imports)]
pub mod prelude {
    use crate::runtime::modules::*;

    pub use super::thread::Context;
    pub use super::value::*;
    pub use super::vm::{self, NativeCallContext, NativeCallReturn, call_scheme};
    pub use rsgc::Gc;
    pub use rsgc::Rootable;
    pub use rsgc::Trace;
    pub use rsgc::global::Global;

    pub type VariableRef<'gc> = Gc<'gc, Variable<'gc>>;
    pub type StringRef<'gc> = Gc<'gc, Str<'gc>>;
    pub type SymbolRef<'gc> = Gc<'gc, Symbol<'gc>>;
}

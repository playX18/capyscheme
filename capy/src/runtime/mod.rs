//! Runtime system: value representation, virtual machine, modules, and thread management.

pub mod class;
pub mod code_memory;
pub mod fasl;
pub mod fluids;
pub mod global;
pub mod libs;
pub mod modules;
pub mod stats;
pub mod symbols;
pub mod thread;
pub mod value;
pub mod vm;
pub mod vmthread;

pub(crate) fn init<'gc>(mc: Context<'gc>) {
    let _ = &*VM_THREAD;
    class::init_builtin_classes(mc);

    VM_GLOBALS
        .set(crate::rsgc::Global::new(global::Globals::new(mc)))
        .unwrap_or_else(|_| {
            panic!("VM_GLOBALS already initialized");
        });

    // modules::current_module(mc).set(mc, (modules::root_module(mc)).into());

    class::init_class_ops(mc);
    fluids::init_fluids(mc);
    vm::load::init_load_path(mc);
    vm::load::init_load(mc);
    vm::arith::init(mc);
    modules::init_modules(mc);
    vm::strings::init_strings(mc);
    vm::vector::init_vectors(mc);
    vm::list::init_lists(mc);
    vm::debug::init_debug(mc);
    vm::eval::init_eval(mc);
    vm::base::init_base(mc);
    vm::io::init_io(mc);
    vm::hash::init_hash(mc);
    vm::persistent_map::init_persistent_map(mc);
    vm::persistent_set::init_persistent_set(mc);
    vm::syntax::init_syntax(mc);
    vm::memoize::init_memoize(mc);
    vm::expand::init_expand(mc);
    vm::ffi::init_ffi(mc);
    vm::dl::init_dl(mc);
    vm::threading::init_threading(mc);
    vm::interrupts::init_interrupts(mc);
    vm::control::init_control(mc);
    vm::gc::gc::register(mc);
    //let _ = crate::expander::primitives::interesting_primitive_vars_loc(mc);
}

use parking_lot::Mutex;
pub use thread::*;

use crate::runtime::{global::VM_GLOBALS, stats::GlobalStats, vmthread::VM_THREAD};

#[allow(ambiguous_glob_imports)]
pub mod prelude {
    use crate::runtime::modules::*;

    pub use super::global::Global;
    pub use super::thread::Context;
    pub use super::value::*;
    pub use super::vm::{self, NativeCallContext, NativeCallReturn, call_scheme};
    pub use crate::rsgc::Gc;
    pub use crate::rsgc::Rootable;
    pub use crate::rsgc::Trace;

    pub type VariableRef<'gc> = Gc<'gc, Variable<'gc>>;
    pub type StringRef<'gc> = Gc<'gc, Str<'gc>>;
    pub type SymbolRef<'gc> = Gc<'gc, Symbol<'gc>>;
}

pub(crate) static GLOBAL_STATS: Mutex<GlobalStats> = Mutex::new(GlobalStats::new());

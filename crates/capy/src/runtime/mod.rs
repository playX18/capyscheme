//! Runtime system: value representation, virtual machine, modules, and thread management.

pub mod class;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub mod code_memory;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub mod codegen;
pub mod fasl;
pub mod fluids;
pub mod gc_stats;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub mod gdb_jit;
pub mod global;
pub mod interp;
pub mod libs;
pub mod modules;
pub mod prims;
pub mod profile;
pub mod root;
pub mod sabi;
pub mod sni;
pub mod stats;
pub mod thread;
pub mod value;
pub mod vm;
pub mod vmthread;
pub(crate) mod watchpoint;

pub(crate) fn init<'gc>(mc: MutationContext<'gc>) {
    let _ = &*VM_THREAD;
    class::init_builtin_classes(mc);

    VM_GLOBALS
        .set(crate::heap::Global::new(global::Globals::new(mc)))
        .unwrap_or_else(|_| {
            panic!("VM_GLOBALS already initialized");
        });

    class::init_class_ops(mc);
    fluids::init_fluids(mc);
    vm::load::init_load_path(mc);
    vm::load::init_load(mc);
    vm::arith::init(mc);
    modules::init_modules(mc);
    vm::strings::init_strings(mc);
    vm::vector::init_vectors(mc);
    vm::portio::init_portio(mc);
    vm::list::init_lists(mc);
    vm::debug::init_debug(mc);
    profile::init_profile(mc);
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
}

use parking_lot::Mutex;
pub use thread::*;

use crate::runtime::{global::VM_GLOBALS, stats::GlobalStats, vmthread::VM_THREAD};

#[allow(ambiguous_glob_imports)]
pub mod prelude {
    use crate::runtime::modules::*;

    pub use super::global::Global;
    pub use super::interp::stack::Slot;
    pub use super::root::{RootScope, RootStack, Rooted, with_rooted_native};
    pub use super::sni;
    pub use super::thread::MutationContext;
    pub use super::value::*;
    pub use super::vm::{self, Call, call_scheme};
    pub use crate::heap::Gc;
    pub use crate::heap::Trace;

    pub type VariableRef<'gc> = Gc<'gc, Variable<'gc>>;
    pub type StringRef<'gc> = Gc<'gc, Str<'gc>>;
    pub type SymbolRef<'gc> = Gc<'gc, Symbol<'gc>>;
}

pub(crate) static GLOBAL_STATS: Mutex<GlobalStats> = Mutex::new(GlobalStats::new());

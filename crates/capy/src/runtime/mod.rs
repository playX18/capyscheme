//! Runtime system: value representation, virtual machine, and thread management.

#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub(crate) mod code_memory;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub(crate) mod codegen;
pub(crate) mod code;
pub(crate) mod compile_service;
pub(crate) mod conditions;
pub(crate) mod fasl;
pub(crate) mod fluids;
pub(crate) mod gc_stats;
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub(crate) mod frame;
pub(crate) mod gdb_jit;
pub mod global;
pub(crate) mod hooks;
pub(crate) mod interp;
pub(crate) mod linklet;
pub(crate) mod prims;
pub(crate) mod profile;
pub(crate) mod record;
pub(crate) mod record_ops;
pub(crate) mod registry;
pub mod root;
pub(crate) mod sabi;
pub mod sched;
pub(crate) mod service;
pub mod sni;
pub(crate) mod stats;
pub mod thread;
pub(crate) mod builtins;
pub mod objectmodel;
pub(crate) mod variable;
pub mod vm;
pub mod vmthread;
pub(crate) mod watchpoint;

pub(crate) fn init_once<'gc>(mc: MutationContext<'gc>) {
    static VM_CORE: parking_lot::Mutex<()> = parking_lot::Mutex::new(());

    static VM_READY: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    if VM_READY.load(std::sync::atomic::Ordering::Acquire) {
        return;
    }

    // Before anything can allocate a `ThreadObject` (the VM thread does so
    // lazily) or another class owned by a higher layer.
    registry::register_builtin_hooks();
    crate::heap::sync::thread::Thread::enter_native();
    let _guard = VM_CORE.lock();
    crate::heap::sync::thread::Thread::leave_native();
    if VM_READY.load(std::sync::atomic::Ordering::Acquire) {
        return;
    }
    init_core_tables(mc);
    init(mc);
    VM_READY.store(true, std::sync::atomic::Ordering::Release);
}

pub(crate) fn init_core_tables<'gc>(mc: MutationContext<'gc>) {
    objectmodel::init_weak_sets(mc);
    objectmodel::init_weak_tables(mc);
    objectmodel::init_symbols(mc);
}

pub(crate) fn init<'gc>(mc: MutationContext<'gc>) {
    let _ = &*VM_THREAD;
    objectmodel::class::init_builtin_classes(mc);

    VM_STATE
        .set(crate::heap::Global::new(global::VmState::new(mc)))
        .unwrap_or_else(|_| {
            panic!("VM_STATE already initialized");
        });

    objectmodel::class::init_class_ops(mc);
    fluids::init_fluids(mc);
    vm::load::init_load_path(mc);
    vm::load::init_load(mc);
    builtins::arith::init(mc);
    variable::variable_ops::register(mc);
    record_ops::record_ops::register(mc);
    hooks::hooks_ops::register(mc);
    linklet::init_linklets(mc);
    builtins::strings::init_strings(mc);
    builtins::format::init_format(mc);
    builtins::vector::init_vectors(mc);
    vm::portio::init_portio(mc);
    builtins::list::init_lists(mc);
    builtins::debug::init_debug(mc);
    profile::init_profile(mc);
    builtins::eval::init_eval(mc);
    vm::base::init_base(mc);
    vm::io::init_io(mc);
    builtins::hash::init_hash(mc);
    builtins::persistent_map::init_persistent_map(mc);
    builtins::persistent_set::init_persistent_set(mc);
    vm::syntax::init_syntax(mc);
    builtins::memoize::init_memoize(mc);
    builtins::expand::init_expand(mc);
    vm::ffi::init_ffi(mc);
    vm::dl::init_dl(mc);
    vm::threading::init_threading(mc);
    vm::interrupts::init_interrupts(mc);
    vm::control::init_control(mc);
    builtins::gc::gc::register(mc);
}

use parking_lot::Mutex;
pub use thread::*;

use crate::runtime::{global::VM_STATE, stats::GlobalStats, vmthread::VM_THREAD};

#[allow(ambiguous_glob_imports)]
pub mod prelude {
    use crate::runtime::variable::Variable;

    pub use super::global::GlobalSlot;
    pub use super::interp::stack::Slot;
    pub use super::root::{RootScope, RootStack, Rooted, with_rooted_native};
    pub use super::sni;
    pub use super::thread::MutationContext;
    pub use super::objectmodel::*;
    pub use super::vm::{self, NativeCall, call_scheme};
    pub use crate::heap::Gc;
    pub use crate::heap::Trace;

    pub type VariableRef<'gc> = Gc<'gc, Variable<'gc>>;
    pub type StringRef<'gc> = Gc<'gc, Str<'gc>>;
    pub type SymbolRef<'gc> = Gc<'gc, Symbol<'gc>>;
}

pub(crate) static GLOBAL_STATS: Mutex<GlobalStats> = Mutex::new(GlobalStats::new());

//! Thread and GC context management.

use crate::{
    heap::{GarbageCollector, Gc, Mutation, Mutator, Trace, sync::thread::Thread},
    prelude::{HashTable, Keyword},
    runtime::stats::ThreadStats,
};
use crate::{
    prelude::{IntoValue, NativeFn, PROCEDURES},
    runtime::{
        fluids::DynamicState,
        global::{Globals, VM_GLOBALS},
        interp::stack::{Ip, Slot, StackState},
        //image::{ALLOWED_GC, AllowedGc, reader::ImageReader},
        linklet::{Instance, Linklet, instantiate_linklet, registry_register, root_registry},
        prelude::VariableRef,
        value::{
            Closure, ReturnCode, Str, Symbol, Value, Vector, init_symbols, init_weak_sets,
            init_weak_tables,
        },
        vm::{
            ExecutionResult, base::program_arguments_fluid, control::ContinuationMarks,
            load::load_thunk_in_vicinity, threading::ThreadObject,
        },
    },
};
use std::{
    cell::Cell,
    sync::{
        Once,
        atomic::{AtomicPtr, AtomicUsize},
    },
};

/// Historical name of [`Mutation`], kept so call sites need not change.
pub type MutationContext<'gc> = Mutation<'gc>;

impl<'gc> Mutation<'gc> {
    /// Open a [`crate::runtime::root::RootScope`] on this thread's root stack.
    #[inline]
    pub fn roots(self) -> crate::runtime::root::RootScope<'gc, 'gc> {
        crate::runtime::root::RootScope::new(self)
    }

    pub fn dynamic_state(self) -> Value<'gc> {
        self.state().dynamic_state.save(self)
    }

    pub fn set_dynamic_state(self, state: Value<'gc>) {
        self.state().dynamic_state.restore(self, state);
    }

    pub fn current_continuation_marks(self) -> Gc<'gc, ContinuationMarks<'gc>> {
        self.allocate_with(
            ContinuationMarks {
                cmarks: self.state().current_marks(),
            },
            crate::runtime::vm::control::continuation_marks_header_word(),
        )
    }

    pub fn keyword(self, s: &str) -> Gc<'gc, Keyword<'gc>> {
        let sym = self.intern(s);
        let globals = self.globals().keyword_map.get().downcast::<HashTable>();
        if let Some(kw) = globals.get(self, sym) {
            return kw.downcast();
        }

        let kw = Keyword::from_symbol(self, sym.downcast());
        globals.put(self, sym, kw);
        kw
    }

    pub fn intern(self, s: &str) -> Value<'gc> {
        Symbol::from_str(self, s).into()
    }

    pub fn str(self, s: &str) -> Value<'gc> {
        Str::from_str(self, s).into()
    }

    pub fn make_native_closure(
        self,
        proc: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(self)
            .make_closure(self, proc, free_vars, meta)
    }

    pub fn make_native_continuation(
        self,
        proc: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(self)
            .make_cont_closure(self, proc, free_vars, meta)
    }

    /// Builds a native procedure closure with three free variables, reusing
    /// the cached proc wrapper and code block for `proc`.
    ///
    /// This is the `call/cc`-family fast path, see
    /// [`crate::runtime::value::ProcedureRegistry::make_cached_closure`].
    pub fn make_cached_native_closure(
        self,
        proc: NativeFn<'gc>,
        captured: [Value<'gc>; 3],
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(self)
            .make_cached_closure(self, proc, captured, meta)
    }

    pub fn make_static_closure(
        self,
        proc: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(self)
            .register_static_closure(self, proc, meta)
    }

    pub fn make_static_continuation(
        self,
        proc: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(self)
            .register_static_cont_closure(self, proc, meta)
    }

    /// Run `f` outside the GC-mutating world (`InNative`).
    ///
    /// Preparation that needs `MutationContext`, allocation, or tracing must happen before calling
    /// this function. The closure must not capture GC handles or interior pointers into
    /// movable objects unless they are rooted on [`State::root_stack`] /
    /// [`crate::runtime::root`] or in OopStorage. After return, reload Values from those
    /// slots, stack copies are not updated by moving GC.
    pub fn call_in_native<T>(self, f: impl FnOnce() -> T) -> T {
        use std::panic::{AssertUnwindSafe, catch_unwind};

        struct OutsideGcGuard;

        impl Drop for OutsideGcGuard {
            fn drop(&mut self) {
                Thread::leave_native();
            }
        }

        self.state().stats.start_blocking();
        // A thread that blocks in native code stays on this worker until
        // it returns, see `PinGuard`.
        let _pin = crate::runtime::sched::scheduler::pin_current();
        Thread::enter_native();
        let result = {
            let _guard = OutsideGcGuard;
            catch_unwind(AssertUnwindSafe(f))
        };
        self.state().stats.end_blocking();

        match result {
            Ok(value) => value,
            Err(payload) => std::panic::resume_unwind(payload),
        }
    }

    /// Alias for [`Self::call_in_native`].
    pub fn outside_gc_world<T>(self, f: impl FnOnce() -> T) -> T {
        self.call_in_native(f)
    }

    /// Stage a return of `values` to the current frame's caller (`fp[0]`).
    ///
    /// The driver (`native_continue_step`) copies the values into `fp[2..]`
    /// and returns to the frame's return address.
    pub fn stage_return_values(self, values: &[Value<'gc>]) -> ReturnCode {
        self.prepare_call_data(Value::current_k(), values, None);
        ReturnCode::Continue
    }

    /// Stage a tail call of `proc` with `args` from the current frame.
    pub fn stage_tail_call(self, proc: Value<'gc>, args: &[Value<'gc>]) -> ReturnCode {
        self.prepare_call_data(proc, args, Some(Value::current_k()));
        ReturnCode::Continue
    }

    /// Stage a call of `proc` with `args` that returns to the native
    /// continuation `k` through `subr_ret`.
    pub fn stage_call_with(
        self,
        proc: Value<'gc>,
        args: &[Value<'gc>],
        k: Value<'gc>,
    ) -> ReturnCode {
        self.prepare_call_data(proc, args, Some(k));
        ReturnCode::Continue
    }

    /// Stage the invocation of the continuation `cont` with `args`: a return
    /// to the caller for the current-continuation token, a throw for a
    /// captured continuation, a tail entry for a native continuation closure.
    pub fn stage_resume(self, cont: Value<'gc>, args: &[Value<'gc>]) -> ReturnCode {
        self.prepare_call_data(cont, args, None);
        ReturnCode::Continue
    }

    /// Stage the call a native procedure asks the interpreter to make when
    /// it returns `ReturnCode::Continue`.
    ///
    /// The wire format below is private to this module and the driver
    /// (`native_continue_step`): `retk` is `Some(current_k)` for a tail
    /// call, `Some(k)` for a native continuation `k`, and `None` when
    /// `rator` is itself a continuation being invoked.
    fn prepare_call_data(self, rator: Value<'gc>, args: &[Value<'gc>], retk: Option<Value<'gc>>) {
        use crate::runtime::interp::stack;
        // `args` arrives already collected outside the frame: reserving room
        // below may move the frame, so the copy must not alias it.
        let stack_state = &self.tc().stack;
        // SAFETY: a native procedure owns the top frame, so `sp` is the
        // first free slot. Four slots are skipped so the arguments sit
        // above wherever the interpreter will move them to.
        unsafe {
            stack::reserve(self, args.len() + 8);
            let staging = stack_state.sp.get().add(4);
            std::ptr::copy_nonoverlapping(
                args.as_ptr().cast::<Value<'static>>(),
                staging,
                args.len(),
            );
            stack_state.call_args.set(staging);
        }
        stack_state.call_argc.set(args.len());
        stack_state.call_rator.set(rator);
        stack_state
            .call_k
            .set(retk.unwrap_or_else(Value::undefined));
    }

    /// The instance registered under the space-separated library name `name`
    /// (`"capy"` is the primitive instance).
    pub fn instance(self, name: &str) -> Option<Gc<'gc, Instance<'gc>>> {
        if name == "capy" {
            return Some(self.globals().capy_instance());
        }
        let key = crate::runtime::linklet::library_name(self, name);
        crate::runtime::linklet::registry_lookup(
            self,
            crate::runtime::linklet::root_registry(self),
            key,
        )
    }

    /// Like [`Self::instance`], creating and registering a mutable instance if the
    /// name is not registered yet.
    pub fn ensure_instance(self, name: &str) -> Gc<'gc, Instance<'gc>> {
        if let Some(inst) = self.instance(name) {
            return inst;
        }
        let key = crate::runtime::linklet::library_name(self, name);
        let inst = Instance::new(self, key, true);
        crate::runtime::linklet::registry_register(
            self,
            crate::runtime::linklet::root_registry(self),
            key,
            inst,
        );
        inst
    }

    pub fn globals(self) -> &'gc Globals<'gc> {
        VM_GLOBALS
            .get()
            .expect("VM globals not initialized")
            .fetch(self)
    }

    pub fn nest_level(&self) -> usize {
        self.state()
            .nest_level
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn public_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        self.private_ref(mname, name)
    }

    pub fn private_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        self.instance(mname)?.get(self, self.intern(name))
    }

    pub fn accumulator(&self) -> Value<'gc> {
        self.state().accumulator.get()
    }

    pub fn define(
        self,
        mname: &str,
        name: &str,
        value: impl IntoValue<'gc>,
    ) -> Option<VariableRef<'gc>> {
        let value = value.into_value(self);
        let inst = self.instance(mname)?;
        let name = self.intern(name);
        Some(inst.define(self, name, value))
    }

    pub fn winders(&self) -> Value<'gc> {
        self.state().winders.get()
    }

    pub fn set_winders(&self, winders: Value<'gc>) {
        self.state().winders.set(winders);
    }

    /// Given a key, get the associated continuation mark from the current marks.
    ///
    /// Returns `None` if the mark is not found.
    pub fn get_mark_first(self, key: Value<'gc>) -> Option<Value<'gc>> {
        let mut set = self.state().current_marks();

        while !set.is_null() {
            let mark_set = set.car();
            if let Some(val) = mark_set.assq(key) {
                return Some(val.cdr());
            }

            set = set.cdr();
        }

        None
    }

    /// Return the current exception handler, if any.
    ///
    /// Searches the current continuation marks for a mark with the key `|exception-handler-key aeee9cb5-b850-45ad-b460-c9868b7f2736|` and returns its value if found.
    pub fn exception_handler(&self) -> Option<Value<'gc>> {
        let key = self.intern("exception-handler-key aeee9cb5-b850-45ad-b460-c9868b7f2736");
        self.get_mark_first(key)
    }
}

/// Execution engine, chosen by `CAPY_ENGINE` (via [`flags::engine`]).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Engine {
    Rust,
    Template,
}

impl Engine {
    fn as_str(self) -> &'static str {
        match self {
            Engine::Rust => "rust",
            Engine::Template => "template",
        }
    }

    /// Build default: the template interpreter when compiled in, else the Rust engine.
    pub fn default_for_build() -> Self {
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            Engine::Template
        }
        #[cfg(not(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64"))))]
        {
            Engine::Rust
        }
    }

    /// Engine from the `engine` flag. An unset flag or an invalid value
    /// (already warned about by the flag registry) gives the build default.
    fn from_flags_or_default() -> Self {
        match crate::utils::flags::engine() {
            "rust" => Engine::Rust,
            "template" => Engine::Template,
            _ => Self::default_for_build(),
        }
    }
}

/// Machine-level thread context: everything SABI code touches.
///
/// Lives directly in the [`VirtualThread`](crate::heap::sync::thread::VirtualThread) header,
/// so the `TC` register reaches every field at a constant offset, see
/// [`sabi`](crate::runtime::sabi).
#[repr(C)]
pub struct Tc<'gc> {
    /// The Scheme stack and the registers the interpreter parks in it.
    pub(crate) stack: StackState<'gc>,
    /// Overflow limit, null forces the slow path. Atomic so another thread
    /// can poke it to null when requesting a yieldpoint, every other
    /// field is only touched by the owning thread.
    pub(crate) esp: AtomicPtr<Value<'gc>>,
    pub(crate) real_esp: Cell<Slot<'gc>>,
    pub(crate) ac: Cell<u64>,
    pub(crate) rv: Cell<Value<'gc>>,
    pub(crate) cp: Cell<Value<'gc>>,
    pub(crate) ip: Cell<Ip>,
    pub(crate) exit_value: Cell<Value<'gc>>,
    /// Stub table for code that calls stubs instead of patching, null until the generated image is installed.
    pub(crate) stubs: *const (),
}

impl<'gc> Tc<'gc> {
    pub(crate) fn new() -> Self {
        Self {
            stack: StackState::new(),
            esp: AtomicPtr::new(std::ptr::null_mut()),
            real_esp: Cell::new(std::ptr::null_mut()),
            ac: Cell::new(0),
            rv: Cell::new(Value::undefined()),
            cp: Cell::new(Value::undefined()),
            ip: Cell::new(std::ptr::null()),
            exit_value: Cell::new(Value::undefined()),
            stubs: std::ptr::null(),
        }
    }
}

// SAFETY: Tc holds GC roots (the Scheme stack, rv/cp/exit_value).
// All traced fields are exclusively owned by this mutator thread during GC stop-the-world.
unsafe impl Trace for Tc<'_> {
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}

    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        // SAFETY: the mutator is stopped, so the stack registers describe a
        // consistent stack: every frame below the top one is parked at a
        // return point, and the top one at a safepoint.
        unsafe {
            crate::runtime::interp::stack::trace_stack(&mut self.stack, visitor);
        }
        visitor.trace(&mut self.rv);
        visitor.trace(&mut self.cp);
        visitor.trace(&mut self.exit_value);
    }
}

pub struct State<'gc> {
    pub(crate) dynamic_state: DynamicState<'gc>,
    /// Stored engine choice (`CAPY_ENGINE`): which engine runs new activations.
    pub(crate) engine: Engine,
    /// Nest depth of Scheme activations entered via the nest API
    /// ([`crate::runtime::sni`] / unsafe [`crate::runtime::vm::call_scheme`]).
    /// A `#[scheme]` leaf does not change this. When nest increases, GC may run
    /// at compiled yieldpoints, outer native locals must be rooted in local frames.
    pub(crate) nest_level: AtomicUsize,
    pub(crate) thread_object: Gc<'gc, ThreadObject<'gc>>,
    pub(crate) accumulator: Cell<Value<'gc>>,
    pub(crate) current_marks: Cell<Value<'gc>>,
    pub(crate) winders: Cell<Value<'gc>>,
    /// Active `setjmp` buffer for exiting CPSed code back to Rust, null when not in Scheme entry.
    pub(crate) exit_jmp: Cell<*mut crate::runtime::vm::setjmp::JmpBuf>,
    /// Staged return code for [`crate::runtime::vm::scheme_longjmp`].
    pub(crate) exit_code: Cell<ReturnCode>,
    /// Staged return value for [`crate::runtime::vm::scheme_longjmp`].
    pub(crate) exit_value: Cell<Value<'gc>>,
    pub(crate) stats: ThreadStats,
    /// Precise relocatable roots for Values held across `call_in_native`.
    pub(crate) root_stack: crate::runtime::root::RootStack<'gc>,
}

// SAFETY: State contains GC roots (dynamic_state, etc.).
// All traced fields are exclusively owned by this mutator thread during GC stop-the-world.
// The Scheme stack moved to `Tc`, which the collector traces separately.
unsafe impl Trace for State<'_> {
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}

    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        visitor.trace(&mut self.dynamic_state);

        visitor.trace(&mut self.root_stack);
        visitor.trace(&mut self.thread_object);
        visitor.trace(&mut self.accumulator);
        visitor.trace(&mut self.current_marks);
        visitor.trace(&mut self.winders);
        visitor.trace(&mut self.exit_value);
    }
}

impl<'gc> State<'gc> {
    pub fn new(mc: Mutation<'gc>, thread_object: Gc<'gc, ThreadObject<'gc>>) -> Self {
        Self::with_dynamic_state(thread_object, DynamicState::new(MutationContext::from(mc)))
    }

    /// Like [`new`](Self::new), around a dynamic state from a worker's pool.
    /// The caller restores the spawner's dynamic state into it.
    pub(crate) fn with_dynamic_state(
        thread_object: Gc<'gc, ThreadObject<'gc>>,
        dynamic_state: DynamicState<'gc>,
    ) -> Self {
        let engine = Engine::from_flags_or_default();
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        if engine == Engine::Template {
            // Load the image before any closure is created: new closures
            // carry its `interpreter_enter` as their machine entry.
            crate::runtime::codegen::template_image::ensure_loaded();
        }
        Self {
            dynamic_state,
            engine,
            nest_level: AtomicUsize::new(0),
            thread_object,
            accumulator: Cell::new(Value::new(false)),
            current_marks: Cell::new(Value::null()),
            winders: Cell::new(Value::null()),
            exit_jmp: Cell::new(std::ptr::null_mut()),
            exit_code: Cell::new(ReturnCode::ReturnOk),
            exit_value: Cell::new(Value::undefined()),
            stats: ThreadStats::new(),
            root_stack: crate::runtime::root::RootStack::new(),
        }
    }

    pub fn current_marks(&self) -> Value<'gc> {
        self.current_marks.get()
    }

    /// Update the current continuation marks.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it allows setting arbitrary continuation marks
    /// which may violate invariants expected by the runtime in places like exception handlers.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn set_current_marks(&self, marks: Value<'gc>) {
        self.current_marks.set(marks);
    }
}

pub struct Scheme {
    pub(crate) mutator: Mutator<crate::Rootable!(())>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(MutationContext<'gc>) -> T,
    {
        self.mutator.mutate(|ctx, _| f(ctx))
    }

    pub fn call_value<PREP, F, R>(&self, prep: PREP, finish: F) -> R
    where
        F: for<'gc> Fn(MutationContext<'gc>, Result<Value<'gc>, Value<'gc>>) -> R,
        PREP: for<'gc> FnOnce(MutationContext<'gc>, &mut Vec<Value<'gc>>) -> Value<'gc>,
    {
        self.enter(|ctx| {
            let mut args = Vec::with_capacity(4);
            ctx.stats.start_execution();
            let rator = prep(ctx, &mut args);
            let run = crate::runtime::sni::call_function(ctx, rator, args);
            ctx.stats.end_execution();

            let result = match run {
                ExecutionResult::Ok(ok) => Ok(ok),
                ExecutionResult::Err(err) => Err(err),
            };
            finish(ctx, result)
        })
    }

    /// Calls `entry_name` in module `mod_name`.
    pub fn call<ARGS, F, R>(&self, mod_name: &str, entry_name: &str, setup: ARGS, finish: F) -> R
    where
        F: for<'gc> Fn(MutationContext<'_>, Result<Value<'gc>, Value<'gc>>) -> R,
        ARGS: for<'gc> FnOnce(MutationContext<'gc>, &mut Vec<Value<'gc>>),
    {
        self.call_value(
            move |ctx, args| {
                setup(ctx, args);
                let entry = ctx
                    .public_ref(mod_name, entry_name)
                    .expect("Entrypoint not found");
                if !entry.is::<Closure>() {
                    panic!("Entrypoint is not a procedure");
                }
                entry
            },
            finish,
        )
    }

    pub fn collect_garbage(&self) {
        self.mutator.collect_garbage();
    }

    pub fn new() -> Self {
        let (scm, should_init) = Self::new_runtime();
        if should_init { scm.boot() } else { scm }
    }

    /// Runs a static bundle, the output of `capyc --static`, without booting the runtime.
    ///
    /// `bytes` is the bundle file: a FASL image, optionally after a `#!` line. Instead of
    /// `boot/main.scm` it is instantiated against the native primitives and registered
    /// as the `(boot static)` instance, which fills the core table. Its body then runs
    /// the program with `args` as `(program-arguments)`. The body ends the process by
    /// calling `exit`, so this returns only if it does not.
    pub fn new_static(bytes: &[u8], args: &[String]) -> Self {
        let scm = Self::new_static_runtime();
        scm.run_static(bytes, args);
        scm
    }

    /// The setup half of [`Scheme::new_static`]: a runtime that is not booted, linked against
    /// the native primitives only. Nothing of the bundle has run yet.
    pub fn new_static_runtime() -> Self {
        let (scm, _) = Self::new_runtime();
        scm.enter(|ctx| crate::runtime::linklet::install_capy_primitive_instance(ctx));
        scm
    }

    /// The body half: instantiates the bundle, registers it and runs it. A caller that wants
    /// the program on the thread scheduler calls this from inside `scheduler::run_main`.
    pub fn run_static(&self, bytes: &[u8], args: &[String]) {
        self.enter(|ctx| run_static_bundle(ctx, bytes, args));
    }

    /// Creates the mutator and runs the one-time process init. The flag tells whether this
    /// call did that init, which is the one that has to boot.
    fn new_runtime() -> (Self, bool) {
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;
            crate::heap::logging::init_rust_logger();
            let mmtk_builder = crate::heap::logging::mmtk_builder();
            /*match *mmtk_builder.options.plan {
                PlanSelector::GenImmix | PlanSelector::StickyImmix | PlanSelector::GenCopy => {
                    let _ = ALLOWED_GC.set(AllowedGc::Generational).unwrap();
                }

                PlanSelector::ConcurrentImmix => {
                    let _ = ALLOWED_GC.set(AllowedGc::Concurrent).unwrap();
                }

                _ => {
                    let _ = ALLOWED_GC.set(AllowedGc::Regular).unwrap();
                }
            }*/
            GarbageCollector::init(mmtk_builder);
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    let ctx = MutationContext::from(mc);
                    init_weak_sets(ctx);
                    init_weak_tables(ctx);
                    init_symbols(ctx);

                    let thread_object = ThreadObject::new(mc, None);
                    thread_object.bind_current_runtime_thread();
                    let state = State::new(mc, thread_object);
                    mc.init_state(state);
                });
                m.mutate(|ctx, _| {
                    super::init_once(ctx);
                });
                m
            },
        };

        (scm, should_init)
    }

    pub fn new_uninit() -> Self {
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;

            crate::heap::logging::init_rust_logger();
            let mmtk_builder = crate::heap::logging::mmtk_builder();
            GarbageCollector::init(mmtk_builder);
        });

        Self {
            mutator: {
                Mutator::new(|mc| {
                    let ctx = MutationContext::from(mc);
                    init_weak_sets(ctx);
                    init_weak_tables(ctx);
                    init_symbols(ctx);

                    let thread_object = ThreadObject::new(mc, None);
                    thread_object.bind_current_runtime_thread();
                    let state = State::new(mc, thread_object);
                    mc.init_state(state);
                })
            },
        }
    }

    /*pub fn from_image(image: &[u8]) -> Self {
        let allowed_gc = match image[0] {
            0 => AllowedGc::Generational,
            1 => AllowedGc::Concurrent,
            2 => AllowedGc::Regular,
            _ => panic!("Invalid allowed GC type in image"),
        };

        let _ = ALLOWED_GC.set(allowed_gc).unwrap();
        SCM_INITIALIZED.call_once(|| {
            let mut mmtk_builder = MMTKBuilder::new();
            allowed_gc.adjust_mmtk_options(&mut mmtk_builder.options);
            GarbageCollector::init(mmtk_builder);
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(mc);
                    init_weak_tables(mc);
                    let state = State::new(mc, ThreadObject::new(mc, None));
                    mc.init_state(state);
                });
                m
            },
        };

        //let mut decoder = lz4::Decoder::new(image).expect("Failed to create LZ4 decoder");
        //let mut image = Vec::new();
        //std::io::copy(&mut decoder, &mut image).expect("Failed to decompress image");

        scm.enter(|ctx| {
            let mut reader = ImageReader::new(ctx, image);

            let img = reader.deserialize().expect("Failed to read image");

            let entrypoint = match img.boot(ctx) {
                ExecutionResult::Ok(entry) => entry,
                _ => unreachable!(),
            };

            ctx.state().accumulator.set(entrypoint);
        });

        scm
    }*/

    fn boot(self) -> Self {
        let scm = self;
        scm.enter(|ctx| {
            crate::runtime::linklet::install_capy_primitive_instance(ctx);

            let thunk = load_thunk_in_vicinity::<true>(ctx, "boot/main.scm", None::<&str>, false, None)
                .expect("Failed to load boot/main.scm");

            match crate::runtime::sni::call_function(ctx, thunk, []) {
                ExecutionResult::Ok(_) => {}
                ExecutionResult::Err(err) => {
                    // Fatal and final: make sure the message is visible
                    // even under default-off logging before exiting.
                    crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
                    log::error!(target: crate::heap::logging::BOOT_LOG_TARGET, "Failed to boot: {err}");
                    std::process::exit(1);
                }
            }
        });

        scm
    }
}

impl Default for Scheme {
    fn default() -> Self {
        Self::new()
    }
}

/// Fatal error while running a static bundle: the message goes to stderr, then exit 70
fn static_fatal(message: impl std::fmt::Display) -> ! {
    eprintln!("{message}");
    std::process::exit(70)
}

/// Instantiates the static bundle `bytes` and runs its body, see [`Scheme::new_static`].
fn run_static_bundle<'gc>(ctx: MutationContext<'gc>, bytes: &[u8], args: &[String]) {
    let payload = crate::runtime::fasl::fasl_payload(bytes);
    let reader = crate::runtime::fasl::Reader::new_with_options(
        ctx,
        std::io::Cursor::new(payload),
        crate::runtime::vm::load::policy::get_fasl_load_options(),
    );
    let thunk = reader
        .read()
        .unwrap_or_else(|err| static_fatal(format_args!("cannot read static bundle: {err}")));

    let parts = match crate::runtime::sni::call_function(ctx, thunk, []) {
        ExecutionResult::Ok(value) if value.is::<Vector>() => value,
        ExecutionResult::Ok(_) => static_fatal("not a static bundle"),
        ExecutionResult::Err(err) => static_fatal(err),
    };
    let parts = parts.downcast::<Vector>();
    let [head, name, imports, exports, body, meta] = parts.as_slice() else {
        static_fatal("not a static bundle: expected a linklet")
    };
    if *head != ctx.intern("#%linklet") {
        static_fatal("not a static bundle: expected a linklet")
    }

    let linklet = Linklet::new(ctx, *name, *imports, *exports, *body, *meta);
    let target = Instance::new(ctx, *name, false);
    let reg = root_registry(ctx);
    let capy = ctx.globals().capy_instance();
    let slots = instantiate_linklet(ctx, reg, linklet, &[capy], target)
        .unwrap_or_else(|bad| static_fatal(format_args!("static bundle: unbound import {bad}")));

    registry_register(ctx, reg, *name, target);

    let argv = args.iter().rev().fold(Value::null(), |tail, arg| {
        Value::cons(ctx, Str::new(ctx, arg, true).into(), tail)
    });
    program_arguments_fluid(ctx).set(ctx, argv);

    match crate::runtime::sni::call_function(ctx, *body, slots) {
        ExecutionResult::Ok(_) => {}
        ExecutionResult::Err(err) => static_fatal(err),
    }
}

pub(crate) static SCM_INITIALIZED: Once = Once::new();

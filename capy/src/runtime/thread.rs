//! Thread and GC context management.

use crate::{
    prelude::{HashTable, Keyword},
    rsgc::{
        GarbageCollector, Gc, Mutation, Mutator, Trace,
        barrier::{self},
        mmtk::util::Address,
        sync::thread::Thread,
    },
    runtime::stats::ThreadStats,
};
use crate::{
    prelude::{IntoValue, NativeContinuation, NativeFn, PROCEDURES, current_module},
    runtime::{
        fluids::DynamicState,
        global::{Globals, VM_GLOBALS},
        //image::{ALLOWED_GC, AllowedGC, reader::ImageReader},
        modules::{Module, ModuleRef, resolve_module},
        prelude::VariableRef,
        value::{
            Closure, NativeReturn, ReturnCode, Str, Symbol, Value, init_symbols, init_weak_sets,
            init_weak_tables,
        },
        vm::{
            VMResult, call_scheme, control::ContinuationMarks, debug, load::load_thunk_in_vicinity,
            threading::ThreadObject,
        },
    },
};
use std::{
    cell::{Cell, UnsafeCell},
    sync::{Once, atomic::AtomicUsize},
};

pub(crate) const REGISTER_ARG_COUNT: usize = 4;
pub(crate) const COMPILED_ENTRY_ARG_COUNT: usize = REGISTER_ARG_COUNT + 2;

/// cbindgen:ignore
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Context<'gc> {
    pub(crate) mc: Mutation<'gc>,
}

impl<'gc> From<Mutation<'gc>> for Context<'gc> {
    fn from(mc: Mutation<'gc>) -> Self {
        Self { mc }
    }
}

impl<'gc> Context<'gc> {
    pub const OFFSET_OF_STATE: usize = Mutation::OFFSET_OF_STATE;

    pub fn as_ptr(self) -> *const () {
        self.mc.as_ptr()
    }

    /// # Safety
    ///
    /// `ptr` must have been obtained from `Context::as_ptr()` on a live mutator thread.
    /// The underlying `Mutation` must still be valid for the `'gc` lifetime.
    pub unsafe fn from_ptr(ptr: *const ()) -> Self {
        Self {
            // SAFETY: Delegates to `Mutation::from_ptr`; caller guarantees the pointer is valid.
            mc: unsafe { Mutation::from_ptr(ptr) },
        }
    }

    pub fn new(mc: Mutation<'gc>) -> Self {
        Self { mc }
    }

    pub fn mutation(self) -> Mutation<'gc> {
        self.mc
    }

    #[inline(always)]
    pub fn state(self) -> &'gc State<'gc> {
        self.mc.state()
    }

    pub fn dynamic_state(self) -> Value<'gc> {
        self.state().dynamic_state.save(self)
    }

    pub fn set_dynamic_state(self, state: Value<'gc>) {
        self.state().dynamic_state.restore(self, state);
    }

    pub fn current_continuation_marks(self) -> Gc<'gc, ContinuationMarks<'gc>> {
        Gc::new_with_info(
            *self,
            ContinuationMarks {
                cmarks: self.state().current_marks(),
            },
            crate::runtime::vm::control::CONTINUATION_MARKS_INFO,
        )
    }

    pub fn keyword(self, s: &str) -> Gc<'gc, Keyword<'gc>> {
        let sym = self.intern(s);
        let globals = self.globals().keyword_map.get().downcast::<HashTable>();
        if let Some(kw) = globals.get(self, sym) {
            return kw.downcast();
        }

        let kw = Keyword::from_symbol(*self, sym.downcast());
        globals.put(self, sym, kw);
        kw
    }

    pub fn intern(self, s: &str) -> Value<'gc> {
        Symbol::from_str(self, s).into()
    }

    pub fn str(self, s: &str) -> Value<'gc> {
        Str::from_str(*self, s).into()
    }

    pub fn make_native_closure(
        self,
        proc: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(*self)
            .make_closure(self, proc, free_vars, meta)
    }

    pub fn make_native_continuation(
        self,
        proc: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(*self)
            .make_cont_closure(self, proc, free_vars, meta)
    }

    pub fn make_static_closure(
        self,
        proc: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(*self)
            .register_static_closure(self, proc, meta)
    }

    pub fn make_static_continuation(
        self,
        proc: NativeContinuation<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(*self)
            .register_static_cont_closure(self, proc, meta)
    }

    /// Run `f` outside the GC-mutating world.
    ///
    /// Preparation that needs `Context`, allocation, or tracing must happen before calling
    /// this function. The closure must not capture GC handles or interior pointers into
    /// movable objects.
    pub fn outside_gc_world<T>(self, f: impl FnOnce() -> T) -> T {
        use std::panic::{AssertUnwindSafe, catch_unwind};

        struct OutsideGcGuard;

        impl Drop for OutsideGcGuard {
            fn drop(&mut self) {
                Thread::leave_native();
            }
        }

        self.state().stats.start_blocking();
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

    pub fn return_call(
        self,
        rator: Value<'gc>,
        rands: impl IntoIterator<Item = Value<'gc>>,
        retk: Option<Value<'gc>>,
    ) -> NativeReturn<'gc> {
        self.prepare_call_data(rator, rands, retk);

        NativeReturn {
            code: ReturnCode::Continue,
            value: Value::new(false),
        }
    }

    pub(crate) fn return_apply(
        self,
        rator: Value<'gc>,
        fixed: &[Value<'gc>],
        mut rest: Value<'gc>,
        retk: Value<'gc>,
    ) -> Result<NativeReturn<'gc>, Value<'gc>> {
        let mut rest_len = 0usize;
        let mut cursor = rest;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                return Err(rest);
            }
            rest_len += 1;
            cursor = cursor.cdr();
        }

        let mut args = Vec::with_capacity(fixed.len() + rest_len);
        args.extend_from_slice(fixed);
        while !rest.is_null() {
            args.push(rest.car());
            rest = rest.cdr();
        }

        Ok(self.return_call(rator, args, Some(retk)))
    }

    fn prepare_call_data(
        self,
        rator: Value<'gc>,
        rands: impl IntoIterator<Item = Value<'gc>>,
        retk: Option<Value<'gc>>,
    ) {
        let state = self.state();
        let overflow = state.runstack.get().to_mut_ptr::<Value>();
        let mut argc = 0usize;

        state.call_data.clear();

        if let Some(retk) = retk {
            state.call_data.set_arg(argc, retk);
            argc += 1;
        }

        for rand in rands {
            if argc < REGISTER_ARG_COUNT {
                state.call_data.set_arg(argc, rand);
            } else {
                let overflow_index = argc - REGISTER_ARG_COUNT;
                unsafe {
                    let slot = overflow.add(overflow_index);
                    if Address::from_ptr(slot) >= state.runstack_end {
                        panic!(
                            "runstack overflow: {} >= {}, too many arguments: {}, can fit: {}",
                            Address::from_ptr(slot),
                            state.runstack_end,
                            argc + 1,
                            (state.runstack_end - state.runstack_start) / size_of::<Value>()
                        );
                    }
                    if Address::from_ptr(slot) < state.runstack_start {
                        panic!("runstack underflow");
                    }
                    *slot = rand;
                }
            }
            argc += 1;
        }

        let overflow_count = argc.saturating_sub(REGISTER_ARG_COUNT);
        unsafe {
            state
                .runstack
                .set(Address::from_ptr(overflow.add(overflow_count)));
        }
        state.call_data.argc.set(argc);
        state.call_data.rator.set(rator);
    }

    pub(crate) fn prepare_scheme_call_args(
        self,
        rands: impl IntoIterator<Item = Value<'gc>>,
        retk: Option<Value<'gc>>,
    ) -> (usize, [Value<'gc>; REGISTER_ARG_COUNT]) {
        let state = self.state();
        let overflow = state.runstack.get().to_mut_ptr::<Value>();
        let mut regs = [Value::undefined(); REGISTER_ARG_COUNT];
        let mut argc = 0usize;

        if let Some(retk) = retk {
            regs[argc] = retk;
            argc += 1;
        }

        for rand in rands {
            if argc < REGISTER_ARG_COUNT {
                regs[argc] = rand;
            } else {
                let overflow_index = argc - REGISTER_ARG_COUNT;
                unsafe {
                    let slot = overflow.add(overflow_index);
                    if Address::from_ptr(slot) >= state.runstack_end {
                        panic!(
                            "runstack overflow: {} >= {}, too many arguments: {}, can fit: {}",
                            Address::from_ptr(slot),
                            state.runstack_end,
                            argc + 1,
                            (state.runstack_end - state.runstack_start) / size_of::<Value>()
                        );
                    }
                    if Address::from_ptr(slot) < state.runstack_start {
                        panic!("runstack underflow");
                    }
                    *slot = rand;
                }
            }
            argc += 1;
        }

        let overflow_count = argc.saturating_sub(REGISTER_ARG_COUNT);
        unsafe {
            state
                .runstack
                .set(Address::from_ptr(overflow.add(overflow_count)));
        }

        (argc, regs)
    }

    pub fn module(self, name: &str) -> Option<Gc<'gc, Module<'gc>>> {
        let name = crate::runtime::modules::convert_module_name(self, name);
        resolve_module(self, name, false, false)
    }

    pub fn ensure_module(self, name: &str) -> Gc<'gc, Module<'gc>> {
        let name = crate::runtime::modules::convert_module_name(self, name);
        resolve_module(self, name, true, false).expect("Failed to ensure module")
    }

    pub fn globals(self) -> &'gc Globals<'gc> {
        VM_GLOBALS
            .get()
            .expect("VM globals not initialized")
            .fetch(*self)
    }

    pub fn nest_level(&self) -> usize {
        self.state()
            .nest_level
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn public_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        crate::runtime::modules::public_ref(self, mname, name)
    }

    pub fn private_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        crate::runtime::modules::private_ref(self, mname, name)
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
        let module = self.module(mname)?;
        let name = self.intern(name);
        Some(module.define(self, name, value))
    }

    pub fn define_module(self, name: &str, size_hint: Option<usize>) -> ModuleRef<'gc> {
        let name = crate::runtime::modules::convert_module_name(self, name);

        let module = resolve_module(self, name, false, true).unwrap();
        let wmodule = Gc::write(*self, module);
        if module.uses.get().is_null() {
            barrier::field!(wmodule, Module, uses)
                .unlock()
                .set(Value::cons(
                    self,
                    self.globals().scm_module().into(),
                    Value::null(),
                ));
        }

        if module.public_interface.get().is_none() {
            let public_interface = Module::new(
                self,
                size_hint.unwrap_or(8),
                Value::null(),
                Value::new(false),
            );
            barrier::field!(wmodule, Module, public_interface)
                .unlock()
                .set(Some(public_interface.into()));
        }
        module
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
            let mark_set = set.caar();
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

impl<'gc> std::ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        &self.mc
    }
}

pub struct DeferYield<'gc> {
    ctx: Context<'gc>,
}

impl<'gc> DeferYield<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        ctx.state()
            .nest_level
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self { ctx }
    }
}

impl<'gc> Drop for DeferYield<'gc> {
    fn drop(&mut self) {
        let nest_level = self
            .ctx
            .state()
            .nest_level
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        assert!(nest_level > 0, "Mismatched DeferYield drop");
    }
}

/// Hardcoded limit for runstack to not make calls too slow and runstack too large.
const RUNSTACK_SIZE: usize = 4096;

pub struct State<'gc> {
    pub(crate) dynamic_state: DynamicState<'gc>,
    pub(crate) runstack: Cell<Address>,
    pub(crate) runstack_start: Address,
    pub(crate) runstack_end: Address,
    /// Nest level of nested calls into Scheme from native code.
    pub(crate) nest_level: AtomicUsize,
    pub(crate) gc_save: GcSave<'gc>,
    pub(crate) call_data: CallData<'gc>,
    pub(crate) shadow_stack: UnsafeCell<debug::ShadowStack<'gc>>,
    pub(crate) last_ret_addr: Cell<Address>,
    pub(crate) thread_object: Gc<'gc, ThreadObject<'gc>>,
    pub(crate) accumulator: Cell<Value<'gc>>,
    pub(crate) current_marks: Cell<Value<'gc>>,
    pub(crate) winders: Cell<Value<'gc>>,
    pub(crate) stats: ThreadStats,
}

#[repr(C)]
pub struct CallData<'gc> {
    pub rator: Cell<Value<'gc>>,
    pub argc: Cell<usize>,
    pub arg0: Cell<Value<'gc>>,
    pub arg1: Cell<Value<'gc>>,
    pub arg2: Cell<Value<'gc>>,
    pub arg3: Cell<Value<'gc>>,
}

impl<'gc> CallData<'gc> {
    fn new() -> Self {
        Self {
            rator: Cell::new(Value::undefined()),
            argc: Cell::new(0),
            arg0: Cell::new(Value::undefined()),
            arg1: Cell::new(Value::undefined()),
            arg2: Cell::new(Value::undefined()),
            arg3: Cell::new(Value::undefined()),
        }
    }

    pub(crate) fn set_arg(&self, index: usize, value: Value<'gc>) {
        match index {
            0 => self.arg0.set(value),
            1 => self.arg1.set(value),
            2 => self.arg2.set(value),
            3 => self.arg3.set(value),
            _ => panic!("call register argument index out of bounds: {index}"),
        }
    }

    pub(crate) fn clear(&self) {
        self.rator.set(Value::undefined());
        self.argc.set(0);
        for index in 0..REGISTER_ARG_COUNT {
            self.set_arg(index, Value::undefined());
        }
    }
}

#[repr(C)]
pub struct GcSave<'gc> {
    pub rator: Cell<Value<'gc>>,
    pub argc: Cell<usize>,
    pub arg0: Cell<Value<'gc>>,
    pub arg1: Cell<Value<'gc>>,
    pub arg2: Cell<Value<'gc>>,
    pub arg3: Cell<Value<'gc>>,
}

impl<'gc> GcSave<'gc> {
    fn new() -> Self {
        Self {
            rator: Cell::new(Value::undefined()),
            argc: Cell::new(0),
            arg0: Cell::new(Value::undefined()),
            arg1: Cell::new(Value::undefined()),
            arg2: Cell::new(Value::undefined()),
            arg3: Cell::new(Value::undefined()),
        }
    }

    pub(crate) fn save(&self, argc: usize, args: [Value<'gc>; REGISTER_ARG_COUNT]) {
        self.argc.set(argc);
        self.arg0.set(args[0]);
        self.arg1.set(args[1]);
        self.arg2.set(args[2]);
        self.arg3.set(args[3]);
    }

    pub(crate) fn save_entry(
        &self,
        rator: Value<'gc>,
        argc: usize,
        args: [Value<'gc>; REGISTER_ARG_COUNT],
    ) {
        self.rator.set(rator);
        self.save(argc, args);
    }

    pub(crate) fn clear(&self) {
        self.rator.set(Value::undefined());
        self.argc.set(0);
        self.arg0.set(Value::undefined());
        self.arg1.set(Value::undefined());
        self.arg2.set(Value::undefined());
        self.arg3.set(Value::undefined());
    }
}

// SAFETY: CallData stores GC-managed Values used to stage a tail call from native code.
unsafe impl<'gc> Trace for CallData<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        visitor.trace(&mut self.rator);
        visitor.trace(&mut self.arg0);
        visitor.trace(&mut self.arg1);
        visitor.trace(&mut self.arg2);
        visitor.trace(&mut self.arg3);
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> Trace for GcSave<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        pin_saved_value(self.rator.get(), visitor);
        pin_saved_value(self.arg0.get(), visitor);
        pin_saved_value(self.arg1.get(), visitor);
        pin_saved_value(self.arg2.get(), visitor);
        pin_saved_value(self.arg3.get(), visitor);

        visitor.trace(&mut self.rator);
        visitor.trace(&mut self.arg0);
        visitor.trace(&mut self.arg1);
        visitor.trace(&mut self.arg2);
        visitor.trace(&mut self.arg3);
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

fn pin_saved_value(value: Value<'_>, visitor: &mut crate::rsgc::collection::Visitor) {
    if value.is_cell() && !value.is_empty() {
        let object = unsafe { value.desc.ptr() };
        if let Some(objref) = object.to_objref() {
            visitor.pin_root(objref);
        }
    }
}

// SAFETY: State contains GC roots (dynamic_state, runstack values, shadow_stack, etc.).
// All traced fields are exclusively owned by this mutator thread during GC stop-the-world.
unsafe impl Trace for State<'_> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        visitor.trace(&mut self.dynamic_state);

        let runstack = unsafe {
            // SAFETY: `runstack_start` through `runstack.get()` is a contiguous buffer of Values
            // allocated in `make_fresh_runstack`. The distance gives the number of live slots.
            std::slice::from_raw_parts_mut(
                self.runstack_start.to_mut_ptr::<Value>(),
                (self.runstack.get() - self.runstack_start) / size_of::<Value>(),
            )
        };

        for value in runstack {
            visitor.trace(value);
        }

        visitor.trace(&mut self.gc_save);
        visitor.trace(&mut self.call_data);

        unsafe {
            // SAFETY: `shadow_stack` UnsafeCell is only accessed during GC tracing (stop-the-world)
            // while no other thread can mutate it.
            let stack = &mut *self.shadow_stack.get();

            stack.for_each_mut(|frame| {
                visitor.trace(&mut frame.rator);
                visitor.trace(&mut frame.rands);
            });
        }

        visitor.trace(&mut self.thread_object);
        visitor.trace(&mut self.accumulator);
        visitor.trace(&mut self.current_marks);
        visitor.trace(&mut self.winders);
    }
}

impl<'gc> State<'gc> {
    pub fn new(mc: Mutation<'gc>, thread_object: Gc<'gc, ThreadObject<'gc>>) -> Self {
        let (runstack_start, _runstack_end) = make_fresh_runstack();

        Self {
            shadow_stack: UnsafeCell::new(debug::ShadowStack::new(64)),
            dynamic_state: DynamicState::new(mc),
            runstack: Cell::new(runstack_start),
            runstack_end: _runstack_end,
            nest_level: AtomicUsize::new(0),
            gc_save: GcSave::new(),
            runstack_start,
            call_data: CallData::new(),
            last_ret_addr: Cell::new(Address::ZERO),
            thread_object,
            accumulator: Cell::new(Value::new(false)),
            current_marks: Cell::new(Value::null()),
            winders: Cell::new(Value::null()),
            stats: ThreadStats::new(),
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
    pub unsafe fn set_current_marks(&self, marks: Value<'gc>) {
        self.current_marks.set(marks);
    }
}

pub struct Scheme {
    pub mutator: Mutator<crate::Rootable!(())>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        self.mutator.mutate(|mc, _| {
            let ctx = Context { mc };
            let result = f(ctx);

            unsafe { (*ctx.state().shadow_stack.get()).clear() };
            result
        })
    }

    pub fn call_value<PREP, F, R>(&self, prep: PREP, finish: F) -> R
    where
        F: for<'gc> Fn(Context<'gc>, Result<Value<'gc>, Value<'gc>>) -> R,
        PREP: for<'gc> FnOnce(Context<'gc>, &mut Vec<Value<'gc>>) -> Value<'gc>,
    {
        self.enter(|ctx| {
            let mut args = Vec::with_capacity(4);
            ctx.stats.start_execution();
            let rator = prep(ctx, &mut args);
            let run = call_scheme(ctx, rator, args);
            ctx.stats.end_execution();

            let result = match run {
                VMResult::Ok(ok) => Ok(ok),
                VMResult::Err(err) => Err(err),
            };
            finish(ctx, result)
        })
    }

    /// Calls `entry_name` in module `mod_name`.
    pub fn call<ARGS, F, R>(&self, mod_name: &str, entry_name: &str, setup: ARGS, finish: F) -> R
    where
        F: for<'gc> Fn(Context<'_>, Result<Value<'gc>, Value<'gc>>) -> R,
        ARGS: for<'gc> FnOnce(Context<'gc>, &mut Vec<Value<'gc>>),
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

    pub(crate) fn new() -> Self {
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;
            crate::rsgc::logging::init_rust_logger();
            let mmtk_builder = crate::rsgc::logging::mmtk_builder();
            /*match *mmtk_builder.options.plan {
                PlanSelector::GenImmix | PlanSelector::StickyImmix | PlanSelector::GenCopy => {
                    let _ = ALLOWED_GC.set(AllowedGC::Generational).unwrap();
                }

                PlanSelector::ConcurrentImmix => {
                    let _ = ALLOWED_GC.set(AllowedGC::Concurrent).unwrap();
                }

                _ => {
                    let _ = ALLOWED_GC.set(AllowedGC::Regular).unwrap();
                }
            }*/
            GarbageCollector::init(mmtk_builder);
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(mc);
                    init_weak_tables(mc);
                    init_symbols(mc);

                    let state = State::new(mc, ThreadObject::new(mc, None));
                    mc.init_state(state);
                    ()
                });
                m.mutate(|mc, _| {
                    if should_init {
                        super::init(Context { mc });
                    }
                });
                m
            },
        };

        if should_init { scm.boot() } else { scm }
    }

    pub fn new_uninit() -> Self {
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;

            crate::rsgc::logging::init_rust_logger();
            let mmtk_builder = crate::rsgc::logging::mmtk_builder();
            GarbageCollector::init(mmtk_builder);
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(mc);
                    init_weak_tables(mc);
                    init_symbols(mc);

                    let state = State::new(mc, ThreadObject::new(mc, None));
                    mc.init_state(state);
                    ()
                });
                m
            },
        };

        scm
    }

    /*pub fn from_image(image: &[u8]) -> Self {
        let allowed_gc = match image[0] {
            0 => AllowedGC::Generational,
            1 => AllowedGC::Concurrent,
            2 => AllowedGC::Regular,
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
                    ()
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
                VMResult::Ok(entry) => entry,
                _ => unreachable!(),
            };

            ctx.state().accumulator.set(entrypoint);
        });

        scm
    }*/

    fn boot(self) -> Self {
        let scm = self;
        scm.enter(|ctx| {
            current_module(ctx).set(ctx, (ctx.globals().root_module()).into());

            let thunk = load_thunk_in_vicinity::<true>(ctx, "boot.scm", None::<&str>, false, None)
                .expect("Failed to load boot.scm");

            match call_scheme(ctx, thunk, []) {
                VMResult::Ok(_) => {}
                VMResult::Err(err) => {
                    eprintln!("Failed to boot: {err}");
                    std::process::exit(1);
                }
            }
        });

        scm
    }

    pub(crate) fn forked(thread_object_bits: u64, dynamic_state_bits: u64) -> Self {
        Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    // SAFETY: `thread_object_bits` was obtained from `Gc::as_ptr()` on the
                    // parent thread. The GC keeps the object alive via the parent's root set.
                    let thread_object = unsafe { Gc::from_ptr(thread_object_bits as _) };
                    let state = State::new(mc, thread_object);
                    mc.init_state(state);
                    ()
                });
                m.mutate(|mc, _| {
                    let ctx = Context { mc };
                    ctx.state()
                        .dynamic_state
                        .restore(ctx, Value::from_raw(dynamic_state_bits));
                });
                m
            },
        }
    }
}

impl<'gc> Drop for State<'gc> {
    fn drop(&mut self) {
        // SAFETY: `runstack_start` was allocated in `make_fresh_runstack` with the same
        // layout. We deallocate it here on State drop; no other code frees this buffer.
        unsafe {
            let layout = std::alloc::Layout::from_size_align(
                RUNSTACK_SIZE * std::mem::size_of::<Value>(),
                std::mem::align_of::<Value>(),
            )
            .unwrap();
            std::alloc::dealloc(self.runstack_start.to_mut_ptr() as *mut u8, layout);
        }
    }
}

fn make_fresh_runstack() -> (Address, Address) {
    let layout = std::alloc::Layout::from_size_align(
        RUNSTACK_SIZE * std::mem::size_of::<Value>(),
        std::mem::align_of::<Value>(),
    )
    .unwrap();
    // SAFETY: Layout is valid (non-zero, properly aligned). `alloc` returns a valid
    // pointer or null; we handle the null case. The returned addresses are valid for the
    // lifetime of the State that owns them.
    unsafe {
        let ptr = std::alloc::alloc(layout) as *mut Value;
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        let start = Address::from_ptr(ptr);
        let end = Address::from_ptr(ptr.add(RUNSTACK_SIZE));
        (start, end)
    }
}

pub(crate) static SCM_INITIALIZED: Once = Once::new();

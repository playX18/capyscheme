use crate::{
    prelude::{
        IntoValue, NativeContinuation, NativeFn, PROCEDURES, ScmHeader, TypeCode8, current_module,
    },
    runtime::{
        fluids::DynamicState,
        global::{Globals, VM_GLOBALS},
        image::reader::ImageReader,
        modules::{Module, ModuleRef, resolve_module},
        prelude::VariableRef,
        value::{
            Closure, NativeReturn, ReturnCode, SavedCall, Str, Symbol, Value, init_symbols,
            init_weak_sets, init_weak_tables,
        },
        vm::{
            VMResult, call_scheme, call_scheme_with_k, continue_to,
            control::ContinuationMarks,
            debug,
            load::load_thunk_in_vicinity,
            threading::{Condition, Mutex, MutexKind, ThreadObject},
        },
    },
};
use rsgc::{
    Gc, Mutation, Mutator, Rootable, Trace,
    barrier::{self},
    mmtk::util::Address,
};
use std::{
    cell::{Cell, UnsafeCell},
    ptr::NonNull,
    sync::{Once, atomic::AtomicUsize},
};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Context<'gc> {
    pub(crate) mc: &'gc Mutation<'gc>,
    pub(crate) state: &'gc State<'gc>,
}

impl<'gc> From<(&'gc Mutation<'gc>, &'gc State<'gc>)> for Context<'gc> {
    fn from((mc, state): (&'gc Mutation<'gc>, &'gc State<'gc>)) -> Self {
        Self { mc, state }
    }
}

impl<'gc> Context<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>, state: &'gc State<'gc>) -> Self {
        Self { mc, state }
    }

    pub fn mutation(&self) -> &'gc Mutation<'gc> {
        self.mc
    }

    pub fn state(&self) -> &'gc State<'gc> {
        self.state
    }

    pub fn dynamic_state(&self) -> Value<'gc> {
        self.state.dynamic_state.save(*self)
    }

    pub fn set_dynamic_state(&self, state: Value<'gc>) {
        self.state.dynamic_state.restore(*self, state);
    }

    pub fn current_continuation_marks(&self) -> Gc<'gc, ContinuationMarks<'gc>> {
        Gc::new(
            &self,
            ContinuationMarks {
                header: ScmHeader::with_type_bits(TypeCode8::CMARKS.bits() as _),
                cmarks: self.state.current_marks(),
            },
        )
    }

    pub fn has_suspended_call(&self) -> bool {
        self.state.saved_call.get().is_some()
    }

    pub fn intern(&self, s: &str) -> Value<'gc> {
        Symbol::from_str(*self, s).into()
    }

    pub fn str(&self, s: &str) -> Value<'gc> {
        Str::from_str(&self, s).into()
    }

    pub fn make_native_closure(
        &self,
        proc: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(&self)
            .make_closure(*self, proc, free_vars, meta)
    }

    pub fn make_native_continuation(
        &self,
        proc: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(&self)
            .make_cont_closure(*self, proc, free_vars, meta)
    }

    pub fn make_static_closure(
        &self,
        proc: NativeFn<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(&self)
            .register_static_closure(*self, proc, meta)
    }

    pub fn make_static_continuation(
        &self,
        proc: NativeContinuation<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        PROCEDURES
            .fetch(&self)
            .register_static_cont_closure(*self, proc, meta)
    }

    pub fn resume_suspended_call(&self) -> VMResult<'gc> {
        let call = self
            .state
            .saved_call
            .replace(None)
            .expect("No suspended call");
        self.state.runstack.set(self.state.runstack_start);

        if call.from_procedure {
            let retk = call.rands[0];
            let reth = call.rands[1];

            call_scheme_with_k(
                self,
                retk,
                reth,
                call.rator,
                call.rands[2..].iter().copied(),
            )
        } else {
            unsafe { continue_to(self, call.rator, call.rands[..].iter().copied()) }
        }
    }

    pub fn return_call(
        &self,
        rator: Value<'gc>,
        rands: impl IntoIterator<Item = Value<'gc>>,
        conts: Option<[Value<'gc>; 2]>,
    ) -> NativeReturn<'gc> {
        let rands_ptr = self.state.runstack.get().to_mut_ptr::<Value>();
        let disp = if conts.is_some() { 2 } else { 0 };
        unsafe {
            if let Some(conts) = conts {
                *rands_ptr = conts[0];
                *(rands_ptr.add(1)) = conts[1];
            }
            let mut count = 0;
            for (i, rand) in rands.into_iter().enumerate() {
                *rands_ptr.add(disp + i) = rand;
                count += 1;
            }

            self.state
                .runstack
                .set(Address::from_ptr(rands_ptr.add(count)));
            self.state.call_data.rands.set(rands_ptr);
            self.state.call_data.num_rands.set(count + disp);
            self.state.call_data.rator.set(rator);
        }

        NativeReturn {
            code: ReturnCode::Continue,
            value: Value::new(false),
        }
    }

    pub fn module(self, name: &str) -> Option<Gc<'gc, Module<'gc>>> {
        let name = crate::runtime::modules::convert_module_name(self, name);
        resolve_module(self, name, false, false)
    }

    pub fn ensure_module(self, name: &str) -> Gc<'gc, Module<'gc>> {
        let name = crate::runtime::modules::convert_module_name(self, name);
        resolve_module(self, name, true, false).expect("Failed to ensure module")
    }

    pub fn globals(&self) -> &'gc Globals<'gc> {
        VM_GLOBALS
            .get()
            .expect("VM globals not initialized")
            .fetch(&self)
    }

    pub fn public_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        crate::runtime::modules::public_ref(self, mname, name)
    }

    pub fn private_ref(self, mname: &str, name: &str) -> Option<Value<'gc>> {
        crate::runtime::modules::private_ref(self, mname, name)
    }

    pub fn accumulator(&self) -> Value<'gc> {
        self.state.accumulator.get()
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

    pub fn define_module(&self, name: &str, size_hint: Option<usize>) -> ModuleRef<'gc> {
        let name = crate::runtime::modules::convert_module_name(*self, name);

        let module = resolve_module(*self, name, false, true).unwrap();
        let wmodule = Gc::write(&self, module);
        if module.uses.get().is_null() {
            barrier::field!(wmodule, Module, uses)
                .unlock()
                .set(Value::cons(
                    *self,
                    self.globals().scm_module().into(),
                    Value::null(),
                ));
        }

        if module.public_interface.get().is_none() {
            let public_interface = Module::new(
                *self,
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

    pub(crate) fn winders(&self) -> Value<'gc> {
        self.state.winders.get()
    }

    pub(crate) fn set_winders(&self, winders: Value<'gc>) {
        self.state.winders.set(winders);
    }
}

impl<'gc> std::ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

pub struct DeferYield<'gc> {
    ctx: Context<'gc>,
}

impl<'gc> DeferYield<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        ctx.state
            .nest_level
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self { ctx }
    }
}

impl<'gc> Drop for DeferYield<'gc> {
    fn drop(&mut self) {
        let nest_level = self
            .ctx
            .state
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
    /// Nest level of this thread. If it's larger than 1, it means
    /// that this thread is currently having 2 or more nested calls into Scheme.
    ///
    /// In that case, some operations (primarily GC) won't be allowed.
    pub(crate) nest_level: AtomicUsize,
    pub(crate) call_data: CallData<'gc>,
    pub(crate) saved_call: Cell<Option<Gc<'gc, SavedCall<'gc>>>>,
    pub(crate) shadow_stack: UnsafeCell<debug::ShadowStack<'gc>>,
    pub(crate) last_ret_addr: Cell<Address>,
    pub(crate) thread_object: Gc<'gc, ThreadObject<'gc>>,
    pub(crate) yield_reason: Cell<Option<YieldReason<'gc>>>,
    /// Accumulator field which is used to pass yield interest
    /// to Rust code.
    pub(crate) accumulator: Cell<Value<'gc>>,
    pub(crate) current_marks: Cell<Value<'gc>>,
    pub(crate) winders: Cell<Value<'gc>>,
}

#[repr(C)]
pub struct CallData<'gc> {
    pub rator: Cell<Value<'gc>>,
    pub rands: Cell<*mut Value<'gc>>,
    pub num_rands: Cell<usize>,
}

unsafe impl<'gc> Trace for CallData<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.rator);
        unsafe {
            if !self.rands.get().is_null() {
                for i in 0..self.num_rands.get() {
                    (*self.rands.get().add(i)).trace(visitor);
                }
            }
        }
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Trace for State<'_> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.dynamic_state);

        let runstack = unsafe {
            std::slice::from_raw_parts_mut(
                self.runstack_start.to_mut_ptr::<Value>(),
                (self.runstack.get() - self.runstack_start) / size_of::<Value>(),
            )
        };

        for value in runstack {
            visitor.trace(value);
        }

        unsafe {
            let stack = &mut *self.shadow_stack.get();

            stack.for_each_mut(|frame| {
                visitor.trace(&mut frame.rator);
                visitor.trace(&mut frame.rands);
            });
        }

        if let Some(saved_call) = self.saved_call.get_mut() {
            visitor.trace(saved_call);
        }

        visitor.trace(&mut self.thread_object);
        visitor.trace(&mut self.accumulator);
        visitor.trace(&mut self.yield_reason);
        visitor.trace(&mut self.current_marks);
        visitor.trace(&mut self.winders);
    }
}

impl<'gc> State<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>, thread_object: Gc<'gc, ThreadObject<'gc>>) -> Self {
        let (runstack_start, _runstack_end) = make_fresh_runstack();

        Self {
            shadow_stack: UnsafeCell::new(debug::ShadowStack::new(64)),
            dynamic_state: DynamicState::new(mc),
            runstack: Cell::new(runstack_start),
            runstack_end: _runstack_end,
            nest_level: AtomicUsize::new(0),
            runstack_start,
            call_data: CallData {
                rator: Cell::new(Value::undefined()),
                rands: Cell::new(std::ptr::null_mut()),
                num_rands: Cell::new(0),
            },
            last_ret_addr: Cell::new(Address::ZERO),
            saved_call: Cell::new(None),
            thread_object,
            yield_reason: Cell::new(None),
            accumulator: Cell::new(Value::new(false)),
            current_marks: Cell::new(Value::null()),
            winders: Cell::new(Value::null()),
        }
    }

    pub fn context(&'gc self, mc: &'gc Mutation<'gc>) -> Context<'gc> {
        Context { mc, state: self }
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
    pub mutator: Mutator<Rootable!(State<'_>)>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        let (result, should_gc) = self.mutator.mutate(|mc, state| {
            let ctx = state.context(mc);
            let result = f(ctx);
            unsafe { (*ctx.state.shadow_stack.get()).clear() };
            (result, mc.take_yieldpoint() != 0)
        });

        if should_gc {
            self.mutator.yieldpoint();
        }

        result
    }

    pub fn call_value<PREP, F, R>(&self, prep: PREP, finish: F) -> R
    where
        F: for<'gc> Fn(&Context<'gc>, Result<Value<'gc>, Value<'gc>>) -> R,
        PREP: for<'gc> FnOnce(&Context<'gc>, &mut Vec<Value<'gc>>) -> Value<'gc>,
    {
        let mut result = self.enter(|ctx| {
            let mut args = Vec::with_capacity(4);
            let rator = prep(&ctx, &mut args);

            let run = call_scheme(ctx, rator, args);

            match run {
                VMResult::Ok(ok) => Ok(finish(&ctx, Ok(ok))),
                VMResult::Err(err) => Ok(finish(&ctx, Err(err))),
                VMResult::Yield => match ctx.state.yield_reason.get() {
                    Some(YieldReason::Yieldpoint) => Err(Yield::None),
                    Some(YieldReason::LockMutex(mutex_obj)) => {
                        let mutex = mutex_obj.downcast::<Mutex>();
                        // SAFETY: mutexes are in non-moving space and `mutex_obj` is saved as root.
                        Err(Yield::Lock(PendingLock {
                            mtx: NonNull::from(&mutex.mutex),
                        }))
                    }

                    Some(YieldReason::WaitCondition { condition, mutex }) => {
                        let mutex = mutex.downcast::<Mutex>();
                        let condition = condition.downcast::<Condition>();
                        // SAFETY: mutexes are in non-moving space and `mutex_obj` is saved as root.
                        Err(Yield::Wait(PendingWait {
                            mtx: NonNull::from(&mutex.mutex),
                            condition: NonNull::from(&condition.cond),
                        }))
                    }

                    Some(YieldReason::CollectGarbage) => Err(Yield::GC),

                    None => Err(Yield::None),
                    _ => todo!(),
                },
            }
        });

        loop {
            if let Ok(res) = result {
                return res;
            }

            let pending = result.err().unwrap();

            match pending {
                Yield::None => {
                    result = self.enter(|ctx| {
                        ctx.state.yield_reason.set(None);

                        if ctx.has_suspended_call() {
                            match ctx.resume_suspended_call() {
                                VMResult::Ok(ok) => Ok(finish(&ctx, Ok(ok))),
                                VMResult::Err(err) => Ok(finish(&ctx, Err(err))),
                                VMResult::Yield => match ctx.state.yield_reason.get() {
                                    Some(YieldReason::Yieldpoint) => Err(Yield::None),
                                    Some(YieldReason::LockMutex(mutex_obj)) => {
                                        let mutex = mutex_obj.downcast::<Mutex>();
                                        // SAFETY: mutexes are in non-moving space and `mutex_obj` is saved as root.
                                        Err(Yield::Lock(PendingLock {
                                            mtx: NonNull::from(&mutex.mutex),
                                        }))
                                    }

                                    Some(YieldReason::WaitCondition { condition, mutex }) => {
                                        let mutex = mutex.downcast::<Mutex>();
                                        let condition = condition.downcast::<Condition>();
                                        // SAFETY: mutexes are in non-moving space and `mutex_obj` is saved as root.
                                        Err(Yield::Wait(PendingWait {
                                            mtx: NonNull::from(&mutex.mutex),
                                            condition: NonNull::from(&condition.cond),
                                        }))
                                    }

                                    Some(YieldReason::CollectGarbage) => Err(Yield::GC),
                                    None => Err(Yield::None),
                                    _ => todo!(),
                                },
                            }
                        } else {
                            Ok(finish(&ctx, Err(Value::undefined())))
                        }
                    });
                }

                Yield::Lock(lock) => {
                    lock.lock();

                    result = Err(Yield::None);
                    continue; // retry
                }

                Yield::Wait(wait) => {
                    wait.wait();

                    result = Err(Yield::None);
                    continue; // retry
                }

                Yield::PollRead(fd) => unsafe {
                    let mut readfs = [rustix::event::FdSetElement::default(); 1];
                    rustix::event::fd_set_insert(&mut readfs, fd);

                    let res = rustix::event::select(
                        readfs.len() as _,
                        Some(&mut readfs),
                        None,
                        None,
                        None,
                    );

                    match res {
                        Ok(_) => (),
                        Err(_) => (),
                    }

                    result = Err(Yield::None);
                },

                Yield::PollWrite(fd) => unsafe {
                    let mut writefs = [rustix::event::FdSetElement::default(); 1];
                    rustix::event::fd_set_insert(&mut writefs, fd);

                    let res = rustix::event::select(
                        writefs.len() as _,
                        None,
                        Some(&mut writefs),
                        None,
                        None,
                    );

                    match res {
                        Ok(_) => (),
                        Err(_) => (),
                    }

                    result = Err(Yield::None);
                },

                Yield::GC => {
                    self.collect_garbage();

                    result = Err(Yield::None);
                }
            }
        }
    }

    /// Calls `entry_name` in module `mod_name`.
    pub fn call<ARGS, F, R>(&self, mod_name: &str, entry_name: &str, setup: ARGS, finish: F) -> R
    where
        F: for<'gc> Fn(&Context<'_>, Result<Value<'gc>, Value<'gc>>) -> R,
        ARGS: for<'gc> FnOnce(&Context<'gc>, &mut Vec<Value<'gc>>),
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
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    if should_init {
                        init_weak_sets(&mc);
                        init_weak_tables(&mc);
                        init_symbols(&mc);
                    }

                    State::new(mc, ThreadObject::new(&mc, None))
                });
                m.mutate(|mc, state: &State<'_>| {
                    if should_init {
                        super::init(state.context(mc));
                    }
                });
                m
            },
        };

        if should_init { scm.boot() } else { scm }
    }

    pub fn from_image(image: &[u8]) -> Self {
        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(&mc);
                    init_weak_tables(&mc);
                    State::new(mc, ThreadObject::new(&mc, None))
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

            ctx.state.accumulator.set(entrypoint);
        });

        scm
    }

    fn boot(self) -> Self {
        let scm = self;
        let mut did_yield = scm.enter(|ctx| {
            current_module(ctx).set(ctx, (ctx.globals().root_module()).into());

            let thunk =
                load_thunk_in_vicinity::<true>(ctx, "boot/main.scm", None::<&str>, false, false)
                    .expect("Failed to load boot/main.scm");

            let result = call_scheme(ctx, thunk, []);

            match result {
                VMResult::Ok(_) => {}
                VMResult::Err(err) => {
                    eprintln!("Failed to boot: {err}");
                    std::process::exit(1);
                }
                VMResult::Yield => {
                    return true;
                }
            }

            false
        });

        while did_yield {
            did_yield = scm.enter(|ctx| {
                if ctx.has_suspended_call() {
                    match ctx.resume_suspended_call() {
                        VMResult::Ok(_) => false,
                        VMResult::Err(err) => {
                            eprintln!("Failed to boot: {err}");
                            std::process::exit(1);
                        }
                        VMResult::Yield => true,
                    }
                } else {
                    false
                }
            })
        }

        scm
    }

    pub(crate) fn forked(thread_object_bits: u64, dynamic_state_bits: u64) -> Self {
        Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    let thread_object = unsafe { Gc::from_ptr(thread_object_bits as _) };
                    State::new(mc, thread_object)
                });
                m.mutate(|mc, state: &State| {
                    let ctx = state.context(mc);
                    ctx.state
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

#[derive(Clone, Copy, Trace)]
pub enum YieldReason<'gc> {
    /// Yield occured from a yieldpoint (function entry).
    ///
    /// Code has to determine if GC is required or if there's any pending
    /// interrupts and handle them.
    Yieldpoint,

    LockMutex(Value<'gc>),
    WaitCondition {
        condition: Value<'gc>,
        mutex: Value<'gc>,
    },

    PollRead(i32),
    PollWrite(i32),

    CollectGarbage,
}

pub struct PendingLock {
    mtx: NonNull<MutexKind>,
}

impl PendingLock {
    pub fn lock(&self) {
        unsafe {
            match self.mtx.as_ref() {
                MutexKind::Reentrant(mutex) => {
                    let guard = mutex.lock();
                    std::mem::forget(guard);
                }
                MutexKind::Regular(mutex) => {
                    let guard = mutex.lock();
                    std::mem::forget(guard);
                }
            }
        }
    }
}

pub struct PendingWait {
    mtx: NonNull<MutexKind>,
    condition: NonNull<parking_lot::Condvar>,
}

impl PendingWait {
    pub fn wait(&self) {
        unsafe {
            match self.mtx.as_ref() {
                MutexKind::Reentrant(_) => {
                    unreachable!("Reentrant mutexes cannot be used with condition variables");
                }
                MutexKind::Regular(mutex) => {
                    let mut guard = mutex.make_guard_unchecked();
                    self.condition.as_ref().wait(&mut guard);
                    std::mem::forget(guard);
                }
            }
        }
    }
}

pub enum Yield {
    None,
    Lock(PendingLock),
    Wait(PendingWait),
    GC,

    PollRead(i32),
    PollWrite(i32),
}

pub(crate) static SCM_INITIALIZED: Once = Once::new();

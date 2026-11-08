use super::*;

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
    /// [`crate::runtime::objectmodel::ProcedureRegistry::make_cached_closure`].
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
    /// movable objects unless they are rooted on [`SchemeState::root_stack`] /
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
    pub fn instance(self, name: &str) -> Option<Gc<'gc, LibraryInstance<'gc>>> {
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
    pub fn ensure_instance(self, name: &str) -> Gc<'gc, LibraryInstance<'gc>> {
        if let Some(inst) = self.instance(name) {
            return inst;
        }
        let key = crate::runtime::linklet::library_name(self, name);
        let inst = LibraryInstance::new(self, key, true);
        crate::runtime::linklet::registry_register(
            self,
            crate::runtime::linklet::root_registry(self),
            key,
            inst,
        );
        inst
    }

    pub fn globals(self) -> &'gc VmState<'gc> {
        VM_STATE
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

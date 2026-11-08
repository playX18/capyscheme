use super::*;

pub struct SchemeState<'gc> {
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

// SAFETY: SchemeState contains GC roots (dynamic_state, etc.).
// All traced fields are exclusively owned by this mutator thread during GC stop-the-world.
// The Scheme stack moved to `ThreadContext`, which the collector traces separately.
unsafe impl Trace for SchemeState<'_> {
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}

    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        visitor.trace(&mut self.dynamic_state);

        visitor.trace(&mut self.root_stack);
        visitor.trace(&mut self.thread_object);
        trace_root_guarded("SchemeState.accumulator", self.accumulator.get_mut(), visitor);
        trace_root_guarded("SchemeState.current_marks", self.current_marks.get_mut(), visitor);
        trace_root_guarded("SchemeState.winders", self.winders.get_mut(), visitor);
        trace_root_guarded("SchemeState.exit_value", self.exit_value.get_mut(), visitor);
    }
}

impl<'gc> SchemeState<'gc> {
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


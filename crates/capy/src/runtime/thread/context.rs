use super::*;

/// Machine-level thread context.
///
/// Lives directly in the [`VirtualThread`](crate::heap::sync::thread::VirtualThread) header,
/// so the `TC` register reaches every field at a constant offset, see
/// [`sabi`](crate::runtime::sabi).
#[repr(C)]

pub struct ThreadContext<'gc> {
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

impl<'gc> ThreadContext<'gc> {
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

// SAFETY: ThreadContext holds GC roots (the Scheme stack, rv/cp/exit_value).
// All traced fields are exclusively owned by this mutator thread during GC stop-the-world.
unsafe impl Trace for ThreadContext<'_> {
    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}

    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::collection::Visitor) {
        // SAFETY: the mutator is stopped, so the stack registers describe a
        // consistent stack: every frame below the top one is parked at a
        // return point, and the top one at a safepoint (a compiled frame is
        // parked at a poll with `sp = fp + 1`; see `Deferred::Poll`).
        unsafe {
            crate::runtime::interp::stack::trace_stack(&mut self.stack, visitor);
        }
        // Scratch roots: optimized code publishes the machine `RV` register
        // here at entry/return points, and not every publication is a value.
        // Never hand the collector a raw word; clear it instead.
        trace_root_guarded("ThreadContext.rv", self.rv.get_mut(), visitor);
        trace_root_guarded("ThreadContext.cp", self.cp.get_mut(), visitor);
        trace_root_guarded(
            "ThreadContext.exit_value",
            self.exit_value.get_mut(),
            visitor,
        );
    }
}

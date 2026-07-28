use std::collections::HashSet;

use mmtk::vm::SlotVisitor;

use crate::heap::{
    ObjectSlot,
    collection::{Visitor, VisitorKind},
    mm::MemoryManager,
    object::GcObject,
    sync::thread::Thread,
    traits::Trace,
};
pub struct RustScanning;

impl mmtk::vm::Scanning<MemoryManager> for RustScanning {
    const UNIQUE_OBJECT_ENQUEUING: bool = false;

    fn scan_object<SV: mmtk::vm::SlotVisitor<<MemoryManager as mmtk::vm::VMBinding>::VMSlot>>(
        _tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        slot_visitor: &mut SV,
    ) {
        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(slot_visitor), Some(object)) };

        GcObject::from(object).trace(&mut visitor);
    }

    fn support_slot_enqueuing(
        _tls: mmtk::util::VMWorkerThread,
        _object: mmtk::util::ObjectReference,
    ) -> bool {
        // TRUE for all objects right now.
        true
    }

    fn scan_object_and_trace_edges<OT: mmtk::vm::ObjectTracer>(
        _tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        object_tracer: &mut OT,
    ) {
        let mut visitor = unsafe { Visitor::new(VisitorKind::Trace(object_tracer), Some(object)) };
        GcObject::from(object).trace(&mut visitor);
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {}

    fn scan_roots_in_mutator_thread(
        _tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<MemoryManager>,
        mut factory: impl mmtk::vm::RootsWorkFactory<<MemoryManager as mmtk::vm::VMBinding>::VMSlot>,
    ) {
        let mtls = mutator.mutator_tls;
        let thread = Thread::from_mutator_thread(mtls);
        let mut sv = RootSlotVisitor::new();
        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(&mut sv), None) };
        unsafe {
            if thread.is_thread_state_initialized() {
                let state = thread
                    .native_data()
                    .state
                    .get()
                    .as_mut()
                    .expect("index in range");
                state.assume_init_mut().trace(&mut visitor);
            }
        }

        let Some(mut state) = unsafe { &mut *thread.native_data_mut_ptr() }.mutator_state else {
            factory.create_process_roots_work(sv.set.into_iter().collect());
            return;
        };

        unsafe {
            state.as_mut().root.trace(&mut visitor);

            if visitor.has_weak_refs() {
                crate::heap::GarbageCollector::get()
                    .weak
                    .add_root_with_weak_ref(&mut state.as_mut().root as *mut dyn Trace);
            }

            factory.create_process_roots_work(sv.set.into_iter().collect());
        }
    }

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        mut factory: impl mmtk::vm::RootsWorkFactory<<MemoryManager as mmtk::vm::VMBinding>::VMSlot>,
    ) {
        crate::heap::oop_storage::OopStorageSet::get().scan_strong(&mut factory);
        crate::heap::GarbageCollector::get()
            .global_registry
            .scan(factory);
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}

    fn process_weak_refs(
        worker: &mut mmtk::scheduler::GCWorker<MemoryManager>,
        mut tracer_context: impl mmtk::vm::ObjectTracerContext<MemoryManager>,
    ) -> bool {
        let gc = crate::heap::GarbageCollector::get();
        // #region agent log
        static WEAK_PASS: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);
        let pass = WEAK_PASS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let t0 = std::time::Instant::now();
        // #endregion
        // Existing weak/ephemeron + finalizer processing (may expand TC).
        let weak_dirty = gc.weak.run(worker, &mut tracer_context)
            || gc.finalizers.process(worker, &mut tracer_context);
        // GreyedByMarking constraint convergence (may schedule VMRefClosure packets).
        let constraint_dirty = {
            let set = gc.constraints.read();
            set.execute_vmref_convergence(worker, &mut tracer_context)
        };
        let ret = weak_dirty || constraint_dirty;
        // #region agent log
        {
            use std::io::Write;
            if let Ok(mut f) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("/home/adel/projects/capyscheme/.cursor/debug-3ed3b0.log")
            {
                let _ = writeln!(
                    f,
                    r#"{{"sessionId":"3ed3b0","runId":"hang1","hypothesisId":"A","location":"scanning.rs:process_weak_refs","message":"weak_refs_pass","data":{{"pass":{},"weak_dirty":{},"constraint_dirty":{},"ret":{},"elapsed_ms":{}}},"timestamp":{}}}"#,
                    pass,
                    weak_dirty,
                    constraint_dirty,
                    ret,
                    t0.elapsed().as_millis(),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .map(|d| d.as_millis())
                        .unwrap_or(0)
                );
            }
        }
        // #endregion
        ret
    }

    fn forward_weak_refs(
        _worker: &mut mmtk::scheduler::GCWorker<MemoryManager>,
        _tracer_context: impl mmtk::vm::ObjectTracerContext<MemoryManager>,
    ) {
    }
}

pub(crate) struct RootSlotVisitor {
    pub set: HashSet<ObjectSlot>,
}

impl RootSlotVisitor {
    pub fn new() -> Self {
        Self {
            set: HashSet::new(),
        }
    }
}

impl SlotVisitor<ObjectSlot> for RootSlotVisitor {
    fn visit_slot(&mut self, slot: ObjectSlot) {
        self.set.insert(slot);
    }
}

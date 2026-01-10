use std::collections::HashSet;

use mmtk::vm::SlotVisitor;

use crate::rsgc::ObjectSlot;
use crate::rsgc::collection::{Visitor, VisitorKind};
use crate::rsgc::{mm::MemoryManager, object::GCObject, sync::thread::Thread, traits::Trace};
pub struct RustScanning;

impl mmtk::vm::Scanning<MemoryManager> for RustScanning {
    const UNIQUE_OBJECT_ENQUEUING: bool = false;

    fn scan_object<SV: mmtk::vm::SlotVisitor<<MemoryManager as mmtk::vm::VMBinding>::VMSlot>>(
        _tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        slot_visitor: &mut SV,
    ) {
        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(slot_visitor), Some(object)) };

        let obj = GCObject::from(object);
        let header = obj.header();
        let vt = header.vtable();

        (vt.trace)(obj, &mut visitor);
    }

    fn support_slot_enqueuing(
        _tls: mmtk::util::VMWorkerThread,
        _object: mmtk::util::ObjectReference,
    ) -> bool {
        true
    }

    fn scan_object_and_trace_edges<OT: mmtk::vm::ObjectTracer>(
        _tls: mmtk::util::VMWorkerThread,
        _object: mmtk::util::ObjectReference,
        _object_tracer: &mut OT,
    ) {
        todo!()
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
                let state = thread.native_data().state.get().as_mut().unwrap();
                state.assume_init_mut().trace(&mut visitor);
            }
        }

        let Some(mut state) = thread.native_data_mut().mutator_state else {
            factory.create_process_pinning_roots_work(visitor.pinned_roots);
            factory.create_process_roots_work(sv.set.into_iter().collect());

            return;
        };

        unsafe {
            state.as_mut().root.trace(&mut visitor);

            if visitor.has_weak_refs() {
                crate::rsgc::GarbageCollector::get()
                    .weak
                    .add_root_with_weak_ref(&mut state.as_mut().root as *mut dyn Trace);
            }
            factory.create_process_pinning_roots_work(visitor.pinned_roots);
            factory.create_process_roots_work(sv.set.into_iter().collect());
        }
    }

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        _factory: impl mmtk::vm::RootsWorkFactory<<MemoryManager as mmtk::vm::VMBinding>::VMSlot>,
    ) {
        crate::rsgc::GarbageCollector::get()
            .global_registry
            .scan(_factory);
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}

    fn process_weak_refs(
        worker: &mut mmtk::scheduler::GCWorker<MemoryManager>,
        mut tracer_context: impl mmtk::vm::ObjectTracerContext<MemoryManager>,
    ) -> bool {
        let gc = crate::rsgc::GarbageCollector::get();
        gc.weak.run(worker, &mut tracer_context)
            || gc.finalizers.process(worker, &mut tracer_context)
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

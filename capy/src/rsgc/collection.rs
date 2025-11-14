use mmtk::{
    util::{ObjectReference, VMThread, VMWorkerThread},
    vm::{ActivePlan, GCThreadContext, ObjectTracer, SlotVisitor, slot::Slot},
};

use crate::rsgc::{
    Gc, ObjectSlot,
    mm::MemoryManager,
    sync::thread::{
        GC_BLOCK_ADAPTER, Thread, ThreadState, deinit_current_thread, init_current_thread,
    },
    traits::Trace,
};

#[derive(Default)]
pub struct Collection;

impl mmtk::vm::Collection<MemoryManager> for Collection {
    fn stop_all_mutators<F>(_tls: mmtk::util::VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<MemoryManager>),
    {
        let threads = &super::GarbageCollector::get().threads;
        threads.block_all_mutators_for_gc();
        unsafe { threads.flush_tlabs() };
        for thread in super::active_plan::ActivePlan::mutators() {
            mutator_visitor(thread);
        }
    }

    fn resume_mutators(_tls: mmtk::util::VMWorkerThread) {
        let threads = &super::GarbageCollector::get().threads;
        threads.resume_all_mutators_from_gc();
    }

    fn block_for_gc(tls: mmtk::util::VMMutatorThread) {
        let thread = Thread::from_mutator_thread(tls);
        assert!(
            matches!(
                thread.get_exec_status(),
                ThreadState::Mutating | ThreadState::MutatingToBlock
            ),
            "thread must be in mutating state to block for GC, got: {:?}",
            thread.get_exec_status()
        );

        thread.block(&GC_BLOCK_ADAPTER, false);
    }

    fn spawn_gc_thread(_tls: mmtk::util::VMThread, ctx: mmtk::vm::GCThreadContext<MemoryManager>) {
        std::thread::spawn(move || match ctx {
            GCThreadContext::Worker(worker) => {
                let thread = Thread::new(false);
                init_current_thread(thread.clone());
                worker.run(
                    VMWorkerThread(thread.to_vmthread()),
                    &super::GarbageCollector::get().mmtk,
                );

                deinit_current_thread();
            }
        });
    }

    fn schedule_finalization(_tls: VMWorkerThread) {
        super::GarbageCollector::get().finalizers.schedule();
    }

    fn out_of_memory(_tls: VMThread, _err_kind: mmtk::util::alloc::AllocationError) {}

    fn post_forwarding(_tls: VMWorkerThread) {}

    fn vm_live_bytes() -> usize {
        0
    }

    fn is_collection_enabled() -> bool {
        true
    }

    fn create_gc_trigger() -> Box<dyn mmtk::util::heap::GCTriggerPolicy<MemoryManager>> {
        todo!()
    }
}

pub enum VisitorKind<'a> {
    Slot(&'a mut dyn SlotVisitor<ObjectSlot>),
    Trace(&'a mut dyn ObjectTracer),
}

impl<'a> From<&'a mut dyn ObjectTracer> for VisitorKind<'a> {
    fn from(val: &'a mut dyn ObjectTracer) -> Self {
        VisitorKind::Trace(val)
    }
}

pub struct Visitor<'a> {
    kind: VisitorKind<'a>,
    pub(crate) has_weak_refs: bool,
    pub(crate) current_object: Option<ObjectReference>,
}

impl<'a> SlotVisitor<ObjectSlot> for Visitor<'a> {
    fn visit_slot(&mut self, slot: ObjectSlot) {
        match &mut self.kind {
            VisitorKind::Slot(sv) => {
                sv.visit_slot(slot);
            }

            VisitorKind::Trace(tracer) => {
                let Some(obj) = slot.load() else {
                    return;
                };
                let new = tracer.trace_object(obj);
                slot.store(new);
            }
        }
    }
}

impl<'a> Visitor<'a> {
    /// Create a new visitor for tracing objects.
    ///
    /// # Safety
    ///
    /// Must be only created by GC or at GC time. Otherwise, it might access
    /// some data which is valid only during GC.
    pub unsafe fn new(
        tracer: impl Into<VisitorKind<'a>>,
        current_object: Option<ObjectReference>,
    ) -> Self {
        Self {
            kind: tracer.into(),
            current_object,
            has_weak_refs: false,
        }
    }

    pub fn has_weak_refs(&self) -> bool {
        self.has_weak_refs
    }

    /// Registers currently scanned object for weak processing.
    ///
    /// After this function is called, [`process_weak_refs`](Trace::process_weak_refs) will be
    /// invoked on corresponding object after marking closure.
    pub fn register_for_weak_processing(&mut self) {
        self.has_weak_refs |= true;
        if let Some(objref) = self.current_object {
            crate::rsgc::GarbageCollector::get()
                .weak
                .add_object_with_weak_ref(objref);
        }
    }

    pub fn register_object_for_weak_processing<T>(&mut self, object: Gc<'_, T>) {
        self.has_weak_refs |= true;

        crate::rsgc::GarbageCollector::get()
            .weak
            .add_object_with_weak_ref(object.to_object_reference());
    }

    pub fn trace<T: Trace>(&mut self, value: &mut T) {
        unsafe {
            value.trace(self);
        }
    }
}

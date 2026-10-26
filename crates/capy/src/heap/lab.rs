//! Local allocation buffer

use mmtk::util::{Address, conversions::raw_align_up};

use crate::heap::sync::thread::VirtualThread;
use crate::utils::align_allocation_no_fill;

impl VirtualThread {
    pub(crate) fn lab_allocate(&mut self, size: usize, alignment: usize, offset: usize) -> Address {
        let size = raw_align_up(size, alignment);
        let result = align_allocation_no_fill(self.ap, alignment, offset);
        if result + size > self.eap {
            return Address::ZERO;
        }

        self.ap = result.add(size);
        result
    }

    pub(crate) fn lab_rebind(&mut self, cursor: Address, limit: Address) {
        self.ap = cursor;
        self.eap = limit;
    }

    /// Hands the window out, leaving it empty.
    pub(crate) fn lab_take(&mut self) -> (Address, Address) {
        let cursor = self.ap;
        let limit = self.eap;

        self.lab_reset();

        (cursor, limit)
    }

    pub(crate) fn lab_reset(&mut self) {
        self.ap = Address::ZERO;
        self.eap = Address::ZERO;
    }
}

impl crate::heap::sync::thread::NativeCtx {
    /// Run `f` on the default-space bump window of this worker's allocator.
    /// `None` when the default space is not bump-allocated (no LAB then).
    ///
    /// # Safety
    ///
    /// The worker's mutator must be bound and not used concurrently. Callers are
    /// the worker itself, or the collector while the world is stopped.
    unsafe fn with_default_window<R>(
        &self,
        f: impl FnOnce(&mut mmtk::util::alloc::BumpPointer) -> R,
    ) -> Option<R> {
        use mmtk::{
            AllocationSemantics,
            util::alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
        };
        let selector = mmtk::memory_manager::get_allocator_mapping(
            &crate::heap::GarbageCollector::get().mmtk,
            AllocationSemantics::Default,
        );
        // SAFETY: see the function contract; MMTk exclusivity holds per worker.
        unsafe {
            let mutator = self.mutator_unchecked();
            match selector {
                AllocatorSelector::Immix(_) => Some(f(&mut mutator
                    .allocator_impl_mut::<ImmixAllocator<crate::heap::mm::MemoryManager>>(selector)
                    .bump_pointer)),
                AllocatorSelector::BumpPointer(_) => Some(f(&mut mutator
                    .allocator_impl_mut::<BumpAllocator<crate::heap::mm::MemoryManager>>(selector)
                    .bump_pointer)),
                _ => None,
            }
        }
    }

    pub(crate) fn donate_lab(&self, g: &mut VirtualThread) {
        if !g.ap.is_zero() {
            return;
        }
        let (cursor, limit) = self.lab_spare.replace((Address::ZERO, Address::ZERO));
        if !cursor.is_zero() {
            g.lab_rebind(cursor, limit);
        }
    }

    /// Give `g`'s window back to this worker's allocator before `g` switches
    /// out, so a thread that is not running never holds one.
    ///
    /// # Safety
    ///
    /// The caller must be in the mutating state on this worker, and `g` must
    /// be the thread running on it.
    pub(crate) unsafe fn flush_lab(&self, g: &mut VirtualThread) {
        debug_assert!(
            matches!(
                crate::heap::sync::thread::current_thread().get_exec_status(),
                crate::heap::sync::thread::ThreadState::Mutating
                    | crate::heap::sync::thread::ThreadState::MutatingToBlock
            ),
            "LAB window moved outside the mutating state"
        );
        let (cursor, limit) = g.lab_take();
        if cursor.is_zero() {
            return;
        }
        // SAFETY: forwarded from the caller's contract.
        unsafe {
            self.with_default_window(|bump| bump.reset(cursor, limit));
        }
    }

    /// Whether [`flush_lab`](Self::flush_lab) would touch the allocator.
    pub(crate) fn spare_lab_held(&self) -> bool {
        !self.lab_spare.get().0.is_zero()
    }

    /// Hand the spare window to the allocator. Called by the collector with the
    /// world stopped.
    ///
    /// # Safety
    ///
    /// Only the collector may call this, while this worker is blocked.
    pub(crate) unsafe fn return_spare_lab(&self) {
        let (cursor, limit) = self.lab_spare.replace((Address::ZERO, Address::ZERO));
        if cursor.is_zero() {
            return;
        }

        unsafe {
            self.with_default_window(|bump| bump.reset(cursor, limit));
        }
    }
}

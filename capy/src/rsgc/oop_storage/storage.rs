//! OopStorage — full port of HotSpot `oopStorage.cpp`.

use std::cell::UnsafeCell;
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

use mmtk::vm::RootsWorkFactory;
use parking_lot::{Mutex, MutexGuard};

use crate::rsgc::{
    ObjectSlot,
    collection::{Visitor, VisitorKind},
    scanning::RootSlotVisitor,
    traits::Trace,
};
use crate::runtime::value::Value;

use super::block::{ActiveArray, AllocationList, Block, is_empty_bitmask, is_full_bitmask};
use super::synchronizer::{CriticalSection, SingleWriterSynchronizer};

const INITIAL_ACTIVE_ARRAY_SIZE: usize = 8;
const CLEANUP_DEFER_PERIOD: Duration = Duration::from_millis(500);

static NEEDS_CLEANUP_REQUESTED: AtomicBool = AtomicBool::new(false);
static CLEANUP_PERMIT_AFTER: Mutex<Option<Instant>> = Mutex::new(None);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryStatus {
    Invalid,
    Unallocated,
    Allocated,
}

pub type NumDeadCallback = fn(usize);

/// Off-heap pool of precise [`Value`] root slots (HotSpot `OopStorage`).
pub struct OopStorage {
    name: &'static str,
    active_array: AtomicPtr<ActiveArray>,
    /// Protected by [`Self::allocation_mutex`].
    allocation_list: UnsafeCell<AllocationList>,
    deferred_updates: AtomicPtr<Block>,
    allocation_mutex: Mutex<()>,
    active_mutex: Mutex<()>,
    num_dead_callback: Mutex<Option<NumDeadCallback>>,
    allocation_count: AtomicUsize,
    protect_active: SingleWriterSynchronizer,
    concurrent_iteration_count: Mutex<i32>,
    needs_cleanup: AtomicBool,
    /// When true, this storage is scanned with weak semantics (`weak_oops_do`).
    weak: bool,
}

// SAFETY: HotSpot synchronization protocols.
unsafe impl Send for OopStorage {}
unsafe impl Sync for OopStorage {}

impl OopStorage {
    /// Alias for [`Self::create`].
    pub fn new(name: &'static str) -> Box<Self> {
        Self::create(name)
    }

    pub fn create(name: &'static str) -> Box<Self> {
        Self::create_with_strength(name, false)
    }

    pub fn create_strong(name: &'static str) -> Box<Self> {
        Self::create_with_strength(name, false)
    }

    pub fn create_weak(name: &'static str) -> Box<Self> {
        Self::create_with_strength(name, true)
    }

    fn create_with_strength(name: &'static str, weak: bool) -> Box<Self> {
        let array = ActiveArray::create(INITIAL_ACTIVE_ARRAY_SIZE).expect("initial ActiveArray");
        let array_ptr = Box::into_raw(array);
        unsafe { (*array_ptr).increment_refcount() };

        Box::new(Self {
            name,
            active_array: AtomicPtr::new(array_ptr),
            allocation_list: UnsafeCell::new(AllocationList::new()),
            deferred_updates: AtomicPtr::new(ptr::null_mut()),
            allocation_mutex: Mutex::new(()),
            active_mutex: Mutex::new(()),
            num_dead_callback: Mutex::new(None),
            allocation_count: AtomicUsize::new(0),
            protect_active: SingleWriterSynchronizer::new(),
            concurrent_iteration_count: Mutex::new(0),
            needs_cleanup: AtomicBool::new(false),
            weak,
        })
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn is_weak(&self) -> bool {
        self.weak
    }

    pub fn allocation_count(&self) -> usize {
        self.allocation_count.load(Ordering::Relaxed)
    }

    pub fn block_count(&self) -> usize {
        let wab = WithActiveArray::new(self);
        wab.active_array().block_count()
    }

    pub fn total_memory_usage(&self) -> usize {
        let mut total = size_of::<OopStorage>();
        total += self.name.len() + 1;
        total += size_of::<ActiveArray>();
        let wab = WithActiveArray::new(self);
        let blocks = wab.active_array();
        total += blocks.block_count() * Block::allocation_size();
        total += blocks.size() * size_of::<*mut Block>();
        total
    }

    pub fn register_num_dead_callback(&self, f: NumDeadCallback) {
        let mut cb = self.num_dead_callback.lock();
        assert!(cb.is_none(), "Only one callback function supported");
        *cb = Some(f);
    }

    pub fn report_num_dead(&self, num_dead: usize) {
        if let Some(f) = *self.num_dead_callback.lock() {
            f(num_dead);
        }
    }

    pub fn should_report_num_dead(&self) -> bool {
        self.num_dead_callback.lock().is_some()
    }

    pub(crate) fn record_needs_cleanup(&self) {
        self.needs_cleanup.store(true, Ordering::Release);
        NEEDS_CLEANUP_REQUESTED.store(true, Ordering::SeqCst);
    }

    /// HotSpot `has_cleanup_work_and_reset` (caller serializes like Service_lock).
    pub fn has_cleanup_work_and_reset() -> bool {
        if !NEEDS_CLEANUP_REQUESTED.load(Ordering::Acquire) {
            return false;
        }
        let mut permit = CLEANUP_PERMIT_AFTER.lock();
        let now = Instant::now();
        if let Some(after) = *permit {
            if now <= after {
                return false;
            }
        }
        *permit = Some(now + CLEANUP_DEFER_PERIOD);
        NEEDS_CLEANUP_REQUESTED.store(false, Ordering::Release);
        true
    }

    pub fn trigger_cleanup_if_needed() {
        // HotSpot notifies Service_lock; Capy clients poll `has_cleanup_work_and_reset`.
    }

    /// Allocate a slot. Returns pointer to an empty Value, or None on OOM.
    pub fn allocate(&self) -> Option<*mut Value<'static>> {
        let mut guard = self.allocation_mutex.lock();
        let block = self.block_for_allocation(&mut guard)?;
        let block_ref = unsafe { &*block };
        debug_assert!(!block_ref.is_full());
        if block_ref.is_empty() {
            self.log_block_transition(block, "not empty");
        }
        let result = block_ref.allocate();
        debug_assert!(!result.is_null());
        debug_assert!(!block_ref.is_empty());
        self.allocation_count.fetch_add(1, Ordering::Relaxed);
        if block_ref.is_full() {
            self.log_block_transition(block, "full");
            unsafe { self.allocation_list_mut().unlink(block_ref) };
        }
        log::trace!(target: "oopstorage", "{}: allocated {:p}", self.name(), result);
        Some(result)
    }

    /// Bulk allocate into `ptrs`. Returns number allocated (≤ min(len, [`BULK_ALLOCATE_LIMIT`])).
    pub fn allocate_bulk(&self, ptrs: &mut [*mut Value<'static>]) -> usize {
        let size = ptrs.len();
        assert!(size > 0);
        let (block, taken) = {
            let mut guard = self.allocation_mutex.lock();
            let block = match self.block_for_allocation(&mut guard) {
                Some(b) => b,
                None => return 0,
            };
            let block_ref = unsafe { &*block };
            unsafe { self.allocation_list_mut().unlink(block_ref) };
            if block_ref.is_empty() {
                self.log_block_transition(block, "not empty");
            }
            let taken = block_ref.allocate_all();
            debug_assert!(!is_empty_bitmask(taken));
            (block, taken)
            // Drop lock: entries claimed.
        };

        let block_ref = unsafe { &*block };
        let num_taken = taken.count_ones() as usize;
        self.allocation_count.fetch_add(num_taken, Ordering::Relaxed);

        let limit = num_taken.min(size);
        let mut remaining = taken;
        for i in 0..limit {
            debug_assert_ne!(remaining, 0);
            let index = remaining.trailing_zeros();
            remaining ^= block_ref.bitmask_for_index(index);
            ptrs[i] = block_ref.get_pointer(index);
        }

        if remaining != 0 {
            debug_assert_eq!(size, limit);
            debug_assert_eq!(num_taken, limit + remaining.count_ones() as usize);
            self.release_entries_of(block_ref, remaining);
            self.allocation_count
                .fetch_sub(num_taken - limit, Ordering::Relaxed);
        }

        log::trace!(
            target: "oopstorage",
            "{}: bulk allocate {}, returned {}",
            self.name(),
            limit,
            num_taken.saturating_sub(limit)
        );
        limit
    }

    /// Release a previously allocated slot. `*ptr` must already be empty.
    ///
    /// # Safety
    /// `ptr` must be a live allocated entry of this storage.
    pub unsafe fn release(&self, ptr: *mut Value<'static>) {
        debug_assert!(!ptr.is_null());
        debug_assert!(unsafe { (*ptr).is_empty() }, "Releasing uncleared entry");
        let block = self.block_for_ptr(ptr);
        assert!(!block.is_null(), "{}: invalid release {:p}", self.name(), ptr);
        log::trace!(target: "oopstorage", "{}: releasing {:p}", self.name(), ptr);
        let block_ref = unsafe { &*block };
        let releasing = block_ref.bitmask_for_entry(ptr);
        self.release_entries_of(block_ref, releasing);
        self.allocation_count.fetch_sub(1, Ordering::Relaxed);
    }

    /// Bulk release. Best if `ptrs` is sorted by address.
    ///
    /// # Safety
    /// Every pointer must be a live allocated empty entry of this storage.
    pub unsafe fn release_bulk(&self, ptrs: &[*mut Value<'static>]) {
        let mut i = 0;
        while i < ptrs.len() {
            let ptr = ptrs[i];
            debug_assert!(!ptr.is_null());
            debug_assert!(unsafe { (*ptr).is_empty() });
            let block = self.block_for_ptr(ptr);
            assert!(!block.is_null(), "{}: invalid release {:p}", self.name(), ptr);
            let block_ref = unsafe { &*block };
            let mut count = 0usize;
            let mut releasing = 0u64;
            while i < ptrs.len() {
                let entry = ptrs[i];
                debug_assert!(!entry.is_null());
                debug_assert!(unsafe { (*entry).is_empty() });
                if !block_ref.contains(entry) {
                    break;
                }
                log::trace!(target: "oopstorage", "{}: releasing {:p}", self.name(), entry);
                let entry_bitmask = block_ref.bitmask_for_entry(entry);
                debug_assert_eq!(releasing & entry_bitmask, 0, "Duplicate entry");
                releasing |= entry_bitmask;
                count += 1;
                i += 1;
            }
            self.release_entries_of(block_ref, releasing);
            self.allocation_count.fetch_sub(count, Ordering::Relaxed);
        }
    }

    fn release_entries_of(&self, block: &Block, releasing: u64) {
        let transition = block.release_entries(releasing, &self.deferred_updates);
        if transition.claimed_deferred {
            if transition.from_full {
                log::trace!(
                    target: "oopstorage",
                    "{}: block not full {:p}",
                    self.name(),
                    block
                );
            }
            if transition.to_empty {
                log::trace!(
                    target: "oopstorage",
                    "{}: block empty {:p}",
                    self.name(),
                    block
                );
                // Only to-empty deferred claims request ServiceThread cleanup.
                self.record_needs_cleanup();
            }
            log::trace!(
                target: "oopstorage",
                "{}: deferred update {:p}",
                self.name(),
                block
            );
        }
    }

    pub fn allocation_status(&self, ptr: *const Value<'static>) -> EntryStatus {
        if ptr.is_null() {
            return EntryStatus::Invalid;
        }
        let block = self.block_for_ptr(ptr);
        if block.is_null() {
            return EntryStatus::Invalid;
        }
        let _guard = self.allocation_mutex.lock();
        let index = Block::active_index_safe(block);
        let array = self.active_array_relaxed();
        let array_ref = unsafe { &*array };
        if index < array_ref.block_count()
            && array_ref.at(index) == block
            && unsafe { (*block).contains(ptr) }
        {
            let block_ref = unsafe { &*block };
            if block_ref.allocated_bitmask() & block_ref.bitmask_for_entry(ptr) != 0 {
                EntryStatus::Allocated
            } else {
                EntryStatus::Unallocated
            }
        } else {
            EntryStatus::Invalid
        }
    }

    pub fn block_for_ptr(&self, ptr: *const Value<'static>) -> *mut Block {
        Block::block_for_ptr(self as *const Self as *const (), ptr)
    }

    #[inline]
    unsafe fn allocation_list_mut(&self) -> &mut AllocationList {
        // SAFETY: caller holds allocation_mutex.
        unsafe { &mut *self.allocation_list.get() }
    }

    #[inline]
    unsafe fn allocation_list(&self) -> &AllocationList {
        unsafe { &*self.allocation_list.get() }
    }

    fn log_block_transition(&self, block: *mut Block, new_state: &str) {
        log::trace!(
            target: "oopstorage",
            "{}: block {} {:p}",
            self.name(),
            new_state,
            block
        );
    }

    fn active_array_relaxed(&self) -> *mut ActiveArray {
        self.active_array.load(Ordering::Relaxed)
    }

    fn try_add_block(&self, guard: &mut MutexGuard<'_, ()>) -> bool {
        let block = MutexGuard::unlocked(guard, || unsafe {
            Block::new_block(self as *const Self as *const ())
        });
        let Some(block_nn) = block else {
            return false;
        };
        let block = block_nn.as_ptr();

        let array = self.active_array_relaxed();
        let pushed = unsafe { (*array).push(block) };
        if !pushed {
            if self.expand_active_array() {
                let array = self.active_array_relaxed();
                assert!(
                    unsafe { (*array).push(block) },
                    "push failed after expansion"
                );
            } else {
                log::debug!(target: "oopstorage", "{}: failed active array expand", self.name());
                unsafe { Block::delete_block(block_nn) };
                return false;
            }
        }

        unsafe { self.allocation_list_mut().push_back(&*block) };
        log::debug!(target: "oopstorage", "{}: new block {:p}", self.name(), block);
        true
    }

    fn block_for_allocation(&self, guard: &mut MutexGuard<'_, ()>) -> Option<*mut Block> {
        loop {
            let block = unsafe { self.allocation_list().head() };
            if !block.is_null() {
                return Some(block);
            } else if self.reduce_deferred_updates() {
                // Might have added a block; retry.
            } else if self.try_add_block(guard) {
                assert!(!unsafe { self.allocation_list().chead() }.is_null());
            } else if !unsafe { self.allocation_list().chead() }.is_null() {
                // Another path added while unlocked over new_block.
            } else if !self.reduce_deferred_updates() {
                log::info!(
                    target: "oopstorage",
                    "{}: failed block allocation",
                    self.name()
                );
                return None;
            }
        }
    }

    fn expand_active_array(&self) -> bool {
        let old_array = self.active_array_relaxed();
        let new_size = unsafe { (*old_array).size() * 2 };
        log::debug!(
            target: "oopstorage",
            "{}: expand active array {}",
            self.name(),
            new_size
        );
        let Some(mut new_array) = ActiveArray::create(new_size) else {
            return false;
        };
        unsafe { new_array.copy_from(&*old_array) };
        let new_ptr = Box::into_raw(new_array);
        self.replace_active_array(new_ptr);
        self.relinquish_block_array(old_array);
        true
    }

    fn replace_active_array(&self, new_array: *mut ActiveArray) {
        unsafe { (*new_array).increment_refcount() };
        self.active_array.store(new_array, Ordering::Release);
        self.protect_active.synchronize();
    }

    pub(crate) fn obtain_active_array(&self) -> *mut ActiveArray {
        let _cs = CriticalSection::new(&self.protect_active);
        let result = self.active_array.load(Ordering::Acquire);
        unsafe { (*result).increment_refcount() };
        result
    }

    pub(crate) fn relinquish_block_array(&self, array: *mut ActiveArray) {
        if unsafe { (*array).decrement_refcount() } {
            debug_assert!(array != self.active_array_relaxed());
            let _ = unsafe { Box::from_raw(array) };
        }
    }

    /// Process one deferred update. Returns true if one was processed.
    ///
    /// # Precondition
    /// `allocation_mutex` is held.
    fn reduce_deferred_updates(&self) -> bool {
        let mut block = self.deferred_updates.load(Ordering::Acquire);
        loop {
            if block.is_null() {
                return false;
            }
            let mut tail = unsafe { (*block).deferred_updates_next() };
            if block == tail {
                tail = ptr::null_mut();
            }
            match self.deferred_updates.compare_exchange_weak(
                block,
                tail,
                Ordering::Relaxed,
                Ordering::Acquire,
            ) {
                Ok(_) => break,
                Err(fetched) => block = fetched,
            }
        }
        unsafe { (*block).set_deferred_updates_next(ptr::null_mut()) };
        std::sync::atomic::fence(Ordering::SeqCst);

        let block_ref = unsafe { &*block };
        let allocated = block_ref.allocated_bitmask();
        let list = unsafe { self.allocation_list_mut() };
        if is_full_bitmask(allocated) {
            debug_assert!(!unsafe { list.contains(block_ref) });
        } else if unsafe { list.contains(block_ref) } {
            if is_empty_bitmask(allocated) {
                unsafe {
                    list.unlink(block_ref);
                    list.push_back(block_ref);
                }
            }
        } else if is_empty_bitmask(allocated) {
            unsafe { list.push_back(block_ref) };
        } else {
            unsafe { list.push_front(block_ref) };
        }

        log::trace!(
            target: "oopstorage",
            "{}: processed deferred update {:p}",
            self.name(),
            block
        );
        true
    }

    /// Delete empty blocks / drain deferred updates.
    pub fn delete_empty_blocks(&self) -> bool {
        if !self.needs_cleanup.load(Ordering::Acquire)
            && self.deferred_updates.load(Ordering::Acquire).is_null()
        {
            return false;
        }

        let mut guard = self.allocation_mutex.lock();
        self.needs_cleanup.store(false, Ordering::SeqCst);

        let limit = unsafe { (*self.active_array_relaxed()).block_count() } + 10;
        for _ in 0..limit {
            if self.reduce_deferred_updates() {
                MutexGuard::unlocked(&mut guard, || {
                    // Safepoint politeness (HotSpot ThreadBlockInVM).
                });
                continue;
            }
            let block = unsafe { self.allocation_list().tail() };
            if block.is_null() || !unsafe { (*block).is_empty() } {
                return false;
            } else if !unsafe { (*block).is_safe_to_delete() } {
                break;
            }

            {
                let _active = self.active_mutex.lock();
                if *self.concurrent_iteration_count.lock() > 0 {
                    return true;
                }
                let array = self.active_array_relaxed();
                unsafe { (*array).remove(block) };
            }

            unsafe { self.allocation_list_mut().unlink(&*block) };
            log::debug!(
                target: "oopstorage",
                "{}: delete empty block {:p}",
                self.name(),
                block
            );
            MutexGuard::unlocked(&mut guard, || {
                unsafe { Block::delete_block(NonNull::new_unchecked(block)) };
            });
        }

        self.record_needs_cleanup();
        true
    }

    pub(crate) fn update_concurrent_iteration_count(&self, value: i32) {
        let _active = self.active_mutex.lock();
        let mut count = self.concurrent_iteration_count.lock();
        *count += value;
        debug_assert!(*count >= 0);
    }

    /// Safepoint serial iteration over allocated entries.
    pub fn iterate_safepoint<F>(&self, mut f: F) -> bool
    where
        F: FnMut(*mut Value<'static>) -> bool,
    {
        let array = self.active_array_relaxed();
        let limit = unsafe { (*array).block_count() };
        for i in 0..limit {
            let block = unsafe { (*array).at(i) };
            if !unsafe { (*block).iterate(|p| f(p)) } {
                return false;
            }
        }
        true
    }

    pub fn oops_do<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        let _ = self.iterate_safepoint(|p| {
            f(p);
            true
        });
    }

    /// Weak iteration: skip empty slots; apply `f` to others.
    pub fn weak_oops_do<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        let _ = self.iterate_safepoint(|p| {
            if !unsafe { (*p).is_empty() } {
                f(p);
            }
            true
        });
    }

    /// Weak iteration with is-alive: clear dead referents to empty.
    pub fn weak_oops_do_if_alive<A, F>(&self, mut is_alive: A, mut f: F)
    where
        A: FnMut(Value<'static>) -> bool,
        F: FnMut(*mut Value<'static>),
    {
        let _ = self.iterate_safepoint(|p| {
            let v = unsafe { *p };
            if !v.is_empty() {
                if is_alive(v) {
                    f(p);
                } else {
                    unsafe { *p = Value::empty() };
                }
            }
            true
        });
    }

    /// Scan as precise MMTk roots (strong).
    pub fn scan_roots(&self, factory: &mut impl RootsWorkFactory<ObjectSlot>) {
        let mut tracer = RootSlotVisitor::new();
        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(&mut tracer), None) };
        self.oops_do(|ptr| {
            unsafe {
                (*ptr).trace(&mut visitor);
            }
        });
        if !tracer.set.is_empty() {
            factory.create_process_roots_work(tracer.set.into_iter().collect());
        }
    }

    /// Weak scan: clear dead cells; report num_dead.
    pub fn scan_weak_roots<A>(&self, mut is_alive: A, factory: &mut impl RootsWorkFactory<ObjectSlot>)
    where
        A: FnMut(Value<'static>) -> bool,
    {
        let mut tracer = RootSlotVisitor::new();
        let mut visitor = unsafe { Visitor::new(VisitorKind::Slot(&mut tracer), None) };
        let mut num_dead = 0usize;
        let _ = self.iterate_safepoint(|ptr| {
            let v = unsafe { *ptr };
            if v.is_empty() {
                num_dead += 1;
            } else if is_alive(v) {
                unsafe { (*ptr).trace(&mut visitor) };
            } else {
                unsafe { *ptr = Value::empty() };
                num_dead += 1;
            }
            true
        });
        self.report_num_dead(num_dead);
        if !tracer.set.is_empty() {
            factory.create_process_roots_work(tracer.set.into_iter().collect());
        }
    }

    pub fn print_containing(&self, addr: *const Value<'static>) -> Option<String> {
        if addr.is_null() {
            return None;
        }
        let block = self.block_for_ptr(addr);
        if block.is_null() {
            return None;
        }
        unsafe { (*block).print_containing(addr) }
            .map(|s| format!("{s} in oop storage \"{}\"", self.name()))
    }
}

impl Drop for OopStorage {
    fn drop(&mut self) {
        let mut block = self.deferred_updates.load(Ordering::Relaxed);
        while !block.is_null() {
            let next = unsafe { (*block).deferred_updates_next() };
            let next = if next == block {
                ptr::null_mut()
            } else {
                next
            };
            unsafe { (*block).set_deferred_updates_next(ptr::null_mut()) };
            self.deferred_updates.store(next, Ordering::Relaxed);
            block = next;
        }

        // SAFETY: exclusive access in Drop.
        let list = unsafe { &mut *self.allocation_list.get() };
        while !list.head().is_null() {
            let b = list.head();
            unsafe { list.unlink(&*b) };
        }

        let array = self.active_array_relaxed();
        let unreferenced = unsafe { (*array).decrement_refcount() };
        debug_assert!(unreferenced);
        let count = unsafe { (*array).block_count() };
        for i in (0..count).rev() {
            let b = unsafe { (*array).at(i) };
            unsafe { Block::delete_block(NonNull::new_unchecked(b)) };
        }
        let _ = unsafe { Box::from_raw(array) };
    }
}

/// RAII obtain/relinquish of the active array.
pub(crate) struct WithActiveArray<'a> {
    storage: &'a OopStorage,
    active_array: *mut ActiveArray,
}

impl<'a> WithActiveArray<'a> {
    pub fn new(storage: &'a OopStorage) -> Self {
        let active_array = storage.obtain_active_array();
        Self {
            storage,
            active_array,
        }
    }

    pub fn active_array(&self) -> &ActiveArray {
        unsafe { &*self.active_array }
    }
}

impl Drop for WithActiveArray<'_> {
    fn drop(&mut self) {
        self.storage.relinquish_block_array(self.active_array);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rsgc::oop_storage::block::BULK_ALLOCATE_LIMIT;
    use crate::rsgc::oop_storage::par_state::ParState;

    #[test]
    fn allocate_store_release() {
        let storage = OopStorage::create("test");
        let ptr = storage.allocate().unwrap();
        unsafe {
            *ptr = Value::from_raw_i64(Value::VALUE_NULL);
            *ptr = Value::empty();
            storage.release(ptr);
        }
        assert_eq!(storage.allocation_count(), 0);
    }

    #[test]
    fn bulk_allocate_and_release() {
        let storage = OopStorage::create("bulk");
        let mut ptrs = [ptr::null_mut(); BULK_ALLOCATE_LIMIT];
        let n = storage.allocate_bulk(&mut ptrs);
        assert!(n > 0);
        assert_eq!(storage.allocation_count(), n);
        for p in &ptrs[..n] {
            unsafe {
                **p = Value::empty();
            }
        }
        unsafe {
            storage.release_bulk(&ptrs[..n]);
        }
        assert_eq!(storage.allocation_count(), 0);
    }

    #[test]
    fn many_allocates_and_cleanup() {
        let storage = OopStorage::create("many");
        let mut ptrs = Vec::new();
        for _ in 0..100 {
            let p = storage.allocate().unwrap();
            unsafe {
                *p = Value::null();
            }
            ptrs.push(p);
        }
        assert_eq!(storage.allocation_count(), 100);
        for p in &ptrs {
            unsafe {
                **p = Value::empty();
                storage.release(*p);
            }
        }
        assert_eq!(storage.allocation_count(), 0);
        // Drain deferred updates / empty blocks.
        while storage.delete_empty_blocks() {}
    }

    #[test]
    fn par_state_iterate() {
        let storage = OopStorage::create("par");
        let mut ptrs = Vec::new();
        for _ in 0..10 {
            let p = storage.allocate().unwrap();
            unsafe {
                *p = Value::null();
            }
            ptrs.push(p);
        }
        let mut seen = 0usize;
        let state = ParState::<false, false>::new(&storage);
        state.iterate(|_| seen += 1);
        assert_eq!(seen, 10);
        for p in ptrs {
            unsafe {
                *p = Value::empty();
                storage.release(p);
            }
        }
    }

    #[test]
    fn allocation_status_roundtrip() {
        let storage = OopStorage::create("status");
        let ptr = storage.allocate().unwrap();
        assert_eq!(
            storage.allocation_status(ptr),
            EntryStatus::Allocated
        );
        unsafe {
            *ptr = Value::empty();
            storage.release(ptr);
        }
        assert_eq!(
            storage.allocation_status(ptr),
            EntryStatus::Unallocated
        );
    }
}

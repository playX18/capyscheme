//! Block, ActiveArray, and AllocationList — HotSpot oopStorage internals.

use std::alloc::{Layout, alloc, dealloc};
use std::cell::UnsafeCell;
use std::mem::{align_of, offset_of, size_of};
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicPtr, AtomicU64, AtomicUsize, Ordering};

use crate::runtime::value::Value;

/// Number of Value slots per block (`BitsPerWord`).
pub const SLOTS_PER_BLOCK: usize = usize::BITS as usize;

/// Maximum bulk allocate request size (`bulk_allocate_limit`).
pub const BULK_ALLOCATE_LIMIT: usize = SLOTS_PER_BLOCK;

/// `BitsPerByte` section size used for block alignment / `block_for_ptr`.
const SECTION_SIZE: usize = 8;
/// `BytesPerWord` section count.
const SECTION_COUNT: usize = size_of::<usize>();
/// Block alignment: `sizeof(oop) * section_size`.
pub const BLOCK_ALIGNMENT: usize = size_of::<Value>() * SECTION_SIZE;

const _: () = assert!(SECTION_SIZE * SECTION_COUNT == SLOTS_PER_BLOCK);
const _: () = assert!(size_of::<Value>() == 8);

#[inline]
pub fn is_full_bitmask(bitmask: u64) -> bool {
    !bitmask == 0
}

#[inline]
pub fn is_empty_bitmask(bitmask: u64) -> bool {
    bitmask == 0
}

/// Intrusive allocation-list links embedded in each [`Block`].
pub struct AllocationListEntry {
    prev: UnsafeCell<*const Block>,
    next: UnsafeCell<*const Block>,
}

// SAFETY: links are only mutated while the owner's allocation mutex is held.
unsafe impl Sync for AllocationListEntry {}

impl AllocationListEntry {
    fn new() -> Self {
        Self {
            prev: UnsafeCell::new(ptr::null()),
            next: UnsafeCell::new(ptr::null()),
        }
    }

    #[inline]
    pub(crate) unsafe fn prev(&self) -> *const Block {
        // SAFETY: caller holds allocation mutex or is reading under that protocol.
        unsafe { *self.prev.get() }
    }

    #[inline]
    pub(crate) unsafe fn next(&self) -> *const Block {
        unsafe { *self.next.get() }
    }

    #[inline]
    pub(crate) unsafe fn set_prev(&self, p: *const Block) {
        unsafe { *self.prev.get() = p }
    }

    #[inline]
    pub(crate) unsafe fn set_next(&self, p: *const Block) {
        unsafe { *self.next.get() = p }
    }
}

/// Doubly-linked list of blocks with free (or empty) entries.
pub struct AllocationList {
    head: *const Block,
    tail: *const Block,
}

// SAFETY: only accessed under OopStorage's allocation mutex.
unsafe impl Send for AllocationList {}
unsafe impl Sync for AllocationList {}

impl AllocationList {
    pub fn new() -> Self {
        Self {
            head: ptr::null(),
            tail: ptr::null(),
        }
    }

    #[inline]
    pub fn head(&self) -> *mut Block {
        self.head as *mut Block
    }

    #[inline]
    pub fn tail(&self) -> *mut Block {
        self.tail as *mut Block
    }

    #[inline]
    pub fn chead(&self) -> *const Block {
        self.head
    }

    #[inline]
    pub fn ctail(&self) -> *const Block {
        self.tail
    }

    pub unsafe fn prev(&self, block: &Block) -> *mut Block {
        unsafe { block.allocation_list_entry().prev() as *mut Block }
    }

    pub unsafe fn next(&self, block: &Block) -> *mut Block {
        unsafe { block.allocation_list_entry().next() as *mut Block }
    }

    pub unsafe fn push_front(&mut self, block: &Block) {
        let block_ptr = block as *const Block;
        let old = self.head;
        if old.is_null() {
            debug_assert!(self.tail.is_null());
            self.head = block_ptr;
            self.tail = block_ptr;
        } else {
            unsafe {
                block.allocation_list_entry().set_next(old);
                (*old).allocation_list_entry().set_prev(block_ptr);
            }
            self.head = block_ptr;
        }
    }

    pub unsafe fn push_back(&mut self, block: &Block) {
        let block_ptr = block as *const Block;
        let old = self.tail;
        if old.is_null() {
            debug_assert!(self.head.is_null());
            self.head = block_ptr;
            self.tail = block_ptr;
        } else {
            unsafe {
                (*old).allocation_list_entry().set_next(block_ptr);
                block.allocation_list_entry().set_prev(old);
            }
            self.tail = block_ptr;
        }
    }

    pub unsafe fn unlink(&mut self, block: &Block) {
        let block_ptr = block as *const Block;
        let entry = block.allocation_list_entry();
        let prev_blk = unsafe { entry.prev() };
        let next_blk = unsafe { entry.next() };
        unsafe {
            entry.set_prev(ptr::null());
            entry.set_next(ptr::null());
        }
        if prev_blk.is_null() && next_blk.is_null() {
            debug_assert!(self.head == block_ptr);
            debug_assert!(self.tail == block_ptr);
            self.head = ptr::null();
            self.tail = ptr::null();
        } else if prev_blk.is_null() {
            debug_assert!(self.head == block_ptr);
            unsafe { (*next_blk).allocation_list_entry().set_prev(ptr::null()) };
            self.head = next_blk;
        } else if next_blk.is_null() {
            debug_assert!(self.tail == block_ptr);
            unsafe { (*prev_blk).allocation_list_entry().set_next(ptr::null()) };
            self.tail = prev_blk;
        } else {
            unsafe {
                (*next_blk).allocation_list_entry().set_prev(prev_blk);
                (*prev_blk).allocation_list_entry().set_next(next_blk);
            }
        }
    }

    pub unsafe fn contains(&self, block: &Block) -> bool {
        let block_ptr = block as *const Block;
        unsafe { !self.next(block).is_null() || self.ctail() == block_ptr }
    }
}

impl Drop for AllocationList {
    fn drop(&mut self) {
        debug_assert!(self.head.is_null());
        debug_assert!(self.tail.is_null());
    }
}

/// Refcounted array of active block pointers (HotSpot `ActiveArray`).
pub struct ActiveArray {
    size: usize,
    block_count: AtomicUsize,
    refcount: AtomicUsize,
    /// Length == `size`; only the first `block_count` entries are live.
    blocks: Box<[*mut Block]>,
}

impl ActiveArray {
    pub fn create(size: usize) -> Option<Box<Self>> {
        let blocks = vec![ptr::null_mut(); size].into_boxed_slice();
        Some(Box::new(Self {
            size,
            block_count: AtomicUsize::new(0),
            refcount: AtomicUsize::new(0),
            blocks,
        }))
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }

    #[inline]
    pub fn block_count(&self) -> usize {
        self.block_count.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn block_count_acquire(&self) -> usize {
        self.block_count.load(Ordering::Acquire)
    }

    pub fn increment_refcount(&self) {
        let old = self.refcount.fetch_add(1, Ordering::Relaxed);
        debug_assert!(old < usize::MAX / 2);
    }

    /// Returns true if the refcount reached zero.
    pub fn decrement_refcount(&self) -> bool {
        let prev = self.refcount.fetch_sub(1, Ordering::Release);
        debug_assert!(prev > 0);
        prev == 1
    }

    #[inline]
    pub fn at(&self, index: usize) -> *mut Block {
        debug_assert!(index < self.block_count());
        self.blocks[index]
    }

    /// Add block to the end. Returns false if full.
    pub fn push(&mut self, block: *mut Block) -> bool {
        let index = self.block_count.load(Ordering::Relaxed);
        if index < self.size {
            // SAFETY: block is a live Block owned by the storage.
            unsafe { (*block).set_active_index(index) };
            self.blocks[index] = block;
            self.block_count.store(index + 1, Ordering::Release);
            true
        } else {
            false
        }
    }

    pub fn remove(&mut self, block: *mut Block) {
        let count = self.block_count.load(Ordering::Relaxed);
        debug_assert!(count > 0);
        // SAFETY: block is in this array.
        let index = unsafe { (*block).active_index() };
        debug_assert!(self.blocks[index] == block);
        let last_index = count - 1;
        let last_block = self.blocks[last_index];
        unsafe { (*last_block).set_active_index(index) };
        self.blocks[index] = last_block;
        self.block_count.store(last_index, Ordering::Relaxed);
    }

    pub fn copy_from(&mut self, from: &ActiveArray) {
        debug_assert_eq!(self.block_count.load(Ordering::Relaxed), 0);
        let count = from.block_count.load(Ordering::Relaxed);
        debug_assert!(count <= self.size);
        for i in 0..count {
            let block = from.blocks[i];
            debug_assert_eq!(unsafe { (*block).active_index() }, i);
            self.blocks[i] = block;
        }
        self.block_count.store(count, Ordering::Relaxed);
    }
}

/// Fixed-size array of Values plus bookkeeping (HotSpot `OopStorage::Block`).
///
/// `_data` / `data` must be the first field so aligning the block aligns slots.
#[repr(C)]
pub struct Block {
    data: [Value<'static>; SLOTS_PER_BLOCK],
    allocated_bitmask: AtomicU64,
    owner_address: usize,
    /// Unaligned allocation base passed to `dealloc`.
    memory: *mut u8,
    active_index: UnsafeCell<usize>,
    allocation_list_entry: AllocationListEntry,
    deferred_updates_next: AtomicPtr<Block>,
    release_refcount: AtomicU64,
}

// SAFETY: Block is shared across threads with atomics / mutex protocol.
unsafe impl Send for Block {}
unsafe impl Sync for Block {}

impl Block {
    const fn data_offset() -> usize {
        0
    }

    pub fn allocation_size() -> usize {
        // data must be first: aligning Block aligns data.
        debug_assert_eq!(Self::data_offset(), offset_of!(Block, data));
        size_of::<Block>() + BLOCK_ALIGNMENT - size_of::<*mut u8>()
    }

    pub fn allocation_alignment_shift() -> u32 {
        BLOCK_ALIGNMENT.trailing_zeros()
    }

    pub unsafe fn new_block(owner: *const ()) -> Option<NonNull<Block>> {
        let size_needed = Self::allocation_size();
        let layout = Layout::from_size_align(size_needed, align_of::<u8>()).ok()?;
        // SAFETY: layout is non-zero.
        let memory = unsafe { alloc(layout) };
        if memory.is_null() {
            return None;
        }
        let block_addr = align_up(memory as usize, BLOCK_ALIGNMENT);
        debug_assert!(block_addr + size_of::<Block>() <= memory as usize + size_needed);

        let block_ptr = block_addr as *mut Block;
        // SAFETY: memory is large enough for an aligned Block.
        unsafe {
            ptr::write(
                block_ptr,
                Block {
                    data: [Value::empty(); SLOTS_PER_BLOCK],
                    allocated_bitmask: AtomicU64::new(0),
                    owner_address: owner as usize,
                    memory,
                    active_index: UnsafeCell::new(0),
                    allocation_list_entry: AllocationListEntry::new(),
                    deferred_updates_next: AtomicPtr::new(ptr::null_mut()),
                    release_refcount: AtomicU64::new(0),
                },
            );
        }
        debug_assert_eq!(block_addr % BLOCK_ALIGNMENT, 0);
        NonNull::new(block_ptr)
    }

    pub unsafe fn delete_block(block: NonNull<Block>) {
        let memory = unsafe { block.as_ref().memory };
        let size_needed = Self::allocation_size();
        let layout = Layout::from_size_align(size_needed, align_of::<u8>()).expect("valid layout");
        unsafe {
            ptr::drop_in_place(block.as_ptr());
            dealloc(memory, layout);
        }
    }

    #[inline]
    pub fn allocation_list_entry(&self) -> &AllocationListEntry {
        &self.allocation_list_entry
    }

    #[inline]
    pub fn allocated_bitmask(&self) -> u64 {
        self.allocated_bitmask.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn is_full(&self) -> bool {
        is_full_bitmask(self.allocated_bitmask())
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        is_empty_bitmask(self.allocated_bitmask())
    }

    #[inline]
    pub fn bitmask_for_index(&self, index: u32) -> u64 {
        debug_assert!((index as usize) < SLOTS_PER_BLOCK);
        1u64 << index
    }

    #[inline]
    pub fn bitmask_for_entry(&self, ptr: *const Value<'static>) -> u64 {
        self.bitmask_for_index(self.get_index(ptr))
    }

    #[inline]
    pub fn get_pointer(&self, index: u32) -> *mut Value<'static> {
        debug_assert!((index as usize) < SLOTS_PER_BLOCK);
        // SAFETY: index in range; data is the slot array.
        unsafe { (self.data.as_ptr() as *mut Value<'static>).add(index as usize) }
    }

    #[inline]
    pub fn contains(&self, ptr: *const Value<'static>) -> bool {
        let base = self.data.as_ptr();
        let end = unsafe { base.add(SLOTS_PER_BLOCK) };
        ptr >= base && ptr < end
    }

    #[inline]
    pub fn get_index(&self, ptr: *const Value<'static>) -> u32 {
        debug_assert!(self.contains(ptr));
        ((ptr as usize) - (self.data.as_ptr() as usize)) as u32 / size_of::<Value>() as u32
    }

    #[inline]
    pub fn active_index(&self) -> usize {
        // SAFETY: active_index is only written under allocation/active mutex protocols.
        unsafe { *self.active_index.get() }
    }

    #[inline]
    pub fn set_active_index(&self, index: usize) {
        unsafe { *self.active_index.get() = index }
    }

    /// Best-effort read of active_index (HotSpot `active_index_safe`).
    pub fn active_index_safe(block: *const Block) -> usize {
        if block.is_null() {
            return 0;
        }
        // SAFETY: caller may have a false-positive block pointer; we only read
        // a usize-sized field. Unlike HotSpot SafeFetchN we assume the address
        // is mapped when used from allocation_status after validation paths.
        let index_ptr =
            (block as usize + offset_of!(Block, active_index)) as *const UnsafeCell<usize>;
        unsafe { *(*index_ptr).get() }
    }

    pub fn is_safe_to_delete(&self) -> bool {
        debug_assert!(self.is_empty());
        std::sync::atomic::fence(Ordering::Acquire);
        self.release_refcount.load(Ordering::Acquire) == 0
            && self.deferred_updates_next.load(Ordering::Acquire).is_null()
    }

    pub fn deferred_updates_next(&self) -> *mut Block {
        self.deferred_updates_next.load(Ordering::Relaxed)
    }

    pub fn set_deferred_updates_next(&self, block: *mut Block) {
        self.deferred_updates_next.store(block, Ordering::Relaxed)
    }

    /// Merge new allocation bits. Only one thread allocates from a block at a time,
    /// but releasers may clear bits concurrently.
    pub fn atomic_add_allocated(&self, add: u64) {
        let sum = self.allocated_bitmask.fetch_add(add, Ordering::Relaxed) + add;
        debug_assert_eq!(sum & add, add, "some bits already present");
    }

    pub fn allocate(&self) -> *mut Value<'static> {
        let allocated = self.allocated_bitmask();
        debug_assert!(!is_full_bitmask(allocated));
        let index = (!allocated).trailing_zeros();
        self.atomic_add_allocated(self.bitmask_for_index(index));
        self.get_pointer(index)
    }

    /// Allocate all remaining free entries; returns the bitmask of newly taken bits.
    pub fn allocate_all(&self) -> u64 {
        let new_allocated = !self.allocated_bitmask();
        debug_assert_ne!(new_allocated, 0);
        self.atomic_add_allocated(new_allocated);
        new_allocated
    }

    /// Map a slot pointer to its containing block (may false-positive).
    pub fn block_for_ptr(owner: *const (), ptr: *const Value<'static>) -> *mut Block {
        debug_assert!(!ptr.is_null());
        let section_start = align_down(ptr as usize, BLOCK_ALIGNMENT);
        let section_size_in_bytes = size_of::<Value>() * SECTION_SIZE;
        let mut section = section_start - (section_size_in_bytes * (SECTION_COUNT - 1));
        let owner_addr = owner as usize;
        for _ in 0..SECTION_COUNT {
            let owner_loc = section + offset_of!(Block, owner_address);
            // SAFETY: probing candidate block starts; false positives read unrelated words.
            let fetched = unsafe { ptr::read(owner_loc as *const usize) };
            if fetched == owner_addr {
                return section as *mut Block;
            }
            section += section_size_in_bytes;
        }
        ptr::null_mut()
    }

    /// Lock-free release of the bits in `releasing`.
    ///
    /// Returns `(transitioned_to_empty, claimed_deferred)` so the owner can
    /// record cleanup / logging (HotSpot `Block::release_entries`).
    pub fn release_entries(
        &self,
        releasing: u64,
        deferred_updates: &AtomicPtr<Block>,
    ) -> ReleaseTransition {
        debug_assert_ne!(releasing, 0);
        self.release_refcount.fetch_add(1, Ordering::Relaxed);

        let mut old_allocated = self.allocated_bitmask.load(Ordering::Relaxed);
        loop {
            debug_assert_eq!(
                releasing & !old_allocated,
                0,
                "releasing unallocated entries"
            );
            let new_value = old_allocated ^ releasing;
            match self.allocated_bitmask.compare_exchange_weak(
                old_allocated,
                new_value,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(fetched) => old_allocated = fetched,
            }
        }

        let to_empty = releasing == old_allocated;
        let from_full = is_full_bitmask(old_allocated);
        let mut claimed_deferred = false;

        if to_empty || from_full {
            if self
                .deferred_updates_next
                .compare_exchange(
                    ptr::null_mut(),
                    self as *const Block as *mut Block,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .is_ok()
            {
                let self_ptr = self as *const Block as *mut Block;
                let mut head = deferred_updates.load(Ordering::Relaxed);
                loop {
                    let next = if head.is_null() { self_ptr } else { head };
                    self.deferred_updates_next.store(next, Ordering::Relaxed);
                    match deferred_updates.compare_exchange_weak(
                        head,
                        self_ptr,
                        Ordering::Relaxed,
                        Ordering::Relaxed,
                    ) {
                        Ok(_) => break,
                        Err(fetched) => head = fetched,
                    }
                }
                claimed_deferred = true;
            }
        }

        self.release_refcount.fetch_sub(1, Ordering::Relaxed);
        ReleaseTransition {
            to_empty,
            from_full,
            claimed_deferred,
        }
    }

    pub fn iterate<F>(&self, mut f: F) -> bool
    where
        F: FnMut(*mut Value<'static>) -> bool,
    {
        let mut bitmask = self.allocated_bitmask();
        while bitmask != 0 {
            let index = bitmask.trailing_zeros();
            bitmask ^= self.bitmask_for_index(index);
            if !f(self.get_pointer(index)) {
                return false;
            }
        }
        true
    }

    pub fn print_containing(&self, addr: *const Value<'static>) -> Option<String> {
        if self.contains(addr) {
            Some(format!(
                "{:p} is a pointer {}/{} into block {}",
                addr,
                self.get_index(addr),
                SLOTS_PER_BLOCK,
                self.active_index()
            ))
        } else {
            None
        }
    }
}

/// Result of [`Block::release_entries`].
#[derive(Debug, Clone, Copy)]
pub struct ReleaseTransition {
    pub to_empty: bool,
    pub from_full: bool,
    pub claimed_deferred: bool,
}

#[inline]
fn align_up(addr: usize, align: usize) -> usize {
    (addr + (align - 1)) & !(align - 1)
}

#[inline]
fn align_down(addr: usize, align: usize) -> usize {
    addr & !(align - 1)
}

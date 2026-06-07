use crate::{
    rsgc::{
        ObjectSlot,
        finalizer::Finalizers,
        mm::MemoryManager,
        object::{AllocationHooksOf, GCObject, HeapObjectHeader, OBJECT_REF_OFFSET},
        ptr::Gc,
        sync::thread::{AllocFastPath, Thread, current_thread, is_current_thread_registed},
        traits::Trace,
    },
    runtime::State,
};
use mmtk::{
    AllocationSemantics, BarrierSelector, MutatorContext,
    util::{
        Address,
        alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
        conversions::raw_align_up,
        metadata::side_metadata::{
            global_side_metadata_vm_base_address, vo_bit_side_metadata_addr,
        },
    },
};
use parking_lot::{Mutex, Once};
use std::{
    alloc::Layout,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::Deref,
    panic::AssertUnwindSafe,
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

pub static GLOBAL_SIDE_METADATA_BASE_ADDRESS: AtomicUsize = AtomicUsize::new(0);
pub static VO_BIT_SIDE_METADATA_BASE_ADDRESS: AtomicUsize = AtomicUsize::new(0);

#[inline(always)]
pub(crate) fn vo_bit_side_metadata_base() -> Address {
    unsafe { Address::from_usize(VO_BIT_SIDE_METADATA_BASE_ADDRESS.load(Ordering::Relaxed)) }
}

/// Set the valid-object (VO) bit for a heap object reference address.
#[inline(always)]
pub(crate) unsafe fn set_vo_bit_for_object_ref(object_ref: Address) {
    let base = vo_bit_side_metadata_base();
    let meta_addr = base + (object_ref.as_usize() >> 6);
    let shift = (object_ref.as_usize() >> 3) & 0b111;
    let byte_val = unsafe { meta_addr.load::<u8>() };
    unsafe {
        meta_addr.store::<u8>(byte_val | (1 << shift));
    }
}

pub trait Rootable<'a> {
    type Root: ?Sized + 'a;
}

#[doc(hidden)]
pub struct __DynRootable<T: ?Sized>(PhantomData<T>);

impl<'a, T: ?Sized + Rootable<'a>> Rootable<'a> for __DynRootable<T> {
    type Root = <T as Rootable<'a>>::Root;
}

#[macro_export]
macro_rules! Rootable {
    ($gc:lifetime => $root: ty) => {
        $crate::__DynRootable<dyn for <$gc> $crate::Rootable<$gc, Root = $root>>
    };

    ($root: ty) => {
        $crate::Rootable!['__gc => $crate::__unelide_lifetimes!('__gc; $root)]
    };
}

const DEFAULT_ALLOC_OPTIONS: AllocationOptions = AllocationOptions {
    allow_oom_call: false,
    allow_overcommit: true,
    at_safepoint: false,
};

pub type Root<'a, R> = <R as Rootable<'a>>::Root;

/// A generic mutator of GC heap.
///
/// Each mutator is associated with a native thread it was created on and each native
/// thread can only have one mutator at a time. When code is running inside [`mutate`]
/// or [`new`] calls, the thread is marked as "in managed" state and access to GC objects is allowed.
///
/// GC may occur at explicit safepoints while inside `mutate`, such as compiler-emitted
/// yieldpoints that call [`Thread::yieldpoint`](crate::rsgc::sync::thread::Thread::yieldpoint).
/// Native code outside compiled safepoints should call [`Mutator::yieldpoint`] when
/// [`take_yieldpoint`](Mutation::take_yieldpoint) indicates a pending GC request.
pub struct Mutator<R>
where
    R: for<'a> Rootable<'a>,
{
    state: NonNull<MutatorState<Root<'static, R>>>,
}

pub(crate) struct MutatorState<R: ?Sized> {
    pub(crate) root: R,
}

static INIT_COLLECTION: Once = Once::new();

impl<R> Mutator<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    pub fn new<F>(f: F) -> Self
    where
        F: for<'gc> FnOnce(Mutation<'gc>) -> Root<'gc, R>,
    {
        let thread = Thread::new(true);

        Self::try_new_with_thread::<_, ()>(thread, |mc| Ok(f(mc))).unwrap()
    }

    pub fn try_new<F, E>(f: F) -> Result<Self, E>
    where
        F: for<'gc> FnOnce(Mutation<'gc>) -> Result<Root<'gc, R>, E>,
    {
        let thread = Thread::new(true);

        Self::try_new_with_thread(thread, f)
    }

    /// Try to construct a new mutator from provided `Thread` handle.
    ///
    /// ## Constraints
    /// - `thread` must not be registered and be a freshly created handle.
    /// - Current thread must not already have `Mutator` instance.
    ///
    /// ## Panics
    ///
    /// Panics if thread is already managed, or current thread is already registered.
    pub fn try_new_with_thread<F, E>(thread: Arc<Thread>, init: F) -> Result<Self, E>
    where
        F: for<'gc> FnOnce(Mutation<'gc>) -> Result<Root<'gc, R>, E>,
    {
        assert!(!thread.is_managed(), "thread is already managed by GC");

        if is_current_thread_registed() {
            panic!("thread is already registered");
        }

        Thread::register(&thread);

        let native_data = unsafe { &mut *thread.native_data_mut_ptr() };
        if native_data.mutator_state.is_some() {
            panic!("Mutator is already registered for thread");
        }

        INIT_COLLECTION.call_once(|| {
            mmtk::memory_manager::initialize_collection(
                &super::GarbageCollector::get().mmtk,
                thread.to_vmthread(),
            );

            GLOBAL_SIDE_METADATA_BASE_ADDRESS.store(
                global_side_metadata_vm_base_address().as_usize(),
                Ordering::Relaxed,
            );
            VO_BIT_SIDE_METADATA_BASE_ADDRESS
                .store(vo_bit_side_metadata_addr().as_usize(), Ordering::Relaxed);

            #[cfg(feature = "bootstrap")]
            {
                crate::rsgc::logging::set_gc_logging_enabled(true);
            }
        });

        let mutation = unsafe { Mutation::new() };
        let static_mutation: Mutation<'_> = unsafe { std::mem::transmute(mutation) };

        let res = std::panic::catch_unwind(AssertUnwindSafe(|| init(static_mutation)));
        Thread::enter_native();

        let init = match res {
            Ok(v) => v,
            Err(e) => {
                Thread::unregister();
                std::panic::resume_unwind(e);
            }
        };
        let root: Root<'static, R> = match init {
            Ok(root) => root,
            Err(err) => {
                Thread::unregister();
                return Err(err);
            }
        };

        let state = Box::leak(Box::new(MutatorState { root }));
        let state_ptr = NonNull::from(state);

        native_data.mutator_state = Some(state_ptr);

        Ok(Self { state: state_ptr })
    }
}

impl<R> Mutator<R>
where
    R: for<'a> Rootable<'a>,
{
    /// Mutate the GC heap. Accepts a callback which receives a handle
    /// to current `Mutation` and a reference to the root, and can return any non GCed value.
    ///
    /// Callback may mutate any part of the GC heap. GC can run at explicit safepoints
    /// while the callback is active (for example compiler-emitted yieldpoints).
    ///
    /// Native code that is not at a safepoint should poll
    /// [`take_yieldpoint()`](Mutation::take_yieldpoint) and call [`Mutator::yieldpoint`]
    /// when a GC request is pending.
    #[inline]
    pub fn mutate<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(Mutation<'gc>, &'gc Root<'gc, R>) -> T,
    {
        unsafe {
            let mc = Mutation::new();
            let mc: Mutation<'static> = std::mem::transmute(mc);
            let state = self.state.as_ptr().as_mut().expect("state must be valid");
            let res = std::panic::catch_unwind(AssertUnwindSafe(|| f(mc, &mut state.root)));
            Thread::enter_native();
            match res {
                Ok(v) => v,
                Err(e) => std::panic::resume_unwind(e),
            }
        }
    }

    pub fn yieldpoint(&self) {
        Thread::leave_native();
        Thread::yieldpoint();
        Thread::enter_native();
    }

    pub fn collect_garbage(&self) -> bool {
        user_collect_garbage()
    }
}

/// Run a user-requested GC collection while the thread is in native code.
///
/// The caller must have already transitioned the thread out of the mutating world
/// (for example via [`Thread::enter_native`](crate::rsgc::sync::thread::Thread::enter_native)).
pub fn user_collect_garbage() -> bool {
    static COLLECT_LOCK: Mutex<()> = Mutex::new(());
    let guard = COLLECT_LOCK.lock();
    Thread::leave_native();
    let did_run = mmtk::memory_manager::handle_user_collection_request(
        &crate::rsgc::GarbageCollector::get().mmtk,
        current_thread().to_mutator_thread(),
    );
    drop(guard);
    Thread::enter_native();

    did_run
}

impl<R> Drop for Mutator<R>
where
    R: for<'a> Rootable<'a>,
{
    fn drop(&mut self) {
        Thread::leave_native();
        let thread = current_thread();
        unsafe { &mut *thread.native_data_mut_ptr() }
            .mutator_state
            .take()
            .expect("must have state");
        //let _ = unsafe { Box::from_raw(self.state.as_ptr()) };
        Thread::unregister();
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Mutation<'gc> {
    thread: &'gc Thread,
}

impl<'gc> Deref for Mutation<'gc> {
    type Target = State<'gc>;

    fn deref(&self) -> &Self::Target {
        debug_assert!(
            self.thread.is_thread_state_initialized(),
            "thread state is not initialized"
        );
        unsafe {
            let state_ptr = self.thread.state_ptr();
            &*(state_ptr as *const State<'gc>)
        }
    }
}

use mmtk::util::alloc::AllocationOptions;

impl<'gc> Mutation<'gc> {
    pub const OFFSET_OF_THREAD: usize = std::mem::offset_of!(Self, thread);

    pub const OFFSET_OF_STATE: usize = Thread::RT_STATE_OFFSET;

    #[inline(always)]
    pub fn state(&self) -> &'gc State<'gc> {
        debug_assert!(
            self.thread.is_thread_state_initialized(),
            "thread state is not initialized"
        );
        unsafe {
            let state_ptr = self.thread.state_ptr();
            &*(state_ptr as *const State<'gc>)
        }
    }

    pub fn as_ptr(self) -> *const () {
        self.thread as *const _ as *const ()
    }

    /// Reconstruct a mutation handle from a raw thread pointer.
    ///
    /// # Safety
    ///
    /// `ptr` must have been produced by [`Mutation::as_ptr`] for a live thread
    /// whose lifetime is valid for the returned mutation handle.
    pub unsafe fn from_ptr(ptr: *const ()) -> Self {
        let thread = unsafe { &*(ptr as *const Thread) };
        Mutation { thread }
    }

    #[inline(always)]
    pub fn init_state(&self, state: State<'gc>) {
        assert!(
            !self.thread.is_thread_state_initialized(),
            "thread state is already initialized"
        );
        unsafe {
            self.thread.initialize_state(state);
        }
    }

    #[inline(always)]
    pub fn thread(&self) -> &'gc Thread {
        self.thread
    }

    /// Returns a value of `take_yieldpoint` for current thread. This value
    /// indicates whether thread should yield or no. Non-zero value indicates
    /// that yield is required, and zero indicates that there is no yield required.
    ///
    /// Usually values greater than zero correspond to "yield always", and lower than zero
    /// "yield on function epilogue". Our GC simply returns `1` when GC is required. Additional
    /// meaning to the value can be assigned by implementing your own blocking adapters.
    #[must_use]
    pub fn take_yieldpoint(&self) -> i32 {
        self.thread.take_yieldpoint()
    }

    pub fn finalizers(&self) -> &'gc Finalizers {
        &crate::rsgc::GarbageCollector::get().finalizers
    }

    pub fn is_movable<T: 'gc + Trace>(&self, obj: Gc<'gc, T>) -> bool {
        unsafe {
            let selector = mmtk::memory_manager::get_allocator_mapping(
                &super::GarbageCollector::get().mmtk,
                AllocationSemantics::NonMoving,
            );

            let allocator = self.thread.mutator_unchecked().allocator(selector);
            allocator.get_space().is_in_space(obj.to_object_reference())
        }
    }

    #[inline(always)]
    pub fn allocate<T: 'gc + Trace>(&self, value: T, semantics: AllocationSemantics) -> Gc<'gc, T> {
        self.allocate_with_header_word(
            value,
            AllocationHooksOf::<'gc, T>::class_header_word((*self).into()),
            semantics,
        )
    }

    #[inline(always)]
    pub fn allocate_with_header_word<T: 'gc>(
        &self,
        value: T,
        header_word: u64,
        semantics: AllocationSemantics,
    ) -> Gc<'gc, T> {
        unsafe {
            let size = size_of::<T>();
            let align = size_of::<usize>().max(align_of::<T>());
            let obj = self.raw_allocate_with_header_word(size, align, header_word, semantics);
            obj.to_address().store(value);

            Gc::from_gcobj(obj)
        }
    }

    /// Flush TLAB and reset it to the initial state. This function must be used before
    /// calling into MMTk API such as
    /// [`raw_allocate_slow_with_header_word`](Self::raw_allocate_slow_with_header_word).
    ///
    /// # Safety
    ///
    /// This function is safe only if TLAB is flushed *before* call into MMTK API such as
    /// [`raw_allocate_slow_with_header_word`](Self::raw_allocate_slow_with_header_word).
    pub unsafe fn flush_tlab(&self) {
        let tlab = unsafe { &mut (*self.thread.native_data_mut_ptr()).lab };

        let (cursor, limit) = tlab.take();

        if cursor.is_zero() {
            debug_assert!(limit.is_zero());
            return;
        }

        let selector = mmtk::memory_manager::get_allocator_mapping(
            &super::GarbageCollector::get().mmtk,
            AllocationSemantics::Default,
        );

        match selector {
            AllocatorSelector::Immix(_) => unsafe {
                let allocator = self
                    .thread
                    .mutator_unchecked()
                    .allocator_impl_mut::<ImmixAllocator<MemoryManager>>(selector);
                allocator.bump_pointer.reset(cursor, limit);
            },

            AllocatorSelector::BumpPointer(_) => unsafe {
                let allocator = self
                    .thread
                    .mutator_unchecked()
                    .allocator_impl_mut::<BumpAllocator<MemoryManager>>(selector);
                allocator.bump_pointer.reset(cursor, limit);
            },

            _ => (),
        }
    }

    /// Refill TLAB with new memory.
    ///
    /// This function must be invoked after slowpath calls like
    /// `raw_allocate_slow_with_header_word`. Otherwise TLAB might contain
    /// invalid data and cause memory corruption.
    ///
    /// # Safety
    ///
    /// This function is safe only if TLAB is refilled *after* call into MMTKA API such as
    /// [`raw_allocate_slow_with_header_word`](Self::raw_allocate_slow_with_header_word).
    pub unsafe fn refill_tlab(&self) {
        let tlab = unsafe { &mut (*self.thread.native_data_mut_ptr()).lab };

        let selector = mmtk::memory_manager::get_allocator_mapping(
            &super::GarbageCollector::get().mmtk,
            AllocationSemantics::Default,
        );

        let allocator = match selector {
            AllocatorSelector::Immix(_) => unsafe {
                let allocator = self
                    .thread
                    .mutator_unchecked()
                    .allocator_impl_mut::<ImmixAllocator<MemoryManager>>(selector);
                &allocator.bump_pointer
            },

            AllocatorSelector::BumpPointer(_) => unsafe {
                let allocator = self
                    .thread
                    .mutator_unchecked()
                    .allocator_impl_mut::<BumpAllocator<MemoryManager>>(selector);
                &allocator.bump_pointer
            },

            _ => return,
        };

        tlab.rebind(allocator.cursor, allocator.limit);
    }

    /// Slow-path raw allocation that installs a precomputed object header word.
    ///
    /// # Safety
    ///
    /// Same as [`raw_allocate`](Self::raw_allocate). `header_word` must encode
    /// the class ID for the object that will be written into the returned
    /// allocation.
    pub unsafe fn raw_allocate_slow_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        semantics: AllocationSemantics,
    ) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                semantics,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            object_start.store(HeapObjectHeader::from_word(header_word));
            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    /// Raw allocation in Immortal Space that installs a precomputed object
    /// header word.
    ///
    /// # Safety
    ///
    /// Same as [`raw_allocate`](Self::raw_allocate). `header_word` must encode
    /// the class ID for the object that will be written into the returned
    /// allocation.
    pub unsafe fn raw_allocate_immortal_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
    ) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::Immortal,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            object_start.store(HeapObjectHeader::from_word(header_word));
            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    /// Raw allocation in Non-Moving Space that installs a precomputed object
    /// header word.
    ///
    /// # Safety
    ///
    /// Same as [`raw_allocate`](Self::raw_allocate). `header_word` must encode
    /// the class ID for the object that will be written into the returned
    /// allocation.
    pub unsafe fn raw_allocate_nonmoving_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
    ) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::NonMoving,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            object_start.store(HeapObjectHeader::from_word(header_word));
            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    /// Raw allocation in Large Object Space that installs a precomputed object
    /// header word.
    ///
    /// # Safety
    ///
    /// Same as [`raw_allocate`](Self::raw_allocate). `header_word` must encode
    /// the class ID for the object that will be written into the returned
    /// allocation.
    pub unsafe fn raw_allocate_los_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
    ) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::Los,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            object_start.store(HeapObjectHeader::from_word(header_word));

            let object = GCObject::from(object_start + OBJECT_REF_OFFSET);

            mmtk::memory_manager::post_alloc(
                self.thread.mutator_unchecked(),
                object.to_objref().unwrap(),
                size,
                AllocationSemantics::Los,
            );

            object
        }
    }

    pub fn allocate_with_layout_header_word<T: 'gc + Trace>(
        &self,
        layout: Layout,
        header_word: u64,
        semantics: AllocationSemantics,
    ) -> Gc<'gc, MaybeUninit<T>> {
        unsafe {
            let size = layout.size();
            let align = layout.align();
            let obj = self.raw_allocate_with_header_word(size, align, header_word, semantics);
            obj.to_address().store(MaybeUninit::<T>::uninit());
            Gc::from_gcobj(obj)
        }
    }

    #[inline(always)]
    /// Allocate raw GC memory and install a precomputed object header word.
    ///
    /// # Safety
    ///
    /// Same as [`raw_allocate`](Self::raw_allocate). `header_word` must encode
    /// the class ID for the object that will be written into the returned
    /// allocation.
    pub unsafe fn raw_allocate_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        mut semantics: AllocationSemantics,
    ) -> GCObject {
        unsafe {
            if size + size_of::<HeapObjectHeader>() >= 8 * 1024 {
                semantics = AllocationSemantics::Los;
            }

            let alignment = alignment.max(std::mem::size_of::<usize>());
            let size = raw_align_up(size, alignment);
            debug_assert!(size.is_multiple_of(alignment));
            if semantics == AllocationSemantics::Default
                && self.thread.alloc_fastpath() == AllocFastPath::TLAB
            {
                let lab = &mut (*self.thread.native_data_mut_ptr()).lab;

                let object_start = lab.allocate(
                    size + size_of::<HeapObjectHeader>(),
                    alignment,
                    OBJECT_REF_OFFSET as _,
                );
                if !object_start.is_zero() {
                    object_start.store(HeapObjectHeader::from_word(header_word));
                    let object_ref = object_start + OBJECT_REF_OFFSET;
                    set_vo_bit_for_object_ref(object_ref);
                    return GCObject::from(object_ref);
                }

                return self.raw_allocate_out_of_line_with_header_word(
                    size,
                    alignment,
                    header_word,
                    semantics,
                );
            }

            self.raw_allocate_out_of_line_with_header_word(size, alignment, header_word, semantics)
        }
    }

    unsafe fn raw_allocate_out_of_line_with_header_word(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        mut semantics: AllocationSemantics,
    ) -> GCObject {
        if semantics == AllocationSemantics::Default
            && size + size_of::<HeapObjectHeader>() >= self.thread.max_non_los_alloc_bytes()
        {
            semantics = AllocationSemantics::Los;
        }

        let size = raw_align_up(size, alignment);
        unsafe {
            match semantics {
                AllocationSemantics::Los => {
                    self.flush_tlab();
                    let object_start = mmtk::memory_manager::alloc_slow_with_options(
                        self.thread.mutator_unchecked(),
                        size + size_of::<HeapObjectHeader>(),
                        alignment,
                        OBJECT_REF_OFFSET as usize,
                        AllocationSemantics::Los,
                        DEFAULT_ALLOC_OPTIONS,
                    );
                    self.refill_tlab();

                    object_start.store(HeapObjectHeader::from_word(header_word));

                    let object = GCObject::from(object_start + OBJECT_REF_OFFSET);

                    mmtk::memory_manager::post_alloc(
                        self.thread.mutator_unchecked(),
                        object.to_objref().unwrap(),
                        size,
                        AllocationSemantics::Los,
                    );

                    object
                }
                semantics => {
                    self.flush_tlab();
                    let object_start = mmtk::memory_manager::alloc_with_options(
                        self.thread.mutator_unchecked(),
                        size + size_of::<HeapObjectHeader>(),
                        alignment,
                        OBJECT_REF_OFFSET as _,
                        semantics,
                        DEFAULT_ALLOC_OPTIONS,
                    );
                    self.refill_tlab();

                    object_start.store(HeapObjectHeader::from_word(header_word));

                    GCObject::from(object_start + OBJECT_REF_OFFSET)
                }
            }
        }
    }

    /// Raw weak reference load barrier.
    ///
    /// # Safety
    ///
    /// May cause UB if not used during a concurrent mark.
    pub unsafe fn raw_weak_reference_load(&self, weak: GCObject) {
        if weak.is_null() {
            return;
        }

        unsafe {
            self.thread
                .mutator_unchecked()
                .barrier()
                .load_weak_reference(weak.to_objref().unwrap());
        }
    }

    /// Raw object reference write post barrier.
    ///
    /// # Safety
    ///
    /// `src` must be a valid GC object. `slot` and `target` are not required
    /// to be valid and can be NULL (`slot` can point to a NULL), but this is
    /// subject to change in the future.
    pub unsafe fn raw_object_reference_write(
        &self,
        src: GCObject,
        slot: ObjectSlot,
        target: GCObject,
    ) {
        match self.thread.barrier() {
            BarrierSelector::ObjectBarrier => unsafe {
                let addr = src.to_address();
                let meta_addr = global_side_metadata_vm_base_address() + (addr >> 6);
                let shift = (addr >> 3) & 0b111;
                let byte_val = meta_addr.load::<u8>();

                if (byte_val >> shift) & 1 == 1 {
                    self.thread
                        .mutator_unchecked()
                        .barrier()
                        .object_reference_write_slow(
                            src.to_objref().unwrap_unchecked(),
                            slot,
                            target.to_objref(),
                        );
                }
            },

            BarrierSelector::SATBBarrier => {
                let addr = src.to_address();
                let meta_addr = global_side_metadata_vm_base_address() + (addr >> 6);
                let shift = (addr >> 3) & 0b111;
                let byte_val = unsafe { meta_addr.load::<u8>() };
                if (byte_val >> shift) & 1 == 1 {
                    unsafe {
                        self.thread
                            .mutator_unchecked()
                            .barrier()
                            .object_reference_write_slow(
                                src.to_objref().unwrap_unchecked(),
                                slot,
                                target.to_objref(),
                            );
                    }
                }
            }

            BarrierSelector::NoBarrier => {}
        }
    }

    /// Allocate raw GC memory without initializing the object header.
    ///
    /// # Safety
    ///
    /// The caller must initialize the returned memory, including any required
    /// header fields, before exposing it to tracing, barriers, or Scheme code.
    pub unsafe fn raw_alloc_uninit(
        &self,
        size: usize,
        alignment: usize,
        mut semantics: AllocationSemantics,
    ) -> GCObject {
        unsafe {
            if size + size_of::<HeapObjectHeader>() >= 8 * 1024 {
                semantics = AllocationSemantics::Los;
            }

            let alignment = alignment.min(8);
            let size = raw_align_up(size, alignment);

            if semantics == AllocationSemantics::Default
                && self.thread.alloc_fastpath() == AllocFastPath::TLAB
            {
                let lab = &mut (*self.thread.native_data_mut_ptr()).lab;

                let object_start = lab.allocate(size + size_of::<HeapObjectHeader>(), alignment, 0);
                if !object_start.is_zero() {
                    let object_ref = object_start + OBJECT_REF_OFFSET;
                    set_vo_bit_for_object_ref(object_ref);
                    return GCObject::from(object_ref);
                }

                return self.raw_allocate_slow_uninit(size, alignment, semantics);
            }

            self.raw_allocate_out_of_line_uninit(size, alignment, semantics)
        }
    }

    /// Slow-path raw allocation without initializing the object header.
    ///
    /// # Safety
    ///
    /// Same as [`raw_alloc_uninit`](Self::raw_alloc_uninit).
    pub unsafe fn raw_allocate_slow_uninit(
        &self,
        size: usize,
        alignment: usize,
        semantics: AllocationSemantics,
    ) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                semantics,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    /// Out-of-line raw allocation without initializing the object header.
    ///
    /// # Safety
    ///
    /// Same as [`raw_alloc_uninit`](Self::raw_alloc_uninit).
    pub unsafe fn raw_allocate_out_of_line_uninit(
        &self,
        size: usize,
        alignment: usize,
        mut semantics: AllocationSemantics,
    ) -> GCObject {
        if semantics == AllocationSemantics::Default
            && size + size_of::<HeapObjectHeader>() >= self.thread.max_non_los_alloc_bytes()
        {
            semantics = AllocationSemantics::Los;
        }

        let size = raw_align_up(size, alignment);
        unsafe {
            match semantics {
                AllocationSemantics::Los => self.raw_allocate_los_uninit(size, alignment),
                AllocationSemantics::Immortal => self.raw_allocate_immortal_uninit(size, alignment),
                AllocationSemantics::NonMoving => {
                    self.raw_allocate_nonmoving_uninit(size, alignment)
                }
                _ => self.raw_allocate_slow_uninit(size, alignment, semantics),
            }
        }
    }

    pub fn raw_allocate_los_uninit(&self, size: usize, alignment: usize) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::Los,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            let object = GCObject::from(object_start + OBJECT_REF_OFFSET);

            mmtk::memory_manager::post_alloc(
                self.thread.mutator_unchecked(),
                object.to_objref().unwrap(),
                size,
                AllocationSemantics::Los,
            );

            object
        }
    }

    /// Allocate immortal raw memory without initializing the object header.
    ///
    /// # Safety
    ///
    /// Same as [`raw_alloc_uninit`](Self::raw_alloc_uninit).
    pub unsafe fn raw_allocate_immortal_uninit(&self, size: usize, alignment: usize) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::Immortal,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    /// Allocate non-moving raw memory without initializing the object header.
    ///
    /// # Safety
    ///
    /// Same as [`raw_alloc_uninit`](Self::raw_alloc_uninit).
    pub unsafe fn raw_allocate_nonmoving_uninit(&self, size: usize, alignment: usize) -> GCObject {
        unsafe {
            self.flush_tlab();
            let object_start = mmtk::memory_manager::alloc_slow_with_options(
                self.thread.mutator_unchecked(),
                size + size_of::<HeapObjectHeader>(),
                alignment,
                OBJECT_REF_OFFSET as usize,
                AllocationSemantics::NonMoving,
                DEFAULT_ALLOC_OPTIONS,
            );
            self.refill_tlab();

            GCObject::from(object_start + OBJECT_REF_OFFSET)
        }
    }

    pub fn barrier(&self) -> BarrierSelector {
        self.thread.barrier()
    }

    /// Return the current thread with an extended lifetime.
    ///
    /// # Safety
    ///
    /// The returned reference must not outlive the actual runtime thread, and
    /// callers must ensure it is not used after the thread has been torn down.
    pub unsafe fn thread_unchecked(&self) -> &'static Thread {
        unsafe { std::mem::transmute(self.thread) }
    }
}

impl<'gc> Mutation<'gc> {
    unsafe fn new() -> Self {
        let thread = current_thread();

        Thread::leave_native();

        Self { thread }
    }
}

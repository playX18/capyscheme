use crate::{
    heap::{
        ObjectSlot,
        finalizer::Finalizers,
        mm::MemoryManager,
        object::{AllocationHooksOf, GcObject, HeapObjectHeader, OBJECT_REF_OFFSET},
        ptr::Gc,
        space::Space,
        sync::thread::{
            AllocFastPath, Goroutine, NativeCtx, Thread, current_goroutine, current_thread,
            is_current_thread_registed,
        },
        traits::Trace,
    },
    runtime::{State, Tc},
};
use mmtk::{
    AllocationSemantics, BarrierSelector, MutatorContext,
    util::{
        Address,
        alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
        conversions::raw_align_up,
        metadata::side_metadata::global_side_metadata_vm_base_address,
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

pub trait Rootable<'a> {
    type Root: ?Sized + 'a;
}

#[derive(Clone, Copy)]
pub struct UnpublishedAllocation {
    object: GcObject,
    payload_size: usize,
    semantics: AllocationSemantics,
}

impl UnpublishedAllocation {
    pub fn object(self) -> GcObject {
        self.object
    }
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

/// Allocations at or above this size (including the object header) go to the large object space.
pub(crate) const LOS_THRESHOLD_BYTES: usize = 8 * 1024;

const DEFAULT_ALLOC_OPTIONS: AllocationOptions = AllocationOptions {
    allow_oom_call: false,
    allow_overcommit: true,
    // Safe `#[scheme]` leaves and compiled alloc sites must not collect inline.
    // MMTk may schedule GC, the mutator pauses only at compiled yieldpoints or
    // while `InNative` or in a nested Scheme call.
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
/// yieldpoints that call [`Thread::yieldpoint`](crate::heap::sync::thread::Thread::yieldpoint).
/// Safe `#[scheme]` leaves must not call [`Mutator::yieldpoint`] or collect inline, nest
/// via [`crate::runtime::sni`] if Scheme re-entry is required.
/// Native code outside compiled safepoints should call [`Mutator::yieldpoint`] when
/// [`take_yieldpoint`](Mutation::take_yieldpoint) indicates a pending GC request.
pub(crate) struct Mutator<R>
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

        Self::try_new_with_thread::<_, ()>(thread, |mc| Ok(f(mc))).expect("invariant holds")
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
                thread.to_vm_thread(),
            );

            GLOBAL_SIDE_METADATA_BASE_ADDRESS.store(
                global_side_metadata_vm_base_address().as_usize(),
                Ordering::Relaxed,
            );
            #[cfg(feature = "bootstrap")]
            {
                crate::heap::logging::set_gc_logging_enabled(true);
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
        F: for<'gc> FnOnce(crate::runtime::MutationContext<'gc>, &'gc Root<'gc, R>) -> T,
    {
        unsafe {
            // A scope already open on this thread (Scheme running on a thread
            // calls back into its own mutator) is continued, not reopened.
            let nested = matches!(
                current_thread().get_exec_status(),
                crate::heap::sync::thread::ThreadState::Mutating
                    | crate::heap::sync::thread::ThreadState::MutatingToBlock
            );
            let mc = if nested {
                Mutation::new_nested()
            } else {
                Mutation::new()
            };
            let mc: Mutation<'static> = std::mem::transmute(mc);
            let ctx = crate::runtime::MutationContext::from(mc);
            let state = self.state.as_ptr().as_mut().expect("state must be valid");
            let res = std::panic::catch_unwind(AssertUnwindSafe(|| f(ctx, &mut state.root)));
            if !nested {
                Thread::enter_native();
            }
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
/// (for example via [`Thread::enter_native`](crate::heap::sync::thread::Thread::enter_native)).
pub fn user_collect_garbage() -> bool {
    static COLLECT_LOCK: Mutex<()> = Mutex::new(());
    let guard = COLLECT_LOCK.lock();
    Thread::leave_native();
    let did_run = mmtk::memory_manager::handle_user_collection_request(
        &crate::heap::GarbageCollector::get().mmtk,
        current_thread().to_mutator_thread(),
        true,
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
        Thread::unregister();
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct Mutation<'gc> {
    /// The running [`Goroutine`]. This is the value the `TC` register holds,
    /// and native procs receive it as `ctx` (`NativeFn` takes a `Mutation` by
    /// value), so the representation must stay the bare pointer. The worker
    /// context is reached through [`Goroutine::native`].
    nd: NonNull<Goroutine>,
    _gc: PhantomData<&'gc ()>,
}

// SAFETY: the handle is a pointer to thread-local data that is only mutated by
// the owning thread, cross-thread state in it is atomic. This matches the
// `&Thread` it replaced.
unsafe impl Send for Mutation<'_> {}
unsafe impl Sync for Mutation<'_> {}

impl<'gc> Deref for Mutation<'gc> {
    type Target = State<'gc>;

    fn deref(&self) -> &Self::Target {
        debug_assert!(
            self.thread().is_thread_state_initialized(),
            "thread state is not initialized"
        );
        unsafe { &*(self.g().state_ptr() as *const State<'gc>) }
    }
}

use mmtk::util::alloc::AllocationOptions;

impl<'gc> Mutation<'gc> {
    pub const OFFSET_OF_STATE: usize = std::mem::offset_of!(Goroutine, state);

    /// The running goroutine.
    #[inline(always)]
    fn g(&self) -> &'gc Goroutine {
        // SAFETY: the slot outlives every `Mutation` handed out for it.
        unsafe { &*self.nd.as_ptr() }
    }

    /// The worker context of the running goroutine.
    #[inline(always)]
    fn nd(&self) -> &'gc NativeCtx {
        self.g().native()
    }

    /// The running goroutine, mutable. Only the owning thread touches it.
    #[inline(always)]
    fn goroutine_ptr(&self) -> *mut Goroutine {
        self.nd.as_ptr()
    }

    #[inline(always)]
    unsafe fn post_alloc(&self, object: GcObject, bytes: usize, semantics: AllocationSemantics) {
        unsafe {
            // SAFETY: a worker touches its mutator for one goroutine at a
            // time, and a goroutine runs on one worker at a time, so this borrow is the
            // only live one.
            mmtk::memory_manager::post_alloc(
                self.nd().mutator_unchecked(),
                object
                    .to_object_reference()
                    .expect("gc object has object reference"),
                bytes,
                semantics,
            );
        }
    }

    #[inline(always)]
    /// # Safety
    ///
    /// `allocation` must be a valid `UnpublishedAllocation` returned by a prior
    /// `allocate_*` call, and the caller must hold the GC's mutator lock.
    pub unsafe fn publish(&self, allocation: UnpublishedAllocation) {
        unsafe {
            self.post_alloc(
                allocation.object,
                allocation.payload_size + size_of::<HeapObjectHeader>(),
                allocation.semantics,
            );
        }
    }

    #[inline(always)]
    /// Publish a default-space object allocated on the fast path.
    ///
    /// # Safety
    ///
    /// `object` must point at a live default-space allocation whose header and
    /// payload size are already initialized.
    pub unsafe fn publish_default_fast_alloc(&self, object: GcObject, payload_size: usize) {
        unsafe {
            self.post_alloc(
                object,
                payload_size + size_of::<HeapObjectHeader>(),
                AllocationSemantics::Default,
            );
        }
    }

    #[inline(always)]
    unsafe fn finish_unpublished_object(
        &self,
        object_start: Address,
        size: usize,
        semantics: AllocationSemantics,
    ) -> UnpublishedAllocation {
        let object = GcObject::from(object_start + OBJECT_REF_OFFSET);
        UnpublishedAllocation {
            object,
            payload_size: size,
            semantics,
        }
    }

    #[inline(always)]
    unsafe fn finish_unpublished_object_with_header(
        &self,
        object_start: Address,
        size: usize,
        header_word: u64,
        semantics: AllocationSemantics,
    ) -> UnpublishedAllocation {
        unsafe {
            object_start.store(HeapObjectHeader::from_word(header_word));
            self.finish_unpublished_object(object_start, size, semantics)
        }
    }

    #[inline(always)]
    pub fn state(&self) -> &'gc State<'gc> {
        debug_assert!(
            self.thread().is_thread_state_initialized(),
            "thread state is not initialized"
        );
        unsafe { &*(self.g().state_ptr() as *const State<'gc>) }
    }

    #[inline(always)]
    pub fn tc(&self) -> &'gc Tc<'gc> {
        // The thread context is built with the thread and needs no init gate.
        unsafe { &*(self.g().tc_ptr() as *const Tc<'gc>) }
    }

    pub fn as_ptr(self) -> *const () {
        self.nd.as_ptr() as *const ()
    }

    /// Reconstruct a mutation handle from a raw native data pointer.
    ///
    /// # Safety
    ///
    /// `ptr` must have been produced by [`Mutation::as_ptr`] for a live thread
    /// whose lifetime is valid for the returned mutation handle.
    pub unsafe fn from_ptr(ptr: *const ()) -> Self {
        Mutation {
            // SAFETY: `ptr` is a live goroutine, see `as_ptr`.
            nd: unsafe { NonNull::new_unchecked(ptr as *mut Goroutine) },
            _gc: PhantomData,
        }
    }

    /// The worker context this handle points at.
    #[inline(always)]
    pub(crate) fn native_data(&self) -> &'gc NativeCtx {
        self.nd()
    }

    #[inline(always)]
    pub fn init_state(&self, state: State<'gc>) {
        assert!(
            !self.thread().is_thread_state_initialized(),
            "thread state is already initialized"
        );
        unsafe {
            self.thread().initialize_state(state);
        }
    }

    #[inline(always)]
    pub fn thread(&self) -> &'gc Thread {
        self.nd().thread()
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
        self.g()
            .take_yieldpoint
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn finalizers(&self) -> &'gc Finalizers {
        &crate::heap::GarbageCollector::get().finalizers
    }

    pub fn is_movable<T: 'gc>(&self, obj: Gc<'gc, T>) -> bool {
        obj.to_object_reference().is_movable()
    }

    /// True if `obj` resides in a non-movable MMTk space (e.g. NonMoving alloc).
    pub fn is_in_nonmoving_space<T: 'gc>(&self, obj: Gc<'gc, T>) -> bool {
        !obj.to_object_reference().is_movable()
    }

    /// Allocate `value` in the default space, with the class derived from `T`.
    #[inline(always)]
    pub fn allocate<T: 'gc + Trace>(&self, value: T) -> Gc<'gc, T> {
        self.allocate_with(value, AllocationHooksOf::<'gc, T>::class_header_word(*self))
    }

    /// Allocate `value` in the default space with an explicit class header.
    #[inline(always)]
    pub fn allocate_with<T: 'gc>(&self, value: T, header_word: u64) -> Gc<'gc, T> {
        self.allocate_in(Space::Default, value, header_word)
    }

    /// Allocate `value` in `space` with an explicit class header.
    #[inline(always)]
    pub fn allocate_in<T: 'gc>(&self, space: Space, value: T, header_word: u64) -> Gc<'gc, T> {
        unsafe {
            let allocation = self.allocate_raw_unpublished(
                size_of::<T>(),
                size_of::<usize>().max(align_of::<T>()),
                header_word,
                space,
            );
            let obj = allocation.object();
            obj.to_address().store(value);
            self.publish(allocation);

            Gc::from_gc_object(obj)
        }
    }

    /// Allocate uninitialized storage for a `T` laid out as `layout`
    /// (`layout` may exceed `size_of::<T>()` for trailing data).
    pub fn allocate_layout<T: 'gc + Trace>(
        &self,
        layout: Layout,
        header_word: u64,
        space: Space,
    ) -> Gc<'gc, MaybeUninit<T>> {
        unsafe {
            let obj = self.allocate_raw(layout.size(), layout.align(), header_word, space);
            obj.to_address().store(MaybeUninit::<T>::uninit());
            Gc::from_gc_object(obj)
        }
    }

    /// Allocate raw GC memory for an object of `size` payload bytes and
    /// install `header_word` as its class header.
    ///
    /// # Safety
    ///
    /// `header_word` must encode the class of the object the caller writes
    /// into the returned allocation, and the caller must initialize the
    /// payload before the next safepoint.
    #[inline(always)]
    pub unsafe fn allocate_raw(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        space: Space,
    ) -> GcObject {
        unsafe {
            let allocation = self.allocate_raw_unpublished(size, alignment, header_word, space);
            let object = allocation.object();
            self.publish(allocation);
            object
        }
    }

    /// Like [`allocate_raw`](Self::allocate_raw), but the object is not yet
    /// visible to MMTk metadata.
    ///
    /// # Safety
    ///
    /// The caller must initialize every field used by GC sizing and tracing
    /// before calling [`publish`](Self::publish).
    #[inline(always)]
    pub unsafe fn allocate_raw_unpublished(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        space: Space,
    ) -> UnpublishedAllocation {
        let mut semantics = self.nd().map_semantics(space.to_semantics());
        unsafe {
            if size + size_of::<HeapObjectHeader>() >= LOS_THRESHOLD_BYTES {
                semantics = AllocationSemantics::Los;
            }

            let alignment = alignment.max(std::mem::size_of::<usize>());
            let size = raw_align_up(size, alignment);
            debug_assert!(size.is_multiple_of(alignment));
            if semantics == AllocationSemantics::Default
                && self.nd().alloc_fastpath() == AllocFastPath::TLAB
            {
                let lab = &mut *self.goroutine_ptr();

                let object_start = lab.lab_allocate(
                    size + size_of::<HeapObjectHeader>(),
                    alignment,
                    OBJECT_REF_OFFSET as _,
                );
                if !object_start.is_zero() {
                    return self.finish_unpublished_object_with_header(
                        object_start,
                        size,
                        header_word,
                        semantics,
                    );
                }
            }

            self.allocate_out_of_line(size, alignment, header_word, semantics)
        }
    }

    /// Allocate through MMTk, bypassing the TLAB.
    unsafe fn allocate_out_of_line(
        &self,
        size: usize,
        alignment: usize,
        header_word: u64,
        mut semantics: AllocationSemantics,
    ) -> UnpublishedAllocation {
        let _no_preempt = crate::runtime::sched::preempt::NoPreempt::enter();
        if semantics == AllocationSemantics::Default
            && size + size_of::<HeapObjectHeader>() >= self.nd().max_non_los_alloc_bytes()
        {
            semantics = AllocationSemantics::Los;
        }

        unsafe {
            self.flush_tlab();
            // MMTk exclusivity: a worker touches its mutator for one goroutine at a
            // time, and a goroutine runs on one worker at a time, so this borrow is the
            // only live one.
            let mutator = self.nd().mutator_unchecked();
            let bytes = size + size_of::<HeapObjectHeader>();
            let offset = OBJECT_REF_OFFSET as usize;
            let object_start = if semantics == AllocationSemantics::Los {
                mmtk::memory_manager::alloc_slow_with_options(
                    mutator,
                    bytes,
                    alignment,
                    offset,
                    semantics,
                    DEFAULT_ALLOC_OPTIONS,
                )
            } else {
                mmtk::memory_manager::alloc_with_options(
                    mutator,
                    bytes,
                    alignment,
                    offset,
                    semantics,
                    DEFAULT_ALLOC_OPTIONS,
                )
            };
            self.refill_tlab();

            self.finish_unpublished_object_with_header(object_start, size, header_word, semantics)
        }
    }

    /// Flush TLAB and reset it to the initial state. This function must be used before
    /// calling into MMTk API such as
    /// [`allocate_out_of_line`](Self::allocate_out_of_line).
    ///
    /// # Safety
    ///
    /// This function is safe only if TLAB is flushed *before* call into MMTK API such as
    /// [`allocate_out_of_line`](Self::allocate_out_of_line).
    pub unsafe fn flush_tlab(&self) {
        let (cursor, limit) = unsafe { (*self.goroutine_ptr()).lab_take() };

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
                // SAFETY: a worker touches its mutator for one goroutine at a
                // time, and a goroutine runs on one worker at a time, so this borrow is the
                // only live one.
                let allocator = self
                    .nd()
                    .mutator_unchecked()
                    .allocator_impl_mut::<ImmixAllocator<MemoryManager>>(selector);
                allocator.bump_pointer.reset(cursor, limit);
            },

            AllocatorSelector::BumpPointer(_) => unsafe {
                //SAFETY: a worker touches its mutator for one goroutine at a
                // time, and a goroutine runs on one worker at a time, so this borrow is the
                // only live one.
                let allocator = self
                    .nd()
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
    /// `allocate_out_of_line`. Otherwise TLAB might contain
    /// invalid data and cause memory corruption.
    ///
    /// # Safety
    ///
    /// This function is safe only if TLAB is refilled *after* call into MMTKA API such as
    /// [`allocate_out_of_line`](Self::allocate_out_of_line).
    pub unsafe fn refill_tlab(&self) {
        let selector = mmtk::memory_manager::get_allocator_mapping(
            &super::GarbageCollector::get().mmtk,
            AllocationSemantics::Default,
        );

        let window = match selector {
            AllocatorSelector::Immix(_) => unsafe {
                let allocator = self
                    .nd()
                    .mutator_unchecked()
                    .allocator_impl_mut::<ImmixAllocator<MemoryManager>>(selector);
                &mut allocator.bump_pointer
            },

            AllocatorSelector::BumpPointer(_) => unsafe {
                let allocator = self
                    .nd()
                    .mutator_unchecked()
                    .allocator_impl_mut::<BumpAllocator<MemoryManager>>(selector);
                &mut allocator.bump_pointer
            },

            _ => return,
        };

        let (cursor, limit) = (window.cursor, window.limit);
        window.reset(Address::ZERO, Address::ZERO);
        unsafe { (*self.goroutine_ptr()).lab_rebind(cursor, limit) };
    }

    /// Raw weak reference load barrier.
    ///
    /// # Safety
    ///
    /// May cause UB if not used during a concurrent mark.
    pub unsafe fn raw_weak_reference_load(&self, weak: GcObject) {
        if weak.is_null() {
            return;
        }

        // SAFETY: a worker touches its mutator for one goroutine at a
        // time, and a goroutine runs on one worker at a time, so this borrow is the
        // only live one.
        unsafe {
            self.nd().mutator_unchecked().barrier().load_weak_reference(
                weak.to_object_reference()
                    .expect("gc object has object reference"),
            );
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
        src: GcObject,
        slot: ObjectSlot,
        target: GcObject,
    ) {
        match self.nd().barrier() {
            BarrierSelector::ObjectBarrier => unsafe {
                let addr = src.to_address();
                let meta_addr = global_side_metadata_vm_base_address() + (addr >> 6);
                let shift = (addr >> 3) & 0b111;
                let byte_val = meta_addr.load::<u8>();

                // SAFETY: a worker touches its mutator for one goroutine at a
                // time, and a goroutine runs on one worker at a time, so this borrow is the
                // only live one.
                if (byte_val >> shift) & 1 == 1 {
                    self.nd()
                        .mutator_unchecked()
                        .barrier()
                        .object_reference_write_slow(
                            src.to_object_reference().unwrap_unchecked(),
                            slot,
                            target.to_object_reference(),
                        );
                }
            },

            BarrierSelector::SATBBarrier => {
                let addr = src.to_address();
                let meta_addr = global_side_metadata_vm_base_address() + (addr >> 6);
                let shift = (addr >> 3) & 0b111;
                let byte_val = unsafe { meta_addr.load::<u8>() };
                if (byte_val >> shift) & 1 == 1 {
                    // SAFETY: a worker touches its mutator for one goroutine at a
                    // time, and a goroutine runs on one worker at a time, so this borrow is the
                    // only live one.
                    unsafe {
                        self.nd()
                            .mutator_unchecked()
                            .barrier()
                            .object_reference_write_slow(
                                src.to_object_reference().unwrap_unchecked(),
                                slot,
                                target.to_object_reference(),
                            );
                    }
                }
            }

            BarrierSelector::FieldBarrier => {}

            BarrierSelector::NoBarrier => {}
        }
    }

    /// Remember a freshly allocated non-nursery object that was initialized
    /// with references to objects that may be in the nursery.
    ///
    /// MMTk's non-moving space does not mark new objects as unlogged, so in a
    /// generational plan the write barrier ignores stores into them and a
    /// nursery collection never looks at them. This marks `src` unlogged and
    /// then logs it, which puts it in the remembered set like any mature
    /// object that was written to.
    ///
    /// # Safety
    ///
    /// `src` must be a live object outside the nursery whose reference
    /// fields are all initialized.
    pub unsafe fn remember_new_object(&self, src: GcObject) {
        match self.nd().barrier() {
            BarrierSelector::ObjectBarrier => unsafe {
                let addr = src.to_address();
                let meta_addr = global_side_metadata_vm_base_address() + (addr >> 6);
                let shift = (addr >> 3) & 0b111;
                (*meta_addr.to_ptr::<std::sync::atomic::AtomicU8>())
                    .fetch_or(1 << shift, std::sync::atomic::Ordering::SeqCst);
                self.raw_object_reference_write(
                    src,
                    ObjectSlot::from_address(Address::ZERO),
                    GcObject::NULL,
                );
            },
            // The other barriers either need nothing for a new object or
            // have no generational remembered set.
            _ => {}
        }
    }

    pub fn barrier(&self) -> BarrierSelector {
        self.nd().barrier()
    }

    /// Return the current thread with an extended lifetime.
    ///
    /// # Safety
    ///
    /// The returned reference must not outlive the actual runtime thread, and
    /// callers must ensure it is not used after the thread has been torn down.
    pub unsafe fn thread_unchecked(&self) -> &'static Thread {
        unsafe { std::mem::transmute(self.thread()) }
    }
}

impl<'gc> Mutation<'gc> {
    pub(crate) unsafe fn new() -> Self {
        let g = current_goroutine();

        Thread::leave_native();

        Self {
            nd: NonNull::from(g),
            _gc: PhantomData,
        }
    }

    /// A handle for a mutation scope that is already open on this thread.
    pub(crate) unsafe fn new_nested() -> Self {
        Self {
            nd: NonNull::from(current_goroutine()),
            _gc: PhantomData,
        }
    }
}

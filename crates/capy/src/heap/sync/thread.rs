use crate::heap::{
    GarbageCollector, lab::LocalAllocationBuffer, mm::MemoryManager, mutator::MutatorState,
    sync::monitor::Monitor, traits::Trace,
};
use crate::runtime::{State, Tc};
use crate::utils::easy_bitfield::*;
use mmtk::{
    AllocationSemantics, BarrierSelector, Mutator,
    util::{
        Address, OpaquePointer, VMMutatorThread, VMThread,
        alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
    },
};
use parking_lot::{Condvar, Mutex};
use std::mem::MaybeUninit;
use std::{
    cell::{Cell, UnsafeCell},
    ptr::NonNull,
    sync::{
        Arc,
        atomic::{AtomicI32, AtomicPtr, AtomicU64, AtomicUsize, Ordering},
    },
    time::Duration,
};

use super::handshake::{self, HandshakeOp, Pending};
use super::monitor::MonitorGuard;
use std::collections::VecDeque;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ThreadState {
    /// Thread has not yet started.
    New = 0,
    /// Thread is executing code that mutates the heap.
    Mutating = 1,
    /// A state used by the sync code to mark that a thread is in a privileged code
    /// that does not need to synchronize with the collector. In our case, this simply
    /// means that thread is not mutating heap (aka not inside `Mutator::mutate()` call).
    InNative = 2,
    /// Thread is mutating the heap but is expected to block. Such transition happens
    /// as a result of an asynchronous call by GC or any other code that requires this
    /// thread to perform asynchronous activity.
    MutatingToBlock = 3,
    /// Thread is in native code, and is to block before being allowed to mutate heap.
    BlockedInNative = 4,
    /// Thread has died. As in, it's no longer executing code that can mutate the heap and will
    /// never do so in the future. Once this is set, the GC will no longer mark any
    /// part of the thread as live.
    Terminated = 5,
}

impl ThreadState {
    /// Checks if this state is equal to thread not running.
    pub const fn not_running(self) -> bool {
        matches!(self, Self::New | Self::Terminated)
    }

    /// Convert thread-state from non-blocked variant to blocked variant.
    ///
    /// If state cannot be blocked/already blocked just returns state as is.
    pub const fn to_blocked(self) -> Self {
        match self {
            Self::Mutating => Self::MutatingToBlock,
            Self::InNative => Self::BlockedInNative,
            _ => self,
        }
    }
}

impl ToBitfield<u64> for ThreadState {
    fn to_bitfield(self) -> u64 {
        self as u8 as u64
    }
    fn one() -> Self {
        Self::New
    }

    fn zero() -> Self {
        Self::Terminated
    }
}

impl FromBitfield<u64> for ThreadState {
    fn from_bitfield(value: u64) -> Self {
        match value {
            0 => Self::New,
            1 => Self::Mutating,
            2 => Self::InNative,
            3 => Self::MutatingToBlock,
            4 => Self::BlockedInNative,
            _ => Self::Terminated,
        }
    }

    fn from_i64(value: i64) -> Self {
        Self::from_bitfield(value as _)
    }
}

type ThreadStateField = BitField<u64, ThreadState, 0, 3, false>;
type IsBlocking = BitField<u64, bool, { ThreadStateField::NEXT_BIT }, 1, false>;
type AtYieldpoint = BitField<u64, bool, { IsBlocking::NEXT_BIT }, 1, false>;
type SoftHandshakeRequested = BitField<u64, bool, { AtYieldpoint::NEXT_BIT }, 1, false>;
type ShouldSuspend = BitField<u64, bool, { SoftHandshakeRequested::NEXT_BIT }, 1, false>;
type IsSuspended = BitField<u64, bool, { ShouldSuspend::NEXT_BIT }, 1, false>;
type ShouldBlockForHandshake = BitField<u64, bool, { IsSuspended::NEXT_BIT }, 1, false>;
type IsBlockedForHandshake = BitField<u64, bool, { ShouldBlockForHandshake::NEXT_BIT }, 1, false>;
type ShouldBlockForGc = BitField<u64, bool, { IsBlockedForHandshake::NEXT_BIT }, 1, false>;
type IsBlockedForGc = BitField<u64, bool, { ShouldBlockForGc::NEXT_BIT }, 1, false>;
type IsAboutToTerminate = BitField<u64, bool, { IsBlockedForGc::NEXT_BIT }, 1, false>;
type ActiveMutatorContext = BitField<u64, bool, { IsAboutToTerminate::NEXT_BIT }, 1, false>;
type ThreadStateInitialized = BitField<u64, bool, { ActiveMutatorContext::NEXT_BIT }, 1, false>;

/// What a [`Thread::request_yieldpoint`] asks for: `Full` (`+1`) is taken
/// at prologues and loop back-edges, `PrologueOnly` (`-1`) at prologues
/// only. A pending `Full` is never downgraded to `PrologueOnly`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum YieldpointKind {
    Full,
    PrologueOnly,
}

#[repr(C)]
#[derive()]
pub struct Thread {
    /// The owning OS thread's [`ThreadNativeData`], null until the thread is
    /// bound with [`init_current_thread`] and again after
    /// [`deinit_current_thread`]. Stored and cleared under `monitor`, other
    /// threads must read it under `monitor` too (see [`Thread::request_yieldpoint`]).
    native_data: AtomicPtr<ThreadNativeData>,
    id: AtomicU64,
    status_word: AtomicBitfieldContainer<u64>,
    monitor: Monitor<()>,
    /// Soft handshake operations waiting for this thread's next yieldpoint
    /// (see [`handshake`]). `SoftHandshakeRequested` is set exactly while
    /// the queue is non-empty; both change together under this lock.
    handshake_ops: Mutex<VecDeque<Pending>>,
    index_in_thread_list: AtomicUsize,
    heap_base: Address,
    heap_shift: u32,
    /// The SIGPROF sample ring
    pub(crate) prof_ring: AtomicPtr<crate::runtime::profile::capture::ProfileRing>,
    is_mutator_thread: bool,
}

impl Drop for Thread {
    fn drop(&mut self) {
        let ring = self.prof_ring.load(Ordering::Relaxed);
        if !ring.is_null() {
            // SAFETY: the ring was heap-allocated at bind/arm, never
            // freed elsewhere, and no aggregator borrow outlives the
            // last `Arc<Thread>`.
            drop(unsafe { Box::from_raw(ring) });
        }
    }
}

impl Thread {
    #[inline(always)]
    pub fn heap_base(&self) -> Address {
        self.heap_base
    }

    #[inline(always)]
    pub fn heap_shift(&self) -> u32 {
        self.heap_shift
    }

    pub(crate) fn stack_bounds(&self) -> (Address, Address) {
        let data = self.native_data();
        (data.stack_low, data.stack_high)
    }

    pub fn active_mutator_context(&self) -> bool {
        self.status_word.read::<ActiveMutatorContext>()
    }

    pub(crate) fn set_active_mutator_context(&self, value: bool) {
        self.status_word.update::<ActiveMutatorContext>(value);
    }

    pub(crate) fn set_is_about_to_terminate(&self, value: bool) {
        self.status_word.update::<IsAboutToTerminate>(value);
    }

    /// Returns the value of `take_yieldpoint` field. When this function
    /// returnrs non-zero value caller is responsible for invoking [`Thread::yieldpoint`].
    ///
    /// Sign encoding (JikesRVM-style): `0` means nothing pending, `> 0` is
    /// taken at prologues and loop back-edges, `< 0` at prologues only.
    #[must_use = "Must check the value and call [`Thread::yieldpoint`] if necessary"]
    pub fn take_yieldpoint(&self) -> i32 {
        self.yieldpoint_flag()
            .map_or(0, |flag| flag.load(Ordering::Relaxed))
    }

    /// The yieldpoint flag in the bound [`ThreadNativeData`], `None` while
    /// the thread is unbound.
    fn yieldpoint_flag(&self) -> Option<&AtomicI32> {
        // SAFETY: a non-null pointer refers to the owner's thread-local slot,
        // which outlives the pointer (cleared before the slot is torn down).
        unsafe { self.native_data.load(Ordering::Acquire).as_ref() }.map(|nd| &nd.take_yieldpoint)
    }

    pub fn request_yieldpoint(&self, kind: YieldpointKind) {
        // Under the monitor so the owner cannot unbind (and drop its native
        // data) while the flag and `esp` are written.
        let guard = self.monitor.lock();
        self.request_yieldpoint_locked(&guard, kind);
    }

    /// [`Self::request_yieldpoint`] for a caller that already holds `monitor`.
    fn request_yieldpoint_locked(&self, guard: &MonitorGuard<'_, ()>, kind: YieldpointKind) {
        let Some(flag) = self.yieldpoint_flag() else {
            return;
        };
        match kind {
            YieldpointKind::Full => {
                // `+1` dominates: upgrade `-1` or `0` unconditionally.
                flag.store(1, Ordering::Relaxed);
            }
            YieldpointKind::PrologueOnly => {
                // CAS that never downgrades `+1` to `-1`.
                let mut current = flag.load(Ordering::Relaxed);
                loop {
                    if current > 0 {
                        break;
                    }
                    match flag.compare_exchange_weak(
                        current,
                        -1,
                        Ordering::Relaxed,
                        Ordering::Relaxed,
                    ) {
                        Ok(_) => break,
                        Err(actual) => current = actual,
                    }
                }
            }
        }
        self.poke_esp();
        guard.notify_all();
    }

    /// Store 0 into the thread's `Tc.esp` so its next procedure entry fails
    /// its overflow check, the overflow path restores the real limit and
    /// takes the yield slow path. The flag above is stored first, and
    /// this store is `Release`, so a thread that observes the poke also
    /// observes the flag.
    ///
    /// Must be called with `monitor` held: that is what keeps the native
    /// data alive. A no-op for an unbound thread.
    fn poke_esp(&self) {
        // SAFETY: the caller holds `monitor`, so a non-null pointer is a live
        // slot with a built `Tc`, and `esp` is atomic precisely for this
        // cross-thread poke.
        if let Some(nd) = unsafe { self.native_data.load(Ordering::Acquire).as_ref() } {
            unsafe { &*nd.tc.get() }
                .esp
                .store(std::ptr::null_mut(), Ordering::Release);
        }
    }

    /// Hand `op` to this thread on behalf of a requester (see
    /// [`handshake`]). Runs under the thread's monitor, which is what makes
    /// the state it observes stable:
    ///
    /// - `Mutating`/`MutatingToBlock`: queue the op, count it in `done` and
    ///   ask for a full yieldpoint. The thread runs it at its next one.
    /// - `InNative`/`BlockedInNative`: the thread cannot touch the heap or
    ///   run Scheme without going through `leave_native`, so the op runs
    ///   right here. `ShouldBlockForHandshake` is set around it so the
    ///   lock-free `leave_native` shortcut cannot start Scheme meanwhile.
    ///   The slow path waits on the monitor we hold.
    /// - `New`/`Terminated`/about to terminate: nothing to do.
    pub(crate) fn deliver_handshake(
        &self,
        op: &Arc<dyn HandshakeOp>,
        done: &Arc<handshake::Completion>,
    ) {
        let guard = self.monitor.lock();
        if self.is_about_to_terminate() {
            return;
        }
        loop {
            let word = self.status_word.load(Ordering::Acquire);
            match ThreadStateField::decode(word) {
                ThreadState::New | ThreadState::Terminated => return,
                ThreadState::Mutating | ThreadState::MutatingToBlock => {
                    done.add_pending();
                    {
                        let mut queue = self.handshake_ops.lock();
                        queue.push_back(Pending::new(op.clone(), done.clone()));
                        self.status_word.update::<SoftHandshakeRequested>(true);
                    }
                    self.request_yieldpoint_locked(&guard, YieldpointKind::Full);
                    return;
                }
                ThreadState::InNative | ThreadState::BlockedInNative => {
                    let new_word = ShouldBlockForHandshake::update(true, word);
                    if self
                        .status_word
                        .compare_exchange_weak(word, new_word, Ordering::AcqRel, Ordering::Acquire)
                        .is_ok()
                    {
                        handshake::run_op(&**op, self);
                        self.status_word.update::<ShouldBlockForHandshake>(false);
                        return;
                    }
                }
            }
        }
    }

    /// Run every queued handshake operation on this thread. The monitor is
    /// released while they run, and the queue is re-checked afterwards, since
    /// a requester may add to it in that window.
    fn run_pending_handshakes(&self, guard: &mut MonitorGuard<'_, ()>) {
        while self.status_word.read::<SoftHandshakeRequested>() {
            let batch: Vec<Pending> = {
                let mut queue = self.handshake_ops.lock();
                self.status_word.update::<SoftHandshakeRequested>(false);
                queue.drain(..).collect()
            };
            guard.unlocked(|| {
                for pending in batch {
                    pending.run(self);
                }
            });
        }
    }

    pub fn get_exec_status(&self) -> ThreadState {
        self.status_word.read::<ThreadStateField>()
    }

    pub fn is_about_to_terminate(&self) -> bool {
        self.status_word.read::<IsAboutToTerminate>()
    }

    pub fn is_blocking(&self) -> bool {
        self.status_word.read::<IsBlocking>()
    }

    pub fn is_mutating(&self) -> bool {
        !self.is_blocking()
            && !self.is_about_to_terminate()
            && matches!(
                self.get_exec_status(),
                ThreadState::Mutating | ThreadState::MutatingToBlock
            )
    }

    pub fn id(&self) -> u64 {
        self.id.load(Ordering::Relaxed)
    }

    pub fn is_blocked(&self) -> bool {
        for adapter in BLOCK_ADAPTERS.iter() {
            if adapter.is_blocked(self) {
                return true;
            }
        }

        false
    }

    fn acknowledge_block_requests(&self, guard: &MonitorGuard<'_, ()>) {
        let mut had_some = false;

        for adapter in BLOCK_ADAPTERS.iter() {
            if adapter.has_block_request(self) {
                adapter.set_blocked(self, true);
                adapter.clear_block_request(self);
                had_some = true;
            }
        }
        if had_some {
            guard.notify_all();
        }
    }

    fn check_block(&self, guard: &mut MonitorGuard<'_, ()>) {
        debug_assert!(!self.is_blocking());
        debug_assert!(!self.is_about_to_terminate());
        let id = self.id();

        self.status_word.update::<IsBlocking>(true);

        // Before blocking: a requester must not wait out a long GC pause
        // for an operation this thread can run right now.
        self.run_pending_handshakes(guard);

        let mut had_really_blocked = false;

        loop {
            self.acknowledge_block_requests(guard);
            if !self.is_blocked() {
                break;
            }

            had_really_blocked = true;

            log::debug!(
                "Thread #{id} is really blocked with status {:?}",
                self.get_exec_status()
            );

            guard.wait();
        }

        if had_really_blocked {
            log::debug!("Thread #{id} is unblocking");
        }

        // An operation queued while blocked runs before Scheme resumes.
        self.run_pending_handshakes(guard);

        self.status_word
            .update::<ThreadStateField>(ThreadState::Mutating);
        self.status_word.update::<IsBlocking>(false);
    }

    #[inline(never)]
    pub fn yieldpoint() {
        let nd = current_native_data();
        let thread = nd.thread();

        thread.status_word.update::<AtYieldpoint>(true);

        let mut guard = thread.monitor.lock();

        if nd.take_yieldpoint.load(Ordering::Relaxed) != 0 {
            nd.take_yieldpoint.store(0, Ordering::Relaxed);
            thread.check_block(&mut guard);
        }

        drop(guard);

        thread.status_word.update::<AtYieldpoint>(false);
    }

    fn enter_native_blocked(&self) {
        let mut guard = self.monitor.lock();
        self.status_word
            .update::<ThreadStateField>(ThreadState::InNative);
        self.acknowledge_block_requests(&guard);
        // Operations queued while this thread was mutating: the requester
        // saw `Mutating` and is waiting for us, so run them before leaving.
        self.run_pending_handshakes(&mut guard);
        drop(guard);
    }

    fn leave_native_blocked(&self) {
        let mut guard = self.monitor.lock();
        self.check_block(&mut guard);
    }

    pub fn enter_native() {
        let thread = current_thread();
        assert!(
            matches!(
                thread.get_exec_status(),
                ThreadState::Mutating | ThreadState::MutatingToBlock
            ),
            "Thread must be in mutating state to enter native, got: {:?}",
            thread.get_exec_status()
        );
        thread.enter_native_blocked();
        assert!(
            matches!(
                thread.get_exec_status(),
                ThreadState::InNative | ThreadState::BlockedInNative
            ),
            "Thread must be in InNative state after entering native, got: {:?}",
            thread.get_exec_status()
        );
        // TODO(GC): Use a CAS loop instead of asserting single-threaded access
    }

    fn attempt_leave_native_no_block() -> bool {
        let t = current_thread();

        loop {
            let word = t.status_word.load(Ordering::Relaxed);
            if ThreadStateField::decode(word) != ThreadState::InNative
                || ShouldBlockForGc::decode(word)
                || IsBlockedForGc::decode(word)
                || ShouldBlockForHandshake::decode(word)
            {
                return false;
            }

            let new_word = ThreadStateField::update(ThreadState::Mutating, word);
            if t.status_word
                .compare_exchange_weak(word, new_word, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                return true;
            }
        }
    }

    pub fn leave_native() {
        if !Self::attempt_leave_native_no_block() {
            current_thread().leave_native_blocked();
        }
    }

    fn set_blocked_exec_status(&self) -> ThreadState {
        let mut old_state = self.get_exec_status();

        loop {
            let new_state = match old_state {
                ThreadState::Mutating => ThreadState::MutatingToBlock,
                ThreadState::InNative => ThreadState::BlockedInNative,
                _ => old_state,
            };

            match self.attempt_fast_exec_status_transition(old_state, new_state) {
                Ok(_) => return new_state,
                Err(state) => old_state = state,
            }
        }
    }

    fn attempt_fast_exec_status_transition(
        &self,
        old: ThreadState,
        new: ThreadState,
    ) -> Result<ThreadState, ThreadState> {
        let word = self.status_word.load(Ordering::Relaxed);
        let old_word = ThreadStateField::update(old, word);
        let new_word = ThreadStateField::update(new, word);

        self.status_word
            .compare_exchange_weak(old_word, new_word, Ordering::Relaxed, Ordering::Relaxed)
            .map(ThreadStateField::decode)
            .map_err(ThreadStateField::decode)
    }

    pub fn block<T: BlockAdapter>(&self, ba: &T, asynchronous: bool) -> ThreadState {
        let mut guard = self.monitor.lock();

        let token = ba.request_block(self);
        let mut result;

        if std::ptr::eq(current_thread(), self) {
            self.check_block(&mut guard);
            result = self.get_exec_status();
        } else if self.is_about_to_terminate() {
            result = ThreadState::Terminated;
        } else {
            // A GC block is a `Full` (`+1`) request: taken everywhere.
            if let Some(flag) = self.yieldpoint_flag() {
                flag.store(1, Ordering::Relaxed);
            }
            self.poke_esp();
            let new_state = self.set_blocked_exec_status();
            result = new_state;

            guard.notify_all();

            if new_state == ThreadState::MutatingToBlock {
                if !asynchronous {
                    while ba.has_block_request_with_token(self, token)
                        && !ba.is_blocked(self)
                        && !self.is_about_to_terminate()
                    {
                        guard.wait_for(Duration::from_secs(1));
                    }

                    if self.is_about_to_terminate() {
                        result = ThreadState::Terminated;
                    } else {
                        result = self.get_exec_status();
                    }
                }
            } else if new_state == ThreadState::BlockedInNative {
                ba.clear_block_request(self);
                ba.set_blocked(self, true);
            }
        }

        drop(guard);

        result
    }

    pub fn unblock<T: BlockAdapter>(&self, ba: &T) {
        let guard = self.monitor.lock();
        ba.clear_block_request(self);
        ba.set_blocked(self, false);
        guard.notify_all();
        drop(guard);
    }

    /// Return the thread's mutator without checking initialization state.
    ///
    /// # Safety
    ///
    /// The thread must have initialized native data and mutator state, and the
    /// caller must ensure exclusive mutable access to the mutator.
    pub unsafe fn mutator_unchecked(&self) -> &'static mut Box<Mutator<MemoryManager>> {
        unsafe {
            std::mem::transmute(
                &mut *self
                    .native_data_mut_ptr()
                    .as_mut()
                    .unwrap_unchecked()
                    .mutator
                    .as_mut()
                    .unwrap_unchecked(),
            )
        }
    }

    pub fn is_mutator(&self) -> bool {
        self.active_mutator_context()
            && unsafe { self.native_data_mut_ptr().as_ref() }.is_some_and(|nd| nd.mutator.is_some())
    }

    pub(crate) fn from_vm_thread(vm_thread: VMThread) -> &'static Self {
        unsafe { vm_thread.0.to_address().as_ref() }
    }

    pub(crate) fn from_mutator_thread(mutator_thread: VMMutatorThread) -> &'static Self {
        unsafe { mutator_thread.0.0.to_address().as_ref() }
    }

    pub(crate) fn to_vm_thread(&self) -> VMThread {
        VMThread(OpaquePointer::from_address(Address::from_ref(self)))
    }

    pub(crate) fn to_mutator_thread(&self) -> VMMutatorThread {
        VMMutatorThread(self.to_vm_thread())
    }

    /// The bound native data. Only valid once the thread is bound.
    pub(crate) fn native_data(&self) -> &ThreadNativeData {
        debug_assert!(!self.native_data_mut_ptr().is_null(), "thread is not bound");
        unsafe { &*self.native_data_mut_ptr() }
    }

    /// Null while the thread is unbound.
    pub(crate) fn native_data_mut_ptr(&self) -> *mut ThreadNativeData {
        self.native_data.load(Ordering::Acquire)
    }

    pub fn is_thread_state_initialized(&self) -> bool {
        self.status_word.read::<ThreadStateInitialized>()
    }

    pub(crate) unsafe fn initialize_state<'gc>(&self, state: State<'gc>) {
        unsafe {
            assert!(!self.is_thread_state_initialized());
            let state_ptr = (*self.native_data_mut_ptr()).state.get();
            state_ptr.write(MaybeUninit::new(std::mem::transmute::<
                State<'gc>,
                State<'static>,
            >(state)));
            self.status_word.update::<ThreadStateInitialized>(true);
        }
    }

    pub(crate) fn state_ptr(&self) -> *mut State<'static> {
        self.native_data().state.get() as *mut State<'static>
    }

    pub(crate) fn tc_ptr(&self) -> *mut Tc<'static> {
        self.native_data().tc.get()
    }

    /// Create a new thread handle. The returned value
    /// is a handle to an object that can be managed
    /// through [`ThreadManager`] efficiently. Once thread is actually spawned (`Mutator::try_new_with_thread` is called)
    /// this handle will get associated with a platform handle to a thread. Not much can be done with the handle except
    /// suspending threads on your own.
    ///
    /// Calling [`block`](Self::block) on a new handle will instantly return as thread is not in running state.
    pub fn new(for_mutator: bool) -> Arc<Self> {
        let heap_end = mmtk::memory_manager::last_heap_address().as_usize();
        let (heap_base, heap_shift) = if heap_end <= (4usize << 30) {
            (Address::ZERO, 0)
        } else if heap_end <= (32usize << 30) {
            (Address::ZERO, 3)
        } else {
            (mmtk::memory_manager::starting_heap_address() - 4096, 3)
        };

        Arc::new(Thread {
            heap_base,
            heap_shift,
            id: AtomicU64::new(0),
            status_word: AtomicBitfieldContainer::new(
                ThreadStateField::encode(ThreadState::New)
                    | ActiveMutatorContext::encode(for_mutator),
            ),
            monitor: Monitor::new(()),
            handshake_ops: Mutex::new(VecDeque::new()),
            native_data: AtomicPtr::new(std::ptr::null_mut()),
            index_in_thread_list: AtomicUsize::new(usize::MAX),
            prof_ring: AtomicPtr::new(std::ptr::null_mut()),
            is_mutator_thread: for_mutator,
        })
    }

    /// Whether this thread runs Scheme (as opposed to a GC worker).
    /// Unlike [`Thread::is_mutator`], fixed at construction, so bind
    /// hooks can consult it.
    pub(crate) fn is_mutator_thread(&self) -> bool {
        self.is_mutator_thread
    }

    /// The current thread without blocking or panicking: `None` when
    /// unbound, mid bind/unbind, or reentered. The only `THREAD` access
    /// the `SIGPROF` handler may use.
    pub(crate) fn try_current_thread() -> Option<&'static Thread> {
        try_current_native_data().map(|nd| nd.thread())
    }

    /// Returns true if thread is managed by thread subsystem.
    ///
    /// Managed threads can be suspended, resumed, inspected by GC etc. Unmanaged
    /// threads are threads which cannot participate in heap mutation and are not
    /// managed by GC.
    pub fn is_managed(&self) -> bool {
        self.index_in_thread_list.load(Ordering::Relaxed) != usize::MAX
    }

    pub fn alloc_fastpath(&self) -> AllocFastPath {
        self.native_data().alloc_fastpath
    }

    pub fn barrier(&self) -> BarrierSelector {
        self.native_data().barrier_selector
    }

    /// The semantics an allocation with `semantics` really gets. MMTk 0.33's
    /// non-moving space is an `ImmixSpace`. SemiSpace and MarkSweep trace it
    /// with a trace kind it rejects (`unreachable!` in
    /// `ImmixSpace::trace_object`), and under ConcurrentImmix objects in it
    /// are lost (a code buffer read from a FASL file is found corrupt, the
    /// large object space does not show it). Under those plans non-moving
    /// objects go where nothing moves them either: the large object space,
    /// or mark-sweep's own default space.
    #[inline(always)]
    pub fn map_semantics(&self, semantics: AllocationSemantics) -> AllocationSemantics {
        if semantics == AllocationSemantics::NonMoving {
            self.native_data().nonmoving_semantics
        } else {
            semantics
        }
    }

    pub fn max_non_los_alloc_bytes(&self) -> usize {
        self.native_data().max_non_los_default_alloc_bytes
    }

    pub(crate) fn register(this: &Arc<Self>) {
        init_current_thread(this.clone());

        let gc = GarbageCollector::get();
        let constraints = gc.mmtk.get_plan().constraints();
        let selector =
            mmtk::memory_manager::get_allocator_mapping(&gc.mmtk, AllocationSemantics::Default);
        let native_data = unsafe { &mut *this.native_data_mut_ptr() };
        native_data.max_non_los_default_alloc_bytes = constraints.max_non_los_default_alloc_bytes;
        native_data.barrier_selector = constraints.barrier;
        native_data.nonmoving_semantics = match *gc.mmtk.get_options().plan {
            mmtk::util::options::PlanSelector::SemiSpace
            | mmtk::util::options::PlanSelector::ConcurrentImmix => AllocationSemantics::Los,
            // Mark-sweep never moves its default space.
            mmtk::util::options::PlanSelector::MarkSweep => AllocationSemantics::Default,
            _ => AllocationSemantics::NonMoving,
        };
        native_data.alloc_fastpath = match selector {
            AllocatorSelector::BumpPointer(_) | AllocatorSelector::Immix(_) => AllocFastPath::TLAB,
            _ => AllocFastPath::None,
        };

        let mutator = mmtk::memory_manager::bind_mutator(&gc.mmtk, this.to_mutator_thread());

        this.set_active_mutator_context(true);
        native_data.mutator = Some(mutator);
        let (stack_low, stack_high) = query_stack_bounds();
        native_data.stack_low = stack_low;
        native_data.stack_high = stack_high;
        gc.threads.add_thread(this.clone());
        // After the manager insert: the profiler's arm enumerates under
        // the same lock with `ARMED` set first, so a thread is either
        // seen by the enumeration or allocates here, never missed.
        crate::runtime::profile::note_thread_bound(this);
        crate::runtime::profile::maybe_arm_from_env();
        this.status_word
            .update::<ThreadStateField>(ThreadState::InNative);
    }

    pub(crate) fn unregister() {
        {
            let this = current_thread();
            // Drain this thread's samples before it leaves the manager:
            // afterwards the aggregator can no longer reach its ring.
            crate::runtime::profile::note_thread_unbinding(this);

            let mut guard = this.monitor.lock();
            this.set_is_about_to_terminate(true);
            this.status_word.update::<ActiveMutatorContext>(false);
            // Requesters skip a terminating thread (checked under this
            // monitor), so draining now strands nothing.
            this.run_pending_handshakes(&mut guard);
            guard.notify_all();

            if let Some(mut mutator) = unsafe { &mut (*this.native_data_mut_ptr()).mutator }.take()
            {
                mmtk::memory_manager::destroy_mutator(&mut mutator);
            }

            drop(guard);
        }

        GarbageCollector::get().threads.remove_current_thread();
        deinit_current_thread();
    }

    pub(crate) fn flush_tlab(&self) {
        let tlab = unsafe { &mut (*self.native_data_mut_ptr()).lab };

        let (cursor, limit) = tlab.take();

        if cursor.is_zero() {
            debug_assert!(limit.is_zero());
            return;
        }

        let selector = mmtk::memory_manager::get_allocator_mapping(
            &crate::heap::GarbageCollector::get().mmtk,
            AllocationSemantics::Default,
        );

        match selector {
            AllocatorSelector::Immix(_) => unsafe {
                let allocator = self
                    .mutator_unchecked()
                    .allocator_impl_mut::<ImmixAllocator<MemoryManager>>(selector);
                allocator.bump_pointer.reset(cursor, limit);
            },

            AllocatorSelector::BumpPointer(_) => unsafe {
                let allocator = self
                    .mutator_unchecked()
                    .allocator_impl_mut::<BumpAllocator<MemoryManager>>(selector);
                allocator.bump_pointer.reset(cursor, limit);
            },

            _ => (),
        }
    }
}

unsafe impl Send for Thread {}
unsafe impl Sync for Thread {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub enum AllocFastPath {
    TLAB,
    None,
}

/// Everything a bound OS thread owns, in one `#[repr(C)]` block that the `TC`
/// register points at. Lives in this thread's `#[thread_local]` slot,
/// [`Thread`] only keeps a pointer to it (see [`Thread::native_data_mut_ptr`]).
#[repr(C)]
pub struct ThreadNativeData {
    /// Yieldpoint request flag, written by other threads.
    pub(crate) take_yieldpoint: AtomicI32,
    pub(crate) lab: LocalAllocationBuffer,
    pub mutator: Option<Box<Mutator<MemoryManager>>>,
    pub(crate) mutator_state: Option<NonNull<MutatorState<dyn Trace>>>,
    pub(crate) barrier_selector: BarrierSelector,
    pub(crate) max_non_los_default_alloc_bytes: usize,
    pub(crate) alloc_fastpath: AllocFastPath,
    pub(crate) nonmoving_semantics: AllocationSemantics,
    pub(crate) tc: UnsafeCell<Tc<'static>>,
    pub(crate) state: UnsafeCell<MaybeUninit<State<'static>>>,
    pub(crate) stack_low: Address,
    pub(crate) stack_high: Address,
    /// The handle this data belongs to. Strong: the handle lives at least as
    /// long as its data, `Thread` points back only through a raw pointer, so
    /// there is no cycle.
    thread: Option<Arc<Thread>>,
}

impl ThreadNativeData {
    pub const TAKE_YIELDPOINT_OFFSET: usize = std::mem::offset_of!(Self, take_yieldpoint);
    pub const STATE_OFFSET: usize = std::mem::offset_of!(Self, state);
    pub const TC_OFFSET: usize = std::mem::offset_of!(Self, tc);
    pub const LAB_OFFSET: usize = std::mem::offset_of!(Self, lab);
    pub const LAB_OFFSET_LIMIT: usize =
        Self::LAB_OFFSET + std::mem::offset_of!(LocalAllocationBuffer, limit);
    pub const LAB_OFFSET_CURSOR: usize =
        Self::LAB_OFFSET + std::mem::offset_of!(LocalAllocationBuffer, cursor);

    /// The cross-thread handle for this data.
    #[inline(always)]
    pub fn thread(&self) -> &Thread {
        // SAFETY: set at init and only cleared when the data is dropped.
        unsafe { self.thread.as_deref().unwrap_unchecked() }
    }

    pub(crate) fn state_ptr(&self) -> *mut State<'static> {
        self.state.get() as *mut State<'static>
    }

    pub(crate) fn tc_ptr(&self) -> *mut Tc<'static> {
        self.tc.get()
    }

    pub fn alloc_fastpath(&self) -> AllocFastPath {
        self.alloc_fastpath
    }

    pub fn barrier(&self) -> BarrierSelector {
        self.barrier_selector
    }

    pub fn max_non_los_alloc_bytes(&self) -> usize {
        self.max_non_los_default_alloc_bytes
    }

    /// The semantics an allocation with `semantics` really gets, see
    /// [`Thread::map_semantics`].
    #[inline(always)]
    pub fn map_semantics(&self, semantics: AllocationSemantics) -> AllocationSemantics {
        if semantics == AllocationSemantics::NonMoving {
            self.nonmoving_semantics
        } else {
            semantics
        }
    }

    /// The thread's mutator without checking initialization.
    ///
    /// # Safety
    ///
    /// The mutator must be bound, and the caller must ensure exclusive
    /// mutable access to it.
    pub unsafe fn mutator_unchecked(&self) -> &'static mut Box<Mutator<MemoryManager>> {
        unsafe {
            let nd = self as *const Self as *mut Self;
            std::mem::transmute((*nd).mutator.as_mut().unwrap_unchecked())
        }
    }
}

/// The per-thread slot behind `#[thread_local]`. Const-constructible, filled
/// by [`init_current_thread`] and emptied by [`deinit_current_thread`]. There
/// is no destructor: a thread must unbind itself before it exits.
struct NativeSlot {
    bound: Cell<bool>,
    data: UnsafeCell<MaybeUninit<ThreadNativeData>>,
}

#[thread_local]
static SLOT: NativeSlot = NativeSlot {
    bound: Cell::new(false),
    data: UnsafeCell::new(MaybeUninit::uninit()),
};

#[inline(always)]
pub(crate) fn try_current_native_data() -> Option<&'static ThreadNativeData> {
    if SLOT.bound.get() {
        // SAFETY: `bound` is set after the data is written and cleared before
        // it is dropped.
        Some(unsafe { &*(SLOT.data.get() as *const ThreadNativeData) })
    } else {
        None
    }
}

#[inline(always)]
pub(crate) fn current_native_data() -> &'static ThreadNativeData {
    try_current_native_data().expect("Thread is not initialized")
}

pub fn current_thread<'a>() -> &'a Thread {
    current_native_data().thread()
}

pub(crate) fn is_current_thread_registed() -> bool {
    SLOT.bound.get()
}

pub(crate) fn init_current_thread(thread: Arc<Thread>) {
    assert!(!SLOT.bound.get(), "thread is already bound");
    let barrier_selector = GarbageCollector::get()
        .mmtk
        .get_plan()
        .constraints()
        .barrier;
    let data = ThreadNativeData {
        take_yieldpoint: AtomicI32::new(0),
        lab: LocalAllocationBuffer::new(),
        mutator: None,
        mutator_state: None,
        barrier_selector,
        max_non_los_default_alloc_bytes: 0,
        alloc_fastpath: AllocFastPath::None,
        nonmoving_semantics: AllocationSemantics::NonMoving,
        tc: UnsafeCell::new(Tc::new()),
        state: UnsafeCell::new(MaybeUninit::uninit()),
        stack_low: Address::ZERO,
        stack_high: Address::ZERO,
        thread: Some(thread.clone()),
    };
    let ptr = SLOT.data.get();
    // SAFETY: the slot is unbound, so nothing reads it.
    unsafe { ptr.write(MaybeUninit::new(data)) };
    SLOT.bound.set(true);
    let guard = thread.monitor.lock();
    thread
        .native_data
        .store(ptr as *mut ThreadNativeData, Ordering::Release);
    drop(guard);
}

pub(crate) fn deinit_current_thread() {
    if !SLOT.bound.get() {
        return;
    }
    let ptr = SLOT.data.get() as *mut ThreadNativeData;
    // SAFETY: bound, so initialized.
    let thread = unsafe { (*ptr).thread.clone().unwrap_unchecked() };
    // Unpublish under the monitor: cross-thread users hold it while they
    // touch the data.
    let guard = thread.monitor.lock();
    thread
        .native_data
        .store(std::ptr::null_mut(), Ordering::Release);
    drop(guard);
    SLOT.bound.set(false);
    // SAFETY: unbound and unpublished, nobody can reach the data anymore.
    unsafe { std::ptr::drop_in_place(ptr) };
}

#[cfg(target_vendor = "apple")]
fn query_stack_bounds() -> (Address, Address) {
    // Darwin has no `pthread_getattr_np`, it reports the stack base (highest
    // address) and size of a thread directly.
    unsafe {
        let thread = libc::pthread_self();
        let high = Address::from_ptr(libc::pthread_get_stackaddr_np(thread));
        let stacksize = libc::pthread_get_stacksize_np(thread);
        if high.is_zero() || stacksize == 0 {
            return (Address::ZERO, Address::ZERO);
        }
        (high - stacksize, high)
    }
}

#[cfg(not(target_vendor = "apple"))]
fn query_stack_bounds() -> (Address, Address) {
    unsafe {
        let mut attr = std::mem::MaybeUninit::uninit();
        if libc::pthread_getattr_np(libc::pthread_self(), attr.as_mut_ptr()) != 0 {
            return (Address::ZERO, Address::ZERO);
        }
        let mut attr = attr.assume_init();
        let mut stackaddr: *mut libc::c_void = std::ptr::null_mut();
        let mut stacksize: usize = 0;
        if libc::pthread_attr_getstack(&attr, &mut stackaddr, &mut stacksize) != 0 {
            libc::pthread_attr_destroy(&mut attr);
            return (Address::ZERO, Address::ZERO);
        }
        libc::pthread_attr_destroy(&mut attr);
        let low = Address::from_ptr(stackaddr);
        (low, low + stacksize)
    }
}

pub trait BlockAdapter: Send + Sync {
    fn is_blocked(&self, thread: &Thread) -> bool;
    fn set_blocked(&self, thread: &Thread, value: bool);
    fn request_block(&self, thread: &Thread) -> u32;
    fn has_block_request(&self, thread: &Thread) -> bool;
    fn has_block_request_with_token(&self, thread: &Thread, token: u32) -> bool;
    fn clear_block_request(&self, thread: &Thread);
}

pub(crate) static GC_BLOCK_ADAPTER: GcBlockAdapter = GcBlockAdapter;
pub(crate) static BLOCK_ADAPTERS: &[&dyn BlockAdapter] = &[&GC_BLOCK_ADAPTER];

pub(crate) struct GcBlockAdapter;

impl BlockAdapter for GcBlockAdapter {
    fn is_blocked(&self, thread: &Thread) -> bool {
        thread.status_word.read::<IsBlockedForGc>()
    }

    fn set_blocked(&self, thread: &Thread, value: bool) {
        unsafe {
            let state = thread
                .state_ptr()
                .as_ref()
                .expect("pointer in allocated entry range");
            if value {
                state.stats.start_stw();
            } else {
                state.stats.end_stw();
            }
        }
        thread.status_word.update::<IsBlockedForGc>(value)
    }

    fn request_block(&self, thread: &Thread) -> u32 {
        thread.status_word.update::<ShouldBlockForGc>(true);
        0
    }

    fn has_block_request_with_token(&self, thread: &Thread, token: u32) -> bool {
        let _ = token;
        self.has_block_request(thread)
    }

    fn has_block_request(&self, thread: &Thread) -> bool {
        thread.status_word.read::<ShouldBlockForGc>()
    }

    fn clear_block_request(&self, thread: &Thread) {
        thread.status_word.update::<ShouldBlockForGc>(false);
    }
}

pub struct ThreadManager {
    threads: Mutex<Vec<Arc<Thread>>>,
    handshake_threads: Mutex<Vec<Arc<Thread>>>,
    cv_join: Condvar,
    next_thread_id: AtomicUsize,
}

impl Default for ThreadManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ThreadManager {
    pub fn new() -> Self {
        Self {
            threads: Mutex::new(Vec::with_capacity(1)),
            handshake_threads: Mutex::new(Vec::with_capacity(1)),
            cv_join: Condvar::new(),
            next_thread_id: AtomicUsize::new(0),
        }
    }

    pub fn add_thread(&self, thread: Arc<Thread>) {
        let mut threads = self.threads.lock();
        let idx = threads.len();
        thread.index_in_thread_list.store(idx, Ordering::Relaxed);
        thread.id.store(
            self.next_thread_id.fetch_add(1, Ordering::Relaxed) as _,
            Ordering::Relaxed,
        );
        threads.push(thread);
    }

    pub fn remove_current_thread(&self) {
        let thread = current_thread();
        let mut threads = self.threads.lock();
        let idx = thread.index_in_thread_list.load(Ordering::Relaxed);
        let last = threads.pop().expect("no threads left");
        if idx != threads.len() {
            last.index_in_thread_list.store(idx, Ordering::Relaxed);
            threads[idx] = last;
        }

        self.cv_join.notify_all();
    }

    /// All currently registered threads. The profiler's arm and
    /// aggregator drain enumerate through snapshots, each `Arc` keeps
    /// its thread (and sample ring) alive past unbind.
    pub(crate) fn snapshot_threads(&self) -> Vec<Arc<Thread>> {
        self.threads.lock().clone()
    }

    pub fn block_all_mutators_for_gc(&self) {
        let mut handshake_threads = self.handshake_threads.lock();

        loop {
            let threads = self.threads.lock();
            for thread in threads.iter() {
                handshake_threads.push(thread.clone());
            }

            drop(threads);

            handshake_threads.retain(|t| {
                let blocked_for_gc = GC_BLOCK_ADAPTER.is_blocked(t)
                    || t.block(&GC_BLOCK_ADAPTER, true).not_running();

                !blocked_for_gc
            });

            if handshake_threads.is_empty() {
                log::debug!("No mutators to block for GC");
                break;
            }

            for thread in handshake_threads.drain(..) {
                thread.block(&GC_BLOCK_ADAPTER, false);
            }
        }
    }

    pub fn resume_all_mutators_from_gc(&self) {
        let threads = self.threads.lock();
        let mut handshake_threads = self.handshake_threads.lock();

        for thread in threads.iter() {
            if thread.active_mutator_context() {
                handshake_threads.push(thread.clone());
            }
        }

        drop(threads);

        for thread in handshake_threads.drain(..) {
            thread.unblock(&GC_BLOCK_ADAPTER);
        }
    }

    pub fn handshake_threads(&self) -> parking_lot::MutexGuard<'_, Vec<Arc<Thread>>> {
        self.handshake_threads.lock()
    }

    pub fn threads(&self) -> parking_lot::MutexGuard<'_, Vec<Arc<Thread>>> {
        self.threads.lock()
    }

    pub(crate) unsafe fn flush_tlabs(&self) {
        let threads = self.threads.lock();
        for thread in threads.iter() {
            if thread.is_mutator() {
                thread.flush_tlab();
            }
        }
    }
}

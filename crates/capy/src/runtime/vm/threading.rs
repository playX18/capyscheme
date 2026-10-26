//! Multi-threading support for CapyScheme.
//!
//! Mutexes and conditions are owned by virtual threads (or, for threads that are
//! not virtual threads, by the OS thread). A contended acquire or condition wait
//! parks the caller on a FIFO queue kept by the scheduler (see
//! `runtime/sched/sync.rs`), so no OS thread blocks on a virtual thread's behalf.

use crate::heap::Space;
use std::{
    collections::VecDeque,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicPtr, AtomicU64, AtomicUsize, Ordering},
    },
    time::Instant,
};

use crate::heap::{
    Mutation,
    barrier::{self},
    cell::Lock,
    object::{ClassId, builtin_class_ids, class_header_word},
    sync::thread::{Thread as RuntimeThread, current_thread as current_runtime_thread},
};
use crate::prelude::*;
use crate::runtime::MutationContext;
use crate::runtime::root::RootScope;
use crate::runtime::sched::{
    scheduler::{self, Killed, SchedCtx, SchemeStart},
    sync::{self, Waiter, current_token},
};

/// Condition variable. Its waiters are queued in the scheduler, keyed by the
/// address of this (non-moving) object.
#[repr(C)]
pub struct Condition {
    _unused: u64,
}

fn condition_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::CONDITION).expect("builtin class id is nonzero"),
    )
}

// SAFETY: GC trace for `Condition`, all reachable heap fields are visited
unsafe impl Trace for Condition {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl Condition {
    fn key(&self) -> usize {
        self as *const Condition as usize
    }
}

/// Mutex. The owner is a thread id (or a thread token), and the count is
/// the owner's reentrancy depth. Only the owner changes `count`, and only
/// under `state`.
#[repr(C)]
pub struct Mutex {
    reentrant: bool,
    state: parking_lot::Mutex<MutexState>,
}

#[derive(Default)]
struct MutexState {
    /// Owner token, `0` when unowned.
    owner: u64,
    count: usize,
    /// Set when the owner died holding the lock. The next acquirer takes it
    /// and reports it once (SRFI-18 abandoned-mutex).
    abandoned: bool,
}

fn mutex_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::MUTEX).expect("builtin class id is nonzero"))
}

impl Mutex {
    fn new(reentrant: bool) -> Self {
        Self {
            reentrant,
            state: parking_lot::Mutex::new(MutexState::default()),
        }
    }

    fn key(&self) -> usize {
        self as *const Mutex as usize
    }

    /// Take the lock if it is free, or if the caller already owns it and the
    /// mutex is reentrant.
    fn try_acquire(&self, token: u64) -> bool {
        let mut st = self.state.lock();
        if st.owner == 0 {
            st.owner = token;
            st.count = 1;
            true
        } else if self.reentrant && st.owner == token {
            st.count += 1;
            true
        } else {
            false
        }
    }

    /// Like [`try_acquire`](Self::try_acquire), but when the lock is held by
    /// someone else, queue `waiter` before releasing the state lock, so a
    /// release cannot miss it.
    fn acquire_or_enqueue(&self, waiter: &Arc<Waiter>) -> bool {
        let mut st = self.state.lock();
        if st.owner == 0 {
            st.owner = waiter.token;
            st.count = 1;
            true
        } else if self.reentrant && st.owner == waiter.token {
            st.count += 1;
            true
        } else {
            sync::enqueue(self.key(), waiter.clone());
            false
        }
    }

    /// Give the lock to the first queued waiter, or free it. Called with the
    /// state lock held. The grant is recorded here; the caller wakes the
    /// returned waiter after dropping the lock.
    fn hand_off(&self, st: &mut MutexState) -> Option<Arc<Waiter>> {
        st.owner = 0;
        st.count = 0;
        let next = sync::grant_first(self.key())?;
        st.owner = next.token;
        st.count = 1;
        Some(next)
    }

    /// Release one level held by `token`. Returns `false` when the caller does
    /// not own the mutex.
    fn release(&self, token: u64) -> bool {
        let mut st = self.state.lock();
        if st.owner != token || st.count == 0 {
            return false;
        }
        if st.count > 1 {
            st.count -= 1;
            return true;
        }
        let next = self.hand_off(&mut st);
        drop(st);
        if let Some(next) = next {
            next.wake();
        }
        true
    }

    /// Release the mutex and queue `waiter` on `cond` as one step, so a signal
    /// after the release always finds the waiter.
    fn release_into_condition(&self, token: u64, cond: usize, waiter: &Arc<Waiter>) -> bool {
        let mut st = self.state.lock();
        if st.owner != token || st.count != 1 {
            return false;
        }
        sync::enqueue(cond, waiter.clone());
        let next = self.hand_off(&mut st);
        drop(st);
        if let Some(next) = next {
            next.wake();
        }
        true
    }

    /// Withdraw a queued acquire. Returns `true` when the waiter was still
    /// queued and is now withdrawn, and `false` when a grant had already
    /// given the lock to it, in which case the caller owns the lock.
    fn cancel_acquire(&self, waiter: &Arc<Waiter>) -> bool {
        let _st = self.state.lock();
        sync::cancel(self.key(), waiter)
    }

    fn is_owned_by(&self, token: u64) -> bool {
        let st = self.state.lock();
        st.owner == token && st.count > 0
    }

    /// Clear and return the abandoned flag. Called by the new owner, so no
    /// other thread can be changing the lock state under it.
    fn take_abandoned(&self) -> bool {
        std::mem::take(&mut self.state.lock().abandoned)
    }

    /// Flag the mutex abandoned again, for an owner that reacquired it but
    /// cannot report the abandonment itself.
    fn restore_abandoned(&self) {
        self.state.lock().abandoned = true;
    }

    /// Force-release the lock held by `token`, which is dying, and flag it
    /// abandoned. The next waiter is granted the lock and returned for waking.
    fn abandon(&self, token: u64) -> Option<Arc<Waiter>> {
        let mut st = self.state.lock();
        if st.owner != token || st.count == 0 {
            return None;
        }
        st.abandoned = true;
        self.hand_off(&mut st)
    }
}

// SAFETY: GC trace for `Mutex`, all reachable heap fields are visited
unsafe impl Trace for Mutex {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Mutex`
unsafe impl ClassTagged for Mutex {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::MUTEX];

    const TYPE_NAME: &'static str = "mutex";
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Condition`
unsafe impl ClassTagged for Condition {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONDITION];

    const TYPE_NAME: &'static str = "condition";
}

#[repr(C)]
pub struct ThreadObject<'gc> {
    entrypoint: Lock<Option<Value<'gc>>>,
    /// Allocated by the first `enqueue_interrupt`: most threads never get one,
    /// and the box is a malloc on every spawn otherwise.
    pending_interrupts: AtomicPtr<PendingInterruptQueue>,
    /// The native thread this object interrupts, for threads that are not
    /// threads. Zero when unused.
    runtime_thread: AtomicUsize,
    /// The thread this object interrupts. Zero when unbound. VirtualThread ids
    /// start at 1, so zero is never a live thread.
    virtual_thread: AtomicU64,
    interrupt_level: AtomicUsize,
    /// Set when the thread's thread has finished. Joiners wait on it.
    done: parking_lot::Mutex<bool>,
    /// The escape continuation of the thread's body, which a kill invokes to
    /// unwind the Scheme stack through `dynamic-wind` after-thunks.
    unwind_k: Lock<Option<Value<'gc>>>,
    /// Set when the thread was killed (`thread-terminate!`).
    killed: AtomicBool,
    /// VirtualThread a kill is sent to before the thread has bound itself. Set at
    /// spawn, so a kill can reach a thread that has not run yet.
    kill_target: AtomicU64,
    /// Mutexes this thread holds, by address, rooted here. A thread that
    /// finishes while holding one has it force-released and abandoned.
    held: parking_lot::Mutex<Vec<(usize, Value<'static>)>>,
    /// Key of this thread's join queue in the wait-queue table. A counter
    /// value with the top bit set, so it is stable when the collector moves
    /// the object and cannot equal the address of a mutex or condition.
    join_key: usize,
}

/// Source of [`ThreadObject::join_key`] values.
static NEXT_JOIN_KEY: AtomicUsize = AtomicUsize::new(0);

struct PendingInterruptQueue {
    queue: parking_lot::Mutex<VecDeque<Value<'static>>>,
}

fn thread_object_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::THREAD).expect("builtin class id is nonzero"))
}

impl<'gc> ThreadObject<'gc> {
    pub(crate) fn new(
        mc: impl Into<Mutation<'gc>>,
        entrypoint: Option<Value<'gc>>,
    ) -> Gc<'gc, Self> {
        let mc = mc.into();
        // Not Los: a large object costs a page of zeroed memory and the
        // large-object lock per thread, which dominates spawning a thread.
        // The object may move: waiters queue on `join_key`, not its address,
        // and no `&ThreadObject` is held across a safepoint.
        mc.allocate_in(
            Space::Default,
            ThreadObject {
                entrypoint: Lock::new(entrypoint),
                pending_interrupts: AtomicPtr::new(std::ptr::null_mut()),
                runtime_thread: AtomicUsize::new(0),
                virtual_thread: AtomicU64::new(0),
                interrupt_level: AtomicUsize::new(0),
                done: parking_lot::Mutex::new(false),
                unwind_k: Lock::new(None),
                killed: AtomicBool::new(false),
                kill_target: AtomicU64::new(0),
                held: parking_lot::Mutex::new(Vec::new()),
                join_key: (1 << (usize::BITS - 1)) | NEXT_JOIN_KEY.fetch_add(1, Ordering::Relaxed),
            },
            thread_object_header_word(),
        )
    }

    /// Bind interrupts to the running thread, or to the native thread when
    /// the caller is not a thread.
    pub(crate) fn bind_current_runtime_thread(&self) {
        match scheduler::current() {
            Some(cx) => {
                self.runtime_thread.store(0, Ordering::Release);
                self.virtual_thread
                    .store(cx.id().as_u64(), Ordering::Release);
            }
            None => {
                self.virtual_thread.store(0, Ordering::Release);
                self.runtime_thread.store(
                    current_runtime_thread() as *const RuntimeThread as usize,
                    Ordering::Release,
                );
            }
        }
    }

    /// The thread this object is bound to, or zero.
    pub(crate) fn virtual_thread_id(&self) -> u64 {
        self.virtual_thread.load(Ordering::Acquire)
    }

    /// Whether interrupts can reach this object's thread.
    pub(crate) fn is_bound(&self) -> bool {
        self.virtual_thread.load(Ordering::Acquire) != 0
            || self.runtime_thread.load(Ordering::Acquire) != 0
    }

    /// Request a yieldpoint on the thread this object interrupts. For a
    /// thread that is the thread itself, wherever it runs or waits.
    pub(crate) fn request_interrupt_yieldpoint(&self) {
        let id = self.virtual_thread.load(Ordering::Acquire);
        if id != 0 {
            scheduler::interrupt_virtual_thread(scheduler::VirtualThreadId::from_u64(id));
            return;
        }
        let ptr = self.runtime_thread.load(Ordering::Acquire);
        if ptr != 0 {
            // SAFETY: the native thread's handle lives as long as the thread,
            // and this object is only bound while that thread runs.
            let thread = unsafe { &*(ptr as *const RuntimeThread) };
            thread.request_yieldpoint(crate::heap::sync::thread::YieldpointKind::Full);
        }
    }

    /// The thread a kill of this thread is sent to, or zero.
    pub(crate) fn kill_target_id(&self) -> u64 {
        match self.virtual_thread_id() {
            0 => self.kill_target.load(Ordering::Acquire),
            id => id,
        }
    }

    pub(crate) fn set_kill_target(&self, id: u64) {
        self.kill_target.store(id, Ordering::Release);
    }

    pub(crate) fn mark_killed(&self) {
        self.killed.store(true, Ordering::Release);
    }

    pub(crate) fn is_killed(&self) -> bool {
        self.killed.load(Ordering::Acquire)
    }

    /// Record that this thread holds `mutex`. A mutex is recorded once, however
    /// deeply it is reentered.
    pub(crate) fn hold(&self, mutex: Value<'gc>) {
        let key = mutex.raw_i64() as usize;
        let mut held = self.held.lock();
        if held.iter().any(|(k, _)| *k == key) {
            return;
        }
        // SAFETY: the value is rooted by `held` until it is released.
        let mutex = unsafe { std::mem::transmute::<Value<'gc>, Value<'static>>(mutex) };
        held.push((key, mutex));
    }

    /// Forget `mutex`, which this thread no longer holds.
    pub(crate) fn unhold(&self, mutex: Value<'gc>) {
        let key = mutex.raw_i64() as usize;
        self.held.lock().retain(|(k, _)| *k != key);
    }

    /// The mutexes this thread still holds, as objects.
    fn held_mutexes(&self) -> Vec<Value<'gc>> {
        self.held
            .lock()
            .iter()
            // SAFETY: the value was stored with its lifetime erased, and `held`
            // keeps it alive for this object's lifetime.
            .map(|(_, v)| unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*v) })
            .collect()
    }

    fn clear_held(&self) {
        self.held.lock().clear();
    }

    /// Key of this thread's join queue.
    fn join_key(&self) -> usize {
        self.join_key
    }

    /// Mark the thread finished and wake everyone waiting to join it.
    pub(crate) fn finish(&self) {
        let mut woken = Vec::new();
        {
            let mut done = self.done.lock();
            *done = true;
            while let Some(waiter) = sync::grant_first(self.join_key()) {
                woken.push(waiter);
            }
        }
        for waiter in woken {
            waiter.wake();
        }
    }

    /// Park the calling context until `thread` has finished. Returns at once if
    /// it already has. `Err(Killed)` when the caller is killed while waiting.
    pub(crate) fn join_wait<'gc_>(
        ctx: MutationContext<'gc_>,
        thread: Gc<'gc_, ThreadObject<'gc_>>,
    ) -> (MutationContext<'gc_>, Result<(), Killed>) {
        let scope = RootScope::new(ctx);
        let thread_root = scope.root(thread.into());
        let thread: Gc<'gc_, ThreadObject<'gc_>> = thread_root.get().downcast();
        let waiter = Waiter::current();
        let key = thread.join_key();
        {
            let done = thread.done.lock();
            if *done {
                return (ctx, Ok(()));
            }
            sync::enqueue(key, waiter.clone());
        }
        let (ctx, blocked) = block_until_granted(ctx, &waiter, None, false, true);
        match blocked {
            Ok(_) => (ctx, Ok(())),
            Err(killed) => {
                sync::cancel(key, &waiter);
                (ctx, Err(killed))
            }
        }
    }

    pub(crate) fn enqueue_interrupt(&self, thunk: Value<'gc>) {
        // SAFETY: Source and destination types have compatible layouts and sizes
        let thunk = unsafe { std::mem::transmute::<Value<'gc>, Value<'static>>(thunk) };
        self.interrupt_queue().queue.lock().push_back(thunk);
        self.request_interrupt_yieldpoint();
    }

    pub(crate) fn pop_pending_interrupt(&self) -> Option<Value<'gc>> {
        self.interrupt_queue_if_any()?
            .queue
            .lock()
            .pop_front()
            // SAFETY: Source and destination types have compatible layouts and sizes
            .map(|value| unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(value) })
    }

    pub(crate) fn has_pending_interrupts(&self) -> bool {
        self.interrupt_queue_if_any()
            .is_some_and(|q| !q.queue.lock().is_empty())
    }

    fn interrupt_queue_if_any(&self) -> Option<&PendingInterruptQueue> {
        // SAFETY: a non-null queue lives until the object is dropped.
        unsafe { self.pending_interrupts.load(Ordering::Acquire).as_ref() }
    }

    fn interrupt_queue(&self) -> &PendingInterruptQueue {
        if let Some(queue) = self.interrupt_queue_if_any() {
            return queue;
        }
        let fresh = Box::into_raw(Box::new(PendingInterruptQueue {
            queue: parking_lot::Mutex::new(VecDeque::new()),
        }));
        match self.pending_interrupts.compare_exchange(
            std::ptr::null_mut(),
            fresh,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            // SAFETY: just published, and never freed before the object drops.
            Ok(_) => unsafe { &*fresh },
            Err(existing) => {
                // SAFETY: `fresh` was never shared.
                drop(unsafe { Box::from_raw(fresh) });
                // SAFETY: `existing` is the published queue.
                unsafe { &*existing }
            }
        }
    }

    pub(crate) fn mask_interrupts(&self) {
        self.interrupt_level.fetch_add(1, Ordering::AcqRel);
    }

    pub(crate) fn unmask_interrupts(&self) {
        let old = self.interrupt_level.fetch_sub(1, Ordering::AcqRel);
        assert!(old > 0, "interrupt level underflow");
        if old == 1 && self.has_pending_interrupts() {
            self.request_interrupt_yieldpoint();
        }
    }

    pub(crate) fn interrupt_level(&self) -> usize {
        self.interrupt_level.load(Ordering::Acquire)
    }
}

// SAFETY: `gc` for `ThreadObject` upholds all trait invariants
unsafe impl<'gc> Trace for ThreadObject<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.entrypoint.trace(visitor);
            self.unwind_k.trace(visitor);
            for (_, value) in self.held.get_mut().iter_mut() {
                visitor.trace(value);
            }
            if let Some(queue) = self.pending_interrupts.get_mut().as_mut() {
                for value in queue.queue.get_mut().iter_mut() {
                    visitor.trace(value);
                }
            }
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.entrypoint.process_weak_refs(weak_processor);
            self.unwind_k.process_weak_refs(weak_processor);
            if let Some(queue) = self.pending_interrupts.get_mut().as_mut() {
                for value in queue.queue.get_mut().iter_mut() {
                    value.process_weak_refs(weak_processor);
                }
            }
        }
    }
}

// SAFETY: `gc` for `ThreadObject` upholds all trait invariants
unsafe impl<'gc> ClassTagged for ThreadObject<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::THREAD];

    const TYPE_NAME: &'static str = "thread";
}

impl Drop for ThreadObject<'_> {
    fn drop(&mut self) {
        let queue = std::mem::replace(self.pending_interrupts.get_mut(), std::ptr::null_mut());
        if !queue.is_null() {
            // SAFETY: created by `Box::into_raw` in `interrupt_queue`.
            drop(unsafe { Box::from_raw(queue) });
        }
    }
}

#[scheme(path=capy)]
pub mod threading_ops {
    #[scheme(name = "%native-fork-thread")]
    pub fn fork_thread(thunk: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        let thread_obj = ThreadObject::new(ctx, Some(thunk.into()));
        let dynamic = ctx.dynamic_state();
        let start = SchemeStart::new(thread_obj.into(), dynamic);
        let id = scheduler::spawn_scheme(start, run_scheme_thread);
        thread_obj.set_kill_target(id.as_u64());
        thread_obj.into()
    }

    /// Kill the thread a thread runs as (`thread-terminate!`). Returns
    /// `#f` when there is nothing to kill or it is already being killed.
    #[scheme(name = "%native-thread-kill!")]
    pub fn thread_kill(thread: Gc<'gc, ThreadObject<'gc>>) -> bool {
        if Gc::ptr_eq(thread, ctx.state().thread_object) {
            if scheduler::kill_current() {
                Killed.unwind();
            }
            return false;
        }
        match thread.kill_target_id() {
            0 => false,
            id => scheduler::kill_virtual_thread(scheduler::VirtualThreadId::from_u64(id)),
        }
    }

    #[scheme(name = "%native-thread-killed?")]
    pub fn thread_killed_p(thread: Gc<'gc, ThreadObject<'gc>>) -> bool {
        thread.is_killed()
    }

    /// Set the escape continuation that a kill of this thread invokes. The
    /// thread body calls it before it runs user code.
    #[scheme(name = "%native-set-thread-unwind-k!")]
    pub fn set_thread_unwind_k(k: Value<'gc>) -> Value<'gc> {
        let thread = ctx.state().thread_object;
        barrier::field!(Gc::write(ctx, thread), ThreadObject, unwind_k)
            .unlock()
            .set(Some(k));
        Value::undefined()
    }

    #[scheme(name = "%native-thread-id")]
    pub fn native_thread_id(thread: Gc<'gc, ThreadObject<'gc>>) -> usize {
        thread.virtual_thread_id() as usize
    }

    #[scheme(name = "%thread-id")]
    pub fn virtual_thread_id() -> usize {
        crate::runtime::sched::scheduler::current().map_or(0, |cx| cx.id().as_u64() as usize)
    }

    /// Yield the running thread. Does nothing outside a thread.
    #[scheme(name = "%thread-yield")]
    pub fn virtual_thread_yield() -> Value<'gc> {
        if let Some(cx) = crate::runtime::sched::scheduler::current() {
            cx.yield_switch();
        }
        Value::undefined()
    }

    /// Index of the worker thread running the calling thread, or #f outside
    /// threads. Worker 0 is the process's main thread.
    #[scheme(name = "%thread-worker")]
    pub fn virtual_thread_worker() -> Value<'gc> {
        match crate::runtime::sched::scheduler::current() {
            Some(cx) => Value::new(cx.worker_index() as i32),
            None => Value::new(false),
        }
    }

    /// Number of scheduler workers, or 0 outside threads.
    #[scheme(name = "%thread-worker-count")]
    pub fn virtual_thread_worker_count() -> usize {
        crate::runtime::sched::scheduler::current().map_or(0, |cx| cx.worker_count())
    }

    /// The worker the calling thread is restricted to, or #f.
    #[scheme(name = "%thread-pinned")]
    pub fn virtual_thread_pinned() -> Value<'gc> {
        match crate::runtime::sched::scheduler::current().and_then(|cx| cx.affinity()) {
            Some(w) => Value::new(w as i32),
            None => Value::new(false),
        }
    }

    /// Number of priority levels. Level 0 is the most urgent.
    #[scheme(name = "%thread-priority-levels")]
    pub fn virtual_thread_priority_levels() -> usize {
        crate::runtime::sched::scheduler::NLEVELS as usize
    }

    /// The calling thread's base priority, or #f outside threads.
    #[scheme(name = "%thread-priority")]
    pub fn virtual_thread_priority() -> Value<'gc> {
        match crate::runtime::sched::scheduler::current() {
            Some(cx) => Value::new(i32::from(cx.priority())),
            None => Value::new(false),
        }
    }

    /// Set the calling thread's base priority. `level` is an integer already
    /// checked to be in range. Does nothing outside a thread.
    #[scheme(name = "%thread-set-priority!")]
    pub fn virtual_thread_set_priority(level: Value<'gc>) -> Value<'gc> {
        if let Some(cx) = crate::runtime::sched::scheduler::current() {
            if level.is_int32() && level.as_int32() >= 0 {
                cx.set_priority(level.as_int32().min(u8::MAX as i32) as u8);
            }
        }
        Value::undefined()
    }

    /// Restrict the calling thread to worker `worker` and move it there, yielding
    /// until it runs on that worker. `#f` lifts the restriction.
    #[scheme(name = "%thread-pin!")]
    pub fn virtual_thread_pin(worker: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        use crate::runtime::sched::scheduler::PinError;
        let fail = |why: &str| {
            call.raise_error("pin", why, &[worker]);
            Err(Value::undefined())
        };
        let Some(cx) = crate::runtime::sched::scheduler::current() else {
            return fail("not running in a thread");
        };
        let target = if worker.is_bool() && !worker.as_bool() {
            None
        } else if worker.is_int32() && worker.as_int32() >= 0 {
            Some(worker.as_int32() as usize)
        } else {
            return fail("expected a worker index or #f");
        };
        match cx.pin_to_worker(target) {
            Ok(()) => Ok(Value::undefined()),
            Err(PinError::NoSuchWorker) => fail("no such worker"),
            Err(PinError::InNativeFrame) => {
                fail("cannot move to another worker from inside a native call")
            }
        }
    }

    /// Sleep for `seconds`. Parks the thread, or blocks the OS thread when
    /// the caller is not a virtual thread. Pending interrupts run during the sleep.
    #[scheme(name = "%thread-sleep")]
    pub fn virtual_thread_sleep(seconds: f64) -> Value<'gc> {
        let secs = if seconds.is_finite() {
            seconds.max(0.0)
        } else {
            0.0
        };
        let deadline = Instant::now() + std::time::Duration::from_secs_f64(secs);
        let mut ctx = ctx;
        while Instant::now() < deadline {
            ctx = match scheduler::current() {
                Some(cx) => {
                    let (next, woken) = cx.sleep_until(ctx, deadline);
                    woken.unwrap_or_else(|killed| killed.unwind());
                    next
                }
                None => {
                    let remaining = deadline.saturating_duration_since(Instant::now());
                    ctx.call_in_native(|| std::thread::sleep(remaining));
                    ctx
                }
            };
            crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
        }
        Value::undefined()
    }

    #[scheme(name = "%native-thread-join")]
    pub fn thread_join(thread: Gc<'gc, ThreadObject<'gc>>) -> Value<'gc> {
        let (_ctx, joined) = ThreadObject::join_wait(ctx, thread);
        joined.unwrap_or_else(|killed| killed.unwind());
        Value::undefined()
    }

    #[scheme(name = "%native-thread?")]
    pub fn thread_p(obj: Value<'gc>) -> bool {
        obj.is::<ThreadObject>()
    }

    #[scheme(name = "%native-current-thread")]
    pub fn current_thread() -> Value<'gc> {
        let thread_obj = ctx.state().thread_object;
        thread_obj.bind_current_runtime_thread();
        thread_obj.into()
    }

    #[scheme(name = "make-mutex")]
    pub fn make_mutex(reentrant: Option<bool>) -> Value<'gc> {
        let mutex = Mutex::new(reentrant.unwrap_or(false));
        ctx.allocate_in(Space::Los, mutex, mutex_header_word())
            .into()
    }

    #[scheme(name = "mutex-acquire")]
    pub fn mutex_acquire(mutex_obj: Gc<'gc, Mutex>, block: Option<bool>) -> bool {
        let block = block.unwrap_or(true);
        let (ctx, acquired) = super::acquire(ctx, mutex_obj, block, true, true);
        let acquired = acquired.unwrap_or_else(|killed| killed.unwind());
        if acquired != Acquire::Failed && block {
            crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
        }
        acquired != Acquire::Failed
    }

    #[scheme(name = "%mutex-acquire-status")]
    pub fn mutex_acquire_status(mutex_obj: Gc<'gc, Mutex>, block: Option<bool>) -> usize {
        let block = block.unwrap_or(true);
        let (ctx, acquired) = super::acquire(ctx, mutex_obj, block, true, true);
        let acquired = acquired.unwrap_or_else(|killed| killed.unwind());
        if acquired != Acquire::Failed && block {
            crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
        }
        match acquired {
            Acquire::Failed => 0,
            Acquire::Acquired => 1,
            Acquire::Abandoned => 2,
        }
    }

    #[scheme(name = "mutex-release")]
    pub fn mutex_release(mutex_obj: Gc<'gc, Mutex>) -> Value<'gc> {
        let token = current_token();
        if !mutex_obj.release(token) {
            return call.wrong_argument_violation(
                "mutex-release",
                "mutex is not owned by the current thread",
                Some(mutex_obj.into()),
                Some(0),
                1,
                &[mutex_obj.into()],
            );
        }
        if !mutex_obj.is_owned_by(token) {
            ctx.state().thread_object.unhold(mutex_obj.into());
        }
        Value::undefined()
    }

    #[scheme(name = "mutex-reentrant?")]
    pub fn mutex_reentrant_p(mutex_obj: Gc<'gc, Mutex>) -> bool {
        mutex_obj.reentrant
    }

    #[scheme(name = "mutex?")]
    pub fn mutex_p(obj: Value<'gc>) -> bool {
        obj.is::<Mutex>()
    }

    #[scheme(name = "thread-condition?")]
    pub fn thread_condition_p(obj: Value<'gc>) -> bool {
        obj.is::<Condition>()
    }

    #[scheme(name = "make-condition")]
    pub fn make_condition() -> Value<'gc> {
        let condition = Condition { _unused: 0 };
        ctx.allocate_in(Space::Los, condition, condition_header_word())
            .into()
    }

    #[scheme(name = "condition-signal")]
    pub fn condition_signal(condition_obj: Gc<'gc, Condition>) -> Value<'gc> {
        if let Some(waiter) = sync::grant_first(condition_obj.key()) {
            waiter.wake();
        }
        Value::undefined()
    }

    #[scheme(name = "condition-broadcast")]
    pub fn condition_broadcast(condition_obj: Gc<'gc, Condition>) -> Value<'gc> {
        while let Some(waiter) = sync::grant_first(condition_obj.key()) {
            waiter.wake();
        }
        Value::undefined()
    }

    /// `(condition-wait condition mutex [timeout-seconds])`. Releases `mutex`
    /// and waits for a signal, then reacquires `mutex`. Returns `#t` when
    /// signalled, `#f` on timeout or when an interrupt ended the wait.
    #[scheme(name = "condition-wait")]
    pub fn condition_wait(
        condition_obj: Gc<'gc, Condition>,
        mutex_obj: Gc<'gc, Mutex>,
        timeout: Option<Value<'gc>>,
    ) -> Value<'gc> {
        if mutex_obj.reentrant {
            return call.wrong_argument_violation(
                "condition-wait",
                "reentrant mutex can't be used with condition-wait",
                Some(mutex_obj.into()),
                Some(1),
                2,
                &[condition_obj.into(), mutex_obj.into()],
            );
        }
        if !mutex_obj.is_owned_by(current_token()) {
            return call.wrong_argument_violation(
                "condition-wait",
                "mutex is not owned by the current thread",
                Some(mutex_obj.into()),
                Some(1),
                2,
                &[condition_obj.into(), mutex_obj.into()],
            );
        }
        let seconds = match timeout {
            None => None,
            Some(value) => match value.flonum().or_else(|| value.int32().map(f64::from)) {
                Some(secs) => Some(secs),
                None => {
                    return call.wrong_argument_violation(
                        "condition-wait",
                        "timeout must be a number of seconds",
                        Some(value),
                        Some(2),
                        3,
                        &[condition_obj.into(), mutex_obj.into(), value],
                    );
                }
            },
        };
        let deadline = seconds.map(|secs| {
            let secs = if secs.is_finite() { secs.max(0.0) } else { 0.0 };
            Instant::now() + std::time::Duration::from_secs_f64(secs)
        });
        let (ctx, signalled) = super::wait_on_condition(ctx, condition_obj, mutex_obj, deadline);
        let signalled = signalled.unwrap_or_else(|killed| killed.unwind());
        crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
        signalled.into()
    }
}

/// How a blocking wait ended.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Blocked {
    Granted,
    TimedOut,
    Interrupted,
}

/// How an acquire ended. `Abandoned` is an acquire that took over a mutex
/// whose owner died holding it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Acquire {
    Failed,
    Acquired,
    Abandoned,
}

/// Whether an interrupt is pending and deliverable on the calling thread. A
/// dying thread ignores interrupts.
fn interrupt_pending(ctx: MutationContext<'_>) -> bool {
    if scheduler::is_dying() {
        return false;
    }
    let thread = ctx.state().thread_object;
    thread.interrupt_level() == 0 && thread.has_pending_interrupts()
}

/// Block until `waiter` is granted, `deadline` passes, or (when
/// `interruptible`) an interrupt is pending. The caller must withdraw the
/// waiter from its queue when the result is not `Granted`.
fn block_until_granted<'gc>(
    mut ctx: MutationContext<'gc>,
    waiter: &Arc<Waiter>,
    deadline: Option<Instant>,
    interruptible: bool,
    cancellable: bool,
) -> (MutationContext<'gc>, Result<Blocked, Killed>) {
    loop {
        if waiter.is_granted() {
            return (ctx, Ok(Blocked::Granted));
        }
        if deadline.is_some_and(|deadline| Instant::now() >= deadline) {
            return (ctx, Ok(Blocked::TimedOut));
        }
        if interruptible && interrupt_pending(ctx) {
            return (ctx, Ok(Blocked::Interrupted));
        }
        let (next, parked) = match scheduler::current() {
            Some(cx) => match deadline {
                Some(deadline) => {
                    let (next, woken) = cx.sleep_until(ctx, deadline);
                    (next, woken.map(|_| ()))
                }
                None => cx.park_with_mode(ctx, cancellable),
            },
            None => {
                let parker = waiter.parker().expect("thread waiter has a parker").clone();
                ctx.call_in_native(|| parker.park_until(deadline));
                (ctx, Ok(()))
            }
        };
        ctx = next;
        if let Err(killed) = parked {
            return (ctx, Err(killed));
        }
    }
}

/// The calling thread now owns `mutex`: record it, and report whether the
/// previous owner abandoned it.
fn took<'gc>(ctx: MutationContext<'gc>, mutex: Gc<'gc, Mutex>) -> Acquire {
    ctx.state().thread_object.hold(mutex.into());
    if mutex.take_abandoned() {
        Acquire::Abandoned
    } else {
        Acquire::Acquired
    }
}

/// Acquire `mutex_obj`, parking until granted. A blocking acquire that is
/// interrupted runs the pending interrupts (in `interrupts`) and retries.
fn acquire<'gc>(
    ctx: MutationContext<'gc>,
    mutex_obj: Gc<'gc, Mutex>,
    block: bool,
    interruptible: bool,
    cancellable: bool,
) -> (MutationContext<'gc>, Result<Acquire, Killed>) {
    if mutex_obj.try_acquire(current_token()) {
        return (ctx, Ok(took(ctx, mutex_obj)));
    }
    if !block {
        return (ctx, Ok(Acquire::Failed));
    }
    let scope = RootScope::new(ctx);
    let mutex_root = scope.root(mutex_obj.into());
    let mut ctx = ctx;
    loop {
        let mutex: Gc<'gc, Mutex> = mutex_root.get().downcast();
        let waiter = Waiter::current();
        if mutex.acquire_or_enqueue(&waiter) {
            return (ctx, Ok(took(ctx, mutex)));
        }
        let (next, blocked) = block_until_granted(ctx, &waiter, None, interruptible, cancellable);
        ctx = next;
        let mutex: Gc<'gc, Mutex> = mutex_root.get().downcast();
        match blocked {
            Ok(Blocked::Granted) => return (ctx, Ok(took(ctx, mutex))),
            Err(killed) => {
                if !mutex.cancel_acquire(&waiter) {
                    mutex.release(current_token());
                    ctx.state().thread_object.unhold(mutex.into());
                }
                return (ctx, Err(killed));
            }
            Ok(_) => {
                if !mutex.cancel_acquire(&waiter) {
                    return (ctx, Ok(took(ctx, mutex)));
                }
                // Interrupted before the grant: run the interrupts, then retry.
                crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
            }
        }
    }
}

/// Wait on `cond_obj`, releasing and then reacquiring `mutex_obj`. The caller
/// owns the mutex. Returns whether the wait ended with a signal, or the kill
/// that ended it. Either way the mutex is held again on return.
fn wait_on_condition<'gc>(
    ctx: MutationContext<'gc>,
    cond_obj: Gc<'gc, Condition>,
    mutex_obj: Gc<'gc, Mutex>,
    deadline: Option<Instant>,
) -> (MutationContext<'gc>, Result<bool, Killed>) {
    let scope = RootScope::new(ctx);
    let cond_root = scope.root(cond_obj.into());
    let mutex_root = scope.root(mutex_obj.into());
    let cond_key = cond_obj.key();
    let waiter = Waiter::current();
    let released = mutex_obj.release_into_condition(current_token(), cond_key, &waiter);
    debug_assert!(released, "condition-wait checked ownership");
    ctx.state().thread_object.unhold(mutex_obj.into());
    let (ctx, blocked) = block_until_granted(ctx, &waiter, deadline, true, true);
    let outcome = match blocked {
        Ok(Blocked::Granted) => Ok(true),
        // `cancel` is false when a signal took the waiter first.
        Ok(_) => Ok(!sync::cancel(cond_key, &waiter)),
        Err(killed) => {
            // A signal this waiter took is passed on to the next waiter.
            if !sync::cancel(cond_key, &waiter) {
                if let Some(next) = sync::grant_first(cond_key) {
                    next.wake();
                }
            }
            Err(killed)
        }
    };
    drop(cond_root);
    let mutex_obj: Gc<'gc, Mutex> = mutex_root.get().downcast();
    // The reacquire is not cancellable, so it returns with the lock held.
    let (ctx, reacquired) = acquire(ctx, mutex_obj, true, false, false);
    let reacquired = reacquired.expect("reacquire after a condition wait is not cancellable");
    // `condition-wait` has no way to raise it, so leave the flag for the next
    // acquirer rather than swallowing the abandonment.
    if matches!(reacquired, Acquire::Abandoned) {
        mutex_obj.restore_abandoned();
    }
    let outcome = match outcome {
        Ok(signalled) => scheduler::check_kill_now().map(|()| signalled),
        Err(killed) => Err(killed),
    };
    (ctx, outcome)
}

/// Release every mutex `thread` still holds, as its owner is finishing. Each is
/// flagged abandoned, and the next waiter is granted it.
fn abandon_held<'gc>(thread: Gc<'gc, ThreadObject<'gc>>) {
    let token = current_token();
    for value in thread.held_mutexes() {
        let mutex: Gc<'gc, Mutex> = value.downcast();
        if let Some(next) = mutex.abandon(token) {
            next.wake();
        }
    }
    thread.clear_held();
}

fn run_scheme_thread(cx: SchedCtx) {
    let body = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        cx.with_mutation(|ctx| {
            let thread = ctx.state().thread_object;
            let thunk = thread
                .entrypoint
                .get()
                .expect("fork-thread thunk is set once");
            barrier::field!(Gc::write(ctx, thread), ThreadObject, entrypoint)
                .unlock()
                .set(None);
            let _ = crate::runtime::sni::call_function(ctx, thunk, []);
        })
    }));
    if body.is_err_and(|payload| payload.is::<Killed>()) {
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            cx.with_mutation(|ctx| {
                let thread = ctx.state().thread_object;
                thread.mark_killed();
                if let Some(k) = thread.unwind_k.get() {
                    // SAFETY: `k` is a live continuation of this thread, read from
                    // its rooted object, and no other activation is in progress.
                    let _ = unsafe { crate::runtime::vm::call_scheme(ctx, k, [Value::new(false)]) };
                }
            })
        }));
    }
    cx.with_mutation(|ctx| {
        let thread = ctx.state().thread_object;
        abandon_held(thread);
        thread.finish()
    });
}

pub(crate) fn init_threading<'gc>(ctx: MutationContext<'gc>) {
    threading_ops::register(ctx);
}

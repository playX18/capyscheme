//! Multi-threading support for CapyScheme.

use std::{
    collections::VecDeque,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};

use crate::prelude::*;
use crate::rsgc::{
    Mutation,
    barrier::{self},
    cell::Lock,
    mmtk::AllocationSemantics,
    object::{ClassId, builtin_class_ids, class_header_word},
    sync::thread::{Thread as RuntimeThread, current_thread as current_runtime_thread},
};
use crate::runtime::Scheme;

#[repr(C)]
pub struct Condition {
    pub(crate) cond: parking_lot::Condvar,
}

fn condition_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::CONDITION).unwrap())
}

unsafe impl Trace for Condition {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[repr(C)]
pub struct Mutex {
    pub(crate) mutex: MutexKind,
    owner: MutexOwnerLock,
}

fn mutex_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::MUTEX).unwrap())
}

pub enum MutexKind {
    Reentrant(parking_lot::ReentrantMutex<()>),
    Regular(parking_lot::Mutex<()>),
}

#[derive(Default)]
struct MutexOwner {
    thread_id: Option<u64>,
    count: usize,
}

struct MutexOwnerLock(parking_lot::Mutex<MutexOwner>);

impl MutexOwnerLock {
    fn new() -> Self {
        Self(parking_lot::Mutex::new(MutexOwner::default()))
    }

    fn lock(&self) -> parking_lot::MutexGuard<'_, MutexOwner> {
        self.0.lock()
    }
}

impl Mutex {
    pub(crate) fn mark_acquired(&self) {
        let thread_id = current_runtime_thread().id();
        let mut owner = self.owner.lock();

        match owner.thread_id {
            Some(id) if id == thread_id => {
                owner.count += 1;
            }
            None => {
                owner.thread_id = Some(thread_id);
                owner.count = 1;
            }
            Some(_) => unreachable!("mutex acquired while marked as owned by another thread"),
        }
    }

    fn mark_released(&self) -> bool {
        let thread_id = current_runtime_thread().id();
        let mut owner = self.owner.lock();

        if owner.thread_id != Some(thread_id) || owner.count == 0 {
            return false;
        }

        owner.count -= 1;
        if owner.count == 0 {
            owner.thread_id = None;
        }

        true
    }

    pub(crate) fn begin_condition_wait(&self) -> bool {
        let thread_id = current_runtime_thread().id();
        let mut owner = self.owner.lock();

        if owner.thread_id != Some(thread_id) || owner.count != 1 {
            return false;
        }

        owner.thread_id = None;
        owner.count = 0;
        true
    }

    pub(crate) fn finish_condition_wait(&self) {
        let thread_id = current_runtime_thread().id();
        let mut owner = self.owner.lock();
        debug_assert!(owner.thread_id.is_none());
        debug_assert_eq!(owner.count, 0);
        owner.thread_id = Some(thread_id);
        owner.count = 1;
    }

    fn is_owned_by_current_thread(&self) -> bool {
        let thread_id = current_runtime_thread().id();
        let owner = self.owner.lock();
        owner.thread_id == Some(thread_id) && owner.count > 0
    }
}

unsafe impl Trace for Mutex {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl ClassTagged for Mutex {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::MUTEX];

    const TYPE_NAME: &'static str = "mutex";
}

unsafe impl ClassTagged for Condition {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::CONDITION];

    const TYPE_NAME: &'static str = "condition";
}

#[repr(C)]
pub struct ThreadObject<'gc> {
    entrypoint: Lock<Option<Value<'gc>>>,
    pending_interrupts: *mut PendingInterruptQueue,
    runtime_thread: AtomicUsize,
    interrupt_level: AtomicUsize,
}

struct PendingInterruptQueue {
    queue: parking_lot::Mutex<VecDeque<Value<'static>>>,
}

fn thread_object_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::THREAD).unwrap())
}

impl<'gc> ThreadObject<'gc> {
    pub(crate) fn new(mc: Mutation<'gc>, entrypoint: Option<Value<'gc>>) -> Gc<'gc, Self> {
        Gc::new_with_header_word(
            mc,
            ThreadObject {
                entrypoint: Lock::new(entrypoint),
                pending_interrupts: Box::into_raw(Box::new(PendingInterruptQueue {
                    queue: parking_lot::Mutex::new(VecDeque::new()),
                })),
                runtime_thread: AtomicUsize::new(0),
                interrupt_level: AtomicUsize::new(0),
            },
            thread_object_header_word(),
        )
    }

    pub(crate) fn bind_current_runtime_thread(&self) {
        self.runtime_thread.store(
            current_runtime_thread() as *const RuntimeThread as usize,
            Ordering::Release,
        );
    }

    pub(crate) fn runtime_thread(&self) -> Option<&'static RuntimeThread> {
        let ptr = self.runtime_thread.load(Ordering::Acquire);
        if ptr == 0 {
            None
        } else {
            Some(unsafe { &*(ptr as *const RuntimeThread) })
        }
    }

    pub(crate) fn request_interrupt_yieldpoint(&self) {
        if let Some(thread) = self.runtime_thread() {
            thread.request_yieldpoint();
        }
    }

    pub(crate) fn enqueue_interrupt(&self, thunk: Value<'gc>) {
        let thunk = unsafe { std::mem::transmute::<Value<'gc>, Value<'static>>(thunk) };
        unsafe { &*self.pending_interrupts }
            .queue
            .lock()
            .push_back(thunk);
        self.request_interrupt_yieldpoint();
    }

    pub(crate) fn pop_pending_interrupt(&self) -> Option<Value<'gc>> {
        unsafe { &*self.pending_interrupts }
            .queue
            .lock()
            .pop_front()
            .map(|value| unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(value) })
    }

    pub(crate) fn has_pending_interrupts(&self) -> bool {
        !unsafe { &*self.pending_interrupts }.queue.lock().is_empty()
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

unsafe impl<'gc> Trace for ThreadObject<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        unsafe {
            self.entrypoint.trace(visitor);
            for value in self
                .pending_interrupts
                .as_mut()
                .expect("pending interrupt queue is initialized")
                .queue
                .get_mut()
                .iter_mut()
            {
                visitor.trace(value);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.entrypoint.process_weak_refs(weak_processor);
            for value in self
                .pending_interrupts
                .as_mut()
                .expect("pending interrupt queue is initialized")
                .queue
                .get_mut()
                .iter_mut()
            {
                value.process_weak_refs(weak_processor);
            }
        }
    }
}

unsafe impl<'gc> ClassTagged for ThreadObject<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::THREAD];

    const TYPE_NAME: &'static str = "thread";
}

impl Drop for ThreadObject<'_> {
    fn drop(&mut self) {
        if !self.pending_interrupts.is_null() {
            unsafe {
                drop(Box::from_raw(self.pending_interrupts));
            }
            self.pending_interrupts = std::ptr::null_mut();
        }
    }
}

#[scheme(path=capy)]
pub mod threading_ops {
    #[scheme(name = "fork-thread")]
    pub fn fork_thread(thunk: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        let thread_obj = ThreadObject::new(*nctx.ctx, Some(thunk.into()));

        let val: Value<'gc> = thread_obj.into();
        let bits = val.bits();
        let dynamic_state = nctx.ctx.state().dynamic_state.save(nctx.ctx);
        let dynamic_state_bits = dynamic_state.bits();
        let (thread_started, thread_started_rx) = std::sync::mpsc::channel();

        let _handle = std::thread::spawn(move || {
            let mut reported_start = false;
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let scm = Scheme::forked(bits, dynamic_state_bits);
                reported_start = true;
                let _ = thread_started.send(true);

                scm.call_value(
                    |ctx, _args| {
                        let thread_obj = ctx.state().thread_object;
                        let thunk = thread_obj.entrypoint.get().unwrap();
                        barrier::field!(Gc::write(*ctx, thread_obj), ThreadObject, entrypoint)
                            .unlock()
                            .set(None);
                        thunk
                    },
                    |ctx, result| {
                        let _ = ctx;
                        let _ = result;
                    },
                )
            }));

            if let Err(err) = result {
                if !reported_start {
                    let _ = thread_started.send(false);
                }
                std::panic::resume_unwind(err);
            }
        });

        if !thread_started_rx.recv().unwrap_or(false) {
            return nctx.raise_error("fork-thread", "failed to start thread", &[]);
        }

        nctx.return_(val)
    }

    #[scheme(name = "thread?")]
    pub fn thread_p(obj: Value<'gc>) -> bool {
        nctx.return_(obj.is::<ThreadObject>())
    }

    #[scheme(name = "current-thread")]
    pub fn current_thread() -> Value<'gc> {
        let thread_obj = nctx.ctx.state().thread_object;
        thread_obj.bind_current_runtime_thread();
        nctx.return_(thread_obj.into())
    }

    #[scheme(name = "make-mutex")]
    pub fn make_mutex(reentrant: Option<bool>) -> Value<'gc> {
        let reentrant = reentrant.unwrap_or(false);
        let mutex = Mutex {
            mutex: if reentrant {
                MutexKind::Reentrant(parking_lot::ReentrantMutex::new(()))
            } else {
                MutexKind::Regular(parking_lot::Mutex::new(()))
            },
            owner: MutexOwnerLock::new(),
        };
        // mutex has to be non-moving so that parking_lot can use its address as a key.
        let gc_mutex = nctx.ctx.allocate_with_header_word(
            mutex,
            mutex_header_word(),
            AllocationSemantics::NonMoving,
        );
        nctx.return_(gc_mutex.into())
    }
    #[scheme(name = "mutex-acquire")]
    pub fn mutex_acquire(mutex_obj: Gc<'gc, Mutex>, block: Option<bool>) -> bool {
        let block = block.unwrap_or(true);
        // fast path: try to acquire the lock without yielding to native

        match &mutex_obj.mutex {
            MutexKind::Reentrant(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    mutex_obj.mark_acquired();
                    std::mem::forget(_guard);
                    return nctx.return_(true);
                }
            }
            MutexKind::Regular(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    mutex_obj.mark_acquired();
                    std::mem::forget(_guard);
                    return nctx.return_(true);
                }
            }
        }

        if block {
            let mutex_ptr = NonNull::from(&*mutex_obj);
            let thread = nctx.ctx.state().thread_object;
            loop {
                nctx.ctx.outside_gc_world(|| {
                    let mutex_obj = unsafe { mutex_ptr.as_ref() };
                    loop {
                        match &mutex_obj.mutex {
                            MutexKind::Reentrant(mutex) => {
                                if let Some(_guard) = mutex.try_lock() {
                                    mutex_obj.mark_acquired();
                                    std::mem::forget(_guard);
                                    break;
                                }
                            }
                            MutexKind::Regular(mutex) => {
                                if let Some(_guard) = mutex.try_lock() {
                                    mutex_obj.mark_acquired();
                                    std::mem::forget(_guard);
                                    break;
                                }
                            }
                        }
                        if thread.has_pending_interrupts() {
                            break;
                        }
                        std::thread::sleep(Duration::from_millis(1));
                    }
                });
                crate::runtime::vm::interrupts::deliver_pending_interrupts(nctx.ctx);
                if mutex_obj.is_owned_by_current_thread() {
                    return nctx.return_(true);
                }
            }
        } else {
            match &mutex_obj.mutex {
                MutexKind::Reentrant(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
                        mutex_obj.mark_acquired();
                        std::mem::forget(_guard);
                        return nctx.return_(true);
                    }
                }
                MutexKind::Regular(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
                        mutex_obj.mark_acquired();
                        std::mem::forget(_guard);
                        return nctx.return_(true);
                    }
                }
            }
            return nctx.return_(false);
        }
    }

    #[scheme(name = "mutex-release")]
    pub fn mutex_release(mutex_obj: Gc<'gc, Mutex>) -> Value<'gc> {
        if !mutex_obj.mark_released() {
            return nctx.wrong_argument_violation(
                "mutex-release",
                "mutex is not owned by the current thread",
                Some(mutex_obj.into()),
                Some(0),
                1,
                &[mutex_obj.into()],
            );
        }

        match &mutex_obj.mutex {
            MutexKind::Reentrant(mutex) => unsafe {
                let guard = mutex.make_guard_unchecked();
                drop(guard);
            },
            MutexKind::Regular(mutex) => unsafe {
                let guard = mutex.make_guard_unchecked();
                drop(guard);
            },
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "mutex-reentrant?")]
    pub fn mutex_reentrant_p(mutex_obj: Gc<'gc, Mutex>) -> bool {
        match &mutex_obj.mutex {
            MutexKind::Reentrant(_) => nctx.return_(true),
            MutexKind::Regular(_) => nctx.return_(false),
        }
    }

    #[scheme(name = "mutex?")]
    pub fn mutex_p(obj: Value<'gc>) -> bool {
        nctx.return_(obj.is::<Mutex>())
    }

    #[scheme(name = "thread-condition?")]
    pub fn thread_condition_p(obj: Value<'gc>) -> bool {
        nctx.return_(obj.is::<Condition>())
    }

    #[scheme(name = "make-condition")]
    pub fn make_condition() -> Value<'gc> {
        let condition = Condition {
            cond: parking_lot::Condvar::new(),
        };
        let gc_condition = nctx.ctx.allocate_with_header_word(
            condition,
            condition_header_word(),
            AllocationSemantics::NonMoving,
        );
        nctx.return_(gc_condition.into())
    }

    #[scheme(name = "condition-signal")]
    pub fn condition_signal(condition_obj: Gc<'gc, Condition>) -> Value<'gc> {
        condition_obj.cond.notify_one();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "condition-broadcast")]
    pub fn condition_broadcast(condition_obj: Gc<'gc, Condition>) -> Value<'gc> {
        condition_obj.cond.notify_all();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "condition-wait")]
    pub fn condition_wait(
        condition_obj: Gc<'gc, Condition>,
        mutex_obj: Gc<'gc, Mutex>,
    ) -> Value<'gc> {
        if matches!(mutex_obj.mutex, MutexKind::Reentrant(_)) {
            return nctx.wrong_argument_violation(
                "condition-wait",
                "reentrant mutex can't be used with condition-wait",
                Some(mutex_obj.into()),
                Some(1),
                2,
                &[condition_obj.into(), mutex_obj.into()],
            );
        }
        if !mutex_obj.is_owned_by_current_thread() {
            return nctx.wrong_argument_violation(
                "condition-wait",
                "mutex is not owned by the current thread",
                Some(mutex_obj.into()),
                Some(1),
                2,
                &[condition_obj.into(), mutex_obj.into()],
            );
        }
        let condition = NonNull::from(&condition_obj.cond);
        let mutex_ptr = NonNull::from(&*mutex_obj);
        let thread = nctx.ctx.state().thread_object;
        nctx.ctx.outside_gc_world(|| {
            let mutex_obj = unsafe { mutex_ptr.as_ref() };
            match &mutex_obj.mutex {
                MutexKind::Regular(mutex) => unsafe {
                    assert!(mutex_obj.begin_condition_wait());
                    let mut guard = mutex.make_guard_unchecked();
                    loop {
                        let timeout = condition
                            .as_ref()
                            .wait_for(&mut guard, Duration::from_millis(10));
                        if thread.has_pending_interrupts() || !timeout.timed_out() {
                            break;
                        }
                    }
                    mutex_obj.finish_condition_wait();
                    std::mem::forget(guard);
                },
                _ => panic!("Only regular mutex can be used with condition waiting"),
            }
        });
        crate::runtime::vm::interrupts::deliver_pending_interrupts(nctx.ctx);
        nctx.return_(Value::undefined())
    }
}

pub(crate) fn init_threading<'gc>(ctx: Context<'gc>) {
    threading_ops::register(ctx);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn thread_sync_objects_allocate_with_class_only_headers() {
        Scheme::new_uninit().enter(|ctx| {
            let thread = ThreadObject::new(*ctx, None);
            assert_eq!(
                thread.as_gcobj().header().class_id(),
                ClassId::new(builtin_class_ids::THREAD).unwrap()
            );

            let mutex = Mutex {
                mutex: MutexKind::Regular(parking_lot::Mutex::new(())),
                owner: MutexOwnerLock::new(),
            };
            let mutex = ctx.allocate_with_header_word(
                mutex,
                mutex_header_word(),
                AllocationSemantics::NonMoving,
            );
            assert_eq!(
                mutex.as_gcobj().header().class_id(),
                ClassId::new(builtin_class_ids::MUTEX).unwrap()
            );

            let condition = ctx.allocate_with_header_word(
                Condition {
                    cond: parking_lot::Condvar::new(),
                },
                condition_header_word(),
                AllocationSemantics::NonMoving,
            );
            assert_eq!(
                condition.as_gcobj().header().class_id(),
                ClassId::new(builtin_class_ids::CONDITION).unwrap()
            );
        });
    }
}

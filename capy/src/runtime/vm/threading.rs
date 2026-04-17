//! Multi-threading support for CapyScheme.

use std::ptr::NonNull;

use crate::prelude::*;
use crate::rsgc::{
    Mutation,
    mmtk::AllocationSemantics,
    object::{HeapTypeInfo, VTableOf},
};
use crate::runtime::{BlockingOperation, Scheme, YieldReason};

#[repr(C)]
pub struct Condition {
    pub(crate) cond: parking_lot::Condvar,
}

static CONDITION_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, Condition>::VT,
    TypeCode8::THREAD_CONDITION.bits() as u16,
);
pub static CONDITION_INFO: &'static HeapTypeInfo = &CONDITION_INFO_VALUE;

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
}

static MUTEX_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, Mutex>::VT,
    TypeCode8::THREAD_MUTEX.bits() as u16,
);
pub static MUTEX_INFO: &'static HeapTypeInfo = &MUTEX_INFO_VALUE;

pub enum MutexKind {
    Reentrant(parking_lot::ReentrantMutex<()>),
    Regular(parking_lot::Mutex<()>),
}

unsafe impl Trace for Mutex {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Tagged for Mutex {
    const TC8: TypeCode8 = TypeCode8::THREAD_MUTEX;

    const TYPE_NAME: &'static str = "mutex";
}

unsafe impl Tagged for Condition {
    const TC8: TypeCode8 = TypeCode8::THREAD_CONDITION;

    const TYPE_NAME: &'static str = "condition";
}

#[derive(Trace)]
#[repr(C)]
pub struct ThreadObject<'gc> {
    entrypoint: Option<Value<'gc>>,
}

static THREAD_OBJECT_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, ThreadObject<'static>>::VT,
    TypeCode8::THREAD.bits() as u16,
);
pub static THREAD_OBJECT_INFO: &'static HeapTypeInfo = &THREAD_OBJECT_INFO_VALUE;

impl<'gc> ThreadObject<'gc> {
    pub(crate) fn new(mc: Mutation<'gc>, entrypoint: Option<Value<'gc>>) -> Gc<'gc, Self> {
        Gc::new_with_info(mc, ThreadObject { entrypoint }, THREAD_OBJECT_INFO)
    }
}

unsafe impl<'gc> Tagged for ThreadObject<'gc> {
    const TC8: TypeCode8 = TypeCode8::THREAD;

    const TYPE_NAME: &'static str = "thread";
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
        let thread_started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let thread_started_clone = thread_started.clone();

        let _handle = std::thread::spawn(move || {
            let scm = Scheme::forked(bits, dynamic_state_bits);
            thread_started_clone.wait();

            scm.call_value(
                |ctx, _args| {
                    let thunk = ctx.state().thread_object.entrypoint.unwrap();
                    thunk
                },
                |ctx, result| {
                    let _ = ctx;
                    let _ = result;
                    ()
                },
            )
        });

        thread_started.wait();

        nctx.return_(val)
    }

    #[scheme(name = "thread?")]
    pub fn thread_p(obj: Value<'gc>) -> bool {
        nctx.return_(obj.is::<ThreadObject>())
    }

    #[scheme(name = "current-thread")]
    pub fn current_thread() -> Value<'gc> {
        let thread_obj = nctx.ctx.state().thread_object;
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
        };
        // mutex has to be non-moving so that parking_lot can use its address as a key.
        let gc_mutex =
            nctx.ctx
                .allocate_with_info(mutex, MUTEX_INFO, AllocationSemantics::NonMoving);
        nctx.return_(gc_mutex.into())
    }
    #[scheme(name = "mutex-acquire")]
    pub fn mutex_acquire(mutex_obj: Gc<'gc, Mutex>, block: Option<bool>) -> bool {
        let block = block.unwrap_or(true);
        // fast path: try to acquire the lock without yielding to native

        match &mutex_obj.mutex {
            MutexKind::Reentrant(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    std::mem::forget(_guard);
                    return nctx.return_(true);
                }
            }
            MutexKind::Regular(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    std::mem::forget(_guard);
                    return nctx.return_(true);
                }
            }
        }

        if block {
            return nctx.yield_and_return(
                YieldReason::Operation(Box::new(MutexAcquireOperation { mutex: mutex_obj })),
                &[Value::new(true)],
            );
        } else {
            match &mutex_obj.mutex {
                MutexKind::Reentrant(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
                        std::mem::forget(_guard);
                        return nctx.return_(true);
                    }
                }
                MutexKind::Regular(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
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
        let gc_condition =
            nctx.ctx
                .allocate_with_info(condition, CONDITION_INFO, AllocationSemantics::NonMoving);
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
        nctx.yield_and_return(
            YieldReason::Operation(Box::new(ConditionWaitOperation {
                condition: condition_obj,
                mutex: mutex_obj,
            })),
            &[Value::undefined()],
        )
    }
}

pub(crate) fn init_threading<'gc>(ctx: Context<'gc>) {
    threading_ops::register(ctx);
}

#[derive(Trace)]
pub struct MutexAcquireOperation<'gc> {
    pub mutex: Gc<'gc, Mutex>,
}

impl<'gc> BlockingOperation<'gc> for MutexAcquireOperation<'gc> {
    fn prepare(&self, _ctx: Context<'gc>) -> Box<dyn FnOnce()> {
        let mutex = NonNull::from(&self.mutex.mutex);

        Box::new(move || match unsafe { mutex.as_ref() } {
            MutexKind::Reentrant(mutex) => {
                let _guard = mutex.lock();
                std::mem::forget(_guard);
            }
            MutexKind::Regular(mutex) => {
                let _guard = mutex.lock();
                std::mem::forget(_guard);
            }
        })
    }
}

#[derive(Trace)]
pub struct ConditionWaitOperation<'gc> {
    pub condition: Gc<'gc, Condition>,
    pub mutex: Gc<'gc, Mutex>,
}

impl<'gc> BlockingOperation<'gc> for ConditionWaitOperation<'gc> {
    fn prepare(&self, _ctx: Context<'gc>) -> Box<dyn FnOnce()> {
        let condition = NonNull::from(&self.condition.cond);
        let mutex = NonNull::from(&self.mutex.mutex);

        Box::new(move || match unsafe { mutex.as_ref() } {
            MutexKind::Regular(mutex) => unsafe {
                let mut guard = mutex.make_guard_unchecked();
                condition.as_ref().wait(&mut guard);
                std::mem::forget(guard);
            },
            _ => panic!("Only regular mutex can be used with condition waiting"),
        })
    }
}

//! Multi-threading support for CapyScheme.

use crate::runtime::{Scheme, YieldReason, prelude::*};
use rsgc::{Mutation, mmtk::AllocationSemantics};

#[repr(C)]
pub struct Condition {
    header: ScmHeader,
    pub(crate) cond: parking_lot::Condvar,
}

unsafe impl Trace for Condition {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[repr(C)]
pub struct Mutex {
    header: ScmHeader,
    pub(crate) mutex: MutexKind,
}

pub enum MutexKind {
    Reentrant(parking_lot::ReentrantMutex<()>),
    Regular(parking_lot::Mutex<()>),
}

unsafe impl Trace for Mutex {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
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
    header: ScmHeader,
    entrypoint: Option<Value<'gc>>,
}

impl<'gc> ThreadObject<'gc> {
    pub(crate) fn new(mc: &Mutation<'gc>, entrypoint: Option<Value<'gc>>) -> Gc<'gc, Self> {
        let obj = ThreadObject {
            header: ScmHeader::with_type_bits(TypeCode8::THREAD.bits() as _),

            entrypoint,
        };
        Gc::new(mc, obj)
    }
}

unsafe impl<'gc> Tagged for ThreadObject<'gc> {
    const TC8: TypeCode8 = TypeCode8::THREAD;

    const TYPE_NAME: &'static str = "thread";
}

crate::native_fn!(

    register_threading_fns:

    pub ("fork-thread") fn fork_thread<'gc>(
        nctx,
        thunk: Gc<'gc, Closure<'gc>>
    ) -> Value<'gc> {
        let thread_obj = ThreadObject::new(&nctx.ctx, Some(thunk.into()));

        let val: Value<'gc> = thread_obj.into();
        let bits = val.bits();
        let dynamic_state = nctx.ctx.state.dynamic_state.save(nctx.ctx);
        let dynamic_state_bits = dynamic_state.bits();
        let thread_started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let thread_started_clone = thread_started.clone();

        let _handle = std::thread::spawn(move || {
            let scm = Scheme::forked(bits, dynamic_state_bits);
            thread_started_clone.wait();

            scm.call_value(
                |ctx, _args| {
                    let thunk = ctx.state.thread_object.entrypoint.unwrap();
                    thunk
                },
                |ctx, result| {
                    let _ = ctx;
                    let _ = result;
                    ()
                }
            )
        });



        thread_started.wait();

        nctx.return_(val)
    }

    pub ("thread?") fn thread_p<'gc>(
        nctx,
        obj: Value<'gc>
    ) -> bool {
        nctx.return_(obj.is::<ThreadObject>())
    }

    pub ("current-thread") fn current_thread<'gc>(
        nctx,
    ) -> Value<'gc> {
        let thread_obj = nctx.ctx.state.thread_object;
        nctx.return_(thread_obj.into())
    }

    pub ("make-mutex") fn make_mutex<'gc>(
        nctx,
        reentrant: Option<bool>
    ) -> Value<'gc> {
        let reentrant = reentrant.unwrap_or(false);
        let mutex = Mutex {
            header: ScmHeader::with_type_bits(TypeCode8::THREAD_MUTEX.bits() as _),
            mutex: if reentrant {
                MutexKind::Reentrant(parking_lot::ReentrantMutex::new(()))
            } else {
                MutexKind::Regular(parking_lot::Mutex::new(()))
            },
        };
        // mutex has to be non-moving so that parking_lot can use its address as a key.
        let gc_mutex = nctx.ctx.allocate(mutex, AllocationSemantics::NonMoving);
        nctx.return_(gc_mutex.into())
    }

    pub ("mutex-acquire") fn mutex_acquire<'gc>(
        nctx,
        mutex_obj: Gc<'gc, Mutex>,
        block: Option<bool>
    ) -> bool {
        let block = block.unwrap_or(true);
        // fast path: try to acquire the lock without yielding to native
        match &mutex_obj.mutex {
            MutexKind::Reentrant(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    std::mem::forget(_guard);
                    return nctx.return_(true)
                }
            }
            MutexKind::Regular(mutex) => {
                if let Some(_guard) = mutex.try_lock() {
                    std::mem::forget(_guard);
                    return nctx.return_(true)
                }
            }
        }
        if block {
            return nctx.yield_and_return(YieldReason::LockMutex(mutex_obj.into()), &[Value::new(true)])
        } else {
            match &mutex_obj.mutex {
                MutexKind::Reentrant(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
                        std::mem::forget(_guard);
                        return nctx.return_(true)
                    }
                }
                MutexKind::Regular(mutex) => {
                    if let Some(_guard) = mutex.try_lock() {
                        std::mem::forget(_guard);
                        return nctx.return_(true)
                    }
                }
            }
            return nctx.return_(false)
        }
    }

    pub ("mutex-release") fn mutex_release<'gc>(
        nctx,
        mutex_obj: Gc<'gc, Mutex>
    ) -> Value<'gc> {
        match &mutex_obj.mutex {
            MutexKind::Reentrant(mutex) => {
                unsafe {
                    let guard = mutex.make_guard_unchecked();
                    drop(guard);
                }
            }
            MutexKind::Regular(mutex) => {
                unsafe {
                    let guard = mutex.make_guard_unchecked();
                    drop(guard);
                }
            }
        }
        nctx.return_(Value::undefined())
    }

    pub ("mutex-reentrant?") fn mutex_reentrant_p<'gc>(
        nctx,
        mutex_obj: Gc<'gc, Mutex>
    ) -> bool {
        match &mutex_obj.mutex {
            MutexKind::Reentrant(_) => nctx.return_(true),
            MutexKind::Regular(_) => nctx.return_(false),
        }
    }

    pub ("mutex?") fn mutex_p<'gc>(
        nctx,
        obj: Value<'gc>
    ) -> bool {
        nctx.return_(obj.is::<Mutex>())
    }

    pub ("thread-condition?") fn thread_condition_p<'gc>(
        nctx,
        obj: Value<'gc>
    ) -> bool {
        nctx.return_(obj.is::<Condition>())
    }

    pub ("make-condition") fn make_condition<'gc>(
        nctx,
    ) -> Value<'gc> {
        let condition = Condition {
            header: ScmHeader::with_type_bits(TypeCode8::THREAD_CONDITION.bits() as _),
            cond: parking_lot::Condvar::new(),
        };
        let gc_condition = nctx.ctx.allocate(condition, AllocationSemantics::NonMoving);
        nctx.return_(gc_condition.into())
    }

    pub ("condition-signal") fn condition_signal<'gc>(
        nctx,
        condition_obj: Gc<'gc, Condition>
    ) -> Value<'gc> {
        condition_obj.cond.notify_one();
        nctx.return_(Value::undefined())
    }

    pub ("condition-broadcast") fn condition_broadcast<'gc>(
        nctx,
        condition_obj: Gc<'gc, Condition>
    ) -> Value<'gc> {
        condition_obj.cond.notify_all();
        nctx.return_(Value::undefined())
    }

    pub ("condition-wait") fn condition_wait<'gc>(
        nctx,
        condition_obj: Gc<'gc, Condition>,
        mutex_obj: Gc<'gc, Mutex>
    ) -> Value<'gc> {
        if matches!(mutex_obj.mutex, MutexKind::Reentrant(_)) {
            return nctx.wrong_argument_violation("condition-wait", "reentrant mutex can't be used with condition-wait", Some(mutex_obj.into()), Some(1), 2, &[condition_obj.into(), mutex_obj.into()])
        }
        nctx.yield_and_return(YieldReason::WaitCondition {
            condition: condition_obj.into(),
            mutex: mutex_obj.into(),
        }, &[Value::undefined()])
    }
);

pub(crate) fn init_threading<'gc>(ctx: Context<'gc>) {
    register_threading_fns(ctx);
}

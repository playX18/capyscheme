//! Multi-threading support for CapyScheme.

use crate::runtime::{Scheme, YieldReason, prelude::*};
use rsgc::{Mutation, mmtk::AllocationSemantics};

#[repr(C)]
pub struct Condition {
    header: ScmHeader,
    cond: parking_lot::Condvar,
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
    pub(crate) mutex: parking_lot::ReentrantMutex<()>,
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
        let thread_started = std::sync::Arc::new(std::sync::Barrier::new(2));
        let thread_started_clone = thread_started.clone();

        let _handle = std::thread::spawn(move || {
            let scm = Scheme::forked(bits);
            thread_started_clone.wait();

            let _ = scm;
        });



        thread_started.wait();

        nctx.return_(val)
    }

    pub ("make-mutex") fn make_mutex<'gc>(
        nctx,
    ) -> Value<'gc> {
        let mutex = Mutex {
            header: ScmHeader::with_type_bits(TypeCode8::THREAD_MUTEX.bits() as _),
            mutex: parking_lot::ReentrantMutex::new(()),
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

        /*if let Some(_guard) = mutex_obj.mutex.try_lock() {
            std::mem::forget(_guard);
            return nctx.return_(true)
        }*/

        if block {
            return nctx.yield_and_return(YieldReason::LockMutex(mutex_obj.into()), &[Value::new(true)])
        } else {
            if let Some(_guard) = mutex_obj.mutex.try_lock() {
                std::mem::forget(_guard);
                nctx.return_(true)
            } else {
                nctx.return_(false)
            }
        }
    }

    pub ("mutex-release") fn mutex_release<'gc>(
        nctx,
        mutex_obj: Gc<'gc, Mutex>
    ) -> Value<'gc> {
        let guard = unsafe { mutex_obj.mutex.make_guard_unchecked() };
        drop(guard);
        nctx.return_(Value::undefined())
    }
);

pub(crate) fn init_threading<'gc>(ctx: Context<'gc>) {
    register_threading_fns(ctx);
}

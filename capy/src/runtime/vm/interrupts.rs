//! Support for interrupting Scheme threads.

use crate::{prelude::*, runtime::vm::threading::ThreadObject};
use super::VMResult;

pub(crate) fn deliver_pending_interrupts<'gc>(ctx: Context<'gc>) {
    let thread = ctx.state().thread_object;
    if thread.interrupt_level() != 0 {
        return;
    }
    thread.mask_interrupts();
    while let Some(thunk) = thread.pop_pending_interrupt() {
        if let VMResult::Err(err) = super::call_scheme(ctx, thunk, []) {
            ctx.state().accumulator.set(err);
            break;
        }
    }
    thread.unmask_interrupts();
}

#[scheme(path = capy)]
pub mod interrupt_ops {
    #[scheme(name = "%thread-interrupt!")]
    pub fn thread_interrupt(thread: Gc<'gc, ThreadObject<'gc>>, thunk: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        if thread.runtime_thread().is_none() {
            return nctx.raise_error("%thread-interrupt!", "thread has not started", &[thread.into()]);
        }

        if Gc::ptr_eq(thread, nctx.ctx.state().thread_object) {
            return nctx.return_call(thunk.into(), &[]);
        }

        thread.enqueue_interrupt(thunk.into());
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "current-interrupt-level")]
    pub fn current_interrupt_level() -> usize {
        let level = nctx.ctx.state().thread_object.interrupt_level();
        nctx.return_(level)
    }

    #[scheme(name = "%call-without-interrupts")]
    pub fn call_without_interrupts(thunk: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        let thread = nctx.ctx.state().thread_object;
        thread.mask_interrupts();
        let retk = nctx.retk;
        let after = nctx.ctx.make_native_continuation(
            call_without_interrupts_after,
            [thread.into(), retk],
            Value::null(),
        );
        unsafe { nctx.return_call_unsafe(after.into(), thunk.into(), &[]) }
    }
}

extern "C-unwind" fn call_without_interrupts_after<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc> {
    let nctx = unsafe { NativeCallContext::<Value>::from_raw(ctx, rator, rands, num_rands, Value::undefined()) };
    let rator = rator.downcast::<Closure>();
    let thread = rator[1].get().downcast::<ThreadObject>();
    let retk = rator[2].get();
    thread.unmask_interrupts();
    deliver_pending_interrupts(ctx);
    let values = unsafe { std::slice::from_raw_parts(rands, num_rands) };
    unsafe { nctx.continue_to(retk, values) }.ret
}

pub(crate) fn init_interrupts<'gc>(ctx: Context<'gc>) {
    interrupt_ops::register(ctx);
}

#[cfg(test)]
mod tests {
    use crate::runtime::{Scheme, value::Value, vm::threading::ThreadObject};

    #[test]
    fn thread_interrupt_queue_is_fifo() {
        Scheme::new_uninit().enter(|ctx| {
            let thread = ThreadObject::new(*ctx, None);
            thread.enqueue_interrupt(Value::new(1));
            thread.enqueue_interrupt(Value::new(2));

            assert_eq!(thread.pop_pending_interrupt(), Some(Value::new(1)));
            assert_eq!(thread.pop_pending_interrupt(), Some(Value::new(2)));
            assert_eq!(thread.pop_pending_interrupt(), None);
        });
    }

    #[test]
    fn masked_interrupts_remain_pending_until_unmasked() {
        Scheme::new_uninit().enter(|ctx| {
            let thread = ThreadObject::new(*ctx, None);
            thread.enqueue_interrupt(Value::new(7));

            thread.mask_interrupts();
            assert!(thread.has_pending_interrupts());

            thread.unmask_interrupts();
            assert_eq!(thread.pop_pending_interrupt(), Some(Value::new(7)));
            assert!(!thread.has_pending_interrupts());
        });
    }
}

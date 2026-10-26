//! Support for interrupting Scheme threads.

use super::ExecutionResult;
use crate::{prelude::*, runtime::vm::threading::ThreadObject};

pub(crate) fn deliver_pending_interrupts<'gc>(ctx: MutationContext<'gc>) {
    let thread = ctx.state().thread_object;
    // A dying thread ignores interrupts
    if thread.interrupt_level() != 0 || crate::runtime::sched::scheduler::is_dying() {
        return;
    }
    thread.mask_interrupts();
    while let Some(thunk) = thread.pop_pending_interrupt() {
        if let ExecutionResult::Err(err) = crate::runtime::sni::call_function(ctx, thunk, []) {
            ctx.state().accumulator.set(err);
            break;
        }
    }
    thread.unmask_interrupts();
}

#[scheme(path = capy)]
pub mod interrupt_ops {
    #[scheme(name = "%native-thread-interrupt!")]
    pub fn thread_interrupt(
        thread: Gc<'gc, ThreadObject<'gc>>,
        thunk: Gc<'gc, Closure<'gc>>,
    ) -> Value<'gc> {
        if !thread.is_bound() {
            return call.raise_error(
                "%native-thread-interrupt!",
                "thread has not started",
                &[thread.into()],
            );
        }

        if Gc::ptr_eq(thread, ctx.state().thread_object) {
            return call.tail(thunk.into(), &[]);
        }

        Gc::write(ctx, thread);
        thread.enqueue_interrupt(thunk.into());
        Value::undefined()
    }

    #[scheme(name = "current-interrupt-level")]
    pub fn current_interrupt_level() -> usize {
        let level = ctx.state().thread_object.interrupt_level();
        level
    }

    #[scheme(name = "%call-without-interrupts")]
    pub fn call_without_interrupts(thunk: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        let thread = ctx.state().thread_object;
        thread.mask_interrupts();
        let after = ctx.make_native_continuation(
            call_without_interrupts_after,
            [thread.into(), Value::current_k()],
            Value::null(),
        );
        call.call_with(thunk.into(), &[], after.into())
    }
}

extern "C" fn call_without_interrupts_after<'gc>(
    ctx: MutationContext<'gc>,
    sfp: Slot<'gc>,
) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        let rator = call.rator.downcast::<Closure>();
        let thread = rator[1].get().downcast::<ThreadObject>();
        let retk = rator[2].get();
        thread.unmask_interrupts();
        deliver_pending_interrupts(ctx);
        ctx.stage_resume(retk, call.args)
    })
}

pub(crate) fn init_interrupts<'gc>(ctx: MutationContext<'gc>) {
    interrupt_ops::register(ctx);
}

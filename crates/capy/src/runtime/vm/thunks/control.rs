use super::make_assertion_violation;
use crate::prelude::PROCEDURES;
use crate::runtime::vm::default_exception_handler;
use crate::runtime::{
    MutationContext,
    value::{Str, Value},
};

pub fn non_applicable<'gc>(ctx: MutationContext<'gc>, subr: Value<'gc>) -> Value<'gc> {
    log::trace!("call {subr}");
    crate::runtime::vm::debug::print_stacktraces_impl(ctx);
    make_assertion_violation(
        ctx,
        Value::new(false),
        Str::new(ctx, "attempt to call non-procedure", true).into(),
        &[subr],
    )
}

pub fn exception_handler<'gc>(ctx: MutationContext<'gc>) -> Value<'gc> {
    let handler = ctx.exception_handler();
    if let Some(handler) = handler {
        handler
    } else {
        let procs = PROCEDURES.fetch(ctx);
        let clos =
            procs.register_static_closure(ctx, default_exception_handler as _, Value::new(false));
        clos.into()
    }
}

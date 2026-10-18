use crate::heap::Gc;

use crate::{
    global,
    runtime::{
        MutationContext,
        modules::{Variable, define},
        value::{PROCEDURES, ReturnCode, Str, Value},
    },
};

global!(
    var_throw<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%throw", Value::new(false));
);

pub fn throw<'gc>(ctx: MutationContext<'gc>, key: Value<'gc>, args: Value<'gc>) -> ReturnCode {
    let throw = var_throw(ctx).get();
    let handler = ctx.exception_handler().unwrap_or_else(|| {
        PROCEDURES
            .fetch(ctx)
            .register_static_closure(
                ctx,
                super::default_exception_handler as _,
                Value::new(false),
            )
            .into()
    });

    if throw == Value::new(false) {
        let str = Str::new(
            ctx,
            format!("Uncaught error, key: {key}, args: {args}"),
            false,
        );

        return ctx.stage_tail_call(handler, &[str.into()]);
    }
    ctx.stage_tail_call(throw, &[handler])
}

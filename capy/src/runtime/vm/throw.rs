use crate::rsgc::Gc;

use crate::{
    global,
    runtime::{
        Context,
        modules::{Variable, define},
        value::{NativeReturn, PROCEDURES, Str, Value},
    },
};

global!(
    var_throw<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%throw", Value::new(false));
);

pub fn throw<'gc>(
    ctx: Context<'gc>,
    key: Value<'gc>,
    args: Value<'gc>,
    retk: Value<'gc>,
) -> NativeReturn<'gc> {
    let throw = var_throw(ctx).get();
    let handler = ctx.exception_handler().unwrap_or_else(|| {
        PROCEDURES
            .fetch(*ctx)
            .register_static_closure(
                ctx,
                super::default_exception_handler as _,
                Value::new(false),
            )
            .into()
    });

    if throw == Value::new(false) {
        let str = Str::new(
            *ctx,
            format!("Uncaught error, key: {key}, args: {args}"),
            false,
        );

        return ctx.return_call(handler, [str.into()], None);
    }
    ctx.return_call(throw, [retk, handler], None)
}

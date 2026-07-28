use super::make_assertion_violation;
use crate::runtime::value::{IntoValue, Number};
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    Context,
    value::{Str, Symbol, Value},
};

pub fn reverse<'gc>(ctx: Context<'gc>, list: Value<'gc>) -> Value<'gc> {
    list.list_reverse(ctx)
}

pub fn append<'gc>(ctx: Context<'gc>, m1: Value<'gc>, m2: Value<'gc>) -> Value<'gc> {
    if !m1.is_list() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "append").into(),
                Str::new(ctx, "not a list", true).into(),
                &[m1],
            ),
        );
    }

    m1.append(ctx, m2)
}

pub fn length<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    if !v.is_list() {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "length").into(),
                Str::new(ctx, "not a list", true).into(),
                &[v],
            ),
        );
    }

    Number::from_usize(ctx, v.list_length()).into_value(ctx)
}

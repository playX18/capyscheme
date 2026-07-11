use super::{ThunkResult, make_assertion_violation};
use crate::runtime::value::{IntoValue, Number};
use crate::runtime::{
    Context,
    value::{Str, Symbol, Value},
};

pub fn reverse<'gc>(ctx: Context<'gc>, list: Value<'gc>) -> Value<'gc> {
    list.list_reverse(ctx)
}

pub fn append<'gc>(ctx: Context<'gc>, m1: Value<'gc>, m2: Value<'gc>) -> ThunkResult<'gc> {
    if !m1.is_list() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "append").into(),
                Str::new(*ctx, "not a list", true).into(),
                &[m1],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: m1.append(ctx, m2),
    }
}

pub fn length<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
    if !v.is_list() {
        return ThunkResult {
            code: 1,
            value: make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "length").into(),
                Str::new(*ctx, "not a list", true).into(),
                &[v],
            ),
        };
    }

    ThunkResult {
        code: 0,
        value: Number::from_usize(ctx, v.list_length()).into_value(ctx),
    }
}

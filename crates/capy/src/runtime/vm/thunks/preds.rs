use super::make_assertion_violation;
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    Context,
    value::{Complex, Str, Symbol, Value},
};

pub fn eqv<'gc>(a: Value<'gc>, b: Value<'gc>) -> bool {
    a.eqv(b)
}

pub fn equal<'gc>(a: Value<'gc>, b: Value<'gc>) -> bool {
    a.equal(b, &mut Default::default())
}

pub fn is_rational<'gc>(value: Value<'gc>) -> bool {
    let Some(num) = value.number() else {
        return false;
    };

    num.is_rational()
}

pub fn listp<'gc>(v: Value<'gc>) -> bool {
    v.is_list()
}

pub fn complexp<'gc>(v: Value<'gc>) -> bool {
    v.is_number()
}

pub fn realp<'gc>(v: Value<'gc>) -> bool {
    v.is_number() && !v.is::<Complex>()
}

pub fn nanp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    let Some(n) = v.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "nan?").into(),
                Str::new(ctx, "not a number", true).into(),
                &[v],
            ),
        );
    };

    n.is_nan().into()
}

pub fn integerp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> bool {
    v.number().is_some_and(|n| n.is_integer())
}

pub fn exactp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    let Some(n) = v.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "exact?").into(),
                Str::new(ctx, "not a number", true).into(),
                &[v],
            ),
        );
    };

    n.is_exact().into()
}

pub fn inexactp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    let Some(n) = v.number() else {
        thunk_raise(
            ctx,
            make_assertion_violation(
                ctx,
                Symbol::from_str(ctx, "inexact?").into(),
                Str::new(ctx, "not a number", true).into(),
                &[v],
            ),
        );
    };

    (!n.is_exact()).into()
}

pub fn exact_integerp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
    let Some(n) = v.number() else {
        return Value::new(false);
    };

    (n.is_exact_integer()).into()
}

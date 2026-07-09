use super::{ThunkResult, make_assertion_violation};
use crate::runtime::value::{IntoValue, Number};
use crate::runtime::{
    Context,
    value::{
        Str, Symbol, Value,
    },
    vm::debug::print_stacktraces_impl,
};
use std::cmp::Ordering;


pub fn is_zero<'gc>(ctx: Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "zero?").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };
        ThunkResult { code: 0, value: num.is_zero().into() }
    }

pub fn negate<'gc>(ctx: Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "negate").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        ThunkResult { code: 0, value: num.negate(ctx).into_value(ctx) }
    }

pub fn memv<'gc>(ctx: Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> ThunkResult<'gc> {
        if !list.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "memv").into(),
                    Str::new(*ctx, "not a proper list", true).into(),
                    &[list],
                )
            }
        }

        let mut cur = list;
        while !cur.is_null() {

            if key.eqv(cur.car()) {
                return ThunkResult { code: 0, value: cur };
            }
            cur = cur.cdr();
        }

        ThunkResult { code: 0, value: Value::new(false) }
    }

pub fn memq<'gc>(ctx: Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> ThunkResult<'gc> {
        if !list.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "memq").into(),
                    Str::new(*ctx, "not a proper list", true).into(),
                    &[list],
                )
            }
        }

        let mut cur = list;
        while !cur.is_null() {
            if key == cur.car() {
                return ThunkResult { code: 0, value: cur };
            }
            cur = cur.cdr();
        }

        ThunkResult { code: 0, value: Value::new(false) }
    }

pub fn number_eq<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::equal(ctx, a, b)).into() }
    }

pub fn number_lt<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "<").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "<").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(ctx, a, b) == Some(Ordering::Less)).into() }
    }

pub fn number_gt<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, ">").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, ">").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(ctx, a, b) == Some(Ordering::Greater)).into() }
    }

pub fn number_le<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "<=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "<=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(ctx, a, b) != Some(Ordering::Greater)).into() }
    }

pub fn number_ge<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, ">=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, ">=").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(ctx, a, b) != Some(Ordering::Less)).into() }
    }

pub fn number_plus<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "+").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "+").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::add(ctx, a, b).into_value(ctx) }
    }

pub fn number_minus<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "-").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "-").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sub(ctx, a, b).into_value(ctx) }
    }

pub fn number_times<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "*").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "*").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::mul(ctx, a, b).into_value(ctx) }
    }

pub fn number_div<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "/").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "/").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() && b.is_exact() && a.is_exact(){
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "/").into(),
                    Str::new(*ctx, "division by zero", true).into(),
                    &[a.into_value(ctx), b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::div(ctx, a, b).into_value(ctx) }
    }

pub fn quotient<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "quotient").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "quotient").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() && b.is_exact() && a.is_exact() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "quotient").into(),
                    Str::new(*ctx, "division by zero", true).into(),
                    &[a.into_value(ctx), b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::quotient(ctx, a, b).into_value(ctx) }
    }

pub fn remainder<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "remainder").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "remainder").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() && b.is_exact() && a.is_exact() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "remainder").into(),
                    Str::new(*ctx, "division by zero", true).into(),
                    &[a.into_value(ctx), b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::remainder(ctx, a, b).into_value(ctx) }
    }

pub fn modulo<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "modulo").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "modulo").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() && b.is_exact() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "modulo").into(),
                    Str::new(*ctx, "division by zero", true).into(),
                    &[a.into_value(ctx), b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::modulo(ctx, a, b).into_value(ctx) }
    }

pub fn exact2inexact<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "exact->inexact").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };


        ThunkResult { code: 0, value: n.to_inexact(ctx).into_value(ctx) }
    }

pub fn inexact_to_exact<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "inexact->exact").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };
        ThunkResult { code: 0, value: n.to_exact(ctx).into_value(ctx) }
    }

pub fn expt<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "expt").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "expt").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::expt(ctx, a, b).into_value(ctx) }
    }

pub fn ash<'gc>(ctx: Context<'gc>, n: Value<'gc>, count: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "ash").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        let Some(count) = count.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "ash").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[count],
                )
            }
        };

        if !count.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "ash").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[count.into_value(ctx)],
                )
            }
        }

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "ash").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[n.into_value(ctx)],
                )
            }
        }

        if !count.is_negative() {

            let Some(res) = n.lsh(ctx, count) else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(ctx, "ash").into(),
                        Str::new(*ctx, "shift out of bounds", true).into(),
                        &[n.into_value(ctx), count.into_value(ctx)],
                    )
                }
            };

            ThunkResult { code: 0, value: res.into_value(ctx) }
        } else {
            let count = count.negate(ctx);
            let Some(res) = n.rsh(ctx, count) else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(ctx, "ash").into(),
                        Str::new(*ctx, "shift out of bounds", true).into(),
                        &[n.into_value(ctx), count.into_value(ctx)],
                    )
                };
            };

            ThunkResult { code: 0, value: res.into_value(ctx) }
        }
    }

pub fn logand<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            log::trace!("logand: {a} is not a number, b={b}");
            print_stacktraces_impl(ctx);
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logand").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            log::trace!("logand: {b} is not a number, a={a}");
            print_stacktraces_impl(ctx);
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logand").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logand").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[a.into_value(ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logand").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: a.logand(ctx, b).into_value(ctx) }
    }

pub fn logior<'gc>(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logior").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logior").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logior").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[a.into_value(ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "logior").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[b.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: a.logior(ctx, b).into_value(ctx) }
    }

pub fn lognot<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "lognot").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "lognot").into(),
                    Str::new(*ctx, "not an exact integer", true).into(),
                    &[n.into_value(ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: n.lognot(ctx).into_value(ctx) }
    }

pub fn sqrt<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "sqrt").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sqrt(ctx, n).into_value(ctx) }
    }

pub fn abs<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "abs").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.abs(ctx).into_value(ctx) }
    }

pub fn floor<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "floor").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.floor(ctx).into_value(ctx) }
    }

pub fn ceiling<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "ceiling").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.ceiling(ctx).into_value(ctx) }
    }

pub fn truncate<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "truncate").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.truncate(ctx).into_value(ctx) }
    }

pub fn sin<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "sin").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sin(ctx, n).into_value(ctx) }
    }

pub fn cos<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "cos").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::cos(ctx, n).into_value(ctx) }
    }

pub fn tan<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "tan").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::tan(ctx, n).into_value(ctx) }
    }

pub fn asin<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "asin").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::asin(ctx, n).into_value(ctx) }
    }

pub fn acos<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "acos").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::acos(ctx, n).into_value(ctx) }
    }

pub fn atan<'gc>(ctx: Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "atan").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };



        ThunkResult {
            code: 0,
            value: Number::atan(ctx, n).into_value(ctx)
        }
    }

pub fn atan2<'gc>(ctx: Context<'gc>, y: Value<'gc>, x: Value<'gc>) -> ThunkResult<'gc> {
        let Some(y) = y.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "atan2").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[y],
                )
            }
        };

        let Some(x) = x.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "atan2").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[x],
                )
            }
        };

        ThunkResult {
            code: 0,
            value: Number::atan2(ctx, y, x).into_value(ctx)
        }
    }

pub fn evenp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "even?").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };



        ThunkResult { code: 0, value: n.is_even().into() }
    }

pub fn oddp<'gc>(ctx: Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "odd?").into(),
                    Str::new(*ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: (!n.is_even()).into() }
    }

pub fn fxeq<'gc>(
        ctx: Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(ctx, "fx=").into(),
                    Str::new(*ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(ctx, "fx=?").into(),
                Str::new(*ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

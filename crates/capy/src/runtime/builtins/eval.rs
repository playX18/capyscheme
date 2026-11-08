use crate::prelude::*;
use crate::runtime::prelude::*;
pub(crate) fn init_eval<'gc>(ctx: MutationContext<'gc>) {
    eval_ops::register(ctx);
}

fn is_applicable(value: Value<'_>) -> bool {
    value.is::<Closure>()
}

#[scheme(path=capy)]
pub mod eval_ops {
    #[scheme(name = "apply")]
    pub fn apply(rator: Value<'gc>, rands: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if !is_applicable(rator) {
            call.wrong_argument_violation(
                "apply",
                "attempt to call non-procedure",
                Some(rator),
                Some(0),
                1,
                &[rator],
            );
            return Err(Value::undefined());
        }

        if rands.is_empty() {
            call.tail(rator, &[]);
            return Err(Value::undefined());
        }
        let fixed = &rands[..rands.len() - 1];
        let rest = rands[rands.len() - 1];
        let mut rest_len = 0usize;
        let mut cursor = rest;
        while !cursor.is_null() {
            if !cursor.is_pair() {
                call.wrong_argument_violation(
                    "apply",
                    "last argument must be a list",
                    Some(rest),
                    Some(rands.len() - 1),
                    rands.len(),
                    rands,
                );
                return Err(Value::undefined());
            }
            rest_len += 1;
            cursor = cursor.cdr();
        }
        let mut args = Vec::with_capacity(fixed.len() + rest_len);
        args.extend_from_slice(fixed);
        let mut items = rest;
        while !items.is_null() {
            args.push(items.car());
            items = items.cdr();
        }
        call.tail(rator, &args);
        Err(Value::undefined())
    }

    #[scheme(name = "procedure?")]
    pub fn procedure_p(v: Value<'gc>) -> Value<'gc> {
        Value::new(is_applicable(v))
    }
}

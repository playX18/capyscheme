use crate::prelude::*;
use crate::runtime::prelude::*;
pub(crate) fn init_eval<'gc>(ctx: Context<'gc>) {
    eval_ops::register(ctx);
}

#[scheme(path=capy)]
pub mod eval_ops {
    #[scheme(name = "apply")]
    pub fn apply(rator: Value<'gc>, rands: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if !rator.is::<Closure>() {
            return nctx.wrong_argument_violation(
                "apply",
                "attempt to call non-procedure",
                Some(rator),
                Some(0),
                1,
                &[rator],
            );
        }

        if rands.is_empty() {
            return nctx.return_call(rator, &[]);
        }
        let fixed = &rands[..rands.len() - 1];
        let rest = rands[rands.len() - 1];
        match nctx.ctx.return_apply(rator, fixed, rest, nctx.retk) {
            Ok(ret) => NativeCallReturn { ret },
            Err(improper_tail) => nctx.wrong_argument_violation(
                "apply",
                "last argument must be a list",
                Some(improper_tail),
                Some(rands.len() - 1),
                rands.len(),
                rands,
            ),
        }
    }

    #[scheme(name = "procedure?")]
    pub fn procedure_p(v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v.is::<Closure>()))
    }
}

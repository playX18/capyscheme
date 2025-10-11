use crate::{native_fn, runtime::prelude::*};

pub(crate) fn init_eval<'gc>(ctx: Context<'gc>) {
    register_eval_fns(ctx);
}

native_fn!(
    register_eval_fns:

    pub ("apply") fn apply<'gc>(nctx, rator: Value<'gc>, rands: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if !rator.is::<Closure>() {
            return nctx.wrong_argument_violation("apply", "attempt to call non-procedure", Some(rator), Some(0), 1, &[rator])
        }

        if rands.len() == 0 {
            return nctx.return_call(rator, &[]);
        }
        let mut args = Vec::with_capacity(rands.len());

        for i in 0..rands.len() - 1 {
            args.push(rands[i]);
        }

        if !rands[rands.len() - 1].is_list() {
            return nctx.wrong_argument_violation("apply", "last argument must be a list", Some(rands[rands.len() - 1]), Some(rands.len() - 1), rands.len(), rands);
        }

        let mut ls = rands[rands.len() - 1];
        while !ls.is_null() {
            args.push(ls.car());
            ls = ls.cdr();
        }

        nctx.return_call(rator, &args)
    }

    pub ("procedure?") fn procedure_p<'gc>(nctx, v: Value<'gc>) -> Value<'gc> {
        nctx.return_((v.is::<Closure>()).into())
    }
);

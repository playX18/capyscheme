use crate::{
    native_fn,
    runtime::{prelude::*, vm::debug::print_stacktraces_impl},
};

pub(crate) fn init_eval<'gc>(ctx: Context<'gc>) {
    register_eval_fns(ctx);
}

native_fn!(
    register_eval_fns:

    pub ("apply") fn apply<'gc>(nctx, rator: Value<'gc>, rands: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if !rator.is::<Closure>() {
            print_stacktraces_impl(nctx.ctx);
            todo!("can only apply closures, got: {rator}");
        }

        if rands.len() == 0 {
            return nctx.return_call(rator, &[]);
        }
        let mut args = Vec::with_capacity(rands.len());

        for i in 0..rands.len() - 1 {
            args.push(rands[i]);
        }

        if !rands[rands.len() - 1].is_list() {
            todo!()
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

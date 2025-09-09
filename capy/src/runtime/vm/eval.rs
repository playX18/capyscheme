use crate::{
    native_fn,
    runtime::{
        prelude::*,
        vm::{debug::print_stacktraces_impl, libraries::lookup_scheme_location},
    },
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
        if rator.downcast::<Closure>().is_continuation() {
            let loc = lookup_scheme_location(nctx.ctx, rator.downcast::<Closure>().code.as_usize() as u64);
            println!("{loc:?}, meta: {}", rator.downcast::<Closure>().meta);
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
);

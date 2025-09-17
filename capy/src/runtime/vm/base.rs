use rsgc::{Gc, barrier::Unlock, cell::Lock};

use crate::{
    native_cont, native_fn,
    runtime::{
        Context,
        value::{Closure, Pair, Str, Symbol, Value, Vector},
    },
};

native_fn!(
    register_base_fns:

    pub ("exit") fn exit<'gc>(_nctx, code: Value<'gc>) -> () {
        let code = if code.is_int32() {
            code.as_int32()
        } else {
            -1
        };

        std::process::exit(code);
    }

    pub (".return-error") fn return_error<'gc>(nctx, err: Value<'gc>) -> () {
        nctx.return_error(err)
    }

    pub ("values") fn values<'gc>(nctx, args: &'gc [Value<'gc>]) -> &'gc [Value<'gc>] {
        nctx.return_(args)
    }

    pub (".call/cc-unsafe") fn call_cc_unsafe<'gc>(
        nctx,
        proc: Value<'gc>
    ) -> Result<Value<'gc>, Value<'gc>> {
        let k = nctx.retk;
        let k = make_call_cc_cont_closure(nctx.ctx, [k]);
        nctx.return_call(proc, &[k.into()])
    }

    pub (".call/cc-cont") fn call_cc_cont<'gc>(
        nctx,
        ans: &'gc [Value<'gc>]
    ) -> &'gc [Value<'gc>] {
        let k = nctx.rator().downcast::<Closure>().free.downcast::<Vector>()[1].get();
        // SAFETY: only `.call/cc-unsafe` can construct us and variable at 1 is always a continuation
        unsafe { nctx.continue_to(k, ans) }
    }

    pub ("not") fn not<'gc>(nctx, v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v == Value::new(false)))
    }

    /// Calls thunk with no arguments and sets `reth` to provided handler.
    ///
    /// This function will make handler to return to `retk` and `reth` of the caller of
    /// `.with-handler`.
    pub (".with-handler") fn with_exception_handler<'gc>(
        nctx,
        handler: Value<'gc>,
        thunk: Value<'gc>
    ) -> Result<Value<'gc>, Value<'gc>> {
        let retk = nctx.retk;
        let reth = nctx.reth;
        let handler_closure = make_handler_cont_closure(nctx.ctx, [retk, reth, handler]);
        unsafe { nctx.return_call_unsafe(retk, handler_closure.into(), thunk, &[]) }
    }

    pub (".raise") fn raise<'gc>(nctx, e: Value<'gc>) -> () {
        unsafe {
            let reth = nctx.reth;
            nctx.continue_to(reth, &[e])
        }
    }

    pub ("gensym") fn gensym<'gc>(nctx, prefix: Option<Gc<'gc, Str<'gc>>>) -> Value<'gc> {
        let sym = Symbol::gensym(nctx.ctx, prefix);
        nctx.return_(sym.into())
    }

    pub ("delete!") fn delete<'gc>(nctx, item: Value<'gc>, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("delete!", "expected a list", Some(lst), Some(2), 2, &[item, lst]);
        }
        let mut walk = lst;
        let lst = Lock::new(lst);

        let mut prev: *const Lock<Value<'gc>> = &lst;

        while walk.is_pair() {
            if walk.car().r5rs_equal(item) {
                unsafe { (*prev).unlock_unchecked().set(walk.cdr()) };
            } else {
                prev = &walk.downcast::<Pair>().cdr;
            }
            walk = walk.cdr();
        }

        nctx.return_(lst.get())
    }
);

native_cont!(
    register_base_conts:

    pub (".handler-cont") fn handler_cont<'gc>(
        nctx,
        ans: Value<'gc>
    ) -> Result<Value<'gc>, Value<'gc>> {
        let free = nctx.rator().downcast::<Closure>().free.downcast::<Vector>();
        let retk = free[1].get();
        let reth = free[2].get();
        let handler = free[3].get();

        unsafe {
            nctx.return_call_unsafe(retk, reth, handler, &[ans])
        }
    }
);

pub fn init_base<'gc>(ctx: Context<'gc>) {
    register_base_fns(ctx);
    let _ = register_base_conts;
}

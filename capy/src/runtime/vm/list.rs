use rsgc::Gc;

use crate::{
    native_fn,
    runtime::{prelude::*, vm::debug::print_stacktraces_impl},
};
pub fn init_lists<'gc>(ctx: Context<'gc>) {
    register_list_fns(ctx);
}

native_fn!(
    register_list_fns:

    pub ("length") fn length<'gc>(nctx, ls: Value<'gc>) -> Result<usize, Value<'gc>> {
        if !ls.is_list() {
            print_stacktraces_impl(nctx.ctx);
            todo!("not a list: {ls}");
        }

        nctx.return_(Ok(ls.list_length()))
    }

    pub ("reverse") fn reverse<'gc>(nctx, ls: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            todo!()
        }
        let res = ls.list_reverse(nctx.ctx);
        nctx.return_(Ok(res))
    }

    pub ("list->vector") fn list_to_vector<'gc>(nctx, ls: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            todo!()
        }
        let res = ls.list_to_vector(nctx.ctx);
        nctx.return_(Ok(res.into()))
    }

    pub ("vector->list") fn vector_to_list<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>) -> Result<Value<'gc>, Value<'gc>> {
        let mut ls = Value::null();

        for i in (0..vec.len()).rev() {
            ls = Value::cons(nctx.ctx, vec[i].get(), ls);
        }

        nctx.return_(Ok(ls))
    }

    pub ("cdr") fn cdr<'gc>(nctx, pair: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            todo!()
        }
        nctx.return_(Ok(pair.cdr()))
    }

    pub ("car") fn car<'gc>(nctx, pair: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            todo!("not a list: {pair}");
        }
        nctx.return_(Ok(pair.car()))
    }

    pub ("set-car!") fn set_car<'gc>(nctx, pair: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            todo!()
        }
        pair.set_car(nctx.ctx, value);
        nctx.return_(Ok(Value::undefined()))
    }

    pub ("set-cdr!") fn set_cdr<'gc>(nctx, pair: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            todo!()
        }
        pair.set_cdr(nctx.ctx, value);
        nctx.return_(Ok(Value::undefined()))
    }

    pub ("list-ref") fn list_ref<'gc>(nctx, ls: Value<'gc>, index: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            todo!()
        }
        let mut current = ls;
        for _ in 0..index {
            if !current.is_pair() {
                todo!()
            }
            current = current.cdr();
        }
        if !current.is_pair() {
            todo!()
        }
        nctx.return_(Ok(current.car()))
    }

    pub ("take") fn take<'gc>(nctx, ls: Value<'gc>, n: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            todo!()
        }
        let mut current = ls;
        let mut res = Value::null();
        let mut count = 0;
        while !current.is_null() && count < n {
            if !current.is_pair() {
                todo!()
            }
            res = Value::cons(nctx.ctx, current.car(), res);
            current = current.cdr();
            count += 1;
        }
        res = res.list_reverse(nctx.ctx);
        nctx.return_(Ok(res))
    }

    pub ("drop") fn drop<'gc>(nctx, ls: Value<'gc>, n: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            todo!()
        }
        let mut current = ls;
        let mut count = 0;
        while !current.is_null() && count < n {
            if !current.is_pair() {
                todo!()
            }
            current = current.cdr();
            count += 1;
        }
        nctx.return_(Ok(current))
    }

    pub ("append") fn append<'gc>(nctx, args: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if args.is_empty() {
            return nctx.return_(Ok(Value::null()));
        }

        let mut res = args[args.len() - 1];
        for arg in args[..args.len() - 1].iter().rev() {
            if !arg.is_list() {
                todo!()
            }
            match append_impl(nctx.ctx, *arg, res) {
                Some(v) => res = v,
                None => todo!(),
            }
        }

        nctx.return_(Ok(res))
    }
);

fn append_impl<'gc>(ctx: Context<'gc>, mut ls1: Value<'gc>, ls2: Value<'gc>) -> Option<Value<'gc>> {
    let mut first = Value::null();
    let mut last = Value::null();

    while ls1.is_pair() {
        let v = Value::cons(ctx, ls1.car(), Value::null());
        if first.is_null() {
            first = v;
        } else {
            last.set_cdr(ctx, v);
        }

        last = v;
        ls1 = ls1.cdr();
    }

    if !ls1.is_null() {
        return None;
    }

    if last.is_null() {
        return Some(ls2);
    }

    last.set_cdr(ctx, ls2);

    Some(first)
}

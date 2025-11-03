use crate::prelude::*;
use crate::runtime::prelude::*;
use rsgc::Gc;
pub fn init_lists<'gc>(ctx: Context<'gc>) {
    list_ops::register(ctx);
}
#[scheme(path=capy)]
pub mod list_ops {
    #[scheme(name = "pair?")]
    pub fn pairp(value: Value<'gc>) -> Result<bool, Value<'gc>> {
        nctx.return_(Ok(value.is_pair()))
    }

    #[scheme(name = "list?")]
    pub fn listp(value: Value<'gc>) -> bool {
        nctx.return_(value.is_list())
    }

    #[scheme(name = "length")]
    pub fn length(ls: Value<'gc>) -> usize {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "length",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }

        nctx.return_(ls.list_length())
    }

    #[scheme(name = "reverse")]
    pub fn reverse(ls: Value<'gc>) -> Value<'gc> {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "reverse",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }
        let res = ls.list_reverse(nctx.ctx);
        nctx.return_(res)
    }

    #[scheme(name = "list->vector")]
    pub fn list_to_vector(ls: Value<'gc>) -> Value<'gc> {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "list->vector",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }
        let res = ls.list_to_vector(nctx.ctx);
        nctx.return_(res.into())
    }

    #[scheme(name = "vector->list")]
    pub fn vector_to_list(vec: Gc<'gc, Vector<'gc>>) -> Result<Value<'gc>, Value<'gc>> {
        let mut ls = Value::null();

        for i in (0..vec.len()).rev() {
            ls = Value::cons(nctx.ctx, vec[i].get(), ls);
        }

        nctx.return_(Ok(ls))
    }

    #[scheme(name = "cdr")]
    pub fn cdr(pair: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            return nctx.wrong_argument_violation(
                "cdr",
                "expected a pair",
                Some(pair),
                Some(1),
                1,
                &[pair],
            );
        }
        nctx.return_(Ok(pair.cdr()))
    }

    #[scheme(name = "car")]
    pub fn car(pair: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            println!("car: {pair} is not a pair");
            crate::runtime::vm::debug::print_stacktraces_impl(nctx.ctx);
            return nctx.wrong_argument_violation(
                "car",
                "expected a pair",
                Some(pair),
                Some(1),
                1,
                &[pair],
            );
        }
        nctx.return_(Ok(pair.car()))
    }

    #[scheme(name = "set-car!")]
    pub fn set_car(pair: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            return nctx.wrong_argument_violation(
                "set-car!",
                "expected a pair",
                Some(pair),
                Some(1),
                1,
                &[pair],
            );
        }
        pair.set_car(nctx.ctx, value);
        nctx.return_(Ok(Value::undefined()))
    }

    #[scheme(name = "set-cdr!")]
    pub fn set_cdr(pair: Value<'gc>, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !pair.is_pair() {
            return nctx.wrong_argument_violation(
                "set-cdr!",
                "expected a pair",
                Some(pair),
                Some(1),
                1,
                &[pair],
            );
        }
        pair.set_cdr(nctx.ctx, value);
        nctx.return_(Ok(Value::undefined()))
    }

    #[scheme(name = "list-ref")]
    pub fn list_ref(ls: Value<'gc>, index: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "list-ref",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }
        let mut current = ls;
        for _ in 0..index {
            if !current.is_pair() {
                return nctx.wrong_argument_violation(
                    "list-ref",
                    "expected a pair",
                    Some(current),
                    Some(1),
                    1,
                    &[current],
                );
            }
            current = current.cdr();
        }
        if !current.is_pair() {
            return nctx.wrong_argument_violation(
                "list-ref",
                "expected a pair",
                Some(current),
                Some(1),
                1,
                &[current],
            );
        }
        nctx.return_(Ok(current.car()))
    }

    #[scheme(name = "take")]
    pub fn take(ls: Value<'gc>, n: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "take",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }
        let mut current = ls;
        let mut res = Value::null();
        let mut count = 0;
        while !current.is_null() && count < n {
            if !current.is_pair() {
                return nctx.wrong_argument_violation(
                    "take",
                    "expected a pair",
                    Some(current),
                    Some(1),
                    1,
                    &[current],
                );
            }
            res = Value::cons(nctx.ctx, current.car(), res);
            current = current.cdr();
            count += 1;
        }
        res = res.list_reverse(nctx.ctx);
        nctx.return_(Ok(res))
    }

    #[scheme(name = "drop")]
    pub fn drop(ls: Value<'gc>, n: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !ls.is_list() {
            return nctx.wrong_argument_violation(
                "drop",
                "expected a list",
                Some(ls),
                Some(1),
                1,
                &[ls],
            );
        }
        let mut current = ls;
        let mut count = 0;
        while !current.is_null() && count < n {
            if !current.is_pair() {
                return nctx.wrong_argument_violation(
                    "drop",
                    "expected a pair",
                    Some(current),
                    Some(1),
                    1,
                    &[current],
                );
            }
            current = current.cdr();
            count += 1;
        }
        nctx.return_(Ok(current))
    }

    #[scheme(name = "append")]
    pub fn append(args: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if args.is_empty() {
            return nctx.return_(Ok(Value::null()));
        }

        let mut res = args[args.len() - 1];
        for arg in args[..args.len() - 1].iter().rev() {
            if !arg.is_list() {
                return nctx.wrong_argument_violation(
                    "append",
                    "expected a list",
                    Some(*arg),
                    Some(1),
                    1,
                    &[*arg],
                );
            }
            match append_impl(nctx.ctx, *arg, res) {
                Some(v) => res = v,
                None => {
                    return nctx.wrong_argument_violation(
                        "append",
                        "expected a list",
                        Some(*arg),
                        Some(1),
                        1,
                        &[*arg],
                    );
                }
            }
        }

        nctx.return_(Ok(res))
    }

    #[scheme(name = "acons")]
    pub fn acons(
        key: Value<'gc>,
        datum: Value<'gc>,
        alist: Value<'gc>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let pair = Value::cons(nctx.ctx, key, datum);
        let res = Value::cons(nctx.ctx, pair, alist);
        nctx.return_(Ok(res))
    }

    #[scheme(name = "cons")]
    pub fn cons(car: Value<'gc>, cdr: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let pair = Value::cons(nctx.ctx, car, cdr);
        nctx.return_(Ok(pair))
    }

    #[scheme(name = "cons*")]
    pub fn cons_star(first: Value<'gc>, args: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        if args.is_empty() {
            return nctx.return_(Ok(first));
        }

        let mut result = args[args.len() - 1];
        for value in args[..args.len() - 1].iter().rev() {
            result = Value::cons(nctx.ctx, *value, result);
        }
        result = Value::cons(nctx.ctx, first, result);
        nctx.return_(Ok(result))
    }

    #[scheme(name = "list")]
    pub fn list(args: &'gc [Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
        let mut ls = Value::null();
        for v in args.iter().rev() {
            ls = Value::cons(nctx.ctx, *v, ls);
        }
        nctx.return_(Ok(ls))
    }

    #[scheme(name = "list-head")]
    pub fn list_head(lst: Value<'gc>, n: usize) -> Value<'gc> {
        if n == 0 {
            return nctx.return_(Value::null());
        }
        if !lst.is_pair() {
            return nctx.wrong_argument_violation(
                "list-head",
                "expected a list",
                Some(lst),
                Some(1),
                2,
                &[lst, Value::new(n as i32)],
            );
        }
        let obj = Value::cons(nctx.ctx, lst.car(), Value::null());
        let mut tail = obj;
        let mut lst = lst;
        lst = lst.cdr();

        for _ in 2..=n {
            if lst.is_pair() {
                let e = Value::cons(nctx.ctx, lst.car(), Value::null());
                tail.set_cdr(nctx.ctx, e);
                tail = e;
                lst = lst.cdr();
            } else {
                return nctx.wrong_argument_violation(
                    "list-head",
                    "list too short",
                    Some(lst),
                    Some(1),
                    2,
                    &[lst, Value::new(n as i32)],
                );
            }
        }

        tail.set_cdr(nctx.ctx, Value::null());
        nctx.return_(obj)
    }

    #[scheme(name = "list-tail")]
    pub fn list_tail(lst: Value<'gc>, n: usize) -> Result<Value<'gc>, Value<'gc>> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation(
                "list-tail",
                "expected a list",
                Some(lst),
                Some(1),
                2,
                &[lst, Value::new(n as i32)],
            );
        }
        let mut walk = lst;
        let mut count = 0;

        while walk.is_pair() && count < n {
            walk = walk.cdr();
            count += 1;
        }

        if count < n {
            return nctx.wrong_argument_violation(
                "list-tail",
                "list too short",
                Some(lst),
                Some(1),
                2,
                &[lst, Value::new(n as i32)],
            );
        }

        nctx.return_(Ok(walk))
    }

    #[scheme(name = "list-transpose")]
    pub fn list_transpose(lists: &'gc [Value<'gc>]) -> Value<'gc> {
        if lists.is_empty() {
            return nctx.wrong_argument_violation(
                "list-transpose",
                "expected at least one list",
                None,
                None,
                0,
                &[],
            );
        }

        if !lists[0].is_list() {
            return nctx.wrong_argument_violation(
                "list-transpose",
                "expected a list",
                Some(lists[0]),
                Some(1),
                lists.len(),
                &[lists[0]],
            );
        }
        let len = lists[0].list_length();
        for (i, lst) in lists.iter().copied().enumerate().skip(1) {
            if !lst.is_list() {
                return nctx.wrong_argument_violation(
                    "list-transpose",
                    "expected a list",
                    Some(lst),
                    Some(i + 2),
                    lists.len() + 1,
                    &[lst],
                );
            }
            if lst.list_length() != len {
                return nctx.wrong_argument_violation(
                    "list-transpose",
                    "lists should have different lengths",
                    Some(lst),
                    Some(i + 2),
                    lists.len() + 1,
                    &[lst],
                );
            }
        }

        let result = transpose_impl(nctx.ctx, len, lists);
        nctx.return_(result)
    }

    #[scheme(name = "list-transpose+")]
    pub fn list_transpose_plus(lists: &'gc [Value<'gc>]) -> Value<'gc> {
        if lists.is_empty() {
            return nctx.wrong_number_of_arguments_violation("list-transpose+", 1, None, 0, &[]);
        }
        if !lists[0].is_list() {
            return nctx.return_(Value::new(false));
        }
        let len = lists[0].list_length();
        for lst in lists.iter().copied().skip(1) {
            if !lst.is_list() {
                return nctx.return_(Value::new(false));
            }
            if lst.list_length() != len {
                return nctx.return_(Value::new(false));
            }
        }

        let result = transpose_impl(nctx.ctx, len, lists);
        nctx.return_(result)
    }

    #[scheme(name = "list-transpose*")]
    pub fn list_transpose_star(lists: &'gc [Value<'gc>]) -> Value<'gc> {
        let mut finite = false;
        let mut each_len = usize::MAX;
        for i in 0..lists.len() {
            if !lists[i].is_list() {
                return nctx.wrong_argument_violation(
                    "list-transpose*",
                    "expected a list",
                    Some(lists[i]),
                    Some(i + 1),
                    lists.len(),
                    &[lists[i]],
                );
            }
            finite = true;
            let n = lists[i].list_length();
            if n < each_len {
                each_len = n;
            }
        }

        if finite {
            let result = transpose_impl(nctx.ctx, each_len, lists);
            return nctx.return_(result);
        } else {
            return nctx.wrong_argument_violation(
                "list-transpose*",
                "all lists must be finite",
                Some(lists[0]),
                Some(1),
                lists.len(),
                &[lists[0]],
            );
        }
    }

    #[scheme(name = "circular-list?")]
    pub fn circular_listp(lst: Value<'gc>) -> bool {
        let mut slow = lst;
        let mut fast = lst;

        while fast.is_pair() && fast.cdr().is_pair() {
            slow = slow.cdr();
            fast = fast.cdr().cdr();

            if slow == fast {
                return nctx.return_(true);
            }
        }

        nctx.return_(false)
    }

    #[scheme(name = "list-copy")]
    pub fn list_copy(lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation(
                "list-copy",
                "expected a list",
                Some(lst),
                Some(1),
                1,
                &[lst],
            );
        }
        if lst.is_pair() {
            let mut lst = lst;
            let obj = Value::cons(nctx.ctx, lst.car(), Value::null());
            let mut tail = obj;

            lst = lst.cdr();

            while lst.is_pair() {
                let e = Value::cons(nctx.ctx, lst.car(), Value::null());
                tail.set_cdr(nctx.ctx, e);
                tail = e;
                lst = lst.cdr();
            }
            tail.set_cdr(nctx.ctx, lst);
            return nctx.return_(obj);
        }

        nctx.return_(lst)
    }
}

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

fn transpose_impl<'gc>(ctx: Context<'gc>, each_len: usize, args: &[Value<'gc>]) -> Value<'gc> {
    let mut args = args.to_vec();
    let mut ans = Value::null();
    let mut ans_tail = Value::null();

    for _ in 0..each_len {
        let elt = Value::cons(ctx, args[0].car(), Value::null());
        let mut elt_tail = elt;
        args[0] = args[0].cdr();
        for arg in args.iter_mut().skip(1) {
            elt_tail.set_cdr(ctx, Value::cons(ctx, arg.car(), Value::null()));
            elt_tail = elt_tail.cdr();
            *arg = arg.cdr();
        }

        if ans.is_null() {
            ans = Value::cons(ctx, elt, Value::null());
            ans_tail = ans;
        } else {
            ans_tail.set_cdr(ctx, Value::cons(ctx, elt, Value::null()));
            ans_tail = ans_tail.cdr();
        }
    }

    ans
}

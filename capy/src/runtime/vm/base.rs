use rsgc::{Gc, barrier};

use crate::{
    fluid, native_cont, native_fn,
    runtime::{
        Context,
        modules::Variable,
        prelude::*,
        value::{Closure, Str, Symbol, Tuple, Value, Vector},
    },
};

fluid!(
    pub program_arguments_fluid = Value::null();
);

pub fn scm_log_level<'gc>(ctx: Context<'gc>) -> i32 {
    let log_level = ctx.private_ref("capy", "*log-level*");
    if let Some(level) = log_level {
        if level.is_int32() {
            return level.as_int32();
        }
    }
    0
}

native_fn!(
    register_base_fns:

    pub ("implementation-version") fn implementation_version<'gc>(nctx) -> Value<'gc> {
        let version = env!("CARGO_PKG_VERSION");
        let version = Str::new(&nctx.ctx, version, true);
        nctx.return_(version.into())
    }

    pub ("native-endianness") fn native_endianness<'gc>(nctx) -> Value<'gc> {
        let endian = if cfg!(target_endian = "little") {
            "little"
        } else {
            "big"
        };
        let endian = nctx.ctx.intern(endian);
        nctx.return_(endian.into())
    }
    pub ("host-arch") fn host_target<'gc>(nctx) -> Value<'gc> {
        let target = Str::new(&nctx.ctx, std::env::consts::ARCH, true);
        nctx.return_(target.into())
    }

    pub ("host-os") fn host_os<'gc>(nctx) -> Value<'gc> {
        let os = Str::new(&nctx.ctx, std::env::consts::OS, true);
        nctx.return_(os.into())
    }

    pub ("host-family") fn host_family<'gc>(nctx) -> Value<'gc> {
        let family = Str::new(&nctx.ctx, std::env::consts::FAMILY, true);
        nctx.return_(family.into())
    }

    pub ("host-triple") fn host_triple<'gc>(nctx) -> Value<'gc> {
        let triple = format!(
            "{}-{}-{}",
            std::env::consts::ARCH,
            std::env::consts::OS,
            std::env::consts::FAMILY
        );
        let triple = Str::new(&nctx.ctx, &triple, true);
        nctx.return_(triple.into())
    }

    pub ("dll-suffix") fn dll_suffix<'gc>(nctx) -> Value<'gc> {
        let suffix = std::env::consts::DLL_SUFFIX;
        let suffix = Str::new(&nctx.ctx, suffix, true);
        nctx.return_(suffix.into())
    }

    pub ("dll-prefix") fn dll_prefix<'gc>(nctx) -> Value<'gc> {
        let prefix = std::env::consts::DLL_PREFIX;
        let prefix = Str::new(&nctx.ctx, prefix, true);
        nctx.return_(prefix.into())
    }

    pub ("procedure?") fn is_procedure<'gc>(nctx, v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v.is::<Closure>()))
    }

    pub ("program-arguments") fn program_arguments<'gc>(nctx) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(program_arguments_fluid(ctx).get(ctx))
    }

    pub ("set-program-arguments!") fn set_program_arguments<'gc>(
        nctx,
        args: Value<'gc>
    ) -> Value<'gc> {
        if !args.is_list() {
            return nctx.wrong_argument_violation("set-program-arguments!", "expected a list", Some(args), Some(1), 1, &[args]);
        }
        let ctx = nctx.ctx;
        program_arguments_fluid(ctx).set(ctx, args);
        nctx.return_(args)
    }


    pub ("eof-object") fn eof_object<'gc>(nctx) -> Value<'gc> {
        nctx.return_(Value::eof())
    }
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

    pub ("values-list") fn values_list<'gc>(nctx, lst: Value<'gc>) -> () {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("values-list", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }
        let mut values = Vec::new();
        let mut walk = lst;
        while walk.is_pair() {
            values.push(walk.car());
            walk = walk.cdr();
        }


        nctx.return_many(&values)
    }

    pub ("delete!") fn delete<'gc>(nctx, item: Value<'gc>, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("delete!", "expected a list", Some(lst), Some(2), 2, &[item, lst]);
        }

        /*let mut lst = lst;
        let mut walk = lst;
        let mut prev = lst;

        while walk.is_pair() {
            if walk.car().r5rs_equal(item) {
                if prev == lst {
                    lst = walk.cdr();
                } else {
                    prev.set_cdr(nctx.ctx, walk.cdr());
                }
            } else {
                prev = walk;
            }
            walk = walk.cdr();
        }

        nctx.return_(lst)*/
        let lst = lst.destructive_delete(nctx.ctx, item);
        nctx.return_(lst)
    }

    pub ("unspecified") fn unspecified<'gc>(nctx) -> Value<'gc> {
        nctx.return_(Value::undefined())
    }

    pub ("unspecified?") fn is_unspecified<'gc>(nctx, v: Value<'gc>) -> bool {
        nctx.return_(v == Value::undefined())
    }

    pub ("procedure-properties") fn procedure_metadata<'gc>(
        nctx,
        proc: Gc<'gc, Closure<'gc>>
    ) -> Value<'gc> {
        nctx.return_(proc.meta.get())
    }

    pub ("set-procedure-properties!") fn set_procedure_metadata<'gc>(
        nctx,
        proc: Gc<'gc, Closure<'gc>>,
        meta: Value<'gc>
    ) -> Value<'gc> {
        let wproc = Gc::write(&nctx.ctx, proc);
        barrier::field!(wproc, Closure, meta).unlock().set(meta);
        nctx.return_(meta)
    }

    pub ("memv") fn memv<'gc>(
        nctx,
        item: Value<'gc>,
        lst: Value<'gc>
    ) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("memv", "expected a list", Some(lst), Some(2), 2, &[item, lst]);
        }
        let mut walk = lst;

        while walk.is_pair() {
            if walk.car().eqv(item) {
                return nctx.return_(walk);
            }
            walk = walk.cdr();
        }

        nctx.return_(Value::new(false))
    }

    pub ("memq") fn memq<'gc>(
        nctx,
        item: Value<'gc>,
        lst: Value<'gc>
    ) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("memq", "expected a list", Some(lst), Some(2), 2, &[item, lst]);
        }
        let mut walk = lst;

        while walk.is_pair() {
            if walk.car() == item {
                return nctx.return_(walk);
            }
            walk = walk.cdr();
        }

        nctx.return_(Value::new(false))
    }

    pub ("make-uuid") fn make_uuid<'gc>(nctx) -> Value<'gc> {
        let uuid = uuid::Uuid::new_v4();
        let ctx = nctx.ctx;
        nctx.return_(Str::new(&ctx, &uuid.to_string(), true).into())
    }

    pub ("boolean=?") fn boolean_eq<'gc>(
        nctx,
        a: Value<'gc>,
        b: Value<'gc>
    ) -> Value<'gc> {
        if !a.is_bool() {
            return nctx.wrong_argument_violation("boolean=?", "expected a boolean", Some(a), Some(1), 2, &[a, b]);
        }
        if !b.is_bool() {
            return nctx.wrong_argument_violation("boolean=?", "expected a boolean", Some(b), Some(2), 2, &[a, b]);
        }
        nctx.return_(Value::new(a.as_bool() == b.as_bool()))
    }

    pub ("variable?") fn is_variable<'gc>(nctx, v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v.is::<Variable>()))
    }

    pub ("symbol=?") fn symbol_eq<'gc>(
        nctx,
        a: Value<'gc>,
        b: Value<'gc>,
        rest: &'gc [Value<'gc>]
    ) -> Value<'gc> {
        if !a.is::<Symbol>() {
            return nctx.wrong_argument_violation("symbol=?", "expected a symbol", Some(a), Some(1), 2, &[a, b]);
        }
        if !b.is::<Symbol>() {
            return nctx.wrong_argument_violation("symbol=?", "expected a symbol", Some(b), Some(2), 2, &[a, b]);
        }
        if rest.is_empty() {
            return nctx.return_(Value::new(a == b));
        }

        let eq = a == b;
        for &sym in rest {
            if !eq {
                return nctx.return_(Value::new(false));
            }
            if !sym.is::<Symbol>() {
                return nctx.wrong_argument_violation("symbol=?", "expected a symbol", Some(sym), None, rest.len() + 2, &[a, b].iter().chain(rest).cloned().collect::<Vec<_>>().as_slice());
            }

            if sym != a {
                return nctx.return_(Value::new(false));
            }
        }

        nctx.return_(Value::new(true))
    }

    pub ("microsecond") fn microsecond<'gc>(nctx) -> u128 {
        unsafe {
            let mut tv: std::mem::MaybeUninit<libc::timeval> = std::mem::MaybeUninit::uninit();
            libc::gettimeofday(tv.as_mut_ptr(), std::ptr::null_mut());
            let tv = tv.assume_init();
            let sec = tv.tv_sec as u128 * 1_000_000 + tv.tv_usec as u128;
            nctx.return_(sec)
        }
    }

    pub ("tuple-size") fn tuple_size<'gc>(
        nctx,
        t: Gc<'gc, Tuple<'gc>>
    ) -> usize {
        nctx.return_(t.len())
    }

    pub ("tuple-set!") fn tuple_set<'gc>(
        nctx,
        t: Gc<'gc, Tuple<'gc>>,
        i: usize,
        v: Value<'gc>
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        if i >= t.len() {
            return nctx.wrong_argument_violation("tuple-set!", "index out of bounds", Some(i.into_value(ctx)), Some(2), 3, &[t.into(), i.into_value(ctx), v]);
        }

        Gc::write(&nctx.ctx, t)[i].unlock().set(v);
        nctx.return_(Value::undefined())
    }

    pub ("tuple-ref") fn tuple_ref<'gc>(
        nctx,
        t: Gc<'gc, Tuple<'gc>>,
        i: usize
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        if i >= t.len() {
            return nctx.wrong_argument_violation("tuple-ref", "index out of bounds", Some(i.into_value(ctx)), Some(2), 2, &[t.into(), i.into_value(ctx)]);
        }

        nctx.return_(t[i].get())
    }

    pub ("tuple?") fn is_tuple<'gc>(
        nctx,
        v: Value<'gc>
    ) -> bool {
        nctx.return_(v.is::<Tuple>())
    }

    pub ("eof-object?") fn is_eof_object<'gc>(
        nctx,
        v: Value<'gc>
    ) -> bool {
        nctx.return_(v.is_eof())
    }

    pub ("make-weak-mapping") fn make_weak_mapping<'gc>(
        nctx,
        key: Value<'gc>,
        value: Value<'gc>
    ) -> Value<'gc> {
        if !key.is_cell() {
            return nctx.wrong_argument_violation("make-weak-mapping", "expected a heap-allocated object as key", Some(key), Some(1), 2, &[key, value]);
        }
        let wmap = WeakMapping::new(nctx.ctx, key, value);
        nctx.return_(Value::new(wmap))
    }

    pub ("weak-mapping?") fn is_weak_mapping<'gc>(
        nctx,
        v: Value<'gc>
    ) -> bool {
        nctx.return_(v.is::<WeakMapping>())
    }

    pub ("weak-mapping-value") fn weak_mapping_value<'gc>(
        nctx,
        wm: Gc<'gc, WeakMapping<'gc>>
    ) -> Value<'gc> {
        let value = wm.value(&nctx.ctx);
        nctx.return_(value)
    }

    pub ("weak-mapping-key") fn weak_mapping_key<'gc>(
        nctx,
        wm: Gc<'gc, WeakMapping<'gc>>
    ) -> Value<'gc> {
        let key = wm.key(&nctx.ctx);
        nctx.return_(key)
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

    let args = std::env::args().rev().fold(Value::null(), |acc, arg| {
        Value::cons(ctx, Str::new(&ctx, arg, true).into(), acc)
    });

    program_arguments_fluid(ctx).set(ctx, args);
}

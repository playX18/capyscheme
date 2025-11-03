use rsgc::{Gc, barrier};

use crate::{
    fluid,
    prelude::*,
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

#[scheme(path=capy)]
pub mod base_ops {
    #[scheme(name = "implementation-version")]
    pub fn implementation_version() -> Value<'gc> {
        let version = env!("CARGO_PKG_VERSION");
        let version = Str::new(&nctx.ctx, version, true);
        nctx.return_(version.into())
    }

    #[scheme(name = "native-endianness")]
    pub fn native_endianness() -> Value<'gc> {
        let endian = if cfg!(target_endian = "little") {
            "little"
        } else {
            "big"
        };
        let endian = nctx.ctx.intern(endian);
        nctx.return_(endian.into())
    }

    #[scheme(name = "host-arch")]
    pub fn host_target() -> Value<'gc> {
        let target = Str::new(&nctx.ctx, std::env::consts::ARCH, true);
        nctx.return_(target.into())
    }

    #[scheme(name = "host-os")]
    pub fn host_os() -> Value<'gc> {
        let os = Str::new(&nctx.ctx, std::env::consts::OS, true);
        nctx.return_(os.into())
    }

    #[scheme(name = "host-family")]
    pub fn host_family() -> Value<'gc> {
        let family = Str::new(&nctx.ctx, std::env::consts::FAMILY, true);
        nctx.return_(family.into())
    }

    #[scheme(name = "host-triple")]
    pub fn host_triple() -> Value<'gc> {
        let triple = format!(
            "{}-{}-{}",
            std::env::consts::ARCH,
            std::env::consts::OS,
            std::env::consts::FAMILY
        );
        let triple = Str::new(&nctx.ctx, &triple, true);
        nctx.return_(triple.into())
    }

    #[scheme(name = "dll-suffix")]
    pub fn dll_suffix() -> Value<'gc> {
        let suffix = std::env::consts::DLL_SUFFIX;
        let suffix = Str::new(&nctx.ctx, suffix, true);
        nctx.return_(suffix.into())
    }

    #[scheme(name = "dll-prefix")]
    pub fn dll_prefix() -> Value<'gc> {
        let prefix = std::env::consts::DLL_PREFIX;
        let prefix = Str::new(&nctx.ctx, prefix, true);
        nctx.return_(prefix.into())
    }

    #[scheme(name = "procedure?")]
    pub fn is_procedure(v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v.is::<Closure>()))
    }

    #[scheme(name = "program-arguments")]
    pub fn program_arguments() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(program_arguments_fluid(ctx).get(ctx))
    }

    #[scheme(name = "set-program-arguments!")]
    pub fn set_program_arguments(args: Value<'gc>) -> Value<'gc> {
        if !args.is_list() {
            return nctx.wrong_argument_violation(
                "set-program-arguments!",
                "expected a list",
                Some(args),
                Some(1),
                1,
                &[args],
            );
        }
        let ctx = nctx.ctx;
        program_arguments_fluid(ctx).set(ctx, args);
        nctx.return_(args)
    }

    #[scheme(name = "eof-object")]
    pub fn eof_object() -> Value<'gc> {
        nctx.return_(Value::eof())
    }

    #[scheme(name = "exit")]
    pub fn exit(code: Value<'gc>) -> () {
        let code = if code.is_int32() { code.as_int32() } else { -1 };

        std::process::exit(code);
    }

    #[scheme(name = ".return-error")]
    pub fn return_error(err: Value<'gc>) -> () {
        nctx.return_error(err)
    }

    #[scheme(name = "values")]
    pub fn values(args: &'gc [Value<'gc>]) -> &'gc [Value<'gc>] {
        nctx.return_(args)
    }

    #[scheme(name = ".call/cc-unsafe")]
    pub fn call_cc_unsafe(proc: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let k = nctx.retk;
        let k = make_closure_call_cc_cont(nctx.ctx, [k]);
        nctx.return_call(proc, &[k.into()])
    }

    #[scheme(name = " .call/cc-cont ")]
    pub fn call_cc_cont(ans: &'gc [Value<'gc>]) -> &'gc [Value<'gc>] {
        let k = nctx.rator().downcast::<Closure>().free.downcast::<Vector>()[1].get();
        // SAFETY: only `.call/cc-unsafe` can construct us and variable at 1 is always a continuation
        unsafe { nctx.continue_to(k, ans) }
    }

    #[scheme(name = "not")]
    pub fn not(v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v == Value::new(false)))
    }

    /// Calls thunk with no arguments and sets `reth` to provided handler.
    ///
    /// This function will make handler to return to `retk` and `reth` of the caller of
    /// `.with-handler`.
    #[scheme(name = ".with-handler")]
    pub fn with_exception_handler(
        handler: Value<'gc>,
        thunk: Value<'gc>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let retk = nctx.retk;
        let reth = nctx.reth;
        let handler_closure = make_closure_handler_cont(nctx.ctx, [retk, reth, handler]);
        unsafe { nctx.return_call_unsafe(retk, handler_closure.into(), thunk, &[]) }
    }

    #[scheme(name = ".raise")]
    pub fn raise(e: Value<'gc>) -> () {
        unsafe {
            let reth = nctx.reth;
            nctx.continue_to(reth, &[e])
        }
    }

    #[scheme(name = "gensym")]
    pub fn gensym(prefix: Option<Gc<'gc, Str<'gc>>>) -> Value<'gc> {
        let sym = Symbol::gensym(nctx.ctx, prefix);
        nctx.return_(sym.into())
    }

    #[scheme(name = "values-list")]
    pub fn values_list(lst: Value<'gc>) -> () {
        if !lst.is_list() {
            return nctx.wrong_argument_violation(
                "values-list",
                "expected a list",
                Some(lst),
                Some(1),
                1,
                &[lst],
            );
        }
        let mut values = Vec::new();
        let mut walk = lst;
        while walk.is_pair() {
            values.push(walk.car());
            walk = walk.cdr();
        }

        nctx.return_many(&values)
    }

    #[scheme(name = "delete!")]
    pub fn delete(item: Value<'gc>, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation(
                "delete!",
                "expected a list",
                Some(lst),
                Some(2),
                2,
                &[item, lst],
            );
        }

        let lst = lst.destructive_delete(nctx.ctx, item);
        nctx.return_(lst)
    }

    #[scheme(name = "unspecified")]
    pub fn unspecified() -> Value<'gc> {
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "unspecified?")]
    pub fn is_unspecified(v: Value<'gc>) -> bool {
        nctx.return_(v == Value::undefined())
    }

    #[scheme(name = "procedure-properties")]
    pub fn procedure_metadata(proc: Gc<'gc, Closure<'gc>>) -> Value<'gc> {
        nctx.return_(proc.meta.get())
    }

    #[scheme(name = "set-procedure-properties!")]
    pub fn set_procedure_metadata(proc: Gc<'gc, Closure<'gc>>, meta: Value<'gc>) -> Value<'gc> {
        let wproc = Gc::write(&nctx.ctx, proc);
        barrier::field!(wproc, Closure, meta).unlock().set(meta);
        nctx.return_(meta)
    }

    #[scheme(name = "make-uuid")]
    pub fn make_uuid() -> Value<'gc> {
        let uuid = uuid::Uuid::new_v4();
        let ctx = nctx.ctx;
        nctx.return_(Str::new(&ctx, &uuid.to_string(), true).into())
    }

    #[scheme(name = "boolean=?")]
    pub fn boolean_eq(a: bool, b: bool) -> bool {
        nctx.return_(a == b)
    }

    #[scheme(name = "variable?")]
    pub fn is_variable(v: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(v.is::<Variable>()))
    }

    #[scheme(name = "symbol=?")]
    fn symbol_eq(a: Value<'gc>, b: Value<'gc>, rest: &'gc [Value<'gc>]) -> Value<'gc> {
        if !a.is::<Symbol>() {
            return nctx.wrong_argument_violation(
                "symbol=?",
                "expected a symbol",
                Some(a),
                Some(1),
                2,
                &[a, b],
            );
        }
        if !b.is::<Symbol>() {
            return nctx.wrong_argument_violation(
                "symbol=?",
                "expected a symbol",
                Some(b),
                Some(2),
                2,
                &[a, b],
            );
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
                return nctx.wrong_argument_violation(
                    "symbol=?",
                    "expected a symbol",
                    Some(sym),
                    None,
                    rest.len() + 2,
                    &[a, b]
                        .iter()
                        .chain(rest)
                        .cloned()
                        .collect::<Vec<_>>()
                        .as_slice(),
                );
            }

            if sym != a {
                return nctx.return_(Value::new(false));
            }
        }

        nctx.return_(Value::new(true))
    }
    #[scheme(name = "microsecond")]
    pub fn microsecond() -> u128 {
        unsafe {
            let mut tv: std::mem::MaybeUninit<libc::timeval> = std::mem::MaybeUninit::uninit();
            libc::gettimeofday(tv.as_mut_ptr(), std::ptr::null_mut());
            let tv = tv.assume_init();
            let sec = tv.tv_sec as u128 * 1_000_000 + tv.tv_usec as u128;
            nctx.return_(sec)
        }
    }

    #[scheme(name = "tuple-size")]
    pub fn tuple_size(t: Gc<'gc, Tuple<'gc>>) -> usize {
        nctx.return_(t.len())
    }

    #[scheme(name = "tuple-set!")]
    pub fn tuple_set(t: Gc<'gc, Tuple<'gc>>, i: usize, v: Value<'gc>) -> Value<'gc> {
        let ctx = nctx.ctx;
        if i >= t.len() {
            return nctx.wrong_argument_violation(
                "tuple-set!",
                "index out of bounds",
                Some(i.into_value(ctx)),
                Some(2),
                3,
                &[t.into(), i.into_value(ctx), v],
            );
        }

        Gc::write(&nctx.ctx, t)[i].unlock().set(v);
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "tuple-ref")]
    pub fn tuple_ref(t: Gc<'gc, Tuple<'gc>>, i: usize) -> Value<'gc> {
        let ctx = nctx.ctx;
        if i >= t.len() {
            return nctx.wrong_argument_violation(
                "tuple-ref",
                "index out of bounds",
                Some(i.into_value(ctx)),
                Some(2),
                2,
                &[t.into(), i.into_value(ctx)],
            );
        }

        nctx.return_(t[i].get())
    }

    #[scheme(name = "tuple?")]
    pub fn is_tuple(v: Value<'gc>) -> bool {
        nctx.return_(v.is::<Tuple>())
    }

    #[scheme(name = "eof-object?")]
    pub fn is_eof_object(v: Value<'gc>) -> bool {
        nctx.return_(v.is_eof())
    }

    #[scheme(name = "make-weak-mapping")]
    pub fn make_weak_mapping(key: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        if !key.is_cell() {
            return nctx.wrong_argument_violation(
                "make-weak-mapping",
                "expected a heap-allocated object as key",
                Some(key),
                Some(1),
                2,
                &[key, value],
            );
        }
        let wmap = WeakMapping::new(nctx.ctx, key, value);
        nctx.return_(Value::new(wmap))
    }

    #[scheme(name = "weak-mapping?")]
    pub fn is_weak_mapping(v: Value<'gc>) -> bool {
        nctx.return_(v.is::<WeakMapping>())
    }

    #[scheme(name = "weak-mapping-value")]
    pub fn weak_mapping_value(wm: Gc<'gc, WeakMapping<'gc>>) -> Value<'gc> {
        let value = wm.value(&nctx.ctx);
        nctx.return_(value)
    }

    #[scheme(name = "weak-mapping-key")]
    pub fn weak_mapping_key(wm: Gc<'gc, WeakMapping<'gc>>) -> Value<'gc> {
        let key = wm.key(&nctx.ctx);
        nctx.return_(key)
    }
}

#[scheme(continuation)]
pub fn handler_cont(ans: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
    let free = nctx.rator().downcast::<Closure>().free.downcast::<Vector>();
    let retk = free[1].get();
    let reth = free[2].get();
    let handler = free[3].get();

    unsafe { nctx.return_call_unsafe(retk, reth, handler, &[ans]) }
}

pub fn init_base<'gc>(ctx: Context<'gc>) {
    base_ops::register(ctx);

    let args = std::env::args().rev().fold(Value::null(), |acc, arg| {
        Value::cons(ctx, Str::new(&ctx, arg, true).into(), acc)
    });

    program_arguments_fluid(ctx).set(ctx, args);
}

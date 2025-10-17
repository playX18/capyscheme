use rsgc::{Gc, barrier, barrier::Unlock, cell::Lock};

use crate::{
    fluid, native_cont, native_fn,
    runtime::{
        Context,
        modules::{Variable, define},
        value::{Closure, Pair, Str, Symbol, Value, Vector},
    },
};

fluid!(
    pub program_arguments_fluid = Value::null();
);

native_fn!(
    register_base_fns:
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

    pub ("unspecified") fn unspecified<'gc>(nctx) -> Value<'gc> {
        nctx.return_(Value::undefined())
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
        b: Value<'gc>
    ) -> Value<'gc> {
        if !a.is::<Symbol>() {
            return nctx.wrong_argument_violation("symbol=?", "expected a symbol", Some(a), Some(1), 2, &[a, b]);
        }
        if !b.is::<Symbol>() {
            return nctx.wrong_argument_violation("symbol=?", "expected a symbol", Some(b), Some(2), 2, &[a, b]);
        }
        nctx.return_(Value::new(a == b))
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

    // Input flags (c_iflag)
    define(ctx, "IGNBRK", Value::new(libc::IGNBRK as i32));
    define(ctx, "BRKINT", Value::new(libc::BRKINT as i32));
    define(ctx, "IGNPAR", Value::new(libc::IGNPAR as i32));
    define(ctx, "PARMRK", Value::new(libc::PARMRK as i32));
    define(ctx, "INPCK", Value::new(libc::INPCK as i32));
    define(ctx, "ISTRIP", Value::new(libc::ISTRIP as i32));
    define(ctx, "INLCR", Value::new(libc::INLCR as i32));
    define(ctx, "IGNCR", Value::new(libc::IGNCR as i32));
    define(ctx, "ICRNL", Value::new(libc::ICRNL as i32));
    define(ctx, "IXON", Value::new(libc::IXON as i32));
    define(ctx, "IXOFF", Value::new(libc::IXOFF as i32));

    // Output flags (c_oflag)
    define(ctx, "OPOST", Value::new(libc::OPOST as i32));
    define(ctx, "ONLCR", Value::new(libc::ONLCR as i32));
    define(ctx, "OCRNL", Value::new(libc::OCRNL as i32));
    define(ctx, "ONOCR", Value::new(libc::ONOCR as i32));
    define(ctx, "ONLRET", Value::new(libc::ONLRET as i32));

    // Control flags (c_cflag)
    define(ctx, "CSIZE", Value::new(libc::CSIZE as i32));
    define(ctx, "CS5", Value::new(libc::CS5 as i32));
    define(ctx, "CS6", Value::new(libc::CS6 as i32));
    define(ctx, "CS7", Value::new(libc::CS7 as i32));
    define(ctx, "CS8", Value::new(libc::CS8 as i32));
    define(ctx, "CSTOPB", Value::new(libc::CSTOPB as i32));
    define(ctx, "CREAD", Value::new(libc::CREAD as i32));
    define(ctx, "PARENB", Value::new(libc::PARENB as i32));
    define(ctx, "PARODD", Value::new(libc::PARODD as i32));
    define(ctx, "HUPCL", Value::new(libc::HUPCL as i32));
    define(ctx, "CLOCAL", Value::new(libc::CLOCAL as i32));

    // Local flags (c_lflag)
    define(ctx, "ISIG", Value::new(libc::ISIG as i32));
    define(ctx, "ICANON", Value::new(libc::ICANON as i32));
    define(ctx, "ECHO", Value::new(libc::ECHO as i32));
    define(ctx, "ECHOE", Value::new(libc::ECHOE as i32));
    define(ctx, "ECHOK", Value::new(libc::ECHOK as i32));
    define(ctx, "ECHONL", Value::new(libc::ECHONL as i32));
    define(ctx, "NOFLSH", Value::new(libc::NOFLSH as i32));
    define(ctx, "TOSTOP", Value::new(libc::TOSTOP as i32));
    define(ctx, "IEXTEN", Value::new(libc::IEXTEN as i32));

    // Control characters indices
    define(ctx, "VINTR", Value::new(libc::VINTR as i32));
    define(ctx, "VQUIT", Value::new(libc::VQUIT as i32));
    define(ctx, "VERASE", Value::new(libc::VERASE as i32));
    define(ctx, "VKILL", Value::new(libc::VKILL as i32));
    define(ctx, "VEOF", Value::new(libc::VEOF as i32));
    define(ctx, "VTIME", Value::new(libc::VTIME as i32));
    define(ctx, "VMIN", Value::new(libc::VMIN as i32));
    define(ctx, "VSTART", Value::new(libc::VSTART as i32));
    define(ctx, "VSTOP", Value::new(libc::VSTOP as i32));
    define(ctx, "VSUSP", Value::new(libc::VSUSP as i32));

    // tcsetattr actions
    define(ctx, "TCSANOW", Value::new(libc::TCSANOW as i32));
    define(ctx, "TCSADRAIN", Value::new(libc::TCSADRAIN as i32));
    define(ctx, "TCSAFLUSH", Value::new(libc::TCSAFLUSH as i32));

    // tcflush queue selectors
    define(ctx, "TCIFLUSH", Value::new(libc::TCIFLUSH as i32));
    define(ctx, "TCOFLUSH", Value::new(libc::TCOFLUSH as i32));
    define(ctx, "TCIOFLUSH", Value::new(libc::TCIOFLUSH as i32));

    // tcflow actions
    define(ctx, "TCOOFF", Value::new(libc::TCOOFF as i32));
    define(ctx, "TCOON", Value::new(libc::TCOON as i32));
    define(ctx, "TCIOFF", Value::new(libc::TCIOFF as i32));
    define(ctx, "TCION", Value::new(libc::TCION as i32));

    // Baud rates
    define(ctx, "B0", Value::new(libc::B0 as i32));
    define(ctx, "B50", Value::new(libc::B50 as i32));
    define(ctx, "B75", Value::new(libc::B75 as i32));
    define(ctx, "B110", Value::new(libc::B110 as i32));
    define(ctx, "B134", Value::new(libc::B134 as i32));
    define(ctx, "B150", Value::new(libc::B150 as i32));
    define(ctx, "B200", Value::new(libc::B200 as i32));
    define(ctx, "B300", Value::new(libc::B300 as i32));
    define(ctx, "B600", Value::new(libc::B600 as i32));
    define(ctx, "B1200", Value::new(libc::B1200 as i32));
    define(ctx, "B1800", Value::new(libc::B1800 as i32));
    define(ctx, "B2400", Value::new(libc::B2400 as i32));
    define(ctx, "B4800", Value::new(libc::B4800 as i32));
    define(ctx, "B9600", Value::new(libc::B9600 as i32));
    define(ctx, "B19200", Value::new(libc::B19200 as i32));
    define(ctx, "B38400", Value::new(libc::B38400 as i32));
    define(ctx, "B57600", Value::new(libc::B57600 as i32));
    define(ctx, "B115200", Value::new(libc::B115200 as i32));
    define(ctx, "B230400", Value::new(libc::B230400 as i32));
}

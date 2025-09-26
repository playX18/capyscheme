use rsgc::Gc;

use crate::native_fn;

use crate::runtime::prelude::*;

native_fn!(
    register_str_fns:

    pub ("string-length") fn string_length<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> usize {
        let len = str.len();
        nctx.return_(len)
    }

    pub ("string-append") fn string_append<'gc>(nctx, strs: &'gc [Value<'gc>]) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if strs.len() == 0 {
            let s = Str::new(&nctx.ctx, "", false);
            return nctx.return_(Ok(s));
        }

        if strs.len() == 1 {
            if !strs[0].is::<Str>() {
                todo!()
            }

            return nctx.return_(Ok(strs[0].downcast::<Str>()));
        }


        let mut buffer = String::new();

        for str in strs.iter() {
            if !str.is::<Str>() {
                todo!()
            }

            buffer.push_str(&str.downcast::<Str>().to_string());
        }
        let s = Ok(Str::new(&nctx.ctx, &buffer, false));
        nctx.return_(s)
    }

    pub ("symbol-append") fn symbol_append<'gc>(nctx, syms: &'gc [Value<'gc>]) -> Result<Gc<'gc, Symbol<'gc>>, Value<'gc>> {
        if syms.len() == 0 {
            let s = Symbol::from_str(nctx.ctx, "");
            return nctx.return_(Ok(s));
        }

        if syms.len() == 1 {
            if !syms[0].is::<Symbol>() {
                todo!()
            }

            return nctx.return_(Ok(syms[0].downcast::<Symbol>()));
        }


        let mut buffer = String::new();

        for sym in syms.iter() {
            if !sym.is::<Symbol>() {
                todo!()
            }

            buffer.push_str(&sym.downcast::<Symbol>().to_string());
        }
        let s = Ok(Symbol::from_str(nctx.ctx, &buffer));
        nctx.return_(s)
    }

    pub ("symbol->string") fn symbol_to_string<'gc>(nctx, sym: Gc<'gc, Symbol<'gc>>) -> Gc<'gc, Str<'gc>> {
        let s = sym.to_str(&nctx.ctx);
        nctx.return_(s)
    }

    pub ("string->symbol") fn string_to_symbol<'gc>(nctx, str: Gc<'gc, Str<'gc>>) -> Gc<'gc, Symbol<'gc>> {
        let sym = Symbol::from_string(nctx.ctx, str);
        nctx.return_(sym)
    }

    pub ("print") fn print<'gc>(nctx, args: &'gc [Value<'gc>]) -> () {
        for arg in args.iter() {
            print!("{}", arg);
        }
        println!();
        nctx.return_(())
    }

    pub ("substring") fn substring<'gc>(nctx, str: Gc<'gc, Str<'gc>>, start: usize, end: usize) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if start > end || end > str.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "substring",
                "invalid start or end",
                None,
                None,
                3,
                &[str.into(), start.into_value(ctx), end.into_value(ctx)]
            );
        }

        let s = Str::substring(str, &nctx.ctx, start, end);

        nctx.return_(Ok(s))
    }

    pub ("string?") fn is_string<'gc>(nctx, val: Value<'gc>) -> bool {
        nctx.return_(val.is::<Str>())
    }

    pub ("symbol?") fn is_symbol<'gc>(nctx, val: Value<'gc>) -> bool {
        nctx.return_(val.is::<Symbol>())
    }
);

pub fn init_strings<'gc>(ctx: Context<'gc>) {
    let _ = register_str_fns(ctx);
}

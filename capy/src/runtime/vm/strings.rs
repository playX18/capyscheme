use rsgc::Gc;

use crate::native_fn;

use crate::runtime::prelude::*;

native_fn!(
    register_str_fns:


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
);

pub fn init_strings<'gc>(ctx: Context<'gc>) {
    let _ = register_str_fns(ctx);
}

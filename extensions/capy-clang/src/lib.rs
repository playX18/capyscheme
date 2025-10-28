use std::sync::OnceLock;

use capy::runtime::vm::VMResult;
use clang::Clang;

use capy::native_fn;
use capy::runtime::prelude::*;

native_fn! {
    register_clang_fns:

    pub ("clang-parse") fn clang_parse<'gc>(
        nctx,
        path: StringRef<'gc>
    ) -> Value<'gc> {
        let path = path.to_string();
        let ctx = nctx.ctx;
        parse(&ctx, &path);
        nctx.return_(Value::null())
    }
}

fn parse<'gc>(_ctx: &Context<'gc>, path: &str) {
    let clang = Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, false);
    let parser = index.parser(path);
    let translation_unit = parser.parse().unwrap();
    let entity = translation_unit.get_entity();

    println!("Entity: {:?}", entity);
}

#[unsafe(no_mangle)]
pub extern "C-unwind" fn capy_register_extension<'gc>(ctx: &Context<'gc>) -> VMResult<'gc> {
    static REGISTER: OnceLock<()> = OnceLock::new();
    REGISTER.get_or_init(|| {
        ctx.define("clang", "clang-parse", get_clang_parse_static_closure(*ctx));
    });

    VMResult::Ok(Value::new(true))
}

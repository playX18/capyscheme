use capy::prelude::*;
use capy::runtime::vm::VMResult;
use clang::Clang;
use std::sync::OnceLock;

#[scheme(path=clang)]
mod clang_ops {
    #[scheme(name = "clang-parse")]
    pub fn clang_parse(path: StringRef<'gc>) -> Value<'gc> {
        let path_str = path.to_string();
        parse(&ctx, &path_str);

        nctx.return_(Value::new(true))
    }
}

#[unsafe(no_mangle)]
pub extern "C-unwind" fn capy_register_extension<'gc>(ctx: &Context<'gc>) -> VMResult<'gc> {
    static REGISTER: OnceLock<()> = OnceLock::new();
    REGISTER.get_or_init(|| {
        clang_ops::register(*ctx);
    });

    VMResult::Ok(Value::new(true))
}
fn parse<'gc>(_ctx: &Context<'gc>, path: &str) {
    let clang = Clang::new().unwrap();
    let index = clang::Index::new(&clang, false, false);
    let parser = index.parser(path);
    let translation_unit = parser.parse().unwrap();
    let entity = translation_unit.get_entity();

    println!("Entity: {:?}", entity);
}

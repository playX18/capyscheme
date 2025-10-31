use std::sync::OnceLock;

use capy::runtime::vm::VMResult;
use clang::Clang;

use capy::prelude::*;

#[scheme(path=clang)]
mod clang_ops {
    #[scheme(name = "clang-parse")]
    pub fn clang_parse(path: StringRef<'gc>) -> Value<'gc> {
        let path_str = path.to_string();
        parse(&ctx, &path_str);

        nctx.return_(Value::new(true))
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
        clang_ops::register(*ctx);
    });

    println!("clang extension loaded");
    VMResult::Ok(Value::new(true))
}

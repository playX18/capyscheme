use std::path::Path;

use crate::cps::term::FuncRef;
use crate::rsgc::Gc;
use crate::runtime::{
    Context,
    modules::Module,
    value::Value,
};

#[cfg(feature = "bootstrap")]
use crate::{
    compiler::lower_to_cps,
    expander::core::denotations,
    runtime::value::Str,
    runtime::vm::thunks::{make_io_error, make_lexical_violation},
};

#[cfg(not(feature = "bootstrap"))]
pub fn compile_file<'gc>(
    _: Context<'gc>,
    file: impl AsRef<Path>,
    _: Option<Gc<'gc, Module<'gc>>>,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    unreachable!(
        "compile_file should not be called after bootstrap is complete, trying to compile: {}",
        file.as_ref().display()
    );
}

#[cfg(feature = "bootstrap")]
pub fn compile_file<'gc>(
    ctx: Context<'gc>,
    file: impl AsRef<Path>,
    env: Option<Gc<'gc, Module<'gc>>>,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    let module = env.unwrap_or_else(|| ctx.globals().root_module());
    let file = file.as_ref();

    let file_in = std::fs::File::open(file).map_err(|e| {
        make_io_error(
            ctx,
            "compile-file",
            Str::new(
                *ctx,
                format!("Cannot open input file '{}': {}", file.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let text = std::io::read_to_string(&file_in).map_err(|e| {
        make_io_error(
            ctx,
            "compile-file",
            Str::new(
                *ctx,
                format!("Cannot read input file '{}': {}", file.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;
    let src = Str::new(*ctx, file.display().to_string(), true);
    let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), false);

    let program = parser.read_program().map_err(|err| {
        make_lexical_violation(ctx, "compile-file", err.display_with_file(file.display()))
    })?;

    let mut env =
        crate::expander::core::Cenv::new(ctx, Value::new(false), module, denotations(ctx));
    let begin = env.denotations.denotation_of_begin;

    let mut ls = Value::null();
    for expr in program.iter().rev() {
        ls = Value::cons(ctx, *expr, ls);
    }
    ls = Value::cons(ctx, begin.into(), ls);

    let il = crate::expander::core::expand(&mut env, ls)
        .map_err(|err| make_lexical_violation(ctx, "compile-file", err.to_string()))?;

    lower_to_cps(ctx, il, Some(module), true)
}

use std::path::Path;

use crate::compiler::LoweredProgram;
use crate::runtime::{MutationContext, value::Value};

#[cfg(feature = "bootstrap")]
use crate::{
    compiler::lower_to_cps,
    expander::core::denotations,
    runtime::value::Str,
    runtime::vm::thunks::{make_io_error, make_lexical_violation},
};

#[cfg(not(feature = "bootstrap"))]
pub fn compile_file<'gc>(
    _: MutationContext<'gc>,
    file: impl AsRef<Path>,
    _: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    unreachable!(
        "compile_file should not be called after bootstrap is complete, trying to compile: {}",
        file.as_ref().display()
    );
}

#[cfg(feature = "bootstrap")]
pub fn compile_file<'gc>(
    ctx: MutationContext<'gc>,
    file: impl AsRef<Path>,
    dump_graph: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    let file = file.as_ref();

    let file_in = std::fs::File::open(file).map_err(|e| {
        make_io_error(
            ctx,
            "compile-file",
            Str::new(
                ctx,
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
                ctx,
                format!("Cannot read input file '{}': {}", file.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;
    let src = Str::new(ctx, file.display().to_string(), true);
    let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), false);

    let program = parser.read_program().map_err(|err| {
        make_lexical_violation(ctx, "compile-file", err.display_with_file(file.display()))
    })?;

    let mut env = crate::expander::core::Cenv::new(ctx, Value::new(false), denotations(ctx));

    if let Some((header, used)) = crate::expander::boot_linklet::parse_header(ctx, &program)
        .map_err(|err| make_lexical_violation(ctx, "compile-file", err.to_string()))?
    {
        let il =
            crate::expander::boot_linklet::expand_linklet(&mut env, file, header, &program[used..])
                .map_err(|err| {
                    make_lexical_violation(
                        ctx,
                        "compile-file",
                        format!("{}: {err}", file.display()),
                    )
                })?;
        return lower_to_cps(ctx, il, true, dump_graph);
    }

    let begin = env.denotations.denotation_of_begin;

    let mut ls = Value::null();
    for expr in program.iter().rev() {
        ls = Value::cons(ctx, *expr, ls);
    }
    ls = Value::cons(ctx, begin, ls);

    let il = crate::expander::core::expand(&mut env, ls).map_err(|err| {
        make_lexical_violation(ctx, "compile-file", format!("{}: {err}", file.display()))
    })?;

    lower_to_cps(ctx, il, true, dump_graph)
}

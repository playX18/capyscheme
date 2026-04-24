use std::{
    fs::{self, OpenOptions},
    io::BufWriter,
    path::Path,
};

use crate::{
    cps::term::FuncRef,
    runtime::{
        Context,
        stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
        value::{Str, Value},
        vm::thunks::make_io_error,
    },
};

pub(crate) fn compile_cps_to_cps_ssa_file<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    output: impl AsRef<Path>,
) -> Result<(), Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::CpsSsaEmit);
    let output = output.as_ref();
    if let Some(parent) = output.parent()
        && !parent.exists()
    {
        fs::create_dir_all(parent).map_err(|err| {
            make_io_error(
                ctx,
                "compile-cps-ssa",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot create output directory '{}': {}",
                        parent.display(),
                        err
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    }

    let module = lower_cps(ctx, cps).map_err(|err| {
        make_io_error(
            ctx,
            "compile-cps-ssa",
            Str::new(
                *ctx,
                format!(
                    "Failed to lower CPS to CPS-SSA for '{}': {}",
                    output.display(),
                    err
                ),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(output)
        .map_err(|err| {
            make_io_error(
                ctx,
                "compile-cps-ssa",
                Str::new(
                    *ctx,
                    format!("Cannot open output file '{}': {}", output.display(), err),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    let writer = BufWriter::new(file);
    module.write_to_writer(ctx, writer).map_err(|err| {
        make_io_error(
            ctx,
            "compile-cps-ssa",
            Str::new(
                *ctx,
                format!(
                    "Cannot write CPS-SSA artifact '{}' to disk: {}",
                    output.display(),
                    err
                ),
                true,
            )
            .into(),
            &[],
        )
    })
}

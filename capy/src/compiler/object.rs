use std::path::PathBuf;

use crate::compiler::LoweredProgram;
use crate::compiler::cranelift::ModuleBuilder;
use crate::compiler::ssa::LinearProgram;
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    Context,
    stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
    value::{Str, Value},
};

#[derive(Clone, Debug, Default)]
pub struct BackendDumpOptions {
    pub cranelift: Option<PathBuf>,
    pub disassembly: Option<PathBuf>,
}

#[derive(Clone, Debug, Default)]
pub struct CompilationOptions {
    pub backtraces: bool,
    pub backend_dumps: BackendDumpOptions,
}

pub(crate) fn compile_lowered_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    lowered: &LoweredProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Cranelift);
    compile_graph_linear_cps_to_fasl_bytes(ctx, lowered.linear_cps.clone(), opts)
}

fn compile_graph_linear_cps_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    linear: LinearProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let mut module_builder = ModuleBuilder::new_with_program(ctx, linear);
    module_builder.stacktraces = opts.backtraces;
    module_builder
        .compile_loaded_fasl_bytes_with_dumps(&opts.backend_dumps)
        .map_err(|err| {
            make_io_error(
                ctx,
                "compile",
                Str::new(*ctx, format!("Cannot compile unified FASL: {err}"), true).into(),
                &[],
            )
        })
}

use crate::compiler::LoweredProgram;
use crate::compiler::ssa::ModuleBuilder;
use crate::cps::{
    ReifyInfo,
    linear::{LinearProgram, linearize},
    reify,
    term::FuncRef,
};
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    Context,
    stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
    value::{Str, Value},
};

#[derive(Clone, Copy, Debug)]
pub struct CompilationOptions {
    pub backtraces: bool,
}

impl Default for CompilationOptions {
    fn default() -> Self {
        Self { backtraces: false }
    }
}

pub fn compile_cps_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Cranelift);
    let reify_info = reify(ctx, cps);
    let linear = linearize(&reify_info);

    compile_linear_cps_to_fasl_bytes(ctx, reify_info, linear, opts)
}

pub(crate) fn compile_lowered_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    lowered: &LoweredProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Cranelift);
    compile_graph_linear_cps_to_fasl_bytes(ctx, lowered.linear_cps.clone(), opts)
}

fn compile_linear_cps_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    reify_info: ReifyInfo<'gc>,
    linear: LinearProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
    module_builder.stacktraces = opts.backtraces;
    module_builder.compile_loaded_fasl_bytes().map_err(|err| {
        make_io_error(
            ctx,
            "compile",
            Str::new(*ctx, format!("Cannot compile unified FASL: {err}"), true).into(),
            &[],
        )
    })
}

fn compile_graph_linear_cps_to_fasl_bytes<'gc>(
    ctx: Context<'gc>,
    linear: LinearProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let mut module_builder = ModuleBuilder::new_graph_linear(ctx, linear);
    module_builder.stacktraces = opts.backtraces;
    module_builder.compile_loaded_fasl_bytes().map_err(|err| {
        make_io_error(
            ctx,
            "compile",
            Str::new(*ctx, format!("Cannot compile unified FASL: {err}"), true).into(),
            &[],
        )
    })
}

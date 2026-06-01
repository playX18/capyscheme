use crate::compiler::ssa::ModuleBuilder;
use crate::cps::{linear::linearize, reify, term::FuncRef};
use crate::runtime::code_image::CompiledCodeImage;
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
        Self { backtraces: true }
    }
}

pub fn compile_cps_to_code_image<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    opts: CompilationOptions,
) -> Result<CompiledCodeImage, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Cranelift);
    let reify_info = reify(ctx, cps);
    let linear = linearize(&reify_info);

    let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
    module_builder.stacktraces = opts.backtraces;
    module_builder.compile_code_image().map_err(|err| {
        make_io_error(
            ctx,
            "compile",
            Str::new(*ctx, format!("Cannot compile FASL code image: {err}"), true).into(),
            &[],
        )
    })
}

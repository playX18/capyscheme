use std::{io::Write, path::Path};

use crate::compiler::{linkutils::Linker, ssa::ModuleBuilder};
use crate::cps::{linear::linearize, reify, term::FuncRef};
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    Context,
    stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
    value::{Str, Value},
};
use cranelift::prelude::Configurable;
use cranelift_codegen::settings;
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectModule, ObjectProduct};

#[derive(Clone, Copy, Debug)]
pub struct CompilationOptions {
    pub backtraces: bool,
}

impl Default for CompilationOptions {
    fn default() -> Self {
        Self { backtraces: true }
    }
}

pub fn compile_cps_to_object<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    opts: CompilationOptions,
) -> Result<ObjectProduct, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Cranelift);
    let reify_info = reify(ctx, cps);
    let linear = linearize(ctx, &reify_info);

    let mut shared_builder = settings::builder();
    shared_builder.set("enable_probestack", "false").unwrap();
    shared_builder
        .set("enable_heap_access_spectre_mitigation", "false")
        .unwrap();
    shared_builder.set("opt_level", "speed").unwrap();
    shared_builder.enable("preserve_frame_pointers").unwrap();
    shared_builder.enable("is_pic").unwrap();
    shared_builder.enable("enable_pinned_reg").unwrap();
    shared_builder.enable("enable_alias_analysis").unwrap();

    let shared_flags = settings::Flags::new(shared_builder);
    let triple = target_lexicon::Triple::host();
    let isa = cranelift_codegen::isa::lookup(triple)
        .unwrap()
        .finish(shared_flags)
        .unwrap();
    let objbuilder =
        cranelift_object::ObjectBuilder::new(isa, "scheme", default_libcall_names()).unwrap();
    let objmodule = ObjectModule::new(objbuilder);

    let mut module_builder = ModuleBuilder::new(ctx, objmodule, reify_info, linear);
    module_builder.stacktraces = opts.backtraces;
    module_builder.compile();

    let mut product = module_builder.module.finish();
    module_builder.debug_context.emit(&mut product);
    Ok(product)
}

#[cfg(feature = "aot")]
pub(crate) fn compile_cps_to_shared_object<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    opts: CompilationOptions,
    output: impl AsRef<Path>,
) -> Result<(), Value<'gc>> {
    let product = compile_cps_to_object(ctx, cps, opts)?;
    link_object_product(ctx, product, output)
}

#[allow(unused_mut)]
pub fn link_object_product<'gc>(
    ctx: Context<'gc>,
    mut product: ObjectProduct,
    output: impl AsRef<Path>,
) -> Result<(), Value<'gc>> {
    let output = output.as_ref();
    let obj_output = output.with_extension("o");
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(&obj_output)
        .map_err(|e| {
            make_io_error(
                ctx,
                "link-object",
                Str::new(
                    *ctx,
                    format!("Cannot open output file '{}': {}", obj_output.display(), e),
                    true,
                )
                .into(),
                &[],
            )
        })?;

    #[cfg(target_os = "macos")]
    {
        product
            .object
            .set_macho_build_version(macho_object_build_version_for_target());
    }

    let bytes = {
        let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::ObjectEmit);
        product.emit().map_err(|e| {
            make_io_error(
                ctx,
                "link-object",
                Str::new(
                    *ctx,
                    format!("Cannot emit object file '{}': {}", obj_output.display(), e),
                    true,
                )
                .into(),
                &[],
            )
        })?
    };

    file.write_all(&bytes).map_err(|e| {
        make_io_error(
            ctx,
            "link-object",
            Str::new(
                *ctx,
                format!("Cannot write object file '{}': {}", obj_output.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let linker = Linker::new().map_err(|e| {
        make_io_error(
            ctx,
            "link-object",
            Str::new(*ctx, format!("Failed to find linker: {}", e), true).into(),
            &[],
        )
    })?;

    {
        let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Link);
        linker.link(&obj_output, output).map_err(|e| {
            make_io_error(
                ctx,
                "link-object",
                Str::new(
                    *ctx,
                    format!(
                        "Linking object file '{}' to output file '{}' failed: {}",
                        obj_output.display(),
                        output.display(),
                        e
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    }

    std::fs::remove_file(&obj_output).map_err(|e| {
        make_io_error(
            ctx,
            "link-object",
            Str::new(
                *ctx,
                format!(
                    "Cannot remove temporary object file '{}': {}",
                    obj_output.display(),
                    e
                ),
                true,
            )
            .into(),
            &[],
        )
    })?;

    Ok(())
}

#[cfg(target_os = "macos")]
fn macho_object_build_version_for_target() -> object::write::MachOBuildVersion {
    fn pack_version((major, minor): (u32, u32)) -> u32 {
        (major << 16) | (minor << 8)
    }

    let platform = object::macho::PLATFORM_MACOS;
    let min_os = (11, 0);
    let sdk = (13, 1);
    let mut build_version = object::write::MachOBuildVersion::default();
    build_version.platform = platform;
    build_version.minos = pack_version(min_os);
    build_version.sdk = pack_version(sdk);
    build_version
}

use std::{io::Write, path::Path};

use crate::{
    compiler::{linkutils::Linker, ssa::ModuleBuilder},
    cps::{contify::contify, reify, term::FuncRef},
    expander::{
        assignment_elimination, compile_cps, core::denotations, fix_letrec::fix_letrec, primitives,
    },
    runtime::{
        Context,
        modules::{Module, root_module},
        value::{Str, Value},
        vm::thunks::{make_io_error, make_lexical_violation},
    },
};
use cranelift::prelude::Configurable;
use cranelift_codegen::settings;
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectModule, ObjectProduct};
use rsgc::Gc;

#[macro_export]
macro_rules! call_signature {
    ($callconv: ident ($($arg: ident),*) -> $ret: ident) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
                sig.returns.push(ir::AbiParam::new(ir::types::$ret));
            }
            sig
        }
    };

    ($callconv: ident ($($arg: ident),*) -> ($($ret:ident),*)) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
                $(
                    sig.returns.push(ir::AbiParam::new(ir::types::$ret));
                )*
            }

            sig
        }
    };

    ($callconv: ident ($($arg: ident),*)) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
            }

            sig
        }
    }
}

pub mod debuginfo;
pub mod linkutils;
pub mod ssa;

pub fn compile_file<'gc>(
    ctx: Context<'gc>,
    file: impl AsRef<Path>,

    env: Option<Gc<'gc, Module<'gc>>>,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    let module = env.unwrap_or_else(|| *root_module(ctx));
    let file = file.as_ref();
    println!(";; Compiling file: {}", file.display());
    let file_in = std::fs::File::open(file).map_err(|e| {
        make_io_error(
            &ctx,
            "compile-file",
            Str::new(
                &ctx,
                format!("Cannot open input file '{}': {}", file.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let text = std::io::read_to_string(&file_in).map_err(|e| {
        make_io_error(
            &ctx,
            "compile-file",
            Str::new(
                &ctx,
                format!("Cannot read input file '{}': {}", file.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;
    let src = Str::new(&ctx, file.display().to_string(), true);
    let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into());

    let program = parser.read_program().map_err(|err| {
        make_lexical_violation(&ctx, "compile-file", err.to_string(file.display()))
    })?;

    let mut env =
        crate::expander::core::Cenv::new(ctx, Value::new(false), module, denotations(ctx));

    let begin = env.denotations.denotation_of_begin;

    let mut ls = Value::null();
    for expr in program.iter().rev() {
        ls = Value::cons(ctx, *expr, ls);
    }
    ls = Value::cons(ctx, begin.into(), ls);

    let mut il = crate::expander::core::expand(&mut env, ls)
        .map_err(|err| make_lexical_violation(&ctx, "compile-file", err.to_string()))?;

    il = fix_letrec(ctx, il);
    il = assignment_elimination::eliminate_assignments(ctx, il);
    il = primitives::resolve_primitives(ctx, il, module);
    il = primitives::expand_primitives(ctx, il);

    let mut cps = compile_cps::cps_toplevel(ctx, &[il]);

    cps = crate::cps::rewrite_func(ctx, cps);

    cps = cps.with_body(ctx, contify(ctx, cps.body));

    if false {
        let file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open("cps.scm")
            .expect("Failed to open file");

        let mut writer = std::io::BufWriter::new(file);
        let doc = cps.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        doc.1.render(70, &mut writer).unwrap();
        writeln!(writer, "").unwrap();
        writer.flush().unwrap();

        let file = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open("tree.scm")
            .expect("Failed to open file");

        let mut writer = std::io::BufWriter::new(file);
        let doc = il.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        doc.1.render(70, &mut writer).unwrap();
        writeln!(writer, "").unwrap();
        writer.flush().unwrap();
    }

    Ok(cps)
}

pub fn compile_cps_to_object<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
) -> Result<ObjectProduct, Value<'gc>> {
    let reify_info = reify(ctx, cps);

    let mut shared_builder = settings::builder();

    shared_builder.set("opt_level", "speed_and_size").unwrap();
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

    let mut module_builder = ModuleBuilder::new(ctx, objmodule, reify_info);

    module_builder.compile();
    let mut product = module_builder.module.finish();

    module_builder.debug_context.emit(&mut product);
    return Ok(product);
}

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
                &ctx,
                "link-object",
                Str::new(
                    &ctx,
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
    let bytes = product.emit().map_err(|e| {
        make_io_error(
            &ctx,
            "link-object",
            Str::new(
                &ctx,
                format!("Cannot emit object file '{}': {}", obj_output.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;

    file.write_all(&bytes).map_err(|e| {
        make_io_error(
            &ctx,
            "link-object",
            Str::new(
                &ctx,
                format!("Cannot write object file '{}': {}", obj_output.display(), e),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let linker = Linker::new();

    linker.link(&obj_output, &output).map_err(|e| {
        make_io_error(
            &ctx,
            "link-object",
            Str::new(
                &ctx,
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
    if false {
        std::fs::remove_file(&obj_output).map_err(|e| {
            make_io_error(
                &ctx,
                "link-object",
                Str::new(
                    &ctx,
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
    }

    Ok(())
}

#[cfg(target_os = "macos")]
fn macho_object_build_version_for_target() -> object::write::MachOBuildVersion {
    /// The `object` crate demands "X.Y.Z encoded in nibbles as xxxx.yy.zz"
    /// e.g. minOS 14.0 = 0x000E0000, or SDK 16.2 = 0x00100200
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

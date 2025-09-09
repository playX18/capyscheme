use std::{io::Write, path::Path, process::Command};

use cranelift::prelude::Configurable;
use cranelift_codegen::settings;
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectModule, ObjectProduct};
use rsgc::Gc;
use std::env;

use crate::{
    compiler::ssa::ModuleBuilder,
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
pub mod ssa;

pub fn compile_file<'gc>(
    ctx: Context<'gc>,
    file: impl AsRef<Path>,

    env: Option<Gc<'gc, Module<'gc>>>,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    let module = env.unwrap_or_else(|| *root_module(ctx));
    let file = file.as_ref();
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

    let mut cps = compile_cps::cps_toplevel(ctx, &[il]);
    cps = crate::cps::rewrite_func(ctx, cps);
    cps = cps.with_body(ctx, contify(ctx, cps.body));
    if log::log_enabled!(log::Level::Debug) {
        let mut buf = Vec::new();
        let doc = cps.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        log::debug!("CPS after contification:");
        doc.render(80, &mut buf).unwrap();
        log::debug!("{}", String::from_utf8(buf).unwrap());
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

    let isa = cranelift_codegen::isa::lookup_by_name("x86_64-unknown-linux")
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
    product: ObjectProduct,
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

    let linker = "cc"; // TODO: configurable/detect automatically
    let mut command = Command::new(linker);
    rlpaths(&mut command);
    let status = command
        .arg("-o")
        .arg(output)
        .arg(&obj_output)
        .arg("-lcapy")
        .arg("-shared")
        .arg("-fPIC")
        .status()
        .map_err(|e| {
            make_io_error(
                &ctx,
                "link-object",
                Str::new(
                    &ctx,
                    format!("Cannot invoke linker '{}': {}", linker, e),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    if !status.success() {
        return Err(make_io_error(
            &ctx,
            "link-object",
            Str::new(
                &ctx,
                format!("Linker '{}' failed with exit code {}", linker, status),
                true,
            )
            .into(),
            &[],
        ));
    }

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

    Ok(())
}

/// Given command add rlpath linker flags to it.
fn rlpaths(command: &mut Command) {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_dir = manifest_dir.parent().unwrap_or(manifest_dir);
    {
        let profile = if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        };
        let target_dir = workspace_dir.join("target").join(profile);
        println!("target dir: {}", target_dir.display());
        command.arg("-Wl,-rpath").arg(target_dir.to_str().unwrap());
        command.arg("-L").arg(target_dir.to_str().unwrap());
    }
}

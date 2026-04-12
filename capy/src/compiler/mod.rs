//! Native code compilation pipeline: CPS → SSA → Cranelift → machine code.

use std::{io::Write, path::Path};

use crate::compiler::{linkutils::Linker, ssa::ModuleBuilder};
use crate::cps::contify::contify;
use crate::cps::{reify, term::FuncRef};
use crate::expander::core::TermRef;
use crate::expander::{
    assignment_elimination, compile_cps, eta_expand::eta_expand, fix_letrec::fix_letrec,
    free_vars::resolve_free_vars, letrectify::letrectify, primitives,
};
use crate::rsgc::Gc;
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    Context,
    modules::Module,
    value::{Str, Value},
};
#[cfg(feature = "bootstrap")]
use crate::{expander::core::denotations, runtime::vm::thunks::make_lexical_violation};
use cranelift::prelude::Configurable;
use cranelift_codegen::settings;
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectModule, ObjectProduct};

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

pub mod bytecompiler;
pub mod debuginfo;
pub mod linkutils;
pub mod ssa;

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

    //println!(";; (Pre-boot) Compiling file: {}", file.display());
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

    Ok(lower_to_cps(ctx, il, Some(module), true)?)
}

pub struct CompilationOptions {
    pub backtraces: bool,
}

pub fn lower_to_cps<'gc>(
    ctx: Context<'gc>,
    mut il: TermRef<'gc>,
    module: Option<Gc<'gc, Module<'gc>>>,
    expand_primitives: bool,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    if expand_primitives && let Some(module) = module {
        il = primitives::resolve_primitives(ctx, il, module);
        il = primitives::expand_primitives(ctx, il);
    }

    let il = resolve_free_vars(ctx, il);
    let il = letrectify(ctx, il);
    let il = fix_letrec(ctx, il);
    let il = eta_expand(ctx, il);
    let il = assignment_elimination::eliminate_assignments(ctx, il);

    let mut cps = compile_cps::cps_toplevel(ctx, &[il]);
    cps = crate::cps::rewrite_func(ctx, cps);
    cps = cps.with_body(ctx, contify(ctx, cps.body()));
    Ok(cps)
}

impl Default for CompilationOptions {
    fn default() -> Self {
        CompilationOptions { backtraces: true }
    }
}

pub fn compile_cps_to_object<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    opts: CompilationOptions,
) -> Result<ObjectProduct, Value<'gc>> {
    let reify_info = reify(ctx, cps);

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

    let mut module_builder = ModuleBuilder::new(ctx, objmodule, reify_info);
    module_builder.stacktraces = opts.backtraces;
    module_builder.compile();
    let mut product = module_builder.module.finish();

    module_builder.debug_context.emit(&mut product);
    return Ok(product);
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
    let bytes = product.emit().map_err(|e| {
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
    })?;

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

    linker.link(&obj_output, &output).map_err(|e| {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::term::{Atom, Cont, Expression, Func, Term},
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };
    use object::{Object, ObjectSymbol};

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[Atom<'gc>]) -> crate::cps::term::Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    fn vars<'gc>(ctx: Context<'gc>, vars: &[LVarRef<'gc>]) -> crate::cps::term::Vars<'gc> {
        Array::from_slice(*ctx, vars)
    }

    fn mk_func<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        retk: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
    ) -> Gc<'gc, Func<'gc>> {
        Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, name).into(),
                source: Value::new(false),
                binding,
                return_cont: retk,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        )
    }

    fn mk_cont<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
    ) -> Gc<'gc, Cont<'gc>> {
        Gc::new(
            *ctx,
            Cont {
                name: Symbol::from_str(ctx, name).into(),
                binding,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                source: Value::new(false),
                free_vars: Lock::new(None),
                reified: std::cell::Cell::new(false),
                cold: false,
                noinline: false,
                meta: Value::new(false),
            },
        )
    }

    fn term_continue<'gc>(
        ctx: Context<'gc>,
        k: LVarRef<'gc>,
        args: &[Atom<'gc>],
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(*ctx, Term::Continue(k, atoms(ctx, args), Value::new(false)))
    }

    fn term_app<'gc>(
        ctx: Context<'gc>,
        callee: Atom<'gc>,
        retk: LVarRef<'gc>,
        args: &[Atom<'gc>],
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(
            *ctx,
            Term::App(callee, retk, atoms(ctx, args), Value::new(false)),
        )
    }

    fn term_let_prim<'gc>(
        ctx: Context<'gc>,
        binding: LVarRef<'gc>,
        prim: &str,
        args: &[Atom<'gc>],
        body: Gc<'gc, Term<'gc>>,
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(
            *ctx,
            Term::Let(
                binding,
                Expression::PrimCall(
                    Symbol::from_str(ctx, prim).into(),
                    atoms(ctx, args),
                    Value::new(false),
                ),
                body,
            ),
        )
    }

    fn term_letk<'gc>(
        ctx: Context<'gc>,
        conts: &[Gc<'gc, Cont<'gc>>],
        body: Gc<'gc, Term<'gc>>,
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(*ctx, Term::Letk(Array::from_slice(*ctx, conts), body))
    }

    fn term_fix<'gc>(
        ctx: Context<'gc>,
        funcs: &[Gc<'gc, Func<'gc>>],
        body: Gc<'gc, Term<'gc>>,
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(*ctx, Term::Fix(Array::from_slice(*ctx, funcs), body))
    }

    #[test]
    fn emitted_object_contains_aot_code_block_init_symbols() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let retk = lvar(ctx, "retk");
            let g = lvar(ctx, "g");
            let g_retk = lvar(ctx, "g_retk");
            let k = lvar(ctx, "k");
            let tmp = lvar(ctx, "tmp");
            let x = lvar(ctx, "x");
            let y = lvar(ctx, "y");

            let g_body = term_continue(ctx, g_retk, &[Atom::Local(y)]);
            let g_func = mk_func(ctx, "g", g, g_retk, &[y], None, g_body);

            let k_body = term_continue(ctx, retk, &[Atom::Local(x)]);
            let k_cont = mk_cont(ctx, "k", k, &[x], None, k_body);

            let body = term_letk(
                ctx,
                &[k_cont],
                term_let_prim(
                    ctx,
                    tmp,
                    "cons",
                    &[Atom::Local(k), 0.into()],
                    term_fix(
                        ctx,
                        &[g_func],
                        term_app(ctx, Atom::Local(g), k, &[1.into()]),
                    ),
                ),
            );
            let func = mk_func(ctx, "f", f, retk, &[], None, body);

            let product = compile_cps_to_object(ctx, func, Default::default()).unwrap();
            let bytes = product.emit().unwrap();
            let file = object::File::parse(bytes.as_slice()).unwrap();
            let symbols = file
                .symbols()
                .filter_map(|symbol| symbol.name().ok().map(str::to_owned))
                .collect::<Vec<_>>();

            assert!(symbols.iter().any(|name| name == "capy_module_init"));
            assert!(
                symbols
                    .iter()
                    .any(|name| name == "capy_thunks_make_aot_code_block")
            );
            assert!(
                symbols
                    .iter()
                    .filter(|name| name.starts_with("codeblock_fn"))
                    .count()
                    >= 2
            );
            assert!(
                symbols
                    .iter()
                    .any(|name| name.starts_with("codeblock_cont"))
            );
        });
    }
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

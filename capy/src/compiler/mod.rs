//! Native code compilation pipeline: CPS → SSA → Cranelift → machine code.

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

mod artifact;
pub mod bytecompiler;
mod bootstrap;
pub mod debuginfo;
pub mod linkutils;
mod object;
mod pipeline;
pub mod ssa;

pub use bootstrap::compile_file;
pub use object::{CompilationOptions, compile_cps_to_object, link_object_product};
pub use pipeline::lower_to_cps;

pub(crate) use artifact::compile_cps_to_cps_ssa_file;
pub(crate) use object::compile_cps_to_shared_object;
pub(crate) use pipeline::{
    DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, lower_expanded_to_cps,
};

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
    use ::object::{File, Object, ObjectSymbol};

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
            let file = File::parse(bytes.as_slice()).unwrap();
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

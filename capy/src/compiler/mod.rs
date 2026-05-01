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
mod bootstrap;
pub mod bytecode;
pub mod debuginfo;
pub mod linkutils;
mod object;
mod pipeline;
pub mod ssa;

pub use bootstrap::compile_file;
pub use object::{CompilationOptions, compile_cps_to_object, link_object_product};
pub use pipeline::lower_to_cps;

pub(crate) use object::compile_cps_to_shared_object;
pub(crate) use pipeline::{
    DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, lower_expanded_to_cps,
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::term::{Atom, BranchHint, Cont, Expression, Func, Term},
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

    fn term_if<'gc>(
        ctx: Context<'gc>,
        test: Atom<'gc>,
        consequent: LVarRef<'gc>,
        consequent_args: Option<&[Atom<'gc>]>,
        alternative: LVarRef<'gc>,
        alternative_args: Option<&[Atom<'gc>]>,
    ) -> Gc<'gc, Term<'gc>> {
        Gc::new(
            *ctx,
            Term::If {
                test,
                consequent,
                consequent_args: consequent_args.map(|args| atoms(ctx, args)),
                alternative,
                alternative_args: alternative_args.map(|args| atoms(ctx, args)),
                hints: [BranchHint::Normal, BranchHint::Normal],
            },
        )
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

    #[test]
    fn inline_keeps_recursive_function_out_of_inline_env() {
        with_ctx(|ctx| {
            let loop_fn = lvar(ctx, "loop");
            let loop_retk = lvar(ctx, "loop_retk");
            let entry_retk = lvar(ctx, "entry_retk");
            let tmp = lvar(ctx, "tmp");

            let loop_body = term_let_prim(
                ctx,
                tmp,
                "cons",
                &[0.into(), 1.into()],
                term_app(ctx, Atom::Local(loop_fn), loop_retk, &[]),
            );
            let loop_func = mk_func(ctx, "loop", loop_fn, loop_retk, &[], None, loop_body);
            let term = term_fix(
                ctx,
                &[loop_func],
                term_app(ctx, Atom::Local(loop_fn), entry_retk, &[]),
            );

            let (optimized, _) = crate::cps::optimizer::inline(ctx, term, usize::MAX);

            match &*optimized {
                Term::Fix(funcs, body) => {
                    assert_eq!(funcs.len(), 1);
                    assert!(matches!(
                        &**body,
                        Term::App(Atom::Local(fun), retk, args, _)
                            if *fun == loop_fn && *retk == entry_retk && args.is_empty()
                    ));
                }
                other => panic!("expected recursive function to remain as Fix/App, got {other:?}"),
            }
        });
    }

    #[test]
    fn inline_keeps_recursive_continuation_out_of_inline_env() {
        with_ctx(|ctx| {
            let loop_k = lvar(ctx, "loop_k");
            let tmp = lvar(ctx, "tmp");

            let loop_body = term_let_prim(
                ctx,
                tmp,
                "cons",
                &[0.into(), 1.into()],
                term_continue(ctx, loop_k, &[]),
            );
            let loop_cont = mk_cont(ctx, "loop_k", loop_k, &[], None, loop_body);
            let term = term_letk(ctx, &[loop_cont], term_continue(ctx, loop_k, &[]));

            let (optimized, _) = crate::cps::optimizer::inline(ctx, term, usize::MAX);

            match &*optimized {
                Term::Letk(conts, body) => {
                    assert_eq!(conts.len(), 1);
                    assert!(matches!(
                        &**body,
                        Term::Continue(k, args, _) if *k == loop_k && args.is_empty()
                    ));
                }
                other => {
                    panic!(
                        "expected recursive continuation to remain as Letk/Continue, got {other:?}"
                    )
                }
            }
        });
    }

    #[test]
    fn rewrite_inlines_non_recursive_sibling_function() {
        with_ctx(|ctx| {
            let entry = lvar(ctx, "entry");
            let entry_retk = lvar(ctx, "entry_retk");
            let helper = lvar(ctx, "helper");
            let helper_retk = lvar(ctx, "helper_retk");
            let helper_tmp = lvar(ctx, "helper_tmp");
            let loop_fn = lvar(ctx, "loop");
            let loop_retk = lvar(ctx, "loop_retk");
            let loop_tmp = lvar(ctx, "loop_tmp");

            let helper_body = term_let_prim(
                ctx,
                helper_tmp,
                "cons",
                &[0.into(), 1.into()],
                term_continue(ctx, helper_retk, &[]),
            );
            let helper_func = mk_func(ctx, "helper", helper, helper_retk, &[], None, helper_body);

            let loop_body = term_let_prim(
                ctx,
                loop_tmp,
                "cons",
                &[0.into(), 1.into()],
                term_app(ctx, Atom::Local(loop_fn), loop_retk, &[]),
            );
            let loop_func = mk_func(ctx, "loop", loop_fn, loop_retk, &[], None, loop_body);

            let body = term_fix(
                ctx,
                &[loop_func, helper_func],
                term_app(ctx, Atom::Local(helper), entry_retk, &[]),
            );
            let entry_func = mk_func(ctx, "entry", entry, entry_retk, &[], None, body);

            let rewritten = crate::cps::rewrite_func(ctx, entry_func);

            assert!(matches!(
                &*rewritten.body(),
                Term::Continue(k, args, _) if *k == entry_retk && args.is_empty()
            ));
        });
    }

    #[test]
    fn shrink_unused_variadic_continuation_skips_list_materialization() {
        with_ctx(|ctx| {
            let retk = lvar(ctx, "retk");
            let restk = lvar(ctx, "restk");
            let head = lvar(ctx, "head");
            let rest = lvar(ctx, "rest");

            let cont = mk_cont(
                ctx,
                "restk",
                restk,
                &[head],
                Some(rest),
                term_continue(ctx, retk, &[Atom::Local(head)]),
            );
            let term = term_letk(
                ctx,
                &[cont],
                term_continue(ctx, restk, &[1.into(), 2.into(), 3.into()]),
            );

            let (shrunk, changed) = crate::cps::optimizer::shrink(ctx, term);

            assert!(changed);
            assert!(matches!(
                &*shrunk,
                Term::Continue(k, args, _)
                    if *k == retk
                        && args.len() == 1
                        && matches!(args[0], Atom::Constant(v) if v == Value::new(1))
            ));
        });
    }

    #[test]
    fn shrink_referenced_variadic_continuation_materializes_list() {
        with_ctx(|ctx| {
            let retk = lvar(ctx, "retk");
            let restk = lvar(ctx, "restk");
            let head = lvar(ctx, "head");
            let rest = lvar(ctx, "rest");

            let cont = mk_cont(
                ctx,
                "restk",
                restk,
                &[head],
                Some(rest),
                term_continue(ctx, retk, &[Atom::Local(rest)]),
            );
            let term = term_letk(
                ctx,
                &[cont],
                term_continue(ctx, restk, &[1.into(), 2.into(), 3.into()]),
            );

            let (shrunk, changed) = crate::cps::optimizer::shrink(ctx, term);

            fn cons_let_count<'gc>(term: Gc<'gc, Term<'gc>>) -> usize {
                match &*term {
                    Term::Let(_, Expression::PrimCall(_, _, _), body) => 1 + cons_let_count(*body),
                    Term::Continue(..) => 0,
                    other => panic!("expected cons materialization let-chain, got {other:?}"),
                }
            }

            fn final_continue<'gc>(term: Gc<'gc, Term<'gc>>) -> Gc<'gc, Term<'gc>> {
                match &*term {
                    Term::Let(_, _, body) => final_continue(*body),
                    Term::Continue(..) => term,
                    other => panic!("expected final continue after materialization, got {other:?}"),
                }
            }

            assert!(changed);
            assert_eq!(cons_let_count(shrunk), 2);
            assert!(matches!(
                &*final_continue(shrunk),
                Term::Continue(k, args, _)
                    if *k == retk
                        && args.len() == 1
                        && matches!(args[0], Atom::Local(_))
            ));
        });
    }

    #[test]
    fn shrink_reports_false_for_stable_if_without_branch_args() {
        with_ctx(|ctx| {
            let test = lvar(ctx, "test");
            let conseq = lvar(ctx, "conseq");
            let alt = lvar(ctx, "alt");
            let term = term_if(ctx, Atom::Local(test), conseq, None, alt, None);

            let (shrunk, changed) = crate::cps::optimizer::shrink(ctx, term);

            assert!(!changed);
            assert!(Gc::ptr_eq(shrunk, term));

            let (shrunk_again, changed_again) = crate::cps::optimizer::shrink(ctx, shrunk);
            assert!(!changed_again);
            assert!(Gc::ptr_eq(shrunk_again, shrunk));
        });
    }

    #[test]
    fn optimizer_handles_deep_let_spines_without_stack_growth() {
        with_ctx(|ctx| {
            const DEPTH: usize = 10_000;

            let retk = lvar(ctx, "retk");
            let seed = lvar(ctx, "seed");
            let mut term = term_continue(ctx, retk, &[]);

            for index in 0..DEPTH {
                let binding = lvar(ctx, &format!("deep_{index}"));
                term = term_let_prim(ctx, binding, "opaque", &[Atom::Local(seed)], term);
            }

            assert_eq!(crate::cps::optimizer::size(term), DEPTH + 1);

            let (shrunk, _) = crate::cps::optimizer::shrink(ctx, term);
            assert_eq!(crate::cps::optimizer::size(shrunk), DEPTH + 1);

            let (inlined, _) = crate::cps::optimizer::inline(ctx, shrunk, usize::MAX);
            assert_eq!(crate::cps::optimizer::size(inlined), DEPTH + 1);
        });
    }

    #[test]
    fn optimizer_copy_handles_deep_let_spines_without_stack_growth() {
        with_ctx(|ctx| {
            const DEPTH: usize = 10_000;

            let retk = lvar(ctx, "retk");
            let seed = lvar(ctx, "seed");
            let mut term = term_continue(ctx, retk, &[]);

            for index in 0..DEPTH {
                let binding = lvar(ctx, &format!("copy_{index}"));
                term = term_let_prim(ctx, binding, "opaque", &[Atom::Local(seed)], term);
            }

            let (copied, copied_size, mentions) =
                crate::cps::optimizer::copy_for_stress_test(ctx, term);

            assert_eq!(copied_size, DEPTH + 1);
            assert_eq!(crate::cps::optimizer::size(copied), DEPTH + 1);
            assert!(!mentions);
        });
    }

    #[test]
    fn shrink_materializes_large_variadic_rest_without_recursive_closures() {
        with_ctx(|ctx| {
            const REST_LEN: usize = 4_096;

            let retk = lvar(ctx, "retk");
            let restk = lvar(ctx, "restk");
            let rest = lvar(ctx, "rest");
            let cont = mk_cont(
                ctx,
                "restk",
                restk,
                &[],
                Some(rest),
                term_continue(ctx, retk, &[Atom::Local(rest)]),
            );
            let args = (0..REST_LEN)
                .map(|index| Atom::Constant(Value::new(index as i32)))
                .collect::<Vec<_>>();
            let term = term_letk(ctx, &[cont], term_continue(ctx, restk, &args));

            let (shrunk, changed) = crate::cps::optimizer::shrink(ctx, term);

            let mut count = 0;
            let mut cursor = shrunk;
            loop {
                match &*cursor {
                    Term::Let(_, Expression::PrimCall(_, _, _), body) => {
                        count += 1;
                        cursor = *body;
                    }
                    Term::Continue(k, args, _) => {
                        assert_eq!(*k, retk);
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Atom::Local(_)));
                        break;
                    }
                    other => panic!("expected materialized rest list, got {other:?}"),
                }
            }

            assert!(changed);
            assert_eq!(count, REST_LEN);
            assert_eq!(crate::cps::optimizer::size(shrunk), REST_LEN + 1);
        });
    }
}

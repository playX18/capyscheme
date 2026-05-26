use crate::{
    cps::{
        linear::{CodeId, Terminator, linearize},
        reify::reify,
        term::{Atom, Expression, Func, Term},
    },
    expander::core::{LVarRef, fresh_lvar},
    rsgc::{Gc, alloc::Array, cell::Lock},
    runtime::{
        Context, Scheme,
        value::{Symbol, Value, init_symbols},
        vm::exceptions::RaiseKind,
    },
};

static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        init_symbols(*ctx);
        f(ctx)
    });
}

fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
    fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
}

fn prim_call_func<'gc>(ctx: Context<'gc>, name: &str, args: &[LVarRef<'gc>]) -> Gc<'gc, Func<'gc>> {
    let f = lvar(ctx, "f");
    let retk = lvar(ctx, "retk");
    let result = lvar(ctx, "result");
    let prim = Symbol::from_str(ctx, name).into();
    let prim_args = args.iter().copied().map(Atom::Local).collect::<Vec<_>>();
    let body = Gc::new(
        *ctx,
        Term::Let(
            result,
            Expression::PrimCall(prim, Array::from_slice(*ctx, &prim_args), Value::new(false)),
            Gc::new(
                *ctx,
                Term::Continue(
                    retk,
                    Array::from_slice(*ctx, &[Atom::Local(result)]),
                    Value::new(false),
                ),
            ),
        ),
    );

    Gc::new(
        *ctx,
        Func {
            name: Symbol::from_str(ctx, "prim-call-entry").into(),
            source: Value::new(false),
            binding: f,
            return_cont: retk,
            args: Array::from_slice(*ctx, args),
            variadic: None,
            body: Lock::new(body),
            free_vars: Lock::new(None),
            meta: Value::new(false),
        },
    )
}

#[test]
fn linearize_raise_term_to_raise_terminator() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let who = Symbol::from_str(ctx, "car").into();
        let irritant = lvar(ctx, "x");
        let body = Gc::new(
            *ctx,
            Term::Raise {
                kind: RaiseKind::AssertionViolation,
                args: Array::from_slice(
                    *ctx,
                    &[
                        Atom::Constant(who),
                        Atom::Constant(Value::new(false)),
                        Atom::Local(irritant),
                    ],
                ),
                source: Value::new(false),
            },
        );
        let func = Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "raise-entry").into(),
                source: Value::new(false),
                binding: f,
                return_cont: retk,
                args: Array::from_slice(*ctx, &[irritant]),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        );

        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let procedure = linear
            .procedures
            .iter()
            .find(|procedure| procedure.code == CodeId::Function(func))
            .expect("entry function should have a linear procedure");

        let Terminator::Raise { kind, args, .. } = &procedure.blocks[0].terminator else {
            panic!("raise term should linearize to raise terminator");
        };

        assert_eq!(*kind, RaiseKind::AssertionViolation);
        assert_eq!(args.len(), 3);
        assert_eq!(procedure.blocks[0].terminator.successors(), Vec::new());
        assert_eq!(procedure.blocks[0].terminator.uses(), args.clone());
    });
}

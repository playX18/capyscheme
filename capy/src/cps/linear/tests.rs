use super::{
    Block, BlockId, BranchTarget, CodeId, Instruction, LinearAtom, Procedure, ProcedureKind,
    SwitchCaseValue, SwitchKind, Terminator, ValueId, infer_switches, linearize,
};
use crate::{
    compiler::ssa::primitive::Primitive,
    cps::{
        reify::reify,
        term::{Atom, BranchHint, Expression, Func, Term},
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
                    Array::from_slice(*ctx, [Atom::Local(result)]),
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
                    [
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
                args: Array::from_slice(*ctx, [irritant]),
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

#[test]
fn infer_switches_from_split_compare_branch_blocks() {
    with_ctx(|ctx| {
        let func = prim_call_func(ctx, "eq?", &[]);
        let source = Value::new(false);
        let scrutinee = ValueId(9);
        let local = |block| BranchTarget::Local {
            block,
            args: vec![],
        };
        let terminal = |id| Block {
            id,
            params: vec![],
            variadic: None,
            instructions: vec![],
            terminator: Terminator::Raise {
                kind: RaiseKind::AssertionViolation,
                args: vec![],
                source,
            },
            source,
        };

        let procedure = Procedure {
            code: CodeId::Function(func),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: source,
            source,
            meta: source,
            return_cont: Some(ValueId(1)),
            params: vec![scrutinee],
            variadic: None,
            free_vars: vec![],
            sources: std::collections::HashMap::new(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    params: vec![scrutinee],
                    variadic: None,
                    instructions: vec![
                        Instruction::Const {
                            dst: ValueId(20),
                            value: Value::new(0),
                        },
                        Instruction::PrimCall {
                            dst: ValueId(21),
                            prim: Primitive::is_eq,
                            args: vec![
                                LinearAtom::Local(scrutinee),
                                LinearAtom::Local(ValueId(20)),
                            ],
                            source,
                        },
                    ],
                    terminator: Terminator::Jump {
                        target: BlockId(1),
                        args: vec![LinearAtom::Local(ValueId(21))],
                    },
                    source,
                },
                Block {
                    id: BlockId(1),
                    params: vec![ValueId(22), ValueId(23)],
                    variadic: Some(ValueId(23)),
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        test: LinearAtom::Local(ValueId(22)),
                        consequent: local(BlockId(2)),
                        alternative: local(BlockId(3)),
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                terminal(BlockId(2)),
                Block {
                    id: BlockId(3),
                    params: vec![],
                    variadic: None,
                    instructions: vec![
                        Instruction::Const {
                            dst: ValueId(30),
                            value: Value::new(1),
                        },
                        Instruction::PrimCall {
                            dst: ValueId(31),
                            prim: Primitive::is_eq,
                            args: vec![
                                LinearAtom::Local(scrutinee),
                                LinearAtom::Local(ValueId(30)),
                            ],
                            source,
                        },
                    ],
                    terminator: Terminator::Jump {
                        target: BlockId(4),
                        args: vec![LinearAtom::Local(ValueId(31))],
                    },
                    source,
                },
                Block {
                    id: BlockId(4),
                    params: vec![ValueId(32), ValueId(33)],
                    variadic: Some(ValueId(33)),
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        test: LinearAtom::Local(ValueId(32)),
                        consequent: local(BlockId(5)),
                        alternative: local(BlockId(6)),
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source,
                },
                terminal(BlockId(5)),
                terminal(BlockId(6)),
            ],
        };

        let procedure = infer_switches(procedure);
        let block_ids = procedure
            .blocks
            .iter()
            .map(|block| block.id)
            .collect::<Vec<_>>();
        assert_eq!(
            block_ids,
            vec![BlockId(0), BlockId(2), BlockId(5), BlockId(6)]
        );

        let entry = procedure
            .blocks
            .iter()
            .find(|block| block.id == BlockId(0))
            .expect("entry block should survive");
        assert!(entry.instructions.is_empty());

        let Terminator::Switch {
            kind,
            scrutinee: switch_scrutinee,
            cases,
            default,
        } = &entry.terminator
        else {
            panic!("split compare/branch chain should infer a switch");
        };

        assert_eq!(*kind, SwitchKind::Eq);
        assert_eq!(*switch_scrutinee, LinearAtom::Local(scrutinee));
        assert_eq!(cases.len(), 2);
        assert_eq!(cases[0].value, SwitchCaseValue::Integer(0));
        assert_eq!(cases[0].target, local(BlockId(2)));
        assert_eq!(cases[1].value, SwitchCaseValue::Integer(1));
        assert_eq!(cases[1].target, local(BlockId(5)));
        assert_eq!(*default, local(BlockId(6)));
    });
}

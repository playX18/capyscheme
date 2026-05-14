
use crate::{
    compiler::ssa::primitive::Primitive,
    cps::{
        linear::{
            Block, BlockId, BranchTarget, CodeId, Instruction, LinearAtom, LinearProgram,
            Procedure, ProcedureKind, Terminator, ValueId, linearize,
        },
        linear_pretty::render_program,
        reify::reify,
        term::{Atom, BranchHint, Expression, Func, Term},
    },
    expander::core::{LVarRef, fresh_lvar},
    rsgc::{Gc, alloc::Array, cell::Lock},
    runtime::{
        Context, Scheme,
        value::{Symbol, Value},
        vm::exceptions::RaiseKind,
    },
};

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

#[test]
fn low_level_lowering_expands_plus_into_fixnum_fastpath() {
    with_ctx(|ctx| {
        let lhs = lvar(ctx, "lhs");
        let rhs = lvar(ctx, "rhs");
        let func = prim_call_func(ctx, "+", &[lhs, rhs]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("(low-call")
                && rendered.contains("is-fixnum")
                && rendered.contains("iadd-overflow.i32")
                && rendered.contains("(runtime-call"),
            "expected + to lower to explicit fixnum fastpath and runtime slowpath:\n{rendered}"
        );
        assert!(
            !rendered.contains("(prim-call"),
            "low-level lowering should remove high-level primitive calls from this program:\n{rendered}"
        );
    });
}

#[test]
fn low_level_lowering_reuses_checked_add_result_without_second_iadd() {
    with_ctx(|ctx| {
        let lhs = lvar(ctx, "lhs");
        let rhs = lvar(ctx, "rhs");
        let func = prim_call_func(ctx, "+", &[lhs, rhs]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);

        let mut checked_adds = 0;
        let mut plain_i32_adds = 0;
        for procedure in &linear.procedures {
            for block in &procedure.blocks {
                for instruction in &block.instructions {
                    match instruction {
                        Instruction::LowPrimCall {
                            dsts,
                            op: super::LowPrim::IAddOverflow(super::LowType::I32),
                            ..
                        } => {
                            checked_adds += 1;
                            assert_eq!(
                                dsts.len(),
                                2,
                                "checked i32 add should define raw result and overflow flag"
                            );
                        }
                        Instruction::LowPrimCall {
                            op: super::LowPrim::IAdd(super::LowType::I32),
                            ..
                        } => plain_i32_adds += 1,
                        _ => {}
                    }
                }
            }
        }

        assert_eq!(checked_adds, 1, "expected one checked i32 add");
        assert_eq!(
            plain_i32_adds,
            0,
            "checked fixnum addition should not also emit a second plain iadd:\n{}",
            render_program(&linear)
        );
    });
}

#[test]
fn low_level_lowering_keeps_split_merge_before_existing_successors() {
    with_ctx(|ctx| {
        let func = prim_call_func(ctx, "unused", &[]);
        let retk = ValueId(1);
        let lhs = ValueId(2);
        let variable = ValueId(3);
        let sum = ValueId(7);
        let predicate = ValueId(8);
        let stored = ValueId(9);
        let procedure = Procedure {
            code: CodeId::Function(func),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
            name: Value::new(false),
            source: Value::new(false),
            meta: Value::new(false),
            return_cont: Some(retk),
            params: vec![lhs, variable],
            variadic: None,
            free_vars: vec![],
            sources: Default::default(),
            entry: BlockId(0),
            blocks: vec![
                Block {
                    id: BlockId(0),
                    params: vec![lhs, variable],
                    variadic: None,
                    instructions: vec![
                        Instruction::PrimCall {
                            dst: sum,
                            prim: Primitive::plus,
                            args: vec![LinearAtom::Local(lhs), LinearAtom::Constant(Value::new(1))],
                            source: Value::new(false),
                        },
                        Instruction::PrimCall {
                            dst: predicate,
                            prim: Primitive::is_variable,
                            args: vec![LinearAtom::Local(variable)],
                            source: Value::new(false),
                        },
                    ],
                    terminator: Terminator::Branch {
                        test: LinearAtom::Local(predicate),
                        consequent: BranchTarget::Local {
                            block: BlockId(1),
                            args: vec![],
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(2),
                            args: vec![],
                        },
                        hints: [BranchHint::Normal, BranchHint::Normal],
                    },
                    source: Value::new(false),
                },
                Block {
                    id: BlockId(1),
                    params: vec![],
                    variadic: None,
                    instructions: vec![Instruction::PrimCall {
                        dst: stored,
                        prim: Primitive::variable_set,
                        args: vec![LinearAtom::Local(variable), LinearAtom::Local(sum)],
                        source: Value::new(false),
                    }],
                    terminator: Terminator::TailCall {
                        callee: LinearAtom::Local(retk),
                        args: vec![LinearAtom::Local(stored)],
                        source: Value::new(false),
                    },
                    source: Value::new(false),
                },
                Block {
                    id: BlockId(2),
                    params: vec![],
                    variadic: None,
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: LinearAtom::Local(retk),
                        args: vec![LinearAtom::Local(lhs)],
                        source: Value::new(false),
                    },
                    source: Value::new(false),
                },
            ],
        };

        let lowered = super::lower_low_level_primitives(procedure);
        let merge_position = lowered
            .blocks
            .iter()
            .position(|block| {
                block.params == vec![sum]
                    && matches!(
                        block.instructions.first(),
                        Some(Instruction::PrimCall {
                            prim: Primitive::is_variable,
                            ..
                        })
                    )
            })
            .expect("plus merge block should contain the original suffix");
        let successor_position = lowered
            .blocks
            .iter()
            .position(|block| block.id == BlockId(1))
            .expect("original successor should still exist");

        assert!(
            merge_position < successor_position,
            "the merge block defining the split result must be lowered before existing successors that use it:\n{}",
            render_program(&LinearProgram {
                entry: func,
                procedures: vec![lowered],
            })
        );
    });
}

#[test]
fn low_level_lowering_reuses_integer_op_family_for_s8_add() {
    with_ctx(|ctx| {
        let lhs = lvar(ctx, "lhs");
        let rhs = lvar(ctx, "rhs");
        let func = prim_call_func(ctx, "s8+", &[lhs, rhs]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("ireduce.i8") && rendered.contains("iadd.i8"),
            "expected s8+ to lower through shared ireduce/iadd low ops:\n{rendered}"
        );
        assert!(
            !rendered.contains("s8+"),
            "typed integer primitives should not survive as primitive-specific low ops:\n{rendered}"
        );
    });
}

#[test]
fn low_level_lowering_expands_vector_ref_into_type_and_bounds_fastpath() {
    with_ctx(|ctx| {
        let vector = lvar(ctx, "vector");
        let index = lvar(ctx, "index");
        let func = prim_call_func(ctx, "vector-ref", &[vector, index]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("has-type8.vector")
                && rendered.contains("is-fixnum")
                && rendered.contains("icmp.ult.i64")
                && rendered.contains("load.value")
                && rendered.contains("(runtime-call"),
            "expected vector-ref to lower to type/index/bounds fastpath and runtime slowpath:\n{rendered}"
        );
        assert!(
            !rendered.contains("(prim-call"),
            "low-level vector-ref lowering should remove high-level primitive calls:\n{rendered}"
        );
    });
}

#[test]
fn low_level_lowering_expands_string_length_into_type_checked_load() {
    with_ctx(|ctx| {
        let string = lvar(ctx, "string");
        let func = prim_call_func(ctx, "string-length", &[string]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("has-type8.string")
                && rendered.contains("load.i64")
                && rendered.contains("tag-fixnum")
                && rendered.contains("(runtime-call"),
            "expected string-length to lower to type-checked length load and runtime slowpath:\n{rendered}"
        );
        assert!(
            !rendered.contains("(prim-call"),
            "low-level string-length lowering should remove high-level primitive calls:\n{rendered}"
        );
    });
}

#[test]
fn low_level_lowering_expands_tuple_ref_into_type_and_bounds_fastpath() {
    with_ctx(|ctx| {
        let tuple = lvar(ctx, "tuple");
        let index = lvar(ctx, "index");
        let func = prim_call_func(ctx, "tuple-ref", &[tuple, index]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("has-type8.tuple")
                && rendered.contains("is-fixnum")
                && rendered.contains("icmp.ult.i64")
                && rendered.contains("load.value")
                && rendered.contains("(runtime-call"),
            "expected tuple-ref to lower to type/index/bounds fastpath and runtime slowpath:\n{rendered}"
        );
        assert!(
            !rendered.contains("(prim-call"),
            "low-level tuple-ref lowering should remove high-level primitive calls:\n{rendered}"
        );
    });
}

#[test]
fn low_level_lowering_expands_bytevector_predicate_to_type_test() {
    with_ctx(|ctx| {
        let value = lvar(ctx, "value");
        let func = prim_call_func(ctx, "bytevector?", &[value]);
        let reify_info = reify(ctx, func);
        let linear = linearize(&reify_info);
        let rendered = render_program(&linear);

        assert!(
            rendered.contains("has-type8.bytevector"),
            "expected bytevector? to lower to a low-level type test:\n{rendered}"
        );
        assert!(
            !rendered.contains("(prim-call"),
            "low-level bytevector? lowering should remove high-level primitive calls:\n{rendered}"
        );
    });
}

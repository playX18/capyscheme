use super::*;
use crate::{
    cps::term::{Atom, Cont, Expression, Func, Term},
    expander::core::{LVarRef, fresh_lvar},
    rsgc::{Gc, alloc::Array, cell::Lock},
    runtime::{
        Context, Scheme,
        value::{Str, Symbol, Value},
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

fn atoms<'gc>(ctx: Context<'gc>, atoms: &[Atom<'gc>]) -> crate::cps::term::Atoms<'gc> {
    Array::from_slice(*ctx, atoms)
}

fn vars<'gc>(ctx: Context<'gc>, vars: &[LVarRef<'gc>]) -> crate::cps::term::Vars<'gc> {
    Array::from_slice(*ctx, vars)
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

fn term_if<'gc>(
    ctx: Context<'gc>,
    test: Atom<'gc>,
    consequent: LVarRef<'gc>,
    consequent_args: &[Atom<'gc>],
    alternative: LVarRef<'gc>,
    alternative_args: &[Atom<'gc>],
) -> Gc<'gc, Term<'gc>> {
    Gc::new(
        *ctx,
        Term::If {
            test,
            consequent,
            consequent_args: (!consequent_args.is_empty()).then(|| atoms(ctx, consequent_args)),
            alternative,
            alternative_args: (!alternative_args.is_empty()).then(|| atoms(ctx, alternative_args)),
            hints: [
                crate::cps::term::BranchHint::Normal,
                crate::cps::term::BranchHint::Normal,
            ],
        },
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

fn entry_block<'a, 'gc>(proc: &'a Proc<'gc>) -> &'a Block<'gc> {
    proc.blocks
        .iter()
        .find(|block| block.id == proc.entry)
        .expect("entry block")
}

fn first_cont_proc<'a, 'gc>(module: &'a Module<'gc>) -> &'a Proc<'gc> {
    module
        .procs
        .iter()
        .find(|proc| proc.kind == ProcKind::Continuation)
        .expect("continuation proc")
}

fn name_string<'gc>(module: &Module<'gc>, name: Option<ConstRef>) -> Option<String> {
    name.map(|name| module.decode_const(name).unwrap().to_string())
}

fn block_named<'a, 'gc>(
    module: &'a Module<'gc>,
    proc: &'a Proc<'gc>,
    name: &str,
) -> &'a Block<'gc> {
    proc.blocks
        .iter()
        .find(|block| name_string(module, block.name).as_deref() == Some(name))
        .expect("named block")
}

fn const_values<'gc>(module: &Module<'gc>) -> Vec<Value<'gc>> {
    module
        .const_pool
        .entries
        .iter()
        .map(|value| value.get())
        .collect()
}

fn clone_module<'gc>(ctx: Context<'gc>, module: Gc<'gc, Module<'gc>>) -> Gc<'gc, Module<'gc>> {
    let mut bytes = Vec::new();
    module.write_to_writer(ctx, &mut bytes).unwrap();
    Module::read_from_reader(ctx, bytes.as_slice()).unwrap()
}

#[test]
fn function_and_continuation_entry_params_are_bound_correctly() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_app(ctx, Atom::Local(f), k, &[42.into()]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let func_proc = module
            .procs
            .iter()
            .find(|proc| proc.kind == ProcKind::Function)
            .unwrap();
        let cont_proc = first_cont_proc(&module);

        let func_entry = entry_block(func_proc);
        assert_eq!(
            func_entry.params.first().unwrap().value,
            func_proc.self_param
        );
        assert!(func_proc.retk_param.is_some());
        assert!(
            func_entry
                .params
                .iter()
                .any(|param| Some(param.value) == func_proc.retk_param)
        );

        let cont_entry = entry_block(cont_proc);
        assert_eq!(
            cont_entry.params.first().unwrap().value,
            cont_proc.self_param
        );
        assert!(cont_proc.retk_param.is_none());
    });
}

#[test]
fn non_reified_letk_lowers_to_blocks_and_jumps() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(ctx, &[k_cont], term_continue(ctx, k, &[7.into()]));
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        assert_eq!(module.procs.len(), 1);
        let proc = &module.procs[0];
        assert_eq!(proc.kind, ProcKind::Function);
        assert_eq!(proc.blocks.len(), 2);

        let entry = entry_block(proc);
        match &entry.term {
            Terminator::Jump { block, args } => {
                assert_eq!(args.len(), 1);
                let target = proc.blocks.iter().find(|bb| bb.id == *block).unwrap();
                assert_eq!(target.params.len(), 1);
            }
            other => panic!("expected jump, got {other:?}"),
        }
    });
}

#[test]
fn reified_letk_lowers_to_closure_ops_and_direct_tail_continue() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let tmp = lvar(ctx, "tmp");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k), 0.into()],
                term_continue(ctx, k, &[1.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        assert_eq!(module.procs.len(), 2);
        let func_proc = module
            .procs
            .iter()
            .find(|proc| proc.kind == ProcKind::Function)
            .unwrap();
        let cont_proc = first_cont_proc(&module);
        let entry = entry_block(func_proc);

        assert!(
            entry
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, InstKind::MakeClosure { is_cont: true, .. }))
        );
        assert!(
            entry
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, InstKind::InitClosure { .. }))
        );
        assert_eq!(cont_proc.captures.len(), 1);
        assert!(matches!(
            &entry.term,
            Terminator::TailContinue {
                callee: TailTarget::Direct { .. },
                ..
            }
        ));
    });
}

#[test]
fn known_and_unknown_apps_lower_to_direct_and_indirect_tail_apps() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let g = lvar(ctx, "g");
        let g_retk = lvar(ctx, "g_retk");
        let y = lvar(ctx, "y");
        let g_body = term_continue(ctx, g_retk, &[Atom::Local(y)]);
        let g_func = mk_func(ctx, "g", g, g_retk, &[y], None, g_body);
        let direct_func = mk_func(
            ctx,
            "f",
            f,
            retk,
            &[],
            None,
            term_fix(
                ctx,
                &[g_func],
                term_app(ctx, Atom::Local(g), retk, &[11.into()]),
            ),
        );
        let direct_module = lower_cps(ctx, direct_func).unwrap();
        let direct_entry = entry_block(&direct_module.procs[0]);
        assert!(matches!(
            &direct_entry.term,
            Terminator::TailApp {
                callee: TailTarget::Direct { .. },
                ..
            }
        ));

        let f2 = lvar(ctx, "f2");
        let retk2 = lvar(ctx, "retk2");
        let callee = lvar(ctx, "callee");
        let indirect_func = mk_func(
            ctx,
            "f2",
            f2,
            retk2,
            &[callee],
            None,
            term_app(ctx, Atom::Local(callee), retk2, &[9.into()]),
        );
        let indirect_module = lower_cps(ctx, indirect_func).unwrap();
        let indirect_entry = entry_block(&indirect_module.procs[0]);
        assert!(matches!(
            &indirect_entry.term,
            Terminator::TailApp {
                callee: TailTarget::Indirect { .. },
                ..
            }
        ));
    });
}

#[test]
fn if_uses_adapter_blocks_for_reified_targets() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k1 = lvar(ctx, "k1");
        let k2 = lvar(ctx, "k2");
        let x = lvar(ctx, "x");
        let y = lvar(ctx, "y");
        let tmp = lvar(ctx, "tmp");
        let k1_body = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k2_body = term_continue(ctx, retk, &[Atom::Local(y)]);
        let cont1 = mk_cont(ctx, "k1", k1, &[x], None, k1_body);
        let cont2 = mk_cont(ctx, "k2", k2, &[y], None, k2_body);
        let body = term_letk(
            ctx,
            &[cont1, cont2],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k1), 0.into()],
                term_if(ctx, 1.into(), k1, &[10.into()], k2, &[20.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let func_proc = module
            .procs
            .iter()
            .find(|proc| proc.kind == ProcKind::Function)
            .unwrap();
        let entry = entry_block(func_proc);
        let Terminator::Branch {
            then_block,
            else_block,
            ..
        } = &entry.term
        else {
            panic!("expected branch");
        };

        let then_target = func_proc
            .blocks
            .iter()
            .find(|bb| bb.id == *then_block)
            .unwrap();
        let else_target = func_proc
            .blocks
            .iter()
            .find(|bb| bb.id == *else_block)
            .unwrap();
        assert!(matches!(&then_target.term, Terminator::TailContinue { .. }));
        assert_eq!(
            name_string(&module, else_target.name).as_deref(),
            Some("k_k2")
        );
    });
}

#[test]
fn atoms_are_hoisted_before_branches_and_jumps() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k2 = lvar(ctx, "k2");
        let y = lvar(ctx, "y");
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let k2_body = term_continue(ctx, retk, &[Atom::Local(y)]);
        let k2_cont = mk_cont(ctx, "k2", k2, &[y], None, k2_body);
        let body = term_letk(
            ctx,
            &[k_cont, k2_cont],
            term_if(ctx, 42.into(), k, &[1.into()], k2, &[2.into()]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let proc = &module.procs[0];
        let entry = entry_block(proc);
        assert!(
            entry
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, InstKind::Const(_)))
        );
        let Terminator::Branch { cond, .. } = &entry.term else {
            panic!("expected branch");
        };
        assert!(
            entry
                .insts
                .iter()
                .any(|inst| inst.result == Some(*cond) && matches!(inst.kind, InstKind::Const(_)))
        );
    });
}

#[test]
fn proc_entry_loads_captures_with_closure_ref() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let tmp = lvar(ctx, "tmp");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k), 0.into()],
                term_continue(ctx, k, &[1.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let cont_proc = first_cont_proc(&module);
        let entry = entry_block(cont_proc);
        assert!(matches!(
            entry.insts.first().map(|inst| &inst.kind),
            Some(InstKind::ClosureRef { closure, index })
                if *closure == cont_proc.self_param && *index == 0
        ));
    });
}

#[test]
fn serialization_round_trips_and_decodes_consts() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(ctx, &[k_cont], term_continue(ctx, k, &[7.into()]));
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let mut bytes = Vec::new();
        module.write_to_writer(ctx, &mut bytes).unwrap();
        let round_trip = Module::read_from_reader(ctx, bytes.as_slice()).unwrap();
        round_trip.validate().unwrap();

        let proc = &round_trip.procs[0];
        let entry = entry_block(proc);
        let const_ref = entry
            .insts
            .iter()
            .find_map(|inst| match inst.kind {
                InstKind::Const(id) => Some(id),
                _ => None,
            })
            .unwrap();
        assert_eq!(round_trip.decode_const(const_ref).unwrap(), Value::new(7));
        assert!(
            !round_trip
                .const_pool
                .entries
                .iter()
                .any(|value| value.get() == Value::new(7))
        );
    });
}

#[test]
fn immediate_consts_stay_inline_and_out_of_heap_const_pool() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let a = lvar(ctx, "a");
        let b = lvar(ctx, "b");
        let c = lvar(ctx, "c");
        let d = lvar(ctx, "d");
        let e = lvar(ctx, "e");
        let g = lvar(ctx, "g");
        let h = lvar(ctx, "h");
        let i = lvar(ctx, "i");
        let j = lvar(ctx, "j");

        let body_k = term_continue(ctx, retk, &[Atom::Local(a)]);
        let k_cont = mk_cont(ctx, "k", k, &[a, b, c, d, e, g, h, i, j], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_continue(
                ctx,
                k,
                &[
                    Atom::Constant(Value::null()),
                    Atom::Constant(Value::new(true)),
                    Atom::Constant(Value::new(false)),
                    Atom::Constant(Value::new(7i32)),
                    Atom::Constant(Value::new(1.5f64)),
                    Atom::Constant(Value::new('x')),
                    Atom::Constant(Value::undefined()),
                    Atom::Constant(Value::unspecified()),
                    Atom::Constant(Value::eof()),
                ],
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let entry = entry_block(&module.procs[0]);
        let inline_consts = entry
            .insts
            .iter()
            .filter_map(|inst| match inst.kind {
                InstKind::Const(ConstRef::Inline(value)) => Some(value),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(inline_consts.contains(&ImmediateConst::Null));
        assert!(inline_consts.contains(&ImmediateConst::Bool(true)));
        assert!(inline_consts.contains(&ImmediateConst::Bool(false)));
        assert!(inline_consts.contains(&ImmediateConst::Int32(7)));
        assert!(inline_consts.contains(&ImmediateConst::Flonum(1.5)));
        assert!(inline_consts.contains(&ImmediateConst::Char('x')));
        assert!(inline_consts.contains(&ImmediateConst::Undefined));
        assert!(inline_consts.contains(&ImmediateConst::Unspecified));
        assert!(inline_consts.contains(&ImmediateConst::Eof));

        let pooled = const_values(&module);
        assert!(!pooled.contains(&Value::null()));
        assert!(!pooled.contains(&Value::new(true)));
        assert!(!pooled.contains(&Value::new(false)));
        assert!(!pooled.contains(&Value::new(7i32)));
        assert!(!pooled.contains(&Value::new(1.5f64)));
        assert!(!pooled.contains(&Value::new('x')));
        assert!(!pooled.contains(&Value::undefined()));
        assert!(!pooled.contains(&Value::unspecified()));
        assert!(!pooled.contains(&Value::eof()));
    });
}

#[test]
fn heap_consts_round_trip_through_scheme_vector_pool() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let heap_value = Value::new(Str::new(*ctx, "heap", true));

        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_continue(ctx, k, &[Atom::Constant(heap_value)]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let const_ref = entry_block(&module.procs[0])
            .insts
            .iter()
            .find_map(|inst| match inst.kind {
                InstKind::Const(ConstRef::Pool(id)) => Some(ConstRef::Pool(id)),
                _ => None,
            })
            .unwrap();
        assert_eq!(module.decode_const(const_ref).unwrap().to_string(), "heap");

        let mut bytes = Vec::new();
        module.write_to_writer(ctx, &mut bytes).unwrap();
        let round_trip = Module::read_from_reader(ctx, bytes.as_slice()).unwrap();
        assert_eq!(
            round_trip.decode_const(const_ref).unwrap().to_string(),
            "heap"
        );
        assert!(
            const_values(&round_trip)
                .iter()
                .any(|value| value.to_string() == "heap")
        );
    });
}

#[test]
fn variadic_rest_uses_inline_null_not_pooled_null() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let rest = lvar(ctx, "rest");
        let head = lvar(ctx, "head");

        let k_body = term_let_prim(
            ctx,
            head,
            "car",
            &[Atom::Local(rest)],
            term_continue(ctx, retk, &[Atom::Local(head)]),
        );
        let k_cont = mk_cont(ctx, "k", k, &[x], Some(rest), k_body);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_continue(ctx, k, &[1.into(), 2.into(), 3.into()]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let entry = entry_block(&module.procs[0]);
        assert!(entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::Const(ConstRef::Inline(ImmediateConst::Null))
            )
        }));
        assert!(!const_values(&module).contains(&Value::null()));
    });
}

#[test]
fn validator_rejects_invalid_retk_and_bad_jump_arity() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let tmp = lvar(ctx, "tmp");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k), 0.into()],
                term_continue(ctx, k, &[1.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);
        let module = lower_cps(ctx, func).unwrap();

        let bad_retk = clone_module(ctx, module);
        let mut bad_retk_procs = bad_retk.procs.iter().cloned().collect::<Vec<_>>();
        let cont_proc = bad_retk_procs
            .iter_mut()
            .find(|proc| proc.kind == ProcKind::Continuation)
            .unwrap();
        cont_proc.retk_param = Some(cont_proc.self_param);
        let bad_retk = Gc::new(
            *ctx,
            Module {
                format_version: bad_retk.format_version,
                entry: bad_retk.entry,
                procs: Array::from_slice(*ctx, &bad_retk_procs),
                const_pool: bad_retk.const_pool.clone(),
            },
        );
        assert!(bad_retk.validate().is_err());

        let bad_jump = {
            let f = lvar(ctx, "f3");
            let retk = lvar(ctx, "retk3");
            let k = lvar(ctx, "k3");
            let x = lvar(ctx, "x3");
            let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
            let k_cont = mk_cont(ctx, "k3", k, &[x], None, body_k);
            let body = term_letk(ctx, &[k_cont], term_continue(ctx, k, &[7.into()]));
            let func = mk_func(ctx, "f3", f, retk, &[], None, body);
            lower_cps(ctx, func).unwrap()
        };
        let mut procs = bad_jump.procs.iter().cloned().collect::<Vec<_>>();
        let proc = &mut procs[0];
        let entry_id = proc.entry;
        let mut blocks = proc.blocks.iter().cloned().collect::<Vec<_>>();
        let entry = blocks
            .iter_mut()
            .find(|block| block.id == entry_id)
            .unwrap();
        if let Terminator::Jump { args, .. } = &mut entry.term {
            *args = Array::from_slice(*ctx, &[] as &[ValueId]);
        } else {
            panic!("expected jump");
        }
        proc.blocks = Array::from_slice(*ctx, &blocks);
        let bad_jump = Gc::new(
            *ctx,
            Module {
                format_version: bad_jump.format_version,
                entry: bad_jump.entry,
                procs: Array::from_slice(*ctx, &procs),
                const_pool: bad_jump.const_pool.clone(),
            },
        );
        assert!(bad_jump.validate().is_err());
    });
}

#[test]
fn local_variadic_continuations_build_plain_lists_for_rest() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let rest = lvar(ctx, "rest");
        let head = lvar(ctx, "head");

        let k_body = term_let_prim(
            ctx,
            head,
            "car",
            &[Atom::Local(rest)],
            term_continue(ctx, retk, &[Atom::Local(head)]),
        );
        let k_cont = mk_cont(ctx, "k", k, &[x], Some(rest), k_body);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_continue(ctx, k, &[1.into(), 2.into(), 3.into()]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let proc = &module.procs[0];
        let entry = entry_block(proc);
        assert!(
            entry
                .insts
                .iter()
                .filter(|inst| {
                    matches!(
                        inst.kind,
                        InstKind::PrimCall {
                            prim: Primitive::Cons,
                            ..
                        }
                    )
                })
                .count()
                == 2
        );
        assert!(
            entry
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, InstKind::Const(_)))
        );
        assert!(!entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::PackRest { .. }
                    | InstKind::RestRef { .. }
                    | InstKind::RestLength { .. }
                    | InstKind::RestToList { .. }
                    | InstKind::RestIsEmpty { .. }
                    | InstKind::RestIsNotEmpty { .. }
            )
        }));

        let k_block = block_named(&module, proc, "k_k");
        assert!(k_block.insts.iter().any(|inst| matches!(
            inst.kind,
            InstKind::PrimCall {
                prim: Primitive::Car,
                ..
            }
        )));
    });
}

#[test]
fn function_rest_length_uses_plain_primitive_call() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let rest = lvar(ctx, "rest");
        let len = lvar(ctx, "len");

        let body = term_let_prim(
            ctx,
            len,
            "length",
            &[Atom::Local(rest)],
            term_continue(ctx, retk, &[Atom::Local(len)]),
        );
        let func = mk_func(ctx, "f", f, retk, &[], Some(rest), body);

        let module = lower_cps(ctx, func).unwrap();
        let entry = entry_block(&module.procs[0]);
        assert!(entry.insts.iter().any(|inst| matches!(
            inst.kind,
            InstKind::PrimCall {
                prim: Primitive::Length,
                ..
            }
        )));
        assert!(!entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::PackRest { .. }
                    | InstKind::RestRef { .. }
                    | InstKind::RestLength { .. }
                    | InstKind::RestToList { .. }
                    | InstKind::RestIsEmpty { .. }
                    | InstKind::RestIsNotEmpty { .. }
            )
        }));
    });
}

#[test]
fn generic_rest_uses_plain_values_without_materialization() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let rest = lvar(ctx, "rest");
        let is_list = lvar(ctx, "is_list");
        let is_pair = lvar(ctx, "is_pair");

        let body = term_let_prim(
            ctx,
            is_list,
            "list?",
            &[Atom::Local(rest)],
            term_let_prim(
                ctx,
                is_pair,
                "pair?",
                &[Atom::Local(rest)],
                term_continue(ctx, retk, &[Atom::Local(is_pair)]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], Some(rest), body);

        let module = lower_cps(ctx, func).unwrap();
        let entry = entry_block(&module.procs[0]);
        assert_eq!(
            entry
                .insts
                .iter()
                .filter(|inst| {
                    matches!(
                        inst.kind,
                        InstKind::PackRest { .. }
                            | InstKind::RestRef { .. }
                            | InstKind::RestLength { .. }
                            | InstKind::RestToList { .. }
                            | InstKind::RestIsEmpty { .. }
                            | InstKind::RestIsNotEmpty { .. }
                    )
                })
                .count(),
            0
        );
        assert!(entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::PrimCall {
                    prim: Primitive::IsList,
                    ..
                }
            )
        }));
        assert!(entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::PrimCall {
                    prim: Primitive::IsPair,
                    ..
                }
            )
        }));
    });
}

#[test]
fn capturing_rest_in_reified_cont_keeps_plain_rest_value() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let rest = lvar(ctx, "rest");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let tmp = lvar(ctx, "tmp");

        let k_body = term_continue(ctx, retk, &[Atom::Local(rest)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, k_body);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k), 0.into()],
                term_continue(ctx, k, &[1.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], Some(rest), body);

        let module = lower_cps(ctx, func).unwrap();
        let func_proc = module
            .procs
            .iter()
            .find(|proc| proc.kind == ProcKind::Function)
            .unwrap();
        let entry = entry_block(func_proc);
        assert!(!entry.insts.iter().any(|inst| {
            matches!(
                inst.kind,
                InstKind::PackRest { .. }
                    | InstKind::RestRef { .. }
                    | InstKind::RestLength { .. }
                    | InstKind::RestToList { .. }
                    | InstKind::RestIsEmpty { .. }
                    | InstKind::RestIsNotEmpty { .. }
            )
        }));
        assert!(
            entry
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, InstKind::InitClosure { .. }))
        );
    });
}

#[test]
fn pretty_print_includes_module_proc_block_and_inst_structure() {
    with_ctx(|ctx| {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let k = lvar(ctx, "k");
        let x = lvar(ctx, "x");
        let tmp = lvar(ctx, "tmp");
        let body_k = term_continue(ctx, retk, &[Atom::Local(x)]);
        let k_cont = mk_cont(ctx, "k", k, &[x], None, body_k);
        let body = term_letk(
            ctx,
            &[k_cont],
            term_let_prim(
                ctx,
                tmp,
                "cons",
                &[Atom::Local(k), 0.into()],
                term_continue(ctx, k, &[1.into()]),
            ),
        );
        let func = mk_func(ctx, "f", f, retk, &[], None, body);

        let module = lower_cps(ctx, func).unwrap();
        let rendered = module.pretty_string();

        assert!(rendered.contains("module format=4 entry=p0"));
        assert!(rendered.contains("consts:"));
        assert!(rendered.contains("proc p0 function f entry=b0"));
        assert!(rendered.contains("display-name: c"));
        assert!(rendered.contains("params: self=v0, retk=v1, args=[]"));
        assert!(rendered.contains("b0 entry(self=v0, retk=v1):"));
        assert!(rendered.contains("const 0"));
        assert!(rendered.contains("make-closure continuation p1"));
        assert!(rendered.contains("init-closure"));
        assert!(rendered.contains("tail-continue target=direct p1 via"));
        assert!(rendered.contains("proc p1 continuation k entry=b0"));
        assert!(rendered.contains("captures: [0:retk]"));
        assert!(rendered.contains("closure-ref v0[0]"));
    });
}

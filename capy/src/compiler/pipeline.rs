use std::{collections::HashMap, fmt::Write, path::Path};

use crate::cps::contify::contify;
use crate::cps::linear::{BranchTarget, LinearAtom, LinearProgram, Procedure, Terminator, ValueId};
use crate::cps::term::FuncRef;
use crate::expander::core::TermRef;
use crate::expander::{
    assignment_elimination, compile_cps, eta_expand::eta_expand, fix_letrec::fix_letrec,
    free_vars::resolve_free_vars, letrectify::letrectify, primitives,
};
use crate::rsgc::Gc;
use crate::runtime::stats::{CompilationBreakdownPhase, CompilationBreakdownScope};
use crate::runtime::{Context, modules::Module, value::Value};
use crate::utils::pass_profile::ProfileScope;

#[derive(Clone, Copy)]
pub(crate) struct LoweredProgram<'gc> {
    pub(crate) original_il: TermRef<'gc>,
    pub(crate) optimized_il: TermRef<'gc>,
    pub(crate) cps: FuncRef<'gc>,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct DumpArtifactsOptions {
    pub(crate) enabled: bool,
    pub(crate) include_unoptimized: bool,
}

pub fn lower_to_cps<'gc>(
    ctx: Context<'gc>,
    il: TermRef<'gc>,
    module: Option<Gc<'gc, Module<'gc>>>,
    expand_primitives: bool,
) -> Result<FuncRef<'gc>, Value<'gc>> {
    lower_expanded_to_cps(ctx, il, module, expand_primitives).map(|lowered| lowered.cps)
}

pub(crate) fn lower_expanded_to_cps<'gc>(
    ctx: Context<'gc>,
    mut il: TermRef<'gc>,
    module: Option<Gc<'gc, Module<'gc>>>,
    expand_primitives: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    let original_il = il;
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Lowering);

    if expand_primitives && let Some(module) = module {
        il = primitives::resolve_primitives(ctx, il, module);
        il = primitives::expand_primitives(ctx, il);
        let _profile = ProfileScope::new("compiler.lower.resolve_free_vars");
        il = resolve_free_vars(ctx, il);
        drop(_profile);
        let _profile = ProfileScope::new("compiler.lower.letrectify");
        il = letrectify(ctx, il);
        drop(_profile);
    }

    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.fix_letrec");
        fix_letrec(ctx, il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.eta_expand");
        eta_expand(ctx, optimized_il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.assignment_elimination");
        assignment_elimination::eliminate_assignments(ctx, optimized_il)
    };

    let mut cps = {
        let _profile = ProfileScope::new("compiler.lower.compile_cps_toplevel");
        compile_cps::cps_toplevel(ctx, &[optimized_il])
    };
    cps = {
        let _profile = ProfileScope::new("compiler.lower.cps.rewrite");
        crate::cps::rewrite_func(ctx, cps)
    };
    cps = {
        let _profile = ProfileScope::new("compiler.lower.cps.contify");
        cps.with_body(ctx, contify(ctx, cps.body()))
    };
    Ok(LoweredProgram {
        original_il,
        optimized_il,
        cps,
    })
}

pub(crate) fn dump_lowered_program_artifacts<'gc>(
    ctx: Context<'gc>,
    destination: impl AsRef<Path>,
    lowered: &LoweredProgram<'gc>,
    options: DumpArtifactsOptions,
) {
    if !options.enabled {
        return;
    }

    let destination = destination.as_ref().display().to_string();

    if options.include_unoptimized {
        let doc = lowered
            .original_il
            .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file_noopt = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(format!("{destination}.ir.noopt.scm"))
            .unwrap();
        println!(";; TRACE  (capy)@load: IR noopt -> {destination}.ir.noopt.scm");
        doc.1.render(80, &mut file_noopt).unwrap();
    }

    let doc = lowered
        .optimized_il
        .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(format!("{destination}.ir.scm"))
        .unwrap();
    println!(";; TRACE  (capy)@load: IR -> {destination}.ir.scm");
    doc.1.render(80, &mut file).unwrap();

    let doc = lowered
        .cps
        .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(format!("{destination}.cps.scm"))
        .unwrap();
    println!(";; TRACE  (capy)@load: CPS -> {destination}.cps.scm");
    doc.1.render(80, &mut file).unwrap();

    let linear_cps = {
        let _profile = ProfileScope::new("compiler.lower.cps.linearize.dump");
        let reify_info = crate::cps::reify(ctx, lowered.cps);
        crate::cps::linear::linearize(&reify_info)
    };
    let rendered = render_lcps_dump(&linear_cps);
    std::fs::write(format!("{destination}.lcps.scm"), rendered).unwrap();
    println!(";; TRACE  (capy)@load: LCPS -> {destination}.lcps.scm");
}

fn render_lcps_dump<'gc>(linear_cps: &crate::cps::linear::LinearProgram<'gc>) -> String {
    let mut rendered = crate::cps::linear_pretty::render_program(linear_cps);
    rendered.push('\n');
    render_slot_allocations(&mut rendered, linear_cps);

    rendered
}

fn render_slot_allocations<'gc>(out: &mut String, linear_cps: &LinearProgram<'gc>) {
    writeln!(out, "(slot-allocations").unwrap();
    for procedure in &linear_cps.procedures {
        render_procedure_slot_allocations(out, procedure, 2);
    }
    writeln!(out, ")").unwrap();
}

#[derive(Clone)]
struct SlotAllocation {
    value: ValueId,
    slot: usize,
    name: String,
}

fn render_procedure_slot_allocations<'gc>(
    out: &mut String,
    procedure: &Procedure<'gc>,
    indent: usize,
) {
    let pad = " ".repeat(indent);
    let allocations = collect_slot_allocations(procedure);
    let slots = allocations
        .iter()
        .map(|allocation| (allocation.value, allocation.slot))
        .collect::<HashMap<_, _>>();

    writeln!(out, "{pad}(procedure").unwrap();
    writeln!(out, "{pad}  (nlocals {})", allocations.len()).unwrap();
    writeln!(out, "{pad}  (values").unwrap();
    for allocation in &allocations {
        writeln!(
            out,
            "{pad}    (%v{} {} local{})",
            allocation.value.0, allocation.name, allocation.slot
        )
        .unwrap();
    }
    writeln!(out, "{pad}  )").unwrap();
    writeln!(out, "{pad}  (blocks").unwrap();
    for block in &procedure.blocks {
        writeln!(
            out,
            "{pad}    (block{} terminator {})",
            block.id.0,
            render_slot_terminator(&block.terminator, &slots)
        )
        .unwrap();
    }
    writeln!(out, "{pad}  )").unwrap();
    writeln!(out, "{pad}  (parallel-copy-tmps)").unwrap();
    writeln!(out, "{pad})").unwrap();
}

fn collect_slot_allocations<'gc>(procedure: &Procedure<'gc>) -> Vec<SlotAllocation> {
    let mut allocations = Vec::new();
    let mut seen = HashMap::new();
    let mut next_local = 0usize;

    for (index, value) in procedure.params.iter().copied().enumerate() {
        push_slot_allocation(&mut allocations, &mut seen, value, format!("arg{index}"));
    }

    if let Some(value) = procedure.variadic {
        push_slot_allocation(
            &mut allocations,
            &mut seen,
            value,
            format!("arg{}", procedure.params.len()),
        );
    }

    for (index, value) in procedure.free_vars.iter().copied().enumerate() {
        push_slot_allocation(&mut allocations, &mut seen, value, format!("free{index}"));
    }

    for block in &procedure.blocks {
        for value in block.params.iter().copied().chain(block.variadic) {
            push_local_slot_allocation(&mut allocations, &mut seen, &mut next_local, value);
        }
        for instruction in &block.instructions {
            for atom in instruction.uses() {
                push_atom_slot_allocation(&mut allocations, &mut seen, &mut next_local, atom);
            }
            for value in instruction.defs() {
                push_local_slot_allocation(&mut allocations, &mut seen, &mut next_local, value);
            }
        }
        for atom in block.terminator.uses() {
            push_atom_slot_allocation(&mut allocations, &mut seen, &mut next_local, atom);
        }
    }

    allocations
}

fn push_slot_allocation(
    allocations: &mut Vec<SlotAllocation>,
    seen: &mut HashMap<ValueId, usize>,
    value: ValueId,
    name: String,
) {
    if seen.contains_key(&value) {
        return;
    }

    let slot = allocations.len();
    seen.insert(value, slot);
    allocations.push(SlotAllocation { value, slot, name });
}

fn push_local_slot_allocation(
    allocations: &mut Vec<SlotAllocation>,
    seen: &mut HashMap<ValueId, usize>,
    next_local: &mut usize,
    value: ValueId,
) {
    if seen.contains_key(&value) {
        return;
    }

    let name = format!("local{}", *next_local);
    *next_local += 1;
    push_slot_allocation(allocations, seen, value, name);
}

fn push_atom_slot_allocation<'gc>(
    allocations: &mut Vec<SlotAllocation>,
    seen: &mut HashMap<ValueId, usize>,
    next_local: &mut usize,
    atom: LinearAtom<'gc>,
) {
    if let LinearAtom::Local(value) = atom {
        push_local_slot_allocation(allocations, seen, next_local, value);
    }
}

fn render_slot_terminator<'gc>(
    terminator: &Terminator<'gc>,
    slots: &HashMap<ValueId, usize>,
) -> String {
    match terminator {
        Terminator::Call {
            callee, retk, args, ..
        } => format!(
            "call {} {} {}",
            render_slot_atom(*callee, slots),
            render_slot_atom(*retk, slots),
            args.len()
        ),
        Terminator::TailCall { callee, args, .. } => {
            format!(
                "tail-call {} {}",
                render_slot_atom(*callee, slots),
                args.len()
            )
        }
        Terminator::Raise { kind, args, .. } => format!("raise {:?} {}", kind, args.len()),
        Terminator::Jump { target, args } => format!("jump block{} {}", target.0, args.len()),
        Terminator::Branch {
            test,
            consequent,
            alternative,
            ..
        } => format!(
            "branch {} {} {}",
            render_slot_atom(*test, slots),
            render_slot_branch_target(consequent, slots),
            render_slot_branch_target(alternative, slots)
        ),
        Terminator::Switch {
            scrutinee,
            cases,
            default,
            ..
        } => format!(
            "switch {} {} {}",
            render_slot_atom(*scrutinee, slots),
            cases.len(),
            render_slot_branch_target(default, slots)
        ),
    }
}

fn render_slot_branch_target<'gc>(
    target: &BranchTarget<'gc>,
    slots: &HashMap<ValueId, usize>,
) -> String {
    match target {
        BranchTarget::Local { block, args } => format!("block{}:{}", block.0, args.len()),
        BranchTarget::Reified { continuation, args } => {
            format!("{}:{}", render_slot_atom(*continuation, slots), args.len())
        }
    }
}

fn render_slot_atom<'gc>(atom: LinearAtom<'gc>, slots: &HashMap<ValueId, usize>) -> String {
    match atom {
        LinearAtom::Constant(_) => "constant".to_string(),
        LinearAtom::Local(value) => slots
            .get(&value)
            .map(|slot| format!("local{slot}"))
            .unwrap_or_else(|| format!("%v{}", value.0)),
    }
}

#[cfg(test)]
mod tests {
    use super::render_lcps_dump;
    use crate::{
        compiler::ssa::primitive::Primitive,
        cps::{
            linear::{
                Block, BlockId, CodeId, LinearAtom, LinearProgram, Procedure, ProcedureKind,
                Terminator, ValueId,
            },
            term::{Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn vars<'gc>(ctx: Context<'gc>, vars: &[LVarRef<'gc>]) -> crate::cps::term::Vars<'gc> {
        Array::from_slice(*ctx, vars)
    }

    fn atoms<'gc>(
        ctx: Context<'gc>,
        atoms: &[crate::cps::term::Atom<'gc>],
    ) -> crate::cps::term::Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    fn dummy_func<'gc>(ctx: Context<'gc>) -> Gc<'gc, Func<'gc>> {
        let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "lcps-dump-test").into());
        let retk = fresh_lvar(ctx, Symbol::from_str(ctx, "lcps-dump-retk").into());
        let body = Gc::new(
            *ctx,
            Term::Continue(retk, atoms(ctx, &[]), Value::new(false)),
        );

        Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "lcps-dump-test").into(),
                source: Value::new(false),
                binding,
                return_cont: retk,
                args: vars(ctx, &[]),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(Some(vars(ctx, &[]))),
                meta: Value::new(false),
            },
        )
    }

    #[test]
    fn lcps_dump_includes_slot_allocation() {
        with_ctx(|ctx| {
            let entry = dummy_func(ctx);
            let p0 = ValueId(0);
            let tmp = ValueId(1);
            let procedure = Procedure {
                code: CodeId::Function(entry),
                kind: ProcedureKind::Function,
                binding: ValueId(10_000),
                name: Value::new(false),
                source: Value::new(false),
                meta: Value::new(false),
                return_cont: None,
                params: vec![p0],
                variadic: None,
                free_vars: vec![],
                sources: Default::default(),
                entry: BlockId(0),
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![p0],
                    variadic: None,
                    instructions: vec![crate::cps::linear::Instruction::PrimCall {
                        dst: tmp,
                        prim: Primitive::car,
                        args: vec![LinearAtom::Local(p0)],
                        source: Value::new(false),
                    }],
                    terminator: Terminator::TailCall {
                        callee: LinearAtom::Local(tmp),
                        args: vec![LinearAtom::Local(p0)],
                        source: Value::new(false),
                    },
                    source: Value::new(false),
                }],
            };
            let linear = LinearProgram {
                entry,
                procedures: vec![procedure],
            };

            let rendered = render_lcps_dump(&linear);

            assert!(rendered.contains("(linear-program"));
            assert!(rendered.contains("(slot-allocations"));
            assert!(rendered.contains("(nlocals 2)"));
            assert!(rendered.contains("(%v0 arg0"));
            assert!(rendered.contains("(%v1 local0"));
            assert!(rendered.contains("(block0 terminator tail-call local1 1)"));
            assert!(rendered.contains("(parallel-copy-tmps"));
        });
    }
}

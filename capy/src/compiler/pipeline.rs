use std::path::Path;

use crate::cps::contify::contify;
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
    }

    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.resolve_free_vars");
        resolve_free_vars(ctx, il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.letrectify");
        letrectify(ctx, optimized_il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.fix_letrec");
        fix_letrec(ctx, optimized_il)
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
        crate::cps::linear::linearize(ctx, &reify_info)
    };
    let rendered = render_lcps_dump(&linear_cps);
    std::fs::write(format!("{destination}.lcps.scm"), rendered).unwrap();
    println!(";; TRACE  (capy)@load: LCPS -> {destination}.lcps.scm");
}

fn render_lcps_dump<'gc>(linear_cps: &crate::cps::linear::LinearProgram<'gc>) -> String {
    let mut rendered = crate::cps::linear_pretty::render_program(linear_cps);
    rendered.push('\n');
    rendered.push_str(
        &crate::compiler::bytecode::greedy_slotalloc::render_program_allocations(linear_cps),
    );
    rendered
}

#[cfg(test)]
mod tests {
    use super::render_lcps_dump;
    use crate::{
        cps::{
            linear::{
                Block, BlockId, CodeId, LinearAtom, LinearProgram, Procedure, ProcedureKind,
                Terminator, ValueId,
            },
            packed::Primitive,
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
            let _entry = dummy_func(ctx);
            let p0 = ValueId(0);
            let tmp = ValueId(1);
            let procedure = Procedure {
                code: CodeId(0),
                kind: ProcedureKind::Function,
                binding: ValueId(10_000),
                name: Value::new(false),
                source: Value::new(false),
                meta: Value::new(false),
                return_cont: None,
                params: Array::from_slice(*ctx, &[p0]),
                variadic: None,
                free_vars: Array::from_slice(*ctx, &[]),
                sources: Default::default(),
                entry: BlockId(0),
                blocks: Array::from_slice(
                    *ctx,
                    &[Block {
                        id: BlockId(0),
                        params: Array::from_slice(*ctx, &[p0]),
                        variadic: None,
                        instructions: Array::from_slice(
                            *ctx,
                            &[crate::cps::linear::Instruction::PrimCall {
                                dst: tmp,
                                prim: Primitive::Car,
                                args: Array::from_slice(*ctx, &[LinearAtom::Local(p0)]),
                                source: Value::new(false),
                            }],
                        ),
                        terminator: Terminator::TailCall {
                            callee: LinearAtom::Local(tmp),
                            args: Array::from_slice(*ctx, &[LinearAtom::Local(p0)]),
                            source: Value::new(false),
                        },
                        source: Value::new(false),
                    }],
                ),
            };
            let linear = LinearProgram {
                entry: CodeId(0),
                procedures: Array::from_slice(*ctx, &[procedure]),
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

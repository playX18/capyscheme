use std::path::Path;

use crate::compiler::ssa::LinearProgram;
use crate::expander::core::TermRef;
use crate::expander::{
    assignment_elimination, compile_cps, eta_expand::eta_expand, fix_letrec::fix_letrec,
    free_vars::resolve_free_vars, letrectify::letrectify, primitives,
};
use crate::compiler::cps::optimize::optimize_graph_func_to_linear;
use crate::rsgc::Gc;
use crate::runtime::stats::{CompilationBreakdownPhase, CompilationBreakdownScope};
use crate::runtime::{Context, modules::Module, value::Value};
use crate::utils::pass_profile::ProfileScope;

#[derive(Clone)]
pub struct LoweredProgram<'gc> {
    pub(crate) original_il: TermRef<'gc>,
    pub(crate) optimized_il: TermRef<'gc>,
    pub(crate) graph_cps: String,
    pub(crate) linear_cps: LinearProgram<'gc>,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct DumpArtifactsOptions {
    pub(crate) enabled: bool,
    pub(crate) include_unoptimized: bool,
    pub(crate) dump_ir: bool,
    pub(crate) dump_graph: bool,
    pub(crate) dump_lcps: bool,
    pub(crate) dump_cranelift: bool,
    pub(crate) dump_disassembly: bool,
}

impl DumpArtifactsOptions {
    pub(crate) fn trace() -> Self {
        Self {
            enabled: true,
            include_unoptimized: true,
            dump_ir: true,
            dump_lcps: true,
            ..Self::default()
        }
    }

    pub(crate) fn enable(&mut self, name: &str) -> bool {
        self.enabled = true;
        match name {
            "ir" => self.dump_ir = true,
            "graph" | "gcps" => self.dump_graph = true,
            "lcps" => self.dump_lcps = true,
            "cranelift" | "clif" => self.dump_cranelift = true,
            "disassembly" | "asm" => self.dump_disassembly = true,
            _ => return false,
        }
        true
    }

    pub(crate) fn has_frontend_artifacts(self) -> bool {
        self.enabled && (self.dump_ir || self.dump_graph || self.dump_lcps)
    }
}

pub fn lower_to_cps<'gc>(
    ctx: Context<'gc>,
    il: TermRef<'gc>,
    module: Option<Gc<'gc, Module<'gc>>>,
    expand_primitives: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    lower_expanded_to_cps(ctx, il, module, expand_primitives)
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

    let graph = {
        let _profile = ProfileScope::new("compiler.lower.compile_cps_toplevel");
        compile_cps::cps_toplevel(ctx, &[optimized_il])
            .unwrap_or_else(|err| panic!("gcps lowering failed: {err}"))
    };
    let graph_root = graph
        .graph
        .read_term_link(graph.root())
        .expect("graph root");
    let graph_cps = graph.graph.pretty_term(graph_root);
    let linear_cps = {
        let _profile = ProfileScope::new("compiler.lower.gcps.optimize");
        optimize_graph_func_to_linear(ctx, graph)
            .unwrap_or_else(|err| panic!("gcps optimization failed: {err}"))
            .linear
    };
    Ok(LoweredProgram {
        original_il,
        optimized_il,
        graph_cps,
        linear_cps,
    })
}

pub(crate) fn dump_lowered_program_artifacts<'gc>(
    ctx: Context<'gc>,
    destination: impl AsRef<Path>,
    lowered: &LoweredProgram<'gc>,
    options: DumpArtifactsOptions,
) {
    if !options.has_frontend_artifacts() {
        return;
    }

    let destination = destination.as_ref().display().to_string();

    if options.dump_ir && options.include_unoptimized {
        let doc = lowered
            .original_il
            .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file_noopt = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(format!("{destination}.ir.noopt.scm"))
            .unwrap();
        log::info!(";; TRACE  (capy)@load: IR noopt -> {destination}.ir.noopt.scm");
        doc.1.render(80, &mut file_noopt).unwrap();
    }

    if options.dump_ir {
        let doc = lowered
            .optimized_il
            .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(format!("{destination}.ir.scm"))
            .unwrap();
        log::info!(";; TRACE  (capy)@load: IR -> {destination}.ir.scm");
        doc.1.render(80, &mut file).unwrap();
    }

    if options.dump_graph {
        std::fs::write(format!("{destination}.gcps.scm"), &lowered.graph_cps).unwrap();
        log::info!(";; TRACE  (capy)@load: GCPS -> {destination}.gcps.scm");
    }

    if options.dump_lcps {
        let _ = ctx;
        let rendered = render_lcps_dump(&lowered.linear_cps);
        std::fs::write(format!("{destination}.lcps.scm"), rendered).unwrap();
        log::info!(";; TRACE  (capy)@load: LCPS -> {destination}.lcps.scm");
    }
}

fn render_lcps_dump<'gc>(linear_cps: &crate::compiler::ssa::LinearProgram<'gc>) -> String {
    let mut rendered = crate::compiler::cps::linear_pretty::render_program(linear_cps);
    rendered.push('\n');

    rendered
}

#[cfg(test)]
mod tests {
    use super::{
        DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, render_lcps_dump,
    };
    use crate::{
        compiler::{
            cranelift::primitive::Primitive,
            ssa::{
                Block, BlockId, CodeId, GraphCodeId, LinearAtom, LinearProgram, Procedure,
                ProcedureKind, Terminator, ValueId,
            },
        },
        expander::{
            core::{LVarRef, fresh_lvar},
            term::{Term as IlTerm, TermKind as IlTermKind},
        },
        rsgc::{Gc, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Str, Symbol, Value, Vector},
        },
    };

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn dummy_il<'gc>(ctx: Context<'gc>) -> crate::expander::core::TermRef<'gc> {
        Gc::new(
            *ctx,
            IlTerm {
                source: Lock::new(Value::new(false)),
                kind: IlTermKind::Const(Value::new(42)),
            },
        )
    }

    fn source<'gc>(ctx: Context<'gc>) -> Value<'gc> {
        Vector::from_slice(
            *ctx,
            &[
                Str::from_str(*ctx, "dump.scm").into(),
                Value::new(2),
                Value::new(4),
                Value::new(2),
                Value::new(12),
                Value::new(false),
                Value::new(false),
                Symbol::from_str(ctx, "read").into(),
                Value::null(),
            ],
        )
        .into()
    }

    fn dummy_graph_linear<'gc>(ctx: Context<'gc>) -> LinearProgram<'gc> {
        let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "lcps-dump-test").into());
        let p0 = ValueId(0);
        let src = source(ctx);
        let mut sources = std::collections::HashMap::new();
        sources.insert(ValueId(10_000), binding);
        LinearProgram {
            entry: CodeId::GraphFunction(GraphCodeId(7)),
            procedures: vec![Procedure {
                code: CodeId::GraphFunction(GraphCodeId(7)),
                kind: ProcedureKind::Function,
                binding: ValueId(10_000),
                name: Symbol::from_str(ctx, "lcps-dump-test").into(),
                source: src,
                meta: Value::new(false),
                return_cont: None,
                params: vec![p0],
                variadic: None,
                free_vars: vec![],
                sources,
                entry: BlockId(0),
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![p0],
                    variadic: None,
                    instructions: vec![],
                    terminator: Terminator::TailCall {
                        callee: LinearAtom::Local(p0),
                        args: vec![],
                        source: src,
                    },
                    source: src,
                }],
            }],
        }
    }

    #[test]
    fn trace_artifacts_omit_tree_cps_dumps() {
        with_ctx(|ctx| {
            let dir = std::env::temp_dir()
                .join(format!("capy-no-tree-cps-dump-test-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            let destination = dir.join("out.fasl");
            let il = dummy_il(ctx);
            let lowered = LoweredProgram {
                original_il: il,
                optimized_il: il,
                graph_cps: "(gcps-dump-test)".to_string(),
                linear_cps: dummy_graph_linear(ctx),
            };

            dump_lowered_program_artifacts(
                ctx,
                &destination,
                &lowered,
                DumpArtifactsOptions::trace(),
            );

            assert!(!dir.join("out.fasl.cps.opt.scm").exists());
            assert!(!dir.join("out.fasl.cps.scm").exists());
            assert!(dir.join("out.fasl.lcps.scm").exists());

            std::fs::remove_dir_all(&dir).unwrap();
        });
    }

    #[test]
    fn trace_artifacts_use_available_linear_cps_dump() {
        with_ctx(|ctx| {
            let dir = std::env::temp_dir()
                .join(format!("capy-linear-cps-dump-test-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            let destination = dir.join("out.fasl");
            let il = dummy_il(ctx);
            let linear_cps = dummy_graph_linear(ctx);
            let lowered = LoweredProgram {
                original_il: il,
                optimized_il: il,
                graph_cps: "(gcps-dump-test)".to_string(),
                linear_cps,
            };

            dump_lowered_program_artifacts(
                ctx,
                &destination,
                &lowered,
                DumpArtifactsOptions::trace(),
            );

            let lcps =
                std::fs::read_to_string(dir.join("out.fasl.lcps.scm")).expect("linear CPS dump");
            assert!(lcps.contains("(entry (graph-function 7))"));
            assert!(lcps.contains("(procedure function (graph-function 7)"));
            assert!(lcps.contains("; @ dump.scm:2:4-2:12"));

            std::fs::remove_dir_all(&dir).unwrap();
        });
    }

    #[test]
    fn explicit_graph_dump_writes_gcps_artifact() {
        with_ctx(|ctx| {
            let dir = std::env::temp_dir()
                .join(format!("capy-graph-cps-dump-test-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            let destination = dir.join("out.fasl");
            let il = dummy_il(ctx);
            let lowered = LoweredProgram {
                original_il: il,
                optimized_il: il,
                graph_cps: "(gcps-dump-test)".to_string(),
                linear_cps: dummy_graph_linear(ctx),
            };
            let mut options = DumpArtifactsOptions::default();
            assert!(options.enable("graph"));

            dump_lowered_program_artifacts(ctx, &destination, &lowered, options);

            let gcps =
                std::fs::read_to_string(dir.join("out.fasl.gcps.scm")).expect("graph CPS dump");
            assert!(gcps.contains("gcps-dump-test"));
            assert!(!dir.join("out.fasl.lcps.scm").exists());

            std::fs::remove_dir_all(&dir).unwrap();
        });
    }

    #[test]
    fn lcps_dump_includes_slot_allocation() {
        with_ctx(|ctx| {
            let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "lcps-dump-test").into());
            let p0 = ValueId(0);
            let tmp = ValueId(1);
            let mut sources = std::collections::HashMap::new();
            sources.insert(ValueId(10_000), binding);
            let procedure = Procedure {
                code: CodeId::GraphFunction(GraphCodeId(7)),
                kind: ProcedureKind::Function,
                binding: ValueId(10_000),
                name: Value::new(false),
                source: Value::new(false),
                meta: Value::new(false),
                return_cont: None,
                params: vec![p0],
                variadic: None,
                free_vars: vec![],
                sources,
                entry: BlockId(0),
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![p0],
                    variadic: None,
                    instructions: vec![crate::compiler::ssa::Instruction::PrimCall {
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
                entry: CodeId::GraphFunction(GraphCodeId(7)),
                procedures: vec![procedure],
            };

            let rendered = render_lcps_dump(&linear);

            assert!(rendered.contains("(linear-program"));
            assert!(rendered.contains("(procedure graph-function"));
            assert!(rendered.contains("(binding %v10000)"));
            assert!(rendered.contains("(params %v0)"));
            assert!(rendered.contains("(prim-call %v1 car %v0)"));
            assert!(rendered.contains("(tail-call %v1 %v0)"));
        });
    }
}

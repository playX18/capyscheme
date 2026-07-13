use std::path::Path;

use crate::compiler::cps::optimize::optimize_graph_func_to_ssa;
use crate::compiler::dump;
use crate::compiler::ssa::Program;
use crate::expander::core::TermRef;
use crate::expander::{
    assignment_elimination, compile_cps, eta_expand::eta_expand, fix_letrec::fix_letrec,
    free_vars::resolve_free_vars, letrectify::letrectify, primitives,
};
use crate::rsgc::Gc;
use crate::runtime::stats::{CompilationBreakdownPhase, CompilationBreakdownScope};
use crate::runtime::{Context, modules::Module, value::Value};
use crate::utils::pass_profile::ProfileScope;

#[derive(Clone)]
pub struct LoweredProgram<'gc> {
    pub(crate) original_il: TermRef<'gc>,
    pub(crate) optimized_il: TermRef<'gc>,
    pub(crate) graph_cps: String,
    pub(crate) ssa: Program<'gc>,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct DumpArtifactsOptions {
    pub(crate) enabled: bool,
    pub(crate) include_unoptimized: bool,
    pub(crate) dump_ir: bool,
    pub(crate) dump_graph: bool,
    pub(crate) dump_ssa: bool,
    pub(crate) dump_cranelift: bool,
    pub(crate) dump_disassembly: bool,
}

impl DumpArtifactsOptions {
    pub(crate) fn trace() -> Self {
        Self {
            enabled: true,
            include_unoptimized: true,
            dump_ir: true,
            dump_ssa: true,
            ..Self::default()
        }
    }

    pub(crate) fn enable(&mut self, name: &str) -> bool {
        self.enabled = true;
        match name {
            "ir" => self.dump_ir = true,
            "graph" | "gcps" => self.dump_graph = true,
            "ssa" => self.dump_ssa = true,
            "cranelift" | "clif" => self.dump_cranelift = true,
            "disassembly" | "asm" => self.dump_disassembly = true,
            _ => return false,
        }
        true
    }

    pub(crate) fn has_frontend_artifacts(self) -> bool {
        self.enabled && (self.dump_ir || self.dump_graph || self.dump_ssa)
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
    let graph_cps = crate::compiler::cps::pretty::render_graph(&graph.graph, graph_root);
    let ssa = {
        let _profile = ProfileScope::new("compiler.lower.gcps.optimize");
        optimize_graph_func_to_ssa(ctx, graph)
            .unwrap_or_else(|err| panic!("gcps optimization failed: {err}"))
            .ssa
    };
    Ok(LoweredProgram {
        original_il,
        optimized_il,
        graph_cps,
        ssa,
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

    let destination = destination.as_ref();

    if options.dump_ir && options.include_unoptimized {
        let path = dump::resolve_artifact_dump_path(destination, ".ir.noopt.scm");
        let doc = lowered
            .original_il
            .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file_noopt = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&path)
            .unwrap();
        dump::log_dump_path("IR noopt", &path);
        doc.1.render(80, &mut file_noopt).unwrap();
    }

    if options.dump_ir {
        let path = dump::resolve_artifact_dump_path(destination, ".ir.scm");
        let doc = lowered
            .optimized_il
            .pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&path)
            .unwrap();
        dump::log_dump_path("IR", &path);
        doc.1.render(80, &mut file).unwrap();
    }

    if options.dump_graph {
        let path = dump::resolve_artifact_dump_path(destination, ".gcps.txt");
        std::fs::write(&path, &lowered.graph_cps).unwrap();
        dump::log_dump_path("GCPS", &path);
    }

    if options.dump_ssa {
        let _ = ctx;
        let path = dump::resolve_artifact_dump_path(destination, ".ssa.txt");
        let mut rendered = crate::compiler::ssa::render_program(&lowered.ssa);
        rendered.push('\n');
        std::fs::write(&path, rendered).unwrap();
        dump::log_dump_path("SSA", &path);
    }
}

#[cfg(test)]
mod tests {
    use super::{DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts};
    use crate::{
        compiler::{
            cranelift::primitive::Primitive,
            ssa::{
                Block, BlockId, CodeId, GraphCodeId, Operand, Procedure, ProcedureKind, Program,
                Terminator, ValueId,
            },
        },
        expander::{
            core::fresh_lvar,
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

    fn dummy_ssa<'gc>(ctx: Context<'gc>) -> Program<'gc> {
        let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "ssa-dump-test").into());
        let p0 = ValueId(0);
        let src = source(ctx);
        let mut sources = std::collections::HashMap::new();
        sources.insert(ValueId(10_000), binding);
        Program {
            entry: CodeId::GraphFunction(GraphCodeId(7)),
            procedures: vec![Procedure {
                code: CodeId::GraphFunction(GraphCodeId(7)),
                kind: ProcedureKind::Function,
                binding: ValueId(10_000),
                name: Symbol::from_str(ctx, "ssa-dump-test").into(),
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
                        callee: Operand::Local(p0),
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
                ssa: dummy_ssa(ctx),
            };

            dump_lowered_program_artifacts(
                ctx,
                &destination,
                &lowered,
                DumpArtifactsOptions::trace(),
            );

            assert!(!dir.join("out.fasl.cps.opt.scm").exists());
            assert!(!dir.join("out.fasl.cps.scm").exists());
            assert!(dir.join("out.fasl.ssa.txt").exists());

            std::fs::remove_dir_all(&dir).unwrap();
        });
    }

    #[test]
    fn trace_artifacts_use_available_ssa_dump() {
        with_ctx(|ctx| {
            let dir =
                std::env::temp_dir().join(format!("capy-ssa-dump-test-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&dir);
            std::fs::create_dir_all(&dir).unwrap();
            let destination = dir.join("out.fasl");
            let il = dummy_il(ctx);
            let ssa = dummy_ssa(ctx);
            let lowered = LoweredProgram {
                original_il: il,
                optimized_il: il,
                graph_cps: "(gcps-dump-test)".to_string(),
                ssa,
            };

            dump_lowered_program_artifacts(
                ctx,
                &destination,
                &lowered,
                DumpArtifactsOptions::trace(),
            );

            let ssa = std::fs::read_to_string(dir.join("out.fasl.ssa.txt")).expect("SSA dump");
            assert!(ssa.contains("function gf7 ssa-dump-test(v0) -> #f {"));
            assert!(ssa.contains("block0(v0):"));

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
                ssa: dummy_ssa(ctx),
            };
            let mut options = DumpArtifactsOptions::default();
            assert!(options.enable("graph"));

            dump_lowered_program_artifacts(ctx, &destination, &lowered, options);

            let gcps =
                std::fs::read_to_string(dir.join("out.fasl.gcps.txt")).expect("graph CPS dump");
            assert!(gcps.contains("gcps-dump-test"));
            assert!(!dir.join("out.fasl.ssa.txt").exists());

            std::fs::remove_dir_all(&dir).unwrap();
        });
    }

    #[test]
    fn ssa_dump_includes_slot_allocation() {
        with_ctx(|ctx| {
            let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "ssa-dump-test").into());
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
                        prim: Primitive::Car,
                        args: vec![Operand::Local(p0)],
                        source: Value::new(false),
                    }],
                    terminator: Terminator::TailCall {
                        callee: Operand::Local(tmp),
                        args: vec![Operand::Local(p0)],
                        source: Value::new(false),
                    },
                    source: Value::new(false),
                }],
            };
            let program = Program {
                entry: CodeId::GraphFunction(GraphCodeId(7)),
                procedures: vec![procedure],
            };

            let rendered = crate::compiler::ssa::render_program(&program);

            assert!(rendered.contains("function gf7 ssa-dump-test(v0) -> #f {"));
            assert!(rendered.contains("block0(v0):"));
            assert!(rendered.contains("v1 = car(v0)"));
            assert!(rendered.contains("tail_call v1(v0)"));
        });
    }
}

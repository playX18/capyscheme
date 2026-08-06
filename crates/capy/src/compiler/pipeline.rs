use std::path::Path;

use crate::compiler::cfg::Program;
use crate::compiler::cps::optimize::optimize_graph_func_to_ssa;
use crate::compiler::dump;
use crate::expander::core::TermRef;
use crate::expander::{
    assignment_elimination, compile_cps, free_vars::annotate_free_vars, primitives,
    rectify_letrec::rectify_letrec, recursive_bindings::rewrite_recursive_bindings,
    well_known_procs::expand_well_known_procs,
};
use crate::heap::Gc;
use crate::runtime::stats::{CompilationBreakdownPhase, CompilationBreakdownScope};
use crate::runtime::{Context, modules::Module, value::Value};
use crate::utils::pass_profile::ProfileScope;

#[derive(Clone)]
pub struct LoweredProgram<'gc> {
    pub(crate) original_il: TermRef<'gc>,
    pub(crate) optimized_il: TermRef<'gc>,
    pub(crate) graph_cps: Option<String>,
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
    expand_primitive_calls: bool,
    dump_graph: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    lower_expanded_to_cps(ctx, il, module, expand_primitive_calls, dump_graph)
}

pub(crate) fn lower_expanded_to_cps<'gc>(
    ctx: Context<'gc>,
    mut il: TermRef<'gc>,
    module: Option<Gc<'gc, Module<'gc>>>,
    expand_primitive_calls: bool,
    dump_graph: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    let original_il = il;
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Lowering);

    if expand_primitive_calls && let Some(module) = module {
        il = primitives::resolve_primitive_refs(ctx, il, module);
        il = primitives::expand_primitive_calls(ctx, il);
        let _profile = ProfileScope::new("compiler.lower.annotate_free_vars");
        il = annotate_free_vars(ctx, il);
        drop(_profile);
        let _profile = ProfileScope::new("compiler.lower.rectify_letrec");
        il = rectify_letrec(ctx, il);
        drop(_profile);
    }

    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.rewrite_recursive_bindings");
        rewrite_recursive_bindings(ctx, il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.expand_well_known_procs");
        expand_well_known_procs(ctx, optimized_il)
    };
    let optimized_il = {
        let _profile = ProfileScope::new("compiler.lower.assignment_elimination");
        assignment_elimination::eliminate_assignments(ctx, optimized_il)
    };

    let graph = {
        let _profile = ProfileScope::new("compiler.lower.compile_cps_toplevel");
        compile_cps::cps_toplevel(ctx, &[optimized_il]).map_err(|err| {
            crate::runtime::vm::exceptions::make_assertion_violation(
                ctx,
                Some("gcps-lowering"),
                &format!("compiler error: {err}"),
                &[],
            )
        })?
    };
    let graph_root = graph
        .graph
        .read_term_link(graph.root())
        .expect("graph root");
   
    let graph_cps =
        dump_graph.then(|| crate::compiler::cps::pretty::render_graph(&graph.graph, graph_root));
    let ssa = {
        let _profile = ProfileScope::new("compiler.lower.gcps.optimize");
        optimize_graph_func_to_ssa(ctx, graph)
            .map_err(|err| {
                crate::runtime::vm::exceptions::make_assertion_violation(
                    ctx,
                    Some("gcps-optimize"),
                    &format!("compiler error: {err}"),
                    &[],
                )
            })?
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
            .expect("infallible allocation callback");
        dump::log_dump_path("IR noopt", &path);
        doc.1.render(80, &mut file_noopt).expect("invariant holds");
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
            .expect("infallible allocation callback");
        dump::log_dump_path("IR", &path);
        doc.1.render(80, &mut file).expect("invariant holds");
    }

    if options.dump_graph {
        let path = dump::resolve_artifact_dump_path(destination, ".gcps.txt");
        if let Some(graph_cps) = &lowered.graph_cps {
            std::fs::write(&path, graph_cps).expect("invariant holds");
        }
        dump::log_dump_path("GCPS", &path);
    }

    if options.dump_ssa {
        let _ = ctx;
        let path = dump::resolve_artifact_dump_path(destination, ".ssa.txt");
        let mut rendered = crate::compiler::cfg::render_program(&lowered.ssa);
        rendered.push('\n');
        std::fs::write(&path, rendered).expect("invariant holds");
        dump::log_dump_path("SSA", &path);
    }
}

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
}

use std::path::PathBuf;

use crate::compiler::{
    CompilationOptions, DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts,
    lower_expanded_to_cps,
};
use crate::list;
use crate::prelude::*;
use crate::runtime::modules::{Module, current_module};
use crate::runtime::value::{Closure, Str, Value};
use crate::runtime::vm::base::scm_log_level;
use crate::runtime::vm::expand::ScmTermToRsTerm;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;
#[cfg(feature = "bootstrap")]
use crate::runtime::vm::thunks::make_lexical_violation;
use capy_derive::scheme;

use super::paths::{fallback_file_name, find_path_to};
use super::{
    artifact::LoadArtifact,
    compile::{
        CompilationPhase, compile_cps_to_destination, destination_artifact_for_current_policy,
        load_thunk_in_vicinity,
    },
};

type LoadResult<'gc> = Result<Value<'gc>, Value<'gc>>;
type OptionalStringRef<'gc> = Option<StringRef<'gc>>;

#[scheme(path=capy)]
pub(super) mod load_ops {
    #[allow(unused_imports)]
    use super::*;

    #[scheme(name = "%find-path-to")]
    pub fn scm_find_path_to(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
        arch: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|path| PathBuf::from(path.as_gc_ref().to_string()));
        let arch = arch.map(|value| value.to_string());
        let result = find_path_to_public(
            nctx.ctx,
            filename,
            resolve_relative,
            in_vicinity,
            arch.as_deref(),
        );
        nctx.return_(result)
    }

    #[scheme(name = "%compile")]
    pub fn compile(
        expanded: Value<'gc>,
        destination: StringRef<'gc>,
        m: Option<Value<'gc>>,
        load_thunk: Option<bool>,
    ) -> LoadResult<'gc> {
        let ctx = nctx.ctx;
        let module = resolve_module_value(ctx, m);
        let result = compile_expanded_to_destination(
            ctx,
            expanded,
            destination.to_string().as_str(),
            module,
            CompilationOptions {
                backtraces: compile_backtraces_enabled(ctx),
            },
            load_thunk.unwrap_or(true),
            DumpArtifactsOptions {
                enabled: scm_log_level(ctx) >= 5,
                include_unoptimized: true,
            },
        );
        nctx.return_(result)
    }

    #[scheme(name = "try-load-thunk-in-vicinity")]
    pub fn scm_try_load_thunk_in_vicinity(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let ctx = nctx.ctx;
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result =
            load_thunk_in_vicinity::<false>(ctx, filename, in_vicinity, resolve_relative, None);
        nctx.return_(result)
    }

    #[scheme(name = "load-thunk-in-vicinity")]
    pub fn scm_load_thunk_in_vicinity(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let ctx = nctx.ctx;
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result =
            load_thunk_in_vicinity::<true>(ctx, filename, in_vicinity, resolve_relative, None);
        nctx.return_(result)
    }

    #[scheme(name = "%bootstrapping?")]
    pub fn scm_bootstrapping() -> LoadResult<'gc> {
        #[cfg(feature = "bootstrap")]
        {
            nctx.return_(Ok(Value::new(true)))
        }
        #[cfg(not(feature = "bootstrap"))]
        {
            nctx.return_(Ok(Value::new(false)))
        }
    }

    #[scheme(name = "load-thunk-in-vicinity-k")]
    #[allow(dead_code)]
    #[allow(unused_variables)]
    pub fn scm_load_thunk_in_vicinity_k(
        filename: StringRef<'gc>,
        k: Value<'gc>,
        env: Value<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result = load_thunk_in_vicinity::<false>(
            nctx.ctx,
            filename,
            in_vicinity,
            resolve_relative,
            None,
        );

        match result {
            Ok(thunk) if thunk.is::<Closure>() => nctx.return_(Ok(thunk)),
            Ok(thunk) => {
                #[cfg(feature = "bootstrap")]
                {
                    let ctx = nctx.ctx;
                    let file = thunk.car().downcast::<Str>().to_string();
                    let file_in = match std::fs::File::open(&file).map_err(|e| {
                        make_io_error(
                            ctx,
                            "compile-file",
                            Str::new(
                                *ctx,
                                format!("Cannot open input file '{}': {}", file, e),
                                true,
                            )
                            .into(),
                            &[],
                        )
                    }) {
                        Ok(file_in) => file_in,
                        Err(err) => return nctx.return_(Err(err)),
                    };

                    let text = match std::io::read_to_string(&file_in).map_err(|e| {
                        make_io_error(
                            ctx,
                            "compile-file",
                            Str::new(
                                *ctx,
                                format!("Cannot read input file '{}': {}", file, e),
                                true,
                            )
                            .into(),
                            &[],
                        )
                    }) {
                        Ok(text) => text,
                        Err(err) => return nctx.return_(Err(err)),
                    };
                    let src = Str::new(*ctx, &file, true);
                    let parser =
                        crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), true);
                    let program = match parser.read_program().map_err(|err| {
                        make_lexical_violation(ctx, "compile-file", err.display_with_file(&file))
                    }) {
                        Ok(program) => program,
                        Err(err) => return nctx.return_(Err(err)),
                    };

                    let retk = nctx.retk;
                    let after_call = make_closure_continue_loading_k(ctx, [thunk, retk]);
                    let program = Value::list_from_slice(ctx, program);
                    return nctx.call(k, &[program, env], after_call.into());
                }

                #[cfg(not(feature = "bootstrap"))]
                {
                    let ctx = nctx.ctx;
                    nctx.return_(Err(make_io_error(
                        ctx,
                        "load-thunk-in-vicinity-k",
                        Str::new(
                            *ctx,
                            "load-thunk-in-vicinity-k is not available in non-bootstrap builds",
                            true,
                        )
                        .into(),
                        &[],
                    )))
                }
            }
            Err(err) => nctx.return_(Err(err)),
        }
    }

    #[scheme(name = "%search-load-path")]
    pub fn search_load_path(filename: StringRef<'gc>, arch: Option<StringRef<'gc>>) -> Value<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let arch = arch.map(|value| value.to_string());
        let result =
            find_path_to_public(nctx.ctx, filename, true, None::<PathBuf>, arch.as_deref());
        match result {
            Ok(value) => nctx.return_(value),
            Err(_) => nctx.return_(Value::new(false)),
        }
    }
}

#[scheme(continuation)]
pub(crate) fn continue_loading_k(
    ir: Value<'gc>,
    cenv: Value<'gc>,
    _unused: Value<'gc>,
) -> LoadResult<'gc> {
    let rator = nctx.rator().downcast::<Closure>();
    let source_and_compiled_path = rator[1].get();
    let retk = rator[2].get();
    nctx.retk = retk;

    let compiled_path = source_and_compiled_path
        .list_ref(2)
        .expect("Expected compiled path")
        .downcast::<Str>()
        .to_string();
    let result = compile_expanded_to_destination(
        nctx.ctx,
        ir,
        compiled_path.as_str(),
        resolve_continuation_module(nctx.ctx, cenv),
        CompilationOptions::default(),
        true,
        DumpArtifactsOptions::default(),
    );

    match result {
        // SAFETY: `retk` is a valid continuation frame on the stack
        Ok(thunk) => unsafe { nctx.continue_to(retk, &[thunk]) },
        Err(err) => nctx.return_(Err(err)),
    }
}

fn compile_backtraces_enabled<'gc>(ctx: Context<'gc>) -> bool {
    if let Some(key) = ctx.private_ref("capy", "*compile-backtrace-key*") {
        match ctx.get_mark_first(key) {
            Some(mark) => mark != Value::new(false),
            None => true,
        }
    } else {
        true
    }
}

fn resolve_module_value<'gc>(
    ctx: Context<'gc>,
    module: Option<Value<'gc>>,
) -> Gc<'gc, Module<'gc>> {
    match module {
        Some(module) if module.is::<Module>() => module.downcast(),
        _ => current_module(ctx).get(ctx).downcast(),
    }
}

#[allow(dead_code)]
fn resolve_continuation_module<'gc>(ctx: Context<'gc>, cenv: Value<'gc>) -> Gc<'gc, Module<'gc>> {
    if cenv.is::<Module>() {
        cenv.downcast()
    } else {
        current_module(ctx).get(ctx).downcast()
    }
}

fn compile_expanded_to_destination<'gc>(
    ctx: Context<'gc>,
    expanded: Value<'gc>,
    destination: &str,
    module: Gc<'gc, Module<'gc>>,
    options: CompilationOptions,
    load_thunk: bool,
    dump_options: DumpArtifactsOptions,
) -> LoadResult<'gc> {
    let _phase = CompilationPhase::new(ctx);
    let lowered = lower_expanded_scheme(ctx, expanded, module)?;
    let destination = destination_artifact_for_current_policy(destination);
    dump_lowered_program_artifacts(ctx, &destination.path, &lowered, dump_options);
    compile_cps_to_destination(ctx, lowered.cps, options, &destination)?;

    if !load_thunk {
        return Ok(Str::new(*ctx, destination.path.display().to_string(), true).into());
    }

    load_compiled_library(ctx, &destination)
}

fn lower_expanded_scheme<'gc>(
    ctx: Context<'gc>,
    expanded: Value<'gc>,
    module: Gc<'gc, Module<'gc>>,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    let mut reader = ScmTermToRsTerm::new(ctx);
    let ir = reader.convert(expanded)?;
    lower_expanded_to_cps(ctx, ir, Some(module), cfg!(feature = "bootstrap"))
}

fn load_compiled_library<'gc>(
    ctx: Context<'gc>,
    compiled_artifact: &LoadArtifact,
) -> LoadResult<'gc> {
    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    libs.load(compiled_artifact, ctx).map_err(|err| {
        make_io_error(
            ctx,
            "load",
            Str::new(
                *ctx,
                format!(
                    "Failed to load compiled artifact {}: {err}",
                    compiled_artifact.path.display()
                ),
                true,
            )
            .into(),
            &[],
        )
    })
}

fn find_path_to_public<'gc>(
    ctx: Context<'gc>,
    filename: PathBuf,
    resolve_relative: bool,
    in_vicinity: Option<PathBuf>,
    arch: Option<&str>,
) -> LoadResult<'gc> {
    match find_path_to(ctx, filename, in_vicinity, resolve_relative, arch)? {
        Some((source, full_source, compiled)) => {
            let compiled = compiled.unwrap_or_else(|| fallback_file_name(ctx, &full_source));
            Ok(list!(
                ctx,
                Str::new(*ctx, source.to_string_lossy(), true),
                Str::new(*ctx, full_source.to_string_lossy(), true),
                Str::new(*ctx, compiled.to_string_lossy(), true)
            ))
        }
        None => Ok(Value::new(false)),
    }
}

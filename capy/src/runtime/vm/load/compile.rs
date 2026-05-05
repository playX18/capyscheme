use std::path::Path;

#[cfg(feature = "aot")]
use crate::compiler::compile_cps_to_shared_object;
use crate::compiler::{CompilationOptions, compile_cps_to_jit_thunk, compile_file};
use crate::cps::{
    linear::linearize,
    linear_artifact::{LinearArtifactOptions, read_linear_program, write_linear_program},
    reify,
};
use crate::runtime::Context;
use crate::runtime::modules::current_module;
use crate::runtime::stats::{CompilationBreakdownPhase, CompilationBreakdownScope};
use crate::runtime::value::{Closure, Str, Value};
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;

use super::{
    artifact::{LoadArtifact, LoadArtifactKind, artifact_extension, artifact_kind_for_policy},
    paths::{ResolvedLoadPath, resolve_load_path},
    policy::get_execution_policy,
};

enum LoadMode {
    DescribeIfCompileNeeded,
    ForceCompile,
}

pub(crate) struct CompilationPhase<'gc> {
    ctx: Context<'gc>,
    restored: bool,
}

impl<'gc> CompilationPhase<'gc> {
    pub(crate) fn new(ctx: Context<'gc>) -> Self {
        ctx.stats.start_compilation();
        Self {
            ctx,
            restored: false,
        }
    }

    fn restore(&mut self) {
        if !self.restored {
            self.ctx.stats.end_compilation();
            self.restored = true;
        }
    }
}

impl Drop for CompilationPhase<'_> {
    fn drop(&mut self) {
        self.restore();
    }
}

pub fn load_thunk_in_vicinity<'gc, const FORCE_COMPILE: bool>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Value<'gc>, Value<'gc>> {
    let mode = if FORCE_COMPILE {
        LoadMode::ForceCompile
    } else {
        LoadMode::DescribeIfCompileNeeded
    };
    load_thunk(ctx, filename, in_vicinity, resolve_relative, arch, mode)
}

fn load_thunk<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
    mode: LoadMode,
) -> Result<Value<'gc>, Value<'gc>> {
    let filename = filename.as_ref();
    let resolved = match resolve_load_path(ctx, filename, in_vicinity, resolve_relative, arch)? {
        Some(resolved) => resolved,
        None => {
            return Err(make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    format!("File not found: {}", filename.to_string_lossy()),
                    true,
                )
                .into(),
                &[],
            ));
        }
    };

    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    match &resolved {
        ResolvedLoadPath::Artifact { artifact, .. } => return load_artifact(ctx, libs, artifact),
        ResolvedLoadPath::Source { .. } => {}
    }

    if let Some(thunk) = load_precompiled_thunk(ctx, libs, &resolved)? {
        return Ok(thunk);
    }

    match mode {
        LoadMode::DescribeIfCompileNeeded => describe_uncompiled_source(ctx, &resolved),
        LoadMode::ForceCompile => compile_and_load_source(ctx, libs, resolved),
    }
}

fn load_precompiled_thunk<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    resolved: &ResolvedLoadPath,
) -> Result<Option<Value<'gc>>, Value<'gc>> {
    let ResolvedLoadPath::Source {
        full_source_path,
        compiled_artifact,
        build_destination,
        ..
    } = resolved
    else {
        return Ok(None);
    };

    let candidate = if compiled_artifact.is_none() && !fresh_auto_compile(ctx) {
        if let Some(build_destination) = build_destination.as_ref()
            && let Some(source_time) = full_source_path
                .metadata()
                .ok()
                .and_then(|metadata| metadata.modified().ok())
            && let Some(compiled_time) = build_destination
                .path
                .metadata()
                .ok()
                .and_then(|metadata| metadata.modified().ok())
            && super::paths::compiled_is_fresh(
                full_source_path,
                &build_destination.path,
                source_time,
                compiled_time,
            )
        {
            Some(build_destination.clone())
        } else {
            None
        }
    } else {
        compiled_artifact.clone()
    };

    let Some(candidate) = candidate else {
        return Ok(None);
    };

    let loaded = load_artifact(ctx, libs, &candidate)?;
    if loaded.is::<Closure>() {
        Ok(Some(loaded))
    } else {
        Ok(None)
    }
}

fn fresh_auto_compile<'gc>(ctx: Context<'gc>) -> bool {
    ctx.globals().loc_fresh_auto_compile().get() != Value::new(false)
}

fn describe_uncompiled_source<'gc>(
    ctx: Context<'gc>,
    resolved: &ResolvedLoadPath,
) -> Result<Value<'gc>, Value<'gc>> {
    let ResolvedLoadPath::Source {
        source_path,
        full_source_path,
        compiled_artifact,
        build_destination,
    } = resolved
    else {
        return Err(make_io_error(
            ctx,
            "load",
            Str::new(*ctx, "Cannot describe a direct artifact load", true).into(),
            &[],
        ));
    };
    let source = Str::new(*ctx, source_path.display().to_string(), true);
    let full_source = Str::new(*ctx, full_source_path.display().to_string(), true);
    let compiled = compiled_artifact
        .clone()
        .or_else(|| build_destination.clone());
    let compiled = compiled
        .map(|compiled| Str::new(*ctx, compiled.path.display().to_string(), true).into())
        .unwrap_or(Value::new(false));
    Ok(crate::list!(ctx, source, full_source, compiled))
}

fn compile_and_load_source<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    resolved: ResolvedLoadPath,
) -> Result<Value<'gc>, Value<'gc>> {
    let ResolvedLoadPath::Source {
        source_path,
        build_destination,
        ..
    } = resolved
    else {
        return Err(make_io_error(
            ctx,
            "load",
            Str::new(*ctx, "Cannot compile a direct artifact load", true).into(),
            &[],
        ));
    };
    let module = current_module(ctx).get(ctx).downcast();
    let _phase = CompilationPhase::new(ctx);
    let cps = compile_file(ctx, &source_path, Some(module))?;
    if matches!(get_execution_policy(), super::policy::ExecutionPolicy::JIT)
        && build_destination.is_none()
    {
        return Err(make_io_error(
            ctx,
            "load",
            Str::new(
                *ctx,
                "JIT compilation requires a .csc destination artifact",
                true,
            )
            .into(),
            &[],
        ));
    }
    compile_cps_for_current_policy(
        ctx,
        cps,
        CompilationOptions::default(),
        build_destination.as_ref(),
        |artifact| load_artifact(ctx, libs, artifact),
    )
}

pub(super) fn compile_cps_for_current_policy<'gc>(
    ctx: Context<'gc>,
    cps: crate::cps::term::FuncRef<'gc>,
    options: CompilationOptions,
    destination: Option<&LoadArtifact>,
    load_artifact: impl FnOnce(&LoadArtifact) -> Result<Value<'gc>, Value<'gc>>,
) -> Result<Value<'gc>, Value<'gc>> {
    match get_execution_policy() {
        super::policy::ExecutionPolicy::JIT => {
            let Some(destination) = destination else {
                return compile_cps_to_jit_thunk(ctx, cps, options);
            };
            compile_cps_to_destination(ctx, cps, options, destination)?;
            load_artifact(destination)
        }
        #[cfg(feature = "aot")]
        super::policy::ExecutionPolicy::AOT => {
            let Some(destination) = destination else {
                return Err(make_io_error(
                    ctx,
                    "load",
                    Str::new(
                        *ctx,
                        "AOT compilation requires a destination artifact",
                        true,
                    )
                    .into(),
                    &[],
                ));
            };
            compile_cps_to_destination(ctx, cps, options, destination)?;
            load_artifact(destination)
        }
    }
}

fn load_compiled_scheme_artifact<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    artifact: &LoadArtifact,
) -> Result<Value<'gc>, Value<'gc>> {
    let file = std::fs::File::open(&artifact.path).map_err(|err| {
        make_io_error(
            ctx,
            "load",
            Str::new(
                *ctx,
                format!(
                    "Cannot open compiled Scheme artifact '{}': {err}",
                    artifact.path.display()
                ),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let (artifact_options, linear) = {
        let _scope = CompilationBreakdownScope::new(CompilationBreakdownPhase::ObjectEmit);
        read_linear_program(ctx, file).map_err(|err| {
            make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot read compiled Scheme artifact '{}': {err}",
                        artifact.path.display()
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?
    };

    let library = crate::jit::compile_linear_program_to_jit_library(
        ctx,
        linear,
        CompilationOptions {
            backtraces: artifact_options.backtraces,
        },
    );
    Ok(libs.register_jit(library))
}

pub(super) fn compile_cps_to_destination<'gc>(
    ctx: Context<'gc>,
    cps: crate::cps::term::FuncRef<'gc>,
    options: CompilationOptions,
    destination: &LoadArtifact,
) -> Result<(), Value<'gc>> {
    match destination.kind {
        LoadArtifactKind::CompiledScheme => {
            compile_cps_to_compiled_scheme_artifact(ctx, cps, options, destination)
        }
        #[cfg(feature = "aot")]
        LoadArtifactKind::SharedObject => {
            compile_cps_to_shared_object(ctx, cps, options, &destination.path)
        }
    }
}

fn compile_cps_to_compiled_scheme_artifact<'gc>(
    ctx: Context<'gc>,
    cps: crate::cps::term::FuncRef<'gc>,
    options: CompilationOptions,
    destination: &LoadArtifact,
) -> Result<(), Value<'gc>> {
    if destination.kind != LoadArtifactKind::CompiledScheme {
        return Err(make_io_error(
            ctx,
            "compile",
            Str::new(
                *ctx,
                format!(
                    "JIT compilation expected a .csc destination, got {}",
                    destination.path.display()
                ),
                true,
            )
            .into(),
            &[],
        ));
    }

    if let Some(parent) = destination.path.parent() {
        std::fs::create_dir_all(parent).map_err(|err| {
            make_io_error(
                ctx,
                "compile",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot create compiled artifact directory '{}': {err}",
                        parent.display()
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    }

    let file = std::fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(&destination.path)
        .map_err(|err| {
            make_io_error(
                ctx,
                "compile",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot open compiled artifact '{}': {err}",
                        destination.path.display()
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;

    let linear = {
        let _scope = CompilationBreakdownScope::new(CompilationBreakdownPhase::Lowering);
        let reify_info = reify(ctx, cps);
        linearize(ctx, &reify_info)
    };
    {
        let _scope = CompilationBreakdownScope::new(CompilationBreakdownPhase::ObjectEmit);
        write_linear_program(
            ctx,
            file,
            &linear,
            LinearArtifactOptions {
                backtraces: options.backtraces,
            },
        )
        .map_err(|err| {
            make_io_error(
                ctx,
                "compile",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot write compiled artifact '{}': {err}",
                        destination.path.display()
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })
    }
}

pub(super) fn load_artifact<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    artifact: &LoadArtifact,
) -> Result<Value<'gc>, Value<'gc>> {
    match artifact.kind {
        LoadArtifactKind::CompiledScheme => load_compiled_scheme_artifact(ctx, libs, artifact),
        #[cfg(feature = "aot")]
        LoadArtifactKind::SharedObject => libs.load(artifact, ctx).map_err(|err| {
            make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    format!("Failed to load {}: {err}", artifact.path.display()),
                    true,
                )
                .into(),
                &[],
            )
        }),
    }
}

pub(super) fn destination_artifact_for_current_policy(
    path: impl AsRef<Path>,
) -> Option<LoadArtifact> {
    let kind = artifact_kind_for_policy(get_execution_policy())?;
    let path = path.as_ref().with_extension(artifact_extension(kind));
    Some(LoadArtifact::new(kind, path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{Context, Scheme};
    use std::sync::Mutex;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    #[test]
    fn compilation_phase_restores_execution_after_success() {
        with_ctx(|ctx| {
            ctx.stats.start_execution();
            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());

            {
                let _phase = CompilationPhase::new(ctx);
                assert!(!ctx.stats.execution_active());
                assert!(ctx.stats.compilation_active());
            }

            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());
        });
    }

    #[test]
    fn compilation_phase_restores_execution_after_error_path() {
        with_ctx(|ctx| {
            ctx.stats.start_execution();

            let _: Result<(), ()> = (|| {
                let _phase = CompilationPhase::new(ctx);
                Err(())
            })();

            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());
        });
    }

    #[test]
    fn nested_compilation_scope_preserves_outer_compilation_state() {
        with_ctx(|ctx| {
            ctx.stats.start_execution();
            ctx.stats.start_compilation();

            assert!(!ctx.stats.execution_active());
            assert!(ctx.stats.compilation_active());

            {
                let _phase = CompilationPhase::new(ctx);
                assert!(!ctx.stats.execution_active());
                assert!(ctx.stats.compilation_active());
            }

            assert!(!ctx.stats.execution_active());
            assert!(ctx.stats.compilation_active());

            ctx.stats.end_compilation();

            assert!(ctx.stats.execution_active());
            assert!(!ctx.stats.compilation_active());
        });
    }
}

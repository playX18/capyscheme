use std::path::Path;

use crate::compiler::{CompilationOptions, compile_cps_to_shared_object, compile_file};
use crate::runtime::modules::current_module;
use crate::runtime::value::{Closure, Str, Value};
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::Context;

use super::paths::{ResolvedLoadPath, fallback_file_name, resolve_load_path};

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
        ctx.stats.end_execution();
        ctx.stats.start_compilation();
        Self {
            ctx,
            restored: false,
        }
    }

    fn restore(&mut self) {
        if !self.restored {
            self.ctx.stats.end_compilation();
            self.ctx.stats.start_execution();
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
    if let Some(thunk) = load_precompiled_thunk(ctx, libs, &resolved)? {
        return Ok(thunk);
    }

    match mode {
        LoadMode::DescribeIfCompileNeeded => describe_uncompiled_source(ctx, &resolved),
        LoadMode::ForceCompile => compile_and_load_source(ctx, libs, resolved),
    }
}

fn fresh_auto_compile<'gc>(ctx: Context<'gc>) -> bool {
    ctx.globals().loc_fresh_auto_compile().get() != Value::new(false)
}

fn load_precompiled_thunk<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    resolved: &ResolvedLoadPath,
) -> Result<Option<Value<'gc>>, Value<'gc>> {
    let fallback = fallback_file_name(ctx, &resolved.full_source_path);
    let candidate = if resolved.compiled_path.is_none() && !fresh_auto_compile(ctx) {
        if let Some(source_time) = resolved
            .full_source_path
            .metadata()
            .ok()
            .and_then(|metadata| metadata.modified().ok())
            && let Some(compiled_time) = fallback
                .metadata()
                .ok()
                .and_then(|metadata| metadata.modified().ok())
            && super::paths::compiled_is_fresh(
                &resolved.full_source_path,
                &fallback,
                source_time,
                compiled_time,
            )
        {
            Some(fallback)
        } else {
            None
        }
    } else {
        resolved.compiled_path.clone()
    };

    let Some(candidate) = candidate else {
        return Ok(None);
    };

    let loaded = libs.load(&candidate, ctx).unwrap_or(Value::new(false));
    if loaded.is::<Closure>() {
        Ok(Some(loaded))
    } else {
        Ok(None)
    }
}

fn describe_uncompiled_source<'gc>(
    ctx: Context<'gc>,
    resolved: &ResolvedLoadPath,
) -> Result<Value<'gc>, Value<'gc>> {
    let source = Str::new(*ctx, resolved.source_path.display().to_string(), true);
    let full_source = Str::new(*ctx, resolved.full_source_path.display().to_string(), true);
    let compiled = resolved
        .compiled_path
        .clone()
        .unwrap_or_else(|| fallback_file_name(ctx, &resolved.full_source_path));
    let compiled = Str::new(*ctx, compiled.display().to_string(), true);
    Ok(crate::list!(ctx, source, full_source, compiled))
}

fn compile_and_load_source<'gc>(
    ctx: Context<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    resolved: ResolvedLoadPath,
) -> Result<Value<'gc>, Value<'gc>> {
    let fallback = fallback_file_name(ctx, &resolved.full_source_path);
    let module = current_module(ctx).get(ctx).downcast();
    let _phase = CompilationPhase::new(ctx);
    let cps = compile_file(ctx, &resolved.source_path, Some(module))?;
    compile_cps_to_shared_object(ctx, cps, CompilationOptions::default(), &fallback)?;

    libs.load(&fallback, ctx).map_err(|err| {
        make_io_error(
            ctx,
            "load",
            Str::new(
                *ctx,
                format!("Failed to load compiled library: {err}"),
                true,
            )
            .into(),
            &[],
        )
    })
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
}

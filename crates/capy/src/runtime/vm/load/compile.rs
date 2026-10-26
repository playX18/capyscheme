use crate::runtime::sched::preempt::retry_eintr;
use std::fs::File;
use std::io::Write;
#[cfg(unix)]
use std::os::fd::AsRawFd;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::compiler::{
    CompilationOptions, DumpArtifactsOptions, LoweredProgram, begin_compilation_artifact,
    compile_file, compile_lowered_to_fasl_bytes, dump_lowered_program_artifacts,
    merge_compile_dump_options, resolve_artifact_dump_path,
};
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::compile_service::{self, Failure, JobInput};
use crate::runtime::modules::{Module, current_module};
use crate::runtime::value::{Closure, Str, Value};
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;

use super::{
    artifact::{LoadArtifact, LoadArtifactKind, artifact_extension, artifact_kind_for_policy},
    paths::{ResolvedLoadPath, resolve_load_path},
    policy::get_execution_policy,
};

static TEMP_ARTIFACT_COUNTER: AtomicU64 = AtomicU64::new(0);

enum LoadMode {
    DescribeIfCompileNeeded,
    ForceCompile,
}

pub(crate) struct CompilationPhase<'gc> {
    ctx: MutationContext<'gc>,
    restored: bool,
}

impl<'gc> CompilationPhase<'gc> {
    pub(crate) fn new(ctx: MutationContext<'gc>) -> Self {
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
    ctx: MutationContext<'gc>,
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
    ctx: MutationContext<'gc>,
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
                    ctx,
                    format!("File not found: {}", filename.to_string_lossy()),
                    true,
                )
                .into(),
                &[],
            ));
        }
    };

    let libs = LIBRARY_COLLECTION.fetch(ctx);
    match &resolved {
        ResolvedLoadPath::Artifact { artifact, .. } => return load_artifact(ctx, libs, artifact),
        ResolvedLoadPath::Source { .. } => {}
    }

    if let Some(thunk) = load_precompiled_thunk(ctx, libs, &resolved)? {
        return Ok(thunk);
    }

    match mode {
        LoadMode::DescribeIfCompileNeeded => describe_uncompiled_source(ctx, &resolved),
        LoadMode::ForceCompile => compile_and_load_source(ctx, resolved),
    }
}

fn fresh_auto_compile<'gc>(ctx: MutationContext<'gc>) -> bool {
    ctx.globals().loc_fresh_auto_compile().get() != Value::new(false)
}

fn load_precompiled_thunk<'gc>(
    ctx: MutationContext<'gc>,
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
        if let Some(source_time) = full_source_path
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

    let loaded = libs.load(&candidate, ctx).unwrap_or(Value::new(false));
    if loaded.is::<Closure>() {
        Ok(Some(loaded))
    } else {
        Ok(None)
    }
}

fn describe_uncompiled_source<'gc>(
    ctx: MutationContext<'gc>,
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
            Str::new(ctx, "Cannot describe a direct artifact load", true).into(),
            &[],
        ));
    };
    let source = Str::new(ctx, source_path.display().to_string(), true);
    let full_source = Str::new(ctx, full_source_path.display().to_string(), true);
    let compiled = compiled_artifact
        .clone()
        .unwrap_or_else(|| build_destination.clone());
    let compiled = Str::new(ctx, compiled.path.display().to_string(), true);
    Ok(crate::list!(ctx, source, full_source, compiled))
}

/// Inputs of a compile of expanded Scheme.
#[derive(Clone)]
pub(crate) struct ExpandedJob<'gc> {
    pub(crate) expanded: Value<'gc>,
    pub(crate) module: Gc<'gc, Module<'gc>>,
    pub(crate) destination: LoadArtifact,
    pub(crate) options: CompilationOptions,
    pub(crate) dump_options: DumpArtifactsOptions,
    pub(crate) preprocessed: bool,
}

/// Inputs of a compile of one source file with the Rust expander.
#[derive(Clone)]
pub(crate) struct SourceJob<'gc> {
    pub(crate) source: PathBuf,
    pub(crate) module: Gc<'gc, Module<'gc>>,
    pub(crate) destination: LoadArtifact,
}

/// Compile one source file on a compile thread. The caller loads the result.
pub(crate) fn run_source_job<'gc>(
    ctx: MutationContext<'gc>,
    job: SourceJob<'gc>,
) -> Result<(), Failure<'gc>> {
    begin_compilation_artifact(&job.destination.path);
    let dump_options = merge_compile_dump_options(DumpArtifactsOptions::default());
    let lowered = compile_file(ctx, &job.source, Some(job.module), dump_options.dump_graph)?;
    dump_lowered_program_artifacts(ctx, &job.destination.path, &lowered, dump_options);
    let mut options = CompilationOptions::default();
    if dump_options.dump_bytecode {
        options.backend_dumps.bytecode =
            Some(resolve_artifact_dump_path(&job.destination.path, ".bc.txt"));
    }
    compile_lowered_to_destination(ctx, lowered, options, &job.destination)?;
    Ok(())
}

fn compile_and_load_source<'gc>(
    ctx: MutationContext<'gc>,
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
            Str::new(ctx, "Cannot compile a direct artifact load", true).into(),
            &[],
        ));
    };
    let module = current_module(ctx).get(ctx).downcast();
    let _phase = CompilationPhase::new(ctx);
    let job = SourceJob {
        source: source_path,
        module,
        destination: build_destination.clone(),
    };
    let (ctx, outcome) = compile_service::run(ctx, JobInput::Source(job));
    compile_service::raise_failure(ctx, outcome.unwrap_or_else(|killed| killed.unwind()))?;

    let libs = LIBRARY_COLLECTION.fetch(ctx);
    load_artifact(ctx, libs, &build_destination)
}

pub(super) fn compile_lowered_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    lowered: LoweredProgram<'gc>,
    options: CompilationOptions,
    destination: &LoadArtifact,
) -> Result<(), Value<'gc>> {
    compile_to_destination(ctx, destination, move || {
        compile_lowered_to_fasl_bytes(ctx, lowered, options)
    })
}

fn compile_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    destination: &LoadArtifact,
    compile_fasl: impl FnOnce() -> Result<Vec<u8>, Value<'gc>>,
) -> Result<(), Value<'gc>> {
    match destination.kind {
        LoadArtifactKind::SharedObject => Err(make_io_error(
            ctx,
            "compile",
            Str::new(
                ctx,
                "Shared-object Scheme artifacts are no longer produced by the compiler",
                true,
            )
            .into(),
            &[],
        )),
        LoadArtifactKind::FaslCode => {
            let _lock = ArtifactLock::acquire(&destination.path).map_err(|err| {
                make_io_error(
                    ctx,
                    "compile",
                    Str::new(
                        ctx,
                        format!(
                            "Cannot lock FASL code image '{}': {err}",
                            destination.path.display()
                        ),
                        true,
                    )
                    .into(),
                    &[],
                )
            })?;
            let bytes = compile_fasl()?;
            write_artifact_atomically(&destination.path, &bytes).map_err(|err| {
                make_io_error(
                    ctx,
                    "compile",
                    Str::new(
                        ctx,
                        format!(
                            "Cannot write FASL code image '{}': {err}",
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
}

struct ArtifactLock {
    #[allow(dead_code)]
    file: File,
}

impl ArtifactLock {
    fn acquire(path: &Path) -> std::io::Result<Self> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let lock_path = path.with_extension(format!(
            "{}.lock",
            path.extension()
                .and_then(|extension| extension.to_str())
                .unwrap_or("artifact")
        ));
        let file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(lock_path)?;
        lock_file(&file)?;
        Ok(Self { file })
    }
}

impl Drop for ArtifactLock {
    fn drop(&mut self) {
        let _ = unlock_file(&self.file);
    }
}

#[cfg(unix)]
fn lock_file(file: &File) -> std::io::Result<()> {
    if unsafe { retry_eintr(|| libc::flock(file.as_raw_fd(), libc::LOCK_EX)) } == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

#[cfg(unix)]
fn unlock_file(file: &File) -> std::io::Result<()> {
    if unsafe { libc::flock(file.as_raw_fd(), libc::LOCK_UN) } == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

#[cfg(not(unix))]
fn lock_file(_: &File) -> std::io::Result<()> {
    Ok(())
}

#[cfg(not(unix))]
fn unlock_file(_: &File) -> std::io::Result<()> {
    Ok(())
}

fn write_artifact_atomically(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("artifact");
    let unique = TEMP_ARTIFACT_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_path = path.with_file_name(format!(
        ".{file_name}.tmp.{}.{}",
        std::process::id(),
        unique
    ));

    let result = (|| {
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temp_path)?;
        file.write_all(bytes)?;
        file.flush()?;
        file.sync_all()?;
        drop(file);
        std::fs::rename(&temp_path, path)
    })();

    if result.is_err() {
        let _ = std::fs::remove_file(&temp_path);
    }

    result
}

fn load_artifact<'gc>(
    ctx: MutationContext<'gc>,
    libs: &crate::runtime::vm::libraries::LibraryCollection<'gc>,
    artifact: &LoadArtifact,
) -> Result<Value<'gc>, Value<'gc>> {
    libs.load(artifact, ctx).map_err(|err| {
        make_io_error(
            ctx,
            "load",
            Str::new(
                ctx,
                format!("Failed to load {}: {err}", artifact.path.display()),
                true,
            )
            .into(),
            &[],
        )
    })
}

pub(crate) fn destination_artifact_for_current_policy(path: impl AsRef<Path>) -> LoadArtifact {
    let kind = artifact_kind_for_policy(get_execution_policy());
    let path = path.as_ref().with_extension(artifact_extension(kind));
    LoadArtifact::new(kind, path)
}

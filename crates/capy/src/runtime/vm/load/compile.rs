use crate::runtime::sched::preempt::retry_eintr;
use std::fs::File;
use std::io::Write;
#[cfg(unix)]
use std::os::fd::AsRawFd;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::compiler::{
    CompilationOptions, DumpArtifactsOptions, LoweredProgram, begin_compilation_artifact,
    compile_file, compile_lowered_to_fasl_bytes, dump_lowered_program_artifacts,
    merge_compile_dump_options, resolve_artifact_dump_path,
};
use crate::runtime::MutationContext;
use crate::runtime::compile_service::{self, Failure, JobInput};
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
    pub(crate) destination: LoadArtifact,
    pub(crate) options: CompilationOptions,
    pub(crate) dump_options: DumpArtifactsOptions,
    pub(crate) preprocessed: bool,
    /// See [`SourceJob::unsaved`].
    pub(crate) unsaved: Option<Unsaved>,
}

/// Inputs of a compile of one source file with the Rust expander.
#[derive(Clone)]
pub(crate) struct SourceJob {
    pub(crate) source: PathBuf,
    pub(crate) destination: LoadArtifact,
    /// Set for an automatic compile. When the cache cannot take the image, the image is left
    /// here and the caller loads it from memory instead of failing.
    pub(crate) unsaved: Option<Unsaved>,
}

/// The image of a compile whose cache write failed, see [`SourceJob::unsaved`].
pub(crate) type Unsaved = Arc<Mutex<Option<Vec<u8>>>>;

/// Compile one source file on a compile thread. The caller loads the result.
pub(crate) fn run_source_job<'gc>(
    ctx: MutationContext<'gc>,
    job: SourceJob,
) -> Result<(), Failure<'gc>> {
    begin_compilation_artifact(&job.destination.path);
    let dump_options = merge_compile_dump_options(DumpArtifactsOptions::default());
    let lowered = compile_file(ctx, &job.source, dump_options.dump_graph)?;
    dump_lowered_program_artifacts(ctx, &job.destination.path, &lowered, dump_options);
    let mut options = CompilationOptions::default();
    if dump_options.dump_bytecode {
        options.backend_dumps.bytecode =
            Some(resolve_artifact_dump_path(&job.destination.path, ".bc.txt"));
    }
    compile_lowered_to_destination(
        ctx,
        lowered,
        options,
        &job.destination,
        job.unsaved.as_ref(),
    )?;
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
    let _phase = CompilationPhase::new(ctx);
    let unsaved: Unsaved = Arc::new(Mutex::new(None));
    let job = SourceJob {
        source: source_path,
        destination: build_destination.clone(),
        unsaved: Some(unsaved.clone()),
    };
    let (ctx, outcome) = compile_service::run(ctx, JobInput::Source(job));
    compile_service::raise_failure(ctx, outcome.unwrap_or_else(|killed| killed.unwind()))?;

    let libs = LIBRARY_COLLECTION.fetch(ctx);
    match take_unsaved(&unsaved) {
        Some(bytes) => load_unsaved(ctx, &bytes, &build_destination.path),
        None => load_artifact(ctx, libs, &build_destination),
    }
}

/// The image a compile left in memory, if the cache could not take it.
pub(crate) fn take_unsaved(unsaved: &Unsaved) -> Option<Vec<u8>> {
    unsaved.lock().unwrap_or_else(|e| e.into_inner()).take()
}

/// Loads an image that is in memory, see [`SourceJob::unsaved`]
pub(crate) fn load_unsaved<'gc>(
    ctx: MutationContext<'gc>,
    bytes: &[u8],
    path: &Path,
) -> Result<Value<'gc>, Value<'gc>> {
    LIBRARY_COLLECTION
        .fetch(ctx)
        .load_image(bytes, ctx)
        .map_err(|err| {
            make_io_error(
                ctx,
                "load",
                Str::new(
                    ctx,
                    format!("Failed to load {}: {err}", path.display()),
                    true,
                )
                .into(),
                &[],
            )
        })
}

pub(super) fn compile_lowered_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    lowered: LoweredProgram<'gc>,
    options: CompilationOptions,
    destination: &LoadArtifact,
    unsaved: Option<&Unsaved>,
) -> Result<(), Value<'gc>> {
    compile_to_destination(ctx, destination, unsaved, move || {
        compile_lowered_to_fasl_bytes(ctx, lowered, options)
    })
}

/// Whether `path` names an artifact of the compile cache, the only artifacts that get a lock.
fn is_cache_artifact<'gc>(ctx: MutationContext<'gc>, path: &Path) -> bool {
    let cache = ctx
        .globals()
        .loc_compile_fallback_path()
        .get()
        .downcast::<Str>()
        .to_string();
    !cache.is_empty() && path.starts_with(&cache)
}

fn compile_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    destination: &LoadArtifact,
    unsaved: Option<&Unsaved>,
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
            let cache = is_cache_artifact(ctx, &destination.path);
            let lock = if cache {
                ArtifactLock::acquire(&destination.path).map(Some)
            } else {
                Ok(None)
            };
            let bytes = compile_fasl()?;
            let stored =
                lock.and_then(|_lock| write_artifact_atomically(&destination.path, &bytes));
            match (stored, unsaved) {
                (Ok(()), _) => Ok(()),
                (Err(err), Some(unsaved)) if cache => {
                    log::debug!(
                        "Cannot cache {}, loading it from memory: {err}",
                        destination.path.display()
                    );
                    *unsaved.lock().unwrap_or_else(|e| e.into_inner()) = Some(bytes);
                    Ok(())
                }
                (Err(err), _) => Err(make_io_error(
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
                )),
            }
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

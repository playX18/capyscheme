use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use crate::heap::Gc;
use crate::list;
use crate::runtime::value::{Str, Value};
use crate::runtime::variable::{Variable, define};
use crate::runtime::vm::thunks::make_io_error;
use crate::{global, runtime::MutationContext};

use super::{
    artifact::{
        LoadArtifact, LoadArtifactKind, artifact_extension, artifact_kind_for_path,
        artifact_kind_for_policy, is_fasl_file,
    },
    policy::get_execution_policy,
};

global!(
    pub loc_load_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-path", Value::null());
    pub loc_load_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-extensions", list!(ctx, Str::new(ctx, "scm", true)));
    pub loc_load_compiled_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-path", Value::null());
    pub loc_load_compiled_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-extensions", Value::null());
    pub loc_native_extension<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%native-extension", Str::new(ctx, if cfg!(target_vendor="apple") {
        "dylib"
    } else {
        "so"
    }, true));
    pub loc_compile_fallback_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%compile-fallback-path", Value::null());
    pub loc_capy_root<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%capy-root", Str::new(ctx, "", true));
    pub loc_fresh_auto_compile<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%fresh-auto-compile", Value::new(false));
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum ResolvedLoadPath {
    Artifact {
        artifact: LoadArtifact,
        full_path: PathBuf,
    },
    Source {
        source_path: PathBuf,
        full_source_path: PathBuf,
        compiled_artifact: Option<LoadArtifact>,
        build_destination: LoadArtifact,
    },
}

impl ResolvedLoadPath {
    fn into_public(self) -> (PathBuf, PathBuf, Option<PathBuf>) {
        match self {
            Self::Artifact {
                artifact,
                full_path,
            } => (artifact.path.clone(), full_path, Some(artifact.path)),
            Self::Source {
                source_path,
                full_source_path,
                compiled_artifact,
                build_destination,
            } => (
                source_path,
                full_source_path,
                Some(compiled_artifact.unwrap_or(build_destination).path),
            ),
        }
    }
}

/// Cons each `:`-separated segment of `raw` onto the front of `tail`,
/// keeping their order.
fn prepend_segments<'gc>(ctx: MutationContext<'gc>, raw: &str, tail: Value<'gc>) -> Value<'gc> {
    let segments: Vec<Value<'gc>> = raw
        .split(':')
        .map(|segment| {
            let mut segment = segment;
            while let Some(rest) = segment.strip_prefix("./") {
                segment = rest;
            }
            Str::new(ctx, segment, true).into()
        })
        .collect();
    segments
        .into_iter()
        .rev()
        .fold(tail, |list, entry| Value::cons(ctx, entry, list))
}

pub fn init_load_path<'gc>(ctx: MutationContext<'gc>) {
    let fallback_dir = format!("capy/cache/{}", env!("CARGO_PKG_VERSION"),);

    let cache_dir = if let Some(env) = crate::utils::flags::xdg_cache_home() {
        format!("{env}/{fallback_dir}")
    } else if let Some(home) = crate::utils::flags::home_dir() {
        format!("{home}/.cache/{fallback_dir}")
    } else if let Some(appdata) = crate::utils::flags::local_app_data() {
        format!("{appdata}/.cache/{fallback_dir}")
    } else if let Some(appdata) = crate::utils::flags::app_data() {
        format!("{appdata}/.cache/{fallback_dir}")
    } else {
        fallback_dir.clone()
    };

    // Bytecode does not depend on the GC plan or its barrier, so one cache
    // and one set of installed artifacts serve every plan.
    let compile_fallback_path = Path::new(&cache_dir);
    ctx.globals().loc_compile_fallback_path().set(
        ctx,
        Str::new(ctx, compile_fallback_path.to_string_lossy(), true).into(),
    );

    let mut path = Value::null();
    let mut compiled_path = Value::null();

    let sysroot_dir: Option<PathBuf> = if cfg!(feature = "portable") {
        std::env::current_exe().ok().and_then(|exe| {
            let exe = std::fs::read_link(&exe).unwrap_or(exe);
            exe.parent().map(|parent| parent.to_owned())
        })
    } else {
        Some(PathBuf::from(env!("CAPY_SYSROOT")))
    };

    if let Some(sysroot_dir) = sysroot_dir.as_ref() {
        if cfg!(feature = "portable") {
            let stdlib_dir = sysroot_dir.join("lib");
            path = Value::cons(
                ctx,
                Str::new(ctx, stdlib_dir.to_string_lossy(), true).into(),
                path,
            );
            let compiled_dir = sysroot_dir.join("compiled");
            compiled_path = Value::cons(
                ctx,
                Str::new(ctx, compiled_dir.to_string_lossy(), true).into(),
                compiled_path,
            );
        } else {
            let stdlib_dir = sysroot_dir.join("share").join("capy").join("lib");
            path = Value::cons(
                ctx,
                Str::new(ctx, stdlib_dir.to_string_lossy(), true).into(),
                path,
            );
            let compiled_dir = sysroot_dir.join("lib").join("capy").join("compiled");
            compiled_path = Value::cons(
                ctx,
                Str::new(ctx, compiled_dir.to_string_lossy(), true).into(),
                compiled_path,
            );
        }
    }

    if let Some(load_path) = crate::utils::flags::load_path() {
        path = prepend_segments(ctx, load_path, path);
    }

    if let Some(load_compiled_path) = crate::utils::flags::load_compiled_path() {
        compiled_path = prepend_segments(ctx, load_compiled_path, compiled_path);
    }

    ctx.globals().loc_load_path().set(ctx, path);
    ctx.globals()
        .loc_load_compiled_path()
        .set(ctx, compiled_path);

    let fallback_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
    let root = sysroot_dir.as_deref().unwrap_or(&fallback_root);
    ctx.globals()
        .loc_capy_root()
        .set(ctx, Str::new(ctx, root.to_string_lossy(), true).into());
}

pub fn find_path_to<'gc>(
    ctx: MutationContext<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Option<(PathBuf, PathBuf, Option<PathBuf>)>, Value<'gc>> {
    resolve_load_path(ctx, filename, in_vicinity, resolve_relative, arch)
        .map(|resolved| resolved.map(ResolvedLoadPath::into_public))
}

pub(super) fn resolve_load_path<'gc>(
    ctx: MutationContext<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Option<ResolvedLoadPath>, Value<'gc>> {
    let filename = filename.as_ref();
    let vicinity = in_vicinity.map(|path| path.as_ref().to_owned());

    let named = find_existing_candidate(
        ctx,
        filename,
        &[filename.to_owned()],
        vicinity.as_deref(),
        resolve_relative,
    );
    let sniffed_fasl = named.as_deref().is_some_and(is_fasl_file);
    let kind =
        artifact_kind_for_path(filename).or(sniffed_fasl.then_some(LoadArtifactKind::FaslCode));
    if let Some(kind) = kind {
        let Some(artifact_path) = named else {
            return Ok(None);
        };
        let full_path = artifact_path.canonicalize().map_err(|err| {
            make_io_error(
                ctx,
                "find-path-to",
                Str::new(
                    ctx,
                    format!(
                        "Failed to canonicalize path {}: {err}",
                        artifact_path.display()
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;
        return Ok(Some(ResolvedLoadPath::Artifact {
            artifact: LoadArtifact::new(kind, artifact_path),
            full_path,
        }));
    }

    let candidates = source_candidates(ctx, filename);
    let source_path = find_existing_candidate(
        ctx,
        filename,
        &candidates,
        vicinity.as_deref(),
        resolve_relative,
    );

    let Some(source_path) = source_path else {
        if let Some(artifact) = find_compiled_candidate(ctx, filename, None, arch) {
            let full_path = artifact.path.canonicalize().map_err(|err| {
                make_io_error(
                    ctx,
                    "find-path-to",
                    Str::new(
                        ctx,
                        format!(
                            "Failed to canonicalize path {}: {err}",
                            artifact.path.display()
                        ),
                        true,
                    )
                    .into(),
                    &[],
                )
            })?;
            return Ok(Some(ResolvedLoadPath::Artifact {
                artifact,
                full_path,
            }));
        }
        return Ok(None);
    };

    let full_source_path = source_path.canonicalize().map_err(|err| {
        make_io_error(
            ctx,
            "find-path-to",
            Str::new(
                ctx,
                format!(
                    "Failed to canonicalize path {}: {err}",
                    source_path.display()
                ),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let compiled_artifact = freshest_compiled_candidate(ctx, filename, &full_source_path, arch);
    let build_destination = fallback_artifact(ctx, &full_source_path, arch);

    let source_path = match std::env::current_dir() {
        Ok(cwd) if source_path.is_absolute() => source_path
            .strip_prefix(&cwd)
            .map(Path::to_path_buf)
            .unwrap_or(source_path),
        _ => source_path,
    };

    Ok(Some(ResolvedLoadPath::Source {
        source_path,
        full_source_path,
        compiled_artifact,
        build_destination,
    }))
}

pub(super) fn fallback_file_name<'gc>(
    ctx: MutationContext<'gc>,
    path: impl AsRef<Path>,
) -> PathBuf {
    fallback_artifact(ctx, path, None).path
}

fn fallback_artifact<'gc>(
    ctx: MutationContext<'gc>,
    path: impl AsRef<Path>,
    arch: Option<&str>,
) -> LoadArtifact {
    let path = path.as_ref();
    let fallback = ctx
        .globals()
        .loc_compile_fallback_path()
        .get()
        .downcast::<Str>()
        .to_string();
    let fallback = Path::new(&fallback).join(target_arch(arch));
    let relative_source = path.strip_prefix("/").unwrap_or(path);
    let kind = artifact_kind_for_policy(get_execution_policy());
    let full_path = fallback
        .join(relative_source)
        .with_extension(artifact_extension(kind));

    LoadArtifact::new(kind, full_path)
}

pub fn compiled_is_fresh(
    full_filename: &Path,
    compiled_filename: &Path,
    time_source: SystemTime,
    time_compiled: SystemTime,
) -> bool {
    if time_source > time_compiled {
        log::info!(
            "Source file {} is newer than compiled file {}",
            full_filename.display(),
            compiled_filename.display()
        );
        return false;
    }

    true
}

fn source_candidates<'gc>(ctx: MutationContext<'gc>, filename: &Path) -> Vec<PathBuf> {
    let mut candidates = Vec::new();

    // Always try the exact path first so extensionless scripts (e.g. `./main`)
    // resolve. Extension-based lookups follow as fallbacks.
    candidates.push(filename.to_owned());

    let mut push_unique = |candidate: PathBuf| {
        if !candidates.contains(&candidate) {
            candidates.push(candidate);
        }
    };

    let mut extensions = ctx.globals().loc_load_extensions().get();
    if extensions.is_null() {
        push_unique(filename.with_extension("scm"));
    } else {
        while extensions.is_pair() {
            let extension = extensions.car().downcast::<Str>().to_string();
            push_unique(filename.with_extension(&extension));
            extensions = extensions.cdr();
        }
    }

    candidates
}

fn find_existing_candidate<'gc>(
    ctx: MutationContext<'gc>,
    filename: &Path,
    candidates: &[PathBuf],
    vicinity: Option<&Path>,
    resolve_relative: bool,
) -> Option<PathBuf> {
    if filename.is_absolute() {
        return candidates
            .iter()
            .find(|candidate| is_regular_file(candidate))
            .cloned();
    }

    if filename.is_relative()
        && resolve_relative
        && let Some(candidate) = candidates
            .iter()
            .find(|candidate| is_regular_file(candidate))
    {
        return Some(candidate.clone());
    }

    if let Some(vicinity) = vicinity
        && let Some(candidate) = candidates
            .iter()
            .map(|candidate| vicinity.join(candidate))
            .find(|candidate| is_regular_file(candidate))
    {
        return Some(candidate);
    }

    let mut load_path = ctx.globals().loc_load_path().get();
    while load_path.is_pair() {
        let next = load_path.cdr();
        let base = PathBuf::from(load_path.car().downcast::<Str>().to_string());
        for candidate in candidates {
            let full = base.join(candidate);
            if is_regular_file(&full) {
                return Some(full);
            }
        }
        load_path = next;
    }

    None
}

fn freshest_compiled_candidate<'gc>(
    ctx: MutationContext<'gc>,
    filename: &Path,
    full_source_path: &Path,
    arch: Option<&str>,
) -> Option<LoadArtifact> {
    let source_time = full_source_path
        .metadata()
        .ok()
        .and_then(|metadata| metadata.modified().ok())?;
    find_compiled_candidate(ctx, filename, Some((full_source_path, source_time)), arch)
}

fn find_compiled_candidate<'gc>(
    ctx: MutationContext<'gc>,
    filename: &Path,
    source: Option<(&Path, SystemTime)>,
    arch: Option<&str>,
) -> Option<LoadArtifact> {
    let kind = artifact_kind_for_policy(get_execution_policy());
    let explicit_arch = arch.is_some();
    let arch = target_arch(arch);
    let mut relative_candidates = Vec::new();
    for candidate in source_candidates(ctx, filename) {
        let artifact = candidate.with_extension(artifact_extension(kind));
        if !relative_candidates.contains(&artifact) {
            relative_candidates.push(artifact);
        }
    }

    let mut compiled_path = ctx.globals().loc_load_compiled_path().get();
    while compiled_path.is_pair() {
        let next = compiled_path.cdr();
        let dir = PathBuf::from(compiled_path.car().downcast::<Str>().to_string());
        let mut candidates = Vec::new();
        for relative_candidate in &relative_candidates {
            candidates.push(dir.join(arch).join(relative_candidate));
            if !explicit_arch {
                candidates.push(dir.join(relative_candidate));
            }
        }

        for candidate in candidates {
            if !is_regular_file(&candidate) {
                continue;
            }
            if let Some((full_source_path, source_time)) = source {
                let Some(compiled_time) = candidate
                    .metadata()
                    .ok()
                    .and_then(|metadata| metadata.modified().ok())
                else {
                    continue;
                };
                if !compiled_is_fresh(full_source_path, &candidate, source_time, compiled_time) {
                    continue;
                }
            }
            return Some(LoadArtifact::new(kind, candidate));
        }

        compiled_path = next;
    }

    None
}

fn target_arch(arch: Option<&str>) -> &str {
    arch.unwrap_or(std::env::consts::ARCH)
}

fn is_regular_file(path: &Path) -> bool {
    path.exists()
        && path
            .metadata()
            .ok()
            .is_some_and(|metadata| metadata.is_file())
}

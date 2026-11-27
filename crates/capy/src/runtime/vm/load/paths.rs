use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use crate::heap::Gc;
use crate::list;
use crate::runtime::objectmodel::{Str, Value};
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

/// The name of the library map the toolchain installs next to its compiled files.
const LIBMAP_FILE_NAME: &str = "capy.libmap";

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
    let fallback_dir = format!("capy/cache/{}", env!("CARGO_PKG_VERSION"));

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
    let mut default_libmap: Option<PathBuf> = None;
    let mut sources = Vec::new();

    // The toolchain directory: `<package>/{src,fasl}` for every package of the toolchain and the
    // `capy.libmap` that maps their libraries. A portable build keeps it next to the executable.
    // The other builds are installed as `<prefix>/bin/capy-rt` with the toolchain in
    // `<prefix>/lib/capy/<version>`: the prefix is the one the executable is in, so an installed
    // tree can be moved, and `CAPY_SYSROOT` of the build is used only when the executable is
    // not in such a tree.
    let exe_dir: Option<PathBuf> = std::env::current_exe().ok().and_then(|exe| {
        let exe = std::fs::canonicalize(&exe).unwrap_or(exe);
        exe.parent().map(|parent| parent.to_owned())
    });
    let sysroot_dir: Option<PathBuf> = if cfg!(feature = "portable") {
        exe_dir
    } else {
        let installed = exe_dir
            .as_deref()
            .and_then(Path::parent)
            .filter(|prefix| {
                prefix
                    .join("lib")
                    .join("capy")
                    .join(env!("CARGO_PKG_VERSION"))
                    .join(LIBMAP_FILE_NAME)
                    .is_file()
            })
            .map(Path::to_path_buf);
        installed.or_else(|| Some(PathBuf::from(env!("CAPY_SYSROOT"))))
    };

    if let Some(sysroot_dir) = sysroot_dir.as_ref() {
        let toolchain_dir = if cfg!(feature = "portable") {
            sysroot_dir.clone()
        } else {
            sysroot_dir
                .join("lib")
                .join("capy")
                .join(env!("CARGO_PKG_VERSION"))
        };
        default_libmap = Some(toolchain_dir.join(LIBMAP_FILE_NAME));
        let mut compiled = Vec::new();
        for package in toolchain_packages(&toolchain_dir) {
            let src = toolchain_dir.join(&package).join("src");
            if src.is_dir() {
                sources.push(src);
            }
            let fasl = toolchain_dir.join(&package).join("fasl");
            if fasl.is_dir() {
                compiled.push(fasl);
            }
        }
        for dir in compiled.iter().rev() {
            compiled_path = Value::cons(
                ctx,
                Str::new(ctx, dir.to_string_lossy(), true).into(),
                compiled_path,
            );
        }
    }

    for dir in sources.iter().rev() {
        path = Value::cons(ctx, Str::new(ctx, dir.to_string_lossy(), true).into(), path);
    }

    // The library map: the one named by `--libmap`/`CAPY_LIBMAP` (it has to exist), else the
    // toolchain's own next to its compiled files, when there is one.
    if let Some(libmap) =
        crate::utils::flags::libmap().filter(|libmap| !libmap.as_os_str().is_empty())
    {
        ctx.globals()
            .loc_libmap_path()
            .set(ctx, Str::new(ctx, libmap.to_string_lossy(), true).into());
        ctx.globals()
            .loc_libmap_explicit()
            .set(ctx, Value::new(true));
    } else if let Some(libmap) = default_libmap.filter(|libmap| libmap.is_file()) {
        ctx.globals()
            .loc_libmap_path()
            .set(ctx, Str::new(ctx, libmap.to_string_lossy(), true).into());
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

/// The names of the packages in the toolchain directory `dir`: its subdirectories that have
/// a `src` or a `fasl` directory, `core` and `std` first, the rest sorted by name.
fn toolchain_packages(dir: &Path) -> Vec<String> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let mut packages: Vec<String> = entries
        .filter_map(Result::ok)
        .filter(|entry| entry.path().join("src").is_dir() || entry.path().join("fasl").is_dir())
        .filter_map(|entry| entry.file_name().into_string().ok())
        .collect();
    packages.sort_by_key(|name| {
        let rank = match name.as_str() {
            "core" => 0,
            "std" => 1,
            _ => 2,
        };
        (rank, name.clone())
    });
    packages
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
    let full_path = append_extension(&fallback.join(relative_source), artifact_extension(kind));

    LoadArtifact::new(kind, full_path)
}

/// `path` with `.extension` appended to its file name: `foo.scm` becomes `foo.scm.fasl`, so
/// the artifacts of `foo.scm` and `foo.sls` do not collide, as they do under
/// `with_extension`.
pub(super) fn append_extension(path: &Path, extension: &str) -> PathBuf {
    let mut name = path.as_os_str().to_owned();
    name.push(".");
    name.push(extension);
    PathBuf::from(name)
}

/// Whether the source hash a compiled file records agrees with the source file's current bytes.
///
/// The modification time says only that the source is not newer; a source restored from a
/// backup, or copied with its time kept (`cp -p`), is older than the artifact and different. The
/// hash is in the library decls of the artifact's directory header, which is read without running
/// anything. An artifact with no library decl, or no readable directory, says nothing, and
/// counts as matching, so the time test alone decides for it.
pub fn compiled_matches_source<'gc>(
    ctx: MutationContext<'gc>,
    source: &Path,
    compiled: &Path,
) -> bool {
    use crate::runtime::fasl::Reader;
    use crate::runtime::objectmodel::{Str, Symbol, Value, Vector};
    let Ok(file) = std::fs::File::open(compiled) else {
        return true;
    };
    let Ok(Some(mut entries)) = Reader::new(ctx, file).read_directory() else {
        return true;
    };
    let mut recorded = None;
    while entries.is_pair() {
        let entry = entries.car();
        entries = entries.cdr();
        // an entry is `(kind name ... meta)`, a list of four with the library vector last
        let mut meta = entry;
        for _ in 0..3 {
            if !meta.is_pair() {
                break;
            }
            meta = meta.cdr();
        }
        if !meta.is_pair() {
            continue;
        }
        let meta = meta.car();
        if !meta.is::<Vector>() {
            continue;
        }
        let vector = meta.downcast::<Vector>();
        let head = if vector.len() > 6 {
            vector[0].get()
        } else {
            Value::new(false)
        };
        if head.is::<Symbol>() && head.downcast::<Symbol>().to_string() == "#%library" {
            let hash = vector[6].get();
            if hash.is::<Str>() {
                recorded = Some(hash.downcast::<Str>().to_string());
                break;
            }
        }
    }
    let Some(recorded) = recorded else {
        return true;
    };
    match std::fs::read(source) {
        Ok(bytes) => super::hash::hash_bytes(&bytes) == recorded,
        Err(_) => true,
    }
}

/// Whether the auto-compile cache file written at `compiled_time` was written by this runtime
/// or a later one. A cache file older than the runtime's executable was compiled by an earlier
/// build of the runtime, whose expanders and compiler may make different code or meta (the
/// knowns of a boot file, say), so it is compiled again. A rebuilt stage of the bootstrap has
/// a new executable, an installed runtime keeps its own.
pub fn compiled_after_runtime(compiled_time: SystemTime) -> bool {
    static RUNTIME_TIME: std::sync::OnceLock<Option<SystemTime>> = std::sync::OnceLock::new();
    let runtime_time = RUNTIME_TIME.get_or_init(|| {
        std::env::current_exe()
            .ok()
            .and_then(|exe| exe.metadata().ok())
            .and_then(|metadata| metadata.modified().ok())
    });
    match runtime_time {
        Some(runtime_time) => compiled_time >= *runtime_time,
        None => true,
    }
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
    compiled_path_candidates(ctx, filename, arch)
        .into_iter()
        .find(|artifact| {
            let Some((full_source_path, source_time)) = source else {
                return true;
            };
            let Some(compiled_time) = artifact
                .path
                .metadata()
                .ok()
                .and_then(|metadata| metadata.modified().ok())
            else {
                return false;
            };
            compiled_is_fresh(full_source_path, &artifact.path, source_time, compiled_time)
                && compiled_matches_source(ctx, full_source_path, &artifact.path)
        })
}

/// The compiled artifacts of `filename` that exist in the compiled load path, in the order
/// they are preferred. Nothing is said about whether they are fresh. A source `foo.sls` is
/// looked up as `foo.sls.fasl` before the older name `foo.fasl`.
fn compiled_path_candidates<'gc>(
    ctx: MutationContext<'gc>,
    filename: &Path,
    arch: Option<&str>,
) -> Vec<LoadArtifact> {
    let kind = artifact_kind_for_policy(get_execution_policy());
    let explicit_arch = arch.is_some();
    let arch = target_arch(arch);
    let extension = artifact_extension(kind);
    let mut relative_candidates = Vec::new();
    for candidate in source_candidates(ctx, filename) {
        for artifact in [
            append_extension(&candidate, extension),
            candidate.with_extension(extension),
        ] {
            if !relative_candidates.contains(&artifact) {
                relative_candidates.push(artifact);
            }
        }
    }

    let mut found = Vec::new();
    let mut compiled_path = ctx.globals().loc_load_compiled_path().get();
    while compiled_path.is_pair() {
        let next = compiled_path.cdr();
        let dir = PathBuf::from(compiled_path.car().downcast::<Str>().to_string());
        for relative_candidate in &relative_candidates {
            let mut candidates = vec![dir.join(arch).join(relative_candidate)];
            if !explicit_arch {
                candidates.push(dir.join(relative_candidate));
            }
            for candidate in candidates {
                if is_regular_file(&candidate) && !found.contains(&candidate) {
                    found.push(candidate);
                }
            }
        }
        compiled_path = next;
    }

    found
        .into_iter()
        .map(|path| LoadArtifact::new(kind, path))
        .collect()
}

/// Every compiled artifact that may hold the compiled form of the source file
/// `filename`, in the order they are preferred: the ones in the compiled load path, then the
/// one in the compile cache. Whether one is fresh is for the caller to decide.
pub(super) fn compiled_candidates<'gc>(
    ctx: MutationContext<'gc>,
    filename: &Path,
    arch: Option<&str>,
    build_destination: &LoadArtifact,
    use_cache: bool,
) -> Vec<PathBuf> {
    let mut found: Vec<PathBuf> = compiled_path_candidates(ctx, filename, arch)
        .into_iter()
        .map(|artifact| artifact.path)
        .collect();
    if use_cache && is_regular_file(&build_destination.path) {
        found.push(build_destination.path.clone());
    }
    found
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

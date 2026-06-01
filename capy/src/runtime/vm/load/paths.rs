use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use crate::list;
use crate::rsgc::Gc;
use crate::runtime::modules::{Variable, define};
use crate::runtime::value::{Str, Value};
use crate::runtime::vm::thunks::make_io_error;
use crate::{global, runtime::Context};
use mmtk::util::options::PlanSelector;

use super::{
    artifact::{
        LoadArtifact, artifact_extension, artifact_kind_for_path, artifact_kind_for_policy,
    },
    policy::get_execution_policy,
};

global!(
    pub loc_load_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-path", Value::null());
    pub loc_load_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-extensions", list!(ctx, Str::new(*ctx, "scm", true)));
    pub loc_load_compiled_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-path", Value::null());
    pub loc_load_compiled_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-extensions", Value::null());
    pub loc_native_extension<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%native-extension", Str::new(*ctx, if cfg!(target_vendor="apple") {
        "dylib"
    } else {
        "so"
    }, true));
    pub loc_compile_fallback_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%compile-fallback-path", Value::null());
    pub loc_capy_root<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%capy-root", Str::new(*ctx, "", true));
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

pub fn init_load_path<'gc>(ctx: Context<'gc>) {
    const FALLBACK_DIR: &str = concat!("capy/cache/", env!("CARGO_PKG_VERSION"));

    let cache_dir = if let Ok(env) = std::env::var("XDG_CACHE_HOME") {
        format!("{env}/{FALLBACK_DIR}")
    } else if let Ok(home) = std::env::var("HOME") {
        format!("{home}/.cache/{FALLBACK_DIR}")
    } else if let Ok(appdata) = std::env::var("LOCALAPPDATA") {
        format!("{appdata}/.cache/{FALLBACK_DIR}")
    } else if let Ok(appdata) = std::env::var("APPDATA") {
        format!("{appdata}/.cache/{FALLBACK_DIR}")
    } else {
        FALLBACK_DIR.to_string()
    };

    let plan = match *crate::GarbageCollector::get().mmtk.get_options().plan {
        PlanSelector::Immix | PlanSelector::MarkSweep => "regular",
        PlanSelector::ConcurrentImmix => "conc",
        PlanSelector::StickyImmix => "gen",
        _ => unreachable!("GC plan validated at startup"),
    };

    let compile_fallback_path = Path::new(&cache_dir).join(plan);
    if !compile_fallback_path.exists() {
        std::fs::create_dir_all(&compile_fallback_path).expect("Failed to create cache directory");
    }
    ctx.globals().loc_compile_fallback_path().set(
        ctx,
        Str::new(*ctx, compile_fallback_path.to_string_lossy(), true).into(),
    );

    let mut path = Value::null();
    let mut compiled_path = Value::null();

    let sysroot_dir: Option<PathBuf> = if cfg!(feature = "portable") {
        let exe = std::env::current_exe().expect("Failed to get current exe path");
        let exe = std::fs::read_link(&exe).unwrap_or(exe);
        exe.parent().map(|parent| parent.to_owned())
    } else {
        Some(PathBuf::from(env!("CAPY_SYSROOT")))
    };

    if let Some(sysroot_dir) = sysroot_dir.as_ref() {
        if cfg!(feature = "portable") {
            let stdlib_dir = sysroot_dir.join("lib");
            path = Value::cons(
                ctx,
                Str::new(*ctx, &stdlib_dir.to_string_lossy(), true).into(),
                path,
            );
            let compiled_dir = sysroot_dir.join("compiled");
            compiled_path = Value::cons(
                ctx,
                Str::new(*ctx, &compiled_dir.to_string_lossy(), true).into(),
                compiled_path,
            );
        } else {
            let stdlib_dir = sysroot_dir.join("share").join("capy").join("lib");
            path = Value::cons(
                ctx,
                Str::new(*ctx, &stdlib_dir.to_string_lossy(), true).into(),
                path,
            );
            let compiled_dir = sysroot_dir.join("lib").join("capy").join("compiled");
            compiled_path = Value::cons(
                ctx,
                Str::new(*ctx, &compiled_dir.to_string_lossy(), true).into(),
                compiled_path,
            );
        }
    }

    if let Ok(load_path) = std::env::var("CAPY_LOAD_PATH") {
        let paths = load_path
            .split(':')
            .map(|segment| Str::new(*ctx, segment, true).into());
        let mut prefix = Value::null();
        for entry in paths.rev() {
            prefix = Value::cons(ctx, entry, prefix);
        }
        path = prefix.append(ctx, path);
    }

    if let Ok(load_compiled_path) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        let paths = load_compiled_path
            .split(':')
            .map(|segment| Str::new(*ctx, segment, true).into());
        let mut prefix = compiled_path;
        for entry in paths.rev() {
            prefix = Value::cons(ctx, entry, prefix);
        }
        compiled_path = prefix;
    }

    ctx.globals().loc_load_path().set(ctx, path);
    ctx.globals()
        .loc_load_compiled_path()
        .set(ctx, compiled_path);

    let fallback_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
    let root = sysroot_dir.as_deref().unwrap_or(&fallback_root);
    ctx.globals()
        .loc_capy_root()
        .set(ctx, Str::new(*ctx, root.to_string_lossy(), true).into());
}

pub fn find_path_to<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Option<(PathBuf, PathBuf, Option<PathBuf>)>, Value<'gc>> {
    resolve_load_path(ctx, filename, in_vicinity, resolve_relative, arch)
        .map(|resolved| resolved.map(ResolvedLoadPath::into_public))
}

pub(super) fn resolve_load_path<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Option<ResolvedLoadPath>, Value<'gc>> {
    let filename = filename.as_ref();
    let vicinity = in_vicinity.map(|path| path.as_ref().to_owned());

    if let Some(kind) = artifact_kind_for_path(filename) {
        let candidates = vec![filename.to_owned()];
        let Some(artifact_path) = find_existing_candidate(
            ctx,
            filename,
            &candidates,
            vicinity.as_deref(),
            resolve_relative,
        ) else {
            return Ok(None);
        };
        let full_path = artifact_path.canonicalize().map_err(|err| {
            make_io_error(
                ctx,
                "find-path-to",
                Str::new(
                    *ctx,
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
        return Ok(None);
    };

    let full_source_path = source_path.canonicalize().map_err(|err| {
        make_io_error(
            ctx,
            "find-path-to",
            Str::new(
                *ctx,
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

    Ok(Some(ResolvedLoadPath::Source {
        source_path,
        full_source_path,
        compiled_artifact,
        build_destination,
    }))
}

pub(super) fn fallback_file_name<'gc>(ctx: Context<'gc>, path: impl AsRef<Path>) -> PathBuf {
    fallback_artifact(ctx, path, None).path
}

fn fallback_artifact<'gc>(
    ctx: Context<'gc>,
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

    if let Some(parent) = full_path.parent()
        && !parent.exists()
    {
        std::fs::create_dir_all(parent).expect("Failed to create fallback directory");
    }

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

fn source_candidates<'gc>(ctx: Context<'gc>, filename: &Path) -> Vec<PathBuf> {
    let mut candidates = Vec::new();

    if filename.extension().is_some() {
        candidates.push(filename.to_owned());
    }

    let mut extensions = ctx.globals().loc_load_extensions().get();
    if extensions.is_null() {
        candidates.push(filename.with_extension("scm"));
    } else {
        while extensions.is_pair() {
            let extension = extensions.car().downcast::<Str>().to_string();
            candidates.push(filename.with_extension(&extension));
            extensions = extensions.cdr();
        }
    }

    candidates
}

fn find_existing_candidate<'gc>(
    ctx: Context<'gc>,
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

    if filename.is_relative() && resolve_relative {
        if let Some(candidate) = candidates
            .iter()
            .find(|candidate| is_regular_file(candidate))
        {
            return Some(candidate.clone());
        }
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
    ctx: Context<'gc>,
    filename: &Path,
    full_source_path: &Path,
    arch: Option<&str>,
) -> Option<LoadArtifact> {
    let source_time = full_source_path
        .metadata()
        .ok()
        .and_then(|metadata| metadata.modified().ok())?;
    let kind = artifact_kind_for_policy(get_execution_policy());
    let explicit_arch = arch.is_some();
    let arch = target_arch(arch);

    let mut compiled_path = ctx.globals().loc_load_compiled_path().get();
    while compiled_path.is_pair() {
        let next = compiled_path.cdr();
        let dir = PathBuf::from(compiled_path.car().downcast::<Str>().to_string());
        let mut candidates = vec![
            dir.join(arch)
                .join(filename)
                .with_extension(artifact_extension(kind)),
        ];
        if !explicit_arch {
            candidates.push(dir.join(filename).with_extension(artifact_extension(kind)));
        }

        for candidate in candidates {
            if is_regular_file(&candidate)
                && let Some(compiled_time) = candidate
                    .metadata()
                    .ok()
                    .and_then(|metadata| metadata.modified().ok())
                && compiled_is_fresh(full_source_path, &candidate, source_time, compiled_time)
            {
                return Some(LoadArtifact::new(kind, candidate));
            }
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

#[cfg(test)]
mod tests {
    use super::super::artifact::FASL_CODE_EXTENSION;
    use super::*;
    use crate::runtime::vm::load::policy::{ExecutionPolicy, set_execution_policy};
    use crate::runtime::{Context, Scheme};
    use std::{fs, sync::Mutex, time::Duration};
    use uuid::Uuid;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    struct TempDir {
        path: PathBuf,
    }

    struct EnvGuard {
        vars: Vec<(&'static str, Option<String>)>,
    }

    impl EnvGuard {
        fn unset(vars: &[&'static str]) -> Self {
            let vars = vars
                .iter()
                .map(|var| {
                    let previous = std::env::var(var).ok();
                    // SAFETY: load-path tests serialize environment mutation with TEST_LOCK.
                    unsafe {
                        std::env::remove_var(var);
                    }
                    (*var, previous)
                })
                .collect();
            Self { vars }
        }

        fn set(var: &'static str, value: &str) -> Self {
            let previous = std::env::var(var).ok();
            // SAFETY: load-path tests serialize environment mutation with TEST_LOCK.
            unsafe {
                std::env::set_var(var, value);
            }
            Self {
                vars: vec![(var, previous)],
            }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            for (var, previous) in self.vars.drain(..) {
                // SAFETY: load-path tests serialize environment mutation with TEST_LOCK.
                unsafe {
                    if let Some(previous) = previous {
                        std::env::set_var(var, previous);
                    } else {
                        std::env::remove_var(var);
                    }
                }
            }
        }
    }

    struct CwdGuard {
        previous: PathBuf,
    }

    impl CwdGuard {
        fn enter(path: &Path) -> Self {
            let previous = std::env::current_dir().unwrap();
            std::env::set_current_dir(path).unwrap();
            Self { previous }
        }
    }

    impl Drop for CwdGuard {
        fn drop(&mut self) {
            let _ = std::env::set_current_dir(&self.previous);
        }
    }

    impl TempDir {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!("capy-load-test-{}", Uuid::new_v4()));
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                crate::runtime::init(ctx);
            }
            f(ctx);
        });
    }

    fn scm_list<'gc>(ctx: Context<'gc>, values: &[&str]) -> Value<'gc> {
        let mut list = Value::null();
        for value in values.iter().rev() {
            list = Value::cons(ctx, Str::new(*ctx, *value, true).into(), list);
        }
        list
    }

    fn with_policy<R>(policy: ExecutionPolicy, f: impl FnOnce() -> R) -> R {
        struct PolicyGuard(ExecutionPolicy);

        impl Drop for PolicyGuard {
            fn drop(&mut self) {
                set_execution_policy(self.0);
            }
        }

        let previous = get_execution_policy();
        set_execution_policy(policy);
        let _guard = PolicyGuard(previous);
        f()
    }

    #[test]
    fn init_load_path_sets_compile_fallback_without_cache_env() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let _env = EnvGuard::unset(&["XDG_CACHE_HOME", "HOME", "LOCALAPPDATA", "APPDATA"]);
        let _plan = EnvGuard::set("MMTK_PLAN", "StickyImmix");
        let temp = TempDir::new();
        let _cwd = CwdGuard::enter(temp.path());
        let scm = Scheme::new_uninit();

        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                crate::runtime::init(ctx);
            }
            ctx.globals()
                .loc_compile_fallback_path()
                .set(ctx, Value::null());

            init_load_path(ctx);

            let fallback = ctx.globals().loc_compile_fallback_path().get();
            assert!(fallback.is::<Str>());
        });
    }

    #[test]
    fn finds_source_by_vicinity_then_extension_then_load_path() {
        with_ctx(|ctx| {
            let temp = TempDir::new();
            let vicinity = temp.path().join("vicinity");
            let load_path = temp.path().join("load-path");
            fs::create_dir_all(&vicinity).unwrap();
            fs::create_dir_all(&load_path).unwrap();
            fs::write(vicinity.join("target.sld"), b"vicinity").unwrap();
            fs::write(load_path.join("target.scm"), b"load-path").unwrap();

            ctx.globals()
                .loc_load_extensions()
                .set(ctx, scm_list(ctx, &["sld", "scm"]));
            ctx.globals()
                .loc_load_path()
                .set(ctx, scm_list(ctx, &[load_path.to_string_lossy().as_ref()]));
            ctx.globals()
                .loc_load_compiled_path()
                .set(ctx, Value::null());

            let resolved = resolve_load_path(ctx, "target", Some(&vicinity), false, None)
                .unwrap()
                .unwrap();

            let ResolvedLoadPath::Source {
                source_path,
                full_source_path,
                compiled_artifact,
                ..
            } = resolved
            else {
                panic!("expected source resolution");
            };

            assert_eq!(source_path, vicinity.join("target.sld"));
            assert_eq!(
                full_source_path,
                vicinity.join("target.sld").canonicalize().unwrap()
            );
            assert_eq!(compiled_artifact, None);
        });
    }

    #[test]
    fn fallback_file_name_uses_arch_scoped_cache_path() {
        with_policy(ExecutionPolicy::AOT, || {
            with_ctx(|ctx| {
                let temp = TempDir::new();
                ctx.globals().loc_compile_fallback_path().set(
                    ctx,
                    Str::new(*ctx, temp.path().to_string_lossy(), true).into(),
                );

                let fallback = fallback_file_name(ctx, "/stdlib/boot.scm");

                assert_eq!(
                    fallback,
                    temp.path()
                        .join(std::env::consts::ARCH)
                        .join("stdlib/boot")
                        .with_extension(FASL_CODE_EXTENSION)
                );
            });
        });
    }

    #[test]
    fn explicit_arch_scopes_compiled_and_fallback_artifacts() {
        with_policy(ExecutionPolicy::AOT, || {
            with_ctx(|ctx| {
                let temp = TempDir::new();
                let source_dir = temp.path().join("src");
                let compiled_dir = temp.path().join("compiled");
                let fallback_dir = temp.path().join("fallback");
                fs::create_dir_all(&source_dir).unwrap();
                fs::create_dir_all(compiled_dir.join("riscv64")).unwrap();
                fs::create_dir_all(compiled_dir.join("x86_64")).unwrap();

                let source = source_dir.join("lib/test.scm");
                fs::create_dir_all(source.parent().unwrap()).unwrap();
                fs::write(&source, b"source").unwrap();

                let riscv_compiled = compiled_dir
                    .join("riscv64/lib/test")
                    .with_extension(FASL_CODE_EXTENSION);
                fs::create_dir_all(riscv_compiled.parent().unwrap()).unwrap();
                fs::write(&riscv_compiled, b"riscv").unwrap();
                std::thread::sleep(Duration::from_millis(20));
                fs::write(&source, b"newer than riscv").unwrap();

                let x86_compiled = compiled_dir
                    .join("x86_64/lib/test")
                    .with_extension(FASL_CODE_EXTENSION);
                fs::create_dir_all(x86_compiled.parent().unwrap()).unwrap();
                fs::write(&x86_compiled, b"x86").unwrap();

                ctx.globals()
                    .loc_load_extensions()
                    .set(ctx, scm_list(ctx, &["scm"]));
                ctx.globals()
                    .loc_load_path()
                    .set(ctx, scm_list(ctx, &[source_dir.to_string_lossy().as_ref()]));
                ctx.globals().loc_load_compiled_path().set(
                    ctx,
                    scm_list(ctx, &[compiled_dir.to_string_lossy().as_ref()]),
                );
                ctx.globals().loc_compile_fallback_path().set(
                    ctx,
                    Str::new(*ctx, fallback_dir.to_string_lossy(), true).into(),
                );

                let resolved =
                    resolve_load_path(ctx, Path::new("lib/test"), None::<&Path>, false, Some("x86_64"))
                        .unwrap()
                        .unwrap();
                let ResolvedLoadPath::Source {
                    compiled_artifact,
                    build_destination,
                    ..
                } = resolved
                else {
                    panic!("expected source resolution");
                };

                assert_eq!(
                    compiled_artifact.map(|artifact| artifact.path),
                    Some(x86_compiled)
                );
                assert_eq!(
                    build_destination.path,
                    fallback_dir
                        .join("x86_64")
                        .join(source.strip_prefix("/").unwrap_or(&source))
                        .with_extension(FASL_CODE_EXTENSION)
                );
            });
        });
    }

    #[test]
    fn ignores_stale_compiled_artifacts() {
        with_policy(ExecutionPolicy::AOT, || {
            with_ctx(|ctx| {
                let temp = TempDir::new();
                let source_dir = temp.path().join("src");
                let compiled_dir = temp.path().join("compiled");
                fs::create_dir_all(&source_dir).unwrap();
                fs::create_dir_all(compiled_dir.join("lib")).unwrap();

                let source = source_dir.join("lib/test.scm");
                fs::create_dir_all(source.parent().unwrap()).unwrap();
                fs::write(&source, b"old").unwrap();

                let compiled = compiled_dir
                    .join("lib/test")
                    .with_extension(FASL_CODE_EXTENSION);
                fs::write(&compiled, b"compiled").unwrap();
                std::thread::sleep(Duration::from_millis(20));
                fs::write(&source, b"new").unwrap();

                ctx.globals()
                    .loc_load_extensions()
                    .set(ctx, scm_list(ctx, &["scm"]));
                ctx.globals()
                    .loc_load_path()
                    .set(ctx, scm_list(ctx, &[source_dir.to_string_lossy().as_ref()]));
                ctx.globals().loc_load_compiled_path().set(
                    ctx,
                    scm_list(ctx, &[compiled_dir.to_string_lossy().as_ref()]),
                );

                let resolved =
                    resolve_load_path(ctx, Path::new("lib/test"), None::<&Path>, false, None)
                        .unwrap()
                        .unwrap();
                let ResolvedLoadPath::Source {
                    compiled_artifact, ..
                } = resolved
                else {
                    panic!("expected source resolution");
                };
                assert_eq!(compiled_artifact, None);
            });
        });
    }
}

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

pub(super) const DYNLIB_EXTENSION: &str = if cfg!(target_os = "linux") {
    "so"
} else if cfg!(windows) {
    "dll"
} else {
    "dylib"
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct ResolvedLoadPath {
    pub(super) source_path: PathBuf,
    pub(super) full_source_path: PathBuf,
    pub(super) compiled_path: Option<PathBuf>,
}

impl ResolvedLoadPath {
    fn into_public(self) -> (PathBuf, PathBuf, Option<PathBuf>) {
        (self.source_path, self.full_source_path, self.compiled_path)
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
        PlanSelector::Immix | PlanSelector::MarkSweep | PlanSelector::SemiSpace => "regular",
        PlanSelector::ConcurrentImmix => "conc",
        PlanSelector::GenImmix | PlanSelector::GenCopy | PlanSelector::StickyImmix => "gen",
        _ => todo!(),
    };

    if cache_dir != FALLBACK_DIR {
        let path = Path::new(&cache_dir).join(plan);
        if !path.exists() {
            std::fs::create_dir_all(&path).expect("Failed to create cache directory");
        }
        ctx.globals()
            .loc_compile_fallback_path()
            .set(ctx, Str::new(*ctx, path.to_string_lossy(), true).into());
    }

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
        let paths = load_path.split(':').map(|segment| Str::new(*ctx, segment, true).into());
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
    _arch: Option<&str>,
) -> Result<Option<ResolvedLoadPath>, Value<'gc>> {
    let filename = filename.as_ref();
    let vicinity = in_vicinity.map(|path| path.as_ref().to_owned());

    let mut source_path = None;

    if filename.is_absolute() {
        if filename.is_file() {
            source_path = Some(filename.to_owned());
        }
    } else if filename.is_relative() && filename.is_file() && resolve_relative {
        source_path = Some(filename.to_owned());
    } else {
        let candidates = source_candidates(ctx, filename);

        if let Some(vicinity) = vicinity.as_ref() {
            source_path = candidates
                .iter()
                .map(|candidate| vicinity.join(candidate))
                .find(|candidate| is_regular_file(candidate));
        }

        if source_path.is_none() {
            let mut load_path = ctx.globals().loc_load_path().get();
            'outer: while load_path.is_pair() {
                let next = load_path.cdr();
                let base = PathBuf::from(load_path.car().downcast::<Str>().to_string());
                for candidate in &candidates {
                    let full = base.join(candidate);
                    if is_regular_file(&full) {
                        source_path = Some(full);
                        break 'outer;
                    }
                }
                load_path = next;
            }
        }
    }

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

    let compiled_path = freshest_compiled_candidate(ctx, filename, &full_source_path);

    Ok(Some(ResolvedLoadPath {
        source_path,
        full_source_path,
        compiled_path,
    }))
}

pub(super) fn fallback_file_name<'gc>(ctx: Context<'gc>, path: impl AsRef<Path>) -> PathBuf {
    let path = path.as_ref();
    let fallback = ctx
        .globals()
        .loc_compile_fallback_path()
        .get()
        .downcast::<Str>()
        .to_string();
    let fallback = Path::new(&fallback).join(std::env::consts::ARCH);
    let relative_source = path.strip_prefix("/").unwrap_or(path);
    let full_path = fallback.join(relative_source).with_extension(DYNLIB_EXTENSION);

    if let Some(parent) = full_path.parent()
        && !parent.exists()
    {
        std::fs::create_dir_all(parent).expect("Failed to create fallback directory");
    }

    full_path
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

fn freshest_compiled_candidate<'gc>(
    ctx: Context<'gc>,
    filename: &Path,
    full_source_path: &Path,
) -> Option<PathBuf> {
    let source_time = full_source_path
        .metadata()
        .ok()
        .and_then(|metadata| metadata.modified().ok())?;

    let mut compiled_path = ctx.globals().loc_load_compiled_path().get();
    while compiled_path.is_pair() {
        let next = compiled_path.cdr();
        let dir = PathBuf::from(compiled_path.car().downcast::<Str>().to_string());
        let candidate = dir.join(filename).with_extension(DYNLIB_EXTENSION);

        if is_regular_file(&candidate)
            && let Some(compiled_time) = candidate
                .metadata()
                .ok()
                .and_then(|metadata| metadata.modified().ok())
            && compiled_is_fresh(full_source_path, &candidate, source_time, compiled_time)
        {
            return Some(candidate);
        }

        compiled_path = next;
    }

    None
}

fn is_regular_file(path: &Path) -> bool {
    path.exists() && path.metadata().ok().is_some_and(|metadata| metadata.is_file())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{Context, Scheme};
    use std::{
        fs,
        sync::Mutex,
        time::Duration,
    };
    use uuid::Uuid;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    struct TempDir {
        path: PathBuf,
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
                let _ = crate::runtime::global::VM_GLOBALS
                    .set(crate::rsgc::Global::new(crate::runtime::global::Globals::new(ctx)))
                    .ok();
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
            ctx.globals().loc_load_path().set(
                ctx,
                scm_list(ctx, &[load_path.to_string_lossy().as_ref()]),
            );
            ctx.globals().loc_load_compiled_path().set(ctx, Value::null());

            let resolved =
                resolve_load_path(ctx, "target", Some(&vicinity), false, None).unwrap().unwrap();

            assert_eq!(resolved.source_path, vicinity.join("target.sld"));
            assert_eq!(
                resolved.full_source_path,
                vicinity.join("target.sld").canonicalize().unwrap()
            );
            assert_eq!(resolved.compiled_path, None);
        });
    }

    #[test]
    fn fallback_file_name_uses_arch_scoped_cache_path() {
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
                    .with_extension(DYNLIB_EXTENSION)
            );
        });
    }

    #[test]
    fn ignores_stale_compiled_artifacts() {
        with_ctx(|ctx| {
            let temp = TempDir::new();
            let source_dir = temp.path().join("src");
            let compiled_dir = temp.path().join("compiled");
            fs::create_dir_all(&source_dir).unwrap();
            fs::create_dir_all(compiled_dir.join("lib")).unwrap();

            let source = source_dir.join("lib/test.scm");
            fs::create_dir_all(source.parent().unwrap()).unwrap();
            fs::write(&source, b"old").unwrap();

            let compiled = compiled_dir.join("lib/test").with_extension(DYNLIB_EXTENSION);
            fs::write(&compiled, b"compiled").unwrap();
            std::thread::sleep(Duration::from_millis(20));
            fs::write(&source, b"new").unwrap();

            ctx.globals()
                .loc_load_extensions()
                .set(ctx, scm_list(ctx, &["scm"]));
            ctx.globals().loc_load_path().set(
                ctx,
                scm_list(ctx, &[source_dir.to_string_lossy().as_ref()]),
            );
            ctx.globals().loc_load_compiled_path().set(
                ctx,
                scm_list(ctx, &[compiled_dir.to_string_lossy().as_ref()]),
            );

            let resolved = resolve_load_path(ctx, Path::new("lib/test"), None::<&Path>, false, None)
                .unwrap()
                .unwrap();
            assert_eq!(resolved.compiled_path, None);
        });
    }
}

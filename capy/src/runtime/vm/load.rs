use crate::compiler::{compile_cps_to_object, link_object_product};
use crate::cps::contify::contify;
use crate::expander::eta_expand::eta_expand;
use crate::expander::fix_letrec::fix_letrec;
use crate::expander::free_vars::resolve_free_vars;
use crate::expander::letrectify::letrectify;
use crate::expander::{assignment_elimination, compile_cps, primitives};
use crate::list;
use crate::rsgc::Gc;
use crate::runtime::image::ALLOWED_GC;
use crate::runtime::modules::{Module, Variable, current_module, define};
use crate::runtime::value::*;
use crate::runtime::vm::base::scm_log_level;
use crate::runtime::vm::expand::ScmTermToRsTerm;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::{make_io_error, make_lexical_violation};
use capy_derive::scheme;
use mmtk::util::options::PlanSelector;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::{global, runtime::Context};
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
    let mut cpath = Value::null();

    // Sysroot discovery:
    // - portable builds: use the executable directory
    // - non-portable (FHS) builds: use a compile-time prefix (CAPY_SYSROOT)
    //
    // Layouts:
    // - portable: <sysroot>/lib and <sysroot>/compiled
    // - FHS:      <sysroot>/share/capy and <sysroot>/lib/capy/compiled
    let sysroot_dir: Option<PathBuf> = if cfg!(feature = "portable") {
        std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|d| d.to_owned()))
    } else {
        Some(PathBuf::from(env!("CAPY_SYSROOT")))
    };

    if let Some(sysroot_dir) = sysroot_dir.as_ref() {
        let candidates = [
            sysroot_dir.join("share").join("capy"),
            sysroot_dir.join("lib"),
        ];
        for stdlib_dir in candidates {
            if stdlib_dir.is_dir() {
                path = Value::cons(
                    ctx,
                    Str::new(*ctx, &stdlib_dir.to_string_lossy(), true).into(),
                    path,
                );
            }
        }

        // Prefer the sysroot cache (populated by FHS installs) for compiled artifacts,
        // then fall back to the historical compiled locations.
        let compiled_candidates = [
            sysroot_dir
                .join("lib")
                .join("capy")
                .join("cache")
                .join(env!("CARGO_PKG_VERSION"))
                .join(plan),
            sysroot_dir.join("lib").join("capy").join("compiled"),
            sysroot_dir.join("compiled"),
        ];

        let mut compiled_dirs = Vec::<PathBuf>::new();
        for compiled_dir in compiled_candidates {
            if compiled_dir.is_dir() {
                compiled_dirs.push(compiled_dir);
            }
        }

        // Preserve candidate order: the loader prefers the first existing directory.
        for compiled_dir in compiled_dirs.into_iter().rev() {
            cpath = Value::cons(
                ctx,
                Str::new(*ctx, &compiled_dir.to_string_lossy(), true).into(),
                cpath,
            );
        }
    }

    if let Ok(load_path) = std::env::var("CAPY_LOAD_PATH") {
        let paths = load_path.split(':').map(|s| Str::new(*ctx, s, true).into());
        let mut sig = Value::null();
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }
        path = sig.append(ctx, path);
    }

    if let Ok(compiled_path) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        let paths = compiled_path
            .split(':')
            .map(|s| Str::new(*ctx, s, true).into());
        let mut sig = cpath;
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }
        cpath = sig;
    }

    ctx.globals().loc_load_path().set(ctx, path);
    ctx.globals().loc_load_compiled_path().set(ctx, cpath);

    let fallback_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
    let root = sysroot_dir.as_deref().unwrap_or(&fallback_root);
    let root_str = root.to_string_lossy();
    ctx.globals()
        .loc_capy_root()
        .set(ctx, Str::new(*ctx, root_str, true).into());
}

static DYNLIB_EXTENSION: &str = if cfg!(target_os = "linux") {
    "so"
} else if cfg!(windows) {
    "dll"
} else {
    "dylib"
};

fn hash_filename(path: impl AsRef<Path>) -> PathBuf {
    use sha3::{Digest, Sha3_256};

    let mut hasher = Sha3_256::new();
    let path = path.as_ref();

    // Content-addressed cache key so compiled artifacts can be shared across machines
    // as long as ABI-relevant inputs match.
    //
    // We include:
    // - CapyScheme version (prevents cross-version ABI mismatches)
    // - OS/ARCH (prevents cross-platform collisions)
    // - GC type + MMTk plan selector (compiled thunks can be GC-plan sensitive)
    // - Source file bytes (content addressability, independent of absolute paths)
    hasher.update(env!("CARGO_PKG_VERSION").as_bytes());
    hasher.update(b"\0");
    hasher.update(std::env::consts::OS.as_bytes());
    hasher.update(b"\0");
    hasher.update(std::env::consts::ARCH.as_bytes());
    hasher.update(b"\0");

    let gc_typ = ALLOWED_GC.get().unwrap();
    let gc_typ = (*gc_typ) as u8;
    hasher.update(&[gc_typ]);
    hasher.update(b"\0");

    let plan_selector = *crate::GarbageCollector::get().mmtk.get_options().plan;
    hasher.update(format!("{:?}", plan_selector).as_bytes());
    hasher.update(b"\0");

    match std::fs::read(path) {
        Ok(bytes) => hasher.update(&bytes),
        Err(_) => hasher.update(path.to_string_lossy().as_bytes()),
    }

    let bytes = hasher.finalize();

    let hex_string = hex::encode(bytes);

    PathBuf::from(hex_string)
}

pub fn init_load<'gc>(ctx: Context<'gc>) {
    load_ops::register(ctx);
}

fn fresh_auto_compile<'gc>(ctx: Context<'gc>) -> bool {
    ctx.globals().loc_fresh_auto_compile().get() != Value::new(false)
}

/// Given a file name find the full path to the file and the path to its compiled version.
pub fn find_path_to<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
    arch: Option<&str>,
) -> Result<Option<(PathBuf, PathBuf)>, Value<'gc>> {
    let arch = arch.unwrap_or(std::env::consts::ARCH);
    let filename = filename.as_ref();
    let dir = in_vicinity.map(|p| p.as_ref().to_owned());

    let mut source_path = None;

    if filename.is_absolute() {
        if filename.is_file() {
            source_path = Some(filename.to_owned());
        }
    } else if filename.is_relative() && filename.is_file() && resolve_relative {
        source_path = Some(filename.to_owned());
    } else {
        let mut candidates = Vec::new();

        if let Some(dir) = dir.as_ref() {
            let path = dir.join(filename);

            if path.is_file() {
                candidates.push(path);
            }
        }
        if filename.extension().is_some() {
            candidates.push(filename.to_owned());
        }

        let mut exts = ctx.globals().loc_load_extensions().get();
        if exts.is_null() {
            candidates.push(filename.with_extension("scm"));
        } else {
            while exts.is_pair() {
                let ext = exts.car().downcast::<Str>().to_string();
                candidates.push(filename.with_extension(&ext));
                exts = exts.cdr();
            }
        }
        let paths = ctx.globals().loc_load_path().get();

        let mut ps = paths;

        for name in candidates.iter() {
            if let Some(dir) = dir.as_ref() {
                let candidate = dir.join(&name);

                if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some()
                {
                    source_path = Some(candidate);
                    break;
                }
            }
        }
        if source_path.is_none() {
            'outer: while ps.is_pair() {
                let next = ps.cdr();
                let path = PathBuf::from(ps.car().downcast::<Str>().to_string());

                for name in candidates.iter() {
                    let candidate = path.join(name);

                    if candidate.exists()
                        && candidate.metadata().ok().filter(|m| m.is_file()).is_some()
                    {
                        source_path = Some(candidate);
                        break 'outer;
                    }
                }

                ps = next;
            }
        }
    }

    let source_path = match source_path {
        Some(p) => p.canonicalize().map_err(|err| {
            make_io_error(
                ctx,
                "find-path-to",
                Str::new(
                    *ctx,
                    format!("Failed to canonicalize path {}: {err}", p.display()),
                    true,
                )
                .into(),
                &[],
            )
        })?,
        None => return Ok(None),
    };

    let hashed = hash_filename(&source_path);
    let mut compiled_file = None;

    let mut cpath = ctx.globals().loc_load_compiled_path().get();

    while cpath.is_pair() {
        let dir = PathBuf::from(cpath.car().downcast::<Str>().to_string());
        if dir.is_dir() {
            let candidate = dir
                .join(arch)
                .join(&hashed)
                .with_extension(DYNLIB_EXTENSION);
            if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some() {
                compiled_file = Some(candidate);
            }
            break;
        }
        cpath = cpath.cdr();
    }

    if compiled_file.is_none() {
        let fallback = ctx
            .globals()
            .loc_compile_fallback_path()
            .get()
            .downcast::<Str>()
            .to_string();
        let fallback = Path::new(&fallback);
        if !fallback.exists() {
            std::fs::create_dir_all(fallback).expect("Failed to create fallback directory");
        }
        // Keep caches separated by arch so a shared cache directory can be used.
        let fallback_arch = fallback.join(arch);
        if !fallback_arch.exists() {
            std::fs::create_dir_all(&fallback_arch).expect("Failed to create fallback directory");
        }
        let candidate = fallback_arch.join(&hashed).with_extension(DYNLIB_EXTENSION);

        compiled_file = Some(candidate);
    }
    Ok(Some((source_path, compiled_file.unwrap())))
}

pub fn load_thunk_in_vicinity<'gc, const FORCE_COMPILE: bool>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,

    arch: Option<&str>,
) -> Result<Value<'gc>, Value<'gc>> {
    let filename = filename.as_ref();

    let (source, compiled) = match find_path_to(ctx, filename, in_vicinity, resolve_relative, arch)?
    {
        Some(v) => v,
        None => {
            return Err(make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    &format!("File not found: {}", filename.to_string_lossy()),
                    true,
                )
                .into(),
                &[],
            ));
        }
    };

    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    let mut compiled_thunk = Value::new(false);

    // Compiled artifacts are content-addressed, so existence is enough to treat them as fresh.
    // If loading fails we fall back to recompilation below.
    if compiled.exists() && !fresh_auto_compile(ctx) {
        compiled_thunk = libs.load(&compiled, ctx).unwrap_or(Value::new(false));
    }

    if compiled_thunk.is::<Closure>() {
        return Ok(compiled_thunk);
    }
    if !FORCE_COMPILE {
        let source_str = Str::new(*ctx, source.display().to_string(), true);
        let compiled_str = Str::new(*ctx, compiled.display().to_string(), true);

        return Ok(Value::cons(ctx, source_str.into(), compiled_str.into()));
    }
    let f =
        crate::compiler::compile_file(ctx, source, Some(current_module(ctx).get(ctx).downcast()))?;
    let product = crate::compiler::compile_cps_to_object(ctx, f, Default::default())?;
    crate::compiler::link_object_product(ctx, product, &compiled)?;

    libs.load(compiled, ctx).map_err(|err| {
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
#[scheme(path=capy)]
pub mod load_ops {
    use crate::{compiler::CompilationOptions, expander::eta_expand::eta_expand};

    #[scheme(name = "%find-path-to")]
    pub fn scm_find_path_to(
        filename: Gc<'gc, Str<'gc>>,
        resolve_relative: bool,
        in_vicinity: Option<Gc<'gc, Str<'gc>>>,
        arch: Option<Gc<'gc, Str<'gc>>>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string()));
        let filename = PathBuf::from(filename.as_ref().to_string());
        let arch = arch.map(|s| s.as_str());
        let ctx = nctx.ctx;
        let result = match find_path_to(
            ctx,
            filename,
            in_vicinity,
            resolve_relative,
            arch.as_ref().map(|x| x.as_ref()),
        ) {
            Ok(v) => v,
            Err(err) => return nctx.return_(Err(err)),
        };

        match result {
            Some((source, compiled)) => nctx.return_(Ok(Value::cons(
                ctx,
                Str::new(*ctx, &source.to_string_lossy(), true).into(),
                Str::new(*ctx, &compiled.to_string_lossy(), true).into(),
            ))),
            None => nctx.return_(Ok(Value::new(false))),
        }
    }

    #[scheme(name = "%compile")]
    pub fn compile(
        expanded: Value<'gc>,
        destination: Gc<'gc, Str<'gc>>,
        m: Option<Value<'gc>>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let backtraces = if let Some(key) = ctx.private_ref("capy", "*compile-backtrace-key*") {
            match ctx.get_mark_first(key) {
                Some(mark) => mark != Value::new(false),
                None => true,
            }
        } else {
            true
        };

        let mut reader = ScmTermToRsTerm::new(nctx.ctx);
        let mut ir = match reader.convert(expanded) {
            Ok(ir) => ir,
            Err(err) => return nctx.return_(Err(err)),
        };

        let m = if let Some(m) = m {
            if m.is::<Module>() {
                m.downcast()
            } else {
                current_module(nctx.ctx).get(nctx.ctx).downcast()
            }
        } else {
            current_module(nctx.ctx).get(nctx.ctx).downcast()
        };

        ir = primitives::resolve_primitives(nctx.ctx, ir, m);
        ir = primitives::expand_primitives(nctx.ctx, ir);

        ir = resolve_free_vars(nctx.ctx, ir);
        ir = letrectify(nctx.ctx, ir);
        ir = fix_letrec(nctx.ctx, ir);
        ir = eta_expand(nctx.ctx, ir);
        ir = assignment_elimination::eliminate_assignments(nctx.ctx, ir);

        let mut cps = compile_cps::cps_toplevel(nctx.ctx, &[ir]);
        cps = crate::cps::rewrite_func(nctx.ctx, cps);
        cps = cps.with_body(nctx.ctx, contify(nctx.ctx, cps.body()));

        if scm_log_level(nctx.ctx) >= 5 {
            let doc = ir.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
            let mut file = std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(format!("{}.ir.scm", destination))
                .unwrap();
            println!(";; TRACE  (capy)@load: IR -> {}.ir.scm", destination);
            doc.1.render(80, &mut file).unwrap();

            let doc = cps.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
            let mut file = std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(format!("{}.cps.scm", destination))
                .unwrap();
            println!(";; TRACE  (capy)@load: CPS -> {}.cps.scm", destination);
            doc.1.render(80, &mut file).unwrap();
        }

        let object = match compile_cps_to_object(nctx.ctx, cps, CompilationOptions { backtraces }) {
            Ok(product) => product,
            Err(err) => return nctx.return_(Err(err)),
        };
        let compiled_path = destination.to_string();
        match link_object_product(nctx.ctx, object, &compiled_path) {
            Ok(_) => (),
            Err(err) => return nctx.return_(Err(err)),
        }
        let ctx = nctx.ctx;
        let libs = LIBRARY_COLLECTION.fetch(*ctx);
        match libs.load(&compiled_path, ctx) {
            Err(err) => nctx.return_(Err(make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    format!("Failed to load compiled library: {err}"),
                    true,
                )
                .into(),
                &[],
            ))),
            Ok(thunk) => nctx.return_(Ok(thunk)),
        }
    }

    #[scheme(name = "try-load-thunk-in-vicinity")]
    pub fn scm_try_load_thunk_in_vicinity(
        filename: Gc<'gc, Str<'gc>>,
        resolve_relative: bool,
        in_vicinity: Option<Gc<'gc, Str<'gc>>>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string()));
        let filename = PathBuf::from(filename.as_ref().to_string());
        let result = load_thunk_in_vicinity::<false>(
            nctx.ctx,
            filename,
            in_vicinity,
            resolve_relative,
            None,
        );

        nctx.return_(result)
    }

    #[scheme(name = "load-thunk-in-vicinity")]
    pub fn scm_load_thunk_in_vicinity(
        filename: Gc<'gc, Str<'gc>>,
        resolve_relative: bool,
        in_vicinity: Option<Gc<'gc, Str<'gc>>>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string()));
        let filename = PathBuf::from(filename.as_ref().to_string());
        let result =
            load_thunk_in_vicinity::<true>(nctx.ctx, filename, in_vicinity, resolve_relative, None);

        nctx.return_(result)
    }

    /// Same as load-thunk-in-vicinity, but takes closure `k` which is involed
    /// when compilation is required. The job of this closure is to compile provided
    /// source S-expressions into TreeIL and return it for further processing.
    #[scheme(name = "load-thunk-in-vicinity-k")]
    pub fn scm_load_thunk_in_vicinity_k(
        filename: Gc<'gc, Str<'gc>>,
        k: Value<'gc>,
        env: Value<'gc>,
        resolve_relative: bool,
        in_vicinity: Option<Gc<'gc, Str<'gc>>>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string()));
        let filename = PathBuf::from(filename.as_ref().to_string());
        let result = load_thunk_in_vicinity::<false>(
            nctx.ctx,
            filename,
            in_vicinity,
            resolve_relative,
            None,
        );

        match result {
            Ok(thunk) => {
                if thunk.is::<Closure>() {
                    return nctx.return_(Ok(thunk));
                }
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
                let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), true);

                let program = match parser.read_program().map_err(|err| {
                    make_lexical_violation(ctx, "compile-file", err.to_string(&file))
                }) {
                    Ok(program) => program,
                    Err(err) => return nctx.return_(Err(err)),
                };

                let reth = nctx.reth;
                let retk = nctx.retk;

                let after_call = make_closure_continue_loading_k(ctx, [thunk, retk, reth]);
                let program = Value::list_from_slice(ctx, program);
                nctx.call(k, &[program, env], after_call.into(), reth)
            }

            Err(err) => nctx.return_(Err(err)),
        }
    }

    #[scheme(name = "%search-load-path")]
    pub fn search_load_path(filename: StringRef<'gc>, arch: Option<StringRef<'gc>>) -> Value<'gc> {
        let ctx = nctx.ctx;
        let arch = arch.map(|s| s.as_str());
        let result = find_path_to(
            ctx,
            PathBuf::from(filename.as_ref().to_string()),
            None::<PathBuf>,
            true,
            arch.as_ref().map(|x| x.as_ref()),
        );

        match result {
            Ok(Some((source, compiled))) => nctx.return_(Value::cons(
                ctx,
                Str::new(*ctx, &source.to_string_lossy(), true).into(),
                Str::new(*ctx, &compiled.to_string_lossy(), true).into(),
            )),
            Ok(None) => nctx.return_(Value::new(false)),
            Err(_err) => nctx.return_(Value::new(false)),
        }
    }
}

use crate::prelude::*;

#[scheme(continuation)]
pub(crate) fn continue_loading_k(
    ir: Value<'gc>,
    cenv: Value<'gc>,
    _unused: Value<'gc>,
) -> Result<Value<'gc>, Value<'gc>> {
    let rator = nctx.rator().downcast::<Closure>();
    let free = rator.free.downcast::<Vector>();
    let source_and_compiled_path = free[1].get();
    let retk = free[2].get();
    let reth = free[3].get();

    nctx.reth = reth;
    nctx.retk = retk;

    let mut reader = ScmTermToRsTerm::new(nctx.ctx);
    let mut ir = match reader.convert(ir) {
        Ok(ir) => ir,
        Err(err) => return nctx.return_(Err(err)),
    };
    // println!(";; Compiling file: {} -> {}", source_and_compiled_path.car(), source_and_compiled_path.cdr());
    let m = if cenv.is::<Module>() {
        cenv.downcast()
    } else {
        current_module(nctx.ctx).get(nctx.ctx).downcast()
    };

    ir = primitives::resolve_primitives(nctx.ctx, ir, m);
    ir = primitives::expand_primitives(nctx.ctx, ir);
    ir = resolve_free_vars(nctx.ctx, ir);
    ir = letrectify(nctx.ctx, ir);
    ir = fix_letrec(nctx.ctx, ir);
    ir = eta_expand(nctx.ctx, ir);
    ir = assignment_elimination::eliminate_assignments(nctx.ctx, ir);

    let mut cps = compile_cps::cps_toplevel(nctx.ctx, &[ir]);

    cps = crate::cps::rewrite_func(nctx.ctx, cps);
    cps = cps.with_body(nctx.ctx, contify(nctx.ctx, cps.body()));

    if !true {
        let doc = ir.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(format!(
                "{}.ir.scm",
                source_and_compiled_path.cdr().downcast::<Str>()
            ))
            .unwrap();
        doc.1.render(80, &mut file).unwrap();
        println!(
            ";; IR {} -> {}.ir.scm",
            source_and_compiled_path.car(),
            source_and_compiled_path.cdr()
        );

        let doc = cps.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(format!(
                "{}.cps.scm",
                source_and_compiled_path.cdr().downcast::<Str>()
            ))
            .unwrap();
        println!(
            ";; CPS {} -> {}.cps.scm",
            source_and_compiled_path.car(),
            source_and_compiled_path.cdr()
        );
        doc.1.render(80, &mut file).unwrap();
    }

    let object = match compile_cps_to_object(nctx.ctx, cps, Default::default()) {
        Ok(product) => product,
        Err(err) => return nctx.return_(Err(err)),
    };
    let compiled_path = source_and_compiled_path.cdr().downcast::<Str>().to_string();
    match link_object_product(nctx.ctx, object, &compiled_path) {
        Ok(_) => (),
        Err(err) => return nctx.return_(Err(err)),
    }
    let ctx = nctx.ctx;

    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    match libs.load(&compiled_path, ctx) {
        Err(err) => {
            return nctx.return_(Err(make_io_error(
                ctx,
                "load",
                Str::new(
                    *ctx,
                    format!("Failed to load compiled library: {err}"),
                    true,
                )
                .into(),
                &[],
            )));
        }
        Ok(thunk) => unsafe { nctx.continue_to(retk, &[thunk]) },
    }
}

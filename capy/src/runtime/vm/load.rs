use crate::compiler::{compile_cps_to_object, link_object_product};
use crate::cps::contify::contify;
use crate::expander::fix_letrec::fix_letrec;
use crate::expander::{assignment_elimination, compile_cps, primitives};
use crate::runtime::modules::{Module, Variable, current_module, define};
use crate::runtime::value::*;
use crate::runtime::vm::expand::ScmTermToRsTerm;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::{make_io_error, make_lexical_violation};
use crate::{list, native_cont, native_fn};
use rsgc::Gc;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::{global, runtime::Context};
global!(
    pub loc_load_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-path", Value::null());
    pub loc_load_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-extensions", list!(ctx, Str::new(&ctx, "scm", true)));
    pub loc_load_compiled_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-path", Value::null());
    pub loc_load_compiled_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-extensions", Value::null());

    pub loc_compile_fallback_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%compile-fallback-path", Value::null());

    pub loc_capy_root<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%capy-root", Str::new(&ctx, env!("CARGO_MANIFEST_DIR"), true).into());
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

    if cache_dir != FALLBACK_DIR {
        let path = Path::new(&cache_dir);
        if !path.exists() {
            std::fs::create_dir_all(path).expect("Failed to create cache directory");
        }
        loc_compile_fallback_path(ctx).set(ctx, Str::new(&ctx, &cache_dir, true).into());
    }

    if let Ok(path) = std::env::var("CAPY_LOAD_PATH") {
        let paths = path.split(':').map(|s| Str::new(&ctx, s, true).into());
        let mut sig = Value::null();
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }
        loc_load_path(ctx).set(ctx, sig);
    }

    if let Ok(path) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        let paths = path.split(':').map(|s| Str::new(&ctx, s, true).into());
        let mut sig = Value::null();
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }

        loc_load_compiled_path(ctx).set(ctx, sig);
    }

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_dir = Path::new(manifest_dir)
        .parent()
        .unwrap_or(Path::new(manifest_dir));

    loc_capy_root(ctx).set(
        ctx,
        Str::new(&ctx, workspace_dir.to_string_lossy(), true).into(),
    );
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
    hasher.update(path.as_ref().to_string_lossy().as_bytes());
    let bytes = hasher.finalize();

    let hex_string = hex::encode(bytes);
    PathBuf::from(hex_string)
}

pub fn init_load<'gc>(ctx: Context<'gc>) {
    register_load_fns(ctx);
}

/// Given a file name find the full path to the file and the path to its compiled version.
pub fn find_path_to<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
) -> Result<Option<(PathBuf, PathBuf)>, Value<'gc>> {
    let filename = filename.as_ref();
    let dir = in_vicinity.map(|p| p.as_ref().to_owned());

    let mut source_path = None;

    if filename.is_absolute() {
        if filename.is_file() {
            source_path = Some(filename.to_owned());
        }
    } else if filename.is_relative() && filename.is_file() {
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

        let mut exts = loc_load_extensions(ctx).get();
        if exts.is_null() {
            candidates.push(filename.with_extension("scm"));
        } else {
            while exts.is_pair() {
                let ext = exts.car().to_string();
                candidates.push(filename.with_extension(&ext));
                exts = exts.cdr();
            }
        }
        let paths = loc_load_path(ctx).get();

        for name in candidates {
            if let Some(dir) = dir.as_ref() {
                let candidate = dir.join(&name);
                if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some()
                {
                    source_path = Some(candidate);
                    break;
                }
            }

            let mut paths = paths;

            while paths.is_pair() {
                let dir = PathBuf::from(paths.car().to_string());
                let candidate = dir.join(&name);
                if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some()
                {
                    source_path = Some(candidate);
                    break;
                }
                paths = paths.cdr();
            }
        }
    }

    let source_path = match source_path {
        Some(p) => p.canonicalize().map_err(|err| {
            make_io_error(
                &ctx,
                "find-path-to",
                Str::new(
                    &ctx,
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

    let mut cpath = loc_load_compiled_path(ctx).get();

    while cpath.is_pair() {
        let dir = PathBuf::from(cpath.car().to_string());
        if dir.is_dir() {
            let candidate = dir.join(&hashed).with_extension(DYNLIB_EXTENSION);
            if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some() {
                compiled_file = Some(candidate);
            }
            break;
        }
        cpath = cpath.cdr();
    }

    if compiled_file.is_none() {
        let fallback = loc_compile_fallback_path(ctx).get().to_string();
        let fallback = Path::new(&fallback);
        if !fallback.exists() {
            std::fs::create_dir_all(fallback).expect("Failed to create fallback directory");
        }
        let candidate = Path::new(&fallback)
            .join(&hashed)
            .with_extension(DYNLIB_EXTENSION);

        compiled_file = Some(candidate);
    }
    Ok(Some((source_path, compiled_file.unwrap())))
}

pub fn load_thunk_in_vicinity<'gc, const FORCE_COMPILE: bool>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
) -> Result<Value<'gc>, Value<'gc>> {
    let filename = filename.as_ref();
    let (source, compiled) = match find_path_to(ctx, filename, in_vicinity)? {
        Some(v) => v,
        None => {
            return Err(make_io_error(
                &ctx,
                "load",
                Str::new(
                    &ctx,
                    &format!("File not found: {}", filename.to_string_lossy()),
                    true,
                )
                .into(),
                &[],
            ));
        }
    };

    let source_time = source
        .metadata()
        .ok()
        .and_then(|m| m.modified().ok())
        .unwrap_or(SystemTime::UNIX_EPOCH);

    let libs = LIBRARY_COLLECTION.fetch(&ctx);
    let mut compiled_thunk = Value::new(false);

    if compiled.exists() {
        let compiled_time = compiled
            .metadata()
            .ok()
            .and_then(|m| m.modified().ok())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        if compiled_is_fresh(&source, &compiled, source_time, compiled_time) {
            compiled_thunk = libs.load(&compiled, ctx).unwrap_or(Value::new(false));
        }
    }

    if compiled_thunk.is::<Closure>() {
        return Ok(compiled_thunk);
    }
    if !FORCE_COMPILE {
        let source_str = Str::new(&ctx, source.display().to_string(), true);
        let compiled_str = Str::new(&ctx, compiled.display().to_string(), true);

        return Ok(Value::cons(ctx, source_str.into(), compiled_str.into()));
    }
    let f =
        crate::compiler::compile_file(ctx, source, Some(current_module(ctx).get(ctx).downcast()))?;
    let product = crate::compiler::compile_cps_to_object(ctx, f)?;
    crate::compiler::link_object_product(ctx, product, &compiled)?;

    libs.load(compiled, ctx).map_err(|err| {
        make_io_error(
            &ctx,
            "load",
            Str::new(
                &ctx,
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

native_fn!(
    register_load_fns:

    pub ("load-thunk-in-vicinity")
        fn scm_load_thunk_in_vicinity<'gc>(
            nctx,
            filename: Gc<'gc, Str<'gc>>,
            in_vicinity: Option<Gc<'gc, Str<'gc>>>
        ) -> Result<Value<'gc>, Value<'gc>> {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string())).unwrap_or_else(|| std::env::current_dir().unwrap());
        let filename = PathBuf::from(filename.as_ref().to_string());
        let result = load_thunk_in_vicinity::<true>(nctx.ctx, filename, Some(in_vicinity));

        nctx.return_(result)
    }

    /// Same as load-thunk-in-vicinity, but takes closure `k` which is involed
    /// when compilation is required. The job of this closure is to compile provided
    /// source S-expressions into TreeIL and return it for further processing.
    pub ("load-thunk-in-vicinity-k")
        fn scm_load_thunk_in_vicinity_k<'gc>(
            nctx,
            filename: Gc<'gc, Str<'gc>>,
            k: Value<'gc>,
            env: Value<'gc>,
            in_vicinity: Option<Gc<'gc, Str<'gc>>>
        ) -> Result<Value<'gc>, Value<'gc>>
    {
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string())).unwrap_or_else(|| std::env::current_dir().unwrap());
        let filename = PathBuf::from(filename.as_ref().to_string());
        let result = load_thunk_in_vicinity::<false>(nctx.ctx, filename, Some(in_vicinity));

        match result {
            Ok(thunk) => {
                if thunk.is::<Closure>() {
                    return nctx.return_(Ok(thunk));
                }
                let ctx = nctx.ctx;
                let file = thunk.car().to_string();
                let file_in = match std::fs::File::open(&file).map_err(|e| {
                    make_io_error(
                        &ctx,
                        "compile-file",
                        Str::new(
                            &ctx,
                            format!("Cannot open input file '{}': {}", file, e),
                            true,
                        )
                        .into(),
                        &[],
                    )
                }) {
                    Ok(file_in) => file_in,
                    Err(err) => return nctx.return_(Err(err))
                };

                let text = match std::io::read_to_string(&file_in).map_err(|e| {
                    make_io_error(
                        &ctx,
                        "compile-file",
                        Str::new(
                            &ctx,
                            format!("Cannot read input file '{}': {}", file, e),
                            true,
                        )
                        .into(),
                        &[],
                    )
                }) {
                    Ok(text) => text,
                    Err(err) => return nctx.return_(Err(err))
                };
                let src = Str::new(&ctx, &file, true);
                let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into());

                let program = match parser.read_program().map_err(|err| {
                    make_lexical_violation(&ctx, "compile-file", err.to_string(&file))
                }) {
                    Ok(program) => program,
                    Err(err) => return nctx.return_(Err(err))
                };

                let reth = nctx.reth;
                let retk = nctx.retk;
                let after_call = make_continue_loading_closure(ctx, [thunk, retk, reth]);
                let program = Value::list_from_slice(ctx, program);
                nctx.call(k, &[program, env], after_call.into(), reth)
            }

            Err(err) => nctx.return_(Err(err))
        }
    }
);

native_cont!(
    _register_conts:
    pub ("load-thunk-in-vicinity-k") fn continue_loading<'gc>(nctx, ir: Value<'gc>, cenv: Value<'gc>, _unused: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
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
            Err(err) => return nctx.return_(Err(err))
        };

        let m = if cenv.is::<Module>() {
            cenv.downcast()
        } else {
            current_module(nctx.ctx).get(nctx.ctx).downcast()
        };

        ir = fix_letrec(nctx.ctx, ir);
        ir = assignment_elimination::eliminate_assignments(nctx.ctx, ir);
        ir = primitives::resolve_primitives(nctx.ctx, ir, m);
        ir = primitives::expand_primitives(nctx.ctx, ir);

        let mut cps = compile_cps::cps_toplevel(nctx.ctx, &[ir]);

        cps = crate::cps::rewrite_func(nctx.ctx, cps);
        cps = cps.with_body(nctx.ctx, contify(nctx.ctx, cps.body));


        let object = match compile_cps_to_object(nctx.ctx, cps) {
            Ok(product) => product,
            Err(err) => return nctx.return_(Err(err)),
        };
        let compiled_path = source_and_compiled_path.cdr().to_string();
        match link_object_product(nctx.ctx, object, &compiled_path) {
            Ok(_) => (),
            Err(err) => return nctx.return_(Err(err))
        }
        let ctx = nctx.ctx;

        let libs = LIBRARY_COLLECTION.fetch(&ctx);
        match libs.load(&compiled_path, ctx) {
            Err(err) => return nctx.return_(Err(make_io_error(
                &ctx,
                "load",
                Str::new(
                    &ctx,
                    format!("Failed to load compiled library: {err}"),
                    true,
                )
                .into(),
                &[],
            ))),
            Ok(thunk) => unsafe { nctx.continue_to(retk, &[thunk]) },
        }
    }
);

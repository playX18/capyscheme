use crate::compiler::{compile_cps_to_object, link_object_product};
use crate::cps::contify::contify;
use crate::expander::fix_letrec::fix_letrec;
use crate::expander::free_vars::resolve_free_vars;
use crate::expander::letrectify::letrectify;
use crate::expander::{assignment_elimination, compile_cps, primitives};
use crate::list;
use crate::runtime::modules::{Module, Variable, current_module, define};
use crate::runtime::value::*;
use crate::runtime::vm::base::scm_log_level;
use crate::runtime::vm::expand::ScmTermToRsTerm;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::{make_io_error, make_lexical_violation};
use capy_derive::scheme;
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

    pub loc_capy_root<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%capy-root", Str::new(&ctx, env!("CARGO_MANIFEST_DIR"), true));

    pub loc_fresh_auto_compile<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%fresh-auto-compile", Value::new(false));
);

pub fn init_load_path<'gc>(ctx: Context<'gc>) {
    let _ = loc_load_path(ctx);
    let _ = loc_load_compiled_path(ctx);
    let _ = loc_compile_fallback_path(ctx);
    let _ = loc_capy_root(ctx);
    let _ = loc_fresh_auto_compile(ctx);
    let _ = loc_load_extensions(ctx);
    let _ = loc_load_compiled_extensions(ctx);
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

    let mut path = Value::null();
    let mut cpath = Value::null();

    if cfg!(feature = "portable") {
        let exe_dir = std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|p| p.to_path_buf()))
            .unwrap_or_else(|| PathBuf::from("."));
        path = Value::cons(
            ctx,
            Str::new(&ctx, &exe_dir.to_string_lossy(), true).into(),
            path,
        );

        let batteris_dir = exe_dir.join("batteries");
        path = Value::cons(
            ctx,
            Str::new(&ctx, &batteris_dir.to_string_lossy(), true).into(),
            path,
        );

        let stdlib_dir = exe_dir.join("stdlib");
        path = Value::cons(
            ctx,
            Str::new(&ctx, &stdlib_dir.to_string_lossy(), true).into(),
            path,
        );

        let _cpath = exe_dir.join("compiled");
        cpath = Value::cons(
            ctx,
            Str::new(&ctx, &_cpath.to_string_lossy(), true).into(),
            cpath,
        );
    }

    if let Ok(load_path) = std::env::var("CAPY_LOAD_PATH") {
        let paths = load_path.split(':').map(|s| Str::new(&ctx, s, true).into());
        let mut sig = path;
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }
        path = sig;
    }

    if let Ok(compiled_path) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        let paths = compiled_path
            .split(':')
            .map(|s| Str::new(&ctx, s, true).into());
        let mut sig = cpath;
        for p in paths.rev() {
            sig = Value::cons(ctx, p, sig);
        }
        cpath = sig;
    }

    loc_load_path(ctx).set(ctx, path);
    loc_load_compiled_path(ctx).set(ctx, cpath);

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
    register_compile(ctx);
    register_scm_find_path_to(ctx);
    register_scm_load_thunk_in_vicinity(ctx);
    register_scm_load_thunk_in_vicinity_k(ctx);
    register_scm_try_load_thunk_in_vicinity(ctx);
}

fn fresh_auto_compile<'gc>(ctx: Context<'gc>) -> bool {
    loc_fresh_auto_compile(ctx).get() != Value::new(false)
}

/// Given a file name find the full path to the file and the path to its compiled version.
pub fn find_path_to<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
    resolve_relative: bool,
) -> Result<Option<(PathBuf, PathBuf)>, Value<'gc>> {
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

        let mut exts = loc_load_extensions(ctx).get();
        if exts.is_null() {
            candidates.push(filename.with_extension("scm"));
        } else {
            while exts.is_pair() {
                let ext = exts.car().downcast::<Str>().to_string();
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
                let dir = PathBuf::from(paths.car().downcast::<Str>().to_string());
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
        let dir = PathBuf::from(cpath.car().downcast::<Str>().to_string());
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
        let fallback = loc_compile_fallback_path(ctx)
            .get()
            .downcast::<Str>()
            .to_string();
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
    resolve_relative: bool,
    _stx: bool,
) -> Result<Value<'gc>, Value<'gc>> {
    let filename = filename.as_ref();
    let (source, compiled) = match find_path_to(ctx, filename, in_vicinity, resolve_relative)? {
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
        if !fresh_auto_compile(ctx)
            && compiled_is_fresh(&source, &compiled, source_time, compiled_time)
        {
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

#[scheme(name = "%find-path-to")]
pub fn scm_find_path_to(
    filename: Gc<'gc, Str<'gc>>,
    resolve_relative: bool,
    in_vicinity: Option<Gc<'gc, Str<'gc>>>,
) -> Result<Value<'gc>, Value<'gc>> {
    let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_ref().to_string()));
    let filename = PathBuf::from(filename.as_ref().to_string());
    let ctx = nctx.ctx;
    let result = match find_path_to(ctx, filename, in_vicinity, resolve_relative) {
        Ok(v) => v,
        Err(err) => return nctx.return_(Err(err)),
    };

    match result {
        Some((source, compiled)) => nctx.return_(Ok(Value::cons(
            ctx,
            Str::new(&ctx, &source.to_string_lossy(), true).into(),
            Str::new(&ctx, &compiled.to_string_lossy(), true).into(),
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

    let object = match compile_cps_to_object(nctx.ctx, cps) {
        Ok(product) => product,
        Err(err) => return nctx.return_(Err(err)),
    };
    let compiled_path = destination.to_string();
    match link_object_product(nctx.ctx, object, &compiled_path) {
        Ok(_) => (),
        Err(err) => return nctx.return_(Err(err)),
    }
    let ctx = nctx.ctx;
    let libs = LIBRARY_COLLECTION.fetch(&ctx);
    match libs.load(&compiled_path, ctx) {
        Err(err) => nctx.return_(Err(make_io_error(
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
    let result =
        load_thunk_in_vicinity::<false>(nctx.ctx, filename, in_vicinity, resolve_relative, false);

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
        load_thunk_in_vicinity::<true>(nctx.ctx, filename, in_vicinity, resolve_relative, false);

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
    let result =
        load_thunk_in_vicinity::<false>(nctx.ctx, filename, in_vicinity, resolve_relative, true);

    match result {
        Ok(thunk) => {
            if thunk.is::<Closure>() {
                return nctx.return_(Ok(thunk));
            }
            let ctx = nctx.ctx;
            let file = thunk.car().downcast::<Str>().to_string();
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
                Err(err) => return nctx.return_(Err(err)),
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
                Err(err) => return nctx.return_(Err(err)),
            };
            let src = Str::new(&ctx, &file, true);
            let parser = crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), true);

            let program = match parser
                .read_program()
                .map_err(|err| make_lexical_violation(&ctx, "compile-file", err.to_string(&file)))
            {
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

use crate::prelude::*;

#[scheme(continuation)]
fn continue_loading_k(
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

    let object = match compile_cps_to_object(nctx.ctx, cps) {
        Ok(product) => product,
        Err(err) => return nctx.return_(Err(err)),
    };
    let compiled_path = source_and_compiled_path.cdr().downcast::<Str>().to_string();
    match link_object_product(nctx.ctx, object, &compiled_path) {
        Ok(_) => (),
        Err(err) => return nctx.return_(Err(err)),
    }
    let ctx = nctx.ctx;

    let libs = LIBRARY_COLLECTION.fetch(&ctx);
    match libs.load(&compiled_path, ctx) {
        Err(err) => {
            return nctx.return_(Err(make_io_error(
                &ctx,
                "load",
                Str::new(
                    &ctx,
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

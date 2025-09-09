use crate::runtime::modules::{Variable, current_module, define};
use crate::runtime::value::*;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;
use crate::{list, native_fn};
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
/*
pub fn load_in_vicinity<'gc>(
    ctx: Context<'gc>,
    dir: impl AsRef<Path>,
    file_name: &str,
) -> Result<Value<'gc>, Value<'gc>> {
    let compiled_extension = {
        let load_compiled_extensions = loc_load_compiled_extensions(ctx).get();
        if (load_compiled_extensions.is_null() || !load_compiled_extensions.is_pair())
            || load_compiled_extensions.car().is::<Str>()
                && load_compiled_extensions.car().downcast::<Str>().len() == 0
        {
            DYNLIB_EXTENSION.to_string()
        } else {
            load_compiled_extensions
                .car()
                .downcast::<Str>()
                .as_ref()
                .to_string()
        }
    };

    let fallback_file_name = |name: &str| {
        let loc = loc_compile_fallback_path(ctx).get();
        let hashed = hash_filename(name);
        if loc.is::<Str>() {
            Some(
                Path::new(&loc.to_string())
                    .join(&hashed)
                    .with_extension(&compiled_extension),
            )
        } else {
            None
        }
    };

    let file_path = Path::new(file_name);

    if !file_path.is_absolute() {
        let path = dir.as_ref().join(file_name);

        return load_from_path(ctx, path);
    }
    let dir = dir.as_ref();

    if !dir.is_absolute() {
        let path = dir.join(file_name);
        return load_from_path(ctx, path);
    }
    let path = if file_path.is_absolute() {
        file_path.to_owned()
    } else {
        dir.join(file_name)
    };

    let load_compiled_path = loc_load_compiled_path(ctx).get();
    let load_compiled_ext = loc_load_compiled_extensions(ctx).get();

    let scm_stat = path
        .metadata()
        .ok()
        .filter(|m| m.is_file())
        .and_then(|m| m.modified().ok());
    let hashed = hash_filename(&path);
    if load_compiled_path.is::<Str>() && load_compiled_ext.is::<Str>() {
        /*let candidate = dir
            .join(file_name)
            .with_extension(load_compiled_ext.to_string());
        */
        let candidate = Path::new(&load_compiled_path.to_string())
            .join(&hashed)
            .with_extension(&load_compiled_ext.to_string());
        if let (Some(mstat), Some(scmmstat)) = (
            candidate
                .metadata()
                .ok()
                .filter(|m| m.is_file())
                .and_then(|m| m.modified().ok()),
            scm_stat,
        ) {
            if mstat > scmmstat {
                return LIBRARY_COLLECTION
                    .fetch(&ctx)
                    .load(candidate, ctx)
                    .map_err(|err| {
                        Str::new(
                            &ctx,
                            format!("Failed to load compiled library: {err}"),
                            true,
                        )
                        .into()
                    });
            } else {
                let product = compile_file(ctx, &path)?;
                let buf = product.emit().map_err(|err| {
                    Str::new(
                        &ctx,
                        format!("Failed to emit compiled library: {err}"),
                        true,
                    )
                })?;

                let output = candidate.with_extension("o");

                std::fs::write(&output, buf).map_err(|err| {
                    Str::new(
                        &ctx,
                        format!(
                            "Failed to write compiled library: {err} {}",
                            output.display()
                        ),
                        true,
                    )
                })?;

                let _clang = std::process::Command::new("clang")
                    .arg("-shared")
                    .arg("-fPIC")
                    .arg("-lcapy")
                    .arg("-o")
                    .arg(&candidate)
                    .arg(&output)
                    .status()
                    .map_err(|err| {
                        Str::new(&ctx, format!("Failed to invoke clang: {err}"), true)
                    })?;
                println!("Compiled {} to {}", path.display(), candidate.display());

                return LIBRARY_COLLECTION
                    .fetch(&ctx)
                    .load(candidate, ctx)
                    .map_err(|err| {
                        Str::new(
                            &ctx,
                            format!("Failed to load compiled library: {err}"),
                            true,
                        )
                        .into()
                    });
            }
        }
    }

    let fallback = fallback_file_name(
        path.canonicalize()
            .map_err(|err| Str::new(&ctx, format!("IO error: {err}"), true))?
            .to_str()
            .unwrap(),
    )
    .ok_or_else(|| Str::new(&ctx, "Invalid fallback path", true))?;

    if fallback.exists() {
        if let (Some(mstat), Some(scmmstat)) = (
            fallback
                .metadata()
                .ok()
                .filter(|m| m.is_file())
                .and_then(|m| m.modified().ok()),
            scm_stat,
        ) {
            if mstat > scmmstat {
                return LIBRARY_COLLECTION
                    .fetch(&ctx)
                    .load(fallback, ctx)
                    .map_err(|err| {
                        Str::new(
                            &ctx,
                            format!("Failed to load compiled library: {err}"),
                            true,
                        )
                        .into()
                    });
            }
        }
    }

    let product = compile_file(ctx, &path)?;
    let buf = product.emit().map_err(|err| {
        Str::new(
            &ctx,
            format!("Failed to emit compiled library: {err}"),
            true,
        )
    })?;

    let output = fallback.with_extension("o");
    std::fs::write(&output, buf).map_err(|err| {
        Str::new(
            &ctx,
            format!(
                "Failed to write compiled library: {err} {}",
                output.display()
            ),
            true,
        )
    })?;

    let _clang = std::process::Command::new("clang")
        .arg("-shared")
        .arg("-fPIC")
        .arg("-lcapy")
        .arg("-o")
        .arg(&fallback)
        .arg(&output)
        .status()
        .map_err(|err| Str::new(&ctx, format!("Failed to invoke clang: {err}"), true))?;
    println!("Compiled {} to {}", path.display(), fallback.display());

    return LIBRARY_COLLECTION
        .fetch(&ctx)
        .load(fallback, ctx)
        .map_err(|err| {
            Str::new(
                &ctx,
                format!("Failed to load compiled library: {err}"),
                true,
            )
            .into()
        });
}

pub fn load_from_path<'gc>(
    ctx: Context<'gc>,
    path: impl AsRef<Path>,
) -> Result<Value<'gc>, Value<'gc>> {
    let path = search_path(ctx, path).ok_or_else(|| Str::new(&ctx, "File not found", true))?;

    let x = load_thunk_from_path(ctx, path)?;

    Ok(x.unwrap())
}

fn load_thunk_from_path<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
) -> Result<Option<Value<'gc>>, Value<'gc>> {
    let filename = filename.as_ref();

    let scm_mstat = filename.metadata().ok().and_then(|m| m.modified().ok());

    let mut path = loc_load_compiled_path(ctx).get();
    let exts = loc_load_compiled_extensions(ctx).get();
    let hashed = hash_filename(&filename);
    while path.is_pair() {
        let elem = path.car().to_string();
        let candidate = Path::new(&elem).join(&hashed);

        let mut exts = exts;

        while exts.is_pair() {
            let ext = exts.car().to_string();
            let candidate = candidate.with_extension(&ext[..]);
            if candidate.exists() {
                let mstat = candidate.metadata().ok().and_then(|m| m.modified().ok());

                if let (Some(mstat), Some(scm_mstat)) = (mstat, scm_mstat) {
                    if mstat > scm_mstat {
                        return LIBRARY_COLLECTION
                            .fetch(&ctx)
                            .load(candidate, ctx)
                            .map(Some)
                            .map_err(|err| {
                                Str::new(
                                    &ctx,
                                    format!("Failed to load compiled library: {err}"),
                                    true,
                                )
                                .into()
                            });
                    }

                    let product = compile_file(ctx, &filename)?;
                    let buf = product.emit().map_err(|err| {
                        Str::new(
                            &ctx,
                            format!("Failed to emit compiled library: {err}"),
                            true,
                        )
                    })?;

                    let output = candidate.with_extension("o");
                    std::fs::write(&output, buf).map_err(|err| {
                        Str::new(
                            &ctx,
                            format!(
                                "Failed to write compiled library: {err} {}",
                                output.display()
                            ),
                            true,
                        )
                    })?;

                    let _clang = std::process::Command::new("clang")
                        .arg("-shared")
                        .arg("-fPIC")
                        .arg("-lcapy")
                        .arg("-o")
                        .arg(&candidate)
                        .arg(&output)
                        .status()
                        .map_err(|err| {
                            Str::new(&ctx, format!("Failed to invoke clang: {err}"), true)
                        })?;
                    println!("Compiled {} to {}", filename.display(), candidate.display());

                    return LIBRARY_COLLECTION
                        .fetch(&ctx)
                        .load(candidate, ctx)
                        .map(Some)
                        .map_err(|err| {
                            Str::new(
                                &ctx,
                                format!("Failed to load compiled library: {err}"),
                                true,
                            )
                            .into()
                        });
                }
            }

            exts = exts.cdr();
        }

        path = path.cdr();
    }

    let fallback = loc_compile_fallback_path(ctx).get().to_string();
    let candidate = Path::new(&fallback)
        .join(&hashed)
        .with_extension(&DYNLIB_EXTENSION);

    if candidate.exists() {
        if let (Some(mstat), Some(scm_mstat)) = (
            candidate
                .metadata()
                .ok()
                .filter(|m| m.is_file())
                .and_then(|m| m.modified().ok()),
            scm_mstat,
        ) {
            if mstat > scm_mstat {
                return LIBRARY_COLLECTION
                    .fetch(&ctx)
                    .load(candidate, ctx)
                    .map(Some)
                    .map_err(|err| {
                        Str::new(
                            &ctx,
                            format!("Failed to load compiled library: {err}"),
                            true,
                        )
                        .into()
                    });
            }
        }
    }

    let product = compile_file(ctx, &filename)?;
    let buf = product.emit().map_err(|err| {
        Str::new(
            &ctx,
            format!("Failed to emit compiled library: {err}"),
            true,
        )
    })?;

    let output = candidate.with_extension("o");
    std::fs::write(&output, buf).map_err(|err| {
        Str::new(
            &ctx,
            format!(
                "Failed to write compiled library: {err} {}",
                output.display()
            ),
            true,
        )
    })?;

    let _clang = std::process::Command::new("clang")
        .arg("-shared")
        .arg("-fPIC")
        .arg("-lcapy")
        .arg("-o")
        .arg(&candidate)
        .arg(&output)
        .status()
        .map_err(|err| Str::new(&ctx, format!("Failed to invoke clang: {err}"), true))?;
    println!("Compiled {} to {}", filename.display(), candidate.display());

    return LIBRARY_COLLECTION
        .fetch(&ctx)
        .load(candidate, ctx)
        .map(Some)
        .map_err(|err| {
            Str::new(
                &ctx,
                format!("Failed to load compiled library: {err}"),
                true,
            )
            .into()
        });
}

fn search_path<'gc>(ctx: Context<'gc>, filename: impl AsRef<Path>) -> Option<PathBuf> {
    let filename = filename.as_ref();

    if filename.is_absolute() && filename.exists() {
        return Some(filename.to_owned());
    }

    let mut path = loc_load_path(ctx).get();
    let exts = loc_load_extensions(ctx).get();
    while path.is_pair() {
        let elem = path.car().to_string();

        let candidate = Path::new(&elem).join(filename);

        let mut exts = exts;

        while exts.is_pair() {
            let ext = exts.car().to_string();

            let candidate = candidate.with_extension(&ext[1..]);
            if candidate.exists() {
                return Some(candidate);
            }
            exts = exts.cdr();
        }

        path = path.cdr();
    }

    None
}

pub extern "C-unwind" fn capy_load<'gc>(
    ctx: &Context<'gc>,
    _: Value<'gc>,
    rands: *const Value<'gc>,
    nrands: usize,
    retk: Value<'gc>,
    reth: Value<'gc>,
) -> NativeReturn<'gc> {
    unsafe {
        let rands = std::slice::from_raw_parts(rands, nrands);
        if rands.len() != 1 {
            todo!("not enough args")
        }

        let file = rands[0].to_string();
        let path = Path::new(&file).to_owned();

        let thunk = if let Some(dir) = path
            .parent()
            .filter(|dir| !dir.to_string_lossy().is_empty())
        {
            load_in_vicinity(*ctx, dir, path.file_name().unwrap().to_str().unwrap())
        } else {
            let cwd = std::env::current_dir().unwrap();
            load_in_vicinity(
                *ctx,
                cwd.as_path(),
                path.file_name().unwrap().to_str().unwrap(),
            )
        };
        match thunk {
            Ok(value) => ctx.return_call(value, [], Some([retk, reth])),
            Err(err) => ctx.return_call(reth, [err], None),
        }
    }
}
pub fn init<'gc>(ctx: Context<'gc>) {
    let proc =
        PROCEDURES
            .fetch(&ctx)
            .register_static_closure(ctx, capy_load, NativeLocation::unknown());

    define(ctx, "load", proc.into());
}
*/
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
/*
/// Given a file path, load and return a thunk that would evaluate top-level code in the file.
pub fn load_thunk_from_file<'gc>(
    ctx: Context<'gc>,
    file: impl AsRef<Path>,
) -> Result<Value<'gc>, Value<'gc>> {
    let file = file.as_ref();
    if !file.exists() {
        return Err(make_io_error(
            &ctx,
            "load-thunk-from-file",
            Str::new(&ctx, format!("File not found: {}", file.display()), true).into(),
            &[],
        ));
    }
    let libs = LIBRARY_COLLECTION.fetch(&ctx);

    libs.load(file, ctx).map_err(|err| {
        make_io_error(
            &ctx,
            "load-thunk-from-file",
            Str::new(&ctx, format!("Failed to load file: {err}"), true).into(),
            &[],
        )
    })
}



fn load_thunk_from_path<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    source_file_name: Option<impl AsRef<Path>>,
) -> Result<Value<'gc>, Value<'gc>> {
    let source_file_name = source_file_name.map(|p| p.as_ref().to_owned());
    let path = loc_load_compiled_path(ctx).get();
    if path.safe_list_length().is_none() {
        return Err(make_error(
            &ctx,
            Symbol::from_str(ctx, "%search-path").into(),
            Str::new(&ctx, &format!("path is not a proper list: {}", path), true).into(),
            &[path],
        ));
    }

    let extensions = loc_load_compiled_extensions(ctx).get();

    if path.safe_list_length().is_none() {
        return Err(make_error(
            &ctx,
            Symbol::from_str(ctx, "%search-path").into(),
            Str::new(
                &ctx,
                &format!("extensions is not a proper list: {}", extensions),
                true,
            )
            .into(),
            &[extensions],
        ));
    }

    let filename = filename.as_ref();

    if filename.is_absolute() {
        if path_has_extensions(extensions, filename) {
            return load_thunk_from_file(ctx, filename);
        }
    }

    let mut path = path;

    while path.is_pair() {
        let dir = PathBuf::from(path.car().to_string());

        let candidate = dir.join(filename);

        let mut ext = extensions;

        while ext.is_pair() {
            let ext_ = ext.car().to_string();
            let candidate = candidate.with_extension(&ext_);

            if let Some(meta) = candidate.metadata().ok().filter(|m| m.is_file())
                && candidate.exists()
            {
                if let Some(source_file_name) = source_file_name.as_ref()
                    && let Some(source_file_time) =
                        source_file_name.metadata().and_then(|m| m.modified()).ok()
                {
                    if !compiled_is_fresh(
                        source_file_name,
                        &candidate,
                        source_file_time,
                        meta.modified().unwrap_or(SystemTime::UNIX_EPOCH),
                    ) {
                        ext = ext.cdr();
                        continue;
                    }
                }

                if let Ok(thunk) = load_thunk_from_file(ctx, &candidate) {
                    return Ok(thunk);
                }
            }

            ext = ext.cdr();
        }

        path = path.cdr();
    }

    Ok(Value::new(false))
}

fn search_path<'gc>(
    ctx: Context<'gc>,
    path: Value<'gc>,
    filename: impl AsRef<Path>,
    extensions: Value<'gc>,
    require_exts: bool,
) -> Result<Option<PathBuf>, Value<'gc>> {
    if path.safe_list_length().is_none() {
        return Err(make_error(
            &ctx,
            Symbol::from_str(ctx, "%search-path").into(),
            Str::new(&ctx, &format!("path is not a proper list: {}", path), true).into(),
            &[path],
        ));
    }

    if extensions.safe_list_length().is_none() {
        return Err(make_error(
            &ctx,
            Symbol::from_str(ctx, "%search-path").into(),
            Str::new(
                &ctx,
                &format!("extensions is not a proper list: {}", extensions),
                true,
            )
            .into(),
            &[extensions],
        ));
    }

    let filename = filename.as_ref();

    if filename.is_absolute() && !require_exts
        || path_has_extensions(extensions, filename)
            && filename.exists()
            && filename.metadata().ok().filter(|m| m.is_file()).is_some()
    {
        return Ok(Some(filename.to_owned()));
    }

    let extensions = if filename.extension().is_some() {
        Value::null()
    } else {
        extensions
    };

    let mut path = path;

    while path.is_pair() {
        let dir = PathBuf::from(path.car().to_string());
        let candidate = dir.join(filename);
        if extensions.is_null() {
            if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some() {
                return Ok(Some(candidate));
            }
        } else {
            let mut exts = extensions;
            while exts.is_pair() {
                let ext = exts.car().to_string();
                let candidate = candidate.with_extension(&ext[1..]);
                if candidate.exists() && candidate.metadata().ok().filter(|m| m.is_file()).is_some()
                {
                    return Ok(Some(candidate));
                }
                exts = exts.cdr();
            }
        }

        path = path.cdr();
    }

    Ok(None)
}

fn path_has_extensions<'gc>(ext: Value<'gc>, path: impl AsRef<Path>) -> bool {
    let path = path.as_ref();
    if !path.exists() {
        return false;
    }
    let mut exts = ext;
    while exts.is_pair() {
        let ext = exts.car().to_string();
        if path.extension().is_some() && path.extension().unwrap().to_string_lossy() == ext {
            return true;
        }
        exts = exts.cdr();
    }
    false
}

pub fn primitive_load_path<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
) -> Result<Value<'gc>, Value<'gc>> {
    let filename = filename.as_ref();

    let Some(full_path) = search_path(
        ctx,
        loc_load_path(ctx).get(),
        filename,
        loc_load_extensions(ctx).get(),
        false,
    )?
    else {
        return Err(make_io_error(
            &ctx,
            "load",
            Str::new(
                &ctx,
                &format!("File not found: {}", filename.display()),
                true,
            )
            .into(),
            &[],
        ));
    };

    let hashed = hash_filename(&full_path);

    let mut compiled_thunk = load_thunk_from_path(ctx, &hashed, Some(&full_path))?;

    if compiled_thunk == Value::new(false) {
        let fallback_path = loc_compile_fallback_path(ctx).get().to_string();
        let fallback = Path::new(&fallback_path)
            .join(&hashed)
            .with_extension(DYNLIB_EXTENSION);

        if fallback.exists()
            && let Some(meta) = fallback.metadata().ok().filter(|m| m.is_file())
        {
            if let Some(source_file_time) = full_path.metadata().and_then(|m| m.modified()).ok() {
                if compiled_is_fresh(
                    &full_path,
                    &fallback,
                    source_file_time,
                    meta.modified().unwrap_or(SystemTime::UNIX_EPOCH),
                ) {
                    compiled_thunk = load_thunk_from_file(ctx, &fallback)?;
                }
            }
        }
    }

    if compiled_thunk.is::<Closure>() {
        Ok(compiled_thunk)
    } else {
        let fallback_path = loc_compile_fallback_path(ctx).get().to_string();
        let fallback = Path::new(&fallback_path)
            .join(&hashed)
            .with_extension(DYNLIB_EXTENSION);
        try_auto_compile(ctx, &full_path, &hashed, &fallback)
    }
}

fn try_auto_compile<'gc>(
    ctx: Context<'gc>,
    source: impl AsRef<Path>,
    hashed: impl AsRef<Path>,
    output: impl AsRef<Path>,
) -> Result<Value<'gc>, Value<'gc>> {
    let f = crate::compiler::compile_file(
        ctx,
        source.as_ref(),
        Some(current_module(ctx).get(ctx).downcast()),
    )?;
    let product = crate::compiler::compile_cps_to_object(ctx, f)?;
    let dest = output
        .as_ref()
        .join(hashed)
        .with_extension(DYNLIB_EXTENSION);
    crate::compiler::link_object_product(ctx, product, &dest)?;

    LIBRARY_COLLECTION
        .fetch(&ctx)
        .load(dest, ctx)
        .map_err(|err| {
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

pub fn load_in_vicinity<'gc>(
    ctx: Context<'gc>,
    dir: impl AsRef<Path>,
    filename: impl AsRef<Path>,
) -> Result<Value<'gc>, Value<'gc>> {
    let compiled_exteion = {
        let loc_compiled_extensions = loc_load_compiled_extensions(ctx).get();

        if (loc_compiled_extensions.is_null() || !loc_compiled_extensions.is_pair())
            || loc_compiled_extensions.car().is::<Str>()
                && loc_compiled_extensions.car().downcast::<Str>().len() == 0
        {
            DYNLIB_EXTENSION.to_string()
        } else {
            loc_compiled_extensions
                .car()
                .downcast::<Str>()
                .as_ref()
                .to_string()
        }
    };

    let fallback_file_name = |name: &str| {
        let loc = loc_compile_fallback_path(ctx).get();
        let hashed = hash_filename(name);
        if loc.is::<Str>() {
            Some(Path::new(&loc.to_string().as_ref()).join(&hashed))
        } else {
            None
        }
    };

    let filename = filename.as_ref();
    let dir = dir.as_ref();

    if !filename.is_absolute() {
        let path = dir.join(filename);


    }
}
*/

/// Given a file name find the full path to the file and the path to its compiled version.
pub fn find_path_to<'gc>(
    ctx: Context<'gc>,
    filename: impl AsRef<Path>,
    in_vicinity: Option<impl AsRef<Path>>,
) -> Result<Option<(PathBuf, PathBuf)>, Value<'gc>> {
    let filename = filename.as_ref();
    let dir = in_vicinity.map(|p| p.as_ref().to_owned());

    let mut source_path = None;
    let mut candidates = Vec::new();

    if filename.is_absolute() {
        if filename.is_file() {
            source_path = Some(filename.to_owned());
        }
    } else if filename.is_relative() && filename.is_file() {
        source_path = Some(filename.to_owned());
    } else {
        if let Some(dir) = dir.as_ref() {
            let path = dir.join(filename);
            println!("Searching in vicinity: {}", path.display());
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
            println!("Searching for {}", name.display());
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

pub fn load_thunk_in_vicinity<'gc>(
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
        let result = load_thunk_in_vicinity(nctx.ctx, filename, Some(in_vicinity));

        nctx.return_(result)
    }
);

use crate::compiler::ssa::compile_file;
use crate::list;
use crate::runtime::modules::{Variable, define};
use crate::runtime::value::*;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use rsgc::Gc;
use std::path::{Path, PathBuf};

use crate::{global, runtime::Context};
global!(
    pub loc_load_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-path", Value::null());
    pub loc_load_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-extensions", list!(ctx, Str::new(&ctx, ".scm", true)));
    pub loc_load_compiled_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-path", Value::null());
    pub loc_load_compiled_extensions<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%load-compiled-extensions", Value::null());

    pub loc_compile_fallback_path<'gc>: Gc<'gc, Variable<'gc>> = (ctx) define(ctx, "%compile-fallback-path", Value::null());
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
        loc_load_path(ctx).set(ctx, Str::new(&ctx, &path, true).into());
    }

    if let Ok(path) = std::env::var("CAPY_LOAD_COMPILED_PATH") {
        loc_load_compiled_path(ctx).set(ctx, Str::new(&ctx, &path, true).into());
    }
}

static DYNLIB_EXTENSION: &str = if cfg!(target_os = "linux") {
    "so"
} else if cfg!(windows) {
    "dll"
} else {
    "dylib"
};

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
    let proc = PROCEDURES
        .fetch(&ctx)
        .register_static_closure(ctx, capy_load);

    define(ctx, "load", proc.into());
}

fn hash_filename(path: impl AsRef<Path>) -> PathBuf {
    use sha3::{Digest, Sha3_256};

    let mut hasher = Sha3_256::new();
    hasher.update(path.as_ref().to_string_lossy().as_bytes());
    let bytes = hasher.finalize();

    let hex_string = hex::encode(bytes);
    PathBuf::from(hex_string)
}

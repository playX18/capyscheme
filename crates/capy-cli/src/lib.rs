use std::cell::Cell;
use std::ffi::{OsStr, OsString};
use std::io::Read;
#[cfg(not(target_os = "macos"))]
use std::io::{Seek, SeekFrom};
use std::path::Path;

use capy_sni::Scm;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CliKind {
    Bool,
    Value,
}

const MANUAL_FLAG_ROWS: &[(&str, &str, CliKind)] =
    &[("--debug", "CAPY_FASL_DEBUG", CliKind::Value)];

const COMPILER_ENTRY_NAME: &str = "enter-compiler";
/// `capy-rt --compile ...` is `capyc ...`.
const COMPILER_ENTRY_ARG: &str = "--compile";

/// Magic at the start of a FASL image
const FASL_MAGIC: &[u8; 8] = b"CAPYFSL\x03";
/// The flags byte follows the magic and the compression byte; this bit marks a static bundle.
const FASL_FLAGS_OFFSET: usize = FASL_MAGIC.len() + 1;
const FASL_FLAG_STATIC_BUNDLE: u8 = 1;

/// How much of a file is inspected to tell a static bundle from anything else.
const BUNDLE_HEAD_BYTES: u64 = 4096;

/// Where an executable made by `capyc --static --exe` keeps its bundle: a Mach-O section
/// on macOS, otherwise a trailer (the bundle, its length as a little-endian `u64`, then
/// this magic)
const EXE_SECTION: &str = "CAPYSTAT";
#[cfg(not(target_os = "macos"))]
const EXE_TRAILER_BYTES: u64 = 16;

fn flag_table() -> Vec<(&'static str, &'static str, CliKind)> {
    let mut table: Vec<(&'static str, &'static str, CliKind)> = Vec::new();
    for line in Scm::flags_cli_aliases().lines() {
        let mut parts = line.split_whitespace();
        if let (Some(flag), Some(env)) = (parts.next(), parts.next()) {
            // A missing kind means an older registry: value-requiring is
            // the previous behavior for every flag.
            let kind = if parts.next() == Some("bool") {
                CliKind::Bool
            } else {
                CliKind::Value
            };
            table.push((flag, env, kind));
        }
    }
    table.extend(MANUAL_FLAG_ROWS.iter().copied());
    table
}

pub fn run_cli(default_entry: &'static str) -> i32 {
    let raw_args: Vec<OsString> = std::env::args_os().collect();

    if let Some(bundle) = embedded_bundle() {
        return Scm::run_static(&bundle, &raw_args);
    }

    if let Some(bundle) = raw_args.get(1).and_then(|path| read_static_bundle(path)) {
        return Scm::run_static(&bundle, &raw_args[1..]);
    }

    // `capy [options] -s bundle args...` runs a bundle like `capy bundle args...`: a bundle
    // carries its own boot records, so it cannot run inside a booted image.
    let compiling = select_entrypoint(raw_args.clone(), default_entry).0 == COMPILER_ENTRY_NAME;
    if !compiling && let Some((index, bundle)) = script_bundle(&raw_args) {
        return Scm::run_static(&bundle, &raw_args[index..]);
    }

    if raw_args
        .iter()
        .skip(1)
        .any(|arg| arg == "--help-flags" || arg == "--dump-flags")
    {
        print!("{}", Scm::flags_help());
        return 0;
    }

    let args = match apply_flag_args(raw_args) {
        Ok(args) => args,
        Err(message) => {
            eprintln!("{message}");
            return 1;
        }
    };

    let (entry_name, args) = select_entrypoint(args, default_entry);
    let scm = Scm::new();
    let args_failed = Cell::new(false);
    let code = Cell::new(1);
    // The program runs as the main thread, so top-level code can block.
    scm.run_main(|| {
        code.set(scm.call_named(
            "boot cli",
            entry_name,
            |env, out_args| {
                match env.init_program_arguments(&args) {
                    Ok(_) => (),
                    Err(err) => {
                        eprintln!(
                            "Failed to initialize VM arguments: {err}",
                            err = env.value_to_utf8(err).expect("not a string")
                        );
                        args_failed.set(true);
                    }
                }
                let prog_args = env.program_arguments();
                let null = env.null();
                *out_args = env.cons(prog_args, null);
            },
            |env, success, result| {
                if args_failed.get() {
                    return 1;
                }
                if !success {
                    match env.value_to_utf8(result) {
                        Ok(message) => eprintln!("{message}"),
                        Err(_) => eprintln!("Unhandled exception (value not printable)"),
                    }
                }
                i32::from(!success)
            },
        ))
    });

    std::mem::forget(scm);
    code.get()
}

/// The script of `-s` or `--script` when it is a static bundle: the index of its
/// path in `args`, and its contents.
fn script_bundle(args: &[OsString]) -> Option<(usize, Vec<u8>)> {
    for (index, arg) in args.iter().enumerate().skip(1) {
        let Some(arg) = arg.to_str() else { continue };
        if arg == "-s" || arg == "--script" {
            let path = args.get(index + 1)?;
            return read_static_bundle(path).map(|bundle| (index + 1, bundle));
        }
    }
    None
}

/// The contents of `path` if it is a static bundle
fn read_static_bundle(path: &OsStr) -> Option<Vec<u8>> {
    let mut head = Vec::new();
    std::fs::File::open(path)
        .ok()?
        .take(BUNDLE_HEAD_BYTES)
        .read_to_end(&mut head)
        .ok()?;
    let payload = if head.starts_with(b"#!") {
        let newline = head.iter().position(|&byte| byte == b'\n')?;
        &head[newline + 1..]
    } else {
        &head[..]
    };
    if !payload.starts_with(FASL_MAGIC) || !names_static_bundle(payload) {
        return None;
    }
    std::fs::read(path).ok()
}

/// Whether the header of the FASL image `payload` has the static-bundle flag, as opposed to a
/// compiled library or program.
fn names_static_bundle(payload: &[u8]) -> bool {
    payload
        .get(FASL_FLAGS_OFFSET)
        .is_some_and(|flags| flags & FASL_FLAG_STATIC_BUNDLE != 0)
}

/// The bundle embedded in the running executable, if there is one
#[cfg(target_os = "macos")]
fn embedded_bundle() -> Option<Vec<u8>> {
    libsui::find_section(EXE_SECTION)
        .ok()
        .flatten()
        .map(<[u8]>::to_vec)
}

#[cfg(not(target_os = "macos"))]
fn embedded_bundle() -> Option<Vec<u8>> {
    let mut file = std::fs::File::open(std::env::current_exe().ok()?).ok()?;
    let len = file.seek(SeekFrom::End(0)).ok()?;
    if len < EXE_TRAILER_BYTES {
        return None;
    }
    file.seek(SeekFrom::End(-(EXE_TRAILER_BYTES as i64))).ok()?;
    let mut trailer = [0u8; EXE_TRAILER_BYTES as usize];
    file.read_exact(&mut trailer).ok()?;
    if &trailer[8..] != EXE_SECTION.as_bytes() {
        return None;
    }
    let payload_len = u64::from_le_bytes(trailer[..8].try_into().ok()?);
    let start = len
        .checked_sub(EXE_TRAILER_BYTES)?
        .checked_sub(payload_len)?;
    file.seek(SeekFrom::Start(start)).ok()?;
    let mut bundle = vec![0u8; payload_len as usize];
    file.read_exact(&mut bundle).ok()?;
    Some(bundle)
}

fn select_entrypoint(
    mut args: Vec<OsString>,
    default_entry: &'static str,
) -> (&'static str, Vec<OsString>) {
    if args.get(1).is_some_and(|arg| arg == COMPILER_ENTRY_ARG) {
        args.remove(1);
        return ("enter-compiler", args);
    }

    if args
        .first()
        .and_then(|arg| Path::new(arg).file_name())
        .is_some_and(|name| name == "capyc")
    {
        return ("enter-compiler", args);
    }

    (default_entry, args)
}

fn apply_flag_args(args: Vec<OsString>) -> Result<Vec<OsString>, String> {
    let table = flag_table();
    let (sets, output) = translate(args, &table)?;
    for (env, value) in sets {
        Scm::flags_set(std::ffi::OsStr::new(env), &value)?;
    }
    Ok(output)
}

#[allow(clippy::type_complexity)]
fn translate(
    args: Vec<OsString>,
    table: &[(&'static str, &'static str, CliKind)],
) -> Result<(Vec<(&'static str, OsString)>, Vec<OsString>), String> {
    let mut sets = Vec::new();
    let mut output = Vec::with_capacity(args.len());
    let mut iter = args.into_iter();
    let Some(program) = iter.next() else {
        return Ok((sets, output));
    };
    output.push(program);

    let mut pass_through = false;
    while let Some(arg) = iter.next() {
        if pass_through {
            output.push(arg);
            continue;
        }

        let arg_str = arg.to_string_lossy();
        if let Some(rest) = arg_str.strip_prefix("--no-") {
            // `--no-flag` clears a boolean flag, anything else (including
            // `--no-` before a value flag) passes through untouched.
            let flag = format!("--{rest}");
            if let Some((_, env, CliKind::Bool)) = table.iter().find(|(known, _, _)| *known == flag)
            {
                sets.push((*env, OsString::from("0")));
                continue;
            }
        }
        if let Some((env, value, kind)) = split_flag(&arg_str, table) {
            let value = match (value, kind) {
                (Some(value), _) => OsString::from(value),
                (None, CliKind::Bool) => OsString::from("1"),
                (None, CliKind::Value) => iter
                    .next()
                    .ok_or_else(|| format!("{arg_str} requires a value"))?,
            };
            sets.push((env, value));
            continue;
        }

        pass_through = arg_str == "--";
        output.push(arg);
    }

    Ok((sets, output))
}

fn split_flag<'a>(
    arg: &'a str,
    table: &[(&'static str, &'static str, CliKind)],
) -> Option<(&'static str, Option<&'a str>, CliKind)> {
    for (flag, env, kind) in table {
        if arg == *flag {
            return Some((*env, None, *kind));
        }
        if let Some(value) = arg.strip_prefix(&format!("{flag}=")) {
            return Some((*env, Some(value), *kind));
        }
    }
    None
}

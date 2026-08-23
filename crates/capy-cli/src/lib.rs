//! Shared thin-CLI logic for `capy` / `capyc`.

use std::ffi::OsString;
use std::path::Path;

use capy_sni::Scm;

const MANUAL_FLAG_ROWS: &[(&str, &str)] = &[
    ("--gc-plan", "MMTK_PLAN"),
    ("--gc-trigger", "MMTK_GC_TRIGGER"),
    ("--debug", "CAPY_FASL_DEBUG"),
];

const COMPILER_ENTRY_ARG: &str = "--capy-compiler-entrypoint";

fn flag_table() -> Vec<(&'static str, &'static str)> {
    let mut table: Vec<(&'static str, &'static str)> = Vec::new();
    for line in Scm::flags_cli_aliases().lines() {
        if let Some((flag, env)) = line.split_once(' ') {
            table.push((flag, env));
        }
    }
    table.extend(MANUAL_FLAG_ROWS.iter().copied());
    table
}

pub fn run_cli(default_entry: &'static str) -> i32 {
    let raw_args: Vec<OsString> = std::env::args_os().collect();

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
    scm.call_named(
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
                }
            }
            let prog_args = env.program_arguments();
            let null = env.null();
            *out_args = env.cons(prog_args, null);
        },
        |env, success, result| {
            if !success {
                // Last-resort reporter: reachable only when no Scheme-level
                // exception handler printed the condition (e.g. a boot-time
                // failure before the CLI/REPL handlers are installed).
                match env.value_to_utf8(result) {
                    Ok(message) => eprintln!("{message}"),
                    Err(_) => eprintln!("Unhandled exception (value not printable)"),
                }
            }
            i32::from(!success)
        },
    )
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
    let mut output = Vec::with_capacity(args.len());
    let mut iter = args.into_iter();
    let Some(program) = iter.next() else {
        return Ok(output);
    };
    output.push(program);

    let mut pass_through = false;
    while let Some(arg) = iter.next() {
        if pass_through {
            output.push(arg);
            continue;
        }

        let arg_str = arg.to_string_lossy();
        if let Some((env, value)) = split_flag(&arg_str, &table) {
            let value = match value {
                Some(value) => OsString::from(value),
                None => iter
                    .next()
                    .ok_or_else(|| format!("{arg_str} requires a value"))?,
            };
            unsafe {
                std::env::set_var(env, value);
            }
            continue;
        }

        pass_through = arg_str == "--";
        output.push(arg);
    }

    Ok(output)
}

fn split_flag<'a>(
    arg: &'a str,
    table: &[(&'static str, &'static str)],
) -> Option<(&'static str, Option<&'a str>)> {
    for (flag, env) in table {
        if arg == *flag {
            return Some((*env, None));
        }
        if let Some(value) = arg.strip_prefix(&format!("{flag}=")) {
            return Some((*env, Some(value)));
        }
    }
    None
}

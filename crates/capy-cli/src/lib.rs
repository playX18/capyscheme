//! Shared thin-CLI logic for `capy` / `capyc`.

use std::ffi::OsString;
use std::path::Path;

use capy_sni::Scm;

const GC_ARGS: &[(&str, &str)] = &[
    ("--gc-plan", "MMTK_PLAN"),
    ("--gc-trigger", "MMTK_GC_TRIGGER"),
    ("--gc-max-heap", "CAPY_GC_MAX_HEAP"),
    ("--gc-heuristic", "CAPY_GC_HEURISTIC"),
    ("--gc-min-free-percent", "CAPY_GC_MIN_FREE_PERCENT"),
    ("--gc-init-free-percent", "CAPY_GC_INIT_FREE_PERCENT"),
    (
        "--gc-allocation-threshold-percent",
        "CAPY_GC_ALLOCATION_THRESHOLD_PERCENT",
    ),
    ("--gc-alloc-spike-percent", "CAPY_GC_ALLOC_SPIKE_PERCENT"),
    ("--gc-learning-steps", "CAPY_GC_LEARNING_STEPS"),
    (
        "--gc-guaranteed-interval-ms",
        "CAPY_GC_GUARANTEED_INTERVAL_MS",
    ),
];

const COMPILER_ENTRY_ARG: &str = "--capy-compiler-entrypoint";

pub fn run_cli(default_entry: &'static str) -> i32 {
    let args = match apply_gc_args(std::env::args_os().collect()) {
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
        |_env, success, _result| i32::from(!success),
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

fn apply_gc_args(args: Vec<OsString>) -> Result<Vec<OsString>, String> {
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
        if let Some((env, value)) = split_gc_arg(&arg_str) {
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

fn split_gc_arg(arg: &str) -> Option<(&'static str, Option<&str>)> {
    for (flag, env) in GC_ARGS {
        if arg == *flag {
            return Some((*env, None));
        }
        if let Some(value) = arg.strip_prefix(&format!("{flag}=")) {
            return Some((*env, Some(value)));
        }
    }
    None
}

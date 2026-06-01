use std::ffi::OsString;

use crate::{
    prelude::Context,
    runtime::{Scheme, value::Value, vm::base::program_arguments_fluid},
};

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

pub fn run_cli() -> i32 {
    let args = match apply_gc_args(std::env::args_os().collect()) {
        Ok(args) => args,
        Err(message) => {
            eprintln!("{message}");
            return 1;
        }
    };

    let (entry_name, args) = select_entrypoint(args);
    let scm = Scheme::new();
    scm.call(
        "boot cli",
        entry_name,
        |ctx, real_args| {
            init_program_arguments(ctx, &args);
            real_args.push(program_arguments_fluid(ctx).get(ctx));
        },
        |_, result| i32::from(result.is_err()),
    )
}

fn select_entrypoint(mut args: Vec<OsString>) -> (&'static str, Vec<OsString>) {
    if args.get(1).is_some_and(|arg| arg == COMPILER_ENTRY_ARG) {
        args.remove(1);
        return ("enter-compiler", args);
    }

    if args
        .first()
        .and_then(|arg| std::path::Path::new(arg).file_name())
        .is_some_and(|name| name == "capyc")
    {
        return ("enter-compiler", args);
    }

    ("enter", args)
}

fn init_program_arguments<'gc>(ctx: Context<'gc>, args: &[OsString]) {
    let mut list = Value::null();
    for arg in args.iter().rev() {
        let value = ctx.str(&arg.to_string_lossy());
        list = Value::cons(ctx, value, list);
    }
    program_arguments_fluid(ctx).set(ctx, list);
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
            // This runs before the runtime starts worker threads.
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

use capy::{
    prelude::*,
    runtime::{Scheme, vm::base::program_arguments_fluid},
};

fn find_heap(exe_dir: &std::path::Path) -> Option<std::path::PathBuf> {
    let mut candidates = Vec::with_capacity(2);
    candidates.push(exe_dir.join("capy.heap"));

    if let Some(var) = std::env::var_os("CAPY_SYSROOT") {
        let sysroot = std::path::PathBuf::from(var);
        candidates.push(sysroot.join("capy.heap"));
    }

    for candidate in &candidates {
        if candidate.exists() {
            return Some(candidate.to_owned());
        }
    }

    if let Some(cwd) = std::env::current_dir().ok() {
        let candidate = cwd.join("capy.heap");
        if candidate.exists() {
            return Some(candidate);
        }
    }

    None
}

fn main() {
    let exe_path = match std::env::current_exe() {
        Ok(path) => path,
        Err(err) => {
            eprintln!("Failed to get current executable path: {}", err);
            std::process::exit(1);
        }
    };

    let exe_dir = match exe_path.parent() {
        Some(dir) => dir.to_owned(),
        None => {
            eprintln!("Failed to get executable directory");
            std::process::exit(1);
        }
    };

    let heap_file = match find_heap(&exe_dir) {
        Some(path) => path,
        None => {
            eprintln!("Failed to find capy.heap");
            std::process::exit(1);
        }
    };

    let image = std::fs::read(heap_file).unwrap();

    let scm = Scheme::from_image(&image);

    scm.call_value(
        |ctx, fargs| {
            let args = std::env::args().rev().fold(Value::null(), |acc, arg| {
                Value::cons(*ctx, Str::new(&ctx, arg, true).into(), acc)
            });

            program_arguments_fluid(*ctx).set(*ctx, args);
            fargs.push(args);

            ctx.accumulator()
        },
        |_ctx, res| match res {
            Ok(val) => {
                println!("Result: {}", val);
            }

            Err(err) => {
                eprintln!("Error: {}", err);
            }
        },
    );
}

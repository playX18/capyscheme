use std::{
    fs::{self, OpenOptions},
    io::BufWriter,
    path::Path,
};

use crate::{
    cps::term::FuncRef,
    cps_ssa::lower_cps,
    runtime::{
        Context,
        value::{Str, Value},
        vm::thunks::make_io_error,
    },
};

pub(crate) fn compile_cps_to_cps_ssa_file<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    output: impl AsRef<Path>,
) -> Result<(), Value<'gc>> {
    let output = output.as_ref();
    if let Some(parent) = output.parent()
        && !parent.exists()
    {
        fs::create_dir_all(parent).map_err(|err| {
            make_io_error(
                ctx,
                "compile-cps-ssa",
                Str::new(
                    *ctx,
                    format!(
                        "Cannot create output directory '{}': {}",
                        parent.display(),
                        err
                    ),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    }

    let module = lower_cps(ctx, cps).map_err(|err| {
        make_io_error(
            ctx,
            "compile-cps-ssa",
            Str::new(
                *ctx,
                format!("Failed to lower CPS to CPS-SSA for '{}': {}", output.display(), err),
                true,
            )
            .into(),
            &[],
        )
    })?;

    let file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(output)
        .map_err(|err| {
            make_io_error(
                ctx,
                "compile-cps-ssa",
                Str::new(
                    *ctx,
                    format!("Cannot open output file '{}': {}", output.display(), err),
                    true,
                )
                .into(),
                &[],
            )
        })?;
    let writer = BufWriter::new(file);
    module.write_to_writer(ctx, writer).map_err(|err| {
        make_io_error(
            ctx,
            "compile-cps-ssa",
            Str::new(
                *ctx,
                format!(
                    "Cannot write CPS-SSA artifact '{}' to disk: {}",
                    output.display(),
                    err
                ),
                true,
            )
            .into(),
            &[],
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{self, Context, Scheme};
    use std::{fs, path::PathBuf, sync::Mutex};
    use uuid::Uuid;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    struct TempDir {
        path: PathBuf,
    }

    impl TempDir {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!("capy-cps-ssa-test-{}", Uuid::new_v4()));
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &std::path::Path {
            &self.path
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                runtime::init(ctx);
            }
            f(ctx);
        });
    }

    #[test]
    fn emits_cps_ssa_file_that_round_trips() {
        with_ctx(|ctx| {
            let temp = TempDir::new();
            let source = temp.path().join("sample.scm");
            let output = temp.path().join("sample.cscm");
            fs::write(&source, "42\n").unwrap();

            let cps = crate::compiler::compile_file(ctx, &source, None).unwrap();
            compile_cps_to_cps_ssa_file(ctx, cps, &output).unwrap();

            let file = fs::File::open(&output).unwrap();
            let module = crate::cps_ssa::Module::read_from_reader(ctx, std::io::BufReader::new(file))
                .unwrap();

            assert_eq!(module.format_version, crate::cps_ssa::FORMAT_VERSION);
            assert!(!module.procs.is_empty());
        });
    }
}

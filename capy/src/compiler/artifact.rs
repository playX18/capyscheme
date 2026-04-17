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
        stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
        value::{Str, Value},
        vm::thunks::make_io_error,
    },
};

pub(crate) fn compile_cps_to_cps_ssa_file<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    output: impl AsRef<Path>,
) -> Result<(), Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::CpsSsaEmit);
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
                format!(
                    "Failed to lower CPS to CPS-SSA for '{}': {}",
                    output.display(),
                    err
                ),
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
    use crate::runtime::{
        self, Context, GLOBAL_STATS, Scheme,
        stats::{TEST_RUNTIME_LOCK, runtime_stats_snapshot, set_runtime_stats_enabled},
    };
    use std::{fs, path::PathBuf};
    use uuid::Uuid;

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
        let _guard = TEST_RUNTIME_LOCK
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

    fn reset_stats() {
        GLOBAL_STATS.lock().reset_for_test();
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
            let module =
                crate::cps_ssa::Module::read_from_reader(ctx, std::io::BufReader::new(file))
                    .unwrap();

            assert_eq!(module.format_version, crate::cps_ssa::FORMAT_VERSION);
            assert!(!module.procs.is_empty());
        });
    }

    #[test]
    fn runtime_stats_report_cps_ssa_emit_without_link_time() {
        with_ctx(|ctx| {
            reset_stats();

            let temp = TempDir::new();
            let source = temp.path().join("sample.scm");
            let output = temp.path().join("sample.cscm");
            fs::write(&source, "42\n").unwrap();

            set_runtime_stats_enabled(&ctx.state().stats, true);
            ctx.stats.start_compilation();

            let cps = crate::compiler::compile_file(ctx, &source, None).unwrap();
            compile_cps_to_cps_ssa_file(ctx, cps, &output).unwrap();

            ctx.stats.end_compilation();

            let snapshot = runtime_stats_snapshot(Some(&ctx.state().stats)).unwrap();
            let report = snapshot.to_string();
            assert!(snapshot.compilation > std::time::Duration::ZERO);
            assert!(snapshot.cps_ssa_emit > std::time::Duration::ZERO);
            assert_eq!(snapshot.link, std::time::Duration::ZERO);
            assert!(report.contains("Compilation:"));
            assert!(report.contains("CPS-SSA Emit:"));
        });
    }
}

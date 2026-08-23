use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::compiler::pipeline::DumpArtifactsOptions;
use crate::utils::flags;

thread_local! {
    static COMPILE_DUMP_STEM: RefCell<Option<String>> = const { RefCell::new(None) };
}

pub fn set_compile_dump_stem(stem: Option<String>) {
    COMPILE_DUMP_STEM.with(|slot| *slot.borrow_mut() = stem);
}

pub fn begin_compilation_artifact(destination: &Path) {
    let stem = destination
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| destination.display().to_string());
    set_compile_dump_stem(Some(stem));
}

pub fn compile_dump_stem() -> Option<String> {
    COMPILE_DUMP_STEM.with(|slot| slot.borrow().clone())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DumpFamily {
    /// Per-compilation artifacts keyed to the destination file.
    CompileArtifacts,
    /// SBBV CFG stage dumps.
    Sbbv,
    /// GCPS contification dumps.
    GcpsContify,
    /// Compiler-failure evidence (always written).
    VerifyFail,
}

impl DumpFamily {
    fn configured_directory(self) -> Option<PathBuf> {
        let pick = |path: Option<&'static Path>| path.map(Path::to_path_buf);
        match self {
            DumpFamily::CompileArtifacts => {
                pick(flags::compile_dump_dir()).or_else(|| pick(flags::dump_dir()))
            }
            DumpFamily::Sbbv => pick(flags::sbbv_dump_dir())
                .or_else(|| pick(flags::compile_dump_dir()))
                .or_else(|| pick(flags::dump_dir())),
            DumpFamily::GcpsContify => pick(flags::gcps_dump_dir())
                .or_else(|| pick(flags::compile_dump_dir()))
                .or_else(|| pick(flags::dump_dir())),
            DumpFamily::VerifyFail => pick(flags::dump_dir()),
        }
    }

    fn default_directory(self) -> PathBuf {
        match self {
            DumpFamily::VerifyFail => PathBuf::from("/tmp/capy-verify-fail"),
            _ => PathBuf::from("capy-dumps"),
        }
    }

    fn limit(self) -> usize {
        match self {
            DumpFamily::CompileArtifacts => flags::compile_dump_limit(),
            DumpFamily::Sbbv => flags::sbbv_dump_limit(),
            DumpFamily::GcpsContify => flags::gcps_dump_limit(),
            DumpFamily::VerifyFail => usize::MAX,
        }
    }

    fn tag(self) -> &'static str {
        match self {
            DumpFamily::CompileArtifacts => "compile",
            DumpFamily::Sbbv => "sbbv",
            DumpFamily::GcpsContify => "gcps",
            DumpFamily::VerifyFail => "verify-fail",
        }
    }
}

pub struct DumpSink {
    family: DumpFamily,
    slots: AtomicUsize,
    limit_warns: AtomicUsize,
}

/// Per-compilation artifact dumps (`CAPY_COMPILE_DUMP*`).
pub const COMPILE_ARTIFACTS: DumpSink = DumpSink::new(DumpFamily::CompileArtifacts);
/// SBBV CFG stage dumps (`CAPY_SBBV_DUMP*`).
pub const SBBV: DumpSink = DumpSink::new(DumpFamily::Sbbv);
/// GCPS contification dumps (`CAPY_GCPS_DUMP*`).
pub const GCPS_CONTIFY: DumpSink = DumpSink::new(DumpFamily::GcpsContify);
/// Compiler-failure evidence (`CAPY_DUMP_DIR` or `/tmp/capy-verify-fail`).
pub const VERIFY_FAIL: DumpSink = DumpSink::new(DumpFamily::VerifyFail);

impl DumpSink {
    pub(crate) const fn new(family: DumpFamily) -> Self {
        Self {
            family,
            slots: AtomicUsize::new(0),
            limit_warns: AtomicUsize::new(0),
        }
    }

    fn directory(&self) -> Option<PathBuf> {
        let dir = self
            .family
            .configured_directory()
            .unwrap_or_else(|| self.family.default_directory());
        if let Err(err) = std::fs::create_dir_all(&dir) {
            return None;
        }
        Some(dir)
    }

    fn next_slot(&self) -> Option<usize> {
        let slot = self.slots.fetch_add(1, Ordering::Relaxed) + 1;
        if slot <= self.family.limit() {
            return Some(slot);
        }

        None
    }

    pub(crate) fn next_slot_id(&self) -> Option<usize> {
        self.next_slot()
    }

    pub fn resolve(&self, label: &str, ext: &str) -> Option<PathBuf> {
        let slot = self.next_slot()?;
        self.resolve_at(slot, label, ext)
    }

    pub fn resolve_at(&self, slot: usize, label: &str, ext: &str) -> Option<PathBuf> {
        let dir = self.directory()?;
        let stem = compile_dump_stem().unwrap_or_else(|| "compile".to_string());
        Some(dir.join(format!("{slot:04}-{stem}-{label}.{ext}")))
    }

    pub fn write(&self, kind: &str, label: &str, ext: &str, contents: &str) -> Option<PathBuf> {
        let path = self.resolve(label, ext)?;
        self.write_at(kind, &path, contents);
        Some(path)
    }

    pub fn write_at(&self, kind: &str, path: &Path, contents: &str) {
        if let Err(err) = std::fs::write(path, contents) {
            self.warn_write(kind, path, &err);
            return;
        }
        self.log_path(kind, path);
    }

    /// `;; TRACE (capy)@<family>: {kind} -> {path}` on stderr.
    pub fn log_path(&self, kind: &str, path: &Path) {
        eprintln!(
            ";; TRACE  (capy)@{}: {kind} -> {}",
            self.family.tag(),
            path.display()
        );
    }

    /// `;; WARN (capy)@<family>: failed to write {kind} dump to {path}`.
    pub fn warn_write(&self, kind: &str, path: &Path, err: &std::io::Error) {
        eprintln!(
            ";; WARN  (capy)@{}: failed to write {kind} dump to {}: {err}",
            self.family.tag(),
            path.display()
        );
    }
}

/// Per-compilation artifact path: `{dump-dir}/{destination-stem}{suffix}`.
pub fn resolve_artifact_dump_path(destination: &Path, suffix: &str) -> PathBuf {
    COMPILE_ARTIFACTS
        .directory()
        .map(|dir| {
            let stem = destination
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
                .unwrap_or_else(|| destination.display().to_string());
            dir.join(format!("{stem}{suffix}"))
        })
        .unwrap_or_else(|| PathBuf::from(format!("{}{suffix}", destination.display())))
}

/// SBBV stage dump path: `{dir}/{slot:04}-{stem}-{label}.ssa.txt`.
pub fn resolve_sbbv_dump_path(label: &str) -> Option<PathBuf> {
    SBBV.resolve(label, "ssa.txt")
}

/// Compiler-failure evidence path: `{dir}/{name}.{ext}` (never slot-limited).
pub fn resolve_verify_fail_path(name: &str, ext: &str) -> Option<PathBuf> {
    let dir = VERIFY_FAIL.directory()?;
    Some(dir.join(format!("{name}.{ext}")))
}

pub fn compile_dump_options() -> DumpArtifactsOptions {
    let words = flags::compile_dump();
    if words.is_empty() {
        return DumpArtifactsOptions::default();
    }
    let mut options = DumpArtifactsOptions {
        enabled: true,
        include_unoptimized: flags::compile_dump_noopt(),
        ..DumpArtifactsOptions::default()
    };
    for word in words {
        match *word {
            "all" | "1" => {
                options.dump_ir = true;
                options.dump_graph = true;
                options.dump_ssa = true;
                options.dump_cranelift = true;
                options.dump_disassembly = true;
                options.include_unoptimized = true;
            }
            "noopt" => options.include_unoptimized = true,
            other => {
                options.enable(other);
            }
        }
    }
    options
}

pub fn merge_compile_dump_options(mut base: DumpArtifactsOptions) -> DumpArtifactsOptions {
    let from_env = compile_dump_options();
    base.enabled |= from_env.enabled;
    base.include_unoptimized |= from_env.include_unoptimized;
    base.dump_ir |= from_env.dump_ir;
    base.dump_graph |= from_env.dump_graph;
    base.dump_ssa |= from_env.dump_ssa;
    base.dump_cranelift |= from_env.dump_cranelift;
    base.dump_disassembly |= from_env.dump_disassembly;
    base
}

pub fn sbbv_dump_requested() -> bool {
    !flags::sbbv_dump().is_empty()
}

pub fn sbbv_dump_stage_enabled(stage: &str) -> bool {
    flags::sbbv_dump()
        .iter()
        .any(|word| sbbv_stage_matches(word, stage))
}

fn sbbv_stage_matches(word: &str, stage: &str) -> bool {
    match word {
        "1" | "all" => true,
        w => stage == w || stage.starts_with(w) || stage.ends_with(w),
    }
}

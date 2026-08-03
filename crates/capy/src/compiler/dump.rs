//! Compile-time artifact dumping controlled by environment variables.
//!
//! ```text
//! CAPY_COMPILE_DUMP=ssa,gcps,ir,clif,asm,all
//! CAPY_COMPILE_DUMP_DIR=/tmp/capy-dumps
//! CAPY_COMPILE_DUMP_LIMIT=32
//! CAPY_SBBV_DUMP=1|pre|expand|specialize|all
//! CAPY_SBBV_DUMP_DIR=/tmp/capy-sbbv-dumps
//! ```
//!
//! When `CAPY_SBBV_DUMP` / `CAPY_COMPILE_DUMP` is set without a directory,
//! dumps go to `./capy-dumps` (or `$CAPY_DUMP_DIR`).

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::compiler::pipeline::DumpArtifactsOptions;

// Keep env var names alive under LTO so release binaries can still dump.
#[used]
static ENV_CAPY_SBBV_DUMP: &str = "CAPY_SBBV_DUMP";
#[used]
static ENV_CAPY_SBBV_DUMP_DIR: &str = "CAPY_SBBV_DUMP_DIR";
#[used]
static ENV_CAPY_COMPILE_DUMP: &str = "CAPY_COMPILE_DUMP";
#[used]
static ENV_CAPY_COMPILE_DUMP_DIR: &str = "CAPY_COMPILE_DUMP_DIR";
#[used]
static ENV_CAPY_DUMP_DIR: &str = "CAPY_DUMP_DIR";

thread_local! {
    static COMPILE_DUMP_STEM: RefCell<Option<String>> = const { RefCell::new(None) };
}

static COMPILE_DUMP_COUNT: AtomicUsize = AtomicUsize::new(0);
static SBBV_DUMP_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Remember the artifact stem for the current compile (e.g. `boot.fasl`).
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

fn truthy_env(name: &str) -> bool {
    match std::env::var(name).ok().as_deref() {
        Some("1" | "true" | "on" | "yes" | "all") => true,
        Some("0" | "false" | "off" | "no" | "none" | "") => false,
        Some(_) => true,
        None => false,
    }
}

fn dump_limit(name: &str) -> usize {
    std::env::var(name)
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(usize::MAX)
}

pub fn compile_dump_options_from_env() -> DumpArtifactsOptions {
    let Some(raw) = std::env::var(ENV_CAPY_COMPILE_DUMP).ok() else {
        return DumpArtifactsOptions::default();
    };

    let mut options = DumpArtifactsOptions {
        enabled: true,
        include_unoptimized: truthy_env("CAPY_COMPILE_DUMP_NOOPT"),
        ..DumpArtifactsOptions::default()
    };

    for part in raw.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        match part.to_ascii_lowercase().as_str() {
            "all" => {
                options.dump_ir = true;
                options.dump_graph = true;
                options.dump_ssa = true;
                options.dump_cranelift = true;
                options.dump_disassembly = true;
                options.include_unoptimized = true;
            }
            "noopt" => options.include_unoptimized = true,
            _ => {
                options.enable(part);
            }
        }
    }

    options
}

pub fn merge_compile_dump_options(mut base: DumpArtifactsOptions) -> DumpArtifactsOptions {
    let from_env = compile_dump_options_from_env();
    if !from_env.enabled {
        return base;
    }
    base.enabled = true;
    base.include_unoptimized |= from_env.include_unoptimized;
    base.dump_ir |= from_env.dump_ir;
    base.dump_graph |= from_env.dump_graph;
    base.dump_ssa |= from_env.dump_ssa;
    base.dump_cranelift |= from_env.dump_cranelift;
    base.dump_disassembly |= from_env.dump_disassembly;
    base
}

fn default_dump_directory() -> PathBuf {
    std::env::var_os(ENV_CAPY_DUMP_DIR)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("capy-dumps"))
}

pub fn compile_dump_directory() -> Option<PathBuf> {
    std::env::var_os(ENV_CAPY_COMPILE_DUMP_DIR)
        .map(PathBuf::from)
        .or_else(|| std::env::var_os(ENV_CAPY_DUMP_DIR).map(PathBuf::from))
        .or_else(|| {
            // If dumps are requested, always have a writable destination.
            compile_dump_options_from_env()
                .enabled
                .then(default_dump_directory)
        })
}

pub fn sbbv_dump_directory() -> Option<PathBuf> {
    std::env::var_os(ENV_CAPY_SBBV_DUMP_DIR)
        .map(PathBuf::from)
        .or_else(compile_dump_directory)
        .or_else(|| sbbv_dump_requested().then(default_dump_directory))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SbbvDumpMode {
    Disabled,
    All,
    Stages,
}

static SBBV_DUMP_MODE: std::sync::LazyLock<(SbbvDumpMode, Vec<String>)> =
    std::sync::LazyLock::new(|| match std::env::var(ENV_CAPY_SBBV_DUMP).ok().as_deref() {
        None | Some("") | Some("0") | Some("off") | Some("false") | Some("none") => {
            (SbbvDumpMode::Disabled, Vec::new())
        }
        Some("1") | Some("on") | Some("true") | Some("all") => (SbbvDumpMode::All, Vec::new()),
        Some(other) => (
            SbbvDumpMode::Stages,
            other
                .split(',')
                .map(|part| part.trim().to_ascii_lowercase())
                .collect(),
        ),
    });

pub(crate) fn sbbv_dump_requested() -> bool {
    SBBV_DUMP_MODE.0 != SbbvDumpMode::Disabled
}

pub fn sbbv_dump_stage_enabled(stage: &str) -> bool {
    match SBBV_DUMP_MODE.0 {
        SbbvDumpMode::Disabled => false,
        SbbvDumpMode::All => true,
        SbbvDumpMode::Stages => SBBV_DUMP_MODE.1.iter().any(|s| s == stage),
    }
}

pub fn next_compile_dump_slot() -> Option<usize> {
    let index = COMPILE_DUMP_COUNT.fetch_add(1, Ordering::Relaxed) + 1;
    let limit = dump_limit("CAPY_COMPILE_DUMP_LIMIT");
    (index <= limit).then_some(index)
}

pub fn next_sbbv_dump_slot() -> Option<usize> {
    let index = SBBV_DUMP_COUNT.fetch_add(1, Ordering::Relaxed) + 1;
    let limit = dump_limit("CAPY_SBBV_DUMP_LIMIT");
    (index <= limit).then_some(index)
}

pub fn resolve_artifact_dump_path(destination: &Path, suffix: &str) -> PathBuf {
    if let Some(dir) = compile_dump_directory() {
        if let Err(err) = std::fs::create_dir_all(&dir) {
            eprintln!(
                ";; WARN  (capy)@compile: cannot create dump dir {}: {err}",
                dir.display()
            );
        }
        let stem = destination
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_else(|| destination.display().to_string());
        dir.join(format!("{stem}{suffix}"))
    } else {
        PathBuf::from(format!("{}{suffix}", destination.display()))
    }
}

pub fn resolve_sbbv_dump_path(label: &str) -> Option<PathBuf> {
    let dir = sbbv_dump_directory()?;
    if let Err(err) = std::fs::create_dir_all(&dir) {
        eprintln!(
            ";; WARN  (capy)@sbbv: cannot create dump dir {}: {err}",
            dir.display()
        );
        return None;
    }
    let Some(slot) = next_sbbv_dump_slot() else {
        // Limit hit — stay quiet after the first few so stage-0 logs stay readable.
        static WARNED: AtomicUsize = AtomicUsize::new(0);
        if WARNED.fetch_add(1, Ordering::Relaxed) < 3 {
            eprintln!(
                ";; WARN  (capy)@sbbv: CAPY_SBBV_DUMP_LIMIT reached; skipping further dumps \
                 (including {label})"
            );
        }
        return None;
    };
    let stem = compile_dump_stem().unwrap_or_else(|| "compile".to_string());
    Some(dir.join(format!("{slot:04}-{stem}-{label}.ssa.txt")))
}

pub fn log_dump_path(kind: &str, path: &Path) {
    // Always print to stderr so dumps are visible without RUST_LOG.
    eprintln!(";; TRACE  (capy)@compile: {kind} -> {}", path.display());
    log::info!(";; TRACE  (capy)@compile: {kind} -> {}", path.display());
}

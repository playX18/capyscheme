//! Runtime for handling coverage
//!
//! ```text
//! [0..8)   unit uid     unique per compiled unit, keeps tables non-`equal?`
//! [8..16)  counters     thin pointer to this process's counter slice
//! [16..24) epoch        this process's epoch once registered, else stale/0
//! ```
//!
//! The unit's toplevel starts with `(%cov-register! table records)`, which
//! allocates the counters in Rust, copies the probe records, and stamps the
//! table with the process epoch. Probes are `(%cov-hit table index)`. A table
//! loaded from a FASL written by another process carries a stale epoch, so a
//! hit before registration is ignored rather than following a dead pointer.
//!
//! At exit the registry is written as LCOV (`DA`, `FN`/`FNDA`, `BRDA`).

use std::collections::BTreeMap;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{LazyLock, Mutex, Once};

use crate::runtime::objectmodel::{ByteVector, Str, Symbol, Value, Vector};

/// Size of a coverage table bytevector.
pub const TABLE_BYTES: usize = 24;

/// Probe kinds, as encoded in the compiler's records.
pub const KIND_LINE: i32 = 0;
pub const KIND_FUNCTION: i32 = 1;
pub const KIND_BRANCH: i32 = 2;

struct Probe {
    file: String,
    line: i32,
    col: i32,
    kind: i32,
    arm: i32,
    name: Option<String>,
}

struct Unit {
    probes: Vec<Probe>,
    counters: &'static [AtomicU64],
}

static REGISTRY: LazyLock<Mutex<Vec<Unit>>> = LazyLock::new(|| Mutex::new(Vec::new()));

/// Nonzero, different in every process (and unlikely to collide with a stale
/// epoch baked into a FASL by another process).
static EPOCH: LazyLock<u64> = LazyLock::new(|| {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    let mixed = (nanos ^ ((std::process::id() as u64) << 32)).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    mixed | 1
});

static UID: AtomicU64 = AtomicU64::new(0);

/// A fresh unit uid for a table the compiler is about to emit.
pub fn fresh_unit_uid() -> u64 {
    let n = UID.fetch_add(1, Ordering::Relaxed);
    (*EPOCH).rotate_left(17) ^ n.wrapping_mul(0xD6E8_FEB8_6659_FD93)
}

/// The three table words of `table`, if it is a coverage table.
fn table_slots<'gc>(table: Value<'gc>) -> Option<&'gc [AtomicU64; 3]> {
    let bv = table.try_as::<ByteVector>()?;
    if bv.len() < TABLE_BYTES {
        return None;
    }
    let ptr = bv.contents().to_mut_ptr::<u8>();
    if (ptr as usize) % std::mem::align_of::<AtomicU64>() != 0 {
        return None;
    }
    // SAFETY: at least 24 bytes, 8-aligned, and only ever accessed atomically.
    Some(unsafe { &*(ptr as *const [AtomicU64; 3]) })
}

#[inline]
pub fn hit<'gc>(table: Value<'gc>, index: Value<'gc>) {
    let Some(slots) = table_slots(table) else {
        return;
    };
    if slots[2].load(Ordering::Acquire) != *EPOCH {
        return;
    }
    let counters = slots[1].load(Ordering::Relaxed) as *const &'static [AtomicU64];
    let Some(i) = index.int32() else {
        return;
    };
    // SAFETY: the epoch matches, so `register` in this process stored a
    // leaked, never-freed slice pointer before publishing the epoch.
    let counters = unsafe { *counters };
    if let Some(counter) = counters.get(i as usize) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

fn value_string<'gc>(v: Value<'gc>) -> Option<String> {
    if let Some(s) = v.try_as::<Str>() {
        Some(s.to_string())
    } else {
        v.try_as::<Symbol>().map(|s| s.to_string())
    }
}

fn parse_probe<'gc>(record: Value<'gc>) -> Option<Probe> {
    let fields = record.try_as::<Vector>()?;
    let fields = fields.as_slice();
    if fields.len() < 6 {
        return None;
    }
    Some(Probe {
        file: value_string(fields[0])?,
        line: fields[1].int32()?,
        col: fields[2].int32().unwrap_or(0),
        kind: fields[3].int32()?,
        arm: fields[4].int32().unwrap_or(0),
        name: value_string(fields[5]),
    })
}

pub fn register<'gc>(table: Value<'gc>, records: Value<'gc>) {
    let Some(slots) = table_slots(table) else {
        return;
    };
    let epoch = *EPOCH;
    if slots[2].load(Ordering::Acquire) == epoch {
        return;
    }
    let mut registry = REGISTRY.lock().unwrap_or_else(|e| e.into_inner());
    if slots[2].load(Ordering::Acquire) == epoch {
        return;
    }
    let probes: Vec<Probe> = records
        .try_as::<Vector>()
        .map(|v| {
            v.as_slice()
                .iter()
                .filter_map(|r| parse_probe(*r))
                .collect()
        })
        .unwrap_or_default();
    let counters: &'static [AtomicU64] =
        Box::leak((0..probes.len()).map(|_| AtomicU64::new(0)).collect());
    let handle: &'static &'static [AtomicU64] = Box::leak(Box::new(counters));
    slots[1].store(handle as *const _ as u64, Ordering::Relaxed);
    slots[2].store(epoch, Ordering::Release);
    registry.push(Unit { probes, counters });
    drop(registry);
    install_exit_hook();
}

unsafe extern "C" {
    fn atexit(callback: extern "C" fn());
}

extern "C" fn dump_at_exit() {
    if let Err(err) = dump(&out_path()) {
        eprintln!(";; capy coverage: cannot write report: {err}");
    }
}

fn install_exit_hook() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        // SAFETY: the hook only reads atomics, locks the registry and writes a file.
        unsafe { atexit(dump_at_exit) };
    });
}

fn out_path() -> PathBuf {
    let template = crate::utils::flags::coverage_out()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| "capy-cov.%p.info".to_owned());
    PathBuf::from(template.replace("%p", &std::process::id().to_string()))
}

#[derive(Default)]
struct FileCov {
    lines: BTreeMap<i32, u64>,
    functions: BTreeMap<(i32, String), u64>,
    /// `(line, col)` of the branch site -> per-arm counts.
    branches: BTreeMap<(i32, i32), BTreeMap<i32, u64>>,
}

fn canonical_file(file: &str) -> String {
    std::fs::canonicalize(file)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| file.to_owned())
}

/// Render the current counters as LCOV.
pub fn render_lcov() -> String {
    let registry = REGISTRY.lock().unwrap_or_else(|e| e.into_inner());
    let mut files: BTreeMap<String, FileCov> = BTreeMap::new();
    let mut canon: BTreeMap<String, String> = BTreeMap::new();
    for unit in registry.iter() {
        for (probe, counter) in unit.probes.iter().zip(unit.counters) {
            let count = counter.load(Ordering::Relaxed);
            let file = canon
                .entry(probe.file.clone())
                .or_insert_with(|| canonical_file(&probe.file))
                .clone();
            let cov = files.entry(file).or_default();
            // Several probes can land on one line (nested scopes, macro
            // copies, several units of one file): a line ran as often as
            // its busiest probe.
            let mut line_hit = |line: i32, count: u64| {
                let slot = cov.lines.entry(line).or_insert(0);
                *slot = (*slot).max(count);
            };
            match probe.kind {
                KIND_LINE => line_hit(probe.line, count),
                KIND_FUNCTION => {
                    line_hit(probe.line, count);
                    let name = probe
                        .name
                        .clone()
                        .unwrap_or_else(|| format!("lambda@{}:{}", probe.line, probe.col));
                    let slot = cov.functions.entry((probe.line, name)).or_insert(0);
                    *slot = (*slot).max(count);
                }
                KIND_BRANCH => {
                    let arms = cov.branches.entry((probe.line, probe.col)).or_default();
                    let slot = arms.entry(probe.arm).or_insert(0);
                    *slot = (*slot).max(count);
                }
                _ => {}
            }
        }
    }
    drop(registry);

    let mut out = String::new();
    for (file, cov) in &files {
        out.push_str("TN:\n");
        out.push_str(&format!("SF:{file}\n"));
        for ((line, name), _) in &cov.functions {
            out.push_str(&format!("FN:{line},{name}\n"));
        }
        for ((_, name), count) in &cov.functions {
            out.push_str(&format!("FNDA:{count},{name}\n"));
        }
        out.push_str(&format!("FNF:{}\n", cov.functions.len()));
        out.push_str(&format!(
            "FNH:{}\n",
            cov.functions.values().filter(|c| **c > 0).count()
        ));
        let (mut brf, mut brh) = (0, 0);
        for (block, ((line, _), arms)) in cov.branches.iter().enumerate() {
            for (arm, count) in arms {
                brf += 1;
                if *count > 0 {
                    brh += 1;
                }
                out.push_str(&format!("BRDA:{line},{block},{arm},{count}\n"));
            }
        }
        out.push_str(&format!("BRF:{brf}\nBRH:{brh}\n"));
        for (line, count) in &cov.lines {
            out.push_str(&format!("DA:{line},{count}\n"));
        }
        out.push_str(&format!("LF:{}\n", cov.lines.len()));
        out.push_str(&format!(
            "LH:{}\n",
            cov.lines.values().filter(|c| **c > 0).count()
        ));
        out.push_str("end_of_record\n");
    }
    out
}

/// Write the LCOV report to `path`, creating parent directories.
pub fn dump(path: &std::path::Path) -> std::io::Result<()> {
    let report = render_lcov();
    if report.is_empty() {
        return Ok(());
    }
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        std::fs::create_dir_all(parent)?;
    }
    let mut file = std::fs::File::create(path)?;
    file.write_all(report.as_bytes())
}

use std::sync::OnceLock;
use std::sync::atomic::Ordering::Relaxed;

use super::super::template_image::loaded;
use super::super::unit::Tier;
use crate::utils::flags;

/// The register allocator Tier 2 runs.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum RegAllocKind {
    /// Wimmer–Franz linear scan with lifetime holes and splitting.
    LinearScan,
    /// Greedy register allocator based on JSC/LLVM Greedy regalloc.
    Greedy,
}

impl RegAllocKind {
    pub(crate) fn name(self) -> &'static str {
        match self {
            RegAllocKind::LinearScan => "linear",
            RegAllocKind::Greedy => "greedy",
        }
    }

    fn parse(name: &str) -> Option<Self> {
        Some(match name {
            "lsra" => RegAllocKind::LinearScan,
            "greedy" => RegAllocKind::Greedy,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct TierSet(u8);

impl TierSet {
    pub(crate) const BASELINE: TierSet = TierSet(1 << Tier::Baseline as u8);

    pub(crate) fn contains(self, tier: Tier) -> bool {
        self.0 & (1 << tier as u8) != 0
    }
}

pub(crate) fn tiers() -> TierSet {
    static TIERS: OnceLock<TierSet> = OnceLock::new();
    *TIERS.get_or_init(|| {
        let mut set = TierSet::BASELINE;
        if flags::opt_jit() && Tier::Optimizing.implemented() {
            set.0 |= 1 << Tier::Optimizing as u8;
        }
        set
    })
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct PassToggles {
    disabled: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ParsedPasses {
    pub(crate) toggles: PassToggles,
    pub(crate) invalid: Vec<String>,
}

impl PassToggles {
    pub(crate) fn parse(raw: &str) -> ParsedPasses {
        let mut disabled = Vec::new();
        let mut invalid = Vec::new();
        for entry in raw
            .split(',')
            .map(str::trim)
            .filter(|entry| !entry.is_empty())
        {
            match entry.strip_prefix('-').map(str::trim) {
                Some(name) if !name.is_empty() => disabled.push(name.to_string()),
                _ => invalid.push(entry.to_string()),
            }
        }
        ParsedPasses {
            toggles: PassToggles { disabled },
            invalid,
        }
    }

    pub(crate) fn enabled(&self, pass: &str) -> bool {
        !self.disabled.iter().any(|name| name == pass)
    }
}

/// Tier 2 (optimizing) settings
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct OptConfig {
    /// Baseline entries or back-edges before a unit is considered for Tier 2.
    pub(crate) threshold: usize,

    /// From `CAPY_OPT_VERSION`: version a loop on the invariant guards in its
    /// body, running a guard-free clone when the tests pass and the original
    /// loop otherwise. Off by default, no significant perf gains yet
    pub(crate) version: bool,
    /// From `CAPY_OPT_UNSWITCH`: split a loop on a loop-invariant branch into
    /// two clones with the branch folded. Off by default
    pub(crate) unswitch: bool,
    /// From `CAPY_OPT_UNROLL_ALL`: replicate small innermost loops that are
    /// not counted.
    pub(crate) unroll_all: bool,
    /// From `CAPY_OPT_SPEC_HOIST`: hoist invariant guards out of conditional
    /// loop blocks, deopting to the loop entry.
    pub(crate) spec_hoist: bool,
    /// From `CAPY_OPT_PREDICATE`: loop predication of bounds checks.
    pub(crate) predicate: bool,
    /// From `CAPY_OPT_LAYOUT`: reorder blocks after allocation for
    /// fallthrough, using static loop weights
    pub(crate) layout: bool,
    /// From `CAPY_OPT_REGALLOC`: the allocator to run, followed by any others
    /// to run in compare mode on the same function (validated and measured,
    /// not emitted). Never empty.
    pub(crate) regallocs: Vec<RegAllocKind>,
    /// From `CAPY_OPT_EVICT`: linear scan may evict a cheaper active piece.
    pub(crate) evict: bool,
    /// Always true in debug builds.
    pub(crate) verify: bool,
    /// Stage names whose IR/LIR is written to `CAPY_JIT_DUMP_DIR`.
    pub(crate) dump: Vec<String>,
    /// Keep per-instruction LIR/IR text so listings can annotate machine
    /// code (`CAPY_OPT_ANNOTATE`). Off by default
    pub(crate) annotate: bool,
    /// Deoptimize at every n-th guard
    pub(crate) stress_deopt: usize,
}

/// Parses `CAPY_OPT_REGALLOC` into the allocator list. Unknown or duplicate
/// names are dropped; the first known name wins, and an empty list means the
/// default linear scan.
fn parse_regalloc(spec: &str) -> Vec<RegAllocKind> {
    let mut out: Vec<RegAllocKind> = Vec::new();
    for name in spec.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        match RegAllocKind::parse(name) {
            Some(kind) if !out.contains(&kind) => out.push(kind),
            Some(_) => {}
            None => {
                crate::heap::logging::ensure_max_level(log::LevelFilter::Warn);
                log::warn!("unknown register allocator `{name}` in CAPY_OPT_REGALLOC");
            }
        }
    }
    if out.is_empty() {
        out.push(RegAllocKind::LinearScan);
    }
    out
}

impl OptConfig {
    /// The allocator whose result is emitted.
    pub(crate) fn regalloc(&self) -> RegAllocKind {
        self.regallocs[0]
    }

    /// The allocators to run in compare mode after the active one.
    pub(crate) fn regalloc_compare(&self) -> &[RegAllocKind] {
        &self.regallocs[1..]
    }

    pub(crate) fn from_flags() -> Self {
        let dump = flags::opt_dump()
            .unwrap_or("")
            .split(',')
            .map(str::trim)
            .filter(|stage| !stage.is_empty())
            .map(str::to_string)
            .collect();
        let config = OptConfig {
            threshold: flags::jit_opt_threshold(),
            version: flags::opt_version(),
            unswitch: flags::opt_unswitch(),
            unroll_all: flags::opt_unroll_all(),
            spec_hoist: flags::opt_spec_hoist(),
            predicate: flags::opt_predicate(),
            layout: flags::opt_layout(),
            regallocs: parse_regalloc(flags::opt_regalloc().unwrap_or("linear")),
            evict: flags::opt_evict(),
            verify: flags::opt_verify() || cfg!(debug_assertions),
            dump,
            annotate: flags::opt_annotate(),
            stress_deopt: flags::opt_stress_deopt(),
        };
        config
    }
}

pub(crate) fn opt() -> &'static OptConfig {
    static OPT: OnceLock<OptConfig> = OnceLock::new();
    OPT.get_or_init(|| OptConfig::from_flags())
}

pub(crate) enum Bisect {
    Allowed,
    PastLimit(usize),
    FilteredOut,
}

pub(crate) fn bisect(ordinal: usize, metadata: impl FnOnce() -> String) -> Bisect {
    let limit = flags::jit_limit();
    if ordinal > limit {
        return Bisect::PastLimit(limit);
    }
    match flags::jit_filter() {
        Some(filter) if !metadata().contains(filter) => Bisect::FilteredOut,
        _ => Bisect::Allowed,
    }
}

pub(crate) static COMPILES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
pub(crate) static RESUME_HIT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
pub(crate) static RESUME_MISS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
pub(crate) static RESUME_UNCOMPILED: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
/// Helper slow paths (`slow::resume_at`) and control slow paths
/// (`slow::stage_control`).
pub(crate) static SLOW_HELPER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
pub(crate) static SLOW_CONTROL: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// Lazily generated stubs.
pub(crate) static MATH_STUBS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// Tier 2 units installed
pub(crate) static OPT_INSTALLS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// Entries into installed Tier 2 code through its normal entry. The prologue
/// bumps it with a plain add, and only when `CAPY_JIT_STATS` was on at its
/// compile, so a run without stats emits no counter. Mutator-only.
pub(crate) static OPT_ENTRIES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// Deoptimizing exits taken from Tier 2 code (`capy_opt_deopt`).
pub(crate) static OPT_DEOPTS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// Objects rebuilt from remat descriptors during deopt.
pub(crate) static OPT_REMAT_OBJECTS: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
/// Transfers from baseline frames into Tier 2 code at a loop (`osr_entry`).
pub(crate) static OPT_OSR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
/// OSR transfers refused because the unit was retired while the frame
/// looked its loop up. They fall back to baseline.
pub(crate) static OPT_OSR_REFUSED_INVALID: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
/// Tier 2 builds that failed (a pass, the verifier, lowering or emission).
pub(crate) static OPT_BUILD_FAILURES: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
/// Wall time inside `OptimizingCompiler::build`, in microseconds.
pub(crate) static OPT_BUILD_MICROS: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
/// Wall time inside `regalloc::allocate` (legalization, intervals, the
/// allocator, move resolution and the stack maps), in microseconds.
pub(crate) static OPT_REGALLOC_MICROS: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

extern "C" fn print_jit_stats() {
    let compiles = COMPILES.load(Relaxed);
    let hit = RESUME_HIT.load(Relaxed);
    let miss = RESUME_MISS.load(Relaxed);
    let uncompiled = RESUME_UNCOMPILED.load(Relaxed);
    let helper = SLOW_HELPER.load(Relaxed);
    let control = SLOW_CONTROL.load(Relaxed);
    let stubs = MATH_STUBS.load(Relaxed);
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "jit stats: {compiles} compiles, {hit} resume hit, {miss} resume miss, \
         {uncompiled} resume uncompiled, {helper} helper slow paths, {control} control slow paths, \
         {stubs} math stubs"
    );
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "tier 2 stats: {} installs, {} entries, {} deopt exits, {} OSR transfers ({} refused invalid), {} remat objects, {} build failures",
        OPT_INSTALLS.load(Relaxed),
        OPT_ENTRIES.load(Relaxed),
        OPT_DEOPTS.load(Relaxed),
        OPT_OSR.load(Relaxed),
        OPT_OSR_REFUSED_INVALID.load(Relaxed),
        OPT_REMAT_OBJECTS.load(Relaxed),
        OPT_BUILD_FAILURES.load(Relaxed),
    );
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "tier 2 time: {} us compiling, {} us in regalloc::allocate",
        OPT_BUILD_MICROS.load(Relaxed),
        OPT_REGALLOC_MICROS.load(Relaxed),
    );
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "{}",
        crate::runtime::codegen::opt::passes::pass_time_report()
    );
    let regalloc = crate::runtime::codegen::opt::regalloc::stats::report();
    if !regalloc.is_empty() {
        log::info!(target: crate::heap::logging::STATS_LOG_TARGET, "{regalloc}");
    }
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "{}",
        crate::runtime::codegen::cache::report()
    );
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "{}",
        crate::runtime::codegen::opt::build::calls::report()
    );
    // Per-site deopt totals: the guard kind, the call site and the inlined
    // code block, so an inlined exit can be told apart from a root one.
    for line in crate::runtime::codegen::opt::deopt::exits::report_exit_sites() {
        log::info!(target: crate::heap::logging::STATS_LOG_TARGET, "{line}");
    }
}

unsafe extern "C" {
    fn atexit(callback: extern "C" fn());
}

/// Install the `CAPY_JIT_STATS=1` exit report, once per process.
pub fn maybe_report_jit_stats() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    if flags::jit_stats() {
        // SAFETY: the callback only reads atomics and logs.
        ONCE.call_once(|| unsafe { atexit(print_jit_stats) });
    }
}

/// Addresses in the interpreter image that compiled code jumps to.
pub(crate) struct TemplateInterpreterAddresses {
    pub(crate) map: usize,
    pub(crate) interpreter_return: usize,
    pub(crate) values_error: usize,
}

pub(crate) fn template_interpreter() -> &'static TemplateInterpreterAddresses {
    static ADDRESSES: OnceLock<TemplateInterpreterAddresses> = OnceLock::new();
    ADDRESSES.get_or_init(|| {
        let image = loaded().expect("the JIT runs on the template interpreter");
        let addr = |name: &str| {
            image
                .symbol_addr(name)
                .unwrap_or_else(|| panic!(" {name} missing from the interpreter image"))
                as usize
        };
        TemplateInterpreterAddresses {
            map: addr("opcode_map"),
            interpreter_return: addr("interp_return"),
            values_error: addr("values_error"),
        }
    })
}

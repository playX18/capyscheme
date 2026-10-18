use std::sync::OnceLock;
use std::sync::atomic::Ordering::Relaxed;

use super::super::template_image::loaded;
use super::super::unit::Tier;
use crate::utils::flags;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct TierSet(u8);

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ParsedTiers {
    pub(crate) set: TierSet,
    pub(crate) unknown: Vec<String>,
}

impl TierSet {
    pub(crate) const BASELINE: TierSet = TierSet(1 << Tier::Baseline as u8);

    pub(crate) fn contains(self, tier: Tier) -> bool {
        self.0 & (1 << tier as u8) != 0
    }

    pub(crate) fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub(crate) fn iter(self) -> impl Iterator<Item = Tier> {
        Tier::ALL
            .into_iter()
            .filter(move |tier| self.contains(*tier))
    }

    pub(crate) fn parse(raw: &str) -> ParsedTiers {
        let mut set = TierSet(0);
        let mut unknown = Vec::new();
        for name in raw
            .split(',')
            .map(str::trim)
            .filter(|name| !name.is_empty())
        {
            match Tier::from_name(name) {
                Some(tier) => set.0 |= 1 << tier as u8,
                None => unknown.push(name.to_string()),
            }
        }
        ParsedTiers { set, unknown }
    }
}

pub(crate) fn tiers() -> TierSet {
    static TIERS: OnceLock<TierSet> = OnceLock::new();
    *TIERS.get_or_init(|| {
        let Some(raw) = flags::jit_tiers() else {
            return TierSet::BASELINE;
        };
        let parsed = TierSet::parse(raw);
        for name in &parsed.unknown {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " CAPY_JIT_TIERS: unknown tier {name:?} ignored");
        }
        for tier in parsed.set.iter().filter(|tier| !tier.implemented()) {
            log::warn!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "jit: CAPY_JIT_TIERS: tier {} is not available in this build",
                tier.name()
            );
        }
        if parsed.set.is_empty() {
            if !raw.trim().is_empty() {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " CAPY_JIT_TIERS={raw:?} names no tier, using baseline");
            }
            return TierSet::BASELINE;
        }
        parsed.set
    })
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

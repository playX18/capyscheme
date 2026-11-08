//! Partitioned accounting for compiled code.

use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::Relaxed;

use super::jit::registry::LIVE_CODE_BYTES;
use crate::utils::flags;

/// The kind of compiled code a byte count belongs to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Partition {
    Baseline,
    Optimizing,
    Lazy,
    Stub,
}

impl Partition {
    fn counter(self) -> &'static AtomicUsize {
        match self {
            Self::Baseline => &BASELINE,
            Self::Optimizing => &OPTIMIZING,
            Self::Lazy => &LAZY,
            Self::Stub => &STUB,
        }
    }

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Baseline => "baseline",
            Self::Optimizing => "optimizing",
            Self::Lazy => "lazy",
            Self::Stub => "stub",
        }
    }
}

static BASELINE: AtomicUsize = AtomicUsize::new(0);
static OPTIMIZING: AtomicUsize = AtomicUsize::new(0);
static LAZY: AtomicUsize = AtomicUsize::new(0);
static STUB: AtomicUsize = AtomicUsize::new(0);

/// Compiles refused because the budget was full after a reclaim.
static BUDGET_STOPS: AtomicUsize = AtomicUsize::new(0);
/// Retired units, for the exit report.
static RETIRED: AtomicUsize = AtomicUsize::new(0);
/// Units released by the reclaim schedule.
static RECLAIMED: AtomicUsize = AtomicUsize::new(0);

/// Charge `bytes` to `partition` and to the total.
pub(crate) fn charge(partition: Partition, bytes: usize) {
    partition.counter().fetch_add(bytes, Relaxed);
    LIVE_CODE_BYTES.fetch_add(bytes, Relaxed);
}

/// Release `bytes` from `partition` and from the total.
pub(crate) fn release(partition: Partition, bytes: usize) {
    partition.counter().fetch_sub(bytes, Relaxed);
    LIVE_CODE_BYTES.fetch_sub(bytes, Relaxed);
}

pub(crate) fn bytes(partition: Partition) -> usize {
    partition.counter().load(Relaxed)
}

pub(crate) fn total() -> usize {
    LIVE_CODE_BYTES.load(Relaxed)
}

pub(crate) fn budget() -> usize {
    flags::jit_max_code()
}

/// Whether a new allocation of `bytes` may proceed. On pressure, drain once
/// (the collector may have queued dead owners since the last check), then
/// refuse and count the stop. A unit that grows past the budget after this
/// check is allowed to finish; the next compile sees the excess.
pub(crate) fn admit(partition: Partition, bytes: usize) -> bool {
    let _ = partition;
    if total().saturating_add(bytes) <= budget() {
        return true;
    }
    super::reclaim::drain_if_needed();
    if total().saturating_add(bytes) <= budget() {
        return true;
    }
    BUDGET_STOPS.fetch_add(1, Relaxed);
    false
}

/// Count a unit whose entry was retired.
pub(crate) fn note_retire() {
    RETIRED.fetch_add(1, Relaxed);
}

/// Count a unit the reclaim schedule released.
pub(crate) fn note_reclaim() {
    RECLAIMED.fetch_add(1, Relaxed);
}

pub(crate) fn report() -> String {
    format!(
        "code cache: {} bytes (baseline {}, optimizing {}, lazy {}, stub {}), \
         {} budget stops, {} retired, {} reclaimed",
        total(),
        bytes(Partition::Baseline),
        bytes(Partition::Optimizing),
        bytes(Partition::Lazy),
        bytes(Partition::Stub),
        BUDGET_STOPS.load(Relaxed),
        RETIRED.load(Relaxed),
        RECLAIMED.load(Relaxed),
    )
}

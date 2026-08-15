

use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};

static COLLECTIONS: AtomicUsize = AtomicUsize::new(0);
static BYTES_RECLAIMED: AtomicUsize = AtomicUsize::new(0);
static GC_CPU_MS: AtomicU64 = AtomicU64::new(0);
static GC_REAL_MS: AtomicU64 = AtomicU64::new(0);

// Per-pause start cells (the trigger-policy hooks are single-threaded with
// respect to collection lifecycle).
static PAUSE_START_CPU_MS: AtomicU64 = AtomicU64::new(0);
static PAUSE_START_REAL_MS: AtomicU64 = AtomicU64::new(0);

// Live heap right before release, for bytes-reclaimed accounting.
static RELEASE_USED: AtomicUsize = AtomicUsize::new(0);
static RELEASE_RECORDED: AtomicBool = AtomicBool::new(false);

/// Record the start of a stop-the-world pause.
pub(crate) fn mark_pause_start(real_ms: u64, cpu_ms: u64) {
    PAUSE_START_REAL_MS.store(real_ms, Ordering::Relaxed);
    PAUSE_START_CPU_MS.store(cpu_ms, Ordering::Relaxed);
}

/// Record the end of a stop-the-world pause and accumulate its duration into
/// the process-wide "collecting" times.
pub(crate) fn mark_pause_end(real_ms: u64, cpu_ms: u64) {
    let real_start = PAUSE_START_REAL_MS.load(Ordering::Relaxed);
    let cpu_start = PAUSE_START_CPU_MS.load(Ordering::Relaxed);
    GC_REAL_MS.fetch_add(real_ms.saturating_sub(real_start), Ordering::Relaxed);
    GC_CPU_MS.fetch_add(cpu_ms.saturating_sub(cpu_start), Ordering::Relaxed);
}

/// Record the live heap just before the cycle's release work.
pub(crate) fn mark_gc_release(used_bytes: usize) {
    RELEASE_USED.store(used_bytes, Ordering::Relaxed);
    RELEASE_RECORDED.store(true, Ordering::Relaxed);
}

/// Record the end of a GC cycle: count it and accumulate the bytes reclaimed
/// since the release point.
pub(crate) fn mark_gc_end(used_bytes_after: usize) {
    COLLECTIONS.fetch_add(1, Ordering::Relaxed);

    if RELEASE_RECORDED.swap(false, Ordering::Relaxed) {
        let reclaimed = RELEASE_USED
            .load(Ordering::Relaxed)
            .saturating_sub(used_bytes_after);
        BYTES_RECLAIMED.fetch_add(reclaimed, Ordering::Relaxed);
    }
}

/// Snapshot of the process-wide GC counters.
///
/// Returns `(collections, bytes-allocated, bytes-reclaimed, gc-cpu-ms,
/// gc-real-ms)` where `bytes-allocated` is the current live heap usage.
pub(crate) fn snapshot(used_bytes_now: usize) -> (usize, usize, usize, u64, u64) {
    (
        COLLECTIONS.load(Ordering::Relaxed),
        used_bytes_now,
        BYTES_RECLAIMED.load(Ordering::Relaxed),
        GC_CPU_MS.load(Ordering::Relaxed),
        GC_REAL_MS.load(Ordering::Relaxed),
    )
}

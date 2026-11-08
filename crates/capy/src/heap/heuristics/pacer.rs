//! `tax` allocation pacer.
//!
//! A bounded mutator stall while a concurrent mark is running, modelled on
//! Shenandoah's pacer.  Triggers can only decide when a
//! GC starts and they cannot slow an allocation that is already happening. The
//! pacer solves this: during a concurrent cycle each allocation slow path
//! is *taxed*, and a mutator that has run ahead of the collector sleeps (at
//! most `max_delay`) before it is allowed to continue.
//!
//! MMTk exposes no marking-progress counter, so progress is proxied by elapsed
//! time against the expected cycle duration (upper bound of recent cycle
//! times). The allowance at time `t` is
//!
//! ```text
//! allowed(t) = slack + (free - slack) * min(1, t / expected) / surcharge
//! ```
//!
//! where `slack` is the untaxed share of free space and `surcharge` (default 1.1) keeps the
//! mutator slightly behind the collector.  Allocation past the allowance sleeps
//! with exponential backoff capped at `max_delay`, then proceeds anyway: the
//! pacer never blocks forever, so a slow collector degrades into the existing
//! full-GC fallback instead of a deadlock.
//!
//! The state is global because mutators cannot reach the boxed trigger policy.

use std::{
    sync::{
        OnceLock,
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
    },
    time::{Duration, Instant},
};

pub(crate) const DEFAULT_MAX_DELAY_MS: usize = 10;
pub(crate) const DEFAULT_CYCLE_SLACK_PERCENT: usize = 10;
pub(crate) const DEFAULT_SURCHARGE: f64 = 1.1;
/// Cycle length assumed before any GC has been measured.
pub(crate) const BOOTSTRAP_EXPECTED_CYCLE: Duration = Duration::from_millis(50);
const FIRST_BACKOFF: Duration = Duration::from_millis(1);

#[derive(Clone, Copy, Debug)]
pub(crate) struct PacerConfig {
    pub(crate) max_delay: Duration,
    pub(crate) cycle_slack_percent: usize,
    pub(crate) surcharge: f64,
}

/// What a finished pacing window cost the mutators.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct PaceSummary {
    pub(crate) stalls: u64,
    pub(crate) stalled: Duration,
}

pub(crate) struct Pacer {
    active: AtomicBool,
    armed_at_nanos: AtomicU64,
    expected_nanos: AtomicU64,
    free_bytes: AtomicUsize,
    slack_bytes: AtomicUsize,
    surcharge_milli: AtomicU64,
    max_delay_nanos: AtomicU64,
    claimed_bytes: AtomicUsize,
    stalls: AtomicU64,
    stalled_nanos: AtomicU64,
    exhausted: AtomicBool,
}

pub(crate) static PACER: Pacer = Pacer::new();

fn now_nanos() -> u64 {
    static ORIGIN: OnceLock<Instant> = OnceLock::new();
    ORIGIN.get_or_init(Instant::now).elapsed().as_nanos() as u64
}

/// Bytes the mutators may have allocated `elapsed_nanos` into the cycle.
pub(super) fn allowance(
    free_bytes: usize,
    slack_bytes: usize,
    expected_nanos: u64,
    elapsed_nanos: u64,
    surcharge_milli: u64,
) -> usize {
    let slack = slack_bytes.min(free_bytes);
    let progress = if expected_nanos == 0 {
        1.0
    } else {
        (elapsed_nanos as f64 / expected_nanos as f64).min(1.0)
    };
    let surcharge = (surcharge_milli.max(1000)) as f64 / 1000.0;
    slack + ((free_bytes - slack) as f64 * progress / surcharge) as usize
}

impl Pacer {
    pub(crate) const fn new() -> Self {
        Self {
            active: AtomicBool::new(false),
            armed_at_nanos: AtomicU64::new(0),
            expected_nanos: AtomicU64::new(0),
            free_bytes: AtomicUsize::new(0),
            slack_bytes: AtomicUsize::new(0),
            surcharge_milli: AtomicU64::new(1100),
            max_delay_nanos: AtomicU64::new(0),
            claimed_bytes: AtomicUsize::new(0),
            stalls: AtomicU64::new(0),
            stalled_nanos: AtomicU64::new(0),
            exhausted: AtomicBool::new(false),
        }
    }

    #[inline(always)]
    pub(crate) fn is_active(&self) -> bool {
        self.active.load(Ordering::Relaxed)
    }

    /// Records that allocation hit a full space during this pacing window.
    /// True only for the first time per window.
    pub(crate) fn note_exhaustion(&self) -> bool {
        self.is_active() && !self.exhausted.swap(true, Ordering::Relaxed)
    }

    /// Starts taxing allocation; call when concurrent marking begins.
    pub(crate) fn arm(&self, free_bytes: usize, expected_cycle: Duration, config: PacerConfig) {
        self.free_bytes.store(free_bytes, Ordering::Relaxed);
        self.slack_bytes.store(
            free_bytes / 100 * config.cycle_slack_percent.min(100),
            Ordering::Relaxed,
        );
        self.expected_nanos
            .store(expected_cycle.as_nanos() as u64, Ordering::Relaxed);
        self.surcharge_milli
            .store((config.surcharge * 1000.0) as u64, Ordering::Relaxed);
        self.max_delay_nanos
            .store(config.max_delay.as_nanos() as u64, Ordering::Relaxed);
        self.claimed_bytes.store(0, Ordering::Relaxed);
        self.stalls.store(0, Ordering::Relaxed);
        self.stalled_nanos.store(0, Ordering::Relaxed);
        self.exhausted.store(false, Ordering::Relaxed);
        self.armed_at_nanos.store(now_nanos(), Ordering::Relaxed);
        self.active.store(true, Ordering::Release);
    }

    /// Stops taxing allocation and reports the stalls it caused.
    pub(crate) fn disarm(&self) -> Option<PaceSummary> {
        if !self.active.swap(false, Ordering::AcqRel) {
            return None;
        }
        Some(PaceSummary {
            stalls: self.stalls.load(Ordering::Relaxed),
            stalled: Duration::from_nanos(self.stalled_nanos.load(Ordering::Relaxed)),
        })
    }

    fn allowed_at(&self, now: u64) -> usize {
        allowance(
            self.free_bytes.load(Ordering::Relaxed),
            self.slack_bytes.load(Ordering::Relaxed),
            self.expected_nanos.load(Ordering::Relaxed),
            now.saturating_sub(self.armed_at_nanos.load(Ordering::Relaxed)),
            self.surcharge_milli.load(Ordering::Relaxed),
        )
    }

    /// Charges `bytes` against the cycle's allowance, sleeping if the mutator
    /// is ahead of the collector.  `sleep` must keep the thread GC-safe.
    pub(crate) fn pace(&self, bytes: usize, sleep: impl FnMut(Duration)) {
        self.pace_with(bytes, now_nanos, sleep);
    }

    fn pace_with(
        &self,
        bytes: usize,
        mut now: impl FnMut() -> u64,
        mut sleep: impl FnMut(Duration),
    ) {
        if !self.is_active() {
            return;
        }
        let claimed = self.claimed_bytes.fetch_add(bytes, Ordering::Relaxed) + bytes;
        let max_delay = self.max_delay_nanos.load(Ordering::Relaxed);
        let mut waited = 0u64;
        let mut backoff = FIRST_BACKOFF.as_nanos() as u64;
        while waited < max_delay && self.is_active() && claimed > self.allowed_at(now()) {
            let nap = backoff.min(max_delay - waited);
            sleep(Duration::from_nanos(nap));
            waited += nap;
            backoff = backoff.saturating_mul(2);
        }
        if waited > 0 {
            self.stalls.fetch_add(1, Ordering::Relaxed);
            self.stalled_nanos.fetch_add(waited, Ordering::Relaxed);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    const MIB: usize = 1024 * 1024;

    #[test]
    fn allowance_starts_at_slack_and_saturates_below_free() {
        let free = 100 * MIB;
        let slack = 10 * MIB;
        assert_eq!(allowance(free, slack, 1000, 0, 1100), slack);
        let half = allowance(free, slack, 1000, 500, 1100);
        let full = allowance(free, slack, 1000, 1000, 1100);
        let beyond = allowance(free, slack, 1000, 9000, 1100);
        assert!(slack < half && half < full);
        assert_eq!(full, beyond);
        // Surcharge keeps the mutator below the whole free space.
        assert!(full < free);
        assert_eq!(full, slack + (90 * MIB * 10 / 11));
    }

    #[test]
    fn allowance_without_an_expected_duration_is_fully_open() {
        assert_eq!(allowance(100, 10, 0, 0, 1000), 100);
    }

    fn armed(free: usize, expected: Duration, max_delay: Duration) -> Pacer {
        let pacer = Pacer::new();
        pacer.arm(
            free,
            expected,
            PacerConfig {
                max_delay,
                cycle_slack_percent: 10,
                surcharge: 1.1,
            },
        );
        pacer
    }

    #[test]
    fn inactive_pacer_never_sleeps() {
        let pacer = Pacer::new();
        pacer.pace_with(usize::MAX / 2, || 0, |_| panic!("slept while inactive"));
    }

    #[test]
    fn mutator_within_allowance_does_not_sleep() {
        let pacer = armed(100 * MIB, Duration::from_secs(1), Duration::from_millis(10));
        pacer.pace_with(MIB, || 0, |_| panic!("slept inside the slack"));
        assert_eq!(pacer.disarm().expect("was armed").stalls, 0);
    }

    #[test]
    fn stall_is_capped_at_max_delay_with_exponential_backoff() {
        let pacer = armed(
            100 * MIB,
            Duration::from_secs(10),
            Duration::from_millis(10),
        );
        let naps = Cell::new(Vec::new());
        // The clock never advances, so the allowance stays at the slack.
        pacer.pace_with(
            50 * MIB,
            || 0,
            |d| {
                let mut v = naps.take();
                v.push(d);
                naps.set(v);
            },
        );
        let naps = naps.take();
        let ms: Vec<u128> = naps.iter().map(|d| d.as_millis()).collect();
        // 1 + 2 + 4 + remaining 3 = 10 ms.
        assert_eq!(ms, vec![1, 2, 4, 3]);
        let summary = pacer.disarm().expect("was armed");
        assert_eq!(summary.stalls, 1);
        assert_eq!(summary.stalled, Duration::from_millis(10));
        assert!(pacer.disarm().is_none());
    }

    #[test]
    fn stall_ends_early_once_the_collector_catches_up() {
        let pacer = armed(
            100 * MIB,
            Duration::from_millis(100),
            Duration::from_millis(10),
        );
        let clock = Cell::new(0u64);
        let sleeps = Cell::new(0u32);
        // Each nap advances time by 150 ms, finishing the expected cycle.
        pacer.pace_with(
            80 * MIB,
            || clock.get(),
            |d| {
                sleeps.set(sleeps.get() + 1);
                clock.set(clock.get() + 150_000_000 + d.as_nanos() as u64);
            },
        );
        // First check: allowance = slack (10 MiB) < 80 MiB, one nap; then
        // allowance at 100%+ = 10 + 90/1.1 = ~91.8 MiB >= 80 MiB, done.
        assert_eq!(sleeps.get(), 1);
    }
}

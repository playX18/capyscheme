//! Heap-size policies.
//!
//! None of these start collections: a GC runs when the plan asks for one
//! (nursery full, or reserved pages above the current heap target) or when a
//! trigger policy fires. They only decide the *heap target*.
//!
//! * [`BandedSizing`] (`banded`): Keep `used` inside
//!   `[min_ratio, max_ratio]` over the live size, otherwise retarget at
//!   `used * (1 + goal_ratio)`. Evaluated after full-heap pauses only, so
//!   promoted nursery survivors never inflate it.
//! * [`OverheadSizing`] (`overhead`): the MPLR '23 "Heap Size Adjustment with
//!   CPU Control" controller. The heap grows when GC CPU overhead is above the target
//!   and shrinks when below it.
//! * [`BalancedSizing`] (`balanced`): MMTk's `MemBalancerTrigger`, after the
//!   MemBalancer paper.  The heap is `live + sqrt(live * g_alloc / (0.2 * g_gc))`
//!   with `g_alloc` the smoothed allocation rate and `g_gc` the smoothed rate at
//!   which the collector processes live data, so the heap leaves more headroom
//!   for programs that allocate fast relative to how quickly they collect.
//!   A concurrent cycle is one sample: it runs from InitialMark (`on_gc_start`)
//!   to FinalMark (`on_gc_end`).

use std::time::Instant;

pub(crate) const DEFAULT_MIN_RATIO: f64 = 0.2;
pub(crate) const DEFAULT_GOAL_RATIO: f64 = 0.4;
pub(crate) const DEFAULT_MAX_RATIO: f64 = 0.65;
pub(crate) const DEFAULT_TARGET_GC_CPU: f64 = 0.15;
pub(crate) const DEFAULT_CPU_WINDOW: usize = 3;

#[derive(Clone, Copy, Debug)]
pub(crate) struct BandedSizing {
    pub(crate) min_ratio: f64,
    pub(crate) goal_ratio: f64,
    pub(crate) max_ratio: f64,
}

impl BandedSizing {
    /// New heap target after a full GC, or `None` to keep the current one.
    ///
    /// `pending_pages` is memory a failed allocation is waiting for. When it is
    /// non-zero the heap is always retargeted so that allocation fits.
    pub(crate) fn retarget(
        &self,
        used_pages: usize,
        current_pages: usize,
        pending_pages: usize,
        min_pages: usize,
        max_pages: usize,
    ) -> Option<usize> {
        let used = used_pages as f64;
        let target_min = (used * (1.0 + self.min_ratio)) as usize;
        let target_max = (used * (1.0 + self.max_ratio)) as usize;
        let new_target = ((used * (1.0 + self.goal_ratio)) as usize)
            .saturating_add(pending_pages)
            .clamp(min_pages, max_pages.max(min_pages));

        (pending_pages > 0 || current_pages < target_min || current_pages > target_max)
            .then_some(new_target)
    }
}

#[derive(Clone, Copy, Debug)]
struct GcSample {
    /// Wall-clock seconds inside the GC cycle.
    gc_seconds: f64,
    /// Process CPU seconds elapsed since the previous full GC ended.
    app_cpu_seconds: f64,
}

#[derive(Debug)]
pub(crate) struct OverheadSizing {
    pub(crate) target_gc_cpu: f64,
    pub(crate) window_size: usize,
    samples: Vec<GcSample>,
    gc_start: Option<Instant>,
    last_gc_end_cpu: Option<f64>,
}

/// Result of a CPU-control step, for logging.
pub(crate) struct OverheadStep {
    pub(crate) gc_cpu: f64,
    pub(crate) factor: f64,
    pub(crate) new_pages: usize,
}

impl OverheadSizing {
    pub(crate) fn new(target_gc_cpu: f64, window_size: usize) -> Self {
        Self {
            target_gc_cpu,
            window_size: window_size.max(1),
            samples: Vec::new(),
            gc_start: None,
            last_gc_end_cpu: None,
        }
    }

    pub(crate) fn pause_start(&mut self) {
        self.gc_start = Some(Instant::now());
    }

    /// Records a finished full-GC cycle and returns the new heap target.
    pub(crate) fn pause_end(
        &mut self,
        used_pages: usize,
        current_pages: usize,
        min_pages: usize,
        max_pages: usize,
    ) -> Option<OverheadStep> {
        let gc_end_cpu = process_cpu_time_seconds();
        let gc_seconds = self
            .gc_start
            .take()
            .map(|start| start.elapsed().as_secs_f64())
            .unwrap_or(0.0);
        if let (Some(last_end), Some(now)) = (self.last_gc_end_cpu, gc_end_cpu) {
            let app_cpu_seconds = (now - last_end).max(0.0);
            if app_cpu_seconds > 0.0 {
                self.push_sample(GcSample {
                    gc_seconds,
                    app_cpu_seconds,
                });
            }
        }
        self.last_gc_end_cpu = gc_end_cpu;

        let gc_cpu = self.mean_gc_cpu()?;
        let factor = sigmoid(gc_cpu - self.target_gc_cpu) + 0.5;
        let suggested = (current_pages as f64 * factor) as usize;
        let floor = (used_pages as f64 * 1.1).ceil() as usize;
        let upper = max_pages;
        let lower = floor.max(min_pages).min(upper);
        Some(OverheadStep {
            gc_cpu,
            factor,
            new_pages: suggested.clamp(lower, upper),
        })
    }

    fn push_sample(&mut self, sample: GcSample) {
        if self.samples.len() >= self.window_size {
            self.samples.remove(0);
        }
        self.samples.push(sample);
    }

    fn mean_gc_cpu(&self) -> Option<f64> {
        let total_gc: f64 = self.samples.iter().map(|s| s.gc_seconds).sum();
        let total_app: f64 = self.samples.iter().map(|s| s.app_cpu_seconds).sum();
        (total_app > 0.0).then(|| total_gc / total_app)
    }
}

/// Smoothing constants from the MemBalancer paper
const ALLOCATION_SMOOTH_FACTOR: f64 = 0.95;
const COLLECTION_SMOOTH_FACTOR: f64 = 0.5;
const TUNING_FACTOR: f64 = 0.2;
const FALLBACK_HEADROOM_SCALE: f64 = 4096.0;

/// One MemBalancer measurement window: pages and seconds.
#[derive(Clone, Copy, Debug, Default)]
struct BalancedWindow {
    alloc_pages: f64,
    alloc_seconds: f64,
    collect_pages: f64,
    collect_seconds: f64,
}

#[derive(Debug)]
pub(crate) struct BalancedSizing {
    window: BalancedWindow,
    previous: Option<BalancedWindow>,
    gc_start: Option<Instant>,
    gc_end: Instant,
    release_live_pages: usize,
    end_live_pages: usize,
}

impl BalancedSizing {
    pub(crate) fn new() -> Self {
        Self {
            window: BalancedWindow::default(),
            previous: None,
            gc_start: None,
            gc_end: Instant::now(),
            release_live_pages: 0,
            end_live_pages: 0,
        }
    }

    /// `reserved_pages` is the plan's reserved total; ignored by generational plans.
    pub(crate) fn gc_start(&mut self, now: Instant, reserved_pages: usize, generational: bool) {
        self.gc_start = Some(now);
        self.window.alloc_seconds += now.saturating_duration_since(self.gc_end).as_secs_f64();
        if !generational {
            self.window.alloc_pages = reserved_pages.saturating_sub(self.end_live_pages) as f64;
        }
    }

    /// `live_pages` is mature reserved pages for generational plans, total
    /// reserved otherwise. Nursery collections are not measured.
    pub(crate) fn gc_release(&mut self, live_pages: usize, generational: bool, nursery: bool) {
        if generational && nursery {
            return;
        }
        self.release_live_pages = live_pages;
        if generational {
            self.window.alloc_pages = live_pages.saturating_sub(self.end_live_pages) as f64;
        }
    }

    /// Finishes a collection. Returns whether a new heap target should be
    /// computed (false for nursery collections).
    pub(crate) fn gc_end(
        &mut self,
        now: Instant,
        live_pages: usize,
        generational: bool,
        nursery: bool,
    ) -> bool {
        self.gc_end = now;
        if let Some(start) = self.gc_start.take() {
            self.window.collect_seconds += now.saturating_duration_since(start).as_secs_f64();
        }
        if generational && nursery {
            return false;
        }
        self.end_live_pages = live_pages;
        // Live pages stand in for the pages traversed during collection.
        self.window.collect_pages = live_pages as f64;
        true
    }

    /// New heap target in pages, clamped to `[min_pages, max_pages]`.
    ///
    /// `extra_reserve` is the plan's copy reserve and `pending_pages` the
    /// allocation that triggered this GC.
    pub(crate) fn retarget(
        &mut self,
        live_pages: usize,
        extra_reserve: usize,
        pending_pages: usize,
        min_pages: usize,
        max_pages: usize,
    ) -> usize {
        let current = self.window;
        let smooth = |previous: Option<f64>, now: f64, factor: f64| {
            previous
                .map(|p| p * factor + now * (1.0 - factor))
                .unwrap_or(now)
        };
        let prev = self.previous;
        let alloc_pages = smooth(
            prev.map(|p| p.alloc_pages),
            current.alloc_pages,
            ALLOCATION_SMOOTH_FACTOR,
        );
        let alloc_seconds = smooth(
            prev.map(|p| p.alloc_seconds),
            current.alloc_seconds,
            ALLOCATION_SMOOTH_FACTOR,
        );
        let collect_pages = smooth(
            prev.map(|p| p.collect_pages),
            current.collect_pages,
            COLLECTION_SMOOTH_FACTOR,
        );
        let collect_seconds = smooth(
            prev.map(|p| p.collect_seconds),
            current.collect_seconds,
            COLLECTION_SMOOTH_FACTOR,
        );
        self.previous = Some(current);
        self.window = BalancedWindow::default();

        let live = live_pages as f64;
        let headroom = if alloc_pages != 0.0
            && collect_pages != 0.0
            && alloc_seconds != 0.0
            && collect_seconds != 0.0
        {
            (live * (alloc_pages / alloc_seconds)
                / TUNING_FACTOR
                / (collect_pages / collect_seconds))
                .sqrt()
        } else {
            (live * FALLBACK_HEADROOM_SCALE).sqrt()
        };
        live_pages
            .saturating_add(headroom as usize)
            .saturating_add(extra_reserve)
            .saturating_add(pending_pages)
            .clamp(min_pages, max_pages.max(min_pages))
    }
}

fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}

/// Process-wide CPU time in seconds, summed over all threads.
fn process_cpu_time_seconds() -> Option<f64> {
    let mut ts = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    // SAFETY: `ts` is a valid local `timespec` for `clock_gettime` to fill.
    let rc = unsafe { libc::clock_gettime(libc::CLOCK_PROCESS_CPUTIME_ID, &mut ts) };
    (rc == 0).then(|| ts.tv_sec as f64 + ts.tv_nsec as f64 / 1_000_000_000.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    const BANDED: BandedSizing = BandedSizing {
        min_ratio: DEFAULT_MIN_RATIO,
        goal_ratio: DEFAULT_GOAL_RATIO,
        max_ratio: DEFAULT_MAX_RATIO,
    };

    #[test]
    fn banded_keeps_target_inside_band() {
        // used 1000: band is [1200, 1650].
        assert_eq!(BANDED.retarget(1000, 1400, 0, 256, 100_000), None);
        assert_eq!(BANDED.retarget(1000, 1100, 0, 256, 100_000), Some(1400));
        assert_eq!(BANDED.retarget(1000, 5000, 0, 256, 100_000), Some(1400));
    }

    #[test]
    fn banded_pending_always_retargets_and_clamps() {
        assert_eq!(BANDED.retarget(1000, 1400, 50, 256, 100_000), Some(1450));
        assert_eq!(BANDED.retarget(10_000, 100, 0, 256, 4096), Some(4096));
    }

    #[test]
    fn sigmoid_is_well_behaved() {
        assert!((sigmoid(0.0) - 0.5).abs() < 1e-12);
        assert!(sigmoid(-100.0) < 1e-9);
        assert!(sigmoid(100.0) > 1.0 - 1e-9);
    }

    #[test]
    fn cpu_mean_is_total_weighted_and_window_drops_oldest() {
        let mut cpu = OverheadSizing::new(0.15, 3);
        cpu.push_sample(GcSample {
            gc_seconds: 1.0,
            app_cpu_seconds: 10.0,
        });
        cpu.push_sample(GcSample {
            gc_seconds: 3.0,
            app_cpu_seconds: 10.0,
        });
        assert!((cpu.mean_gc_cpu().unwrap() - 0.2).abs() < 1e-12);
        for i in 0..5 {
            cpu.push_sample(GcSample {
                gc_seconds: i as f64,
                app_cpu_seconds: 1.0,
            });
        }
        assert_eq!(cpu.samples.len(), 3);
        assert_eq!(cpu.samples[0].gc_seconds, 2.0);
    }

    #[test]
    fn balanced_falls_back_to_sqrt_live_headroom_before_any_samples() {
        let mut sizing = BalancedSizing::new();
        // sqrt(10_000 * 4096) = 6400.
        assert_eq!(sizing.retarget(10_000, 0, 0, 1, 1_000_000), 16_400);
    }

    #[test]
    fn balanced_follows_the_memory_balancer_formula() {
        let mut sizing = BalancedSizing::new();
        sizing.window = BalancedWindow {
            alloc_pages: 4000.0,
            alloc_seconds: 2.0,
            collect_pages: 1000.0,
            collect_seconds: 0.5,
        };
        // headroom = sqrt(1000 * (4000/2) / 0.2 / (1000/0.5)) = sqrt(5000).
        let expected = 1000 + 5000f64.sqrt() as usize + 7 + 3;
        assert_eq!(sizing.retarget(1000, 7, 3, 1, 1_000_000), expected);
    }

    #[test]
    fn balanced_smooths_against_the_previous_window_and_clamps() {
        let mut sizing = BalancedSizing::new();
        sizing.previous = Some(BalancedWindow {
            alloc_pages: 100.0,
            alloc_seconds: 1.0,
            collect_pages: 100.0,
            collect_seconds: 1.0,
        });
        sizing.window = BalancedWindow {
            alloc_pages: 200.0,
            alloc_seconds: 1.0,
            collect_pages: 100.0,
            collect_seconds: 1.0,
        };
        // alloc smoothed 0.95*100 + 0.05*200 = 105.
        let expected = 100 + (100.0f64 * 105.0 / 0.2 / 100.0).sqrt() as usize;
        assert_eq!(sizing.retarget(100, 0, 0, 1, 10_000), expected);
        assert_eq!(sizing.retarget(100, 0, 0, 5000, 10_000), 5000);
        assert_eq!(sizing.retarget(100_000, 0, 0, 1, 10_000), 10_000);
    }

    #[test]
    fn balanced_tracks_allocation_and_collection_time() {
        let mut sizing = BalancedSizing::new();
        let t0 = sizing.gc_end;
        let start = t0 + std::time::Duration::from_secs(2);
        let end = start + std::time::Duration::from_secs(1);
        sizing.gc_start(start, 900, false);
        sizing.gc_release(900, false, false);
        assert!(sizing.gc_end(end, 400, false, false));
        assert!((sizing.window.alloc_seconds - 2.0).abs() < 1e-9);
        assert!((sizing.window.collect_seconds - 1.0).abs() < 1e-9);
        assert_eq!(sizing.window.alloc_pages, 900.0);
        assert_eq!(sizing.window.collect_pages, 400.0);
    }

    #[test]
    fn balanced_ignores_nursery_collections_of_generational_plans() {
        let mut sizing = BalancedSizing::new();
        let now = Instant::now();
        sizing.gc_start(now, 0, true);
        sizing.gc_release(500, true, true);
        assert!(!sizing.gc_end(now, 500, true, true));
        assert_eq!(sizing.window.alloc_pages, 0.0);
        // A mature collection counts promoted volume.
        sizing.gc_start(now, 0, true);
        sizing.gc_release(500, true, false);
        assert!(sizing.gc_end(now, 300, true, false));
        assert_eq!(sizing.window.alloc_pages, 500.0);
        assert_eq!(sizing.window.collect_pages, 300.0);
    }
}

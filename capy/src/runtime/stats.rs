use std::{
    cell::Cell,
    time::{Duration, Instant},
};

use crate::runtime::GLOBAL_STATS;

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct RuntimeStatsSnapshot {
    pub(crate) execution: Duration,
    pub(crate) compilation: Duration,
    pub(crate) stw: Duration,
    pub(crate) blocking: Duration,
    pub(crate) gc: Duration,
    pub(crate) reader: Duration,
    pub(crate) psyntax: Duration,
    pub(crate) lowering: Duration,
    pub(crate) cranelift: Duration,
    pub(crate) object_emit: Duration,
    pub(crate) link: Duration,
}

impl RuntimeStatsSnapshot {
    pub(crate) fn total(self) -> Duration {
        self.execution + self.compilation + self.stw + self.blocking + self.gc
    }

    pub(crate) fn frontend_total(self) -> Duration {
        self.compilation
            .saturating_sub(self.lowering + self.cranelift + self.object_emit + self.link)
    }

    pub(crate) fn frontend_other(self) -> Duration {
        self.frontend_total()
            .saturating_sub(self.reader + self.psyntax)
    }

    fn percentage(self, value: Duration) -> f64 {
        let total = self.total().as_secs_f64();
        if total > 0.0 {
            (value.as_secs_f64() / total) * 100.0
        } else {
            0.0
        }
    }
}

impl std::fmt::Display for RuntimeStatsSnapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            concat!(
                "Runtime Statistics:\n",
                "  Execution: {:.3}s ({:.1}%)\n",
                "  Compilation: {:.3}s ({:.1}%)\n",
                "  STW: {:.3}s ({:.1}%)\n",
                "  Blocking: {:.3}s ({:.1}%)\n",
                "  GC: {:.3}s ({:.1}%)\n",
                "  Total: {:.3}s\n",
                "  Compilation Breakdown:\n",
                "    Frontend Total: {:.3}s\n",
                "      Reader (reader.scm): {:.3}s\n",
                "      Psyntax (psyntax.scm): {:.3}s\n",
                "      Frontend Other: {:.3}s\n",
                "    Lowering: {:.3}s\n",
                "    Cranelift: {:.3}s\n",
                "    Object Emit: {:.3}s\n",
                "    Link: {:.3}s\n",
            ),
            self.execution.as_secs_f64(),
            self.percentage(self.execution),
            self.compilation.as_secs_f64(),
            self.percentage(self.compilation),
            self.stw.as_secs_f64(),
            self.percentage(self.stw),
            self.blocking.as_secs_f64(),
            self.percentage(self.blocking),
            self.gc.as_secs_f64(),
            self.percentage(self.gc),
            self.total().as_secs_f64(),
            self.frontend_total().as_secs_f64(),
            self.reader.as_secs_f64(),
            self.psyntax.as_secs_f64(),
            self.frontend_other().as_secs_f64(),
            self.lowering.as_secs_f64(),
            self.cranelift.as_secs_f64(),
            self.object_emit.as_secs_f64(),
            self.link.as_secs_f64(),
        )
    }
}

#[derive(Clone, Copy, Debug)]
enum TopLevelPhase {
    Execution,
    Compilation,
    Stw,
    Blocking,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum CompilationBreakdownPhase {
    Reader,
    Psyntax,
    Lowering,
    Cranelift,
    ObjectEmit,
    Link,
}

#[derive(Clone, Copy, Debug)]
struct ActiveBreakdown {
    token: u64,
    phase: CompilationBreakdownPhase,
    generation: u64,
    start: Instant,
}

pub(crate) struct CompilationBreakdownScope {
    token: u64,
}

impl CompilationBreakdownScope {
    pub(crate) fn new(phase: CompilationBreakdownPhase) -> Self {
        Self {
            token: begin_compilation_breakdown(phase),
        }
    }
}

impl Drop for CompilationBreakdownScope {
    fn drop(&mut self) {
        end_compilation_breakdown(self.token);
    }
}

pub(crate) fn begin_compilation_breakdown(phase: CompilationBreakdownPhase) -> u64 {
    GLOBAL_STATS.lock().begin_breakdown(phase)
}

pub(crate) fn end_compilation_breakdown(token: u64) {
    GLOBAL_STATS.lock().end_breakdown(token);
}

pub(crate) fn runtime_stats_enabled() -> bool {
    GLOBAL_STATS.lock().enabled
}

pub(crate) fn set_runtime_stats_enabled(thread_stats: &ThreadStats, enabled: bool) {
    if !enabled {
        thread_stats.flush_active_durations();
    }

    GLOBAL_STATS.lock().set_enabled(enabled);
    thread_stats.sync_session_state();
}

pub(crate) fn runtime_stats_snapshot(
    current_thread: Option<&ThreadStats>,
) -> Option<RuntimeStatsSnapshot> {
    let mut snapshot = GLOBAL_STATS.lock().snapshot();
    if !snapshot.0 {
        return None;
    }

    if let Some(thread_stats) = current_thread {
        let active = thread_stats.snapshot_active();
        snapshot.1.execution += active.execution;
        snapshot.1.compilation += active.compilation;
        snapshot.1.stw += active.stw;
        snapshot.1.blocking += active.blocking;
    }

    Some(snapshot.1)
}

pub struct ThreadStats {
    execution_depth: Cell<u32>,
    compilation_depth: Cell<u32>,
    stw_depth: Cell<u32>,
    blocking_depth: Cell<u32>,

    execution_start: Cell<Option<Instant>>,
    compilation_start: Cell<Option<Instant>>,
    stw_start: Cell<Option<Instant>>,
    blocking_start: Cell<Option<Instant>>,

    generation: Cell<u64>,
}

impl ThreadStats {
    pub fn new() -> Self {
        let generation = GLOBAL_STATS.lock().generation;
        Self {
            execution_depth: Cell::new(0),
            compilation_depth: Cell::new(0),
            stw_depth: Cell::new(0),
            blocking_depth: Cell::new(0),
            execution_start: Cell::new(None),
            compilation_start: Cell::new(None),
            stw_start: Cell::new(None),
            blocking_start: Cell::new(None),
            generation: Cell::new(generation),
        }
    }

    pub fn start_compilation(&self) {
        self.sync_session_state();

        let depth = self.compilation_depth.get();
        self.compilation_depth.set(depth + 1);
        if depth == 0 {
            if self.execution_depth.get() > 0 {
                self.end_interval(TopLevelPhase::Execution);
            }
            self.start_interval(TopLevelPhase::Compilation);
        }
    }

    pub fn end_compilation(&self) {
        self.sync_session_state();

        let depth = self.compilation_depth.get();
        if depth == 0 {
            return;
        }

        self.compilation_depth.set(depth - 1);
        if depth == 1 {
            self.end_interval(TopLevelPhase::Compilation);

            if self.execution_depth.get() > 0 {
                self.start_interval(TopLevelPhase::Execution);
            }
        }
    }

    pub fn start_execution(&self) {
        self.sync_session_state();

        let depth = self.execution_depth.get();
        self.execution_depth.set(depth + 1);
        if depth == 0 && self.compilation_depth.get() == 0 {
            self.start_interval(TopLevelPhase::Execution);
        }
    }

    pub fn end_execution(&self) {
        self.sync_session_state();

        let depth = self.execution_depth.get();
        if depth == 0 {
            return;
        }

        self.execution_depth.set(depth - 1);
        if depth == 1 {
            self.end_interval(TopLevelPhase::Execution);
        }
    }

    pub fn start_stw(&self) {
        self.sync_session_state();

        let depth = self.stw_depth.get();
        self.stw_depth.set(depth + 1);
        if depth == 0 {
            self.start_interval(TopLevelPhase::Stw);
        }
    }

    pub fn end_stw(&self) {
        self.sync_session_state();

        let depth = self.stw_depth.get();
        if depth == 0 {
            return;
        }

        self.stw_depth.set(depth - 1);
        if depth == 1 {
            self.end_interval(TopLevelPhase::Stw);
        }
    }

    pub fn start_blocking(&self) {
        self.sync_session_state();

        let depth = self.blocking_depth.get();
        self.blocking_depth.set(depth + 1);
        if depth == 0 {
            self.start_interval(TopLevelPhase::Blocking);
        }
    }

    pub fn end_blocking(&self) {
        self.sync_session_state();

        let depth = self.blocking_depth.get();
        if depth == 0 {
            return;
        }

        self.blocking_depth.set(depth - 1);
        if depth == 1 {
            self.end_interval(TopLevelPhase::Blocking);
        }
    }

    pub(crate) fn sync_session_state(&self) {
        let global = GLOBAL_STATS.lock();
        let enabled = global.enabled;
        let generation = global.generation;
        drop(global);

        if self.generation.get() == generation {
            return;
        }

        self.generation.set(generation);
        let now = Instant::now();

        self.rebase_interval(
            &self.execution_start,
            enabled && self.execution_depth.get() > 0 && self.compilation_depth.get() == 0,
            now,
        );
        self.rebase_interval(
            &self.compilation_start,
            enabled && self.compilation_depth.get() > 0,
            now,
        );
        self.rebase_interval(&self.stw_start, enabled && self.stw_depth.get() > 0, now);
        self.rebase_interval(
            &self.blocking_start,
            enabled && self.blocking_depth.get() > 0,
            now,
        );
    }

    pub(crate) fn flush_active_durations(&self) {
        self.sync_session_state();

        let generation = self.generation.get();
        let now = Instant::now();
        self.flush_interval(
            &self.execution_start,
            TopLevelPhase::Execution,
            generation,
            now,
        );
        self.flush_interval(
            &self.compilation_start,
            TopLevelPhase::Compilation,
            generation,
            now,
        );
        self.flush_interval(&self.stw_start, TopLevelPhase::Stw, generation, now);
        self.flush_interval(
            &self.blocking_start,
            TopLevelPhase::Blocking,
            generation,
            now,
        );
    }

    fn snapshot_active(&self) -> RuntimeStatsSnapshot {
        self.sync_session_state();

        let now = Instant::now();
        RuntimeStatsSnapshot {
            execution: self
                .execution_start
                .get()
                .map(|start| now.duration_since(start))
                .unwrap_or(Duration::ZERO),
            compilation: self
                .compilation_start
                .get()
                .map(|start| now.duration_since(start))
                .unwrap_or(Duration::ZERO),
            stw: self
                .stw_start
                .get()
                .map(|start| now.duration_since(start))
                .unwrap_or(Duration::ZERO),
            blocking: self
                .blocking_start
                .get()
                .map(|start| now.duration_since(start))
                .unwrap_or(Duration::ZERO),
            ..RuntimeStatsSnapshot::default()
        }
    }

    fn start_interval(&self, phase: TopLevelPhase) {
        let start_cell = self.start_cell(phase);
        if start_cell.get().is_none() && runtime_stats_enabled() {
            start_cell.set(Some(Instant::now()));
        }
    }

    fn end_interval(&self, phase: TopLevelPhase) {
        let Some(start) = self.start_cell(phase).take() else {
            return;
        };

        GLOBAL_STATS
            .lock()
            .add_top_level_elapsed(phase, start.elapsed(), self.generation.get());
    }

    fn flush_interval(
        &self,
        start_cell: &Cell<Option<Instant>>,
        phase: TopLevelPhase,
        generation: u64,
        now: Instant,
    ) {
        let Some(start) = start_cell.get() else {
            return;
        };

        GLOBAL_STATS
            .lock()
            .add_top_level_elapsed(phase, now.duration_since(start), generation);
        start_cell.set(Some(now));
    }

    fn rebase_interval(&self, start_cell: &Cell<Option<Instant>>, active: bool, now: Instant) {
        start_cell.set(active.then_some(now));
    }

    fn start_cell(&self, phase: TopLevelPhase) -> &Cell<Option<Instant>> {
        match phase {
            TopLevelPhase::Execution => &self.execution_start,
            TopLevelPhase::Compilation => &self.compilation_start,
            TopLevelPhase::Stw => &self.stw_start,
            TopLevelPhase::Blocking => &self.blocking_start,
        }
    }

    #[cfg(test)]
    pub(crate) fn compilation_active(&self) -> bool {
        self.compilation_depth.get() > 0
    }

    #[cfg(test)]
    pub(crate) fn execution_active(&self) -> bool {
        self.execution_depth.get() > 0 && self.compilation_depth.get() == 0
    }
}

pub struct GlobalStats {
    enabled: bool,
    ever_enabled: bool,
    generation: u64,
    snapshot: RuntimeStatsSnapshot,
    gc_start: Option<Instant>,
    next_breakdown_token: u64,
    active_breakdowns: Vec<ActiveBreakdown>,
}

impl GlobalStats {
    pub const fn new() -> Self {
        Self {
            enabled: false,
            ever_enabled: false,
            generation: 0,
            snapshot: RuntimeStatsSnapshot {
                execution: Duration::ZERO,
                compilation: Duration::ZERO,
                stw: Duration::ZERO,
                blocking: Duration::ZERO,
                gc: Duration::ZERO,
                reader: Duration::ZERO,
                psyntax: Duration::ZERO,
                lowering: Duration::ZERO,
                cranelift: Duration::ZERO,
                object_emit: Duration::ZERO,
                link: Duration::ZERO,
            },
            gc_start: None,
            next_breakdown_token: 1,
            active_breakdowns: Vec::new(),
        }
    }

    pub fn start_gc(&mut self) {
        self.start_gc_at(Instant::now());
    }

    pub fn end_gc(&mut self) {
        self.end_gc_at(Instant::now());
    }

    pub(crate) fn start_gc_at(&mut self, start: Instant) {
        self.gc_start = self.enabled.then_some(start);
    }

    pub(crate) fn end_gc_at(&mut self, end: Instant) {
        let Some(start) = self.gc_start.take() else {
            return;
        };

        if self.enabled {
            self.snapshot.gc += end.saturating_duration_since(start);
        }
    }

    fn set_enabled(&mut self, enabled: bool) {
        if enabled {
            self.snapshot = RuntimeStatsSnapshot::default();
            self.gc_start = None;
            self.active_breakdowns.clear();
            self.enabled = true;
            self.ever_enabled = true;
            self.generation = self.generation.wrapping_add(1);
            return;
        }

        if self.enabled {
            self.gc_start = None;
            self.active_breakdowns.clear();
            self.enabled = false;
            self.generation = self.generation.wrapping_add(1);
        }
    }

    fn begin_breakdown(&mut self, phase: CompilationBreakdownPhase) -> u64 {
        if !self.enabled {
            return 0;
        }

        let token = self.next_breakdown_token;
        self.next_breakdown_token = self.next_breakdown_token.wrapping_add(1);
        self.active_breakdowns.push(ActiveBreakdown {
            token,
            phase,
            generation: self.generation,
            start: Instant::now(),
        });
        token
    }

    fn end_breakdown(&mut self, token: u64) {
        if token == 0 {
            return;
        }

        let Some(index) = self
            .active_breakdowns
            .iter()
            .position(|active| active.token == token)
        else {
            return;
        };

        let active = self.active_breakdowns.swap_remove(index);
        if self.enabled && self.generation == active.generation {
            self.add_breakdown_elapsed(active.phase, active.start.elapsed(), active.generation);
        }
    }

    fn add_top_level_elapsed(&mut self, phase: TopLevelPhase, elapsed: Duration, generation: u64) {
        if !self.enabled || self.generation != generation {
            return;
        }

        match phase {
            TopLevelPhase::Execution => self.snapshot.execution += elapsed,
            TopLevelPhase::Compilation => self.snapshot.compilation += elapsed,
            TopLevelPhase::Stw => self.snapshot.stw += elapsed,
            TopLevelPhase::Blocking => self.snapshot.blocking += elapsed,
        }
    }

    fn add_breakdown_elapsed(
        &mut self,
        phase: CompilationBreakdownPhase,
        elapsed: Duration,
        generation: u64,
    ) {
        if !self.enabled || self.generation != generation {
            return;
        }

        match phase {
            CompilationBreakdownPhase::Reader => self.snapshot.reader += elapsed,
            CompilationBreakdownPhase::Psyntax => self.snapshot.psyntax += elapsed,
            CompilationBreakdownPhase::Lowering => self.snapshot.lowering += elapsed,
            CompilationBreakdownPhase::Cranelift => self.snapshot.cranelift += elapsed,
            CompilationBreakdownPhase::ObjectEmit => self.snapshot.object_emit += elapsed,
            CompilationBreakdownPhase::Link => self.snapshot.link += elapsed,
        }
    }

    fn snapshot(&self) -> (bool, RuntimeStatsSnapshot) {
        let mut snapshot = self.snapshot;
        if self.enabled
            && let Some(start) = self.gc_start
        {
            snapshot.gc += start.elapsed();
        }

        if self.enabled {
            let now = Instant::now();
            for active in &self.active_breakdowns {
                if active.generation == self.generation {
                    let elapsed = now.duration_since(active.start);
                    match active.phase {
                        CompilationBreakdownPhase::Reader => snapshot.reader += elapsed,
                        CompilationBreakdownPhase::Psyntax => snapshot.psyntax += elapsed,
                        CompilationBreakdownPhase::Lowering => snapshot.lowering += elapsed,
                        CompilationBreakdownPhase::Cranelift => snapshot.cranelift += elapsed,
                        CompilationBreakdownPhase::ObjectEmit => snapshot.object_emit += elapsed,
                        CompilationBreakdownPhase::Link => snapshot.link += elapsed,
                    }
                }
            }
        }

        (self.ever_enabled, snapshot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn records_gc_duration_from_explicit_cycle_bounds() {
        let mut stats = GlobalStats::new();
        stats.set_enabled(true);

        let start = Instant::now();
        let end = start + Duration::from_millis(25);
        stats.start_gc_at(start);
        stats.end_gc_at(end);

        let (_, snapshot) = stats.snapshot();
        assert_eq!(snapshot.gc, Duration::from_millis(25));
    }
}

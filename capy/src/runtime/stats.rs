use std::cell::Cell;
use std::time::Duration;
pub struct ThreadStats {
    comp_time: Cell<Duration>,
    exec_time: Cell<Duration>,
    stw_time: Cell<Duration>,
    blocking_time: Cell<Duration>,

    comp_start: Cell<Option<std::time::Instant>>,
    exec_start: Cell<Option<std::time::Instant>>,
    stw_start: Cell<Option<std::time::Instant>>,
    blocking_start: Cell<Option<std::time::Instant>>,
}

impl ThreadStats {
    pub fn new() -> Self {
        Self {
            comp_time: Cell::new(Duration::ZERO),
            exec_time: Cell::new(Duration::ZERO),
            stw_time: Cell::new(Duration::ZERO),
            blocking_time: Cell::new(Duration::ZERO),
            comp_start: Cell::new(None),
            exec_start: Cell::new(None),
            stw_start: Cell::new(None),
            blocking_start: Cell::new(None),
        }
    }

    pub fn start_compilation(&self) {
        self.comp_start.set(Some(std::time::Instant::now()));
    }

    pub fn end_compilation(&self) {
        if let Some(start) = self.comp_start.take() {
            self.comp_time.set(self.comp_time.get() + start.elapsed());
        }
    }

    pub fn start_execution(&self) {
        self.exec_start.set(Some(std::time::Instant::now()));
    }

    pub fn end_execution(&self) {
        if let Some(start) = self.exec_start.take() {
            self.exec_time.set(self.exec_time.get() + start.elapsed());
        }
    }

    pub fn start_stw(&self) {
        self.stw_start.set(Some(std::time::Instant::now()));
    }

    pub fn end_stw(&self) {
        if let Some(start) = self.stw_start.take() {
            self.stw_time.set(self.stw_time.get() + start.elapsed());
        }
    }

    pub fn start_blocking(&self) {
        self.blocking_start.set(Some(std::time::Instant::now()));
    }

    pub fn end_blocking(&self) {
        if let Some(start) = self.blocking_start.take() {
            self.blocking_time
                .set(self.blocking_time.get() + start.elapsed());
        }
    }
}

pub struct GlobalStats {
    total_comp_time: Duration,
    total_exec_time: Duration,
    total_stw_time: Duration,
    total_blocking_time: Duration,
    total_gc_time: Duration,
    gc_start: Option<std::time::Instant>,
}

impl GlobalStats {
    pub const fn new() -> Self {
        Self {
            total_comp_time: Duration::ZERO,
            total_exec_time: Duration::ZERO,
            total_stw_time: Duration::ZERO,
            total_blocking_time: Duration::ZERO,
            total_gc_time: Duration::ZERO,
            gc_start: None,
        }
    }

    pub fn start_gc(&mut self) {
        self.gc_start = Some(std::time::Instant::now());
    }

    pub fn end_gc(&mut self) {
        if let Some(start) = self.gc_start.take() {
            self.total_gc_time += start.elapsed();
        }
    }

    pub fn add_thread_stats(&mut self, thread_stats: &ThreadStats) {
        self.total_comp_time += thread_stats.comp_time.get();
        self.total_exec_time += thread_stats.exec_time.get();
        self.total_stw_time += thread_stats.stw_time.get();
        self.total_blocking_time += thread_stats.blocking_time.get();
    }
}

impl std::fmt::Display for GlobalStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let total_runtime = self.total_comp_time
            + self.total_exec_time
            + self.total_stw_time
            + self.total_blocking_time
            + self.total_gc_time;

        let percentage_exec = if total_runtime.as_secs_f64() > 0.0 {
            (self.total_exec_time.as_secs_f64() / total_runtime.as_secs_f64()) * 100.0
        } else {
            0.0
        };

        let percentage_stw = if total_runtime.as_secs_f64() > 0.0 {
            (self.total_stw_time.as_secs_f64() / total_runtime.as_secs_f64()) * 100.0
        } else {
            0.0
        };

        let percentage_comp = if total_runtime.as_secs_f64() > 0.0 {
            (self.total_comp_time.as_secs_f64() / total_runtime.as_secs_f64()) * 100.0
        } else {
            0.0
        };

        let percentage_blocking = if total_runtime.as_secs_f64() > 0.0 {
            (self.total_blocking_time.as_secs_f64() / total_runtime.as_secs_f64()) * 100.0
        } else {
            0.0
        };

        let percentage_gc = if total_runtime.as_secs_f64() > 0.0 {
            (self.total_gc_time.as_secs_f64() / total_runtime.as_secs_f64()) * 100.0
        } else {
            0.0
        };

        write!(
            f,
            "Runtime Statistics:\n  Execution: {:.2}s ({:.1}%)\n  STW: {:.2}s ({:.1}%)\n  Compilation: {:.2}s ({:.1}%)\n  Blocking: {:.2}s ({:.1}%)\n  GC: {:.2}s ({:.1}%)\n  Total: {:.2}s",
            self.total_exec_time.as_secs_f64(),
            percentage_exec,
            self.total_stw_time.as_secs_f64(),
            percentage_stw,
            self.total_comp_time.as_secs_f64(),
            percentage_comp,
            self.total_blocking_time.as_secs_f64(),
            percentage_blocking,
            self.total_gc_time.as_secs_f64(),
            percentage_gc,
            total_runtime.as_secs_f64(),
        )
    }
}

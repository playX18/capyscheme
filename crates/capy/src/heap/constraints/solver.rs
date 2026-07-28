//! Parallel / sequential constraint execution via MMTk work packets.

use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicUsize, Ordering},
};

use mmtk::{
    MMTK, memory_manager,
    scheduler::{GCWork, GCWorker, WorkBucketStage},
    vm::ObjectTracerContext,
};

use super::MarkingConstraintSet;
use crate::heap::{GarbageCollector, mm::MemoryManager};

/// When false, all constraints run on the coordinator worker (option A fallback).
pub static PARALLEL_CONSTRAINTS: AtomicBool = AtomicBool::new(true);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstraintExecuteMode {
    Sequential,
    Parallel,
}

/// Shared result of a sharded constraint pass.
struct ShardResult {
    visited: AtomicUsize,
    dirty: AtomicBool,
}

impl ShardResult {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            visited: AtomicUsize::new(0),
            dirty: AtomicBool::new(false),
        })
    }
}

struct ConstraintShard<C: ObjectTracerContext<MemoryManager>> {
    indices: Vec<usize>,
    tracer_context: C,
    result: Arc<ShardResult>,
}

impl<C: ObjectTracerContext<MemoryManager> + 'static> GCWork<MemoryManager> for ConstraintShard<C> {
    fn do_work(
        &mut self,
        worker: &mut GCWorker<MemoryManager>,
        _mmtk: &'static MMTK<MemoryManager>,
    ) {
        let set = GarbageCollector::get().constraints.read();
        self.tracer_context.with_tracer(worker, |tracer| {
            for &index in &self.indices {
                let n = set.execute_one(index, tracer);
                if n > 0 {
                    self.result.visited.fetch_add(n, Ordering::Relaxed);
                    self.result.dirty.store(true, Ordering::Relaxed);
                    set.mark_dirty();
                }
            }
        });
    }
}

/// Execute `indices` either sequentially or as `VMRefClosure` work packets.
///
/// Returns whether MMTk should invoke `process_weak_refs` again after draining.
pub fn execute_constraints<C: ObjectTracerContext<MemoryManager> + Clone + 'static>(
    set: &MarkingConstraintSet,
    indices: &[usize],
    worker: &mut GCWorker<MemoryManager>,
    tracer_context: &mut C,
    mode: ConstraintExecuteMode,
) -> bool {
    if indices.is_empty() {
        return false;
    }

    // Post-drain re-entry after parallel shards: do not reschedule the same work.
    let post_parallel_drain = set.take_parallel_reentry();

    let use_parallel = !post_parallel_drain
        && mode == ConstraintExecuteMode::Parallel
        && PARALLEL_CONSTRAINTS.load(Ordering::Relaxed)
        && indices.len() > 1;

    if !use_parallel {
        let mut visited = false;
        tracer_context.with_tracer(worker, |tracer| {
            for &index in indices {
                if set.execute_one(index, tracer) > 0 {
                    visited = true;
                    set.mark_dirty();
                }
            }
        });
        // #region agent log
        {
            use std::io::Write;
            if let Ok(mut f) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("/home/adel/projects/capyscheme/.cursor/debug-3ed3b0.log")
            {
                let _ = writeln!(
                    f,
                    r#"{{"sessionId":"3ed3b0","runId":"post-fix","hypothesisId":"A","location":"solver.rs:execute_constraints","message":"constraints_seq","data":{{"post_parallel_drain":{},"indices":{},"visited":{}}},"timestamp":{}}}"#,
                    post_parallel_drain,
                    indices.len(),
                    visited,
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .map(|d| d.as_millis())
                        .unwrap_or(0)
                );
            }
        }
        // #endregion
        return visited;
    }

    // Split into sequential (must run alone) vs parallel-eligible.
    let mut sequential = Vec::new();
    let mut parallel = Vec::new();
    for &i in indices {
        match set.constraint(i).parallelism() {
            super::ConstraintParallelism::Sequential => sequential.push(i),
            super::ConstraintParallelism::Parallel => parallel.push(i),
        }
    }

    let result = ShardResult::new();
    let mut scheduled_parallel = false;

    if !parallel.is_empty() {
        let workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1)
            .max(1);
        let chunk = (parallel.len() + workers - 1) / workers;
        let mut packets: Vec<Box<dyn GCWork<MemoryManager>>> = Vec::new();
        for start in (0..parallel.len()).step_by(chunk.max(1)) {
            let end = (start + chunk).min(parallel.len());
            packets.push(Box::new(ConstraintShard {
                indices: parallel[start..end].to_vec(),
                tracer_context: tracer_context.clone(),
                result: Arc::clone(&result),
            }));
        }
        memory_manager::add_work_packets(
            &GarbageCollector::get().mmtk,
            WorkBucketStage::VMRefClosure,
            packets,
        );
        scheduled_parallel = true;
        set.arm_parallel_reentry();
    }

    // Sequential constraints run on this worker after scheduling parallel shards.
    if !sequential.is_empty() {
        tracer_context.with_tracer(worker, |tracer| {
            for &index in &sequential {
                if set.execute_one(index, tracer) > 0 {
                    result.visited.fetch_add(1, Ordering::Relaxed);
                    result.dirty.store(true, Ordering::Relaxed);
                    set.mark_dirty();
                }
            }
        });
    }

    let dirty =
        result.dirty.load(Ordering::Relaxed) || result.visited.load(Ordering::Relaxed) > 0;
    // #region agent log
    {
        use std::io::Write;
        if let Ok(mut f) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("/home/adel/projects/capyscheme/.cursor/debug-3ed3b0.log")
        {
            let _ = writeln!(
                f,
                r#"{{"sessionId":"3ed3b0","runId":"post-fix","hypothesisId":"A","location":"solver.rs:execute_constraints","message":"constraints_par","data":{{"scheduled_parallel":{},"dirty":{},"ret":{}}},"timestamp":{}}}"#,
                scheduled_parallel,
                dirty,
                scheduled_parallel || dirty,
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map(|d| d.as_millis())
                    .unwrap_or(0)
            );
        }
    }
    // #endregion
    // Request one post-drain re-entry when shards were scheduled; otherwise only
    // continue while constraints actually greyed/retained work.
    scheduled_parallel || dirty
}

/// Disable parallel constraint sharding (tests / fallback).
pub fn set_parallel_constraints(enabled: bool) {
    PARALLEL_CONSTRAINTS.store(enabled, Ordering::Relaxed);
}

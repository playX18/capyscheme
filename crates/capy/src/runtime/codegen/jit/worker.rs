use std::cell::Cell;
use std::collections::VecDeque;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Duration;

use mmtk::util::Address;

use super::super::install::Generation;
use super::super::masm::Masm;
use super::super::template_image::ensure_loaded;
use super::super::unit::Tier;
use super::compile::{BuiltUnit, CompileRequest, attach, compiler_for, finish_optimized};
use super::promotion::release_claim;
use crate::heap::collection::Visitor;
use crate::heap::mutator::Mutator;
use crate::heap::{Gc, Trace, WeakProcessor};
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::proc::Closure;
use crate::runtime::objectmodel::{CodeBlock, CodeBuffer};
use crate::runtime::service::{self, ServiceCtl, ServiceThread};
use crate::utils::flags;

/// Pending compiles per worker past which submit evicts the oldest job
///
/// TODO: tune this, too low: boot jobs are evicted, too high: takes too long to do anything useful
const QUEUE_DEPTH_PER_WORKER: usize = 64;

/// Queued jobs per worker that justify another worker
const TASKS_PER_WORKER: usize = 4;

/// How long a worker above the floor waits for work before exiting, so the
/// pool shrinks back after a burst
const IDLE_TIMEOUT: Duration = Duration::from_millis(200);

/// The queue bound, scaled with the configured worker maximum
fn queue_depth() -> usize {
    QUEUE_DEPTH_PER_WORKER * max_workers().max(1)
}

/// The configured worker cap. `CAPY_JIT_WORKERS` is the maximum: the pool
/// starts with one worker and grows toward it under queue pressure.
fn max_workers() -> usize {
    flags::jit_workers()
}

/// How many workers the pool wants for `pending` queued jobs with `active`
/// live workers and a `max` cap. Growth only: shrinking is left to the
/// idle timeout in [`worker_loop`]. Pure, so tests can pin the curve.
fn desired_workers(pending: usize, active: usize, max: usize) -> usize {
    if max == 0 {
        return 0;
    }
    let floor = 1.min(max);
    let need = pending.div_ceil(TASKS_PER_WORKER).clamp(floor, max);
    need.max(active.min(max))
}

struct CompileJob<'gc> {
    id: usize,
    /// The tier whose compiler builds it.
    tier: Tier,
    generation: Generation,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    closure: Option<Gc<'gc, Closure<'gc>>>,
    /// The procedure's `file:line:col` for the image symbol, computed on
    /// the submitting mutator.
    label: Option<String>,
}

// SAFETY: the block and the closure are the only GC references.
unsafe impl Trace for CompileJob<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.code_block);
        if let Some(closure) = &mut self.closure {
            visitor.trace(closure);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

/// The bounded queue of jobs waiting for a worker
#[derive(Default)]
pub struct JobQueue<'gc> {
    jobs: Mutex<VecDeque<CompileJob<'gc>>>,
}

// SAFETY: every queued job's block is traced. The world is stopped, so no
// mutator holds the lock across the scan, and `&mut self` is exclusive.
unsafe impl<'gc> Trace for JobQueue<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let jobs = self
            .jobs
            .get_mut()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        for job in jobs.iter_mut() {
            visitor.trace(job);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

impl<'gc> JobQueue<'gc> {
    /// Append `job`, evicting and returning the oldest one if the queue is
    /// full.
    fn push(&self, job: CompileJob<'gc>) -> Option<CompileJob<'gc>> {
        let mut jobs = self.jobs.lock().unwrap();
        let evicted = if jobs.len() >= queue_depth() {
            jobs.pop_front()
        } else {
            None
        };
        jobs.push_back(job);
        evicted
    }

    fn pop(&self) -> Option<CompileJob<'gc>> {
        self.jobs.lock().unwrap().pop_front()
    }
}

/// A worker's mutator root: the job it is compiling, popped from the queue
/// and so no longer traced there.
struct WorkerRoot<'gc> {
    current: Cell<Option<CompileJob<'gc>>>,
}

// SAFETY: the current job is traced, and the world is stopped during a scan.
unsafe impl Trace for WorkerRoot<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        if let Some(job) = self.current.get_mut() {
            visitor.trace(job);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

/// How many pushed jobs no worker has claimed, so an idle worker can sleep
/// in native state (never holding up a collection) until there is one.
static PENDING: Mutex<usize> = Mutex::new(0);
static PENDING_READY: Condvar = Condvar::new();
static ACTIVE_WORKERS: AtomicUsize = AtomicUsize::new(0);
static NEXT_WORKER: AtomicUsize = AtomicUsize::new(0);
static SPAWN_LOCK: Mutex<()> = Mutex::new(());

const JIT_WORKER_STACK_BYTES: usize = 16 << 20;

struct JitWorker {
    index: usize,
}

impl ServiceThread for JitWorker {
    fn name(&self) -> String {
        format!("capy-jit-{}", self.index)
    }

    fn stack_size(&self) -> Option<usize> {
        Some(JIT_WORKER_STACK_BYTES)
    }

    fn run_service(&self, ctl: &ServiceCtl) {
        worker_loop(ctl);
    }

    fn stop_service(&self) {
        // Order the terminate flag before a worker that has not yet blocked.
        drop(PENDING.lock().unwrap_or_else(|e| e.into_inner()));
        PENDING_READY.notify_all();
    }
}

/// Spawn one worker, reserving its [`ACTIVE_WORKERS`] slot first. Returns
/// whether a thread is now (or will soon be) running.
fn spawn_one() -> bool {
    let index = NEXT_WORKER.fetch_add(1, Ordering::SeqCst);
    ACTIVE_WORKERS.fetch_add(1, Ordering::SeqCst);
    match service::start(Arc::new(JitWorker { index })) {
        Ok(_) => true,
        Err(err) => {
            ACTIVE_WORKERS.fetch_sub(1, Ordering::SeqCst);
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "worker thread failed to spawn: {err}");
            false
        }
    }
}

/// Grow the pool toward what the queue justifies: start the floor worker on
/// the first job, then add while each worker has more than its share, up to
/// the configured maximum. Best-effort: a failed spawn just leaves the
/// queue to the workers it has.
fn ensure_workers() {
    let max = max_workers();
    if max == 0 {
        return;
    }
    let _guard = SPAWN_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    let pending = *PENDING.lock().unwrap_or_else(|e| e.into_inner());
    if pending == 0 {
        return;
    }
    let active = ACTIVE_WORKERS.load(Ordering::SeqCst);
    for _ in active..desired_workers(pending, active, max) {
        if !spawn_one() {
            break;
        }
    }
}

/// Submit `code_block` for background compilation at `tier` as `id`
pub(crate) fn submit<'gc>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    id: usize,
    tier: Tier,
    generation: Generation,
    closure: Option<Gc<'gc, Closure<'gc>>>,
) {
    if service::is_shutting_down() {
        release_for(tier, code_block, generation);
        return;
    }
    if max_workers() == 0 {
        // No workers configured
        release_for(tier, code_block, generation);
        return;
    }
    ensure_loaded();
    let label = Some(crate::runtime::objectmodel::proc::procedure_label(
        code_block,
    ));
    let evicted = ctx.globals().jit_jobs.push(CompileJob {
        id,
        tier,
        generation,
        code_block,
        closure,
        label,
    });
    match evicted {
        // The queue length is unchanged: no new wakeup is owed.
        Some(old) => {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "evict job #{} {} (queue full)",
                old.id,
                old.label.as_deref().unwrap_or("<unknown procedure>")
            );

            release_for(old.tier, old.code_block, old.generation);
        }
        None => {
            *PENDING.lock().unwrap() += 1;
            ensure_workers();
            PENDING_READY.notify_one();
        }
    }
}

/// Compile jobs until the process exits, as a mutator: wait in native,
/// take and compile a job while mutating.
fn worker_loop(ctl: &ServiceCtl) {
    crate::runtime::profile::capture::block_sigprof_for_worker();
    // This thread's [`ACTIVE_WORKERS`] slot, dropped on exit by any path
    // (shutdown, idle timeout, or panic) so a later burst can grow again.
    struct Slot;
    impl Drop for Slot {
        fn drop(&mut self) {
            ACTIVE_WORKERS.fetch_sub(1, Ordering::SeqCst);
        }
    }
    let _slot = Slot;

    let mut masm = Masm::new();
    let mutator = Mutator::<crate::Rootable!(WorkerRoot<'_>)>::new(|_| WorkerRoot {
        current: Cell::new(None),
    });
    loop {
        // In native: a waiting worker never holds up a collection.
        {
            let (guard, timeout) = PENDING_READY
                .wait_timeout_while(PENDING.lock().unwrap(), IDLE_TIMEOUT, |pending| {
                    *pending == 0 && !ctl.should_terminate()
                })
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            if ctl.should_terminate() {
                break;
            }
            let mut pending = guard;
            if *pending > 0 {
                *pending -= 1;
            } else {
                // The wait timed out with no work
                debug_assert!(timeout.timed_out());
                if ACTIVE_WORKERS.load(Ordering::SeqCst) <= 1 {
                    continue;
                }
                return;
            }
        }
        mutator.mutate(|ctx, root| {
            // A submit may have evicted the job meanwhile.
            let job = ctx.globals().jit_jobs.pop()?;
            let (id, tier, generation, code_block, closure) =
                (job.id, job.tier, job.generation, job.code_block, job.closure);
            let buffer = code_block.buffer;
            let label = job.label.clone();
            root.current.set(Some(job));
            // The root traces the block, and the buffer never moves, so its
            // regions stay valid while compiling polls for collections.
            let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                compile_job(
                    ctx,
                    id,
                    tier,
                    code_block,
                    label.as_deref(),
                    closure,
                    &mut masm,
                )
            }))
            .unwrap_or_else(|_| {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "worker panicked on job #{id}");
                Err(())
            });
            if tier == Tier::Optimizing {
                // A Tier 2 failure leaves the baseline unit usable, so it
                // never sets `build_failed`.
                if ctl.should_terminate() {
                    finish_optimized(ctx, code_block, id, generation, Err(()));
                } else {
                    finish_optimized(ctx, code_block, id, generation, result);
                }
            } else if ctl.should_terminate() {
                if let Ok(built) = result {
                    std::mem::forget(built);
                }
                buffer.build_failed.store(true, Ordering::Release);
            } else {
                // The root still holds the buffer: attach before releasing it.
                finish_job(ctx, buffer, id, result);
            }
            // Released while mutating, so never during a root scan.
            root.current.set(None);
            Some(())
        });
    }
}

/// Attach a finished job's unit to its buffer, or mark the buffer's
/// compile failed.
fn finish_job<'gc>(
    ctx: MutationContext<'gc>,
    buffer: Gc<'gc, CodeBuffer<'gc>>,
    id: usize,
    result: Result<BuiltUnit<'gc>, ()>,
) {
    let attached = match result {
        Ok(built) => attach(ctx, buffer, built, id).map_err(|err| {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "job #{id} cannot attach: {err}");
        }),
        Err(()) => Err(()),
    };
    if attached.is_err() {
        // Installing mutators poison the block.
        buffer.build_failed.store(true, Ordering::Release);
    }
}

/// Release the claim of a job that will not run, so a later window retries.
fn release_for<'gc>(tier: Tier, code_block: Gc<'gc, CodeBlock<'gc>>, generation: Generation) {
    match tier {
        Tier::Optimizing => {
            // A job that will not run restores the phase its claim replaced.
            let _ = code_block.abort_opt(generation);
        }
        _ => release_claim(code_block),
    }
}

/// Compile one job
pub(crate) fn compile_job<'gc>(
    mutation: MutationContext<'gc>,
    id: usize,
    tier: Tier,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    label: Option<&str>,
    closure: Option<Gc<'gc, Closure<'gc>>>,
    masm: &mut Masm,
) -> Result<BuiltUnit<'gc>, ()> {
    let Some(compiler) = compiler_for(tier) else {
        log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "job #{id}: no compiler for tier {}", tier.name());
        return Err(());
    };
    let buffer = code_block.buffer;
    let request = CompileRequest {
        code: buffer.code(),
        md: Address::from_ptr(buffer.metadata()),
        pool: buffer.pool(),
        label,
        code_block,
        closure,
        mutation,
    };
    match compiler.build(&request, masm) {
        Ok(Some(built)) => Ok(built),
        Ok(None) => Err(()), // over the per-block limit, already logged
        Err(err) => {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " job #{id} failed: {err}");
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pool_starts_at_one_and_grows_with_pressure() {
        assert_eq!(desired_workers(1, 0, 8), 1, "first job starts the floor");
        assert_eq!(desired_workers(4, 1, 8), 1, "one share needs no growth");
        assert_eq!(desired_workers(5, 1, 8), 2, "past one share grows");
        assert_eq!(desired_workers(9, 2, 8), 3, "two shares grow again");
        assert_eq!(desired_workers(100, 2, 8), 8, "burst wants the cap");
    }

    #[test]
    fn pool_never_shrinks_here_or_exceeds_its_max() {
        assert_eq!(desired_workers(0, 3, 8), 3, "drained queue keeps workers");
        assert_eq!(desired_workers(1, 8, 8), 8, "at the cap stays at the cap");
        assert_eq!(desired_workers(100, 0, 1), 1, "a cap of one is the floor");
        assert_eq!(desired_workers(0, 0, 0), 0, "workers off wants none");
        assert_eq!(desired_workers(100, 0, 0), 0, "workers off never grows");
    }
}

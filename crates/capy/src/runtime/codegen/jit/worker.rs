use std::cell::Cell;
use std::collections::VecDeque;
use std::panic::AssertUnwindSafe;
use std::sync::atomic::Ordering::Release;
use std::sync::{Condvar, Mutex, Once};

use mmtk::util::Address;

use super::super::template_image::ensure_loaded;
use super::super::unit::Tier;
use super::compile::{BuiltUnit, CompileRequest, attach, compiler_for};
use super::promotion::release_claim;
use crate::heap::collection::Visitor;
use crate::heap::mutator::Mutator;
use crate::heap::{Gc, Trace, WeakProcessor};
use crate::runtime::MutationContext;
use crate::runtime::value::{CodeBlock, CodeBuffer};
use crate::utils::flags;

/// Pending compiles per worker past which submit evicts the oldest job
const QUEUE_DEPTH_PER_WORKER: usize = 16;

/// The queue bound, scaled with the worker count so more workers get
/// more in-flight work.
fn queue_depth() -> usize {
    QUEUE_DEPTH_PER_WORKER * flags::jit_workers().max(1)
}

struct CompileJob<'gc> {
    id: usize,
    /// The tier whose compiler builds it.
    tier: Tier,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    /// The procedure's `file:line:col` for the image symbol, computed on
    /// the submitting mutator.
    label: Option<String>,
}

// SAFETY: the block is the only GC reference.
unsafe impl Trace for CompileJob<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.code_block);
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
static THREADS: Once = Once::new();

fn spawn_workers() {
    THREADS.call_once(|| {
        for index in 0..flags::jit_workers() {
            if let Err(err) = std::thread::Builder::new()
                .name(format!("capy-jit-{index}"))
                .spawn(worker_loop)
            {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "worker thread failed to spawn: {err}");
            }
        }
    });
}

/// Submit `code_block` for background compilation at `tier` as `id`: queue
/// it, rooted through the VM globals. Compiled code jumps into the
/// interpreter image, and the worker only wakes on jobs, so ensure that
/// image is loaded first. A full queue evicts its oldest job, whose claim is
/// released so its block can fire again: the newest submit is the one hot
/// right now.
pub(crate) fn submit<'gc>(
    ctx: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    id: usize,
    tier: Tier,
) {
    ensure_loaded();
    spawn_workers();
    let label = crate::runtime::value::proc::procedure_span_short(ctx, code_block);
    let evicted = ctx.globals().jit_jobs.push(CompileJob {
        id,
        tier,
        code_block,
        label,
    });
    match evicted {
        // The queue length is unchanged: no new wakeup is owed.
        Some(old) => {
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "evict job #{} constant_pointer={} (queue full)",
                old.id,
                Address::from_ptr(old.code_block.buffer.entry())
            );

            release_claim(old.code_block);
        }
        None => {
            *PENDING.lock().unwrap() += 1;
            PENDING_READY.notify_one();
        }
    }
}

/// Compile jobs until the process exits, as a mutator: wait in native,
/// take and compile a job while mutating.
fn worker_loop() {
    crate::runtime::profile::capture::block_sigprof_for_worker();
    let mutator = Mutator::<crate::Rootable!(WorkerRoot<'_>)>::new(|_| WorkerRoot {
        current: Cell::new(None),
    });
    loop {
        // In native: a waiting worker never holds up a collection.
        {
            let mut pending = PENDING_READY
                .wait_while(PENDING.lock().unwrap(), |pending| *pending == 0)
                .unwrap();
            *pending -= 1;
        }
        mutator.mutate(|ctx, root| {
            // A submit may have evicted the job meanwhile.
            let job = ctx.globals().jit_jobs.pop()?;
            let (id, tier, buffer) = (job.id, job.tier, job.code_block.buffer);
            let label = job.label.clone();
            root.current.set(Some(job));
            // The root traces the block, and the buffer never moves, so its
            // regions stay valid while compiling polls for collections.
            let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                let md = Address::from_ptr(buffer.metadata());
                compile_job(id, tier, buffer.code(), md, buffer.pool(), label.as_deref())
            }))
            .unwrap_or_else(|_| {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "worker panicked on job #{id}");
                Err(())
            });
            // The root still holds the buffer: attach before releasing it.
            finish_job(ctx, buffer, id, result);
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
    result: Result<BuiltUnit, ()>,
) {
    let attached = match result {
        Ok(built) => attach(ctx, buffer, built, id).map_err(|err| {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "job #{id} cannot attach: {err}");
        }),
        Err(()) => Err(()),
    };
    if attached.is_err() {
        // Installing mutators poison the block.
        buffer.build_failed.store(true, Release);
    }
}

/// Compile one job: pure codegen over the rooted buffer regions.
fn compile_job(
    id: usize,
    tier: Tier,
    code: &[u8],
    md: Address,
    pool: &[u32],
    label: Option<&str>,
) -> Result<BuiltUnit, ()> {
    let Some(compiler) = compiler_for(tier) else {
        log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "job #{id}: no compiler for tier {}", tier.name());
        return Err(());
    };
    let request = CompileRequest {
        code,
        md,
        pool,
        label,
    };
    match compiler.build(&request) {
        Ok(Some(built)) => Ok(built),
        Ok(None) => Err(()), // over the per-block limit, already logged
        Err(err) => {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " job #{id} failed: {err}");
            Err(())
        }
    }
}

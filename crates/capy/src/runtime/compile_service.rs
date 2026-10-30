//! Compile threads
//!
//! Compile threads take a TreeIL and compile it to bytecode. Until we fully self-host the compiler
//! we have to rely on this, because thread stack size is rather small and can't grow for Rust stack,
//! so we instead run a few native threads that do the job for us with sufficiently large stack.

use std::cell::Cell;
use std::collections::{HashMap, VecDeque};
use std::panic::AssertUnwindSafe;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, Once};

use crate::heap::collection::Visitor;
use crate::heap::mutator::Mutator;
use crate::heap::{Trace, WeakProcessor};
use crate::runtime::MutationContext;
use crate::runtime::root::RootScope;
use crate::runtime::sched::scheduler::{Killed, Waker, current};
use crate::runtime::service::{self, ServiceCtl, ServiceThread};
use crate::runtime::thread::State;
use crate::runtime::value::{Str, Value};
use crate::runtime::vm::load::{ExpandedJob, SourceJob, run_expanded_job, run_source_job};
use crate::runtime::vm::threading::ThreadObject;
use crate::runtime::vm::thunks::make_io_error;
use crate::utils::flags;

const DEFAULT_THREADS: usize = 2;

const DEFAULT_STACK_KB: usize = 256 * 1024;

thread_local! {
    static ON_COMPILE_THREAD: Cell<bool> = const { Cell::new(false) };
}

#[derive(Clone)]
pub(crate) enum JobInput<'gc> {
    /// Expanded Scheme handed to `%compile`.
    Expanded(ExpandedJob<'gc>),
    /// A source file compiled by the Rust expander.
    Source(SourceJob),
}

pub(crate) enum Failure<'gc> {
    Scheme(Value<'gc>),
    Message(String),
}

impl<'gc> From<Value<'gc>> for Failure<'gc> {
    fn from(value: Value<'gc>) -> Self {
        Failure::Scheme(value)
    }
}

pub(crate) type Outcome<'gc> = Result<(), Failure<'gc>>;

enum Waiter {
    VirtualThread(Waker),
    Thread(Arc<Gate>),
}

#[derive(Default)]
struct Gate {
    done: Mutex<bool>,
    signal: Condvar,
}

impl Gate {
    fn fire(&self) {
        *self.done.lock().unwrap_or_else(|e| e.into_inner()) = true;
        self.signal.notify_all();
    }

    fn wait(&self) {
        let mut done = self.done.lock().unwrap_or_else(|e| e.into_inner());
        while !*done {
            done = self.signal.wait(done).unwrap_or_else(|e| e.into_inner());
        }
    }
}

impl Waiter {
    fn wake(self) {
        match self {
            Waiter::VirtualThread(waker) => waker.wake(),
            Waiter::Thread(gate) => gate.fire(),
        }
    }
}

struct Entry<'gc> {
    input: Option<JobInput<'gc>>,
    outcome: Option<Outcome<'gc>>,
    waiter: Option<Waiter>,
    abandoned: bool,
}

#[derive(Default)]
struct QueueState<'gc> {
    next_id: u64,
    pending: VecDeque<u64>,
    entries: HashMap<u64, Entry<'gc>>,
}

pub struct CompileQueue<'gc> {
    state: Mutex<QueueState<'gc>>,
}

impl Default for CompileQueue<'_> {
    fn default() -> Self {
        Self {
            state: Mutex::new(QueueState::default()),
        }
    }
}

unsafe impl<'gc> Trace for CompileQueue<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let state = self
            .state
            .get_mut()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        for entry in state.entries.values_mut() {
            if let Some(input) = &mut entry.input {
                visitor.trace(input);
            }
            if let Some(Err(Failure::Scheme(value))) = &mut entry.outcome {
                visitor.trace(value);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

unsafe impl Trace for JobInput<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        match self {
            JobInput::Expanded(job) => visitor.trace(&mut job.expanded),
            JobInput::Source(_) => {}
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

struct ThreadRoot<'gc> {
    current: Cell<Option<JobInput<'gc>>>,
}

unsafe impl Trace for ThreadRoot<'_> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        if let Some(input) = self.current.get_mut() {
            visitor.trace(input);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

static PENDING: Mutex<usize> = Mutex::new(0);
static PENDING_READY: Condvar = Condvar::new();
static STARTED: Once = Once::new();
static STARTED_THREADS: AtomicUsize = AtomicUsize::new(0);

struct CompileWorker {
    index: usize,
    stack_bytes: usize,
}

impl ServiceThread for CompileWorker {
    fn name(&self) -> String {
        format!("capy-compile-{}", self.index)
    }

    fn stack_size(&self) -> Option<usize> {
        Some(self.stack_bytes)
    }

    fn run_service(&self, ctl: &ServiceCtl) {
        worker_loop(ctl);
    }

    fn stop_service(&self) {
        drop(PENDING.lock().unwrap_or_else(|e| e.into_inner()));
        PENDING_READY.notify_all();
    }
}

fn positive(value: usize) -> Option<usize> {
    (value > 0).then_some(value)
}

fn thread_count() -> usize {
    positive(flags::compile_threads()).unwrap_or_else(|| {
        std::thread::available_parallelism()
            .map_or(1, |n| n.get())
            .min(DEFAULT_THREADS)
    })
}

fn stack_bytes() -> usize {
    positive(flags::compile_stack_kb()).unwrap_or(DEFAULT_STACK_KB) * 1024
}

fn ensure_threads() -> Result<(), String> {
    STARTED.call_once(|| {
        let (count, stack_bytes) = (thread_count(), stack_bytes());
        let mut started = 0;
        for index in 0..count {
            let worker = CompileWorker { index, stack_bytes };
            match service::start(Arc::new(worker)) {
                Ok(_) => started += 1,
                Err(err) => log::warn!("compile thread failed to spawn: {err}"),
            }
        }
        STARTED_THREADS.store(started, Ordering::SeqCst);
    });
    if STARTED_THREADS.load(Ordering::SeqCst) == 0 {
        return Err("cannot start compile threads".to_string());
    }
    Ok(())
}

pub(crate) fn run<'gc>(
    ctx: MutationContext<'gc>,
    input: JobInput<'gc>,
) -> (MutationContext<'gc>, Result<Outcome<'gc>, Killed>) {
    if ON_COMPILE_THREAD.with(Cell::get) {
        return (ctx, Ok(execute(ctx, input)));
    }

    if service::is_shutting_down() {
        let failure = Failure::Message("compile service is shutting down".to_string());
        return (ctx, Ok(Err(failure)));
    }

    if let Err(message) = ensure_threads() {
        return (ctx, Ok(Err(Failure::Message(message))));
    }

    let scope = RootScope::new(ctx);
    match &input {
        JobInput::Expanded(job) => {
            scope.root(job.expanded);
        }
        JobInput::Source(_) => {}
    }
    let id = enqueue(ctx, input);
    let result = wait(ctx, id);
    drop(scope);
    result
}

fn enqueue<'gc>(ctx: MutationContext<'gc>, input: JobInput<'gc>) -> u64 {
    let id = {
        let mut state = ctx
            .globals()
            .compile_jobs
            .state
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        let id = state.next_id;
        state.next_id += 1;
        state.entries.insert(
            id,
            Entry {
                input: Some(input),
                outcome: None,
                waiter: None,
                abandoned: false,
            },
        );
        state.pending.push_back(id);
        id
    };
    *PENDING.lock().unwrap_or_else(|e| e.into_inner()) += 1;
    PENDING_READY.notify_one();
    id
}

fn take_or_register<'gc>(
    ctx: MutationContext<'gc>,
    id: u64,
    waiter: impl FnOnce() -> Waiter,
) -> Option<Outcome<'gc>> {
    let mut state = ctx
        .globals()
        .compile_jobs
        .state
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let entry = state.entries.get_mut(&id)?;
    if let Some(outcome) = entry.outcome.take() {
        state.entries.remove(&id);
        return Some(outcome);
    }
    entry.waiter = Some(waiter());
    None
}

fn abandon(ctx: MutationContext<'_>, id: u64) {
    let mut state = ctx
        .globals()
        .compile_jobs
        .state
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    if let Some(entry) = state.entries.get_mut(&id) {
        if entry.outcome.is_some() {
            state.entries.remove(&id);
        } else {
            entry.abandoned = true;
            entry.waiter = None;
        }
    }
}

fn wait<'gc>(
    mut ctx: MutationContext<'gc>,
    id: u64,
) -> (MutationContext<'gc>, Result<Outcome<'gc>, Killed>) {
    let thread = current();
    loop {
        let gate = Arc::new(Gate::default());
        let ready = take_or_register(ctx, id, || match &thread {
            Some(cx) => Waiter::VirtualThread(cx.waker()),
            None => Waiter::Thread(gate.clone()),
        });
        if let Some(outcome) = ready {
            return (ctx, Ok(outcome));
        }
        match &thread {
            Some(cx) => {
                let (next, parked) = cx.park_with(ctx);
                ctx = next;
                if let Err(killed) = parked {
                    abandon(ctx, id);
                    return (ctx, Err(killed));
                }
            }
            None => ctx.call_in_native(|| gate.wait()),
        }
    }
}

fn worker_loop(ctl: &ServiceCtl) {
    ON_COMPILE_THREAD.with(|flag| flag.set(true));
    crate::runtime::profile::capture::block_sigprof_for_worker();
    let mutator = Mutator::<crate::Rootable!(ThreadRoot<'_>)>::new(|mc| {
        let thread_object = ThreadObject::new(mc, None);
        thread_object.bind_current_runtime_thread();
        mc.init_state(State::new(mc, thread_object));
        ThreadRoot {
            current: Cell::new(None),
        }
    });
    loop {
        {
            let mut pending = PENDING_READY
                .wait_while(
                    PENDING.lock().unwrap_or_else(|e| e.into_inner()),
                    |pending| *pending == 0 && !ctl.should_terminate(),
                )
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            if ctl.should_terminate() {
                break;
            }
            *pending -= 1;
        }
        mutator.mutate(|ctx, root| {
            let Some((id, input)) = take_next(ctx) else {
                return;
            };

            root.current.set(Some(input.clone()));

            let outcome = std::panic::catch_unwind(AssertUnwindSafe(|| execute(ctx, input)))
                .unwrap_or_else(|payload| {
                    let text = payload
                        .downcast_ref::<&str>()
                        .map(|s| s.to_string())
                        .or_else(|| payload.downcast_ref::<String>().cloned())
                        .unwrap_or_else(|| "unknown panic".to_string());
                    log::warn!("compile thread panicked on job #{id}: {text}");
                    Err(Failure::Message(format!("compile thread panicked: {text}")))
                });
            root.current.set(None);
            finish(ctx, id, outcome);
        });
    }
}

fn take_next<'gc>(ctx: MutationContext<'gc>) -> Option<(u64, JobInput<'gc>)> {
    let mut state = ctx
        .globals()
        .compile_jobs
        .state
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let id = state.pending.pop_front()?;
    let input = state.entries.get(&id)?.input.clone()?;
    Some((id, input))
}

fn finish<'gc>(ctx: MutationContext<'gc>, id: u64, outcome: Outcome<'gc>) {
    let waiter = {
        let mut state = ctx
            .globals()
            .compile_jobs
            .state
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        let Some(entry) = state.entries.get_mut(&id) else {
            return;
        };
        entry.input = None;
        if entry.abandoned {
            state.entries.remove(&id);
            return;
        }
        entry.outcome = Some(outcome);
        entry.waiter.take()
    };
    if let Some(waiter) = waiter {
        waiter.wake();
    }
}

fn execute<'gc>(ctx: MutationContext<'gc>, input: JobInput<'gc>) -> Outcome<'gc> {
    match input {
        JobInput::Expanded(job) => run_expanded_job(ctx, job),
        JobInput::Source(job) => run_source_job(ctx, job),
    }
}

pub(crate) fn raise_failure<'gc>(
    ctx: MutationContext<'gc>,
    outcome: Outcome<'gc>,
) -> Result<(), Value<'gc>> {
    match outcome {
        Ok(()) => Ok(()),
        Err(Failure::Scheme(value)) => Err(value),
        Err(Failure::Message(message)) => Err(make_io_error(
            ctx,
            "compile",
            Str::new(ctx, message, true).into(),
            &[],
        )),
    }
}

//! Scheduler for threads

use std::any::Any;
use std::cell::{Cell, UnsafeCell};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::ptr::NonNull;
use std::sync::atomic::{
    AtomicBool, AtomicI32, AtomicPtr, AtomicU8, AtomicU32, AtomicU64, AtomicUsize, Ordering,
};
use std::sync::{Arc, OnceLock, Weak};
use std::time::Duration;

use mmtk::util::Address;
use parking_lot::{Condvar, Mutex};

use super::context::{SavedContext, context_init, context_switch};
use super::preempt;
use super::queue::{DYNAMIC_POOL_MAX, PooledDynamic, Shared, WorkerRng};
use super::reactor::{self, Dir, WAKE_CLOSED, WAKE_NONE, WAKE_READY, WAKE_TIMER};
use super::stack::{Stack, StackCache, install_guard_handler};
use super::sysmon;
use crate::heap::mutator::Mutator;
use crate::heap::sync::thread::{
    NativeCtx, Thread, VirtualThread, current_thread, try_current_native_data,
};
use crate::prelude::Value;
use crate::runtime::MutationContext;
use crate::runtime::fluids::DynamicState;
use crate::runtime::interp::stack::{StackPool, retire_stacks};
use crate::runtime::thread::{State, Tc};
use crate::runtime::vm::threading::ThreadObject;
use crate::utils::flags;

type Entry = Box<dyn FnOnce(SchedCtx) + Send>;

#[derive(Clone, Copy)]
pub(crate) struct SchemeStart {
    pub(crate) thread: Value<'static>,
    pub(crate) dynamic: Value<'static>,
}

impl SchemeStart {
    pub(crate) fn new<'gc>(thread: Value<'gc>, dynamic: Value<'gc>) -> Self {
        Self {
            thread: unsafe { std::mem::transmute::<Value<'gc>, Value<'static>>(thread) },
            dynamic: unsafe { std::mem::transmute::<Value<'gc>, Value<'static>>(dynamic) },
        }
    }
}

pub(crate) const NO_AFFINITY: usize = usize::MAX;

/// Number of queue levels. Level `0` is the most urgent.
pub(crate) const NLEVELS: u8 = 4;

/// Why a thread left its worker, as seen by [`next_level`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SwitchOut {
    /// It ran its whole quantum, or sysmon preempted it for a more urgent
    /// thread. Its level goes down one.
    Quantum,
    /// It yielded voluntarily. Its level stays the same.
    Yield,
    /// It parked before its quantum ran out, for example on I/O or a channel.
    /// Its level goes up one.
    Blocked,
}

/// The level a thread is queued at after it leaves a worker
pub(crate) fn next_level(base: u8, level: u8, leave: SwitchOut) -> u8 {
    let level = match leave {
        SwitchOut::Quantum => level.saturating_add(1).min(NLEVELS - 1),
        SwitchOut::Yield => level,
        SwitchOut::Blocked => level.saturating_sub(1),
    };
    level.max(base)
}

const PHASE_RUNNABLE: u8 = 0;
/// Spawned but never switched in
const PHASE_NEW: u8 = 6;
const PHASE_RUNNING: u8 = 1;
/// Switched out to park
const PHASE_PARKING: u8 = 2;
const PHASE_PARKED: u8 = 3;
const PHASE_YIELDING: u8 = 4;
const PHASE_DONE: u8 = 5;
/// Preempted at a yieldpoint because its quantum ran out or a more urgent
/// thread is waiting.
const PHASE_PREEMPTED: u8 = 7;

/// Kill states of a thread: `Live -> Requested -> Dying ->
/// Done`. `Requested` is a pending kill, `Dying` is the unwind, during which
/// further kills and interrupts are ignored.
pub(crate) const KILL_LIVE: u8 = 0;
pub(crate) const KILL_REQUESTED: u8 = 1;
pub(crate) const KILL_DYING: u8 = 2;
pub(crate) const KILL_DONE: u8 = 3;

/// Marks a thread unwinding because it was killed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Killed;

impl Killed {
    /// Unwind the current Rust stack with this marker.
    pub(crate) fn unwind(self) -> ! {
        std::panic::resume_unwind(Box::new(self))
    }
}

/// VirtualThread id of the main thread started by [`run_main`], or 0
static MAIN_VIRTUAL_THREAD: AtomicU64 = AtomicU64::new(0);

/// A thread ID.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VirtualThreadId(u64);

impl VirtualThreadId {
    /// The raw id
    pub fn as_u64(self) -> u64 {
        self.0
    }

    pub(crate) fn from_u64(raw: u64) -> Self {
        Self(raw)
    }
}

/// Why a thread did not finish normally.
#[derive(Debug)]
pub enum JoinError {
    /// The thread body panicked.
    Panicked(Box<dyn Any + Send>),
    /// The thread was killed and unwound.
    Killed,
}

/// Configuration for a [`Runtime`].
#[derive(Clone, Copy, Debug)]
pub struct Config {
    /// Number of worker OS threads.
    pub workers: usize,
    /// Usable stack size of a spawned thread, in KiB.
    pub stack_kb: usize,
    /// Time a thread may run while others are queued before sysmon
    /// preempts it at its next yieldpoint.
    pub quantum: Duration,
}

const DEFAULT_STACK_KB: usize = 2048;
const DEFAULT_QUANTUM_MS: u64 = 10;

impl Config {
    /// Read the `workers`, `thread_stack_kb` and `thread_quantum_ms`
    /// flags.
    pub(crate) fn from_env() -> Self {
        let workers = Some(flags::workers())
            .filter(|&n| n > 0)
            .unwrap_or_else(|| std::thread::available_parallelism().map_or(1, |n| n.get()));
        let stack_kb = Some(flags::thread_stack_kb())
            .filter(|&n| n > 0)
            .unwrap_or(DEFAULT_STACK_KB);
        let quantum_ms = Some(flags::thread_quantum_ms())
            .filter(|&n| n > 0)
            .map_or(DEFAULT_QUANTUM_MS, |ms| ms as u64);
        Self {
            workers,
            stack_kb,
            quantum: Duration::from_millis(quantum_ms),
        }
    }
}

static RUNTIMES: Mutex<Vec<Weak<Shared>>> = Mutex::new(Vec::new());
static NEXT_VIRTUAL_THREAD_ID: AtomicU64 = AtomicU64::new(1);

/// Every thread that has not finished.
static LIVE: [LiveShard; LIVE_SHARDS] =
    [const { LiveShard(Mutex::new(HashMap::with_hasher(IdHash))) }; LIVE_SHARDS];

const LIVE_SHARDS: usize = 64;

#[repr(align(64))]
struct LiveShard(Mutex<LiveMap>);

type LiveMap = HashMap<u64, Arc<Task>, IdHash>;

#[derive(Clone, Copy, Default)]
struct IdHash;

struct IdHasher(u64);

impl std::hash::BuildHasher for IdHash {
    type Hasher = IdHasher;
    fn build_hasher(&self) -> IdHasher {
        IdHasher(0)
    }
}

impl std::hash::Hasher for IdHasher {
    fn finish(&self) -> u64 {
        self.0
    }
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.0 = (self.0 ^ b as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
        }
    }
    fn write_u64(&mut self, n: u64) {
        self.0 = n.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }
}

/// The shard of [`LIVE`] that holds thread `id`.
fn live_shard(id: u64) -> &'static Mutex<LiveMap> {
    &LIVE[id as usize % LIVE_SHARDS].0
}

/// Set while the collector walks [`LIVE`]. A thread must not wake or
/// interrupt another while its state is being traced.
static SCANNING: AtomicBool = AtomicBool::new(false);

/// A pool of worker threads that run threads.
///
/// Workers are OS threads bound to the collector. They run until
/// [`Runtime::shutdown`] is called. Dropping a runtime without shutting it down
/// leaves its workers running.
pub struct Runtime {
    shared: Arc<Shared>,
}

/// Initialise the collector once per process.
pub(crate) fn ensure_gc() {
    crate::runtime::thread::SCM_INITIALIZED.call_once(|| {
        crate::heap::logging::init_rust_logger();
        let mmtk_builder = crate::heap::logging::mmtk_builder();
        crate::heap::GarbageCollector::init(mmtk_builder);
    });
}

/// Bring the VM core up before any thread builds its state.
fn bring_up_vm() {
    static VM_BOOT: std::sync::Once = std::sync::Once::new();
    VM_BOOT.call_once(|| {
        std::thread::spawn(|| {
            let scm = crate::runtime::Scheme::new_uninit();
            scm.enter(|ctx| crate::runtime::init_once(ctx));
        })
        .join()
        .expect("failed to initialise the VM for threads");
    });
}

/// Start the workers, reactor and sysmon for a new runtime.
fn start_workers(cfg: Config) -> Arc<Shared> {
    install_guard_handler();
    preempt::install();
    ensure_gc();
    bring_up_vm();
    let workers = cfg.workers.max(1);
    let shared = Arc::new(Shared::new(
        workers,
        cfg.stack_kb.max(1) * 1024,
        cfg.quantum,
    ));
    RUNTIMES.lock().push(Arc::downgrade(&shared));
    for index in 0..workers {
        let shared = shared.clone();
        std::thread::Builder::new()
            .name(format!("capy-worker-{index}"))
            .spawn(move || worker_main(shared, index))
            .expect("failed to start scheduler worker");
    }
    sysmon::spawn(shared.clone());
    shared
}

impl Runtime {
    /// Start a runtime with the workers, reactor and sysmon from `cfg`.
    pub fn start(cfg: Config) -> Self {
        let shared = start_workers(cfg);
        let _ = GLOBAL.set(shared.clone());
        Self { shared }
    }

    /// Run `f` as a thread on this runtime and block the calling OS thread
    /// until it finishes.
    ///
    /// Must not be called from a thread, because it blocks a worker.
    pub fn block_on_main<F>(&self, f: F) -> Result<(), JoinError>
    where
        F: FnOnce(SchedCtx) + Send + 'static,
    {
        self.spawn(f).join_blocking()
    }

    /// Stop the workers and wait up to one second for them to exit.
    pub fn shutdown(self) {
        self.shared.stop.store(true, Ordering::SeqCst);
        let deadline = std::time::Instant::now() + Duration::from_secs(1);
        while self.shared.running_workers() > 0 && std::time::Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
    }

    pub(crate) fn spawn<F>(&self, f: F) -> JoinHandle
    where
        F: FnOnce(SchedCtx) + Send + 'static,
    {
        Builder::new().spawn_on(&self.shared, f, None)
    }

    pub(crate) fn shared(&self) -> Arc<Shared> {
        self.shared.clone()
    }

    /// The OS threads of the workers that have started.
    pub(crate) fn worker_pthreads(&self) -> Vec<libc::pthread_t> {
        self.shared
            .slots
            .iter()
            .filter_map(|slot| slot.thread.get())
            .map(|thread| {
                // SAFETY: a bound worker's native data lives as long as the worker,
                // which is for the life of this runtime.
                let native = unsafe { &*thread.native_data_mut_ptr() };
                native.pthread.load(Ordering::Relaxed)
            })
            .filter(|&p| p != 0)
            .map(|p| p as libc::pthread_t)
            .collect()
    }
}

/// The process-wide runtime, started with [`Config::from_env`] on first use
/// unless [`Runtime::start`] or [`run_main`] came first.
static GLOBAL: OnceLock<Arc<Shared>> = OnceLock::new();

fn global() -> &'static Arc<Shared> {
    GLOBAL.get_or_init(|| start_workers(Config::from_env()))
}

/// The stack pool of the worker running the calling thread.
pub(crate) fn stack_pool() -> Option<&'static StackPool> {
    let cx = current()?;
    let g = cx.g();
    let shared = g.sched.shared.get();
    if shared.is_null() {
        return None;
    }
    // SAFETY: a thread's task holds an `Arc` of its runtime, so `shared`
    // outlives the thread and the pool is only used while it runs.
    Some(unsafe { &(&(*shared).slots)[g.sched.worker.get()].stack_pool })
}

fn dynamic_pool() -> Option<&'static Mutex<Vec<PooledDynamic>>> {
    let cx = current()?;
    let g = cx.g();
    let shared = g.sched.shared.get();
    if shared.is_null() {
        return None;
    }
    // SAFETY: as for `stack_pool`.
    Some(unsafe { &(&(*shared).slots)[g.sched.worker.get()].dynamic_pool })
}

/// Empty every worker's stack pool
fn clear_stack_pools() {
    for shared in RUNTIMES.lock().iter().filter_map(Weak::upgrade) {
        for slot in &shared.slots {
            slot.stack_pool.clear();
            slot.dynamic_pool.lock().clear();
        }
    }
}

/// Whether the caller is a thread other than the main one.
pub(crate) fn is_spawned_virtual_thread() -> bool {
    current().is_some_and(|cx| cx.id().as_u64() != MAIN_VIRTUAL_THREAD.load(Ordering::Relaxed))
}

/// Spawn a thread on the process wide runtime
pub fn spawn<F>(f: F) -> JoinHandle
where
    F: FnOnce(SchedCtx) + Send + 'static,
{
    Builder::new().spawn(f)
}

/// Spawn a thread that runs Scheme code
pub(crate) fn spawn_scheme(
    start: SchemeStart,
    entry: impl FnOnce(SchedCtx) + Send + 'static,
) -> VirtualThreadId {
    Builder::new().spawn_on(global(), entry, Some(start)).id()
}

/// Run `entry` as the main thread, with the calling thread as worker 0.
pub(crate) fn run_main<F>(mutator: &Mutator<crate::Rootable!(())>, stack_bytes: usize, entry: F)
where
    F: FnOnce(SchedCtx),
{
    struct AssertSend<T>(T);
    unsafe impl<T> Send for AssertSend<T> {}

    struct StopOnDrop(Arc<Shared>);
    impl Drop for StopOnDrop {
        fn drop(&mut self) {
            self.0.stop.store(true, Ordering::SeqCst);
        }
    }

    install_guard_handler();
    preempt::install();
    let cfg = Config::from_env();
    let workers = cfg.workers.max(1);

    let shared = Arc::new(Shared::new(
        workers,
        cfg.stack_kb.max(1) * 1024,
        cfg.quantum,
    ));
    RUNTIMES.lock().push(Arc::downgrade(&shared));
    if GLOBAL.set(shared.clone()).is_err() {
        panic!("the thread scheduler is already running");
    }

    let entry = AssertSend(entry);
    let stop = StopOnDrop(shared.clone());
    let main: Box<dyn FnOnce(SchedCtx) + '_> = Box::new(move |cx: SchedCtx| {
        let _stop = stop;
        let entry = entry;
        cx.with_mutation(|_| (entry.0)(cx));
    });
    // SAFETY: the box is consumed by the main thread before this function
    // returns
    let main: Entry = unsafe { std::mem::transmute::<Box<dyn FnOnce(SchedCtx) + '_>, Entry>(main) };

    // Build the main thread's start state and queue it while the other
    // workers are not running yet
    mutator.mutate(|mc, _| {
        let thread = ThreadObject::new(mc, None);
        let dynamic = mc.dynamic_state();
        let start = SchemeStart::new(thread.into(), dynamic);
        let handle = Builder {
            stack_size: Some(stack_bytes),
            name: None,
            affinity: None,
            priority: None,
        }
        .spawn_on(&shared, main, Some(start));

        MAIN_VIRTUAL_THREAD.store(handle.id().as_u64(), Ordering::SeqCst);

        let native = crate::heap::sync::thread::current_native_data();
        // SAFETY: this thread is mutating
        unsafe { native.flush_lab(&mut *native.home_virtual_thread) };
    });

    for index in 1..workers {
        let shared = shared.clone();
        std::thread::Builder::new()
            .name(format!("capy-worker-{index}"))
            .spawn(move || worker_main(shared, index))
            .expect("failed to start scheduler worker");
    }
    sysmon::spawn(shared.clone());

    worker_loop(&shared, 0, mutator);

    // Give the other workers a moment to notice the stop flag
    let deadline = std::time::Instant::now() + Duration::from_secs(1);
    while shared.running_workers() > 0 && std::time::Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(1));
    }
}

/// Why [`SchedCtx::pin_to_worker`] failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PinError {
    /// No running worker has that index.
    NoSuchWorker,
    /// The thread is inside a native call and cannot change threads.
    InNativeFrame,
}

/// Options for a thread before it is spawned.
#[derive(Default)]
pub struct Builder {
    stack_size: Option<usize>,
    name: Option<String>,
    affinity: Option<usize>,
    priority: Option<u8>,
}

impl Builder {
    /// Default options
    pub fn new() -> Self {
        Self::default()
    }

    /// Usable stack bytes for this thread. The default is the runtime's
    /// configured stack size
    pub fn stack_size(mut self, bytes: usize) -> Self {
        self.stack_size = Some(bytes);
        self
    }

    /// A name for thread. See [`SchedCtx::name`].
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Run the thread only on worker `worker`. Worker 0 is the thread that called
    /// `run_main`, the process's main thread. You can use this for e.g UI libraries or OpenGL
    /// which require code to run on main thread.
    pub fn pinned_to(mut self, worker: usize) -> Self {
        self.affinity = Some(worker);
        self
    }

    /// The base priority, `0` (most urgent) to `NLEVELS - 1`. Values past the
    /// last level are clamped. A thread's level moves away from this base as
    /// it runs, but never becomes more urgent than it. See [`Task::base`].
    pub fn priority(mut self, level: u8) -> Self {
        self.priority = Some(level.min(NLEVELS - 1));
        self
    }

    /// Spawn the thread on the process-wide runtime.
    pub fn spawn<F>(self, f: F) -> JoinHandle
    where
        F: FnOnce(SchedCtx) + Send + 'static,
    {
        self.spawn_on(global(), f, None)
    }

    pub(crate) fn spawn_on<F>(
        self,
        shared: &Arc<Shared>,
        f: F,
        start: Option<SchemeStart>,
    ) -> JoinHandle
    where
        F: FnOnce(SchedCtx) + Send + 'static,
    {
        let stack_bytes = self.stack_size.unwrap_or(shared.stack_bytes);
        let id = VirtualThreadId(NEXT_VIRTUAL_THREAD_ID.fetch_add(1, Ordering::Relaxed));
        let entry: Entry = Box::new(f);
        let cached = shared.gfree_get(current_worker_of(shared));
        let task = Arc::new_cyclic(|weak| {
            let thread: *mut VirtualThread = match cached {
                Some(mut g) => {
                    g.reinit(weak.as_ptr(), entry, start);
                    Box::into_raw(g)
                }
                None => Box::into_raw(Box::new(VirtualThread {
                    ap: Address::ZERO,
                    eap: Address::ZERO,
                    mutation_ptr: std::ptr::null(),
                    take_yieldpoint: AtomicI32::new(0),
                    tc: UnsafeCell::new(Tc::new()),
                    state: UnsafeCell::new(MaybeUninit::uninit()),
                    sched: VirtualThreadSched::new(weak.as_ptr(), Some(entry), start),
                })),
            };
            Task {
                id,
                name: self.name,
                shared: shared.clone(),
                stack_bytes,
                inner: Mutex::new(Inner::default()),
                finished: Condvar::new(),
                thread: AtomicPtr::new(thread),
                self_ref: weak.clone(),
                phase: AtomicU8::new(PHASE_NEW),
                notified: AtomicBool::new(false),
                pins: AtomicU32::new(0),
                home: AtomicUsize::new(0),
                affinity: AtomicUsize::new(self.affinity.unwrap_or(NO_AFFINITY)),
                base: AtomicU8::new(self.priority.unwrap_or(0)),
                level: AtomicU8::new(self.priority.unwrap_or(0)),
                wait_gen: AtomicU64::new(0),
                wake_reason: AtomicU8::new(WAKE_NONE),
                interrupted: AtomicBool::new(false),
            }
        });
        live_shard(id.0).lock().insert(id.0, task.clone());
        submit(shared, task.clone());
        JoinHandle { task }
    }
}

/// Queue a new task. Queues to local worker if there is any, otherwise to global queue.
fn submit(shared: &Arc<Shared>, task: Arc<Task>) {
    let affinity = task.affinity.load(Ordering::SeqCst);
    if affinity != NO_AFFINITY {
        shared.push_pinned(affinity, task);
        return;
    }
    match current_worker_of(shared) {
        Some(w) => shared.push_local(w, task, true),
        None => shared.push_global(task),
    }
}

/// The worker running the calling thread, if it belongs to `shared`.
fn current_worker_of(shared: &Shared) -> Option<usize> {
    let cx = current()?;
    let g = cx.g();
    (g.sched.shared.get() == shared as *const Shared).then(|| g.sched.worker.get())
}

pub(crate) struct Inner {
    pub(crate) result: Option<Result<(), JoinError>>,
    pub(crate) joiners: smallvec::SmallVec<[Arc<Task>; 1]>,
}

impl Default for Inner {
    fn default() -> Self {
        Self {
            result: None,
            joiners: smallvec::SmallVec::new(),
        }
    }
}

pub(crate) struct Task {
    pub(crate) id: VirtualThreadId,
    pub(crate) name: Option<String>,
    pub(crate) shared: Arc<Shared>,
    pub(crate) stack_bytes: usize,
    pub(crate) inner: Mutex<Inner>,
    pub(crate) finished: Condvar,
    /// The header, null once the thread finished and the worker took it
    /// back for the free list.
    thread: AtomicPtr<VirtualThread>,
    pub(crate) self_ref: Weak<Task>,
    pub(crate) phase: AtomicU8,
    /// An unpark arrived while the thread was not parked. The next park
    /// returns at once. Spurious early returns are allowed, lost wakes are not.
    pub(crate) notified: AtomicBool,
    /// Native depth
    pub(crate) pins: AtomicU32,
    /// The worker the thread last ran on. A pinned thread resumes here.
    pub(crate) home: AtomicUsize,
    /// The only worker allowed to run this thread, or [`NO_AFFINITY`]. Unlike
    /// `pins` this is chosen by the program (see [`SchedCtx::pin_to_worker`]):
    /// it is honoured when the thread is woken, yielded, spawned or stolen from.
    pub(crate) affinity: AtomicUsize,
    /// The priority the program asked for, `0` (most urgent) to `NLEVELS - 1`.
    pub(crate) base: AtomicU8,
    /// The queue level, within `base..NLEVELS`. It goes down on a full quantum
    /// and up on a park, and sysmon's boost resets it to `base`.
    pub(crate) level: AtomicU8,
    /// Generation of the current reactor wait (fd or timer). The reactor
    /// ignores a waiter whose generation is no longer this one.
    pub(crate) wait_gen: AtomicU64,
    /// Why the reactor woke the current wait.
    pub(crate) wake_reason: AtomicU8,
    /// Set by `interrupt_virtual_thread`, cleared when an await reports it.
    pub(crate) interrupted: AtomicBool,
}

unsafe impl Send for Task {}
unsafe impl Sync for Task {}

impl Task {
    /// Move the level for a thread that just left its worker
    pub(crate) fn adapt_level(&self, leave: SwitchOut) {
        let base = self.base.load(Ordering::Relaxed);
        let level = self.level.load(Ordering::Relaxed);
        self.level
            .store(next_level(base, level, leave), Ordering::Relaxed);
    }

    /// Start a new reactor wait with generation `generation`
    pub(crate) fn arm_wait(&self, generation: u64) {
        self.wake_reason.store(WAKE_NONE, Ordering::SeqCst);
        self.wait_gen.store(generation, Ordering::SeqCst);
    }

    pub(crate) fn wait_gen(&self) -> u64 {
        self.wait_gen.load(Ordering::SeqCst)
    }

    pub(crate) fn set_wake_reason(&self, reason: u8) {
        self.wake_reason.store(reason, Ordering::SeqCst);
    }

    pub(crate) fn virtual_thread_kill_state(&self) -> u8 {
        let _inner = self.inner.lock();
        let gp = self.thread.load(Ordering::Acquire);
        if gp.is_null() {
            return KILL_DONE;
        }
        // SAFETY: `inner` is held, so the header is not recycled.
        unsafe { (*gp).sched.kill.load(Ordering::SeqCst) }
    }

    /// The header of a task
    fn header(&self) -> *mut VirtualThread {
        let gp = self.thread.load(Ordering::Acquire);
        debug_assert!(!gp.is_null(), "header of a finished task");
        gp
    }

    /// Take the header back after the thread switched out for the last time.
    fn detach_header(&self) -> *mut VirtualThread {
        let _inner = self.inner.lock();
        self.thread.swap(std::ptr::null_mut(), Ordering::AcqRel)
    }
}

impl Drop for Task {
    fn drop(&mut self) {
        // SAFETY: created by `Box::into_raw` in `spawn_on`, and this is the
        // last owner.
        unsafe {
            let gp = *self.thread.get_mut();
            if !gp.is_null() {
                let thread = Box::from_raw(gp);
                if thread.sched.state_ready.get() {
                    std::ptr::drop_in_place(thread.state_ptr());
                }
            }
        }
    }
}

/// Request that thread `id` be killed
pub(crate) fn kill_virtual_thread(id: VirtualThreadId) -> bool {
    debug_assert!(
        !(SCANNING.load(Ordering::SeqCst) && current().is_some()),
        "a thread was killed while the collector traces threads"
    );
    let Some(task) = live_shard(id.0).lock().get(&id.0).cloned() else {
        return false;
    };
    {
        let _inner = task.inner.lock();
        let gp = task.thread.load(Ordering::Acquire);
        if gp.is_null() {
            return false;
        }

        unsafe {
            let sched = &(*gp).sched;
            if sched
                .kill
                .compare_exchange(
                    KILL_LIVE,
                    KILL_REQUESTED,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                )
                .is_err()
            {
                return false;
            }
            (*gp).take_yieldpoint.store(1, Ordering::SeqCst);
            (*(*gp).tc.get())
                .esp
                .store(std::ptr::null_mut(), Ordering::SeqCst);
        }
    }
    if MAIN_VIRTUAL_THREAD.load(Ordering::SeqCst) == id.0 {
        // Killing the main thread ends the program, as `(exit)` does.
        std::process::exit(1);
    }
    unpark(&task);
    true
}

/// Start the unwind of the running thread by its own request, if it is
/// live or requested. Returns `false` when there is no thread or it is
/// already dying. The caller unwinds with [`Killed::unwind`].
pub(crate) fn kill_current() -> bool {
    let Some(cx) = current() else {
        return false;
    };
    let sched = &cx.g().sched;
    let state = sched.kill.load(Ordering::SeqCst);
    if state != KILL_LIVE && state != KILL_REQUESTED {
        return false;
    }
    sched.kill.store(KILL_DYING, Ordering::SeqCst);
    true
}

/// Deliver a pending kill of the running thread, if any, at a switch point
/// in Rust code (the end of a `preemptible` region).
pub(crate) fn check_kill_now() -> Result<(), Killed> {
    match current() {
        Some(cx) => cx.check_kill(),
        None => Ok(()),
    }
}

/// Called at a yieldpoint, after GC blocking and before interrupts. A kill
/// that is due starts the unwind through Scheme: the thread goes `Dying`,
/// and the exit longjmps to the innermost Scheme activation, which the nearest
/// `call_scheme` return turns into a [`Killed`] panic. A pinned thread
/// keeps the request pending and re-arms its yieldpoint.
///
/// # Safety
///
/// Must be called from a yieldpoint inside a Scheme activation, where
/// [`crate::runtime::vm::scheme_longjmp`] has a target.
pub(crate) unsafe fn kill_at_yieldpoint(ctx: MutationContext<'_>) {
    let Some(cx) = current() else {
        return;
    };
    let sched = &cx.g().sched;
    if sched.kill.load(Ordering::SeqCst) != KILL_REQUESTED {
        return;
    }
    if cx.task().pins.load(Ordering::SeqCst) > 0 {
        if let Some(thread) = try_current_native_data().map(|nd| nd.thread()) {
            thread.request_yieldpoint(crate::heap::sync::thread::YieldpointKind::Full);
        }
        return;
    }
    sched.kill.store(KILL_DYING, Ordering::SeqCst);
    sched.kill_exit.set(true);
    // SAFETY: forwarded from the caller's contract.
    unsafe {
        crate::runtime::vm::scheme_longjmp(
            ctx,
            crate::runtime::value::ReturnCode::ReturnErr,
            crate::prelude::Value::undefined(),
        )
    }
}

/// Whether the running thread is unwinding. Interrupts are ignored and
/// parks run to completion while it is.
pub(crate) fn is_dying() -> bool {
    current().is_some_and(|cx| cx.g().sched.kill.load(Ordering::SeqCst) == KILL_DYING)
}

/// Consume the kill-exit marker of the running thread. `call_scheme`
/// calls this when an activation ends with an error, and a `true` answer
/// means that error is a kill and must unwind the Rust stack.
pub(crate) fn take_kill_exit() -> bool {
    current().is_some_and(|cx| cx.g().sched.kill_exit.replace(false))
}

/// Mark the running thread as unwinding through a Scheme exit. Used when a
/// [`Killed`] panic reaches a native call boundary and has to become a Scheme
/// exit again. The caller then longjmps to the enclosing activation.
pub(crate) fn begin_kill_exit() {
    if let Some(cx) = current() {
        cx.g().sched.kill.store(KILL_DYING, Ordering::SeqCst);
        cx.g().sched.kill_exit.set(true);
    }
}

/// Make a parked task runnable.
pub(crate) fn unpark(task: &Arc<Task>) {
    debug_assert!(
        !(SCANNING.load(Ordering::SeqCst) && current().is_some()),
        "a thread was woken while the GC traces threads"
    );
    task.notified.store(true, Ordering::SeqCst);
    if task
        .phase
        .compare_exchange(
            PHASE_PARKED,
            PHASE_RUNNABLE,
            Ordering::SeqCst,
            Ordering::SeqCst,
        )
        .is_ok()
    {
        let origin = current_worker_of(&task.shared);
        task.shared.enqueue_woken(task.clone(), origin);
    }
}

/// How a reactor wait ended. `Ready` means the caller should retry its
/// operation (or re-check its deadline)
pub(crate) enum Woken<R> {
    Done(R),
    Ready,
    Closed,
    Interrupted,
}

/// Scheduler fields of a thread, the tail of its [`VirtualThread`] header.
pub(crate) struct VirtualThreadSched {
    /// The thread's own saved stack pointer while it is switched out.
    saved: UnsafeCell<SavedContext>,
    /// Where to switch when the thread parks or finishes: the worker's
    /// saved context.
    home: Cell<*mut SavedContext>,
    task: *const Task,
    entry: Cell<Option<Entry>>,
    stack: Cell<Option<Stack>>,
    /// Guard page of `stack`, read by the fault handler.
    guard: Cell<(Address, Address)>,
    /// The worker's mutator, set at each switch-in.
    mutator: Cell<*const Mutator<crate::Rootable!(())>>,
    /// Index of the worker that last ran this thread.
    worker: Cell<usize>,
    /// The runtime that owns this thread's worker.
    shared: Cell<*const Shared>,
    /// The worker's switch-in counter when this thread was installed. Sysmon
    /// compares it to identify one slice.
    pub(crate) epoch: Cell<u64>,
    /// Set by sysmon: yield at the next yieldpoint.
    pub(crate) preempt: AtomicBool,
    /// Whether `State` has been built for this thread.
    pub(crate) state_ready: Cell<bool>,
    /// `with_mutation` scopes currently open on this thread.
    mutating: Cell<u32>,
    start: Cell<Option<SchemeStart>>,
    locks_held: Cell<u32>,
    pub(crate) kill: AtomicU8,
    /// Set by the yieldpoint that starts a kill unwind through Scheme. The
    /// nearest `call_scheme` return turns the resulting exit into a [`Killed`]
    /// panic, see `take_kill_exit`.
    pub(crate) kill_exit: Cell<bool>,
}

impl VirtualThread {
    /// Make a header from the free list ready for a new task
    fn reinit(&mut self, task: *const Task, entry: Entry, start: Option<SchemeStart>) {
        self.ap = Address::ZERO;
        self.eap = Address::ZERO;
        self.mutation_ptr = std::ptr::null();
        self.take_yieldpoint.store(0, Ordering::Relaxed);
        *self.tc.get_mut() = Tc::new();
        self.sched = VirtualThreadSched::new(task, Some(entry), start);
    }
}

impl VirtualThreadSched {
    fn new(task: *const Task, entry: Option<Entry>, start: Option<SchemeStart>) -> Self {
        Self {
            saved: UnsafeCell::new(SavedContext::default()),
            home: Cell::new(std::ptr::null_mut()),
            task,
            entry: Cell::new(entry),
            stack: Cell::new(None),
            guard: Cell::new((Address::ZERO, Address::ZERO)),
            mutator: Cell::new(std::ptr::null()),
            worker: Cell::new(0),
            shared: Cell::new(std::ptr::null()),
            epoch: Cell::new(0),
            preempt: AtomicBool::new(false),
            state_ready: Cell::new(false),
            mutating: Cell::new(0),
            locks_held: Cell::new(0),
            start: Cell::new(start),
            kill: AtomicU8::new(KILL_LIVE),
            kill_exit: Cell::new(false),
        }
    }

    /// The header of a worker's own thread thread, which runs no task.
    pub(crate) fn home() -> Self {
        Self::new(std::ptr::null(), None, None)
    }

    /// True for a worker's home context, which has no task.
    pub(crate) fn task_is_null(&self) -> bool {
        self.task.is_null()
    }

    /// Visit the Scheme values a thread holds before its first run. The
    /// world must be stopped, or the thread must be the caller's own.
    ///
    /// # Safety
    ///
    /// No other code may touch `start` during the call.
    pub(crate) unsafe fn trace_start(&self, visitor: &mut crate::heap::Visitor) {
        // SAFETY: forwarded from the caller's contract.
        if let Some(start) = unsafe { &mut *self.start.as_ptr() } {
            visitor.trace(&mut start.thread);
            visitor.trace(&mut start.dynamic);
        }
    }

    pub(crate) fn has_start(&self) -> bool {
        unsafe { &*self.start.as_ptr() }.is_some()
    }

    /// Whether the thread has a stack, which is from its first switch-in.
    pub(crate) fn has_stack(&self) -> bool {
        unsafe { &*self.stack.as_ptr() }.is_some()
    }
}

/// The guard page of the thread running on this thread, if any.
pub(crate) fn current_guard_range() -> Option<(Address, Address)> {
    try_current_native_data().map(|nd| nd.virtual_thread().sched.guard.get())
}

/// Direction of readiness that [`SchedCtx::await_fd`] waits for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Interest {
    /// Readable.
    Read,
    /// Writable.
    Write,
}

/// How a [`SchedCtx::await_fd`] wait ended.
#[derive(Debug)]
pub enum FdWait {
    /// The fd is ready in the requested direction.
    Ready,
    /// The timeout passed before the fd was ready.
    TimedOut,
    /// The fd was closed while the thread was waiting.
    Closed,
    /// The fd cannot be polled, for example a regular file. Such fds are
    /// always ready, so the caller may retry the operation directly.
    Failed(std::io::Error),
}

/// A thread's handle to the scheduler.
#[derive(Clone, Copy)]
pub struct SchedCtx {
    g: NonNull<VirtualThread>,
    _not_send: PhantomData<*const ()>,
}

impl SchedCtx {
    fn g(&self) -> &VirtualThread {
        // SAFETY: the thread header lives until its task is dropped, which
        // cannot happen while the thread is running or parked with a waker.
        unsafe { self.g.as_ref() }
    }

    fn task(&self) -> &Task {
        // SAFETY: the task owns the header
        unsafe { &*self.g().sched.task }
    }

    fn task_arc(&self) -> Arc<Task> {
        self.task()
            .self_ref
            .upgrade()
            .expect("running task is alive")
    }

    /// This thread's id.
    pub fn id(&self) -> VirtualThreadId {
        self.task().id
    }

    /// The name set with [`Builder::name`], if any.
    pub fn name(&self) -> Option<&str> {
        self.task().name.as_deref()
    }

    /// The worker this thread last ran on.
    pub(crate) fn worker_index(&self) -> usize {
        self.g().sched.worker.get()
    }

    /// Number of workers in this thread's runtime. Worker 0 is the main thread.
    pub(crate) fn worker_count(&self) -> usize {
        self.task().shared.slots.len()
    }

    /// The worker this thread is restricted to, if any.
    pub(crate) fn affinity(&self) -> Option<usize> {
        match self.task().affinity.load(Ordering::SeqCst) {
            NO_AFFINITY => None,
            w => Some(w),
        }
    }

    /// Restrict this thread to `worker` and move there: if it is running on
    /// another worker it yields, and the scheduler resumes it only on `worker`.
    /// `None` lifts the restriction. Returns once the thread is on `worker`.
    ///
    /// Fails when `worker` does not exist, or when the thread is inside a
    /// native frame (`pins > 0`), which cannot move between threads.
    pub(crate) fn pin_to_worker(&self, worker: Option<usize>) -> Result<(), PinError> {
        let task = self.task();
        let Some(target) = worker else {
            task.affinity.store(NO_AFFINITY, Ordering::SeqCst);
            return Ok(());
        };
        if target >= task.shared.slots.len()
            || !task.shared.slots[target]
                .running_loop
                .load(Ordering::SeqCst)
        {
            return Err(PinError::NoSuchWorker);
        }
        if self.worker_index() != target && task.pins.load(Ordering::SeqCst) > 0 {
            return Err(PinError::InNativeFrame);
        }
        task.affinity.store(target, Ordering::SeqCst);
        while self.worker_index() != target {
            // Requeued onto `target` by `requeue_yielded` so no other worker may take it.
            self.yield_switch();
        }
        Ok(())
    }

    /// A handle that can wake this thread from any thread. See [`Waker`].
    pub fn waker(&self) -> Waker {
        Waker {
            task: self.task_arc(),
        }
    }

    /// Block until a [`Waker`] fires or another wake arrives. The call may
    /// return early, so re-check the condition after it returns.
    ///
    /// Must not be called inside [`with_mutation`](Self::with_mutation).
    pub fn park(&self) -> Result<(), Killed> {
        assert_eq!(
            self.g().sched.mutating.get(),
            0,
            "park inside with_mutation, use park_with instead"
        );
        debug_assert_eq!(
            self.g().sched.locks_held.get(),
            0,
            "park while holding a lock"
        );
        self.check_kill()?;
        if self.task().notified.swap(false, Ordering::SeqCst) {
            return self.check_kill();
        }
        self.flush_own_lab_from_native();
        self.switch_out(PHASE_PARKING);
        self.check_kill()
    }

    /// Park from inside a mutation scope. The context is consumed, and the
    /// one returned is built again after the thread resumes. Anything
    /// rooted only on the Rust stack is stale after this call. The context is
    /// returned even when the park ends with `Err(Killed)`, so the caller can
    /// still run its cleanup.
    pub(crate) fn park_with<'gc>(
        &self,
        mc: MutationContext<'gc>,
    ) -> (MutationContext<'gc>, Result<(), Killed>) {
        self.park_with_mode(mc, true)
    }

    /// [`park_with`](Self::park_with), where `cancellable == false` makes the
    /// park ignore kill requests.
    pub(crate) fn park_with_mode<'gc>(
        &self,
        mc: MutationContext<'gc>,
        cancellable: bool,
    ) -> (MutationContext<'gc>, Result<(), Killed>) {
        debug_assert_eq!(
            self.g().sched.locks_held.get(),
            0,
            "park while holding a lock"
        );
        if cancellable {
            if let Err(killed) = self.check_kill() {
                return (mc, Err(killed));
            }
        }
        if self.task().notified.swap(false, Ordering::SeqCst) {
            return (
                mc,
                if cancellable {
                    self.check_kill()
                } else {
                    Ok(())
                },
            );
        }
        // The window must be back with the worker before the collector can
        // run while we are off-CPU, and only a mutating thread may touch the
        // allocator. So flush here, then leave the mutating state.
        self.flush_own_lab();
        Thread::enter_native();
        self.switch_out(PHASE_PARKING);
        self.assert_on_running_worker();
        // SAFETY: the thread is running again on its worker, which is
        // bound to this thread, and is in native state (see `park`).
        let mc = unsafe { std::mem::transmute(crate::heap::Mutation::new()) };
        (
            mc,
            if cancellable {
                self.check_kill()
            } else {
                Ok(())
            },
        )
    }

    /// Deliver a pending kill at a switch point. A kill that is requested and
    /// not held back by a pin moves the thread to `Dying` and returns
    /// `Err(Killed)`. Any other state is a no-op.
    pub(crate) fn check_kill(&self) -> Result<(), Killed> {
        let sched = &self.g().sched;
        if sched.kill.load(Ordering::SeqCst) != KILL_REQUESTED
            || self.task().pins.load(Ordering::SeqCst) > 0
        {
            return Ok(());
        }
        sched.kill.store(KILL_DYING, Ordering::SeqCst);
        Err(Killed)
    }

    fn flush_own_lab_from_native(&self) {
        Thread::leave_native();
        self.flush_own_lab();
        Thread::enter_native();
    }

    /// Hand the LAB window back to the worker before a switch-out. Called
    /// while the thread is still mutating.
    fn flush_own_lab(&self) {
        let g = self.g.as_ptr();
        // SAFETY: the thread is running on this thread, so its worker
        // context is ours to use and the window is not shared.
        unsafe {
            let native: *const NativeCtx = (*g).mutation_ptr;
            (*native).flush_lab(&mut *g);
        }
    }

    /// Whether a kill of this thread is requested and not yet delivered.
    pub(crate) fn kill_pending(&self) -> bool {
        self.g().sched.kill.load(Ordering::SeqCst) == KILL_REQUESTED
    }

    /// Whether a yieldpoint request is pending on this thread.
    pub(crate) fn yieldpoint_pending(&self) -> bool {
        self.g().take_yieldpoint.load(Ordering::SeqCst) != 0
    }

    /// Park until the reactor reports `fd` ready in direction `dir`, or the
    /// thread is interrupted or the fd closes.
    ///
    /// Returns the refreshed context, which must replace the one passed in.
    /// Registration failure (an fd that cannot be polled) is an `Err` with
    /// the errno.
    pub(crate) fn await_fd_in<'gc, R>(
        &self,
        ctx: MutationContext<'gc>,
        fd: i32,
        dir: Dir,
        mut recheck: impl FnMut() -> Option<R>,
    ) -> (MutationContext<'gc>, Result<Result<Woken<R>, i32>, Killed>) {
        let task = self.task_arc();
        let generation = match reactor::register_fd(&task, fd, dir) {
            Ok(generation) => generation,
            Err(errno) => return (ctx, Ok(Err(errno))),
        };
        if let Some(value) = recheck() {
            reactor::cancel_fd(fd, generation);
            return (ctx, Ok(Ok(Woken::Done(value))));
        }
        let (ctx, parked) = self.park_with(ctx);
        // Withdraw the registration on every exit
        reactor::cancel_fd(fd, generation);
        (ctx, parked.map(|()| Ok(self.woken())))
    }

    /// Park until `deadline`. Returns `Woken::Ready` when the deadline has
    /// passed or the wake was spurious, so the caller re-checks the time.
    pub(crate) fn sleep_until<'gc>(
        &self,
        ctx: MutationContext<'gc>,
        deadline: std::time::Instant,
    ) -> (MutationContext<'gc>, Result<Woken<()>, Killed>) {
        let task = self.task_arc();
        let seq = reactor::register_timer(&task, deadline);
        let (ctx, parked) = self.park_with(ctx);
        reactor::cancel_timer(seq);
        (ctx, parked.map(|()| self.woken()))
    }

    /// Classify why the current reactor wait ended.
    fn woken<R>(&self) -> Woken<R> {
        match self.task().wake_reason.load(Ordering::SeqCst) {
            WAKE_READY | WAKE_TIMER => Woken::Ready,
            WAKE_CLOSED => Woken::Closed,
            _ if self.task().interrupted.swap(false, Ordering::SeqCst) => Woken::Interrupted,
            // A notify or unpark that was not a reactor event: retry the wait.
            _ => Woken::Ready,
        }
    }

    /// Park until `fd` is ready for `interest` or `timeout` passes.
    ///
    /// The fd should be nonblocking. Readiness is only a hint, so retry the
    /// operation after [`FdWait::Ready`]. A `timeout` of `None` has no deadline.
    pub fn await_fd(
        &self,
        fd: std::os::fd::RawFd,
        interest: Interest,
        timeout: Option<Duration>,
    ) -> Result<FdWait, Killed> {
        let (read, write) = match interest {
            Interest::Read => (true, false),
            Interest::Write => (false, true),
        };
        self.await_fd_dirs(fd, read, write, timeout)
    }

    /// Park until `fd` is ready in any of the directions set in `read` and
    /// `write`, or `timeout` passes. Both directions share one wait, so a
    /// readable event does not leave the writable waiter behind. With neither
    /// direction set this only waits out the timeout.
    pub fn await_fd_dirs(
        &self,
        fd: std::os::fd::RawFd,
        read: bool,
        write: bool,
        timeout: Option<Duration>,
    ) -> Result<FdWait, Killed> {
        let task = self.task_arc();
        let deadline = timeout.map(|d| std::time::Instant::now() + d);
        let generation = match reactor::register_fd_dirs(&task, fd, read, write) {
            Ok(generation) => generation,
            Err(errno) => {
                return Ok(FdWait::Failed(std::io::Error::from_raw_os_error(errno)));
            }
        };
        let timer = deadline.map(|d| reactor::register_timer_in_wait(&task, d, generation));
        let outcome = loop {
            if let Err(killed) = self.park() {
                break Err(killed);
            }
            match task.wake_reason.load(Ordering::SeqCst) {
                WAKE_READY => break Ok(FdWait::Ready),
                WAKE_CLOSED => break Ok(FdWait::Closed),
                WAKE_TIMER => break Ok(FdWait::TimedOut),
                // A wake that was not a reactor event. Wait again unless the
                // deadline has passed.
                _ if deadline.is_some_and(|d| std::time::Instant::now() >= d) => {
                    break Ok(FdWait::TimedOut);
                }
                _ => {}
            }
        };
        reactor::cancel_fd(fd, generation);
        if let Some(seq) = timer {
            reactor::cancel_timer(seq);
        }
        outcome
    }

    /// One wait on `fd` in the directions set in `read` and `write`.
    pub(crate) fn await_fd_dirs_in<'gc, R>(
        &self,
        ctx: MutationContext<'gc>,
        fd: i32,
        read: bool,
        write: bool,
        deadline: Option<std::time::Instant>,
        mut recheck: impl FnMut() -> Option<R>,
    ) -> (MutationContext<'gc>, Result<Result<Woken<R>, i32>, Killed>) {
        let task = self.task_arc();
        let generation = match reactor::register_fd_dirs(&task, fd, read, write) {
            Ok(generation) => generation,
            Err(errno) => return (ctx, Ok(Err(errno))),
        };
        let timer = deadline.map(|d| reactor::register_timer_in_wait(&task, d, generation));
        let withdraw = || {
            reactor::cancel_fd(fd, generation);
            if let Some(seq) = timer {
                reactor::cancel_timer(seq);
            }
        };
        if let Some(value) = recheck() {
            withdraw();
            return (ctx, Ok(Ok(Woken::Done(value))));
        }
        let (ctx, parked) = self.park_with(ctx);
        // Withdraw on every exit, a killed wait included.
        withdraw();
        (ctx, parked.map(|()| Ok(self.woken())))
    }

    /// Park for at least `duration`.
    pub fn sleep(&self, duration: Duration) -> Result<(), Killed> {
        let task = self.task_arc();
        let deadline = std::time::Instant::now() + duration;
        loop {
            let now = std::time::Instant::now();
            if now >= deadline {
                return Ok(());
            }
            let seq = reactor::register_timer(&task, deadline);
            let parked = self.park();
            reactor::cancel_timer(seq);
            parked?;
        }
    }

    /// Whether a kill has been requested for this thread or it is already
    /// unwinding. Blocking calls return `Err(Killed)` once this is true.
    pub fn is_killed(&self) -> bool {
        matches!(
            self.g().sched.kill.load(Ordering::SeqCst),
            KILL_REQUESTED | KILL_DYING
        )
    }

    /// Give the worker to the next runnable thread. Returns `Err(Killed)`
    /// if the thread was killed while it was switched out.
    pub fn yield_now(&self) -> Result<(), Killed> {
        assert_eq!(
            self.g().sched.mutating.get(),
            0,
            "yield_now inside with_mutation"
        );
        self.flush_own_lab_from_native();
        self.switch_out(PHASE_YIELDING);
        self.check_kill()
    }

    /// Preempt from a yieldpoint, where the thread is `Mutating`. The thread
    /// goes to the back of its worker's queue, one level down.
    pub(crate) fn preempt_switch(&self) {
        self.switch_from_yieldpoint(PHASE_PREEMPTED);
    }

    /// Yield from a yieldpoint, where the thread is `Mutating`. The thread
    /// goes to the back of its worker's queue at the same level.
    pub(crate) fn yield_switch(&self) {
        self.switch_from_yieldpoint(PHASE_YIELDING);
    }

    /// Switch out with `phase` from a yieldpoint.
    fn switch_from_yieldpoint(&self, phase: u8) {
        self.flush_own_lab();
        Thread::enter_native();
        self.switch_out(phase);
        Thread::leave_native();
    }

    /// This thread's base priority, `0` (most urgent) to `NLEVELS - 1`.
    pub fn priority(&self) -> u8 {
        self.task().base.load(Ordering::SeqCst)
    }

    /// Set this thread's base priority. The current level moves to the new
    /// base if the level was more urgent than it. Queued threads keep their
    /// level until they next leave a worker.
    pub fn set_priority(&self, level: u8) {
        let level = level.min(NLEVELS - 1);
        let task = self.task();
        task.base.store(level, Ordering::SeqCst);
        task.level.fetch_max(level, Ordering::SeqCst);
    }

    /// Run `f` with the Scheme heap available. The [`MutationContext`] lives
    /// only for the call. Values rooted on this thread's stack are stale
    /// after any park, so do not hold them across one.
    pub fn with_mutation<T>(&self, f: impl for<'gc> FnOnce(MutationContext<'gc>) -> T) -> T {
        let g = self.g();
        let _scope = Scope::enter(&g.sched.mutating);
        self.assert_on_running_worker();
        // SAFETY: set at switch-in to the mutator of the worker running us,
        // which outlives every thread that worker runs.
        let mutator = unsafe { &*g.sched.mutator.get() };
        mutator.mutate(|mc, _| f(mc))
    }

    fn assert_on_running_worker(&self) {
        if cfg!(debug_assertions) {
            let current = try_current_native_data().map(|nd| nd as *const NativeCtx);
            assert_eq!(
                Some(self.g().mutation_ptr),
                current,
                "thread resumed without refreshing its worker state"
            );
        }
    }

    /// Hand the task to its worker with `phase`, then switch to the worker.
    fn switch_out(&self, phase: u8) {
        let g = self.g();
        self.task().phase.store(phase, Ordering::SeqCst);
        // SAFETY: `home` was set at switch-in and is the worker's live context.
        unsafe { context_switch(g.sched.saved.get(), g.sched.home.get()) }
    }
}

struct Scope<'a>(&'a Cell<u32>);

impl<'a> Scope<'a> {
    fn enter(counter: &'a Cell<u32>) -> Self {
        counter.set(counter.get() + 1);
        Self(counter)
    }
}

impl Drop for Scope<'_> {
    fn drop(&mut self) {
        self.0.set(self.0.get() - 1);
    }
}

/// Native-depth guard: while alive the thread is pinned to its worker.
pub(crate) struct PinGuard(Option<NonNull<Task>>);

/// Pin the running thread, if any. Called by `call_in_native` and the nest
/// API. The count is per task and only the owning worker writes it.
pub(crate) fn pin_current() -> PinGuard {
    match current() {
        Some(cx) => {
            let task = cx.g().sched.task as *mut Task;
            // SAFETY: the running task is alive.
            unsafe { &*task }.pins.fetch_add(1, Ordering::SeqCst);
            PinGuard(NonNull::new(task))
        }
        None => PinGuard(None),
    }
}

impl Drop for PinGuard {
    fn drop(&mut self) {
        if let Some(task) = self.0 {
            // SAFETY: the task outlives the thread running it, which is
            // still running (the guard is dropped on its own stack).
            let task = unsafe { task.as_ref() };
            let left = task.pins.fetch_sub(1, Ordering::SeqCst) - 1;
            // A kill held back by this pin may have left the thread parked.
            // Wake it so the kill is delivered at its next switch point.
            if left == 0 && task.virtual_thread_kill_state() == KILL_REQUESTED {
                if let Some(arc) = task.self_ref.upgrade() {
                    unpark(&arc);
                }
            }
        }
    }
}

pub(crate) fn preempt_at_yieldpoint() {
    let Some(cx) = current() else {
        return;
    };
    if !cx.g().sched.preempt.swap(false, Ordering::SeqCst) {
        return;
    }
    if cx.task().pins.load(Ordering::SeqCst) > 0 {
        cx.g().sched.preempt.store(true, Ordering::SeqCst);
        if let Some(thread) = try_current_native_data().map(|nd| nd.thread()) {
            thread.request_yieldpoint(crate::heap::sync::thread::YieldpointKind::Full);
        }
        return;
    }
    cx.preempt_switch();
}

pub(crate) unsafe fn flush_queued_labs() {
    let runtimes: Vec<Arc<Shared>> = RUNTIMES.lock().iter().filter_map(Weak::upgrade).collect();
    for shared in runtimes {
        shared.for_each_queued(|task| {
            let gp = task.header();
            // SAFETY: a queued task is not running, and the world is stopped.
            unsafe {
                if (*gp).ap.is_zero() {
                    return;
                }
                let native = &*(*gp).mutation_ptr;
                native.flush_lab(&mut *gp);
            }
        });
    }
}

/// Wakes a thread parked in [`SchedCtx::park`].
#[derive(Clone)]
pub struct Waker {
    task: Arc<Task>,
}

impl Waker {
    /// Wake the thread. A wake that arrives while it is running makes its
    /// next [`SchedCtx::park`] return at once.
    pub fn wake(&self) {
        unpark(&self.task);
    }
}

/// Owned handle to a spawned thread. Dropping it does not stop the
/// thread.
pub struct JoinHandle {
    task: Arc<Task>,
}

impl JoinHandle {
    /// The thread's id.
    pub fn id(&self) -> VirtualThreadId {
        self.task.id
    }

    /// Whether the thread has finished.
    pub fn is_finished(&self) -> bool {
        self.task.phase.load(Ordering::SeqCst) == PHASE_DONE
    }

    /// Wait for the thread to finish, parking the calling thread.
    ///
    /// If the caller is killed while waiting, the call unwinds instead of
    /// returning, as a kill does at any park. Use [`JoinHandle::join_blocking`]
    /// from plain threads.
    ///
    /// The result is taken on the first successful join. Later joins of the
    /// same thread panic.
    pub fn join(self, cx: &SchedCtx) -> Result<(), JoinError> {
        match self.wait(cx) {
            Ok(()) => self.take_result(),
            Err(killed) => killed.unwind(),
        }
    }

    /// Wait like [`JoinHandle::join`], but report a kill of the caller as
    /// `Err(Killed)` rather than unwinding. For the C-ABI, which cannot
    /// unwind.
    pub(crate) fn wait(&self, cx: &SchedCtx) -> Result<(), Killed> {
        loop {
            {
                let mut inner = self.task.inner.lock();
                if self.task.phase.load(Ordering::SeqCst) == PHASE_DONE {
                    return Ok(());
                }
                inner.joiners.push(cx.task_arc());
            }
            cx.park()?;
        }
    }

    /// Take the result of a finished thread. Panics if it was already taken.
    pub(crate) fn take_result(&self) -> Result<(), JoinError> {
        self.task
            .inner
            .lock()
            .result
            .take()
            .expect("join result taken once")
    }

    /// Ask the thread to die. The request is delivered at its next switch
    /// point and unwinds it. Returns `false` when a kill was already requested
    /// or the thread has finished, so repeated calls are no-ops.
    pub fn kill(&self) -> bool {
        kill_virtual_thread(self.task.id)
    }

    /// Wait for the thread from a thread that is not a thread. This
    /// blocks the OS thread. Do not call it from a thread.
    pub fn join_blocking(self) -> Result<(), JoinError> {
        self.wait_blocking();
        self.take_result()
    }

    /// The blocking wait behind [`JoinHandle::join_blocking`], by reference.
    pub(crate) fn wait_blocking(&self) {
        let mut inner = self.task.inner.lock();
        while self.task.phase.load(Ordering::SeqCst) != PHASE_DONE {
            self.task.finished.wait(&mut inner);
        }
    }
}

fn worker_main(shared: Arc<Shared>, index: usize) {
    // Binds this OS thread to the GC. VirtualThreads build their own state.
    let mutator = Mutator::new(|_mc| ());
    worker_loop(&shared, index, &mutator);
}

/// Run threads on the calling thread, which must already be bound to
/// `mutator`, until the runtime stops.
fn worker_loop(shared: &Arc<Shared>, index: usize, mutator: &Mutator<crate::Rootable!(())>) {
    let thread = current_thread();
    let _ = shared.slots[index].thread.set(thread);
    shared.slots[index]
        .running_loop
        .store(true, Ordering::SeqCst);
    let native: *mut NativeCtx = thread.native_data_mut_ptr();
    // SAFETY: the thread is bound, so its slot is initialised.
    let home_virtual_thread = unsafe { (*native).current_virtual_thread };
    let mut home = SavedContext::default();
    let mut stacks = StackCache::default();
    let mut rng = WorkerRng::new(index);
    // Alternate signal stack for SIGURG, restored when the loop ends. Std's
    // stack for this thread is put back by the drop, see `preempt`.
    let _altstack = preempt::AltStack::install().expect("failed to install worker signal stack");
    // SAFETY: `pthread_self` has no preconditions.
    unsafe {
        (*native)
            .pthread
            .store(libc::pthread_self() as usize, Ordering::Relaxed)
    };
    shared.worker_started();
    loop {
        let task = {
            let _no_preempt = preempt::NoPreempt::enter();
            shared.acquire(index, &mut rng)
        };
        let Some(task) = task else { break };
        run(
            shared,
            index,
            mutator,
            native,
            home_virtual_thread,
            &mut home,
            &mut stacks,
            task,
        );
    }
    shared.slots[index]
        .running_loop
        .store(false, Ordering::SeqCst);
    shared.worker_stopped();
}

/// Switch into `task`'s thread and handle how it came back.
#[allow(clippy::too_many_arguments)]
fn run(
    shared: &Arc<Shared>,
    w: usize,
    mutator: &Mutator<crate::Rootable!(())>,
    native: *mut NativeCtx,
    home_virtual_thread: *mut VirtualThread,
    home: &mut SavedContext,
    stacks: &mut StackCache,
    task: Arc<Task>,
) {
    let gp = task.header();
    let slot = &shared.slots[w];
    let no_preempt = preempt::NoPreempt::enter();
    // SAFETY: the task is alive for the whole run (we hold an `Arc`), and the
    // thread is not running anywhere else. The header is accessed through
    // the raw pointer because the thread itself runs on the other side of
    // the switch.
    unsafe {
        let stack = match (*gp).sched.stack.take() {
            Some(stack) => stack,
            None => {
                let stack = stacks
                    .take(task.stack_bytes)
                    .expect("failed to allocate thread stack");
                context_init((*gp).sched.saved.get(), stack.top(), gp);
                (*gp).sched.guard.set(stack.guard_range());
                stack
            }
        };
        (*gp).sched.stack.set(Some(stack));
        (*gp).sched.home.set(home);
        (*gp).sched.mutator.set(mutator);
        (*gp).sched.worker.set(w);
        (*gp).sched.shared.set(Arc::as_ptr(shared));
        (*gp).sched.preempt.store(false, Ordering::SeqCst);

        // Mutation epoch: every switch-in starts a new slice on this worker.
        (*native).epoch += 1;
        let epoch = (*native).epoch;
        (*gp).sched.epoch.set(epoch);
        (*gp).mutation_ptr = native;
        task.home.store(w, Ordering::SeqCst);

        debug_assert!((*gp).ap.is_zero(), "switched-in thread holds a LAB window");

        task.phase.store(PHASE_RUNNING, Ordering::SeqCst);
        slot.slice_id.store(epoch, Ordering::Release);
        (*native).thread().publish_current_virtual_thread(gp);
        slot.running_level
            .store(task.level.load(Ordering::Relaxed), Ordering::Relaxed);

        drop(no_preempt);
        context_switch(home, (*gp).sched.saved.get());
        let _no_preempt = preempt::NoPreempt::enter();

        slot.slice_id.store(0, Ordering::Release);
        slot.running_level.store(NLEVELS, Ordering::Relaxed);
        (*native)
            .thread()
            .publish_current_virtual_thread(home_virtual_thread);

        match task.phase.load(Ordering::SeqCst) {
            PHASE_DONE => {
                debug_assert!(
                    (*gp).ap.is_zero(),
                    "finished thread still holds a LAB window"
                );
                unregister_live(task.id);
                if let Some(stack) = (*gp).sched.stack.take() {
                    stacks.put(stack);
                }
                if (*gp).sched.state_ready.replace(false) {
                    std::ptr::drop_in_place((*gp).state_ptr());
                }
                // Last use of the header: after this the task has none, and
                // the next spawn on this worker may take it.
                let header = task.detach_header();
                debug_assert_eq!(header, gp);
                shared.gfree_put(w, Box::from_raw(header));
            }
            PHASE_PARKING => {
                // Invariant: the thread flushed its window before switching
                // out, so a parked thread holds nothing.
                debug_assert!((*gp).ap.is_zero(), "parked thread still holds a LAB window");
                task.adapt_level(SwitchOut::Blocked);
                task.phase.store(PHASE_PARKED, Ordering::SeqCst);
                if task.notified.swap(false, Ordering::SeqCst)
                    && task
                        .phase
                        .compare_exchange(
                            PHASE_PARKED,
                            PHASE_RUNNABLE,
                            Ordering::SeqCst,
                            Ordering::SeqCst,
                        )
                        .is_ok()
                {
                    shared.enqueue_woken(task.clone(), Some(w));
                }
            }
            phase @ (PHASE_YIELDING | PHASE_PREEMPTED) => {
                debug_assert!(
                    (*gp).ap.is_zero(),
                    "yielded thread still holds a LAB window"
                );
                let leave = if phase == PHASE_PREEMPTED {
                    SwitchOut::Quantum
                } else {
                    SwitchOut::Yield
                };
                task.adapt_level(leave);
                task.phase.store(PHASE_RUNNABLE, Ordering::SeqCst);
                shared.requeue_yielded(w, task);
            }
            other => unreachable!("thread switched out in phase {other}"),
        }
    }
}

/// Drop a finished thread from [`LIVE`]. The caller's own `Arc` keeps the
/// header alive until the worker is done with it.
fn unregister_live(id: VirtualThreadId) {
    live_shard(id.0).lock().remove(&id.0);
}

/// Call `f` for every thread whose Scheme state is a root
///
/// # Safety
///
/// Only the collector may call this, with every worker blocked. The callback
/// must not keep the reference past the call.
pub(crate) unsafe fn trace_registered_virtual_threads(mut f: impl FnMut(&VirtualThread)) {
    check_worker_registry();
    clear_stack_pools();
    SCANNING.store(true, Ordering::SeqCst);
    for shard in LIVE.iter() {
        let live = shard.0.lock();
        for task in live.values() {
            let phase = task.phase.load(Ordering::SeqCst);
            if phase == PHASE_DONE {
                continue;
            }

            let g = unsafe { &*task.header() };
            if phase == PHASE_NEW {
                if g.sched.has_start() {
                    f(g);
                }
                continue;
            }
            if phase == PHASE_RUNNING {
                let worker = task.shared.slots[g.sched.worker.get()].thread.get();
                debug_assert!(
                    worker.is_some_and(|t| t.is_blocked()),
                    "running thread scanned before its worker blocked"
                );
            } else {
                debug_assert!(
                    unsafe { &*g.sched.stack.as_ptr() }.is_some(),
                    "switched-out thread has no stack"
                );
            }
            f(g);
        }
    }
    SCANNING.store(false, Ordering::SeqCst);
}

fn check_worker_registry() {
    if !cfg!(debug_assertions) {
        return;
    }
    for shared in RUNTIMES.lock().iter().filter_map(Weak::upgrade) {
        for slot in &shared.slots {
            if slot.running_loop.load(Ordering::SeqCst) {
                debug_assert!(
                    slot.thread.get().is_some(),
                    "running worker has no bound thread"
                );
            }
        }
    }
}

/// Request a yieldpoint on thread `id` and wake it if it is parked.
pub(crate) fn interrupt_virtual_thread(id: VirtualThreadId) -> bool {
    debug_assert!(
        !(SCANNING.load(Ordering::SeqCst) && current().is_some()),
        "a thread was interrupted while the collector traces threads"
    );
    let Some(task) = live_shard(id.0).lock().get(&id.0).cloned() else {
        return false;
    };
    {
        let _inner = task.inner.lock();
        let gp = task.thread.load(Ordering::Acquire);
        if gp.is_null() {
            return false;
        }
        // SAFETY: `inner` is held, so the header is not recycled. The flag and
        // `esp` are atomic and are the only fields written here.
        unsafe {
            (*gp).take_yieldpoint.store(1, Ordering::SeqCst);
            (*(*gp).tc.get())
                .esp
                .store(std::ptr::null_mut(), Ordering::SeqCst);
        }
    }
    task.interrupted.store(true, Ordering::SeqCst);
    unpark(&task);
    true
}

fn pool_dynamic_state(state: &State<'static>) {
    let Some(pool) = dynamic_pool() else { return };
    if !state.dynamic_state.is_reusable() {
        return;
    }
    let mut pool = pool.lock();
    if pool.len() < DYNAMIC_POOL_MAX {
        // SAFETY: the state is dropped right after, and the pool is emptied
        // before the next collection.
        pool.push(PooledDynamic(unsafe { state.dynamic_state.duplicate() }));
    }
}

pub(crate) extern "C" fn virtual_thread_entry(gp: *mut VirtualThread) -> ! {
    let result = run_entry(gp);

    // SAFETY: the thread runs on this worker, whose context it points at.
    unsafe {
        Thread::leave_native();
        (*(*gp).mutation_ptr).flush_lab(&mut *gp);
        if (*gp).sched.state_ready.get() {
            if let Some(pool) = stack_pool() {
                retire_stacks(&*(*gp).tc_ptr(), pool);
            }
            pool_dynamic_state(&*(*gp).state_ptr());
        }
        Thread::enter_native();
    }
    finish(gp, result);
    // SAFETY: the thread is done and is never resumed. Its frames hold no
    // owned values at this point, so nothing leaks by leaving them.
    unsafe {
        let g = &*gp;
        context_switch(g.sched.saved.get(), g.sched.home.get());
    }
    std::process::abort()
}

fn run_entry(gp: *mut VirtualThread) -> Result<(), JoinError> {
    // SAFETY: called from `virtual_thread_entry` with the live header.
    let g = unsafe { &*gp };
    let entry = g.sched.entry.take().expect("thread entry is set once");
    // SAFETY: `gp` is non-null.
    let cx = SchedCtx {
        g: unsafe { NonNull::new_unchecked(gp) },
        _not_send: PhantomData,
    };
    let id = cx.id();
    // A Rust body killed before its first run never starts. A Scheme thread
    // still runs its trampoline, which marks the thread finished for joiners.
    if !g.sched.has_start() && g.sched.kill.load(Ordering::SeqCst) == KILL_REQUESTED {
        g.sched.kill.store(KILL_DYING, Ordering::SeqCst);
        return Err(JoinError::Killed);
    }
    match catch_unwind(AssertUnwindSafe(|| {
        ensure_state(cx);
        entry(cx);
    })) {
        // A body that saw its kill (`Err(Killed)` from a blocking call) and
        // returned normally still reports the kill.
        Ok(()) if g.sched.kill.load(Ordering::SeqCst) == KILL_DYING => Err(JoinError::Killed),
        Ok(()) => Ok(()),
        Err(payload) if payload.is::<Killed>() => Err(JoinError::Killed),
        Err(payload) => {
            report_panic(id, payload.as_ref());
            Err(JoinError::Panicked(payload))
        }
    }
}

/// Build the thread's `State` the first time it runs. A thread started
/// from a Scheme spawn runs as its `ThreadObject` with its spawner's dynamic
/// state, and both are read back from the header after each allocation, since
/// a collection may have moved the dynamic state.
fn ensure_state(cx: SchedCtx) {
    if cx.g().sched.state_ready.get() {
        return;
    }
    cx.with_mutation(|mc| {
        let Some(start) = cx.g().sched.start.get() else {
            let thread_object = ThreadObject::new(mc, None);
            thread_object.bind_current_runtime_thread();
            mc.init_state(State::new(mc, thread_object));
            return;
        };
        // SAFETY: the start values are Scheme values of this runtime, live
        // until consumed below and only used on this thread.
        let thread_object =
            unsafe { std::mem::transmute::<Value<'static>, Value<'_>>(start.thread) }
                .downcast::<ThreadObject>();
        thread_object.bind_current_runtime_thread();

        let pooled = dynamic_pool().and_then(|pool| pool.lock().pop());
        let state = match pooled {
            Some(pooled) => State::with_dynamic_state(thread_object, unsafe {
                std::mem::transmute::<DynamicState<'static>, DynamicState<'_>>(pooled.0)
            }),
            None => State::new(mc, thread_object),
        };
        mc.init_state(state);
        let start = cx
            .g()
            .sched
            .start
            .get()
            .expect("start values are kept until first run");

        let dynamic = unsafe { std::mem::transmute::<Value<'static>, Value<'_>>(start.dynamic) };
        mc.state().dynamic_state.restore(mc, dynamic);
        cx.g().sched.start.set(None);
    });
}

fn report_panic(id: VirtualThreadId, payload: &(dyn Any + Send)) {
    let msg = payload
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
        .unwrap_or("non-string panic payload");
    eprintln!("capy: thread {} panicked: {msg}", id.as_u64());
}

fn finish(gp: *mut VirtualThread, result: Result<(), JoinError>) {
    // SAFETY: the task is alive while the worker runs the thread.
    let task = unsafe { &*(*gp).sched.task };
    // SAFETY: as above.
    unsafe { (*gp).sched.kill.store(KILL_DONE, Ordering::SeqCst) };
    let joiners = {
        let mut inner = task.inner.lock();
        inner.result = Some(result);
        task.phase.store(PHASE_DONE, Ordering::SeqCst);
        task.finished.notify_all();
        std::mem::take(&mut inner.joiners)
    };
    for joiner in joiners {
        unpark(&joiner);
    }
}

/// The thread running on this thread. `None` on a worker's own context or
/// an unbound thread.
///
/// Never inlined: a thread can resume on a different worker after a park or
/// yield, and an inlined read of the `#[thread_local]` slot lets the compiler
/// reuse the previous worker's slot address in the caller.
#[inline(never)]
pub(crate) fn current() -> Option<SchedCtx> {
    let nd = try_current_native_data()?;
    let g = NonNull::new(nd.current_virtual_thread)?;
    // SAFETY: the bound thread's thread header is always initialised.
    if unsafe { g.as_ref() }.sched.task_is_null() {
        return None;
    }
    Some(SchedCtx {
        g,
        _not_send: PhantomData,
    })
}

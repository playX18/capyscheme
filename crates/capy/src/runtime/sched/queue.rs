//! Run queues: one local queue per worker, one global
//! injector, stealing between workers.
//!
//! Each queue is split into [`NLEVELS`] levels, and the most urgent non-empty
//! level is served first. A thread's level is kept on its task and moves
//! as it runs (see `next_level`), and sysmon resets every queued thread to
//! its base level on a fixed period so none can starve.
//!
//! Within a level, a worker pops the front of its own queue (LIFO for hot
//! wakeups and spawns, which are pushed to the front). Yields and
//! preemptions go to the back. Thieves take from the back, the coldest end.
//! With one worker every push goes to the back, which is plain FIFO.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use parking_lot::{Condvar, Mutex};

use super::scheduler::{NLEVELS, NO_AFFINITY, Task};
use crate::heap::sync::thread::{Thread, VirtualThread};
use crate::runtime::fluids::DynamicState;
use crate::runtime::interp::stack::StackPool;

/// How long an idle worker sleeps before rechecking, so a lost notify costs
/// at most this much latency.
const IDLE_RECHECK: Duration = Duration::from_millis(10);

/// Dispatch decisions between two global-queue checks (taken from Go).
const GLOBAL_CHECK_INTERVAL: u64 = 61;

/// Dispatch decisions between two picks of the least urgent local level. Keeps
/// a demoted thread from waiting for every more urgent one to finish.
const STARVATION_INTERVAL: u64 = 16;

/// Idle spins before a worker sleeps on the condvar
const IDLE_SPINS: usize = 1024;

/// Most dead thread headers a worker keeps before spilling to the global list.
const GFREE_LOCAL_MAX: usize = 64;

/// Headers a worker takes from the global list when its own list is empty.
const GFREE_REFILL: usize = 32;

/// Dynamic states a worker keeps for the next spawns.
pub(crate) const DYNAMIC_POOL_MAX: usize = 64;

/// A pooled [`DynamicState`], valid until the next collection.
pub(crate) struct PooledDynamic(pub(crate) DynamicState<'static>);

unsafe impl Send for PooledDynamic {}

/// A dead thread's header, parked on a free list until the next spawn.
pub(crate) struct FreeG(pub(crate) Box<VirtualThread>);

unsafe impl Send for FreeG {}

/// How many headers to move to the global list when a local list holds `len`.
fn gfree_spill_count(len: usize) -> usize {
    // Taken from Go: above the high watermark, move half to global list
    if len > GFREE_LOCAL_MAX { len / 2 } else { 0 }
}

/// A run queue with one FIFO per level. Level `0` is served first. The item
/// count and the non-empty levels are kept up to date on every change, so the
/// most urgent level is found without scanning.
pub(crate) struct LevelQueue<T> {
    levels: [VecDeque<T>; NLEVELS as usize],
    len: usize,
    /// Bit `i` is set when level `i` holds an item.
    mask: u8,
}

impl<T> LevelQueue<T> {
    pub(crate) fn new() -> Self {
        Self {
            levels: std::array::from_fn(|_| VecDeque::new()),
            len: 0,
            mask: 0,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(crate) fn mask(&self) -> u8 {
        self.mask
    }

    /// Queue `item` at `level`. `front` puts it at the hot end of that level.
    pub(crate) fn push(&mut self, item: T, level: u8, front: bool) {
        let level = usize::from(level).min(NLEVELS as usize - 1);
        let items = &mut self.levels[level];
        if front {
            items.push_front(item);
        } else {
            items.push_back(item);
        }
        self.len += 1;
        self.mask |= 1u8 << level;
    }

    /// Take an item from the front (or back) of `level`, which must be non-empty.
    fn pop_at(&mut self, level: usize, back: bool) -> Option<T> {
        let items = &mut self.levels[level];
        let item = if back {
            items.pop_back()
        } else {
            items.pop_front()
        }?;
        self.len -= 1;
        if items.is_empty() {
            self.mask &= !(1u8 << level);
        }
        Some(item)
    }

    /// Take the head of the most urgent non-empty level.
    pub(crate) fn pop_best(&mut self) -> Option<T> {
        if self.mask == 0 {
            return None;
        }
        self.pop_at(self.mask.trailing_zeros() as usize, false)
    }

    /// Take the head of the least urgent non-empty level.
    pub(crate) fn pop_worst(&mut self) -> Option<T> {
        if self.mask == 0 {
            return None;
        }
        self.pop_at((u8::BITS - 1 - self.mask.leading_zeros()) as usize, false)
    }

    /// Take up to half of the most urgent non-empty level, from its back.
    /// Items that `takeable` refuses stay where they were, in their order.
    pub(crate) fn steal_half(&mut self, takeable: impl Fn(&T) -> bool) -> Vec<T> {
        if self.mask == 0 {
            return Vec::new();
        }
        let level = self.mask.trailing_zeros() as usize;
        let want = self.levels[level].len().div_ceil(2);
        let mut taken = Vec::with_capacity(want);
        let mut kept = Vec::new();
        while taken.len() < want {
            let Some(item) = self.pop_at(level, true) else {
                break;
            };
            if takeable(&item) {
                taken.push(item);
            } else {
                kept.push(item);
            }
        }
        for item in kept.into_iter().rev() {
            self.levels[level].push_back(item);
            self.len += 1;
            self.mask |= 1u8 << level;
        }
        taken
    }

    /// Re-file every item at the level `level_of` gives it. Items keep their
    /// relative order, level by level.
    pub(crate) fn rebucket(&mut self, mut level_of: impl FnMut(&T) -> u8) {
        let mut all = Vec::with_capacity(self.len);
        for items in &mut self.levels {
            all.extend(items.drain(..));
        }
        self.mask = 0;
        for item in all {
            let level = level_of(&item).min(NLEVELS - 1) as usize;
            self.levels[level].push_back(item);
            self.mask |= 1u8 << level;
        }
    }

    /// Visit every item, most urgent level first.
    pub(crate) fn for_each(&self, mut f: impl FnMut(&T)) {
        for items in &self.levels {
            for item in items {
                f(item);
            }
        }
    }
}

/// Lock-free view of a [`LevelQueue`]
pub(crate) struct Summary {
    len: AtomicUsize,
    mask: AtomicU8,
}

impl Summary {
    fn new() -> Self {
        Self {
            len: AtomicUsize::new(0),
            mask: AtomicU8::new(0),
        }
    }

    /// Readers take these values as hints.
    fn publish<T>(&self, queue: &LevelQueue<T>) {
        self.len.store(queue.len(), Ordering::Relaxed);
        self.mask.store(queue.mask(), Ordering::Relaxed);
    }

    fn len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    fn mask(&self) -> u8 {
        self.mask.load(Ordering::Relaxed)
    }
}

/// The level a task is filed under.
fn level_of(task: &Task) -> u8 {
    task.level.load(Ordering::Relaxed).min(NLEVELS - 1)
}

/// Reset a queued task to its base level, and return that level.
fn reset_to_base(task: &Task) -> u8 {
    let base = task.base.load(Ordering::Relaxed);
    task.level.store(base, Ordering::Relaxed);
    base
}

/// One worker's queue
pub(crate) struct WorkerSlot {
    /// Free-list of dead thread headers.
    gfree: Mutex<Vec<FreeG>>,
    local: Mutex<LevelQueue<Arc<Task>>>,
    /// Summary of `local`. Thieves read it to skip an empty queue, and to pick
    /// the most urgent level to steal from.
    local_sum: Summary,
    /// Level of the thread running on this worker, or `NLEVELS` while the
    /// worker runs none.
    pub(crate) running_level: AtomicU8,
    /// The worker's bound thread, set once the worker has bound itself.
    pub(crate) thread: OnceLock<&'static Thread>,
    /// True while the worker runs its loop.
    pub(crate) running_loop: AtomicBool,
    /// Epoch of the thread slice running now, `0` when idle.
    pub(crate) slice_id: AtomicU64,
    /// Scheme stacks of threads that finished on this worker.
    pub(crate) stack_pool: StackPool,
    /// Dynamic states of threads that finished on this worker.
    pub(crate) dynamic_pool: Mutex<Vec<PooledDynamic>>,
}

/// State shared by a runtime's workers, sysmon and handles.
pub(crate) struct Shared {
    /// Overflow of the per-worker free-list of dead headers .
    gfree_global: Mutex<Vec<FreeG>>,
    global: Mutex<LevelQueue<Arc<Task>>>,
    /// Summary of `global`
    global_sum: Summary,
    pub(crate) slots: Vec<WorkerSlot>,
    /// Tasks in any queue. Sysmon preempts only when this is nonzero.
    queued: AtomicUsize,
    idle: AtomicUsize,
    /// Workers that are spinning in `acquire` for work.
    searching: AtomicUsize,
    sleep: Mutex<()>,
    sleep_cv: Condvar,
    pub(crate) stack_bytes: usize,
    pub(crate) quantum: Duration,
    /// One worker: every push goes to the back and nothing is stolen.
    pub(crate) fifo: bool,
    pub(crate) stop: AtomicBool,
    /// Workers currently inside their loop, including worker 0 when it is the
    /// calling thread.
    running: AtomicUsize,
}

/// Per-worker scheduling state: the dispatch counter and the steal RNG.
pub(crate) struct WorkerRng {
    tick: u64,
    state: u64,
}

impl WorkerRng {
    pub(crate) fn new(worker: usize) -> Self {
        Self {
            tick: 0,
            state: 0x9E37_79B9_7F4A_7C15 ^ (worker as u64 + 1),
        }
    }

    fn next(&mut self) -> u64 {
        // xorshift64*
        self.state ^= self.state >> 12;
        self.state ^= self.state << 25;
        self.state ^= self.state >> 27;
        self.state.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }
}

impl Shared {
    pub(crate) fn new(workers: usize, stack_bytes: usize, quantum: Duration) -> Self {
        Self {
            gfree_global: Mutex::new(Vec::new()),
            global: Mutex::new(LevelQueue::new()),
            global_sum: Summary::new(),
            slots: (0..workers)
                .map(|_| WorkerSlot {
                    gfree: Mutex::new(Vec::new()),
                    local: Mutex::new(LevelQueue::new()),
                    local_sum: Summary::new(),
                    running_level: AtomicU8::new(NLEVELS),
                    thread: OnceLock::new(),
                    running_loop: AtomicBool::new(false),
                    slice_id: AtomicU64::new(0),
                    stack_pool: StackPool::new(),
                    dynamic_pool: Mutex::new(Vec::new()),
                })
                .collect(),
            queued: AtomicUsize::new(0),
            idle: AtomicUsize::new(0),
            searching: AtomicUsize::new(0),
            sleep: Mutex::new(()),
            sleep_cv: Condvar::new(),
            stack_bytes,
            quantum,
            fifo: workers == 1,
            stop: AtomicBool::new(false),
            running: AtomicUsize::new(0),
        }
    }

    /// Take a cached header for a spawn on worker `w`
    pub(crate) fn gfree_get(&self, w: Option<usize>) -> Option<Box<VirtualThread>> {
        let Some(w) = w else {
            return self.gfree_global.lock().pop().map(|g| g.0);
        };
        let mut local = self.slots[w].gfree.lock();
        if local.is_empty() {
            let mut global = self.gfree_global.lock();
            let take = global.len().min(GFREE_REFILL);
            let at = global.len() - take;
            local.extend(global.drain(at..));
        }
        local.pop().map(|g| g.0)
    }

    /// Cache the header of a thread that finished on worker `w`.
    pub(crate) fn gfree_put(&self, w: usize, g: Box<VirtualThread>) {
        let mut local = self.slots[w].gfree.lock();
        local.push(FreeG(g));
        let spill = gfree_spill_count(local.len());
        if spill > 0 {
            let at = local.len() - spill;
            self.gfree_global.lock().extend(local.drain(at..));
        }
    }

    pub(crate) fn worker_started(&self) {
        self.running.fetch_add(1, Ordering::SeqCst);
    }

    pub(crate) fn worker_stopped(&self) {
        self.running.fetch_sub(1, Ordering::SeqCst);
    }

    pub(crate) fn running_workers(&self) -> usize {
        self.running.load(Ordering::SeqCst)
    }

    pub(crate) fn queued(&self) -> usize {
        self.queued.load(Ordering::SeqCst)
    }

    /// Whether some worker is asleep or looking for work, so a queued task
    /// will be picked up without preempting anyone.
    pub(crate) fn worker_free(&self) -> bool {
        self.idle.load(Ordering::SeqCst) > 0 || self.searching.load(Ordering::SeqCst) > 0
    }

    /// Whether a task queued on the global queue or on worker `w` is more
    /// urgent than `running`, the level of the thread on worker `w`.
    pub(crate) fn outranked(&self, w: usize, running: u8) -> bool {
        let more_urgent = |mask: u8| mask != 0 && mask.trailing_zeros() < u32::from(running);
        more_urgent(self.global_sum.mask()) || more_urgent(self.slots[w].local_sum.mask())
    }

    /// Reset every queued thread to its base level. Sysmon calls this on a
    /// fixed period, so a thread demoted under load eventually competes
    /// again with the threads around it.
    pub(crate) fn boost(&self) {
        {
            let mut global = self.global.lock();
            global.rebucket(|task| reset_to_base(task));
            self.global_sum.publish(&global);
        }
        for slot in &self.slots {
            let mut local = slot.local.lock();
            local.rebucket(|task| reset_to_base(task));
            slot.local_sum.publish(&local);
        }
    }

    /// Put `task` on worker `w`'s queue at its level. `front` is the LIFO hot end.
    pub(crate) fn push_local(&self, w: usize, task: Arc<Task>, front: bool) {
        self.queued.fetch_add(1, Ordering::SeqCst);
        {
            let slot = &self.slots[w];
            let level = level_of(&task);
            let mut local = slot.local.lock();
            local.push(task, level, front && !self.fifo);
            slot.local_sum.publish(&local);
        }
        self.wake_one();
    }

    pub(crate) fn push_global(&self, task: Arc<Task>) {
        self.queued.fetch_add(1, Ordering::SeqCst);
        {
            let level = level_of(&task);
            let mut global = self.global.lock();
            global.push(task, level, false);
            self.global_sum.publish(&global);
        }
        self.wake_one();
    }

    /// Queue a task that only worker `w` may run, and make sure that worker wakes:
    /// `wake_one` could pick another idle worker, which cannot take it.
    pub(crate) fn push_pinned(&self, w: usize, task: Arc<Task>) {
        self.queued.fetch_add(1, Ordering::SeqCst);
        {
            let slot = &self.slots[w];
            let level = level_of(&task);
            let mut local = slot.local.lock();
            local.push(task, level, false);
            slot.local_sum.publish(&local);
        }
        let _guard = self.sleep.lock();
        self.sleep_cv.notify_all();
    }

    /// Requeue a thread that yielded or was preempted on worker `w`.
    pub(crate) fn requeue_yielded(&self, w: usize, task: Arc<Task>) {
        match task.affinity.load(Ordering::SeqCst) {
            NO_AFFINITY => self.push_local(w, task, false),
            target => self.push_pinned(target, task),
        }
    }

    /// Queue a thread that became runnable. A pinned thread goes back
    /// to the worker it parked on.
    pub(crate) fn enqueue_woken(&self, task: Arc<Task>, origin: Option<usize>) {
        let affinity = task.affinity.load(Ordering::SeqCst);
        if affinity != NO_AFFINITY {
            self.push_pinned(affinity, task);
            return;
        }
        if task.pins.load(Ordering::SeqCst) > 0 {
            let home = task.home.load(Ordering::SeqCst);
            self.push_local(home, task, false);
            return;
        }
        match origin {
            Some(w) => self.push_local(w, task, true),
            None => self.push_global(task),
        }
    }

    /// Wake one idle worker, if any and if no worker is already searching.
    fn wake_one(&self) {
        if self.idle.load(Ordering::SeqCst) > 0 && self.searching.load(Ordering::SeqCst) == 0 {
            let _guard = self.sleep.lock();
            self.sleep_cv.notify_one();
        }
    }

    fn taken(&self, task: Arc<Task>) -> Arc<Task> {
        self.queued.fetch_sub(1, Ordering::SeqCst);
        task
    }

    fn pop_global(&self) -> Option<Arc<Task>> {
        if self.global_sum.len() == 0 {
            return None;
        }
        let mut global = self.global.lock();
        let task = global.pop_best()?;
        self.global_sum.publish(&global);
        drop(global);
        Some(self.taken(task))
    }

    /// Take the most urgent task from worker `w`'s queue, or the least urgent
    /// one when `worst` is set.
    fn pop_local(&self, w: usize, worst: bool) -> Option<Arc<Task>> {
        let slot = &self.slots[w];
        let task = {
            let mut local = slot.local.lock();
            let task = if worst {
                local.pop_worst()?
            } else {
                local.pop_best()?
            };
            slot.local_sum.publish(&local);
            task
        };
        Some(self.taken(task))
    }

    /// One pass of the selection order:
    /// the global queue every 61st decision, then the most urgent local task
    /// (the least urgent every 16th decision), then the global queue, then stealing.
    fn find(&self, w: usize, rng: &mut WorkerRng) -> Option<Arc<Task>> {
        if self.queued.load(Ordering::SeqCst) == 0 {
            return None;
        }
        rng.tick += 1;
        if self.fifo {
            // One worker: the global queue is served first, so an outside
            // wake cannot wait behind a thread that keeps yielding.
            return self.pop_global().or_else(|| self.pop_local(w, false));
        }
        if rng.tick % GLOBAL_CHECK_INTERVAL == 0 {
            if let Some(task) = self.pop_global() {
                return Some(task);
            }
        }
        let worst = rng.tick % STARVATION_INTERVAL == 0;
        if let Some(task) = self.pop_local(w, worst) {
            return Some(task);
        }
        if let Some(task) = self.pop_global() {
            return Some(task);
        }
        self.steal(w, rng)
    }

    /// Up to one attempt per worker against a random victim: take half of the
    /// victim's most urgent level from the back, skipping pinned threads.
    /// The first stolen task runs now, the rest join this worker's queue at
    /// their levels.
    fn steal(&self, w: usize, rng: &mut WorkerRng) -> Option<Arc<Task>> {
        let n = self.slots.len();
        for _ in 0..n {
            let victim = (rng.next() % n as u64) as usize;
            if victim == w || self.slots[victim].local_sum.mask() == 0 {
                continue;
            }
            let stolen = {
                let slot = &self.slots[victim];
                let mut queue = slot.local.lock();
                let stolen = queue.steal_half(|task| {
                    task.pins.load(Ordering::SeqCst) == 0
                        && task.affinity.load(Ordering::SeqCst) == NO_AFFINITY
                });
                slot.local_sum.publish(&queue);
                stolen
            };
            if stolen.is_empty() {
                continue;
            }
            let mut stolen = stolen.into_iter();
            let first = stolen.next().expect("nonempty");
            let slot = &self.slots[w];
            let mut local = slot.local.lock();
            for task in stolen {
                let level = level_of(&task);
                local.push(task, level, false);
            }
            slot.local_sum.publish(&local);
            drop(local);
            return Some(self.taken(first));
        }
        None
    }

    /// The next task for worker `w`.
    pub(crate) fn acquire(&self, w: usize, rng: &mut WorkerRng) -> Option<Arc<Task>> {
        loop {
            if self.stop.load(Ordering::SeqCst) {
                return None;
            }
            if let Some(task) = self.find(w, rng) {
                return Some(task);
            }
            self.searching.fetch_add(1, Ordering::SeqCst);
            let mut found = None;
            for _ in 0..IDLE_SPINS {
                std::hint::spin_loop();
                if let Some(task) = self.find(w, rng) {
                    found = Some(task);
                    break;
                }
            }
            let last = self.searching.fetch_sub(1, Ordering::SeqCst) == 1;
            if let Some(task) = found {
                // The last searcher is about to run a task. Pushes that came
                // while it searched did not wake anyone, so hand the search on.
                if last && self.queued.load(Ordering::SeqCst) > 0 {
                    self.wake_one();
                }
                return Some(task);
            }
            let mut guard = self.sleep.lock();
            self.idle.fetch_add(1, Ordering::SeqCst);
            if self.queued.load(Ordering::SeqCst) == 0 && !self.stop.load(Ordering::SeqCst) {
                self.sleep_cv.wait_for(&mut guard, IDLE_RECHECK);
            }
            self.idle.fetch_sub(1, Ordering::SeqCst);
        }
    }

    /// Visit every queued task
    pub(crate) fn for_each_queued(&self, mut f: impl FnMut(&Arc<Task>)) {
        self.global.lock().for_each(&mut f);
        for slot in &self.slots {
            slot.local.lock().for_each(&mut f);
        }
    }
}

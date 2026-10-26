//! The blocking pool.
//!
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;

use parking_lot::{Condvar, Mutex};

use super::scheduler::{Killed, SchedCtx};
use crate::runtime::MutationContext;

/// Upper bound on pool threads
pub(crate) const MAX_THREADS: usize = 64;

/// How long an idle pool thread waits for work before it exits.
pub(crate) const IDLE_EXIT: Duration = Duration::from_secs(10);

type Job = Box<dyn FnOnce() + Send + 'static>;

struct PoolState {
    queue: VecDeque<Job>,
    threads: usize,
    idle: usize,
}

struct Pool {
    state: Mutex<PoolState>,
    work: Condvar,
}

static POOL: Pool = Pool {
    state: Mutex::new(PoolState {
        queue: VecDeque::new(),
        threads: 0,
        idle: 0,
    }),
    work: Condvar::new(),
};

fn submit(job: Job) {
    let mut st = POOL.state.lock();
    st.queue.push_back(job);
    if st.idle > 0 {
        POOL.work.notify_one();
    } else if st.threads < MAX_THREADS {
        st.threads += 1;
        std::thread::Builder::new()
            .name("capy-blocking".into())
            .spawn(pool_thread)
            .expect("failed to start a blocking pool thread");
    }
}

fn pool_thread() {
    let mut st = POOL.state.lock();
    loop {
        if let Some(job) = st.queue.pop_front() {
            drop(st);
            job();
            st = POOL.state.lock();
            continue;
        }
        st.idle += 1;
        let timed_out = POOL.work.wait_for(&mut st, IDLE_EXIT).timed_out();
        st.idle -= 1;
        if timed_out && st.queue.is_empty() {
            st.threads -= 1;
            return;
        }
    }
}

/// Number of pool threads currently alive (idle or busy).
#[cfg(test)]
pub(crate) fn thread_count() -> usize {
    POOL.state.lock().threads
}

struct Slot<R> {
    value: Option<R>,
    waker: Option<super::scheduler::Waker>,
}

/// Run `f` on the blocking pool and park the calling thread until it
/// returns. Returns the refreshed context from the park and `f`'s result, or
/// `Err(Killed)` when the thread is killed first. A job that is abandoned
/// still runs to completion on its pool thread, and its result is dropped.
///
/// `f` must not touch the GC heap.
pub(crate) fn run<'gc, R: Send + 'static>(
    ctx: MutationContext<'gc>,
    cx: &SchedCtx,
    f: impl FnOnce() -> R + Send + 'static,
) -> (MutationContext<'gc>, Result<R, Killed>) {
    let slot = Arc::new(Mutex::new(Slot {
        value: None,
        waker: None,
    }));
    let done = slot.clone();
    submit(Box::new(move || {
        let value = f();
        let mut slot = done.lock();
        slot.value = Some(value);
        if let Some(waker) = slot.waker.take() {
            waker.wake();
        }
    }));
    let mut ctx = ctx;
    loop {
        {
            let mut slot = slot.lock();
            if let Some(value) = slot.value.take() {
                return (ctx, Ok(value));
            }
            // Stored under the lock, so the job cannot finish between the
            // check above and the park below without waking us.
            slot.waker = Some(cx.waker());
        }
        let (next, parked) = cx.park_with(ctx);
        ctx = next;
        if let Err(killed) = parked {
            return (ctx, Err(killed));
        }
    }
}

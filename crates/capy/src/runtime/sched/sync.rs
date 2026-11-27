//! Wait queues for the blocking Scheme primitives (`mutex-acquire`,
//! `condition-wait`).
//!
//! A waiter is either a thread, woken through its scheduler [`Waker`], or
//! a plain OS thread that is not a virtual thread (an SNI-attached thread), woken
//! through a [`Parker`]. Queues live here, keyed by the address of the
//! waiting-on object. Those objects are allocated in the non-moving space, so
//! the address is stable for as long as anyone waits on it.
//!
//! Lock order: an object's own state lock first, then [`QUEUES`].

use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;

use parking_lot::{Condvar, Mutex};

use super::introspect::wait;
use super::scheduler::{self, Waker};

/// Owner tokens for threads that are not threads carry this bit, so they
/// never collide with a thread id.
const THREAD_TOKEN_BIT: u64 = 1 << 63;
static NEXT_THREAD_TOKEN: AtomicU64 = AtomicU64::new(1);

thread_local! {
    static THREAD_TOKEN: u64 =
        THREAD_TOKEN_BIT | NEXT_THREAD_TOKEN.fetch_add(1, Ordering::Relaxed);
}

/// The owner token of the calling context: its thread id, or a per-OS-thread
/// token when it is not running a thread. Never zero.
pub(crate) fn current_token() -> u64 {
    match scheduler::current() {
        Some(cx) => cx.id().as_u64(),
        None => THREAD_TOKEN.with(|t| *t),
    }
}

/// Blocking primitive for threads that are not virtual threads.
pub(crate) struct Parker {
    notified: Mutex<bool>,
    cv: Condvar,
}

impl Parker {
    pub(crate) fn new() -> Self {
        Self {
            notified: Mutex::new(false),
            cv: Condvar::new(),
        }
    }

    pub(crate) fn unpark(&self) {
        *self.notified.lock() = true;
        self.cv.notify_one();
    }

    /// Block until unparked (consuming the notification) or `deadline`.
    pub(crate) fn park_until(&self, deadline: Option<Instant>) {
        let mut notified = self.notified.lock();
        while !*notified {
            match deadline {
                None => self.cv.wait(&mut notified),
                Some(deadline) => {
                    if self.cv.wait_until(&mut notified, deadline).timed_out() {
                        break;
                    }
                }
            }
        }
        *notified = false;
    }
}

enum Wakeup {
    VirtualThread(Waker),
    Thread(Arc<Parker>),
}

/// One blocked acquirer or condition waiter.
pub(crate) struct Waiter {
    /// The owner token this waiter takes on a grant (its thread id or
    /// thread token).
    pub(crate) token: u64,
    /// Set by the granter before it wakes the waiter. Only set under the
    /// lock that guards the queue the waiter sits in.
    granted: AtomicBool,
    wakeup: Wakeup,
    /// What the waiter waits for (a [`wait`] kind), for introspection.
    pub(crate) kind: u8,
    /// The object it waits on: a mutex or condition address, or the id of the
    /// thread a joiner waits for.
    pub(crate) arg: u64,
}

impl Waiter {
    /// A waiter for the calling context, waiting for `kind` on `arg`.
    pub(crate) fn current(kind: u8, arg: u64) -> Arc<Self> {
        let (token, wakeup) = match scheduler::current() {
            Some(cx) => (cx.id().as_u64(), Wakeup::VirtualThread(cx.waker())),
            None => (
                current_token(),
                Wakeup::Thread(THREAD_PARKER.with(Arc::clone)),
            ),
        };
        Arc::new(Self {
            token,
            granted: AtomicBool::new(false),
            wakeup,
            kind,
            arg,
        })
    }

    pub(crate) fn is_granted(&self) -> bool {
        self.granted.load(Ordering::SeqCst)
    }

    /// Wake the waiter after a grant. The grant itself was recorded under the
    /// queue lock, so a wake that races a timeout cannot be lost.
    pub(crate) fn wake(&self) {
        match &self.wakeup {
            Wakeup::VirtualThread(waker) => waker.wake(),
            Wakeup::Thread(parker) => parker.unpark(),
        }
    }

    /// The parker of a non-virtual thread waiter.
    pub(crate) fn parker(&self) -> Option<&Arc<Parker>> {
        match &self.wakeup {
            Wakeup::Thread(parker) => Some(parker),
            Wakeup::VirtualThread(_) => None,
        }
    }
}

thread_local! {
    static THREAD_PARKER: Arc<Parker> = Arc::new(Parker::new());
}

type Queue = VecDeque<Arc<Waiter>>;

/// Queues keyed by object address, for every object that has waiters.
static QUEUES: Mutex<Option<HashMap<usize, Queue>>> = Mutex::new(None);

fn with_queues<T>(f: impl FnOnce(&mut HashMap<usize, Queue>) -> T) -> T {
    let mut guard = QUEUES.lock();
    f(guard.get_or_insert_with(HashMap::new))
}

/// Append `waiter` to the queue of the object at `key`.
pub(crate) fn enqueue(key: usize, waiter: Arc<Waiter>) {
    with_queues(|queues| queues.entry(key).or_default().push_back(waiter));
}

/// Remove the first waiter of `key` and grant it. The grant is recorded while
/// the queue lock is held, so a concurrent [`cancel`] sees it.
pub(crate) fn grant_first(key: usize) -> Option<Arc<Waiter>> {
    with_queues(|queues| {
        let queue = queues.get_mut(&key)?;
        let waiter = queue.pop_front()?;
        if queue.is_empty() {
            queues.remove(&key);
        }
        waiter.granted.store(true, Ordering::SeqCst);
        Some(waiter)
    })
}

/// Remove `waiter` from the queue of `key` if it is still there. Returns
/// `true` when it was removed, `false` when a grant already took it.
pub(crate) fn cancel(key: usize, waiter: &Arc<Waiter>) -> bool {
    with_queues(|queues| {
        let Some(queue) = queues.get_mut(&key) else {
            return false;
        };
        let Some(pos) = queue.iter().position(|w| Arc::ptr_eq(w, waiter)) else {
            return false;
        };
        queue.remove(pos);
        if queue.is_empty() {
            queues.remove(&key);
        }
        true
    })
}

/// A blocked thread and the thread it waits for.
#[derive(Clone, Copy, Debug)]
pub(crate) struct WaitEdge {
    /// The blocked thread's id.
    pub(crate) waiter: u64,
    /// A [`wait`] kind.
    pub(crate) kind: u8,
    /// The thread that has to act before the waiter can go on: a mutex's
    /// owner, or the thread being joined. `None` for a condition wait, whose
    /// signaller is anyone, and for a mutex nobody holds right now.
    pub(crate) holder: Option<u64>,
}

/// Every queued waiter that is a virtual thread, with its holder.
///
/// `mutex_owner` maps a mutex address to its current owner. It is called with
/// the queue lock held, which is what keeps the mutex alive: a waiter roots its
/// mutex until it leaves the queue, and leaving takes this lock. It must not
/// block on the mutex's own state lock, because the order elsewhere is state
/// first, then this lock.
pub(crate) fn wait_edges(mutex_owner: impl Fn(usize) -> Option<u64>) -> Vec<WaitEdge> {
    with_queues(|queues| {
        let mut edges = Vec::new();
        for queue in queues.values() {
            for waiter in queue {
                if waiter.token & THREAD_TOKEN_BIT != 0 {
                    continue;
                }
                let holder = match waiter.kind {
                    wait::MUTEX => mutex_owner(waiter.arg as usize),
                    wait::JOIN => Some(waiter.arg).filter(|&id| id != 0),
                    _ => None,
                };
                edges.push(WaitEdge {
                    waiter: waiter.token,
                    kind: waiter.kind,
                    holder,
                });
            }
        }
        edges.sort_unstable_by_key(|e| e.waiter);
        edges
    })
}

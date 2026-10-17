//! Soft handshakes: run an operation on every mutator at its next
//! yieldpoint without stopping the world.
//!
//! - an op is a closure object run by the target itself
//! - a target in native code is not waited for: the requester runs the op on
//!   its behalf while holding the target's monitor (see
//!   [`Thread::deliver_handshake`])
//! - each thread has an op queue, so concurrent requesters do not serialize
//!   on one request bit, and each op has its own completion counter
//! - a lone mutator runs the op inline
//!
//! # Rules for [`HandshakeOp::do_thread`]
//!
//! It runs either on the target at a yieldpoint, or on the requester while
//! the target cannot run Scheme. It must not allocate on the GC heap, enter
//! Scheme, request a handshake, or lock the target's monitor.

use std::cell::Cell;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use parking_lot::{Condvar, Mutex};

use super::thread::Thread;
use crate::heap::GarbageCollector;

/// One operation to run once per mutator.
pub trait HandshakeOp: Send + Sync {
    /// Runs once per mutator: on that thread at a yieldpoint, or on the
    /// requester while the target is in native code.
    fn do_thread(&self, thread: &Thread);
    fn name(&self) -> &'static str;
}

struct NoOp;

impl HandshakeOp for NoOp {
    fn do_thread(&self, _: &Thread) {}
    fn name(&self) -> &'static str {
        "grace-period"
    }
}

/// Counts the threads that still owe an op. The requester holds one count
/// of its own while it queues, so the total cannot reach zero early.
pub struct Completion {
    remaining: AtomicUsize,
    lock: Mutex<()>,
    done: Condvar,
}

impl Completion {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            remaining: AtomicUsize::new(1),
            lock: Mutex::new(()),
            done: Condvar::new(),
        })
    }

    pub(crate) fn add_pending(&self) {
        self.remaining.fetch_add(1, Ordering::AcqRel);
    }

    pub(crate) fn finish_one(&self) {
        if self.remaining.fetch_sub(1, Ordering::AcqRel) == 1 {
            // Take the lock so a waiter between its check and its wait
            // cannot miss the notification.
            drop(self.lock.lock());
            self.done.notify_all();
        }
    }

    fn wait(&self) {
        let mut guard = self.lock.lock();
        while self.remaining.load(Ordering::Acquire) != 0 {
            self.done.wait(&mut guard);
        }
    }

    #[cfg(test)]
    fn remaining(&self) -> usize {
        self.remaining.load(Ordering::Acquire)
    }
}

/// An op queued on a thread, with the completion it counts toward.
pub(crate) struct Pending {
    op: Arc<dyn HandshakeOp>,
    done: Arc<Completion>,
}

impl Pending {
    pub(crate) fn new(op: Arc<dyn HandshakeOp>, done: Arc<Completion>) -> Self {
        Self { op, done }
    }

    pub(crate) fn run(self, thread: &Thread) {
        run_op(&*self.op, thread);
        self.done.finish_one();
    }
}

thread_local! {
    static IN_HANDSHAKE_OP: Cell<bool> = const { Cell::new(false) };
}

pub(crate) fn run_op(op: &dyn HandshakeOp, thread: &Thread) {
    IN_HANDSHAKE_OP.with(|flag| {
        debug_assert!(!flag.get(), "handshake ops do not nest");
        flag.set(true);
    });
    op.do_thread(thread);
    IN_HANDSHAKE_OP.with(|flag| flag.set(false));
}

/// Whether the current thread is inside a [`HandshakeOp::do_thread`].
pub fn in_handshake_op() -> bool {
    IN_HANDSHAKE_OP.with(Cell::get)
}

/// Queue `op` on every other mutator and run it for the current thread.
/// Returns the completion to wait on.
fn dispatch(op: &Arc<dyn HandshakeOp>) -> Arc<Completion> {
    debug_assert!(
        !in_handshake_op(),
        "a handshake op must not request a handshake"
    );
    let done = Completion::new();
    let current = Thread::try_current_thread();
    let threads = GarbageCollector::get().threads.snapshot_threads();
    for thread in &threads {
        if current.is_some_and(|c| std::ptr::eq(c, &**thread)) || !thread.is_mutator_thread() {
            continue;
        }
        thread.deliver_handshake(op, &done);
    }
    if let Some(current) = current {
        run_op(&**op, current);
    }
    done
}

/// Run `op` for every mutator and return when all have finished it: each
/// ran it at a yieldpoint, or the requester ran it for a thread in native
/// code. A thread that registers after the snapshot is not included; it
/// starts with no stale state.
///
/// Waits in the `InNative` state, so a collection started meanwhile does not
/// wait for the requester.
pub fn handshake(op: Arc<dyn HandshakeOp>) {
    let done = dispatch(&op);
    done.finish_one();
    if done.remaining.load(Ordering::Acquire) == 0 {
        return; // lone mutator, or every target was in native code
    }
    let mutating = Thread::try_current_thread().is_some_and(|t| t.is_mutating());
    if mutating {
        Thread::enter_native();
    }
    done.wait();
    if mutating {
        Thread::leave_native();
    }
}

/// Like [`handshake`] but does not wait. Targets in native code still run
/// the op before this returns.
pub fn handshake_async(op: Arc<dyn HandshakeOp>) {
    let done = dispatch(&op);
    done.finish_one();
}

/// Returns once every mutator has passed a yieldpoint (or was outside
/// Scheme) since the call began. Run after clearing caches, this guarantees
/// no thread still executes through a cleared entry.
pub fn grace_period() {
    handshake(Arc::new(NoOp));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap::sync::thread::current_thread;
    use crate::runtime::Scheme;
    use std::sync::atomic::{AtomicBool, AtomicUsize};
    use std::sync::mpsc;
    use std::time::Duration;

    struct Count(AtomicUsize);

    impl HandshakeOp for Count {
        fn do_thread(&self, _: &Thread) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
        fn name(&self) -> &'static str {
            "count"
        }
    }

    /// Mutators a handshake reaches right now. Threads of earlier tests
    /// stay registered after their OS thread exits (idle in native code),
    /// so absolute counts would depend on test order.
    fn live_mutators() -> usize {
        GarbageCollector::get()
            .threads
            .snapshot_threads()
            .iter()
            .filter(|t| t.is_mutator_thread() && !t.get_exec_status().not_running())
            .count()
    }

    /// A mutator that polls yieldpoints until told to stop. Reports ready
    /// once it is inside `enter`.
    fn spawn_mutator(
        stop: Arc<AtomicBool>,
        ready: mpsc::Sender<()>,
    ) -> std::thread::JoinHandle<()> {
        std::thread::spawn(move || {
            let scheme = Scheme::new_uninit();
            scheme.enter(|_| {
                ready.send(()).unwrap();
                while !stop.load(Ordering::Relaxed) {
                    if current_thread().take_yieldpoint() != 0 {
                        Thread::yieldpoint();
                    }
                    std::hint::spin_loop();
                }
            });
        })
    }

    #[test]
    fn single_mutator_runs_inline() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let op = Arc::new(Count(AtomicUsize::new(0)));
            let expected = live_mutators();
            handshake(op.clone());
            assert_eq!(op.0.load(Ordering::SeqCst), expected);
            grace_period();
        });
    }

    #[test]
    fn hot_mutators_all_run_the_op() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let stop = Arc::new(AtomicBool::new(false));
            let (tx, rx) = mpsc::channel();
            let workers: Vec<_> = (0..4)
                .map(|_| spawn_mutator(stop.clone(), tx.clone()))
                .collect();
            for _ in 0..4 {
                rx.recv().unwrap();
            }
            let op = Arc::new(Count(AtomicUsize::new(0)));
            let expected = live_mutators();
            assert!(expected >= 5, "four workers plus the requester");
            handshake(op.clone());
            assert_eq!(op.0.load(Ordering::SeqCst), expected);
            stop.store(true, Ordering::Relaxed);
            for worker in workers {
                worker.join().unwrap();
            }
        });
    }

    #[test]
    fn thread_in_native_does_not_block_requester() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let (ready_tx, ready_rx) = mpsc::channel();
            let (stop_tx, stop_rx) = mpsc::channel::<()>();
            let worker = std::thread::spawn(move || {
                let scheme = Scheme::new_uninit();
                scheme.enter(|_| {
                    Thread::enter_native();
                    ready_tx.send(()).unwrap();
                    // Parked in native code: never reaches a yieldpoint.
                    stop_rx.recv().unwrap();
                    Thread::leave_native();
                });
            });
            ready_rx.recv().unwrap();
            let op = Arc::new(Count(AtomicUsize::new(0)));
            let expected = live_mutators();
            handshake(op.clone());
            assert_eq!(op.0.load(Ordering::SeqCst), expected);
            stop_tx.send(()).unwrap();
            worker.join().unwrap();
        });
    }

    #[test]
    fn concurrent_requesters_both_complete() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let stop = Arc::new(AtomicBool::new(false));
            let (tx, rx) = mpsc::channel();
            let hot = spawn_mutator(stop.clone(), tx.clone());
            rx.recv().unwrap();
            let (done_tx, done_rx) = mpsc::channel();
            let other = std::thread::spawn(move || {
                let scheme = Scheme::new_uninit();
                scheme.enter(|_| {
                    for _ in 0..200 {
                        grace_period();
                    }
                });
                done_tx.send(()).unwrap();
            });
            for _ in 0..200 {
                grace_period();
            }
            // A mutator that blocks outside Scheme must be in native code, or
            // the other requester waits for it to reach a yieldpoint.
            Thread::enter_native();
            let finished = done_rx.recv_timeout(Duration::from_secs(60));
            Thread::leave_native();
            finished.expect("other requester deadlocked");
            other.join().unwrap();
            stop.store(true, Ordering::Relaxed);
            hot.join().unwrap();
        });
    }

    #[test]
    fn handshakes_interleave_with_collections() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let stop = Arc::new(AtomicBool::new(false));
            let (tx, rx) = mpsc::channel();
            let hot = spawn_mutator(stop.clone(), tx);
            rx.recv().unwrap();
            let collector_stop = stop.clone();
            let (done_tx, done_rx) = mpsc::channel();
            let collector = std::thread::spawn(move || {
                let scheme = Scheme::new_uninit();
                // Outside `enter`: a collection request runs in native code.
                for _ in 0..20 {
                    if collector_stop.load(Ordering::Relaxed) {
                        break;
                    }
                    scheme.collect_garbage();
                }
                done_tx.send(()).unwrap();
            });
            for _ in 0..100 {
                grace_period();
            }
            Thread::enter_native();
            let finished = done_rx.recv_timeout(Duration::from_secs(120));
            Thread::leave_native();
            finished.expect("collector thread stuck");
            collector.join().unwrap();
            stop.store(true, Ordering::Relaxed);
            hot.join().unwrap();
        });
    }

    #[test]
    fn completion_counts_down_to_zero() {
        let done = Completion::new();
        done.add_pending();
        assert_eq!(done.remaining(), 2);
        done.finish_one();
        done.finish_one();
        assert_eq!(done.remaining(), 0);
        done.wait();
    }

    #[test]
    fn terminating_thread_is_not_waited_for() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            for _ in 0..50 {
                let worker = std::thread::spawn(|| {
                    let scheme = Scheme::new_uninit();
                    scheme.enter(|_| {});
                });
                grace_period();
                worker.join().unwrap();
            }
        });
    }

    #[test]
    fn async_handshake_reaches_threads_in_native() {
        let scheme = Scheme::new_uninit();
        scheme.enter(|_| {
            let op = Arc::new(Count(AtomicUsize::new(0)));
            let expected = live_mutators();
            handshake_async(op.clone());
            // Only the requester and natives ran it synchronously; with no
            // hot mutators that is everyone.
            assert_eq!(op.0.load(Ordering::SeqCst), expected);
        });
    }
}

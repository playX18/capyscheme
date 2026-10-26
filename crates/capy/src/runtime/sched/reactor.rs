//! The reactor
//!
//! Reactor is a native thread that manages readiness of a file descriptors threads
//! wait on.
//!
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::io;
use std::os::fd::BorrowedFd;
use std::sync::{Arc, OnceLock};
use std::time::Instant;

use parking_lot::Mutex;
use polling::{Event, Events, PollMode};

use super::scheduler::{Task, unpark};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Dir {
    Read,
    Write,
}

pub(crate) const WAKE_NONE: u8 = 0;
pub(crate) const WAKE_READY: u8 = 1;
pub(crate) const WAKE_TIMER: u8 = 2;
pub(crate) const WAKE_CLOSED: u8 = 3;

struct Waiter {
    task: Arc<Task>,
    /// The task's wait generation when this waiter was registered. A waiter
    /// whose generation no longer matches the task's is stale and ignored.
    generation: u64,
}

#[derive(Default)]
struct FdEntry {
    read: Vec<Waiter>,
    write: Vec<Waiter>,
    /// Whether the fd is in the poller (possibly disarmed by a oneshot event).
    registered: bool,
}

#[derive(Default)]
struct State {
    fds: HashMap<i32, FdEntry>,
    timers: BinaryHeap<Reverse<(Instant, u64)>>,
    timer_waiters: HashMap<u64, Waiter>,
    last: u64,
}

struct Reactor {
    poller: polling::Poller,
    state: Mutex<State>,
}

static REACTOR: OnceLock<&'static Reactor> = OnceLock::new();

fn reactor() -> &'static Reactor {
    REACTOR.get_or_init(|| {
        let poller = polling::Poller::new().expect("failed to create the reactor poller");
        let reactor: &'static Reactor = Box::leak(Box::new(Reactor {
            poller,
            state: Mutex::new(State::default()),
        }));
        std::thread::Builder::new()
            .name("capy-reactor".into())
            .spawn(move || run(reactor))
            .expect("failed to start the reactor thread");
        reactor
    })
}

/// Wake `waiter` with `reason` if it is still the task's current wait.
fn wake(waiter: &Waiter, reason: u8) {
    if waiter.task.wait_gen() != waiter.generation {
        return;
    }
    waiter.task.set_wake_reason(reason);
    unpark(&waiter.task);
}

/// Whether `entry` has no waiters and no poller registration, so it can be
/// dropped from the map.
fn idle(entry: &FdEntry) -> bool {
    entry.read.is_empty() && entry.write.is_empty() && !entry.registered
}

/// Make the poller's interest in `fd` match the waiters it has, or drop the
/// fd from the poller when it has none.
fn rearm(poller: &polling::Poller, fd: i32, entry: &mut FdEntry) -> io::Result<()> {
    let read = !entry.read.is_empty();
    let write = !entry.write.is_empty();
    // SAFETY: the fd is open while it has waiters. Every close goes through
    // `fd_closed`, which deletes the fd from the poller before the close.
    let source = unsafe { BorrowedFd::borrow_raw(fd) };
    if !read && !write {
        if entry.registered {
            entry.registered = false;
            let _ = poller.delete(source);
        }
        return Ok(());
    }
    let interest = Event::new(fd as usize, read, write);
    if entry.registered {
        if poller
            .modify_with_mode(source, interest, PollMode::Oneshot)
            .is_ok()
        {
            return Ok(());
        }
        // The kernel no longer knows this registration.
        entry.registered = false;
    }
    // SAFETY: as above. The fd is removed again before it is closed.
    match unsafe { poller.add_with_mode(&source, interest, PollMode::Oneshot) } {
        Ok(()) => {}
        Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {
            // A stale source for this number. The fd is open now, so a modify
            // registers it.
            poller.modify_with_mode(source, interest, PollMode::Oneshot)?;
        }
        Err(err) => return Err(err),
    }
    entry.registered = true;
    Ok(())
}

/// Drop `fd` from the poller after a failed registration, so the next `rearm`
/// starts clean. The delete fails when the fd is already closed, and that is
/// ignored
fn forget_source(poller: &polling::Poller, fd: i32, entry: &mut FdEntry) {
    entry.registered = false;
    // SAFETY: the borrow only names the fd for the delete; if the fd is
    // already closed the delete fails and the error is ignored.
    let source = unsafe { BorrowedFd::borrow_raw(fd) };
    let _ = poller.delete(source);
}

/// Give up on `fd` after its poller registration failed. The poller can no
/// longer wake anyone on it, so every remaining waiter wakes as closed rather
/// than parking forever.
fn abandon(poller: &polling::Poller, fd: i32, entry: &mut FdEntry) {
    forget_source(poller, fd, entry);
    for waiter in entry.read.drain(..).chain(entry.write.drain(..)) {
        wake(&waiter, WAKE_CLOSED);
    }
}

/// Register `task` as a waiter for `fd`. Both directions share one wait
/// generation, so the first event in either direction wins the wait and the
/// other is stale once the task re-arms. Returns the generation, or the errno when the
/// fd cannot be polled (regular files, for example) or is not open.
pub(crate) fn register_fd_dirs(
    task: &Arc<Task>,
    fd: i32,
    read: bool,
    write: bool,
) -> Result<u64, i32> {
    let r = reactor();
    let mut st = r.state.lock();
    st.last += 1;
    let generation = st.last;
    task.arm_wait(generation);
    let entry = st.fds.entry(fd).or_default();
    if read {
        entry.read.push(Waiter {
            task: task.clone(),
            generation,
        });
    }
    if write {
        entry.write.push(Waiter {
            task: task.clone(),
            generation,
        });
    }
    if let Err(err) = rearm(&r.poller, fd, entry) {
        // This wait never reached the poller. Withdraw it, and wake the
        // others on the fd, which the poller cannot serve either.
        entry.read.retain(|w| w.generation != generation);
        entry.write.retain(|w| w.generation != generation);
        abandon(&r.poller, fd, entry);
        if idle(entry) {
            st.fds.remove(&fd);
        }
        return Err(err.raw_os_error().unwrap_or(libc::EINVAL));
    }
    Ok(generation)
}

/// Register `task` as a waiter for `fd` in direction `dir`. See
/// [`register_fd_dirs`].
pub(crate) fn register_fd(task: &Arc<Task>, fd: i32, dir: Dir) -> Result<u64, i32> {
    register_fd_dirs(task, fd, dir == Dir::Read, dir == Dir::Write)
}

/// Remove the waiter registered with generation `generation` from `fd`, in
/// either direction, if the reactor has not already removed it. Generations
/// are unique, so this cannot remove another wait's waiter.
pub(crate) fn cancel_fd(fd: i32, generation: u64) {
    let r = reactor();
    let mut st = r.state.lock();
    let Some(entry) = st.fds.get_mut(&fd) else {
        return;
    };
    entry.read.retain(|w| w.generation != generation);
    entry.write.retain(|w| w.generation != generation);
    if rearm(&r.poller, fd, entry).is_err() {
        abandon(&r.poller, fd, entry);
    }
    if idle(entry) {
        st.fds.remove(&fd);
    }
}

pub(crate) fn fd_closed(fd: i32) {
    let Some(r) = REACTOR.get() else {
        return;
    };
    let mut st = r.state.lock();
    if let Some(mut entry) = st.fds.remove(&fd) {
        for waiter in entry.read.drain(..).chain(entry.write.drain(..)) {
            wake(&waiter, WAKE_CLOSED);
        }
    }

    let source = unsafe { BorrowedFd::borrow_raw(fd) };
    let _ = r.poller.delete(source);
}

/// Arrange for `task` to be woken at `deadline`. Returns the timer sequence
/// number, which `cancel_timer` takes.
pub(crate) fn register_timer(task: &Arc<Task>, deadline: Instant) -> u64 {
    let r = reactor();
    let seq = {
        let mut st = r.state.lock();
        st.last += 1;
        let seq = st.last;
        task.arm_wait(seq);
        push_timer(&mut st, task, deadline, seq, seq);
        seq
    };
    // The reactor may be sleeping past this deadline.
    let _ = r.poller.notify();
    seq
}

/// Arrange for `task` to be woken at `deadline`
pub(crate) fn register_timer_in_wait(task: &Arc<Task>, deadline: Instant, generation: u64) -> u64 {
    let r = reactor();
    let seq = {
        let mut st = r.state.lock();
        st.last += 1;
        let seq = st.last;
        push_timer(&mut st, task, deadline, seq, generation);
        seq
    };
    let _ = r.poller.notify();
    seq
}

/// Insert a timer heap entry under `seq` whose waiter wakes `task` if its
/// generation is still current.
fn push_timer(st: &mut State, task: &Arc<Task>, deadline: Instant, seq: u64, generation: u64) {
    st.timers.push(Reverse((deadline, seq)));
    st.timer_waiters.insert(
        seq,
        Waiter {
            task: task.clone(),
            generation,
        },
    );
}

/// Drop a timer registered by `register_timer`, if it has not fired.
pub(crate) fn cancel_timer(seq: u64) {
    if let Some(r) = REACTOR.get() {
        r.state.lock().timer_waiters.remove(&seq);
    }
}

fn run(r: &'static Reactor) {
    let mut events = Events::new();
    let mut touched = Vec::new();
    loop {
        let timeout = {
            let st = r.state.lock();
            st.timers
                .peek()
                .map(|Reverse((deadline, _))| deadline.saturating_duration_since(Instant::now()))
        };
        events.clear();
        let _ = r.poller.wait(&mut events, timeout);

        let mut st = r.state.lock();
        touched.clear();
        for event in events.iter() {
            let fd = event.key as i32;
            let Some(entry) = st.fds.get_mut(&fd) else {
                continue;
            };
            let failed = event.is_err() == Some(true) || event.is_interrupt();
            if event.readable || failed {
                for waiter in entry.read.drain(..) {
                    wake(&waiter, WAKE_READY);
                }
            }
            if event.writable || failed {
                for waiter in entry.write.drain(..) {
                    wake(&waiter, WAKE_READY);
                }
            }
            touched.push(fd);
        }
        for fd in touched.drain(..) {
            if let Some(entry) = st.fds.get_mut(&fd) {
                // Oneshot delivery disarmed the fd. Re-arm what remains.
                if rearm(&r.poller, fd, entry).is_err() {
                    abandon(&r.poller, fd, entry);
                }
                if idle(entry) {
                    st.fds.remove(&fd);
                }
            }
        }

        let now = Instant::now();
        while let Some(Reverse((deadline, seq))) = st.timers.peek().copied() {
            if deadline > now {
                break;
            }
            st.timers.pop();
            if let Some(waiter) = st.timer_waiters.remove(&seq) {
                wake(&waiter, WAKE_TIMER);
            }
        }
    }
}

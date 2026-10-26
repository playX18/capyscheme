//! Signal-assisted preemption
//!
//! Sysmon sends `SIGURG` to a worker whose thread has run past its quantum.
//! The handler then does the following: it arms the thread's
//! yieldpoint and preempt flags and pokes `Tc::esp`, so the thread parks at its next `enter`/`loop_hint`
//! with precise stack state.

use std::ffi::c_void;
use std::io;
use std::sync::Once;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering, compiler_fence};

use crate::heap::sync::thread::try_current_native_data;

/// The signal sysmon uses for preemption.
pub(crate) const PREEMPT_SIGNAL: libc::c_int = libc::SIGURG;

/// Size of each worker's alternate signal stack.
const ALTSTACK_BYTES: usize = 64 * 1024;

pub(crate) struct Stats {
    /// Signals delivered to a thread with a bound worker.
    pub(crate) seen: AtomicU64,
    /// Signals that armed a thread's flags.
    pub(crate) armed: AtomicU64,
    /// Signals dropped because the worker was in a `NoPreempt` region.
    pub(crate) no_preempt: AtomicU64,
    /// Signals dropped because the worker had no thread (home context).
    pub(crate) no_virtual_thread: AtomicU64,
    /// Signals for a slice other than the one sysmon asked to preempt.
    pub(crate) stale: AtomicU64,
}

pub(crate) static STATS: Stats = Stats {
    seen: AtomicU64::new(0),
    armed: AtomicU64::new(0),
    no_preempt: AtomicU64::new(0),
    no_virtual_thread: AtomicU64::new(0),
    stale: AtomicU64::new(0),
};

/// Whether sysmon sends the signal at all.
pub(crate) fn enabled() -> bool {
    crate::utils::flags::signal_preempt()
}

static INSTALL: Once = Once::new();

/// Install the `SIGURG` handler
pub(crate) fn install() {
    INSTALL.call_once(|| unsafe {
        let mut act: libc::sigaction = std::mem::zeroed();
        act.sa_sigaction = on_preempt as *const () as usize;
        act.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
        libc::sigemptyset(&mut act.sa_mask);
        let rc = libc::sigaction(PREEMPT_SIGNAL, &act, std::ptr::null_mut());
        assert_eq!(rc, 0, "sigaction(SIGURG) failed");
    });
}

/// The `SIGURG` handler
extern "C" fn on_preempt(_sig: libc::c_int, _info: *mut libc::siginfo_t, _uctx: *mut c_void) {
    STATS.seen.fetch_add(1, Ordering::Relaxed);
    let Some(nd) = try_current_native_data() else {
        return;
    };
    // a runtime-critical section on this worker wins, and so does
    // the worker's home context.
    if nd.no_preempt.load(Ordering::Relaxed) > 0 {
        STATS.no_preempt.fetch_add(1, Ordering::Relaxed);
        return;
    }
    let gp = nd.current_virtual_thread;
    // SAFETY: `current_virtual_thread` is a live header while this worker is bound
    // and is published under the thread monitor, which this handler runs
    // beside on the same thread. The flag and `esp` writes are atomic.
    unsafe {
        if gp.is_null() || (*gp).sched.task_is_null() {
            STATS.no_virtual_thread.fetch_add(1, Ordering::Relaxed);
            return;
        }
        // Only the slice sysmon asked about. The signal can land after that
        // slice switched out, and then the next thread must not be armed.
        let wanted = nd.preempt_epoch.swap(0, Ordering::AcqRel);
        if wanted == 0 || wanted != (*gp).sched.epoch.get() {
            STATS.stale.fetch_add(1, Ordering::Relaxed);
            return;
        }

        (*gp).sched.preempt.store(true, Ordering::SeqCst);
        (*gp).take_yieldpoint.store(1, Ordering::SeqCst);
        (*(*gp).tc_ptr())
            .esp
            .store(std::ptr::null_mut(), Ordering::SeqCst);
    }

    // TODO: rewrite PC for async preempt
    STATS.armed.fetch_add(1, Ordering::Relaxed);
}

#[inline]
pub(crate) fn retry_eintr<T: Copy + PartialEq + From<i8>>(mut call: impl FnMut() -> T) -> T {
    loop {
        let ret = call();
        if ret != T::from(-1) || std::io::Error::last_os_error().raw_os_error() != Some(libc::EINTR)
        {
            return ret;
        }
    }
}

/// Keeps the worker's `no_preempt` count up while alive. Taken around
/// runtime-critical sections where the handler must not poke a thread:
/// allocator slow paths, GC handshakes, scheduler code and the worker loop.
/// On a thread with no bound worker it does nothing.
pub(crate) struct NoPreempt(Option<&'static AtomicU32>);

impl NoPreempt {
    #[inline(always)]
    pub(crate) fn enter() -> Self {
        let counter = try_current_native_data().map(|nd| &nd.no_preempt);
        if let Some(counter) = counter {
            counter.fetch_add(1, Ordering::Relaxed);
            compiler_fence(Ordering::SeqCst);
        }
        Self(counter)
    }
}

impl Drop for NoPreempt {
    #[inline(always)]
    fn drop(&mut self) {
        if let Some(counter) = self.0 {
            compiler_fence(Ordering::SeqCst);
            counter.fetch_sub(1, Ordering::Relaxed);
        }
    }
}

pub(crate) struct AltStack {
    prev: libc::stack_t,
    base: *mut c_void,
    len: usize,
}

impl AltStack {
    pub(crate) fn install() -> io::Result<Self> {
        // SAFETY: sysconf has no preconditions.
        let page = unsafe { libc::sysconf(libc::_SC_PAGESIZE) } as usize;
        let len = ALTSTACK_BYTES + page;
        // SAFETY: anonymous private mapping, no existing memory is touched.
        let base = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            )
        };
        if base == libc::MAP_FAILED {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: the guard page is the first page of the mapping just made.
        unsafe { libc::mprotect(base, page, libc::PROT_NONE) };
        let mut prev: libc::stack_t = unsafe { std::mem::zeroed() };
        let new = libc::stack_t {
            ss_sp: unsafe { (base as *mut u8).add(page) } as *mut c_void,
            ss_size: ALTSTACK_BYTES,
            ss_flags: 0,
        };
        // SAFETY: both pointers refer to valid stack_t values.
        let rc = unsafe { libc::sigaltstack(&new, &mut prev) };
        if rc != 0 {
            let err = io::Error::last_os_error();
            // SAFETY: unmapping the mapping created above.
            unsafe { libc::munmap(base, len) };
            return Err(err);
        }
        Ok(Self { prev, base, len })
    }
}

impl Drop for AltStack {
    fn drop(&mut self) {
        // SAFETY: restoring the stack_t captured at install time, then
        // unmapping the stack that was current until now.
        unsafe {
            libc::sigaltstack(&self.prev, std::ptr::null_mut());
            libc::munmap(self.base, self.len);
        }
    }
}

/// Run `f` as a preemptible region: code that a forced switch could
/// interrupt. This build does not force switches, so the region is only a
/// marker and `f` runs unchanged.
///
/// # Safety
///
/// `f` must be a bounded computation. It must not allocate on the Rust heap,
/// take any lock (including a `parking_lot` guard), touch `Gc`, `Value` or
/// `MutationContext`, or call into Scheme. When forced switching is enabled,
/// violating this may corrupt the heap or deadlock another thread on the
/// same worker. A kill that arrives inside the region is delivered when the
/// region returns (see [`region_exit_hook`]).
#[inline(always)]
pub unsafe fn preemptible<R>(f: impl FnOnce() -> R) -> R {
    let result = f();
    region_exit_hook();
    result
}

/// Called on the way out of [`preemptible`]. A kill that arrived inside the
/// region is delivered here, and the thread unwinds from the region's end.
#[inline(always)]
fn region_exit_hook() {
    if let Err(killed) = super::scheduler::check_kill_now() {
        killed.unwind();
    }
}

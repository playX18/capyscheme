//! The C-ABI for scheduler
use std::cell::RefCell;
use std::ffi::{c_int, c_void};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::time::Duration;

use super::scheduler::{
    Builder, FdWait, Interest, JoinError, JoinHandle, Killed, current, ensure_gc,
};
use crate::heap::mutator::Mutator;
use crate::heap::sync::thread::try_current_native_data;

/// Opaque thread handle. The C side sees only a pointer.
pub enum CapyThread {}

/// Body of a C thread.
type GoFn = unsafe extern "C" fn(arg: *mut c_void);

const KILLED: c_int = 2;
const CAPY_THREAD_READABLE: c_int = 1;
const CAPY_THREAD_WRITABLE: c_int = 2;

thread_local! {
    /// The collector binding made by `capy_sched_attach` on this thread.
    static ATTACHED: RefCell<Option<Mutator<crate::Rootable!(())>>> = const { RefCell::new(None) };
}

/// Run `f`, mapping a panic to `fallback`. A kill payload also maps to
/// `fallback`, since the C-ABI cannot unwind. Blocking calls report kills
/// through their result instead, so this path should not see one.
fn guard<T>(fallback: T, f: impl FnOnce() -> T) -> T {
    catch_unwind(AssertUnwindSafe(f)).unwrap_or(fallback)
}

/// Whether the calling thread may use the C-ABI: a thread, or an attached thread.
fn callable() -> bool {
    current().is_some() || try_current_native_data().is_some()
}

/// Record `code` in errno and return -1.
fn fail(code: c_int) -> c_int {
    errno::set_errno(errno::Errno(code));
    -1
}

/// Map a blocking call's outcome onto the C result code.
fn killed_or(ok: c_int, outcome: Result<(), Killed>) -> c_int {
    match outcome {
        Ok(()) => ok,
        Err(Killed) => KILLED,
    }
}

/// Start a thread that runs `f(arg)` on `stack_kb` KiB of stack (0 for the
/// default). Returns null on failure. Callable from a thread or an attached
/// thread.
///
/// # Safety
///
/// `f` must be a valid function that accepts `arg`, and `arg` must stay valid
/// until `f` returns.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn capy_thread_spawn(
    f: Option<GoFn>,
    arg: *mut c_void,
    stack_kb: usize,
) -> *mut CapyThread {
    guard(std::ptr::null_mut(), || {
        let Some(f) = f else {
            return fail_null(libc::EINVAL);
        };
        if !callable() {
            return fail_null(libc::EPERM);
        }
        let arg = arg as usize;
        let mut builder = Builder::new();
        if stack_kb > 0 {
            builder = builder.stack_size(stack_kb.saturating_mul(1024));
        }
        let handle = builder.spawn(move |cx| {
            // SAFETY: the caller guarantees `f` and `arg` for the call.
            unsafe { f(arg as *mut c_void) };
            // C returned. A kill that arrived meanwhile unwinds from here,
            // where Rust frames can be unwound.
            if cx.is_killed() {
                Killed.unwind();
            }
        });
        Box::into_raw(Box::new(handle)).cast::<CapyThread>()
    })
}

fn fail_null<T>(code: c_int) -> *mut T {
    errno::set_errno(errno::Errno(code));
    std::ptr::null_mut()
}

/// Wait for the thread `g` to finish. Parks when called from a thread,
/// and blocks the OS thread otherwise. Returns 0 when it finished normally,
/// 1 when it panicked, 2 when it was killed or the caller was killed, and -1
/// on error (bad handle, not callable, or already joined).
///
/// # Safety
///
/// `g` must be a live handle from `capy_thread_spawn`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn capy_thread_join(g: *mut CapyThread) -> c_int {
    guard(-1, || {
        let Some(handle) = (unsafe { handle_ref(g) }) else {
            return fail(libc::EINVAL);
        };
        if !callable() {
            return fail(libc::EPERM);
        }
        match current() {
            Some(cx) => {
                if let Err(Killed) = handle.wait(&cx) {
                    return KILLED;
                }
            }
            None => handle.wait_blocking(),
        }
        match handle.take_result() {
            Ok(()) => 0,
            Err(JoinError::Panicked(_)) => 1,
            Err(JoinError::Killed) => KILLED,
        }
    })
}

/// Request that the thread `g` be killed. Idempotent, and does not wait.
///
/// # Safety
///
/// `g` must be a live handle from `capy_thread_spawn`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn capy_thread_kill(g: *mut CapyThread) {
    guard((), || {
        if let Some(handle) = unsafe { handle_ref(g) } {
            handle.kill();
        }
    })
}

/// Release the handle `g`. The thread keeps running, detached.
///
/// # Safety
///
/// `g` must be a handle from `capy_thread_spawn` that is not used afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn capy_thread_free(g: *mut CapyThread) {
    guard((), || {
        if !g.is_null() {
            // SAFETY: from `Box::into_raw` in `capy_thread_spawn`, freed once by contract.
            drop(unsafe { Box::from_raw(g.cast::<JoinHandle>()) });
        }
    })
}

/// The id of the running thread, or 0 when the caller is not a thread.
#[unsafe(no_mangle)]
pub extern "C" fn capy_thread_id() -> u64 {
    guard(0, || current().map_or(0, |cx| cx.id().as_u64()))
}

/// Yield to other threads. Returns 0, 2 when killed, or -1 when the
/// caller is not a thread.
#[unsafe(no_mangle)]
pub extern "C" fn capy_thread_yield() -> c_int {
    guard(-1, || match current() {
        Some(cx) => killed_or(0, cx.yield_now()),
        None => fail(libc::EPERM),
    })
}

/// Sleep for `ms` milliseconds. Returns 0, 2 when killed, or -1 when the
/// caller is not a thread.
#[unsafe(no_mangle)]
pub extern "C" fn capy_thread_sleep_ms(ms: u64) -> c_int {
    guard(-1, || match current() {
        Some(cx) => killed_or(0, cx.sleep(Duration::from_millis(ms))),
        None => fail(libc::EPERM),
    })
}

/// Wait until `fd` is ready. `events` is `CAPY_THREAD_READABLE` or
/// `CAPY_THREAD_WRITABLE`, and not both. A negative `timeout_ms` waits forever.
///
/// Returns 0 when ready, 1 on timeout, 2 when killed, 3 when the fd was closed
/// while waiting, and -1 with errno set on error.
#[unsafe(no_mangle)]
pub extern "C" fn capy_thread_await_fd(fd: c_int, events: c_int, timeout_ms: i64) -> c_int {
    guard(-1, || {
        let interest = match events {
            CAPY_THREAD_READABLE => Interest::Read,
            CAPY_THREAD_WRITABLE => Interest::Write,
            _ => return fail(libc::EINVAL),
        };
        let Some(cx) = current() else {
            return fail(libc::EPERM);
        };
        let timeout = u64::try_from(timeout_ms).ok().map(Duration::from_millis);
        match cx.await_fd(fd, interest, timeout) {
            Ok(FdWait::Ready) => 0,
            Ok(FdWait::TimedOut) => 1,
            Ok(FdWait::Closed) => 3,
            Ok(FdWait::Failed(err)) => fail(err.raw_os_error().unwrap_or(libc::EINVAL)),
            Err(Killed) => KILLED,
        }
    })
}

/// Whether the running thread has been killed or is unwinding. Returns 1
/// or 0. A thread that is not running returns 0.
#[unsafe(no_mangle)]
pub extern "C" fn capy_thread_is_killed() -> c_int {
    guard(0, || current().is_some_and(|cx| cx.is_killed()) as c_int)
}

/// Bind the calling OS thread to the collector so it may call the C-ABI and
/// the SNI API. Idempotent. Returns 0, or -1 on failure. Call
/// `capy_sched_detach` before the thread exits.
#[unsafe(no_mangle)]
pub extern "C" fn capy_sched_attach() -> c_int {
    guard(-1, || {
        if try_current_native_data().is_some() {
            return 0;
        }
        ensure_gc();
        ATTACHED.with(|slot| {
            *slot.borrow_mut() = Some(Mutator::new(|_mc| ()));
        });
        0
    })
}

/// Unbind a thread bound by `capy_sched_attach`. Returns 0, or -1 if the
/// thread was not attached by it.
#[unsafe(no_mangle)]
pub extern "C" fn capy_sched_detach() -> c_int {
    guard(-1, || {
        let mutator = ATTACHED.with(|slot| slot.borrow_mut().take());
        match mutator {
            Some(mutator) => {
                drop(mutator);
                0
            }
            None => -1,
        }
    })
}

/// # Safety
///
/// `g` must be null or a live handle from `capy_thread_spawn`.
unsafe fn handle_ref<'a>(g: *mut CapyThread) -> Option<&'a JoinHandle> {
    // SAFETY: forwarded from the caller's contract.
    unsafe { g.cast::<JoinHandle>().as_ref() }
}

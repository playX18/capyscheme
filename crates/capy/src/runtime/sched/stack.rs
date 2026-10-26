//! VirtualThread stacks: lazily committed anonymous mappings with a guard page,
//! and the SIGSEGV/SIGBUS handler that turns a guard hit into an abort.

use std::cell::UnsafeCell;
use std::io;
use std::mem::MaybeUninit;
use std::sync::Once;

use mmtk::util::Address;

/// One thread stack. The lowest page is `PROT_NONE`, so overflowing the
/// stack faults on it instead of corrupting a neighbour.
pub(crate) struct Stack {
    base: Address,
    len: usize,
    usable: usize,
    guard: usize,
}

fn page_size() -> usize {
    // SAFETY: sysconf has no preconditions.
    unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize }
}

impl Stack {
    /// Map `usable` bytes plus a guard page. Pages are committed by the kernel
    /// on first touch, so an unused stack costs address space only.
    pub(crate) fn new(usable: usize) -> io::Result<Self> {
        let guard = page_size();
        let usable = usable.next_multiple_of(guard);
        let len = usable + guard;
        // SAFETY: anonymous private mapping, no existing memory is touched.
        let base = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE,
                -1,
                0,
            )
        };
        if base == libc::MAP_FAILED {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: the guard page is the first page of the mapping just made.
        if unsafe { libc::mprotect(base, guard, libc::PROT_NONE) } != 0 {
            let err = io::Error::last_os_error();
            // SAFETY: unmapping the mapping created above.
            unsafe { libc::munmap(base, len) };
            return Err(err);
        }
        Ok(Self {
            base: Address::from_mut_ptr(base as *mut u8),
            len,
            usable,
            guard,
        })
    }

    /// The high end of the usable range, 16-byte aligned.
    pub(crate) fn top(&self) -> *mut u8 {
        (self.base + self.len).to_mut_ptr()
    }

    pub(crate) fn usable(&self) -> usize {
        self.usable
    }

    /// The guard page as `[low, high)`.
    pub(crate) fn guard_range(&self) -> (Address, Address) {
        (self.base, self.base + self.guard)
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        // SAFETY: the mapping was created by `new` with this base and length.
        unsafe { libc::munmap(self.base.to_mut_ptr(), self.len) };
    }
}

/// Stacks a worker keeps before spilling half of them to [`GLOBAL_STACKS`].
const LOCAL_LIMIT: usize = 128;

/// Stacks moved from [`GLOBAL_STACKS`] when a worker's own cache is empty.
const REFILL: usize = 32;

/// Stacks the shared pool holds. Beyond this they are unmapped.
const GLOBAL_LIMIT: usize = 4096;

/// Overflow of the per-worker caches. VirtualThreads often finish on a different
/// worker than the one that started them, so without it one worker's cache
/// fills and unmaps stacks while another maps new ones.
static GLOBAL_STACKS: parking_lot::Mutex<Vec<Stack>> = parking_lot::Mutex::new(Vec::new());

/// Per-worker pool of finished stacks.
#[derive(Default)]
pub(crate) struct StackCache {
    free: Vec<Stack>,
}

impl StackCache {
    pub(crate) fn take(&mut self, usable: usize) -> io::Result<Stack> {
        let rounded = usable.next_multiple_of(page_size());
        if let Some(i) = self.free.iter().rposition(|s| s.usable == rounded) {
            return Ok(self.free.swap_remove(i));
        }
        let mut global = GLOBAL_STACKS.lock();
        if let Some(i) = global.iter().rposition(|s| s.usable == rounded) {
            let stack = global.swap_remove(i);
            // Take a batch of the same size so the next spawns skip the lock.
            let mut moved = 0;
            while moved < REFILL {
                match global.iter().rposition(|s| s.usable == rounded) {
                    Some(j) => self.free.push(global.swap_remove(j)),
                    None => break,
                }
                moved += 1;
            }
            return Ok(stack);
        }
        drop(global);
        Stack::new(usable)
    }

    pub(crate) fn put(&mut self, stack: Stack) {
        self.free.push(stack);
        if self.free.len() > LOCAL_LIMIT {
            let keep = LOCAL_LIMIT / 2;
            let mut global = GLOBAL_STACKS.lock();
            let room = GLOBAL_LIMIT.saturating_sub(global.len());
            let spill = self.free.split_off(keep);
            // Stacks past the global limit drop here and are unmapped.
            global.extend(spill.into_iter().take(room));
        }
    }
}

/// The handlers that were installed before ours, for faults that are not
/// thread guard hits. Index 0 is SIGSEGV, 1 is SIGBUS.
struct Previous(UnsafeCell<[MaybeUninit<libc::sigaction>; 2]>);

// SAFETY: written once inside `INSTALL`, read only from the signal handler
// after that write has completed.
unsafe impl Sync for Previous {}

static PREVIOUS: Previous = Previous(UnsafeCell::new([const { MaybeUninit::uninit() }; 2]));
static INSTALL: Once = Once::new();

/// Install the guard-page handler for SIGSEGV and SIGBUS. Rust std has
/// already installed its own handler and a per-thread alternate signal stack
/// for every thread it spawned, so the handler runs on that stack and chains
/// to std's handler for faults that are not ours.
pub(crate) fn install_guard_handler() {
    INSTALL.call_once(|| {
        for (i, sig) in [libc::SIGSEGV, libc::SIGBUS].into_iter().enumerate() {
            // SAFETY: plain sigaction calls with valid, zero-initialised structs.
            unsafe {
                let mut act: libc::sigaction = std::mem::zeroed();
                act.sa_sigaction = on_fault as *const () as usize;
                act.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
                libc::sigemptyset(&mut act.sa_mask);
                let prev = &mut (*PREVIOUS.0.get())[i];
                libc::sigaction(sig, &act, prev.as_mut_ptr());
            }
        }
    });
}

extern "C" fn on_fault(sig: libc::c_int, info: *mut libc::siginfo_t, uctx: *mut libc::c_void) {
    // SAFETY: the kernel passes a valid siginfo for SA_SIGINFO handlers.
    let addr = unsafe { (*info).si_addr() } as usize;
    if let Some((low, high)) = crate::heap::sync::thread::current_guard_range() {
        if addr >= low.as_usize() && addr < high.as_usize() {
            overflow_abort(addr);
        }
    }
    let idx = if sig == libc::SIGBUS { 1 } else { 0 };
    // SAFETY: initialised by `install_guard_handler` before this handler was
    // registered.
    let prev = unsafe { (*PREVIOUS.0.get())[idx].assume_init_read() };
    let handler = prev.sa_sigaction;
    if handler == libc::SIG_DFL || handler == libc::SIG_IGN {
        // SAFETY: resetting the disposition to default; returning re-executes
        // the faulting instruction, which then terminates the process.
        unsafe { libc::signal(sig, libc::SIG_DFL) };
    } else if prev.sa_flags & libc::SA_SIGINFO != 0 {
        // SAFETY: the previous handler was installed with SA_SIGINFO.
        let f: extern "C" fn(libc::c_int, *mut libc::siginfo_t, *mut libc::c_void) =
            unsafe { std::mem::transmute(handler) };
        f(sig, info, uctx);
    } else {
        // SAFETY: the previous handler has the one-argument form.
        let f: extern "C" fn(libc::c_int) = unsafe { std::mem::transmute(handler) };
        f(sig);
    }
}

/// Write a fixed message with raw `write(2)` and abort. Nothing here
/// allocates or takes a lock.
fn overflow_abort(addr: usize) -> ! {
    const MSG: &[u8] = b"capy: thread stack overflow (guard page hit at 0x";
    const TAIL: &[u8] = b"); increase CAPY_THREAD_STACK_KB\n";
    let mut hex = [0u8; 16];
    for (i, digit) in hex.iter_mut().enumerate() {
        let nibble = (addr >> (60 - 4 * i)) & 0xf;
        *digit = b"0123456789abcdef"[nibble];
    }
    // SAFETY: writing constant buffers to stderr; the result is ignored.
    unsafe {
        libc::write(2, MSG.as_ptr().cast(), MSG.len());
        libc::write(2, hex.as_ptr().cast(), hex.len());
        libc::write(2, TAIL.as_ptr().cast(), TAIL.len());
        libc::abort();
    }
}

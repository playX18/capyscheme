//! Operating-system entropy for `(capy crypto random)`. SRFI 27's
//! xoshiro256** generator is Scheme code on the Tier 2 u64 lane
//! (`u64+/wraparound`, `u64rotl`, `bytevector-u64-ref`, ...).

use rand::TryRngCore;

use crate::heap::Gc;
use crate::prelude::*;

pub(crate) fn init<'gc>(ctx: MutationContext<'gc>) {
    random_ops::register(ctx);
}

#[cfg(not(target_vendor = "apple"))]
/// Requests up to this size are served from the per-thread pool (where the
/// source is a system call per request).
const POOL_REQUEST_MAX: usize = 256;
#[cfg(not(target_vendor = "apple"))]
/// Bytes the pool draws from the operating system at a time.
const POOL_BYTES: usize = 4096;

/// A buffer of operating-system random bytes. Small requests (a 32-byte
/// key, a 16-byte token) would otherwise pay a system call each; the pool
/// amortizes it the way the buffered `/dev/urandom` port used to. Each byte
/// is handed out once and wiped as it leaves the pool.
#[cfg(not(target_vendor = "apple"))]
struct EntropyPool {
    bytes: [u8; POOL_BYTES],
    /// The first unused byte; `POOL_BYTES` when the pool is empty.
    next: usize,
}

#[cfg(not(target_vendor = "apple"))]
thread_local! {
    static POOL: std::cell::RefCell<EntropyPool> = const {
        std::cell::RefCell::new(EntropyPool { bytes: [0; POOL_BYTES], next: POOL_BYTES })
    };
}

/// Fills `out` with operating-system random bytes. On Apple platforms that is
/// `arc4random_buf`, the system's recommended source: kernel-seeded, with no
/// size limit (`getentropy` takes 256 bytes per call there) and no failure.
#[cfg(target_vendor = "apple")]
fn fill_os_random(out: &mut [u8]) -> Result<(), <rand::rngs::OsRng as TryRngCore>::Error> {
    // SAFETY: `out` is a valid, writable buffer of `out.len()` bytes.
    unsafe { libc::arc4random_buf(out.as_mut_ptr().cast(), out.len()) };
    Ok(())
}

/// Fills `out` with operating-system random bytes (`getrandom` and the like).
#[cfg(not(target_vendor = "apple"))]
fn fill_os_random(out: &mut [u8]) -> Result<(), <rand::rngs::OsRng as TryRngCore>::Error> {
    if out.len() > POOL_REQUEST_MAX {
        return rand::rngs::OsRng.try_fill_bytes(out);
    }
    POOL.with_borrow_mut(|pool| {
        if POOL_BYTES - pool.next < out.len() {
            rand::rngs::OsRng.try_fill_bytes(&mut pool.bytes)?;
            pool.next = 0;
        }
        let taken = &mut pool.bytes[pool.next..pool.next + out.len()];
        out.copy_from_slice(taken);
        taken.fill(0);
        pool.next += out.len();
        Ok(())
    })
}

#[scheme(path = capy)]
pub mod random_ops {
    /// `(os-random-fill! bv)` fills `bv` with bytes from the operating
    /// system's cryptographically secure generator (`getentropy`,
    /// `getrandom` or the platform equivalent).
    #[scheme(name = "os-random-fill!")]
    pub fn os_random_fill(bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        // SAFETY: bytevector contents are raw bytes with no references.
        let bytes = unsafe { bv.as_slice_mut_unchecked() };
        if let Err(err) = super::fill_os_random(bytes) {
            return call.raise_error(
                "os-random-fill!",
                &format!("the system random source failed: {err}"),
                &[bv.into()],
            );
        }
        Value::unspecified()
    }
}

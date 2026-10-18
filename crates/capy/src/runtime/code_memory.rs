//! Executable memory for generated code, over onlineasm's asmkit allocator.
//!
//! Spans are the allocator's plain `Copy` handles.

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering::Release;
#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{io, sync::LazyLock};

use parking_lot::Mutex;

use onlineasm::asmkit::{JitAllocator, JitAllocatorOptions, Span};

#[cfg(test)]
static RELEASED_SPANS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
pub fn released_span_count() -> usize {
    RELEASED_SPANS.load(Ordering::SeqCst)
}

/// The executable memory of the whole runtime: one allocator behind one
/// lock, shared by baseline images, lazy cells, call IC stubs and the
/// template image.
///
/// Every method locks internally and for the duration of one allocator
/// call only, so callers never hold the lock across their own work and an
/// image may release its span from `Drop` wherever it is dropped.
///
/// Instruction-cache coherency after code writes/patches is handled by
/// `JitAllocator`, which manages the underlying memory mappings (including
/// the MAP_JIT per-thread write toggle and icache flush on Apple Silicon).
pub struct CodeMemory {
    allocator: Mutex<Box<JitAllocator>>,
}

// SAFETY: the allocator is only reached through the mutex. The raw pointers
// inside `Span`s identify executable mappings, which stay valid when the
// handle moves between threads.
unsafe impl Send for CodeMemory {}
unsafe impl Sync for CodeMemory {}

static CODE_MEMORY: LazyLock<CodeMemory> = LazyLock::new(CodeMemory::new);

/// The runtime-wide executable memory.
pub fn code_memory() -> &'static CodeMemory {
    &CODE_MEMORY
}

impl CodeMemory {
    pub fn new() -> Self {
        Self {
            allocator: Mutex::new(JitAllocator::new(JitAllocatorOptions::default())),
        }
    }

    /// Run `f` on the underlying allocator, for loading finalized images
    /// with `onlineasm::Finished::load`.
    pub fn with_allocator<R>(&self, f: impl FnOnce(&mut JitAllocator) -> R) -> R {
        f(&mut self.allocator.lock())
    }

    pub fn allocate_copy(&self, bytes: &[u8]) -> io::Result<Span> {
        if bytes.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot allocate empty code",
            ));
        }
        let mut allocator = self.allocator.lock();
        let mut span = allocator
            .alloc(bytes.len())
            .map_err(|err| io::Error::other(format!("failed to allocate JIT memory: {err:?}")))?;
        // SAFETY: `span` is a live allocation of this allocator, `bytes` fits
        // (it sized the allocation), and nothing executes it yet.
        unsafe {
            allocator
                .write(&mut span, |span| {
                    span.rw()
                        .copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
                })
                .map_err(|err| io::Error::other(format!("failed to copy JIT memory: {err:?}")))?;
        }
        Ok(span)
    }

    pub fn patch(&self, span: &mut Span, offset: usize, bytes: &[u8]) -> io::Result<()> {
        validate_patch_bounds(span, offset, bytes)?;
        // SAFETY: `span` is a live allocation of this allocator, the range was
        // bounds-checked, and the caller guarantees no thread executes the
        // patched range while it is rewritten.
        unsafe {
            self.allocator
                .lock()
                .copy_from_slice(span, offset, bytes)
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    pub fn patch_many(&self, span: &mut Span, patches: &[(usize, &[u8])]) -> io::Result<()> {
        for (offset, bytes) in patches {
            validate_patch_bounds(span, *offset, bytes)?;
        }

        // SAFETY: as in `patch`, for every range.
        unsafe {
            self.allocator
                .lock()
                .write(span, |span| {
                    for (offset, bytes) in patches {
                        if bytes.is_empty() {
                            continue;
                        }
                        span.rw()
                            .add(*offset)
                            .copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
                    }
                })
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    /// Store the 32-bit `word` at `offset` of `span` while threads may be
    /// executing the span, as one atomic store through the RW alias. The
    /// allocator's `write` does the W^X toggle and flushes the icache. Used
    /// to retarget a patch-aligned direct jump in place: the old and the new
    /// word are both valid instructions (or displacements), so a thread
    /// racing the store sees one or the other.
    ///
    /// `offset` must be 4-byte aligned and inside the span.
    pub fn patch_live_u32(&self, span: &mut Span, offset: usize, word: u32) -> io::Result<()> {
        if offset % 4 != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "live code patch is not 4-byte aligned",
            ));
        }
        validate_patch_bounds(span, offset, &[0; 4])?;
        // SAFETY: `span` is a live allocation of this allocator, the word is
        // aligned and inside it (the RW alias shares the RX alias' offsets,
        // and spans start at least 4-byte aligned), and the store is
        // atomic, so concurrent executors read a whole word.
        unsafe {
            self.allocator
                .lock()
                .write(span, |span| {
                    AtomicU32::from_ptr(span.rw().add(offset).cast::<u32>()).store(word, Release);
                })
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    pub fn release_span(&self, span: Span) -> io::Result<()> {
        // SAFETY: the caller hands over a live span of this allocator, after
        // this call it must not be used. `Span` is a plain handle, so there is
        // nothing else to drop.
        unsafe {
            self.allocator.lock().release(span.rx()).map_err(|err| {
                io::Error::other(format!("failed to release JIT memory: {err:?}"))
            })?;
        }
        #[cfg(test)]
        RELEASED_SPANS.fetch_add(1, Ordering::SeqCst);
        Ok(())
    }
}

fn validate_patch_bounds(span: &Span, offset: usize, bytes: &[u8]) -> io::Result<()> {
    if offset
        .checked_add(bytes.len())
        .is_none_or(|end| end > span.size())
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "code patch is out of bounds",
        ));
    }
    Ok(())
}

impl Default for CodeMemory {
    fn default() -> Self {
        Self::new()
    }
}

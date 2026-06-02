#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{
    io,
    mem::size_of,
    sync::{LazyLock, Mutex},
};

use asmkit::core::jit_allocator::{JitAllocator, JitAllocatorOptions, Span};

use crate::rsgc::mmtk::util::Address;

#[cfg(test)]
static RELEASED_SPANS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
pub fn released_span_count() -> usize {
    RELEASED_SPANS.load(Ordering::SeqCst)
}

#[derive(Debug)]
pub struct CodeAllocation {
    span: Option<CodeSpan>,
    pub entrypoint: Address,
    pub data_rx_base: Address,
    pub data_rw_base: Address,
    pub data_len: usize,
    pub size: usize,
}

#[derive(Debug)]
pub struct CodeSpan {
    span: Span,
}

impl CodeSpan {
    fn new(span: Span) -> Self {
        Self { span }
    }

    pub(crate) fn from_raw(span: Span) -> Self {
        Self::new(span)
    }

    pub(crate) fn into_raw(self) -> Span {
        self.span
    }

    fn as_raw_mut(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl CodeAllocation {
    pub fn span_mut(&mut self) -> &mut CodeSpan {
        self.span.as_mut().expect("code span has been moved")
    }

    pub fn take_span(&mut self) -> Option<CodeSpan> {
        self.span.take()
    }

    pub fn into_span(self) -> CodeSpan {
        self.span.expect("code span has been moved")
    }
}

/// Manages JIT-allocated executable memory for compiled code blocks.
///
/// Instruction-cache coherency after code writes/patches is handled by
/// `asmkit::core::jit_allocator::JitAllocator`, which manages the underlying
/// memory mappings (including any necessary `mprotect` + icache flush on
/// architectures that require explicit flushing, e.g., AArch64).
pub struct CodeMemory {
    allocator: Box<JitAllocator>,
}

// JIT spans are only accessed through CodeMemory's synchronized API when stored
// in the runtime-wide allocator. The raw pointers inside asmkit::Span identify
// executable mappings; moving the wrapper between threads does not invalidate
// those mappings.
unsafe impl Send for CodeMemory {}

static RUNTIME_CODE_MEMORY: LazyLock<Mutex<CodeMemory>> =
    LazyLock::new(|| Mutex::new(CodeMemory::new()));

pub fn runtime_code_memory() -> &'static Mutex<CodeMemory> {
    &RUNTIME_CODE_MEMORY
}

impl CodeMemory {
    pub fn new() -> Self {
        Self {
            allocator: JitAllocator::new(JitAllocatorOptions::default()),
        }
    }

    pub fn allocate_copy(&mut self, bytes: &[u8]) -> io::Result<CodeAllocation> {
        self.allocate_copy_with_data_slots(bytes, 0)
    }

    pub fn allocate_copy_with_data_slots(
        &mut self,
        bytes: &[u8],
        data_slot_count: usize,
    ) -> io::Result<CodeAllocation> {
        let data_offset = align_up(bytes.len(), size_of::<usize>())?;
        let data_size = data_slot_count
            .checked_mul(size_of::<usize>())
            .ok_or_else(|| io::Error::other("loaded code data section is too large"))?;
        let allocation_size = data_offset
            .checked_add(data_size)
            .ok_or_else(|| io::Error::other("loaded code allocation is too large"))?;
        let mut span = self
            .allocator
            .alloc(allocation_size)
            .map_err(|err| io::Error::other(format!("failed to allocate JIT memory: {err:?}")))?;
        unsafe {
            self.allocator
                .write(&mut span, |span| {
                    if !bytes.is_empty() {
                        span.rw()
                            .copy_from_nonoverlapping(bytes.as_ptr(), bytes.len());
                    }
                    if data_size != 0 {
                        span.rw().add(data_offset).write_bytes(0, data_size);
                    }
                })
                .map_err(|err| io::Error::other(format!("failed to copy JIT memory: {err:?}")))?;
        }

        let entrypoint = Address::from_ptr(span.rx());
        let (data_rx_base, data_rw_base) = if data_slot_count == 0 {
            (Address::ZERO, Address::ZERO)
        } else {
            (
                Address::from_ptr(unsafe { span.rx().add(data_offset) }),
                Address::from_ptr(unsafe { span.rw().add(data_offset) }),
            )
        };
        let size = span.size();
        Ok(CodeAllocation {
            span: Some(CodeSpan::new(span)),
            entrypoint,
            data_rx_base,
            data_rw_base,
            data_len: data_slot_count,
            size,
        })
    }

    pub fn patch(&mut self, span: &mut CodeSpan, offset: usize, bytes: &[u8]) -> io::Result<()> {
        self.patch_raw(span.as_raw_mut(), offset, bytes)
    }

    pub(crate) fn patch_raw(
        &mut self,
        span: &mut Span,
        offset: usize,
        bytes: &[u8],
    ) -> io::Result<()> {
        validate_patch_bounds(span, offset, bytes)?;
        unsafe {
            self.allocator
                .copy_from_slice(span, offset, bytes)
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    pub fn patch_many(
        &mut self,
        span: &mut CodeSpan,
        patches: &[(usize, &[u8])],
    ) -> io::Result<()> {
        self.patch_many_raw(span.as_raw_mut(), patches)
    }

    pub(crate) fn patch_many_raw(
        &mut self,
        span: &mut Span,
        patches: &[(usize, &[u8])],
    ) -> io::Result<()> {
        for (offset, bytes) in patches {
            validate_patch_bounds(span, *offset, bytes)?;
        }

        unsafe {
            self.allocator
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

    pub fn release_span(&mut self, span: CodeSpan) -> io::Result<()> {
        let span = span.into_raw();
        unsafe {
            self.allocator.release(span.rx()).map_err(|err| {
                io::Error::other(format!("failed to release JIT memory: {err:?}"))
            })?;
        }
        #[cfg(test)]
        RELEASED_SPANS.fetch_add(1, Ordering::SeqCst);
        Ok(())
    }
}

fn align_up(value: usize, alignment: usize) -> io::Result<usize> {
    debug_assert!(alignment.is_power_of_two());
    value
        .checked_add(alignment - 1)
        .map(|value| value & !(alignment - 1))
        .ok_or_else(|| io::Error::other("loaded code section is too large"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn allocate_copy_returns_executable_entrypoint() {
        let mut memory = CodeMemory::new();
        let loaded = memory
            .allocate_copy(&[0xb8, 42, 0, 0, 0, 0xc3])
            .expect("copy machine code");

        assert!(!loaded.entrypoint.is_zero());
        assert!(loaded.size >= 6);

        let f: extern "C" fn() -> i32 =
            unsafe { std::mem::transmute(loaded.entrypoint.as_usize()) };
        assert_eq!(f(), 42);

        memory
            .release_span(loaded.into_span())
            .expect("release code span");
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn patch_many_updates_code_and_leaves_entrypoint_executable() {
        let mut memory = CodeMemory::new();
        let mut loaded = memory
            .allocate_copy(&[0xb8, 1, 0, 0, 0, 0xc3])
            .expect("copy machine code");
        let replacement = 42i32.to_le_bytes();

        memory
            .patch_many(loaded.span_mut(), &[(1, replacement.as_slice())])
            .expect("patch code");

        let f: extern "C" fn() -> i32 =
            unsafe { std::mem::transmute(loaded.entrypoint.as_usize()) };
        assert_eq!(f(), 42);

        memory
            .release_span(loaded.into_span())
            .expect("release code span");
    }
}

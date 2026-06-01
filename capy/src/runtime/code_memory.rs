use std::{
    io,
    mem::size_of,
    sync::{LazyLock, Mutex},
};

use asmkit::core::jit_allocator::{JitAllocator, JitAllocatorOptions, Span};

use crate::rsgc::mmtk::util::Address;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CodeAllocation {
    pub handle: u32,
    pub entrypoint: Address,
    pub data_rx_base: Address,
    pub data_rw_base: Address,
    pub data_len: usize,
    pub size: usize,
}

pub struct CodeMemory {
    allocator: Box<JitAllocator>,
    spans: Vec<Span>,
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
            spans: Vec::new(),
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

        let handle = u32::try_from(self.spans.len() + 1)
            .map_err(|_| io::Error::other("too many loaded code spans"))?;
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
        self.spans.push(span);
        Ok(CodeAllocation {
            handle,
            entrypoint,
            data_rx_base,
            data_rw_base,
            data_len: data_slot_count,
            size,
        })
    }

    pub fn patch(&mut self, handle: u32, offset: usize, bytes: &[u8]) -> io::Result<()> {
        let span = code_span_mut(&mut self.spans, handle)?;
        validate_patch_bounds(span, offset, bytes)?;
        unsafe {
            self.allocator
                .copy_from_slice(span, offset, bytes)
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    pub fn patch_many(&mut self, handle: u32, patches: &[(usize, &[u8])]) -> io::Result<()> {
        let span = code_span_mut(&mut self.spans, handle)?;
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

    pub fn span_count(&self) -> usize {
        self.spans.len()
    }
}

fn align_up(value: usize, alignment: usize) -> io::Result<usize> {
    debug_assert!(alignment.is_power_of_two());
    value
        .checked_add(alignment - 1)
        .map(|value| value & !(alignment - 1))
        .ok_or_else(|| io::Error::other("loaded code section is too large"))
}

fn code_span_mut(spans: &mut [Span], handle: u32) -> io::Result<&mut Span> {
    let span_index = usize::try_from(handle)
        .ok()
        .and_then(|handle| handle.checked_sub(1))
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid code handle"))?;
    spans
        .get_mut(span_index)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "unknown code handle"))
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

impl Drop for CodeMemory {
    fn drop(&mut self) {
        for span in self.spans.drain(..) {
            unsafe {
                let _ = self.allocator.release(span.rx());
            }
        }
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

        assert_ne!(loaded.handle, 0);
        assert!(!loaded.entrypoint.is_zero());
        assert!(loaded.size >= 6);

        let f: extern "C" fn() -> i32 =
            unsafe { std::mem::transmute(loaded.entrypoint.as_usize()) };
        assert_eq!(f(), 42);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn patch_many_updates_code_and_leaves_entrypoint_executable() {
        let mut memory = CodeMemory::new();
        let loaded = memory
            .allocate_copy(&[0xb8, 1, 0, 0, 0, 0xc3])
            .expect("copy machine code");
        let replacement = 42i32.to_le_bytes();

        memory
            .patch_many(loaded.handle, &[(1, replacement.as_slice())])
            .expect("patch code");

        let f: extern "C" fn() -> i32 =
            unsafe { std::mem::transmute(loaded.entrypoint.as_usize()) };
        assert_eq!(f(), 42);
    }
}

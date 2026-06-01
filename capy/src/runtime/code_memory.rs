use std::io;

use asmkit::core::jit_allocator::{JitAllocator, JitAllocatorOptions, Span};

use crate::rsgc::mmtk::util::Address;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LoadedCodeRef {
    pub handle: u32,
    pub entrypoint: Address,
    pub size: usize,
}

pub struct CodeMemory {
    allocator: Box<JitAllocator>,
    spans: Vec<Span>,
}

impl CodeMemory {
    pub fn new() -> Self {
        Self {
            allocator: JitAllocator::new(JitAllocatorOptions::default()),
            spans: Vec::new(),
        }
    }

    pub fn allocate_copy(&mut self, bytes: &[u8]) -> io::Result<LoadedCodeRef> {
        let mut span = self
            .allocator
            .alloc(bytes.len())
            .map_err(|err| io::Error::other(format!("failed to allocate JIT memory: {err:?}")))?;
        unsafe {
            self.allocator
                .copy_from_slice(&mut span, 0, bytes)
                .map_err(|err| io::Error::other(format!("failed to copy JIT memory: {err:?}")))?;
        }

        let handle = u32::try_from(self.spans.len() + 1)
            .map_err(|_| io::Error::other("too many loaded code spans"))?;
        let entrypoint = Address::from_ptr(span.rx());
        let size = span.size();
        self.spans.push(span);
        Ok(LoadedCodeRef {
            handle,
            entrypoint,
            size,
        })
    }

    pub fn patch(&mut self, handle: u32, offset: usize, bytes: &[u8]) -> io::Result<()> {
        let span_index = usize::try_from(handle)
            .ok()
            .and_then(|handle| handle.checked_sub(1))
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid code handle"))?;
        let span = self
            .spans
            .get_mut(span_index)
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "unknown code handle"))?;
        if offset
            .checked_add(bytes.len())
            .is_none_or(|end| end > span.size())
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "code patch is out of bounds",
            ));
        }

        unsafe {
            self.allocator
                .copy_from_slice(span, offset, bytes)
                .map_err(|err| io::Error::other(format!("failed to patch JIT memory: {err:?}")))?;
        }
        Ok(())
    }

    pub fn span_count(&self) -> usize {
        self.spans.len()
    }
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
}

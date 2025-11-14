//! Local Allocation Buffer

use mmtk::util::{Address, conversions::raw_align_up};

use crate::rsgc::util::align_allocation_no_fill;

/// Local allocation buffer: simple bump-pointer
/// allocator for fast allocation of objects when possible.
///
/// This type works by requesting chunk of memory from MMTk.
#[repr(C)]
pub struct LocalAllocationBuffer {
    pub cursor: Address,
    pub limit: Address,
}

impl Default for LocalAllocationBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalAllocationBuffer {
    pub const fn new() -> Self {
        Self {
            cursor: Address::ZERO,
            limit: Address::ZERO,
        }
    }

    pub fn allocate(&mut self, size: usize, alignment: usize, offset: usize) -> Address {
        let size = raw_align_up(size, alignment);
        let result = align_allocation_no_fill(self.cursor, alignment, offset);
        if result + size > self.limit {
            return Address::ZERO;
        }

        self.cursor = result.add(size);
        result
    }

    pub fn rebind(&mut self, cursor: Address, limit: Address) {
        self.cursor = cursor;
        self.limit = limit;
    }

    pub fn take(&mut self) -> (Address, Address) {
        let cursor = self.cursor;
        let limit = self.limit;

        self.reset();

        (cursor, limit)
    }

    pub fn reset(&mut self) {
        self.cursor = Address::ZERO;
        self.limit = Address::ZERO;
    }
}

use std::{cell::Cell, marker::PhantomData, panic::AssertUnwindSafe};

use mmtk::{util::Address, vm::SlotVisitor};

use crate::{ObjectSlot, Trace, prelude::Value, runtime::Context};

/// Interpreted code stack.
pub struct InterpreterStack<'gc> {
    pub stack_top: Address,
    pub stack_bottom: Address,
    pub last_sp: Cell<Address>,
    pub last_bp: Cell<Address>,
    marker: PhantomData<&'gc ()>,
}

unsafe impl<'gc> Trace for InterpreterStack<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::Visitor) {
        let mut current = self.last_sp.get();
        let end = self.stack_top;
        while current < end {
            // safety: current is in [stack_bottom, stack_top)
            unsafe {
                let slot: *mut Value<'gc> = current.to_mut_ptr();
                let value = slot.read();
                // our stack can potentially hold non-GC values (i.e FFI pointers, integers etc)
                // and they might not be NaN boxed as well, so we need to check if the value is a cell
                // and that it is in MMTk spaces.
                if value.is_cell() {
                    // safety: `is_cell()` checks if object is not null
                    let cell = value.as_cell_raw().to_objref().unwrap_unchecked();
                    if mmtk::memory_manager::is_in_mmtk_spaces(cell) {
                        visitor.visit_slot(ObjectSlot::from_address(current));
                    }
                }

                current = current.add(size_of::<Value<'gc>>());
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> InterpreterStack<'gc> {
    /// Create a new interpreter stack with the given size.
    ///
    /// Size is given in multiple of [`Value`](crate::prelude::Value).
    pub fn new(stack_size: usize) -> Self {
        let stack_size_in_bytes = stack_size * size_of::<Value<'gc>>();
        let stack_bottom = internal::allocate_stack(stack_size_in_bytes);
        let stack_top = stack_bottom.add(stack_size_in_bytes);
        Self {
            stack_top,
            stack_bottom,
            last_sp: Cell::new(stack_bottom),
            last_bp: Cell::new(stack_bottom),
            marker: PhantomData,
        }
    }

    pub fn empty() -> Self {
        Self {
            stack_top: Address::ZERO,
            stack_bottom: Address::ZERO,
            last_sp: Cell::new(Address::ZERO),
            last_bp: Cell::new(Address::ZERO),
            marker: PhantomData,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stack_top.is_zero()
    }
}

impl<'gc> Drop for InterpreterStack<'gc> {
    fn drop(&mut self) {
        if self.stack_top.is_zero() {
            return;
        }
        let stack_size_in_bytes = self.stack_top.get_offset(self.stack_bottom);
        assert!(stack_size_in_bytes >= 0);
        internal::deallocate_stack(self.stack_bottom, stack_size_in_bytes as usize);
    }
}

#[cfg(unix)]
mod internal {
    use std::ptr::null_mut;

    use mmtk::util::{Address, conversions::raw_align_up};

    pub fn allocate_stack(stack_size_in_bytes: usize) -> Address {
        let page_size = mmtk::util::constants::BYTES_IN_PAGE;
        let aligned_stack_size = raw_align_up(stack_size_in_bytes, page_size);
        unsafe {
            let mmap = libc::mmap(
                null_mut(),
                aligned_stack_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_GROWSDOWN | libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );
            if mmap == libc::MAP_FAILED {
                return Address::zero();
            }
            libc::madvise(mmap, aligned_stack_size, libc::MADV_SEQUENTIAL);
            Address::from_mut_ptr(mmap)
        }
    }

    pub fn deallocate_stack(stack_bottom: Address, stack_size_in_bytes: usize) {
        let aligned_stack_size =
            raw_align_up(stack_size_in_bytes, mmtk::util::constants::BYTES_IN_PAGE);
        unsafe {
            libc::munmap(stack_bottom.to_mut_ptr(), aligned_stack_size);
        }
    }
}

/// Run the given closure with a new interpreter stack. The old stack is restored after the closure returns.
pub fn call_with_stack<'gc, F, R>(ctx: Context<'gc>, stack_size: usize, f: F) -> R
where
    F: FnOnce(Context<'gc>) -> R,
{
    let mut stack = InterpreterStack::new(stack_size);

    unsafe { ctx.stack.get().swap(&mut stack) };
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| f(ctx)));
    unsafe { ctx.stack.get().swap(&mut stack) };
    match result {
        Ok(r) => r,
        Err(e) => std::panic::resume_unwind(e),
    }
}

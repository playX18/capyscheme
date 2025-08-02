use rsgc::{Trace, mmtk::util::Address};

use crate::runtime::value::Value;

pub mod primitive;

#[repr(C)]
pub struct VMState<'gc> {
    pub proc: Value<'gc>,
    pub argc: usize,
    pub argv: Address,

    pub args_start: Address,
    pub args_size: usize,
}
unsafe impl<'gc> Trace for VMState<'gc> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.proc);

        for i in 0..self.argc {
            unsafe {
                let addr = self.argv.sub(i * size_of::<Value>()).as_mut_ref::<Value>();
                visitor.trace(addr);
            }
        }
    }
}

const ARGS_SIZE: usize = 8192;

impl<'gc> VMState<'gc> {
    pub fn new() -> Self {
        unsafe {
            let args_start = libc::mmap(
                std::ptr::null_mut(),
                ARGS_SIZE * size_of::<Value>(),
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );

            if args_start == libc::MAP_FAILED {
                panic!("Failed to allocate memory for VMState arguments");
            }

            let args_start = Address::from_ptr(args_start as *mut u8);
            let args_size = ARGS_SIZE * size_of::<Value>();
            let argv = args_start;

            VMState {
                proc: Value::undefined(),
                argc: 0,
                argv,

                args_start: args_start,
                args_size: args_size,
            }
        }
    }

    pub fn argument_count(&self) -> usize {
        self.argc
    }

    pub fn arguments(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.argv.to_ptr::<Value>(), self.argc) }
    }
}

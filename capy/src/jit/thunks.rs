use mmtk::AllocationSemantics;

use crate::runtime::Context;

pub extern "C-unwind" fn allocate_object<'gc>(ctx: Context<'gc>, size: usize) -> *mut () {
    unsafe {
        ctx.mutation()
            .raw_alloc_uninit(size, 8, AllocationSemantics::Default)
            .to_address()
            .to_mut_ptr()
    }
}

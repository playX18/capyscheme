use mmtk::{
    MMTKBuilder,
    util::{ObjectReference, heap::vm_layout::VMLayout},
    vm::{VMBinding, slot::UnimplementedMemorySlice},
};
#[cfg(feature = "aslr")]
use mmtk::{
    util::Address, util::conversions::chunk_align_down, util::heap::vm_layout::BYTES_IN_CHUNK,
};
#[cfg(feature = "aslr")]
use rand::Rng;

use crate::rsgc::ObjectSlot;

#[derive(Default)]
pub struct MemoryManager;

impl VMBinding for MemoryManager {
    type VMObjectModel = super::object::ObjectModel;
    type VMScanning = super::scanning::RustScanning;
    type VMCollection = super::collection::Collection;
    type VMActivePlan = super::active_plan::ActivePlan;

    type VMMemorySlice = UnimplementedMemorySlice<ObjectSlot>;
    type VMSlot = ObjectSlot;
    type VMReferenceGlue = ReferenceGlue;

    const MAX_ALIGNMENT: usize = 512;
    const MIN_ALIGNMENT: usize = 8;
}

pub struct ReferenceGlue;

impl mmtk::vm::ReferenceGlue<MemoryManager> for ReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(_new_reference: ObjectReference) {
        unreachable!()
    }

    fn get_referent(_object: ObjectReference) -> Option<ObjectReference> {
        unreachable!()
    }

    fn set_referent(_reff: ObjectReference, _referent: ObjectReference) {
        unreachable!()
    }

    fn enqueue_references(_references: &[ObjectReference], _tls: mmtk::util::VMWorkerThread) {
        unreachable!()
    }
}

#[cfg(feature = "aslr")]
fn get_random_mmap_addr() -> Address {
    let mut rng = rand::rng();
    let uniform = rand::distr::Uniform::new(0x4000_0000, usize::MAX).unwrap();
    let mut raw_addr = unsafe { Address::from_usize(rng.sample(uniform)) };

    raw_addr = chunk_align_down(raw_addr);
    raw_addr = unsafe { Address::from_usize(raw_addr & 0x3FFFFFFFF000usize) };

    raw_addr
}
#[cfg(feature = "aslr")]
fn is_address_maybe_mmaped(addr: Address, size: usize) -> bool {
    #[cfg(unix)]
    {
        unsafe {
            let res = libc::msync(addr.to_mut_ptr(), size, libc::MS_ASYNC);

            if res == -1 && errno::errno().0 == libc::ENOMEM {
                return true;
            }
        }
    }

    false
}

/// Set the Virtual Memory Layout for MMTK.
///
/// If `aslr` feature is enabled, this will try to randomize the heap address.
///
/// ## Note on ASLR
///
/// ASLR has a performance penalty, notably it disables contiguous spaces which can lead to worser
/// performance.
#[allow(unused_mut, unused_variables)]
pub fn vm_layout(builder: &mut MMTKBuilder) {
    //return;
    let mut vm_layout = VMLayout::default();

    let size = builder.options.gc_trigger.max_heap_size();

    #[cfg(feature = "aslr")]
    {
        vm_layout.force_use_contiguous_spaces = false;

        loop {
            let start = get_random_mmap_addr().align_down(BYTES_IN_CHUNK);

            if !is_address_maybe_mmaped(start, size) {
                continue;
            }

            let end = (start + size).align_up(BYTES_IN_CHUNK);
            vm_layout.heap_start = start;
            vm_layout.heap_end = end;

            vm_layout.log_address_space = if cfg!(target_pointer_width = "64") {
                47
            } else {
                31
            };

            vm_layout.log_space_extent = size.trailing_zeros() as usize - 1;

            break;
        }
    }

    builder.set_vm_layout(vm_layout);
}

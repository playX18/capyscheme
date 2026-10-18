//! Slot storage of Scheme instances.
//!
//! A [`SlotStorage`] carries the layout stamp of the class descriptor it was
//! built for, so one object holds both what an inline cache checks (the
//! stamp) and the data it then loads (the values). An instance points at its
//! storage with one word; migrating to a new layout allocates a new storage
//! and publishes that one pointer.
//!
//! ```text
//! +0  stamp: u32     layout stamp of the descriptor, fixed at allocation
//! +4  len:   u32     number of values, fixed at allocation
//! +8  values[len]: Lock<Value>
//! ```
//!
//! The values stay at offset 8 where the array length word used to be
//! followed by its elements, which is what compiled code indexes.

use std::alloc::Layout;
use std::mem::MaybeUninit;
use std::ops::Index;

use mmtk::{AllocationSemantics, util::conversions::raw_align_up};

use crate::heap::barrier::IndexWrite;
use crate::heap::cell::Lock;
use crate::heap::collection::Visitor;
use crate::heap::object::{AllocationHooks, GcObject, type_class_header_word};
use crate::heap::{Gc, Mutation, Trace, WeakProcessor};
use crate::runtime::MutationContext;
use crate::runtime::value::Value;

/// Offset of the values from the start of the object.
pub const STORAGE_DATA_OFFSET: usize = 8;
/// Offset of the stamp.
pub const STORAGE_STAMP_OFFSET: usize = 0;

#[repr(C)]
pub struct SlotStorage<'gc> {
    stamp: u32,
    len: u32,
    data: [MaybeUninit<Lock<Value<'gc>>>; 1],
}

const _: () = assert!(std::mem::offset_of!(SlotStorage<'static>, data) == STORAGE_DATA_OFFSET);
const _: () = assert!(std::mem::offset_of!(SlotStorage<'static>, stamp) == STORAGE_STAMP_OFFSET);

// SAFETY: traces exactly the `len` initialized values.
unsafe impl<'gc> Trace for SlotStorage<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for value in self.values_mut() {
            unsafe { value.trace(visitor) };
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        for value in self.values_mut() {
            unsafe { value.process_weak_refs(weak_processor) };
        }
    }
}

extern "C" fn storage_size(obj: GcObject) -> usize {
    // SAFETY: `obj` is a `SlotStorage` allocated by `SlotStorage::with`.
    unsafe {
        let storage = obj.to_address().as_ref::<SlotStorage<'static>>();
        let layout = Layout::array::<Lock<Value<'static>>>(storage.len as usize);
        raw_align_up(
            layout.expect("invariant holds").size() + STORAGE_DATA_OFFSET,
            align_of::<SlotStorage<'static>>(),
        )
    }
}

extern "C" fn storage_trace(obj: GcObject, vis: &mut Visitor) {
    // SAFETY: as in `storage_size`.
    unsafe {
        obj.to_address()
            .as_mut_ref::<SlotStorage<'static>>()
            .trace(vis)
    }
}

extern "C" fn storage_weak(obj: GcObject, weak_processor: &mut WeakProcessor) {
    // SAFETY: as in `storage_size`.
    unsafe {
        obj.to_address()
            .as_mut_ref::<SlotStorage<'static>>()
            .process_weak_refs(weak_processor)
    }
}

impl<'gc> SlotStorage<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "SlotStorage",
        instance_size: 0,
        compute_size: Some(storage_size),
        compute_alignment: None,
        alignment: align_of::<SlotStorage<'static>>(),
        trace: storage_trace,
        weak_proc: storage_weak,
    };

    /// Allocate storage stamped `stamp` with `len` values, value `i` being
    /// `f(i)`. `f` may allocate: the storage is published, with the values
    /// built so far, before it runs.
    pub fn with(
        mc: impl Into<Mutation<'gc>>,
        stamp: u32,
        len: usize,
        mut f: impl FnMut(Mutation<'gc>, usize) -> Value<'gc>,
    ) -> Gc<'gc, Self> {
        let mc = mc.into();
        let size = STORAGE_DATA_OFFSET + len * size_of::<Lock<Value<'gc>>>();
        // SAFETY: the allocation is zeroed and large enough for `len`
        // values; `len` counts the initialized prefix while publishing, so
        // a collection during `f` traces only initialized values.
        unsafe {
            let allocation = mc.raw_allocate_with_header_word_unpublished(
                size,
                align_of::<Self>(),
                type_class_header_word::<SlotStorage<'static>>(
                    MutationContext::from(mc),
                    Self::HOOKS,
                ),
                AllocationSemantics::Default,
            );
            let alloc = allocation.object();
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.stamp = stamp;
            this.len = 0;
            mc.publish_allocated_object(allocation);
            for i in 0..len {
                this.data
                    .as_mut_ptr()
                    .add(i)
                    .write(MaybeUninit::new(Lock::new(f(mc, i))));
                this.len = i as u32 + 1;
            }
            Gc::from_gc_object(alloc)
        }
    }

    /// The layout stamp this storage was built for.
    #[inline(always)]
    pub fn stamp(&self) -> u32 {
        self.stamp
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline(always)]
    pub fn values(&self) -> &[Lock<Value<'gc>>] {
        // SAFETY: the first `len` values are initialized.
        unsafe { std::slice::from_raw_parts(self.data.as_ptr().cast(), self.len as usize) }
    }

    fn values_mut(&mut self) -> &mut [Lock<Value<'gc>>] {
        // SAFETY: as in `values`.
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr().cast(), self.len as usize) }
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<Value<'gc>> {
        self.values().get(index).map(Lock::get)
    }
}

impl<'gc> Index<usize> for SlotStorage<'gc> {
    type Output = Lock<Value<'gc>>;

    #[inline(always)]
    fn index(&self, index: usize) -> &Self::Output {
        &self.values()[index]
    }
}

// SAFETY: indexing yields a `Lock` inside this object, which `Write` may
// project to.
unsafe impl<'gc> IndexWrite<usize> for SlotStorage<'gc> {}

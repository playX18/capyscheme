//! Shared closure environment records (`EnvRecord`).
//!
//! One heap record shared by all closures created at the same site: it holds
//! the free variables common to the group, and each closure stores a single
//! pointer to it plus its private captures.
//!
//! Layout: `[length | slot 0 .. slot length-1]`. The length word is stored
//! before the slot array (mirroring `Closure`'s `nfree`); the GC traces
//! exactly `length` slots.

use std::ops::Index;

use mmtk::AllocationSemantics;

use crate::heap::object::{
    AllocationHooks, ClassId, GcObject, builtin_class_ids, class_header_word,
};
use crate::heap::{Gc, cell::Lock, collection::Visitor};
use crate::runtime::Context;
use crate::runtime::value::Value;
use crate::IndexWrite;

#[repr(C)]
pub struct EnvRecord<'gc> {
    pub length: usize,
    pub slots: [Lock<Value<'gc>>; 0],
}

extern "C" fn trace_env_record(obj: GcObject, visitor: &mut Visitor) {
    // SAFETY: `obj` is guaranteed by the GC to point to a valid `EnvRecord`
    // allocated with an env-record class header. We iterate exactly `length`
    // trailing slots.
    unsafe {
        let record = obj.to_address().as_mut_ref::<EnvRecord>();
        for i in 0..record.length {
            visitor.trace(
                record
                    .slots
                    .as_mut_ptr()
                    .add(i)
                    .as_mut()
                    .expect("pointer in allocated entry range"),
            );
        }
    }
}

extern "C" fn process_weak(_obj: GcObject, _weak_processor: &mut crate::heap::WeakProcessor) {
    // No weak references in EnvRecord, so do nothing.
}

extern "C" fn compute_env_record_size(obj: GcObject) -> usize {
    // SAFETY: `obj` is a valid `EnvRecord` allocated by the GC with an
    // env-record class header.
    unsafe {
        let record = obj.to_address().as_ref::<EnvRecord>();
        size_of::<Value>() * record.length
    }
}

pub static ENV_RECORD_HOOKS: AllocationHooks = AllocationHooks {
    alignment: align_of::<EnvRecord>(),
    compute_alignment: None,
    instance_size: size_of::<EnvRecord>(),
    compute_size: Some(compute_env_record_size),
    trace: trace_env_record,
    weak_proc: process_weak,
    type_name: "env-record",
};

impl<'gc> EnvRecord<'gc> {
    /// Offset of the slot array from the start of the struct. Used for codegen.
    pub const DATA_OFFSET: isize = std::mem::offset_of!(EnvRecord, slots) as isize;

    /// Allocate an env record holding exactly `slots` values.
    ///
    /// `length` is written before anything else so the GC never observes an
    /// uninitialized trace length (same discipline as `Closure::nfree`).
    pub fn new(ctx: Context<'gc>, slots: &[Value<'gc>]) -> Gc<'gc, Self> {
        let length = slots.len();
        let size = size_of::<Self>() + size_of::<Value>() * length;
        // SAFETY: We raw-allocate an `EnvRecord` + trailing slot array via the
        // GC allocator. The env-record class hooks ensure proper tracing. We
        // initialize all fields before returning the Gc handle.
        unsafe {
            let ptr = ctx.raw_allocate_with_header_word(
                size,
                align_of::<Self>(),
                class_header_word(
                    ClassId::new(builtin_class_ids::ENV_RECORD).expect("builtin class id is nonzero"),
                ),
                AllocationSemantics::Default,
            );
            let this = ptr.to_address().as_mut_ref::<Self>();
            this.length = length;
            for i in 0..length {
                this.slots.as_mut_ptr().add(i).write(Lock::new(slots[i]));
            }
            Gc::from_gc_object(ptr)
        }
    }
}

impl<'gc> Index<usize> for EnvRecord<'gc> {
    type Output = Lock<Value<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(
            index < self.length,
            "index out of bounds: index={index}, len={length}",
            length = self.length
        );
        // SAFETY: `slots` is a flexible array member allocated with `length`
        // elements; the debug_assert above ensures `index < length`.
        unsafe { self.slots.as_ptr().add(index).as_ref_unchecked() }
    }
}

// SAFETY: EnvRecord's Index impl already validates bounds; IndexWrite just
// permits mutable access to the same in-bounds slots. The GC write barrier is
// handled at the call site.
unsafe impl<'gc> IndexWrite<usize> for EnvRecord<'gc> {}

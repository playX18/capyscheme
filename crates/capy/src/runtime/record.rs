//! Record type descriptors and record constructor descriptors.

use std::cell::Cell;

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, cell::Lock};

use crate::runtime::{
    MutationContext,
    value::{ClassTagged, Tuple, Value},
};

/// The record type is generative: every `make-record-type-descriptor` call
/// creates a distinct type, even with the same name and fields.
pub const RTD_GENERATIVE: u32 = 1;
/// The record type is opaque: `record?` is false for its instances.
pub const RTD_OPAQUE: u32 = 2;
/// The record type cannot be extended.
pub const RTD_SEALED: u32 = 4;

/// A record type descriptor.
#[repr(C)]
pub struct Rtd<'gc> {
    /// Tuple of ancestor descriptors, `#f ... grandparent parent self`. The
    /// last element is always this descriptor, patched in after allocation.
    pub ancestry: Gc<'gc, Tuple<'gc>>,
    /// The record type's name, a symbol.
    pub name: Value<'gc>,
    /// List of `(mutable? . name)` pairs for the fields this type declares.
    pub fields: Value<'gc>,
    /// Total number of fields, including inherited ones.
    pub field_count: u32,
    /// Bit set of `RTD_*` flags.
    pub flags: Cell<u32>,
    /// The nongenerative unique id (a symbol), or `#f` when generative.
    pub uid: Value<'gc>,
    /// Printer procedure, or `#f` for the default printer.
    pub printer: Lock<Value<'gc>>,
}

fn rtd_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::RTD).expect("builtin class id is nonzero"))
}

unsafe impl<'gc> Trace for Rtd<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.ancestry.trace(visitor);
            self.name.trace(visitor);
            self.fields.trace(visitor);
            self.uid.trace(visitor);
            self.printer.trace(visitor);
        }
    }

    // SAFETY: Descriptors hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for Rtd<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::RTD];
    const TYPE_NAME: &'static str = "record-type-descriptor";
}

impl<'gc> Rtd<'gc> {
    /// Allocates a record type descriptor.
    ///
    /// `parent` is the parent type, if any. `uid` is a symbol for a
    /// nongenerative type, or `#f` for a generative one. `fields_list` is the
    /// normalized list of `(mutable? . name)` pairs for the fields this type
    /// declares, and `own_field_count` is the number of those fields. Opacity
    /// is inherited from `parent`.
    pub fn new(
        ctx: MutationContext<'gc>,
        name: Value<'gc>,
        parent: Option<Gc<'gc, Rtd<'gc>>>,
        uid: Value<'gc>,
        sealed: bool,
        opaque: bool,
        fields_list: Value<'gc>,
        own_field_count: u32,
    ) -> Gc<'gc, Self> {
        let parent_ancestry_len = parent.map_or(0, |p| p.ancestry.len());
        let depth = parent_ancestry_len; // index of self in the new ancestry
        let ancestry = Tuple::new(ctx, depth + 1, Value::new(false));
        if let Some(parent) = parent {
            let parent_slice = parent.ancestry.as_slice();
            let tuple = Gc::write(ctx, ancestry);
            for (i, value) in parent_slice.iter().enumerate() {
                tuple[i].unlock().set(*value);
            }
        }

        let (inherited_opaque, inherited_fields) = match parent {
            Some(p) => (p.is_opaque(), p.field_count),
            None => (false, 0),
        };

        let mut flags = 0;
        if uid == Value::new(false) {
            flags |= RTD_GENERATIVE;
        }
        if sealed {
            flags |= RTD_SEALED;
        }
        if opaque || inherited_opaque {
            flags |= RTD_OPAQUE;
        }

        let rtd = ctx.allocate_with(
            Self {
                ancestry,
                name,
                fields: fields_list,
                field_count: inherited_fields + own_field_count,
                flags: Cell::new(flags),
                uid,
                printer: Lock::new(Value::new(false)),
            },
            rtd_header_word(),
        );

        // Patch self into the last slot now that the descriptor exists.
        let tuple = Gc::write(ctx, ancestry);
        tuple[depth].unlock().set(rtd.into());
        rtd
    }

    /// Depth of this type in its hierarchy; a root type has depth 0.
    pub fn depth(&self) -> usize {
        self.ancestry.len() - 1
    }

    /// The parent descriptor, or `None` for a root type.
    pub fn parent(&self) -> Option<Gc<'gc, Rtd<'gc>>> {
        let depth = self.depth();
        if depth == 0 {
            return None;
        }
        let parent = self.ancestry[depth - 1].get();
        Some(parent.downcast::<Rtd<'gc>>())
    }

    /// Whether this type is `other` or one of its descendants.
    pub fn is_subtype_of(&self, other: Gc<'gc, Rtd<'gc>>) -> bool {
        let depth = other.depth();
        if depth >= self.ancestry.len() {
            return false;
        }
        self.ancestry[depth].get() == Value::from(other)
    }

    pub fn is_generative(&self) -> bool {
        self.flags.get() & RTD_GENERATIVE != 0
    }

    pub fn is_opaque(&self) -> bool {
        self.flags.get() & RTD_OPAQUE != 0
    }

    pub fn is_sealed(&self) -> bool {
        self.flags.get() & RTD_SEALED != 0
    }

    /// Whether a second nongenerative declaration with the same uid may reuse
    /// this descriptor instead of creating a new one.
    ///
    /// `parent`, `fields_list`, `sealed` and `opaque` are the arguments of the
    /// new declaration. Returning `false` makes the caller raise the
    /// "mismatched subsequent call for nongenerative record-type" violation.
    pub fn nongenerative_compatible(
        &self,
        parent: Option<Gc<'gc, Rtd<'gc>>>,
        fields_list: Value<'gc>,
        sealed: bool,
        opaque: bool,
    ) -> bool {
        // Chez also compares flags and layout (record.ss:617-660). Opacity is
        // inherited, so compare against what `Rtd::new` would compute.
        let same_parent = match (self.parent(), parent) {
            (None, None) => true,
            (Some(a), Some(b)) => Gc::ptr_eq(a, b),
            _ => false,
        };
        let effective_opaque = opaque || parent.is_some_and(|p| p.is_opaque());

        same_parent
            && self.is_sealed() == sealed
            && self.is_opaque() == effective_opaque
            && self.fields.equal(fields_list, &mut Default::default())
    }
}

/// A record constructor descriptor: a record type plus the constructor
/// protocol used to build its instances.
#[repr(C)]
pub struct Rcd<'gc> {
    /// The record type this constructor builds.
    pub rtd: Gc<'gc, Rtd<'gc>>,
    /// The parent constructor descriptor, or `#f`.
    pub prcd: Value<'gc>,
    /// The constructor protocol procedure, or `#f`.
    pub protocol: Value<'gc>,
}

fn rcd_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::RCD).expect("builtin class id is nonzero"))
}

unsafe impl<'gc> Trace for Rcd<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.rtd.trace(visitor);
            self.prcd.trace(visitor);
            self.protocol.trace(visitor);
        }
    }

    // SAFETY: Descriptors hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for Rcd<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::RCD];
    const TYPE_NAME: &'static str = "record-constructor-descriptor";
}

impl<'gc> Rcd<'gc> {
    /// Allocates a record constructor descriptor for `rtd`. `prcd` is the
    /// parent constructor descriptor or `#f`, `protocol` the protocol
    /// procedure or `#f`.
    pub fn new(
        ctx: MutationContext<'gc>,
        rtd: Gc<'gc, Rtd<'gc>>,
        prcd: Value<'gc>,
        protocol: Value<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                rtd,
                prcd,
                protocol,
            },
            rcd_header_word(),
        )
    }
}

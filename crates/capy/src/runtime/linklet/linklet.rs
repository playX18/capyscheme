use super::*;

/// A linklet: compiled body plus its import and export signature.
///
/// Immutable after construction. The layout is read by the Scheme accessors,
/// so keep the field order in sync with [`linklet_ops`].
#[repr(C)]
pub struct Linklet<'gc> {
    /// The linklet's name, or `#f`.
    pub name: Value<'gc>,
    /// List of `(key sym ...)` groups, `key` being a library-name list.
    pub imports: Value<'gc>,
    /// List of exported symbols.
    pub exports: Value<'gc>,
    /// Procedure taking one variable per import symbol, then one per export symbol.
    pub body: Value<'gc>,
    /// Arbitrary metadata attached to the linklet, or `#f`.
    pub meta: Value<'gc>,
}

fn linklet_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::LINKLET).expect("builtin class id is nonzero"),
    )
}
unsafe impl<'gc> Trace for Linklet<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.name.trace(visitor);
            self.imports.trace(visitor);
            self.exports.trace(visitor);
            self.body.trace(visitor);
            self.meta.trace(visitor);
        }
    }

    // SAFETY: Linklets hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for Linklet<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::LINKLET];
    const TYPE_NAME: &'static str = "linklet";
}

impl<'gc> Linklet<'gc> {
    /// Allocates a linklet with the given signature and body.
    pub fn new(
        ctx: MutationContext<'gc>,
        name: Value<'gc>,
        imports: Value<'gc>,
        exports: Value<'gc>,
        body: Value<'gc>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                name,
                imports,
                exports,
                body,
                meta,
            },
            linklet_header_word(),
        )
    }
}


use super::*;

/// An ordered collection of linklets, the top-level result of a file that defines several.
///
/// Immutable after construction.
#[repr(C)]
pub struct LinkletDirectory<'gc> {
    /// Vector of [`Linklet`]s, in the order they run.
    pub linklets: Gc<'gc, Vector<'gc>>,
}

fn directory_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::LINKLET_DIRECTORY).expect("builtin class id is nonzero"),
    )
}

unsafe impl<'gc> Trace for LinkletDirectory<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`, the caller
    // guarantees the visitor outlives this call.
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        unsafe {
            self.linklets.trace(visitor);
        }
    }

    // SAFETY: Directories hold no weak references, so there is nothing to process.
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl<'gc> ClassTagged for LinkletDirectory<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::LINKLET_DIRECTORY];
    const TYPE_NAME: &'static str = "linklet-directory";
}

impl<'gc> LinkletDirectory<'gc> {
    /// Allocates a directory over `linklets`.
    pub fn new(ctx: MutationContext<'gc>, linklets: &[Value<'gc>]) -> Gc<'gc, Self> {
        ctx.allocate_with(
            Self {
                linklets: Vector::from_slice(ctx, linklets),
            },
            directory_header_word(),
        )
    }
}

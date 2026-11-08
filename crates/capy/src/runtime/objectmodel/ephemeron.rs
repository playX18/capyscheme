use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Visitor, WeakProcessor};
use crate::prelude::*;
#[repr(C)]
pub struct Ephemeron<'gc> {
    pub(crate) key: Value<'gc>,
    pub(crate) value: Value<'gc>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;

    #[test]
    fn ephemeron_allocates_with_class_only_header() {
        Scheme::new_uninit().enter(|ctx| {
            let ephemeron = ctx.allocate_with(
                Ephemeron {
                    key: Value::null(),
                    value: Value::undefined(),
                },
                ephemeron_header_word(),
            );

            assert_eq!(
                ephemeron.as_gc_object().header().class_id(),
                ClassId::new(builtin_class_ids::EPHEMERON).unwrap()
            );
        });
    }
}

pub(crate) fn ephemeron_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::EPHEMERON).expect("builtin class id is nonzero"),
    )
}

// SAFETY: `gc` for `Ephemeron` upholds all trait invariants
unsafe impl<'gc> Trace for Ephemeron<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.register_for_weak_processing();
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        // SAFETY: The pointer is a valid GC object descriptor from the current heap
        unsafe {
            if self.key.is_cell() {
                let obj = self.key.as_cell_raw();
                let new_ptr = weak_processor.is_live_object(obj);

                // key is not alive anymore: break ephemeron pair
                if new_ptr.is_null() {
                    self.key = Value::new(false);
                    self.value = Value::new(false);
                } else {
                    // key is alive: update key and trace the value
                    self.key = Value {
                        desc: EncodedValueDescriptor {
                            ptr: new_ptr.to_address().to_mut_ptr(),
                        },
                        pd: Default::default(),
                    };
                    weak_processor.visitor().trace(&mut self.value);
                }
            } else {
                // key is not a cell: impossible
                unreachable!("Ephemeron key is not a cell");
            }
        }
    }
}

// SAFETY: `gc` for `Ephemeron` upholds all trait invariants
unsafe impl<'gc> ClassTagged for Ephemeron<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::EPHEMERON];
    const TYPE_NAME: &'static str = "#<ephemeron>";
}


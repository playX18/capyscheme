use crate::prelude::*;

#[repr(C)]
pub struct Ephemeron<'gc> {
    hdr: ScmHeader,
    key: Value<'gc>,
    value: Value<'gc>,
}

unsafe impl<'gc> Trace for Ephemeron<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            if self.key.is_cell() {
                let obj = self.key.as_cell_raw();
                let new_ptr = weak_processor.is_live_object(obj);

                // key is not alive anymore: break ephemeron pair
                if new_ptr.is_null() {
                    self.key = Value::new(false);
                    self.value = Value::new(false);
                    return;
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

unsafe impl<'gc> Tagged for Ephemeron<'gc> {
    const TC8: TypeCode8 = TypeCode8::EPHEMERON;
    const TYPE_NAME: &'static str = "#<ephemeron>";
}

#[scheme(path=capy)]
pub mod gc {
    #[scheme(name = "ephemeron?")]
    pub fn is_ephemeron(value: Value<'gc>) -> bool {
        nctx.return_(value.is::<Ephemeron>())
    }

    /// Creates a new SRFI-124 Ephemeron pair. If `key` is not a heap-allocated object,
    /// error is raised. If `(eq? key value)` is #t then the ephemeron pair is effectively
    /// a weak reference.
    ///
    /// On GC cycle, if `key` is not reachable from root set, both `key` and `value`
    /// are set to `#f`. Otherwise `value` is traced normally.
    ///
    /// NOTE: value does not keep key alive if it has references to it.
    #[scheme(name = "make-ephemeron")]
    pub fn make_ephemeron(key: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        if !key.is_cell() {
            return nctx.raise_error(
                "make-ephemeron",
                "ephemeron key must be a heap-allocated object",
                &[key],
            );
        }

        let ephemeron = Ephemeron {
            key,
            value,
            hdr: ScmHeader::with_type_bits(TypeCode8::EPHEMERON.bits() as _),
        };

        let cell = Gc::new(*ctx, ephemeron);

        nctx.return_(cell.into())
    }

    /// Returns key of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-key")]
    pub fn ephemeron_key(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.key.is_cell() {
            return nctx.return_(Value::new(false));
        }
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.key.as_cell_raw());
        }
        nctx.return_(ephemeron.key)
    }

    /// Returns value of the ephemeron. If the key is not reachable, returns #f.
    #[scheme(name = "ephemeron-datum")]
    pub fn ephemeron_datum(ephemeron: Gc<'gc, Ephemeron<'gc>>) -> Value<'gc> {
        if !ephemeron.value.is_cell() {
            // return value, not #f because value can be anything
            return nctx.return_(ephemeron.value);
        }
        // SAFETY: `value` is a cell and we need to perform weak reference load.
        unsafe {
            ctx.raw_weak_reference_load(ephemeron.value.as_cell_raw());
        }

        nctx.return_(ephemeron.value)
    }

    /// A no-op function that is required by SRFI-124. We can't reliably
    /// provide reference barrier as a native procedure. GC can only happen in
    /// between function calls or continuation jumps, so this function does nothing.
    #[scheme(name = "reference-barrier")]
    pub fn reference_barrier(_x: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::undefined())
    }
}

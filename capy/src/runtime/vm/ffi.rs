use crate::{global, native_fn, runtime::prelude::*};
use rsgc::{Trace, mmtk::util::Address};

/// A wrapped C pointer. This allows us to protect
/// from accidental UBs, but we won't take away
/// the ability to do unsafe things.
#[repr(C)]
pub struct Pointer {
    header: ScmHeader,
    value: Address,
}

global!(
    PTRS<'gc>: Gc<'gc, WeakTable<'gc>> = (ctx) WeakTable::new(&ctx, 8, 0.75);
);

unsafe impl Trace for Pointer {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl Pointer {
    pub fn new(value: *mut std::ffi::c_void) -> Self {
        Self {
            header: ScmHeader::with_type_bits(TypeCode8::POINTER.bits() as _),
            value: Address::from_ptr(value as *mut u8),
        }
    }

    pub fn value(&self) -> *mut std::ffi::c_void {
        self.value.to_mut_ptr()
    }
}

unsafe impl Tagged for Pointer {
    const TC8: TypeCode8 = TypeCode8::POINTER;
    const TYPE_NAME: &'static str = "pointer";
}

native_fn!(
    register_ffi_fns:

    pub ("pointer?") fn pointerp<'gc>(nctx, x: Value<'gc>) -> bool {
        nctx.return_(x.is::<Pointer>())
    }

    pub ("make-pointer") fn make_pointer<'gc>(nctx, addr: usize) -> Value<'gc> {
        let ptr = Pointer::new(addr as *mut std::ffi::c_void);
        let ptr = Gc::new(&nctx.ctx, ptr);
        nctx.return_(ptr.into())
    }

    pub ("pointer-address") fn pointer_address<'gc>(nctx, p: Gc<'gc, Pointer>) -> usize {
        nctx.return_(p.value() as usize)
    }

    /// unsafely convert a pointer to a Scheme value.
    pub ("pointer->scm") fn pointer_to_value<'gc>(nctx, p: Gc<'gc, Pointer>) -> Value<'gc> {
        nctx.return_(Value::from_raw(p.value() as u64))
    }

    pub ("scm->pointer") fn value_to_pointer<'gc>(nctx, v: Value<'gc>) -> Gc<'gc, Pointer> {
        let p = Pointer::new(v.bits() as *mut std::ffi::c_void);
        let p = Gc::new(&nctx.ctx, p);
        nctx.return_(p)
    }

    pub ("pointer->bytevector") fn pointer_to_bytevector<'gc>(
        nctx,
        pointer: Gc<'gc, Pointer>,
        length: usize,
        offset: Option<usize>
    ) -> Gc<'gc, ByteVector> {
        let value = pointer.value();
        if value.is_null() {
            return nctx.wrong_argument_violation(
                "pointer->bytevector",
                "null pointer",
                Some(pointer.into()),
                None,
                1,
                &[pointer.into()]
            );
        }

        let offset = offset.unwrap_or(0);
        let ptr = pointer.value + offset;
        let bvec = ByteVector::new_mapping(&nctx.ctx, ptr, length);
        nctx.return_(bvec)
    }

    pub ("bytevector->pointer") fn bytevector_to_pointer<'gc>(
        nctx,
        bv: Gc<'gc, ByteVector>,
        offset: Option<usize>
    ) -> Gc<'gc, Pointer> {
        let offset = offset.unwrap_or(0);
        if offset >= bv.len() {
            return nctx.wrong_argument_violation(
                "bytevector->pointer",
                "offset out of bounds",
                Some(bv.into()),
                None,
                1,
                &[bv.into()]
            );
        }

        let ptr = bv.contents() + offset;
        let p = Pointer::new(ptr.to_mut_ptr());
        let p = Gc::new(&nctx.ctx, p);
        // TODO(Adel): pin bytevector in memory OR move contents to non-moving space.
        // create ephemeron to keep the bytevector alive if `p` is alive.
        ptrs(nctx.ctx).put(nctx.ctx, p, bv);

        nctx.return_(p)
    }
);

pub(crate) fn init_ffi<'gc>(ctx: Context<'gc>) {
    register_ffi_fns(ctx);
}

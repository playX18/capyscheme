use rsgc::{
    alloc::array::{Array, ArrayRef},
    collection::Visitor,
};

use crate::runtime::Context;

use super::*;
pub enum Return<'gc> {
    Ok(Value<'gc>),
    Err(Value<'gc>),
}

#[repr(C)]
pub struct NativeProc<'gc> {
    pub header: ScmHeader,
    pub closure: Option<ArrayRef<'gc, Value<'gc>>>,
    pub proc: fn(Context<'gc>, &[Value<'gc>], Option<ArrayRef<'gc, Value<'gc>>>) -> Return<'gc>,
}

impl<'gc> NativeProc<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        closure: Option<impl AsRef<[Value<'gc>]>>,
        proc: fn(Context<'gc>, &[Value<'gc>], Option<ArrayRef<'gc, Value<'gc>>>) -> Return<'gc>,
    ) -> Gc<'gc, Self> {
        let closure = closure.map(|c| Array::from_array(&ctx, c.as_ref()));

        Gc::new(
            &ctx,
            NativeProc {
                header: ScmHeader::with_type_bits(TypeCode8::NATIVE_PROCEDURE.bits() as _),
                closure,
                proc,
            },
        )
    }
}

unsafe impl<'gc> Tagged for NativeProc<'gc> {
    const TC8: TypeCode8 = TypeCode8::NATIVE_PROCEDURE;
}

unsafe impl<'gc> Trace for NativeProc<'gc> {
    unsafe fn trace(&mut self, vis: &mut Visitor) {
        if let Some(closure) = &mut self.closure {
            unsafe {
                closure.trace(vis);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        if let Some(closure) = &mut self.closure {
            unsafe {
                closure.process_weak_refs(weak_processor);
            }
        }
    }
}

#[repr(C)]
pub struct Closure<'gc> {
    pub header: ScmHeader,
    pub code: Address,
    pub free: Value<'gc>,
}

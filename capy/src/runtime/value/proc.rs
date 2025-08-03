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
    pub meta: Value<'gc>,
}

unsafe impl<'gc> Tagged for Closure<'gc> {
    const TC8: TypeCode8 = TypeCode8::CLOSURE;

    const TC16: &'static [TypeCode16] = &[TypeCode16::CLOSURE_PROC, TypeCode16::CLOSURE_K];
}

unsafe impl<'gc> Trace for Closure<'gc> {
    unsafe fn trace(&mut self, vis: &mut Visitor) {
        vis.trace(&mut self.free);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Closure<'gc> {
    pub fn is_continuation(&self) -> bool {
        self.header.type_bits() == TypeCode16::CLOSURE_K.bits()
    }
}

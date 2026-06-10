use std::marker::PhantomData;

use crate::rsgc::{Visitor, WeakProcessor};

#[repr(C)]
pub struct NativeObject<'gc> {
    pd: PhantomData<&'gc ()>,
    vtab: &'static NativeVTable,
}

pub struct NativeVTable {
    // SAFETY: Invariants are upheld at this call site
    pub finalize: Option<unsafe extern "C" fn(*mut ())>,
    // SAFETY: Invariants are upheld at this call site
    pub trace: Option<unsafe extern "C" fn(*mut (), &mut Visitor<'_>)>,
    // SAFETY: Invariants are upheld at this call site
    pub process_weaks: Option<unsafe extern "C" fn(*mut (), &mut WeakProcessor<'_>)>,
}

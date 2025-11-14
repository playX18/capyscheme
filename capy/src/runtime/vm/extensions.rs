use std::marker::PhantomData;

use crate::rsgc::{Visitor, WeakProcessor};

use crate::prelude::ScmHeader;

#[repr(C)]
pub struct NativeObject<'gc> {
    pd: PhantomData<&'gc ()>,
    header: ScmHeader,
    vtab: &'static NativeVTable,
}

pub struct NativeVTable {
    pub finalize: Option<unsafe extern "C" fn(*mut ())>,
    pub trace: Option<unsafe extern "C" fn(*mut (), &mut Visitor<'_>)>,
    pub process_weaks: Option<unsafe extern "C" fn(*mut (), &mut WeakProcessor<'_>)>,
}

//! setjmp/longjmp via a tiny C shim so returns-twice stays out of Rust.

use std::ffi::c_void;
use std::mem::MaybeUninit;

/// Jump buffer matching [`capy_jmp_buf`] in `setjmp_shim.c`.
#[repr(C, align(16))]
pub struct JmpBuf {
    _data: [u8; 256],
}

impl JmpBuf {
    pub const fn uninit() -> MaybeUninit<Self> {
        MaybeUninit::uninit()
    }
}

pub type SchemeEntryFn = unsafe extern "C" fn(*mut c_void);

unsafe extern "C" {
    /// Run `entry(data)` under `setjmp(*env)`; returns after a matching `capy_longjmp`.
    pub fn capy_with_setjmp(
        env: *mut JmpBuf,
        entry: SchemeEntryFn,
        data: *mut c_void,
    ) -> i32;

    /// Restore the environment saved by `capy_with_setjmp` and never return.
    pub fn capy_longjmp(env: *mut JmpBuf, val: i32) -> !;
}

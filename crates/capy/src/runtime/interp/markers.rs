//! Machine return-address markers for the Rust engine.

use std::sync::LazyLock;
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

use crate::runtime::sabi::{self, MV_DISP, Ra, RpHeader};

/// `subr_ret`'s header: the frame below is a native continuation's, return
/// address and closure, with the closure live.
pub const SUBR_RET_FRAME_SIZE: isize = 2 * 8;
pub const SUBR_RET_LIVEMASK: usize = sabi::livemask_fixnum(0b1);

/// One marker: a header followed by the return address, which points at the
/// pad word and is never executed.
#[repr(C, align(8))]
struct MarkerSlot {
    header: RpHeader,
    _pad: u64,
}

const _: () = {
    assert!(size_of::<MarkerSlot>() == MV_DISP + 8);
    assert!(std::mem::offset_of!(MarkerSlot, _pad) == MV_DISP);
};

/// Plain marker addresses: the statics behind the table, and the shape the generated image
/// installs when the generated image exists.
pub struct MarkerValues {
    pub interpreter_return: usize,
    pub scheme_exit: usize,
    pub underflow: usize,
    pub subr_ret: usize,
    pub raise_ret: usize,
}

fn slot(livemask: usize, frame_size: isize) -> &'static mut MarkerSlot {
    Box::leak(Box::new(MarkerSlot {
        header: RpHeader::new(0, livemask, 0, frame_size),
        _pad: 0,
    }))
}

fn slot_ra(slot: &MarkerSlot) -> Ra {
    std::ptr::addr_of!(slot._pad) as Ra
}

static DEFAULTS: LazyLock<MarkerValues> = LazyLock::new(|| {
    let interpreter_return = slot(0, sabi::INTERPRETER_FRAME_SIZE);
    let scheme_exit = slot(0, 0);
    let underflow = slot(0, 0);
    let subr_ret = slot(SUBR_RET_LIVEMASK, SUBR_RET_FRAME_SIZE);
    let raise_ret = slot(0, 0);

    for stub in [
        &mut *interpreter_return,
        &mut *scheme_exit,
        &mut *underflow,
        &mut *subr_ret,
        &mut *raise_ret,
    ] {
        stub.header.mv_return_address = slot_ra(stub) as usize;
    }
    MarkerValues {
        interpreter_return: slot_ra(interpreter_return) as usize,
        scheme_exit: slot_ra(scheme_exit) as usize,
        underflow: slot_ra(underflow) as usize,
        subr_ret: slot_ra(subr_ret) as usize,
        raise_ret: slot_ra(raise_ret) as usize,
    }
});

pub struct MarkerTable {
    pub interpreter_return: AtomicUsize,
    pub scheme_exit: AtomicUsize,
    pub underflow: AtomicUsize,
    pub subr_ret: AtomicUsize,
    pub raise_ret: AtomicUsize,
}

static TABLE: LazyLock<MarkerTable> = LazyLock::new(|| {
    let defaults = &*DEFAULTS;
    MarkerTable {
        interpreter_return: AtomicUsize::new(defaults.interpreter_return),
        scheme_exit: AtomicUsize::new(defaults.scheme_exit),
        underflow: AtomicUsize::new(defaults.underflow),
        subr_ret: AtomicUsize::new(defaults.subr_ret),
        raise_ret: AtomicUsize::new(defaults.raise_ret),
    }
});

pub fn markers() -> &'static MarkerTable {
    &TABLE
}

pub fn set_markers(values: &MarkerValues) {
    let table = markers();
    table
        .interpreter_return
        .store(values.interpreter_return, Relaxed);
    table.scheme_exit.store(values.scheme_exit, Relaxed);
    table.underflow.store(values.underflow, Relaxed);
    table.subr_ret.store(values.subr_ret, Relaxed);
    table.raise_ret.store(values.raise_ret, Relaxed);
}

#[inline(always)]
pub fn interpreter_return() -> Ra {
    markers().interpreter_return.load(Relaxed) as Ra
}

#[inline(always)]
pub fn scheme_exit() -> Ra {
    markers().scheme_exit.load(Relaxed) as Ra
}

#[inline(always)]
pub fn underflow() -> Ra {
    markers().underflow.load(Relaxed) as Ra
}

#[inline(always)]
pub fn subr_ret() -> Ra {
    markers().subr_ret.load(Relaxed) as Ra
}

#[inline(always)]
pub fn raise_ret() -> Ra {
    markers().raise_ret.load(Relaxed) as Ra
}

/// Where a return of any count but one goes:
/// for a compact header the return address itself, or `None` for the
/// `values_error` stub, for a full one its mv return address.
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack.
#[inline(always)]
pub unsafe fn mv_target(ra: Ra) -> Option<Ra> {
    unsafe {
        if sabi::is_entry_compact(ra) {
            // compact: use regular return or error?
            let mode =
                *(ra.offset(sabi::COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP) as *const isize);
            if mode & sabi::COMPACT_HEADER_VALUES_ERROR_MASK != 0 {
                None
            } else {
                Some(ra)
            }
        } else {
            // non-compact rp-header
            Some(sabi::rp_header(ra).mv_return_address as Ra)
        }
    }
}

//! Machine-level Scheme ABI (SABI) constants.
//!
//! The `TC` register points at the [`ThreadNativeData`](crate::heap::sync::thread::ThreadNativeData),
//! so every `TC_*` offset is from the thread base.

use core::mem::{align_of, offset_of, size_of};

use crate::heap::object::{MAX_CLASS_ID, OBJECT_HEADER_OFFSET};
use crate::heap::sync::thread::ThreadNativeData;
use crate::runtime::interp::stack::StackState;
use crate::runtime::thread::Tc;
use crate::runtime::value::{
    ByteVector, Closure, CodeBlock, EnvRecord, NativeProc, Pair, Str, Tuple, Value, Vector,
};

pub use crate::heap::object::builtin_class_ids;

/// Native data base to the thread context.
pub const TC_OFFSET: usize = ThreadNativeData::TC_OFFSET;
/// `AP`/`EAP` are the LAB cursor and limit.
pub const TC_AP: usize = ThreadNativeData::LAB_OFFSET_CURSOR;
pub const TC_EAP: usize = ThreadNativeData::LAB_OFFSET_LIMIT;

/// Native data base to `Tc.stack` fields.
pub const TC_STACK_FP: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, fp);
pub const TC_STACK_SP: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, sp);
pub const TC_SCHEME_STACK: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, scheme_stack);
pub const TC_SCHEME_STACK_SIZE: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, scheme_stack_size);
pub const TC_STACK_SEGMENT: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, segment);
pub const TC_STACK_LINK: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, stack_link);
pub const TC_STACK_TOP_IP: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, top_ip);
pub const TC_STACK_ACTIVATION: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, activation);
pub const TC_STACK_CALL_RATOR: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, call_rator);
pub const TC_STACK_CALL_K: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, call_k);
pub const TC_STACK_CALL_ARGC: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, call_argc);
pub const TC_STACK_CALL_ARGS: usize =
    TC_OFFSET + offset_of!(Tc<'static>, stack) + offset_of!(StackState<'static>, call_args);

/// Native data base to the yieldpoint flag: `0` idle, `> 0` taken at
/// prologues and loop back-edges, `< 0` at prologues only.
pub const TC_YIELDPOINT: usize = ThreadNativeData::TAKE_YIELDPOINT_OFFSET;

/// Native data base to the remaining `Tc` fields.
pub const TC_ESP: usize = TC_OFFSET + offset_of!(Tc<'static>, esp);
pub const TC_REAL_ESP: usize = TC_OFFSET + offset_of!(Tc<'static>, real_esp);
pub const TC_AC: usize = TC_OFFSET + offset_of!(Tc<'static>, ac);
pub const TC_RV: usize = TC_OFFSET + offset_of!(Tc<'static>, rv);
pub const TC_CP: usize = TC_OFFSET + offset_of!(Tc<'static>, cp);
pub const TC_IP: usize = TC_OFFSET + offset_of!(Tc<'static>, ip);
pub const TC_EXIT_VALUE: usize = TC_OFFSET + offset_of!(Tc<'static>, exit_value);
pub const TC_STUBS: usize = TC_OFFSET + offset_of!(Tc<'static>, stubs);

/// Object layout offsets for templates.
pub const CLOSURE_CODE: usize = offset_of!(Closure<'static>, code);
pub const CLOSURE_FREE: usize = offset_of!(Closure<'static>, free);
pub const NATIVE_PROC_ADDRESS: usize = offset_of!(NativeProc, proc);
pub const PAIR_CAR: usize = offset_of!(Pair<'static>, car);
pub const PAIR_CDR: usize = offset_of!(Pair<'static>, cdr);
pub const VECTOR_LENGTH: usize = offset_of!(Vector<'static>, length);
pub const VECTOR_DATA: usize = Vector::OFFSET_OF_DATA;
pub const TUPLE_LENGTH: usize = offset_of!(Tuple<'static>, length);
pub const TUPLE_DATA: usize = offset_of!(Tuple<'static>, data);
pub const STRING_LENGTH: usize = offset_of!(Str<'static>, length);
pub const BYTEVECTOR_LENGTH: usize = offset_of!(ByteVector, len);
pub const BYTEVECTOR_CONTENTS: usize = offset_of!(ByteVector, contents);
pub const VECTOR_SIZE: usize = size_of::<Vector<'static>>();
pub const ENV_DATA: usize = EnvRecord::DATA_OFFSET as usize;
pub const ENV_LENGTH: usize = offset_of!(EnvRecord<'static>, length);
pub const ENV_SIZE: usize = size_of::<EnvRecord<'static>>();
pub const PAIR_SIZE: usize = size_of::<Pair<'static>>();
pub const CLOSURE_CODE_BLOCK: usize = offset_of!(Closure<'static>, code_block);
pub const CLOSURE_META: usize = offset_of!(Closure<'static>, meta);
pub const CLOSURE_NFREE: usize = offset_of!(Closure<'static>, nfree);
pub const CLOSURE_SIZE: usize = size_of::<Closure<'static>>();
pub const CODE_BLOCK_ENTRYPOINT: usize = offset_of!(CodeBlock<'static>, entrypoint);
pub const CODE_BLOCK_METADATA: usize = offset_of!(CodeBlock<'static>, metadata);
pub const CODE_BLOCK_JIT_ENTRY: usize = offset_of!(CodeBlock<'static>, jit_entry);
pub const CODE_BLOCK_HOT_COUNT: usize = offset_of!(CodeBlock<'static>, hot_count);
/// Bytes of the object header before an object reference.
pub const OBJECT_HEADER_BYTES: usize = 8;
/// Heap header word: this far before the object pointer.
pub const HEADER_OFFSET: isize = OBJECT_HEADER_OFFSET;
/// Low bits of the header word holding the class id.
pub const CLASS_ID_MASK: u64 = MAX_CLASS_ID as u64;

/// NaN-boxing tags, re-exported from the value encoding.
#[cfg(target_pointer_width = "64")]
pub const NUMBER_TAG: i64 = Value::NUMBER_TAG;
#[cfg(target_pointer_width = "64")]
pub const NOT_CELL_MASK: i64 = Value::NOT_CELL_MASK;

/// A machine return address: the address just after an [`RpHeader`].
pub type Ra = *const u8;

/// The full return-point header, immediately
/// before every machine return address that is not compact (`ra` is
/// 8-aligned). The word before `ra` tells the two apart: `frame_size` here
/// has its low bit 0, a [`RpCompactHeader`]'s `mask_size_mode` has it 1.
#[repr(C)]
pub struct RpHeader {
    /// Where a return of any count but one goes: an mv entry, the
    /// `values_error` stub, or `ra` itself.
    pub mv_return_address: usize,
    /// Live slots of the frame returned into, bit `i` for `fp[i + 1]`. A mask
    /// that fits is stored shifted left by one with the low bit set
    /// ([`livemask_fixnum`]), a larger one as a pointer to `[nwords:
    /// u64][words: u64...]`.
    pub livemask: usize,
    /// Bytes from this field back to the start of the code that owns `ra`
    /// (`code-top-link`): the walker keeps that code alive through it.
    pub toplink: usize,
    /// Size in bytes of the frame returned into, low bit 0.
    /// [`INTERPRETER_FRAME_SIZE`] for the interpreter glue, whose frame size and
    /// mask are those of the bytecode return point in `fp[-1]`.
    pub frame_size: isize,
}

/// The compact return-point header: `toplink` at the same distance
/// from `ra` as in [`RpHeader`], then frame size, live mask and mode in one
/// word. There is no mv entry: a return of any count but one goes to `ra`,
/// or to `values_error` when [`COMPACT_HEADER_VALUES_ERROR_MASK`] is set.
#[repr(C)]
pub struct RpCompactHeader {
    pub toplink: usize,
    pub mask_size_mode: isize,
}

pub const SIZE_RP_HEADER: usize = size_of::<RpHeader>();
pub const SIZE_RP_COMPACT_HEADER: usize = size_of::<RpCompactHeader>();
pub const RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP: isize = -(SIZE_RP_HEADER as isize);
pub const RETURN_ADDRESS_LIVEMASK_DISP: isize = RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP + 8;
pub const RETURN_ADDRESS_TOPLINK_DISP: isize = RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP + 16;
pub const RETURN_ADDRESS_FRAME_SIZE_DISP: isize = RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP + 24;
pub const COMPACT_RETURN_ADDRESS_TOPLINK_DISP: isize = -(SIZE_RP_COMPACT_HEADER as isize);
pub const COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP: isize =
    COMPACT_RETURN_ADDRESS_TOPLINK_DISP + 8;

/// `mask+size+mode`: bit 0 is 1.
pub const COMPACT_HEADER_MASK: isize = 0b01;
/// `mask+size+mode`: bit 1 is 1 when the mv return address is
/// `values_error`, 0 when it is the return address.
pub const COMPACT_HEADER_VALUES_ERROR_MASK: isize = 0b10;
/// `mask+size+mode`: bits 2 through 1 + `COMPACT_FRAME_WORDS_BITS` are the
/// frame size in words, the remaining bits are the live mask.
pub const COMPACT_FRAME_WORDS_OFFSET: u32 = 2;
pub const COMPACT_FRAME_WORDS_BITS: u32 = 5;
pub const COMPACT_FRAME_MAX_WORDS: usize = (1 << COMPACT_FRAME_WORDS_BITS) - 1;
pub const COMPACT_FRAME_WORDS_MASK: usize = COMPACT_FRAME_MAX_WORDS;
pub const COMPACT_FRAME_MASK_OFFSET: u32 = 2 + COMPACT_FRAME_WORDS_BITS;

/// `frame_size` of the interpreter glue's header (capy): the frame returned
/// into is interpreted, and its size and mask are those of the bytecode
/// return point in `fp[-1]`.
pub const INTERPRETER_FRAME_SIZE: isize = -8;

/// A live mask of up to 63 slots in the inline form of
/// [`RpHeader::livemask`] (the fixnum form).
pub const fn livemask_fixnum(bits: usize) -> usize {
    (bits << 1) | 1
}

impl RpHeader {
    pub const fn new(
        mv_return_address: usize,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) -> Self {
        Self {
            mv_return_address,
            livemask,
            toplink,
            frame_size,
        }
    }
}

impl RpCompactHeader {
    /// Builds the compact header. `frame_words` is at most
    /// [`COMPACT_FRAME_MAX_WORDS`] and `livemask` fits in the rest of the
    /// word.
    pub const fn new(
        toplink: usize,
        values_error: bool,
        frame_words: usize,
        livemask: usize,
    ) -> Self {
        assert!(frame_words <= COMPACT_FRAME_MAX_WORDS);
        assert!(livemask < 1 << (63 - COMPACT_FRAME_MASK_OFFSET));
        let error = if values_error {
            COMPACT_HEADER_VALUES_ERROR_MASK
        } else {
            0
        };
        Self {
            toplink,
            mask_size_mode: COMPACT_HEADER_MASK
                | error
                | ((frame_words as isize) << COMPACT_FRAME_WORDS_OFFSET)
                | ((livemask as isize) << COMPACT_FRAME_MASK_OFFSET),
        }
    }
}

/// The full header before the machine return address `ra`.
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack whose header is not compact
/// (`!is_entry_compact(ra)`).
#[inline(always)]
pub unsafe fn rp_header(ra: Ra) -> &'static RpHeader {
    unsafe { &*(ra.offset(RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP) as *const RpHeader) }
}

/// `RPCOMPACTHEADERMASKANDSIZE`: the word before `ra`, either header's.
#[inline(always)]
unsafe fn mask_size_mode(ra: Ra) -> isize {
    unsafe { *(ra.offset(COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP) as *const isize) }
}

/// Is the return header compact?
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack.
#[inline(always)]
pub unsafe fn is_entry_compact(ra: Ra) -> bool {
    unsafe { mask_size_mode(ra) & COMPACT_HEADER_MASK != 0 }
}

/// Fetch compact field of entry
#[inline(always)]
unsafe fn compact_entry_field(ra: Ra, offset: u32) -> usize {
    unsafe { (mask_size_mode(ra) as usize) >> offset }
}

/// The size in bytes of the frame `ra`
/// returns into. [`INTERPRETER_FRAME_SIZE`] for the interpreter glue.
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack.
#[inline(always)]
pub unsafe fn entry_frame_size(ra: Ra) -> isize {
    unsafe {
        if is_entry_compact(ra) {
            ((compact_entry_field(ra, COMPACT_FRAME_WORDS_OFFSET) & COMPACT_FRAME_WORDS_MASK) << 3)
                as isize
        } else {
            rp_header(ra).frame_size
        }
    }
}

/// livemask in the form of [`RpHeader::livemask`].
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack.
#[inline(always)]
pub unsafe fn entry_livemask(ra: Ra) -> usize {
    unsafe {
        if is_entry_compact(ra) {
            livemask_fixnum(compact_entry_field(ra, COMPACT_FRAME_MASK_OFFSET))
        } else {
            rp_header(ra).livemask
        }
    }
}

/// `toplink`, at the same place in both
/// headers.
///
/// # Safety
///
/// `ra` is a return address on the Scheme stack.
#[inline(always)]
pub unsafe fn entry_offset(ra: Ra) -> usize {
    unsafe { *(ra.offset(COMPACT_RETURN_ADDRESS_TOPLINK_DISP) as *const usize) }
}

/// `ra - MV_DISP` is the mv return address of a full header.
pub const MV_DISP: usize = SIZE_RP_HEADER;

/// The `scheme_entry`-style prologue (`Masm::scheme_entry_prologue`) subtracts
/// this many bytes from `sp` and saves the SABI callee-saved registers.
///
/// x86-64 (entry `sp` is 16n+8, so the frame keeps `sp` aligned):
/// ```text
/// [sp+ 0] rbx (numberTag)   [sp+ 8] r12 (CONSTANT_POINTER)         [sp+16] r13 (MD)
/// [sp+24] r14 (TC)          [sp+32] r15 (notCellMask) [sp+40] pad
/// [sp+48] saved rbp (caller's) <- native fp         [sp+56] return address
/// ```
/// AArch64 (entry `sp` is 16-aligned, `x29` is never touched):
/// ```text
/// [sp+ 0] x19 (TC)  [sp+ 8] x20 (SFP) [sp+16] x21 (AP)    [sp+24] x22 (DISPATCH)
/// [sp+32] x23 (PC)  [sp+40] x24 (CONSTANT_POINTER)  [sp+48] x25 (MD)    [sp+56] x26 (numberTag)
/// [sp+64] x27 (notCellMask) [sp+72] x28 (free)            [sp+80] lr  [sp+88] pad
/// ```
/// `x22` holds the template interpreter's opcode-map base (`regs::DISPATCH`), set once per
/// activation in `scheme_entry`, the save/restore makes nested activations
/// see their own image's map.
#[cfg(target_arch = "x86_64")]
pub const SCHEME_ENTRY_FRAME_BYTES: usize = 56;
#[cfg(target_arch = "aarch64")]
pub const SCHEME_ENTRY_FRAME_BYTES: usize = 96;
#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const SCHEME_ENTRY_FRAME_BYTES: usize = 0;
/// `CFA = sp + SCHEME_ENTRY_CFA_OFFSET` inside the frame above: the caller's
/// `sp` at the call. The return address is at `[CFA-8]` (x86-64, pushed by
/// `call`) or in `lr`, saved at `[CFA-16]` (AArch64).
#[cfg(target_arch = "x86_64")]
pub const SCHEME_ENTRY_CFA_OFFSET: usize = 64;
#[cfg(target_arch = "aarch64")]
pub const SCHEME_ENTRY_CFA_OFFSET: usize = 96;
#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
pub const SCHEME_ENTRY_CFA_OFFSET: usize = 0;
/// Native entry frame from `sp` while Scheme runs: `save_scheme_state`'s
/// `lea NATIVE_FP_FROM_SP[sp], rbp` on x86-64 points `rbp` at the saved
/// caller's `rbp` above. Zero elsewhere, on AArch64 `x29` is never written.
#[cfg(target_arch = "x86_64")]
pub const NATIVE_FP_FROM_SP: usize = 48;
#[cfg(not(target_arch = "x86_64"))]
pub const NATIVE_FP_FROM_SP: usize = 0;

const _: () = {
    assert!(SIZE_RP_HEADER == 32);
    assert!(SIZE_RP_COMPACT_HEADER == 16);
    assert!(align_of::<RpHeader>() == 8);
    assert!(offset_of!(RpHeader, mv_return_address) == 0);
    assert!(offset_of!(RpHeader, livemask) == 8);
    assert!(offset_of!(RpHeader, toplink) == 16);
    assert!(offset_of!(RpHeader, frame_size) == 24);
    // `top-link` must be a fixed distance from the end of either header.
    assert!(COMPACT_RETURN_ADDRESS_TOPLINK_DISP == RETURN_ADDRESS_TOPLINK_DISP);
    assert!(COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP == RETURN_ADDRESS_FRAME_SIZE_DISP);
    assert!(INTERPRETER_FRAME_SIZE & COMPACT_HEADER_MASK == 0);
    assert!(CLOSURE_CODE == 0);
    assert!(CLOSURE_FREE == Closure::DATA_OFFSET as usize);
    assert!(NATIVE_PROC_ADDRESS == 0);
    assert!(PAIR_CAR == 0);
    assert!(PAIR_CDR == size_of::<Value<'static>>());
    assert!(VECTOR_LENGTH == 0);
    assert!(VECTOR_DATA == size_of::<usize>());
    assert!(TUPLE_LENGTH == 0);
    assert!(TUPLE_DATA == size_of::<usize>());
    assert!(STRING_LENGTH == 2 * size_of::<usize>());
    assert!(BYTEVECTOR_LENGTH == 0);
    assert!(BYTEVECTOR_CONTENTS == size_of::<usize>());
    assert!(VECTOR_SIZE == VECTOR_DATA);
    assert!(HEADER_OFFSET == -8);
    assert!(OBJECT_HEADER_BYTES as isize == -HEADER_OFFSET as isize);
    assert!(CLOSURE_FREE == CLOSURE_SIZE && ENV_DATA == ENV_SIZE);
    assert!(CLASS_ID_MASK == 0x00ff_ffff);
    assert!(size_of::<Value<'static>>() == 8);
    assert!(TC_OFFSET + size_of::<Tc<'static>>() < i32::MAX as usize);
    assert!(ThreadNativeData::LAB_OFFSET_LIMIT + 8 < i32::MAX as usize);
};

#[cfg(target_arch = "x86_64")]
const _: () = {
    // Entry sp is 16n+8, the frame keeps it aligned. CFA is the caller's sp.
    assert!(SCHEME_ENTRY_FRAME_BYTES % 16 == 8);
    assert!(SCHEME_ENTRY_CFA_OFFSET == SCHEME_ENTRY_FRAME_BYTES + 8);
    assert!(NATIVE_FP_FROM_SP == SCHEME_ENTRY_FRAME_BYTES - 8);
};

#[cfg(target_arch = "aarch64")]
const _: () = {
    assert!(SCHEME_ENTRY_FRAME_BYTES % 16 == 0);
    assert!(SCHEME_ENTRY_CFA_OFFSET == SCHEME_ENTRY_FRAME_BYTES);
};

/// Role to portable register map, per target.
///
/// On x86-64 `AP` is a `Tc` slot ([`TC_AP`]) rather than a register, so
/// only AArch64 defines that role here. `DISPATCH` is likewise AArch64-only:
/// on x86-64 `csr3` is `TC`, so there is no spare callee-saved register for
/// the opcode-map base and dispatch keeps its `lea`.
#[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
pub mod regs {
    #[cfg(target_arch = "x86_64")]
    mod target {
        use onlineasm::regs as r;
        use onlineasm::regs::VReg;

        pub const TC: VReg = r::csr3;
        pub const SFP: VReg = r::cfr;
        pub const CP: VReg = r::t3;
        pub const AC: VReg = r::t2;
        pub const RV: VReg = r::t0;
        pub const PC: VReg = r::t4;
        pub const CONSTANT_POINTER: VReg = r::csr1;
        pub const MD: VReg = r::csr2;
        pub const NUMBER_TAG: VReg = r::csr0;
        pub const NOT_CELL_MASK: VReg = r::csr4;
    }

    #[cfg(target_arch = "aarch64")]
    mod target {
        use onlineasm::regs as r;
        use onlineasm::regs::VReg;

        pub const TC: VReg = r::csr0;
        pub const SFP: VReg = r::csr1;
        pub const AP: VReg = r::csr2;
        pub const CP: VReg = r::t3;
        pub const AC: VReg = r::t2;
        pub const RV: VReg = r::t0;
        pub const PC: VReg = r::csr4;
        pub const CONSTANT_POINTER: VReg = r::csr5;
        pub const MD: VReg = r::csr6;
        pub const NUMBER_TAG: VReg = r::csr7;
        pub const NOT_CELL_MASK: VReg = r::csr8;
        pub const DISPATCH: VReg = r::csr3;
    }

    pub use target::*;
}

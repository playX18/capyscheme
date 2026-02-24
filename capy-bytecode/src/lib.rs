#![allow(unused)]

use std::io::Write;

pub use bytecodegen::{self, *};

#[macro_export]
bytecode! {
    op_enter {},

    op_arity_exact {
        display: "arity.exact",
        arguments: {
            expected: u32
        }
    },

    op_arity_min {
        display: "arity.min",
        arguments: {
            expected: u32
        }
    },

    op_arity_max {
        display: "arity.max",
        arguments: {
            expected: u32
        }
    },

    /// Load argument 0 onto the stack.
    op_ldarg_0 {
        display: "ldarg.0",
    },

    /// Load argument 1 onto the stack.
    op_ldarg_1 {
        display: "ldarg.1",
    },

    /// Load argument 2 onto the stack.
    op_ldarg_2 {
        display: "ldarg.2",
    },

    /// Load argument 3 onto the stack.
    op_ldarg_3 {
        display: "ldarg.3",
    },

    op_ldarg_4 {
        display: "ldarg.4",
    },

    /// Load local variable 0 onto the stack.
    op_ldloc_0 {
        display: "ldloc.0",
    },

    /// Load local variable 1 onto the stack.
    op_ldloc_1 {
        display: "ldloc.1",
    },

    /// Load local variable 2 onto the stack.
    op_ldloc_2 {
        display: "ldloc.2",
    },

    /// Load local variable 3 onto the stack.
    op_ldloc_3 {
        display: "ldloc.3",
    },

    /// Store top of stack into local variable 0.
    op_stloc_0 {
        display: "stloc.0",
    },

    /// Store top of stack into local variable 1.
    op_stloc_1 {
        display: "stloc.1",
    },

    /// Store top of stack into local variable 2.
    op_stloc_2 {
        display: "stloc.2",
    },

    /// Store top of stack into local variable 3.
    op_stloc_3 {
        display: "stloc.3",
    },

    op_ldfree_0 {
        display: "ldfree.0",
    },

    op_ldfree_1 {
        display: "ldfree.1",
    },

    op_ldfree_2 {
        display: "ldfree.2",
    },

    op_ldfree_3 {
        display: "ldfree.3",
    },

    op_stfree_0 {
        display: "stfree.0",
    },

    op_stfree_1 {
        display: "stfree.1",
    },

    op_stfree_2 {
        display: "stfree.2",
    },

    op_stfree_3 {
        display: "stfree.3",
    },

    /// Load argument at given index onto the stack.
    op_ldarg {
        arguments: {
            index: u16
        }
    },

    op_ldfree {
        arguments: {
            index: u32
        }
    },

    op_stfree {
        arguments: {
            index: u32
        }
    },

    /// Load local variable at given index onto the stack.
    op_ldloc {
        arguments: {
            index: u16
        }
    },

    /// Store top of stack into local variable at given index.
    op_stloc {
        arguments: {
            index: u16
        }
    },

    /// Store top of stack into argument at given index.
    op_starg {
        arguments: {
            index: u16
        }
    },

    /// Load [Value::null] onto the stack.
    op_ldnull {
        display: "ld.null",
    },

    /// Load [Value::eof] onto the stack.
    op_ldeof {
        display: "ld.eof",
    },

    /// Load [Value::undefined] onto the stack.
    op_ldundefined {
        display: "ld.undef",
    },

    /// Load [Value::new(true)] onto the stack.
    op_ldtrue {
        display: "ld.true",
    },

    /// Load [Value::new(false)] onto the stack.
    op_ld_false {
        display: "ld.false",
    },

    /// Load constant I32 value -1 onto the stack first boxing it as fixnum.
    op_ldc_i4_m1 {
        display: "ldc.i4.m1",
    },

    /// Load constant I32 value 0 onto the stack first boxing it as fixnum.
    op_ldc_i4_0 {
        display: "ldc.i4.0",
    },

    /// Load constant I32 value 1 onto the stack first boxing it as fixnum.
    op_ldc_i4_1 {
        display: "ldc.i4.1",
    },

    /// Load constant I32 value 2 onto the stack first boxing it as fixnum.
    op_ldc_i4_2 {
        display: "ldc.i4.2",
    },

    /// Load constant I32 value 3 onto the stack first boxing it as fixnum.
    op_ldc_i4_3 {
        display: "ldc.i4.3",
    },

    /// Load constant I32 value 3 onto the stack first boxing it as fixnum.
    op_ldc_i4_4 {
        display: "ldc.i4.4",
    },

    /// Load constant I32 value 5 onto the stack first boxing it as fixnum.
    op_ldc_i4_5 {
        display: "ldc.i4.5",
    },

    /// Load constant I32 value 6 onto the stack first boxing it as fixnum.
    op_ldc_i4_6 {
        display: "ldc.i4.6",
    },

    /// Load constant I32 value 7 onto the stack first boxing it as fixnum.
    op_ldc_i4_7 {
        display: "ldc.i4.7",
    },

    /// Load constant I32 value 8 onto the stack first boxing it as fixnum.
    op_ldc_i4_8 {
        display: "ldc.i4.8",
    }

    /* no need for short form, byecode! macro produces it for us. */
    /// Load constant I32 value onto the stack first boxing it as
    /// Fixnum.
    ///
    /// This opcode comes in various forms for common constants:
    /// - [ldc.i4.m1](OpLdcI4M1)
    /// - [ldc.i4.0](OpLdcI40)
    /// - [ldc.i4.1](OpLdcI41)
    /// - [ldc.i4.2](OpLdcI42)
    /// - [ldc.i4.3](OpLdcI43)
    /// - [ldc.i4.4](OpLdcI44)
    /// - [ldc.i4.5](OpLdcI45)
    /// - [ldc.i4.6](OpLdcI46)
    /// - [ldc.i4.7](OpLdcI47)
    /// - [ldc.i4.8](OpLdcI48)
    op_ldc_i4 {
        display: "ldc.i4",
        arguments: {
            value: i32
        }
    },

    /// Load constant U64 value onto the stack.
    ///
    /// Unlike [ldc.i4](OpLdcI4) this opcode loads raw 64-bit integer
    /// rather than fixnum.
    ldc_i8 {
        arguments: {
            value_lo: u32,
            value_hi: u32
        }
    }

    /// Load constant from constant pool at given index.
    op_ldc {
        arguments: {
            index: u32
        }
    },

    /// Store top of stack into constant pool at given index.
    ///
    /// This opcode is mainly used for Inline Caches and should not
    /// be used for global variables or other purposes.
    op_stc {
        arguments: {
            index: u32
        }
    },

    op_dup {},
    op_pop {},
    op_swap {},


    /// Call function from top of stack and pass `argc` arguments.
    ///
    /// This opcode during execution shifts `argc` values from stack bottom
    /// to stack top and creates a new stack frame.
    op_return_call {
        arguments: {
            argc: u32
        }
    },

    /// Call subroutine at `procedures[subroutine]` with `argc` arguments.
    op_return_call_subr {
        arguments: {
            argc: u32,
            subroutine: u32
        }
    },

    op_br {
        arguments: {
            target: JumpTarget
        }
    },

    op_brfalse {
        arguments: {
            target: JumpTarget
        }
    },

    op_brtrue {
        arguments: {
            target: JumpTarget
        }
    },

    op_brnull {
        arguments: {
            target: JumpTarget
        }
    },

    op_brundef {
        arguments: {
            target: JumpTarget
        }
    },

    op_brtc8 {
        arguments: {
            tc8: u8,
            target: JumpTarget
        }
    },

    op_brtc16 {
        arguments: {
            tc16: u16,
            target: JumpTarget
        }
    },

    op_brimmediate {
        arguments: {
            immtag: u8,
            target: JumpTarget
        }
    },

    op_brheapobj {
        arguments: {
            target: JumpTarget
        }
    },

    op_brzero {
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumeq {
        display: "brnum.eq"
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumne {
        display: "brnum.ne"
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumlt {
        display: "brnum.lt"
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumle {
        display: "brnum.le"
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumgt {
        display: "brnum.gt"
        arguments: {
            target: JumpTarget
        }
    },

    op_brnumge {
        display: "brnum.ge"
        arguments: {
            target: JumpTarget
        }
    },

    op_beq {
        arguments: {
            target: JumpTarget
        }
    },

    op_breqv {
        arguments: {
            target: JumpTarget
        }
    },

    op_brequal {
        arguments: {
            target: JumpTarget
        }
    },

    op_brneq {
        arguments: {
            target: JumpTarget
        }
    },

    op_brneqv {
        arguments: {
            target: JumpTarget
        }
    },

    op_brnequal {
        arguments: {
            target: JumpTarget
        }
    },

    op_cnumeq {
        display: "cnum.eq",
    },
    op_cnumne {
        display: "cnum.ne",
    },
    op_cnumlt {
        display: "cnum.lt",
    },
    op_cnumle {
        display: "cnum.le",
    },
    op_cnumgt {
        display: "cnum.gt",
    },
    op_cnumge {
        display: "cnum.ge",
    },

    op_ceq {
        display: "ceq",
    },

    op_cne {
        display: "cne",
    },

    op_ceqv {
        display: "ceqv",
    },

    op_cneqv {
        display: "cneqv",
    },

    op_cequal {
        display: "cequal",
    },

    op_cnequal {
        display: "cnequal",
    },

    op_add {
        display: "add",

    },

    op_sub {
        display: "sub",

    },

    op_mul {
        display: "mul",

    },

    op_div {
        display: "div",

    },

    op_quo {
        display: "quo",

    },

    op_rem {
        display: "rem",

    },

    op_and {
        display: "and",

    },

    op_or {
        display: "or",

    },

    op_xor {
        display: "xor",

    },

    op_shl {
        display: "shl",

    },

    op_shr {
        display: "shr",

    },

    op_ash {
        display: "ash",

    },

    op_neg {
        display: "neg",
    },

    op_lognot {
        display: "lognot",

    },

    op_not {
        display: "not",

    },


    // Raw arithmetic ops which just perform machine arithmetic on primitive types
    op_add_i4 {
        display: "add.i4",
    },

    /// Add two i4 values with overflow checking.
    ///
    /// Pushed (i4, bool) tuple where bool indicates whether overflow occurred.
    op_add_i4_ovf {
        display: "add.i4.ovf",
    },

    op_add_i4_ovf_un {
        display: "add.i4.ovf.un",
    },

    op_sub_i4 {
        display: "sub.i4",
    },

    op_sub_i4_ovf {
        display: "sub.i4.ovf",
    },

    op_sub_i4_ovf_un {
        display: "sub.i4.ovf.un",
    },

    op_mul_i4 {
        display: "mul.i4",
    },

    op_mul_i4_ovf {
        display: "mul.i4.ovf",
    },

    op_div_i4 {
        display: "div.i4",
    },

    op_rem_i4 {
        display: "rem.i4",
    },

    op_and_i4 {
        display: "and.i4",
    },

    op_or_i4 {
        display: "or.i4",
    },

    op_xor_i4 {
        display: "xor.i4",
    },

    op_shl_i4 {
        display: "shl.i4",
    },

    op_shr_i4 {
        display: "shr.i4",
    },

    op_shr_u4 {
        display: "shr.u4",
    },

    op_neg_i4 {
        display: "neg.i4",
    },

    op_not_i4 {
        display: "not.i4",
    },

    op_add_i8 {
        display: "add.i8",
    },

    op_add_i8_ovf {
        display: "add.i8.ovf",
    },

    op_add_i8_ovf_un {
        display: "add.i8.ovf.un",
    },

    op_sub_i8 {
        display: "sub.i8",
    },

    op_sub_i8_ovf {
        display: "sub.i8.ovf",
    },

    op_sub_i8_ovf_un {
        display: "sub.i8.ovf.un",
    },

    op_mul_i8 {
        display: "mul.i8",
    },

    op_mul_i8_ovf {
        display: "mul.i8.ovf",
    },

    op_div_i8 {
        display: "div.i8",
    },

    op_rem_i8 {
        display: "rem.i8",
    },

    op_and_i8 {
        display: "and.i8",
    },

    op_or_i8 {
        display: "or.i8",
    },

    op_xor_i8 {
        display: "xor.i8",
    },

    op_shl_i8 {
        display: "shl.i8",
    },

    op_shr_i8 {
        display: "shr.i8",
    },

    op_shr_u8 {
        display: "shr.u8",
    },

    op_neg_i8 {
        display: "neg.i8",
    },

    op_not_i8 {
        display: "not.i8",
    },

    op_unbox_r8 {
        display: "unbox.r8",
    },

    op_box_r8 {
        display: "box.r8",
    },

    op_unbox_i4 {
        display: "unbox.i4",
    },

    op_box_i4 {
        display: "box.i4",
    },

    op_isimm {
        display: "isimm",
    },

    op_isheapobj {
        display: "isheapobj",
    },

    op_tc8 {
        display: "tc8",
    },

    op_tc16 {
        display: "tc16",
    },

    op_istc8 {
        display: "istc8",
        arguments: {
            tc8: u8
        }
    },

    op_istc16 {
        display: "istc16",
        arguments: {
            tc16: u16
        }
    }

    /// Allocate a new closure. The prot parameter indicates
    /// index into the constant pool where the closure prototype is stored.
    op_newclos {
        arguments: {
            prot: u32,
            nfree: u32
        }
    },


    op_ldcallee {},

    op_ldind_i1 {
        display: "ldind.i1",
        arguments: {
            offset: i32
        }
    },

    op_ldind_i2 {
        display: "ldind.i2",
        arguments: {
            offset: i32
        }
    },

    op_ldind_i4 {
        display: "ldind.i4",
        arguments: {
            offset: i32
        }
    },

    op_ldind_i8 {
        display: "ldind.i8",
        arguments: {
            offset: i32
        }
    },

    op_ldind_r8 {
        display: "ldind.r8",
        arguments: {
            offset: i32
        }
    },

    op_ldind_ref {
        display: "ldind.ref",
        arguments: {
            offset: i32
        }
    },

    op_stind_i1 {
        display: "stind.i1",
        arguments: {
            offset: i32
        }
    },

    op_stind_i2 {
        display: "stind.i2",
        arguments: {
            offset: i32
        }
    },

    op_stind_i4 {
        display: "stind.i4",
        arguments: {
            offset: i32
        }
    },

    op_stind_i8 {
        display: "stind.i8",
        arguments: {
            offset: i32
        }
    },

    op_stind_r8 {
        display: "stind.r8",
        arguments: {
            offset: i32
        }
    },

    op_stind_ref {
        display: "stind.ref",
        arguments: {
            offset: i32
        }
    },

    /// Resolve a global variable via symbol at `index` in constant-pool.
    op_ldglob {
        arguments: {
            index: u32,
            metadata_id: u32
        }
    },

    op_stglob {
        arguments: {
            index: u32,
            metadata_id: u32
        }
    },

    op_ldmod {
        arguments: {
            index: u32,
            metadata_id: u32
        }
    },

    op_stmod {
        arguments: {
            index: u32,
            metadata_id: u32
        }
    },


    op_calli0 {
        display: "calli0",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli1 {
        display: "calli1",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli2 {
        display: "calli2",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli3 {
        display: "calli3",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli4 {
        display: "calli4",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli5 {
        display: "calli5",
        arguments: {
            intrinsic_id: u8
        }
    },

    op_calli {
        display: "calli",
        arguments: {
            argc: u8,
            intrinsic_id: u8
        }
    },

    /// Capture rest of arguments from the argument list onto cons-list.
    op_arglist {
        arguments: {
            start_index: u32
        }
    },


}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct JumpTarget(pub i32);

impl BytecodeOperand for JumpTarget {
    fn required_width(&self) -> OperandWidth {
        OperandWidth::Wide32
    }

    fn decode<R: std::io::Read>(reader: &mut R, width: OperandWidth) -> std::io::Result<Self> {
        let val = i32::decode(reader, width)?;
        Ok(Self(val))
    }

    fn encode<W: Write>(&self, writer: &mut W, width: OperandWidth) -> std::io::Result<()> {
        self.0.encode(writer, width)
    }

    fn decode_from_buffer<const WIDTH: u8, B: BytecodeBuffer + ?Sized>(
        buffer: &B,
        pc: usize,
    ) -> (Self, usize) {
        let (val, size) = i32::decode_from_buffer::<WIDTH, B>(buffer, pc);
        (Self(val), size)
    }

    unsafe fn decode_from_ptr(ptr: *const u8, width: OperandWidth) -> (Self, *const u8) {
        unsafe {
            let (val, ptr) = i32::decode_from_ptr(ptr, width);
            (Self(val), ptr)
        }
    }

    unsafe fn decode_from_ptr_static<const WIDTH: u8>(ptr: *const u8) -> (Self, *const u8) {
        unsafe {
            let (val, ptr) = i32::decode_from_ptr_static::<WIDTH>(ptr);
            (Self(val), ptr)
        }
    }

    fn encode_size(width: OperandWidth) -> usize {
        i32::encode_size(width)
    }

    fn encode_to_slice(&self, buf: &mut [u8], width: OperandWidth) -> usize {
        self.0.encode_to_slice(buf, width)
    }
}

impl DumpValue for JumpTarget {
    fn dump<D: BytecodeDumper + ?Sized>(&self, dumper: &mut D) -> std::fmt::Result {
        let target = dumper.location() as isize + self.0 as isize;
        write!(dumper, "->{:04}", target)
    }
}

use bytecodegen::{self, *};
use std::fmt;

use super::type_profile::TypeProfile;
use crate::interpreter::frame::{CallFrame, CallFrameSlot};

// Narrow:
// -128..-1  local variables
//    0..127 arguments
//
// Wide16:
// -2**15..-1  local variables
//      0..2**15-1 arguments
//
// Wide32:
// -2**31..-1  local variables
//      0..2**31-1 arguments
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VirtualRegister(pub isize);

impl Operand for VirtualRegister {
    fn can_decode_from_ptr() -> bool {
        true
    }

    fn decode(width: OperandWidth, input: &mut ByteCursor<'_>) -> Result<Self, DecodeError> {
        match width {
            OperandWidth::Narrow => {
                let operand = input.read_u8()? as i8 as isize;

                Ok(VirtualRegister(operand))
            }

            OperandWidth::Wide16 => {
                let operand = input.read_u16()? as i16 as isize;

                Ok(VirtualRegister(operand))
            }

            OperandWidth::Wide32 => {
                let operand = input.read_u32()? as i32 as isize;

                Ok(VirtualRegister(operand))
            }
        }
    }

    unsafe fn decode_from_ptr(width: OperandWidth, input: *const u8) -> (Self, *const u8) {
        match width {
            OperandWidth::Narrow => unsafe {
                let operand = input.add(0).read() as i8 as isize;

                (VirtualRegister(operand), input.add(1))
            },
            OperandWidth::Wide16 => unsafe {
                let operand = input.add(0).read() as i16 as isize;

                (VirtualRegister(operand), input.add(2))
            },
            OperandWidth::Wide32 => unsafe {
                let operand = input.add(0).read() as i32 as isize;

                (VirtualRegister(operand), input.add(4))
            },
        }
    }

    fn encode<S: ByteSink + ?Sized>(value: &Self, width: OperandWidth, output: &mut S) {
        match width {
            OperandWidth::Narrow => {
                output.push(value.0 as i8 as u8);
            }
            OperandWidth::Wide16 => {
                let bytes = (value.0 as i16 as u16).to_le_bytes();
                output.extend_from_slice(&bytes);
            }
            OperandWidth::Wide32 => {
                let bytes = (value.0 as i32 as u32).to_le_bytes();
                output.extend_from_slice(&bytes);
            }
        }
    }

    fn encoded_len(width: OperandWidth) -> usize {
        match width {
            OperandWidth::Narrow => 1,
            OperandWidth::Wide16 => 2,
            OperandWidth::Wide32 => 4,
        }
    }

    fn fits_width(value: &Self, width: OperandWidth) -> bool {
        let target_max = match width {
            OperandWidth::Narrow => i8::MAX as isize,
            OperandWidth::Wide16 => i16::MAX as isize,
            OperandWidth::Wide32 => i32::MAX as isize,
        };

        let target_min = match width {
            OperandWidth::Narrow => i8::MIN as isize,
            OperandWidth::Wide16 => i16::MIN as isize,
            OperandWidth::Wide32 => i32::MIN as isize,
        };

        value.offset() >= target_min && value.offset() <= target_max
    }
}

impl VirtualRegister {
    /* stack grows downwards thus locals
      are negative, arguments are positive
    */
    pub const fn is_local(self) -> bool {
        self.0 < 0
    }

    pub const fn is_argument(self) -> bool {
        self.0 >= 0
    }

    pub const fn for_local(uidx: usize) -> Self {
        Self(Self::local_to_operand(uidx as _))
    }

    pub const fn for_argument(argidx: usize) -> Self {
        Self(Self::argument_to_operand(argidx as _))
    }

    const fn local_to_operand(local: isize) -> isize {
        -1 - local
    }

    const fn operand_to_local(operand: isize) -> isize {
        -1 - operand
    }

    const fn operand_to_argument(operand: isize) -> isize {
        operand - CallFrame::argument_offset(0)
    }

    const fn argument_to_operand(argument: isize) -> isize {
        CallFrame::argument_offset(0) + argument
    }

    pub const INVALID: isize = 0x3fffffff;

    pub const fn is_valid(self) -> bool {
        self.0 != Self::INVALID
    }

    pub const fn is_header(self) -> bool {
        self.0 >= 0 && self.0 < CallFrameSlot::FirstArgument as isize
    }

    pub const fn to_local(self) -> isize {
        Self::operand_to_local(self.0)
    }

    pub const fn to_argument(self) -> isize {
        Self::operand_to_argument(self.0)
    }

    pub const fn offset(self) -> isize {
        self.0
    }

    pub const fn offset_in_bytes(self) -> isize {
        self.0 * size_of::<u64>() as isize
    }
}

impl fmt::Display for VirtualRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_local() {
            write!(f, "local{}", self.to_local())
        } else if self.is_header() {
            write!(f, "header{}", self.offset())
        } else {
            write!(f, "arg{}", self.to_argument())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Operand)]
#[repr(u16)]
pub enum Primitive {
    MakeBox,
    EnsureLocalVar,
    Lookup,
    LookupBound,
    LookupBoundPublic,
    LookupBoundPrivate,
    CurrentModule,
    Define,
    Reverse,
    Append,
    Memq,
    Memv,
    Breakpoint,
    SymbolToString,
    StringToSymbol,
    Length,
    PushCframe,
    CurrentContinuationMarks,
    SetAttachments,
    Winders,
    MakeSyntax,
    DefaultRetk,
}

impl Primitive {
    pub const ALL: &'static [Self] = &[
        Self::MakeBox,
        Self::EnsureLocalVar,
        Self::Lookup,
        Self::LookupBound,
        Self::LookupBoundPublic,
        Self::LookupBoundPrivate,
        Self::CurrentModule,
        Self::Define,
        Self::Reverse,
        Self::Append,
        Self::Memq,
        Self::Memv,
        Self::Breakpoint,
        Self::SymbolToString,
        Self::StringToSymbol,
        Self::Length,
        Self::PushCframe,
        Self::CurrentContinuationMarks,
        Self::SetAttachments,
        Self::Winders,
        Self::MakeSyntax,
        Self::DefaultRetk,
    ];

    pub const fn name(self) -> &'static str {
        match self {
            Self::MakeBox => "make-variable",
            Self::EnsureLocalVar => "module-ensure-local-variable!",
            Self::Lookup => "lookup",
            Self::LookupBound => "lookup-bound",
            Self::LookupBoundPublic => "lookup-bound-public",
            Self::LookupBoundPrivate => "lookup-bound-private",
            Self::CurrentModule => "current-module",
            Self::Define => "define",
            Self::Reverse => "reverse",
            Self::Append => "append",
            Self::Memq => "memq",
            Self::Memv => "memv",
            Self::Breakpoint => ".breakpoint",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::Length => "length",
            Self::PushCframe => "push-cframe",
            Self::CurrentContinuationMarks => "current-continuation-marks",
            Self::SetAttachments => "$set-attachments!",
            Self::Winders => "$winders",
            Self::MakeSyntax => "make-syntax",
            Self::DefaultRetk => "#%default-retk",
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str((*self).name())
    }
}

bytecode_list! {
    section Bytecode {
        Nop {}

        Mov {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        Movk {
            args {
                dst: VirtualRegister,
                constant: usize
            }
        }

        AssertNargsEq {
            args {
                expected: u32
            }
        }

        /// Assert that the number of arguments is greater than or equal to the expected number.
        AssertNargsGe {
            args {
                expected: u32
            }
        }

        /// Box rest arguments into a list and store it in the destination register.
        ConsRest {
            args {
                /// Destination register to store the list.
                dst: VirtualRegister,
                /// Index from which the rest arguments start.
                rest_start: usize
            }
        }

        /// Enter a new call-frame.
        ///
        /// This opcode is a prelude and can trigger a JIT or GC.
        ///
        /// `nlocals` is the number of local values to allocate in a frame.
        Enter {
            args {
                nlocals: usize
            }

            metadata {} /* TODO: metadata for JIT */
        }

        /// Tail call the procedure in the given register.
        ///
        /// Calling convention is as follows:
        /// ```
        /// for i in 0..argc {
        ///     fp.argument(i) = arg[argv+i];
        /// }
        /// fp.callee = callee;
        /// fp.code_block = callee.as_::<Closure>().code_block;
        /// pc = 0;
        /// cb = fp.code_block;
        ///
        /// ```
        TailCall {
            args {
                callee: VirtualRegister,
                /// First register containing argument[0]. This register
                /// must be local, not an argument.
                argv: VirtualRegister,
                /// Number of arguments.
                ///
                /// VM accesses arguments using `fp[argv..argv+argc]`.
                argc: usize
            }
        }

        Jump {
            args {
                #[relocatable]
                target: isize
            }
        }

        /// Jump to the target if the source is not #f.
        JumpIfTruthy {
            args {
                src: VirtualRegister,
                #[relocatable]
                target: isize,
            }
        }

        /// Jump to the target if the source is #f.
        JumpIfFalse {
            args {
                src: VirtualRegister,
                #[relocatable]
                target: isize,
            }
        }

        Unspecified {
            args {
                dst: VirtualRegister,
            }
        }

        PrimitiveCall0 {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
            }
        }

        PrimitiveCall1 {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
                src0: VirtualRegister,
            }
        }

        PrimitiveCall2 {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
                src0: VirtualRegister,
                src1: VirtualRegister,
            }
        }

        PrimitiveCall3 {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
                src0: VirtualRegister,
                src1: VirtualRegister,
                src2: VirtualRegister,
            }
        }

        PrimitiveCall4 {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
                src0: VirtualRegister,
                src1: VirtualRegister,
                src2: VirtualRegister,
                src3: VirtualRegister,
            }
        }


        CacheRef {
            args {
                dst: VirtualRegister,
                cache_key: usize,
            }
        }

        CacheSet {
            args {
                cache_key: usize,
                value: VirtualRegister,
            }
        }

        /// Perform `(eq? lhs rhs)` and store the result in the destination register.
        Eq {
            args {
                dst: VirtualRegister,
                lhs: VirtualRegister,
                rhs: VirtualRegister,
            }
        }

        /// Perform `(eqv? lhs rhs)` and store the result in the destination register.
        Eqv {
            args {
                dst: VirtualRegister,
                lhs: VirtualRegister,
                rhs: VirtualRegister,
            }
        }

        /// Perform `(equal? lhs rhs)` and store the result in the destination register.
        Equal {
            args {
                dst: VirtualRegister,
                lhs: VirtualRegister,
                rhs: VirtualRegister,
            }
        }

        /// Perform `(not src)` and store the result in the destination register.
        ///
        /// This operation is equivalent to `(eq? src #f)`, but is encoded
        /// separately to avoid emitting more bytes than necessary.
        Not {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        IsTc8 {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
                tc8: u8,
            }
        }

        IsTc16 {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
                tc16: u16,
            }
        }

        group TypeCheck =
        [
            IsImmediate,
            IsHeapObject,
            IsProcedure,
            IsVariable,
            IsEofObject,
            IsNull,
            IsUnspecified,
            IsPair,
            IsList,
            IsVector,
            IsBytevector,
            IsString,
            IsBoolean,
            IsSymbol,
            IsFixnum,
            IsExactInteger,
            IsInteger,
            IsChar,
            IsNumber,
            IsComplex,
            IsReal,
            IsRational,
            IsInexact,
            IsExact,
            IsNan,
            IsEven,
            IsOdd,
            IsZero,
            IsTuple
        ] {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        group NumericBinOps = [
            Add,
            Sub,
            Mul,
            Div,
            Quotient,
            Remainder,
            Modulo,
            Ash,
            Logand,
            Logior,
            Expt
        ] {
            args {
                dst: VirtualRegister,
                lhs: VirtualRegister,
                rhs: VirtualRegister,
            }
            metadata {
                type_profile: TypeProfile
            }
        }

        group NumericUnaryOps =
        [
            Neg,
            Abs,
            Sqrt,
            Cos,
            Sin,
            Tan,
            Atan,
            Asin,
            Acos,
            Ceiling,
            Floor,
            Truncate,
            Lognot,
            ExactToInexact,
            InexactToExact,
            IntegerToChar,
            CharToInteger,
        ] {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
            metadata {
                type_profile: TypeProfile
            }
        }

        group NumericCmpOps = [
            NumericEqual,
            NumericLt,
            NumericGt,
            NumericLte,
            NumericGte
        ] {
            args {
                dst: VirtualRegister,
                lhs: VirtualRegister,
                rhs: VirtualRegister,
            }
            metadata {
                type_profile: TypeProfile
            }
        }

        VariableBound {
            args {
                dst: VirtualRegister,
                variable: VirtualRegister,
            }
        }

        VariableRef {
            args {
                dst: VirtualRegister,
                variable: VirtualRegister,
            }
        }

        VariableSet {
            args {
                variable: VirtualRegister,
                value: VirtualRegister,
            }
        }

        Car {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        Cdr {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        SetCar {
            args {
                pair: VirtualRegister,
                value: VirtualRegister,
            }
        }

        SetCdr {
            args {
                pair: VirtualRegister,
                value: VirtualRegister,
            }
        }

        Cons {
            args {
                dst: VirtualRegister,
                car: VirtualRegister,
                cdr: VirtualRegister,
            }
        }

        group IndexRefOps = [StringRef, VectorRef, TupleRef, BytevectorRef] {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
                index: VirtualRegister,
            }
        }

        group IndexSetOps = [StringSet, VectorSet, TupleSet, BytevectorSet] {
            args {
                src: VirtualRegister,
                index: VirtualRegister,
                value: VirtualRegister,
            }
        }

        group LengthOps = [StringLength, VectorLength, TupleLength, BytevectorLength] {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
            }
        }

        MakeVector {
            args {
                dst: VirtualRegister,
                length: VirtualRegister,
                init: VirtualRegister,
            }
        }

        MakeTuple {
            args {
                dst: VirtualRegister,
                length: VirtualRegister,
                init: VirtualRegister,
            }
        }

        MakeBytevector {
            args {
                dst: VirtualRegister,
                length: VirtualRegister,
                init: VirtualRegister,
            }
        }

        MakeString {
            args {
                dst: VirtualRegister,
                length: VirtualRegister,
                init: VirtualRegister,
            }
        }

        MakeClosue {
            args {
                dst: VirtualRegister,
                code: VirtualRegister,
            }
        }

        ClosureRef {
            args {
                dst: VirtualRegister,
                closure: VirtualRegister,
                index: VirtualRegister,
            }
        }

        ClosureRefImm {
            args {
                dst: VirtualRegister,
                closure: VirtualRegister,
                index: u32,
            }
        }

        ClosureSet {
            args {
                closure: VirtualRegister,
                index: u32,
                value: VirtualRegister,
            }
        }

        ClosureSetImm {
            args {
                closure: VirtualRegister,
                index: u32,
                value: VirtualRegister,
            }
        }
    }
}

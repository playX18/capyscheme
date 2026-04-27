use bytecodegen::{self, *};
use std::fmt;

use super::type_profile::TypeProfile;
use crate::interpreter::frame::{CallFrame, CallFrameSlot};

// Narrow:
// -128..-1  local variables
//    0..15  arguments
//   16..127 constants
//
// Wide16:
// -2**15..-1  local variables
//      0..64  arguments
//     64..2**15-1 constants
//
// Wide32:
// -2**31..-1  local variables
//      0..1073741824  arguments
//     1073741824..2**31-1 constants
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
                if operand >= FIRST_CONSTANT_REGISTER_INDEX8 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX8;
                    return Ok(Self::for_constant(idx as usize));
                }

                Ok(VirtualRegister(operand))
            }

            OperandWidth::Wide16 => {
                let operand = input.read_u16()? as i16 as isize;
                if operand >= FIRST_CONSTANT_REGISTER_INDEX16 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX16;
                    return Ok(Self::for_constant(idx as usize));
                }

                Ok(VirtualRegister(operand))
            }

            OperandWidth::Wide32 => {
                let operand = input.read_u32()? as i32 as isize;
                if operand >= FIRST_CONSTANT_REGISTER_INDEX32 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX32;
                    return Ok(Self::for_constant(idx as usize));
                }

                Ok(VirtualRegister(operand))
            }
        }
    }

    unsafe fn decode_from_ptr(width: OperandWidth, input: *const u8) -> (Self, *const u8) {
        match width {
            OperandWidth::Narrow => unsafe {
                let operand = input.add(0).read() as i8 as isize;
                if operand >= FIRST_CONSTANT_REGISTER_INDEX8 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX8;
                    return (Self::for_constant(idx as usize), input.add(1));
                }

                (VirtualRegister(operand), input.add(1))
            },
            OperandWidth::Wide16 => unsafe {
                let operand = input.add(0).read() as i16 as isize;
                if operand >= FIRST_CONSTANT_REGISTER_INDEX16 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX16;
                    return (Self::for_constant(idx as usize), input.add(2));
                }

                (VirtualRegister(operand), input.add(2))
            },
            OperandWidth::Wide32 => unsafe {
                let operand = input.add(0).read() as i32 as isize;
                if operand >= FIRST_CONSTANT_REGISTER_INDEX32 {
                    let idx = operand - FIRST_CONSTANT_REGISTER_INDEX32;
                    return (Self::for_constant(idx as usize), input.add(4));
                }

                (VirtualRegister(operand), input.add(4))
            },
        }
    }

    fn encode<S: ByteSink + ?Sized>(value: &Self, width: OperandWidth, output: &mut S) {
        let first_constant_index = match width {
            OperandWidth::Narrow => FIRST_CONSTANT_REGISTER_INDEX8,
            OperandWidth::Wide16 => FIRST_CONSTANT_REGISTER_INDEX16,
            OperandWidth::Wide32 => FIRST_CONSTANT_REGISTER_INDEX32,
        };

        match width {
            OperandWidth::Narrow => {
                if value.is_constant() {
                    output.push((first_constant_index + value.to_constant_index()) as u8);
                } else {
                    output.push(value.0 as i8 as u8);
                }
            }
            OperandWidth::Wide16 => {
                let bytes;
                if value.is_constant() {
                    bytes =
                        ((first_constant_index + value.to_constant_index()) as u16).to_le_bytes();
                } else {
                    bytes = (value.0 as i16 as u16).to_le_bytes();
                }

                output.extend_from_slice(&bytes);
            }
            OperandWidth::Wide32 => {
                let bytes;
                if value.is_constant() {
                    bytes =
                        ((first_constant_index + value.to_constant_index()) as u32).to_le_bytes();
                } else {
                    bytes = (value.0 as i32 as u32).to_le_bytes();
                }

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
        let first_constant_index = match width {
            OperandWidth::Narrow => FIRST_CONSTANT_REGISTER_INDEX8,
            OperandWidth::Wide16 => FIRST_CONSTANT_REGISTER_INDEX16,
            OperandWidth::Wide32 => FIRST_CONSTANT_REGISTER_INDEX32,
        };

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

        if value.is_constant() {
            return first_constant_index + value.to_constant_index() as isize <= target_max;
        }

        value.offset() >= target_min && value.offset() < first_constant_index
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

    pub const fn for_constant(idx: usize) -> Self {
        Self(FIRST_CONSTANT_REGISTER_INDEX + idx as isize)
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

    pub const fn is_constant(self) -> bool {
        self.0 >= FIRST_CONSTANT_REGISTER_INDEX
    }

    pub const fn to_local(self) -> isize {
        Self::operand_to_local(self.0)
    }

    pub const fn to_argument(self) -> isize {
        Self::operand_to_argument(self.0)
    }

    pub const fn to_constant_index(self) -> isize {
        self.0 - FIRST_CONSTANT_REGISTER_INDEX
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
        if self.is_constant() {
            write!(f, "const{}", self.to_constant_index())
        } else if self.is_local() {
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

pub const FIRST_CONSTANT_REGISTER_INDEX: isize = 0x40000000;
pub const FIRST_CONSTANT_REGISTER_INDEX8: isize = 16;
pub const FIRST_CONSTANT_REGISTER_INDEX16: isize = 64;
pub const FIRST_CONSTANT_REGISTER_INDEX32: isize = FIRST_CONSTANT_REGISTER_INDEX;

bytecode_list! {
    section Bytecode {
        Nop {}

        Move {
            args {
                dst: VirtualRegister,
                src: VirtualRegister,
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

        Enter {
            args {
                nlocals: usize
            }

            metadata {} /* TODO: metadata for JIT */
        }

        App {
            args {
                callee: VirtualRegister,
                argc: u32,
                retk: VirtualRegister,
                arg_start: VirtualRegister,
            }
        }

        Continue {
            args {
                callee: VirtualRegister,
                arg_start: VirtualRegister,
                argc: u32,
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

        PrimitiveCallN {
            args {
                dst: VirtualRegister,
                primitive: Primitive,
                arg_start: VirtualRegister,
                argc: u32,
            }
        }

        CacheRef {
            args {
                dst: VirtualRegister,
                cache_key: VirtualRegister,
            }
        }

        CacheSet {
            args {
                cache_key: VirtualRegister,
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

#[cfg(test)]
mod tests {
    use super::*;
    use bytecodegen::{ByteCursor, LinkedBytecodeView};

    fn linked_bytes(instructions: impl IntoIterator<Item = LinkableInstruction>) -> Vec<u8> {
        generated::link(instructions).unwrap().bytes().to_vec()
    }

    #[test]
    fn opcode_count_stays_within_single_byte_budget() {
        assert!(SectionId::Bytecode.opcodes().len() < 254);
    }

    #[test]
    fn encodes_cps_app_and_continue_argument_windows() {
        let bytes = linked_bytes([
            App::new(
                VirtualRegister::for_argument(1),
                3,
                VirtualRegister::for_argument(2),
                VirtualRegister::for_argument(5),
            )
            .into(),
            Continue::new(
                VirtualRegister::for_argument(3),
                VirtualRegister::for_argument(5),
                3,
            )
            .into(),
        ]);

        let mut cursor = ByteCursor::new(&bytes);
        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::App(op) => {
                assert_eq!(op.callee, VirtualRegister::for_argument(1));
                assert_eq!(op.argc, 3);
                assert_eq!(op.retk, VirtualRegister::for_argument(2));
                assert_eq!(op.arg_start, VirtualRegister::for_argument(5));
            }
            _ => panic!("expected App"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::Continue(op) => {
                assert_eq!(op.callee, VirtualRegister::for_argument(3));
                assert_eq!(op.arg_start, VirtualRegister::for_argument(5));
                assert_eq!(op.argc, 3);
            }
            _ => panic!("expected Continue"),
        }
    }

    #[test]
    fn encodes_primitive_call_arities() {
        let bytes = linked_bytes([
            PrimitiveCall0::new(
                VirtualRegister::for_local(0),
                Primitive::CurrentContinuationMarks,
            )
            .into(),
            PrimitiveCall3::new(
                VirtualRegister::for_local(1),
                Primitive::PushCframe,
                VirtualRegister::for_argument(0),
                VirtualRegister::for_argument(1),
                VirtualRegister::for_argument(2),
            )
            .into(),
            PrimitiveCall4::new(
                VirtualRegister::for_local(2),
                Primitive::MakeSyntax,
                VirtualRegister::for_argument(0),
                VirtualRegister::for_argument(1),
                VirtualRegister::for_argument(2),
                VirtualRegister::for_argument(3),
            )
            .into(),
            PrimitiveCallN::new(
                VirtualRegister::for_local(3),
                Primitive::Lookup,
                VirtualRegister::for_argument(4),
                5,
            )
            .into(),
        ]);

        let mut cursor = ByteCursor::new(&bytes);
        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::PrimitiveCall0(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(0));
                assert_eq!(op.primitive, Primitive::CurrentContinuationMarks);
            }
            _ => panic!("expected PrimitiveCall0"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::PrimitiveCall3(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(1));
                assert_eq!(op.primitive, Primitive::PushCframe);
                assert_eq!(op.src0, VirtualRegister::for_argument(0));
                assert_eq!(op.src1, VirtualRegister::for_argument(1));
                assert_eq!(op.src2, VirtualRegister::for_argument(2));
            }
            _ => panic!("expected PrimitiveCall3"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::PrimitiveCall4(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(2));
                assert_eq!(op.primitive, Primitive::MakeSyntax);
                assert_eq!(op.src0, VirtualRegister::for_argument(0));
                assert_eq!(op.src1, VirtualRegister::for_argument(1));
                assert_eq!(op.src2, VirtualRegister::for_argument(2));
                assert_eq!(op.src3, VirtualRegister::for_argument(3));
            }
            _ => panic!("expected PrimitiveCall4"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::PrimitiveCallN(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(3));
                assert_eq!(op.primitive, Primitive::Lookup);
                assert_eq!(op.arg_start, VirtualRegister::for_argument(4));
                assert_eq!(op.argc, 5);
            }
            _ => panic!("expected PrimitiveCallN"),
        }
    }

    #[test]
    fn encodes_cache_ops_as_direct_bytecodes() {
        let bytes = linked_bytes([
            CacheRef::new(
                VirtualRegister::for_local(0),
                VirtualRegister::for_constant(0),
            )
            .into(),
            CacheSet::new(
                VirtualRegister::for_constant(0),
                VirtualRegister::for_argument(0),
            )
            .into(),
        ]);

        let mut cursor = ByteCursor::new(&bytes);
        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::CacheRef(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(0));
                assert_eq!(op.cache_key, VirtualRegister::for_constant(0));
            }
            _ => panic!("expected CacheRef"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::CacheSet(op) => {
                assert_eq!(op.cache_key, VirtualRegister::for_constant(0));
                assert_eq!(op.value, VirtualRegister::for_argument(0));
            }
            _ => panic!("expected CacheSet"),
        }
    }

    #[test]
    fn fallback_primitive_enum_excludes_inline_and_cache_ops() {
        let names = Primitive::ALL.iter().map(|p| p.name()).collect::<Vec<_>>();

        assert!(names.contains(&"lookup"));
        assert!(names.contains(&"make-syntax"));
        assert!(!names.contains(&"cache-ref"));
        assert!(!names.contains(&"cache-set!"));
        assert!(!names.contains(&"vector-set!"));
        assert!(!names.contains(&"s32+"));
        assert!(!names.contains(&"zero?"));
    }

    #[test]
    fn bytecode_schema_excludes_low_level_integer_and_raw_ops() {
        let names = SectionId::Bytecode
            .opcodes()
            .iter()
            .map(|op| format!("{op:?}"))
            .collect::<Vec<_>>();

        for removed in [
            "I32Add",
            "I64UShr",
            "S32Eq",
            "U64Lt",
            "S8ToS64",
            "Ref8",
            "Refptr",
            "Store64",
            "Typecode8",
            "UsizeToValue",
        ] {
            assert!(
                !names.iter().any(|name| name == removed),
                "{removed} should not be a bytecode opcode"
            );
        }
    }

    #[test]
    fn encodes_scheme_level_direct_primitive_opcodes() {
        let bytes = linked_bytes([
            IsImmediate::new(
                VirtualRegister::for_local(0),
                VirtualRegister::for_argument(0),
            )
            .into(),
            VectorRef::new(
                VirtualRegister::for_local(1),
                VirtualRegister::for_argument(2),
                VirtualRegister::for_argument(3),
            )
            .into(),
            Eq::new(
                VirtualRegister::for_local(2),
                VirtualRegister::for_argument(6),
                VirtualRegister::for_argument(7),
            )
            .into(),
        ]);

        let mut cursor = ByteCursor::new(&bytes);
        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::IsImmediate(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(0));
                assert_eq!(op.src, VirtualRegister::for_argument(0));
            }
            _ => panic!("expected IsImmediate"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::VectorRef(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(1));
                assert_eq!(op.src, VirtualRegister::for_argument(2));
                assert_eq!(op.index, VirtualRegister::for_argument(3));
            }
            _ => panic!("expected VectorRef"),
        }

        match Instruction::decode_from(&mut cursor).unwrap() {
            Instruction::Eq(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(2));
                assert_eq!(op.lhs, VirtualRegister::for_argument(6));
                assert_eq!(op.rhs, VirtualRegister::for_argument(7));
            }
            _ => panic!("expected Eq"),
        }
    }

    #[test]
    fn narrow_constant_register_consumes_one_operand_byte() {
        let bytes = linked_bytes([Move::new(
            VirtualRegister::for_local(0),
            VirtualRegister::for_constant(0),
        )
        .into()]);
        let record = Instruction::decode_record(&bytes).unwrap();

        assert_eq!(record.size, bytes.len());
        match record.instruction {
            Instruction::Move(op) => {
                assert_eq!(op.dst, VirtualRegister::for_local(0));
                assert_eq!(op.src, VirtualRegister::for_constant(0));
            }
            _ => panic!("expected Move"),
        }
    }
}

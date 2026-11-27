//! Inline lowerings of the string, character and list natives that the
//! optimizer recognizes by name (`list-ref`, `list-head`, `string=?`,
//! `string-append`, `substring`, `list->vector`, `vector->list`,
//! `char-whitespace?`).

use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::Value;
use crate::runtime::codegen::opt::ir::inst::{Cmp, Helper, MemoryRegion, Op, Repr};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;

use super::Builder;

/// A `list-ref` with a constant index up to this unrolls into pair loads.
const MAX_UNROLLED_LIST_REF: i32 = 6;

/// The most arguments of an inline `string-append`: a call with more arguments
/// stays a call.
const MAX_APPEND_ARGS: usize = 4;

/// The inline shape of a recognized text native.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Text {
    /// `(list-ref list n)` for a constant `n`.
    ListRefConst(u8),
    ListRef,
    ListHead,
    StringEq,
    /// `(string-append a ...)` with this many arguments (1 to
    /// [`MAX_APPEND_ARGS`]).
    StringAppend(u8),
    Substring {
        has_end: bool,
    },
    ListToVector,
    VectorToList,
    CharWhitespace,
}

impl<'s, 'gc> Builder<'s, 'gc> {
    /// The shape of a call to the native `name` on `args`, if there is an
    /// inline lowering.
    pub(super) fn text_shape(&self, name: &str, args: &[Value]) -> Option<Text> {
        Some(match (name, args.len()) {
            ("list-ref", 2) => match self.fixnum_constant(args[1]) {
                Some(n) if (0..=MAX_UNROLLED_LIST_REF).contains(&n) => Text::ListRefConst(n as u8),
                _ => Text::ListRef,
            },
            ("list-head", 2) => Text::ListHead,
            ("string=?", 2) => Text::StringEq,
            ("string-append", n) if (1..=MAX_APPEND_ARGS).contains(&n) => {
                Text::StringAppend(n as u8)
            }
            ("substring", 2) => Text::Substring { has_end: false },
            ("substring", 3) => Text::Substring { has_end: true },
            ("list->vector", 1) => Text::ListToVector,
            ("vector->list", 1) => Text::VectorToList,
            ("char-whitespace?", 1) => Text::CharWhitespace,
            _ => return None,
        })
    }

    fn helper_call(&mut self, helper: Helper, args: &[Value], frame: FrameState) -> Value {
        self.value_op(
            Op::CallOp(helper.helper_ref()),
            args,
            Type::Any,
            Some(frame),
        )
    }

    /// The value of the text native `shape` on `args`.
    pub(super) fn text_lower(&mut self, shape: Text, args: &[Value], frame: FrameState) -> Value {
        match shape {
            Text::ListRefConst(n) => {
                let mut list = args[0];
                for _ in 0..n {
                    let pair = self.check_class(list, ClassRef::PAIR, frame);
                    list = self.pair_field(pair, 1);
                }
                let pair = self.check_class(list, ClassRef::PAIR, frame);
                self.pair_field(pair, 0)
            }
            Text::ListRef => self.helper_call(Helper::ListRef, args, frame),
            Text::ListHead => self.helper_call(Helper::ListHead, args, frame),
            Text::StringEq => self.helper_call(Helper::StringEq, args, frame),
            Text::StringAppend(1) => self.check_class(args[0], ClassRef::STRING, frame),
            Text::StringAppend(_) => {
                // Left to right, so a bad operand deopts before any
                // allocation is observable (there is none).
                let mut acc = self.helper_call(Helper::StringAppend, &args[..2], frame);
                for &next in &args[2..] {
                    acc = self.helper_call(Helper::StringAppend, &[acc, next], frame);
                }
                acc
            }
            Text::Substring { has_end } => {
                let end = if has_end {
                    args[2]
                } else {
                    // The two-argument form ends at the string's length.
                    let string = self.check_class(args[0], ClassRef::STRING, frame);
                    let len = self.value_op(
                        Op::Load {
                            field: (crate::runtime::sabi::STRING_LENGTH / 8) as u32,
                            region: MemoryRegion::Heap,
                        },
                        &[string],
                        Type::Int32,
                        None,
                    );
                    self.value_op(Op::Box(Repr::Int32), &[len], Type::Any, None)
                };
                self.helper_call(Helper::Substring, &[args[0], args[1], end], frame)
            }
            Text::ListToVector => self.helper_call(Helper::ListToVector, args, frame),
            Text::VectorToList => self.helper_call(Helper::VectorToList, args, frame),
            Text::CharWhitespace => self.char_whitespace(args[0], frame),
        }
    }

    fn pair_field(&mut self, pair: Value, field: u32) -> Value {
        self.value_op(
            Op::Load {
                field,
                region: MemoryRegion::Heap,
            },
            &[pair],
            Type::Any,
            None,
        )
    }

    /// `(char-whitespace? c)`: ASCII in line, anything above 127 through the
    /// helper.
    fn char_whitespace(&mut self, ch: Value, frame: FrameState) -> Value {
        let code = self.value_op(Op::CheckChar, &[ch], Type::Int32, Some(frame));
        let (join, result) = self.join_block(Type::Any);
        let yes = self.func.new_block();
        let no = self.func.new_block();
        let high = self.func.new_block();
        let low = self.func.new_block();
        let slow = self.func.new_block();
        let low_ge9 = self.func.new_block();
        let low_le13 = self.func.new_block();

        let above = self.test_i32(Cmp::Gt, code, 32);
        self.branch_to(above, high, low);
        // 33..=127 is never whitespace, 128 and up asks the helper.
        self.cur = high;
        let ascii = self.test_i32(Cmp::Lt, code, 128);
        self.branch_to(ascii, no, slow);
        // 0..=32 is whitespace for 32 and for 9..=13.
        self.cur = low;
        let space = self.test_i32(Cmp::Eq, code, 32);
        self.branch_to(space, yes, low_ge9);
        self.cur = low_ge9;
        let ge9 = self.test_i32(Cmp::Ge, code, 9);
        self.branch_to(ge9, low_le13, no);
        self.cur = low_le13;
        let le13 = self.test_i32(Cmp::Le, code, 13);
        self.branch_to(le13, yes, no);

        self.cur = yes;
        let t = self.bits(SchemeValue::VALUE_TRUE as u64);
        self.jump_with(join, &[t]);
        self.cur = no;
        let f = self.bits(SchemeValue::VALUE_FALSE as u64);
        self.jump_with(join, &[f]);
        self.cur = slow;
        let r = self.helper_call(Helper::CharWhitespace, &[ch], frame);
        self.jump_with(join, &[r]);
        self.cur = join;
        result
    }

    fn test_i32(&mut self, cmp: Cmp, code: Value, n: i32) -> Value {
        let n = self.const_i32(n);
        self.value_op(Op::CmpI32(cmp), &[code, n], Type::Bool, None)
    }
}

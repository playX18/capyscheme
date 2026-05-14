use crate::{
    compiler::ssa::primitive::Primitive,
    cps::term::BranchHint,
    runtime::value::{Str, Tuple, TypeCode8, Value, Vector},
};
use smallvec::smallvec;
use std::mem::{offset_of, size_of};

use super::constant::local_values;
use super::*;
struct LowLevelLowerer {
    next_value: u32,
    next_block: usize,
}

impl LowLevelLowerer {
    fn new<'gc>(procedure: &Procedure<'gc>) -> Self {
        let mut max_value = procedure.binding.0;
        if let Some(return_cont) = procedure.return_cont {
            max_value = max_value.max(return_cont.0);
        }
        let mut max_block = procedure.entry.0;
        for value in procedure
            .params
            .iter()
            .chain(procedure.variadic.iter())
            .chain(procedure.free_vars.iter())
            .copied()
        {
            max_value = max_value.max(value.0);
        }
        for block in &procedure.blocks {
            max_block = max_block.max(block.id.0);
            for value in block.params.iter().chain(block.variadic.iter()).copied() {
                max_value = max_value.max(value.0);
            }
            for instruction in &block.instructions {
                for def in instruction.defs() {
                    max_value = max_value.max(def.0);
                }
                for value in local_values(instruction.uses()) {
                    max_value = max_value.max(value.0);
                }
            }
            for value in local_values(block.terminator.uses()) {
                max_value = max_value.max(value.0);
            }
        }
        Self {
            next_value: max_value + 1,
            next_block: max_block + 1,
        }
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn fresh_block(&mut self) -> BlockId {
        let block = BlockId(self.next_block);
        self.next_block += 1;
        block
    }

    fn lower_procedure<'gc>(&mut self, mut procedure: Procedure<'gc>) -> Procedure<'gc> {
        let mut lowered = Vec::with_capacity(procedure.blocks.len());
        let blocks = std::mem::take(&mut procedure.blocks);
        for block in blocks {
            self.lower_block(block, &mut lowered);
        }
        procedure.blocks = lowered;
        procedure
    }

    fn lower_block<'gc>(&mut self, block: Block<'gc>, out: &mut Vec<Block<'gc>>) {
        let Block {
            id,
            params,
            variadic,
            instructions,
            terminator,
            source: block_source,
        } = block;

        let mut lowered = Vec::with_capacity(instructions.len());
        let mut iter = instructions.into_iter();
        while let Some(instruction) = iter.next() {
            match instruction {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::vector_ref && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_indexed_ref_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        TypeCode8::VECTOR.bits(),
                        offset_of!(Vector, length) as i32,
                        Vector::OFFSET_OF_DATA as i64,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::tuple_ref && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_indexed_ref_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        TypeCode8::TUPLE.bits(),
                        offset_of!(Tuple, length) as i32,
                        offset_of!(Tuple, data) as i64,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::string_length && args.len() == 1 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_type_checked_length_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args[0],
                        TypeCode8::STRING.bits(),
                        offset_of!(Str, length) as i32,
                        source,
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if args.len() == 1 => {
                    if let Some(typecode) = low_type_predicate_prim(prim) {
                        lowered.push(Instruction::low_prim_call(
                            dst,
                            LowPrim::HasType8(typecode),
                            args,
                            source,
                        ));
                    } else {
                        lowered.push(Instruction::PrimCall {
                            dst,
                            prim,
                            args,
                            source,
                        });
                    }
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if prim == Primitive::plus && args.len() == 2 => {
                    let suffix = iter.collect::<Vec<_>>();
                    self.lower_fixnum_binary_block(
                        Block {
                            id,
                            params,
                            variadic,
                            instructions: lowered,
                            terminator,
                            source: block_source,
                        },
                        dst,
                        prim,
                        args,
                        source,
                        LowPrim::IAddOverflow(LowType::I32),
                        suffix,
                        out,
                    );
                    return;
                }
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } if args.len() == 2 => {
                    if let Some((ty, op)) = low_integer_binary_prim(prim) {
                        let lhs = self.fresh_value();
                        let rhs = self.fresh_value();
                        lowered.push(Instruction::low_prim_call(
                            lhs,
                            LowPrim::IReduce(ty),
                            vec![args[0]],
                            source,
                        ));
                        lowered.push(Instruction::low_prim_call(
                            rhs,
                            LowPrim::IReduce(ty),
                            vec![args[1]],
                            source,
                        ));
                        lowered.push(Instruction::low_prim_call(
                            dst,
                            op,
                            vec![LinearAtom::Local(lhs), LinearAtom::Local(rhs)],
                            source,
                        ));
                    } else {
                        lowered.push(Instruction::PrimCall {
                            dst,
                            prim,
                            args,
                            source,
                        });
                    }
                }
                other => lowered.push(other),
            }
        }

        out.push(Block {
            id,
            params,
            variadic,
            instructions: lowered,
            terminator,
            source: block_source,
        });
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_type_checked_length_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        arg: LinearAtom<'gc>,
        typecode: u8,
        length_offset: i32,
        source: Value<'gc>,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();
        let type_ok = self.fresh_value();
        let raw_len = self.fresh_value();
        let tagged_len = self.fresh_value();

        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            type_ok,
            LowPrim::HasType8(typecode),
            vec![arg],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(type_ok),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(
                    raw_len,
                    LowPrim::Load {
                        ty: LowType::I64,
                        offset: length_offset,
                    },
                    vec![arg],
                    source,
                ),
                Instruction::low_prim_call(
                    tagged_len,
                    LowPrim::TagFixnum,
                    vec![LinearAtom::Local(raw_len)],
                    source,
                ),
            ],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(tagged_len)],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args: vec![arg],
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_indexed_ref_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        typecode: u8,
        length_offset: i32,
        data_offset: i64,
        source: Value<'gc>,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let index_check = self.fresh_block();
        let bounds_check = self.fresh_block();
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();

        let type_ok = self.fresh_value();
        let index_is_fixnum = self.fresh_value();
        let raw_index = self.fresh_value();
        let index64 = self.fresh_value();
        let length = self.fresh_value();
        let in_bounds = self.fresh_value();
        let index_offset = self.fresh_value();
        let data_ptr = self.fresh_value();
        let elem_ptr = self.fresh_value();
        let elem = self.fresh_value();

        let vector = args[0];
        let index = args[1];
        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            type_ok,
            LowPrim::HasType8(typecode),
            vec![vector],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(type_ok),
                consequent: BranchTarget::Local {
                    block: index_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: index_check,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::low_prim_call(
                index_is_fixnum,
                LowPrim::IsFixnum,
                vec![index],
                source,
            )],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(index_is_fixnum),
                consequent: BranchTarget::Local {
                    block: bounds_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: bounds_check,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(raw_index, LowPrim::UntagFixnum, vec![index], source),
                Instruction::low_prim_call(
                    index64,
                    LowPrim::ZExt(LowType::I64),
                    vec![LinearAtom::Local(raw_index)],
                    source,
                ),
                Instruction::low_prim_call(
                    length,
                    LowPrim::Load {
                        ty: LowType::I64,
                        offset: length_offset,
                    },
                    vec![vector],
                    source,
                ),
                Instruction::low_prim_call(
                    in_bounds,
                    LowPrim::ICmp(LowIntPredicate::UnsignedLessThan, LowType::I64),
                    vec![LinearAtom::Local(index64), LinearAtom::Local(length)],
                    source,
                ),
            ],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(in_bounds),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(
                    index_offset,
                    LowPrim::IMulImm(LowType::I64, size_of::<Value>() as i64),
                    vec![LinearAtom::Local(index64)],
                    source,
                ),
                Instruction::low_prim_call(
                    data_ptr,
                    LowPrim::IAddImm(LowType::Ptr, data_offset),
                    vec![vector],
                    source,
                ),
                Instruction::low_prim_call(
                    elem_ptr,
                    LowPrim::IAdd(LowType::Ptr),
                    vec![LinearAtom::Local(data_ptr), LinearAtom::Local(index_offset)],
                    source,
                ),
                Instruction::low_prim_call(
                    elem,
                    LowPrim::Load {
                        ty: LowType::Value,
                        offset: 0,
                    },
                    vec![LinearAtom::Local(elem_ptr)],
                    source,
                ),
            ],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(elem)],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args,
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_fixnum_binary_block<'gc>(
        &mut self,
        block: Block<'gc>,
        dst: ValueId,
        prim: Primitive,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
        overflow_op: LowPrim,
        suffix: Vec<Instruction<'gc>>,
        out: &mut Vec<Block<'gc>>,
    ) {
        let rhs_check = self.fresh_block();
        let fast = self.fresh_block();
        let slow = self.fresh_block();
        let merge = self.fresh_block();

        let lhs_is_fixnum = self.fresh_value();
        let rhs_is_fixnum = self.fresh_value();
        let lhs_raw = self.fresh_value();
        let rhs_raw = self.fresh_value();
        let overflow = self.fresh_value();
        let raw_result = self.fresh_value();
        let tagged_result = self.fresh_value();

        let lhs = args[0];
        let rhs = args[1];
        let mut entry_instructions = block.instructions;
        entry_instructions.push(Instruction::low_prim_call(
            lhs_is_fixnum,
            LowPrim::IsFixnum,
            vec![lhs],
            source,
        ));
        out.push(Block {
            id: block.id,
            params: block.params,
            variadic: block.variadic,
            instructions: entry_instructions,
            terminator: Terminator::Branch {
                test: LinearAtom::Local(lhs_is_fixnum),
                consequent: BranchTarget::Local {
                    block: rhs_check,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source: block.source,
        });

        out.push(Block {
            id: rhs_check,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::low_prim_call(
                rhs_is_fixnum,
                LowPrim::IsFixnum,
                vec![rhs],
                source,
            )],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(rhs_is_fixnum),
                consequent: BranchTarget::Local {
                    block: fast,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                hints: [BranchHint::Hot, BranchHint::Cold],
            },
            source,
        });

        out.push(Block {
            id: fast,
            params: vec![],
            variadic: None,
            instructions: vec![
                Instruction::low_prim_call(lhs_raw, LowPrim::UntagFixnum, vec![lhs], source),
                Instruction::low_prim_call(rhs_raw, LowPrim::UntagFixnum, vec![rhs], source),
                Instruction::low_prim_multi_call(
                    smallvec![raw_result, overflow],
                    overflow_op,
                    vec![LinearAtom::Local(lhs_raw), LinearAtom::Local(rhs_raw)],
                    source,
                ),
                Instruction::low_prim_call(
                    tagged_result,
                    LowPrim::TagFixnum,
                    vec![LinearAtom::Local(raw_result)],
                    source,
                ),
            ],
            terminator: Terminator::Branch {
                test: LinearAtom::Local(overflow),
                consequent: BranchTarget::Local {
                    block: slow,
                    args: vec![],
                },
                alternative: BranchTarget::Local {
                    block: merge,
                    args: vec![LinearAtom::Local(tagged_result)],
                },
                hints: [BranchHint::Cold, BranchHint::Hot],
            },
            source,
        });

        out.push(Block {
            id: slow,
            params: vec![],
            variadic: None,
            instructions: vec![Instruction::RuntimePrimCall {
                dst,
                prim,
                args,
                source,
            }],
            terminator: Terminator::Jump {
                target: merge,
                args: vec![LinearAtom::Local(dst)],
            },
            source,
        });

        self.lower_block(
            Block {
                id: merge,
                params: vec![dst],
                variadic: None,
                instructions: suffix,
                terminator: block.terminator,
                source,
            },
            out,
        );
    }
}

fn low_integer_binary_prim(prim: Primitive) -> Option<(LowType, LowPrim)> {
    let name = prim.name();
    let (rest, ty) = integer_prefix(name)?;
    let op = match rest {
        "+" => LowPrim::IAdd(ty),
        "-" => LowPrim::ISub(ty),
        "*" => LowPrim::IMul(ty),
        "/" => LowPrim::IDiv(ty),
        "%" => LowPrim::IRem(ty),
        "<<" => LowPrim::IShl(ty),
        ">>" => LowPrim::IShr(ty),
        "&" => LowPrim::IAnd(ty),
        "|" => LowPrim::IOr(ty),
        "^" => LowPrim::IXor(ty),
        _ => return None,
    };
    Some((ty, op))
}

fn integer_prefix(name: &str) -> Option<(&str, LowType)> {
    [
        ("s64", LowType::I64),
        ("s32", LowType::I32),
        ("s16", LowType::I16),
        ("s8", LowType::I8),
        ("u64", LowType::U64),
        ("u32", LowType::U32),
        ("u16", LowType::U16),
        ("u8", LowType::U8),
    ]
    .into_iter()
    .find_map(|(prefix, ty)| name.strip_prefix(prefix).map(|rest| (rest, ty)))
}

fn low_type_predicate_prim(prim: Primitive) -> Option<u8> {
    match prim {
        Primitive::is_vector => Some(TypeCode8::VECTOR.bits()),
        Primitive::is_bytevector => Some(TypeCode8::BYTEVECTOR.bits()),
        Primitive::is_string => Some(TypeCode8::STRING.bits()),
        Primitive::is_tuple => Some(TypeCode8::TUPLE.bits()),
        Primitive::is_pair => Some(TypeCode8::PAIR.bits()),
        Primitive::is_procedure => Some(TypeCode8::CLOSURE.bits()),
        _ => None,
    }
}

pub(super) fn lower_low_level_primitives<'gc>(procedure: Procedure<'gc>) -> Procedure<'gc> {
    LowLevelLowerer::new(&procedure).lower_procedure(procedure)
}

//! Bytecode verifier.
//!
//! This verifier helps us avoid interpreter doing checked accesses.
//! We simply establish safety of bytecode during FASL load times. This verifier
//! is best effort to verify safety of programs.

use std::collections::{HashMap, HashSet};

use super::def::{self, Instruction, OpcodeId};
use crate::runtime::prims::def::Primitive;

/// What the verifier needs to know about a code block besides its bytes.
pub struct CodeShape<'a> {
    pub code: &'a [u8],
    pub pool: &'a [u32],
    pub nconsts: usize,
    pub frame_size: usize,
    pub metadata_counts: &'a [u32; def::NUM_METADATA_KINDS],
}

fn fail<T>(pc: usize, message: impl std::fmt::Display) -> Result<T, String> {
    Err(format!("bytecode verification failed at {pc}: {message}"))
}

/// Check that `shape` is safe to interpret.
pub fn verify(shape: &CodeShape<'_>) -> Result<(), String> {
    let mut order: Vec<usize> = Vec::new();
    let mut decoded: HashMap<usize, Instruction> = HashMap::new();
    let mut pc = 0usize;
    while pc < shape.code.len() {
        let Some((ins, _, len)) = Instruction::decode_slice(shape.code, pc) else {
            return fail(pc, "undecodable instruction");
        };
        order.push(pc);
        decoded.insert(pc, ins);
        pc += len;
    }
    if order.is_empty() {
        return fail(0, "code block does not start with an entry instruction");
    }
    // The compiler keeps frames below 64K slots, the size of a stack
    // segment.
    if shape.frame_size >= 1 << 16 {
        return fail(
            0,
            format!("frame of {} slots is too large", shape.frame_size),
        );
    }
    // Every metadata entry belongs to an instruction of at least two bytes,
    // so a larger count can only come from a corrupt file. The counts size
    // the code buffer's metadata area.
    for (kind, count) in shape.metadata_counts.iter().enumerate() {
        if *count as usize > shape.code.len() {
            return fail(
                0,
                format!(
                    "metadata kind {kind} has {count} entries for {} code bytes",
                    shape.code.len()
                ),
            );
        }
    }
    let at = |pc: usize| -> &Instruction { &decoded[&pc] };
    let opcode_at = |pc: usize| -> OpcodeId { at(pc).opcode_id() };

    // Jumps and switches may land on any instruction except the entry and
    // the return points: those expect `w` and a frame that only a call or a
    // return sets up.
    let landing: HashSet<usize> = order
        .iter()
        .copied()
        .filter(|pc| !opcode_at(*pc).is_entry() && !opcode_at(*pc).is_return_point())
        .collect();

    // Running off the end would execute the aux pool.
    let last = *order.last().expect("checked non-empty above");
    if !matches!(
        opcode_at(last),
        OpcodeId::OpJump
            | OpcodeId::OpReturn
            | OpcodeId::OpReturnValues
            | OpcodeId::OpTailCall
            | OpcodeId::OpTailCallSelf
            | OpcodeId::OpRaise
    ) {
        return fail(
            last,
            "code block ends with an instruction that falls through",
        );
    }

    if !opcode_at(order[0]).is_entry() {
        return fail(0, "code block does not start with an entry instruction");
    }

    let register = |pc: usize, index: u32| -> Result<(), String> {
        if index as u64 >= shape.frame_size as u64 {
            fail(
                pc,
                format!("register {index} outside frame of {}", shape.frame_size),
            )
        } else {
            Ok(())
        }
    };
    let constant = |pc: usize, index: u32| -> Result<(), String> {
        if index as usize >= shape.nconsts {
            fail(pc, format!("constant {index} out of range"))
        } else {
            Ok(())
        }
    };
    let target = |pc: usize, disp: i32| -> Result<(), String> {
        let destination = pc as i64 + disp as i64;
        if destination < 0 || !landing.contains(&(destination as usize)) {
            fail(pc, format!("jump to {destination} is not a jump target"))
        } else {
            Ok(())
        }
    };
    // `limit` is the first register the mask must not name: the frame size,
    // or for a return point the call's `base - 1`, which is reserved and raw
    // across the call.
    let mask = |pc: usize, index: u32, limit: u32| -> Result<(), String> {
        let Some(nwords) = shape.pool.get(index as usize) else {
            return fail(pc, format!("liveness mask @{index} outside the pool"));
        };
        let start = index as usize + 1;
        if start as u64 + *nwords as u64 > shape.pool.len() as u64 {
            return fail(pc, format!("liveness mask @{index} runs past the pool"));
        }
        let words = &shape.pool[start..start + *nwords as usize];
        // Bits 0 and 1 name the return address and the closure, which the
        // walker never reads from the mask.
        let highest = words.iter().enumerate().rev().find_map(|(word, bits)| {
            let bits = if word == 0 { bits & !0b11 } else { *bits };
            (bits != 0).then(|| word as u64 * 32 + 31 - bits.leading_zeros() as u64)
        });
        if let Some(highest) = highest
            && highest >= limit as u64
        {
            return fail(
                pc,
                format!("liveness mask @{index} names register {highest} at or above {limit}"),
            );
        }
        Ok(())
    };
    let prim = |pc: usize, id: u32| -> Result<(), String> {
        let ok = u16::try_from(id)
            .ok()
            .and_then(Primitive::from_id)
            .is_some();
        if ok {
            Ok(())
        } else {
            fail(pc, format!("unknown primitive {id}"))
        }
    };

    for index in 0..order.len() {
        let pc = order[index];
        let ins = at(pc);
        let opcode = opcode_at(pc);
        let next = order.get(index + 1).copied();
        let previous = index.checked_sub(1).map(|index| opcode_at(order[index]));
        // A return point is entered only by returning to the call before it.
        match opcode {
            OpcodeId::OpReturnPoint | OpcodeId::OpReturnPointForward
                if !matches!(previous, Some(OpcodeId::OpCall | OpcodeId::OpCallSelf)) =>
            {
                return fail(pc, "return point does not follow a call");
            }
            OpcodeId::OpReturnPointValues | OpcodeId::OpReturnPointValuesRest
                if previous != Some(OpcodeId::OpReturnPointForward) =>
            {
                return fail(pc, "multiple-value entry does not follow retpt_fwd");
            }
            _ => {}
        }
        if pc != order[0] && opcode.is_entry() {
            return fail(pc, "entry instruction in the middle of a code block");
        }

        if let Some(id) = ins.metadata_id() {
            let Some(kind) = opcode.metadata_kind() else {
                return fail(pc, "metadata id on an opcode with no metadata kind");
            };
            if id.0 >= shape.metadata_counts[kind] {
                return fail(pc, format!("metadata id {} out of range", id.0));
            }
        }
        if let Some(offset) = ins.jump_offset() {
            target(pc, offset as i32)?;
        }

        match ins {
            Instruction::OpNop(_) => {}
            Instruction::OpEnter(op) => {
                if op.frame.0 as u64 > shape.frame_size as u64 {
                    return fail(pc, "entry frame size exceeds the code block's");
                }
            }
            Instruction::OpEnterRest(op) => {
                register(pc, op.cnt.0)?;
                register(pc, op.base.0)?;
                if op.frame.0 as u64 > shape.frame_size as u64 {
                    return fail(pc, "entry frame size exceeds the code block's");
                }
            }
            Instruction::OpReturnPoint(op) => {
                register(pc, op.frame.0)?;
                register(pc, op.dst.0)?;
                mask(pc, op.mask.0, op.frame.0.saturating_sub(1))?;
                if op.mv.0 != def::MV_ERROR && op.mv.0 != def::MV_IGNORE {
                    return fail(pc, format!("bad retpt mv {}", op.mv.0));
                }
            }
            Instruction::OpReturnPointForward(op) => {
                register(pc, op.frame.0)?;
                mask(pc, op.mask.0, op.frame.0.saturating_sub(1))?;
                match next.map(opcode_at) {
                    Some(OpcodeId::OpReturnPointValues | OpcodeId::OpReturnPointValuesRest) => {
                        let follower = next.map(at).expect("checked above");
                        let frame = match follower {
                            Instruction::OpReturnPointValues(op) => op.frame.0,
                            Instruction::OpReturnPointValuesRest(op) => op.frame.0,
                            _ => unreachable!("checked above"),
                        };
                        if frame != op.frame.0 {
                            return fail(pc, "retpt_fwd entry has another frame size");
                        }
                    }
                    _ => return fail(pc, "retpt_fwd is not followed by a multiple-value entry"),
                }
            }
            Instruction::OpReturnPointValues(_) | Instruction::OpReturnPointValuesRest(_) => {
                let (nvals, frame) = match ins {
                    Instruction::OpReturnPointValues(op) => (op.nvals.0, op.frame.0),
                    Instruction::OpReturnPointValuesRest(op) => (op.nvals.0, op.frame.0),
                    _ => unreachable!("matched above"),
                };
                // The values arrive in the callee's frame at `r[frame+2..]`,
                // which the callee sized. Reading them out is a `mov`, and
                // each `mov` is range-checked on its own. A frame cannot hold
                // more values than the frame limit, so a larger count is malformed.
                if nvals as u64 >= 1 << 16 {
                    return fail(pc, format!("value count {nvals} exceeds the frame limit"));
                }
                register(pc, frame)?;
            }
            Instruction::OpMove(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpLoadConstant(op) => {
                register(pc, op.dst.0)?;
                constant(pc, op.constant.0)?;
            }
            Instruction::OpLoadImmediate(op) => {
                register(pc, op.dst.0)?;
            }
            Instruction::OpLoadRaw(op) => {
                register(pc, op.dst.0)?;
            }
            Instruction::OpCellRef(op) => {
                register(pc, op.dst.0)?;
                constant(pc, op.constant.0)?;
            }
            Instruction::OpCellSet(op) => {
                register(pc, op.src.0)?;
                constant(pc, op.constant.0)?;
            }
            Instruction::OpRuntimeProcedure(op) => {
                register(pc, op.dst.0)?;
                if crate::runtime::interp::runtime_procs::RuntimeProc::from_id(op.id.0).is_none() {
                    return fail(pc, format!("unknown runtime procedure {}", op.id.0));
                }
            }
            Instruction::OpClosure(op) => {
                register(pc, op.dst.0)?;
                constant(pc, op.constant.0)?;
            }
            Instruction::OpClosureEnv(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.env.0)?;
                constant(pc, op.constant.0)?;
            }
            Instruction::OpFreeRef(op) => {
                register(pc, op.dst.0)?;
            }
            Instruction::OpClosureRef(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.closure.0)?;
            }
            Instruction::OpClosureSet(op) => {
                register(pc, op.closure.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpMakeEnv(op) => {
                register(pc, op.dst.0)?;
            }
            Instruction::OpEnvRef(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.env.0)?;
            }
            Instruction::OpEnvSet(op) => {
                register(pc, op.env.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpJump(_) => {}
            Instruction::OpJumpIfTrue(_) | Instruction::OpJumpIfFalse(_) => {
                let cond = match ins {
                    Instruction::OpJumpIfTrue(op) => op.cond.0,
                    Instruction::OpJumpIfFalse(op) => op.cond.0,
                    _ => unreachable!("matched above"),
                };
                register(pc, cond)?;
            }
            Instruction::OpJumpIfNull(_)
            | Instruction::OpJumpIfNotNull(_)
            | Instruction::OpJumpIfPair(_)
            | Instruction::OpJumpIfNotPair(_)
            | Instruction::OpJumpIfFixnum(_)
            | Instruction::OpJumpIfNotFixnum(_)
            | Instruction::OpJumpIfUndefined(_)
            | Instruction::OpJumpIfNotUndefined(_) => {
                let src = match ins {
                    Instruction::OpJumpIfNull(op) => op.src.0,
                    Instruction::OpJumpIfNotNull(op) => op.src.0,
                    Instruction::OpJumpIfPair(op) => op.src.0,
                    Instruction::OpJumpIfNotPair(op) => op.src.0,
                    Instruction::OpJumpIfFixnum(op) => op.src.0,
                    Instruction::OpJumpIfNotFixnum(op) => op.src.0,
                    Instruction::OpJumpIfUndefined(op) => op.src.0,
                    Instruction::OpJumpIfNotUndefined(op) => op.src.0,
                    _ => unreachable!("matched above"),
                };
                register(pc, src)?;
            }
            Instruction::OpJumpIfEq(_) | Instruction::OpJumpIfNotEq(_) => {
                let (lhs, rhs) = match ins {
                    Instruction::OpJumpIfEq(op) => (op.lhs.0, op.rhs.0),
                    Instruction::OpJumpIfNotEq(op) => (op.lhs.0, op.rhs.0),
                    _ => unreachable!("matched above"),
                };
                register(pc, lhs)?;
                register(pc, rhs)?;
            }
            Instruction::OpJumpIfLess(_)
            | Instruction::OpJumpIfLessEqual(_)
            | Instruction::OpJumpIfNumberEqual(_)
            | Instruction::OpJumpIfGreaterEqual(_)
            | Instruction::OpJumpIfGreater(_)
            | Instruction::OpJumpIfNotLess(_)
            | Instruction::OpJumpIfNotLessEqual(_)
            | Instruction::OpJumpIfNotNumberEqual(_)
            | Instruction::OpJumpIfNotGreaterEqual(_)
            | Instruction::OpJumpIfNotGreater(_) => {
                let (lhs, rhs) = jump_regs(ins);
                register(pc, lhs)?;
                register(pc, rhs)?;
            }
            Instruction::OpSwitchFixnum(op) => {
                register(pc, op.src.0)?;
                verify_int_switch(shape, &landing, pc, op.table.0)?;
            }
            Instruction::OpSwitchChar(op) => {
                register(pc, op.src.0)?;
                verify_int_switch(shape, &landing, pc, op.table.0)?;
            }
            Instruction::OpSwitchSymbol(op) => {
                register(pc, op.src.0)?;
                verify_sym_switch(shape, &landing, pc, op.table.0)?;
            }
            Instruction::OpLoopHint(op) => {
                mask(pc, op.mask.0, shape.frame_size as u32)?;
            }
            Instruction::OpCall(op) => {
                if op.base.0 == 0 {
                    return fail(pc, "call with base 0 has no reserved r[base-1] slot");
                }
                if op.base.0 as u64 + 1 + op.argc.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "call frame outside the frame");
                }
                match next.map(at) {
                    Some(Instruction::OpReturnPoint(next)) if next.frame.0 == op.base.0 => {}
                    Some(Instruction::OpReturnPointForward(next)) if next.frame.0 == op.base.0 => {}
                    _ => return fail(pc, "call is not followed by its return point"),
                }
            }
            Instruction::OpTailCall(op) => {
                if 1 + op.argc.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "tail call arguments outside the frame");
                }
            }
            Instruction::OpCallSelf(op) => {
                if op.base.0 == 0 {
                    return fail(pc, "call with base 0 has no reserved r[base-1] slot");
                }
                if op.base.0 as u64 + 1 + op.argc.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "call frame outside the frame");
                }
                match next.map(at) {
                    Some(Instruction::OpReturnPoint(next)) if next.frame.0 == op.base.0 => {}
                    Some(Instruction::OpReturnPointForward(next)) if next.frame.0 == op.base.0 => {}
                    _ => return fail(pc, "call is not followed by its return point"),
                }
            }
            Instruction::OpTailCallSelf(op) => {
                if 1 + op.argc.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "tail call arguments outside the frame");
                }
            }
            Instruction::OpReturn(op) => {
                register(pc, op.src.0)?;
                register(pc, 2)?;
            }
            Instruction::OpReturnValues(op) => {
                if 1 + op.count.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "returned values outside the frame");
                }
            }
            Instruction::OpRaise(op) => {
                if 1 + op.count.0 as u64 >= shape.frame_size as u64 {
                    return fail(pc, "raise irritants outside the frame");
                }
            }
            Instruction::OpRestRef(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.cnt.0)?;
                register(pc, op.base.0)?;
            }
            Instruction::OpRestLen(_) | Instruction::OpRestNull(_) | Instruction::OpRestPair(_) => {
                let (dst, cnt) = match ins {
                    Instruction::OpRestLen(op) => (op.dst.0, op.cnt.0),
                    Instruction::OpRestNull(op) => (op.dst.0, op.cnt.0),
                    Instruction::OpRestPair(op) => (op.dst.0, op.cnt.0),
                    _ => unreachable!("matched above"),
                };
                register(pc, dst)?;
                register(pc, cnt)?;
            }
            Instruction::OpRestList(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.cnt.0)?;
                register(pc, op.base.0)?;
            }
            Instruction::OpValuesRest(op) => {
                register(pc, op.base.0)?;
            }
            Instruction::OpCallPrimitive0(op) => {
                register(pc, op.dst.0)?;
                prim(pc, op.primitive.0)?;
            }
            Instruction::OpCallPrimitive1(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.a.0)?;
                prim(pc, op.primitive.0)?;
            }
            Instruction::OpCallPrimitive2(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.a.0)?;
                register(pc, op.b.0)?;
                prim(pc, op.primitive.0)?;
            }
            Instruction::OpCallPrimitive3(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.a.0)?;
                register(pc, op.b.0)?;
                register(pc, op.c.0)?;
                prim(pc, op.primitive.0)?;
            }
            Instruction::OpCallPrimitiveN(op) => {
                register(pc, op.dst.0)?;
                if op.first.0 as u64 + op.argc.0 as u64 > shape.frame_size as u64 {
                    return fail(pc, "call_primitive_n arguments outside the frame");
                }
                prim(pc, op.primitive.0)?;
            }
            Instruction::OpCar(_) | Instruction::OpCdr(_) => {
                let (dst, src) = match ins {
                    Instruction::OpCar(op) => (op.dst.0, op.src.0),
                    Instruction::OpCdr(op) => (op.dst.0, op.src.0),
                    _ => unreachable!("matched above"),
                };
                register(pc, dst)?;
                register(pc, src)?;
            }
            Instruction::OpCons(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.car.0)?;
                register(pc, op.cdr.0)?;
            }
            Instruction::OpSetCar(_) | Instruction::OpSetCdr(_) => {
                let (pair, src) = match ins {
                    Instruction::OpSetCar(op) => (op.pair.0, op.src.0),
                    Instruction::OpSetCdr(op) => (op.pair.0, op.src.0),
                    _ => unreachable!("matched above"),
                };
                register(pc, pair)?;
                register(pc, src)?;
            }
            Instruction::OpEq(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.lhs.0)?;
                register(pc, op.rhs.0)?;
            }
            Instruction::OpNot(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpAdd(_)
            | Instruction::OpSub(_)
            | Instruction::OpMul(_)
            | Instruction::OpLess(_)
            | Instruction::OpLessEqual(_)
            | Instruction::OpNumberEqual(_)
            | Instruction::OpGreaterEqual(_)
            | Instruction::OpGreater(_)
            | Instruction::OpDiv(_)
            | Instruction::OpQuotient(_)
            | Instruction::OpRemainder(_)
            | Instruction::OpModulo(_) => {
                let (dst, lhs, rhs) = arith_regs(ins);
                register(pc, dst)?;
                register(pc, lhs)?;
                register(pc, rhs)?;
            }
            Instruction::OpAddImmediate(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpNegate(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpAbs(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpSqrt(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpVectorRef(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.vector.0)?;
                register(pc, op.index.0)?;
            }
            Instruction::OpVectorSet(op) => {
                register(pc, op.vector.0)?;
                register(pc, op.index.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpVectorLength(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpVariableRef(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.var.0)?;
            }
            Instruction::OpVariableSet(op) => {
                register(pc, op.var.0)?;
                register(pc, op.src.0)?;
            }
            Instruction::OpGetSlot(op) => {
                register(pc, op.dst.0)?;
                register(pc, op.obj.0)?;
                constant(pc, op.name.0)?;
            }
            Instruction::OpPutSlot(op) => {
                register(pc, op.obj.0)?;
                register(pc, op.src.0)?;
                constant(pc, op.name.0)?;
            }
            Instruction::OpMoveTwo(op) => {
                register(pc, op.dst1.0)?;
                register(pc, op.src1.0)?;
                register(pc, op.dst2.0)?;
                register(pc, op.src2.0)?;
            }
            Instruction::OpAddImmediateMove(op) => {
                register(pc, op.tmp.0)?;
                register(pc, op.src.0)?;
                register(pc, op.dst.0)?;
            }
            Instruction::OpVectorRefMul(op) => {
                register(pc, op.vdst.0)?;
                register(pc, op.vector.0)?;
                register(pc, op.index.0)?;
                register(pc, op.mdst.0)?;
                register(pc, op.lhs.0)?;
                register(pc, op.rhs.0)?;
            }
        }
    }
    Ok(())
}

impl OpcodeId {
    pub fn is_entry(self) -> bool {
        matches!(self, OpcodeId::OpEnter | OpcodeId::OpEnterRest)
    }

    pub const fn is_return_point(self) -> bool {
        matches!(
            self,
            OpcodeId::OpReturnPoint
                | OpcodeId::OpReturnPointForward
                | OpcodeId::OpReturnPointValues
                | OpcodeId::OpReturnPointValuesRest
        )
    }
}

/// `(lhs, rhs)` of a numeric compare-and-jump.
fn jump_regs(ins: &Instruction) -> (u32, u32) {
    match ins {
        Instruction::OpJumpIfLess(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfLessEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNumberEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfGreaterEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfGreater(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNotLess(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNotLessEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNotNumberEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNotGreaterEqual(op) => (op.lhs.0, op.rhs.0),
        Instruction::OpJumpIfNotGreater(op) => (op.lhs.0, op.rhs.0),
        _ => unreachable!("not a numeric jump"),
    }
}

/// `(dst, lhs, rhs)` of a binary arithmetic or value comparison instruction.
fn arith_regs(ins: &Instruction) -> (u32, u32, u32) {
    match ins {
        Instruction::OpAdd(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpSub(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpMul(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpLess(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpLessEqual(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpNumberEqual(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpGreaterEqual(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpGreater(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpDiv(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpQuotient(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpRemainder(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        Instruction::OpModulo(op) => (op.dst.0, op.lhs.0, op.rhs.0),
        _ => unreachable!("not an arithmetic instruction"),
    }
}

fn verify_int_switch(
    shape: &CodeShape<'_>,
    landing: &HashSet<usize>,
    pc: usize,
    table: u32,
) -> Result<(), String> {
    let table = table as usize;
    let Some(count_word) = shape.pool.get(table + 1).copied() else {
        return fail(pc, "switch table outside the pool");
    };
    let check = |pc: usize, disp: u32| {
        let destination = pc as i64 + disp as i32 as i64;
        if destination < 0 || !landing.contains(&(destination as usize)) {
            fail(
                pc,
                format!("switch target {destination} is not a jump target"),
            )
        } else {
            Ok(())
        }
    };
    check(pc, shape.pool[table])?;
    if def::switch_table_is_dense(count_word) {
        let span = def::switch_table_len(count_word);
        if table as u64 + 3 + span as u64 > shape.pool.len() as u64 {
            return fail(pc, "switch table runs past the pool");
        }
        if shape.pool.get(table + 2).is_none() {
            return fail(pc, "switch table outside the pool");
        }
        for entry in 0..span {
            check(pc, shape.pool[table + 3 + entry])?;
        }
    } else {
        let count = count_word as usize;
        if table as u64 + 2 + 2 * count as u64 > shape.pool.len() as u64 {
            return fail(pc, "switch table runs past the pool");
        }
        for case in 0..count {
            check(pc, shape.pool[table + 2 + 2 * case + 1])?;
        }
    }
    Ok(())
}

fn verify_sym_switch(
    shape: &CodeShape<'_>,
    landing: &HashSet<usize>,
    pc: usize,
    table: u32,
) -> Result<(), String> {
    let table = table as usize;
    let Some(count) = shape.pool.get(table + 1).copied() else {
        return fail(pc, "switch table outside the pool");
    };
    if table as u64 + 4 + 3 * count as u64 > shape.pool.len() as u64 {
        return fail(pc, "switch table runs past the pool");
    }
    if shape.pool.get(table + 3).is_none() {
        return fail(pc, "switch table outside the pool");
    }
    let check = |pc: usize, disp: u32| {
        let destination = pc as i64 + disp as i32 as i64;
        if destination < 0 || !landing.contains(&(destination as usize)) {
            fail(
                pc,
                format!("switch target {destination} is not a jump target"),
            )
        } else {
            Ok(())
        }
    };
    check(pc, shape.pool[table])?;
    for case in 0..count as usize {
        check(pc, shape.pool[table + 4 + 3 * case + 2])?;
    }
    Ok(())
}

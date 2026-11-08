//! Register liveness of a procedure's bytecode.

use std::ops::Range;

use crate::bytecode::def::{Instruction, switch_table_is_dense, switch_table_len};

/// A basic block
#[derive(Clone, Debug)]
pub struct Block {
    /// The pc of the first instruction.
    pub start: usize,
    /// The pc just past the last instruction.
    pub end: usize,
    /// The blocks control can reach from the last instruction.
    pub successors: Vec<usize>,
    /// The instructions of this block, as indices into the procedure's list.
    insts: Range<usize>,
}

/// Live registers of a procedure at every block boundary and instruction.
#[derive(Debug)]
pub struct Liveness {
    frame_size: usize,
    /// Words per register set.
    words: usize,
    blocks: Vec<Block>,
    /// The instruction index of each pc, or `u32::MAX` if no instruction
    /// starts there.
    inst_at: Vec<u32>,
    /// Live-before set of each instruction.
    live_before: Vec<u64>,
    live_in: Vec<u64>,
    live_out: Vec<u64>,
}

/// A borrowed set of registers.
#[derive(Clone, Copy, Debug)]
pub struct RegSet<'a> {
    bits: &'a [u64],
}

impl RegSet<'_> {
    pub fn contains(&self, reg: usize) -> bool {
        self.bits
            .get(reg / 64)
            .is_some_and(|word| (word >> (reg % 64)) & 1 != 0)
    }

    pub fn is_empty(&self) -> bool {
        self.bits.iter().all(|word| *word == 0)
    }
}

impl<'a> RegSet<'a> {
    /// The registers in the set, in increasing order.
    pub fn iter(self) -> impl Iterator<Item = usize> + 'a {
        self.bits.iter().enumerate().flat_map(|(index, &word)| {
            let mut word = word;
            std::iter::from_fn(move || {
                if word == 0 {
                    return None;
                }
                let bit = word.trailing_zeros() as usize;
                word &= word - 1;
                Some(index * 64 + bit)
            })
        })
    }
}

impl Liveness {
    pub fn compute(code: &[u8], pool: &[u32], frame_size: usize) -> Result<Liveness, String> {
        if code.is_empty() {
            return Err("empty code block".to_string());
        }
        if frame_size > u32::MAX as usize {
            return Err(format!("frame of {frame_size} slots is too large"));
        }
        let words = frame_size.div_ceil(64);

        let mut insts = Vec::new();
        let mut inst_at = vec![u32::MAX; code.len()];
        let mut pc = 0;
        while pc < code.len() {
            let Some((ins, _, len)) = Instruction::decode_slice(code, pc) else {
                return Err(format!("undecodable instruction at {pc}"));
            };
            let mut effects = Effects::new(frame_size as u64);
            effects_of(&ins, &mut effects);
            let targets = branch_targets(&ins, pc, pool)?;
            inst_at[pc] = insts.len() as u32;
            insts.push(Inst {
                pc,
                len,
                uses: effects.uses,
                defs: effects.defs,
                targets,
                falls_through: effects.falls_through,
                leader: effects.leader,
            });
            pc += len;
        }

        let last = insts.last().expect("code is not empty");
        if last.falls_through {
            return Err(format!("code block falls off the end at {}", last.pc));
        }

        let mut is_leader = vec![false; insts.len()];
        is_leader[0] = true;
        for (index, inst) in insts.iter().enumerate() {
            is_leader[index] |= inst.leader;
            for &target in &inst.targets {
                let Some(&target_index) = inst_at.get(target).filter(|at| **at != u32::MAX) else {
                    return Err(format!(
                        "jump from {} to {target}, which starts no instruction",
                        inst.pc
                    ));
                };
                is_leader[target_index as usize] = true;
            }
            if (!inst.falls_through || !inst.targets.is_empty()) && index + 1 < insts.len() {
                is_leader[index + 1] = true;
            }
        }

        // Blocks: runs of instructions cut at each leader.
        let mut blocks = Vec::new();
        let mut block_of_inst = vec![0usize; insts.len()];
        let mut first = 0;
        for index in 1..=insts.len() {
            if index == insts.len() || is_leader[index] {
                let block = blocks.len();
                for slot in &mut block_of_inst[first..index] {
                    *slot = block;
                }
                let tail = &insts[index - 1];
                blocks.push(Block {
                    start: insts[first].pc,
                    end: tail.pc + tail.len,
                    successors: Vec::new(),
                    insts: first..index,
                });
                first = index;
            }
        }
        for block in 0..blocks.len() {
            let tail = &insts[blocks[block].insts.end - 1];
            let mut successors = Vec::new();
            for &target in &tail.targets {
                let target_index = inst_at[target] as usize;
                successors.push(block_of_inst[target_index]);
            }
            if tail.falls_through && block + 1 < blocks.len() {
                successors.push(block + 1);
            }
            successors.sort_unstable();
            successors.dedup();
            blocks[block].successors = successors;
        }

        // Backward dataflow over the blocks until nothing changes.
        let nblocks = blocks.len();
        let mut live_before = vec![0u64; insts.len() * words];
        let mut live_in = vec![0u64; nblocks * words];
        let mut live_out = vec![0u64; nblocks * words];
        let mut live = vec![0u64; words];
        loop {
            let mut changed = false;
            for block in (0..nblocks).rev() {
                let out = &mut live_out[block * words..(block + 1) * words];
                out.fill(0);
                for &successor in &blocks[block].successors {
                    let source = &live_in[successor * words..(successor + 1) * words];
                    for (word, bits) in out.iter_mut().zip(source) {
                        *word |= bits;
                    }
                }
                live.copy_from_slice(out);
                for index in blocks[block].insts.clone().rev() {
                    let inst = &insts[index];
                    for &def in &inst.defs {
                        live[def as usize / 64] &= !(1 << (def % 64));
                    }
                    for &use_ in &inst.uses {
                        live[use_ as usize / 64] |= 1 << (use_ % 64);
                    }
                    live_before[index * words..(index + 1) * words].copy_from_slice(&live);
                }
                let entry = &mut live_in[block * words..(block + 1) * words];
                if entry[..] != live[..] {
                    entry.copy_from_slice(&live);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        Ok(Liveness {
            frame_size,
            words,
            blocks,
            inst_at,
            live_before,
            live_in,
            live_out,
        })
    }

    /// The number of registers in the frame.
    pub fn frame_size(&self) -> usize {
        self.frame_size
    }

    /// The basic blocks, in pc order. Block 0 is the entry.
    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    /// The pcs that start a block.
    pub fn leaders(&self) -> impl Iterator<Item = usize> + '_ {
        self.blocks.iter().map(|block| block.start)
    }

    /// The block containing `pc`.
    pub fn block_at(&self, pc: usize) -> Option<usize> {
        let after = self.blocks.partition_point(|block| block.start <= pc);
        let block = after.checked_sub(1)?;
        (pc < self.blocks[block].end).then_some(block)
    }

    /// Registers live on entry to `block`.
    pub fn live_in(&self, block: usize) -> RegSet<'_> {
        RegSet {
            bits: &self.live_in[block * self.words..(block + 1) * self.words],
        }
    }

    /// Registers live on exit from `block`.
    pub fn live_out(&self, block: usize) -> RegSet<'_> {
        RegSet {
            bits: &self.live_out[block * self.words..(block + 1) * self.words],
        }
    }

    /// Registers live before the instruction at `pc`, or `None` if no
    /// instruction starts there.
    pub fn live_at(&self, pc: usize) -> Option<RegSet<'_>> {
        let index = *self.inst_at.get(pc)?;
        if index == u32::MAX {
            return None;
        }
        let index = index as usize;
        Some(RegSet {
            bits: &self.live_before[index * self.words..(index + 1) * self.words],
        })
    }
}

/// One decoded instruction, with what the analysis needs from it.
struct Inst {
    pc: usize,
    len: usize,
    /// Registers read before any write by this instruction.
    uses: Vec<u32>,
    /// Registers this instruction writes on every path.
    defs: Vec<u32>,
    /// Pcs control can transfer to other than by falling through.
    targets: Vec<usize>,
    falls_through: bool,
    /// Starts a block whatever precedes it (a loop header or return point).
    leader: bool,
}

/// The register effects of one instruction, collected in execution order.
struct Effects {
    frame: u64,
    uses: Vec<u32>,
    defs: Vec<u32>,
    falls_through: bool,
    leader: bool,
}

impl Effects {
    fn new(frame: u64) -> Effects {
        Effects {
            frame,
            uses: Vec::new(),
            defs: Vec::new(),
            falls_through: true,
            leader: false,
        }
    }

    /// Read `reg`. A read after a write in the same instruction sees the
    /// written value, so it is not a use.
    fn read(&mut self, reg: u64) {
        if reg < self.frame && !self.defs.contains(&(reg as u32)) {
            self.uses.push(reg as u32);
        }
    }

    fn write(&mut self, reg: u64) {
        if reg < self.frame && !self.defs.contains(&(reg as u32)) {
            self.defs.push(reg as u32);
        }
    }

    /// Read `from..to`, clipped to the frame.
    fn read_range(&mut self, from: u64, to: u64) {
        for reg in from..to.min(self.frame) {
            self.read(reg);
        }
    }

    /// Write `from..to`, clipped to the frame.
    fn write_range(&mut self, from: u64, to: u64) {
        for reg in from..to.min(self.frame) {
            self.write(reg);
        }
    }
}

/// The register effects of `ins`, in the order the interpreter performs them.
fn effects_of(ins: &Instruction, e: &mut Effects) {
    match ins {
        Instruction::OpNop(_) => {}
        Instruction::OpEnter(_) => {}
        // The rest area `base..` is written with a run-time count, so only
        // the count register is a definite write.
        Instruction::OpEnterRest(op) => e.write(op.cnt.0.into()),
        // The dispatch replaces r1 with the selected clause's closure.
        Instruction::OpEnterCase(_) => e.write(1),
        Instruction::OpReturnPoint(op) => {
            e.leader = true;
            e.write(op.dst.0.into());
        }
        Instruction::OpReturnPointForward(_) => e.leader = true,
        // The callee's values land at `frame + 2..`, the callee's `r[2..]`.
        Instruction::OpReturnPointValues(op) => {
            e.leader = true;
            let frame = u64::from(op.frame.0);
            e.write_range(frame + 2, frame + 2 + u64::from(op.nvals.0));
        }
        Instruction::OpReturnPointValuesRest(op) => {
            e.leader = true;
            let frame = u64::from(op.frame.0);
            e.write(frame);
            // The callee returns at least `nvals` values and may return more,
            // each landing at `frame + 2..` (the callee sizes the count; see
            // `bytecode/verify.rs`). They are *definitions*: treating them as
            // upward-exposed uses would make registers the procedure never
            // initializes live at its entry, and the optimizer would load
            // them from uninitialized frame slots and mark those spill slots
            // as roots (a small heap then traces a stale pointer).
            e.write_range(frame + 2, u64::MAX);
        }
        Instruction::OpMove(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpLoadConstant(op) => e.write(op.dst.0.into()),
        Instruction::OpLoadImmediate(op) => e.write(op.dst.0.into()),
        Instruction::OpLoadRaw(op) => e.write(op.dst.0.into()),
        Instruction::OpCellRef(op) => e.write(op.dst.0.into()),
        Instruction::OpCellSet(op) => e.read(op.src.0.into()),
        Instruction::OpRuntimeProcedure(op) => e.write(op.dst.0.into()),
        Instruction::OpClosure(op) => e.write(op.dst.0.into()),
        Instruction::OpClosureEnv(op) => {
            e.read(op.env.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpFreeRef(op) => e.write(op.dst.0.into()),
        Instruction::OpClosureRef(op) => {
            e.read(op.closure.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpClosureSet(op) => {
            e.read(op.closure.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpMakeEnv(op) => e.write(op.dst.0.into()),
        Instruction::OpEnvRef(op) => {
            e.read(op.env.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpEnvSet(op) => {
            e.read(op.env.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpJump(_) => e.falls_through = false,
        Instruction::OpJumpIfTrue(op) => e.read(op.cond.0.into()),
        Instruction::OpJumpIfFalse(op) => e.read(op.cond.0.into()),
        Instruction::OpJumpIfNull(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfNotNull(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfPair(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfNotPair(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfFixnum(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfNotFixnum(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfUndefined(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfNotUndefined(op) => e.read(op.src.0.into()),
        Instruction::OpJumpIfEq(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotEq(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfLess(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfLessEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNumberEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfGreaterEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfGreater(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotLess(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotLessEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotNumberEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotGreaterEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpJumpIfNotGreater(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
        }
        Instruction::OpSwitchFixnum(op) => {
            e.read(op.src.0.into());
            e.falls_through = false;
        }
        Instruction::OpSwitchChar(op) => {
            e.read(op.src.0.into());
            e.falls_through = false;
        }
        Instruction::OpSwitchSymbol(op) => {
            e.read(op.src.0.into());
            e.falls_through = false;
        }
        Instruction::OpLoopHint(_) => e.leader = true,
        // The callee is `r[base+1]`, the arguments follow it. The frame from
        // `base` up belongs to the callee, so nothing above `base - 1` is
        // live across the call.
        Instruction::OpCall(op) => {
            let base = u64::from(op.base.0);
            e.read_range(base + 1, base + 2 + u64::from(op.argc.0));
        }
        Instruction::OpTailCall(op) => {
            e.read(1);
            e.read_range(2, 2 + u64::from(op.argc.0));
            e.falls_through = false;
        }
        Instruction::OpReturn(op) => {
            e.read(op.src.0.into());
            e.falls_through = false;
        }
        Instruction::OpReturnValues(op) => {
            e.read_range(2, 2 + u64::from(op.count.0));
            e.falls_through = false;
        }
        Instruction::OpRaise(op) => {
            e.read_range(2, 2 + u64::from(op.count.0));
            e.falls_through = false;
        }
        Instruction::OpRestRef(op) => {
            e.read(op.cnt.0.into());
            e.read(u64::from(op.base.0) + u64::from(op.index.0));
            e.write(op.dst.0.into());
        }
        Instruction::OpRestLen(op) => {
            e.read(op.cnt.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpRestNull(op) => {
            e.read(op.cnt.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpRestPair(op) => {
            e.read(op.cnt.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpRestList(op) => {
            e.read(op.cnt.0.into());
            e.read_range(op.base.0.into(), u64::MAX);
            e.write(op.dst.0.into());
        }
        Instruction::OpValuesRest(op) => {
            let base = u64::from(op.base.0);
            e.read(base);
            e.read_range(base + 2 + u64::from(op.from.0), u64::MAX);
            e.write(base);
        }
        Instruction::OpCallPrimitive0(op) => e.write(op.dst.0.into()),
        Instruction::OpCallPrimitive1(op) => {
            e.read(op.a.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpCallPrimitive2(op) => {
            e.read(op.a.0.into());
            e.read(op.b.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpCallPrimitive3(op) => {
            e.read(op.a.0.into());
            e.read(op.b.0.into());
            e.read(op.c.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpCallPrimitiveN(op) => {
            let first = u64::from(op.first.0);
            e.read_range(first, first + u64::from(op.argc.0));
            e.write(op.dst.0.into());
        }
        Instruction::OpCar(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpCdr(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpCons(op) => {
            e.read(op.car.0.into());
            e.read(op.cdr.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpSetCar(op) => {
            e.read(op.pair.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpSetCdr(op) => {
            e.read(op.pair.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpEq(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpNot(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpAdd(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpSub(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpMul(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpAddImmediate(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpLess(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpLessEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpNumberEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpGreaterEqual(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpGreater(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpDiv(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpQuotient(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpRemainder(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpModulo(op) => {
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpNegate(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpAbs(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpSqrt(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpVectorRef(op) => {
            e.read(op.vector.0.into());
            e.read(op.index.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpVectorSet(op) => {
            e.read(op.vector.0.into());
            e.read(op.index.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpVectorLength(op) => {
            e.read(op.src.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpVariableRef(op) => {
            e.read(op.var.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpVariableSet(op) => {
            e.read(op.var.0.into());
            e.read(op.src.0.into());
        }
        // `dst` is written on the fast edge only; the slow edge may leave it
        // alone, so it does not kill.
        Instruction::OpGetSlot(op) => e.read(op.obj.0.into()),
        Instruction::OpPutSlot(op) => {
            e.read(op.obj.0.into());
            e.read(op.src.0.into());
        }
        Instruction::OpMoveTwo(op) => {
            e.read(op.src1.0.into());
            e.write(op.dst1.0.into());
            e.read(op.src2.0.into());
            e.write(op.dst2.0.into());
        }
        Instruction::OpAddImmediateMove(op) => {
            e.read(op.src.0.into());
            e.write(op.tmp.0.into());
            e.read(op.tmp.0.into());
            e.write(op.dst.0.into());
        }
        Instruction::OpVectorRefMul(op) => {
            e.read(op.vector.0.into());
            e.read(op.index.0.into());
            e.write(op.vdst.0.into());
            e.read(op.lhs.0.into());
            e.read(op.rhs.0.into());
            e.write(op.mdst.0.into());
        }
        // `call_self` takes its callee from `r[1]` and writes its own slot;
        // only the arguments are read.
        Instruction::OpCallSelf(op) => {
            let base = u64::from(op.base.0);
            e.read_range(base + 2, base + 2 + u64::from(op.argc.0));
        }
        Instruction::OpTailCallSelf(op) => {
            e.read(1);
            e.read_range(2, 2 + u64::from(op.argc.0));
            e.falls_through = false;
        }
    }
}

/// The pcs `ins` at `pc` can branch to, besides falling through.
fn branch_targets(ins: &Instruction, pc: usize, pool: &[u32]) -> Result<Vec<usize>, String> {
    let mut targets = Vec::new();
    if let Some(offset) = ins.jump_offset() {
        targets.push(displaced(pc, offset as i32)?);
    }
    match ins {
        Instruction::OpSwitchFixnum(op) => int_switch_targets(pool, pc, op.table.0, &mut targets)?,
        Instruction::OpSwitchChar(op) => int_switch_targets(pool, pc, op.table.0, &mut targets)?,
        Instruction::OpSwitchSymbol(op) => sym_switch_targets(pool, pc, op.table.0, &mut targets)?,
        Instruction::OpEnterCase(op) => case_targets(pool, pc, op.table.0, &mut targets)?,
        _ => {}
    }
    Ok(targets)
}

/// Targets of a `case-lambda` clause table: one per row, in source order.
fn case_targets(
    pool: &[u32],
    pc: usize,
    table: u32,
    targets: &mut Vec<usize>,
) -> Result<(), String> {
    let table = table as usize;
    let count = pool_word(pool, pc, table)? as usize;
    for row in 0..count {
        let disp = pool_word(pool, pc, table + 1 + row * 6 + 2)?;
        targets.push(displaced(pc, disp as i32)?);
    }
    Ok(())
}

fn displaced(pc: usize, disp: i32) -> Result<usize, String> {
    usize::try_from(pc as i64 + disp as i64).map_err(|_| format!("jump from {pc} before the code"))
}

fn pool_word(pool: &[u32], pc: usize, index: usize) -> Result<u32, String> {
    pool.get(index)
        .copied()
        .ok_or_else(|| format!("switch at {pc} reads past the aux pool"))
}

/// Targets of a fixnum or char switch table: the default, then either the
/// dense run of displacements or the `(key, displacement)` pairs.
fn int_switch_targets(
    pool: &[u32],
    pc: usize,
    table: u32,
    targets: &mut Vec<usize>,
) -> Result<(), String> {
    let table = table as usize;
    targets.push(displaced(pc, pool_word(pool, pc, table)? as i32)?);
    let count = pool_word(pool, pc, table + 1)?;
    if switch_table_is_dense(count) {
        let span = switch_table_len(count);
        for entry in 0..span {
            let disp = pool_word(pool, pc, table + 3 + entry)?;
            targets.push(displaced(pc, disp as i32)?);
        }
    } else {
        for case in 0..count as usize {
            let disp = pool_word(pool, pc, table + 2 + 2 * case + 1)?;
            targets.push(displaced(pc, disp as i32)?);
        }
    }
    Ok(())
}

/// Targets of a symbol switch table: the default, then three words per case
/// whose last word is the displacement.
fn sym_switch_targets(
    pool: &[u32],
    pc: usize,
    table: u32,
    targets: &mut Vec<usize>,
) -> Result<(), String> {
    let table = table as usize;
    targets.push(displaced(pc, pool_word(pool, pc, table)? as i32)?);
    let count = pool_word(pool, pc, table + 1)? as usize;
    for case in 0..count {
        let disp = pool_word(pool, pc, table + 4 + 3 * case + 2)?;
        targets.push(displaced(pc, disp as i32)?);
    }
    Ok(())
}

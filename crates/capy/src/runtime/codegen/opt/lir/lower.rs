//! Lowering from SSA IR to LIR.

use cranelift_entity::{EntityRef, SecondaryMap};

use super::schedule::{self, Schedule};
use super::{
    AllocLayout, CallTarget, DeoptExit, FloatOp, FrameValue, GuardKind, IntOp, LBlock, LEdge,
    LirFunc, LirInst, LirRematDesc, LirTerm, NumberStoreKind, ReturnedKind, SafePoint, Test, VReg,
    VRepr,
    WideShiftOp,
};
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, Edge, Inst, Op, Repr, ReturnValues, Terminator,
};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::prims::calls::PRIM_ARITY;
use crate::utils::flags;

/// The most arguments a runtime helper takes in registers.
const MAX_HELPER_ARGS: usize = 4;
const MAX_PRIM_ARGS: usize = 3;

/// The payload word that holds a vector's (or bytevector's) length.
const VECTOR_LENGTH_FIELD: u32 = 0;
/// `Str::length`, in payload words
const STRING_LENGTH_FIELD: u32 = (crate::runtime::sabi::STRING_LENGTH / size_of::<usize>()) as u32;
/// The payload word that holds a bytevector's contents pointer.
const BYTEVECTOR_CONTENTS_FIELD: u32 = 1;

/// Lowers `func` to LIR.
pub fn lower(func: &Func<'_>, name: &str) -> Result<LirFunc, String> {
    let sched = schedule::schedule(func);
    let lir = Lowerer::new(func, &sched).run()?;
    if opt().verify {
        lir.verify()?;
    }
    if opt().dump.iter().any(|stage| stage == "lir") {
        dump(&lir, name);
    }
    Ok(lir)
}

struct Lowerer<'a> {
    func: &'a Func<'a>,
    sched: &'a Schedule,
    out: LirFunc,
    /// The LIR block of each reachable IR block.
    blocks: SecondaryMap<Block, Option<LBlock>>,
    /// The register of each lowered value. An alias shares its operand's. For
    /// a two-limb `Int128` value this is the low limb's register; the high
    /// limb is in `int128_hi`.
    vregs: SecondaryMap<Value, Option<VReg>>,
    /// The high-limb register of each lowered `Int128` value.
    int128_hi: SecondaryMap<Value, Option<VReg>>,
    /// Immediate constants are rebuilt in each block that uses them, so they
    /// never live across a call or take a spill slot: one register per
    /// (IR block, value).
    local_consts: std::collections::HashMap<(Block, Value), VReg>,
    const_cache: SecondaryMap<Block, std::collections::HashMap<Constant, VReg>>,
    frame_consts: std::collections::HashMap<Value, VReg>,
    closures: std::collections::HashSet<VReg>,
    cur_block: Block,
    cur: LBlock,
}

impl<'a> Lowerer<'a> {
    fn new(func: &'a Func<'a>, sched: &'a Schedule) -> Self {
        let out = LirFunc::new(func.entry);
        let cur = out.entry;
        Self {
            func,
            sched,
            out,
            blocks: SecondaryMap::new(),
            vregs: SecondaryMap::new(),
            int128_hi: SecondaryMap::new(),
            closures: Default::default(),
            local_consts: Default::default(),
            const_cache: SecondaryMap::new(),
            frame_consts: Default::default(),
            cur_block: func.entry,
            cur,
        }
    }

    fn run(mut self) -> Result<LirFunc, String> {
        let func = self.func;
        let dom = func.dom_tree();
        let rpo = dom.rpo().to_vec();

        for (i, &block) in rpo.iter().enumerate() {
            let lir_block = if i == 0 {
                self.out.entry
            } else {
                self.out.new_block(block)
            };
            self.blocks[block] = Some(lir_block);
            for &param in func.params(block) {
                let ty = func.value_type(param);

                if ty == Type::Int128 {
                    return Err(format!(
                        "{param} of {block} is a two-limb Int128 block parameter, which is not supported"
                    ));
                }
                let repr = VRepr::of_type(ty);
                let vreg = self.out.new_vreg(repr, Some(param));
                self.vregs[param] = Some(vreg);
                self.out.blocks[lir_block].params.push(vreg);
            }
        }

        for &block in &rpo {
            self.cur_block = block;
            self.closures.clear();
            self.cur = self.blocks[block].ok_or_else(|| format!("{block} has no LIR block"))?;
            for &inst in &func.blocks[block].insts {
                if schedule::is_floating(func.insts[inst].op) {
                    continue;
                }
                self.lower_inst(inst)?;
            }
            let term = func.blocks[block]
                .term
                .as_ref()
                .ok_or_else(|| format!("{block} has no terminator"))?;
            let lir_term = self.lower_term(term)?;

            for &value in self.sched.values_in(block) {
                if self.immediate(value).is_some() {
                    continue;
                }
                self.ensure(value)?;
            }
            self.out.blocks[self.cur].term = Some(lir_term);
        }
        Ok(self.out)
    }

    fn push(&mut self, inst: LirInst) {
        self.out.blocks[self.cur].insts.push(inst);
    }

    fn immediate(&self, value: Value) -> Option<Constant> {
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return None;
        };
        match self.func.insts[inst].op {
            Op::Const(
                c @ (Constant::Int32(_)
                | Constant::Int64(_)
                | Constant::Bits(_)
                | Constant::Block { .. }
                | Constant::Double(_)),
            ) => Some(c),
            _ => None,
        }
    }

    fn number_store_kind(&self, value: Value) -> NumberStoreKind {
        if self.number_store_fixnum(value, &mut std::collections::HashSet::new()) {
            NumberStoreKind::Fixnum
        } else if self.number_store_number(value, &mut std::collections::HashSet::new()) {
            NumberStoreKind::Number
        } else {
            NumberStoreKind::Any
        }
    }

    /// Whether `value` is always a tagged fixnum.
    fn number_store_fixnum(
        &self,
        value: Value,
        seen: &mut std::collections::HashSet<Value>,
    ) -> bool {
        if !seen.insert(value) {
            return false;
        }
        match self.func.value_def(value) {
            ValueDef::Inst(inst) => match self.func.insts[inst].op {
                Op::Const(Constant::Bits(bits)) => {
                    crate::runtime::objectmodel::Value::from_raw(bits)
                        .int32()
                        .is_some()
                }
                Op::Box(Repr::Int32) => true,
                _ => false,
            },
            ValueDef::Param { block, index } => {
                self.number_store_edges(block, index, seen, Self::number_store_fixnum)
            }
        }
    }

    /// Whether `value` is always a fixnum or a one-limb `BIGINT`.
    fn number_store_number(
        &self,
        value: Value,
        seen: &mut std::collections::HashSet<Value>,
    ) -> bool {
        if !seen.insert(value) {
            return false;
        }
        match self.func.value_def(value) {
            ValueDef::Inst(inst) => match self.func.insts[inst].op {
                Op::Const(Constant::Bits(bits)) => {
                    crate::runtime::objectmodel::Value::from_raw(bits)
                        .int32()
                        .is_some()
                }
                Op::Box(Repr::Int32) | Op::Box(Repr::Int64) => true,
                _ => false,
            },
            ValueDef::Param { block, index } => {
                self.number_store_edges(block, index, seen, Self::number_store_number)
            }
        }
    }

    /// Every incoming edge of `block` passes a value that `check` accepts.
    fn number_store_edges(
        &self,
        block: Block,
        index: u32,
        seen: &mut std::collections::HashSet<Value>,
        check: fn(&Self, Value, &mut std::collections::HashSet<Value>) -> bool,
    ) -> bool {
        let mut found = false;
        for (_, data) in self.func.blocks.iter() {
            let Some(term) = &data.term else {
                continue;
            };
            for edge in term.edges() {
                if edge.block != block {
                    continue;
                }
                let Some(&arg) = self.func.edge_args(&edge).get(index as usize) else {
                    return false;
                };
                found = true;
                if !check(self, arg, seen) {
                    return false;
                }
            }
        }
        found
    }

    /// The register of immediate `c` in the current block, emitting its
    /// `Const` at the first use. One register per distinct constant, so two
    /// IR values that materialize `0L` cost one `mov`, and the constants the
    /// lowering synthesizes after the passes are shared too.
    fn const_vreg(&mut self, c: Constant) -> VReg {
        if let Some(&vreg) = self.const_cache.get(self.cur_block).and_then(|m| m.get(&c)) {
            return vreg;
        }
        let vreg = self.out.new_vreg(VRepr::of_constant(c), None);
        self.push(LirInst::Const {
            dst: vreg,
            value: c,
        });
        self.const_cache[self.cur_block].insert(c, vreg);
        vreg
    }

    fn reg(&self, value: Value) -> Result<VReg, String> {
        if self.immediate(value).is_some() {
            return self
                .local_consts
                .get(&(self.cur_block, value))
                .copied()
                .ok_or_else(|| {
                    format!("{value} is used before it is rebuilt in {}", self.cur_block)
                });
        }
        self.vregs[value].ok_or_else(|| format!("{value} is used before it is lowered"))
    }

    fn regs(&self, values: &[Value]) -> Result<Vec<VReg>, String> {
        values.iter().map(|&value| self.reg(value)).collect()
    }

    /// The register of `value`, lowering it first when it is a floating
    /// value that has not been emitted yet.
    fn ensure(&mut self, value: Value) -> Result<VReg, String> {
        if let Some(c) = self.immediate(value) {
            if let Some(&vreg) = self.local_consts.get(&(self.cur_block, value)) {
                return Ok(vreg);
            }
            let vreg = self.const_vreg(c);
            self.local_consts.insert((self.cur_block, value), vreg);
            return Ok(vreg);
        }
        if let Some(vreg) = self.vregs[value] {
            return Ok(vreg);
        }
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return Err(format!("parameter {value} has no register"));
        };
        if !schedule::is_floating(self.func.insts[inst].op) {
            return Err(format!("{value} is used before its pinned definition"));
        }
        if self.sched.block_of(value) != Some(self.cur_block) {
            return Err(format!(
                "floating {value} is placed in {:?}, not in {}",
                self.sched.block_of(value),
                self.cur_block
            ));
        }
        self.lower_inst(inst)?;
        self.reg(value)
    }

    fn ensure_all(&mut self, values: &[Value]) -> Result<Vec<VReg>, String> {
        values.iter().map(|&value| self.ensure(value)).collect()
    }

    /// Emits the operands and frame of `inst`, then the instruction itself.
    fn lower_inst(&mut self, inst: Inst) -> Result<(), String> {
        let func = self.func;
        for &arg in func.args(inst) {
            self.ensure(arg)?;
        }
        if let Some(frame) = func.insts[inst].frame {
            self.ensure_frame(frame)?;
        }
        self.emit(inst)
    }

    fn ensure_frame(&mut self, frame: FrameState) -> Result<(), String> {
        let func = self.func;
        for value in func.frame_values(frame) {
            if matches!(self.immediate(value), Some(Constant::Bits(_))) {
                continue;
            }
            self.ensure(value)?;
        }
        Ok(())
    }

    fn frame_values(&mut self, frame: FrameState) -> Result<Vec<FrameValue>, String> {
        let func = self.func;
        let mut out = Vec::new();
        for value in func.frame_values(frame) {
            let vreg = match self.immediate(value) {
                Some(c @ Constant::Bits(_)) => match self.frame_consts.get(&value) {
                    Some(&v) => v,
                    None => {
                        let v = self.out.new_vreg(VRepr::Tagged, Some(value));
                        self.out.vregs[v].imm = Some(c);
                        self.frame_consts.insert(value, v);
                        v
                    }
                },
                _ => self.reg(value)?,
            };
            let ty = func.value_type(value);
            if ty == Type::Int128 {
                return Err(format!(
                    "frame value {value} is a two-limb Int128, which a deopt cannot rebuild"
                ));
            }
            out.push(FrameValue {
                ir: value,
                vreg,
                ty,
            });
        }
        Ok(out)
    }

    fn exit(&mut self, frame: Option<FrameState>, kind: GuardKind) -> Result<DeoptExit, String> {
        let frame = frame.ok_or("a deopt has no frame state")?;
        let values = self.frame_values(frame)?;
        Ok(self.out.new_exit(frame, values, kind))
    }

    fn safepoint(&mut self, frame: Option<FrameState>) -> Result<SafePoint, String> {
        let frame = frame.ok_or("a call has no frame state")?;
        let values = self.frame_values(frame)?;
        Ok(self.out.new_safepoint(frame, values))
    }

    /// A fresh register for the result of `inst`, in the representation its
    /// IR type gives.
    fn define(&mut self, inst: Inst) -> Result<VReg, String> {
        let func = self.func;
        let value = func.insts[inst]
            .result
            .ok_or("an instruction has no result")?;
        self.define_as(inst, VRepr::of_type(func.value_type(value)))
    }

    /// A fresh register for the result of `inst`
    fn define_as(&mut self, inst: Inst, repr: VRepr) -> Result<VReg, String> {
        let func = self.func;
        let value = func.insts[inst]
            .result
            .ok_or("an instruction has no result")?;
        let ty = func.value_type(value);
        if VRepr::of_type(ty) != repr {
            return Err(format!(
                "{value} has IR type {ty} but its instruction produces {repr}"
            ));
        }
        let vreg = self.out.new_vreg(repr, Some(value));
        self.vregs[value] = Some(vreg);
        Ok(vreg)
    }

    /// The low and high limb registers of a two-limb IR value, lowering it
    /// first when it is a floating value that has not been emitted yet.
    fn reg_wide(&mut self, value: Value) -> Result<(VReg, VReg), String> {
        let lo = self.ensure(value)?;
        let hi = self.int128_hi[value]
            .ok_or_else(|| format!("{value} is two-limb but has no high-limb register"))?;
        Ok((lo, hi))
    }

    /// Defines the two registers of an `Int128` result: the low limb as an
    /// `RawInt128` (the value's named register) and the high limb as an
    /// `RawInt64`.
    fn define_wide(&mut self, inst: Inst) -> Result<(VReg, VReg), String> {
        let func = self.func;
        let value = func.insts[inst]
            .result
            .ok_or("an instruction has no result")?;
        let ty = func.value_type(value);
        if ty != Type::Int128 {
            return Err(format!(
                "{value} has IR type {ty} but its instruction produces two limbs"
            ));
        }
        let lo = self.out.new_vreg(VRepr::RawInt128, Some(value));
        let hi = self.out.new_vreg(VRepr::RawInt64, None);
        self.vregs[value] = Some(lo);
        self.int128_hi[value] = Some(hi);
        Ok((lo, hi))
    }

    /// Makes the result of `inst` share the register `vreg`, which must hold
    /// the representation the result's IR type gives.
    fn alias(&mut self, inst: Inst, vreg: VReg) -> Result<(), String> {
        let func = self.func;
        let value = func.insts[inst]
            .result
            .ok_or("an instruction has no result")?;
        let want = VRepr::of_type(func.value_type(value));
        if self.out.repr(vreg) != want {
            return Err(format!(
                "{value} aliases {vreg}, which is {}, but its IR type needs {want}",
                self.out.repr(vreg)
            ));
        }
        self.vregs[value] = Some(vreg);
        Ok(())
    }

    fn edge(&mut self, edge: &Edge) -> Result<LEdge, String> {
        let func = self.func;
        let block =
            self.blocks[edge.block].ok_or_else(|| format!("{} is unreachable", edge.block))?;
        let args = self.ensure_all(func.edge_args(edge))?;
        Ok(LEdge { block, args })
    }

    fn emit(&mut self, inst: Inst) -> Result<(), String> {
        let func = self.func;
        let op = func.insts[inst].op;
        let frame = func.insts[inst].frame;
        let args = func.args(inst);
        match op {
            Op::Const(constant) => {
                if let Some(&vreg) = self
                    .const_cache
                    .get(self.cur_block)
                    .and_then(|m| m.get(&constant))
                {
                    self.alias(inst, vreg)?;
                    return Ok(());
                }
                let dst = self.define_as(inst, VRepr::of_constant(constant))?;
                self.push(LirInst::Const {
                    dst,
                    value: constant,
                });
                self.const_cache[self.cur_block].insert(constant, dst);
            }
            Op::CheckInt32 => {
                let src = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Fixnum)?;
                self.push(LirInst::Guard {
                    test: Test::Fixnum,
                    src: Some(src),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::UntagInt { dst, src });
            }
            Op::CheckInt64 => {
                let src = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Fixnum)?;
                self.push(LirInst::Guard {
                    test: Test::Fixnum,
                    src: Some(src),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::UntagInt64 { dst, src });
            }
            Op::CheckBigIntFits64 => {
                let obj = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::BigInt)?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::UnboxBigInt64 { dst, obj, exit });
            }
            Op::CheckDouble => {
                let src = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Flonum)?;
                self.push(LirInst::Guard {
                    test: Test::Flonum,
                    src: Some(src),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::LoadFlonum { dst, obj: src });
            }
            Op::CheckClass(class) => {
                let src = self.reg(args[0])?;
                if class == crate::runtime::codegen::opt::ir::types::ClassRef::CLOSURE {
                    self.closures.insert(src);
                }
                let exit = self.exit(frame, GuardKind::Class)?;
                self.push(LirInst::Guard {
                    test: Test::Class(class),
                    src: Some(src),
                    exit,
                });
                self.alias(inst, src)?;
            }
            Op::CheckCode(code) => {
                let src = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Code)?;
                let test = if self.closures.contains(&src) {
                    Test::CodeOfClosure(code)
                } else {
                    Test::Code(code)
                };
                self.push(LirInst::Guard {
                    test,
                    src: Some(src),
                    exit,
                });
                self.alias(inst, src)?;
            }
            Op::CheckNotInvalid => {
                let exit = self.exit(frame, GuardKind::Invalidated)?;
                self.push(LirInst::Guard {
                    test: Test::NotInvalid,
                    src: None,
                    exit,
                });
            }
            Op::BoolToValue => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Tagged)?;
                self.push(LirInst::BoolToValue { dst, src });
            }
            Op::Box(Repr::Int32) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Tagged)?;
                self.push(LirInst::TagInt { dst, src });
            }
            Op::Box(Repr::Double) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Tagged)?;
                self.push(LirInst::BoxDouble { dst, src });
            }
            Op::Box(Repr::Int64) => {
                // Tags a fixnum when the value fits i32 and otherwise
                // allocates a one-limb bignum in the emitter's deferred
                // allocation block.
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Tagged)?;
                self.push(LirInst::BoxInt64 { dst, src });
            }
            Op::Unbox(Repr::Int32) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::UntagInt { dst, src });
            }
            Op::Unbox(Repr::Int64) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::UntagInt64 { dst, src });
            }
            Op::Int64ToInt32 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::TruncateI64ToI32 { dst, src });
            }
            Op::Unbox(Repr::Double) => {
                let obj = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::LoadFlonum { dst, obj });
            }
            Op::AddI32Ovf
            | Op::SubI32Ovf
            | Op::MulI32Ovf
            | Op::QuotI32
            | Op::RemI32
            | Op::ModI32 => {
                let op = match op {
                    Op::AddI32Ovf => IntOp::Add,
                    Op::SubI32Ovf => IntOp::Sub,
                    Op::QuotI32 => IntOp::Quot,
                    Op::RemI32 => IntOp::Rem,
                    Op::ModI32 => IntOp::Mod,
                    _ => IntOp::Mul,
                };
                let lhs = self.reg(args[0])?;
                let rhs = self.reg(args[1])?;
                let kind = match op {
                    IntOp::Add | IntOp::Sub | IntOp::Mul => GuardKind::IntOverflow,
                    _ => GuardKind::Division,
                };
                let exit = self.exit(frame, kind)?;
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::IntArith {
                    op,
                    dst,
                    lhs,
                    rhs,
                    exit,
                });
            }
            Op::AddI64
            | Op::SubI64
            | Op::MulI64
            | Op::AddI64Ovf
            | Op::SubI64Ovf
            | Op::MulI64Ovf
            | Op::QuotI64
            | Op::RemI64
            | Op::ModI64 => {
                let int_op = match op {
                    Op::AddI64 | Op::AddI64Ovf => IntOp::Add,
                    Op::SubI64 | Op::SubI64Ovf => IntOp::Sub,
                    Op::MulI64 | Op::MulI64Ovf => IntOp::Mul,
                    Op::QuotI64 => IntOp::Quot,
                    Op::RemI64 => IntOp::Rem,
                    _ => IntOp::Mod,
                };
                let total = matches!(op, Op::AddI64 | Op::SubI64 | Op::MulI64);
                let lhs = self.reg(args[0])?;
                let rhs = self.reg(args[1])?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                if total {
                    // The total forms cannot fail for two fixnums, so they
                    // carry no exit and no frame.
                    self.push(LirInst::Int64Wrapping {
                        op: int_op,
                        dst,
                        lhs,
                        rhs,
                    });
                } else {
                    let kind = match int_op {
                        IntOp::Add | IntOp::Sub | IntOp::Mul => GuardKind::IntOverflow,
                        _ => GuardKind::Division,
                    };
                    let exit = self.exit(frame, kind)?;
                    self.push(LirInst::Int64Arith {
                        op: int_op,
                        dst,
                        lhs,
                        rhs,
                        exit,
                    });
                }
            }
            Op::AddF64 | Op::SubF64 | Op::MulF64 | Op::DivF64 => {
                let op = match op {
                    Op::AddF64 => FloatOp::Add,
                    Op::SubF64 => FloatOp::Sub,
                    Op::MulF64 => FloatOp::Mul,
                    _ => FloatOp::Div,
                };
                let lhs = self.reg(args[0])?;
                let rhs = self.reg(args[1])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::FloatArith { op, dst, lhs, rhs });
            }
            Op::CmpI32(cmp) | Op::CmpI64(cmp) => self.emit_cmp(inst, cmp, false)?,
            Op::CmpF64(cmp) => self.emit_cmp(inst, cmp, true)?,
            Op::EqBits => self.emit_cmp(inst, Cmp::Eq, false)?,
            Op::TestInt32 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Fixnum,
                });
            }
            Op::TestDouble => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Flonum,
                });
            }
            Op::TestInt64 => {
                // Runtime fixnums are still 32-bit, so this is the same test
                // as `TestInt32`.
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Fixnum,
                });
            }
            Op::Int32ToDouble => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::IntToDouble { dst, src });
            }
            Op::Int64ToDouble => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::Int64ToDouble { dst, src });
            }
            Op::FloorF64 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::FloorF64 { dst, src });
            }
            Op::CeilingF64 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::CeilingF64 { dst, src });
            }
            Op::TruncateF64 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::TruncateF64 { dst, src });
            }
            Op::SqrtF64 => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::Double)?;
                self.push(LirInst::SqrtF64 { dst, src });
            }
            Op::TestCode(code) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Code(code),
                });
            }
            Op::TestPair => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Pair,
                });
            }
            Op::TestChar => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Char,
                });
            }
            Op::TestBoolean => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Boolean,
                });
            }
            Op::TestNumber => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Number,
                });
            }
            Op::TestClass(class) => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Class(class),
                });
            }
            Op::TestHeap => {
                let src = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::TestTag {
                    dst,
                    src,
                    test: Test::Heap,
                });
            }
            Op::LoadRationalComponent { index } => {
                let obj = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Rational)?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::LoadNumberFixnum {
                    dst,
                    obj,
                    offset: 16 * index,
                    exit,
                });
            }
            Op::LoadRationalComponentWide { index } => {
                let obj = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Rational)?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::LoadNumberWide {
                    dst,
                    obj,
                    offset: 16 * index,
                    exit,
                });
            }
            Op::Mul64x64To128 => {
                let lhs = self.reg(args[0])?;
                let rhs = self.reg(args[1])?;
                let (dst_lo, dst_hi) = self.define_wide(inst)?;
                self.push(LirInst::MulWide64 {
                    dst_lo,
                    dst_hi,
                    lhs,
                    rhs,
                });
            }
            Op::Add128Ovf | Op::Sub128Ovf => {
                let (lhs_lo, lhs_hi) = self.reg_wide(args[0])?;
                let (rhs_lo, rhs_hi) = self.reg_wide(args[1])?;
                let (dst_lo, dst_hi) = self.define_wide(inst)?;
                let exit = self.exit(frame, GuardKind::IntOverflow)?;
                self.push(if op == Op::Add128Ovf {
                    LirInst::WideAdd {
                        dst_lo,
                        dst_hi,
                        lhs_lo,
                        lhs_hi,
                        rhs_lo,
                        rhs_hi,
                        exit,
                    }
                } else {
                    LirInst::WideSub {
                        dst_lo,
                        dst_hi,
                        lhs_lo,
                        lhs_hi,
                        rhs_lo,
                        rhs_hi,
                        exit,
                    }
                });
            }
            Op::Cmp128(cmp) => {
                let (lhs_lo, lhs_hi) = self.reg_wide(args[0])?;
                let (rhs_lo, rhs_hi) = self.reg_wide(args[1])?;
                let dst = self.define_as(inst, VRepr::RawBits)?;
                self.push(LirInst::WideCmp {
                    cmp,
                    dst,
                    lhs_lo,
                    lhs_hi,
                    rhs_lo,
                    rhs_hi,
                });
            }
            Op::Shl128(amount) | Op::Shr128(amount) | Op::Ashr128(amount) => {
                let (src_lo, src_hi) = self.reg_wide(args[0])?;
                let (dst_lo, dst_hi) = self.define_wide(inst)?;
                let shift = match op {
                    Op::Shl128(_) => WideShiftOp::Shl,
                    Op::Shr128(_) => WideShiftOp::Shr,
                    _ => WideShiftOp::Ashr,
                };
                self.push(LirInst::WideShift {
                    op: shift,
                    dst_lo,
                    dst_hi,
                    src_lo,
                    src_hi,
                    amount,
                });
            }
            Op::ExtendI64To128 => {
                let src = self.reg(args[0])?;
                let (dst_lo, dst_hi) = self.define_wide(inst)?;
                self.push(LirInst::WideExtend {
                    dst_lo,
                    dst_hi,
                    src,
                });
            }
            Op::Int128Low | Op::Int128High => {
                let (src_lo, src_hi) = self.reg_wide(args[0])?;
                let dst = self.define_as(inst, VRepr::RawInt64)?;
                self.push(LirInst::WideSplit {
                    dst,
                    src_lo,
                    src_hi,
                    high: op == Op::Int128High,
                });
            }
            Op::Box(Repr::Int128) => {
                let (src_lo, src_hi) = self.reg_wide(args[0])?;
                let dst = self.define_as(inst, VRepr::Tagged)?;
                self.push(LirInst::BoxInt128 {
                    dst,
                    src_lo,
                    src_hi,
                });
            }
            Op::Unbox(Repr::Int128) => {
                return Err(format!(
                    "{inst}: unbox.int128 is not implemented; use i128.low/i128.high"
                ));
            }
            Op::Load { field, .. } => {
                let obj = self.reg(args[0])?;
                let dst = self.define(inst)?;
                self.push(LirInst::LoadField { dst, obj, field });
            }
            Op::Store { field, .. } => {
                let obj = self.reg(args[0])?;
                let src = self.reg(args[1])?;
                self.push(LirInst::StoreField { obj, field, src });
            }
            Op::LoadFree { index } => {
                let closure = self.reg(args[0])?;
                let dst = self.define(inst)?;
                self.push(LirInst::LoadFree {
                    dst,
                    closure,
                    index,
                });
            }
            Op::ReturnedValue(index) => {
                let dst = self.define(inst)?;
                let kind = ReturnedKind::Value;
                self.push(LirInst::LoadReturned { dst, index, kind });
            }
            Op::ReturnedAddr(index) => {
                let dst = self.define(inst)?;
                let kind = ReturnedKind::Addr;
                self.push(LirInst::LoadReturned { dst, index, kind });
            }
            Op::ReturnedCount(index) => {
                let dst = self.define(inst)?;
                let kind = ReturnedKind::Count;
                self.push(LirInst::LoadReturned { dst, index, kind });
            }
            Op::LoadCell => {
                let cell = self.reg(args[0])?;
                let dst = self.define(inst)?;
                self.push(LirInst::LoadCell { dst, cell });
            }
            Op::StoreCell => {
                let cell = self.reg(args[0])?;
                let src = self.reg(args[1])?;
                self.push(LirInst::StoreCell { cell, src });
            }
            Op::WriteBarrier => {
                let obj = self.reg(args[0])?;
                let value = self.reg(args[1])?;
                self.push(LirInst::WriteBarrier { obj, value });
            }
            Op::StoreFree { index } => {
                let closure = self.reg(args[0])?;
                let src = self.reg(args[1])?;
                self.push(LirInst::StoreFree {
                    closure,
                    index,
                    src,
                });
            }
            Op::CheckChar => {
                let src = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Char)?;
                self.push(LirInst::Guard {
                    test: Test::Char,
                    src: Some(src),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::UntagChar { dst, src });
            }
            Op::VectorLength => {
                // The length word is read as is, then checked to fit an int32.
                // The register is an int32 from the start. The range checks
                // below run before any use, and they compare full words.
                let vector = self.reg(args[0])?;
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::LoadField {
                    dst,
                    obj: vector,
                    field: VECTOR_LENGTH_FIELD,
                });
                self.check_int32_range(dst, frame, GuardKind::Range)?;
            }
            Op::CheckIndexRaw => {
                // A raw i64 index: one unsigned 64-bit compare rejects a
                // negative value (a huge word) and an out-of-range one, then
                // the low 32 bits feed the load.
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let len = self.out.new_vreg(VRepr::RawBits, None);
                self.push(LirInst::LoadField {
                    dst: len,
                    obj: vector,
                    field: VECTOR_LENGTH_FIELD,
                });
                let exit = self.exit(frame, GuardKind::Range)?;
                self.push(LirInst::GuardIndex {
                    index,
                    length: len,
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::TruncateI64ToI32 { dst, src: index });
            }
            Op::CheckIndex => {
                // The index must be a fixnum with 0 <= index < length.
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let exit = self.exit(frame, GuardKind::Fixnum)?;
                self.push(LirInst::Guard {
                    test: Test::Fixnum,
                    src: Some(index),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::UntagInt { dst, src: index });
                // The raw length word, which is not yet known to fit an int32.
                let len = self.out.new_vreg(VRepr::RawBits, None);
                self.push(LirInst::LoadField {
                    dst: len,
                    obj: vector,
                    field: VECTOR_LENGTH_FIELD,
                });
                let exit = self.exit(frame, GuardKind::Range)?;
                self.push(LirInst::GuardIndex {
                    index: dst,
                    length: len,
                    exit,
                });
            }
            Op::CheckStringIndex => {
                // The same fixnum and unsigned-bounds checks as a vector,
                // against the string's length word.
                let string = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let exit = self.exit(frame, GuardKind::Fixnum)?;
                self.push(LirInst::Guard {
                    test: Test::Fixnum,
                    src: Some(index),
                    exit,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::UntagInt { dst, src: index });
                let len = self.out.new_vreg(VRepr::RawBits, None);
                self.push(LirInst::LoadField {
                    dst: len,
                    obj: string,
                    field: STRING_LENGTH_FIELD,
                });
                let exit = self.exit(frame, GuardKind::Range)?;
                self.push(LirInst::GuardIndex {
                    index: dst,
                    length: len,
                    exit,
                });
            }
            Op::StringRef => {
                let string = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let dst = self.define(inst)?;
                let exit = self.exit(frame, GuardKind::Class)?;
                self.push(LirInst::StringRef {
                    dst,
                    string,
                    index,
                    exit,
                });
            }
            Op::LoadElement => {
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let dst = self.define(inst)?;
                self.push(LirInst::LoadElement { dst, vector, index });
            }
            Op::StoreElement => {
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let src = self.reg(args[2])?;
                self.push(LirInst::StoreElement { vector, index, src });
            }
            Op::LoadBytevectorU8 => {
                // A bytevector stores its data behind the contents pointer,
                // not inline past the length word, so the address is
                // `contents + index` rather than a fixed element offset.
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let base = self.out.new_vreg(VRepr::RawBits, None);
                self.push(LirInst::LoadField {
                    dst: base,
                    obj: vector,
                    field: BYTEVECTOR_CONTENTS_FIELD,
                });
                let dst = self.define_as(inst, VRepr::RawInt)?;
                self.push(LirInst::LoadByte { dst, base, index });
            }
            Op::StoreBytevectorU8 => {
                let vector = self.reg(args[0])?;
                let index = self.reg(args[1])?;
                let src = self.reg(args[2])?;
                let base = self.out.new_vreg(VRepr::RawBits, None);
                self.push(LirInst::LoadField {
                    dst: base,
                    obj: vector,
                    field: BYTEVECTOR_CONTENTS_FIELD,
                });
                self.push(LirInst::StoreByte { base, index, src });
            }
            Op::SlotGet { meta } => {
                let obj = self.reg(args[0])?;
                let exit = self.exit(frame, GuardKind::Slot)?;
                let dst = self.define(inst)?;
                self.push(LirInst::SlotGet {
                    dst,
                    obj,
                    meta,
                    exit,
                });
            }
            Op::SlotPut { meta } => {
                let obj = self.reg(args[0])?;
                let src = self.reg(args[1])?;
                let exit = self.exit(frame, GuardKind::Slot)?;
                self.push(LirInst::SlotPut {
                    obj,
                    src,
                    meta,
                    exit,
                });
            }
            Op::AllocPair => {
                let car = self.reg(args[0])?;
                let cdr = self.reg(args[1])?;
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Pair,
                });
                self.push(LirInst::StoreField {
                    obj: dst,
                    field: 0,
                    src: car,
                });
                self.push(LirInst::StoreField {
                    obj: dst,
                    field: 1,
                    src: cdr,
                });
            }
            Op::AllocClosure(code) => {
                let free = self.regs(args)?;
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Closure {
                        code,
                        free: free.len() as u32,
                    },
                });
                for (index, src) in free.into_iter().enumerate() {
                    self.push(LirInst::StoreFree {
                        closure: dst,
                        index: index as u32,
                        src,
                    });
                }
            }
            Op::AllocEnv => {
                let slots = self.regs(args)?;
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Env {
                        slots: slots.len() as u32,
                    },
                });
                self.store_fields(dst, slots);
            }
            Op::AllocVector => {
                let elements = self.regs(args)?;
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Vector {
                        len: elements.len() as u32,
                    },
                });
                self.store_fields(dst, elements);
            }
            Op::AllocVectorFill => {
                let size = self.reg(args[0])?;
                let fill = self.reg(args[1])?;
                let dst = self.define(inst)?;
                let exit = self.exit(frame, GuardKind::Range)?;
                self.push(LirInst::AllocVectorFill {
                    dst,
                    size,
                    fill,
                    exit,
                });
            }
            Op::AllocTuple => {
                let elements = self.regs(args)?;
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Tuple {
                        len: elements.len() as u32,
                    },
                });
                self.store_fields(dst, elements);
            }
            Op::AllocInstance { class, slots } => {
                if args.len() != slots as usize + 3 {
                    return Err(format!(
                        "{inst}: alloc.instance names {slots} slots but has {} operands",
                        args.len()
                    ));
                }
                if class == 0 {
                    return Err(format!("{inst}: alloc.instance names class 0"));
                }
                // The storage header and its packed stamp/length word are
                // raw immediates the builder put in the operand list.
                let header = constant_i64(func, args[1]).ok_or_else(|| {
                    format!("{inst}: the slot storage header is not an integer constant")
                })? as u64;
                let stamp_len = constant_i64(func, args[2]).ok_or_else(|| {
                    format!("{inst}: the slot storage stamp/length is not an integer constant")
                })? as u64;
                let stamp = stamp_len as u32;
                let len = (stamp_len >> 32) as u32;
                if len != slots {
                    return Err(format!(
                        "{inst}: the slot storage length {len} does not match {slots} slots"
                    ));
                }
                let class_value = self.reg(args[0])?;
                let values: Vec<VReg> = (0..slots as usize)
                    .map(|index| self.reg(args[index + 3]))
                    .collect::<Result<_, _>>()?;
                let dst = self.define(inst)?;
                let storage = self.out.new_vreg(VRepr::Tagged, None);
                self.push(LirInst::Alloc {
                    dst: storage,
                    layout: AllocLayout::SlotStorage { header, stamp, len },
                });
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Instance { class, slots },
                });
                // A fresh instance: the class pointer and storage need no
                // write barrier, and no safepoint runs before both stores.
                self.push(LirInst::StoreField {
                    obj: dst,
                    field: 0,
                    src: class_value,
                });
                self.push(LirInst::StoreField {
                    obj: dst,
                    field: 1,
                    src: storage,
                });
                for (index, src) in values.into_iter().enumerate() {
                    self.push(LirInst::Store {
                        base: storage,
                        offset: (crate::runtime::objectmodel::class::storage::STORAGE_DATA_OFFSET
                            + 8 * index) as i32,
                        src,
                    });
                }
            }
            Op::Remat(kind) => {
                let fields = self.regs(args)?;
                if fields.len() != kind.field_count() {
                    return Err(format!(
                        "{inst} rebuilds {kind} with {} fields",
                        fields.len()
                    ));
                }
                let dst = self.define_as(inst, VRepr::Tagged)?;
                let id = self.out.remats.push(LirRematDesc { kind, fields });
                self.out.vregs[dst].remat = Some(id);
            }
            Op::AllocRational => {
                let args = self.func.args(inst);
                let num = self.reg(args[0])?;
                let den = self.reg(args[1])?;
                let num_kind = self.number_store_kind(args[0]);
                let den_kind = self.number_store_kind(args[1]);
                let dst = self.define(inst)?;
                self.push(LirInst::Alloc {
                    dst,
                    layout: AllocLayout::Rational,
                });
                let num_exit = self.exit(frame, GuardKind::Rational)?;
                self.push(LirInst::StoreNumberTagged {
                    obj: dst,
                    offset: 0,
                    src: num,
                    kind: num_kind,
                    exit: num_exit,
                });
                let den_exit = self.exit(frame, GuardKind::Rational)?;
                self.push(LirInst::StoreNumberTagged {
                    obj: dst,
                    offset: 16,
                    src: den,
                    kind: den_kind,
                    exit: den_exit,
                });
            }
            Op::CallKnown(code, values) => self.emit_call(inst, CallTarget::Known(code), values)?,
            Op::CallGeneric(values) => self.emit_call(inst, CallTarget::Generic, values)?,
            Op::CallOp(helper) => {
                self.emit_call(inst, CallTarget::Helper(helper), ReturnValues::Ignore)?
            }
            Op::CallPrim { prim, .. } => {
                // A variadic primitive, or one with more arguments than the
                // direct wrapper takes, goes through the packed helper.
                let packed = self.func.args(inst).len() > MAX_PRIM_ARGS
                    || PRIM_ARITY.get(prim.index()).is_some_and(|&arity| arity < 0);
                let target = if packed {
                    CallTarget::PrimN(prim)
                } else {
                    CallTarget::Prim(prim)
                };
                self.emit_call(inst, target, ReturnValues::Ignore)?
            }
            Op::Safepoint => {
                let safepoint = self.safepoint(frame)?;
                self.push(LirInst::Poll { safepoint });
            }
        }
        Ok(())
    }

    fn emit_cmp(&mut self, inst: Inst, cmp: Cmp, float: bool) -> Result<(), String> {
        let args = self.func.args(inst);
        let lhs = self.reg(args[0])?;
        let rhs = self.reg(args[1])?;
        let dst = self.define_as(inst, VRepr::RawBits)?;
        self.push(if float {
            LirInst::FloatCmp { cmp, dst, lhs, rhs }
        } else {
            LirInst::IntCmp { cmp, dst, lhs, rhs }
        });
        Ok(())
    }

    /// Deopts unless the word in `vreg` lies in `0..=i32::MAX`, so that it
    /// can be represented as an int32.
    fn check_int32_range(
        &mut self,
        vreg: VReg,
        frame: Option<FrameState>,
        kind: GuardKind,
    ) -> Result<(), String> {
        let zero = self.const_vreg(Constant::Int32(0));
        let exit = self.exit(frame, kind)?;
        self.push(LirInst::GuardCmp {
            cmp: Cmp::Ge,
            lhs: vreg,
            rhs: zero,
            exit,
        });
        let max = self.const_vreg(Constant::Int32(i32::MAX));
        let exit = self.exit(frame, kind)?;
        self.push(LirInst::GuardCmp {
            cmp: Cmp::Le,
            lhs: vreg,
            rhs: max,
            exit,
        });
        Ok(())
    }

    /// Stores the elements of a vector or an env record. Payload word 0 is
    /// the length, so element `i` goes to field `i + 1`.
    fn store_fields(&mut self, obj: VReg, values: Vec<VReg>) {
        for (index, src) in values.into_iter().enumerate() {
            self.push(LirInst::StoreField {
                obj,
                field: index as u32 + 1,
                src,
            });
        }
    }

    fn emit_call(
        &mut self,
        inst: Inst,
        target: CallTarget,
        values: ReturnValues,
    ) -> Result<(), String> {
        let func = self.func;
        let frame = func.insts[inst].frame;
        let args = self.regs(func.args(inst))?;
        match target {
            CallTarget::Helper(_) if args.len() > MAX_HELPER_ARGS => {
                return Err(format!(
                    "a runtime helper takes at most {MAX_HELPER_ARGS} register arguments, got {}",
                    args.len()
                ));
            }
            CallTarget::Prim(_) if args.len() > MAX_PRIM_ARGS => {
                return Err(format!(
                    "a primitive call takes at most {MAX_PRIM_ARGS} register arguments, got {}",
                    args.len()
                ));
            }
            _ => {}
        }

        let exit = match target {
            CallTarget::Prim(_) | CallTarget::PrimN(_) | CallTarget::Helper(_) => {
                Some(self.exit(frame, GuardKind::Call)?)
            }
            CallTarget::Known(_) | CallTarget::Generic
                if matches!(
                    values,
                    ReturnValues::Forward | ReturnValues::Take(_) | ReturnValues::TakeRest(_)
                ) =>
            {
                Some(self.exit(frame, GuardKind::Return)?)
            }
            _ => None,
        };
        // A call returns a Scheme value in RV, so its IR type must be one too.
        let rv = if func.insts[inst].result.is_some() {
            Some(self.define_as(inst, VRepr::Tagged)?)
        } else {
            None
        };
        let safepoint = self.safepoint(frame)?;
        self.push(LirInst::Call {
            target,
            args,
            rv,
            safepoint,
            exit,
            values,
        });
        Ok(())
    }

    fn lower_term(&mut self, term: &Terminator) -> Result<LirTerm, String> {
        let func = self.func;
        Ok(match term {
            Terminator::Jump(edge) => LirTerm::Jump(self.edge(edge)?),
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.ensure(*cond)?;
                LirTerm::Branch {
                    cond,
                    then: self.edge(then)?,
                    otherwise: self.edge(otherwise)?,
                }
            }
            Terminator::Switch {
                value,
                cases,
                default,
                density,
            } => {
                let value = self.ensure(*value)?;
                let mut lowered = Vec::with_capacity(cases.len());
                for (n, edge) in cases {
                    lowered.push((*n, self.edge(edge)?));
                }
                LirTerm::Switch {
                    value,
                    cases: lowered,
                    default: self.edge(default)?,
                    density: *density,
                }
            }
            Terminator::Return(value) => LirTerm::Return {
                value: self.ensure(*value)?,
            },
            Terminator::TailCall {
                callee,
                args,
                check,
                ..
            } => {
                // The caller's frame is replaced, so its frame state is
                // never rebuilt and its values are not needed. A profile
                // `check` enters that block after a runtime code comparison;
                // a callee whose type names its code block (a fresh closure
                // or a checked value) enters it directly. Everything else
                // goes through the generic tail-call protocol.
                let (target, checked) = match check {
                    Some(code) => (CallTarget::Known(*code), true),
                    None => match func.value_type(*callee) {
                        crate::runtime::codegen::opt::ir::types::Type::Closure(Some(code)) => {
                            (CallTarget::Known(code), false)
                        }
                        _ => (CallTarget::Generic, false),
                    },
                };
                let mut values = vec![*callee];
                values.extend_from_slice(args.as_slice(&func.pool));
                let args = self.ensure_all(&values)?;
                LirTerm::TailCall {
                    target,
                    args,
                    checked,
                }
            }
            Terminator::ReturnMany { values } => {
                let mut lowered = Vec::with_capacity(values.len());
                for value in values {
                    lowered.push(self.ensure(*value)?);
                }
                LirTerm::ReturnMany { values: lowered }
            }
            Terminator::Deopt { frame } => {
                self.ensure_frame(*frame)?;
                LirTerm::Deopt {
                    exit: self.exit(Some(*frame), GuardKind::Explicit)?,
                }
            }
            Terminator::Unreachable => LirTerm::Unreachable,
        })
    }
}

/// The raw integer an IR value is a constant of, if it is one.
fn constant_i64(func: &Func<'_>, value: Value) -> Option<i64> {
    let ValueDef::Inst(inst) = func.value_def(value) else {
        return None;
    };
    match func.insts[inst].op {
        Op::Const(Constant::Int64(value)) => Some(value),
        Op::Const(Constant::Int32(value)) => Some(value as i64),
        _ => None,
    }
}

fn dump(lir: &LirFunc, name: &str) {
    let text = lir.to_string();
    let Some(dir) = flags::jit_dump_dir() else {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::info!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "{name} LIR\n{text}"
        );
        return;
    };
    let file = format!(
        "opt-lir-{:04}-{}.lir",
        crate::runtime::codegen::opt::next_dump_id(),
        crate::runtime::codegen::opt::file_safe(name)
    );
    let written =
        std::fs::create_dir_all(dir).and_then(|()| std::fs::write(dir.join(&file), &text));
    if let Err(err) = written {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::warn!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "cannot write LIR dump to {}: {err}",
            dir.display()
        );
    }
}

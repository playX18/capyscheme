use super::*;
use crate::runtime::codegen::opt::ir::func::Block;
use std::fmt::{self, Display, Formatter};

/// A function in LIR.
pub struct LirFunc {
    pub entry: LBlock,
    pub blocks: PrimaryMap<LBlock, LirBlock>,
    pub vregs: PrimaryMap<VReg, VRegData>,
    pub exits: PrimaryMap<DeoptExit, DeoptExitData>,
    pub safepoints: PrimaryMap<SafePoint, SafePointData>,
    pub remats: PrimaryMap<RematId, LirRematDesc>,
}

impl LirFunc {
    pub fn packed_prim_extent(&self) -> u32 {
        self.blocks
            .values()
            .flat_map(|block| block.insts.iter())
            .filter_map(|inst| match inst {
                LirInst::Call {
                    target: CallTarget::PrimN(_),
                    args,
                    ..
                } => Some(2 + args.len() as u32),
                _ => None,
            })
            .max()
            .unwrap_or(0)
    }

    pub fn new(source: Block) -> Self {
        let mut blocks = PrimaryMap::new();
        let entry = blocks.push(LirBlock::new(Some(source)));
        Self {
            entry,
            blocks,
            vregs: PrimaryMap::new(),
            exits: PrimaryMap::new(),
            safepoints: PrimaryMap::new(),
            remats: PrimaryMap::new(),
        }
    }

    pub fn new_block(&mut self, source: Block) -> LBlock {
        self.blocks.push(LirBlock::new(Some(source)))
    }

    pub fn new_vreg(&mut self, repr: VRepr, value: Option<Value>) -> VReg {
        self.vregs.push(VRegData {
            repr,
            value,
            imm: None,
            remat: None,
        })
    }

    pub fn is_imm(&self, vreg: VReg) -> bool {
        self.vregs[vreg].imm.is_some()
    }

    pub fn live_frame_values<'a>(
        &'a self,
        values: &'a [FrameValue],
    ) -> impl Iterator<Item = &'a FrameValue> {
        values.iter().filter(|fv| !self.is_imm(fv.vreg))
    }

    pub fn frame_regs(&self, values: &[FrameValue]) -> Vec<VReg> {
        let mut out = Vec::new();
        let mut visiting = std::collections::HashSet::new();
        for fv in values {
            self.push_frame_regs(fv.vreg, &mut out, &mut visiting);
        }
        out
    }

    fn push_frame_regs(
        &self,
        vreg: VReg,
        out: &mut Vec<VReg>,
        visiting: &mut std::collections::HashSet<VReg>,
    ) {
        if self.vregs[vreg].imm.is_some() {
            return;
        }
        let Some(id) = self.vregs[vreg].remat else {
            out.push(vreg);
            return;
        };

        if !visiting.insert(vreg) {
            return;
        }
        for &field in &self.remats[id].fields {
            self.push_frame_regs(field, out, visiting);
        }
        visiting.remove(&vreg);
    }

    pub fn frame_ops(&self, values: &[FrameValue]) -> Vec<Operand> {
        self.frame_regs(values).into_iter().map(use_any).collect()
    }

    pub fn repr(&self, vreg: VReg) -> VRepr {
        self.vregs[vreg].repr
    }

    pub fn class(&self, vreg: VReg) -> RegClass {
        self.repr(vreg).class()
    }

    pub fn is_gc_root(&self, vreg: VReg) -> bool {
        self.repr(vreg) == VRepr::Tagged
    }

    pub fn verify(&self) -> Result<(), String> {
        for (block, data) in self.blocks.iter() {
            for inst in &data.insts {
                self.verify_inst(inst)
                    .map_err(|err| format!("{block}: `{inst}`: {err}"))?;
            }
            let term = data
                .term
                .as_ref()
                .ok_or_else(|| format!("{block} has no terminator"))?;
            self.verify_term(term)
                .map_err(|err| format!("{block}: `{term}`: {err}"))?;
        }
        for (exit, data) in self.exits.iter() {
            self.verify_frame(&data.values)
                .map_err(|err| format!("{exit}: {err}"))?;
        }
        for (sp, data) in self.safepoints.iter() {
            self.verify_frame(&data.values)
                .map_err(|err| format!("{sp}: {err}"))?;
        }
        Ok(())
    }

    /// Fails unless `vreg` satisfies `ok`. `expected` names what it must be.
    fn require(
        &self,
        vreg: VReg,
        ok: impl Fn(VRepr) -> bool,
        expected: &str,
    ) -> Result<(), String> {
        let repr = self.repr(vreg);
        if ok(repr) {
            Ok(())
        } else {
            Err(format!("{vreg} is {repr}, but {expected} is needed"))
        }
    }

    fn require_repr(&self, vreg: VReg, want: VRepr) -> Result<(), String> {
        self.require(vreg, |r| r == want, &want.to_string())
    }

    fn verify_inst(&self, inst: &LirInst) -> Result<(), String> {
        use VRepr::{Double, RawBits, RawInt, Tagged};
        let word = |r: VRepr| r != Double;
        let raw = |r: VRepr| r.is_raw_word();
        match inst {
            LirInst::Move { dst, src } => {
                self.require_repr(*src, self.repr(*dst))?;
            }
            LirInst::Const { dst, value } => {
                self.require_repr(*dst, VRepr::of_constant(*value))?;
            }
            LirInst::TagInt { dst, src } => {
                self.require_repr(*src, RawInt)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::BoolToValue { dst, src } => {
                self.require_repr(*src, RawBits)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::UntagInt { dst, src } | LirInst::UntagChar { dst, src } => {
                self.require_repr(*src, Tagged)?;
                self.require_repr(*dst, RawInt)?;
            }
            LirInst::Guard { test, src, .. } => match (test, src) {
                (Test::NotInvalid, None) => {}
                (Test::NotInvalid, Some(_)) => {
                    return Err("a not-invalid guard has no operand".into());
                }
                (_, Some(src)) => self.require_repr(*src, Tagged)?,
                (_, None) => return Err(format!("a {test} guard needs an operand")),
            },
            LirInst::GuardCmp { lhs, rhs, .. } => {
                self.require(*lhs, raw, "a raw word")?;
                self.require(*rhs, raw, "a raw word")?;
            }
            LirInst::GuardIndex { index, length, .. } => {
                self.require(*index, raw, "a raw word")?;
                self.require(*length, raw, "a raw word")?;
            }
            LirInst::TestTag { dst, src, .. } => {
                self.require_repr(*src, Tagged)?;
                self.require_repr(*dst, RawBits)?;
            }
            LirInst::IntArith { dst, lhs, rhs, .. } => {
                self.require_repr(*lhs, RawInt)?;
                self.require_repr(*rhs, RawInt)?;
                self.require_repr(*dst, RawInt)?;
            }
            LirInst::Int64Arith { dst, lhs, rhs, .. }
            | LirInst::Int64Wrapping { dst, lhs, rhs, .. } => {
                self.require_repr(*lhs, VRepr::RawInt64)?;
                self.require_repr(*rhs, VRepr::RawInt64)?;
                self.require_repr(*dst, VRepr::RawInt64)?;
            }
            LirInst::BoxInt64 { dst, src } => {
                self.require_repr(*src, VRepr::RawInt64)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::UnboxBigInt64 { dst, obj, .. } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*dst, VRepr::RawInt64)?;
            }
            LirInst::UntagInt64 { dst, src } => {
                self.require_repr(*src, Tagged)?;
                self.require_repr(*dst, VRepr::RawInt64)?;
            }
            LirInst::TruncateI64ToI32 { dst, src } => {
                self.require_repr(*src, VRepr::RawInt64)?;
                self.require_repr(*dst, VRepr::RawInt)?;
            }
            LirInst::IntCmp { cmp, dst, lhs, rhs } => {
                // Equality also compares Scheme words bit for bit, as `eq?`
                // does. Ordering is only defined on raw words.
                let ordered = !matches!(cmp, Cmp::Eq | Cmp::Ne);
                let ok = |r: VRepr| {
                    if ordered {
                        raw(r)
                    } else {
                        r == Tagged || raw(r)
                    }
                };
                self.require(*lhs, ok, "an integer word")?;
                self.require(*rhs, ok, "an integer word")?;
                let lhs_tagged = self.repr(*lhs) == Tagged;
                if lhs_tagged != (self.repr(*rhs) == Tagged) {
                    return Err(format!("{lhs} and {rhs} mix tagged and raw words"));
                }
                self.require_repr(*dst, RawBits)?;
            }
            LirInst::FloatArith { dst, lhs, rhs, .. } => {
                self.require_repr(*lhs, Double)?;
                self.require_repr(*rhs, Double)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::IntToDouble { dst, src } => {
                self.require_repr(*src, RawInt)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::Int64ToDouble { dst, src } => {
                self.require_repr(*src, VRepr::RawInt64)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::FloorF64 { dst, src } => {
                self.require_repr(*src, Double)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::CeilingF64 { dst, src } => {
                self.require_repr(*src, Double)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::TruncateF64 { dst, src } => {
                self.require_repr(*src, Double)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::SqrtF64 { dst, src } => {
                self.require_repr(*src, Double)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::FloatCmp { dst, lhs, rhs, .. } => {
                self.require_repr(*lhs, Double)?;
                self.require_repr(*rhs, Double)?;
                self.require_repr(*dst, RawBits)?;
            }
            LirInst::LoadFlonum { dst, obj } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*dst, Double)?;
            }
            LirInst::StoreFlonum { obj, src } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*src, Double)?;
            }
            LirInst::BoxDouble { dst, src } => {
                self.require_repr(*src, Double)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::Alloc { dst, .. } => self.require_repr(*dst, Tagged)?,
            LirInst::AllocVectorFill {
                dst, size, fill, ..
            } => {
                self.require_repr(*dst, Tagged)?;
                self.require_repr(*size, Tagged)?;
                self.require_repr(*fill, Tagged)?;
            }
            LirInst::Load { dst, base, .. } | LirInst::LoadField { dst, obj: base, .. } => {
                self.require_repr(*base, Tagged)?;
                self.require(*dst, word, "a word")?;
            }
            LirInst::Store { base, src, .. } => {
                self.require_repr(*base, Tagged)?;
                self.require(*src, word, "a word")?;
            }
            LirInst::StoreField { obj, src, .. }
            | LirInst::StoreNumberTagged { obj, src, .. }
            | LirInst::StoreCell { cell: obj, src }
            | LirInst::StoreFree {
                closure: obj, src, ..
            }
            | LirInst::WriteBarrier { obj, value: src } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*src, Tagged)?;
            }
            LirInst::LoadFree {
                dst, closure: obj, ..
            }
            | LirInst::LoadCell { dst, cell: obj } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::LoadNumberFixnum { dst, obj, .. }
            | LirInst::LoadNumberWide { dst, obj, .. } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*dst, VRepr::RawInt64)?;
            }
            LirInst::MulWide64 {
                dst_lo,
                dst_hi,
                lhs,
                rhs,
            } => {
                self.require_repr(*dst_lo, VRepr::RawInt128)?;
                self.require_repr(*dst_hi, VRepr::RawInt64)?;
                self.require_repr(*lhs, VRepr::RawInt64)?;
                self.require_repr(*rhs, VRepr::RawInt64)?;
            }
            LirInst::WideAdd {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                ..
            }
            | LirInst::WideSub {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                ..
            } => {
                self.require_repr(*dst_lo, VRepr::RawInt128)?;
                self.require_repr(*dst_hi, VRepr::RawInt64)?;
                self.require_repr(*lhs_lo, VRepr::RawInt128)?;
                self.require_repr(*lhs_hi, VRepr::RawInt64)?;
                self.require_repr(*rhs_lo, VRepr::RawInt128)?;
                self.require_repr(*rhs_hi, VRepr::RawInt64)?;
            }
            LirInst::WideCmp {
                dst,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                ..
            } => {
                self.require_repr(*dst, VRepr::RawBits)?;
                self.require_repr(*lhs_lo, VRepr::RawInt128)?;
                self.require_repr(*rhs_lo, VRepr::RawInt128)?;
                self.require_repr(*lhs_hi, VRepr::RawInt64)?;
                self.require_repr(*rhs_hi, VRepr::RawInt64)?;
            }
            LirInst::WideShift {
                dst_lo,
                dst_hi,
                src_lo,
                src_hi,
                ..
            } => {
                self.require_repr(*dst_lo, VRepr::RawInt128)?;
                self.require_repr(*dst_hi, VRepr::RawInt64)?;
                self.require_repr(*src_lo, VRepr::RawInt128)?;
                self.require_repr(*src_hi, VRepr::RawInt64)?;
            }
            LirInst::WideExtend {
                dst_lo,
                dst_hi,
                src,
            } => {
                self.require_repr(*dst_lo, VRepr::RawInt128)?;
                self.require_repr(*dst_hi, VRepr::RawInt64)?;
                self.require_repr(*src, VRepr::RawInt64)?;
            }
            LirInst::WideSplit {
                dst,
                src_lo,
                src_hi,
                ..
            } => {
                self.require_repr(*dst, VRepr::RawInt64)?;
                self.require_repr(*src_lo, VRepr::RawInt128)?;
                self.require_repr(*src_hi, VRepr::RawInt64)?;
            }
            LirInst::BoxInt128 {
                dst,
                src_lo,
                src_hi,
            } => {
                self.require_repr(*dst, Tagged)?;
                self.require_repr(*src_lo, VRepr::RawInt128)?;
                self.require_repr(*src_hi, VRepr::RawInt64)?;
            }
            LirInst::LoadElement { dst, vector, index } => {
                self.require_repr(*vector, Tagged)?;
                self.require_repr(*index, RawInt)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::StoreElement { vector, index, src } => {
                self.require_repr(*vector, Tagged)?;
                self.require_repr(*index, RawInt)?;
                self.require_repr(*src, Tagged)?;
            }
            LirInst::StringRef {
                dst, string, index, ..
            } => {
                self.require_repr(*string, Tagged)?;
                self.require_repr(*index, RawInt)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::LoadByte { dst, base, index } => {
                self.require(*base, word, "a word")?;
                self.require_repr(*index, RawInt)?;
                self.require_repr(*dst, RawInt)?;
            }
            LirInst::StoreByte { base, index, src } => {
                self.require(*base, word, "a word")?;
                self.require_repr(*index, RawInt)?;
                self.require_repr(*src, RawInt)?;
            }
            LirInst::SlotGet { dst, obj, .. } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*dst, Tagged)?;
            }
            LirInst::SlotPut { obj, src, .. } => {
                self.require_repr(*obj, Tagged)?;
                self.require_repr(*src, Tagged)?;
            }
            LirInst::Call {
                target, args, rv, ..
            } => {
                self.verify_args(target, args)?;
                if let Some(rv) = rv {
                    self.require_repr(*rv, Tagged)?;
                }
            }
            LirInst::Poll { .. } => {}
        }
        Ok(())
    }

    /// Arguments of a call.
    fn verify_args(&self, target: &CallTarget, args: &[VReg]) -> Result<(), String> {
        for &arg in args {
            match target {
                CallTarget::Helper(_) => self.require(arg, |r| r != VRepr::Double, "a word")?,
                _ => self.require_repr(arg, VRepr::Tagged)?,
            }
        }
        Ok(())
    }

    fn verify_term(&self, term: &LirTerm) -> Result<(), String> {
        match term {
            LirTerm::Jump(edge) => self.verify_edge(edge)?,
            LirTerm::Branch {
                cond,
                then,
                otherwise,
            } => {
                self.require_repr(*cond, VRepr::RawBits)?;
                self.verify_edge(then)?;
                self.verify_edge(otherwise)?;
            }
            LirTerm::Switch {
                value,
                cases,
                default,
                ..
            } => {
                self.require_repr(*value, VRepr::RawInt)?;
                for (_, edge) in cases {
                    self.verify_edge(edge)?;
                }
                self.verify_edge(default)?;
            }
            LirTerm::Return { value } => self.require_repr(*value, VRepr::Tagged)?,
            LirTerm::TailCall { target, args, .. } => self.verify_args(target, args)?,
            LirTerm::Deopt { .. } | LirTerm::Unreachable => {}
        }
        Ok(())
    }

    /// Each argument of an edge has the representation of its parameter.
    fn verify_edge(&self, edge: &LEdge) -> Result<(), String> {
        let params = &self.blocks[edge.block].params;
        if params.len() != edge.args.len() {
            return Err(format!(
                "an edge to {} passes {} values for {} parameters",
                edge.block,
                edge.args.len(),
                params.len()
            ));
        }
        for (&arg, &param) in edge.args.iter().zip(params) {
            self.require_repr(arg, self.repr(param))?;
        }
        Ok(())
    }

    /// Every frame value is in the representation of its IR type.
    fn verify_frame(&self, values: &[FrameValue]) -> Result<(), String> {
        for fv in values {
            if fv.ty == Type::Int128 {
                return Err(format!(
                    "frame value {} is a two-limb Int128, which a deopt cannot rebuild",
                    fv.ir
                ));
            }
            let want = VRepr::of_type(fv.ty);
            if self.repr(fv.vreg) != want {
                return Err(format!(
                    "frame value {} ({}) is {}, but its type needs {want}",
                    fv.ir,
                    fv.ty,
                    self.repr(fv.vreg)
                ));
            }
        }
        Ok(())
    }

    pub fn new_exit(
        &mut self,
        frame: FrameState,
        values: Vec<FrameValue>,
        kind: GuardKind,
    ) -> DeoptExit {
        self.exits.push(DeoptExitData {
            frame,
            values,
            kind,
        })
    }

    pub fn new_safepoint(&mut self, frame: FrameState, values: Vec<FrameValue>) -> SafePoint {
        self.safepoints.push(SafePointData {
            frame,
            values,
            livemask: None,
            regmask: None,
        })
    }
}

fn write_frame_values(f: &mut Formatter<'_>, values: &[FrameValue]) -> fmt::Result {
    f.write_str("[")?;
    for (i, fv) in values.iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        write!(f, "{}={}:{}", fv.vreg, fv.ir, fv.ty)?;
    }
    f.write_str("]")
}

impl Display for LirFunc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "func entry={} {{", self.entry)?;
        for (block, data) in self.blocks.iter() {
            write!(f, "{block}(")?;
            for (i, &param) in data.params.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{param}: {}", self.repr(param))?;
            }
            write!(f, ")")?;
            if let Some(source) = data.source {
                write!(f, " from {source}")?;
            }
            writeln!(f, ":")?;
            for inst in &data.insts {
                writeln!(f, "    {inst}")?;
            }
            match &data.term {
                Some(term) => writeln!(f, "    {term}")?,
                None => writeln!(f, "    <no terminator>")?,
            }
        }
        writeln!(f, "}}")?;

        for (vreg, data) in self.vregs.iter() {
            write!(f, "{vreg}: {}", data.repr)?;
            if let Some(value) = data.value {
                write!(f, " from {value}")?;
            }
            writeln!(f)?;
        }
        for (exit, data) in self.exits.iter() {
            write!(f, "{exit} = {} ", data.frame)?;
            write_frame_values(f, &data.values)?;
            writeln!(f)?;
        }
        for (sp, data) in self.safepoints.iter() {
            write!(f, "{sp} = {} ", data.frame)?;
            write_frame_values(f, &data.values)?;
            if let Some(mask) = &data.livemask {
                write!(f, " livemask={:x?}", mask.words)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

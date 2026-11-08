use super::*;

impl Display for Test {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Test::Fixnum => f.write_str("fixnum"),
            Test::Flonum => f.write_str("flonum"),
            Test::Pair => f.write_str("pair"),
            Test::Char => f.write_str("char"),
            Test::Boolean => f.write_str("boolean"),
            Test::Number => f.write_str("number"),
            Test::Class(class) => write!(f, "class {class}"),
            Test::Heap => f.write_str("heap"),
            Test::Code(code) => write!(f, "code {code}"),
            Test::CodeOfClosure(code) => write!(f, "code-of-closure {code}"),
            Test::NotInvalid => f.write_str("not-invalid"),
        }
    }
}

impl Display for AllocLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AllocLayout::Pair => f.write_str("pair"),
            AllocLayout::Flonum => f.write_str("flonum"),
            AllocLayout::Closure { code, free } => write!(f, "closure {code} free={free}"),
            AllocLayout::Env { slots } => write!(f, "env slots={slots}"),
            AllocLayout::Vector { len } => write!(f, "vector len={len}"),
            AllocLayout::Tuple { len } => write!(f, "tuple len={len}"),
            AllocLayout::Instance { class, slots } => {
                write!(f, "instance class={class} slots={slots}")
            }
            AllocLayout::SlotStorage { stamp, len, .. } => {
                write!(f, "slot-storage stamp={stamp} len={len}")
            }
            AllocLayout::Rational => f.write_str("rational"),
        }
    }
}

impl Display for CallTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CallTarget::Known(code) => write!(f, "known {code}"),
            CallTarget::Generic => f.write_str("generic"),
            CallTarget::Helper(helper) => write!(f, "helper {helper}"),
            CallTarget::Prim(prim) => write!(f, "prim {prim}"),
            CallTarget::PrimN(prim) => write!(f, "prim-n {prim}"),
        }
    }
}

impl Display for PhysRole {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PhysRole::Rv => f.write_str("rv"),
            PhysRole::OpArg(n) => write!(f, "op-arg{n}"),
        }
    }
}

impl Display for IntOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            IntOp::Add => "add",
            IntOp::Sub => "sub",
            IntOp::Mul => "mul",
            IntOp::Quot => "quot",
            IntOp::Rem => "rem",
            IntOp::Mod => "mod",
        })
    }
}

impl Display for FloatOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            FloatOp::Add => "add",
            FloatOp::Sub => "sub",
            FloatOp::Mul => "mul",
            FloatOp::Div => "div",
        })
    }
}

impl Display for WideShiftOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            WideShiftOp::Shl => "shl",
            WideShiftOp::Shr => "shr",
            WideShiftOp::Ashr => "ashr",
        })
    }
}

fn write_vregs(f: &mut Formatter<'_>, vregs: &[VReg]) -> fmt::Result {
    for (i, vreg) in vregs.iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        write!(f, "{vreg}")?;
    }
    Ok(())
}

impl Display for LirInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LirInst::Move { dst, src } => write!(f, "{dst} = move {src}"),
            LirInst::Const { dst, value } => write!(f, "{dst} = const {value}"),
            LirInst::TagInt { dst, src } => write!(f, "{dst} = tag.int {src}"),
            LirInst::BoolToValue { dst, src } => write!(f, "{dst} = bool.value {src}"),
            LirInst::UntagInt { dst, src } => write!(f, "{dst} = untag.int {src}"),
            LirInst::UntagInt64 { dst, src } => write!(f, "{dst} = untag.int64 {src}"),
            LirInst::TruncateI64ToI32 { dst, src } => write!(f, "{dst} = truncate.int64 {src}"),
            LirInst::IntToDouble { dst, src } => write!(f, "{dst} = int.to.double {src}"),
            LirInst::Int64ToDouble { dst, src } => write!(f, "{dst} = int64.to.double {src}"),
            LirInst::FloorF64 { dst, src } => write!(f, "{dst} = floor.f64 {src}"),
            LirInst::CeilingF64 { dst, src } => write!(f, "{dst} = ceiling.f64 {src}"),
            LirInst::TruncateF64 { dst, src } => write!(f, "{dst} = truncate.f64 {src}"),
            LirInst::SqrtF64 { dst, src } => write!(f, "{dst} = sqrt.f64 {src}"),
            LirInst::UntagChar { dst, src } => write!(f, "{dst} = untag.char {src}"),
            LirInst::Guard { test, src, exit } => {
                write!(f, "guard {test}")?;
                if let Some(src) = src {
                    write!(f, " {src}")?;
                }
                write!(f, " [{exit}]")
            }
            LirInst::GuardCmp {
                cmp,
                lhs,
                rhs,
                exit,
            } => write!(f, "guard.icmp.{cmp} {lhs}, {rhs} [{exit}]"),
            LirInst::GuardIndex {
                index,
                length,
                exit,
            } => write!(f, "guard.index {index}, {length} [{exit}]"),
            LirInst::TestTag { dst, src, test } => write!(f, "{dst} = test {test} {src}"),
            LirInst::IntArith {
                op,
                dst,
                lhs,
                rhs,
                exit,
            } => write!(f, "{dst} = int.{op} {lhs}, {rhs} [{exit}]"),
            LirInst::Int64Arith {
                op,
                dst,
                lhs,
                rhs,
                exit,
            } => write!(f, "{dst} = int64.{op} {lhs}, {rhs} [{exit}]"),
            LirInst::Int64Wrapping {
                op,
                dst,
                lhs,
                rhs,
            } => write!(f, "{dst} = int64.{op}.wrap {lhs}, {rhs}"),
            LirInst::BoxInt64 { dst, src } => {
                write!(f, "{dst} = box.int64 {src}")
            }
            LirInst::UnboxBigInt64 { dst, obj, exit } => {
                write!(f, "{dst} = unbox.bigint64 {obj} [{exit}]")
            }
            LirInst::IntCmp {
                cmp,
                dst,
                lhs,
                rhs,
            } => write!(f, "{dst} = icmp.{cmp} {lhs}, {rhs}"),
            LirInst::FloatArith {
                op,
                dst,
                lhs,
                rhs,
            } => write!(f, "{dst} = f64.{op} {lhs}, {rhs}"),
            LirInst::FloatCmp {
                cmp,
                dst,
                lhs,
                rhs,
            } => write!(f, "{dst} = fcmp.{cmp} {lhs}, {rhs}"),
            LirInst::LoadFlonum { dst, obj } => write!(f, "{dst} = load.flonum {obj}"),
            LirInst::StoreFlonum { obj, src } => write!(f, "store.flonum {obj}, {src}"),
            LirInst::BoxDouble { dst, src } => write!(f, "{dst} = box.double {src}"),
            LirInst::Alloc { dst, layout } => write!(f, "{dst} = alloc {layout}"),
            LirInst::AllocVectorFill {
                dst,
                size,
                fill,
                exit,
            } => write!(f, "{dst} = alloc.vector.fill {size}, {fill} [{exit}]"),
            LirInst::Load { dst, base, offset } => write!(f, "{dst} = load {base}+{offset}"),
            LirInst::Store { base, offset, src } => write!(f, "store {base}+{offset}, {src}"),
            LirInst::LoadField { dst, obj, field } => write!(f, "{dst} = field {obj}[{field}]"),
            LirInst::StoreField { obj, field, src } => {
                write!(f, "store.field {obj}[{field}], {src}")
            }
            LirInst::StoreNumberTagged {
                obj,
                offset,
                src,
                exit,
                ..
            } => write!(f, "store.rational {obj}+{offset}, {src} [{exit}]"),
            LirInst::LoadNumberFixnum {
                dst,
                obj,
                offset,
                exit,
            } => write!(f, "{dst} = load.rational {obj}+{offset} [{exit}]"),
            LirInst::LoadNumberWide {
                dst,
                obj,
                offset,
                exit,
            } => write!(f, "{dst} = load.rational.wide {obj}+{offset} [{exit}]"),
            LirInst::MulWide64 {
                dst_lo,
                dst_hi,
                lhs,
                rhs,
            } => write!(f, "({dst_lo}, {dst_hi}) = mul.wide {lhs}, {rhs}"),
            LirInst::WideAdd {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            } => write!(
                f,
                "({dst_lo}, {dst_hi}) = add.wide ({lhs_lo}, {lhs_hi}), ({rhs_lo}, {rhs_hi}) [{exit}]"
            ),
            LirInst::WideSub {
                dst_lo,
                dst_hi,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
                exit,
            } => write!(
                f,
                "({dst_lo}, {dst_hi}) = sub.wide ({lhs_lo}, {lhs_hi}), ({rhs_lo}, {rhs_hi}) [{exit}]"
            ),
            LirInst::WideCmp {
                cmp,
                dst,
                lhs_lo,
                lhs_hi,
                rhs_lo,
                rhs_hi,
            } => write!(
                f,
                "{dst} = cmp.wide.{cmp} ({lhs_lo}, {lhs_hi}), ({rhs_lo}, {rhs_hi})"
            ),
            LirInst::WideShift {
                op,
                dst_lo,
                dst_hi,
                src_lo,
                src_hi,
                amount,
            } => write!(
                f,
                "({dst_lo}, {dst_hi}) = {op}.wide ({src_lo}, {src_hi}), {amount}"
            ),
            LirInst::WideExtend { dst_lo, dst_hi, src } => {
                write!(f, "({dst_lo}, {dst_hi}) = extend.wide {src}")
            }
            LirInst::WideSplit {
                dst,
                src_lo,
                src_hi,
                high,
            } => write!(
                f,
                "{dst} = split.wide ({src_lo}, {src_hi}) {}",
                if *high { "high" } else { "low" }
            ),
            LirInst::BoxInt128 {
                dst, src_lo, src_hi,
            } => write!(f, "{dst} = box.int128 ({src_lo}, {src_hi})"),
            LirInst::LoadFree {
                dst,
                closure,
                index,
            } => write!(f, "{dst} = free {closure}[{index}]"),
            LirInst::StoreFree {
                closure,
                index,
                src,
            } => write!(f, "store.free {closure}[{index}], {src}"),
            LirInst::LoadCell { dst, cell } => write!(f, "{dst} = cell.ref {cell}"),
            LirInst::StoreCell { cell, src } => write!(f, "cell.set {cell}, {src}"),
            LirInst::WriteBarrier { obj, value } => write!(f, "write-barrier {obj}, {value}"),
            LirInst::LoadElement {
                dst,
                vector,
                index,
            } => write!(f, "{dst} = element {vector}[{index}]"),
            LirInst::StoreElement { vector, index, src } => {
                write!(f, "store.element {vector}[{index}], {src}")
            }
            LirInst::LoadByte { dst, base, index } => {
                write!(f, "{dst} = load.byte {base}[{index}]")
            }
            LirInst::StringRef {
                dst,
                string,
                index,
                exit,
            } => {
                write!(f, "{dst} = string.ref {string}, {index} [{exit}]")
            }
            LirInst::StoreByte { base, index, src } => {
                write!(f, "store.byte {base}[{index}], {src}")
            }
            LirInst::SlotGet { dst, obj, meta, exit } => {
                write!(f, "{dst} = slot.get {obj} @{meta:#x} else {exit}")
            }
            LirInst::SlotPut { obj, src, meta, exit } => {
                write!(f, "slot.put {obj}, {src} @{meta:#x} else {exit}")
            }
            LirInst::Call {
                target,
                args,
                rv,
                safepoint,
                ..
            } => {
                if let Some(rv) = rv {
                    write!(f, "{rv} = ")?;
                }
                write!(f, "call {target}(")?;
                write_vregs(f, args)?;
                write!(f, ") [{safepoint}]")
            }
            LirInst::Poll { safepoint } => write!(f, "poll [{safepoint}]"),
        }
    }
}

fn write_edge(f: &mut Formatter<'_>, edge: &LEdge) -> fmt::Result {
    write!(f, "{}", edge.block)?;
    if !edge.args.is_empty() {
        f.write_str("(")?;
        write_vregs(f, &edge.args)?;
        f.write_str(")")?;
    }
    Ok(())
}

impl Display for LirTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LirTerm::Jump(edge) => {
                f.write_str("jump ")?;
                write_edge(f, edge)
            }
            LirTerm::Branch {
                cond,
                then,
                otherwise,
            } => {
                write!(f, "branch {cond} ")?;
                write_edge(f, then)?;
                f.write_str(" else ")?;
                write_edge(f, otherwise)
            }
            LirTerm::Switch {
                value,
                cases,
                default,
                density,
            } => {
                match density {
                    SwitchDensity::Dense { min } => write!(f, "switch.dense {value} min {min} [")?,
                    SwitchDensity::Sparse => write!(f, "switch {value} [")?,
                }
                for (i, (n, edge)) in cases.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{n} -> ")?;
                    write_edge(f, edge)?;
                }
                f.write_str("] default ")?;
                write_edge(f, default)
            }
            LirTerm::Return { value } => write!(f, "return {value}"),
            LirTerm::TailCall { target, args, checked } => {
                write!(f, "tailcall {target}(")?;
                write_vregs(f, args)?;
                write!(f, "){}", if *checked { " checked" } else { "" })
            }
            LirTerm::Deopt { exit } => write!(f, "deopt [{exit}]"),
            LirTerm::Unreachable => f.write_str("unreachable"),
        }
    }
}


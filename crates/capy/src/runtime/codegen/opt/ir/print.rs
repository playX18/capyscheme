//! The textual form of IR, for `CAPY_OPT_DUMP`.
//!
//! The format is one block per paragraph, with instructions indented under
//! their block. Frame states are listed after the blocks and named where
//! they are used.

use std::fmt::{self, Display, Formatter, Write};

use super::func::{Block, Func, Value};
use super::inst::{
    Cmp, Constant, Edge, Op, RematKind, Repr, ReturnValues, SwitchDensity, Terminator,
};
use super::types::{CodeSource, Type};

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bottom => write!(f, "bottom"),
            Type::Int32 => write!(f, "int32"),
            Type::Int64 => write!(f, "int64"),
            Type::Int128 => write!(f, "int128"),
            Type::Double => write!(f, "double"),
            Type::Bool => write!(f, "bool"),
            Type::Null => write!(f, "null"),
            Type::Pair => write!(f, "pair"),
            Type::Closure(None) => write!(f, "closure"),
            Type::Closure(Some(code)) => write!(f, "closure<{code}>"),
            Type::Vector => write!(f, "vector"),
            Type::Cell => write!(f, "cell"),
            Type::Heap(class) => write!(f, "heap<{class}>"),
            Type::Rational => write!(f, "rational"),
            Type::Any => write!(f, "any"),
        }
    }
}

impl Display for RematKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RematKind::Pair => write!(f, "pair"),
            RematKind::Closure { code, free } => write!(f, "closure {code} free={free}"),
            RematKind::Env { slots } => write!(f, "env slots={slots}"),
            RematKind::Vector { len } => write!(f, "vector len={len}"),
            RematKind::Tuple { len } => write!(f, "tuple len={len}"),
            RematKind::Instance { class, slots } => {
                write!(f, "instance class={class} slots={slots}")
            }
            RematKind::Variable => write!(f, "variable"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Int32(n) => write!(f, "{n}"),
            Constant::Int64(n) => write!(f, "{n}L"),
            Constant::Double(bits) => write!(f, "{:?}", f64::from_bits(*bits)),
            Constant::Bits(bits) => write!(f, "{bits:#x}"),
            Constant::Table(slot) => write!(f, "table[{slot}]"),
            Constant::Pool { code, index } => write!(f, "pool[{code}][{index}]"),
        }
    }
}

impl Display for Cmp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            Cmp::Eq => "eq",
            Cmp::Ne => "ne",
            Cmp::Lt => "lt",
            Cmp::Le => "le",
            Cmp::Gt => "gt",
            Cmp::Ge => "ge",
        };
        f.write_str(name)
    }
}

impl Display for ReturnValues {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ReturnValues::Error => f.write_str("values-error"),
            ReturnValues::Ignore => f.write_str("values-ignore"),
            ReturnValues::Forward => f.write_str("values-forward"),
            ReturnValues::Take(count) => write!(f, "values-take{count}"),
            ReturnValues::TakeRest(count) => write!(f, "values-take{count}+"),
        }
    }
}

impl Display for Repr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Repr::Int32 => f.write_str("int32"),
            Repr::Int64 => f.write_str("int64"),
            Repr::Int128 => f.write_str("int128"),
            Repr::Double => f.write_str("double"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Op::Const(c) => write!(f, "const {c}"),
            Op::CheckInt32 => write!(f, "check.int32"),
            Op::CheckInt64 => write!(f, "check.int64"),
            Op::CheckBigIntFits64 => write!(f, "check.bigint.fits64"),
            Op::CheckDouble => write!(f, "check.double"),
            Op::CheckChar => write!(f, "check.char"),
            Op::CheckClass(class) => write!(f, "check.class {class}"),
            Op::CheckCode(code) => write!(f, "check.code {code}"),
            Op::CheckNotInvalid => write!(f, "check.not-invalid"),
            Op::Box(repr) => write!(f, "box.{repr}"),
            Op::Unbox(repr) => write!(f, "unbox.{repr}"),
            Op::Int64ToInt32 => write!(f, "int64.to.int32"),
            Op::AddI32Ovf => write!(f, "add.i32.ovf"),
            Op::SubI32Ovf => write!(f, "sub.i32.ovf"),
            Op::MulI32Ovf => write!(f, "mul.i32.ovf"),
            Op::QuotI32 => write!(f, "quot.i32"),
            Op::RemI32 => write!(f, "rem.i32"),
            Op::ModI32 => write!(f, "mod.i32"),
            Op::AddI64 => write!(f, "add.i64"),
            Op::SubI64 => write!(f, "sub.i64"),
            Op::MulI64 => write!(f, "mul.i64"),
            Op::AddI64Ovf => write!(f, "add.i64.ovf"),
            Op::SubI64Ovf => write!(f, "sub.i64.ovf"),
            Op::MulI64Ovf => write!(f, "mul.i64.ovf"),
            Op::QuotI64 => write!(f, "quot.i64"),
            Op::RemI64 => write!(f, "rem.i64"),
            Op::ModI64 => write!(f, "mod.i64"),
            Op::CmpI64(cmp) => write!(f, "cmp.i64.{cmp}"),
            Op::AddF64 => write!(f, "add.f64"),
            Op::SubF64 => write!(f, "sub.f64"),
            Op::MulF64 => write!(f, "mul.f64"),
            Op::DivF64 => write!(f, "div.f64"),
            Op::FloorF64 => write!(f, "floor.f64"),
            Op::CeilingF64 => write!(f, "ceiling.f64"),
            Op::TruncateF64 => write!(f, "truncate.f64"),
            Op::SqrtF64 => write!(f, "sqrt.f64"),
            Op::CmpI32(cmp) => write!(f, "cmp.i32.{cmp}"),
            Op::CmpF64(cmp) => write!(f, "cmp.f64.{cmp}"),
            Op::EqBits => write!(f, "eq.bits"),
            Op::TestInt32 => write!(f, "test.int32"),
            Op::TestInt64 => write!(f, "test.int64"),
            Op::TestDouble => write!(f, "test.double"),
            Op::Int32ToDouble => write!(f, "int32.to.double"),
            Op::Int64ToDouble => write!(f, "int64.to.double"),
            Op::TestPair => write!(f, "test.pair"),
            Op::TestChar => write!(f, "test.char"),
            Op::TestBoolean => write!(f, "test.boolean"),
            Op::TestNumber => write!(f, "test.number"),
            Op::TestClass(class) => write!(f, "test.class {class}"),
            Op::TestHeap => write!(f, "test.heap"),
            Op::TestCode(code) => write!(f, "test.code {code}"),
            Op::BoolToValue => write!(f, "box.bool"),
            Op::LoadRationalComponent { index } => write!(f, "load.rational[{index}]"),
            Op::LoadRationalComponentWide { index } => write!(f, "load.rational.wide[{index}]"),
            Op::Mul64x64To128 => write!(f, "mul.64x64.128"),
            Op::Add128Ovf => write!(f, "add.i128.ovf"),
            Op::Sub128Ovf => write!(f, "sub.i128.ovf"),
            Op::Cmp128(cmp) => write!(f, "cmp.i128.{cmp}"),
            Op::Shl128(amount) => write!(f, "shl.i128 {amount}"),
            Op::Shr128(amount) => write!(f, "shr.i128 {amount}"),
            Op::Ashr128(amount) => write!(f, "ashr.i128 {amount}"),
            Op::ExtendI64To128 => write!(f, "extend.i64.i128"),
            Op::Int128Low => write!(f, "i128.low"),
            Op::Int128High => write!(f, "i128.high"),
            Op::Load { field, region } => write!(f, "load[{field}] {region:?}"),
            Op::Store { field, region } => write!(f, "store[{field}] {region:?}"),
            Op::LoadFree { index } => write!(f, "load.free[{index}]"),
            Op::ReturnedValue(index) => write!(f, "returned[{index}]"),
            Op::ReturnedCount(skip) => write!(f, "returned.count -{skip}"),
            Op::ReturnedAddr(index) => write!(f, "returned.addr[{index}]"),
            Op::LoadCell => write!(f, "load.cell"),
            Op::StoreCell => write!(f, "store.cell"),
            Op::StoreFree { index } => write!(f, "store.free[{index}]"),
            Op::WriteBarrier => write!(f, "write-barrier"),
            Op::VectorLength => write!(f, "vector.length"),
            Op::CheckStringIndex => write!(f, "check.string.index"),
            Op::StringRef => write!(f, "string.ref"),
            Op::CheckIndex => write!(f, "check.index"),
            Op::CheckIndexRaw => write!(f, "check.index.raw"),
            Op::LoadElement => write!(f, "load.element"),
            Op::StoreElement => write!(f, "store.element"),
            Op::LoadBytevectorU8 => write!(f, "load.bytevector.u8"),
            Op::StoreBytevectorU8 => write!(f, "store.bytevector.u8"),
            Op::SlotGet { meta } => write!(f, "slot.get @{meta:#x}"),
            Op::SlotPut { meta } => write!(f, "slot.put @{meta:#x}"),
            Op::AllocPair => write!(f, "alloc.pair"),
            Op::AllocClosure(code) => write!(f, "alloc.closure {code}"),
            Op::AllocEnv => write!(f, "alloc.env"),
            Op::AllocVector => write!(f, "alloc.vector"),
            Op::AllocVectorFill => write!(f, "alloc.vector.fill"),
            Op::AllocTuple => write!(f, "alloc.tuple"),
            Op::AllocInstance { class, slots } => {
                write!(f, "alloc.instance class={class} slots={slots}")
            }
            Op::AllocRational => write!(f, "alloc.rational"),
            Op::Remat(kind) => write!(f, "remat {kind}"),
            Op::CallKnown(code, values) => write!(f, "call.known {code} {values}"),
            Op::CallGeneric(values) => write!(f, "call.generic {values}"),
            Op::CallOp(helper) => write!(f, "call.op {helper}"),
            Op::CallPrim { prim, .. } => write!(f, "call.prim {prim}"),
            Op::Safepoint => write!(f, "safepoint"),
        }
    }
}

impl Display for Func<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "func {} entry={} {{", self.label, self.entry)?;
        for (block, data) in self.blocks.iter() {
            write_block_header(self, f, block)?;
            for &inst in &data.insts {
                let idata = &self.insts[inst];
                f.write_str("    ")?;
                if let Some(result) = idata.result {
                    write!(f, "{result}: {} = ", self.value_type(result))?;
                }
                write!(f, "{}", idata.op)?;
                for arg in self.args(inst) {
                    write!(f, " {arg}")?;
                }
                if let Some(frame) = idata.frame {
                    write!(f, " [{frame}]")?;
                }
                writeln!(f)?;
            }
            f.write_str("    ")?;
            match &data.term {
                Some(term) => write_terminator(self, f, term)?,
                None => f.write_str("<no terminator>")?,
            }
            writeln!(f)?;
        }
        writeln!(f, "}}")?;

        for (frame, data) in self.frames.iter() {
            write!(
                f,
                "{frame} = {} pc={} closure={} regs=[",
                data.code, data.pc, data.closure
            )?;
            for (i, (reg, value)) in data.registers.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write!(f, "r{reg}={value}")?;
            }
            f.write_char(']')?;
            if let Some(parent) = data.parent {
                write!(f, " parent={parent}")?;
            }
            if let Some(returned) = data.returned {
                write!(f, " returned(base={} count={:?})", returned.base, returned.count)?;
            }
            writeln!(f)?;
        }
        if !self.codes.is_empty() {
            writeln!(f, "codes:")?;
            for (code, source) in self.codes.iter() {
                writeln!(f, "  {code} = {}", describe_code(self, source))?;
            }
        }
        if !self.heap_constants.is_empty() {
            writeln!(f, "heap constants:")?;
            for (index, value) in self.heap_constants.iter().enumerate() {
                writeln!(f, "  table[{}] = {}", index + 1, describe_heap(*value))?;
            }
        }
        if !self.class_deps.is_empty() {
            writeln!(f, "class dependencies:")?;
            for dep in &self.class_deps {
                writeln!(
                    f,
                    "  {} id={} stamp={}",
                    describe_heap(dep.class),
                    dep.id,
                    dep.stamp
                )?;
            }
        }
        Ok(())
    }
}

fn describe_code(func: &Func<'_>, source: &CodeSource<'_>) -> String {
    match source {
        CodeSource::Root => func.label.clone(),
        CodeSource::Pool(index) => format!("pool[{index}]"),
        CodeSource::Heap(value) => describe_heap(*value),
    }
}

fn describe_heap(value: crate::runtime::objectmodel::Value<'_>) -> String {
    if value.is::<crate::runtime::objectmodel::proc::Closure>() {
        let closure = value.downcast::<crate::runtime::objectmodel::proc::Closure>();
        crate::runtime::objectmodel::proc::procedure_label(
            crate::runtime::objectmodel::proc::Closure::code_block(closure),
        )
    } else {
        "<heap value>".to_string()
    }
}

fn write_block_header(func: &Func<'_>, f: &mut Formatter<'_>, block: Block) -> fmt::Result {
    write!(f, "{block}(")?;
    for (i, &param) in func.params(block).iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        write!(f, "{param}: {}", func.value_type(param))?;
    }
    writeln!(f, "):")
}

fn write_edge(func: &Func<'_>, f: &mut Formatter<'_>, edge: &Edge) -> fmt::Result {
    write!(f, "{}", edge.block)?;
    let args = func.edge_args(edge);
    if !args.is_empty() {
        f.write_str("(")?;
        write_values(f, args)?;
        f.write_str(")")?;
    }
    Ok(())
}

fn write_values(f: &mut Formatter<'_>, values: &[Value]) -> fmt::Result {
    for (i, value) in values.iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        write!(f, "{value}")?;
    }
    Ok(())
}

fn write_terminator(func: &Func<'_>, f: &mut Formatter<'_>, term: &Terminator) -> fmt::Result {
    match term {
        Terminator::Jump(edge) => {
            f.write_str("jump ")?;
            write_edge(func, f, edge)
        }
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => {
            write!(f, "branch {cond} ")?;
            write_edge(func, f, then)?;
            f.write_str(" else ")?;
            write_edge(func, f, otherwise)
        }
        Terminator::Switch {
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
                write_edge(func, f, edge)?;
            }
            f.write_str("] default ")?;
            write_edge(func, f, default)
        }
        Terminator::Return(value) => write!(f, "return {value}"),
        Terminator::ReturnMany { values } => {
            f.write_str("return-values (")?;
            write_values(f, values)?;
            f.write_str(")")
        }
        Terminator::TailCall {
            callee,
            args,
            frame,
            check,
        } => {
            write!(f, "tailcall {callee}(")?;
            write_values(f, args.as_slice(&func.pool))?;
            write!(f, ") [{frame}]")?;
            match check {
                Some(code) => write!(f, " check {code}"),
                None => Ok(()),
            }
        }
        Terminator::Deopt { frame } => write!(f, "deopt [{frame}]"),
        Terminator::Unreachable => f.write_str("unreachable"),
    }
}

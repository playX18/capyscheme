//! The allocations whose object cannot have a second reference before an OSR
//! entry.

use cranelift_entity::{EntityRef, SecondaryMap};

use crate::runtime::codegen::opt::ir::func::{Func, Value};
use crate::runtime::codegen::opt::ir::inst::{Helper, Inst, Op, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::prims::def::Primitive;

/// Fills `func.osr_unaliased`. A function without OSR entries gets an empty
/// set.
pub(crate) fn compute(func: &mut Func) {
    func.osr_unaliased.clear();
    if func.osr.is_empty() {
        return;
    }
    let mut sites = Vec::new();
    for (_, data) in func.blocks.iter() {
        for &inst in &data.insts {
            if func.insts[inst].result.is_some() && is_alloc(func, inst) {
                sites.push(inst);
            }
        }
    }
    let unaliased: Vec<Inst> = sites
        .into_iter()
        .filter(|&site| !escapes(func, site))
        .collect();
    func.osr_unaliased.extend(unaliased);
}

/// Whether `inst` allocates an object that PEA can track.
pub(super) fn is_alloc(func: &Func, inst: Inst) -> bool {
    match func.insts[inst].op {
        Op::AllocPair
        | Op::AllocClosure(_)
        | Op::AllocEnv
        | Op::AllocVector
        | Op::AllocVectorFill
        | Op::AllocTuple
        | Op::AllocInstance { .. } => true,
        Op::CallPrim { prim, .. } => {
            Primitive::from_id(prim.index() as u16) == Some(Primitive::MakeBox)
        }
        _ => false,
    }
}

/// The level of a value in the flow from one site.
const NOT: u8 = 0;
/// The object itself, or something that holds it.
const DIRECT: u8 = 1;
/// A value loaded from such an object once the object has been put into
/// another object: it may be the object. It may go to operations that do not
/// retain their operands.
const LOADED: u8 = 2;

fn escapes(func: &Func, site: Inst) -> bool {
    let mut level: SecondaryMap<Value, u8> = SecondaryMap::new();
    let mut contained = false;
    let mut changed = true;
    let mut bad = false;
    let add = |level: &mut SecondaryMap<Value, u8>, changed: &mut bool, v: Value, l: u8| {
        let cur = level[v];
        if cur == NOT || cur > l {
            level[v] = l;
            *changed = true;
        }
    };
    let result = func.insts[site].result.expect("a site has a result");
    add(&mut level, &mut changed, result, DIRECT);
    while changed && !bad {
        changed = false;
        for (_, data) in func.blocks.iter() {
            for &inst in &data.insts {
                let op = func.insts[inst].op;
                let args = func.args(inst);
                let held = |v: &Value| level[*v] != NOT;
                if !args.iter().any(held) {
                    continue;
                }
                let res = func.insts[inst].result;
                if is_alloc(func, inst) {
                    if !contained {
                        contained = true;
                        changed = true;
                    }
                    if let Some(r) = res {
                        add(&mut level, &mut changed, r, DIRECT);
                    }
                    continue;
                }
                match op {
                    Op::CheckClass(_) | Op::CheckCode(_) => {
                        if let Some(r) = res {
                            let l = level[args[0]];
                            if l != NOT {
                                add(&mut level, &mut changed, r, l);
                            }
                        }
                    }
                    Op::Load { .. }
                    | Op::LoadFree { .. }
                    | Op::LoadCell
                    | Op::LoadElement
                    | Op::SlotGet { .. } => {
                        // Only the object read from may be held; any other
                        // operand (an index) must not be.
                        if held(&args[0]) && !args[1..].iter().any(held) {
                            if let (Some(r), true) = (res, contained) {
                                add(&mut level, &mut changed, r, LOADED);
                            }
                        } else {
                            bad = true;
                        }
                    }
                    Op::Store { .. }
                    | Op::StoreCell
                    | Op::StoreFree { .. }
                    | Op::StoreElement
                    | Op::SlotPut { .. }
                    | Op::WriteBarrier => {
                        // A held value may be stored only into an object that
                        // holds the object too.
                        for arg in &args[1..] {
                            if held(arg) {
                                if held(&args[0]) {
                                    if !contained {
                                        contained = true;
                                        changed = true;
                                    }
                                } else {
                                    bad = true;
                                }
                            }
                        }
                    }
                    Op::CheckIndex
                    | Op::CheckIndexRaw
                    | Op::VectorLength
                    | Op::EqBits
                    | Op::CheckNotInvalid
                    | Op::Safepoint => {}
                    Op::CallHelper(helper)
                        if helper == Helper::Arith.helper_ref()
                            || helper == Helper::Length.helper_ref() =>
                    {
                        if args.iter().any(|v| level[*v] == DIRECT) {
                            bad = true;
                        }
                    }
                    _ if !op.is_call() && res.is_none_or(|r| raw_result(func.value_type(r))) => {}
                    _ => bad = true,
                }
            }
            let Some(term) = &data.term else {
                continue;
            };
            let returned = match term {
                Terminator::Return(v) => level[*v] != NOT,
                Terminator::ReturnMany { values } => values.iter().any(|v| level[*v] != NOT),
                Terminator::TailCall { callee, args, .. } => {
                    level[*callee] != NOT
                        || args.as_slice(&func.pool).iter().any(|v| level[*v] != NOT)
                }
                _ => false,
            };
            if returned {
                bad = true;
            }
            for edge in term.edges() {
                let params = func.params(edge.block);
                for (&param, &arg) in params.iter().zip(func.edge_args(&edge)) {
                    let l = level[arg];
                    if l != NOT {
                        add(&mut level, &mut changed, param, l);
                    }
                }
            }
        }
    }
    bad
}

/// Whether a value of type `ty` is not a reference.
fn raw_result(ty: Type) -> bool {
    matches!(
        ty,
        Type::Int32 | Type::Int64 | Type::Int128 | Type::Double | Type::Bool
    )
}

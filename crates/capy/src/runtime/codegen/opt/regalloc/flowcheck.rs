//! A value-flow checker for allocations.
//!
//! The structural checks in [`super::check`] ask where a value *should* be at
//! each operand. This one asks how it gets there: it replays the resolved moves
//! and the instructions over the control-flow graph, tracking which vreg each
//! location holds, and rejects any use that does not find its vreg where the
//! allocation says it lives.

use std::collections::HashMap;

use cranelift_entity::SecondaryMap;

use super::liveness::Numbering;
use super::{Allocation, Loc, RegFile, intervals, pos};
use crate::runtime::codegen::opt::lir::{Clobbers, LBlock, LirFunc, Role, VReg};

type State = HashMap<Loc, VReg>;

pub(crate) fn check(
    func: &LirFunc,
    num: &Numbering,
    alloc: &Allocation,
    regs: &RegFile,
) -> Result<(), String> {
    let mut preds: SecondaryMap<LBlock, Vec<LBlock>> = SecondaryMap::new();
    for &b in &num.order {
        if let Some(term) = &func.blocks[b].term {
            for e in term.edges() {
                preds[e.block].push(b);
            }
        }
    }

    // The function entry defines its parameters where the allocation says.
    let mut seed = State::new();
    let entry_pos = num.from(func.entry);
    for &p in &func.blocks[func.entry].params {
        if let Some(loc) = alloc.loc_at(p, entry_pos) {
            seed.insert(loc, alloc.canon(p));
        }
    }

    let mut outs: SecondaryMap<LBlock, Option<State>> = SecondaryMap::new();
    let entry_in = |outs: &SecondaryMap<LBlock, Option<State>>, b: LBlock| -> Option<State> {
        let mut acc: Option<State> = if b == func.entry {
            Some(seed.clone())
        } else {
            None
        };
        for &p in &preds[b] {
            let Some(out) = &outs[p] else { continue };
            let Some(term) = &func.blocks[p].term else {
                continue;
            };
            for e in term.edges().into_iter().filter(|e| e.block == b) {
                // Crossing an edge renames each argument to its parameter: the
                // move put the value where the parameter lives, but the state
                // still calls it by the argument's name.
                let mut st = out.clone();
                let target_pos = num.from(b);
                for (i, &arg) in e.args.iter().enumerate() {
                    let Some(&param) = func.blocks[b].params.get(i) else {
                        break;
                    };
                    if let Some(loc) = alloc.loc_at(param, target_pos) {
                        if out.get(&loc) == Some(&alloc.canon(arg)) {
                            st.insert(loc, alloc.canon(param));
                        }
                    }
                }
                acc = Some(match acc {
                    None => st,
                    Some(mut a) => {
                        a.retain(|loc, v| st.get(loc) == Some(v));
                        a
                    }
                });
            }
        }
        acc
    };

    // Iterate to a fixed point without reporting: states only shrink, so an
    // optimistic early state never hides an error from the final pass.
    let limit = num.order.len() * 8 + 16;
    for _ in 0..limit {
        let mut changed = false;
        for &b in &num.order {
            let Some(input) = entry_in(&outs, b) else {
                continue;
            };
            let out = simulate(func, num, alloc, regs, b, input, false)?;
            if outs[b].as_ref() != Some(&out) {
                outs[b] = Some(out);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    for &b in &num.order {
        let Some(input) = entry_in(&outs, b) else {
            return Err(format!("{b} is never reached by the value-flow check"));
        };
        simulate(func, num, alloc, regs, b, input, true)?;
    }
    Ok(())
}

/// Runs one block over `state` and returns the state at its end. With `report`
/// set, a use that finds the wrong value is an error.
fn simulate(
    func: &LirFunc,
    num: &Numbering,
    alloc: &Allocation,
    regs: &RegFile,
    b: LBlock,
    mut state: State,
    report: bool,
) -> Result<State, String> {
    let data = &func.blocks[b];
    let base = num.base[b];

    let apply_moves = |state: &mut State, g: u32| {
        for m in &alloc.moves[g as usize] {
            match state.get(&m.src).copied() {
                Some(v) => {
                    state.insert(m.dst, v);
                }
                None => {
                    state.remove(&m.dst);
                }
            }
        }
    };

    let check_uses = |state: &State, g: u32, ops: &[intervals::OpInfo]| -> Result<(), String> {
        if !report {
            return Ok(());
        }
        for op in ops.iter().filter(|o| o.role == Role::Use) {
            let p = pos::early(g);
            let loc = alloc
                .loc_at(op.vreg, p)
                .ok_or_else(|| format!("{} has no location at {p} in {b}", op.vreg))?;
            match state.get(&loc) {
                Some(&v) if v == alloc.canon(op.vreg) => {}
                Some(&other) => {
                    return Err(format!(
                        "{b}: {} is read from {loc} at {p}, which holds {other}",
                        op.vreg
                    ));
                }
                None => {
                    return Err(format!(
                        "{b}: {} is read from {loc} at {p}, which holds nothing",
                        op.vreg
                    ));
                }
            }
        }
        Ok(())
    };

    for (k, inst) in data.insts.iter().enumerate() {
        let g = base + k as u32;
        apply_moves(&mut state, g);
        let ops = intervals::inst_ops(func, inst);
        check_uses(&state, g, &ops)?;

        match inst.clobbers() {
            Clobbers::All => {
                for r in regs.gprs.iter().chain(&regs.fprs) {
                    state.remove(&Loc::Reg(*r));
                }
            }
            Clobbers::RustCall => {
                for r in regs.call_clobbered() {
                    state.remove(&Loc::Reg(r));
                }
            }
            Clobbers::Slow => {
                for r in &regs.slow_clobbers {
                    state.remove(&Loc::Reg(*r));
                }
            }
            Clobbers::None => {}
        }

        for op in ops.iter().filter(|o| o.role == Role::Def) {
            let p = pos::late(g);
            let loc = alloc.loc_at(op.vreg, p).ok_or_else(|| {
                format!("{} is defined without a location at {p} in {b}", op.vreg)
            })?;
            state.insert(loc, alloc.canon(op.vreg));
        }
    }

    if let Some(term) = &data.term {
        let g = base + data.insts.len() as u32;
        apply_moves(&mut state, g);

        // The arguments of an edge are consumed by the edge moves, which run
        // before the terminator: their delivery is checked at the target
        // block, through the parameter rename, not here.
        let edge_args: Vec<VReg> = term
            .edges()
            .iter()
            .flat_map(|e| e.args.iter().copied())
            .collect();
        let ops: Vec<intervals::OpInfo> = intervals::term_ops(func, term)
            .into_iter()
            .filter(|o| !edge_args.contains(&o.vreg))
            .collect();
        check_uses(&state, g, &ops)?;
    }
    Ok(state)
}

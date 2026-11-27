//! Dead code elimination.

use std::collections::HashSet;

use cranelift_entity::{EntityList, SecondaryMap};

use super::{PassOutcome, reachable};
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Inst, Terminator};

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = prune_virtuals(func);
    let live_block = reachable(func);
    let blocks: Vec<Block> = func.blocks.keys().collect();
    for &block in &blocks {
        if live_block[block] {
            continue;
        }
        let data = &mut func.blocks[block];
        let dirty = !data.insts.is_empty()
            || data.params.len(&func.pool) > 0
            || !matches!(data.term, Some(Terminator::Unreachable));
        if dirty {
            data.insts.clear();
            data.params = EntityList::new();
            data.term = Some(Terminator::Unreachable);
            changed = true;
        }
    }

    let mut marks = Marks::new();
    let mut work = 0;
    for &block in &blocks {
        if !live_block[block] {
            continue;
        }
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            if !removable(func, inst) {
                marks.mark_inst(inst);
            }
        }
        if let Some(term) = &func.blocks[block].term {
            for value in func.terminator_uses(term) {
                marks.mark_value(value);
            }
            if let Terminator::TailCall { frame, .. } | Terminator::Deopt { frame } = term {
                for value in func.frame_values(*frame) {
                    marks.mark_value(value);
                }
            }
        }
    }
    marks.drain(func);

    for &block in &blocks {
        if !live_block[block] {
            continue;
        }
        let before = func.blocks[block].insts.len();
        let live = &marks.live;
        func.blocks[block].insts.retain(|&inst| live[inst]);
        changed |= func.blocks[block].insts.len() != before;
    }
    PassOutcome {
        changed,
        work: work + func.blocks.len(),
    }
}

/// Drops the virtual objects that no frame state names any more, so that
/// their fields stop being uses. A frame state keeps the fields of the
/// objects it still names (F4): `Func::frame_values` lists them, and the
/// marking below treats them as uses of the state. An entry of a frame is
/// kept when any state whose parent chain contains that frame reaches it.
fn prune_virtuals(func: &mut Func) -> bool {
    if func.frames.values().all(|data| data.virtuals.is_empty()) {
        return false;
    }
    let mut needed: HashSet<(FrameState, Value)> = HashSet::new();
    for head in func.frames.keys().collect::<Vec<_>>() {
        let mut work = func.frame_placed_values(head);
        let mut seen: HashSet<Value> = HashSet::new();
        while let Some(value) = work.pop() {
            if !seen.insert(value) {
                continue;
            }
            let mut cur = Some(head);
            while let Some(fs) = cur {
                let data = &func.frames[fs];
                if let Some(object) = data.virtuals.iter().find(|o| o.alias == value) {
                    needed.insert((fs, value));
                    work.extend(object.fields.iter().copied());
                    break;
                }
                cur = data.parent;
            }
        }
    }
    let mut changed = false;
    for frame in func.frames.keys().collect::<Vec<_>>() {
        let data = &mut func.frames[frame];
        let before = data.virtuals.len();
        data.virtuals
            .retain(|object| needed.contains(&(frame, object.alias)));
        changed |= data.virtuals.len() != before;
    }
    changed
}

/// Whether `inst` may be dropped when its result is unused. Only a pure
/// operation or a fresh allocation qualifies. A guard, a call and an effect
/// always stay.
fn removable(func: &Func, inst: Inst) -> bool {
    let data = &func.insts[inst];
    data.result.is_some()
        && !data.op.can_deopt()
        && !data.op.is_call()
        && (data.op.effects().effect_free() || data.op.effects().allocation_only())
}

/// The live instructions, found by a worklist from the roots.
struct Marks {
    live: SecondaryMap<Inst, bool>,
    /// Values whose defining instruction may still need marking.
    pending: Vec<Value>,
    /// Live instructions whose operands and frame still need marking.
    queue: Vec<Inst>,
}

impl Marks {
    fn new() -> Self {
        Self {
            live: SecondaryMap::new(),
            pending: Vec::new(),
            queue: Vec::new(),
        }
    }

    fn mark_inst(&mut self, inst: Inst) {
        if !self.live[inst] {
            self.live[inst] = true;
            self.queue.push(inst);
        }
    }

    fn mark_value(&mut self, value: Value) {
        self.pending.push(value);
    }

    /// Propagates liveness from the roots to every operand and frame value.
    fn drain(&mut self, func: &Func) {
        loop {
            if let Some(inst) = self.queue.pop() {
                self.pending.extend_from_slice(func.args(inst));
                if let Some(frame) = func.insts[inst].frame {
                    self.pending.extend(func.frame_values(frame));
                }
            } else if let Some(value) = self.pending.pop() {
                if let ValueDef::Inst(def) = func.value_def(value) {
                    self.mark_inst(def);
                }
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::frame_state::{FrameStateData, VirtualObject};
    use crate::runtime::codegen::opt::ir::inst::{Constant, Op, RematKind};
    use crate::runtime::codegen::opt::ir::types::{CodeRef, Type};
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{const_any, placed, push};

    fn pair(alias: Value, fields: [Value; 2]) -> VirtualObject {
        VirtualObject {
            alias,
            kind: RematKind::Pair,
            fields: fields.to_vec(),
        }
    }

    /// Two placeholders and two constants in the entry, which ends in a deopt
    /// whose frame is built from them.
    struct Fixture {
        func: Func<'static>,
        /// The constants are only reachable as virtual fields.
        field_a: Value,
        field_b: Value,
        p: Value,
        q: Value,
        frame: FrameState,
    }

    fn fixture(registers: impl FnOnce(Value, Value) -> Vec<(u32, Value)>) -> Fixture {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x = func.append_param(entry, Type::Any);
        let field_a = const_any(&mut func, entry, 0x1a);
        let field_b = const_any(&mut func, entry, 0x2b);
        let p = push(&mut func, entry, Op::Remat(RematKind::Pair), &[], Type::Any);
        let q = push(&mut func, entry, Op::Remat(RematKind::Pair), &[], Type::Any);
        let frame = func.push_frame(FrameStateData {
            code: CodeRef::from_u32(0),
            pc: 0,
            closure: x,
            registers: registers(p, q),
            parent: None,
            call_base: None,
            returned: None,
            rest: None,
            inline_site: None,
            virtuals: vec![pair(p, [field_a, x]), pair(q, [field_b, x])],
        });
        func.set_terminator(entry, Terminator::Deopt { frame });
        Fixture {
            func,
            field_a,
            field_b,
            p,
            q,
            frame,
        }
    }

    fn defining_inst(func: &Func, value: Value) -> Inst {
        match func.value_def(value) {
            ValueDef::Inst(inst) => inst,
            ValueDef::Param { .. } => panic!("{value} is a parameter"),
        }
    }

    /// F4: the field values of a virtual object that a state still names are
    /// uses of the state, even though nothing else reads them.
    #[test]
    fn virtual_fields_keep_their_definitions_alive() {
        let mut f = fixture(|p, _| vec![(1, p)]);
        verify(&f.func).expect("valid before");
        let a = defining_inst(&f.func, f.field_a);
        let p = defining_inst(&f.func, f.p);
        super::run(&mut f.func);
        verify(&f.func).expect("valid after");
        assert!(
            placed(&f.func, a),
            "the field constant of a named object stays"
        );
        assert!(placed(&f.func, p), "the placeholder stays");
    }

    /// An entry that no register, closure or field names is dropped, and its
    /// fields stop being uses.
    #[test]
    fn an_unnamed_virtual_is_pruned_with_its_fields() {
        let mut f = fixture(|p, _| vec![(1, p)]);
        let b = defining_inst(&f.func, f.field_b);
        let a = defining_inst(&f.func, f.field_a);
        assert!(placed(&f.func, b));
        let outcome = super::run(&mut f.func);
        assert!(outcome.changed);
        let objects = &f.func.frames[f.frame].virtuals;
        assert_eq!(objects.len(), 1, "q is pruned");
        assert_eq!(objects[0].alias, f.p);
        assert!(!placed(&f.func, b), "the field of the pruned object dies");
        assert!(placed(&f.func, a));
        verify(&f.func).expect("valid after");
        // A second run changes nothing.
        assert!(!super::run(&mut f.func).changed);
    }

    /// An entry reached only through another entry's field is kept (and so
    /// are its own fields).
    #[test]
    fn a_virtual_named_by_another_virtuals_field_is_kept() {
        let mut f = fixture(|p, _| vec![(1, p)]);
        // p = (q . x) instead of (a . x)
        let x = f.func.frames[f.frame].closure;
        f.func.frames[f.frame].virtuals[0] = pair(f.p, [f.q, x]);
        verify(&f.func).expect("valid before");
        let b = defining_inst(&f.func, f.field_b);
        let a = defining_inst(&f.func, f.field_a);
        super::run(&mut f.func);
        assert_eq!(f.func.frames[f.frame].virtuals.len(), 2);
        assert!(placed(&f.func, b), "q's field is live through p");
        assert!(!placed(&f.func, a), "a is no longer named");
        verify(&f.func).expect("valid after");
    }

    /// With no virtual objects the pass leaves frame states alone.
    #[test]
    fn a_state_without_virtuals_is_untouched() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let x = func.append_param(entry, Type::Any);
        let unused = push(
            &mut func,
            entry,
            Op::Const(Constant::Bits(9)),
            &[],
            Type::Any,
        );
        let frame = func.push_frame(FrameStateData {
            code: CodeRef::from_u32(0),
            pc: 0,
            closure: x,
            registers: vec![(1, x)],
            parent: None,
            call_base: None,
            returned: None,
            rest: None,
            inline_site: None,
            virtuals: Vec::new(),
        });
        func.set_terminator(entry, Terminator::Deopt { frame });
        let unused = defining_inst(&func, unused);
        assert!(super::run(&mut func).changed);
        assert!(!placed(&func, unused));
        assert!(func.frames[frame].virtuals.is_empty());
    }
}

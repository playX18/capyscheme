//! Verifier for [`Func`](super::func::Func).

use cranelift_entity::SecondaryMap;

use super::frame_state::FrameState;
use super::func::{Block, DomTree, Func, Value, ValueDef};
use super::inst::{Inst, Op, Terminator};
use super::types::Type;

/// Checks `func`, returning every violation found.
pub fn verify(func: &Func<'_>) -> Result<(), Vec<String>> {
    let dom = func.dom_tree();
    let mut verifier = Verifier {
        func,
        dom: &dom,
        pos: SecondaryMap::new(),
        errors: Vec::new(),
    };
    verifier.run();
    if verifier.errors.is_empty() {
        Ok(())
    } else {
        Err(verifier.errors)
    }
}

struct Verifier<'a> {
    func: &'a Func<'a>,
    dom: &'a DomTree,
    /// The block and index of each placed instruction.
    pos: SecondaryMap<Inst, Option<(Block, usize)>>,
    errors: Vec<String>,
}

impl Verifier<'_> {
    fn check_barrier_order(&mut self, block: Block) {
        let func = self.func;
        let insts = &func.blocks[block].insts;
        for (index, &inst) in insts.iter().enumerate() {
            if func.insts[inst].op != Op::WriteBarrier {
                continue;
            }
            let barrier_args = func.args(inst);
            let (obj, value) = (barrier_args[0], barrier_args[1]);
            for &earlier in insts[..index].iter().rev() {
                let op = func.insts[earlier].op;
                if op == Op::WriteBarrier || op.is_call() {
                    break;
                }
                let args = func.args(earlier);
                let stored = match op {
                    Op::Store { .. } | Op::StoreFree { .. } if args.len() == 2 => {
                        Some((args[0], args[1]))
                    }
                    Op::StoreElement if args.len() == 3 => Some((args[0], args[2])),
                    _ => None,
                };
                if stored == Some((obj, value)) {
                    self.error(format!(
                        "write barrier {inst} follows its store {earlier} in {block}"
                    ));
                    break;
                }
            }
        }
    }

    fn check_osr(&mut self) {
        let func = self.func;
        if func.osr.is_empty() {
            // A multi-entry block keeps a selector for clause dispatch even
            // without OSR entries.
            if func.entry_selector.is_some() && func.case_clauses.is_empty() {
                self.error("the entry has a selector but no OSR entry".to_string());
            }
            return;
        }
        let Some(selector) = func.entry_selector else {
            self.error("OSR entries but no entry selector".to_string());
            return;
        };
        let params = func.params(func.entry);
        if params.last() != Some(&selector) || params.len() != func.entry_regs.len() + 1 {
            self.error("the entry selector is not its last parameter".to_string());
        }
        let Some(Terminator::Switch { value, cases, .. }) = &func.blocks[func.entry].term else {
            self.error("the entry does not dispatch on its selector".to_string());
            return;
        };
        if *value != selector {
            self.error("the entry dispatches on something other than its selector".to_string());
        }

        let expected = func.osr.len() + func.case_clauses.len();
        if cases.len() != expected {
            self.error("the entry does not dispatch to each OSR entry".to_string());
        }
    }

    fn run(&mut self) {
        let func = self.func;
        self.check_osr();
        for (block, data) in func.blocks.iter() {
            for (index, &inst) in data.insts.iter().enumerate() {
                if self.pos[inst].is_some() {
                    self.error(format!("{inst} is placed in two blocks"));
                }
                self.pos[inst] = Some((block, index));
            }
        }

        for (block, data) in func.blocks.iter() {
            let Some(term) = &data.term else {
                self.error(format!("{block} has no terminator"));
                continue;
            };
            let end = data.insts.len();

            for &inst in &data.insts {
                self.check_inst(block, inst);
            }
            self.check_barrier_order(block);

            self.check_frame_uses(block, end, term_frame(term));
            if let Terminator::Return(value) = term {
                self.check_not_bool(*value, block, "return value");
            }
            if let Terminator::ReturnMany { values, .. } = term {
                for value in values {
                    self.check_not_bool(*value, block, "return value");
                }
            }
            for value in func.terminator_uses(term) {
                self.use_at(value, block, end, "terminator operand");
                self.check_not_remat_operand(block, value, "terminator operand");
            }
            for edge in term.edges() {
                if !func.blocks.is_valid(edge.block) {
                    self.error(format!("{block} branches to an invalid block"));
                    continue;
                }
                let args = func.edge_args(&edge);
                let params = func.params(edge.block);
                if args.len() != params.len() {
                    self.error(format!(
                        "{block} passes {} values to {}, which takes {}",
                        args.len(),
                        edge.block,
                        params.len()
                    ));
                    continue;
                }
                for (&arg, &param) in args.iter().zip(params) {
                    if self.func.values.is_valid(arg)
                        && self.func.values.is_valid(param)
                        && !func.value_type(arg).is_subtype_of(func.value_type(param))
                    {
                        self.error(format!(
                            "{block} passes {arg} of type {:?} to {param} of type {:?}",
                            func.value_type(arg),
                            func.value_type(param)
                        ));
                    } else if self.func.values.is_valid(arg)
                        && self.func.values.is_valid(param)
                        && repr_class(func.value_type(arg)) != repr_class(func.value_type(param))
                    {
                        self.error(format!(
                            "{block} passes {arg} of type {:?} to {param} of type {:?}, which have different representations",
                            func.value_type(arg),
                            func.value_type(param)
                        ));
                    } else if self.func.values.is_valid(arg)
                        && self.func.values.is_valid(param)
                        && func.value_type(param) != Type::Bool
                    {
                        self.check_not_bool(arg, block, "edge argument");
                    }
                }
            }
        }
    }

    fn check_inst(&mut self, block: Block, inst: Inst) {
        let func = self.func;
        let data = &func.insts[inst];
        let index = self.pos[inst].map_or(0, |(_, index)| index);

        if let Some(arity) = data.op.fixed_arity() {
            let got = func.args(inst).len();
            if got != arity {
                self.error(format!(
                    "{inst} in {block} takes {arity} operands, got {got}"
                ));
            }
        }
        if data.op.has_result() != data.result.is_some() {
            self.error(format!("{inst} in {block} has a mismatched result"));
        }
        if let Some(result) = data.result {
            if func.values.is_valid(result) && func.value_def(result) != ValueDef::Inst(inst) {
                self.error(format!("{result} does not name {inst} as its definition"));
            }
        }
        if data.op == Op::BoolToValue {
            for &arg in func.args(inst) {
                if func.values.is_valid(arg) && func.value_type(arg) != Type::Bool {
                    self.error(format!(
                        "{inst} in {block} boxes {arg} of type {:?}, not a Bool",
                        func.value_type(arg)
                    ));
                }
            }
        }
        if (data.op.can_deopt() || data.op.is_call()) && data.frame.is_none() {
            self.error(format!("{inst} in {block} needs a frame state"));
        }
        if let Op::Remat(kind) = data.op {
            let args = func.args(inst);
            if args.len() != kind.field_count() {
                self.error(format!(
                    "{inst} in {block} rebuilds {} with {} fields",
                    kind,
                    args.len()
                ));
            }
            // The fields become tagged object slots, so a raw Bool flag
            // cannot stand there.
            for &arg in args {
                self.check_not_bool(arg, block, "remat field");
            }
        }

        for &arg in func.args(inst) {
            self.use_at(arg, block, index, "operand");
            if !matches!(data.op, Op::Remat(_)) {
                self.check_not_remat_operand(block, arg, "operand");
            }
        }
        self.check_frame_uses(block, index, data.frame);
    }

    /// Checks the values of a frame state, and of its parents, at `index`.
    fn check_frame_uses(&mut self, block: Block, index: usize, frame: Option<FrameState>) {
        let func = self.func;
        let mut cur = frame;
        while let Some(fs) = cur {
            if !func.frames.is_valid(fs) {
                self.error(format!("{block} refers to an invalid frame state {fs}"));
                return;
            }
            let data = &func.frames[fs];
            self.use_at(data.closure, block, index, "frame closure");
            for &(_, value) in &data.registers {
                self.use_at(value, block, index, "frame register");
                self.check_not_bool(value, block, "frame register");
            }
            cur = data.parent;
        }
    }

    /// Checks that `value`, used at `index` in `block`, is defined before it.
    fn use_at(&mut self, value: Value, block: Block, index: usize, what: &str) {
        if !self.func.values.is_valid(value) {
            self.error(format!("{block} has a {what} that is not a value: {value}"));
            return;
        }
        if !self.dom.is_reachable(block) {
            return;
        }
        let dominated = match self.func.value_def(value) {
            ValueDef::Param { block: def, .. } => self.dom.dominates(def, block),
            ValueDef::Inst(inst) => match self.pos[inst] {
                None => false,
                Some((def, def_index)) if def == block => def_index < index,
                Some((def, _)) => self.dom.dominates(def, block),
            },
        };
        if !dominated {
            self.error(format!(
                "{block} uses {value} as a {what}, which its definition does not dominate"
            ));
        }
    }

    /// A remat marker has no runtime value: it may only be named by a frame
    /// state or by another marker's field list.
    fn check_not_remat_operand(&mut self, block: Block, value: Value, what: &str) {
        let func = self.func;
        if let ValueDef::Inst(inst) = func.value_def(value) {
            if matches!(func.insts[inst].op, Op::Remat(_)) {
                self.error(format!(
                    "{block} reads the remat marker {value} as a {what}, which has no runtime value"
                ));
            }
        }
    }

    /// A Bool is a raw flag, so it cannot stand where a Scheme value is read.
    fn check_not_bool(&mut self, value: Value, block: Block, what: &str) {
        if self.func.values.is_valid(value) && self.func.value_type(value) == Type::Bool {
            self.error(format!(
                "{block} uses the Bool {value} as a {what}, which needs box.bool first"
            ));
        }
    }

    fn error(&mut self, message: String) {
        self.errors.push(message);
    }
}

fn term_frame(term: &Terminator) -> Option<FrameState> {
    match term {
        Terminator::TailCall { frame, .. }
        | Terminator::Deopt { frame } => Some(*frame),
        _ => None,
    }
}

/// The machine representation of a type: a raw int32, a raw double, a raw
/// flag, or a tagged Scheme value. Values of different classes cannot meet
/// at a block parameter.
fn repr_class(ty: Type) -> u8 {
    match ty {
        Type::Int32 => 1,
        Type::Double => 2,
        Type::Bool => 3,
        Type::Int64 => 4,
        Type::Int128 => 5,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::inst::MemoryRegion;
    use crate::runtime::codegen::opt::passes::{
        forward,
        testutil::{const_i32, ret},
    };

    /// `obj.slot <- value` with its barrier, in the order `barrier_first`
    /// says. `store` picks the store: a field, a closure free variable or a
    /// vector element.
    fn store_with_barrier(store: Op, barrier_first: bool) -> Func<'static> {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let obj = func.append_param(entry, Type::Any);
        let value = func.append_param(entry, Type::Any);
        let index = const_i32(&mut func, entry, 0);
        let args: Vec<Value> = match store {
            Op::StoreElement => vec![obj, index, value],
            _ => vec![obj, value],
        };
        let barrier = |func: &mut Func<'static>| {
            func.push_inst(entry, Op::WriteBarrier, &[obj, value], None, None);
        };
        if barrier_first {
            barrier(&mut func);
        }
        func.push_inst(entry, store, &args, None, None);
        if !barrier_first {
            barrier(&mut func);
        }
        ret(&mut func, entry, value);
        func
    }

    fn stores() -> [Op; 3] {
        [
            Op::Store {
                field: 1,
                region: MemoryRegion::Heap,
            },
            Op::StoreFree { index: 0 },
            Op::StoreElement,
        ]
    }

    #[test]
    fn a_barrier_before_its_store_is_valid() {
        for store in stores() {
            verify(&store_with_barrier(store, true))
                .unwrap_or_else(|errors| panic!("{store:?}: {errors:?}"));
        }
    }

    #[test]
    fn a_barrier_after_its_store_is_rejected() {
        for store in stores() {
            let errors = verify(&store_with_barrier(store, false))
                .expect_err("a post-store barrier loses the SATB old value");
            assert!(
                errors.iter().any(|e| e.contains("follows its store")),
                "{store:?}: {errors:?}"
            );
        }
    }

    /// Store forwarding and barrier elision keep a live object's barrier
    /// ahead of its store.
    #[test]
    fn forwarding_keeps_the_barrier_before_the_store() {
        for store in stores() {
            let mut func = store_with_barrier(store, true);
            forward::run(&mut func);
            verify(&func).unwrap_or_else(|errors| panic!("{store:?}: {errors:?}"));
            let insts = &func.blocks[func.entry].insts;
            let barrier = insts
                .iter()
                .position(|&i| func.insts[i].op == Op::WriteBarrier)
                .expect("a live object keeps its barrier");
            let stored = insts
                .iter()
                .position(|&i| func.insts[i].op == store)
                .expect("the store stays");
            assert!(barrier < stored, "{store:?}");
        }
    }
}

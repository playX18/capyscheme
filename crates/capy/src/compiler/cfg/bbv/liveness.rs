use super::super::{BlockId, Instruction, Operand, Procedure, Terminator, UVar};
use std::collections::{HashMap, HashSet};

/// Live values at each block entry (classic backward dataflow).
///
/// A value is live-in at block entry when it is used in the block or needed
/// by a successor before being defined in this block.
pub(super) fn compute_live_in(procedure: &Procedure<'_>) -> HashMap<BlockId, HashSet<UVar>> {
    let mut live_in: HashMap<BlockId, HashSet<UVar>> = procedure
        .blocks
        .iter()
        .map(|block| (block.id, HashSet::new()))
        .collect();

    let mut changed = true;
    while changed {
        changed = false;
        for block in &procedure.blocks {
            let mut live = HashSet::new();

            for succ in block.terminator.successors() {
                if let Some(succ_live) = live_in.get(&succ) {
                    live.extend(succ_live.iter().copied());
                }
            }

            collect_uses(&block.terminator, &mut live);
            for instruction in block.instructions.iter().rev() {
                for def in instruction.defs() {
                    live.remove(&def);
                }
                collect_instruction_uses(instruction, &mut live);
            }

            if live != live_in[&block.id] {
                live_in.insert(block.id, live);
                changed = true;
            }
        }
    }

    live_in
}

fn collect_uses(terminator: &Terminator<'_>, live: &mut HashSet<UVar>) {
    for atom in terminator.uses() {
        if let Operand::Local(value) = atom {
            live.insert(value);
        }
    }
}

fn collect_instruction_uses(instruction: &Instruction<'_>, live: &mut HashSet<UVar>) {
    for atom in instruction.uses() {
        if let Operand::Local(value) = atom {
            live.insert(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::cfg::{Block, CodeId, GraphCodeId, Procedure, ProcedureKind, Terminator};
    use crate::compiler::cranelift::primitive::Primitive;
    use crate::runtime::value::Value;
    use std::collections::HashMap;

    fn empty_procedure(blocks: Vec<Block<'static>>) -> Procedure<'static> {
        Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: UVar(0),
            name: Value::new(false),
            source: Value::new(false),
            meta: Value::new(false),
            return_cont: None,
            params: vec![],
            variadic: None,
            free_vars: vec![],
            sources: HashMap::new(),
            entry: BlockId(0),
            blocks,
        }
    }

    #[test]
    fn live_in_tracks_free_uses_across_edges() {
        let x = UVar(1);
        let y = UVar(2);
        let z = UVar(3);
        let procedure = empty_procedure(vec![
            Block {
                id: BlockId(0),
                instructions: vec![Instruction::PrimCall {
                    dst: z,
                    prim: Primitive::Plus,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                }],
                terminator: Terminator::Jump { target: BlockId(1) },
                source: Value::new(false),
            },
            Block {
                id: BlockId(1),
                instructions: vec![],
                terminator: Terminator::TailCall {
                    callee: Operand::Local(z),
                    args: vec![],
                    source: Value::new(false),
                },
                source: Value::new(false),
            },
        ]);

        let live_in = compute_live_in(&procedure);
        assert!(live_in[&BlockId(0)].contains(&x));
        assert!(live_in[&BlockId(0)].contains(&y));
        assert!(!live_in[&BlockId(1)].contains(&x));
        assert!(live_in[&BlockId(1)].contains(&z));
    }

    #[test]
    fn live_in_includes_values_used_after_assign_in_successor() {
        let x = UVar(1);
        let y = UVar(2);
        let procedure = empty_procedure(vec![
            Block {
                id: BlockId(0),
                instructions: vec![Instruction::Assign {
                    dst: y,
                    src: Operand::Local(x),
                }],
                terminator: Terminator::Jump { target: BlockId(1) },
                source: Value::new(false),
            },
            Block {
                id: BlockId(1),
                instructions: vec![],
                terminator: Terminator::TailCall {
                    callee: Operand::Local(y),
                    args: vec![],
                    source: Value::new(false),
                },
                source: Value::new(false),
            },
        ]);

        let live_in = compute_live_in(&procedure);
        assert!(live_in[&BlockId(0)].contains(&x));
        assert!(live_in[&BlockId(1)].contains(&y));
    }
}

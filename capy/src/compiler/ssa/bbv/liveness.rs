use super::super::{BlockId, BranchTarget, Instruction, Operand, Procedure, Terminator, ValueId};
use std::collections::{HashMap, HashSet};

/// Live values at each block entry (backward dataflow over SSA).
///
/// Includes block parameters and free dominated uses. Free live-ins are values
/// used in the block (or needed by successors) that are not defined in the block
/// and rely on dominance rather than explicit edge arguments.
pub(super) fn compute_live_in(procedure: &Procedure<'_>) -> HashMap<BlockId, HashSet<ValueId>> {
    let blocks: HashMap<_, _> = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect();

    let mut live_in: HashMap<BlockId, HashSet<ValueId>> = procedure
        .blocks
        .iter()
        .map(|block| (block.id, HashSet::new()))
        .collect();

    let mut changed = true;
    while changed {
        changed = false;
        for block in &procedure.blocks {
            let mut live = HashSet::new();

            // Free live-ins of successors must be available at the end of this
            // block (dominance). Param live-ins are supplied by edge arguments,
            // which are already counted as terminator uses below.
            for succ in block.terminator.successors() {
                let Some(succ_block) = blocks.get(&succ) else {
                    continue;
                };
                let succ_live = live_in.get(&succ).cloned().unwrap_or_default();
                let params: HashSet<_> = succ_block.params.iter().copied().collect();
                for value in succ_live {
                    if !params.contains(&value) {
                        live.insert(value);
                    }
                }
            }

            collect_uses(&block.terminator, &mut live);
            for instruction in block.instructions.iter().rev() {
                for def in instruction.defs() {
                    live.remove(&def);
                }
                collect_instruction_uses(instruction, &mut live);
            }

            // Params are defined at entry; keep them in live_in so callers can
            // see the full entry demand set, matching the previous convention.
            for param in &block.params {
                live.insert(*param);
            }
            if let Some(variadic) = block.variadic {
                live.insert(variadic);
            }

            if live != live_in[&block.id] {
                live_in.insert(block.id, live);
                changed = true;
            }
        }
    }

    live_in
}

/// Rewrites the procedure so every non-procedure-level live-in is an explicit
/// block parameter, with predecessors passing that value on every edge.
///
/// SSA normally relies on dominance for free values. SBBV specialization
/// clones blocks under different contexts and may reuse a version from a
/// predecessor that does not dominate another entry edge; free uses remapped
/// under the first reach then fail Cranelift's dominance check. Threading
/// live-ins as params makes every edge carry its own copy.
pub(super) fn thread_live_ins<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let mut proc_values = HashSet::new();
    proc_values.insert(procedure.binding);
    proc_values.extend(procedure.return_cont);
    proc_values.extend(procedure.params.iter().copied());
    proc_values.extend(procedure.variadic);
    proc_values.extend(procedure.free_vars.iter().copied());

    // Iterate to a fixpoint: adding params can change which values are free
    // live-ins of successors (they become params and stop propagating as free).
    for _ in 0..procedure.blocks.len().saturating_add(2) {
        let live_in = compute_live_in(&procedure);
        let mut extras: HashMap<BlockId, Vec<ValueId>> = HashMap::new();
        let mut any = false;

        for block in &procedure.blocks {
            let params: HashSet<_> = block.params.iter().copied().chain(block.variadic).collect();
            let mut extra: Vec<ValueId> = live_in
                .get(&block.id)
                .into_iter()
                .flatten()
                .copied()
                .filter(|value| !params.contains(value) && !proc_values.contains(value))
                .collect();
            extra.sort_by_key(|value| value.0);
            extra.dedup();
            if !extra.is_empty() {
                any = true;
            }
            extras.insert(block.id, extra);
        }

        if !any {
            break;
        }

        // Fixed arity of each target *before* we splice extras into params.
        // Jump args are [fixed..., rest...]; extras must land after the
        // current fixed prefix, not after the rest list.
        let fixed_arities: HashMap<BlockId, usize> = procedure
            .blocks
            .iter()
            .map(|block| {
                let fixed = if block.variadic.is_some() {
                    block.params.len().saturating_sub(1)
                } else {
                    block.params.len()
                };
                (block.id, fixed)
            })
            .collect();

        for block in &mut procedure.blocks {
            let extra = extras.get(&block.id).cloned().unwrap_or_default();
            if !extra.is_empty() {
                if block.variadic.is_some() {
                    // Last param is the rest formal; keep extras among fixed.
                    let insert_at = block.params.len().saturating_sub(1);
                    block.params.splice(insert_at..insert_at, extra);
                } else {
                    block.params.extend(extra);
                }
            }
            block.terminator = append_edge_args(block.terminator.clone(), &extras, &fixed_arities);
        }
    }

    procedure
}

fn splice_edge_args(
    args: &mut Vec<Operand<'_>>,
    target: BlockId,
    extras: &HashMap<BlockId, Vec<ValueId>>,
    fixed_arities: &HashMap<BlockId, usize>,
) {
    let Some(extra) = extras.get(&target) else {
        return;
    };
    if extra.is_empty() {
        return;
    }
    let at = fixed_arities.get(&target).copied().unwrap_or(args.len());
    let at = at.min(args.len());
    args.splice(at..at, extra.iter().map(|value| Operand::Local(*value)));
}

fn append_edge_args<'gc>(
    terminator: Terminator<'gc>,
    extras: &HashMap<BlockId, Vec<ValueId>>,
    fixed_arities: &HashMap<BlockId, usize>,
) -> Terminator<'gc> {
    match terminator {
        Terminator::Jump { target, mut args } => {
            splice_edge_args(&mut args, target, extras, fixed_arities);
            Terminator::Jump { target, args }
        }
        Terminator::Branch {
            test,
            consequent,
            alternative,
            hints,
        } => Terminator::Branch {
            test,
            consequent: append_branch_args(consequent, extras, fixed_arities),
            alternative: append_branch_args(alternative, extras, fixed_arities),
            hints,
        },
        Terminator::Switch {
            kind,
            scrutinee,
            cases,
            default,
        } => Terminator::Switch {
            kind,
            scrutinee,
            cases: cases
                .into_iter()
                .map(|case| super::super::SwitchCase {
                    value: case.value,
                    target: append_branch_args(case.target, extras, fixed_arities),
                })
                .collect(),
            default: append_branch_args(default, extras, fixed_arities),
        },
        other => other,
    }
}

fn append_branch_args<'gc>(
    target: BranchTarget<'gc>,
    extras: &HashMap<BlockId, Vec<ValueId>>,
    fixed_arities: &HashMap<BlockId, usize>,
) -> BranchTarget<'gc> {
    match target {
        BranchTarget::Local { block, mut args } => {
            splice_edge_args(&mut args, block, extras, fixed_arities);
            BranchTarget::Local { block, args }
        }
        other => other,
    }
}

fn collect_uses(terminator: &Terminator<'_>, live: &mut HashSet<ValueId>) {
    for atom in terminator.uses() {
        if let Operand::Local(value) = atom {
            live.insert(value);
        }
    }
}

fn collect_instruction_uses(instruction: &Instruction<'_>, live: &mut HashSet<ValueId>) {
    for atom in instruction.uses() {
        if let Operand::Local(value) = atom {
            live.insert(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::cps::graph::BranchHint;
    use crate::compiler::cranelift::primitive::Primitive;
    use crate::compiler::ssa::{
        Block, CodeId, GraphCodeId, Procedure, ProcedureKind, Terminator, ValueId,
    };
    use crate::runtime::value::Value;
    use std::collections::HashMap;

    fn empty_procedure(blocks: Vec<Block<'static>>) -> Procedure<'static> {
        Procedure {
            code: CodeId::GraphFunction(GraphCodeId(0)),
            kind: ProcedureKind::Function,
            binding: ValueId(0),
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
    fn live_in_tracks_block_params() {
        let x = ValueId(1);
        let y = ValueId(2);
        let z = ValueId(3);
        let procedure = empty_procedure(vec![
            Block {
                id: BlockId(0),
                params: vec![],
                variadic: None,
                instructions: vec![Instruction::PrimCall {
                    dst: z,
                    prim: Primitive::Plus,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                }],
                terminator: Terminator::Jump {
                    target: BlockId(1),
                    args: vec![Operand::Local(z)],
                },
                source: Value::new(false),
            },
            Block {
                id: BlockId(1),
                params: vec![z],
                variadic: None,
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
    fn thread_live_ins_promotes_dominated_uses() {
        let x = ValueId(1);
        let y = ValueId(2);
        let z_then = ValueId(3);
        let z_else = ValueId(4);
        let z_join = ValueId(5);
        // Diamond: x/y are defined in entry and used free in both arms and the join.
        let procedure = empty_procedure(vec![
            Block {
                id: BlockId(0),
                params: vec![x],
                variadic: None,
                instructions: vec![Instruction::Const {
                    dst: y,
                    value: Value::from_i32(1),
                }],
                terminator: Terminator::Branch {
                    test: Operand::Local(x),
                    consequent: BranchTarget::Local {
                        block: BlockId(1),
                        args: vec![],
                    },
                    alternative: BranchTarget::Local {
                        block: BlockId(2),
                        args: vec![],
                    },
                    hints: [BranchHint::Normal, BranchHint::Normal],
                },
                source: Value::new(false),
            },
            Block {
                id: BlockId(1),
                params: vec![],
                variadic: None,
                instructions: vec![Instruction::PrimCall {
                    dst: z_then,
                    prim: Primitive::FxAdd,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                }],
                terminator: Terminator::Jump {
                    target: BlockId(3),
                    args: vec![Operand::Local(z_then)],
                },
                source: Value::new(false),
            },
            Block {
                id: BlockId(2),
                params: vec![],
                variadic: None,
                instructions: vec![Instruction::PrimCall {
                    dst: z_else,
                    prim: Primitive::FxSub,
                    args: vec![Operand::Local(x), Operand::Local(y)],
                    source: Value::new(false),
                }],
                terminator: Terminator::Jump {
                    target: BlockId(3),
                    args: vec![Operand::Local(z_else)],
                },
                source: Value::new(false),
            },
            Block {
                id: BlockId(3),
                params: vec![z_join],
                variadic: None,
                instructions: vec![],
                terminator: Terminator::TailCall {
                    callee: Operand::Local(z_join),
                    args: vec![Operand::Local(x)],
                    source: Value::new(false),
                },
                source: Value::new(false),
            },
        ]);

        let threaded = thread_live_ins(procedure);
        let by_id: HashMap<_, _> = threaded.blocks.iter().map(|b| (b.id, b)).collect();

        // Join must take x explicitly; arms must pass it.
        assert!(by_id[&BlockId(3)].params.contains(&x));
        for arm in [BlockId(1), BlockId(2)] {
            let Terminator::Jump { args, .. } = &by_id[&arm].terminator else {
                panic!("expected jump");
            };
            assert!(
                args.iter()
                    .any(|a| matches!(a, Operand::Local(v) if *v == x)),
                "arm {arm:?} must pass x"
            );
        }

        // After threading, no free live-ins remain (except proc-level, none here).
        let live_in = compute_live_in(&threaded);
        for block in &threaded.blocks {
            let params: HashSet<_> = block.params.iter().copied().chain(block.variadic).collect();
            for value in &live_in[&block.id] {
                assert!(
                    params.contains(value),
                    "block {:?} still has free live-in {:?}",
                    block.id,
                    value
                );
            }
        }
    }

    #[test]
    fn thread_live_ins_keeps_extras_before_variadic() {
        let closure = ValueId(1);
        let rest = ValueId(2);
        let callee = ValueId(3);
        // Cont block: (closure, ...rest) with free use of callee.
        // Jump passes only the fixed arg; rest is empty.
        let procedure = empty_procedure(vec![
            Block {
                id: BlockId(0),
                params: vec![callee],
                variadic: None,
                instructions: vec![Instruction::Const {
                    dst: closure,
                    value: Value::from_i32(0),
                }],
                terminator: Terminator::Jump {
                    target: BlockId(1),
                    args: vec![Operand::Local(closure)],
                },
                source: Value::new(false),
            },
            Block {
                id: BlockId(1),
                // Last param is the rest formal (params_with_variadic layout).
                params: vec![closure, rest],
                variadic: Some(rest),
                instructions: vec![],
                terminator: Terminator::TailCall {
                    callee: Operand::Local(callee),
                    args: vec![Operand::Local(closure)],
                    source: Value::new(false),
                },
                source: Value::new(false),
            },
        ]);

        let threaded = thread_live_ins(procedure);
        let by_id: HashMap<_, _> = threaded.blocks.iter().map(|b| (b.id, b)).collect();
        let cont = by_id[&BlockId(1)];

        assert_eq!(cont.variadic, Some(rest));
        assert_eq!(
            cont.params.last().copied(),
            Some(rest),
            "rest formal must remain last in params"
        );
        assert!(
            cont.params[..cont.params.len() - 1].contains(&callee),
            "threaded live-in must sit among fixed params, got {:?}",
            cont.params
        );

        let Terminator::Jump { args, .. } = &by_id[&BlockId(0)].terminator else {
            panic!("expected jump");
        };
        // Args must be [fixed..., extras..., rest...] with fixed_count = 2.
        assert_eq!(
            args,
            &vec![Operand::Local(closure), Operand::Local(callee)],
            "edge args must splice extras after fixed prefix, before rest"
        );
    }
}

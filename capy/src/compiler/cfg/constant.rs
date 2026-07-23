use super::*;
pub(super) fn hoist_constants<'gc>(mut procedure: Procedure<'gc>) -> Procedure<'gc> {
    let mut hoister = ConstantHoister::new(&procedure);
    for block in &mut procedure.blocks {
        let mut instructions = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.drain(..) {
            let instruction = hoister.instruction(instruction, &mut instructions);
            instructions.push(instruction);
        }
        block.terminator = hoister.terminator(block.terminator.clone(), &mut instructions);
        block.instructions = instructions;
    }
    procedure
}

struct ConstantHoister {
    next_value: u32,
}

impl ConstantHoister {
    fn new<'gc>(procedure: &Procedure<'gc>) -> Self {
        let mut max_value = procedure.binding.0;
        if let Some(return_cont) = procedure.return_cont {
            max_value = max_value.max(return_cont.0);
        }
        for value in procedure
            .params
            .iter()
            .chain(procedure.variadic.iter())
            .chain(procedure.free_vars.iter())
            .copied()
        {
            max_value = max_value.max(value.0);
        }
        for block in &procedure.blocks {
            for instruction in &block.instructions {
                for def in instruction.defs() {
                    max_value = max_value.max(def.0);
                }
                for value in local_values(instruction.uses()) {
                    max_value = max_value.max(value.0);
                }
            }
            for value in local_values(block.terminator.uses()) {
                max_value = max_value.max(value.0);
            }
        }
        Self {
            next_value: max_value + 1,
        }
    }

    fn fresh_value(&mut self) -> UVar {
        let value = UVar(self.next_value);
        self.next_value += 1;
        value
    }

    fn atom<'gc>(
        &mut self,
        atom: Operand<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Operand<'gc> {
        match atom {
            Operand::Local(_) => atom,
            Operand::Constant(value) => {
                let dst = self.fresh_value();
                instructions.push(Instruction::Const { dst, value });
                Operand::Local(dst)
            }
        }
    }

    fn atoms<'gc>(
        &mut self,
        atoms: Vec<Operand<'gc>>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Vec<Operand<'gc>> {
        atoms
            .into_iter()
            .map(|atom| self.atom(atom, instructions))
            .collect()
    }

    fn instruction<'gc>(
        &mut self,
        instruction: Instruction<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Instruction<'gc> {
        match instruction {
            Instruction::Assign { dst, src } => Instruction::Assign {
                dst,
                src: self.atom(src, instructions),
            },
            Instruction::Const { .. }
            | Instruction::MakeClosure { .. }
            | Instruction::CacheRef { .. }
            | Instruction::CacheSet { .. }
            | Instruction::RestToList { .. }
            | Instruction::RestRef { .. }
            | Instruction::RestLength { .. }
            | Instruction::RestPredicate { .. } => instruction,
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => Instruction::ClosureRef {
                dst,
                closure: self.atom(closure, instructions),
                index,
            },
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => Instruction::ClosureSet {
                closure: self.atom(closure, instructions),
                index,
                value: self.atom(value, instructions),
            },
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => Instruction::PrimCall {
                dst,
                prim,
                args: self.atoms(args, instructions),
                source,
            },
        }
    }

    fn branch_target<'gc>(
        &mut self,
        target: BranchTarget<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> BranchTarget<'gc> {
        match target {
            BranchTarget::Local { block, edge_assigns } => BranchTarget::Local { block, edge_assigns },
            BranchTarget::Reified { continuation, args } => BranchTarget::Reified {
                continuation: self.atom(continuation, instructions),
                args: self.atoms(args, instructions),
            },
        }
    }

    fn terminator<'gc>(
        &mut self,
        terminator: Terminator<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => Terminator::Call {
                callee: self.atom(callee, instructions),
                retk: self.atom(retk, instructions),
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::TailCall {
                callee,
                args,
                source,
            } => Terminator::TailCall {
                callee: self.atom(callee, instructions),
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::Raise { kind, args, source } => Terminator::Raise {
                kind,
                args: self.atoms(args, instructions),
                source,
            },
            Terminator::Jump { target } => Terminator::Jump { target },
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints,
            } => Terminator::Branch {
                test: self.atom(test, instructions),
                consequent: self.branch_target(consequent, instructions),
                alternative: self.branch_target(alternative, instructions),
                hints,
            },
            Terminator::BranchPrim {
                prim,
                args,
                consequent,
                alternative,
                hints,
            } => Terminator::BranchPrim {
                prim,
                args: self.atoms(args, instructions),
                consequent: self.branch_target(consequent, instructions),
                alternative: self.branch_target(alternative, instructions),
                hints,
            },
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => Terminator::Switch {
                kind,
                scrutinee: self.atom(scrutinee, instructions),
                cases: cases
                    .into_iter()
                    .map(|case| SwitchCase {
                        value: case.value,
                        target: self.branch_target(case.target, instructions),
                    })
                    .collect(),
                default: self.branch_target(default, instructions),
            },
        }
    }
}

pub(super) fn local_values<'gc>(uses: Vec<Operand<'gc>>) -> impl Iterator<Item = UVar> {
    uses.into_iter().filter_map(|atom| match atom {
        Operand::Local(value) => Some(value),
        Operand::Constant(_) => None,
    })
}

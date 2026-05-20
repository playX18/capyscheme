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
            for value in block.params.iter().chain(block.variadic.iter()).copied() {
                max_value = max_value.max(value.0);
            }
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

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn atom<'gc>(
        &mut self,
        atom: LinearAtom<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> LinearAtom<'gc> {
        match atom {
            LinearAtom::Local(_) => atom,
            LinearAtom::Constant(value) => {
                let dst = self.fresh_value();
                instructions.push(Instruction::Const { dst, value });
                LinearAtom::Local(dst)
            }
        }
    }

    fn atoms<'gc>(
        &mut self,
        atoms: Vec<LinearAtom<'gc>>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Vec<LinearAtom<'gc>> {
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
            BranchTarget::Local { block, args } => BranchTarget::Local {
                block,
                args: self.atoms(args, instructions),
            },
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
            Terminator::Jump { target, args } => Terminator::Jump {
                target,
                args: self.atoms(args, instructions),
            },
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

pub(super) fn local_values<'gc>(uses: Vec<LinearAtom<'gc>>) -> impl Iterator<Item = ValueId> {
    uses.into_iter().filter_map(|atom| match atom {
        LinearAtom::Local(value) => Some(value),
        LinearAtom::Constant(_) => None,
    })
}

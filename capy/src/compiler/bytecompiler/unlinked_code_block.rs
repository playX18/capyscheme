use capy_bytecode::{BytecodeOperand, Instruction, JumpTarget, OperandWidth};
use im::HashMap;

pub enum InstructionOrControl {
    Instruction(Instruction),
    ControlInsn(Instruction, Label, OperandWidth),
}

pub struct UnlinkedCodeBlock {
    pub instructions: Vec<InstructionOrControl>,

    pub labels: HashMap<usize, Label>,
    pub next_label: Label,
}

impl UnlinkedCodeBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: HashMap::new(),
            next_label: Label(0),
        }
    }

    pub fn new_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label.0 += 1;
        label
    }

    /// Relax program by calculating offsets of all labels.
    ///
    /// This function runs in a fixpoint loop until no more changes are made.
    ///
    /// The effect of this pass is that all branches get narrowed down from 32-bit
    /// offsets to 16-bit or 8-bit offsets.
    pub fn relax_program(&mut self) {
        const RELAXATION_LIMIT: usize = 3;
        let mut changed = true;
        let mut iterations = 0;

        while changed {
            changed = false;
            iterations += 1;

            if iterations > RELAXATION_LIMIT {
                // too many passes: exit the pass. It's not necessary to narrow down
                // offsets any further.
                break;
            }

            let mut symtab = HashMap::new();
            let mut current_offset = 0;

            // first pass: calculate label offests
            for (idx, ins) in self.instructions.iter().enumerate() {
                if let Some(label) = self.labels.get(&idx) {
                    symtab.insert(*label, current_offset);
                }
                match ins {
                    InstructionOrControl::Instruction(ins) => {
                        current_offset += ins.encode_size();
                    }

                    InstructionOrControl::ControlInsn(ins, _, _) => {
                        current_offset += ins.encode_size();
                    }
                }
            }

            // second pass: check constraints and narrow offsets
            current_offset = 0;
            for (_, ins) in self.instructions.iter_mut().enumerate() {
                match ins {
                    InstructionOrControl::Instruction(ins) => {
                        current_offset += ins.encode_size();
                    }

                    InstructionOrControl::ControlInsn(ins, label, width) => {
                        current_offset += ins.encode_size();
                        let pc = current_offset;
                        if let Some(&target_offset) = symtab.get(label) {
                            let dist = (target_offset as isize - pc as isize) as i32;
                            match width {
                                OperandWidth::Narrow => {
                                    if dist.required_width() != OperandWidth::Narrow {
                                        *width = dist.required_width();
                                        changed = true;
                                    }
                                }

                                OperandWidth::Wide16 => {
                                    if dist.required_width() == OperandWidth::Wide32 {
                                        *width = OperandWidth::Wide32;
                                        changed = true;
                                    }
                                }

                                OperandWidth::Wide32 => {
                                    // already as wide as possible
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn peephole(&mut self) {}

    pub fn assemble<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let mut symbol_table = HashMap::new();
        let mut current_offset = 0;

        for (idx, ins) in self.instructions.iter().enumerate() {
            if let Some(label) = self.labels.get(&idx) {
                symbol_table.insert(*label, current_offset);
            }
            match ins {
                InstructionOrControl::Instruction(ins) => {
                    current_offset += ins.encode_size();
                }

                InstructionOrControl::ControlInsn(ins, _, _) => {
                    current_offset += ins.encode_size();
                }
            }
        }

        current_offset = 0;
        for ins in self.instructions.iter() {
            match ins {
                InstructionOrControl::Instruction(ins) => {
                    ins.encode(writer)?;
                    current_offset += ins.encode_size();
                }

                InstructionOrControl::ControlInsn(ins, label, _width) => {
                    let target_offset = symbol_table.get(label).unwrap();
                    let dist = (target_offset - current_offset) as i32;

                    let mut ins = ins.clone();
                    patch_jump_target(&mut ins, JumpTarget(dist));
                    ins.encode(writer)?;
                    current_offset += ins.encode_size();
                }
            }
        }

        Ok(())
    }
}

impl Default for UnlinkedCodeBlock {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Label(usize);

fn patch_jump_target(ins: &mut Instruction, target: JumpTarget) {
    match ins {
        Instruction::OpBr(opts) => opts.target = target,
        Instruction::OpBrfalse(opts) => opts.target = target,
        Instruction::OpBrtrue(opts) => opts.target = target,
        Instruction::OpBrnull(opts) => opts.target = target,
        Instruction::OpBrundef(opts) => opts.target = target,
        Instruction::OpBrtc8(opts) => opts.target = target,
        Instruction::OpBrtc16(opts) => opts.target = target,
        Instruction::OpBrimmediate(opts) => opts.target = target,
        Instruction::OpBrheapobj(opts) => opts.target = target,
        Instruction::OpBrzero(opts) => opts.target = target,
        Instruction::OpBrnumeq(opts) => opts.target = target,
        Instruction::OpBrnumne(opts) => opts.target = target,
        Instruction::OpBrnumlt(opts) => opts.target = target,
        Instruction::OpBrnumle(opts) => opts.target = target,
        Instruction::OpBrnumgt(opts) => opts.target = target,
        Instruction::OpBrnumge(opts) => opts.target = target,
        Instruction::OpBeq(opts) => opts.target = target,
        Instruction::OpBreqv(opts) => opts.target = target,
        Instruction::OpBrequal(opts) => opts.target = target,
        Instruction::OpBrneq(opts) => opts.target = target,
        Instruction::OpBrneqv(opts) => opts.target = target,
        Instruction::OpBrnequal(opts) => opts.target = target,
        _ => panic!("Instruction {:?} is not a jump", ins),
    }
}

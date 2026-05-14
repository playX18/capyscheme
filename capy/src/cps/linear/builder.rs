use crate::{
    compiler::ssa::primitive::Primitive,
    cps::{
        ReifyInfo,
        term::{Atom, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::value::Value,
};
use std::collections::HashMap;

use super::*;

pub fn linearize<'gc>(reify: &ReifyInfo<'gc>) -> LinearProgram<'gc> {
    let mut procedures = Vec::new();

    for func in reify.functions.iter() {
        procedures.push(hoist_constants(lower_low_level_primitives(
            lower_cache_operations(lower_rest_arguments(infer_switches(linearize_function(
                *func,
            )))),
        )));
    }

    for cont in reify.continuations.iter().filter(|cont| cont.reified.get()) {
        procedures.push(hoist_constants(lower_low_level_primitives(
            lower_cache_operations(lower_rest_arguments(infer_switches(
                linearize_continuation(*cont),
            ))),
        )));
    }

    LinearProgram {
        entry: reify.entrypoint,
        procedures,
    }
}
fn linearize_function<'gc>(func: FuncRef<'gc>) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        func.free_vars
            .get()
            .expect("function free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let binding = builder.value(func.binding);
    let return_cont = builder.value(func.return_cont);
    let params = builder.values(func.args);
    let variadic = func.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        func.body(),
    );
    let (blocks, sources) = builder.finish();

    Procedure {
        code: CodeId::Function(func),
        kind: ProcedureKind::Function,
        binding,
        name: func.name,
        source: func.source,
        meta: func.meta,
        return_cont: Some(return_cont),
        params,
        variadic,
        free_vars,
        entry,
        sources,
        blocks,
    }
}

fn linearize_continuation<'gc>(cont: ContRef<'gc>) -> Procedure<'gc> {
    let source_free_vars = vars_to_vec(
        cont.free_vars
            .get()
            .expect("continuation free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let binding = builder.value(cont.binding);
    let params = builder.values(cont.args);
    let variadic = cont.variadic.map(|var| builder.value(var));
    let free_vars = builder.value_slice(&source_free_vars);
    let entry = BlockId(0);
    let instructions = closure_refs(&mut builder, binding, &source_free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(params.clone(), variadic),
        variadic,
        instructions,
        cont.body(),
    );
    let (blocks, sources) = builder.finish();

    Procedure {
        code: CodeId::Continuation(cont),
        kind: ProcedureKind::Continuation,
        binding,
        name: cont.name,
        source: cont.source,
        meta: cont.meta,
        return_cont: None,
        params,
        variadic,
        free_vars,
        entry,
        sources,
        blocks,
    }
}

fn vars_to_vec<'gc>(vars: crate::cps::term::Vars<'gc>) -> Vec<LVarRef<'gc>> {
    vars.iter().copied().collect()
}

fn primitive_from_value<'gc>(value: Value<'gc>) -> Primitive {
    let name = value
        .downcast::<crate::runtime::value::Symbol>()
        .to_string();
    Primitive::from_name(&name).unwrap_or_else(|| panic!("undefined primitive: {value}"))
}

fn params_with_variadic(mut args: Vec<ValueId>, variadic: Option<ValueId>) -> Vec<ValueId> {
    args.extend(variadic);
    args
}

fn closure_refs<'gc>(
    builder: &mut ProcedureBuilder<'gc>,
    binding: ValueId,
    free_vars: &[LVarRef<'gc>],
) -> Vec<Instruction<'gc>> {
    free_vars
        .iter()
        .enumerate()
        .map(|(index, free_var)| Instruction::ClosureRef {
            dst: builder.value(*free_var),
            closure: LinearAtom::Local(binding),
            index,
        })
        .collect()
}

struct ProcedureBuilder<'gc> {
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<LVarRef<'gc>, BlockId>,
    values: HashMap<LVarRef<'gc>, ValueId>,
    sources: HashMap<ValueId, LVarRef<'gc>>,
    next_value: u32,
    next_block: usize,
}

impl<'gc> ProcedureBuilder<'gc> {
    fn new() -> Self {
        Self {
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            values: HashMap::new(),
            sources: HashMap::new(),
            next_value: 0,
            next_block: 1,
        }
    }

    fn finish(mut self) -> (Vec<Block<'gc>>, HashMap<ValueId, LVarRef<'gc>>) {
        self.blocks.sort_by_key(|block| block.id.0);
        (self.blocks, self.sources)
    }

    fn value(&mut self, var: LVarRef<'gc>) -> ValueId {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = ValueId(self.next_value);
        self.next_value += 1;
        self.values.insert(var, id);
        self.sources.insert(id, var);
        id
    }

    fn values(&mut self, vars: crate::cps::term::Vars<'gc>) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn value_slice(&mut self, vars: &[LVarRef<'gc>]) -> Vec<ValueId> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn atom(&mut self, atom: Atom<'gc>) -> LinearAtom<'gc> {
        match atom {
            Atom::Constant(value) => LinearAtom::Constant(value),
            Atom::Local(var) => LinearAtom::Local(self.value(var)),
        }
    }

    fn atoms(&mut self, args: crate::cps::term::Atoms<'gc>) -> Vec<LinearAtom<'gc>> {
        args.iter().copied().map(|atom| self.atom(atom)).collect()
    }

    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    fn convert_block(
        &mut self,
        id: BlockId,
        params: Vec<ValueId>,
        variadic: Option<ValueId>,
        mut instructions: Vec<Instruction<'gc>>,
        term: TermRef<'gc>,
    ) {
        let source = term.source();
        let terminator = self.convert_term(term, &mut instructions);
        self.blocks.push(Block {
            id,
            params,
            variadic,
            instructions,
            terminator,
            source,
        });
    }

    fn convert_term(
        &mut self,
        term: TermRef<'gc>,
        instructions: &mut Vec<Instruction<'gc>>,
    ) -> Terminator<'gc> {
        match *term {
            Term::Let(var, Expression::PrimCall(prim, args, source), next) => {
                let prim = primitive_from_value(prim);
                let args = self.atoms(args);
                let dst = self.value(var);
                instructions.push(Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                });
                self.convert_term(next, instructions)
            }

            Term::Fix(funcs, next) => {
                for func in funcs.iter() {
                    let free_vars = vars_to_vec(
                        func.free_vars
                            .get()
                            .expect("function free vars are reified"),
                    );
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(func.binding),
                        code: CodeId::Function(*func),
                        kind: ClosureKind::Function,
                        free_count: free_vars.len(),
                    });
                }

                for func in funcs.iter() {
                    let closure = self.value(func.binding);
                    let free_vars = vars_to_vec(
                        func.free_vars
                            .get()
                            .expect("function free vars are reified"),
                    );
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                self.convert_term(next, instructions)
            }

            Term::Letk(conts, next) => {
                let local_conts: Vec<_> = conts
                    .iter()
                    .filter(|cont| !cont.reified.get())
                    .copied()
                    .collect();

                for cont in conts.iter().filter(|cont| cont.reified.get()) {
                    let free_vars = vars_to_vec(
                        cont.free_vars
                            .get()
                            .expect("continuation free vars are reified"),
                    );
                    instructions.push(Instruction::MakeClosure {
                        dst: self.value(cont.binding),
                        code: CodeId::Continuation(*cont),
                        kind: ClosureKind::Continuation,
                        free_count: free_vars.len(),
                    });
                }

                for cont in conts.iter().filter(|cont| cont.reified.get()) {
                    let closure = self.value(cont.binding);
                    let free_vars = vars_to_vec(
                        cont.free_vars
                            .get()
                            .expect("continuation free vars are reified"),
                    );
                    emit_closure_sets(self, instructions, closure, &free_vars);
                }

                for cont in &local_conts {
                    let id = self.alloc_block();
                    self.local_blocks.insert(cont.binding, id);
                }

                for cont in local_conts {
                    let id = self.local_blocks[&cont.binding];
                    let params = self.values(cont.args);
                    let variadic = cont.variadic.map(|var| self.value(var));
                    self.convert_block(
                        id,
                        params_with_variadic(params, variadic),
                        variadic,
                        vec![],
                        cont.body(),
                    );
                }

                self.convert_term(next, instructions)
            }

            Term::Continue(k, args, source) => {
                if let Some(target) = self.local_blocks.get(&k) {
                    Terminator::Jump {
                        target: *target,
                        args: self.atoms(args),
                    }
                } else {
                    Terminator::TailCall {
                        callee: LinearAtom::Local(self.value(k)),
                        args: self.atoms(args),
                        source,
                    }
                }
            }

            Term::App(callee, retk, args, source) => Terminator::Call {
                callee: self.atom(callee),
                retk: LinearAtom::Local(self.value(retk)),
                args: self.atoms(args),
                source,
            },

            Term::Raise { kind, args, source } => Terminator::Raise {
                kind,
                args: self.atoms(args),
                source,
            },

            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints,
            } => Terminator::Branch {
                test: self.atom(test),
                consequent: self.branch_target(consequent, consequent_args),
                alternative: self.branch_target(alternative, alternative_args),
                hints,
            },
        }
    }

    fn branch_target(
        &mut self,
        continuation: LVarRef<'gc>,
        args: Option<crate::cps::term::Atoms<'gc>>,
    ) -> BranchTarget<'gc> {
        let args = args.map(|args| self.atoms(args)).unwrap_or_default();
        if let Some(block) = self.local_blocks.get(&continuation) {
            BranchTarget::Local {
                block: *block,
                args,
            }
        } else {
            BranchTarget::Reified {
                continuation: LinearAtom::Local(self.value(continuation)),
                args,
            }
        }
    }
}

fn emit_closure_sets<'gc>(
    builder: &mut ProcedureBuilder<'gc>,
    instructions: &mut Vec<Instruction<'gc>>,
    closure: ValueId,
    free_vars: &[LVarRef<'gc>],
) {
    for (index, free_var) in free_vars.iter().enumerate() {
        instructions.push(Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index,
            value: LinearAtom::Local(builder.value(*free_var)),
        });
    }
}

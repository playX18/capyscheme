use crate::{
    cps::{
        ReifyInfo,
        term::{Atom, BranchHint, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::LVarRef,
    runtime::value::Value,
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinearVar<'gc> {
    Source(LVarRef<'gc>),
    Synthetic(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinearAtom<'gc> {
    Constant(Value<'gc>),
    Local(LinearVar<'gc>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcedureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeId<'gc> {
    Function(FuncRef<'gc>),
    Continuation(ContRef<'gc>),
}

#[derive(Debug, Clone)]
pub struct LinearProgram<'gc> {
    pub entry: FuncRef<'gc>,
    pub procedures: Vec<Procedure<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Procedure<'gc> {
    pub code: CodeId<'gc>,
    pub kind: ProcedureKind,
    pub binding: LVarRef<'gc>,
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub meta: Value<'gc>,
    pub return_cont: Option<LVarRef<'gc>>,
    pub params: Vec<LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub free_vars: Vec<LVarRef<'gc>>,
    pub entry: BlockId,
    pub blocks: Vec<Block<'gc>>,
}

#[derive(Debug, Clone)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub params: Vec<LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub instructions: Vec<Instruction<'gc>>,
    pub terminator: Terminator<'gc>,
    pub source: Value<'gc>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction<'gc> {
    MakeClosure {
        dst: LinearVar<'gc>,
        code: CodeId<'gc>,
        kind: ClosureKind,
        free_count: usize,
    },
    ClosureRef {
        dst: LinearVar<'gc>,
        closure: LinearAtom<'gc>,
        index: usize,
    },
    ClosureSet {
        closure: LinearAtom<'gc>,
        index: usize,
        value: LinearAtom<'gc>,
    },
    PrimCall {
        dst: LinearVar<'gc>,
        prim: Value<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchTarget<'gc> {
    Local {
        block: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Reified {
        continuation: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator<'gc> {
    Call {
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    TailCall {
        callee: LinearAtom<'gc>,
        args: Vec<LinearAtom<'gc>>,
        source: Value<'gc>,
    },
    Jump {
        target: BlockId,
        args: Vec<LinearAtom<'gc>>,
    },
    Branch {
        test: LinearAtom<'gc>,
        consequent: BranchTarget<'gc>,
        alternative: BranchTarget<'gc>,
        hints: [BranchHint; 2],
    },
}

impl<'gc> From<Atom<'gc>> for LinearAtom<'gc> {
    fn from(value: Atom<'gc>) -> Self {
        match value {
            Atom::Constant(value) => Self::Constant(value),
            Atom::Local(var) => Self::Local(LinearVar::Source(var)),
        }
    }
}

pub fn linearize<'gc>(reify: &ReifyInfo<'gc>) -> LinearProgram<'gc> {
    let mut procedures = Vec::new();

    for func in reify.functions.iter() {
        procedures.push(linearize_function(*func));
    }

    for cont in reify.continuations.iter().filter(|cont| cont.reified.get()) {
        procedures.push(linearize_continuation(*cont));
    }

    LinearProgram {
        entry: reify.entrypoint,
        procedures,
    }
}

fn linearize_function<'gc>(func: FuncRef<'gc>) -> Procedure<'gc> {
    let free_vars = vars_to_vec(
        func.free_vars
            .get()
            .expect("function free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let entry = BlockId(0);
    let instructions = closure_refs(func.binding, &free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(func.args, func.variadic),
        func.variadic,
        instructions,
        func.body(),
    );

    Procedure {
        code: CodeId::Function(func),
        kind: ProcedureKind::Function,
        binding: func.binding,
        name: func.name,
        source: func.source,
        meta: func.meta,
        return_cont: Some(func.return_cont),
        params: func.args.iter().copied().collect(),
        variadic: func.variadic,
        free_vars,
        entry,
        blocks: builder.finish(),
    }
}

fn linearize_continuation<'gc>(cont: ContRef<'gc>) -> Procedure<'gc> {
    let free_vars = vars_to_vec(
        cont.free_vars
            .get()
            .expect("continuation free vars are reified"),
    );
    let mut builder = ProcedureBuilder::new();
    let entry = BlockId(0);
    let instructions = closure_refs(cont.binding, &free_vars);
    builder.convert_block(
        entry,
        params_with_variadic(cont.args, cont.variadic),
        cont.variadic,
        instructions,
        cont.body(),
    );

    Procedure {
        code: CodeId::Continuation(cont),
        kind: ProcedureKind::Continuation,
        binding: cont.binding,
        name: cont.name,
        source: cont.source,
        meta: cont.meta,
        return_cont: None,
        params: cont.args.iter().copied().collect(),
        variadic: cont.variadic,
        free_vars,
        entry,
        blocks: builder.finish(),
    }
}

fn vars_to_vec<'gc>(vars: crate::cps::term::Vars<'gc>) -> Vec<LVarRef<'gc>> {
    vars.iter().copied().collect()
}

fn params_with_variadic<'gc>(
    args: crate::cps::term::Vars<'gc>,
    variadic: Option<LVarRef<'gc>>,
) -> Vec<LVarRef<'gc>> {
    args.iter().copied().chain(variadic).collect()
}

fn closure_refs<'gc>(binding: LVarRef<'gc>, free_vars: &[LVarRef<'gc>]) -> Vec<Instruction<'gc>> {
    free_vars
        .iter()
        .enumerate()
        .map(|(index, free_var)| Instruction::ClosureRef {
            dst: LinearVar::Source(*free_var),
            closure: LinearAtom::Local(LinearVar::Source(binding)),
            index,
        })
        .collect()
}

struct ProcedureBuilder<'gc> {
    blocks: Vec<Block<'gc>>,
    local_blocks: HashMap<LVarRef<'gc>, BlockId>,
    next_block: usize,
}

impl<'gc> ProcedureBuilder<'gc> {
    fn new() -> Self {
        Self {
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            next_block: 1,
        }
    }

    fn finish(mut self) -> Vec<Block<'gc>> {
        self.blocks.sort_by_key(|block| block.id.0);
        self.blocks
    }

    fn alloc_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    fn convert_block(
        &mut self,
        id: BlockId,
        params: Vec<LVarRef<'gc>>,
        variadic: Option<LVarRef<'gc>>,
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
                instructions.push(Instruction::PrimCall {
                    dst: LinearVar::Source(var),
                    prim,
                    args: linear_atoms(args),
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
                        dst: LinearVar::Source(func.binding),
                        code: CodeId::Function(*func),
                        kind: ClosureKind::Function,
                        free_count: free_vars.len(),
                    });
                }

                for func in funcs.iter() {
                    emit_closure_sets(
                        instructions,
                        LinearVar::Source(func.binding),
                        &vars_to_vec(
                            func.free_vars
                                .get()
                                .expect("function free vars are reified"),
                        ),
                    );
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
                        dst: LinearVar::Source(cont.binding),
                        code: CodeId::Continuation(*cont),
                        kind: ClosureKind::Continuation,
                        free_count: free_vars.len(),
                    });
                }

                for cont in conts.iter().filter(|cont| cont.reified.get()) {
                    emit_closure_sets(
                        instructions,
                        LinearVar::Source(cont.binding),
                        &vars_to_vec(
                            cont.free_vars
                                .get()
                                .expect("continuation free vars are reified"),
                        ),
                    );
                }

                for cont in &local_conts {
                    let id = self.alloc_block();
                    self.local_blocks.insert(cont.binding, id);
                }

                for cont in local_conts {
                    let id = self.local_blocks[&cont.binding];
                    self.convert_block(
                        id,
                        params_with_variadic(cont.args, cont.variadic),
                        cont.variadic,
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
                        args: linear_atoms(args),
                    }
                } else {
                    Terminator::TailCall {
                        callee: LinearAtom::Local(LinearVar::Source(k)),
                        args: linear_atoms(args),
                        source,
                    }
                }
            }

            Term::App(callee, retk, args, source) => Terminator::Call {
                callee: callee.into(),
                retk: LinearAtom::Local(LinearVar::Source(retk)),
                args: linear_atoms(args),
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
                test: test.into(),
                consequent: self.branch_target(consequent, consequent_args),
                alternative: self.branch_target(alternative, alternative_args),
                hints,
            },
        }
    }

    fn branch_target(
        &self,
        continuation: LVarRef<'gc>,
        args: Option<crate::cps::term::Atoms<'gc>>,
    ) -> BranchTarget<'gc> {
        let args = args.map(linear_atoms).unwrap_or_default();
        if let Some(block) = self.local_blocks.get(&continuation) {
            BranchTarget::Local {
                block: *block,
                args,
            }
        } else {
            BranchTarget::Reified {
                continuation: LinearAtom::Local(LinearVar::Source(continuation)),
                args,
            }
        }
    }
}

fn linear_atoms<'gc>(args: crate::cps::term::Atoms<'gc>) -> Vec<LinearAtom<'gc>> {
    args.iter().copied().map(LinearAtom::from).collect()
}

fn emit_closure_sets<'gc>(
    instructions: &mut Vec<Instruction<'gc>>,
    closure: LinearVar<'gc>,
    free_vars: &[LVarRef<'gc>],
) {
    for (index, free_var) in free_vars.iter().enumerate() {
        instructions.push(Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index,
            value: LinearAtom::Local(LinearVar::Source(*free_var)),
        });
    }
}

impl<'gc> Instruction<'gc> {
    pub fn def(&self) -> Option<LinearVar<'gc>> {
        match self {
            Self::MakeClosure { dst, .. }
            | Self::ClosureRef { dst, .. }
            | Self::PrimCall { dst, .. } => Some(*dst),
            Self::ClosureSet { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::MakeClosure { .. } => vec![],
            Self::ClosureRef { closure, .. } => vec![*closure],
            Self::ClosureSet { closure, value, .. } => vec![*closure, *value],
            Self::PrimCall { args, .. } => args.clone(),
        }
    }
}

impl<'gc> BranchTarget<'gc> {
    pub fn local_successor(&self) -> Option<BlockId> {
        match self {
            Self::Local { block, .. } => Some(*block),
            Self::Reified { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Local { args, .. } => args.clone(),
            Self::Reified { continuation, args } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*continuation);
                uses.extend(args.iter().copied());
                uses
            }
        }
    }
}

impl<'gc> Terminator<'gc> {
    pub fn uses(&self) -> Vec<LinearAtom<'gc>> {
        match self {
            Self::Call {
                callee, retk, args, ..
            } => {
                let mut uses = Vec::with_capacity(args.len() + 2);
                uses.push(*callee);
                uses.push(*retk);
                uses.extend(args.iter().copied());
                uses
            }
            Self::TailCall { callee, args, .. } => {
                let mut uses = Vec::with_capacity(args.len() + 1);
                uses.push(*callee);
                uses.extend(args.iter().copied());
                uses
            }
            Self::Jump { args, .. } => args.clone(),
            Self::Branch {
                test,
                consequent,
                alternative,
                ..
            } => {
                let mut uses =
                    Vec::with_capacity(1 + consequent.uses().len() + alternative.uses().len());
                uses.push(*test);
                uses.extend(consequent.uses());
                uses.extend(alternative.uses());
                uses
            }
        }
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Self::Call { .. } | Self::TailCall { .. } => vec![],
            Self::Jump { target, .. } => vec![*target],
            Self::Branch {
                consequent,
                alternative,
                ..
            } => [consequent.local_successor(), alternative.local_successor()]
                .into_iter()
                .flatten()
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::{
            ReifyInfo,
            free_vars::FreeVars,
            term::{Atom, Cont, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn lvar<'gc>(ctx: Context<'gc>, name: &str) -> LVarRef<'gc> {
        fresh_lvar(ctx, Symbol::from_str(ctx, name).into())
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[Atom<'gc>]) -> crate::cps::term::Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    fn vars<'gc>(ctx: Context<'gc>, vars: &[LVarRef<'gc>]) -> crate::cps::term::Vars<'gc> {
        Array::from_slice(*ctx, vars)
    }

    fn empty_term<'gc>(ctx: Context<'gc>, k: LVarRef<'gc>) -> Gc<'gc, Term<'gc>> {
        Gc::new(*ctx, Term::Continue(k, atoms(ctx, &[]), Value::new(false)))
    }

    fn mk_func<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        retk: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: Gc<'gc, Term<'gc>>,
        free_vars: &[LVarRef<'gc>],
    ) -> Gc<'gc, Func<'gc>> {
        mk_func_with_variadic(ctx, name, binding, retk, args, None, body, Some(free_vars))
    }

    fn mk_func_with_variadic<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        retk: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
        free_vars: Option<&[LVarRef<'gc>]>,
    ) -> Gc<'gc, Func<'gc>> {
        Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, name).into(),
                source: Value::new(false),
                binding,
                return_cont: retk,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                free_vars: Lock::new(free_vars.map(|free_vars| vars(ctx, free_vars))),
                meta: Value::new(false),
            },
        )
    }

    fn mk_cont<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        body: Gc<'gc, Term<'gc>>,
        free_vars: &[LVarRef<'gc>],
        reified: bool,
    ) -> Gc<'gc, Cont<'gc>> {
        mk_cont_with_variadic(
            ctx,
            name,
            binding,
            args,
            None,
            body,
            Some(free_vars),
            reified,
        )
    }

    fn mk_cont_with_variadic<'gc>(
        ctx: Context<'gc>,
        name: &str,
        binding: LVarRef<'gc>,
        args: &[LVarRef<'gc>],
        variadic: Option<LVarRef<'gc>>,
        body: Gc<'gc, Term<'gc>>,
        free_vars: Option<&[LVarRef<'gc>]>,
        reified: bool,
    ) -> Gc<'gc, Cont<'gc>> {
        Gc::new(
            *ctx,
            Cont {
                name: Symbol::from_str(ctx, name).into(),
                binding,
                args: vars(ctx, args),
                variadic,
                body: Lock::new(body),
                source: Value::new(false),
                free_vars: Lock::new(free_vars.map(|free_vars| vars(ctx, free_vars))),
                reified: std::cell::Cell::new(reified),
                cold: false,
                noinline: false,
                meta: Value::new(false),
            },
        )
    }

    fn reify_info<'gc>(
        ctx: Context<'gc>,
        entrypoint: Gc<'gc, Func<'gc>>,
        functions: &[Gc<'gc, Func<'gc>>],
        continuations: &[Gc<'gc, Cont<'gc>>],
    ) -> ReifyInfo<'gc> {
        let mut free_vars = FreeVars::new();
        for func in functions {
            free_vars.funcs.insert(func.binding, *func);
        }
        for cont in continuations {
            free_vars.conts.insert(cont.binding, *cont);
        }

        ReifyInfo {
            entrypoint,
            functions: Array::from_slice(*ctx, functions),
            continuations: Array::from_slice(*ctx, continuations),
            free_vars,
        }
    }

    fn procedure<'gc>(program: &LinearProgram<'gc>, code: CodeId<'gc>) -> Procedure<'gc> {
        program
            .procedures
            .iter()
            .find(|procedure| procedure.code == code)
            .cloned()
            .expect("procedure exists")
    }

    #[test]
    fn instruction_defs_and_uses_are_explicit() {
        let dst = LinearVar::Synthetic(1);
        let closure = LinearVar::Synthetic(2);
        let value = LinearAtom::Local(LinearVar::Synthetic(3));
        let instr = Instruction::ClosureSet {
            closure: LinearAtom::Local(closure),
            index: 0,
            value,
        };

        assert_eq!(instr.def(), None);
        assert_eq!(
            instr.uses(),
            vec![
                LinearAtom::Local(closure),
                LinearAtom::Local(LinearVar::Synthetic(3))
            ]
        );

        let instr = Instruction::ClosureRef {
            dst,
            closure: LinearAtom::Local(closure),
            index: 1,
        };
        assert_eq!(instr.def(), Some(dst));
        assert_eq!(instr.uses(), vec![LinearAtom::Local(closure)]);
    }

    #[test]
    fn terminator_successors_are_explicit() {
        let then_block = BlockId(1);
        let else_block = BlockId(2);
        let term = Terminator::Branch {
            test: LinearAtom::Local(LinearVar::Synthetic(9)),
            consequent: BranchTarget::Local {
                block: then_block,
                args: vec![],
            },
            alternative: BranchTarget::Local {
                block: else_block,
                args: vec![],
            },
            hints: [crate::cps::term::BranchHint::Normal; 2],
        };

        assert_eq!(term.successors(), vec![then_block, else_block]);
    }

    #[test]
    fn linearize_creates_procedures_for_functions() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let entry = mk_func(ctx, "entry", f, retk, &[], empty_term(ctx, retk), &[]);
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);

            assert_eq!(program.entry, entry);
            assert!(
                program
                    .procedures
                    .iter()
                    .any(|procedure| procedure.code == CodeId::Function(entry)
                        && procedure.kind == ProcedureKind::Function)
            );
        });
    }

    #[test]
    fn function_entry_block_records_variadic_parameter() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let arg = lvar(ctx, "arg");
            let rest = lvar(ctx, "rest");
            let entry = mk_func_with_variadic(
                ctx,
                "entry",
                f,
                retk,
                &[arg],
                Some(rest),
                empty_term(ctx, retk),
                Some(&[]),
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert_eq!(entry_block.params, vec![arg, rest]);
            assert_eq!(entry_block.variadic, Some(rest));
        });
    }

    #[test]
    fn non_reified_continuation_becomes_local_block() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let value = lvar(ctx, "value");
            let cont_arg = lvar(ctx, "cont-arg");
            let cont = mk_cont(ctx, "k", k, &[cont_arg], empty_term(ctx, retk), &[], false);
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(
                        *ctx,
                        Term::Continue(k, atoms(ctx, &[Atom::Local(value)]), Value::new(false)),
                    ),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));

            assert!(
                !program
                    .procedures
                    .iter()
                    .any(|procedure| procedure.code == CodeId::Continuation(cont))
            );

            let local_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id != entry_proc.entry)
                .expect("local continuation block");
            assert_eq!(local_block.params, vec![cont_arg]);

            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");
            assert_eq!(
                entry_block.terminator,
                Terminator::Jump {
                    target: local_block.id,
                    args: vec![LinearAtom::Local(LinearVar::Source(value))]
                }
            );
        });
    }

    #[test]
    fn local_continuation_block_records_variadic_metadata() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let cont_arg = lvar(ctx, "cont-arg");
            let rest = lvar(ctx, "rest");
            let cont = mk_cont_with_variadic(
                ctx,
                "k",
                k,
                &[cont_arg],
                Some(rest),
                empty_term(ctx, retk),
                Some(&[]),
                false,
            );
            let body = Gc::new(
                *ctx,
                Term::Letk(
                    Array::from_slice(*ctx, &[cont]),
                    Gc::new(*ctx, Term::Continue(k, atoms(ctx, &[]), Value::new(false))),
                ),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let local_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id != entry_proc.entry)
                .expect("local continuation block");

            assert_eq!(local_block.params, vec![cont_arg, rest]);
            assert_eq!(local_block.variadic, Some(rest));
        });
    }

    #[test]
    fn reified_continuation_gets_make_closure() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let captured = lvar(ctx, "captured");
            let cont = mk_cont(ctx, "k", k, &[], empty_term(ctx, retk), &[captured], true);
            let body = Gc::new(
                *ctx,
                Term::Letk(Array::from_slice(*ctx, &[cont]), empty_term(ctx, retk)),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(
                entry_block
                    .instructions
                    .contains(&Instruction::MakeClosure {
                        dst: LinearVar::Source(k),
                        code: CodeId::Continuation(cont),
                        kind: ClosureKind::Continuation,
                        free_count: 1,
                    })
            );
            assert!(entry_block.instructions.contains(&Instruction::ClosureSet {
                closure: LinearAtom::Local(LinearVar::Source(k)),
                index: 0,
                value: LinearAtom::Local(LinearVar::Source(captured)),
            }));
        });
    }

    #[test]
    fn function_fix_gets_closure_sets_for_free_vars() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let child_binding = lvar(ctx, "child");
            let child_retk = lvar(ctx, "child-retk");
            let captured = lvar(ctx, "captured");
            let child = mk_func(
                ctx,
                "child",
                child_binding,
                child_retk,
                &[],
                empty_term(ctx, child_retk),
                &[captured],
            );
            let body = Gc::new(
                *ctx,
                Term::Fix(Array::from_slice(*ctx, &[child]), empty_term(ctx, retk)),
            );
            let entry = mk_func(ctx, "entry", f, retk, &[], body, &[]);
            let reify = reify_info(ctx, entry, &[entry, child], &[]);

            let program = linearize(&reify);
            let entry_proc = procedure(&program, CodeId::Function(entry));
            let entry_block = entry_proc
                .blocks
                .iter()
                .find(|block| block.id == entry_proc.entry)
                .expect("entry block");

            assert!(
                entry_block
                    .instructions
                    .contains(&Instruction::MakeClosure {
                        dst: LinearVar::Source(child_binding),
                        code: CodeId::Function(child),
                        kind: ClosureKind::Function,
                        free_count: 1,
                    })
            );
            assert!(entry_block.instructions.contains(&Instruction::ClosureSet {
                closure: LinearAtom::Local(LinearVar::Source(child_binding)),
                index: 0,
                value: LinearAtom::Local(LinearVar::Source(captured)),
            }));
        });
    }

    #[test]
    fn linearize_panics_when_function_free_vars_are_missing() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let entry = mk_func_with_variadic(
                ctx,
                "entry",
                f,
                retk,
                &[],
                None,
                empty_term(ctx, retk),
                None,
            );
            let reify = reify_info(ctx, entry, &[entry], &[]);

            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                linearize(&reify);
            }));

            assert!(result.is_err());
        });
    }

    #[test]
    fn linearize_panics_when_reified_continuation_free_vars_are_missing() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "entry");
            let retk = lvar(ctx, "retk");
            let k = lvar(ctx, "k");
            let entry = mk_func(ctx, "entry", f, retk, &[], empty_term(ctx, retk), &[]);
            let cont =
                mk_cont_with_variadic(ctx, "k", k, &[], None, empty_term(ctx, retk), None, true);
            let reify = reify_info(ctx, entry, &[entry], &[cont]);

            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                linearize(&reify);
            }));

            assert!(result.is_err());
        });
    }
}

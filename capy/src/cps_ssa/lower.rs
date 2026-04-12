use std::collections::{HashMap, VecDeque};

use crate::{
    cps::{
        ReifyInfo,
        packed::Primitive,
        reify,
        term::{Atom, BranchHint, ContRef, Expression, FuncRef, Term, TermRef},
    },
    cps_ssa::{
        Block, BlockId, BlockParam, Capture, ClassifiedConst, ConstId, ConstPool, ConstRef, Inst,
        InstKind, Module, Proc, ProcId, ProcKind, TailTarget, Terminator, ValueId,
        classify_const_value,
    },
    expander::core::LVarRef,
    rsgc::{
        Gc,
        alloc::{Array, array::ArrayRef},
    },
    runtime::{
        Context,
        value::{Str, Symbol, Value, ValueEqual, Vector},
    },
};

#[derive(Debug)]
pub enum LowerError {
    UnknownPrimitive(String),
    MissingValue(String),
    MissingConst(String),
    NonReifiedContinuationValue(String),
    MissingFunctionProc(String),
    MissingContinuationProc(String),
    Validation(super::ValidationError),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownPrimitive(name) => write!(f, "unknown primitive {name}"),
            Self::MissingValue(name) => write!(f, "missing value binding for {name}"),
            Self::MissingConst(name) => write!(f, "missing const binding for {name}"),
            Self::NonReifiedContinuationValue(name) => {
                write!(f, "non-reified continuation {name} used as a value")
            }
            Self::MissingFunctionProc(name) => {
                write!(f, "missing lowered proc for function {name}")
            }
            Self::MissingContinuationProc(name) => {
                write!(f, "missing lowered proc for continuation {name}")
            }
            Self::Validation(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for LowerError {}

impl From<super::ValidationError> for LowerError {
    fn from(value: super::ValidationError) -> Self {
        Self::Validation(value)
    }
}

pub fn lower_cps<'gc>(
    ctx: Context<'gc>,
    entry: FuncRef<'gc>,
) -> Result<Gc<'gc, Module<'gc>>, LowerError> {
    lower_reified(ctx, reify(ctx, entry))
}

pub fn lower_reified<'gc>(
    ctx: Context<'gc>,
    reify: ReifyInfo<'gc>,
) -> Result<Gc<'gc, Module<'gc>>, LowerError> {
    let lexical = LexicalOrder::new(reify.entrypoint);
    let mut lowerer = ModuleLowerer::new(ctx, reify, lexical);
    lowerer.lower()
}

struct LexicalOrder<'gc> {
    vars: HashMap<LVarRef<'gc>, u32>,
    next: u32,
}

impl<'gc> LexicalOrder<'gc> {
    fn new(entry: FuncRef<'gc>) -> Self {
        let mut this = Self {
            vars: HashMap::new(),
            next: 0,
        };
        this.visit_func(entry);
        this
    }

    fn order_of(&self, var: LVarRef<'gc>) -> u32 {
        *self
            .vars
            .get(&var)
            .unwrap_or_else(|| panic!("missing lexical order for {}", var.name))
    }

    fn bind(&mut self, var: LVarRef<'gc>) {
        if self.vars.contains_key(&var) {
            return;
        }
        let next = self.next;
        self.next += 1;
        self.vars.insert(var, next);
    }

    fn visit_func(&mut self, func: FuncRef<'gc>) {
        self.bind(func.binding);
        self.bind(func.return_cont);
        for &arg in func.args.iter() {
            self.bind(arg);
        }
        if let Some(rest) = func.variadic {
            self.bind(rest);
        }
        self.visit_term(func.body());
    }

    fn visit_term(&mut self, term: TermRef<'gc>) {
        match &*term {
            Term::Continue(..) | Term::App(..) | Term::If { .. } => {}
            Term::Let(binding, _, body) => {
                self.bind(*binding);
                self.visit_term(*body);
            }
            Term::Fix(funcs, body) => {
                for &func in funcs.iter() {
                    self.bind(func.binding);
                }
                for &func in funcs.iter() {
                    self.bind(func.return_cont);
                    for &arg in func.args.iter() {
                        self.bind(arg);
                    }
                    if let Some(rest) = func.variadic {
                        self.bind(rest);
                    }
                    self.visit_term(func.body());
                }
                self.visit_term(*body);
            }
            Term::Letk(conts, body) => {
                for &cont in conts.iter() {
                    self.bind(cont.binding);
                }
                for &cont in conts.iter() {
                    for &arg in cont.args.iter() {
                        self.bind(arg);
                    }
                    if let Some(rest) = cont.variadic {
                        self.bind(rest);
                    }
                    self.visit_term(cont.body());
                }
                self.visit_term(*body);
            }
        }
    }
}

struct ModuleLowerer<'gc> {
    ctx: Context<'gc>,
    reify: ReifyInfo<'gc>,
    lexical: LexicalOrder<'gc>,
    const_ids: HashMap<ValueEqual<'gc>, ConstId>,
    const_entries: Vec<Value<'gc>>,
    proc_for_func: HashMap<FuncRef<'gc>, ProcId>,
    proc_for_cont: HashMap<ContRef<'gc>, ProcId>,
}

impl<'gc> ModuleLowerer<'gc> {
    fn new(ctx: Context<'gc>, reify: ReifyInfo<'gc>, lexical: LexicalOrder<'gc>) -> Self {
        Self {
            ctx,
            reify,
            lexical,
            const_ids: HashMap::new(),
            const_entries: Vec::new(),
            proc_for_func: HashMap::new(),
            proc_for_cont: HashMap::new(),
        }
    }

    fn lower(&mut self) -> Result<Gc<'gc, Module<'gc>>, LowerError> {
        let mut funcs = self.reify.functions.iter().copied().collect::<Vec<_>>();
        funcs.sort_by_key(|func| self.lexical.order_of(func.binding));

        let mut conts = self
            .reify
            .continuations
            .iter()
            .copied()
            .filter(|cont| cont.reified.get())
            .collect::<Vec<_>>();
        conts.sort_by_key(|cont| self.lexical.order_of(cont.binding));

        let mut next_proc = 0u32;
        for &func in &funcs {
            let id = ProcId(next_proc);
            next_proc += 1;
            self.proc_for_func.insert(func, id);
        }
        for &cont in &conts {
            let id = ProcId(next_proc);
            next_proc += 1;
            self.proc_for_cont.insert(cont, id);
        }

        let mut procs = Vec::with_capacity(funcs.len() + conts.len());
        for &func in &funcs {
            procs.push(self.lower_func(func)?);
        }
        for &cont in &conts {
            procs.push(self.lower_cont(cont)?);
        }

        let module = Gc::new(
            *self.ctx,
            Module {
                format_version: super::FORMAT_VERSION,
                entry: self.proc_for_func[&self.reify.entrypoint],
                procs: Array::from_slice(*self.ctx, &procs),
                const_pool: self.build_const_pool(),
            },
        );
        module.validate()?;
        Ok(module)
    }

    fn build_const_pool(&self) -> ConstPool<'gc> {
        let entries = Vector::new::<true>(*self.ctx, self.const_entries.len(), Value::undefined());
        let wentries = Gc::write(*self.ctx, entries);
        for (index, value) in self.const_entries.iter().copied().enumerate() {
            wentries[index].unlock().set(value);
        }
        ConstPool { entries }
    }

    fn lower_func(&mut self, func: FuncRef<'gc>) -> Result<Proc<'gc>, LowerError> {
        let proc_id = self.proc_for_func[&func];
        let captures = self.ordered_func_captures(func);
        let display_name = self.const_opt(func.name)?;
        let source = self.const_opt(func.source)?;
        let meta = self.const_opt(func.meta)?;
        let mut capture_meta = Vec::with_capacity(captures.len());
        for (index, var) in captures.iter().enumerate() {
            capture_meta.push(Capture {
                index: index as u32,
                binding_name: self.const_ref(var.name)?,
            });
        }
        let binding_name = self.const_ref(func.binding.name)?;
        let mut builder = ProcLowerer::new(
            self,
            proc_id,
            ProcKind::Function,
            binding_name,
            display_name,
            source,
            meta,
            false,
            false,
            capture_meta,
        );

        let mut env = builder.build_entry(
            Some(func.binding),
            Some(func.return_cont),
            func.args.iter().copied().collect::<Vec<_>>(),
            func.variadic,
        )?;
        builder.load_captures(&mut env, &captures, func.source)?;
        builder.lower_term(func.body(), env)?;
        builder.finish()
    }

    fn lower_cont(&mut self, cont: ContRef<'gc>) -> Result<Proc<'gc>, LowerError> {
        let proc_id = self.proc_for_cont[&cont];
        let captures = self.ordered_cont_captures(cont);
        let display_name = self.const_opt(cont.name)?;
        let source = self.const_opt(cont.source)?;
        let meta = self.const_opt(cont.meta)?;
        let mut capture_meta = Vec::with_capacity(captures.len());
        for (index, var) in captures.iter().enumerate() {
            capture_meta.push(Capture {
                index: index as u32,
                binding_name: self.const_ref(var.name)?,
            });
        }
        let binding_name = self.const_ref(cont.binding.name)?;
        let mut builder = ProcLowerer::new(
            self,
            proc_id,
            ProcKind::Continuation,
            binding_name,
            display_name,
            source,
            meta,
            cont.cold,
            cont.noinline,
            capture_meta,
        );

        let mut env = builder.build_entry(
            Some(cont.binding),
            None,
            cont.args.iter().copied().collect::<Vec<_>>(),
            cont.variadic,
        )?;
        builder.load_captures(&mut env, &captures, cont.source)?;
        builder.lower_term(cont.body(), env)?;
        builder.finish()
    }

    fn ordered_func_captures(&self, func: FuncRef<'gc>) -> Vec<LVarRef<'gc>> {
        self.ordered_captures(self.reify.free_vars.fvars.get(&func))
    }

    fn ordered_cont_captures(&self, cont: ContRef<'gc>) -> Vec<LVarRef<'gc>> {
        self.ordered_captures(self.reify.free_vars.cvars.get(&cont))
    }

    fn ordered_captures(&self, captures: Option<&im::HashSet<LVarRef<'gc>>>) -> Vec<LVarRef<'gc>> {
        let mut captures = captures
            .into_iter()
            .flat_map(|captures| captures.iter().copied())
            .collect::<Vec<_>>();
        captures.sort_by_key(|var| self.lexical.order_of(*var));
        captures
    }

    fn const_opt(&mut self, value: Value<'gc>) -> Result<Option<ConstRef>, LowerError> {
        if value == Value::new(false) {
            Ok(None)
        } else {
            self.const_ref(value).map(Some)
        }
    }

    fn const_ref(&mut self, value: Value<'gc>) -> Result<ConstRef, LowerError> {
        match classify_const_value(value) {
            ClassifiedConst::Inline(value) => Ok(ConstRef::Inline(value)),
            ClassifiedConst::Heap(value) => Ok(ConstRef::Pool(self.intern_heap_const(value))),
        }
    }

    fn intern_name_text(&mut self, text: &str) -> ConstRef {
        ConstRef::Pool(self.intern_heap_const(Str::new(*self.ctx, text, true).into()))
    }

    fn intern_heap_const(&mut self, value: Value<'gc>) -> ConstId {
        if let Some(&id) = self.const_ids.get(&ValueEqual(value)) {
            return id;
        }
        let id = ConstId(self.const_entries.len() as u32);
        self.const_ids.insert(ValueEqual(value), id);
        self.const_entries.push(value);
        id
    }

    fn primitive_for_value(&self, value: Value<'gc>) -> Result<Primitive, LowerError> {
        if let Some(sym) = value.try_as::<Symbol>() {
            return Primitive::from_name(sym.as_str().as_ref())
                .ok_or_else(|| LowerError::UnknownPrimitive(sym.as_str().into_owned()));
        }

        let name = format!("{value}");
        Primitive::from_name(&name).ok_or(LowerError::UnknownPrimitive(name))
    }
}

#[derive(Clone)]
struct Env<'gc> {
    values: HashMap<LVarRef<'gc>, ValueId>,
    local_conts: HashMap<LVarRef<'gc>, LocalCont<'gc>>,
}

impl<'gc> Env<'gc> {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            local_conts: HashMap::new(),
        }
    }
}

#[derive(Clone, Copy)]
struct LocalCont<'gc> {
    cont: ContRef<'gc>,
    block: BlockId,
}

struct PendingBlock<'gc> {
    block: BlockId,
    env: Env<'gc>,
    body: TermRef<'gc>,
}

struct ProcLowerer<'a, 'gc> {
    module: &'a mut ModuleLowerer<'gc>,
    proc_id: ProcId,
    kind: ProcKind,
    binding_name: ConstRef,
    display_name: Option<ConstRef>,
    source: Option<ConstRef>,
    meta: Option<ConstRef>,
    cold: bool,
    noinline: bool,
    entry: BlockId,
    self_param: ValueId,
    retk_param: Option<ValueId>,
    arg_params: Vec<ValueId>,
    rest_param: Option<ValueId>,
    captures: Vec<Capture>,
    blocks: Vec<BlockBuilder<'gc>>,
    block_index: HashMap<BlockId, usize>,
    pending: VecDeque<PendingBlock<'gc>>,
    next_block: u32,
    next_value: u32,
}

#[derive(Clone)]
struct BlockBuilder<'gc> {
    id: BlockId,
    name: Option<ConstRef>,
    params: Vec<BlockParam>,
    insts: Vec<Inst<'gc>>,
    term: Option<Terminator<'gc>>,
    cold: bool,
}

impl<'a, 'gc> ProcLowerer<'a, 'gc> {
    fn new(
        module: &'a mut ModuleLowerer<'gc>,
        id: ProcId,
        kind: ProcKind,
        binding_name: ConstRef,
        display_name: Option<ConstRef>,
        source: Option<ConstRef>,
        meta: Option<ConstRef>,
        cold: bool,
        noinline: bool,
        captures: Vec<Capture>,
    ) -> Self {
        Self {
            module,
            proc_id: id,
            kind,
            binding_name,
            display_name,
            source,
            meta,
            cold,
            noinline,
            entry: BlockId::default(),
            self_param: ValueId::default(),
            retk_param: None,
            arg_params: Vec::new(),
            rest_param: None,
            captures,
            blocks: Vec::new(),
            block_index: HashMap::new(),
            pending: VecDeque::new(),
            next_block: 0,
            next_value: 0,
        }
    }

    fn finish(mut self) -> Result<Proc<'gc>, LowerError> {
        while let Some(pending) = self.pending.pop_front() {
            self.lower_term_in_block(pending.block, pending.body, pending.env)?;
        }

        let blocks = self
            .blocks
            .into_iter()
            .map(|block| Block {
                id: block.id,
                name: block.name,
                params: Array::from_slice(*self.module.ctx, &block.params),
                insts: Array::from_slice(*self.module.ctx, &block.insts),
                term: block
                    .term
                    .unwrap_or_else(|| panic!("block {:?} missing terminator", block.id)),
                cold: block.cold,
            })
            .collect::<Vec<_>>();

        Ok(Proc {
            id: self.proc_id,
            kind: self.kind,
            binding_name: self.binding_name,
            display_name: self.display_name,
            source: self.source,
            meta: self.meta,
            cold: self.cold,
            noinline: self.noinline,
            entry: self.entry,
            self_param: self.self_param,
            retk_param: self.retk_param,
            arg_params: Array::from_slice(*self.module.ctx, &self.arg_params),
            rest_param: self.rest_param,
            captures: Array::from_slice(*self.module.ctx, &self.captures),
            blocks: Array::from_slice(*self.module.ctx, &blocks),
        })
    }

    fn build_entry(
        &mut self,
        self_binding: Option<LVarRef<'gc>>,
        retk_binding: Option<LVarRef<'gc>>,
        args: Vec<LVarRef<'gc>>,
        rest: Option<LVarRef<'gc>>,
    ) -> Result<Env<'gc>, LowerError> {
        let entry_name = self.module.intern_name_text("entry");
        let entry = self.new_block(Some(entry_name), self.cold);
        self.entry = entry;
        let mut env = Env::new();

        let self_name = self.module.intern_name_text("self");
        let self_param = self.append_param(entry, Some(self_name));
        self.self_param = self_param;
        if let Some(binding) = self_binding {
            self.bind_value(&mut env, binding, self_param);
        }

        if let Some(retk_binding) = retk_binding {
            let retk_name = self.module.intern_name_text("retk");
            let retk = self.append_param(entry, Some(retk_name));
            self.retk_param = Some(retk);
            self.bind_value(&mut env, retk_binding, retk);
        }

        for arg in args {
            let arg_name = self.module.const_ref(arg.name)?;
            let value = self.append_param(entry, Some(arg_name));
            self.arg_params.push(value);
            self.bind_value(&mut env, arg, value);
        }

        if let Some(rest) = rest {
            let rest_name = self.module.const_ref(rest.name)?;
            let value = self.append_param(entry, Some(rest_name));
            self.rest_param = Some(value);
            self.bind_value(&mut env, rest, value);
        }

        Ok(env)
    }

    fn load_captures(
        &mut self,
        env: &mut Env<'gc>,
        captures: &[LVarRef<'gc>],
        source: Value<'gc>,
    ) -> Result<(), LowerError> {
        let source = self.module.const_opt(source)?;
        for (index, &capture) in captures.iter().enumerate() {
            let value = self.push_inst(
                self.entry,
                InstKind::ClosureRef {
                    closure: self.self_param,
                    index: index as u32,
                },
                source,
                true,
            );
            self.bind_value(env, capture, value);
        }
        Ok(())
    }

    fn lower_term(&mut self, term: TermRef<'gc>, env: Env<'gc>) -> Result<(), LowerError> {
        self.lower_term_in_block(self.entry, term, env)
    }

    fn lower_term_in_block(
        &mut self,
        block: BlockId,
        term: TermRef<'gc>,
        mut env: Env<'gc>,
    ) -> Result<(), LowerError> {
        let source = self.module.const_opt(term.source())?;
        match &*term {
            Term::Let(binding, expr, next) => {
                let value = match expr {
                    Expression::PrimCall(prim, args, expr_source) => {
                        let expr_source = self.module.const_opt(*expr_source)?;
                        self.lower_prim_expr(block, &mut env, *prim, *args, expr_source)?
                    }
                };
                env.values.insert(*binding, value);
                self.lower_term_in_block(block, *next, env)
            }
            Term::Fix(funcs, next) => {
                let mut extended = env.clone();
                let mut created = Vec::with_capacity(funcs.len());

                for &func in funcs.iter() {
                    let proc = *self.module.proc_for_func.get(&func).ok_or_else(|| {
                        LowerError::MissingFunctionProc(format!("{}", func.binding.name))
                    })?;
                    let meta = self.module.const_opt(func.meta)?;
                    let slots = self.module.ordered_func_captures(func).len() as u32;
                    let closure = self.push_inst(
                        block,
                        InstKind::MakeClosure {
                            proc,
                            is_cont: false,
                            meta,
                            slots,
                        },
                        source,
                        true,
                    );
                    self.bind_value(&mut extended, func.binding, closure);
                    created.push((func, closure));
                }

                for (func, closure) in created {
                    let mut captures = Vec::new();
                    for capture in self.module.ordered_func_captures(func) {
                        captures.push(self.lower_value(block, &mut extended, capture, source)?);
                    }
                    self.push_inst(
                        block,
                        InstKind::InitClosure {
                            closure,
                            captures: self.value_ids(&captures),
                        },
                        source,
                        false,
                    );
                }

                self.lower_term_in_block(block, *next, extended)
            }
            Term::Letk(conts, next) => {
                let mut extended = env.clone();
                let mut local_blocks = Vec::new();
                for &cont in conts.iter().filter(|cont| !cont.reified.get()) {
                    let block_name = self
                        .module
                        .intern_name_text(&format!("k_{}", cont.binding.name));
                    let block_id = self.new_block(Some(block_name), cont.cold);
                    extended.local_conts.insert(
                        cont.binding,
                        LocalCont {
                            cont,
                            block: block_id,
                        },
                    );
                    local_blocks.push((cont, block_id));
                }

                let mut reified_conts = Vec::new();
                for &cont in conts.iter().filter(|cont| cont.reified.get()) {
                    let proc = *self.module.proc_for_cont.get(&cont).ok_or_else(|| {
                        LowerError::MissingContinuationProc(format!("{}", cont.binding.name))
                    })?;
                    let meta = self.module.const_opt(cont.meta)?;
                    let slots = self.module.ordered_cont_captures(cont).len() as u32;
                    let closure = self.push_inst(
                        block,
                        InstKind::MakeClosure {
                            proc,
                            is_cont: true,
                            meta,
                            slots,
                        },
                        source,
                        true,
                    );
                    self.bind_value(&mut extended, cont.binding, closure);
                    reified_conts.push((cont, closure));
                }

                for (cont, closure) in reified_conts {
                    let mut captures = Vec::new();
                    for capture in self.module.ordered_cont_captures(cont) {
                        captures.push(self.lower_value(block, &mut extended, capture, source)?);
                    }
                    self.push_inst(
                        block,
                        InstKind::InitClosure {
                            closure,
                            captures: self.value_ids(&captures),
                        },
                        source,
                        false,
                    );
                }

                for (cont, block_id) in local_blocks {
                    let mut cont_env = extended.clone();
                    for &arg in cont.args.iter() {
                        let arg_name = self.module.const_ref(arg.name)?;
                        let value = self.append_param(block_id, Some(arg_name));
                        self.bind_value(&mut cont_env, arg, value);
                    }
                    if let Some(rest) = cont.variadic {
                        let rest_name = self.module.const_ref(rest.name)?;
                        let value = self.append_param(block_id, Some(rest_name));
                        self.bind_value(&mut cont_env, rest, value);
                    }
                    self.pending.push_back(PendingBlock {
                        block: block_id,
                        env: cont_env,
                        body: cont.body(),
                    });
                }

                self.lower_term_in_block(block, *next, extended)
            }
            Term::Continue(k, args, _) => {
                let args = args
                    .iter()
                    .copied()
                    .map(|atom| self.lower_atom(block, &mut env, atom, source))
                    .collect::<Result<Vec<_>, _>>()?;
                self.emit_continue(block, &mut env, *k, args, source)
            }
            Term::App(callee, retk, args, _) => {
                let callee_value = self.lower_atom(block, &mut env, *callee, source)?;
                let retk_value = self.lower_value(block, &mut env, *retk, source)?;
                let args = args
                    .iter()
                    .copied()
                    .map(|atom| self.lower_atom(block, &mut env, atom, source))
                    .collect::<Result<Vec<_>, _>>()?;

                let target = if let Atom::Local(var) = *callee {
                    if let Some(func) = self.module.reify.free_vars.funcs.get(&var) {
                        let proc = *self.module.proc_for_func.get(func).ok_or_else(|| {
                            LowerError::MissingFunctionProc(format!("{}", var.name))
                        })?;
                        TailTarget::Direct {
                            proc,
                            closure: callee_value,
                        }
                    } else {
                        TailTarget::Indirect {
                            closure: callee_value,
                        }
                    }
                } else {
                    TailTarget::Indirect {
                        closure: callee_value,
                    }
                };

                self.set_term(
                    block,
                    Terminator::TailApp {
                        callee: target,
                        retk: retk_value,
                        args: self.value_ids(&args),
                        source,
                    },
                );
                Ok(())
            }
            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints,
            } => {
                let cond = self.lower_atom(block, &mut env, *test, source)?;
                let consequent_args = consequent_args
                    .iter()
                    .flat_map(|args| args.iter().copied())
                    .map(|atom| self.lower_atom(block, &mut env, atom, source))
                    .collect::<Result<Vec<_>, _>>()?;
                let alternative_args = alternative_args
                    .iter()
                    .flat_map(|args| args.iter().copied())
                    .map(|atom| self.lower_atom(block, &mut env, atom, source))
                    .collect::<Result<Vec<_>, _>>()?;

                let (then_block, then_args) = self.branch_target(
                    block,
                    &mut env,
                    *consequent,
                    consequent_args,
                    hints[0],
                    source,
                )?;
                let (else_block, else_args) = self.branch_target(
                    block,
                    &mut env,
                    *alternative,
                    alternative_args,
                    hints[1],
                    source,
                )?;

                self.set_term(
                    block,
                    Terminator::Branch {
                        cond,
                        then_block,
                        then_args: self.value_ids(&then_args),
                        else_block,
                        else_args: self.value_ids(&else_args),
                        hints: *hints,
                    },
                );
                Ok(())
            }
        }
    }

    fn branch_target(
        &mut self,
        block: BlockId,
        env: &mut Env<'gc>,
        target: LVarRef<'gc>,
        args: Vec<ValueId>,
        hint: BranchHint,
        source: Option<ConstRef>,
    ) -> Result<(BlockId, Vec<ValueId>), LowerError> {
        if let Some(local) = env.local_conts.get(&target).copied() {
            let args = self.pack_local_cont_args(block, local.cont, args, source)?;
            return Ok((local.block, args));
        }

        let adapter_name = self
            .module
            .intern_name_text(&format!("branch_to_{}", target.name));
        let adapter = self.new_block(Some(adapter_name), matches!(hint, BranchHint::Cold));
        let mut adapter_args = Vec::with_capacity(args.len());
        for (index, _) in args.iter().enumerate() {
            let arg_name = self.module.intern_name_text(&format!("arg{index}"));
            adapter_args.push(self.append_param(adapter, Some(arg_name)));
        }

        self.emit_reified_continue(adapter, env, target, adapter_args.clone(), source)?;
        Ok((adapter, args))
    }

    fn emit_continue(
        &mut self,
        block: BlockId,
        env: &mut Env<'gc>,
        target: LVarRef<'gc>,
        args: Vec<ValueId>,
        source: Option<ConstRef>,
    ) -> Result<(), LowerError> {
        if let Some(local) = env.local_conts.get(&target).copied() {
            let args = self.pack_local_cont_args(block, local.cont, args, source)?;
            self.set_term(
                block,
                Terminator::Jump {
                    block: local.block,
                    args: self.value_ids(&args),
                },
            );
            return Ok(());
        }

        self.emit_reified_continue(block, env, target, args, source)
    }

    fn emit_reified_continue(
        &mut self,
        block: BlockId,
        env: &mut Env<'gc>,
        target: LVarRef<'gc>,
        args: Vec<ValueId>,
        source: Option<ConstRef>,
    ) -> Result<(), LowerError> {
        let closure = self.lower_value(block, env, target, source)?;
        let callee = if let Some(cont) = self.module.reify.free_vars.conts.get(&target).copied() {
            if cont.reified.get() {
                TailTarget::Direct {
                    proc: *self.module.proc_for_cont.get(&cont).ok_or_else(|| {
                        LowerError::MissingContinuationProc(format!("{}", target.name))
                    })?,
                    closure,
                }
            } else {
                return Err(LowerError::MissingValue(format!("{}", target.name)));
            }
        } else {
            TailTarget::Indirect { closure }
        };

        self.set_term(
            block,
            Terminator::TailContinue {
                callee,
                args: self.value_ids(&args),
                source,
            },
        );
        Ok(())
    }

    fn pack_local_cont_args(
        &mut self,
        block: BlockId,
        cont: ContRef<'gc>,
        args: Vec<ValueId>,
        source: Option<ConstRef>,
    ) -> Result<Vec<ValueId>, LowerError> {
        self.pack_args_for_arity(
            block,
            cont.args.len(),
            cont.variadic.is_some(),
            args,
            source,
        )
    }

    fn pack_args_for_arity(
        &mut self,
        block: BlockId,
        fixed: usize,
        variadic: bool,
        args: Vec<ValueId>,
        source: Option<ConstRef>,
    ) -> Result<Vec<ValueId>, LowerError> {
        if !variadic {
            return Ok(args);
        }

        let mut packed = args.iter().take(fixed).copied().collect::<Vec<_>>();
        let rest = args.into_iter().skip(fixed).collect::<Vec<_>>();
        let rest = self.pack_rest(block, rest, source)?;
        packed.push(rest);
        Ok(packed)
    }

    fn pack_rest(
        &mut self,
        block: BlockId,
        items: Vec<ValueId>,
        source: Option<ConstRef>,
    ) -> Result<ValueId, LowerError> {
        let nil = self.module.const_ref(Value::null())?;
        let mut list = self.push_inst(block, InstKind::Const(nil), source, true);

        for item in items.into_iter().rev() {
            list = self.push_inst(
                block,
                InstKind::PrimCall {
                    prim: Primitive::Cons,
                    args: self.value_ids(&[item, list]),
                },
                source,
                true,
            );
        }

        Ok(list)
    }

    fn lower_atom(
        &mut self,
        block: BlockId,
        env: &mut Env<'gc>,
        atom: Atom<'gc>,
        source: Option<ConstRef>,
    ) -> Result<ValueId, LowerError> {
        match atom {
            Atom::Local(var) => self.lower_value(block, env, var, source),
            Atom::Constant(value) => {
                let value = self.module.const_ref(value)?;
                Ok(self.push_inst(block, InstKind::Const(value), source, true))
            }
        }
    }

    fn lower_prim_expr(
        &mut self,
        block: BlockId,
        env: &mut Env<'gc>,
        prim_value: Value<'gc>,
        args: crate::cps::term::Atoms<'gc>,
        source: Option<ConstRef>,
    ) -> Result<ValueId, LowerError> {
        let prim = self.module.primitive_for_value(prim_value)?;
        let args = args
            .iter()
            .copied()
            .map(|atom| self.lower_atom(block, env, atom, source))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.push_inst(
            block,
            InstKind::PrimCall {
                prim,
                args: self.value_ids(&args),
            },
            source,
            true,
        ))
    }

    fn lower_value(
        &mut self,
        _block: BlockId,
        env: &mut Env<'gc>,
        var: LVarRef<'gc>,
        _source: Option<ConstRef>,
    ) -> Result<ValueId, LowerError> {
        match env.values.get(&var).copied() {
            Some(value) => Ok(value),
            None => Err(if env.local_conts.contains_key(&var) {
                LowerError::NonReifiedContinuationValue(format!("{}", var.name))
            } else {
                LowerError::MissingValue(format!("{}", var.name))
            }),
        }
    }

    fn bind_value(&self, env: &mut Env<'gc>, var: LVarRef<'gc>, value: ValueId) {
        env.values.insert(var, value);
    }

    fn push_inst(
        &mut self,
        block: BlockId,
        kind: InstKind<'gc>,
        source: Option<ConstRef>,
        has_result: bool,
    ) -> ValueId {
        let result = has_result.then(|| self.fresh_value());
        self.block_mut(block).insts.push(Inst {
            result,
            source,
            kind,
        });
        result.unwrap_or_default()
    }

    fn set_term(&mut self, block: BlockId, term: Terminator<'gc>) {
        let block = self.block_mut(block);
        assert!(
            block.term.is_none(),
            "block {:?} already terminated",
            block.id
        );
        block.term = Some(term);
    }

    fn new_block(&mut self, name: Option<ConstRef>, cold: bool) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.block_index.insert(id, self.blocks.len());
        self.blocks.push(BlockBuilder {
            id,
            name,
            params: Vec::new(),
            insts: Vec::new(),
            term: None,
            cold,
        });
        id
    }

    fn append_param(&mut self, block: BlockId, name: Option<ConstRef>) -> ValueId {
        let value = self.fresh_value();
        self.block_mut(block)
            .params
            .push(BlockParam { value, name });
        value
    }

    fn value_ids(&self, values: &[ValueId]) -> ArrayRef<'gc, ValueId> {
        Array::from_slice(*self.module.ctx, values)
    }

    fn fresh_value(&mut self) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        value
    }

    fn block_mut(&mut self, block: BlockId) -> &mut BlockBuilder<'gc> {
        let index = self.block_index[&block];
        &mut self.blocks[index]
    }
}

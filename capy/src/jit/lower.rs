#![allow(dead_code)]

use std::{collections::HashMap, io::Write, mem::offset_of};

use crate::rsgc::{Gc, sync::thread::Thread};

use crate::{
    call_signature,
    compiler::debuginfo::{DebugContext, FunctionDebugContext},
    cps::{
        ReifyInfo,
        free_vars::FreeVars,
        linear::{
            Block as LinearBlock, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom,
            LinearProgram, Procedure, ProcedureKind, RestPredicate, SwitchCaseValue, SwitchKind,
            Terminator, ValueId, ValueSource,
        },
        term::{Atom, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::core::{LVarRef, fresh_lvar},
    runtime::{
        Context, State,
        value::{Closure, ReturnCode, Symbol, Tagged, TypeCode8, Value},
        vm::thunks::{ImportedThunks, Thunks},
    },
};
use cranelift::frontend::Switch;
use cranelift::prelude::{FunctionBuilder, InstBuilder, IntCC, types};
use cranelift_codegen::{
    ir::{self, BlockArg, ExtFuncData, ExternalName, SourceLoc, UserExternalName},
    isa::CallConv,
};
use pretty::BoxAllocator;

use super::primitive::{PrimValue, Primitive as JitPrimitive, PrimitiveLowerer};
use super::{JitCompiler, JitNameMap, JitRelocTarget, JitUnitDecls};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum VarDef {
    Value(ir::Value),
    Comparison(ir::Value),
}

#[derive(Clone, Copy)]
pub(super) struct LinearRestSource {
    pub rands: ir::Value,
    pub num_rands: ir::Value,
    pub fixed_count: usize,
}

#[derive(Clone)]
pub(super) enum JitTarget<'gc> {
    Func(FuncRef<'gc>),
    Cont(ContRef<'gc>),
    Procedure(Procedure<'gc>),
}

pub(super) struct JitBuilder<'gc, 'a> {
    pub ctx: Context<'gc>,
    pub(crate) debug_context: DebugContext<'gc>,
    pub known_free_vars: Option<FreeVars<'gc>>,
    pub constants: HashMap<Value<'gc>, (u32, u32)>,
    pub cache_cells: HashMap<Value<'gc>, (u32, u32)>,
    pub name_map: &'a mut JitNameMap,
    pub decls: &'a JitUnitDecls,
    pub compiler: &'a mut JitCompiler,
    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_code: HashMap<CodeId, (u32, u32)>,
    pub func_for_cont: HashMap<ContRef<'gc>, (u32, u32)>,
    pub func_for_func: HashMap<FuncRef<'gc>, (u32, u32)>,
    pub code_block_for_code: HashMap<CodeId, (u32, u32)>,
    pub code_block_for_cont: HashMap<ContRef<'gc>, (u32, u32)>,
    pub code_block_for_func: HashMap<FuncRef<'gc>, (u32, u32)>,
    pub import_data: HashMap<&'static str, (u32, u32)>,
    pub global_side_metadata_base_address: (u32, u32),
    pub thunks: Thunks,
    pub stacktraces: bool,
    pub direct_calls: bool,
}

impl<'gc, 'a> JitBuilder<'gc, 'a> {
    pub(super) fn new(
        ctx: Context<'gc>,
        compiler: &'a mut JitCompiler,
        name_map: &'a mut JitNameMap,
        decls: &'a JitUnitDecls,
        reify_info: ReifyInfo<'gc>,
        global_side_metadata_symbol: &str,
        thunks: Thunks,
    ) -> Self {
        let debug_context = DebugContext::new(&reify_info, compiler.isa.as_ref());
        Self::new_with_debug_context(
            ctx,
            compiler,
            name_map,
            decls,
            debug_context,
            Some(reify_info.free_vars),
            global_side_metadata_symbol,
            thunks,
        )
    }

    pub(super) fn new_for_linear_program(
        ctx: Context<'gc>,
        compiler: &'a mut JitCompiler,
        name_map: &'a mut JitNameMap,
        decls: &'a JitUnitDecls,
        linear: &LinearProgram<'gc>,
        global_side_metadata_symbol: &str,
        thunks: Thunks,
    ) -> Self {
        let debug_context = DebugContext::new_for_linear_program(linear, compiler.isa.as_ref());
        Self::new_with_debug_context(
            ctx,
            compiler,
            name_map,
            decls,
            debug_context,
            None,
            global_side_metadata_symbol,
            thunks,
        )
    }

    fn new_with_debug_context(
        ctx: Context<'gc>,
        compiler: &'a mut JitCompiler,
        name_map: &'a mut JitNameMap,
        decls: &'a JitUnitDecls,
        debug_context: DebugContext<'gc>,
        known_free_vars: Option<FreeVars<'gc>>,
        _global_side_metadata_symbol: &str,
        thunks: Thunks,
    ) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let global_side_metadata_base_address =
            name_map.declare_data(JitRelocTarget::GlobalSideMetadataBaseAddress, 0);

        Self {
            debug_context,
            ctx,
            stacktraces: true,
            direct_calls: false,
            known_free_vars,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            compiler,
            name_map,
            decls,
            import_data: HashMap::new(),
            prims,
            func_for_code: decls.funcs_by_code.clone(),
            func_for_cont: HashMap::new(),
            func_for_func: HashMap::new(),
            code_block_for_code: decls.code_blocks_by_code.clone(),
            code_block_for_cont: HashMap::new(),
            code_block_for_func: HashMap::new(),
            global_side_metadata_base_address,
            thunks,
        }
    }

    fn known_func(&self, var: LVarRef<'gc>) -> Option<FuncRef<'gc>> {
        self.known_free_vars
            .as_ref()
            .and_then(|free_vars| free_vars.funcs.get(&var).copied())
    }

    fn known_cont(&self, var: LVarRef<'gc>) -> Option<ContRef<'gc>> {
        self.known_free_vars
            .as_ref()
            .and_then(|free_vars| free_vars.conts.get(&var).copied())
    }

    fn has_known_cont(&self, var: LVarRef<'gc>) -> bool {
        self.known_free_vars
            .as_ref()
            .is_some_and(|free_vars| free_vars.conts.contains_key(&var))
    }

    pub fn intern_constant(&mut self, obj: Value<'gc>) -> Option<(u32, u32)> {
        if obj.is_immediate() {
            return None;
        }
        self.constants.get(&obj).copied().or_else(|| {
            panic!("JIT constant slot not found for {obj}");
        })
    }

    pub fn intern_cache_cell(&mut self, key: Value<'gc>) -> (u32, u32) {
        self.cache_cells
            .get(&key)
            .copied()
            .unwrap_or_else(|| panic!("JIT cache cell slot not found for {key}"))
    }
}

pub(super) struct JitLowerer<'gc, 'a, 'f> {
    pub module_builder: &'a mut JitBuilder<'gc, 'a>,
    pub builder: FunctionBuilder<'f>,
    pub(crate) func_debug_cx: FunctionDebugContext<'gc>,

    pub blockmap: HashMap<ContRef<'gc>, ir::Block>,
    pub linear_blockmap: HashMap<crate::cps::linear::BlockId, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,
    pub linear_variables: HashMap<ValueId, VarDef>,
    pub linear_rest_sources: HashMap<ValueId, LinearRestSource>,
    pub synthetic_aliases: HashMap<ValueId, LVarRef<'gc>>,

    pub target: JitTarget<'gc>,
    pub exit_block: ir::Block,
    pub entry_block: ir::Block,
    pub app_block: Option<ir::Block>,

    pub rator: ir::Value,
    pub thunks: ImportedThunks,

    pub sig_call: ir::SigRef,
    pub to_generate: Vec<ContRef<'gc>>,

    pub data_imports: HashMap<(u32, u32), ir::GlobalValue>,

    pub observed_code_refs: Vec<CodeId>,
    pub srcloc: Option<SourceLoc>,
}

impl<'gc, 'a, 'f> JitLowerer<'gc, 'a, 'f> {
    pub(crate) fn new(
        module_builder: &'a mut JitBuilder<'gc, 'a>,
        mut builder: FunctionBuilder<'f>,
        target: JitTarget<'gc>,
        thunks: ImportedThunks,
        mut func_debug_cx: FunctionDebugContext<'gc>,
    ) -> Self {
        builder.func.dfg.collect_debug_info();
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        let rator = builder.block_params(entry)[0];
        let rands = builder.block_params(entry)[1];
        let num_rands = builder.block_params(entry)[2];

        let variables = HashMap::new();

        let sig_call = call_signature!(Tail (I64 /* rator */, I64 /* rands */, I64 /* num_rands */) -> (I64, I64));
        let sig_call = builder.import_signature(sig_call);

        let exit_block = builder.create_block();

        builder.append_block_param(exit_block, types::I64);
        builder.append_block_param(exit_block, types::I64);
        builder.append_block_param(exit_block, types::I64);
        builder.append_block_param(exit_block, types::I64);

        builder.set_val_label(rator, func_debug_cx.internal_variable(0));
        builder.set_val_label(num_rands, func_debug_cx.internal_variable(1));
        builder.set_val_label(rands, func_debug_cx.internal_variable(2));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.ins().jump(
            entry_block,
            &[
                BlockArg::Value(rator),
                BlockArg::Value(rands),
                BlockArg::Value(num_rands),
            ],
        );
        builder.switch_to_block(entry_block);
        let entry_rator = builder.block_params(entry_block)[0];
        let entry_rands = builder.block_params(entry_block)[1];
        let entry_num_rands = builder.block_params(entry_block)[2];
        builder.set_val_label(entry_rator, func_debug_cx.internal_variable(0));
        builder.set_val_label(entry_num_rands, func_debug_cx.internal_variable(1));
        builder.set_val_label(entry_rands, func_debug_cx.internal_variable(2));

        let mut this = Self {
            module_builder,
            builder,
            target,
            exit_block,
            app_block: None,
            rator: entry_rator,
            func_debug_cx,
            entry_block,
            variables,
            linear_variables: HashMap::new(),
            linear_rest_sources: HashMap::new(),
            synthetic_aliases: HashMap::new(),
            blockmap: HashMap::new(),
            linear_blockmap: HashMap::new(),
            to_generate: Vec::new(),
            thunks,
            sig_call,
            data_imports: HashMap::new(),
            observed_code_refs: Vec::new(),
            srcloc: None,
        };

        this.entrypoint(entry_rands, entry_num_rands);

        this
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Callee {
    /// Callee is obtained by loading code pointer from a closure
    /// and doing an indirect tail-call.
    Indirect {
        target: ir::Value,
        closure: ir::Value,
    },
    /// Callee is a direct function reference, and a direct tail-call can be performed.
    Direct {
        target: ir::FuncRef,
        closure: ir::Value,
    },

    /// Callee is a self-recursive function, and the function body block is returned.
    SelfRec(ir::Block),
}

impl<'gc, 'a, 'f> JitLowerer<'gc, 'a, 'f> {
    fn target_binding(&self) -> LVarRef<'gc> {
        match &self.target {
            JitTarget::Cont(c) => c.binding,
            JitTarget::Func(f) => f.binding,
            JitTarget::Procedure(_) => {
                panic!("linear procedures do not expose LVarRef bindings")
            }
        }
    }

    fn target_binding_debug_name(&self) -> String {
        match &self.target {
            JitTarget::Cont(c) => c.binding.name.to_string(),
            JitTarget::Func(f) => f.binding.name.to_string(),
            JitTarget::Procedure(p) => p
                .sources
                .get(&p.binding)
                .map(|source| source.name.to_string())
                .unwrap_or_else(|| format!("v{}", p.binding.0)),
        }
    }

    fn target_name(&self) -> Value<'gc> {
        match &self.target {
            JitTarget::Cont(c) => c.name,
            JitTarget::Func(f) => f.name,
            JitTarget::Procedure(p) => p.name,
        }
    }

    fn target_source(&self) -> Value<'gc> {
        match &self.target {
            JitTarget::Cont(c) => c.source(),
            JitTarget::Func(f) => f.source,
            JitTarget::Procedure(p) => p.source,
        }
    }

    fn target_params(&self) -> (Vec<LVarRef<'gc>>, Option<LVarRef<'gc>>) {
        match &self.target {
            JitTarget::Cont(c) => (c.args().iter().copied().collect(), c.variadic()),
            JitTarget::Func(f) => (f.args.iter().copied().collect(), f.variadic),
            JitTarget::Procedure(_) => {
                panic!("linear procedures load ValueId parameters directly")
            }
        }
    }

    fn target_linear_variadic(&self) -> Option<ValueId> {
        match &self.target {
            JitTarget::Procedure(p) => p.variadic,
            JitTarget::Cont(_) | JitTarget::Func(_) => None,
        }
    }

    fn should_materialize_entry_rest(&self, rest: LVarRef<'gc>) -> bool {
        match &self.target {
            JitTarget::Procedure(_) => false,
            JitTarget::Cont(_) | JitTarget::Func(_) => rest.is_referenced(),
        }
    }

    fn target_return_cont(&self) -> Option<LVarRef<'gc>> {
        match &self.target {
            JitTarget::Func(f) => Some(f.return_cont),
            JitTarget::Procedure(_) => None,
            JitTarget::Cont(_) => None,
        }
    }

    fn target_is_function(&self) -> bool {
        match &self.target {
            JitTarget::Func(_) => true,
            JitTarget::Procedure(p) => p.kind == ProcedureKind::Function,
            JitTarget::Cont(_) => false,
        }
    }

    pub fn is_self_reference(&mut self, var: LVarRef<'gc>) -> bool {
        match &self.target {
            JitTarget::Cont(c) => Gc::ptr_eq(c.binding, var),
            JitTarget::Func(f) => Gc::ptr_eq(f.binding, var),
            JitTarget::Procedure(_) => false,
        }
    }

    pub fn is_linear_self_reference(&self, var: ValueId) -> bool {
        match &self.target {
            JitTarget::Procedure(p) => p.binding == var,
            JitTarget::Cont(_) | JitTarget::Func(_) => false,
        }
    }

    pub fn closure_from_callee(&mut self, callee: Callee) -> ir::Value {
        match callee {
            Callee::Indirect { closure, .. } => closure,
            Callee::Direct { closure, .. } => closure,
            Callee::SelfRec(_) => self.rator,
        }
    }

    fn declare_func_in_func(&mut self, name_pair: (u32, u32)) -> ir::FuncRef {
        let (namespace, index) = name_pair;
        let target = self.module_builder.name_map.targets[&(namespace, index)].clone();
        let sig = self.signature_for_target(&target);
        let sig_ref = self.builder.func.import_signature(sig);
        let name_ref = self
            .builder
            .func
            .declare_imported_user_function(UserExternalName { namespace, index });
        self.builder.func.import_function(ExtFuncData {
            name: ExternalName::user(name_ref),
            signature: sig_ref,
            colocated: true,
        })
    }

    fn signature_for_target(&self, target: &JitRelocTarget) -> ir::Signature {
        let callconv = CallConv::SystemV;
        match target {
            JitRelocTarget::Procedure { .. } => call_signature!(Tail (I64, I64, I64) -> (I64, I64)),
            _ => ir::Signature::new(callconv),
        }
    }

    pub fn import_static(&mut self, name: &'static str, typ: ir::Type) -> ir::Value {
        let name_pair = if let Some(&pair) = self.module_builder.import_data.get(name) {
            pair
        } else {
            let pair = self.module_builder.name_map.declare_data(
                JitRelocTarget::RuntimeSymbol {
                    name: name.to_owned(),
                },
                0,
            );
            self.module_builder.import_data.insert(name, pair);
            pair
        };
        let data = self.import_data(name_pair);

        self.builder.ins().global_value(typ, data)
    }

    pub fn import_data(&mut self, data: (u32, u32)) -> ir::GlobalValue {
        self.data_imports
            .entry(data)
            .or_insert_with(|| {
                let (namespace, index) = data;
                let name_ref = self
                    .builder
                    .func
                    .declare_imported_user_function(UserExternalName { namespace, index });
                self.builder
                    .func
                    .create_global_value(ir::GlobalValueData::Symbol {
                        name: ExternalName::user(name_ref),
                        offset: ir::immediates::Imm64::new(0),
                        colocated: false,
                        tls: false,
                    })
            })
            .clone()
    }

    fn load_data_value(&mut self, data: (u32, u32)) -> ir::Value {
        let global_value = self.import_data(data);
        let addr = self.builder.ins().global_value(types::I64, global_value);
        self.builder
            .ins()
            .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0)
    }

    pub fn var(&mut self, var: LVarRef<'gc>) -> ir::Value {
        if self.is_self_reference(var) {
            return self.rator;
        }
        match *self.variables.get(&var).unwrap_or_else(|| {
            let alloc = ::pretty::BoxAllocator;
            let pretty = match &self.target {
                JitTarget::Func(f) => f.pretty::<_, &BoxAllocator>(&alloc),
                JitTarget::Cont(c) => c.pretty::<_, &BoxAllocator>(&alloc),
                JitTarget::Procedure(_) => {
                    panic!(
                        "{}@{:p} var not found when compiling {}({})",
                        var.name,
                        var,
                        self.target_binding_debug_name(),
                        self.target_name()
                    )
                }
            };

            let _ = pretty.1.render(80, &mut std::io::stdout());
            println!();
            let _ = std::io::stdout().flush();
            panic!(
                "{}@{:p} var not found when compiling {}({})",
                var.name,
                var,
                self.target_binding_debug_name(),
                self.target_name()
            )
        }) {
            VarDef::Comparison(val) => {
                let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);

                let val = self.builder.ins().select(val, true_, false_);
                self.debug_local(var, val);
                val
            }

            VarDef::Value(val) => val,
        }
    }

    pub fn entrypoint(&mut self, rands: ir::Value, num_rands: ir::Value) {
        let source = self.target_source();
        let is_function = self.target_is_function();
        self.set_debug_loc(source);
        if is_function {
            self.check_yield(self.rator, rands, num_rands);
        }

        if let JitTarget::Procedure(procedure) = self.target.clone() {
            self.load_linear_arguments(&procedure, rands, num_rands);
        } else {
            self.load_free_vars();
            self.load_arguments(rands, num_rands);
        }
    }

    pub(crate) fn current_retk_value(&mut self) -> ir::Value {
        match self.target.clone() {
            JitTarget::Procedure(procedure) => procedure
                .return_cont
                .map(|return_cont| self.linear_var(return_cont))
                .unwrap_or_else(|| self.default_retk_value()),
            JitTarget::Func(_) | JitTarget::Cont(_) => match self.target_return_cont() {
                Some(return_cont) => self.var(return_cont),
                None => self.default_retk_value(),
            },
        }
    }

    fn load_linear_arguments(
        &mut self,
        procedure: &Procedure<'gc>,
        mut rands: ir::Value,
        mut num_rands: ir::Value,
    ) {
        let params = procedure.params;
        let rest = procedure.variadic;

        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let state = self
            .builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            rands,
            state,
            offset_of!(State, runstack) as i32,
        );

        if let Some(return_cont) = procedure.return_cont {
            let retk = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                0,
            );

            rands = self.builder.ins().iadd_imm(rands, 8);
            num_rands = self.builder.ins().iadd_imm(num_rands, -1);
            self.bind_linear_var(return_cont, VarDef::Value(retk));
        }

        if let Some(rest) = rest {
            self.linear_rest_sources.insert(
                rest,
                LinearRestSource {
                    rands,
                    num_rands,
                    fixed_count: params.len(),
                },
            );
        }

        if params.len() != 0 {
            if let Some(rest) = rest {
                let not_enough = self.builder.ins().icmp_imm(
                    IntCC::UnsignedLessThan,
                    num_rands,
                    params.len() as i64,
                );

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(not_enough, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = -(params.len() as isize);
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected, rands],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.raise_to_exception_handler(err);
                }
                self.builder.switch_to_block(succ);
                {
                    for (i, param) in params.iter().copied().enumerate() {
                        let value = self.builder.ins().load(
                            types::I64,
                            ir::MemFlags::trusted().with_can_move(),
                            rands,
                            (i * 8) as i32,
                        );
                        self.bind_linear_var(param, VarDef::Value(value));
                    }

                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);
                    self.bind_linear_var(rest, VarDef::Value(null));
                }
            } else {
                let exact =
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::Equal, num_rands, params.len() as i64);

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(exact, succ, &[], err, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = params.len() as isize;
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected, rands],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.raise_to_exception_handler(err);
                }
                self.builder.switch_to_block(succ);
                for (i, param) in params.iter().copied().enumerate() {
                    let value = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        rands,
                        (i * 8) as i32,
                    );
                    self.bind_linear_var(param, VarDef::Value(value));
                }
            }
        } else if let Some(rest) = rest {
            let null = self
                .builder
                .ins()
                .iconst(types::I64, Value::null().bits() as i64);
            self.bind_linear_var(rest, VarDef::Value(null));
        } else {
            let succ = self.builder.create_block();
            let err = self.builder.create_block();

            self.builder.func.layout.set_cold(err);

            let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

            self.builder.ins().brif(nonzero, err, &[], succ, &[]);

            self.builder.switch_to_block(err);
            {
                let got = num_rands;
                let expected = 0isize;
                let expected = self.builder.ins().iconst(types::I64, expected as i64);
                let err = self.builder.ins().call(
                    self.thunks.wrong_number_of_args,
                    &[ctx, num_rands, got, expected, rands],
                );
                let err = self.builder.inst_results(err)[0];
                self.raise_to_exception_handler(err);
            }

            self.builder.switch_to_block(succ);
        }
    }

    fn default_retk_value(&mut self) -> ir::Value {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let call = self.builder.ins().call(self.thunks.default_retk, &[ctx]);
        self.builder.inst_results(call)[0]
    }

    pub(crate) fn call_proc_with_retk(
        &mut self,
        proc: ir::Value,
        retk: ir::Value,
        args: &[ir::Value],
    ) -> ir::Inst {
        let rands = std::iter::once(retk)
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        let rands = self.push_args(&rands);
        let num_rands = self
            .builder
            .ins()
            .iconst(types::I64, (args.len() + 1) as i64);
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            proc,
            offset_of!(Closure, code) as i32,
        );
        self.builder.ins().jump(
            self.exit_block,
            &[
                BlockArg::Value(code),
                BlockArg::Value(proc),
                BlockArg::Value(rands),
                BlockArg::Value(num_rands),
            ],
        )
    }

    pub(crate) fn raise_to_exception_handler(&mut self, err: ir::Value) -> ir::Inst {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let call = self
            .builder
            .ins()
            .call(self.thunks.exception_handler, &[ctx]);
        let handler = self.builder.inst_results(call)[0];
        let retk = self.current_retk_value();
        self.call_proc_with_retk(handler, retk, &[err])
    }

    fn load_arguments(&mut self, mut rands: ir::Value, mut num_rands: ir::Value) {
        let (params, rest) = self.target_params();

        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let state = self
            .builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        /* reset the runstack to the state it was before call */
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            rands,
            state,
            offset_of!(State, runstack) as i32,
        );

        if let Some(return_cont) = self.target_return_cont() {
            let retk = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                0,
            );

            rands = self.builder.ins().iadd_imm(rands, 8);
            num_rands = self.builder.ins().iadd_imm(num_rands, -1);

            self.debug_local(return_cont, retk);

            self.variables.insert(return_cont, VarDef::Value(retk));
        }

        if let Some(rest) = self.target_linear_variadic() {
            self.linear_rest_sources.insert(
                rest,
                LinearRestSource {
                    rands,
                    num_rands,
                    fixed_count: params.len(),
                },
            );
        }

        if params.len() != 0 {
            if let Some(rest) = rest {
                let not_enough = self.builder.ins().icmp_imm(
                    IntCC::UnsignedLessThan,
                    num_rands,
                    params.len() as i64,
                );

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(not_enough, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = -(params.len() as isize);
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected, rands],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.raise_to_exception_handler(err);
                }
                self.builder.switch_to_block(succ);
                {
                    for (i, param) in params.iter().enumerate() {
                        let value = self.builder.ins().load(
                            types::I64,
                            ir::MemFlags::trusted().with_can_move(),
                            rands,
                            (i * 8) as i32,
                        );
                        self.variables.insert(*param, VarDef::Value(value));
                        self.debug_local(*param, value);
                    }

                    let need_cons = self.builder.ins().icmp_imm(
                        IntCC::NotEqual,
                        num_rands,
                        params.len() as i64,
                    );

                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);

                    let succ = self.builder.create_block();
                    let cons_block = self.builder.create_block();
                    self.builder.append_block_param(succ, types::I64);
                    self.builder.ins().brif(
                        need_cons,
                        cons_block,
                        &[],
                        succ,
                        &[BlockArg::Value(null)],
                    );
                    self.builder.switch_to_block(cons_block);
                    {
                        if self.should_materialize_entry_rest(rest) {
                            let from = self.builder.ins().iconst(types::I64, params.len() as i64);
                            let cons = self
                                .builder
                                .ins()
                                .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                            let list = self.builder.inst_results(cons)[0];

                            self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                        } else {
                            // rest list is not referenced: do not waste time materializing it
                            let null = self
                                .builder
                                .ins()
                                .iconst(types::I64, Value::null().bits() as i64);
                            self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                        }
                    }
                    self.builder.switch_to_block(succ);
                    {
                        let ls = self.builder.block_params(succ)[0];
                        self.variables.insert(rest, VarDef::Value(ls));
                        self.debug_local(rest, ls);
                    }
                }
            } else {
                let exact =
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::Equal, num_rands, params.len() as i64);

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(exact, succ, &[], err, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = params.len() as isize;
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, self.rator, got, expected, rands],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.raise_to_exception_handler(err);
                }
                self.builder.switch_to_block(succ);
                for (i, param) in params.iter().enumerate() {
                    let value = self.builder.ins().load(
                        types::I64,
                        ir::MemFlags::trusted().with_can_move(),
                        rands,
                        (i * 8) as i32,
                    );
                    self.variables.insert(*param, VarDef::Value(value));
                    self.debug_local(*param, value);
                }
            }
        } else {
            if let Some(rest) = rest {
                let succ = self.builder.create_block();
                let cons_block = self.builder.create_block();

                let null = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);

                self.builder.append_block_param(succ, types::I64);

                let need_cons = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder
                    .ins()
                    .brif(need_cons, cons_block, &[], succ, &[BlockArg::Value(null)]);
                self.builder.switch_to_block(cons_block);
                {
                    if self.should_materialize_entry_rest(rest) {
                        let from = self.builder.ins().iconst(types::I64, 0);
                        let cons = self
                            .builder
                            .ins()
                            .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                        let list = self.builder.inst_results(cons)[0];

                        self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                    } else {
                        // rest list is not referenced: do not waste time materializing it
                        let null = self
                            .builder
                            .ins()
                            .iconst(types::I64, Value::null().bits() as i64);
                        self.builder.ins().jump(succ, &[BlockArg::Value(null)]);
                    }
                }
                self.builder.switch_to_block(succ);
                {
                    let ls = self.builder.block_params(succ)[0];
                    self.variables.insert(rest, VarDef::Value(ls));
                    self.debug_local(rest, ls);
                }
            } else {
                let succ = self.builder.create_block();
                let err = self.builder.create_block();

                self.builder.func.layout.set_cold(err);

                let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder.ins().brif(nonzero, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let got = num_rands;
                    let expected = 0isize;
                    let expected = self.builder.ins().iconst(types::I64, expected as i64);
                    let err = self.builder.ins().call(
                        self.thunks.wrong_number_of_args,
                        &[ctx, num_rands, got, expected, rands],
                    );
                    let err = self.builder.inst_results(err)[0];
                    self.raise_to_exception_handler(err);
                }

                self.builder.switch_to_block(succ);
            }
        }
    }

    pub fn block_for_cont(&mut self, k: ContRef<'gc>) -> ir::Block {
        if let Some(block) = self.blockmap.get(&k) {
            return *block;
        }

        let block = self.builder.create_block();
        if k.cold {
            self.builder.func.layout.set_cold(block);
        }
        self.blockmap.insert(k, block);
        for _ in k.args.iter() {
            self.builder.append_block_param(block, types::I64);
        }

        if k.variadic().is_some() {
            self.builder.append_block_param(block, types::I64);
        }

        for i in 0..k.args.len() {
            self.variables.insert(
                k.args[i],
                VarDef::Value(self.builder.block_params(block)[i]),
            );
        }
        if let Some(rest) = k.variadic() {
            self.variables.insert(
                rest,
                VarDef::Value(self.builder.block_params(block)[k.args.len()]),
            );
        }

        block
    }

    pub fn get_callee_k(&mut self, var: LVarRef<'gc>) -> Callee {
        if let Some(cont) = self.module_builder.known_cont(var)
            && let Some(name_pair) = self.module_builder.func_for_cont.get(&cont)
        {
            let name_pair = *name_pair;
            let closure = self.var(var);
            let func_ref = self.declare_func_in_func(name_pair);
            return Callee::Direct {
                target: func_ref,
                closure,
            };
        }

        let callee = self.atom(Atom::Local(var));
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            callee,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure: callee,
        }
    }

    /// Get callee entrypoint or report non-applicable error via current exception handler.
    pub fn get_callee(&mut self, callee: &Atom<'gc>) -> Callee {
        if let Atom::Local(var) = callee {
            if let Some(func) = self.module_builder.known_func(*var)
                && let Some(name_pair) = self.module_builder.func_for_func.get(&func)
            {
                let name_pair = *name_pair;
                let closure = self.atom(*callee);
                let func_ref = self.declare_func_in_func(name_pair);
                return Callee::Direct {
                    target: func_ref,
                    closure,
                };
            }
        }
        let callee = self.atom(*callee);

        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let get_clos_code = self.builder.create_block();
        let error = self.builder.create_block();
        self.builder.func.layout.set_cold(error);
        self.branch_if_has_typ8(callee, Closure::TC8.bits(), get_clos_code, &[], error, &[]);
        self.builder.switch_to_block(error);
        {
            let err = self
                .builder
                .ins()
                .call(self.thunks.non_applicable, &[ctx, callee]);
            let err = self.builder.inst_results(err)[0];
            self.raise_to_exception_handler(err);
        }

        self.builder.switch_to_block(get_clos_code);
        {
            let code = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                callee,
                offset_of!(Closure, code) as i32,
            );
            Callee::Indirect {
                target: code,
                closure: callee,
            }
        }
    }

    /// Push arguments to runstack. Returns the base of the arguments array.
    pub fn push_args(&mut self, args: &[ir::Value]) -> ir::Value {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let state = self
            .builder
            .ins()
            .iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let runstack = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            offset_of!(State, runstack) as i32,
        );
        let new_runstack = self
            .builder
            .ins()
            .iadd_imm(runstack, (args.len() * 8) as i64);
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            new_runstack,
            state,
            offset_of!(State, runstack) as i32,
        );

        for (i, arg) in args.iter().enumerate() {
            self.builder.ins().store(
                ir::MemFlags::trusted().with_can_move(),
                *arg,
                runstack,
                (i * 8) as i32,
            );
        }

        runstack
    }

    /// Continue execution at continuation `k` with arguments `args`.
    ///
    /// If `k` is reified this performs return call instead of local jump. Arguments
    /// are pushed onto runstack in that case.
    pub fn continue_to(&mut self, k: LVarRef<'gc>, args: &[ir::Value]) -> ir::Inst {
        if let Some(k) = self.module_builder.known_cont(k)
            && !k.reified.get()
        {
            // TODO: Support `k` where variadic is used and materialize a list.
            let block = self.block_for_cont(k);
            if k.args.len() > args.len() {
                panic!(
                    "not enough args when jumping to continuation {}@{:p}, expected {}, got {}",
                    k.binding.name,
                    k.binding,
                    k.args.len(),
                    args.len()
                );
            }
            let mut block_args = args[..k.args.len()]
                .into_iter()
                .map(|v| ir::BlockArg::Value(*v))
                .collect::<Vec<_>>();
            if let Some(variadic) = k.variadic() {
                if variadic.is_referenced() {
                    let mut ls = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);
                    for arg in args[k.args.len()..].iter().rev() {
                        let call = self.builder.ins().call(self.thunks.cons, &[ctx, *arg, ls]);
                        ls = self.builder.inst_results(call)[0];
                    }
                    block_args.push(ir::BlockArg::Value(ls));
                } else {
                    // Do not materialize the list if variadic is not referenced
                    let null = self
                        .builder
                        .ins()
                        .iconst(types::I64, Value::null().bits() as i64);
                    block_args.push(ir::BlockArg::Value(null));
                }
            }

            let bb_args_len = self.builder.block_params(block).len();
            if bb_args_len != block_args.len() {
                panic!(
                    "wrong number of args when jumping to continuation {}@{:p}, expected {}, got {}",
                    k.binding.name,
                    k.binding,
                    bb_args_len,
                    block_args.len()
                );
            }
            return self.builder.ins().jump(block, &block_args);
        }

        let callee = self.get_callee_k(k);
        let rands = self.push_args(args);
        let num_rands = self.builder.ins().iconst(types::I64, args.len() as i64);
        match callee {
            Callee::Indirect { target, closure } => self.builder.ins().jump(
                self.exit_block,
                &[
                    BlockArg::Value(target),
                    BlockArg::Value(closure),
                    BlockArg::Value(rands),
                    BlockArg::Value(num_rands),
                ],
            ),
            Callee::Direct { target, closure } => self
                .builder
                .ins()
                .return_call(target, &[closure, rands, num_rands]),

            Callee::SelfRec(block) => self.builder.ins().jump(
                block,
                &[
                    BlockArg::Value(self.rator),
                    BlockArg::Value(rands),
                    BlockArg::Value(num_rands),
                ],
            ),
        }
    }

    fn load_free_vars(&mut self) {
        let Some(fvs) = (match &self.target {
            JitTarget::Func(func) => func.free_vars.get(),
            JitTarget::Cont(cont) => cont.free_vars.get(),
            JitTarget::Procedure(_) => None,
        }) else {
            return;
        };

        /* load all free-variables of closure.

           We can directly load values without later modifying `free` vector
           as all variables are immutable, mutable variables should've been boxed by optimizing compiler.
        */
        for (i, var) in fvs.iter().copied().enumerate() {
            let value = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                self.rator,
                Closure::DATA_OFFSET as i32 + (i * 8) as i32,
            );
            self.variables.insert(var, VarDef::Value(value));
        }
    }

    /* helpers */

    pub fn check_argcount(&mut self, proc: &str, got_: usize, expected_: usize) {
        let got = self.builder.ins().iconst(types::I64, got_ as i64);
        let expected = self.builder.ins().iconst(types::I64, expected_ as i64);

        let on_err = self.builder.create_block();
        let on_succ = self.builder.create_block();

        let cmp = self.builder.ins().icmp(IntCC::Equal, got, expected);

        self.builder.ins().brif(cmp, on_succ, &[], on_err, &[]);
        self.builder.func.layout.set_cold(on_err);
        self.builder.switch_to_block(on_err);
        {
            self.wrong_num_args(proc, got_, expected_ as _);
        }
        self.builder.switch_to_block(on_succ);
    }

    pub fn atom_for_cond(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Local(var) => match self.variables[&var] {
                VarDef::Comparison(val) => val,
                _ => {
                    let val = self.var(var);
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
                }
            },

            _ => {
                let val = self.atom(atom);
                self.builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
            }
        }
    }

    pub fn atom(&mut self, atom: Atom<'gc>) -> ir::Value {
        match atom {
            Atom::Local(var) => self.var(var),
            Atom::Constant(obj) => {
                let Some(data_id) = self.module_builder.intern_constant(obj) else {
                    return self.builder.ins().iconst(types::I64, obj.bits() as i64);
                };

                let global_value = self.import_data(data_id);

                let addr = self.builder.ins().global_value(types::I64, global_value);
                self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    addr,
                    0,
                )
            }
        }
    }

    pub fn fix(&mut self, funcs: &[FuncRef<'gc>]) {
        for func in funcs.iter() {
            let free = func.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());
            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 0);
            let code_block = self.load_data_value(self.module_builder.code_block_for_func[&func]);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);

            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, code_block, nfree, is_cont]);
            let clos = self.builder.inst_results(clos)[0];
            self.debug_local_with_source(func.binding, clos, func.source);
            self.variables.insert(func.binding, VarDef::Value(clos));
        }

        for func in funcs.iter() {
            let clos = self.var(func.binding);
            let free = func.free_vars.get();
            if let Some(free) = free {
                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        clos,
                        (Closure::DATA_OFFSET as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }
    }

    pub fn letk(&mut self, conts: &[ContRef<'gc>]) {
        for cont in conts.iter().filter(|k| k.reified.get()) {
            let free = cont.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());

            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 1);
            let code_block = self.load_data_value(self.module_builder.code_block_for_cont[&cont]);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, code_block, nfree, is_cont]);
            let clos = self.builder.inst_results(clos)[0];
            self.debug_local_with_source(cont.binding, clos, cont.source);
            self.variables.insert(cont.binding, VarDef::Value(clos));
        }

        for cont in conts.iter().filter(|k| k.reified.get()) {
            let clos = self.var(cont.binding);
            let free = cont.free_vars.get();

            if let Some(free) = free {
                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        clos,
                        (Closure::DATA_OFFSET as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }

        for &cont in conts.iter().filter(|k| !k.reified.get()) {
            let _block = self.block_for_cont(cont);
            self.to_generate.push(cont);
        }
    }

    pub fn linear_procedure(&mut self, procedure: &Procedure<'gc>) {
        for block in &procedure.blocks {
            if block.id == procedure.entry {
                self.linear_blockmap.insert(block.id, self.entry_block);
                continue;
            }

            let clif_block = self.builder.create_block();
            for _ in &block.params {
                self.builder.append_block_param(clif_block, types::I64);
            }
            self.linear_blockmap.insert(block.id, clif_block);
        }

        let entry = procedure
            .blocks
            .iter()
            .find(|block| block.id == procedure.entry)
            .expect("linear procedure should contain its entry block");
        self.lower_linear_block(procedure, entry, true);

        for block in &procedure.blocks {
            if block.id == procedure.entry {
                continue;
            }

            let clif_block = self.linear_blockmap[&block.id];
            self.builder.switch_to_block(clif_block);
            self.lower_linear_block(procedure, block, false);
        }
    }

    fn lower_linear_block(
        &mut self,
        procedure: &Procedure<'gc>,
        block: &LinearBlock<'gc>,
        is_entry: bool,
    ) {
        self.set_debug_loc(block.source);
        if is_entry {
            self.bind_linear_var(procedure.binding, VarDef::Value(self.rator));
        } else {
            let clif_block = self.linear_blockmap[&block.id];
            let params = self.builder.block_params(clif_block).to_vec();
            for (var, value) in block.params.iter().copied().zip(params.iter().copied()) {
                self.bind_linear_var(var, VarDef::Value(value));
            }
        }

        for instruction in &block.instructions {
            self.linear_instruction(instruction);
        }

        self.linear_terminator(procedure, &block.terminator);
    }

    fn bind_linear_var(&mut self, var: ValueId, def: VarDef) {
        self.linear_variables.insert(var, def);
        if let (Some(source), VarDef::Value(value)) = (self.linear_value_source(var), def) {
            let label = self
                .func_debug_cx
                .add_linear_variable(var, source, self.srcloc);
            self.builder.set_val_label(value, label);
        }
    }

    fn linear_value_source(&self, var: ValueId) -> Option<ValueSource<'gc>> {
        match &self.target {
            JitTarget::Procedure(procedure) => procedure.sources.get(&var).copied(),
            JitTarget::Func(_) | JitTarget::Cont(_) => None,
        }
    }

    fn comparison_to_value(&mut self, val: ir::Value) -> ir::Value {
        let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
        let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);
        self.builder.ins().select(val, true_, false_)
    }

    fn linear_var(&mut self, var: ValueId) -> ir::Value {
        match *self
            .linear_variables
            .get(&var)
            .unwrap_or_else(|| panic!("linear variable {var:?} not found"))
        {
            VarDef::Value(value) => value,
            VarDef::Comparison(value) => self.comparison_to_value(value),
        }
    }

    fn linear_rest_source(&self, rest: ValueId) -> LinearRestSource {
        *self
            .linear_rest_sources
            .get(&rest)
            .unwrap_or_else(|| panic!("linear rest source {rest:?} not found"))
    }

    fn linear_rest_suffix_len(&mut self, rest: ValueId, skip: usize) -> ir::Value {
        let source = self.linear_rest_source(rest);
        self.builder
            .ins()
            .iadd_imm(source.num_rands, -((source.fixed_count + skip) as i64))
    }

    fn linear_fixnum_from_usize_value(&mut self, value: ir::Value) -> ir::Value {
        let value = self.ireduce(types::I32, value);
        let value = self.zextend(types::I64, value);
        self.builder.ins().bor_imm(value, Value::NUMBER_TAG as i64)
    }

    fn linear_atom(&mut self, atom: LinearAtom<'gc>) -> ir::Value {
        match atom {
            LinearAtom::Constant(value) => self.atom(Atom::Constant(value)),
            LinearAtom::Local(var) => self.linear_var(var),
        }
    }

    fn linear_atom_for_cond(&mut self, atom: LinearAtom<'gc>) -> ir::Value {
        match atom {
            LinearAtom::Local(var) => match self.linear_variables.get(&var).copied() {
                Some(VarDef::Comparison(val)) => val,
                _ => {
                    let val = self.linear_var(var);
                    self.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
                }
            },
            LinearAtom::Constant(_) => {
                let val = self.linear_atom(atom);
                self.builder
                    .ins()
                    .icmp_imm(IntCC::NotEqual, val, Value::VALUE_FALSE)
            }
        }
    }

    fn linear_atom_as_term_atom(&mut self, atom: LinearAtom<'gc>) -> Atom<'gc> {
        match atom {
            LinearAtom::Constant(value) => Atom::Constant(value),
            LinearAtom::Local(var) => {
                let alias = if let Some(alias) = self.synthetic_aliases.get(&var).copied() {
                    alias
                } else {
                    let name = self
                        .linear_value_source(var)
                        .map(|source| source.name)
                        .unwrap_or_else(|| {
                            Symbol::from_str(self.module_builder.ctx, "linear-synthetic").into()
                        });
                    let alias = fresh_lvar(self.module_builder.ctx, name);
                    self.synthetic_aliases.insert(var, alias);
                    alias
                };
                let def = *self
                    .linear_variables
                    .get(&var)
                    .unwrap_or_else(|| panic!("linear variable {var:?} not found"));
                self.variables.insert(alias, def);
                Atom::Local(alias)
            }
        }
    }

    fn code_block_data(&self, code: CodeId) -> (u32, u32) {
        self.module_builder.code_block_for_code[&code]
    }

    fn observe_code_ref(&mut self, code: CodeId) {
        if let JitTarget::Procedure(procedure) = &self.target
            && procedure.code == code
        {
            return;
        }

        if !self.observed_code_refs.contains(&code) {
            self.observed_code_refs.push(code);
        }
    }

    fn linear_instruction(&mut self, instruction: &Instruction<'gc>) {
        match instruction {
            Instruction::Const { dst, value } => {
                let value = self.atom(Atom::Constant(*value));
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::MakeClosure {
                dst,
                code,
                kind,
                free_count,
            } => {
                self.observe_code_ref(*code);
                let nfree = self.builder.ins().iconst(types::I64, *free_count as i64);
                let is_cont = self.builder.ins().iconst(
                    types::I8,
                    match kind {
                        ClosureKind::Function => 0,
                        ClosureKind::Continuation => 1,
                    },
                );
                let code_block = self.load_data_value(self.code_block_data(*code));
                let ctx = self.builder.ins().get_pinned_reg(types::I64);

                let clos = self
                    .builder
                    .ins()
                    .call(self.thunks.make_closure, &[ctx, code_block, nfree, is_cont]);
                let clos = self.builder.inst_results(clos)[0];
                self.bind_linear_var(*dst, VarDef::Value(clos));
            }
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => {
                let closure = self.linear_atom(*closure);
                let value = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    closure,
                    Closure::DATA_OFFSET as i32 + (*index * 8) as i32,
                );
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => {
                let closure = self.linear_atom(*closure);
                let value = self.linear_atom(*value);
                self.builder.ins().store(
                    ir::MemFlags::trusted().with_can_move(),
                    value,
                    closure,
                    Closure::DATA_OFFSET as i32 + (*index * 8) as i32,
                );
            }
            Instruction::CacheRef {
                dst,
                cache_key,
                source,
            } => {
                self.set_debug_loc(*source);
                let LinearAtom::Constant(cache_key) = cache_key else {
                    panic!("invalid cache-ref: expected constant cache key, got {cache_key:?}");
                };
                let cell = self.module_builder.intern_cache_cell(*cache_key);
                let cell = self.import_data(cell);
                let addr = self.builder.ins().global_value(types::I64, cell);
                let value = self
                    .builder
                    .ins()
                    .load(types::I64, ir::MemFlags::new(), addr, 0);
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::CacheSet {
                dst,
                cache_key,
                value,
                source,
            } => {
                self.set_debug_loc(*source);
                let LinearAtom::Constant(cache_key) = cache_key else {
                    panic!("invalid cache-set!: expected constant cache key, got {cache_key:?}");
                };
                let value = self.linear_atom(*value);
                let cell = self.module_builder.intern_cache_cell(*cache_key);
                let cell = self.import_data(cell);
                let addr = self.builder.ins().global_value(types::I64, cell);
                self.builder
                    .ins()
                    .store(ir::MemFlags::new(), value, addr, 0);
                let undefined = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::undefined().bits() as i64);
                self.bind_linear_var(*dst, VarDef::Value(undefined));
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                self.set_debug_loc(*source);
                let args = args
                    .iter()
                    .copied()
                    .map(|arg| self.linear_atom_as_term_atom(arg))
                    .collect::<Vec<_>>();
                let prim = JitPrimitive::from_name(prim.name())
                    .unwrap_or_else(|| panic!("undefined JIT primitive: {prim}"));
                let val = match prim.lower(self, &args) {
                    PrimValue::Value(val) => VarDef::Value(val),
                    PrimValue::Comparison(val) => VarDef::Comparison(val),
                };
                self.bind_linear_var(*dst, val);
            }
            Instruction::RestToList { dst, rest, source } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                let from = self
                    .builder
                    .ins()
                    .iconst(types::I64, rest_source.fixed_count as i64);
                let call = self.builder.ins().call(
                    self.thunks.cons_rest,
                    &[ctx, rest_source.rands, rest_source.num_rands, from],
                );
                let list = self.builder.inst_results(call)[0];
                self.bind_linear_var(*dst, VarDef::Value(list));
            }
            Instruction::RestRef {
                dst,
                rest,
                index,
                source,
            } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let value = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    rest_source.rands,
                    ((rest_source.fixed_count + *index) * 8) as i32,
                );
                self.bind_linear_var(*dst, VarDef::Value(value));
            }
            Instruction::RestLength {
                dst,
                rest,
                skip,
                source,
            } => {
                self.set_debug_loc(*source);
                let len = self.linear_rest_suffix_len(*rest, *skip);
                let len = self.linear_fixnum_from_usize_value(len);
                self.bind_linear_var(*dst, VarDef::Value(len));
            }
            Instruction::RestPredicate {
                dst,
                rest,
                predicate,
                skip,
                source,
            } => {
                self.set_debug_loc(*source);
                let rest_source = self.linear_rest_source(*rest);
                let threshold = (rest_source.fixed_count + *skip) as i64;
                let val = match predicate {
                    RestPredicate::Null => VarDef::Comparison(self.builder.ins().icmp_imm(
                        IntCC::Equal,
                        rest_source.num_rands,
                        threshold,
                    )),
                    RestPredicate::Pair => VarDef::Comparison(self.builder.ins().icmp_imm(
                        IntCC::UnsignedGreaterThan,
                        rest_source.num_rands,
                        threshold,
                    )),
                    RestPredicate::List => {
                        let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                        VarDef::Value(true_)
                    }
                };
                self.bind_linear_var(*dst, val);
            }
        }
    }

    fn get_callee_linear(&mut self, callee: LinearAtom<'gc>) -> Callee {
        if let LinearAtom::Local(var_id) = callee
            && self.is_linear_self_reference(var_id)
        {
            return Callee::SelfRec(self.entry_block);
        }

        let callee = self.linear_atom(callee);
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let get_clos_code = self.builder.create_block();
        let error = self.builder.create_block();
        self.builder.func.layout.set_cold(error);
        self.branch_if_has_typ8(callee, Closure::TC8.bits(), get_clos_code, &[], error, &[]);
        self.builder.switch_to_block(error);
        {
            let err = self
                .builder
                .ins()
                .call(self.thunks.non_applicable, &[ctx, callee]);
            let err = self.builder.inst_results(err)[0];
            self.raise_to_exception_handler(err);
        }

        self.builder.switch_to_block(get_clos_code);
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            callee,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure: callee,
        }
    }

    fn get_tail_callee_linear(&mut self, callee: LinearAtom<'gc>) -> Callee {
        let closure = self.linear_atom(callee);
        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            closure,
            offset_of!(Closure, code) as i32,
        );
        Callee::Indirect {
            target: code,
            closure,
        }
    }

    fn linear_call(
        &mut self,
        callee: LinearAtom<'gc>,
        retk: LinearAtom<'gc>,
        args: &[LinearAtom<'gc>],
        source: Value<'gc>,
    ) {
        self.set_debug_loc(source);
        let num_rands = self
            .builder
            .ins()
            .iconst(types::I64, (args.len() + 1) as i64);
        let callee = self.get_callee_linear(callee);
        let retk = self.linear_atom(retk);
        let rands = std::iter::once(retk)
            .chain(args.iter().copied().map(|arg| self.linear_atom(arg)))
            .collect::<Vec<_>>();
        let rands = self.push_args(&rands);

        if self.module_builder.stacktraces {
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let src_info = self.atom(Atom::Constant(source));
            let rator = self.closure_from_callee(callee);

            self.builder.ins().call(
                self.thunks.push_dframe,
                &[ctx, src_info, rator, num_rands, rands],
            );
        }

        self.emit_callee_jump(callee, rands, num_rands);
    }

    fn linear_tail_call(
        &mut self,
        callee: LinearAtom<'gc>,
        args: &[LinearAtom<'gc>],
        source: Value<'gc>,
    ) {
        self.set_debug_loc(source);
        let callee = self.get_tail_callee_linear(callee);
        let args = args
            .iter()
            .copied()
            .map(|arg| self.linear_atom(arg))
            .collect::<Vec<_>>();
        let rands = self.push_args(&args);
        let num_rands = self.builder.ins().iconst(types::I64, args.len() as i64);
        self.emit_callee_jump(callee, rands, num_rands);
    }

    fn emit_callee_jump(&mut self, callee: Callee, rands: ir::Value, num_rands: ir::Value) {
        match callee {
            Callee::Indirect { target, closure } => {
                self.builder.ins().jump(
                    self.exit_block,
                    &[
                        BlockArg::Value(target),
                        BlockArg::Value(closure),
                        BlockArg::Value(rands),
                        BlockArg::Value(num_rands),
                    ],
                );
            }
            Callee::Direct { target, closure } => {
                self.builder
                    .ins()
                    .return_call(target, &[closure, rands, num_rands]);
            }
            Callee::SelfRec(block) => {
                self.builder.ins().jump(
                    block,
                    &[
                        BlockArg::Value(self.rator),
                        BlockArg::Value(rands),
                        BlockArg::Value(num_rands),
                    ],
                );
            }
        }
    }

    fn jump_to_linear_block(
        &mut self,
        procedure: &Procedure<'gc>,
        target: crate::cps::linear::BlockId,
        args: &[LinearAtom<'gc>],
    ) {
        let block = procedure
            .blocks
            .iter()
            .find(|block| block.id == target)
            .expect("linear jump target should exist");
        let fixed_count = if block.variadic.is_some() {
            block.params.len() - 1
        } else {
            block.params.len()
        };
        if args.len() < fixed_count {
            self.raise_wrong_linear_block_arity(args, -(fixed_count as isize));
            return;
        }

        let mut block_args = args[..fixed_count]
            .iter()
            .copied()
            .map(|arg| BlockArg::Value(self.linear_atom(arg)))
            .collect::<Vec<_>>();

        if let Some(variadic) = block.variadic {
            let variadic_is_referenced = procedure
                .sources
                .get(&variadic)
                .is_some_and(|source| source.is_referenced());
            if variadic_is_referenced {
                let mut ls = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);
                let ctx = self.builder.ins().get_pinned_reg(types::I64);
                for arg in args[fixed_count..].iter().rev().copied() {
                    let arg = self.linear_atom(arg);
                    let call = self.builder.ins().call(self.thunks.cons, &[ctx, arg, ls]);
                    ls = self.builder.inst_results(call)[0];
                }
                block_args.push(BlockArg::Value(ls));
            } else {
                let null = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::null().bits() as i64);
                block_args.push(BlockArg::Value(null));
            }
        } else if args.len() != fixed_count {
            self.raise_wrong_linear_block_arity(args, fixed_count as isize);
            return;
        }

        let clif_block = self.linear_blockmap[&target];
        self.builder.ins().jump(clif_block, &block_args);
    }

    fn raise_wrong_linear_block_arity(&mut self, args: &[LinearAtom<'gc>], expected: isize) {
        let args = args
            .iter()
            .copied()
            .map(|arg| self.linear_atom(arg))
            .collect::<Vec<_>>();
        let rands = self.push_args(&args);
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let got = self.builder.ins().iconst(types::I64, args.len() as i64);
        let expected = self.builder.ins().iconst(types::I64, expected as i64);
        let err = self.builder.ins().call(
            self.thunks.wrong_number_of_args,
            &[ctx, self.rator, got, expected, rands],
        );
        let err = self.builder.inst_results(err)[0];
        self.raise_to_exception_handler(err);
    }

    fn linear_branch_target(&mut self, procedure: &Procedure<'gc>, target: &BranchTarget<'gc>) {
        match target {
            BranchTarget::Local { block, args } => {
                self.jump_to_linear_block(procedure, *block, args)
            }
            BranchTarget::Reified { continuation, args } => {
                self.linear_tail_call(*continuation, args, Value::new(false));
            }
        }
    }

    fn linear_terminator(&mut self, procedure: &Procedure<'gc>, terminator: &Terminator<'gc>) {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => self.linear_call(*callee, *retk, args, *source),
            Terminator::TailCall {
                callee,
                args,
                source,
            } => self.linear_tail_call(*callee, args, *source),
            Terminator::Jump { target, args } => {
                self.jump_to_linear_block(procedure, *target, args);
            }
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints: _,
            } => {
                let truthy = self.linear_atom_for_cond(*test);
                let kcons = self.builder.create_block();
                let kalt = self.builder.create_block();

                self.builder.ins().brif(truthy, kcons, &[], kalt, &[]);
                self.builder.switch_to_block(kalt);
                self.linear_branch_target(procedure, alternative);

                self.builder.switch_to_block(kcons);
                self.linear_branch_target(procedure, consequent);
            }
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => self.linear_switch(procedure, *kind, *scrutinee, cases, default),
        }
    }

    fn linear_switch(
        &mut self,
        procedure: &Procedure<'gc>,
        kind: SwitchKind,
        scrutinee: LinearAtom<'gc>,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        default: &BranchTarget<'gc>,
    ) {
        let value = self.linear_atom(scrutinee);
        let switch_block = self.builder.create_block();
        let switch_default_block = self.builder.create_block();
        let type_miss_block = if matches!(
            kind,
            SwitchKind::Eq | SwitchKind::CharEq | SwitchKind::SymbolEq { .. }
        ) {
            switch_default_block
        } else {
            self.builder.create_block()
        };
        let mut case_blocks: Vec<(ir::Block, Vec<&crate::cps::linear::SwitchCase<'gc>>)> =
            Vec::with_capacity(cases.len());
        let mut case_block_by_key: HashMap<u128, usize> = HashMap::new();
        let mut switch = Switch::new();

        let switch_value = match kind {
            SwitchKind::Char | SwitchKind::CharEq => {
                let mask = self
                    .builder
                    .ins()
                    .iconst(types::I64, Value::CHAR_MASK as i64);
                let tag = self.builder.ins().iconst(types::I64, Value::CHAR_TAG);
                let masked = self.builder.ins().band(value, mask);
                let is_char = self.builder.ins().icmp(IntCC::Equal, masked, tag);
                self.builder
                    .ins()
                    .brif(is_char, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                let value = self.builder.ins().ushr_imm(value, 16);
                self.builder.ins().ireduce(types::I32, value)
            }
            SwitchKind::Eq | SwitchKind::Fixnum | SwitchKind::Numeric => {
                let is_fixnum = self.is_int32(value);
                self.builder
                    .ins()
                    .brif(is_fixnum, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                self.builder.ins().ireduce(types::I32, value)
            }
            SwitchKind::SymbolEq { mask } => {
                let is_symbol = self.has_typ8(value, TypeCode8::SYMBOL.bits());
                self.builder
                    .ins()
                    .brif(is_symbol, switch_block, &[], type_miss_block, &[]);
                self.builder.switch_to_block(switch_block);
                let hash = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    value,
                    offset_of!(Symbol, hash) as i32,
                );
                if mask == u64::MAX {
                    hash
                } else {
                    self.builder.ins().band_imm(hash, mask as i64)
                }
            }
        };

        for case in cases {
            let key = switch_case_key(case.value);
            if let Some(index) = case_block_by_key.get(&key).copied() {
                case_blocks[index].1.push(case);
            } else {
                let block = self.builder.create_block();
                switch.set_entry(key, block);
                case_block_by_key.insert(key, case_blocks.len());
                case_blocks.push((block, vec![case]));
            }
        }

        switch.emit(&mut self.builder, switch_value, switch_default_block);

        self.builder.switch_to_block(switch_default_block);
        self.linear_branch_target(procedure, default);

        if type_miss_block != switch_default_block {
            self.builder.switch_to_block(type_miss_block);
        }
        match kind {
            SwitchKind::Eq | SwitchKind::CharEq | SwitchKind::SymbolEq { .. } => {}
            SwitchKind::Fixnum => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_fixnum_switch_error_or_default(scrutinee, cases, procedure, default)
            }
            SwitchKind::Numeric => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_numeric_switch_fallback(procedure, scrutinee, cases, default)
            }
            SwitchKind::Char => {
                let scrutinee = self.linear_atom(scrutinee);
                self.linear_char_switch_error_or_default(procedure, scrutinee, default)
            }
        }

        for (block, cases) in case_blocks {
            self.builder.switch_to_block(block);
            if matches!(kind, SwitchKind::SymbolEq { .. }) {
                for (index, case) in cases.iter().enumerate() {
                    let matched_block = self.builder.create_block();
                    let next_block = if index + 1 == cases.len() {
                        switch_default_block
                    } else {
                        self.builder.create_block()
                    };
                    let SwitchCaseValue::Symbol {
                        value: symbol_value,
                        ..
                    } = case.value
                    else {
                        continue;
                    };
                    let symbol = self.atom(Atom::Constant(symbol_value));
                    let matched = self.builder.ins().icmp(IntCC::Equal, value, symbol);
                    self.builder
                        .ins()
                        .brif(matched, matched_block, &[], next_block, &[]);
                    self.builder.switch_to_block(matched_block);
                    self.linear_branch_target(procedure, &case.target);
                    if index + 1 != cases.len() {
                        self.builder.switch_to_block(next_block);
                    }
                }
            } else {
                let Some(case) = cases.first() else {
                    self.linear_branch_target(procedure, default);
                    continue;
                };
                self.linear_branch_target(procedure, &case.target);
            }
        }
    }

    fn linear_fixnum_switch_error_or_default(
        &mut self,
        scrutinee: ir::Value,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        procedure: &Procedure<'gc>,
        default: &BranchTarget<'gc>,
    ) {
        let Some(case) = cases.first() else {
            self.linear_branch_target(procedure, default);
            return;
        };
        let SwitchCaseValue::Integer(value) = case.value else {
            self.linear_branch_target(procedure, default);
            return;
        };
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let constant = self.atom(Atom::Constant(Value::new(value)));
        let _ = self.handle_thunk_call_result(self.thunks.fxeq, &[ctx, scrutinee, constant]);
        self.linear_branch_target(procedure, default);
    }

    fn linear_char_switch_error_or_default(
        &mut self,
        procedure: &Procedure<'gc>,
        scrutinee: ir::Value,
        default: &BranchTarget<'gc>,
    ) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let _ = self.handle_thunk_call_result(self.thunks.char_to_integer, &[ctx, scrutinee]);
        self.linear_branch_target(procedure, default);
    }

    fn linear_numeric_switch_fallback(
        &mut self,
        procedure: &Procedure<'gc>,
        scrutinee: ir::Value,
        cases: &[crate::cps::linear::SwitchCase<'gc>],
        default: &BranchTarget<'gc>,
    ) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        for case in cases {
            let SwitchCaseValue::Integer(value) = case.value else {
                continue;
            };
            let next = self.builder.create_block();
            let matched = self.builder.create_block();
            let constant = self.atom(Atom::Constant(Value::new(value)));
            let result =
                self.handle_thunk_call_result(self.thunks.number_eq, &[ctx, scrutinee, constant]);
            let truthy = self.to_boolean(result);
            self.builder.ins().brif(truthy, matched, &[], next, &[]);
            self.builder.switch_to_block(matched);
            self.linear_branch_target(procedure, &case.target);
            self.builder.switch_to_block(next);
        }
        self.linear_branch_target(procedure, default);
    }

    pub fn finalize(&mut self) {
        while let Some(cont) = self.to_generate.pop() {
            let block = self.block_for_cont(cont);
            self.builder.switch_to_block(block);
            self.set_debug_loc(cont.source());
            self.term(cont.body());
        }

        self.builder.switch_to_block(self.exit_block);

        let code = self.builder.block_params(self.exit_block)[0];
        let rator = self.builder.block_params(self.exit_block)[1];
        let rands = self.builder.block_params(self.exit_block)[2];
        let num_rands = self.builder.block_params(self.exit_block)[3];

        self.builder
            .ins()
            .return_call_indirect(self.sig_call, code, &[rator, rands, num_rands]);

        self.builder.seal_all_blocks();
    }

    pub fn set_debug_loc(&mut self, loc: Value<'gc>) {
        if loc != Value::new(false) {
            let (file_id, line, column) = self.module_builder.debug_context.get_span_loc(loc);

            let source_loc = self.func_debug_cx.add_dbg_loc(file_id, line, column);
            self.builder.set_srcloc(source_loc);
            self.srcloc = Some(source_loc);
        }
    }

    pub fn term(&mut self, term: TermRef<'gc>) {
        let src = term.source();
        self.set_debug_loc(src);
        match &*term {
            Term::Let(var, expr, next) => {
                match expr {
                    Expression::PrimCall(prim, args, _) => {
                        let Some(prim) = self.module_builder.prims.primitive(*prim) else {
                            panic!("undefined primitive: {prim}")
                        };

                        let val = match prim.lower(self, args) {
                            PrimValue::Value(val) => VarDef::Value(val),
                            PrimValue::Comparison(val) => VarDef::Comparison(val),
                        };
                        self.variables.insert(*var, val);
                    }
                }
                self.term(*next);
            }

            Term::Letk(conts, next) => {
                self.letk(conts);
                self.term(*next);
            }

            Term::Fix(funs, next) => {
                self.fix(funs);
                self.term(*next);
            }

            Term::App(rator, retk, rands, _) => {
                let num_rands = rands.len() + 1;
                let num_rands = self.builder.ins().iconst(types::I64, num_rands as i64);

                let callee = self.get_callee(rator);

                let retk = self.var(*retk);

                let rands = [retk]
                    .into_iter()
                    .chain(rands.iter().copied().map(|a| self.atom(a)))
                    .collect::<Vec<_>>();

                let rands = self.push_args(&rands);

                if self.module_builder.stacktraces {
                    /* do `(with-continuation-mark <stacktrace-key> <info> app)` */
                    let ctx = self.builder.ins().get_pinned_reg(types::I64);

                    let src_info = self.atom(Atom::Constant(src));
                    let rator = self.closure_from_callee(callee);

                    self.builder.ins().call(
                        self.thunks.push_dframe,
                        &[ctx, src_info, rator, num_rands, rands],
                    );
                }

                match callee {
                    Callee::Indirect { target, closure } => {
                        self.builder.ins().jump(
                            self.exit_block,
                            &[
                                BlockArg::Value(target),
                                BlockArg::Value(closure),
                                BlockArg::Value(rands),
                                BlockArg::Value(num_rands),
                            ],
                        );
                    }

                    Callee::Direct { target, closure } => {
                        self.builder
                            .ins()
                            .return_call(target, &[closure, rands, num_rands]);
                    }

                    Callee::SelfRec(block) => {
                        // just jump back to entrypoint
                        let block_args = [self.rator, rands, num_rands]
                            .into_iter()
                            .map(|v| ir::BlockArg::Value(v))
                            .collect::<Vec<_>>();
                        self.builder.ins().jump(block, &block_args);
                    }
                }
            }

            Term::Continue(k, rands, _) => {
                let rands = rands.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();

                self.continue_to(*k, &rands);
            }

            Term::If {
                test,
                consequent,
                consequent_args,
                alternative,
                alternative_args,
                hints: _,
            } => {
                let truthy = self.atom_for_cond(*test);

                let kcons = self.builder.create_block();
                let kalt = self.builder.create_block();

                self.builder.ins().brif(truthy, kcons, &[], kalt, &[]);
                self.builder.switch_to_block(kalt);
                {
                    let alternative_args = alternative_args.map_or(vec![], |args| {
                        args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>()
                    });
                    self.continue_to(*alternative, &alternative_args);
                }

                self.builder.switch_to_block(kcons);
                {
                    let consequent_args = consequent_args.map_or(vec![], |args| {
                        args.iter().map(|a| self.atom(*a)).collect::<Vec<_>>()
                    });
                    self.continue_to(*consequent, &consequent_args);
                }
            }
        }
    }

    pub fn maybe_known_function(&mut self, rator: Atom<'gc>) -> Option<ir::FuncRef> {
        if !self.module_builder.direct_calls {
            return None;
        }

        let Atom::Local(var) = rator else {
            return None;
        };

        let func = self.module_builder.known_func(var)?;
        let name_pair = self
            .module_builder
            .func_for_func
            .get(&func)
            .expect("must be defined");

        Some(self.declare_func_in_func(*name_pair))
    }

    pub fn check_yield(&mut self, rator: ir::Value, rands: ir::Value, num_rands: ir::Value) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let thread = ctx;

        let yieldpoint = self.builder.ins().load(
            types::I32,
            ir::MemFlags::trusted().with_can_move(),
            thread,
            Thread::TAKE_YIELDPOINT_OFFSET as i32,
        );
        let on_yieldpoint = self.builder.create_block();
        let on_no_yieldpoint = self.builder.create_block();
        self.builder.func.layout.set_cold(on_yieldpoint);

        let take_yieldpoint = self.builder.ins().icmp_imm(IntCC::NotEqual, yieldpoint, 0);

        self.builder
            .ins()
            .brif(take_yieldpoint, on_yieldpoint, &[], on_no_yieldpoint, &[]);
        self.builder.switch_to_block(on_yieldpoint);
        {
            let ctx = self.builder.ins().get_pinned_reg(types::I64);

            let nest_level = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                ctx,
                (Context::OFFSET_OF_STATE + offset_of!(State, nest_level)) as i32,
            );

            let is_nested = self
                .builder
                .ins()
                .icmp_imm(IntCC::UnsignedGreaterThan, nest_level, 1);
            let on_not_nested = self.builder.create_block();
            self.builder
                .ins()
                .brif(is_nested, on_no_yieldpoint, &[], on_not_nested, &[]);
            self.builder.switch_to_block(on_not_nested);
            self.builder.func.layout.set_cold(on_not_nested);
            let saved_call = self
                .builder
                .ins()
                .call(self.thunks.yieldpoint, &[ctx, rator, rands, num_rands]);
            let code = self
                .builder
                .ins()
                .iconst(types::I64, ReturnCode::Yield as i64);
            let val = self.builder.inst_results(saved_call)[0];
            self.builder.ins().return_(&[code, val]);
        }
        self.builder.switch_to_block(on_no_yieldpoint);
    }
}

fn switch_case_key(value: SwitchCaseValue<'_>) -> u128 {
    match value {
        SwitchCaseValue::Integer(value) => value as u32 as u128,
        SwitchCaseValue::Symbol { hash, .. } => hash as u128,
    }
}

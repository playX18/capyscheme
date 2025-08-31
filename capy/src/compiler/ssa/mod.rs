use crate::{
    compiler::ssa::primitive::{PrimValue, PrimitiveLowerer},
    cps::{
        ReifyInfo,
        term::{Atom, ContRef, Expression, FuncRef, Term, TermRef},
    },
    expander::{compile_program, core::LVarRef, read_from_string},
    runtime::{
        Context, State,
        fasl::FASLWriter,
        modules::root_module,
        value::{
            Closure, ReturnCode, Str, Symbol, Tagged, TypeCode8, TypeCode16, Value, ValueEqual,
            Vector,
        },
    },
};
use cranelift::prelude::{
    Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, types,
};
use cranelift_codegen::{
    ir::{self, BlockArg},
    isa::CallConv,
    settings,
};

use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectModule, ObjectProduct};
use rsgc::{Gc, Mutation, sync::thread::Thread};
use std::{collections::HashMap, mem::offset_of, path::Path};

use crate::runtime::vm::thunks::*;

pub mod helpers;
pub mod primitive;
pub mod traits;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDef {
    Value(ir::Value),
    Comparison(ir::Value),
    Free(usize),
}

/// A SSA Builder. Constructs Cranelift module and SSA from single compilation unit.
pub struct ModuleBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub reify_info: ReifyInfo<'gc>,
    pub constants: HashMap<ValueEqual<'gc>, DataId>,
    pub cache_cells: HashMap<ValueEqual<'gc>, DataId>,

    pub module: ObjectModule,

    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_cont: HashMap<ContRef<'gc>, FuncId>,
    pub func_for_func: HashMap<FuncRef<'gc>, FuncId>,
    pub import_data: HashMap<&'static str, DataId>,
    pub thunks: Thunks,
}

impl<'gc> ModuleBuilder<'gc> {
    pub fn new(ctx: Context<'gc>, mut module: ObjectModule, reify_info: ReifyInfo<'gc>) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let thunks = Thunks::new(&mut module);
        Self {
            ctx,
            reify_info,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            module,
            import_data: HashMap::new(),
            prims,
            func_for_cont: HashMap::new(),
            func_for_func: HashMap::new(),
            thunks,
        }
    }
    pub fn compile(&mut self) {
        let sig = call_signature!(Tail (I64 /* rator */, I64 /* rands */, I64 /* num_rands */) -> (I64, I64));
        for (i, &func) in self.reify_info.functions.iter().enumerate() {
            let name = format!("fn{}:{}", i, func.name);
            let func_id = self
                .module
                .declare_function(&name, Linkage::Local, &sig)
                .unwrap();

            self.func_for_func.insert(func, func_id);
        }

        let sig = call_signature!(Tail (I64 /* rator */, I64 /* rands */, I64 /* num_rands */) -> (I64, I64));

        for (i, &cont) in self
            .reify_info
            .continuations
            .iter()
            .filter(|c| c.reified.get())
            .enumerate()
        {
            let name = format!("cont{}:{}", i, cont.binding.name);
            let cont_id = self
                .module
                .declare_function(&name, Linkage::Local, &sig)
                .unwrap();
            self.func_for_cont.insert(cont, cont_id);
        }

        let mut context = self.module.make_context();
        let mut fctx = FunctionBuilderContext::new();
        let funcs = self.reify_info.functions;
        for &func in funcs.iter() {
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(&self.thunks, &mut builder.func, &mut self.module);
            let mut ssa = SSABuilder::new(self, builder, ContOrFunc::Func(func), thunks);

            ssa.term(func.body);
            ssa.finalize();

            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();

            let func_id = self.func_for_func[&func];

            self.module.define_function(func_id, &mut context).unwrap();
            self.module.clear_context(&mut context);
        }

        let conts = self.reify_info.continuations;

        for &cont in conts.iter().filter(|c| c.reified.get()) {
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(&self.thunks, &mut builder.func, &mut self.module);
            let mut ssa = SSABuilder::new(self, builder, ContOrFunc::Cont(cont), thunks);

            ssa.term(cont.body);
            ssa.finalize();

            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();

            let func_id = self.func_for_cont[&cont];

            self.module.define_function(func_id, &mut context).unwrap();
            self.module.clear_context(&mut context);
        }

        self.initialize_constants(&mut context, &mut fctx);
    }

    /// Generate code to initialize constants.
    ///
    /// This function will emit FASL vector of all constants and at startup
    /// it will be deserialized.
    fn initialize_constants(
        &mut self,
        ctx: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        let globals_array = self
            .module
            .declare_data("CAPY_GLOBALS", Linkage::Export, true, false)
            .unwrap();
        let globals_size = self
            .module
            .declare_data("CAPY_GLOBALS_LEN", Linkage::Export, false, false)
            .unwrap();

        let mut desc = DataDescription::new();
        desc.set_align(size_of::<usize>() as _);
        let datas = self
            .constants
            .values()
            .copied()
            .chain(self.cache_cells.values().copied());
        let mut offset = 0;
        for data in datas {
            let gv = self.module.declare_data_in_data(data, &mut desc);

            desc.write_data_addr(offset, gv, 0);
            offset += 8;
        }
        desc.define(vec![0; offset as usize].into_boxed_slice());

        self.module.define_data(globals_array, &desc).unwrap();

        desc = DataDescription::new();

        desc.define(Box::new((offset / 8).to_le_bytes()));
        desc.set_align(size_of::<usize>() as _);
        self.module.define_data(globals_size, &desc).unwrap();

        // First let's get some stable way to iterate constants
        let constants = self
            .constants
            .iter()
            .map(|(key, id)| (key.0, *id))
            .collect::<Vec<_>>();

        let vec = Vector::new::<true>(&self.ctx, constants.len(), Value::undefined());
        let wvec = Gc::write(&self.ctx, vec);
        for (i, (val, _)) in constants.iter().enumerate() {
            wvec[i].unlock().set(*val);
        }

        let mut buf = Vec::with_capacity(1024);
        let writer = FASLWriter::new(self.ctx, &mut buf);
        writer.write(vec.into()).expect("should not fail");

        let sig = call_signature!(SystemV(I64) -> I64);
        let init_fn_id = self
            .module
            .declare_function("capy_module_init", Linkage::Export, &sig)
            .unwrap();

        let fasl_data = self
            .module
            .declare_data("capy_fasl_constants", Linkage::Export, false, false)
            .unwrap();
        let mut desc = DataDescription::new();

        let size = buf.len();
        desc.set_align(align_of::<usize>() as _);
        desc.define(buf.into_boxed_slice());
        self.module.define_data(fasl_data, &desc).unwrap();
        {
            ctx.func.signature = sig;
            let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);

            let ctx = builder.block_params(entry)[0];
            let fasl_data = self
                .module
                .declare_data_in_func(fasl_data, &mut builder.func);
            let addr = builder.ins().global_value(types::I64, fasl_data);

            let size = builder.ins().iconst(types::I64, size as i64);
            let fasl_read_nofail = self
                .module
                .declare_func_in_func(self.thunks.fasl_read_nofail, &mut builder.func);
            let call = builder.ins().call(fasl_read_nofail, &[ctx, addr, size]);
            let fvec = builder.inst_results(call)[0];

            for (i, (_, data_id)) in constants.iter().enumerate() {
                let data_gv = self
                    .module
                    .declare_data_in_func(*data_id, &mut builder.func);
                let addr = builder.ins().global_value(types::I64, data_gv);
                let value = builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    fvec,
                    Vector::OFFSET_OF_DATA as i32 + (i * size_of::<Value>()) as i32,
                );
                builder
                    .ins()
                    .store(ir::MemFlags::trusted().with_can_move(), value, addr, 0);
            }

            let entrypoint_fn_id = self.func_for_func[&self.reify_info.entrypoint];
            let entrypoint_fn_ref = self
                .module
                .declare_func_in_func(entrypoint_fn_id, &mut builder.func);

            let nfree = builder.ins().iconst(types::I64, 0);
            let is_cont = builder.ins().iconst(types::I8, 0);

            let make_closure = self
                .module
                .declare_func_in_func(self.thunks.make_closure, &mut builder.func);
            let entrypoint_fn_ref = builder.ins().func_addr(types::I64, entrypoint_fn_ref);
            let call = builder
                .ins()
                .call(make_closure, &[ctx, entrypoint_fn_ref, nfree, is_cont]);
            let clos = builder.inst_results(call)[0];

            builder.ins().return_(&[clos]);
            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module.define_function(init_fn_id, ctx).unwrap();
    }

    /// Add an object to constant table, and return a data ID that can be used
    /// to reference it. If object is already present in constant table, returns
    /// the existing data ID.
    ///
    /// If object is immediate, no data ID is returned.
    pub fn intern_constant(&mut self, obj: Value<'gc>) -> Option<DataId> {
        if obj.is_immediate() {
            return None;
        }

        if let Some(data_id) = self.constants.get(&ValueEqual(obj)) {
            return Some(*data_id);
        }

        let ix = self.constants.len();
        let name = format!("constant{}", ix);
        // declare data as writable even though not all objects need to be written to. We don't currently have a way of knowing
        // ahead of time if constant will be read-only or not.
        let data_id = self
            .module
            .declare_data(&name, Linkage::Local, true, false)
            .unwrap();
        let mut desc = DataDescription::new();
        desc.define_zeroinit(size_of::<Value>());
        desc.set_align(align_of::<usize>() as _);
        self.module.define_data(data_id, &desc).unwrap();

        self.constants.insert(ValueEqual(obj), data_id);

        Some(data_id)
    }

    pub fn intern_cache_cell(&mut self, key: Value<'gc>) -> DataId {
        if let Some(data_id) = self.cache_cells.get(&ValueEqual(key)) {
            return *data_id;
        }

        let ix = self.cache_cells.len();
        let name = format!("cache_cell{}", ix);

        let data_id = self
            .module
            .declare_data(&name, Linkage::Local, true, false)
            .unwrap();
        let mut desc = DataDescription::new();
        desc.define_zeroinit(size_of::<Value>());
        desc.set_align(align_of::<usize>() as _);
        self.module.define_data(data_id, &desc).unwrap();

        self.cache_cells.insert(ValueEqual(key), data_id);

        data_id
    }
}

pub struct SSABuilder<'gc, 'a, 'f> {
    pub module_builder: &'a mut ModuleBuilder<'gc>,
    pub builder: FunctionBuilder<'f>,

    /// Map from non reified continuations to corresponding Cranelift block.
    pub blockmap: HashMap<ContRef<'gc>, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,

    pub target: ContOrFunc<'gc>,
    pub exit_block: ir::Block,

    pub rator: ir::Value,
    pub thunks: ImportedThunks,

    pub sig_call: ir::SigRef,
    pub to_generate: Vec<ContRef<'gc>>,

    pub data_imports: HashMap<DataId, ir::GlobalValue>,
}

#[derive(Clone, Copy)]
pub enum ContOrFunc<'gc> {
    Func(FuncRef<'gc>),
    Cont(ContRef<'gc>),
}

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    pub fn new(
        module_builder: &'a mut ModuleBuilder<'gc>,
        mut builder: FunctionBuilder<'f>,
        target: ContOrFunc<'gc>,
        thunks: ImportedThunks,
    ) -> Self {
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

        builder.append_block_param(exit_block, types::I64); /* code */
        builder.append_block_param(exit_block, types::I64); /* rator */
        builder.append_block_param(exit_block, types::I64); /* rands */
        builder.append_block_param(exit_block, types::I64); /* num_rands */

        let mut this = Self {
            module_builder,
            builder,
            target,
            exit_block,
            rator,

            variables,
            blockmap: HashMap::new(),
            to_generate: Vec::new(),
            thunks,

            sig_call,

            data_imports: HashMap::new(),
        };

        this.entrypoint(rands, num_rands);

        this
    }

    pub fn is_self_reference(&mut self, var: LVarRef<'gc>) -> bool {
        match self.target {
            ContOrFunc::Cont(c) => Gc::ptr_eq(c.binding, var),
            ContOrFunc::Func(f) => Gc::ptr_eq(f.binding, var),
        }
    }

    pub fn import_static(&mut self, name: &'static str, typ: ir::Type) -> ir::Value {
        let data_id = *self
            .module_builder
            .import_data
            .entry(name)
            .or_insert_with(|| {
                self.module_builder
                    .module
                    .declare_data(name, Linkage::Import, false, false)
                    .unwrap()
            });
        let data = self.import_data(data_id);

        self.builder.ins().global_value(typ, data)
    }

    pub fn import_data(&mut self, data: DataId) -> ir::GlobalValue {
        self.data_imports
            .entry(data)
            .or_insert_with(|| {
                let gv = self
                    .module_builder
                    .module
                    .declare_data_in_func(data, &mut self.builder.func);
                gv
            })
            .clone()
    }

    pub fn var(&mut self, var: LVarRef<'gc>) -> ir::Value {
        if self.is_self_reference(var) {
            return self.rator;
        }
        match *self
            .variables
            .get(&var)
            .unwrap_or_else(|| panic!("{}@{:p} var not found", var.name, var))
        {
            VarDef::Comparison(val) => {
                let true_ = self.builder.ins().iconst(types::I64, Value::VALUE_TRUE);
                let false_ = self.builder.ins().iconst(types::I64, Value::VALUE_FALSE);

                self.builder.ins().select(val, true_, false_)
            }
            VarDef::Free(ix) => {
                let free = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    self.rator,
                    offset_of!(Closure, free) as i32,
                );
                self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    free,
                    Vector::OFFSET_OF_DATA as i32 + (ix * 8) as i32,
                )
            }

            VarDef::Value(val) => val,
        }
    }

    pub fn entrypoint(&mut self, rands: ir::Value, num_rands: ir::Value) {
        self.check_yield(self.rator, rands, num_rands);
        self.load_free_vars();
        self.load_arguments(rands, num_rands);
    }

    pub fn default_error_handler(&mut self) -> LVarRef<'gc> {
        match self.target {
            ContOrFunc::Func(func) => func.handler_cont,
            ContOrFunc::Cont(cont) => cont.handler.get(),
        }
    }

    fn load_arguments(&mut self, mut rands: ir::Value, mut num_rands: ir::Value) {
        let (params, rest) = match self.target {
            ContOrFunc::Func(func) => (func.args, func.variadic),
            ContOrFunc::Cont(cont) => (cont.args(), cont.variadic()),
        };

        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let state = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            ctx,
            offset_of!(Context, state) as i32,
        );
        /* reset the runstack to the state it was before call */
        self.builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            rands,
            state,
            offset_of!(State, runstack) as i32,
        );

        if let ContOrFunc::Func(func) = self.target {
            let retk = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                0,
            );
            let reth = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                rands,
                8,
            );

            rands = self.builder.ins().iadd_imm(rands, 16);
            num_rands = self.builder.ins().iadd_imm(num_rands, -2);

            self.variables.insert(func.return_cont, VarDef::Value(retk));
            self.variables
                .insert(func.handler_cont, VarDef::Value(reth));
        }

        if params.len() != 0 {
            for (i, param) in params.iter().enumerate() {
                let value = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    rands,
                    (i * 8) as i32,
                );
                self.variables.insert(*param, VarDef::Value(value));
            }
            if let Some(rest) = rest {
                let not_enough = self.builder.ins().icmp_imm(
                    IntCC::UnsignedLessThan,
                    num_rands,
                    params.len() as i64 - 1,
                );

                let succ = self.builder.create_block();
                let err = self.builder.create_block();
                self.builder.func.layout.set_cold(err);
                self.builder.ins().brif(not_enough, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let err = self
                        .builder
                        .ins()
                        .call(self.thunks.wrong_number_of_args, &[ctx, self.rator]);
                    let err = self.builder.inst_results(err)[0];
                    let error_handler = self.default_error_handler();

                    self.continue_to(error_handler, &[err]);
                }
                self.builder.switch_to_block(succ);
                {
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
                        let from = self.builder.ins().iconst(types::I64, params.len() as i64);
                        let cons = self
                            .builder
                            .ins()
                            .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                        let list = self.builder.inst_results(cons)[0];

                        self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                    }
                    self.builder.switch_to_block(succ);
                    {
                        let ls = self.builder.block_params(succ)[0];
                        self.variables.insert(rest, VarDef::Value(ls));
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
                    let err = self
                        .builder
                        .ins()
                        .call(self.thunks.wrong_number_of_args, &[ctx, self.rator]);
                    let err = self.builder.inst_results(err)[0];
                    let error_handler = self.default_error_handler();

                    self.continue_to(error_handler, &[err]);
                }
                self.builder.switch_to_block(succ);
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
                    let from = self.builder.ins().iconst(types::I64, 0);
                    let cons = self
                        .builder
                        .ins()
                        .call(self.thunks.cons_rest, &[ctx, rands, num_rands, from]);
                    let list = self.builder.inst_results(cons)[0];

                    self.builder.ins().jump(succ, &[BlockArg::Value(list)]);
                }
                self.builder.switch_to_block(succ);
                {
                    let ls = self.builder.block_params(succ)[0];
                    self.variables.insert(rest, VarDef::Value(ls));
                }
            } else {
                let succ = self.builder.create_block();
                let err = self.builder.create_block();

                self.builder.func.layout.set_cold(err);

                let nonzero = self.builder.ins().icmp_imm(IntCC::NotEqual, num_rands, 0);

                self.builder.ins().brif(nonzero, err, &[], succ, &[]);

                self.builder.switch_to_block(err);
                {
                    let handler = self.default_error_handler();
                    let err = self
                        .builder
                        .ins()
                        .call(self.thunks.wrong_number_of_args, &[ctx, num_rands]);
                    let err = self.builder.inst_results(err)[0];
                    self.continue_to(handler, &[err]);
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
        self.blockmap.insert(k, block);
        for _ in k.args.iter() {
            self.builder.append_block_param(block, types::I64);
        }

        for i in 0..k.args.len() {
            self.variables.insert(
                k.args[i],
                VarDef::Value(self.builder.block_params(block)[i]),
            );
        }
        block
    }

    /// Get callee entrypoint or report error to `handler`.
    pub fn get_callee_code(&mut self, callee: ir::Value, handler: LVarRef<'gc>) -> ir::Value {
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
            self.continue_to(handler, &[err]);
        }

        self.builder.switch_to_block(get_clos_code);
        {
            let code = self.builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                callee,
                offset_of!(Closure, code) as i32,
            );
            code
        }
    }

    /// Push arguments to runstack. Returns the base of the arguments array.
    pub fn push_args(&mut self, args: &[ir::Value]) -> ir::Value {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let state = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            ctx,
            offset_of!(Context, state) as i32,
        );
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
        if let Some(k) = self.module_builder.reify_info.free_vars.conts.get(&k)
            && !k.reified.get()
        {
            // TODO: Support `k` where variadic is used and materialize a list.
            let block = self.block_for_cont(*k);
            let block_args = args
                .into_iter()
                .map(|v| ir::BlockArg::Value(*v))
                .collect::<Vec<_>>();

            return self.builder.ins().jump(block, &block_args);
        }

        let k = self.var(k);

        let code = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            k,
            offset_of!(Closure, code) as i32,
        );
        let rands = self.push_args(args);
        let num_rands = self.builder.ins().iconst(types::I64, args.len() as i64);
        /**/
        self.builder.ins().jump(
            self.exit_block,
            &[
                BlockArg::Value(code),
                BlockArg::Value(k),
                BlockArg::Value(rands),
                BlockArg::Value(num_rands),
            ],
        )
    }

    fn load_free_vars(&mut self) {
        let Some(fvs) = (match self.target {
            ContOrFunc::Func(func) => func.free_vars.get(),
            ContOrFunc::Cont(cont) => cont.free_vars.get(),
        }) else {
            return;
        };

        /* load all free-variables of closure.

           We can directly load values without later modifying `free` vector
           as all variables are immutable, mutable variables should've been boxed by optimizing compiler.
        */

        for (i, var) in fvs.iter().copied().enumerate() {
            /*let offset = Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8);
            let fv = self
                .builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), free, offset);
            self.variables.insert(var, fv);*/
            self.variables.insert(var, VarDef::Free(i));
        }
    }

    /* helpers */

    pub fn vector_ref_imm(&mut self, vec: ir::Value, ix: usize) -> ir::Value {
        let offset = Vector::OFFSET_OF_DATA as i32 + (ix as i32 * 8);
        self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            vec,
            offset,
        )
    }

    pub fn vector_set_imm(&mut self, vec: ir::Value, ix: usize, value: ir::Value) {
        let offset = Vector::OFFSET_OF_DATA as i32 + (ix as i32 * 8);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted().with_can_move(), value, vec, offset);
    }

    pub fn vector_ref(&mut self, vec: ir::Value, ix: ir::Value) -> ir::Value {
        let mut offset = self.builder.ins().imul_imm(ix, 8);
        offset = self
            .builder
            .ins()
            .iadd_imm(offset, Vector::OFFSET_OF_DATA as i64);
        let addr = self.builder.ins().iadd(vec, offset);
        self.builder
            .ins()
            .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0)
    }

    pub fn vector_set(&mut self, vec: ir::Value, ix: ir::Value, value: ir::Value) {
        let mut offset = self.builder.ins().imul_imm(ix, 8);
        offset = self
            .builder
            .ins()
            .iadd_imm(offset, Vector::OFFSET_OF_DATA as i64);
        let addr = self.builder.ins().iadd(vec, offset);
        self.builder
            .ins()
            .store(ir::MemFlags::trusted().with_can_move(), value, addr, 0);
    }

    pub fn branch_if_int32(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let tag = self.builder.ins().band_imm(v, Value::NUMBER_TAG as i64);
        let is_int = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, Value::NUMBER_TAG as i64);
        self.builder
            .ins()
            .brif(is_int, then, then_args, else_, else_args);
    }

    pub fn is_immediate(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK as i64);
        self.builder.ins().icmp_imm(IntCC::NotEqual, tag, 0)
    }

    pub fn is_heap_object(&mut self, v: ir::Value) -> ir::Value {
        let tag = self.builder.ins().band_imm(v, Value::NOT_CELL_MASK as i64);
        let non_zero = self.builder.ins().icmp_imm(IntCC::NotEqual, v, 0);
        let is_cell = self.builder.ins().icmp_imm(IntCC::Equal, tag, 0);

        self.builder.ins().band(is_cell, non_zero)
    }

    pub fn is_heap_object_tc8(&mut self, v: ir::Value, tc8: TypeCode8) -> ir::Value {
        let if_heap_obj = self.builder.create_block();
        let succ = self.builder.create_block();
        self.builder.append_block_param(succ, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_cell = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_cell, if_heap_obj, &[], succ, &[BlockArg::Value(zero)]);
        self.builder.switch_to_block(if_heap_obj);
        {
            let t =
                self.builder
                    .ins()
                    .load(types::I8, ir::MemFlags::trusted().with_can_move(), v, 0);

            let cmp = self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, t, tc8.bits() as i64);
            self.builder.ins().jump(succ, &[BlockArg::Value(cmp)]);
        }
        self.builder.switch_to_block(succ);
        self.builder.block_params(succ)[0]
    }

    pub fn is_heap_object_tc16(&mut self, v: ir::Value, tc16: TypeCode16) -> ir::Value {
        let if_heap_obj = self.builder.create_block();
        let succ = self.builder.create_block();
        self.builder.append_block_param(succ, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_cell = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_cell, if_heap_obj, &[], succ, &[BlockArg::Value(zero)]);
        self.builder.switch_to_block(if_heap_obj);
        {
            let t =
                self.builder
                    .ins()
                    .load(types::I16, ir::MemFlags::trusted().with_can_move(), v, 0);

            let cmp = self
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, t, tc16.bits() as i64);
            self.builder.ins().jump(succ, &[BlockArg::Value(cmp)]);
        }
        self.builder.switch_to_block(succ);
        self.builder.block_params(succ)[0]
    }

    pub fn has_typ8(&mut self, v: ir::Value, typ: u8) -> ir::Value {
        let check_object = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.append_block_param(done, types::I8);

        let false_ = self.builder.ins().iconst(types::I8, 0);
        self.branch_if_immediate(v, done, &[BlockArg::Value(false_)], check_object, &[]);
        self.builder.switch_to_block(check_object);
        {
            let tc8 =
                self.builder
                    .ins()
                    .load(types::I8, ir::MemFlags::trusted().with_can_move(), v, 0);
            let check = self.builder.ins().icmp_imm(IntCC::Equal, tc8, typ as i64);
            self.builder.ins().jump(done, &[BlockArg::Value(check)]);
        }
        self.builder.switch_to_block(done);
        self.builder.block_params(done)[0]
    }

    pub fn has_typ16(&mut self, v: ir::Value, typ: u16) -> ir::Value {
        let check_object = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.append_block_param(done, types::I8);

        let false_ = self.builder.ins().iconst(types::I8, 0);
        self.branch_if_immediate(v, done, &[BlockArg::Value(false_)], check_object, &[]);
        self.builder.switch_to_block(check_object);
        {
            let tc16 =
                self.builder
                    .ins()
                    .load(types::I16, ir::MemFlags::trusted().with_can_move(), v, 0);
            let check = self.builder.ins().icmp_imm(IntCC::Equal, tc16, typ as i64);
            self.builder.ins().jump(done, &[BlockArg::Value(check)]);
        }

        self.builder.switch_to_block(done);
        self.builder.block_params(done)[0]
    }

    pub fn branch_if_immediate(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let is_immediate = self.is_immediate(v);
        self.builder
            .ins()
            .brif(is_immediate, then, then_args, else_, else_args);
    }

    pub fn branch_if_heap_object(
        &mut self,
        v: ir::Value,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let is_heap_object = self.is_heap_object(v);
        self.builder
            .ins()
            .brif(is_heap_object, then, then_args, else_, else_args);
    }

    pub fn branch_if_has_typ8(
        &mut self,
        v: ir::Value,
        typ: u8,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_typ = self.has_typ8(v, typ);
        self.builder
            .ins()
            .brif(has_typ, then, then_args, else_, else_args);
    }

    pub fn branch_if_has_typ16(
        &mut self,
        v: ir::Value,
        typ: u16,
        then: ir::Block,
        then_args: &[BlockArg],
        else_: ir::Block,
        else_args: &[BlockArg],
    ) {
        let has_typ = self.has_typ16(v, typ);
        self.builder
            .ins()
            .brif(has_typ, then, then_args, else_, else_args);
    }

    pub fn wrong_num_args(&mut self, proc: &str, h: LVarRef<'gc>) {
        let c = Symbol::from_str(self.module_builder.ctx, proc);
        let c = self.module_builder.intern_constant(c.into()).unwrap();
        let c = self.import_data(c);

        let addr = self.builder.ins().global_value(types::I64, c);
        let value =
            self.builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0);
        let ctx = self.builder.ins().get_pinned_reg(types::I64);
        let err = self
            .builder
            .ins()
            .call(self.thunks.wrong_number_of_args, &[ctx, value]);
        let val = self.builder.inst_results(err)[0];
        self.continue_to(h, &[val]);
    }

    pub fn check_argcount(&mut self, proc: &str, h: LVarRef<'gc>, got: usize, expected: usize) {
        let got = self.builder.ins().iconst(types::I64, got as i64);
        let expected = self.builder.ins().iconst(types::I64, expected as i64);

        let on_err = self.builder.create_block();
        let on_succ = self.builder.create_block();

        let cmp = self.builder.ins().icmp(IntCC::Equal, got, expected);

        self.builder.ins().brif(cmp, on_succ, &[], on_err, &[]);
        self.builder.func.layout.set_cold(on_err);
        self.builder.switch_to_block(on_err);
        {
            self.wrong_num_args(proc, h);
        }
        self.builder.switch_to_block(on_succ);
    }

    pub fn handle_thunk_call_result(
        &mut self,
        thunk: ir::FuncRef,
        args: &[ir::Value],
        handler: LVarRef<'gc>,
    ) -> ir::Value {
        let call = self.builder.ins().call(thunk, args);

        let code = self.builder.inst_results(call)[0];
        let value = self.builder.inst_results(call)[1];

        let is_error = self.builder.ins().icmp_imm(IntCC::NotEqual, code, 0);

        let on_error = self.builder.create_block();
        let on_success = self.builder.create_block();
        self.builder.append_block_param(on_error, types::I64);
        self.builder.append_block_param(on_success, types::I64);
        self.builder.func.layout.set_cold(on_error);

        self.builder.ins().brif(
            is_error,
            on_error,
            &[BlockArg::Value(value)],
            on_success,
            &[BlockArg::Value(value)],
        );
        self.builder.switch_to_block(on_error);
        {
            let value = self.builder.block_params(on_error)[0];
            self.continue_to(handler, &[value]);
        }
        self.builder.switch_to_block(on_success);
        self.builder.block_params(on_success)[0]
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

            _ => todo!(),
        }
    }

    pub fn cont(&mut self, cont: ContRef<'gc>) {
        if cont.reified.get() {
        } else {
        }
    }

    pub fn fix(&mut self, funcs: &[FuncRef<'gc>]) {
        for func in funcs.iter() {
            let free = func.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());
            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 1);
            let fref = self.module_builder.func_for_func[&func];
            let fref = self
                .module_builder
                .module
                .declare_func_in_func(fref, &mut self.builder.func);
            let addr = self.builder.ins().func_addr(types::I64, fref);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, addr, nfree, is_cont]);
            let clos = self.builder.inst_results(clos)[0];
            self.variables.insert(func.binding, VarDef::Value(clos));
        }

        for func in funcs.iter() {
            let clos = self.var(func.binding);
            let free = func.free_vars.get();
            if let Some(free) = free {
                let fv = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    clos,
                    offset_of!(Closure, free) as i32,
                );
                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        fv,
                        (Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }
    }

    pub fn letk(&mut self, conts: &[ContRef<'gc>]) {
        for cont in conts.iter().filter(|k| k.reified.get()) {
            let free = cont.free_vars.get();
            let nfree = free.map_or(0, |f| f.len());

            let func_for_k = self.module_builder.func_for_cont[&cont];
            let func_for_k = self
                .module_builder
                .module
                .declare_func_in_func(func_for_k, &mut self.builder.func);

            let nfree = self.builder.ins().iconst(types::I64, nfree as i64);
            let is_cont = self.builder.ins().iconst(types::I8, 1);
            let addr = self.builder.ins().func_addr(types::I64, func_for_k);
            let ctx = self.builder.ins().get_pinned_reg(types::I64);
            let clos = self
                .builder
                .ins()
                .call(self.thunks.make_closure, &[ctx, addr, nfree, is_cont]);
            let clos = self.builder.inst_results(clos)[0];

            self.variables.insert(cont.binding, VarDef::Value(clos));
        }

        for cont in conts.iter().filter(|k| k.reified.get()) {
            let clos = self.var(cont.binding);
            let free = cont.free_vars.get();

            if let Some(free) = free {
                let fv = self.builder.ins().load(
                    types::I64,
                    ir::MemFlags::trusted().with_can_move(),
                    clos,
                    offset_of!(Closure, free) as i32,
                );

                for (i, &var) in free.iter().enumerate() {
                    let var = self.var(var);

                    self.builder.ins().store(
                        ir::MemFlags::trusted().with_can_move(),
                        var,
                        fv,
                        (Vector::OFFSET_OF_DATA as i32 + (i as i32 * 8)) as i32,
                    );
                }
            }
        }

        for &cont in conts.iter().filter(|k| !k.reified.get()) {
            let _block = self.block_for_cont(cont);
            self.to_generate.push(cont);
        }
    }

    pub fn finalize(&mut self) {
        while let Some(cont) = self.to_generate.pop() {
            let block = self.block_for_cont(cont);
            self.builder.switch_to_block(block);
            self.term(cont.body);
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

    pub fn term(&mut self, term: TermRef<'gc>) {
        match &*term {
            Term::Let(var, expr, next) => {
                match expr {
                    Expression::PrimCall(prim, args, handler, _) => {
                        let Some(lowerer) = self.module_builder.prims.map.get(prim).copied() else {
                            panic!("undefined primitive: {prim}")
                        };

                        let val = match lowerer(self, args, *handler) {
                            PrimValue::Value(val) => VarDef::Value(val),
                            PrimValue::Comparison(val) => VarDef::Comparison(val),
                        };
                        self.variables.insert(*var, val);
                    }

                    _ => todo!("{expr:?}"),
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

            Term::App(rator, retk, reth, rands, _) => {
                let rator = self.atom(*rator);

                let num_rands = rands.len() + 2;
                let num_rands = self.builder.ins().iconst(types::I64, num_rands as i64);
                let code = self.get_callee_code(rator, *reth);

                let retk = self.var(*retk);
                let reth = self.var(*reth);

                let rands = [retk, reth]
                    .into_iter()
                    .chain(rands.iter().map(|a| self.atom(*a)))
                    .collect::<Vec<_>>();

                let rands = self.push_args(&rands);

                self.builder.ins().jump(
                    self.exit_block,
                    &[
                        BlockArg::Value(code),
                        BlockArg::Value(rator),
                        BlockArg::Value(rands),
                        BlockArg::Value(num_rands),
                    ],
                );
            }

            Term::Continue(k, rands, _) => {
                let rands = rands.iter().map(|a| self.atom(*a)).collect::<Vec<_>>();
                self.continue_to(*k, &rands);
            }

            Term::If(cond, cons, alt, _) => {
                /*let cond = self.atom(*cond);

                let is_false = self.builder.ins().icmp_imm(
                    IntCC::Equal,
                    cond,
                    Value::new(false).bits() as i64,
                );*/

                let truthy = self.atom_for_cond(*cond);

                let kcons = self.builder.create_block();
                let kalt = self.builder.create_block();
                //self.builder.ins().debugtrap();
                self.builder.ins().brif(truthy, kcons, &[], kalt, &[]);
                self.builder.switch_to_block(kalt);
                {
                    self.continue_to(*alt, &[]);
                }

                self.builder.switch_to_block(kcons);
                {
                    self.continue_to(*cons, &[]);
                }
            }

            _ => todo!(),
        }
    }

    pub fn check_yield(&mut self, rator: ir::Value, rands: ir::Value, num_rands: ir::Value) {
        let ctx = self.builder.ins().get_pinned_reg(types::I64);

        let mc = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            ctx,
            offset_of!(Context, mc) as i32,
        );
        let thread = self.builder.ins().load(
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            mc,
            Mutation::OFFSET_OF_THREAD as i32,
        );

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

pub fn compile_file<'gc>(
    ctx: Context<'gc>,
    path: impl AsRef<Path>,
) -> Result<ObjectProduct, Value<'gc>> {
    let path = path.as_ref();
    let filename = path.to_string_lossy();
    let source = std::fs::read_to_string(path)
        .map_err(|err| Str::new(&ctx, format!("IO error, path={}: {err}", filename), true))?;
    let sexp = read_from_string(ctx, source, filename)
        .map_err(|err| Str::new(&ctx, format!("Failed to parse source: {err:?}"), true))?;

    let cps = compile_program(ctx, sexp, *root_module(ctx))
        .map_err(|err| Str::new(&ctx, format!("Failed to compile program: {err:?}"), true))?;

    let doc = cps.pretty::<_, &pretty::BoxAllocator>(&pretty::BoxAllocator);

    doc.1.render(70, &mut std::io::stdout()).unwrap();
    println!();

    let reify_info = crate::cps::reify(ctx, cps);

    let mut shared_builder = settings::builder();

    shared_builder.set("opt_level", "speed").unwrap();
    shared_builder.enable("preserve_frame_pointers").unwrap();
    shared_builder.enable("is_pic").unwrap();
    shared_builder.enable("enable_pinned_reg").unwrap();
    shared_builder.enable("enable_alias_analysis").unwrap();

    let shared_flags = settings::Flags::new(shared_builder);

    let isa = cranelift_codegen::isa::lookup_by_name("x86_64-unknown-linux")
        .unwrap()
        .finish(shared_flags)
        .unwrap();

    let objbuilder =
        cranelift_object::ObjectBuilder::new(isa, "test", default_libcall_names()).unwrap();

    let objmodule = ObjectModule::new(objbuilder);

    let mut module_builder = ModuleBuilder::new(ctx, objmodule, reify_info);

    module_builder.compile();
    let product = module_builder.module.finish();

    return Ok(product);
}

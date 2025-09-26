use crate::{
    compiler::{
        debuginfo::{DebugContext, FunctionDebugContext},
        ssa::primitive::PrimitiveLowerer,
    },
    cps::{
        ReifyInfo,
        term::{ContRef, FuncRef},
    },
    expander::core::LVarRef,
    runtime::{
        Context,
        fasl::FASLWriter,
        value::{Value, ValueEqual, Vector},
    },
};

use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, InstBuilder, types};
use cranelift_codegen::{
    ir::{self, SourceLoc},
    isa::CallConv,
};

use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use rsgc::Gc;
use std::collections::HashMap;

use crate::runtime::vm::thunks::*;

pub mod helpers;
pub mod primitive;
pub mod traits;
pub mod translate;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDef {
    Value(ir::Value),
    Comparison(ir::Value),
    Free(usize),
}

/// A SSA Builder. Constructs Cranelift module and SSA from single compilation unit.
pub struct ModuleBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub(crate) debug_context: DebugContext<'gc>,
    pub reify_info: ReifyInfo<'gc>,
    pub constants: HashMap<ValueEqual<'gc>, DataId>,
    pub cache_cells: HashMap<ValueEqual<'gc>, DataId>,

    pub module: ObjectModule,

    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_cont: HashMap<ContRef<'gc>, FuncId>,
    pub func_for_func: HashMap<FuncRef<'gc>, FuncId>,
    pub import_data: HashMap<&'static str, DataId>,

    pub thunks: Thunks,
    pub stacktraces: bool,
}

impl<'gc> ModuleBuilder<'gc> {
    pub fn new(ctx: Context<'gc>, mut module: ObjectModule, reify_info: ReifyInfo<'gc>) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let thunks = Thunks::new(&mut module);
        let debug_context = DebugContext::new(&reify_info, module.isa());
        Self {
            debug_context,
            ctx,
            stacktraces: true,
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
            let name = format!("fn{}:{}:{}", i, func.name, func.binding.name);
            let func_id = self
                .module
                .declare_function(&name, Linkage::Export, &sig)
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
            let name = format!("cont{}:{}:{}", i, cont.name, cont.binding.name);
            let cont_id = self
                .module
                .declare_function(&name, Linkage::Local, &sig)
                .unwrap();
            self.func_for_cont.insert(cont, cont_id);
        }

        let mut context = self.module.make_context();
        let mut fctx = FunctionBuilderContext::new();
        let funcs = self.reify_info.functions;
        for (i, &func) in funcs.iter().enumerate() {
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(&self.thunks, &mut builder.func, &mut self.module);
            let name = format!("fn{}:{}:{}", i, func.name, func.binding.name);
            let func_debug_cx = self.debug_context.define_function(func, &name);

            let func_id = self.func_for_func.get(&func).copied().unwrap();
            let mut ssa =
                SSABuilder::new(self, builder, ContOrFunc::Func(func), thunks, func_debug_cx);

            ssa.term(func.body);
            ssa.finalize();

            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();
            let func_debug_cx = ssa.func_debug_cx;
            self.module.define_function(func_id, &mut context).unwrap();
            func_debug_cx.finalize(
                &mut self.debug_context,
                func_id,
                &context,
                self.module.isa(),
            );
            self.module.clear_context(&mut context);
        }

        let conts = self.reify_info.continuations;

        for (i, &cont) in conts.iter().filter(|c| c.reified.get()).enumerate() {
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let name = format!("fn{}:{}:{}", i, cont.name, cont.binding.name);
            let func_debug_cx = self.debug_context.define_cont(cont, &name);
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(&self.thunks, &mut builder.func, &mut self.module);
            let func_id = self.func_for_cont[&cont];
            let mut ssa =
                SSABuilder::new(self, builder, ContOrFunc::Cont(cont), thunks, func_debug_cx);

            ssa.term(cont.body);
            ssa.finalize();

            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();

            let func_debug_cx = ssa.func_debug_cx;
            self.module
                .define_function(func_id, &mut context)
                .unwrap_or_else(|err| {
                    panic!(
                        "error when compiling continuation {}: {}",
                        cont.binding.name, err
                    )
                });
            func_debug_cx.finalize(
                &mut self.debug_context,
                func_id,
                &context,
                self.module.isa(),
            );
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
            let meta = builder
                .ins()
                .iconst(types::I64, Value::new(false).bits() as i64);
            let call = builder.ins().call(
                make_closure,
                &[ctx, entrypoint_fn_ref, nfree, is_cont, meta],
            );
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
    pub(crate) func_debug_cx: FunctionDebugContext<'gc>,

    /// Map from non reified continuations to corresponding Cranelift block.
    pub blockmap: HashMap<ContRef<'gc>, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,

    pub target: ContOrFunc<'gc>,
    pub exit_block: ir::Block,
    /// A basic-block that performs application of function.
    ///
    /// This is mainly used to save on a code size when multiple
    /// application sites are present in a function/continuation.
    pub app_block: Option<ir::Block>,

    pub rator: ir::Value,
    pub thunks: ImportedThunks,

    pub sig_call: ir::SigRef,
    pub to_generate: Vec<ContRef<'gc>>,

    pub data_imports: HashMap<DataId, ir::GlobalValue>,

    pub srcloc: Option<SourceLoc>,
}

#[derive(Clone, Copy)]
pub enum ContOrFunc<'gc> {
    Func(FuncRef<'gc>),
    Cont(ContRef<'gc>),
}

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
    pub(crate) fn new(
        module_builder: &'a mut ModuleBuilder<'gc>,
        mut builder: FunctionBuilder<'f>,
        target: ContOrFunc<'gc>,
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

        builder.append_block_param(exit_block, types::I64); /* code */
        builder.append_block_param(exit_block, types::I64); /* rator */
        builder.append_block_param(exit_block, types::I64); /* rands */
        builder.append_block_param(exit_block, types::I64); /* num_rands */

        builder.set_val_label(rator, func_debug_cx.internal_variable(0));
        builder.set_val_label(num_rands, func_debug_cx.internal_variable(1));
        builder.set_val_label(rands, func_debug_cx.internal_variable(2));

        let mut this = Self {
            module_builder,
            builder,
            target,
            exit_block,
            app_block: None,
            rator,
            func_debug_cx,

            variables,
            blockmap: HashMap::new(),
            to_generate: Vec::new(),
            thunks,

            sig_call,

            data_imports: HashMap::new(),
            srcloc: None,
        };

        this.entrypoint(rands, num_rands);

        this
    }
}

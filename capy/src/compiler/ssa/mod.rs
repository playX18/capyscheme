//! SSA (Static Single Assignment) code generation using Cranelift.

use crate::{
    compiler::{
        debuginfo::{DebugContext, FunctionDebugContext},
        ssa::primitive::PrimitiveLowerer,
    },
    cps::{
        ReifyInfo,
        linear::{
            BlockId, CodeId, CodeIdTable, CodeRef, LinearProgram, Procedure, ProcedureKind, ValueId,
        },
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
    ir::{self, BlockArg, SourceLoc},
    isa::CallConv,
};

use crate::rsgc::Gc;
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
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
}

#[derive(Clone, Copy)]
pub struct LinearRestSource {
    pub rands: ir::Value,
    pub num_rands: ir::Value,
    pub fixed_count: usize,
}

/// A SSA Builder. Constructs Cranelift module and SSA from single compilation unit.
pub struct ImportedDataSlots<'gc> {
    pub constants: Vec<(Value<'gc>, String)>,
    pub cache_cells: Vec<(Value<'gc>, String)>,
    pub global_side_metadata_base_address: String,
}

enum DataSlotMode<'gc> {
    Object,
    Imported(ImportedDataSlots<'gc>),
}

pub(crate) fn procedure_binding_name<'gc>(procedure: &Procedure<'gc>) -> String {
    procedure
        .sources
        .get(&procedure.binding)
        .map(|binding| binding.name.to_string())
        .unwrap_or_else(|| format!("v{}", procedure.binding.0))
}

pub(crate) fn procedure_linkage_name<'gc>(
    prefix: &str,
    index: usize,
    procedure: &Procedure<'gc>,
) -> String {
    format!(
        "{}{}:{}:{}",
        prefix,
        index,
        procedure.name,
        procedure_binding_name(procedure)
    )
}

/// A SSA Builder. Constructs Cranelift module and SSA from single compilation unit.
pub struct ModuleBuilder<'gc, M: Module = ObjectModule> {
    pub ctx: Context<'gc>,
    pub(crate) debug_context: DebugContext<'gc>,
    pub reify_info: ReifyInfo<'gc>,
    pub linear: LinearProgram<'gc>,
    pub constants: HashMap<ValueEqual<'gc>, DataId>,
    pub cache_cells: HashMap<ValueEqual<'gc>, DataId>,

    pub module: M,
    data_slot_mode: DataSlotMode<'gc>,

    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_code: HashMap<CodeId, FuncId>,
    pub func_for_cont: HashMap<ContRef<'gc>, FuncId>,
    pub func_for_func: HashMap<FuncRef<'gc>, FuncId>,
    pub code_block_for_code: HashMap<CodeId, DataId>,
    pub code_block_for_cont: HashMap<ContRef<'gc>, DataId>,
    pub code_block_for_func: HashMap<FuncRef<'gc>, DataId>,
    pub import_data: HashMap<&'static str, DataId>,
    pub global_side_metadata_base_address: DataId,

    pub thunks: Thunks,
    pub stacktraces: bool,
    pub direct_calls: bool,
}

impl<'gc> ModuleBuilder<'gc, ObjectModule> {
    pub fn new(
        ctx: Context<'gc>,
        mut module: ObjectModule,
        reify_info: ReifyInfo<'gc>,
        linear: LinearProgram<'gc>,
    ) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let thunks = Thunks::new(&mut module);
        let debug_context = DebugContext::new(&reify_info, module.isa());
        let global_side_metadata_base_address = module
            .declare_data(
                "__GLOBAL_SIDE_METADATA_VM_ADDRESS",
                Linkage::Local,
                true,
                false,
            )
            .unwrap();

        let mut desc = DataDescription::new();
        desc.define_zeroinit(size_of::<usize>());
        module
            .define_data(global_side_metadata_base_address, &desc)
            .unwrap();
        Self {
            debug_context,
            ctx,
            stacktraces: true,
            direct_calls: true,
            reify_info,
            linear,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            module,
            data_slot_mode: DataSlotMode::Object,
            import_data: HashMap::new(),
            prims,
            func_for_code: HashMap::new(),
            func_for_cont: HashMap::new(),
            func_for_func: HashMap::new(),
            code_block_for_code: HashMap::new(),
            code_block_for_cont: HashMap::new(),
            code_block_for_func: HashMap::new(),
            global_side_metadata_base_address,

            thunks,
        }
    }
}

impl<'gc, M: Module> ModuleBuilder<'gc, M> {
    pub fn new_with_imported_data(
        ctx: Context<'gc>,
        mut module: M,
        reify_info: ReifyInfo<'gc>,
        linear: LinearProgram<'gc>,
        imported_slots: ImportedDataSlots<'gc>,
    ) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let thunks = Thunks::new(&mut module);
        let debug_context = DebugContext::new(&reify_info, module.isa());
        let global_side_metadata_base_address = module
            .declare_data(
                &imported_slots.global_side_metadata_base_address,
                Linkage::Import,
                false,
                false,
            )
            .unwrap();

        Self {
            debug_context,
            ctx,
            stacktraces: true,
            direct_calls: false,
            reify_info,
            linear,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            module,
            data_slot_mode: DataSlotMode::Imported(imported_slots),
            import_data: HashMap::new(),
            prims,
            func_for_code: HashMap::new(),
            func_for_cont: HashMap::new(),
            func_for_func: HashMap::new(),
            code_block_for_code: HashMap::new(),
            code_block_for_cont: HashMap::new(),
            code_block_for_func: HashMap::new(),
            global_side_metadata_base_address,

            thunks,
        }
    }

    pub fn declare_imported_code_block_slot(&mut self, name: &str) -> DataId {
        self.module
            .declare_data(name, Linkage::Import, false, false)
            .expect("failed to declare imported code block data")
    }

    pub fn compile(&mut self) {
        let procedures = self.linear.procedures.clone();
        let code_ids = CodeIdTable::new(&self.reify_info);

        let sig = call_signature!(Tail (I64 /* rator */, I64 /* rands */, I64 /* num_rands */) -> (I64, I64));
        let mut function_index = 0;
        let mut continuation_index = 0;
        for procedure in procedures.iter() {
            match procedure.kind {
                ProcedureKind::Function => {
                    let i = function_index;
                    function_index += 1;
                    let name = procedure_linkage_name("fn", i, procedure);
                    let func_id = self
                        .module
                        .declare_function(&name, Linkage::Export, &sig)
                        .expect("failed to declare function in cranelift module");

                    self.func_for_code.insert(procedure.code, func_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_fn{}", i));
                    self.code_block_for_code
                        .insert(procedure.code, code_block_data_id);
                    if let CodeRef::Function(func) = code_ids.code_ref(procedure.code) {
                        self.func_for_func.insert(func, func_id);
                        self.code_block_for_func.insert(func, code_block_data_id);
                    }
                }
                ProcedureKind::Continuation => {
                    let i = continuation_index;
                    continuation_index += 1;
                    let name = procedure_linkage_name("cont", i, procedure);
                    let cont_id = self
                        .module
                        .declare_function(&name, Linkage::Local, &sig)
                        .expect("failed to declare continuation in cranelift module");
                    self.func_for_code.insert(procedure.code, cont_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_cont{}", i));
                    self.code_block_for_code
                        .insert(procedure.code, code_block_data_id);
                    if let CodeRef::Continuation(cont) = code_ids.code_ref(procedure.code) {
                        self.func_for_cont.insert(cont, cont_id);
                        self.code_block_for_cont.insert(cont, code_block_data_id);
                    }
                }
            }
        }

        let mut context = self.module.make_context();
        let mut fctx = FunctionBuilderContext::new();
        function_index = 0;
        continuation_index = 0;
        for procedure in procedures.iter() {
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(&self.thunks, &mut builder.func, &mut self.module);
            let (func_id, func_debug_cx) = match procedure.kind {
                ProcedureKind::Function => {
                    let i = function_index;
                    function_index += 1;
                    let name = procedure_linkage_name("fn", i, procedure);
                    let func_debug_cx =
                        self.debug_context.define_linear_procedure(procedure, &name);
                    let func_id = self
                        .func_for_code
                        .get(&procedure.code)
                        .copied()
                        .expect("function should have been declared before compilation");
                    (func_id, func_debug_cx)
                }
                ProcedureKind::Continuation => {
                    let i = continuation_index;
                    continuation_index += 1;
                    let name = procedure_linkage_name("cont", i, procedure);
                    let func_debug_cx =
                        self.debug_context.define_linear_procedure(procedure, &name);
                    let func_id = self
                        .func_for_code
                        .get(&procedure.code)
                        .copied()
                        .expect("continuation should have been declared before compilation");
                    (func_id, func_debug_cx)
                }
            };
            let mut ssa = SSABuilder::new(
                self,
                builder,
                ContOrFunc::Procedure(procedure.clone()),
                thunks,
                func_debug_cx,
            );

            ssa.linear_procedure(procedure);
            ssa.finalize();

            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();
            let func_debug_cx = ssa.func_debug_cx;
            self.module
                .define_function(func_id, &mut context)
                .unwrap_or_else(|err| {
                    panic!(
                        "error when compiling procedure {}: {}",
                        procedure.sources[&procedure.binding].name, err
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
        let funcs = self
            .reify_info
            .functions
            .iter()
            .copied()
            .collect::<Vec<_>>();
        let conts = self
            .reify_info
            .continuations
            .iter()
            .copied()
            .filter(|cont| cont.reified.get())
            .collect::<Vec<_>>();

        for func in funcs.iter().copied() {
            let _ = self.intern_constant(func.meta);
        }
        for cont in conts.iter().copied() {
            let _ = self.intern_constant(cont.meta);
        }

        let globals_array = self
            .module
            .declare_data("CAPY_GLOBALS", Linkage::Export, true, false)
            .expect("failed to declare CAPY_GLOBALS data");
        let globals_size = self
            .module
            .declare_data("CAPY_GLOBALS_LEN", Linkage::Export, false, false)
            .expect("failed to declare CAPY_GLOBALS_LEN data");

        let mut desc = DataDescription::new();
        desc.set_align(size_of::<usize>() as _);
        let datas = self
            .constants
            .values()
            .copied()
            .chain(self.cache_cells.values().copied())
            .chain(self.code_block_for_func.values().copied())
            .chain(self.code_block_for_cont.values().copied());
        let mut offset = 0;
        for data in datas {
            let gv = self.module.declare_data_in_data(data, &mut desc);

            desc.write_data_addr(offset, gv, 0);
            offset += 8;
        }
        desc.define(vec![0; offset as usize].into_boxed_slice());

        self.module
            .define_data(globals_array, &desc)
            .expect("failed to define CAPY_GLOBALS data");

        desc = DataDescription::new();

        desc.define(Box::new((offset / 8).to_le_bytes()));
        desc.set_align(size_of::<usize>() as _);
        self.module
            .define_data(globals_size, &desc)
            .expect("failed to define CAPY_GLOBALS_LEN data");

        // First let's get some stable way to iterate constants
        let constants = self
            .constants
            .iter()
            .map(|(key, id)| (key.0, *id))
            .collect::<Vec<_>>();

        let vec = Vector::new::<true>(*self.ctx, constants.len(), Value::undefined());
        let wvec = Gc::write(*self.ctx, vec);
        for (i, (val, _)) in constants.iter().enumerate() {
            wvec[i].unlock().set(*val);
        }

        let mut buf = Vec::with_capacity(1024);
        let writer = FASLWriter::new(self.ctx, &mut buf);
        writer.write(vec.into()).expect("should not fail");

        let sig = call_signature!(SystemV(I64, I64) -> I64);
        let init_fn_id = self
            .module
            .declare_function("capy_module_init", Linkage::Export, &sig)
            .expect("failed to declare capy_module_init function");

        let fasl_data = self
            .module
            .declare_data("capy_fasl_constants", Linkage::Export, false, false)
            .expect("failed to declare capy_fasl_constants data");
        let mut desc = DataDescription::new();

        let size = buf.len();
        desc.set_align(align_of::<usize>() as _);
        desc.define(buf.into_boxed_slice());
        self.module
            .define_data(fasl_data, &desc)
            .expect("failed to define capy_fasl_constants data");
        {
            ctx.func.signature = sig;
            let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);

            let ctx = builder.block_params(entry)[0];
            let global_side_metadata_base_address_val = builder.block_params(entry)[1];

            let global_side_metadata_data = self
                .module
                .declare_data_in_func(self.global_side_metadata_base_address, &mut builder.func);
            let global_side_metadata_base_address = builder
                .ins()
                .global_value(types::I64, global_side_metadata_data);

            builder.ins().store(
                ir::MemFlags::trusted().with_can_move(),
                global_side_metadata_base_address_val,
                global_side_metadata_base_address,
                0,
            );

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

            let make_aot_code_block = self
                .module
                .declare_func_in_func(self.thunks.make_aot_code_block, &mut builder.func);
            let make_closure = self
                .module
                .declare_func_in_func(self.thunks.make_closure, &mut builder.func);

            for func in funcs.iter().copied() {
                let func_id = self.func_for_func[&func];
                let func_ref = self.module.declare_func_in_func(func_id, &mut builder.func);
                let entrypoint = builder.ins().func_addr(types::I64, func_ref);
                let arity = builder
                    .ins()
                    .iconst(types::I32, Self::arity_for_func(func) as i64);
                let is_cont = builder.ins().iconst(types::I8, 0);
                let metadata = self.load_constant(&mut builder, func.meta);
                let call = builder.ins().call(
                    make_aot_code_block,
                    &[ctx, entrypoint, arity, is_cont, metadata],
                );
                let code_block = builder.inst_results(call)[0];
                let data_id = self.code_block_for_func[&func];
                let gv = self.module.declare_data_in_func(data_id, &mut builder.func);
                let addr = builder.ins().global_value(types::I64, gv);
                builder
                    .ins()
                    .store(ir::MemFlags::trusted().with_can_move(), code_block, addr, 0);
            }

            for cont in conts.iter().copied() {
                let cont_id = self.func_for_cont[&cont];
                let cont_ref = self.module.declare_func_in_func(cont_id, &mut builder.func);
                let entrypoint = builder.ins().func_addr(types::I64, cont_ref);
                let arity = builder
                    .ins()
                    .iconst(types::I32, Self::arity_for_cont(cont) as i64);
                let is_cont = builder.ins().iconst(types::I8, 1);
                let metadata = self.load_constant(&mut builder, cont.meta);
                let call = builder.ins().call(
                    make_aot_code_block,
                    &[ctx, entrypoint, arity, is_cont, metadata],
                );
                let code_block = builder.inst_results(call)[0];
                let data_id = self.code_block_for_cont[&cont];
                let gv = self.module.declare_data_in_func(data_id, &mut builder.func);
                let addr = builder.ins().global_value(types::I64, gv);
                builder
                    .ins()
                    .store(ir::MemFlags::trusted().with_can_move(), code_block, addr, 0);
            }

            let entry_code_block_data = self.code_block_for_func[&self.reify_info.entrypoint];
            let entry_gv = self
                .module
                .declare_data_in_func(entry_code_block_data, &mut builder.func);
            let entry_addr = builder.ins().global_value(types::I64, entry_gv);
            let entry_code_block = builder.ins().load(
                types::I64,
                ir::MemFlags::trusted().with_can_move(),
                entry_addr,
                0,
            );

            let nfree = builder.ins().iconst(types::I64, 0);
            let is_cont = builder.ins().iconst(types::I8, 0);
            let call = builder
                .ins()
                .call(make_closure, &[ctx, entry_code_block, nfree, is_cont]);
            let clos = builder.inst_results(call)[0];

            builder.ins().return_(&[clos]);
            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module
            .define_function(init_fn_id, ctx)
            .expect("failed to define capy_module_init function");
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

        let data_id = match &self.data_slot_mode {
            DataSlotMode::Object => {
                let ix = self.constants.len();
                let name = format!("constant{}", ix);
                // declare data as writable even though not all objects need to be written to. We don't currently have a way of knowing
                // ahead of time if constant will be read-only or not.
                let data_id = self
                    .module
                    .declare_data(&name, Linkage::Local, true, false)
                    .expect("failed to declare constant data");
                let mut desc = DataDescription::new();
                desc.define_zeroinit(size_of::<Value>());
                desc.set_align(align_of::<usize>() as _);
                self.module
                    .define_data(data_id, &desc)
                    .expect("failed to define constant data");
                data_id
            }
            DataSlotMode::Imported(imported) => {
                let name = imported
                    .constants
                    .iter()
                    .find_map(|(value, name)| (ValueEqual(*value) == obj).then_some(name))
                    .unwrap_or_else(|| panic!("JIT constant slot not found for {obj}"));
                self.module
                    .declare_data(name, Linkage::Import, false, false)
                    .expect("failed to declare imported constant data")
            }
        };

        self.constants.insert(ValueEqual(obj), data_id);

        Some(data_id)
    }

    pub fn intern_cache_cell(&mut self, key: Value<'gc>) -> DataId {
        if let Some(data_id) = self.cache_cells.get(&ValueEqual(key)) {
            return *data_id;
        }

        let data_id = match &self.data_slot_mode {
            DataSlotMode::Object => {
                let ix = self.cache_cells.len();
                let name = format!("cache_cell{}", ix);

                let data_id = self
                    .module
                    .declare_data(&name, Linkage::Local, true, false)
                    .expect("failed to declare cache cell data");
                let mut desc = DataDescription::new();
                desc.define_zeroinit(size_of::<Value>());
                desc.set_align(align_of::<usize>() as _);
                self.module
                    .define_data(data_id, &desc)
                    .expect("failed to define cache cell data");
                data_id
            }
            DataSlotMode::Imported(imported) => {
                let name = imported
                    .cache_cells
                    .iter()
                    .find_map(|(value, name)| (ValueEqual(*value) == key).then_some(name))
                    .unwrap_or_else(|| panic!("JIT cache cell slot not found for {key}"));
                self.module
                    .declare_data(name, Linkage::Import, false, false)
                    .expect("failed to declare imported cache cell data")
            }
        };

        self.cache_cells.insert(ValueEqual(key), data_id);

        data_id
    }

    fn declare_code_block_slot(&mut self, name: &str) -> DataId {
        let data_id = self
            .module
            .declare_data(name, Linkage::Local, true, false)
            .expect("failed to declare code block data");
        let mut desc = DataDescription::new();
        desc.define_zeroinit(size_of::<Value>());
        desc.set_align(align_of::<usize>() as _);
        self.module
            .define_data(data_id, &desc)
            .expect("failed to define code block data");
        data_id
    }

    fn load_constant(&mut self, builder: &mut FunctionBuilder<'_>, value: Value<'gc>) -> ir::Value {
        if let Some(data_id) = self.intern_constant(value) {
            let gv = self.module.declare_data_in_func(data_id, &mut builder.func);
            let addr = builder.ins().global_value(types::I64, gv);
            builder
                .ins()
                .load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0)
        } else {
            builder.ins().iconst(types::I64, value.bits() as i64)
        }
    }

    fn arity_for_func(func: FuncRef<'gc>) -> i32 {
        if func.variadic.is_some() {
            -((func.args.len() as i32) + 1)
        } else {
            func.args.len() as i32
        }
    }

    fn arity_for_cont(cont: ContRef<'gc>) -> i32 {
        if cont.variadic.is_some() {
            -((cont.args.len() as i32) + 1)
        } else {
            cont.args.len() as i32
        }
    }
}

pub struct SSABuilder<'gc, 'a, 'f, M: Module = ObjectModule> {
    pub module_builder: &'a mut ModuleBuilder<'gc, M>,
    pub builder: FunctionBuilder<'f>,
    pub(crate) func_debug_cx: FunctionDebugContext<'gc>,

    /// Map from non reified continuations to corresponding Cranelift block.
    pub blockmap: HashMap<ContRef<'gc>, ir::Block>,
    pub linear_blockmap: HashMap<BlockId, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,
    pub linear_variables: HashMap<ValueId, VarDef>,
    pub linear_rest_sources: HashMap<ValueId, LinearRestSource>,
    pub synthetic_aliases: HashMap<ValueId, LVarRef<'gc>>,

    pub target: ContOrFunc<'gc>,
    pub exit_block: ir::Block,
    /// Real entrypoint block of the function/continuation.
    ///
    /// We can't jump to entrypoint directly so we hav this extra block.
    pub entry_block: ir::Block,
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

#[derive(Clone)]
pub enum ContOrFunc<'gc> {
    Func(FuncRef<'gc>),
    Cont(ContRef<'gc>),
    Procedure(Procedure<'gc>),
}

impl<'gc, 'a, 'f, M: Module> SSABuilder<'gc, 'a, 'f, M> {
    pub(crate) fn new(
        module_builder: &'a mut ModuleBuilder<'gc, M>,
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
            srcloc: None,
        };

        this.entrypoint(entry_rands, entry_num_rands);

        this
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cps::{
            linear::linearize,
            reify,
            term::{Atom, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Symbol, Value},
        },
    };
    use cranelift::prelude::Configurable;
    use cranelift_module::default_libcall_names;
    use cranelift_object::ObjectBuilder;

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

    fn object_module() -> ObjectModule {
        let mut shared_builder = cranelift_codegen::settings::builder();
        shared_builder.set("enable_probestack", "false").unwrap();
        shared_builder
            .set("enable_heap_access_spectre_mitigation", "false")
            .unwrap();
        shared_builder.set("opt_level", "speed").unwrap();
        shared_builder.enable("preserve_frame_pointers").unwrap();
        shared_builder.enable("is_pic").unwrap();
        shared_builder.enable("enable_pinned_reg").unwrap();
        shared_builder.enable("enable_alias_analysis").unwrap();

        let shared_flags = cranelift_codegen::settings::Flags::new(shared_builder);
        let triple = target_lexicon::Triple::host();
        let isa = cranelift_codegen::isa::lookup(triple)
            .unwrap()
            .finish(shared_flags)
            .unwrap();
        let objbuilder = ObjectBuilder::new(isa, "scheme-test", default_libcall_names()).unwrap();
        ObjectModule::new(objbuilder)
    }

    #[test]
    fn recursive_entry_block_uses_loop_params() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let retk = lvar(ctx, "retk");
            let body = Gc::new(
                *ctx,
                Term::App(
                    Atom::Local(f),
                    retk,
                    Array::from_slice(*ctx, &[]),
                    Value::new(false),
                ),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Symbol::from_str(ctx, "loop").into(),
                    source: Value::new(false),
                    binding: f,
                    return_cont: retk,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let reify_info = reify(ctx, func);
            let linear = linearize(ctx, &reify_info);
            let entry_code = linear.entry;
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == entry_code)
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, object_module(), reify_info, linear);
            let mut context = module_builder.module.make_context();
            context.func.signature = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = ImportedThunks::new(
                &module_builder.thunks,
                &mut builder.func,
                &mut module_builder.module,
            );
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:loop:f");

            let ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                ContOrFunc::Procedure(procedure),
                thunks,
                func_debug_cx,
            );

            assert_eq!(ssa.rator, ssa.builder.block_params(ssa.entry_block)[0]);
        });
    }
}

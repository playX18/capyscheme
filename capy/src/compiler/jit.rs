use std::{
    collections::HashMap,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::{
        LazyLock, RwLock,
        atomic::{AtomicU64, AtomicUsize, Ordering},
    },
};

use crate::{
    compiler::{
        CompilationOptions,
        ssa::{ContOrFunc, ImportedDataSlots, ModuleBuilder, SSABuilder, procedure_linkage_name},
    },
    cps::{
        ReifyInfo,
        linear::{CodeId, Instruction, LinearProgram, Procedure, ProcedureKind, linearize},
        reify,
        term::FuncRef,
    },
    rsgc::{
        Trace, Visitor,
        mmtk::util::Address,
        sync::{monitor::Monitor, thread::Thread},
    },
    runtime::{
        Context, Scheme,
        value::{Closure, CodeArity, CodeBlock, LazyJitHandle, Value, ValueEqual},
        vm::{
            libraries::{JitLibrary, LIBRARY_COLLECTION},
            thunks::Thunks,
        },
    },
};
use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, InstBuilder, types};
use cranelift_codegen::{
    ir::{self, AbiParam},
    isa::CallConv,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataId, FuncId, Linkage, Module, default_libcall_names};
use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

const JIT_GLOBAL_SIDE_METADATA_SYMBOL: &str = "capy_jit_global_side_metadata_base_address";

type JitUnitId = u64;

static NEXT_JIT_UNIT_ID: AtomicU64 = AtomicU64::new(1);

static JIT_GLOBAL_SIDE_METADATA_VALUE: LazyLock<usize> =
    LazyLock::new(|| global_side_metadata_vm_base_address().as_usize());

static JIT_EXTERNAL_SYMBOLS: LazyLock<RwLock<HashMap<String, usize>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

pub(crate) fn compile_cps_to_jit_thunk<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    options: CompilationOptions,
) -> Result<Value<'gc>, Value<'gc>> {
    let reify_info = reify(ctx, cps);
    let linear = linearize(ctx, &reify_info);
    let state = Box::new(JitModuleState::new(ctx, reify_info, linear, options));
    let state_ptr = &*state as *const JitModuleState<'gc> as *const ();
    state.initialize_lazy_handles(ctx, state_ptr);
    state.register_external_symbols();
    let entrypoint = state.entrypoint;
    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    Ok(libs.register_jit(JitLibrary::new(entrypoint, Vec::<Value<'gc>>::new(), state)))
}

static LAZY_JIT_STUB: LazyLock<Address> = LazyLock::new(build_lazy_jit_stub);

fn lazy_jit_stub_entrypoint() -> Address {
    *LAZY_JIT_STUB
}

fn build_jit_builder() -> JITBuilder {
    let mut builder = JITBuilder::with_flags(
        &[
            ("enable_heap_access_spectre_mitigation", "false"),
            ("enable_pinned_reg", "true"),
            ("enable_probestack", "false"),
            ("opt_level", "speed"),
            ("preserve_frame_pointers", "true"),
        ],
        default_libcall_names(),
    )
    .expect("failed to build Cranelift JIT builder");
    builder.symbols(Thunks::symbols());
    builder.symbol_lookup_fn(Box::new(|name| {
        JIT_EXTERNAL_SYMBOLS
            .read()
            .ok()
            .and_then(|symbols| symbols.get(name).copied())
            .map(|addr| addr as *const u8)
    }));
    builder.symbol(
        JIT_GLOBAL_SIDE_METADATA_SYMBOL,
        (&*JIT_GLOBAL_SIDE_METADATA_VALUE as *const usize).cast(),
    );
    builder
}

fn build_lazy_jit_stub() -> Address {
    let mut builder = build_jit_builder();
    builder.symbol("capy_jit_resolve", capy_jit_resolve as *const u8);
    let mut module = JITModule::new(builder);

    let sig_tail = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
    let func_id = module
        .declare_function("capy_lazy_jit_stub", Linkage::Export, &sig_tail)
        .expect("failed to declare lazy JIT stub");

    let mut ctx = module.make_context();
    ctx.func.signature = sig_tail;
    let mut fctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fctx);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let rator = builder.block_params(entry)[0];
        let rands = builder.block_params(entry)[1];
        let num_rands = builder.block_params(entry)[2];
        let runtime_ctx = builder.ins().get_pinned_reg(types::I64);

        let resolver_sig = {
            let mut sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
            sig
        };
        let resolver_id = module
            .declare_function("capy_jit_resolve", Linkage::Import, &resolver_sig)
            .expect("failed to declare lazy JIT resolver");
        let resolver = module.declare_func_in_func(resolver_id, &mut builder.func);
        let call = builder.ins().call(resolver, &[runtime_ctx, rator]);
        let compiled = builder.inst_results(call)[0];

        let tail_sig =
            builder.import_signature(call_signature!(Tail (I64, I64, I64) -> (I64, I64)));
        builder
            .ins()
            .return_call_indirect(tail_sig, compiled, &[rator, rands, num_rands]);

        builder.seal_all_blocks();
        builder.finalize();
    }
    module
        .define_function(func_id, &mut ctx)
        .expect("failed to define lazy JIT stub");
    module
        .finalize_definitions()
        .expect("failed to finalize lazy JIT stub");

    let entrypoint = Address::from_ptr(module.get_finalized_function(func_id));
    Box::leak(Box::new(module));
    entrypoint
}

extern "C-unwind" fn capy_jit_resolve<'gc>(ctx: Context<'gc>, rator: Value<'gc>) -> *const u8 {
    let root = ctx.push_runstack_root(rator);
    let stub = lazy_jit_stub_entrypoint();

    let closure = root.get().downcast::<Closure>();
    if closure.code != stub {
        return closure.code.to_ptr();
    }

    let code_block = closure.code_block;
    if code_block.entrypoint != stub {
        Closure::sync_entrypoint_from_code_block(ctx, closure);
        return code_block.entrypoint.to_ptr();
    }

    let handle = code_block
        .lazy_jit_handle()
        .expect("lazy JIT code block should contain procedure state");
    assert!(
        !handle.program.is_null(),
        "lazy JIT code block program was not initialized"
    );
    let program = unsafe { &*(handle.program.cast::<JitModuleState<'gc>>()) };
    let entrypoint = program.ensure_compiled(ctx, handle.procedure);

    let closure = root.get().downcast::<Closure>();
    if closure.code_block.entrypoint == stub {
        Closure::patch_code_entrypoint(ctx, closure, entrypoint);
    } else {
        Closure::sync_entrypoint_from_code_block(ctx, closure);
    }
    entrypoint.to_ptr()
}

struct JitSlot<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
    symbol: String,
}

struct JitCodeBlockSlot<'gc> {
    code: CodeId,
    value: Value<'gc>,
    symbol: String,
}

#[derive(Clone, Copy)]
struct JitCompileJob {
    program: usize,
    procedure: usize,
}

enum JitCompileStatus {
    Idle,
    Queued,
    Compiling,
    Compiled(usize),
    Failed(String),
}

struct JitProcedureState<'gc> {
    procedure: Procedure<'gc>,
    compiled_entry: AtomicUsize,
    status: Monitor<JitCompileStatus>,
}

impl<'gc> JitProcedureState<'gc> {
    fn compiled_address(&self) -> Option<Address> {
        let compiled = self.compiled_entry.load(Ordering::Acquire);
        if compiled != 0 {
            Some(unsafe { Address::from_usize(compiled) })
        } else {
            None
        }
    }

    fn wait_for_compilation(&self) -> Address {
        loop {
            let mut status = self.status.lock();
            match &*status {
                JitCompileStatus::Compiled(entrypoint) => {
                    return unsafe { Address::from_usize(*entrypoint) };
                }
                JitCompileStatus::Failed(message) => {
                    panic!("lazy JIT compilation failed: {message}");
                }
                JitCompileStatus::Idle => {
                    panic!("lazy JIT procedure was not queued for compilation");
                }
                JitCompileStatus::Queued | JitCompileStatus::Compiling => {
                    let _native = Thread::enter_native_scope();
                    status.wait();
                }
            }
        }
    }
}

struct JitModuleState<'gc> {
    unit_id: JitUnitId,
    entrypoint: Value<'gc>,
    reify_info: ReifyInfo<'gc>,
    linear: LinearProgram<'gc>,
    constant_slots: Vec<JitSlot<'gc>>,
    cache_slots: Vec<JitSlot<'gc>>,
    code_block_slots: Vec<JitCodeBlockSlot<'gc>>,
    procedures: Vec<Box<JitProcedureState<'gc>>>,
    options: CompilationOptions,
}

impl<'gc> JitModuleState<'gc> {
    fn new(
        ctx: Context<'gc>,
        reify_info: ReifyInfo<'gc>,
        linear: LinearProgram<'gc>,
        options: CompilationOptions,
    ) -> Self {
        let unit_id = NEXT_JIT_UNIT_ID.fetch_add(1, Ordering::Relaxed);
        let mut constant_slots = collect_constant_slots(&linear);
        let mut cache_slots = collect_cache_slots(&linear);
        let mut code_block_slots = Vec::with_capacity(linear.procedures.len());
        let mut procedures = Vec::with_capacity(linear.procedures.len());

        for (index, slot) in constant_slots.iter_mut().enumerate() {
            slot.symbol = format!("capy_jit_u{unit_id}_constant_{index}");
        }
        for (index, slot) in cache_slots.iter_mut().enumerate() {
            slot.symbol = format!("capy_jit_u{unit_id}_cache_cell_{index}");
        }

        for (index, procedure) in linear.procedures.iter().cloned().enumerate() {
            let procedure_state = Box::new(JitProcedureState {
                procedure,
                compiled_entry: AtomicUsize::new(0),
                status: Monitor::new(JitCompileStatus::Idle),
            });
            let code_block = CodeBlock::new_lazy_jit(
                ctx,
                lazy_jit_stub_entrypoint(),
                arity_for_procedure(&procedure_state.procedure),
                procedure_state.procedure.kind == ProcedureKind::Continuation,
                procedure_state.procedure.meta,
                LazyJitHandle {
                    program: std::ptr::null(),
                    procedure: index,
                },
            );
            code_block_slots.push(JitCodeBlockSlot {
                code: procedure_state.procedure.code,
                value: code_block.into(),
                symbol: format!("capy_jit_u{unit_id}_code_block_{index}"),
            });
            procedures.push(procedure_state);
        }

        let entry_code_block = code_block_slots
            .iter()
            .find_map(|slot| {
                (slot.code == linear.entry).then(|| slot.value.downcast::<CodeBlock>())
            })
            .expect("linear JIT module should contain entrypoint code block");
        let entrypoint = Closure::new(ctx, entry_code_block, &[], false).into();

        Self {
            unit_id,
            entrypoint,
            reify_info,
            linear,
            constant_slots,
            cache_slots,
            code_block_slots,
            procedures,
            options,
        }
    }

    fn initialize_lazy_handles(&self, ctx: Context<'gc>, program: *const ()) {
        for (procedure, slot) in self.code_block_slots.iter().enumerate() {
            let code_block = slot.value.downcast::<CodeBlock>();
            CodeBlock::patch_lazy_jit_handle(ctx, code_block, LazyJitHandle { program, procedure });
        }
    }

    fn register_external_symbols(&self) {
        let mut symbols = JIT_EXTERNAL_SYMBOLS
            .write()
            .expect("JIT external symbol registry should not be poisoned");

        for slot in &self.constant_slots {
            symbols.insert(
                slot.symbol.clone(),
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
        }
        for slot in &self.cache_slots {
            symbols.insert(
                slot.symbol.clone(),
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
        }
        for slot in &self.code_block_slots {
            symbols.insert(
                slot.symbol.clone(),
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
        }
    }

    fn imported_slots(&self) -> ImportedDataSlots<'gc> {
        ImportedDataSlots {
            constants: self
                .constant_slots
                .iter()
                .map(|slot| (slot.key, slot.symbol.clone()))
                .collect(),
            cache_cells: self
                .cache_slots
                .iter()
                .map(|slot| (slot.key, slot.symbol.clone()))
                .collect(),
            global_side_metadata_base_address: JIT_GLOBAL_SIDE_METADATA_SYMBOL.to_owned(),
        }
    }

    fn ensure_compiled(&self, _ctx: Context<'gc>, procedure: usize) -> Address {
        let procedure_state = self
            .procedures
            .get(procedure)
            .unwrap_or_else(|| panic!("lazy JIT procedure index {procedure} is out of bounds"));
        if let Some(compiled) = procedure_state.compiled_address() {
            return compiled;
        }

        let mut enqueue = false;
        {
            let mut status = procedure_state.status.lock();
            match &*status {
                JitCompileStatus::Idle => {
                    *status = JitCompileStatus::Queued;
                    enqueue = true;
                }
                JitCompileStatus::Queued | JitCompileStatus::Compiling => {}
                JitCompileStatus::Compiled(entrypoint) => {
                    return unsafe { Address::from_usize(*entrypoint) };
                }
                JitCompileStatus::Failed(message) => {
                    panic!("lazy JIT compilation failed: {message}");
                }
            }
        }

        if enqueue {
            let job = JitCompileJob {
                program: self as *const Self as usize,
                procedure,
            };
            if let Err(message) = JIT_WORKER_POOL.enqueue(job) {
                self.fail_background_compile(procedure, message.clone());
                panic!("lazy JIT compilation failed: {message}");
            }
        }

        procedure_state.wait_for_compilation()
    }

    fn start_background_compile(&self, procedure: usize) -> bool {
        let procedure_state = self
            .procedures
            .get(procedure)
            .unwrap_or_else(|| panic!("lazy JIT procedure index {procedure} is out of bounds"));
        let mut status = procedure_state.status.lock();
        match &*status {
            JitCompileStatus::Queued | JitCompileStatus::Idle => {
                *status = JitCompileStatus::Compiling;
                true
            }
            JitCompileStatus::Compiling => false,
            JitCompileStatus::Compiled(_) | JitCompileStatus::Failed(_) => false,
        }
    }

    fn finish_background_compile(&self, ctx: Context<'gc>, procedure: usize, entrypoint: Address) {
        let procedure_state = self
            .procedures
            .get(procedure)
            .unwrap_or_else(|| panic!("lazy JIT procedure index {procedure} is out of bounds"));
        if let Some(slot) = self.code_block_slots.get(procedure) {
            CodeBlock::patch_entrypoint(ctx, slot.value.downcast::<CodeBlock>(), entrypoint);
        }
        procedure_state
            .compiled_entry
            .store(entrypoint.as_usize(), Ordering::Release);

        let mut status = procedure_state.status.lock();
        *status = JitCompileStatus::Compiled(entrypoint.as_usize());
        status.notify_all();
    }

    fn fail_background_compile(&self, procedure: usize, message: String) {
        let procedure_state = self
            .procedures
            .get(procedure)
            .unwrap_or_else(|| panic!("lazy JIT procedure index {procedure} is out of bounds"));
        let mut status = procedure_state.status.lock();
        *status = JitCompileStatus::Failed(message);
        status.notify_all();
    }

    fn reify_info_for_worker(&self) -> ReifyInfo<'gc> {
        ReifyInfo {
            entrypoint: self.reify_info.entrypoint,
            functions: self.reify_info.functions,
            continuations: self.reify_info.continuations,
            free_vars: crate::cps::free_vars::FreeVars {
                fvars: self.reify_info.free_vars.fvars.clone(),
                cvars: self.reify_info.free_vars.cvars.clone(),
                funcs: self.reify_info.free_vars.funcs.clone(),
                conts: self.reify_info.free_vars.conts.clone(),
                cvals: self.reify_info.free_vars.cvals.clone(),
            },
        }
    }

    fn code_block_symbol(&self, code: CodeId) -> &str {
        self.code_block_slots
            .iter()
            .find_map(|slot| (slot.code == code).then_some(slot.symbol.as_str()))
            .expect("JIT code block slot should exist")
    }
}

#[derive(Clone)]
struct JitUnitDecls {
    funcs_by_code: HashMap<CodeId, FuncId>,
    code_blocks_by_code: HashMap<CodeId, DataId>,
    constants_by_symbol: HashMap<String, DataId>,
    cache_cells_by_symbol: HashMap<String, DataId>,
}

struct JitWorker {
    module: JITModule,
    units: HashMap<JitUnitId, JitUnitDecls>,
}

impl JitWorker {
    fn new() -> Self {
        Self {
            module: JITModule::new(build_jit_builder()),
            units: HashMap::new(),
        }
    }

    fn ensure_unit_declared<'gc>(&mut self, program: &JitModuleState<'gc>) -> JitUnitDecls {
        if let Some(decls) = self.units.get(&program.unit_id) {
            return decls.clone();
        }

        let sig = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
        let mut funcs_by_code = HashMap::new();
        let mut code_blocks_by_code = HashMap::new();
        let mut constants_by_symbol = HashMap::new();
        let mut cache_cells_by_symbol = HashMap::new();

        for procedure in &program.linear.procedures {
            let name = jit_function_name(program.unit_id, procedure);
            let func_id = self
                .module
                .declare_function(&name, Linkage::Export, &sig)
                .expect("failed to declare JIT procedure");
            funcs_by_code.insert(procedure.code, func_id);

            let symbol = program.code_block_symbol(procedure.code);
            let data_id = self
                .module
                .declare_data(symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT code block data");
            code_blocks_by_code.insert(procedure.code, data_id);
        }

        for slot in &program.constant_slots {
            let data_id = self
                .module
                .declare_data(&slot.symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT constant data");
            constants_by_symbol.insert(slot.symbol.clone(), data_id);
        }

        for slot in &program.cache_slots {
            let data_id = self
                .module
                .declare_data(&slot.symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT cache cell data");
            cache_cells_by_symbol.insert(slot.symbol.clone(), data_id);
        }

        let decls = JitUnitDecls {
            funcs_by_code,
            code_blocks_by_code,
            constants_by_symbol,
            cache_cells_by_symbol,
        };
        self.units.insert(program.unit_id, decls.clone());
        decls
    }

    fn compile_procedure<'gc>(
        &mut self,
        ctx: Context<'gc>,
        program: &JitModuleState<'gc>,
        procedure_index: usize,
    ) -> Address {
        let decls = self.ensure_unit_declared(program);
        let imported_slots = program.imported_slots();

        let mut module_builder = ModuleBuilder::new_with_imported_data(
            ctx,
            &mut self.module,
            program.reify_info_for_worker(),
            program.linear.clone(),
            imported_slots,
        );
        module_builder.stacktraces = program.options.backtraces;
        module_builder.direct_calls = false;
        module_builder.func_for_code = decls.funcs_by_code.clone();
        module_builder.code_block_for_code = decls.code_blocks_by_code.clone();
        for slot in &program.constant_slots {
            let data_id = decls.constants_by_symbol[&slot.symbol];
            module_builder
                .constants
                .insert(ValueEqual(slot.key), data_id);
        }
        for slot in &program.cache_slots {
            let data_id = decls.cache_cells_by_symbol[&slot.symbol];
            module_builder
                .cache_cells
                .insert(ValueEqual(slot.key), data_id);
        }

        let procedure = &program.procedures[procedure_index].procedure;
        let func_id = module_builder.func_for_code[&procedure.code];
        let sig = call_signature!(Tail (I64, I64, I64) -> (I64, I64));

        let mut context = module_builder.module.make_context();
        context.func.signature = sig;
        let mut fctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
        let thunks = crate::runtime::vm::thunks::ImportedThunks::new(
            &module_builder.thunks,
            &mut builder.func,
            &mut module_builder.module,
        );
        let function_name = jit_function_name(program.unit_id, procedure);
        let func_debug_cx = module_builder
            .debug_context
            .define_linear_procedure(procedure, &function_name);
        let mut ssa = SSABuilder::new(
            &mut module_builder,
            builder,
            ContOrFunc::Procedure(procedure.clone()),
            thunks,
            func_debug_cx,
        );
        ssa.linear_procedure(procedure);
        ssa.finalize();
        ssa.builder.seal_all_blocks();
        ssa.builder.finalize();
        module_builder
            .module
            .define_function(func_id, &mut context)
            .unwrap_or_else(|err| panic!("failed to define JIT procedure: {err}"));
        module_builder.module.clear_context(&mut context);
        module_builder
            .module
            .finalize_definitions()
            .expect("failed to finalize JIT procedure");

        Address::from_ptr(module_builder.module.get_finalized_function(func_id))
    }
}

struct JitWorkerPool {
    sender: crossbeam_channel::Sender<JitCompileJob>,
}

impl JitWorkerPool {
    fn new() -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        for worker_id in 0..jit_worker_count() {
            let receiver = receiver.clone();
            std::thread::Builder::new()
                .name(format!("capy-jit-worker-{worker_id}"))
                .spawn(move || jit_worker_loop(receiver))
                .expect("failed to spawn JIT worker thread");
        }
        Self { sender }
    }

    fn enqueue(&self, job: JitCompileJob) -> Result<(), String> {
        self.sender
            .send(job)
            .map_err(|_| "JIT worker pool is shut down".to_owned())
    }
}

static JIT_WORKER_POOL: LazyLock<JitWorkerPool> = LazyLock::new(JitWorkerPool::new);

fn jit_worker_loop(receiver: crossbeam_channel::Receiver<JitCompileJob>) {
    let scheme = Scheme::new_uninit();
    let mut worker = JitWorker::new();

    while let Ok(job) = receiver.recv() {
        let result = catch_unwind(AssertUnwindSafe(|| {
            scheme.enter(|ctx| {
                let program = unsafe { &*(job.program as *const JitModuleState<'_>) };
                if !program.start_background_compile(job.procedure) {
                    return;
                }

                let entrypoint = worker.compile_procedure(ctx, program, job.procedure);
                program.finish_background_compile(ctx, job.procedure, entrypoint);
            });
        }));

        if let Err(payload) = result {
            let message = panic_payload_to_string(payload);
            unsafe {
                let program = &*(job.program as *const JitModuleState<'static>);
                program.fail_background_compile(job.procedure, message);
            }
        }
    }
}

fn panic_payload_to_string(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_owned()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "JIT worker panicked".to_owned()
    }
}

fn jit_worker_count() -> usize {
    let available = std::thread::available_parallelism().map_or(1, usize::from);
    parse_jit_worker_count(std::env::var("CAPY_JIT_WORKERS").ok().as_deref(), available)
}

fn parse_jit_worker_count(value: Option<&str>, available_parallelism: usize) -> usize {
    if let Some(value) = value
        && let Ok(parsed) = value.parse::<usize>()
        && parsed != 0
    {
        return parsed;
    }

    available_parallelism.clamp(1, 4)
}

fn jit_function_name(unit_id: JitUnitId, procedure: &Procedure<'_>) -> String {
    let prefix = match procedure.kind {
        ProcedureKind::Function => format!("jit_u{unit_id}_fn"),
        ProcedureKind::Continuation => format!("jit_u{unit_id}_cont"),
    };
    procedure_linkage_name(&prefix, procedure.code.0 as usize, procedure)
}

fn arity_for_procedure(procedure: &Procedure<'_>) -> CodeArity {
    if procedure.variadic.is_some() {
        CodeArity::variadic(procedure.params.len())
    } else {
        CodeArity::new(procedure.params.len() as i32)
    }
}

fn collect_constant_slots<'gc>(linear: &LinearProgram<'gc>) -> Vec<JitSlot<'gc>> {
    let mut slots = Vec::new();
    for procedure in &linear.procedures {
        intern_constant_slot(&mut slots, procedure.name);
        intern_constant_slot(&mut slots, procedure.source);
        intern_constant_slot(&mut slots, procedure.meta);
        for block in &procedure.blocks {
            intern_constant_slot(&mut slots, block.source);
            for instruction in &block.instructions {
                collect_instruction_constants(&mut slots, instruction);
            }
            for atom in block.terminator.uses() {
                if let crate::cps::linear::LinearAtom::Constant(value) = atom {
                    intern_constant_slot(&mut slots, value);
                }
            }
            if let crate::cps::linear::Terminator::Switch { cases, .. } = &block.terminator {
                for case in cases {
                    if let crate::cps::linear::SwitchCaseValue::Symbol { value, .. } = case.value {
                        intern_constant_slot(&mut slots, value);
                    }
                }
            }
        }
    }
    slots
}

fn collect_instruction_constants<'gc>(
    slots: &mut Vec<JitSlot<'gc>>,
    instruction: &Instruction<'gc>,
) {
    for atom in instruction.uses() {
        if let crate::cps::linear::LinearAtom::Constant(value) = atom {
            intern_constant_slot(slots, value);
        }
    }
    match instruction {
        Instruction::Const { value, .. } => intern_constant_slot(slots, *value),
        Instruction::CacheRef { source, .. }
        | Instruction::CacheSet { source, .. }
        | Instruction::PrimCall { source, .. }
        | Instruction::RestToList { source, .. }
        | Instruction::RestRef { source, .. }
        | Instruction::RestLength { source, .. }
        | Instruction::RestPredicate { source, .. } => intern_constant_slot(slots, *source),
        Instruction::MakeClosure { .. }
        | Instruction::ClosureRef { .. }
        | Instruction::ClosureSet { .. } => {}
    }
}

fn collect_cache_slots<'gc>(linear: &LinearProgram<'gc>) -> Vec<JitSlot<'gc>> {
    let mut slots = Vec::new();
    for procedure in &linear.procedures {
        for block in &procedure.blocks {
            for instruction in &block.instructions {
                match instruction {
                    Instruction::CacheRef {
                        cache_key: crate::cps::linear::LinearAtom::Constant(key),
                        ..
                    }
                    | Instruction::CacheSet {
                        cache_key: crate::cps::linear::LinearAtom::Constant(key),
                        ..
                    } => intern_slot(&mut slots, *key, Value::empty(), "capy_jit_cache_cell"),
                    _ => {}
                }
            }
        }
    }
    slots
}

fn intern_constant_slot<'gc>(slots: &mut Vec<JitSlot<'gc>>, value: Value<'gc>) {
    if !value.is_immediate() {
        intern_slot(slots, value, value, "capy_jit_constant");
    }
}

fn intern_slot<'gc>(
    slots: &mut Vec<JitSlot<'gc>>,
    key: Value<'gc>,
    initial: Value<'gc>,
    prefix: &str,
) {
    if slots.iter().any(|slot| ValueEqual(slot.key) == key) {
        return;
    }
    let symbol = format!("{prefix}_{}", slots.len());
    slots.push(JitSlot {
        key,
        value: initial,
        symbol,
    });
}

unsafe impl<'gc> Trace for JitSlot<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.key.trace(visitor);
            self.value.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.key.process_weak_refs(weak_processor);
            self.value.process_weak_refs(weak_processor);
        }
    }
}

unsafe impl<'gc> Trace for JitCodeBlockSlot<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.code.trace(visitor);
            self.value.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.code.process_weak_refs(weak_processor);
            self.value.process_weak_refs(weak_processor);
        }
    }
}

unsafe impl<'gc> Trace for JitProcedureState<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.procedure.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.procedure.process_weak_refs(weak_processor);
        }
    }
}

unsafe impl<'gc> Trace for JitModuleState<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.entrypoint.trace(visitor);
            self.reify_info.entrypoint.trace(visitor);
            self.reify_info.functions.trace(visitor);
            self.reify_info.continuations.trace(visitor);
            self.linear.trace(visitor);
            self.constant_slots.trace(visitor);
            self.cache_slots.trace(visitor);
            self.code_block_slots.trace(visitor);
            self.procedures.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            self.entrypoint.process_weak_refs(weak_processor);
            self.reify_info.entrypoint.process_weak_refs(weak_processor);
            self.reify_info.functions.process_weak_refs(weak_processor);
            self.reify_info
                .continuations
                .process_weak_refs(weak_processor);
            self.linear.process_weak_refs(weak_processor);
            self.constant_slots.process_weak_refs(weak_processor);
            self.cache_slots.process_weak_refs(weak_processor);
            self.code_block_slots.process_weak_refs(weak_processor);
            self.procedures.process_weak_refs(weak_processor);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{JitModuleState, capy_jit_resolve, compile_cps_to_jit_thunk};
    use crate::{
        compiler::CompilationOptions,
        cps::term::{Atoms, Func, Term},
        expander::core::fresh_lvar,
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Closure, CodeBlockKind, Symbol, Value},
        },
    };

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[crate::cps::term::Atom<'gc>]) -> Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    #[test]
    fn compile_cps_to_jit_thunk_returns_lazy_entry_closure() {
        with_ctx(|ctx| {
            let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "jit-test").into());
            let retk = fresh_lvar(ctx, Symbol::from_str(ctx, "jit-retk").into());
            let body = Gc::new(
                *ctx,
                Term::Continue(retk, atoms(ctx, &[]), Value::new(false)),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Symbol::from_str(ctx, "jit-test").into(),
                    source: Value::new(false),
                    binding,
                    return_cont: retk,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let thunk = compile_cps_to_jit_thunk(ctx, func, CompilationOptions::default())
                .expect("JIT thunk creation should succeed");
            let closure = thunk.downcast::<Closure>();

            assert!(matches!(
                closure.code_block.kind,
                CodeBlockKind::LazyJit { .. }
            ));
            assert_eq!(closure.code, closure.code_block.entrypoint);
            let handle = closure
                .code_block
                .lazy_jit_handle()
                .expect("entry code block should have lazy JIT handle");
            assert!(!handle.program.is_null());
            let state = unsafe { &*(handle.program.cast::<JitModuleState<'_>>()) };
            assert_eq!(
                state.procedures[handle.procedure].procedure.code,
                state.linear.entry
            );
            assert_eq!(
                state.procedures[handle.procedure]
                    .compiled_entry
                    .load(std::sync::atomic::Ordering::Acquire),
                0
            );

            let lazy_entrypoint = closure.code;
            let compiled = capy_jit_resolve(ctx, thunk);

            assert_ne!(compiled, lazy_entrypoint.to_ptr::<u8>());
            assert_eq!(closure.code.to_ptr::<u8>(), compiled);
            assert_eq!(closure.code_block.entrypoint.to_ptr::<u8>(), compiled);
            assert_eq!(
                state.procedures[handle.procedure]
                    .compiled_entry
                    .load(std::sync::atomic::Ordering::Acquire),
                compiled as usize
            );

            let compiled_again = capy_jit_resolve(ctx, thunk);
            assert_eq!(compiled_again, compiled);
            assert_eq!(
                state.procedures[handle.procedure]
                    .compiled_entry
                    .load(std::sync::atomic::Ordering::Acquire),
                compiled as usize
            );
        });
    }

    #[test]
    fn shared_lazy_code_block_syncs_second_closure_without_recompile() {
        with_ctx(|ctx| {
            let binding = fresh_lvar(ctx, Symbol::from_str(ctx, "jit-shared").into());
            let retk = fresh_lvar(ctx, Symbol::from_str(ctx, "jit-shared-retk").into());
            let body = Gc::new(
                *ctx,
                Term::Continue(retk, atoms(ctx, &[]), Value::new(false)),
            );
            let func = Gc::new(
                *ctx,
                Func {
                    name: Symbol::from_str(ctx, "jit-shared").into(),
                    source: Value::new(false),
                    binding,
                    return_cont: retk,
                    args: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let thunk = compile_cps_to_jit_thunk(ctx, func, CompilationOptions::default())
                .expect("JIT thunk creation should succeed");
            let first = thunk.downcast::<Closure>();
            let second: Value<'_> = Closure::new(ctx, first.code_block, &[], false).into();
            let second_closure = second.downcast::<Closure>();

            assert_eq!(first.code, second_closure.code);
            let lazy_entrypoint = first.code;
            let compiled = capy_jit_resolve(ctx, thunk);

            assert_ne!(compiled, lazy_entrypoint.to_ptr::<u8>());
            assert_eq!(first.code_block.entrypoint.to_ptr::<u8>(), compiled);
            assert_eq!(second_closure.code, lazy_entrypoint);

            let second_compiled = capy_jit_resolve(ctx, second);
            assert_eq!(second_compiled, compiled);
            assert_eq!(second_closure.code.to_ptr::<u8>(), compiled);
        });
    }

    #[test]
    fn jit_worker_count_env_parsing_uses_valid_positive_value() {
        assert_eq!(super::parse_jit_worker_count(Some("7"), 2), 7);
    }

    #[test]
    fn jit_worker_count_env_parsing_falls_back_to_capped_parallelism() {
        assert_eq!(super::parse_jit_worker_count(None, 8), 4);
        assert_eq!(super::parse_jit_worker_count(Some("0"), 8), 4);
        assert_eq!(super::parse_jit_worker_count(Some("invalid"), 2), 2);
        assert_eq!(super::parse_jit_worker_count(None, 0), 1);
    }
}

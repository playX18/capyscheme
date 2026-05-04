use std::sync::{
    LazyLock, Mutex,
    atomic::{AtomicUsize, Ordering},
};

use crate::{
    compiler::{
        CompilationOptions,
        ssa::{ContOrFunc, ImportedDataSlots, ModuleBuilder, SSABuilder},
    },
    cps::{
        ReifyInfo,
        linear::{CodeId, Instruction, LinearProgram, Procedure, ProcedureKind, linearize},
        reify,
        term::FuncRef,
    },
    rsgc::{Trace, Visitor, mmtk::util::Address},
    runtime::{
        Context,
        value::{Closure, CodeArity, CodeBlock, Value, ValueEqual},
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
use cranelift_module::{Linkage, Module, default_libcall_names};
use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

const JIT_GLOBAL_SIDE_METADATA_SYMBOL: &str = "capy_jit_global_side_metadata_base_address";

static JIT_GLOBAL_SIDE_METADATA_VALUE: LazyLock<usize> =
    LazyLock::new(|| global_side_metadata_vm_base_address().as_usize());

pub(crate) fn compile_cps_to_jit_thunk<'gc>(
    ctx: Context<'gc>,
    cps: FuncRef<'gc>,
    options: CompilationOptions,
) -> Result<Value<'gc>, Value<'gc>> {
    let reify_info = reify(ctx, cps);
    let linear = linearize(&reify_info);
    let state = Box::new(JitModuleState::new(ctx, reify_info, linear, options));
    let state_ptr = &*state as *const JitModuleState<'gc> as usize;
    state.initialize_parents(state_ptr);
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
    let closure = rator.downcast::<Closure>();
    let stub = lazy_jit_stub_entrypoint();
    if closure.code != stub {
        return closure.code.to_ptr();
    }

    let code_block = closure.code_block;
    if code_block.entrypoint != stub {
        Closure::sync_entrypoint_from_code_block(ctx, closure);
        return code_block.entrypoint.to_ptr();
    }

    let procedure = code_block
        .lazy_jit_state()
        .expect("lazy JIT code block should contain procedure state");
    let procedure = unsafe { &*(procedure.cast::<JitProcedureState<'gc>>()) };
    let entrypoint = procedure.ensure_compiled(ctx);
    Closure::patch_code_entrypoint(ctx, closure, entrypoint);
    entrypoint.to_ptr()
}

struct JitSlot<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
    symbol: String,
}

struct JitCodeBlockSlot<'gc> {
    code: CodeId<'gc>,
    value: Value<'gc>,
    symbol: String,
}

struct JitProcedureState<'gc> {
    parent: AtomicUsize,
    procedure: Procedure<'gc>,
    code_block: Option<crate::rsgc::Gc<'gc, CodeBlock<'gc>>>,
    compiled_entry: AtomicUsize,
    compile_lock: Mutex<()>,
}

impl<'gc> JitProcedureState<'gc> {
    fn ensure_compiled(&self, ctx: Context<'gc>) -> Address {
        let compiled = self.compiled_entry.load(Ordering::Acquire);
        if compiled != 0 {
            return unsafe { Address::from_usize(compiled) };
        }

        let _guard = self
            .compile_lock
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let compiled = self.compiled_entry.load(Ordering::Acquire);
        if compiled != 0 {
            return unsafe { Address::from_usize(compiled) };
        }

        let compiled = self.parent().compile_procedure(ctx, self);
        self.compiled_entry
            .store(compiled.as_usize(), Ordering::Release);
        compiled
    }

    fn parent(&self) -> &JitModuleState<'gc> {
        let ptr = self.parent.load(Ordering::Acquire);
        assert_ne!(ptr, 0, "lazy JIT procedure parent was not initialized");
        unsafe { &*(ptr as *const JitModuleState<'gc>) }
    }
}

struct JitModuleState<'gc> {
    entrypoint: Value<'gc>,
    reify_info: ReifyInfo<'gc>,
    linear: LinearProgram<'gc>,
    constant_slots: Vec<JitSlot<'gc>>,
    cache_slots: Vec<JitSlot<'gc>>,
    code_block_slots: Vec<JitCodeBlockSlot<'gc>>,
    procedures: Vec<Box<JitProcedureState<'gc>>>,
    compiled_modules: Mutex<Vec<JITModule>>,
    options: CompilationOptions,
}

impl<'gc> JitModuleState<'gc> {
    fn new(
        ctx: Context<'gc>,
        reify_info: ReifyInfo<'gc>,
        linear: LinearProgram<'gc>,
        options: CompilationOptions,
    ) -> Self {
        let constant_slots = collect_constant_slots(&linear);
        let cache_slots = collect_cache_slots(&linear);
        let mut code_block_slots = Vec::with_capacity(linear.procedures.len());
        let mut procedures = Vec::with_capacity(linear.procedures.len());

        for procedure in linear.procedures.iter().cloned() {
            let mut procedure_state = Box::new(JitProcedureState {
                parent: AtomicUsize::new(0),
                procedure,
                code_block: None,
                compiled_entry: AtomicUsize::new(0),
                compile_lock: Mutex::new(()),
            });
            let procedure_ptr = &*procedure_state as *const JitProcedureState<'gc> as *const ();
            let code_block = CodeBlock::new_lazy_jit(
                ctx,
                lazy_jit_stub_entrypoint(),
                arity_for_procedure(&procedure_state.procedure),
                procedure_state.procedure.kind == ProcedureKind::Continuation,
                procedure_state.procedure.meta,
                procedure_ptr,
            );
            procedure_state.code_block = Some(code_block);
            code_block_slots.push(JitCodeBlockSlot {
                code: procedure_state.procedure.code,
                value: code_block.into(),
                symbol: format!("capy_jit_code_block_{}", code_block_slots.len()),
            });
            procedures.push(procedure_state);
        }

        let entry_code_block = code_block_slots
            .iter()
            .find_map(|slot| {
                (slot.code == CodeId::Function(reify_info.entrypoint))
                    .then(|| slot.value.downcast::<CodeBlock>())
            })
            .expect("linear JIT module should contain entrypoint code block");
        let entrypoint = Closure::new(ctx, entry_code_block, &[], false).into();

        Self {
            entrypoint,
            reify_info,
            linear,
            constant_slots,
            cache_slots,
            code_block_slots,
            procedures,
            compiled_modules: Mutex::new(Vec::new()),
            options,
        }
    }

    fn initialize_parents(&self, state_ptr: usize) {
        for procedure in &self.procedures {
            procedure.parent.store(state_ptr, Ordering::Release);
        }
    }

    fn compile_procedure(
        &self,
        ctx: Context<'gc>,
        procedure_state: &JitProcedureState<'gc>,
    ) -> Address {
        let mut builder = build_jit_builder();
        for slot in &self.constant_slots {
            builder.symbol(&slot.symbol, (&slot.value as *const Value<'gc>).cast());
        }
        for slot in &self.cache_slots {
            builder.symbol(&slot.symbol, (&slot.value as *const Value<'gc>).cast());
        }
        for slot in &self.code_block_slots {
            builder.symbol(&slot.symbol, (&slot.value as *const Value<'gc>).cast());
        }

        let module = JITModule::new(builder);
        let imported_slots = ImportedDataSlots {
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
        };

        let mut module_builder = ModuleBuilder::new_with_imported_data(
            ctx,
            module,
            self.reify_info_for_worker(),
            self.linear.clone(),
            imported_slots,
        );
        module_builder.stacktraces = self.options.backtraces;
        module_builder.direct_calls = false;

        let sig = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
        for procedure in &self.linear.procedures {
            match procedure.code {
                CodeId::Function(func) => {
                    let name = jit_function_name(procedure);
                    let func_id = module_builder
                        .module
                        .declare_function(&name, Linkage::Export, &sig)
                        .expect("failed to declare JIT function");
                    module_builder.func_for_func.insert(func, func_id);
                    let symbol = self.code_block_symbol(procedure.code);
                    let data_id = module_builder.declare_imported_code_block_slot(symbol);
                    module_builder.code_block_for_func.insert(func, data_id);
                }
                CodeId::Continuation(cont) => {
                    let name = jit_function_name(procedure);
                    let func_id = module_builder
                        .module
                        .declare_function(&name, Linkage::Export, &sig)
                        .expect("failed to declare JIT continuation");
                    module_builder.func_for_cont.insert(cont, func_id);
                    let symbol = self.code_block_symbol(procedure.code);
                    let data_id = module_builder.declare_imported_code_block_slot(symbol);
                    module_builder.code_block_for_cont.insert(cont, data_id);
                }
            }
        }

        let procedure = &procedure_state.procedure;
        let func_id = match procedure.code {
            CodeId::Function(func) => module_builder.func_for_func[&func],
            CodeId::Continuation(cont) => module_builder.func_for_cont[&cont],
        };

        let mut context = module_builder.module.make_context();
        context.func.signature = sig;
        let mut fctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
        let thunks = crate::runtime::vm::thunks::ImportedThunks::new(
            &module_builder.thunks,
            &mut builder.func,
            &mut module_builder.module,
        );
        let func_debug_cx = match procedure.code {
            CodeId::Function(func) => module_builder
                .debug_context
                .define_function(func, &jit_function_name(procedure)),
            CodeId::Continuation(cont) => module_builder
                .debug_context
                .define_cont(cont, &jit_function_name(procedure)),
        };
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
        let entrypoint = Address::from_ptr(module_builder.module.get_finalized_function(func_id));

        let module = module_builder.module;
        self.compiled_modules
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .push(module);
        entrypoint
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

    fn code_block_symbol(&self, code: CodeId<'gc>) -> &str {
        self.code_block_slots
            .iter()
            .find_map(|slot| (slot.code == code).then_some(slot.symbol.as_str()))
            .expect("JIT code block slot should exist")
    }
}

fn jit_function_name(procedure: &Procedure<'_>) -> String {
    match procedure.code {
        CodeId::Function(func) => format!("jit_fn:{}:{}", func.name, func.binding.name),
        CodeId::Continuation(cont) => format!("jit_cont:{}:{}", cont.name, cont.binding.name),
    }
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
            trace_code_id(&mut self.code, visitor);
            self.value.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            process_code_id_weak_refs(&mut self.code, weak_processor);
            self.value.process_weak_refs(weak_processor);
        }
    }
}

unsafe impl<'gc> Trace for JitProcedureState<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            trace_procedure(&mut self.procedure, visitor);
            self.code_block.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        unsafe {
            process_procedure_weak_refs(&mut self.procedure, weak_processor);
            self.code_block.process_weak_refs(weak_processor);
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
            trace_linear_program(&mut self.linear, visitor);
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
            process_linear_program_weak_refs(&mut self.linear, weak_processor);
            self.constant_slots.process_weak_refs(weak_processor);
            self.cache_slots.process_weak_refs(weak_processor);
            self.code_block_slots.process_weak_refs(weak_processor);
            self.procedures.process_weak_refs(weak_processor);
        }
    }
}

fn trace_linear_program<'gc>(program: &mut LinearProgram<'gc>, visitor: &mut Visitor) {
    unsafe {
        program.entry.trace(visitor);
        for procedure in &mut program.procedures {
            trace_procedure(procedure, visitor);
        }
    }
}

fn process_linear_program_weak_refs<'gc>(
    program: &mut LinearProgram<'gc>,
    weak_processor: &mut crate::rsgc::WeakProcessor,
) {
    unsafe {
        program.entry.process_weak_refs(weak_processor);
        for procedure in &mut program.procedures {
            process_procedure_weak_refs(procedure, weak_processor);
        }
    }
}

fn trace_procedure<'gc>(procedure: &mut Procedure<'gc>, visitor: &mut Visitor) {
    unsafe {
        trace_code_id(&mut procedure.code, visitor);
        procedure.name.trace(visitor);
        procedure.source.trace(visitor);
        procedure.meta.trace(visitor);
        for source in procedure.sources.values_mut() {
            source.trace(visitor);
        }
        for block in &mut procedure.blocks {
            block.source.trace(visitor);
            for instruction in &mut block.instructions {
                trace_instruction(instruction, visitor);
            }
            trace_terminator(&mut block.terminator, visitor);
        }
    }
}

fn process_procedure_weak_refs<'gc>(
    procedure: &mut Procedure<'gc>,
    weak_processor: &mut crate::rsgc::WeakProcessor,
) {
    unsafe {
        process_code_id_weak_refs(&mut procedure.code, weak_processor);
        procedure.name.process_weak_refs(weak_processor);
        procedure.source.process_weak_refs(weak_processor);
        procedure.meta.process_weak_refs(weak_processor);
        for source in procedure.sources.values_mut() {
            source.process_weak_refs(weak_processor);
        }
        for block in &mut procedure.blocks {
            block.source.process_weak_refs(weak_processor);
            for instruction in &mut block.instructions {
                process_instruction_weak_refs(instruction, weak_processor);
            }
            process_terminator_weak_refs(&mut block.terminator, weak_processor);
        }
    }
}

fn trace_code_id<'gc>(code: &mut CodeId<'gc>, visitor: &mut Visitor) {
    unsafe {
        match code {
            CodeId::Function(func) => func.trace(visitor),
            CodeId::Continuation(cont) => cont.trace(visitor),
        }
    }
}

fn process_code_id_weak_refs<'gc>(
    code: &mut CodeId<'gc>,
    weak_processor: &mut crate::rsgc::WeakProcessor,
) {
    unsafe {
        match code {
            CodeId::Function(func) => func.process_weak_refs(weak_processor),
            CodeId::Continuation(cont) => cont.process_weak_refs(weak_processor),
        }
    }
}

fn trace_instruction<'gc>(instruction: &mut Instruction<'gc>, visitor: &mut Visitor) {
    unsafe {
        match instruction {
            Instruction::Const { value, .. } => value.trace(visitor),
            Instruction::CacheRef { source, .. }
            | Instruction::CacheSet { source, .. }
            | Instruction::PrimCall { source, .. }
            | Instruction::RestToList { source, .. }
            | Instruction::RestRef { source, .. }
            | Instruction::RestLength { source, .. }
            | Instruction::RestPredicate { source, .. } => source.trace(visitor),
            Instruction::MakeClosure { code, .. } => trace_code_id(code, visitor),
            Instruction::ClosureRef { .. } | Instruction::ClosureSet { .. } => {}
        }
    }
}

fn process_instruction_weak_refs<'gc>(
    instruction: &mut Instruction<'gc>,
    weak_processor: &mut crate::rsgc::WeakProcessor,
) {
    unsafe {
        match instruction {
            Instruction::Const { value, .. } => value.process_weak_refs(weak_processor),
            Instruction::CacheRef { source, .. }
            | Instruction::CacheSet { source, .. }
            | Instruction::PrimCall { source, .. }
            | Instruction::RestToList { source, .. }
            | Instruction::RestRef { source, .. }
            | Instruction::RestLength { source, .. }
            | Instruction::RestPredicate { source, .. } => source.process_weak_refs(weak_processor),
            Instruction::MakeClosure { code, .. } => {
                process_code_id_weak_refs(code, weak_processor)
            }
            Instruction::ClosureRef { .. } | Instruction::ClosureSet { .. } => {}
        }
    }
}

fn trace_terminator<'gc>(
    terminator: &mut crate::cps::linear::Terminator<'gc>,
    visitor: &mut Visitor,
) {
    unsafe {
        match terminator {
            crate::cps::linear::Terminator::Call { source, .. }
            | crate::cps::linear::Terminator::TailCall { source, .. } => source.trace(visitor),
            crate::cps::linear::Terminator::Jump { .. }
            | crate::cps::linear::Terminator::Branch { .. } => {}
            crate::cps::linear::Terminator::Switch { cases, .. } => {
                for case in cases {
                    if let crate::cps::linear::SwitchCaseValue::Symbol { value, .. } =
                        &mut case.value
                    {
                        value.trace(visitor);
                    }
                }
            }
        }
    }
}

fn process_terminator_weak_refs<'gc>(
    terminator: &mut crate::cps::linear::Terminator<'gc>,
    weak_processor: &mut crate::rsgc::WeakProcessor,
) {
    unsafe {
        match terminator {
            crate::cps::linear::Terminator::Call { source, .. }
            | crate::cps::linear::Terminator::TailCall { source, .. } => {
                source.process_weak_refs(weak_processor)
            }
            crate::cps::linear::Terminator::Jump { .. }
            | crate::cps::linear::Terminator::Branch { .. } => {}
            crate::cps::linear::Terminator::Switch { cases, .. } => {
                for case in cases {
                    if let crate::cps::linear::SwitchCaseValue::Symbol { value, .. } =
                        &mut case.value
                    {
                        value.process_weak_refs(weak_processor);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{capy_jit_resolve, compile_cps_to_jit_thunk};
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

            let lazy_entrypoint = closure.code;
            let compiled = capy_jit_resolve(ctx, thunk);

            assert_ne!(compiled, lazy_entrypoint.to_ptr::<u8>());
            assert_eq!(closure.code.to_ptr::<u8>(), compiled);
            assert_eq!(closure.code_block.entrypoint.to_ptr::<u8>(), compiled);
        });
    }
}

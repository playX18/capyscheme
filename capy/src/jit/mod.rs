use std::{
    collections::HashMap,
    ffi::CString,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::{
        LazyLock,
        atomic::{AtomicU64, AtomicUsize, Ordering},
    },
};

mod helpers;
mod lower;
mod primitive;

use crate::{
    call_signature,
    compiler::CompilationOptions,
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
use asmkit::core::jit_allocator::{JitAllocator, JitAllocatorOptions, Span};
use cranelift::prelude::{
    Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, types,
};
use cranelift_codegen::{
    CodegenError, FinalizedMachReloc, FinalizedRelocTarget,
    binemit::{CodeOffset, Reloc},
    control::ControlPlane,
    ir::{self, AbiParam, ExternalName, KnownSymbol, LibCall},
    isa::{CallConv, OwnedTargetIsa, TargetIsa},
    settings,
};
use cranelift_module::{
    DataDescription, DataId, FuncId, FuncOrDataId, Linkage, Module, ModuleDeclarations,
    ModuleError, ModuleReloc, ModuleRelocTarget, ModuleResult, default_libcall_names,
};
use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

const JIT_GLOBAL_SIDE_METADATA_SYMBOL: &str = "capy_jit_global_side_metadata_base_address";

type JitUnitId = u64;

static NEXT_JIT_UNIT_ID: AtomicU64 = AtomicU64::new(1);

static JIT_GLOBAL_SIDE_METADATA_VALUE: LazyLock<usize> =
    LazyLock::new(|| global_side_metadata_vm_base_address().as_usize());

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
    let entrypoint = state.entrypoint;
    let libs = LIBRARY_COLLECTION.fetch(*ctx);
    Ok(libs.register_jit(JitLibrary::new(entrypoint, Vec::<Value<'gc>>::new(), state)))
}

static LAZY_JIT_STUB: LazyLock<Address> = LazyLock::new(build_lazy_jit_stub);

fn lazy_jit_stub_entrypoint() -> Address {
    *LAZY_JIT_STUB
}

fn build_lazy_jit_stub() -> Address {
    let mut module = CapyJitModule::new();
    module.define_symbol("capy_jit_resolve", capy_jit_resolve as *const u8 as usize);

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
            let mut sig = ir::Signature::new(CallConv::SystemV);
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

#[derive(Clone, Debug, PartialEq, Eq)]
enum JitRelocTarget {
    Procedure {
        unit_id: JitUnitId,
        index: usize,
        offset: CodeOffset,
    },
    FunctionOffset {
        function: FuncId,
        offset: CodeOffset,
    },
    ConstantSlot {
        unit_id: JitUnitId,
        index: usize,
    },
    CacheSlot {
        unit_id: JitUnitId,
        index: usize,
    },
    CodeBlockSlot {
        unit_id: JitUnitId,
        index: usize,
    },
    RuntimeSymbol {
        name: String,
    },
    LibCall {
        libcall: LibCall,
    },
    GlobalSideMetadataBaseAddress,
}

impl JitRelocTarget {
    fn can_use_local_call_veneer(&self) -> bool {
        matches!(
            self,
            Self::RuntimeSymbol { .. } | Self::LibCall { .. } | Self::GlobalSideMetadataBaseAddress
        )
    }
}

#[derive(Clone, Debug)]
struct JitRelocation {
    kind: Reloc,
    offset: CodeOffset,
    target: JitRelocTarget,
    addend: i64,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
struct JitCompiledFunctionRecord {
    code: Box<[u8]>,
    alignment: u64,
    target_triple: String,
    isa_fingerprint: String,
    relocations: Vec<JitRelocation>,
}

struct JitInstalledFunction {
    entrypoint: Address,
    record: JitCompiledFunctionRecord,
}

struct InstalledFunction {
    record: JitCompiledFunctionRecord,
    span: Span,
}

struct ResolvedRelocation<'a> {
    relocation: &'a JitRelocation,
    address: usize,
}

struct CapyJitModule {
    isa: OwnedTargetIsa,
    declarations: ModuleDeclarations,
    allocator: Box<JitAllocator>,
    symbols: HashMap<String, usize>,
    function_targets: HashMap<FuncId, JitRelocTarget>,
    data_targets: HashMap<DataId, JitRelocTarget>,
    data_addresses: HashMap<DataId, usize>,
    installed_procedures: HashMap<(JitUnitId, usize), usize>,
    compiled_functions: HashMap<FuncId, InstalledFunction>,
    libcall_names: Box<dyn Fn(LibCall) -> String + Send + Sync>,
}

impl CapyJitModule {
    fn new() -> Self {
        let mut options = JitAllocatorOptions::default();
        options.granularity = 256;

        let mut module = Self {
            isa: build_jit_isa(),
            declarations: ModuleDeclarations::default(),
            allocator: JitAllocator::new(options),
            symbols: HashMap::new(),
            function_targets: HashMap::new(),
            data_targets: HashMap::new(),
            data_addresses: HashMap::new(),
            installed_procedures: HashMap::new(),
            compiled_functions: HashMap::new(),
            libcall_names: default_libcall_names(),
        };

        for (name, address) in Thunks::symbols() {
            module.define_symbol(name, address as usize);
        }
        module
    }

    fn define_symbol(&mut self, name: impl Into<String>, address: usize) {
        self.symbols.insert(name.into(), address);
    }

    fn bind_function_target(&mut self, func: FuncId, target: JitRelocTarget) {
        self.function_targets.insert(func, target);
    }

    fn bind_data_target(&mut self, data: DataId, target: JitRelocTarget) {
        self.data_targets.insert(data, target);
    }

    fn bind_data_address(&mut self, data: DataId, address: usize) {
        self.data_addresses.insert(data, address);
    }

    fn get_finalized_function(&self, func: FuncId) -> *const u8 {
        self.compiled_functions
            .get(&func)
            .unwrap_or_else(|| panic!("function {func} was not compiled"))
            .span
            .rx()
    }

    fn take_compiled_record(&self, func: FuncId) -> JitCompiledFunctionRecord {
        self.compiled_functions
            .get(&func)
            .unwrap_or_else(|| panic!("function {func} was not compiled"))
            .record
            .clone()
    }

    fn stable_target_for_module_target(
        &self,
        target: &ModuleRelocTarget,
    ) -> Result<JitRelocTarget, String> {
        match *target {
            ModuleRelocTarget::User { namespace, index } => match namespace {
                0 => {
                    let func_id = FuncId::from_u32(index);
                    if let Some(target) = self.function_targets.get(&func_id) {
                        Ok(target.clone())
                    } else {
                        let decl = self.declarations.get_function_decl(func_id);
                        let name = decl.linkage_name(func_id).into_owned();
                        Ok(JitRelocTarget::RuntimeSymbol { name })
                    }
                }
                1 => {
                    let data_id = DataId::from_u32(index);
                    if let Some(target) = self.data_targets.get(&data_id) {
                        Ok(target.clone())
                    } else {
                        let decl = self.declarations.get_data_decl(data_id);
                        let name = decl.linkage_name(data_id).into_owned();
                        if name == JIT_GLOBAL_SIDE_METADATA_SYMBOL {
                            Ok(JitRelocTarget::GlobalSideMetadataBaseAddress)
                        } else {
                            Ok(JitRelocTarget::RuntimeSymbol { name })
                        }
                    }
                }
                _ => Err(format!("unsupported JIT relocation namespace {namespace}")),
            },
            ModuleRelocTarget::LibCall(libcall) => Ok(JitRelocTarget::LibCall { libcall }),
            ModuleRelocTarget::KnownSymbol(symbol) => Ok(JitRelocTarget::RuntimeSymbol {
                name: known_symbol_name(symbol),
            }),
            ModuleRelocTarget::FunctionOffset(func_id, offset) => {
                match self.function_targets.get(&func_id) {
                    Some(JitRelocTarget::Procedure { unit_id, index, .. }) => {
                        Ok(JitRelocTarget::Procedure {
                            unit_id: *unit_id,
                            index: *index,
                            offset,
                        })
                    }
                    Some(other) => Err(format!(
                        "function offset relocation points at non-procedure target {other:?}"
                    )),
                    None => Ok(JitRelocTarget::FunctionOffset {
                        function: func_id,
                        offset,
                    }),
                }
            }
        }
    }

    fn stable_target_for_finalized_reloc_target(
        &self,
        target: &FinalizedRelocTarget,
        func: &ir::Function,
        current_func: FuncId,
    ) -> Result<JitRelocTarget, String> {
        match target {
            FinalizedRelocTarget::ExternalName(ExternalName::User(reff)) => {
                let name = &func.params.user_named_funcs()[*reff];
                self.stable_target_for_module_target(&ModuleRelocTarget::user(
                    name.namespace,
                    name.index,
                ))
            }
            FinalizedRelocTarget::ExternalName(ExternalName::TestCase(name)) => {
                Err(format!("unsupported testcase relocation target {name:?}"))
            }
            FinalizedRelocTarget::ExternalName(ExternalName::LibCall(libcall)) => {
                Ok(JitRelocTarget::LibCall { libcall: *libcall })
            }
            FinalizedRelocTarget::ExternalName(ExternalName::KnownSymbol(symbol)) => {
                Ok(JitRelocTarget::RuntimeSymbol {
                    name: known_symbol_name(*symbol),
                })
            }
            FinalizedRelocTarget::Func(offset) => self.stable_target_for_module_target(
                &ModuleRelocTarget::FunctionOffset(current_func, *offset),
            ),
        }
    }

    fn relocation_from_mach_reloc(
        &self,
        reloc: &FinalizedMachReloc,
        func: &ir::Function,
        current_func: FuncId,
    ) -> Result<JitRelocation, String> {
        Ok(JitRelocation {
            kind: reloc.kind,
            offset: reloc.offset,
            target: self.stable_target_for_finalized_reloc_target(
                &reloc.target,
                func,
                current_func,
            )?,
            addend: reloc.addend,
        })
    }

    fn translate_relocations(
        &self,
        ctx: &cranelift_codegen::Context,
        func: FuncId,
    ) -> ModuleResult<Vec<JitRelocation>> {
        let compiled = ctx
            .compiled_code()
            .expect("Cranelift context should contain compiled code");
        compiled
            .buffer
            .relocs()
            .iter()
            .map(|reloc| {
                self.relocation_from_mach_reloc(reloc, &ctx.func, func)
                    .map_err(jit_module_error)
            })
            .collect()
    }

    fn target_address(
        &self,
        current_func: FuncId,
        current_entrypoint: usize,
        target: &JitRelocTarget,
    ) -> Result<usize, String> {
        match target {
            JitRelocTarget::Procedure {
                unit_id,
                index,
                offset,
            } => {
                if matches!(
                    self.function_targets.get(&current_func),
                    Some(JitRelocTarget::Procedure {
                        unit_id: current_unit,
                        index: current_index,
                        ..
                    }) if current_unit == unit_id && current_index == index
                ) {
                    return Ok(current_entrypoint + *offset as usize);
                }
                self.installed_procedures
                    .get(&(*unit_id, *index))
                    .map(|entry| entry + *offset as usize)
                    .ok_or_else(|| format!("JIT procedure {unit_id}:{index} is not installed"))
            }
            JitRelocTarget::FunctionOffset { function, offset } => {
                if *function == current_func {
                    Ok(current_entrypoint + *offset as usize)
                } else {
                    self.compiled_functions
                        .get(function)
                        .map(|function| function.span.rx() as usize + *offset as usize)
                        .ok_or_else(|| format!("JIT function {function} is not installed"))
                }
            }
            JitRelocTarget::ConstantSlot { unit_id, index } => self
                .data_targets
                .iter()
                .find_map(|(data, target)| {
                    matches!(target, JitRelocTarget::ConstantSlot { unit_id: other_unit, index: other_index } if other_unit == unit_id && other_index == index)
                        .then_some(*data)
                })
                .and_then(|data| self.data_address(data))
                .ok_or_else(|| format!("JIT constant slot {unit_id}:{index} has no address")),
            JitRelocTarget::CacheSlot { unit_id, index } => self
                .data_targets
                .iter()
                .find_map(|(data, target)| {
                    matches!(target, JitRelocTarget::CacheSlot { unit_id: other_unit, index: other_index } if other_unit == unit_id && other_index == index)
                        .then_some(*data)
                })
                .and_then(|data| self.data_address(data))
                .ok_or_else(|| format!("JIT cache slot {unit_id}:{index} has no address")),
            JitRelocTarget::CodeBlockSlot { unit_id, index } => self
                .data_targets
                .iter()
                .find_map(|(data, target)| {
                    matches!(target, JitRelocTarget::CodeBlockSlot { unit_id: other_unit, index: other_index } if other_unit == unit_id && other_index == index)
                        .then_some(*data)
                })
                .and_then(|data| self.data_address(data))
                .ok_or_else(|| format!("JIT code block slot {unit_id}:{index} has no address")),
            JitRelocTarget::RuntimeSymbol { name } => self
                .lookup_symbol(name)
                .ok_or_else(|| format!("cannot resolve runtime symbol {name}")),
            JitRelocTarget::LibCall { libcall } => {
                let name = (self.libcall_names)(*libcall);
                self.lookup_symbol(&name)
                    .ok_or_else(|| format!("cannot resolve libcall {name}"))
            }
            JitRelocTarget::GlobalSideMetadataBaseAddress => {
                Ok((&*JIT_GLOBAL_SIDE_METADATA_VALUE as *const usize).cast::<u8>() as usize)
            }
        }
    }

    fn data_address(&self, data: DataId) -> Option<usize> {
        self.data_addresses.get(&data).copied()
    }

    fn lookup_symbol(&self, name: &str) -> Option<usize> {
        self.symbols
            .get(name)
            .copied()
            .or_else(|| lookup_with_dlsym(name).map(|ptr| ptr as usize))
    }

    fn install_record(
        &mut self,
        func: FuncId,
        record: JitCompiledFunctionRecord,
    ) -> ModuleResult<Span> {
        let alloc_size = (record.code.len() + record.relocations.len() * MAX_VENEER_SIZE).max(1);
        let mut span = self
            .allocator
            .alloc(alloc_size)
            .map_err(|err| jit_allocation_error("unable to allocate JIT function", err))?;

        if span.rx() as usize % record.alignment.max(1) as usize != 0 {
            return Err(jit_module_error(format!(
                "asmkit returned function address {:p} that does not satisfy Cranelift alignment {}",
                span.rx(),
                record.alignment
            )));
        }

        let current_entrypoint = span.rx() as usize;
        let resolved = record
            .relocations
            .iter()
            .map(|relocation| {
                let address = self
                    .target_address(func, current_entrypoint, &relocation.target)
                    .map_err(jit_module_error)?;
                Ok(ResolvedRelocation {
                    relocation,
                    address,
                })
            })
            .collect::<ModuleResult<Vec<_>>>()?;

        let mut image = vec![0; alloc_size];
        image[..record.code.len()].copy_from_slice(&record.code);
        unsafe {
            self.allocator
                .copy_from_slice(&mut span, 0, &image)
                .map_err(|err| jit_allocation_error("unable to write JIT function", err))?;
        }

        let mut relocation_error = None;
        unsafe {
            self.allocator
                .write(&mut span, |span| {
                    if let Err(err) = apply_relocations(span, record.code.len(), &resolved) {
                        relocation_error = Some(err);
                    }
                })
                .map_err(|err| jit_allocation_error("unable to patch JIT function", err))?;
        }
        if let Some(err) = relocation_error {
            return Err(jit_module_error(err));
        }

        Ok(span)
    }
}

impl Module for CapyJitModule {
    fn isa(&self) -> &dyn TargetIsa {
        &*self.isa
    }

    fn declarations(&self) -> &ModuleDeclarations {
        &self.declarations
    }

    fn get_name(&self, name: &str) -> Option<FuncOrDataId> {
        self.declarations.get_name(name)
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &ir::Signature,
    ) -> ModuleResult<FuncId> {
        self.declarations
            .declare_function(name, linkage, signature)
            .map(|(id, _)| id)
    }

    fn declare_anonymous_function(&mut self, signature: &ir::Signature) -> ModuleResult<FuncId> {
        self.declarations.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        let (id, _) = self
            .declarations
            .declare_data(name, linkage, writable, tls)?;
        if name == JIT_GLOBAL_SIDE_METADATA_SYMBOL {
            self.bind_data_target(id, JitRelocTarget::GlobalSideMetadataBaseAddress);
        }
        Ok(id)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        self.declarations.declare_anonymous_data(writable, tls)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut cranelift_codegen::Context,
        ctrl_plane: &mut ControlPlane,
    ) -> ModuleResult<()> {
        let decl = self.declarations.get_function_decl(func);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(
                decl.linkage_name(func).into_owned(),
            ));
        }
        if self.compiled_functions.contains_key(&func) {
            return Err(ModuleError::DuplicateDefinition(
                decl.linkage_name(func).into_owned(),
            ));
        }

        let compiled = ctx.compile(self.isa(), ctrl_plane)?;
        let alignment = (compiled.buffer.alignment as u64)
            .max(self.isa.function_alignment().minimum as u64)
            .max(self.isa.symbol_alignment());
        let record = JitCompiledFunctionRecord {
            code: compiled.code_buffer().to_vec().into_boxed_slice(),
            alignment,
            target_triple: self.isa.triple().to_string(),
            isa_fingerprint: self.isa.to_string(),
            relocations: self.translate_relocations(ctx, func)?,
        };
        let span = self.install_record(func, record.clone())?;
        if let Some(JitRelocTarget::Procedure { unit_id, index, .. }) =
            self.function_targets.get(&func)
        {
            self.installed_procedures
                .insert((*unit_id, *index), span.rx() as usize);
        }
        self.compiled_functions
            .insert(func, InstalledFunction { record, span });
        Ok(())
    }

    fn define_function_bytes(
        &mut self,
        func: FuncId,
        alignment: u64,
        bytes: &[u8],
        relocs: &[ModuleReloc],
    ) -> ModuleResult<()> {
        let decl = self.declarations.get_function_decl(func);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(
                decl.linkage_name(func).into_owned(),
            ));
        }
        if self.compiled_functions.contains_key(&func) {
            return Err(ModuleError::DuplicateDefinition(
                decl.linkage_name(func).into_owned(),
            ));
        }

        let relocations = relocs
            .iter()
            .map(|reloc| {
                let target = self
                    .stable_target_for_module_target(&reloc.name)
                    .map_err(jit_module_error)?;
                Ok(JitRelocation {
                    kind: reloc.kind,
                    offset: reloc.offset,
                    target,
                    addend: reloc.addend,
                })
            })
            .collect::<ModuleResult<Vec<_>>>()?;
        let record = JitCompiledFunctionRecord {
            code: bytes.to_vec().into_boxed_slice(),
            alignment: alignment
                .max(self.isa.function_alignment().minimum as u64)
                .max(self.isa.symbol_alignment()),
            target_triple: self.isa.triple().to_string(),
            isa_fingerprint: self.isa.to_string(),
            relocations,
        };
        let span = self.install_record(func, record.clone())?;
        if let Some(JitRelocTarget::Procedure { unit_id, index, .. }) =
            self.function_targets.get(&func)
        {
            self.installed_procedures
                .insert((*unit_id, *index), span.rx() as usize);
        }
        self.compiled_functions
            .insert(func, InstalledFunction { record, span });
        Ok(())
    }

    fn define_data(&mut self, _data: DataId, _desc: &DataDescription) -> ModuleResult<()> {
        Err(jit_module_error(
            "Capy direct JIT module does not support owned data definitions",
        ))
    }
}

const MAX_VENEER_SIZE: usize = 24;

fn build_jit_isa() -> OwnedTargetIsa {
    let mut flag_builder = settings::builder();
    flag_builder
        .set("enable_heap_access_spectre_mitigation", "false")
        .unwrap();
    flag_builder.set("enable_pinned_reg", "true").unwrap();
    flag_builder.set("enable_probestack", "false").unwrap();
    flag_builder.set("opt_level", "speed").unwrap();
    flag_builder.set("preserve_frame_pointers", "true").unwrap();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();

    let triple = target_lexicon::Triple::host();
    let isa_builder = cranelift_codegen::isa::lookup(triple).unwrap_or_else(|err| panic!("{err}"));
    isa_builder
        .finish(settings::Flags::new(flag_builder))
        .expect("failed to build Cranelift JIT ISA")
}

fn jit_module_error(message: impl Into<String>) -> ModuleError {
    ModuleError::Compilation(CodegenError::Unsupported(message.into()))
}

fn jit_allocation_error(message: &'static str, err: impl std::fmt::Debug) -> ModuleError {
    ModuleError::Allocation {
        message,
        err: std::io::Error::new(std::io::ErrorKind::Other, format!("{err:?}")),
    }
}

fn known_symbol_name(symbol: KnownSymbol) -> String {
    match symbol {
        KnownSymbol::ElfGlobalOffsetTable => "_GLOBAL_OFFSET_TABLE_",
        KnownSymbol::CoffTlsIndex => "_tls_index",
    }
    .to_owned()
}

#[cfg(not(windows))]
fn lookup_with_dlsym(name: &str) -> Option<*const u8> {
    let c_str = CString::new(name).ok()?;
    let symbol = unsafe { libc::dlsym(libc::RTLD_DEFAULT, c_str.as_ptr()) };
    (!symbol.is_null()).then_some(symbol.cast::<u8>())
}

#[cfg(windows)]
fn lookup_with_dlsym(_name: &str) -> Option<*const u8> {
    None
}

unsafe fn modify_inst32(iptr: *mut u32, modifier: impl FnOnce(u32) -> u32) {
    let inst = unsafe { iptr.read_unaligned() };
    let new_inst = modifier(inst);
    unsafe {
        iptr.write_unaligned(new_inst);
    }
}

fn apply_relocations(
    span: &mut Span,
    code_len: usize,
    relocs: &[ResolvedRelocation<'_>],
) -> Result<(), String> {
    if span.rw().is_null() {
        return Err("asmkit JIT span is not directly writable during relocation".to_owned());
    }

    let mut veneer_offset = code_len;
    for (i, resolved) in relocs.iter().enumerate() {
        let relocation = resolved.relocation;
        let offset = relocation.offset as usize;
        if offset >= code_len {
            return Err(format!(
                "relocation offset {offset} is outside JIT function body of {code_len} bytes"
            ));
        }

        let at_rx = unsafe { span.rx().add(offset) as usize };
        let at_rw = unsafe { span.rw().add(offset) };
        let target_base = resolved.address;
        let target = address_with_addend(target_base, relocation.addend)?;

        match relocation.kind {
            Reloc::Abs4 => unsafe {
                std::ptr::write_unaligned(
                    at_rw.cast::<u32>(),
                    u32::try_from(target).map_err(|_| {
                        format!("absolute 32-bit relocation target {target:#x} is out of range")
                    })?,
                );
            },
            Reloc::Abs8 => unsafe {
                std::ptr::write_unaligned(at_rw.cast::<u64>(), target as u64);
            },
            Reloc::X86PCRel4 => {
                let pcrel = pcrel_i32(target, at_rx)?;
                unsafe {
                    std::ptr::write_unaligned(at_rw.cast::<i32>(), pcrel);
                }
            }
            Reloc::X86CallPCRel4 => {
                let target = match pcrel_i32(target, at_rx) {
                    Ok(pcrel) => pcrel,
                    Err(_) => {
                        if !relocation.target.can_use_local_call_veneer() {
                            return Err(format!(
                                "PC-relative relocation from {at_rx:#x} to {target:#x} is out of i32 range"
                            ));
                        }
                        let veneer =
                            write_x86_absolute_jump_veneer(span, &mut veneer_offset, target_base)?;
                        pcrel_i32(address_with_addend(veneer, relocation.addend)?, at_rx)?
                    }
                };
                unsafe {
                    std::ptr::write_unaligned(at_rw.cast::<i32>(), target);
                }
            }
            Reloc::X86GOTPCRel4 | Reloc::X86CallPLTRel4 => {
                return Err(format!(
                    "PIC/GOT/PLT relocation {:?} should not be generated for non-PIC JIT code",
                    relocation.kind
                ));
            }
            Reloc::S390xPCRel32Dbl | Reloc::S390xPLTRel32Dbl => {
                let pcrel = i32::try_from(((target as isize) - (at_rx as isize)) >> 1).map_err(
                    |_| {
                        format!(
                            "s390x PC-relative relocation from {at_rx:#x} to {target:#x} is out of range"
                        )
                    },
                )?;
                unsafe {
                    std::ptr::write_unaligned(at_rw.cast::<i32>(), pcrel);
                }
            }
            Reloc::Arm64Call => {
                let target = match arm64_call_imm26(target, at_rx) {
                    Ok(imm26) => imm26,
                    Err(_) => {
                        if !relocation.target.can_use_local_call_veneer() {
                            return Err(format!(
                                "AArch64 call relocation from {at_rx:#x} to {target:#x} is out of imm26 range"
                            ));
                        }
                        let veneer =
                            write_aarch64_absolute_branch_veneer(span, &mut veneer_offset, target)?;
                        arm64_call_imm26(veneer, at_rx)?
                    }
                };
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |inst| (inst & 0xfc00_0000) | target);
                }
            }
            Reloc::Aarch64AdrGotPage21 | Reloc::Aarch64Ld64GotLo12Nc => {
                return Err(format!(
                    "GOT relocation {:?} should not be generated for non-PIC JIT code",
                    relocation.kind
                ));
            }
            Reloc::Aarch64AdrPrelPgHi21 => {
                let get_page = |x: isize| x & !0xfff;
                let pcrel = i32::try_from(get_page(target as isize) - get_page(at_rx as isize))
                    .map_err(|_| {
                        format!(
                            "AArch64 page relocation from {at_rx:#x} to {target:#x} is out of range"
                        )
                    })?;
                let hi21 = (pcrel >> 12) as u32;
                let lo = (hi21 & 0x3) << 29;
                let hi = (hi21 & 0x1f_fffc) << 3;
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |inst| inst | lo | hi);
                }
            }
            Reloc::Aarch64AddAbsLo12Nc => {
                let imm12 = (target as u32 & 0xfff) << 10;
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |inst| inst | imm12);
                }
            }
            Reloc::RiscvCallPlt => {
                let pcrel = i32::try_from((target as isize) - (at_rx as isize)).map_err(|_| {
                    format!("RISC-V call relocation from {at_rx:#x} to {target:#x} is out of range")
                })? as u32;
                let hi20 = pcrel.wrapping_add(0x800) & 0xffff_f000;
                let lo12 = pcrel.wrapping_sub(hi20) & 0xfff;
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |auipc| (auipc & 0xfff) | hi20);
                    modify_inst32(at_rw.add(4).cast::<u32>(), |jalr| {
                        (jalr & 0x000f_ffff) | (lo12 << 20)
                    });
                }
            }
            Reloc::PulleyPcRel => {
                let pcrel = pcrel_i32(target, at_rx)?;
                unsafe {
                    let at = at_rw.cast::<i32>();
                    at.write_unaligned(at.read_unaligned().wrapping_add(pcrel));
                }
            }
            Reloc::RiscvPCRelHi20 => {
                let pcrel =
                    i32::try_from((target as isize) - (at_rx as isize) + 0x800).map_err(|_| {
                        format!(
                            "RISC-V hi20 relocation from {at_rx:#x} to {target:#x} is out of range"
                        )
                    })? as u32;
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |inst| inst | (pcrel & 0xffff_f000));
                }
            }
            Reloc::RiscvPCRelLo12I => {
                let prev = relocs
                    .get(i.wrapping_sub(1))
                    .ok_or_else(|| "RISC-V lo12 relocation did not follow hi20".to_owned())?;
                if prev.relocation.kind != Reloc::RiscvPCRelHi20 {
                    return Err("RISC-V lo12 relocation did not follow hi20".to_owned());
                }
                let hi_rx = unsafe { span.rx().add(prev.relocation.offset as usize) as usize };
                if target != hi_rx {
                    return Err(format!(
                        "RISC-V lo12 target {target:#x} does not match previous hi20 address {hi_rx:#x}"
                    ));
                }
                let hi_target = address_with_addend(prev.address, prev.relocation.addend)?;
                let pcrel = i32::try_from((hi_target as isize) - (hi_rx as isize))
                    .map_err(|_| {
                        format!(
                            "RISC-V lo12 relocation from {hi_rx:#x} to {hi_target:#x} is out of range"
                        )
                    })? as u32;
                unsafe {
                    modify_inst32(at_rw.cast::<u32>(), |inst| inst | ((pcrel & 0xfff) << 20));
                }
            }
            other => return Err(format!("unsupported JIT relocation {other:?}")),
        }
    }

    Ok(())
}

fn address_with_addend(address: usize, addend: i64) -> Result<usize, String> {
    if addend >= 0 {
        address
            .checked_add(addend as usize)
            .ok_or_else(|| format!("address {address:#x} plus addend {addend} overflowed"))
    } else {
        address
            .checked_sub(addend.unsigned_abs() as usize)
            .ok_or_else(|| format!("address {address:#x} plus addend {addend} underflowed"))
    }
}

fn pcrel_i32(target: usize, at: usize) -> Result<i32, String> {
    i32::try_from((target as isize) - (at as isize)).map_err(|_| {
        format!("PC-relative relocation from {at:#x} to {target:#x} is out of i32 range")
    })
}

fn arm64_call_imm26(target: usize, at: usize) -> Result<u32, String> {
    let diff = ((target as isize) - (at as isize)) >> 2;
    if (diff >> 26 == -1) || (diff >> 26 == 0) {
        let chop = 32 - 26;
        Ok((diff as u32) << chop >> chop)
    } else {
        Err(format!(
            "AArch64 call relocation from {at:#x} to {target:#x} is out of imm26 range"
        ))
    }
}

fn align_offset(offset: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (offset + align - 1) & !(align - 1)
}

fn reserve_veneer(
    span: &Span,
    veneer_offset: &mut usize,
    size: usize,
    align: usize,
) -> Result<usize, String> {
    let offset = align_offset(*veneer_offset, align);
    let end = offset
        .checked_add(size)
        .ok_or_else(|| "veneer allocation overflowed".to_owned())?;
    if end > span.size() {
        return Err(format!(
            "JIT function ran out of reserved veneer space: need {end}, span has {}",
            span.size()
        ));
    }
    *veneer_offset = end;
    Ok(offset)
}

fn write_x86_absolute_jump_veneer(
    span: &mut Span,
    veneer_offset: &mut usize,
    target: usize,
) -> Result<usize, String> {
    let offset = reserve_veneer(span, veneer_offset, 13, 1)?;
    let rw = unsafe { span.rw().add(offset) };
    unsafe {
        *rw.add(0) = 0x49;
        *rw.add(1) = 0xbb;
        std::ptr::write_unaligned(rw.add(2).cast::<u64>(), target as u64);
        *rw.add(10) = 0x41;
        *rw.add(11) = 0xff;
        *rw.add(12) = 0xe3;
    }
    Ok(unsafe { span.rx().add(offset) as usize })
}

fn write_aarch64_absolute_branch_veneer(
    span: &mut Span,
    veneer_offset: &mut usize,
    target: usize,
) -> Result<usize, String> {
    let offset = reserve_veneer(span, veneer_offset, 16, 8)?;
    let rw = unsafe { span.rw().add(offset) };
    unsafe {
        std::ptr::write_unaligned(rw.cast::<u32>(), 0x5800_0050);
        std::ptr::write_unaligned(rw.add(4).cast::<u32>(), 0xd61f_0200);
        std::ptr::write_unaligned(rw.add(8).cast::<u64>(), target as u64);
    }
    Ok(unsafe { span.rx().add(offset) as usize })
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
    compiled_code: Monitor<Option<JitCompiledFunctionRecord>>,
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
                compiled_code: Monitor::new(None),
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

    fn finish_background_compile(
        &self,
        ctx: Context<'gc>,
        procedure: usize,
        installed: JitInstalledFunction,
    ) {
        let procedure_state = self
            .procedures
            .get(procedure)
            .unwrap_or_else(|| panic!("lazy JIT procedure index {procedure} is out of bounds"));
        if let Some(slot) = self.code_block_slots.get(procedure) {
            CodeBlock::patch_entrypoint(
                ctx,
                slot.value.downcast::<CodeBlock>(),
                installed.entrypoint,
            );
        }
        *procedure_state.compiled_code.lock() = Some(installed.record);
        procedure_state
            .compiled_entry
            .store(installed.entrypoint.as_usize(), Ordering::Release);

        let mut status = procedure_state.status.lock();
        *status = JitCompileStatus::Compiled(installed.entrypoint.as_usize());
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
    module: CapyJitModule,
    units: HashMap<JitUnitId, JitUnitDecls>,
}

impl JitWorker {
    fn new() -> Self {
        Self {
            module: CapyJitModule::new(),
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

        for (index, procedure) in program.linear.procedures.iter().enumerate() {
            let name = jit_function_name(program.unit_id, procedure);
            let func_id = self
                .module
                .declare_function(&name, Linkage::Export, &sig)
                .expect("failed to declare JIT procedure");
            self.module.bind_function_target(
                func_id,
                JitRelocTarget::Procedure {
                    unit_id: program.unit_id,
                    index,
                    offset: 0,
                },
            );
            funcs_by_code.insert(procedure.code, func_id);

            let symbol = program.code_block_symbol(procedure.code);
            let data_id = self
                .module
                .declare_data(symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT code block data");
            self.module.bind_data_target(
                data_id,
                JitRelocTarget::CodeBlockSlot {
                    unit_id: program.unit_id,
                    index,
                },
            );
            self.module.bind_data_address(
                data_id,
                (&program.code_block_slots[index].value as *const Value<'gc>).cast::<u8>() as usize,
            );
            code_blocks_by_code.insert(procedure.code, data_id);
        }

        for (index, slot) in program.constant_slots.iter().enumerate() {
            let data_id = self
                .module
                .declare_data(&slot.symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT constant data");
            self.module.bind_data_target(
                data_id,
                JitRelocTarget::ConstantSlot {
                    unit_id: program.unit_id,
                    index,
                },
            );
            self.module.bind_data_address(
                data_id,
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
            constants_by_symbol.insert(slot.symbol.clone(), data_id);
        }

        for (index, slot) in program.cache_slots.iter().enumerate() {
            let data_id = self
                .module
                .declare_data(&slot.symbol, Linkage::Import, false, false)
                .expect("failed to declare JIT cache cell data");
            self.module.bind_data_target(
                data_id,
                JitRelocTarget::CacheSlot {
                    unit_id: program.unit_id,
                    index,
                },
            );
            self.module.bind_data_address(
                data_id,
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
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
    ) -> JitInstalledFunction {
        let decls = self.ensure_unit_declared(program);

        let mut module_builder = lower::JitModuleBuilder::new(
            ctx,
            &mut self.module,
            program.reify_info_for_worker(),
            JIT_GLOBAL_SIDE_METADATA_SYMBOL,
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
        let mut ssa = lower::JitLowerer::new(
            &mut module_builder,
            builder,
            lower::JitTarget::Procedure(procedure.clone()),
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
        let entrypoint = Address::from_ptr(module_builder.module.get_finalized_function(func_id));
        let record = module_builder.module.take_compiled_record(func_id);
        module_builder.module.clear_context(&mut context);

        JitInstalledFunction { entrypoint, record }
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

                let installed = worker.compile_procedure(ctx, program, job.procedure);
                program.finish_background_compile(ctx, job.procedure, installed);
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

fn procedure_binding_name<'gc>(procedure: &Procedure<'gc>) -> String {
    procedure
        .sources
        .get(&procedure.binding)
        .map(|binding| binding.name.to_string())
        .unwrap_or_else(|| format!("v{}", procedure.binding.0))
}

fn procedure_linkage_name<'gc>(prefix: &str, index: usize, procedure: &Procedure<'gc>) -> String {
    format!(
        "{}{}:{}:{}",
        prefix,
        index,
        procedure.name,
        procedure_binding_name(procedure)
    )
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
    use super::{
        CapyJitModule, JitModuleState, JitRelocTarget, capy_jit_resolve, compile_cps_to_jit_thunk,
    };
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
    use cranelift_codegen::{FinalizedMachReloc, FinalizedRelocTarget, binemit::Reloc, ir};
    use cranelift_module::{Linkage, Module, ModuleReloc, ModuleRelocTarget};

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[crate::cps::term::Atom<'gc>]) -> Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    #[test]
    fn direct_jit_records_stable_runtime_symbol_relocations() {
        let mut module = CapyJitModule::new();
        let sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        let func = module
            .declare_function("reloc_record_test", Linkage::Export, &sig)
            .unwrap();
        let data = module
            .declare_data("capy_test_reloc_symbol", Linkage::Import, false, false)
            .unwrap();
        let target = 0x1234_5678usize;
        module.define_symbol("capy_test_reloc_symbol", target);
        let reloc = ModuleReloc {
            kind: Reloc::Abs8,
            offset: 0,
            name: ModuleRelocTarget::user(1, data.as_u32()),
            addend: 0,
        };

        module
            .define_function_bytes(func, 8, &[0; 8], &[reloc])
            .unwrap();
        let record = module.take_compiled_record(func);

        assert!(matches!(
            &record.relocations[0].target,
            JitRelocTarget::RuntimeSymbol { name } if name == "capy_test_reloc_symbol"
        ));
        let patched =
            unsafe { (module.get_finalized_function(func) as *const u64).read_unaligned() };
        assert_eq!(patched, target as u64);
    }

    #[test]
    fn module_reloc_targets_translate_to_stable_slot_identities() {
        let mut module = CapyJitModule::new();
        let data = module
            .declare_data("capy_test_constant_slot", Linkage::Import, false, false)
            .unwrap();
        module.bind_data_target(
            data,
            JitRelocTarget::ConstantSlot {
                unit_id: 77,
                index: 5,
            },
        );

        let target = module
            .stable_target_for_module_target(&ModuleRelocTarget::user(1, data.as_u32()))
            .unwrap();

        assert_eq!(
            target,
            JitRelocTarget::ConstantSlot {
                unit_id: 77,
                index: 5
            }
        );
    }

    #[test]
    fn finalized_func_reloc_translates_to_current_procedure_offset() {
        let mut module = CapyJitModule::new();
        let sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        let func = module
            .declare_function("capy_test_func_reloc", Linkage::Export, &sig)
            .unwrap();
        module.bind_function_target(
            func,
            JitRelocTarget::Procedure {
                unit_id: 88,
                index: 9,
                offset: 0,
            },
        );
        let reloc = FinalizedMachReloc {
            kind: Reloc::X86PCRel4,
            offset: 4,
            target: FinalizedRelocTarget::Func(12),
            addend: -4,
        };

        let translated = module
            .relocation_from_mach_reloc(&reloc, &ir::Function::new(), func)
            .unwrap();

        assert_eq!(translated.kind, Reloc::X86PCRel4);
        assert_eq!(translated.offset, 4);
        assert_eq!(translated.addend, -4);
        assert_eq!(
            translated.target,
            JitRelocTarget::Procedure {
                unit_id: 88,
                index: 9,
                offset: 12
            }
        );
    }

    #[test]
    fn x86_call_veneer_keeps_raw_symbol_target() {
        let mut module = CapyJitModule::new();
        let sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        let func = module
            .declare_function("capy_test_veneer_source", Linkage::Export, &sig)
            .unwrap();
        let target_func = module
            .declare_function("capy_test_veneer_target", Linkage::Import, &sig)
            .unwrap();
        let far_target = usize::MAX - 0x1000;
        module.define_symbol("capy_test_veneer_target", far_target);
        let reloc = ModuleReloc {
            kind: Reloc::X86CallPCRel4,
            offset: 0,
            name: ModuleRelocTarget::user(0, target_func.as_u32()),
            addend: -4,
        };

        module
            .define_function_bytes(func, 8, &[0; 8], &[reloc])
            .unwrap();

        let entry = module.get_finalized_function(func) as *const u8;
        let immediate = unsafe { (entry as *const i32).read_unaligned() };
        let veneer = unsafe { entry.offset(immediate as isize + 4) };
        let veneer_target = unsafe { veneer.add(2).cast::<u64>().read_unaligned() };

        assert_eq!(unsafe { *veneer.add(0) }, 0x49);
        assert_eq!(unsafe { *veneer.add(1) }, 0xbb);
        assert_eq!(veneer_target, far_target as u64);
        assert_eq!(unsafe { *veneer.add(10) }, 0x41);
        assert_eq!(unsafe { *veneer.add(11) }, 0xff);
        assert_eq!(unsafe { *veneer.add(12) }, 0xe3);
    }

    #[test]
    fn saved_jit_record_can_be_reinstalled_at_fresh_address() {
        let mut module = CapyJitModule::new();
        let sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        let func = module
            .declare_function("reloc_reinstall_test", Linkage::Export, &sig)
            .unwrap();
        let data = module
            .declare_data("capy_test_reinstall_symbol", Linkage::Import, false, false)
            .unwrap();
        let target = 0xfeed_face_cafe_beefusize;
        module.define_symbol("capy_test_reinstall_symbol", target);
        let reloc = ModuleReloc {
            kind: Reloc::Abs8,
            offset: 0,
            name: ModuleRelocTarget::user(1, data.as_u32()),
            addend: 0,
        };
        module
            .define_function_bytes(func, 8, &[0; 8], &[reloc])
            .unwrap();
        let first = module.get_finalized_function(func);
        let record = module.take_compiled_record(func);

        let mut restored = CapyJitModule::new();
        restored.define_symbol("capy_test_reinstall_symbol", target);
        let span = restored.install_record(func, record).unwrap();

        assert_ne!(first, span.rx());
        let patched = unsafe { (span.rx() as *const u64).read_unaligned() };
        assert_eq!(patched, target as u64);
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

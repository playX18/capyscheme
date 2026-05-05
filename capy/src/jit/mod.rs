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
            thunks::{ImportedThunks, Thunks},
        },
    },
};
use asmkit::core::jit_allocator::{JitAllocator, JitAllocatorOptions, Span};
use cranelift::prelude::{
    Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, types,
};
use cranelift_codegen::{
    CodegenError, FinalizedRelocTarget,
    binemit::{CodeOffset, Reloc},
    control::ControlPlane,
    ir::{self, AbiParam, ExtFuncData, ExternalName, KnownSymbol, LibCall, UserExternalName},
    isa::{CallConv, OwnedTargetIsa},
    settings,
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
    let mut compiler = JitCompiler::new();
    compiler.define_symbol("capy_jit_resolve", capy_jit_resolve as *const u8 as usize);

    let mut name_map = JitNameMap::new();
    let (resolver_ns, resolver_idx) = name_map.declare_function(JitRelocTarget::RuntimeSymbol {
        name: "capy_jit_resolve".to_owned(),
    });

    let sig_tail = call_signature!(Tail (I64, I64, I64) -> (I64, I64));
    let mut func = ir::Function::with_name_signature(ir::UserFuncName::user(0, 0), sig_tail);
    let mut fctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut func, &mut fctx);
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
        let sig_ref = builder.func.import_signature(resolver_sig);
        let name_ref = builder.func.declare_imported_user_function(UserExternalName {
            namespace: resolver_ns,
            index: resolver_idx,
        });
        let resolver = builder.func.import_function(ExtFuncData {
            name: ExternalName::user(name_ref),
            signature: sig_ref,
            colocated: true,
        });
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

    let stub_target = JitRelocTarget::RuntimeSymbol {
        name: "capy_lazy_jit_stub".to_owned(),
    };
    let installed = compiler
        .compile_function(&mut func, &name_map, stub_target)
        .expect("failed to compile lazy JIT stub");

    Box::leak(Box::new(compiler));
    installed.entrypoint
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

enum JitError {
    Compilation(CodegenError),
    Allocation { message: &'static str, err: std::io::Error },
    Relocation(String),
    Other(String),
}

impl std::fmt::Debug for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JitError::Compilation(err) => write!(f, "JIT compilation error: {err}"),
            JitError::Allocation { message, err } => {
                write!(f, "JIT allocation error: {message}: {err}")
            }
            JitError::Relocation(msg) => write!(f, "JIT relocation error: {msg}"),
            JitError::Other(msg) => write!(f, "JIT error: {msg}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum JitRelocTarget {
    Procedure {
        unit_id: JitUnitId,
        index: usize,
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

struct ResolvedRelocation<'a> {
    relocation: &'a JitRelocation,
    address: usize,
}

struct JitNameMap {
    targets: HashMap<(u32, u32), JitRelocTarget>,
    target_addresses: HashMap<JitRelocTarget, usize>,
    next_func_index: u32,
    next_data_index: u32,
}

impl JitNameMap {
    fn new() -> Self {
        Self {
            targets: HashMap::new(),
            target_addresses: HashMap::new(),
            next_func_index: 0,
            next_data_index: 0,
        }
    }

    fn declare_function(&mut self, target: JitRelocTarget) -> (u32, u32) {
        let index = self.next_func_index;
        self.next_func_index += 1;
        self.targets.insert((0, index), target);
        (0, index)
    }

    fn declare_data(&mut self, target: JitRelocTarget, address: usize) -> (u32, u32) {
        let index = self.next_data_index;
        self.next_data_index += 1;
        self.targets.insert((1, index), target.clone());
        self.target_addresses.insert(target, address);
        (1, index)
    }

    fn resolve_reloc_target(
        &self,
        target: &FinalizedRelocTarget,
        func: &ir::Function,
        current_func_target: &JitRelocTarget,
    ) -> Result<JitRelocTarget, String> {
        match target {
            FinalizedRelocTarget::ExternalName(ExternalName::User(reff)) => {
                let name = &func.params.user_named_funcs()[*reff];
                self.targets
                    .get(&(name.namespace, name.index))
                    .cloned()
                    .ok_or_else(|| {
                        format!(
                            "cannot resolve JIT reloc target for namespace={} index={}",
                            name.namespace, name.index
                        )
                    })
            }
            FinalizedRelocTarget::ExternalName(ExternalName::LibCall(lc)) => {
                Ok(JitRelocTarget::LibCall { libcall: *lc })
            }
            FinalizedRelocTarget::ExternalName(ExternalName::KnownSymbol(symbol)) => {
                Ok(JitRelocTarget::RuntimeSymbol {
                    name: known_symbol_name(*symbol),
                })
            }
            FinalizedRelocTarget::ExternalName(ExternalName::TestCase(_)) => {
                Err("unsupported testcase relocation target".to_owned())
            }
            FinalizedRelocTarget::Func(offset) => match current_func_target {
                JitRelocTarget::Procedure {
                    unit_id,
                    index,
                    offset: base_offset,
                } => Ok(JitRelocTarget::Procedure {
                    unit_id: *unit_id,
                    index: *index,
                    offset: *base_offset + *offset,
                }),
                _ => Err(format!(
                    "Func-offset relocation on non-procedure target {:?}",
                    current_func_target
                )),
            },
        }
    }

    fn target_address(
        &self,
        current_entrypoint: usize,
        current_func_target: &JitRelocTarget,
        target: &JitRelocTarget,
        symbols: &HashMap<String, usize>,
        installed_procedures: &HashMap<(JitUnitId, usize), usize>,
        libcall_names: &dyn Fn(LibCall) -> String,
    ) -> Result<usize, String> {
        match target {
            JitRelocTarget::Procedure {
                unit_id,
                index,
                offset,
            } => {
                if let JitRelocTarget::Procedure {
                    unit_id: current_unit,
                    index: current_index,
                    ..
                } = current_func_target
                {
                    if current_unit == unit_id && current_index == index {
                        return Ok(current_entrypoint + *offset as usize);
                    }
                }
                installed_procedures
                    .get(&(*unit_id, *index))
                    .map(|entry| entry + *offset as usize)
                    .ok_or_else(|| format!("JIT procedure {unit_id}:{index} is not installed"))
            }
            JitRelocTarget::ConstantSlot { .. }
            | JitRelocTarget::CacheSlot { .. }
            | JitRelocTarget::CodeBlockSlot { .. } => self
                .target_addresses
                .get(target)
                .copied()
                .ok_or_else(|| format!("JIT slot has no address: {target:?}")),
            JitRelocTarget::RuntimeSymbol { name } => lookup_symbol(name, symbols)
                .ok_or_else(|| format!("cannot resolve runtime symbol {name}")),
            JitRelocTarget::LibCall { libcall } => {
                let name = libcall_names(*libcall);
                lookup_symbol(&name, symbols)
                    .ok_or_else(|| format!("cannot resolve libcall {name}"))
            }
            JitRelocTarget::GlobalSideMetadataBaseAddress => {
                Ok((&*JIT_GLOBAL_SIDE_METADATA_VALUE as *const usize).cast::<u8>() as usize)
            }
        }
    }
}

struct JitCompiler {
    isa: OwnedTargetIsa,
    allocator: Box<JitAllocator>,
    symbols: HashMap<String, usize>,
    installed_procedures: HashMap<(JitUnitId, usize), usize>,
    libcall_names: Box<dyn Fn(LibCall) -> String + Send + Sync>,
}

impl JitCompiler {
    fn new() -> Self {
        let mut options = JitAllocatorOptions::default();
        options.granularity = 256;

        let mut compiler = Self {
            isa: build_jit_isa(),
            allocator: JitAllocator::new(options),
            symbols: HashMap::new(),
            installed_procedures: HashMap::new(),
            libcall_names: default_libcall_names(),
        };

        for (name, address) in Thunks::symbols() {
            compiler.define_symbol(name, address as usize);
        }
        compiler
    }

    fn define_symbol(&mut self, name: impl Into<String>, address: usize) {
        self.symbols.insert(name.into(), address);
    }

    fn lookup_symbol(&self, name: &str) -> Option<usize> {
        lookup_symbol(name, &self.symbols)
    }

    fn compile_function(
        &mut self,
        func: &mut ir::Function,
        name_map: &JitNameMap,
        current_func_target: JitRelocTarget,
    ) -> Result<JitInstalledFunction, JitError> {
        let mut ctx = cranelift_codegen::Context::for_function(std::mem::replace(
            func,
            ir::Function::with_name_signature(ir::UserFuncName::user(0, 0), ir::Signature::new(CallConv::SystemV)),
        ));
        let mut ctrl_plane = ControlPlane::default();
        let compiled = ctx
            .compile(&*self.isa, &mut ctrl_plane)
            .map_err(|err| JitError::Compilation(err.inner))?;

        let code = compiled.buffer.data().to_vec().into_boxed_slice();
        let alignment = (compiled.buffer.alignment as u64)
            .max(self.isa.function_alignment().minimum as u64)
            .max(self.isa.symbol_alignment());

        let raw_relocs: Vec<_> = compiled.buffer.relocs().to_vec();

        let relocations = raw_relocs
            .iter()
            .map(|reloc| {
                let target = name_map
                    .resolve_reloc_target(&reloc.target, &ctx.func, &current_func_target)
                    .map_err(JitError::Relocation)?;
                Ok(JitRelocation {
                    kind: reloc.kind,
                    offset: reloc.offset,
                    target,
                    addend: reloc.addend,
                })
            })
            .collect::<Result<Vec<_>, JitError>>()?;

        let record = JitCompiledFunctionRecord {
            code,
            alignment,
            target_triple: self.isa.triple().to_string(),
            isa_fingerprint: self.isa.to_string(),
            relocations,
        };

        let entrypoint = self.install_record(name_map, current_func_target.clone(), &record)?;

        *func = ctx.func;

        Ok(JitInstalledFunction { entrypoint, record })
    }

    fn install_record(
        &mut self,
        name_map: &JitNameMap,
        current_func_target: JitRelocTarget,
        record: &JitCompiledFunctionRecord,
    ) -> Result<Address, JitError> {
        let alloc_size =
            (record.code.len() + record.relocations.len() * MAX_VENEER_SIZE).max(1);
        let mut span = self
            .allocator
            .alloc(alloc_size)
            .map_err(|err| JitError::Allocation {
                message: "unable to allocate JIT function",
                err: std::io::Error::new(std::io::ErrorKind::Other, format!("{err:?}")),
            })?;

        if span.rx() as usize % record.alignment.max(1) as usize != 0 {
            return Err(JitError::Other(format!(
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
                let address = name_map
                    .target_address(
                        current_entrypoint,
                        &current_func_target,
                        &relocation.target,
                        &self.symbols,
                        &self.installed_procedures,
                        &*self.libcall_names,
                    )
                    .map_err(JitError::Relocation)?;
                Ok(ResolvedRelocation {
                    relocation,
                    address,
                })
            })
            .collect::<Result<Vec<_>, JitError>>()?;

        let mut image = vec![0; alloc_size];
        image[..record.code.len()].copy_from_slice(&record.code);
        unsafe {
            self.allocator
                .copy_from_slice(&mut span, 0, &image)
                .map_err(|err| JitError::Allocation {
                    message: "unable to write JIT function",
                    err: std::io::Error::new(std::io::ErrorKind::Other, format!("{err:?}")),
                })?;
        }

        let mut relocation_error = None;
        unsafe {
            self.allocator
                .write(&mut span, |span| {
                    if let Err(err) = apply_relocations(span, record.code.len(), &resolved) {
                        relocation_error = Some(err);
                    }
                })
                .map_err(|err| JitError::Allocation {
                    message: "unable to patch JIT function",
                    err: std::io::Error::new(std::io::ErrorKind::Other, format!("{err:?}")),
                })?;
        }
        if let Some(err) = relocation_error {
            return Err(JitError::Relocation(err));
        }

        Ok(Address::from_ptr(span.rx()))
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
    flag_builder
        .set("preserve_frame_pointers", "true")
        .unwrap();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();

    let triple = target_lexicon::Triple::host();
    let isa_builder = cranelift_codegen::isa::lookup(triple).unwrap_or_else(|err| panic!("{err}"));
    isa_builder
        .finish(settings::Flags::new(flag_builder))
        .expect("failed to build Cranelift JIT ISA")
}

fn default_libcall_names() -> Box<dyn Fn(LibCall) -> String + Send + Sync> {
    Box::new(|libcall| match libcall {
        LibCall::Probestack => "__cranelift_probestack".to_owned(),
        LibCall::CeilF32 => "ceilf".to_owned(),
        LibCall::CeilF64 => "ceil".to_owned(),
        LibCall::FloorF32 => "floorf".to_owned(),
        LibCall::FloorF64 => "floor".to_owned(),
        LibCall::TruncF32 => "truncf".to_owned(),
        LibCall::TruncF64 => "trunc".to_owned(),
        LibCall::NearestF32 => "nearbyintf".to_owned(),
        LibCall::NearestF64 => "nearbyint".to_owned(),
        LibCall::FmaF32 => "fmaf".to_owned(),
        LibCall::FmaF64 => "fma".to_owned(),
        LibCall::Memcpy => "memcpy".to_owned(),
        LibCall::Memset => "memset".to_owned(),
        LibCall::Memmove => "memmove".to_owned(),
        LibCall::Memcmp => "memcmp".to_owned(),
        LibCall::ElfTlsGetAddr => "__tls_get_addr".to_owned(),
        LibCall::ElfTlsGetOffset => "__tls_get_offset".to_owned(),
        LibCall::X86Pshufb => "__cranelift_x86_pshufb".to_owned(),
    })
}

fn known_symbol_name(symbol: KnownSymbol) -> String {
    match symbol {
        KnownSymbol::ElfGlobalOffsetTable => "_GLOBAL_OFFSET_TABLE_",
        KnownSymbol::CoffTlsIndex => "_tls_index",
    }
    .to_owned()
}

fn lookup_symbol(name: &str, symbols: &HashMap<String, usize>) -> Option<usize> {
    symbols
        .get(name)
        .copied()
        .or_else(|| lookup_with_dlsym(name).map(|ptr| ptr as usize))
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
    funcs_by_code: HashMap<CodeId, (u32, u32)>,
    code_blocks_by_code: HashMap<CodeId, (u32, u32)>,
    constants_by_symbol: HashMap<String, (u32, u32)>,
    cache_cells_by_symbol: HashMap<String, (u32, u32)>,
}

struct JitWorker {
    compiler: JitCompiler,
    name_map: JitNameMap,
    units: HashMap<JitUnitId, JitUnitDecls>,
}

impl JitWorker {
    fn new() -> Self {
        let compiler = JitCompiler::new();
        let mut name_map = JitNameMap::new();

        let mut thunk_index: u32 = 0;
        for (name, _address) in Thunks::symbols() {
            name_map.targets.insert(
                (2, thunk_index),
                JitRelocTarget::RuntimeSymbol {
                    name: name.to_owned(),
                },
            );
            thunk_index += 1;
        }

        Self {
            compiler,
            name_map,
            units: HashMap::new(),
        }
    }

    fn ensure_unit_declared<'gc>(&mut self, program: &JitModuleState<'gc>) -> JitUnitDecls {
        if let Some(decls) = self.units.get(&program.unit_id) {
            return decls.clone();
        }

        let mut funcs_by_code = HashMap::new();
        let mut code_blocks_by_code = HashMap::new();
        let mut constants_by_symbol = HashMap::new();
        let mut cache_cells_by_symbol = HashMap::new();

        for (index, procedure) in program.linear.procedures.iter().enumerate() {
            let (ns, idx) = self.name_map.declare_function(JitRelocTarget::Procedure {
                unit_id: program.unit_id,
                index,
                offset: 0,
            });
            funcs_by_code.insert(procedure.code, (ns, idx));

            let (ns, idx) = self.name_map.declare_data(
                JitRelocTarget::CodeBlockSlot {
                    unit_id: program.unit_id,
                    index,
                },
                (&program.code_block_slots[index].value as *const Value<'gc>).cast::<u8>() as usize,
            );
            code_blocks_by_code.insert(procedure.code, (ns, idx));
        }

        for (index, slot) in program.constant_slots.iter().enumerate() {
            let (ns, idx) = self.name_map.declare_data(
                JitRelocTarget::ConstantSlot {
                    unit_id: program.unit_id,
                    index,
                },
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
            constants_by_symbol.insert(slot.symbol.clone(), (ns, idx));
        }

        for (index, slot) in program.cache_slots.iter().enumerate() {
            let (ns, idx) = self.name_map.declare_data(
                JitRelocTarget::CacheSlot {
                    unit_id: program.unit_id,
                    index,
                },
                (&slot.value as *const Value<'gc>).cast::<u8>() as usize,
            );
            cache_cells_by_symbol.insert(slot.symbol.clone(), (ns, idx));
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

        let thunks = Thunks::new_jit();

        let procedure = &program.procedures[procedure_index].procedure;
        let sig = call_signature!(Tail (I64, I64, I64) -> (I64, I64));

        let mut func = ir::Function::with_name_signature(ir::UserFuncName::user(0, 0), sig);
        let mut fctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fctx);

        let mut thunk_namespace_index: u32 = 0;
        let thunks_imported = ImportedThunks::new_jit(
            &thunks,
            &mut builder.func,
            |sig, _func_id_u32, func| {
                let index = thunk_namespace_index;
                thunk_namespace_index += 1;
                let sig_ref = func.import_signature(sig);
                let name_ref = func.declare_imported_user_function(UserExternalName {
                    namespace: 2,
                    index,
                });
                func.import_function(ExtFuncData {
                    name: ExternalName::user(name_ref),
                    signature: sig_ref,
                    colocated: true,
                })
            },
        );

        let mut jit_builder = lower::JitBuilder::new(
            ctx,
            &mut self.compiler,
            &mut self.name_map,
            &decls,
            program.reify_info_for_worker(),
            JIT_GLOBAL_SIDE_METADATA_SYMBOL,
            thunks,
        );
        jit_builder.stacktraces = program.options.backtraces;
        jit_builder.direct_calls = false;
        for slot in &program.constant_slots {
            let name_pair = decls.constants_by_symbol[&slot.symbol];
            jit_builder.constants.insert(ValueEqual(slot.key), name_pair);
        }
        for slot in &program.cache_slots {
            let name_pair = decls.cache_cells_by_symbol[&slot.symbol];
            jit_builder.cache_cells.insert(ValueEqual(slot.key), name_pair);
        }

        let function_name = jit_function_name(program.unit_id, procedure);
        let func_debug_cx = jit_builder
            .debug_context
            .define_linear_procedure(procedure, &function_name);
        let mut ssa = lower::JitLowerer::new(
            &mut jit_builder,
            builder,
            lower::JitTarget::Procedure(procedure.clone()),
            thunks_imported,
            func_debug_cx,
        );
        ssa.linear_procedure(procedure);
        ssa.finalize();
        ssa.builder.seal_all_blocks();
        ssa.builder.finalize();

        let reloc_target = JitRelocTarget::Procedure {
            unit_id: program.unit_id,
            index: procedure_index,
            offset: 0,
        };

        let installed = self
            .compiler
            .compile_function(&mut func, &self.name_map, reloc_target)
            .unwrap_or_else(|err| panic!("failed to compile JIT procedure: {err:?}"));

        self.compiler.installed_procedures.insert(
            (program.unit_id, procedure_index),
            installed.entrypoint.as_usize(),
        );

        installed
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
        JitCompiler, JitModuleState, JitNameMap, JitRelocTarget, capy_jit_resolve,
        compile_cps_to_jit_thunk,
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
    use cranelift_codegen::{
        FinalizedRelocTarget, binemit::Reloc, ir,
        ir::{ExternalName, UserExternalName},
    };

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn atoms<'gc>(ctx: Context<'gc>, atoms: &[crate::cps::term::Atom<'gc>]) -> Atoms<'gc> {
        Array::from_slice(*ctx, atoms)
    }

    #[test]
    fn direct_jit_records_stable_runtime_symbol_relocations() {
        let mut compiler = JitCompiler::new();
        let mut name_map = JitNameMap::new();
        let target = 0x1234_5678usize;
        compiler.define_symbol("capy_test_reloc_symbol", target);

        let (ns, idx) = name_map.declare_data(
            JitRelocTarget::RuntimeSymbol {
                name: "capy_test_reloc_symbol".to_owned(),
            },
            target,
        );

        let record = JitCompiledFunctionRecord {
            code: vec![0u8; 8].into_boxed_slice(),
            alignment: 8,
            target_triple: String::new(),
            isa_fingerprint: String::new(),
            relocations: vec![super::JitRelocation {
                kind: Reloc::Abs8,
                offset: 0,
                target: JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_reloc_symbol".to_owned(),
                },
                addend: 0,
            }],
        };

        let entrypoint = compiler
            .install_record(
                &name_map,
                JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_reloc_symbol".to_owned(),
                },
                &record,
            )
            .unwrap();

        assert!(matches!(
            &record.relocations[0].target,
            JitRelocTarget::RuntimeSymbol { name } if name == "capy_test_reloc_symbol"
        ));
        let patched = unsafe { (entrypoint.to_ptr::<u8>() as *const u64).read_unaligned() };
        assert_eq!(patched, target as u64);
    }

    #[test]
    fn name_map_resolves_data_to_stable_slot_identity() {
        let mut name_map = JitNameMap::new();
        let (ns, idx) = name_map.declare_data(
            JitRelocTarget::ConstantSlot {
                unit_id: 77,
                index: 5,
            },
            0x1000,
        );

        let mut func = ir::Function::new();
        let name_ref = func.declare_imported_user_function(UserExternalName {
            namespace: ns,
            index: idx,
        });

        let target =
            FinalizedRelocTarget::ExternalName(ExternalName::user(name_ref));
        let resolved = name_map
            .resolve_reloc_target(
                &target,
                &func,
                &JitRelocTarget::Procedure {
                    unit_id: 0,
                    index: 0,
                    offset: 0,
                },
            )
            .unwrap();

        assert_eq!(
            resolved,
            JitRelocTarget::ConstantSlot {
                unit_id: 77,
                index: 5
            }
        );
    }

    #[test]
    fn finalized_func_reloc_translates_to_current_procedure_offset() {
        let name_map = JitNameMap::new();
        let current_target = JitRelocTarget::Procedure {
            unit_id: 88,
            index: 9,
            offset: 0,
        };

        let func = ir::Function::new();
        let resolved = name_map
            .resolve_reloc_target(&FinalizedRelocTarget::Func(12), &func, &current_target)
            .unwrap();

        assert_eq!(
            resolved,
            JitRelocTarget::Procedure {
                unit_id: 88,
                index: 9,
                offset: 12
            }
        );
    }

    #[test]
    fn x86_call_veneer_keeps_raw_symbol_target() {
        let mut compiler = JitCompiler::new();
        let mut name_map = JitNameMap::new();
        let far_target = usize::MAX - 0x1000;
        compiler.define_symbol("capy_test_veneer_target", far_target);

        name_map.declare_function(JitRelocTarget::RuntimeSymbol {
            name: "capy_test_veneer_target".to_owned(),
        });

        let record = JitCompiledFunctionRecord {
            code: vec![0u8; 8].into_boxed_slice(),
            alignment: 8,
            target_triple: String::new(),
            isa_fingerprint: String::new(),
            relocations: vec![super::JitRelocation {
                kind: Reloc::X86CallPCRel4,
                offset: 0,
                target: JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_veneer_target".to_owned(),
                },
                addend: -4,
            }],
        };

        let entrypoint = compiler
            .install_record(
                &name_map,
                JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_veneer_source".to_owned(),
                },
                &record,
            )
            .unwrap();

        let entry = entrypoint.to_ptr::<u8>();
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
        let mut compiler = JitCompiler::new();
        let mut name_map = JitNameMap::new();
        let target = 0xfeed_face_cafe_beefusize;
        compiler.define_symbol("capy_test_reinstall_symbol", target);

        name_map.declare_data(
            JitRelocTarget::RuntimeSymbol {
                name: "capy_test_reinstall_symbol".to_owned(),
            },
            target,
        );

        let record = JitCompiledFunctionRecord {
            code: vec![0u8; 8].into_boxed_slice(),
            alignment: 8,
            target_triple: String::new(),
            isa_fingerprint: String::new(),
            relocations: vec![super::JitRelocation {
                kind: Reloc::Abs8,
                offset: 0,
                target: JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_reinstall_symbol".to_owned(),
                },
                addend: 0,
            }],
        };

        let first = compiler
            .install_record(
                &name_map,
                JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_reinstall_func".to_owned(),
                },
                &record,
            )
            .unwrap();

        let mut restored = JitCompiler::new();
        let mut restored_name_map = JitNameMap::new();
        restored.define_symbol("capy_test_reinstall_symbol", target);

        restored_name_map.declare_data(
            JitRelocTarget::RuntimeSymbol {
                name: "capy_test_reinstall_symbol".to_owned(),
            },
            target,
        );

        let second = restored
            .install_record(
                &restored_name_map,
                JitRelocTarget::RuntimeSymbol {
                    name: "capy_test_reinstall_func".to_owned(),
                },
                &record,
            )
            .unwrap();

        assert_ne!(first, second);
        let patched = unsafe { (second.to_ptr::<u8>() as *const u64).read_unaligned() };
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

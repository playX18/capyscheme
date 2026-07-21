//! SSA (Static Single Assignment) code generation using Cranelift.

use std::sync::Arc;

use cranelift::prelude::Configurable;
use cranelift_codegen::{
    Context as ClifContext,
    control::ControlPlane,
    isa::TargetIsa,
    settings::{self, Flags},
};

use crate::compiler::symbols::{DataKind, DataSymbol, Symbol};
use crate::runtime::symbols::RuntimeData;

pub fn declare_function(
    function: &mut cranelift_codegen::ir::Function,
    symbol: Symbol,
    signature: cranelift_codegen::ir::SigRef,
    colocated: bool,
) -> cranelift_codegen::ir::FuncRef {
    let name_ref = function.declare_imported_user_function(symbol.to_external_name());
    function.import_function(cranelift_codegen::ir::ExtFuncData {
        name: cranelift_codegen::ir::ExternalName::user(name_ref),
        signature,
        colocated,
    })
}

pub fn declare_data(
    function: &mut cranelift_codegen::ir::Function,
    symbol: Symbol,
    colocated: bool,
    tls: bool,
) -> cranelift_codegen::ir::GlobalValue {
    let name_ref = function.declare_imported_user_function(symbol.to_external_name());
    function.create_global_value(cranelift_codegen::ir::GlobalValueData::Symbol {
        name: cranelift_codegen::ir::ExternalName::user(name_ref),
        offset: cranelift_codegen::ir::immediates::Imm64::new(0),
        colocated,
        tls,
    })
}

pub fn runtime_data(data: RuntimeData) -> Symbol {
    Symbol::data(DataKind::RuntimeData, DataSymbol::new(data.id()))
}

pub fn declare_runtime_data(
    function: &mut cranelift_codegen::ir::Function,
    data: RuntimeData,
) -> cranelift_codegen::ir::GlobalValue {
    declare_data(function, runtime_data(data), false, false)
}

pub fn host_isa() -> Arc<dyn TargetIsa> {
    let mut shared_builder = settings::builder();
    shared_builder.set("enable_probestack", "false").unwrap();
    shared_builder
        .set("enable_heap_access_spectre_mitigation", "false")
        .unwrap();
    shared_builder.set("opt_level", "speed_and_size").unwrap();
    shared_builder.enable("preserve_frame_pointers").unwrap();
    shared_builder.enable("enable_pinned_reg").unwrap();
    shared_builder.enable("enable_alias_analysis").unwrap();

    let shared_flags = Flags::new(shared_builder);
    cranelift_codegen::isa::lookup(target_lexicon::Triple::host())
        .expect("host target should be supported by Cranelift")
        .finish(shared_flags)
        .expect("host ISA should finish")
}

pub struct CompileContext {
    pub ctx: ClifContext,
    pub builder_ctx: cranelift::prelude::FunctionBuilderContext,
    pub ctrl: ControlPlane,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            ctx: ClifContext::new(),
            builder_ctx: cranelift::prelude::FunctionBuilderContext::new(),
            ctrl: ControlPlane::default(),
        }
    }

    pub fn clear(&mut self) {
        self.ctx.clear();
        self.builder_ctx = cranelift::prelude::FunctionBuilderContext::new();
        self.ctrl = ControlPlane::default();
    }
}

impl Default for CompileContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
use crate::compiler::codegen::declare_function as codegen_declare_function;
use crate::{
    compiler::{
        BackendDumpOptions,
        cranelift::primitive::PrimitiveLowerer,
        debuginfo::{DebugContext, DebugSourceLocation, FunctionDebugContext},
        direct::{
            CompiledFunction, Relocation as DirectRelocation, Target as DirectTarget,
            compile_function,
        },
        ssa::{BlockId, CodeId, Procedure, Program, ValueId},
        symbols::FunctionSymbol,
    },
    disassembly::{DisassemblySource, SourceAnnotation, SourceRangeAnnotation},
    expander::core::LVarRef,
    rsgc::object::{OBJECT_HEADER_OFFSET, builtin_class_ids},
    runtime::{
        CallData, Context, REGISTER_ARG_COUNT, State,
        fasl::{
            CodeSourceMapEntry, CodeSpec, Compression, GraphCodeSpec, GraphValueSpec, Image,
            ProgramSpec, Writer,
            reloc::{RelocKind, RelocTarget, Relocation, SideMetadataSlot},
        },
        value::{Closure, ReturnCode, Symbol as SchemeSymbol, Value, ValueEqual},
    },
};

use cranelift::prelude::{
    FunctionBuilder, FunctionBuilderContext, InstBuilder, types as clif_types,
};
use cranelift_codegen::{
    binemit::Reloc,
    ir::{self, BlockArg, SourceLoc},
};

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    mem::offset_of,
    path::{Path, PathBuf},
};

use crate::runtime::vm::thunks::*;

pub mod helpers;
pub mod primitive;
pub mod translate;
pub use primitive::{PrimValue, Primitive};
pub mod traits;
mod types;

pub(crate) use translate::AllocationHeaderPreset;
pub(crate) use types::{MAX_RAISE_ARITY, compiled_scheme_signature, overflow_base_from_argc};
pub use types::{RegisterCallArgs, RestSource, VarDef};

fn declare_direct_function(
    next_function_symbol: &mut u32,
    _name: &str,
    _sig: &ir::Signature,
) -> FunctionSymbol {
    let symbol = FunctionSymbol::new(*next_function_symbol);
    *next_function_symbol += 1;
    symbol
}

fn declare_direct_data(next_data_symbol: &mut u32, _name: &str) -> DataSymbol {
    let symbol = DataSymbol::new(*next_data_symbol);
    *next_data_symbol += 1;
    symbol
}

fn open_dump_file(path: Option<&Path>) -> Result<Option<File>, String> {
    let Some(path) = path else {
        return Ok(None);
    };
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|err| {
            format!(
                "failed to create dump directory {}: {err}",
                parent.display()
            )
        })?;
    }
    File::create(path)
        .map(Some)
        .map_err(|err| format!("failed to create dump file {}: {err}", path.display()))
}

fn write_cranelift_dump(
    file: &mut File,
    symbol_id: u32,
    name: &str,
    function: &ir::Function,
) -> Result<(), String> {
    writeln!(file, ";; function {symbol_id} {name}")
        .map_err(|err| format!("failed to write Cranelift dump for {name}: {err}"))?;
    writeln!(file, "{}", function.display())
        .map_err(|err| format!("failed to write Cranelift dump for {name}: {err}"))?;
    Ok(())
}

fn write_disassembly_dump(
    file: &mut File,
    symbol_id: u32,
    name: &str,
    bytes: &[u8],
    source: Option<&DisassemblySource>,
) -> Result<(), String> {
    writeln!(file, ";; function {symbol_id} {name}")
        .map_err(|err| format!("failed to write disassembly dump for {name}: {err}"))?;
    let rendered = crate::disassembly::disassemble_host_with_annotations(bytes, 0, source)
        .map_err(|err| format!("failed to disassemble {name}: {err}"))?;
    writeln!(file, "{rendered}")
        .map_err(|err| format!("failed to write disassembly dump for {name}: {err}"))?;
    Ok(())
}

fn disassembly_source<'gc>(
    func_debug_cx: &FunctionDebugContext<'gc>,
    debug_context: &DebugContext<'gc>,
    compiled: &CompiledFunction,
) -> DisassemblySource {
    let function = source_annotation(func_debug_cx.default_source_location(debug_context));
    let instructions = compiled
        .source_locs
        .iter()
        .filter_map(|source_loc| {
            if source_loc.start == source_loc.end {
                return None;
            }
            let source = func_debug_cx.source_location(debug_context, source_loc.loc);
            source_annotation(source)
                .map(|source| SourceRangeAnnotation::new(source_loc.start, source_loc.end, source))
        })
        .collect();

    DisassemblySource::new(function, instructions)
}

fn code_source_map(source: &DisassemblySource) -> Vec<CodeSourceMapEntry> {
    source
        .instructions()
        .iter()
        .map(|range| {
            CodeSourceMapEntry::new(
                range.start(),
                range.end(),
                range.source().to_code_source_location(),
            )
        })
        .collect()
}

fn source_annotation(source: DebugSourceLocation) -> Option<SourceAnnotation> {
    if source.file == "<unknown>" || source.line == 0 {
        return None;
    }

    Some(SourceAnnotation::new(
        source.file,
        source.line,
        source.column,
        None,
        None,
    ))
}

fn metadata_with_source<'gc>(
    ctx: Context<'gc>,
    metadata: Value<'gc>,
    source: Value<'gc>,
) -> Value<'gc> {
    if source == Value::new(false) {
        return metadata;
    }

    let source_key = SchemeSymbol::from_str(ctx, "source").into();
    if metadata.is_pair() && metadata.assq(source_key).is_some() {
        return metadata;
    }

    let metadata = if metadata == Value::new(false) {
        Value::null()
    } else {
        metadata
    };
    if !metadata.is_alist() {
        return metadata;
    }

    let source_entry = Value::cons(ctx, source_key, source);
    Value::cons(ctx, source_entry, metadata)
}

fn fasl_relocation_from_direct_relocation(
    relocation: &DirectRelocation,
    data_slot_targets: &HashMap<u32, RelocTarget>,
    debug_entry_functions: &HashSet<u32>,
) -> Result<Relocation, String> {
    let (target, abs8_kind) = match relocation.target {
        DirectTarget::Symbol(Symbol::Function(symbol)) => (
            if debug_entry_functions.contains(&symbol.index()) {
                RelocTarget::DebugEntry(symbol.index())
            } else {
                RelocTarget::Entry(symbol.index())
            },
            RelocKind::CodeEntry,
        ),
        DirectTarget::Symbol(Symbol::Data { kind, symbol }) => {
            if kind == DataKind::RuntimeData {
                (
                    RelocTarget::RuntimeSymbol(symbol.index()),
                    RelocKind::RuntimeData,
                )
            } else if kind == DataKind::CacheCell {
                (
                    match data_slot_targets.get(&symbol.index()).copied().ok_or_else(|| {
                        format!(
                            "data slot {} cannot be represented as a unified FASL data slot yet",
                            symbol.index()
                        )
                    })? {
                        RelocTarget::Object(index) => RelocTarget::CacheCell(index),
                        other => other,
                    },
                    RelocKind::CacheCell,
                )
            } else {
                (
                    data_slot_targets.get(&symbol.index()).copied().ok_or_else(|| {
                        format!(
                            "data slot {} cannot be represented as a unified FASL data slot yet",
                            symbol.index()
                        )
                    })?,
                    RelocKind::DataSlotAddress,
                )
            }
        }
        DirectTarget::Symbol(Symbol::Imported { kind, symbol }) => match kind {
            crate::compiler::codegen::ImportKind::RuntimeThunk => (
                RelocTarget::RuntimeSymbol(symbol.index()),
                RelocKind::RuntimeThunk,
            ),
            crate::compiler::codegen::ImportKind::Trampoline => {
                return Err(
                    "trampoline relocations are not supported in unified FASL yet".to_string(),
                );
            }
        },
        DirectTarget::FunctionOffset(offset) => {
            return Err(format!(
                "function-offset relocation target {offset} cannot be encoded in unified FASL"
            ));
        }
    };

    let kind = match relocation.kind {
        Reloc::Abs8 => abs8_kind,
        kind if matches!(abs8_kind, RelocKind::DataSlotAddress | RelocKind::CacheCell) => {
            RelocKind::CraneliftDataSlot(kind)
        }
        kind => RelocKind::Cranelift(kind),
    };

    Ok(Relocation::new(
        relocation.offset,
        kind,
        target,
        relocation.addend,
    ))
}

/// Builds a Cranelift module from one SSA compilation unit.
pub struct ModuleBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub(crate) debug_context: DebugContext<'gc>,
    pub program: Program<'gc>,
    pub constants: HashMap<ValueEqual<'gc>, DataSymbol>,
    pub cache_cells: HashMap<ValueEqual<'gc>, DataSymbol>,

    next_function_symbol: u32,
    data_kinds: HashMap<DataSymbol, DataKind>,
    next_data_symbol: u32,

    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_code: HashMap<CodeId, FunctionSymbol>,
    pub code_block_for_code: HashMap<CodeId, DataSymbol>,
    pub pointer_slot_for_function: HashMap<FunctionSymbol, DataSymbol>,
    pub raise_trampolines: Vec<FunctionSymbol>,
    pub raise_to_exception_handler_trampoline: FunctionSymbol,
    pub wrong_arity_trampoline: FunctionSymbol,
    pub generic_apply_trampoline: FunctionSymbol,
    pub generic_tail_apply_trampoline: FunctionSymbol,
    pub global_side_metadata_base_address: DataSymbol,
    pub vo_bit_side_metadata_base_address: DataSymbol,

    pub stacktraces: bool,
}

#[derive(Clone)]
pub(crate) struct DeclaredProcedure<'gc> {
    pub procedure: Procedure<'gc>,
    pub function: FunctionSymbol,
    pub name: String,
}

struct CompiledFaslFunction {
    symbol: FunctionSymbol,
    compiled: crate::compiler::direct::CompiledFunction,
    entry_offset: u32,
    arity: i32,
    is_cont: bool,
    metadata_constant: Option<u32>,
    source_map: Vec<CodeSourceMapEntry>,
}

#[derive(Clone, Copy)]
struct DataSlot {
    symbol: DataSymbol,
    kind: DataKind,
    constant_index: Option<u32>,
    code: Option<FunctionSymbol>,
    pointer_code: Option<u32>,
    side_metadata: Option<SideMetadataSlot>,
}

impl DataSlot {
    fn constant(symbol: DataSymbol, index: Option<u32>) -> Self {
        Self::new(symbol, DataKind::Constant).with_constant(index)
    }

    fn cache_cell(symbol: DataSymbol) -> Self {
        Self::new(symbol, DataKind::CacheCell)
    }

    fn code(symbol: DataSymbol, function: Option<FunctionSymbol>) -> Self {
        Self::new(symbol, DataKind::CodeBlock).with_code(function)
    }

    fn pointer(symbol: DataSymbol, function: FunctionSymbol) -> Self {
        Self::new(symbol, DataKind::PointerSlot).with_pointer(function.index())
    }

    fn side_metadata(symbol: DataSymbol, kind: SideMetadataSlot) -> Self {
        Self::new(symbol, DataKind::SideMetadata).with_side_metadata(kind)
    }

    fn new(symbol: DataSymbol, kind: DataKind) -> Self {
        Self {
            symbol,
            kind,
            constant_index: None,
            code: None,
            pointer_code: None,
            side_metadata: None,
        }
    }

    fn with_constant(mut self, index: Option<u32>) -> Self {
        self.constant_index = index;
        self
    }

    fn with_code(mut self, function: Option<FunctionSymbol>) -> Self {
        self.code = function;
        self
    }

    fn with_pointer(mut self, function: u32) -> Self {
        self.pointer_code = Some(function);
        self
    }

    fn with_side_metadata(mut self, kind: SideMetadataSlot) -> Self {
        self.side_metadata = Some(kind);
        self
    }
}

impl<'gc> ModuleBuilder<'gc> {
    pub fn new_with_program(ctx: Context<'gc>, program: Program<'gc>) -> Self {
        let isa = host_isa();
        let entry = program
            .procedures
            .iter()
            .find(|procedure| procedure.code == program.entry)
            .expect("program should contain its entry procedure");
        let debug_context = DebugContext::new_for_entry(
            entry.source,
            entry.name,
            entry.sources[&entry.binding],
            &*isa,
        );
        Self::new_with_debug_context(ctx, program, debug_context)
    }

    fn new_with_debug_context(
        ctx: Context<'gc>,
        program: Program<'gc>,
        debug_context: DebugContext<'gc>,
    ) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let mut next_function_symbol = 0;
        let mut data_kinds = HashMap::new();
        let mut next_data_symbol = 0;
        let global_side_metadata_base_address =
            declare_direct_data(&mut next_data_symbol, "__GLOBAL_SIDE_METADATA_VM_ADDRESS");
        data_kinds.insert(global_side_metadata_base_address, DataKind::SideMetadata);
        let vo_bit_side_metadata_base_address =
            declare_direct_data(&mut next_data_symbol, "__VO_BIT_SIDE_METADATA_VM_ADDRESS");
        data_kinds.insert(vo_bit_side_metadata_base_address, DataKind::SideMetadata);
        let raise_sig = compiled_scheme_signature();
        let raise_trampolines = (0..=MAX_RAISE_ARITY)
            .map(|arity| {
                declare_direct_function(
                    &mut next_function_symbol,
                    &format!("capy_raise{arity}"),
                    &raise_sig,
                )
            })
            .collect();
        let raise_to_exception_handler_trampoline = declare_direct_function(
            &mut next_function_symbol,
            "capy_raise_to_exception_handler",
            &raise_sig,
        );
        let wrong_arity_trampoline = declare_direct_function(
            &mut next_function_symbol,
            "capy_raise_wrong_arity",
            &raise_sig,
        );
        let generic_apply_trampoline =
            declare_direct_function(&mut next_function_symbol, "capy_generic_apply", &raise_sig);
        let generic_tail_apply_trampoline = declare_direct_function(
            &mut next_function_symbol,
            "capy_generic_tail_apply",
            &raise_sig,
        );
        Self {
            debug_context,
            ctx,
            stacktraces: false,
            program,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            next_function_symbol,
            data_kinds,
            next_data_symbol,
            prims,
            func_for_code: HashMap::new(),
            code_block_for_code: HashMap::new(),
            pointer_slot_for_function: HashMap::new(),
            raise_trampolines,
            raise_to_exception_handler_trampoline,
            wrong_arity_trampoline,
            generic_apply_trampoline,
            generic_tail_apply_trampoline,
            global_side_metadata_base_address,
            vo_bit_side_metadata_base_address,
        }
    }

    fn declare_function_symbol(&mut self, name: &str, sig: &ir::Signature) -> FunctionSymbol {
        declare_direct_function(&mut self.next_function_symbol, name, sig)
    }

    fn declare_data_symbol(&mut self, kind: DataKind, name: &str) -> DataSymbol {
        let symbol = declare_direct_data(&mut self.next_data_symbol, name);
        self.data_kinds.insert(symbol, kind);
        symbol
    }

    #[allow(dead_code)]
    pub(crate) fn data_kind(&self, symbol: DataSymbol) -> DataKind {
        self.data_kinds
            .get(&symbol)
            .copied()
            .expect("data symbol should have a direct data kind")
    }

    #[cfg(test)]
    pub(crate) fn declare_function_in_func(
        &mut self,
        symbol: FunctionSymbol,
        function: &mut ir::Function,
    ) -> ir::FuncRef {
        let signature = function.import_signature(compiled_scheme_signature());
        codegen_declare_function(function, Symbol::function(symbol), signature, false)
    }

    pub(crate) fn declare_data_in_func(
        &mut self,
        symbol: DataSymbol,
        function: &mut ir::Function,
    ) -> ir::GlobalValue {
        declare_data(
            function,
            Symbol::data(self.data_kind(symbol), symbol),
            false,
            false,
        )
    }

    pub(crate) fn declare_function_pointer_slot(&mut self, symbol: FunctionSymbol) -> DataSymbol {
        if let Some(slot) = self.pointer_slot_for_function.get(&symbol) {
            return *slot;
        }

        let slot =
            self.declare_data_symbol(DataKind::PointerSlot, &format!("codeptr{}", symbol.index()));
        self.pointer_slot_for_function.insert(symbol, slot);
        slot
    }

    pub(crate) fn import_thunks(&mut self, function: &mut ir::Function) -> ImportedThunks {
        ImportedThunks::new_direct(function)
    }

    pub(crate) fn declare_runtime_data_in_func(
        &mut self,
        data: RuntimeData,
        function: &mut ir::Function,
    ) -> ir::GlobalValue {
        declare_runtime_data(function, data)
    }

    pub(crate) fn declare_procedures(&mut self) -> Vec<DeclaredProcedure<'gc>> {
        let procedures = self.program.procedures.clone();

        let sig = compiled_scheme_signature();
        let mut function_index = 0;
        let mut continuation_index = 0;
        let mut declared = Vec::with_capacity(procedures.len());
        for procedure in procedures.iter() {
            match procedure.code {
                CodeId::GraphFunction(_) => {
                    let i = function_index;
                    function_index += 1;
                    let binding = procedure.sources[&procedure.binding];
                    let name = format!("graph_fn{}:{}:{}", i, procedure.name, binding.name);
                    let func_id = self.declare_function_symbol(&name, &sig);
                    self.func_for_code.insert(procedure.code, func_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_graph_fn{}", i));
                    self.code_block_for_code
                        .insert(procedure.code, code_block_data_id);
                    declared.push(DeclaredProcedure {
                        procedure: procedure.clone(),
                        function: func_id,
                        name,
                    });
                }
                CodeId::GraphContinuation(_) => {
                    let i = continuation_index;
                    continuation_index += 1;
                    let binding = procedure.sources[&procedure.binding];
                    let name = format!("graph_cont{}:{}:{}", i, procedure.name, binding.name);
                    let cont_id = self.declare_function_symbol(&name, &sig);
                    self.func_for_code.insert(procedure.code, cont_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_graph_cont{}", i));
                    self.code_block_for_code
                        .insert(procedure.code, code_block_data_id);
                    declared.push(DeclaredProcedure {
                        procedure: procedure.clone(),
                        function: cont_id,
                        name,
                    });
                }
            }
        }

        declared
    }

    pub fn compile_loaded_fasl_bytes(&mut self) -> Result<Vec<u8>, String> {
        self.compile_loaded_fasl_bytes_with_dumps(&BackendDumpOptions::default())
    }

    pub fn compile_loaded_fasl_bytes_with_dumps(
        &mut self,
        backend_dumps: &BackendDumpOptions,
    ) -> Result<Vec<u8>, String> {
        let declared_procedures = self.declare_procedures();
        let debug_entry_function_ids = declared_procedures
            .iter()
            .map(|declared| declared.function.index())
            .collect::<HashSet<_>>();
        let isa = host_isa();
        let mut cache = CompileContext::new();
        let mut functions = Vec::with_capacity(declared_procedures.len());
        let mut pending_metadata = Vec::with_capacity(declared_procedures.len());
        let mut cranelift_dump = open_dump_file(backend_dumps.cranelift.as_deref())?;
        let mut disassembly_dump = open_dump_file(backend_dumps.disassembly.as_deref())?;

        self.compile_fasl_trampolines(&*isa, &mut cache, &mut functions)?;
        let procedure_function_start = functions.len();

        for declared in declared_procedures.iter() {
            cache.ctx.func = ir::Function::with_name_signature(
                ir::UserFuncName::user(0, declared.function.index()),
                compiled_scheme_signature(),
            );
            let builder = FunctionBuilder::new(&mut cache.ctx.func, &mut cache.builder_ctx);
            let thunks = self.import_thunks(builder.func);
            let (func_debug_cx, arity, is_cont, metadata) = match declared.procedure.code {
                CodeId::GraphFunction(_) => (
                    self.debug_context.define_procedure(
                        declared.procedure.source,
                        declared.procedure.name,
                        declared.procedure.sources[&declared.procedure.binding],
                        &declared.name,
                    ),
                    Self::arity_for_procedure(&declared.procedure),
                    false,
                    declared.procedure.meta,
                ),
                CodeId::GraphContinuation(_) => (
                    self.debug_context.define_procedure(
                        declared.procedure.source,
                        declared.procedure.name,
                        declared.procedure.sources[&declared.procedure.binding],
                        &declared.name,
                    ),
                    Self::arity_for_procedure(&declared.procedure),
                    true,
                    declared.procedure.meta,
                ),
            };
            let metadata = metadata_with_source(self.ctx, metadata, declared.procedure.source);
            let func_debug_cx = {
                let mut ssa = SsaBuilder::new(
                    self,
                    builder,
                    declared.procedure.clone(),
                    thunks,
                    func_debug_cx,
                );

                ssa.translate_procedure(&declared.procedure);
                ssa.finalize();
                ssa.builder.seal_all_blocks();
                ssa.builder.finalize();
                ssa.func_debug_cx
            };

            if let Some(file) = cranelift_dump.as_mut() {
                write_cranelift_dump(
                    file,
                    declared.function.index(),
                    &declared.name,
                    &cache.ctx.func,
                )?;
            }

            let compiled = match compile_function(&*isa, &mut cache) {
                Ok(compiled) => compiled,
                Err(err) => {
                    // Hard-coded paths so a verify failure is always inspectable,
                    // independent of CAPY_*_DUMP_DIR ownership / limits.
                    let dump_dir = std::env::var_os("CAPY_DUMP_DIR")
                        .map(PathBuf::from)
                        .unwrap_or_else(|| PathBuf::from("/tmp/capy-verify-fail"));
                    let _ = std::fs::create_dir_all(&dump_dir);
                    let safe_name = declared.name.replace(['/', '\\', ' '], "_");
                    let fail_path = dump_dir.join(format!("{safe_name}.clif"));
                    match File::create(&fail_path) {
                        Ok(mut file) => {
                            let _ = write_cranelift_dump(
                                &mut file,
                                declared.function.index(),
                                &declared.name,
                                &cache.ctx.func,
                            );
                            eprintln!(
                                ";; TRACE  (capy)@compile: verify-fail CLIF -> {}",
                                fail_path.display()
                            );
                        }
                        Err(io_err) => eprintln!(
                            ";; WARN  (capy)@compile: could not write {}: {io_err}",
                            fail_path.display()
                        ),
                    }
                    let ssa_path = dump_dir.join(format!("{safe_name}.ssa.txt"));
                    let rendered = crate::compiler::ssa::render_program(&Program {
                        entry: declared.procedure.code,
                        procedures: vec![declared.procedure.clone()],
                    });
                    match std::fs::write(&ssa_path, &rendered) {
                        Ok(()) => eprintln!(
                            ";; TRACE  (capy)@compile: verify-fail SSA -> {}",
                            ssa_path.display()
                        ),
                        Err(io_err) => eprintln!(
                            ";; WARN  (capy)@compile: could not write {}: {io_err}",
                            ssa_path.display()
                        ),
                    }
                    // Also dump the full module SSA for cross-procedure context.
                    let full_path = dump_dir.join("module.ssa.txt");
                    let full = crate::compiler::ssa::render_program(&self.program);
                    let _ = std::fs::write(&full_path, full);
                    eprintln!(
                        ";; TRACE  (capy)@compile: verify-fail module SSA -> {}",
                        full_path.display()
                    );
                    return Err(err);
                }
            };
            let source = disassembly_source(&func_debug_cx, &self.debug_context, &compiled);
            let source_map = code_source_map(&source);
            if let Some(file) = disassembly_dump.as_mut() {
                write_disassembly_dump(
                    file,
                    declared.function.index(),
                    &declared.name,
                    &compiled.bytes,
                    Some(&source),
                )?;
            }
            pending_metadata.push(metadata);
            functions.push(CompiledFaslFunction {
                symbol: declared.function,
                compiled,
                entry_offset: 0,
                arity,
                is_cont,
                metadata_constant: None,
                source_map,
            });
            cache.clear();
        }

        for metadata in &pending_metadata {
            let _ = self.intern_constant(*metadata);
        }

        let constant_indices = self.constant_indices();
        let constants = self
            .constants
            .iter()
            .map(|(key, symbol)| (key.0, *symbol))
            .collect::<Vec<_>>();
        let mut constants_by_index = vec![Value::undefined(); constants.len()];
        for (value, symbol) in &constants {
            if let Some(index) = constant_indices.get(symbol) {
                constants_by_index[*index as usize] = *value;
            }
        }
        let procedure_functions = &mut functions[procedure_function_start..];
        if procedure_functions.len() != pending_metadata.len() {
            return Err("procedure metadata count does not match compiled procedure count".into());
        }
        for (function, metadata) in procedure_functions.iter_mut().zip(pending_metadata) {
            function.metadata_constant = self
                .intern_constant(metadata)
                .and_then(|symbol| constant_indices.get(&symbol).copied());
        }
        let data_slots = self.fasl_data_slots(&constant_indices);
        let entry_code = self
            .func_for_code
            .get(&self.program.entry)
            .copied()
            .ok_or_else(|| "entry function was not declared".to_string())?;

        let mut function_ids = HashSet::new();
        for function in &functions {
            let code_id = function.symbol.index();
            if !function_ids.insert(code_id) {
                return Err(format!("duplicate function symbol id {code_id}"));
            }
        }
        if !function_ids.contains(&entry_code.index()) {
            return Err(format!(
                "entry function symbol id {} was not declared",
                entry_code.index()
            ));
        }

        let mut graph_len = function_ids
            .iter()
            .copied()
            .max()
            .unwrap_or(0)
            .saturating_add(1);
        let mut value_defs = Vec::new();
        let mut data_slot_targets = HashMap::new();
        let mut data_ids = HashSet::new();
        for slot in &data_slots {
            let slot_id = slot.symbol.index();
            if !data_ids.insert(slot_id) {
                return Err(format!("duplicate data symbol id {slot_id}"));
            }
            match slot.kind {
                DataKind::CodeBlock => {
                    let code_id = slot
                        .code
                        .ok_or_else(|| {
                            "code-block data slot requires a function symbol".to_string()
                        })?
                        .index();
                    if !function_ids.contains(&code_id) {
                        return Err(format!(
                            "code-block data slot references undeclared function symbol id {code_id}"
                        ));
                    }
                    data_slot_targets.insert(slot_id, RelocTarget::Object(code_id));
                }
                DataKind::Constant => {
                    let constant_index = slot.constant_index.ok_or_else(|| {
                        "constant data slot requires a constant index".to_string()
                    })?;
                    let value = constants_by_index
                        .get(constant_index as usize)
                        .copied()
                        .ok_or_else(|| "constant data slot index out of bounds".to_string())?;
                    let index = graph_len;
                    graph_len = graph_len
                        .checked_add(1)
                        .ok_or_else(|| "unified FASL graph is too large".to_string())?;
                    data_slot_targets.insert(slot_id, RelocTarget::Object(index));
                    value_defs.push(GraphValueSpec::new(index, value));
                }
                DataKind::CacheCell => {
                    let index = graph_len;
                    graph_len = graph_len
                        .checked_add(1)
                        .ok_or_else(|| "unified FASL graph is too large".to_string())?;
                    data_slot_targets.insert(slot_id, RelocTarget::Object(index));
                    value_defs.push(GraphValueSpec::new(index, Value::new(false)));
                }
                DataKind::PointerSlot => {
                    let code_id = slot
                        .pointer_code
                        .ok_or_else(|| "pointer data slot requires a code target".to_string())?;
                    data_slot_targets.insert(
                        slot_id,
                        if debug_entry_function_ids.contains(&code_id) {
                            RelocTarget::DebugEntry(code_id)
                        } else {
                            RelocTarget::Entry(code_id)
                        },
                    );
                }
                DataKind::SideMetadata => {
                    let kind = slot.side_metadata.ok_or_else(|| {
                        "side-metadata data slot requires a side metadata kind".to_string()
                    })?;
                    data_slot_targets.insert(slot_id, RelocTarget::SideMetadata(kind));
                }
                other => {
                    return Err(format!(
                        "data symbol kind {other:?} cannot be represented as a unified FASL data slot"
                    ));
                }
            }
        }

        let mut converted_relocations = Vec::with_capacity(functions.len());
        for function in &functions {
            converted_relocations.push(
                function
                    .compiled
                    .relocs
                    .iter()
                    .map(|relocation| {
                        fasl_relocation_from_direct_relocation(
                            relocation,
                            &data_slot_targets,
                            &debug_entry_function_ids,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            );
        }

        let mut code_defs = Vec::with_capacity(functions.len());
        for (function, relocations) in functions.iter().zip(converted_relocations.iter()) {
            let metadata = function
                .metadata_constant
                .map(|index| {
                    constants_by_index
                        .get(index as usize)
                        .copied()
                        .ok_or_else(|| {
                            "unified FASL metadata constant index out of bounds".to_string()
                        })
                })
                .transpose()?
                .unwrap_or_else(|| Value::new(false));
            code_defs.push(GraphCodeSpec::new(
                function.symbol.index(),
                CodeSpec::new(
                    &function.compiled.bytes,
                    function.entry_offset,
                    function.arity,
                    function.is_cont,
                    metadata,
                    relocations,
                    &function.source_map,
                ),
            ));
        }

        let mut bytes = Vec::new();
        let program = ProgramSpec::new(
            graph_len,
            &value_defs,
            &code_defs,
            entry_code.index(),
            false,
        );
        Writer::new(self.ctx, &mut bytes)
            .write_image(Image::Program(&program), Compression::Gzip)
            .map_err(|err| err.to_string())?;
        Ok(bytes)
    }

    fn compile_fasl_trampolines(
        &mut self,
        isa: &dyn cranelift_codegen::isa::TargetIsa,
        cache: &mut CompileContext,
        functions: &mut Vec<CompiledFaslFunction>,
    ) -> Result<(), String> {
        self.compile_fasl_trampoline(
            isa,
            cache,
            functions,
            self.raise_to_exception_handler_trampoline,
            "capy_raise_to_exception_handler",
            Self::build_raise_to_exception_handler_trampoline,
        )?;
        self.compile_fasl_trampoline(
            isa,
            cache,
            functions,
            self.wrong_arity_trampoline,
            "capy_raise_wrong_arity",
            Self::build_wrong_arity_trampoline,
        )?;
        self.compile_fasl_trampoline(
            isa,
            cache,
            functions,
            self.generic_apply_trampoline,
            "capy_generic_apply",
            Self::build_generic_apply_trampoline,
        )?;
        self.compile_fasl_trampoline(
            isa,
            cache,
            functions,
            self.generic_tail_apply_trampoline,
            "capy_generic_tail_apply",
            Self::build_generic_tail_apply_trampoline,
        )?;

        let raise_trampolines = self.raise_trampolines.clone();
        for (arity, symbol) in raise_trampolines.into_iter().enumerate() {
            self.compile_fasl_trampoline(
                isa,
                cache,
                functions,
                symbol,
                &format!("capy_raise{arity}"),
                Self::build_raise_trampoline,
            )?;
        }

        Ok(())
    }

    fn compile_fasl_trampoline(
        &mut self,
        isa: &dyn cranelift_codegen::isa::TargetIsa,
        cache: &mut CompileContext,
        functions: &mut Vec<CompiledFaslFunction>,
        symbol: FunctionSymbol,
        _name: &str,
        build: fn(&mut Self, &mut cranelift_codegen::Context, &mut FunctionBuilderContext),
    ) -> Result<(), String> {
        cache.ctx.func = ir::Function::with_name_signature(
            ir::UserFuncName::user(0, symbol.index()),
            compiled_scheme_signature(),
        );
        build(self, &mut cache.ctx, &mut cache.builder_ctx);
        let compiled = compile_function(isa, cache)?;
        functions.push(CompiledFaslFunction {
            symbol,
            compiled,
            entry_offset: 0,
            arity: 0,
            is_cont: false,
            metadata_constant: None,
            source_map: Vec::new(),
        });
        cache.clear();
        Ok(())
    }

    fn build_raise_to_exception_handler_trampoline(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        context.func.signature = compiled_scheme_signature();
        let mut builder = FunctionBuilder::new(&mut context.func, fctx);
        let thunks = self.import_thunks(builder.func);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let err = builder.block_params(entry)[0];
        let retk_or_zero = builder.block_params(entry)[2];
        let ctx = builder.ins().get_pinned_reg(clif_types::I64);

        let load_default_retk = builder.create_block();
        let got_retk = builder.create_block();
        builder.append_block_param(got_retk, clif_types::I64);
        builder.func.layout.set_cold(load_default_retk);

        let is_zero = builder
            .ins()
            .icmp_imm(ir::condcodes::IntCC::Equal, retk_or_zero, 0);
        builder.ins().brif(
            is_zero,
            load_default_retk,
            &[],
            got_retk,
            &[BlockArg::Value(retk_or_zero)],
        );

        builder.switch_to_block(load_default_retk);
        let default_retk = builder.ins().call(thunks.default_retk, &[ctx]);
        let default_retk = builder.inst_results(default_retk)[0];
        builder
            .ins()
            .jump(got_retk, &[BlockArg::Value(default_retk)]);

        builder.switch_to_block(got_retk);
        let retk = builder.block_params(got_retk)[0];
        let handler = builder.ins().call(thunks.exception_handler, &[ctx]);
        let handler = builder.inst_results(handler)[0];
        let handler_code = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let argc = builder.ins().iconst(clif_types::I64, 2);
        let undefined = builder
            .ins()
            .iconst(clif_types::I64, Value::undefined().bits() as i64);
        builder.ins().return_call_indirect(
            sig_call,
            handler_code,
            &[handler, argc, retk, err, undefined, undefined],
        );
        builder.seal_all_blocks();
        builder.finalize();
    }

    fn build_wrong_arity_trampoline(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        context.func.signature = compiled_scheme_signature();
        let mut builder = FunctionBuilder::new(&mut context.func, fctx);
        let thunks = self.import_thunks(builder.func);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let rator = builder.block_params(entry)[0];
        let actual_argc = builder.block_params(entry)[1];
        let retk_or_zero = builder.block_params(entry)[2];
        let got = builder.block_params(entry)[3];
        let expected = builder.block_params(entry)[4];

        let ctx = builder.ins().get_pinned_reg(clif_types::I64);
        let state = builder.ins().iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let overflow = overflow_base_from_argc(&mut builder, state, actual_argc);
        builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            overflow,
            state,
            offset_of!(State, runstack) as i32,
        );

        let code = builder.ins().iconst(
            clif_types::I64,
            crate::runtime::vm::exceptions::RaiseKind::WrongNumberOfArguments.code() as i64,
        );
        let raise_argc = builder.ins().iconst(clif_types::I64, 4);
        let undefined = builder
            .ins()
            .iconst(clif_types::I64, Value::undefined().bits() as i64);
        let from = builder.ins().iconst(clif_types::I64, 1);
        let condition = builder.ins().call(
            thunks.raise_condition_regs,
            &[
                ctx,
                code,
                raise_argc,
                retk_or_zero,
                rator,
                got,
                expected,
                overflow,
                from,
            ],
        );
        let condition = builder.inst_results(condition)[0];

        let load_default_retk = builder.create_block();
        let got_retk = builder.create_block();
        builder.append_block_param(got_retk, clif_types::I64);
        builder.func.layout.set_cold(load_default_retk);

        let is_zero = builder
            .ins()
            .icmp_imm(ir::condcodes::IntCC::Equal, retk_or_zero, 0);
        builder.ins().brif(
            is_zero,
            load_default_retk,
            &[],
            got_retk,
            &[BlockArg::Value(retk_or_zero)],
        );

        builder.switch_to_block(load_default_retk);
        let default_retk = builder.ins().call(thunks.default_retk, &[ctx]);
        let default_retk = builder.inst_results(default_retk)[0];
        builder
            .ins()
            .jump(got_retk, &[BlockArg::Value(default_retk)]);

        builder.switch_to_block(got_retk);
        let retk = builder.block_params(got_retk)[0];
        let handler = builder.ins().call(thunks.exception_handler, &[ctx]);
        let handler = builder.inst_results(handler)[0];
        let handler_code = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let handler_argc = builder.ins().iconst(clif_types::I64, 2);
        builder.ins().return_call_indirect(
            sig_call,
            handler_code,
            &[handler, handler_argc, retk, condition, undefined, undefined],
        );
        builder.seal_all_blocks();
        builder.finalize();
    }

    fn build_raise_trampoline(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        context.func.signature = compiled_scheme_signature();
        let mut builder = FunctionBuilder::new(&mut context.func, fctx);
        let thunks = self.import_thunks(builder.func);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let code = builder.block_params(entry)[0];
        let argc = builder.block_params(entry)[1];
        let arg0 = builder.block_params(entry)[2];
        let arg1 = builder.block_params(entry)[3];
        let arg2 = builder.block_params(entry)[4];
        let arg3 = builder.block_params(entry)[5];

        let ctx = builder.ins().get_pinned_reg(clif_types::I64);
        let state = builder.ins().iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let overflow = overflow_base_from_argc(&mut builder, state, argc);
        let from = builder.ins().iconst(clif_types::I64, 1);
        let condition = builder.ins().call(
            thunks.raise_condition_with_source_regs,
            &[ctx, code, argc, arg0, arg1, arg2, arg3, overflow, from],
        );
        let condition = builder.inst_results(condition)[0];
        builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            overflow,
            state,
            offset_of!(State, runstack) as i32,
        );

        let handler = builder.ins().call(thunks.exception_handler, &[ctx]);
        let handler = builder.inst_results(handler)[0];
        let handler_code = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let argc = builder.ins().iconst(clif_types::I64, 2);
        let undefined = builder
            .ins()
            .iconst(clif_types::I64, Value::undefined().bits() as i64);
        builder.ins().return_call_indirect(
            sig_call,
            handler_code,
            &[handler, argc, arg0, condition, undefined, undefined],
        );
        builder.seal_all_blocks();
        builder.finalize();
    }

    fn build_generic_apply_trampoline(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        self.build_generic_apply_trampoline_inner(context, fctx, true);
    }

    fn build_generic_tail_apply_trampoline(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
    ) {
        self.build_generic_apply_trampoline_inner(context, fctx, false);
    }

    fn build_generic_apply_trampoline_inner(
        &mut self,
        context: &mut cranelift_codegen::Context,
        fctx: &mut FunctionBuilderContext,
        has_retk: bool,
    ) {
        context.func.signature = compiled_scheme_signature();
        let mut builder = FunctionBuilder::new(&mut context.func, fctx);
        let thunks = self.import_thunks(builder.func);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let generic = builder.block_params(entry)[0];
        let argc = builder.block_params(entry)[1];
        let arg0 = builder.block_params(entry)[2];
        let arg1 = builder.block_params(entry)[3];
        let arg2 = builder.block_params(entry)[4];
        let arg3 = builder.block_params(entry)[5];

        let ctx = builder.ins().get_pinned_reg(clif_types::I64);
        let state = builder.ins().iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let value_tag = builder.ins().band_imm(generic, Value::NOT_CELL_MASK);
        let is_cell = builder
            .ins()
            .icmp_imm(ir::condcodes::IntCC::Equal, value_tag, 0);
        let non_zero = builder
            .ins()
            .icmp_imm(ir::condcodes::IntCC::NotEqual, generic, 0);
        let is_heap_object = builder.ins().band(is_cell, non_zero);
        let check_closure_tag = builder.create_block();
        let closure_call = builder.create_block();
        let generic_call = builder.create_block();
        builder
            .ins()
            .brif(is_heap_object, check_closure_tag, &[], generic_call, &[]);

        builder.switch_to_block(check_closure_tag);
        let header = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            generic,
            OBJECT_HEADER_OFFSET as i32,
        );
        let object_class_id = builder.ins().band_imm(header, 0x00ff_ffff);
        let is_closure = builder.ins().icmp_imm(
            ir::condcodes::IntCC::Equal,
            object_class_id,
            builtin_class_ids::CLOSURE as i64,
        );
        builder
            .ins()
            .brif(is_closure, closure_call, &[], generic_call, &[]);

        builder.switch_to_block(closure_call);
        let code = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            generic,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        builder.ins().return_call_indirect(
            sig_call,
            code,
            &[generic, argc, arg0, arg1, arg2, arg3],
        );

        builder.switch_to_block(generic_call);
        let overflow = overflow_base_from_argc(&mut builder, state, argc);
        let has_retk = builder
            .ins()
            .iconst(clif_types::I8, u8::from(has_retk) as i64);
        let call = builder.ins().call(
            thunks.generic_apply_regs,
            &[
                ctx, generic, argc, arg0, arg1, arg2, arg3, overflow, has_retk,
            ],
        );
        let code = builder.inst_results(call)[0];
        let value = builder.inst_results(call)[1];

        let on_ret = builder.create_block();
        let on_cont = builder.create_block();
        let is_cont = builder.ins().icmp_imm(
            ir::condcodes::IntCC::Equal,
            code,
            ReturnCode::Continue as i64,
        );
        builder.ins().brif(is_cont, on_cont, &[], on_ret, &[]);

        builder.switch_to_block(on_ret);
        builder
            .ins()
            .call(thunks.scheme_longjmp, &[ctx, code, value]);
        builder
            .ins()
            .trap(cranelift_codegen::ir::TrapCode::STACK_OVERFLOW);

        builder.switch_to_block(on_cont);
        let cdata = offset_of!(State, call_data) as i32;
        let rator = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        let argc = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        let arg0 = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, arg0) as i32,
        );
        let arg1 = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, arg1) as i32,
        );
        let arg2 = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, arg2) as i32,
        );
        let arg3 = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            state,
            cdata + offset_of!(CallData, arg3) as i32,
        );
        let zero = builder.ins().iconst(clif_types::I64, 0);
        let undefined = builder
            .ins()
            .iconst(clif_types::I64, Value::undefined().bits() as i64);
        builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            undefined,
            state,
            cdata + offset_of!(CallData, rator) as i32,
        );
        builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            zero,
            state,
            cdata + offset_of!(CallData, argc) as i32,
        );
        for offset in [
            offset_of!(CallData, arg0),
            offset_of!(CallData, arg1),
            offset_of!(CallData, arg2),
            offset_of!(CallData, arg3),
        ] {
            builder.ins().store(
                ir::MemFlags::trusted().with_can_move(),
                undefined,
                state,
                cdata + offset as i32,
            );
        }

        let body_code = builder.ins().load(
            clif_types::I64,
            ir::MemFlags::trusted().with_can_move(),
            rator,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        builder.ins().return_call_indirect(
            sig_call,
            body_code,
            &[rator, argc, arg0, arg1, arg2, arg3],
        );

        builder.seal_all_blocks();
        builder.finalize();
    }

    fn constant_indices(&mut self) -> HashMap<DataSymbol, u32> {
        let metadata = self
            .program
            .procedures
            .iter()
            .map(|procedure| procedure.meta)
            .collect::<Vec<_>>();

        for metadata in metadata {
            let _ = self.intern_constant(metadata);
        }

        let constants = self
            .constants
            .iter()
            .map(|(key, id)| (key.0, *id))
            .collect::<Vec<_>>();
        constants
            .iter()
            .enumerate()
            .map(|(index, (_, symbol))| (*symbol, index as u32))
            .collect::<HashMap<_, _>>()
    }

    fn fasl_data_slots(&self, constant_indices: &HashMap<DataSymbol, u32>) -> Vec<DataSlot> {
        let mut slots = Vec::new();

        for symbol in self.constants.values().copied() {
            slots.push(DataSlot::constant(
                symbol,
                constant_indices.get(&symbol).copied(),
            ));
        }
        for symbol in self.cache_cells.values().copied() {
            slots.push(DataSlot::cache_cell(symbol));
        }
        for (code, symbol) in self.code_block_for_code.iter() {
            slots.push(DataSlot::code(
                *symbol,
                self.func_for_code.get(code).copied(),
            ));
        }
        for (function, symbol) in self.pointer_slot_for_function.iter() {
            slots.push(DataSlot::pointer(*symbol, *function));
        }
        slots.push(DataSlot::side_metadata(
            self.global_side_metadata_base_address,
            SideMetadataSlot::Global,
        ));
        slots.push(DataSlot::side_metadata(
            self.vo_bit_side_metadata_base_address,
            SideMetadataSlot::VoBit,
        ));

        slots
    }

    /// Add an object to constant table, and return a data ID that can be used
    /// to reference it. If object is already present in constant table, returns
    /// the existing data ID.
    ///
    /// If object is immediate, no data ID is returned.
    pub fn intern_constant(&mut self, obj: Value<'gc>) -> Option<DataSymbol> {
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
        let data_id = self.declare_data_symbol(DataKind::Constant, &name);

        self.constants.insert(ValueEqual(obj), data_id);

        Some(data_id)
    }

    pub fn intern_cache_cell(&mut self, key: Value<'gc>) -> DataSymbol {
        if let Some(data_id) = self.cache_cells.get(&ValueEqual(key)) {
            return *data_id;
        }

        let ix = self.cache_cells.len();
        let name = format!("cache_cell{}", ix);

        let data_id = self.declare_data_symbol(DataKind::CacheCell, &name);

        self.cache_cells.insert(ValueEqual(key), data_id);

        data_id
    }

    fn declare_code_block_slot(&mut self, name: &str) -> DataSymbol {
        self.declare_data_symbol(DataKind::CodeBlock, name)
    }

    fn arity_for_procedure(procedure: &Procedure<'gc>) -> i32 {
        if procedure.variadic.is_some() {
            -((procedure.params.len() as i32) + 1)
        } else {
            procedure.params.len() as i32
        }
    }
}

pub struct SsaBuilder<'gc, 'a, 'f> {
    pub module_builder: &'a mut ModuleBuilder<'gc>,
    pub builder: FunctionBuilder<'f>,
    pub(crate) func_debug_cx: FunctionDebugContext<'gc>,

    pub block_map: HashMap<BlockId, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,
    pub ssa_variables: HashMap<ValueId, VarDef>,
    pub rest_sources: HashMap<ValueId, RestSource>,
    pub synthetic_aliases: HashMap<ValueId, LVarRef<'gc>>,

    pub target: Procedure<'gc>,
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

    pub data_imports: HashMap<DataSymbol, ir::GlobalValue>,

    pub srcloc: Option<SourceLoc>,
}

impl<'gc, 'a, 'f> SsaBuilder<'gc, 'a, 'f> {
    pub(crate) fn new(
        module_builder: &'a mut ModuleBuilder<'gc>,
        mut builder: FunctionBuilder<'f>,
        target: Procedure<'gc>,
        thunks: ImportedThunks,
        mut func_debug_cx: FunctionDebugContext<'gc>,
    ) -> Self {
        builder.func.dfg.collect_debug_info();
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        let rator = builder.block_params(entry)[0];
        let argc = builder.block_params(entry)[1];
        let args = [
            builder.block_params(entry)[2],
            builder.block_params(entry)[3],
            builder.block_params(entry)[4],
            builder.block_params(entry)[5],
        ];

        let variables = HashMap::new();

        let sig_call = compiled_scheme_signature();
        let sig_call = builder.import_signature(sig_call);

        let exit_block = builder.create_block();

        builder.append_block_param(exit_block, clif_types::I64); /* code */
        builder.append_block_param(exit_block, clif_types::I64); /* rator */
        builder.append_block_param(exit_block, clif_types::I64); /* argc */
        for _ in 0..REGISTER_ARG_COUNT {
            builder.append_block_param(exit_block, clif_types::I64);
        }

        builder.set_val_label(rator, func_debug_cx.internal_variable(0));
        builder.set_val_label(argc, func_debug_cx.internal_variable(1));
        for (index, arg) in args.iter().copied().enumerate() {
            builder.set_val_label(arg, func_debug_cx.internal_variable((index + 2) as u32));
        }

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        let entry_args = std::iter::once(rator)
            .chain(std::iter::once(argc))
            .chain(args)
            .map(BlockArg::Value)
            .collect::<Vec<_>>();
        builder.ins().jump(entry_block, &entry_args);
        builder.switch_to_block(entry_block);
        let entry_rator = builder.block_params(entry_block)[0];
        let entry_argc = builder.block_params(entry_block)[1];
        let entry_args = [
            builder.block_params(entry_block)[2],
            builder.block_params(entry_block)[3],
            builder.block_params(entry_block)[4],
            builder.block_params(entry_block)[5],
        ];
        builder.set_val_label(entry_rator, func_debug_cx.internal_variable(0));
        builder.set_val_label(entry_argc, func_debug_cx.internal_variable(1));
        for (index, arg) in entry_args.iter().copied().enumerate() {
            builder.set_val_label(arg, func_debug_cx.internal_variable((index + 2) as u32));
        }

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
            ssa_variables: HashMap::new(),
            rest_sources: HashMap::new(),
            synthetic_aliases: HashMap::new(),
            block_map: HashMap::new(),
            thunks,

            sig_call,

            data_imports: HashMap::new(),
            srcloc: None,
        };

        this.entrypoint(entry_argc, entry_args);

        this
    }
}

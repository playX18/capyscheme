//! SSA (Static Single Assignment) code generation using Cranelift.

#[cfg(test)]
use crate::compiler::codegen::declare_backend_function_import;
use crate::{
    compiler::{
        codegen::{
            BackendSymbol, DataSymbol, DataSymbolKind, FunctionCompileContext, FunctionSymbol,
            declare_backend_data_symbol, declare_runtime_data_symbol, host_isa,
        },
        debuginfo::{DebugContext, FunctionDebugContext},
        direct::{DirectRelocation, DirectRelocationTarget, compile_function},
        ssa::primitive::PrimitiveLowerer,
    },
    cps::{
        ReifyInfo,
        linear::{BlockId, CodeId, LinearProgram, Procedure, ValueId},
        term::{ContRef, FuncRef},
    },
    expander::core::LVarRef,
    runtime::{
        COMPILED_ENTRY_ARG_COUNT, Context, REGISTER_ARG_COUNT, State,
        fasl::{
            FASLWriter, FaslCodeBlockSpec, FaslGraphCodeBlockSpec, FaslGraphValueSpec,
            FaslProgramSpec,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind},
        },
        symbols::RuntimeData,
        value::{Closure, Value, ValueEqual, Vector},
    },
};

use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, InstBuilder, types};
use cranelift_codegen::{
    binemit::Reloc,
    ir::{self, BlockArg, SourceLoc},
    isa::CallConv,
};

use crate::rsgc::Gc;
use std::{
    collections::{HashMap, HashSet},
    mem::{offset_of, size_of},
};

use crate::runtime::vm::thunks::*;

pub mod helpers;
pub mod linear;
pub mod primitive;
pub mod traits;

pub(crate) use linear::AllocInfoPreset;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDef {
    Value(ir::Value),
    Comparison(ir::Value),
}

#[derive(Clone, Copy)]
pub struct LinearRestSource {
    pub argc: ir::Value,
    pub args: [ir::Value; REGISTER_ARG_COUNT],
    pub overflow: ir::Value,
    pub first_rest: usize,
}

#[derive(Clone, Copy)]
pub struct RegisterCallArgs {
    pub argc: ir::Value,
    pub args: [ir::Value; REGISTER_ARG_COUNT],
    pub overflow: ir::Value,
}

pub(crate) fn overflow_base_from_argc(
    builder: &mut FunctionBuilder<'_>,
    state: ir::Value,
    argc: ir::Value,
) -> ir::Value {
    let overflow_count = builder.ins().iadd_imm(argc, -(REGISTER_ARG_COUNT as i64));
    let zero = builder.ins().iconst(types::I64, 0);
    let has_overflow = builder.ins().icmp_imm(
        ir::condcodes::IntCC::UnsignedGreaterThan,
        argc,
        REGISTER_ARG_COUNT as i64,
    );
    let overflow_count = builder.ins().select(has_overflow, overflow_count, zero);
    let overflow_bytes = builder
        .ins()
        .imul_imm(overflow_count, size_of::<Value>() as i64);
    let runstack = builder.ins().load(
        types::I64,
        ir::MemFlags::trusted().with_can_move(),
        state,
        offset_of!(State, runstack) as i32,
    );
    builder.ins().isub(runstack, overflow_bytes)
}

pub(crate) fn compiled_scheme_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::Tail);
    for _ in 0..COMPILED_ENTRY_ARG_COUNT {
        sig.params.push(ir::AbiParam::new(types::I64));
    }
    sig.returns.push(ir::AbiParam::new(types::I64));
    sig.returns.push(ir::AbiParam::new(types::I64));
    sig
}

pub(crate) const MAX_RAISE_ARITY: usize = 4;

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

fn fasl_relocation_from_direct_relocation(
    relocation: &DirectRelocation,
    data_slot_targets: &HashMap<u32, FaslRelocTarget>,
) -> Result<FaslRelocation, String> {
    let (target, abs8_kind) = match relocation.target {
        DirectRelocationTarget::BackendSymbol(BackendSymbol::Function(symbol)) => (
            FaslRelocTarget::Entry(symbol.index()),
            FaslRelocKind::CodeEntry,
        ),
        DirectRelocationTarget::BackendSymbol(BackendSymbol::Data { kind, symbol }) => {
            if kind == DataSymbolKind::RuntimeData {
                (
                    FaslRelocTarget::RuntimeSymbol(symbol.index()),
                    FaslRelocKind::RuntimeData,
                )
            } else if kind == DataSymbolKind::CacheCell {
                (
                    match data_slot_targets.get(&symbol.index()).copied().ok_or_else(|| {
                        format!(
                            "data slot {} cannot be represented as a unified FASL data slot yet",
                            symbol.index()
                        )
                    })? {
                        FaslRelocTarget::Object(index) => FaslRelocTarget::CacheCell(index),
                        other => other,
                    },
                    FaslRelocKind::CacheCell,
                )
            } else {
                (
                    data_slot_targets.get(&symbol.index()).copied().ok_or_else(|| {
                        format!(
                            "data slot {} cannot be represented as a unified FASL data slot yet",
                            symbol.index()
                        )
                    })?,
                    FaslRelocKind::DataSlotAddress,
                )
            }
        }
        DirectRelocationTarget::BackendSymbol(BackendSymbol::Imported { kind, symbol }) => {
            match kind {
                crate::compiler::codegen::ImportedSymbolKind::RuntimeThunk => (
                    FaslRelocTarget::RuntimeSymbol(symbol.index()),
                    FaslRelocKind::RuntimeThunk,
                ),
                crate::compiler::codegen::ImportedSymbolKind::Trampoline => {
                    return Err(
                        "trampoline relocations are not supported in unified FASL yet".to_string(),
                    );
                }
            }
        }
        DirectRelocationTarget::FunctionOffset(offset) => {
            return Err(format!(
                "function-offset relocation target {offset} cannot be encoded in unified FASL"
            ));
        }
    };

    let kind = match relocation.kind {
        Reloc::Abs8 => abs8_kind,
        kind if matches!(
            abs8_kind,
            FaslRelocKind::DataSlotAddress | FaslRelocKind::CacheCell
        ) =>
        {
            FaslRelocKind::CraneliftDataSlot(kind)
        }
        kind => FaslRelocKind::Cranelift(kind),
    };

    Ok(FaslRelocation {
        offset: relocation.offset,
        kind,
        target,
        addend: relocation.addend,
    })
}

/// A SSA Builder. Constructs Cranelift module and SSA from single compilation unit.
pub struct ModuleBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub(crate) debug_context: DebugContext<'gc>,
    pub reify_info: ReifyInfo<'gc>,
    pub linear: LinearProgram<'gc>,
    pub constants: HashMap<ValueEqual<'gc>, DataSymbol>,
    pub cache_cells: HashMap<ValueEqual<'gc>, DataSymbol>,

    next_function_symbol: u32,
    data_kinds: HashMap<DataSymbol, DataSymbolKind>,
    next_data_symbol: u32,

    pub prims: PrimitiveLowerer<'gc>,
    pub func_for_cont: HashMap<ContRef<'gc>, FunctionSymbol>,
    pub func_for_func: HashMap<FuncRef<'gc>, FunctionSymbol>,
    pub code_block_for_cont: HashMap<ContRef<'gc>, DataSymbol>,
    pub code_block_for_func: HashMap<FuncRef<'gc>, DataSymbol>,
    pub pointer_slot_for_function: HashMap<FunctionSymbol, DataSymbol>,
    pub raise_trampolines: Vec<FunctionSymbol>,
    pub raise_to_exception_handler_trampoline: FunctionSymbol,
    pub wrong_arity_trampoline: FunctionSymbol,
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
    compiled: crate::compiler::direct::DirectCompiledFunction,
    entry_offset: u32,
    arity: i32,
    is_cont: bool,
    metadata_constant: Option<u32>,
}

#[derive(Clone, Copy)]
struct FaslDataSlot {
    symbol: DataSymbol,
    kind: DataSymbolKind,
    constant_index: Option<u32>,
    code: Option<FunctionSymbol>,
    pointer_code: Option<u32>,
    side_metadata: Option<SideMetadataSlotKind>,
}

impl<'gc> ModuleBuilder<'gc> {
    pub fn new(ctx: Context<'gc>, reify_info: ReifyInfo<'gc>, linear: LinearProgram<'gc>) -> Self {
        let prims = PrimitiveLowerer::new(ctx);
        let isa = host_isa();
        let debug_context = DebugContext::new(&reify_info, &*isa);
        let mut next_function_symbol = 0;
        let mut data_kinds = HashMap::new();
        let mut next_data_symbol = 0;
        let global_side_metadata_base_address =
            declare_direct_data(&mut next_data_symbol, "__GLOBAL_SIDE_METADATA_VM_ADDRESS");
        data_kinds.insert(
            global_side_metadata_base_address,
            DataSymbolKind::SideMetadata,
        );
        let vo_bit_side_metadata_base_address =
            declare_direct_data(&mut next_data_symbol, "__VO_BIT_SIDE_METADATA_VM_ADDRESS");
        data_kinds.insert(
            vo_bit_side_metadata_base_address,
            DataSymbolKind::SideMetadata,
        );
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
        Self {
            debug_context,
            ctx,
            stacktraces: true,
            reify_info,
            linear,
            constants: HashMap::new(),
            cache_cells: HashMap::new(),
            next_function_symbol,
            data_kinds,
            next_data_symbol,
            prims,
            func_for_cont: HashMap::new(),
            func_for_func: HashMap::new(),
            code_block_for_cont: HashMap::new(),
            code_block_for_func: HashMap::new(),
            pointer_slot_for_function: HashMap::new(),
            raise_trampolines,
            raise_to_exception_handler_trampoline,
            wrong_arity_trampoline,
            global_side_metadata_base_address,
            vo_bit_side_metadata_base_address,
        }
    }

    fn declare_function_symbol(&mut self, name: &str, sig: &ir::Signature) -> FunctionSymbol {
        declare_direct_function(&mut self.next_function_symbol, name, sig)
    }

    fn declare_data_symbol(&mut self, kind: DataSymbolKind, name: &str) -> DataSymbol {
        let symbol = declare_direct_data(&mut self.next_data_symbol, name);
        self.data_kinds.insert(symbol, kind);
        symbol
    }

    #[allow(dead_code)]
    pub(crate) fn data_kind(&self, symbol: DataSymbol) -> DataSymbolKind {
        self.data_kinds
            .get(&symbol)
            .copied()
            .expect("data symbol should have a direct backend kind")
    }

    #[cfg(test)]
    pub(crate) fn declare_function_in_func(
        &mut self,
        symbol: FunctionSymbol,
        function: &mut ir::Function,
    ) -> ir::FuncRef {
        let signature = function.import_signature(compiled_scheme_signature());
        declare_backend_function_import(function, BackendSymbol::Function(symbol), signature, false)
    }

    pub(crate) fn declare_data_in_func(
        &mut self,
        symbol: DataSymbol,
        function: &mut ir::Function,
    ) -> ir::GlobalValue {
        declare_backend_data_symbol(
            function,
            BackendSymbol::Data {
                kind: self.data_kind(symbol),
                symbol,
            },
            false,
            false,
        )
    }

    pub(crate) fn declare_function_pointer_slot(&mut self, symbol: FunctionSymbol) -> DataSymbol {
        if let Some(slot) = self.pointer_slot_for_function.get(&symbol) {
            return *slot;
        }

        let slot = self.declare_data_symbol(
            DataSymbolKind::PointerSlot,
            &format!("codeptr{}", symbol.index()),
        );
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
        declare_runtime_data_symbol(function, data)
    }

    pub(crate) fn declare_procedures(&mut self) -> Vec<DeclaredProcedure<'gc>> {
        let procedures = self.linear.procedures.clone();

        let sig = compiled_scheme_signature();
        let mut function_index = 0;
        let mut continuation_index = 0;
        let mut declared = Vec::with_capacity(procedures.len());
        for procedure in procedures.iter() {
            match procedure.code {
                CodeId::Function(func) => {
                    let i = function_index;
                    function_index += 1;
                    let name = format!("fn{}:{}:{}", i, func.name, func.binding.name);
                    let func_id = self.declare_function_symbol(&name, &sig);

                    self.func_for_func.insert(func, func_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_fn{}", i));
                    self.code_block_for_func.insert(func, code_block_data_id);
                    declared.push(DeclaredProcedure {
                        procedure: procedure.clone(),
                        function: func_id,
                        name,
                    });
                }
                CodeId::Continuation(cont) => {
                    let i = continuation_index;
                    continuation_index += 1;
                    let name = format!("cont{}:{}:{}", i, cont.name, cont.binding.name);
                    let cont_id = self.declare_function_symbol(&name, &sig);
                    self.func_for_cont.insert(cont, cont_id);
                    let code_block_data_id =
                        self.declare_code_block_slot(&format!("codeblock_cont{}", i));
                    self.code_block_for_cont.insert(cont, code_block_data_id);
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
        let declared_procedures = self.declare_procedures();
        let isa = host_isa();
        let mut cache = FunctionCompileContext::new();
        let mut functions = Vec::with_capacity(declared_procedures.len());
        let mut pending_metadata = Vec::with_capacity(declared_procedures.len());

        self.compile_fasl_trampolines(&*isa, &mut cache, &mut functions)?;

        for declared in declared_procedures.iter() {
            cache.context.func = ir::Function::with_name_signature(
                ir::UserFuncName::user(0, declared.function.index()),
                compiled_scheme_signature(),
            );
            let builder = FunctionBuilder::new(&mut cache.context.func, &mut cache.fctx);
            let thunks = self.import_thunks(builder.func);
            let (func_debug_cx, arity, is_cont, metadata) = match declared.procedure.code {
                CodeId::Function(func) => (
                    self.debug_context.define_function(func, &declared.name),
                    Self::arity_for_func(func),
                    false,
                    func.meta,
                ),
                CodeId::Continuation(cont) => (
                    self.debug_context.define_cont(cont, &declared.name),
                    Self::arity_for_cont(cont),
                    true,
                    cont.meta,
                ),
            };
            let mut ssa = SSABuilder::new(
                self,
                builder,
                declared.procedure.clone(),
                thunks,
                func_debug_cx,
            );

            ssa.linear_procedure(&declared.procedure);
            ssa.finalize();
            ssa.builder.seal_all_blocks();
            ssa.builder.finalize();

            let compiled = compile_function(&*isa, &mut cache)?;
            pending_metadata.push(metadata);
            functions.push(CompiledFaslFunction {
                symbol: declared.function,
                compiled,
                entry_offset: 0,
                arity,
                is_cont,
                metadata_constant: None,
            });
            cache.clear();
        }

        let (_constants_fasl, constant_indices) = self.constants_fasl_and_indices();
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
        for (function, metadata) in functions.iter_mut().zip(pending_metadata) {
            function.metadata_constant = self
                .intern_constant(metadata)
                .and_then(|symbol| constant_indices.get(&symbol).copied());
        }
        let data_slots = self.fasl_data_slots(&constant_indices);
        let entry_code = self
            .func_for_func
            .get(&self.reify_info.entrypoint)
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
                DataSymbolKind::CodeBlock => {
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
                    data_slot_targets.insert(slot_id, FaslRelocTarget::Object(code_id));
                }
                DataSymbolKind::Constant => {
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
                    data_slot_targets.insert(slot_id, FaslRelocTarget::Object(index));
                    value_defs.push(FaslGraphValueSpec { index, value });
                }
                DataSymbolKind::CacheCell => {
                    let index = graph_len;
                    graph_len = graph_len
                        .checked_add(1)
                        .ok_or_else(|| "unified FASL graph is too large".to_string())?;
                    data_slot_targets.insert(slot_id, FaslRelocTarget::Object(index));
                    value_defs.push(FaslGraphValueSpec {
                        index,
                        value: Value::new(false),
                    });
                }
                DataSymbolKind::PointerSlot => {
                    let code_id = slot
                        .pointer_code
                        .ok_or_else(|| "pointer data slot requires a code target".to_string())?;
                    data_slot_targets.insert(slot_id, FaslRelocTarget::Entry(code_id));
                }
                DataSymbolKind::SideMetadata => {
                    let kind = slot.side_metadata.ok_or_else(|| {
                        "side-metadata data slot requires a side metadata kind".to_string()
                    })?;
                    data_slot_targets.insert(slot_id, FaslRelocTarget::SideMetadata(kind));
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
                        fasl_relocation_from_direct_relocation(relocation, &data_slot_targets)
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
            code_defs.push(FaslGraphCodeBlockSpec {
                index: function.symbol.index(),
                code: FaslCodeBlockSpec {
                    bytes: &function.compiled.bytes,
                    entry_offset: function.entry_offset,
                    arity: function.arity,
                    is_cont: function.is_cont,
                    metadata,
                    relocations,
                },
            });
        }

        let mut bytes = Vec::new();
        FASLWriter::new(self.ctx, &mut bytes)
            .write_loaded_program(&FaslProgramSpec {
                graph_len,
                values: &value_defs,
                code_blocks: &code_defs,
                entry_code_index: entry_code.index(),
                entry_is_cont: false,
            })
            .map_err(|err| err.to_string())?;
        Ok(bytes)
    }

    fn compile_fasl_trampolines(
        &mut self,
        isa: &dyn cranelift_codegen::isa::TargetIsa,
        cache: &mut FunctionCompileContext,
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
        cache: &mut FunctionCompileContext,
        functions: &mut Vec<CompiledFaslFunction>,
        symbol: FunctionSymbol,
        _name: &str,
        build: fn(&mut Self, &mut cranelift_codegen::Context, &mut FunctionBuilderContext),
    ) -> Result<(), String> {
        cache.context.func = ir::Function::with_name_signature(
            ir::UserFuncName::user(0, symbol.index()),
            compiled_scheme_signature(),
        );
        build(self, &mut cache.context, &mut cache.fctx);
        let compiled = compile_function(isa, cache)?;
        functions.push(CompiledFaslFunction {
            symbol,
            compiled,
            entry_offset: 0,
            arity: 0,
            is_cont: false,
            metadata_constant: None,
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
        let ctx = builder.ins().get_pinned_reg(types::I64);

        let load_default_retk = builder.create_block();
        let got_retk = builder.create_block();
        builder.append_block_param(got_retk, types::I64);
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
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let argc = builder.ins().iconst(types::I64, 2);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
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

        let ctx = builder.ins().get_pinned_reg(types::I64);
        let state = builder.ins().iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let overflow = overflow_base_from_argc(&mut builder, state, actual_argc);
        builder.ins().store(
            ir::MemFlags::trusted().with_can_move(),
            overflow,
            state,
            offset_of!(State, runstack) as i32,
        );

        let code = builder.ins().iconst(
            types::I64,
            crate::runtime::vm::exceptions::RaiseKind::WrongNumberOfArguments.code() as i64,
        );
        let raise_argc = builder.ins().iconst(types::I64, 4);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        let from = builder.ins().iconst(types::I64, 1);
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
        builder.append_block_param(got_retk, types::I64);
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
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let handler_argc = builder.ins().iconst(types::I64, 2);
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

        let ctx = builder.ins().get_pinned_reg(types::I64);
        let state = builder.ins().iadd_imm(ctx, Context::OFFSET_OF_STATE as i64);
        let overflow = overflow_base_from_argc(&mut builder, state, argc);
        let from = builder.ins().iconst(types::I64, 1);
        let condition = builder.ins().call(
            thunks.raise_condition_regs,
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
            types::I64,
            ir::MemFlags::trusted().with_can_move(),
            handler,
            offset_of!(Closure, code) as i32,
        );
        let sig_call = builder.import_signature(compiled_scheme_signature());
        let argc = builder.ins().iconst(types::I64, 2);
        let undefined = builder
            .ins()
            .iconst(types::I64, Value::undefined().bits() as i64);
        builder.ins().return_call_indirect(
            sig_call,
            handler_code,
            &[handler, argc, arg0, condition, undefined, undefined],
        );
        builder.seal_all_blocks();
        builder.finalize();
    }

    fn constants_fasl_and_indices(&mut self) -> (Vec<u8>, HashMap<DataSymbol, u32>) {
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

        for func in funcs {
            let _ = self.intern_constant(func.meta);
        }
        for cont in conts {
            let _ = self.intern_constant(cont.meta);
        }

        let constants = self
            .constants
            .iter()
            .map(|(key, id)| (key.0, *id))
            .collect::<Vec<_>>();
        let constant_indices = constants
            .iter()
            .enumerate()
            .map(|(index, (_, symbol))| (*symbol, index as u32))
            .collect::<HashMap<_, _>>();

        let vec = Vector::new::<true>(*self.ctx, constants.len(), Value::undefined());
        let wvec = Gc::write(*self.ctx, vec);
        for (i, (val, _)) in constants.iter().enumerate() {
            wvec[i].unlock().set(*val);
        }

        let mut buf = Vec::with_capacity(1024);
        let writer = FASLWriter::new(self.ctx, &mut buf);
        writer.write(vec.into()).expect("should not fail");

        (buf, constant_indices)
    }

    fn fasl_data_slots(&self, constant_indices: &HashMap<DataSymbol, u32>) -> Vec<FaslDataSlot> {
        let mut slots = Vec::new();

        for symbol in self.constants.values().copied() {
            slots.push(FaslDataSlot {
                symbol,
                kind: DataSymbolKind::Constant,
                constant_index: constant_indices.get(&symbol).copied(),
                code: None,
                pointer_code: None,
                side_metadata: None,
            });
        }
        for symbol in self.cache_cells.values().copied() {
            slots.push(FaslDataSlot {
                symbol,
                kind: DataSymbolKind::CacheCell,
                constant_index: None,
                code: None,
                pointer_code: None,
                side_metadata: None,
            });
        }
        for (func, symbol) in self.code_block_for_func.iter() {
            slots.push(FaslDataSlot {
                symbol: *symbol,
                kind: DataSymbolKind::CodeBlock,
                constant_index: None,
                code: self.func_for_func.get(func).copied(),
                pointer_code: None,
                side_metadata: None,
            });
        }
        for (cont, symbol) in self.code_block_for_cont.iter() {
            slots.push(FaslDataSlot {
                symbol: *symbol,
                kind: DataSymbolKind::CodeBlock,
                constant_index: None,
                code: self.func_for_cont.get(cont).copied(),
                pointer_code: None,
                side_metadata: None,
            });
        }
        for (function, symbol) in self.pointer_slot_for_function.iter() {
            slots.push(FaslDataSlot {
                symbol: *symbol,
                kind: DataSymbolKind::PointerSlot,
                constant_index: None,
                code: None,
                pointer_code: Some(function.index()),
                side_metadata: None,
            });
        }
        slots.push(FaslDataSlot {
            symbol: self.global_side_metadata_base_address,
            kind: DataSymbolKind::SideMetadata,
            constant_index: None,
            code: None,
            pointer_code: None,
            side_metadata: Some(SideMetadataSlotKind::Global),
        });
        slots.push(FaslDataSlot {
            symbol: self.vo_bit_side_metadata_base_address,
            kind: DataSymbolKind::SideMetadata,
            constant_index: None,
            code: None,
            pointer_code: None,
            side_metadata: Some(SideMetadataSlotKind::VoBit),
        });

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
        let data_id = self.declare_data_symbol(DataSymbolKind::Constant, &name);

        self.constants.insert(ValueEqual(obj), data_id);

        Some(data_id)
    }

    pub fn intern_cache_cell(&mut self, key: Value<'gc>) -> DataSymbol {
        if let Some(data_id) = self.cache_cells.get(&ValueEqual(key)) {
            return *data_id;
        }

        let ix = self.cache_cells.len();
        let name = format!("cache_cell{}", ix);

        let data_id = self.declare_data_symbol(DataSymbolKind::CacheCell, &name);

        self.cache_cells.insert(ValueEqual(key), data_id);

        data_id
    }

    fn declare_code_block_slot(&mut self, name: &str) -> DataSymbol {
        self.declare_data_symbol(DataSymbolKind::CodeBlock, name)
    }

    #[cfg(test)]
    fn load_constant(&mut self, builder: &mut FunctionBuilder<'_>, value: Value<'gc>) -> ir::Value {
        if let Some(data_id) = self.intern_constant(value) {
            let gv = self.declare_data_in_func(data_id, builder.func);
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

pub struct SSABuilder<'gc, 'a, 'f> {
    pub module_builder: &'a mut ModuleBuilder<'gc>,
    pub builder: FunctionBuilder<'f>,
    pub(crate) func_debug_cx: FunctionDebugContext<'gc>,

    pub linear_blockmap: HashMap<BlockId, ir::Block>,
    pub variables: HashMap<LVarRef<'gc>, VarDef>,
    pub linear_variables: HashMap<ValueId, VarDef>,
    pub linear_rest_sources: HashMap<ValueId, LinearRestSource>,
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

impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
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

        builder.append_block_param(exit_block, types::I64); /* code */
        builder.append_block_param(exit_block, types::I64); /* rator */
        builder.append_block_param(exit_block, types::I64); /* argc */
        for _ in 0..REGISTER_ARG_COUNT {
            builder.append_block_param(exit_block, types::I64);
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
            linear_variables: HashMap::new(),
            linear_rest_sources: HashMap::new(),
            synthetic_aliases: HashMap::new(),
            linear_blockmap: HashMap::new(),
            thunks,

            sig_call,

            data_imports: HashMap::new(),
            srcloc: None,
        };

        this.entrypoint(entry_argc, entry_args);

        this
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::codegen::{BackendSymbol, DataSymbolKind, ImportedSymbol, ImportedSymbolKind},
        cps::{
            linear::linearize,
            reify,
            term::{Atom, Func, Term},
        },
        expander::core::{LVarRef, fresh_lvar},
        rsgc::{Gc, alloc::Array, cell::Lock},
        runtime::{
            Context, Scheme,
            value::{Str, Symbol, Value},
        },
    };
    use cranelift_codegen::ir::{ExternalName, Function, UserFuncName};

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

    fn one_arg_identity_func<'gc>(
        ctx: Context<'gc>,
    ) -> (Gc<'gc, Func<'gc>>, LVarRef<'gc>, LVarRef<'gc>, LVarRef<'gc>) {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let arg = lvar(ctx, "arg");
        let body = Gc::new(
            *ctx,
            Term::Continue(
                retk,
                Array::from_slice(*ctx, [Atom::Local(arg)]),
                Value::new(false),
            ),
        );
        let func = Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "identity").into(),
                source: Value::new(false),
                binding: f,
                return_cont: retk,
                args: Array::from_slice(*ctx, [arg]),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        );

        (func, f, retk, arg)
    }

    fn local_call_func<'gc>(ctx: Context<'gc>) -> (Gc<'gc, Func<'gc>>, Gc<'gc, Func<'gc>>) {
        let f = lvar(ctx, "f");
        let retk = lvar(ctx, "retk");
        let g = lvar(ctx, "g");
        let g_retk = lvar(ctx, "g-retk");
        let g_body = Gc::new(
            *ctx,
            Term::Continue(
                g_retk,
                Array::from_slice(*ctx, [Atom::Constant(Value::new(42))]),
                Value::new(false),
            ),
        );
        let g_func = Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "callee").into(),
                source: Value::new(false),
                binding: g,
                return_cont: g_retk,
                args: Array::from_slice(*ctx, []),
                variadic: None,
                body: Lock::new(g_body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        );
        let body = Gc::new(
            *ctx,
            Term::Fix(
                Array::from_slice(*ctx, [g_func]),
                Gc::new(
                    *ctx,
                    Term::App(
                        Atom::Local(g),
                        retk,
                        Array::from_slice(*ctx, []),
                        Value::new(false),
                    ),
                ),
            ),
        );
        let entry = Gc::new(
            *ctx,
            Func {
                name: Symbol::from_str(ctx, "entry").into(),
                source: Value::new(false),
                binding: f,
                return_cont: retk,
                args: Array::from_slice(*ctx, []),
                variadic: None,
                body: Lock::new(body),
                free_vars: Lock::new(None),
                meta: Value::new(false),
            },
        );

        (entry, g_func)
    }

    fn empty_linear_program<'gc>(entry: FuncRef<'gc>) -> LinearProgram<'gc> {
        LinearProgram {
            entry,
            procedures: Vec::new(),
        }
    }

    fn direct_test_function() -> Function {
        Function::with_name_signature(UserFuncName::user(0, 0), compiled_scheme_signature())
    }

    #[test]
    fn fasl_relocation_conversion_preserves_non_x86_cranelift_relocations() {
        use cranelift_codegen::binemit::Reloc;

        let target = BackendSymbol::Function(FunctionSymbol::new(7));
        for kind in [
            Reloc::Arm64Call,
            Reloc::Aarch64AdrPrelPgHi21,
            Reloc::Aarch64AddAbsLo12Nc,
            Reloc::RiscvCallPlt,
            Reloc::RiscvGotHi20,
            Reloc::RiscvPCRelHi20,
            Reloc::RiscvPCRelLo12I,
        ] {
            let relocation = DirectRelocation {
                offset: 4,
                kind,
                addend: -4,
                target: DirectRelocationTarget::BackendSymbol(target),
            };

            assert_eq!(
                fasl_relocation_from_direct_relocation(&relocation, &HashMap::new())
                    .expect("convert non-x86 relocation"),
                FaslRelocation {
                    offset: 4,
                    kind: FaslRelocKind::Cranelift(kind),
                    target: FaslRelocTarget::Entry(7),
                    addend: -4,
                }
            );
        }
    }

    #[test]
    fn fasl_relocation_conversion_preserves_non_x86_data_slot_relocations() {
        use cranelift_codegen::binemit::Reloc;

        let mut data_slot_targets = HashMap::new();
        data_slot_targets.insert(3, FaslRelocTarget::Entry(11));

        let relocation = DirectRelocation {
            offset: 4,
            kind: Reloc::Aarch64AdrPrelPgHi21,
            addend: 0,
            target: DirectRelocationTarget::BackendSymbol(BackendSymbol::Data {
                kind: DataSymbolKind::PointerSlot,
                symbol: DataSymbol::new(3),
            }),
        };

        assert_eq!(
            fasl_relocation_from_direct_relocation(&relocation, &data_slot_targets)
                .expect("convert data-slot relocation"),
            FaslRelocation {
                offset: 4,
                kind: FaslRelocKind::CraneliftDataSlot(Reloc::Aarch64AdrPrelPgHi21),
                target: FaslRelocTarget::Entry(11),
                addend: 0,
            }
        );
    }

    #[test]
    fn fasl_relocation_conversion_marks_cache_cell_targets() {
        use cranelift_codegen::binemit::Reloc;

        let mut data_slot_targets = HashMap::new();
        data_slot_targets.insert(3, FaslRelocTarget::Object(11));

        let relocation = DirectRelocation {
            offset: 4,
            kind: Reloc::X86PCRel4,
            addend: 0,
            target: DirectRelocationTarget::BackendSymbol(BackendSymbol::Data {
                kind: DataSymbolKind::CacheCell,
                symbol: DataSymbol::new(3),
            }),
        };

        assert_eq!(
            fasl_relocation_from_direct_relocation(&relocation, &data_slot_targets)
                .expect("convert cache-cell relocation"),
            FaslRelocation {
                offset: 4,
                kind: FaslRelocKind::CraneliftDataSlot(Reloc::X86PCRel4),
                target: FaslRelocTarget::CacheCell(11),
                addend: 0,
            }
        );
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
                    Array::from_slice(*ctx, []),
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
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:loop:f");

            let ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );

            assert_eq!(ssa.rator, ssa.builder.block_params(ssa.entry_block)[0]);
        });
    }

    #[test]
    fn compiled_scheme_signature_uses_arg_register_abi() {
        let sig = compiled_scheme_signature();

        assert_eq!(REGISTER_ARG_COUNT, 4);
        assert_eq!(sig.params.len(), REGISTER_ARG_COUNT + 2);
        assert_eq!(sig.params.len(), 6);
        assert_eq!(sig.returns.len(), 2);
        assert!(
            sig.params
                .iter()
                .all(|param| param.value_type == types::I64)
        );
        assert!(sig.returns.iter().all(|ret| ret.value_type == types::I64));
    }

    #[test]
    fn fixed_arity_register_entry_skips_generic_overflow_adjustment() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:identity:f");

            let mut ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );
            ssa.finalize();
            let clif = ssa.builder.func.display().to_string();

            assert!(
                !clif.contains("imul_imm") && !clif.contains("iadd_imm v1, -4"),
                "fixed arity <= register arg count should not emit generic overflow-base adjustment:\n{clif}"
            );
        });
    }

    #[test]
    fn register_only_call_args_do_not_touch_runstack() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:identity:f");

            let mut ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );
            let arg = ssa.builder.ins().iconst(types::I64, 42);
            ssa.prepare_call_args(&[arg]);
            let clif = ssa.builder.func.display().to_string();

            assert!(
                !clif.contains("+40"),
                "register-only call arg preparation should not touch State.runstack:\n{clif}"
            );
        });
    }

    #[test]
    fn full_register_call_args_do_not_touch_runstack() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:identity:f");

            let mut ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );
            let args = [
                ssa.builder.ins().iconst(types::I64, 1),
                ssa.builder.ins().iconst(types::I64, 2),
                ssa.builder.ins().iconst(types::I64, 3),
                ssa.builder.ins().iconst(types::I64, 4),
            ];
            ssa.prepare_call_args(&args);
            let clif = ssa.builder.func.display().to_string();

            assert!(
                !clif.contains("+40"),
                "4-arg register call preparation should not touch State.runstack:\n{clif}"
            );
        });
    }

    #[test]
    fn register_only_call_args_pad_with_zero() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:identity:f");

            let mut ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );
            let arg = ssa.builder.ins().iconst(types::I64, 42);
            let call_args = ssa.prepare_call_args(&[arg]);
            let clif = ssa.builder.func.display().to_string();

            assert_eq!(call_args.args[0], arg);
            assert_eq!(call_args.args[1], call_args.args[2]);
            assert_eq!(call_args.args[2], call_args.args[3]);
            assert!(
                clif.contains("iconst.i64 0"),
                "unused register arguments should be padded with zero:\n{clif}"
            );
        });
    }

    #[test]
    fn overflow_call_args_still_use_runstack() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let procedure = linear
                .procedures
                .iter()
                .find(|procedure| procedure.code == CodeId::Function(func))
                .expect("entry function should have a linear procedure")
                .clone();
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);
            let mut context = cranelift_codegen::Context::new();
            context.func.signature = compiled_scheme_signature();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut fctx);
            let thunks = module_builder.import_thunks(builder.func);
            let func_debug_cx = module_builder
                .debug_context
                .define_function(func, "fn0:identity:f");

            let mut ssa = SSABuilder::new(
                &mut module_builder,
                builder,
                procedure,
                thunks,
                func_debug_cx,
            );
            let args = [
                ssa.builder.ins().iconst(types::I64, 1),
                ssa.builder.ins().iconst(types::I64, 2),
                ssa.builder.ins().iconst(types::I64, 3),
                ssa.builder.ins().iconst(types::I64, 4),
                ssa.builder.ins().iconst(types::I64, 5),
            ];
            ssa.prepare_call_args(&args);
            let clif = ssa.builder.func.display().to_string();

            assert!(
                clif.contains("+40") && clif.contains("store"),
                "overflow call arg preparation should still store to runstack:\n{clif}"
            );
        });
    }

    #[test]
    fn module_builder_declares_shared_slowpath_trampolines() {
        with_ctx(|ctx| {
            let f = lvar(ctx, "f");
            let retk = lvar(ctx, "retk");
            let body = Gc::new(
                *ctx,
                Term::App(
                    Atom::Local(f),
                    retk,
                    Array::from_slice(*ctx, []),
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
                    args: Array::from_slice(*ctx, []),
                    variadic: None,
                    body: Lock::new(body),
                    free_vars: Lock::new(None),
                    meta: Value::new(false),
                },
            );

            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            assert!(
                !module_builder
                    .raise_trampolines
                    .contains(&module_builder.raise_to_exception_handler_trampoline)
            );
            assert!(
                !module_builder
                    .raise_trampolines
                    .contains(&module_builder.wrong_arity_trampoline)
            );
            assert_ne!(
                module_builder.wrong_arity_trampoline,
                module_builder.raise_to_exception_handler_trampoline
            );
            module_builder
                .compile_loaded_fasl_bytes()
                .expect("compile unified FASL");
        });
    }

    #[test]
    fn direct_import_backend_uses_numeric_function_and_data_symbols() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = empty_linear_program(func);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            let mut function = direct_test_function();
            let imported_function = module_builder
                .declare_function_in_func(module_builder.wrong_arity_trampoline, &mut function);
            let function_name_ref = match &function.dfg.ext_funcs[imported_function].name {
                ExternalName::User(name_ref) => *name_ref,
                other => panic!("expected user function name, got {other:?}"),
            };
            assert_eq!(
                BackendSymbol::from_user_external_name(
                    function.params.user_named_funcs()[function_name_ref].clone()
                ),
                Some(BackendSymbol::Function(
                    module_builder.wrong_arity_trampoline
                ))
            );

            let global = module_builder.declare_data_in_func(
                module_builder.global_side_metadata_base_address,
                &mut function,
            );
            let global_name_ref = match &function.global_values[global] {
                ir::GlobalValueData::Symbol { name, .. } => match name {
                    ExternalName::User(name_ref) => *name_ref,
                    other => panic!("expected user data symbol name, got {other:?}"),
                },
                other => panic!("expected symbol global value, got {other:?}"),
            };
            assert_eq!(
                BackendSymbol::from_user_external_name(
                    function.params.user_named_funcs()[global_name_ref].clone()
                ),
                Some(BackendSymbol::Data {
                    kind: DataSymbolKind::SideMetadata,
                    symbol: module_builder.global_side_metadata_base_address,
                })
            );
        });
    }

    #[test]
    fn direct_import_backend_uses_numeric_runtime_data_and_thunks() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = empty_linear_program(func);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            let mut function = direct_test_function();
            let global =
                module_builder.declare_runtime_data_in_func(RuntimeData::PairInfo, &mut function);
            let global_name_ref = match &function.global_values[global] {
                ir::GlobalValueData::Symbol { name, .. } => match name {
                    ExternalName::User(name_ref) => *name_ref,
                    other => panic!("expected user runtime data symbol name, got {other:?}"),
                },
                other => panic!("expected symbol global value, got {other:?}"),
            };
            assert_eq!(
                BackendSymbol::from_user_external_name(
                    function.params.user_named_funcs()[global_name_ref].clone()
                ),
                Some(crate::compiler::codegen::runtime_data_symbol(
                    RuntimeData::PairInfo
                ))
            );

            let thunks = module_builder.import_thunks(&mut function);
            let thunk_name_ref = match &function.dfg.ext_funcs[thunks.wrong_number_of_args].name {
                ExternalName::User(name_ref) => *name_ref,
                other => panic!("expected user thunk symbol name, got {other:?}"),
            };
            assert_eq!(
                BackendSymbol::from_user_external_name(
                    function.params.user_named_funcs()[thunk_name_ref].clone()
                ),
                Some(BackendSymbol::Imported {
                    kind: ImportedSymbolKind::RuntimeThunk,
                    symbol: ImportedSymbol::new(RuntimeThunk::Thunk_wrong_number_of_args.id()),
                })
            );
        });
    }

    #[test]
    fn direct_constant_load_uses_numeric_data_symbol() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = empty_linear_program(func);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            let mut function = direct_test_function();
            let mut fctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut function, &mut fctx);
            let entry = builder.create_block();
            builder.switch_to_block(entry);
            module_builder.load_constant(&mut builder, Str::new(*ctx, "constant", true).into());

            let symbol = *module_builder
                .constants
                .values()
                .next()
                .expect("constant should be interned");
            let global = builder
                .func
                .global_values
                .keys()
                .find(|gv| {
                    matches!(
                        builder.func.global_values[*gv],
                        ir::GlobalValueData::Symbol { .. }
                    )
                })
                .expect("constant load should create a symbol global value");
            let name_ref = match &builder.func.global_values[global] {
                ir::GlobalValueData::Symbol { name, .. } => match name {
                    ExternalName::User(name_ref) => *name_ref,
                    other => panic!("expected user constant symbol name, got {other:?}"),
                },
                other => panic!("expected symbol global value, got {other:?}"),
            };

            assert_eq!(
                BackendSymbol::from_user_external_name(
                    builder.func.params.user_named_funcs()[name_ref].clone()
                ),
                Some(BackendSymbol::Data {
                    kind: DataSymbolKind::Constant,
                    symbol,
                })
            );
        });
    }

    #[test]
    fn module_builder_compiles_loadable_unified_fasl_for_simple_function() {
        with_ctx(|ctx| {
            let (func, _, _, _) = one_arg_identity_func(ctx);
            let reify_info = reify(ctx, func);
            let linear = linearize(&reify_info);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            let bytes = module_builder
                .compile_loaded_fasl_bytes()
                .expect("compile unified FASL");
            let value = crate::runtime::fasl::FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load unified FASL");

            assert!(value.is::<Closure>());
        });
    }

    #[test]
    fn module_builder_compiles_loadable_unified_fasl_for_local_direct_call() {
        with_ctx(|ctx| {
            let (entry, _) = local_call_func(ctx);
            let reify_info = reify(ctx, entry);
            let linear = linearize(&reify_info);
            let mut module_builder = ModuleBuilder::new(ctx, reify_info, linear);

            let bytes = module_builder
                .compile_loaded_fasl_bytes()
                .expect("compile unified FASL with pointer and side-metadata data slots");
            let value = crate::runtime::fasl::FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load unified FASL with pointer and side-metadata data slots");

            assert!(value.is::<Closure>());
        });
    }
}

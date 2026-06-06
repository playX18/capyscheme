use std::sync::Arc;

use cranelift::prelude::Configurable;
use cranelift_codegen::{
    Context as ClifContext,
    control::ControlPlane,
    ir,
    isa::TargetIsa,
    settings::{self, Flags},
};

use crate::{
    compiler::symbols::{DataKind, DataSymbol, Symbol},
    runtime::symbols::RuntimeData,
};

pub fn declare_function(
    function: &mut ir::Function,
    symbol: Symbol,
    signature: ir::SigRef,
    colocated: bool,
) -> ir::FuncRef {
    let name_ref = function.declare_imported_user_function(symbol.to_external_name());
    function.import_function(ir::ExtFuncData {
        name: ir::ExternalName::user(name_ref),
        signature,
        colocated,
    })
}

pub fn declare_data(
    function: &mut ir::Function,
    symbol: Symbol,
    colocated: bool,
    tls: bool,
) -> ir::GlobalValue {
    let name_ref = function.declare_imported_user_function(symbol.to_external_name());
    function.create_global_value(ir::GlobalValueData::Symbol {
        name: ir::ExternalName::user(name_ref),
        offset: ir::immediates::Imm64::new(0),
        colocated,
        tls,
    })
}

pub fn runtime_data(data: RuntimeData) -> Symbol {
    Symbol::data(DataKind::RuntimeData, DataSymbol::new(data.id()))
}

pub fn declare_runtime_data(function: &mut ir::Function, data: RuntimeData) -> ir::GlobalValue {
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

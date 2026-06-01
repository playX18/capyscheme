use std::sync::Arc;

use cranelift::prelude::Configurable;
use cranelift_codegen::{
    Context,
    control::ControlPlane,
    ir::{self, UserExternalName},
    isa::TargetIsa,
    settings::{self, Flags},
};

const FUNCTION_SYMBOL_NAMESPACE: u32 = 0;
const DATA_SYMBOL_NAMESPACE_BASE: u32 = 1;
const IMPORTED_SYMBOL_NAMESPACE_BASE: u32 = 100;

macro_rules! symbol_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(u32);

        impl $name {
            pub const fn new(index: u32) -> Self {
                Self(index)
            }

            pub const fn index(self) -> u32 {
                self.0
            }
        }
    };
}

symbol_id!(FunctionSymbol);
symbol_id!(DataSymbol);
symbol_id!(ImportedSymbol);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum DataSymbolKind {
    Constant = 0,
    CacheCell = 1,
    CodeBlock = 2,
    RuntimeData = 3,
    SideMetadata = 4,
    RootTable = 5,
    FaslBlob = 6,
    PointerSlot = 7,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ImportedSymbolKind {
    RuntimeThunk = 0,
    Trampoline = 1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BackendSymbol {
    Function(FunctionSymbol),
    Data {
        kind: DataSymbolKind,
        symbol: DataSymbol,
    },
    Imported {
        kind: ImportedSymbolKind,
        symbol: ImportedSymbol,
    },
}

impl BackendSymbol {
    pub fn to_user_external_name(self) -> UserExternalName {
        match self {
            Self::Function(symbol) => {
                UserExternalName::new(FUNCTION_SYMBOL_NAMESPACE, symbol.index())
            }
            Self::Data { kind, symbol } => UserExternalName::new(kind.namespace(), symbol.index()),
            Self::Imported { kind, symbol } => {
                UserExternalName::new(kind.namespace(), symbol.index())
            }
        }
    }

    pub const fn from_user_external_name(name: UserExternalName) -> Option<Self> {
        match name.namespace {
            FUNCTION_SYMBOL_NAMESPACE => Some(Self::Function(FunctionSymbol::new(name.index))),
            _ => match DataSymbolKind::from_namespace(name.namespace) {
                Some(kind) => Some(Self::Data {
                    kind,
                    symbol: DataSymbol::new(name.index),
                }),
                None => match ImportedSymbolKind::from_namespace(name.namespace) {
                    Some(kind) => Some(Self::Imported {
                        kind,
                        symbol: ImportedSymbol::new(name.index),
                    }),
                    None => None,
                },
            },
        }
    }
}

impl DataSymbolKind {
    pub const fn namespace(self) -> u32 {
        DATA_SYMBOL_NAMESPACE_BASE + self as u32
    }

    pub const fn from_namespace(namespace: u32) -> Option<Self> {
        match namespace {
            1 => Some(Self::Constant),
            2 => Some(Self::CacheCell),
            3 => Some(Self::CodeBlock),
            4 => Some(Self::RuntimeData),
            5 => Some(Self::SideMetadata),
            6 => Some(Self::RootTable),
            7 => Some(Self::FaslBlob),
            8 => Some(Self::PointerSlot),
            _ => None,
        }
    }
}

impl ImportedSymbolKind {
    pub const fn namespace(self) -> u32 {
        IMPORTED_SYMBOL_NAMESPACE_BASE + self as u32
    }

    pub const fn from_namespace(namespace: u32) -> Option<Self> {
        match namespace {
            100 => Some(Self::RuntimeThunk),
            101 => Some(Self::Trampoline),
            _ => None,
        }
    }
}

pub fn declare_backend_function_import(
    function: &mut ir::Function,
    symbol: BackendSymbol,
    signature: ir::SigRef,
    colocated: bool,
) -> ir::FuncRef {
    let name_ref = function.declare_imported_user_function(symbol.to_user_external_name());
    function.import_function(ir::ExtFuncData {
        name: ir::ExternalName::user(name_ref),
        signature,
        colocated,
    })
}

pub fn declare_backend_data_symbol(
    function: &mut ir::Function,
    symbol: BackendSymbol,
    colocated: bool,
    tls: bool,
) -> ir::GlobalValue {
    let name_ref = function.declare_imported_user_function(symbol.to_user_external_name());
    function.create_global_value(ir::GlobalValueData::Symbol {
        name: ir::ExternalName::user(name_ref),
        offset: ir::immediates::Imm64::new(0),
        colocated,
        tls,
    })
}

pub fn runtime_data_symbol(data: crate::runtime::symbols::RuntimeData) -> BackendSymbol {
    BackendSymbol::Data {
        kind: DataSymbolKind::RuntimeData,
        symbol: DataSymbol::new(data.id()),
    }
}

pub fn declare_runtime_data_symbol(
    function: &mut ir::Function,
    data: crate::runtime::symbols::RuntimeData,
) -> ir::GlobalValue {
    declare_backend_data_symbol(function, runtime_data_symbol(data), false, false)
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

pub struct FunctionCompileContext {
    pub context: Context,
    pub fctx: cranelift::prelude::FunctionBuilderContext,
    pub ctrl_plane: ControlPlane,
}

impl FunctionCompileContext {
    pub fn new() -> Self {
        Self {
            context: Context::new(),
            fctx: cranelift::prelude::FunctionBuilderContext::new(),
            ctrl_plane: ControlPlane::default(),
        }
    }

    pub fn clear(&mut self) {
        self.context.clear();
        self.fctx = cranelift::prelude::FunctionBuilderContext::new();
        self.ctrl_plane = ControlPlane::default();
    }
}

impl Default for FunctionCompileContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backend_symbols_roundtrip_through_numeric_cranelift_names() {
        let function = BackendSymbol::Function(FunctionSymbol::new(7));
        let data = BackendSymbol::Data {
            kind: DataSymbolKind::CodeBlock,
            symbol: DataSymbol::new(11),
        };
        let imported = BackendSymbol::Imported {
            kind: ImportedSymbolKind::RuntimeThunk,
            symbol: ImportedSymbol::new(13),
        };

        assert_eq!(
            BackendSymbol::from_user_external_name(function.to_user_external_name()),
            Some(function)
        );
        assert_eq!(
            BackendSymbol::from_user_external_name(data.to_user_external_name()),
            Some(data)
        );
        assert_eq!(
            BackendSymbol::from_user_external_name(imported.to_user_external_name()),
            Some(imported)
        );
    }

    #[test]
    fn unknown_backend_symbol_namespace_is_rejected() {
        assert_eq!(
            BackendSymbol::from_user_external_name(UserExternalName::new(99, 1)),
            None
        );
    }

    #[test]
    fn data_symbol_kinds_use_distinct_numeric_namespaces() {
        let kinds = [
            DataSymbolKind::Constant,
            DataSymbolKind::CacheCell,
            DataSymbolKind::CodeBlock,
            DataSymbolKind::RuntimeData,
            DataSymbolKind::SideMetadata,
            DataSymbolKind::RootTable,
            DataSymbolKind::FaslBlob,
            DataSymbolKind::PointerSlot,
        ];

        for kind in kinds {
            assert_eq!(DataSymbolKind::from_namespace(kind.namespace()), Some(kind));
        }
    }

    #[test]
    fn imported_symbol_kinds_use_distinct_numeric_namespaces() {
        let kinds = [
            ImportedSymbolKind::RuntimeThunk,
            ImportedSymbolKind::Trampoline,
        ];

        for kind in kinds {
            assert_eq!(
                ImportedSymbolKind::from_namespace(kind.namespace()),
                Some(kind)
            );
            assert!(DataSymbolKind::from_namespace(kind.namespace()).is_none());
        }
    }

    #[test]
    fn declare_backend_function_import_uses_numeric_symbol_name() {
        let mut function = cranelift_codegen::ir::Function::new();
        let signature = function.import_signature(cranelift_codegen::ir::Signature::new(
            cranelift_codegen::isa::CallConv::SystemV,
        ));
        let symbol = BackendSymbol::Imported {
            kind: ImportedSymbolKind::RuntimeThunk,
            symbol: ImportedSymbol::new(42),
        };

        let func_ref = declare_backend_function_import(&mut function, symbol, signature, false);
        let import = &function.dfg.ext_funcs[func_ref];
        let name_ref = cranelift_codegen::ir::UserExternalNameRef::from_u32(0);

        assert_eq!(
            import.name,
            cranelift_codegen::ir::ExternalName::user(name_ref)
        );
        assert_eq!(
            BackendSymbol::from_user_external_name(
                function.params.user_named_funcs()[name_ref].clone()
            ),
            Some(symbol)
        );
        assert!(!import.colocated);
    }

    #[test]
    fn declare_backend_data_symbol_uses_numeric_symbol_name() {
        let mut function = cranelift_codegen::ir::Function::new();
        let symbol = BackendSymbol::Data {
            kind: DataSymbolKind::PointerSlot,
            symbol: DataSymbol::new(9),
        };

        let global = declare_backend_data_symbol(&mut function, symbol, false, false);
        let data = &function.global_values[global];
        let name_ref = cranelift_codegen::ir::UserExternalNameRef::from_u32(0);

        assert_eq!(
            data.symbol_name(),
            &cranelift_codegen::ir::ExternalName::user(name_ref)
        );
        assert_eq!(
            BackendSymbol::from_user_external_name(
                function.params.user_named_funcs()[name_ref].clone()
            ),
            Some(symbol)
        );
    }

    #[test]
    fn runtime_data_symbol_uses_runtime_data_id() {
        let mut function = cranelift_codegen::ir::Function::new();

        let global = declare_runtime_data_symbol(
            &mut function,
            crate::runtime::symbols::RuntimeData::PairInfo,
        );
        let data = &function.global_values[global];
        let cranelift_codegen::ir::ExternalName::User(name_ref) = data.symbol_name() else {
            panic!("runtime data symbol should use a user external name");
        };

        assert_eq!(
            BackendSymbol::from_user_external_name(
                function.params.user_named_funcs()[*name_ref].clone()
            ),
            Some(BackendSymbol::Data {
                kind: DataSymbolKind::RuntimeData,
                symbol: DataSymbol::new(crate::runtime::symbols::RuntimeData::PairInfo.id()),
            })
        );
    }
}

//! Compatibility facade for Cranelift code generation helpers.

pub use crate::compiler::{
    cranelift::{
        CompileContext, declare_data, declare_function, declare_runtime_data, host_isa,
        runtime_data,
    },
    symbols::{DataKind, DataSymbol, FunctionSymbol, ImportKind, ImportedSymbol, Symbol, SymbolId},
};

#[cfg(test)]
mod tests {
    use cranelift_codegen::ir::UserExternalName;

    use super::*;

    #[test]
    fn symbols_roundtrip_through_numeric_cranelift_names() {
        let function = Symbol::function(FunctionSymbol::new(7));
        let data = Symbol::data(DataKind::CodeBlock, DataSymbol::new(11));
        let imported = Symbol::imported(ImportKind::RuntimeThunk, ImportedSymbol::new(13));

        assert_eq!(
            Symbol::from_external_name(function.to_external_name()),
            Some(function)
        );
        assert_eq!(
            Symbol::from_external_name(data.to_external_name()),
            Some(data)
        );
        assert_eq!(
            Symbol::from_external_name(imported.to_external_name()),
            Some(imported)
        );
    }

    #[test]
    fn unknown_symbol_namespace_is_rejected() {
        assert_eq!(
            Symbol::from_external_name(UserExternalName::new(99, 1)),
            None
        );
    }

    #[test]
    fn data_symbol_kinds_use_distinct_numeric_namespaces() {
        let kinds = [
            DataKind::Constant,
            DataKind::CacheCell,
            DataKind::CodeBlock,
            DataKind::RuntimeData,
            DataKind::SideMetadata,
            DataKind::RootTable,
            DataKind::FaslBlob,
            DataKind::PointerSlot,
        ];

        for kind in kinds {
            assert_eq!(DataKind::from_namespace(kind.namespace()), Some(kind));
        }
    }

    #[test]
    fn imported_symbol_kinds_use_distinct_numeric_namespaces() {
        let kinds = [ImportKind::RuntimeThunk, ImportKind::Trampoline];

        for kind in kinds {
            assert_eq!(ImportKind::from_namespace(kind.namespace()), Some(kind));
            assert!(DataKind::from_namespace(kind.namespace()).is_none());
        }
    }

    #[test]
    fn declare_function_uses_numeric_symbol_name() {
        let mut function = cranelift_codegen::ir::Function::new();
        let signature = function.import_signature(cranelift_codegen::ir::Signature::new(
            cranelift_codegen::isa::CallConv::SystemV,
        ));
        let symbol = Symbol::imported(ImportKind::RuntimeThunk, ImportedSymbol::new(42));

        let func_ref = declare_function(&mut function, symbol, signature, false);
        let import = &function.dfg.ext_funcs[func_ref];
        let name_ref = cranelift_codegen::ir::UserExternalNameRef::from_u32(0);

        assert_eq!(
            import.name,
            cranelift_codegen::ir::ExternalName::user(name_ref)
        );
        assert_eq!(
            Symbol::from_external_name(function.params.user_named_funcs()[name_ref].clone()),
            Some(symbol)
        );
        assert!(!import.colocated);
    }

    #[test]
    fn declare_data_uses_numeric_symbol_name() {
        let mut function = cranelift_codegen::ir::Function::new();
        let symbol = Symbol::data(DataKind::PointerSlot, DataSymbol::new(9));

        let global = declare_data(&mut function, symbol, false, false);
        let data = &function.global_values[global];
        let name_ref = cranelift_codegen::ir::UserExternalNameRef::from_u32(0);

        let cranelift_codegen::ir::GlobalValueData::Symbol { name, .. } = data else {
            panic!("expected symbol global value");
        };
        assert_eq!(name, &cranelift_codegen::ir::ExternalName::user(name_ref));
        assert_eq!(
            Symbol::from_external_name(function.params.user_named_funcs()[name_ref].clone()),
            Some(symbol)
        );
    }

    #[test]
    fn runtime_data_uses_runtime_data_id() {
        let mut function = cranelift_codegen::ir::Function::new();

        let global = declare_runtime_data(
            &mut function,
            crate::runtime::symbols::RuntimeData::PairHeaderWord,
        );
        let cranelift_codegen::ir::GlobalValueData::Symbol { name, .. } =
            &function.global_values[global]
        else {
            panic!("expected symbol global value");
        };
        let cranelift_codegen::ir::ExternalName::User(name_ref) = name else {
            panic!("expected user external name");
        };

        assert_eq!(
            Symbol::from_external_name(function.params.user_named_funcs()[*name_ref].clone()),
            Some(Symbol::data(
                DataKind::RuntimeData,
                DataSymbol::new(crate::runtime::symbols::RuntimeData::PairHeaderWord.id()),
            ))
        );
    }
}

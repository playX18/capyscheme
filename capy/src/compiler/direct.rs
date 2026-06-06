use cranelift_codegen::{FinalizedRelocTarget, binemit::Reloc, entity::PrimaryMap, ir};

use crate::compiler::codegen::{BackendSymbol, FunctionCompileContext};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectCompiledFunction {
    pub bytes: Vec<u8>,
    pub relocs: Vec<DirectRelocation>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectRelocation {
    pub offset: u32,
    pub kind: Reloc,
    pub addend: i64,
    pub target: DirectRelocationTarget,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DirectRelocationTarget {
    BackendSymbol(BackendSymbol),
    FunctionOffset(u32),
}

pub fn compile_function(
    isa: &dyn cranelift_codegen::isa::TargetIsa,
    cache: &mut FunctionCompileContext,
) -> Result<DirectCompiledFunction, String> {
    let user_names = cache.context.func.params.user_named_funcs().clone();
    let compiled = cache
        .context
        .compile(isa, &mut cache.ctrl_plane)
        .map_err(|err| format!("{err:?}"))?;
    let bytes = compiled.code_buffer().to_vec();
    let relocs = compiled
        .buffer
        .relocs()
        .iter()
        .map(|reloc| {
            let target = direct_relocation_target(&user_names, &reloc.target)?;
            Ok(DirectRelocation {
                offset: reloc.offset,
                kind: reloc.kind,
                addend: reloc.addend,
                target,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(DirectCompiledFunction { bytes, relocs })
}

fn direct_relocation_target(
    user_names: &PrimaryMap<ir::UserExternalNameRef, ir::UserExternalName>,
    target: &FinalizedRelocTarget,
) -> Result<DirectRelocationTarget, String> {
    match target {
        FinalizedRelocTarget::Func(offset) => Ok(DirectRelocationTarget::FunctionOffset(*offset)),
        FinalizedRelocTarget::ExternalName(ir::ExternalName::User(name_ref)) => {
            let name = user_names[*name_ref].clone();
            BackendSymbol::from_user_external_name(name)
                .map(DirectRelocationTarget::BackendSymbol)
                .ok_or_else(|| "unknown backend symbol namespace in relocation".to_string())
        }
        FinalizedRelocTarget::ExternalName(name) => {
            Err(format!("unsupported external relocation target: {name:?}"))
        }
    }
}

#[cfg(test)]
fn trivial_i64_signature() -> ir::Signature {
    let mut sig = ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
    sig.returns.push(ir::AbiParam::new(ir::types::I64));
    sig
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::codegen::host_isa;
    use cranelift::prelude::{FunctionBuilder, InstBuilder};
    use cranelift_codegen::ir::{Function, types};

    #[test]
    fn direct_compile_trivial_function_produces_code_bytes() {
        let isa = host_isa();
        let mut cache = FunctionCompileContext::new();
        cache.context.func = Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, 0),
            trivial_i64_signature(),
        );

        {
            let mut builder = FunctionBuilder::new(&mut cache.context.func, &mut cache.fctx);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            let value = builder.ins().iconst(types::I64, 42);
            builder.ins().return_(&[value]);
            builder.seal_all_blocks();
            builder.finalize();
        }

        let compiled = compile_function(&*isa, &mut cache).expect("compile trivial function");

        assert!(!compiled.bytes.is_empty());
        assert!(compiled.relocs.is_empty());
    }

    #[test]
    fn direct_compile_preserves_numeric_external_relocation_target() {
        use crate::compiler::codegen::{
            BackendSymbol, ImportedSymbol, ImportedSymbolKind, declare_backend_function_import,
        };

        let isa = host_isa();
        let mut cache = FunctionCompileContext::new();
        cache.context.func = Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, 1),
            trivial_i64_signature(),
        );

        let target = BackendSymbol::Imported {
            kind: ImportedSymbolKind::RuntimeThunk,
            symbol: ImportedSymbol::new(3),
        };

        {
            let imported_sig = cache.context.func.import_signature(trivial_i64_signature());
            let imported = declare_backend_function_import(
                &mut cache.context.func,
                target,
                imported_sig,
                false,
            );
            let mut builder = FunctionBuilder::new(&mut cache.context.func, &mut cache.fctx);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            let call = builder.ins().call(imported, &[]);
            let result = builder.inst_results(call)[0];
            builder.ins().return_(&[result]);
            builder.seal_all_blocks();
            builder.finalize();
        }

        let compiled = compile_function(&*isa, &mut cache).expect("compile imported call");

        assert!(
            compiled
                .relocs
                .iter()
                .any(|reloc| { reloc.target == DirectRelocationTarget::BackendSymbol(target) })
        );
    }

    #[test]
    fn direct_relocations_preserve_runtime_thunk_ids_without_names() {
        use crate::{
            compiler::codegen::{BackendSymbol, ImportedSymbol, ImportedSymbolKind},
            runtime::vm::thunks::RuntimeThunk,
        };

        let thunk_id = RuntimeThunk::Thunk_wrong_number_of_args.id();
        let relocs = [DirectRelocation {
            offset: 17,
            kind: Reloc::X86CallPCRel4,
            addend: -4,
            target: DirectRelocationTarget::BackendSymbol(BackendSymbol::Imported {
                kind: ImportedSymbolKind::RuntimeThunk,
                symbol: ImportedSymbol::new(thunk_id),
            }),
        }];

        assert_eq!(
            relocs[0].target,
            DirectRelocationTarget::BackendSymbol(BackendSymbol::Imported {
                kind: ImportedSymbolKind::RuntimeThunk,
                symbol: ImportedSymbol::new(thunk_id),
            })
        );
    }
}

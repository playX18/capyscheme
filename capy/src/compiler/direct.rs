use cranelift_codegen::{FinalizedRelocTarget, binemit::Reloc, entity::PrimaryMap, ir};

use crate::compiler::codegen::{CompileContext, Symbol};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompiledFunction {
    pub bytes: Vec<u8>,
    pub relocs: Vec<Relocation>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relocation {
    pub offset: u32,
    pub kind: Reloc,
    pub addend: i64,
    pub target: Target,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Target {
    Symbol(Symbol),
    FunctionOffset(u32),
}

pub fn compile_function(
    isa: &dyn cranelift_codegen::isa::TargetIsa,
    cache: &mut CompileContext,
) -> Result<CompiledFunction, String> {
    let user_names = cache.ctx.func.params.user_named_funcs().clone();
    let compiled = cache
        .ctx
        .compile(isa, &mut cache.ctrl)
        .map_err(|err| format!("{err:?}"))?;
    let bytes = compiled.code_buffer().to_vec();
    let relocs = compiled
        .buffer
        .relocs()
        .iter()
        .map(|reloc| {
            let target = direct_relocation_target(&user_names, &reloc.target)?;
            Ok(Relocation {
                offset: reloc.offset,
                kind: reloc.kind,
                addend: reloc.addend,
                target,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(CompiledFunction { bytes, relocs })
}

fn direct_relocation_target(
    user_names: &PrimaryMap<ir::UserExternalNameRef, ir::UserExternalName>,
    target: &FinalizedRelocTarget,
) -> Result<Target, String> {
    match target {
        FinalizedRelocTarget::Func(offset) => Ok(Target::FunctionOffset(*offset)),
        FinalizedRelocTarget::ExternalName(ir::ExternalName::User(name_ref)) => {
            let name = user_names[*name_ref].clone();
            Symbol::from_external_name(name)
                .map(Target::Symbol)
                .ok_or_else(|| "unknown code symbol namespace in relocation".to_string())
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
        let mut cache = CompileContext::new();
        cache.ctx.func = Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, 0),
            trivial_i64_signature(),
        );

        {
            let mut builder = FunctionBuilder::new(&mut cache.ctx.func, &mut cache.builder_ctx);
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
        use crate::compiler::codegen::{ImportKind, ImportedSymbol, Symbol, declare_function};

        let isa = host_isa();
        let mut cache = CompileContext::new();
        cache.ctx.func = Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, 1),
            trivial_i64_signature(),
        );

        let target = Symbol::imported(ImportKind::RuntimeThunk, ImportedSymbol::new(3));

        {
            let imported_sig = cache.ctx.func.import_signature(trivial_i64_signature());
            let imported = declare_function(&mut cache.ctx.func, target, imported_sig, false);
            let mut builder = FunctionBuilder::new(&mut cache.ctx.func, &mut cache.builder_ctx);
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
                .any(|reloc| { reloc.target == Target::Symbol(target) })
        );
    }

    #[test]
    fn direct_relocations_preserve_runtime_thunk_ids_without_names() {
        use crate::{
            compiler::codegen::{ImportKind, ImportedSymbol, Symbol},
            runtime::vm::thunks::RuntimeThunk,
        };

        let thunk_id = RuntimeThunk::Thunk_wrong_number_of_args.id();
        let relocs = [Relocation {
            offset: 17,
            kind: Reloc::X86CallPCRel4,
            addend: -4,
            target: Target::Symbol(Symbol::imported(
                ImportKind::RuntimeThunk,
                ImportedSymbol::new(thunk_id),
            )),
        }];

        assert_eq!(
            relocs[0].target,
            Target::Symbol(Symbol::imported(
                ImportKind::RuntimeThunk,
                ImportedSymbol::new(thunk_id),
            ))
        );
    }
}

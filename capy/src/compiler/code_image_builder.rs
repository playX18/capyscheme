use std::collections::HashSet;

use crate::{
    compiler::{
        codegen::{DataSymbol, DataSymbolKind, FunctionSymbol},
        direct::{DirectCompiledFunction, code_image_relocations},
    },
    runtime::code_image::{
        CODE_IMAGE_VERSION, CodeEntry, CompiledCodeImage, DataSlot, DataSlotKind, Relocation,
        RelocationTarget, SideMetadataSlotKind,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeImageMetadata {
    pub constants_fasl: Vec<u8>,
    pub functions: Vec<CodeImageFunction>,
    pub data_slots: Vec<CodeImageDataSlot>,
    pub entry_code: FunctionSymbol,
    pub entry_is_cont: bool,
    pub target_triple: String,
}

impl CodeImageMetadata {
    pub fn for_host(
        constants_fasl: Vec<u8>,
        functions: Vec<CodeImageFunction>,
        data_slots: Vec<CodeImageDataSlot>,
        entry_code: FunctionSymbol,
        entry_is_cont: bool,
    ) -> Self {
        Self {
            constants_fasl,
            functions,
            data_slots,
            entry_code,
            entry_is_cont,
            target_triple: target_lexicon::Triple::host().to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeImageFunction {
    pub symbol: FunctionSymbol,
    pub name: String,
    pub compiled: DirectCompiledFunction,
    pub entry_offset: u32,
    pub arity: i32,
    pub is_cont: bool,
    pub metadata_constant: Option<u32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CodeImageDataSlot {
    pub symbol: DataSymbol,
    pub kind: DataSymbolKind,
    pub constant_index: Option<u32>,
    pub code: Option<FunctionSymbol>,
    pub pointer_target: Option<RelocationTarget>,
    pub side_metadata: Option<SideMetadataSlotKind>,
}

pub fn assemble_code_image(metadata: CodeImageMetadata) -> Result<CompiledCodeImage, String> {
    let mut function_ids = HashSet::new();
    let mut data_ids = HashSet::new();
    let mut code = Vec::with_capacity(metadata.functions.len());
    let mut relocations = Vec::new();

    for function in metadata.functions {
        let code_id = function.symbol.index();
        if !function_ids.insert(code_id) {
            return Err(format!("duplicate function symbol id {code_id}"));
        }

        let function_relocations = encode_function_relocations(code_id, &function.compiled)?;
        relocations.extend(function_relocations);
        code.push(CodeEntry {
            id: code_id,
            name: function.name,
            bytes: function.compiled.bytes,
            entry_offset: function.entry_offset,
            arity: function.arity,
            is_cont: function.is_cont,
            metadata_constant: function.metadata_constant,
        });
    }

    let entry_code_id = metadata.entry_code.index();
    if !function_ids.contains(&entry_code_id) {
        return Err(format!(
            "entry function symbol id {entry_code_id} was not declared"
        ));
    }

    let mut data_slots = Vec::with_capacity(metadata.data_slots.len());
    for slot in metadata.data_slots {
        let slot_id = slot.symbol.index();
        if !data_ids.insert(slot_id) {
            return Err(format!("duplicate data symbol id {slot_id}"));
        }

        data_slots.push(DataSlot {
            id: slot_id,
            kind: data_slot_kind(slot, &function_ids)?,
        });
    }

    Ok(CompiledCodeImage {
        version: CODE_IMAGE_VERSION,
        target_triple: metadata.target_triple,
        constants_fasl: metadata.constants_fasl,
        code,
        data_slots,
        relocations,
        entry_code_id,
        entry_is_cont: metadata.entry_is_cont,
    })
}

fn encode_function_relocations(
    code_id: u32,
    function: &DirectCompiledFunction,
) -> Result<Vec<Relocation>, String> {
    code_image_relocations(code_id, &function.relocs)
}

fn data_slot_kind(
    slot: CodeImageDataSlot,
    function_ids: &HashSet<u32>,
) -> Result<DataSlotKind, String> {
    match slot.kind {
        DataSymbolKind::Constant => {
            let constant_index = slot
                .constant_index
                .ok_or_else(|| "constant data slot requires a constant index".to_string())?;
            if slot.code.is_some() {
                return Err("constant data slot cannot reference code".to_string());
            }
            if slot.pointer_target.is_some() {
                return Err("constant data slot cannot reference a pointer target".to_string());
            }
            Ok(DataSlotKind::Constant { constant_index })
        }
        DataSymbolKind::CacheCell => {
            reject_payload(slot, "cache-cell")?;
            Ok(DataSlotKind::CacheCell)
        }
        DataSymbolKind::CodeBlock => {
            let code_id = slot
                .code
                .ok_or_else(|| "code-block data slot requires a function symbol".to_string())?
                .index();
            if slot.constant_index.is_some() {
                return Err("code-block data slot cannot reference a constant".to_string());
            }
            if slot.pointer_target.is_some() {
                return Err("code-block data slot cannot reference a pointer target".to_string());
            }
            if !function_ids.contains(&code_id) {
                return Err(format!(
                    "code-block data slot references undeclared function symbol id {code_id}"
                ));
            }
            Ok(DataSlotKind::CodeBlock { code_id })
        }
        DataSymbolKind::PointerSlot => {
            let target = slot
                .pointer_target
                .ok_or_else(|| "pointer data slot requires a relocation target".to_string())?;
            if slot.constant_index.is_some() {
                return Err("pointer data slot cannot reference a constant".to_string());
            }
            if slot.code.is_some() {
                return Err("pointer data slot cannot reference code metadata".to_string());
            }
            if slot.side_metadata.is_some() {
                return Err("pointer data slot cannot reference side metadata".to_string());
            }
            Ok(DataSlotKind::Pointer { target })
        }
        DataSymbolKind::SideMetadata => {
            let kind = slot.side_metadata.ok_or_else(|| {
                "side-metadata data slot requires a side metadata kind".to_string()
            })?;
            if slot.constant_index.is_some() {
                return Err("side-metadata data slot cannot reference a constant".to_string());
            }
            if slot.code.is_some() {
                return Err("side-metadata data slot cannot reference code".to_string());
            }
            Ok(DataSlotKind::SideMetadata { kind })
        }
        other => Err(format!(
            "data symbol kind {other:?} cannot be represented as a code image data slot"
        )),
    }
}

fn reject_payload(slot: CodeImageDataSlot, label: &str) -> Result<(), String> {
    if slot.constant_index.is_some() {
        return Err(format!("{label} data slot cannot reference a constant"));
    }
    if slot.code.is_some() {
        return Err(format!("{label} data slot cannot reference code"));
    }
    if slot.pointer_target.is_some() {
        return Err(format!("{label} data slot cannot reference a pointer target"));
    }
    if slot.side_metadata.is_some() {
        return Err(format!("{label} data slot cannot reference side metadata"));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use cranelift_codegen::binemit::Reloc;

    use crate::{
        compiler::{
            code_image_builder::{
                CodeImageDataSlot, CodeImageFunction, CodeImageMetadata, assemble_code_image,
            },
            codegen::{DataSymbol, DataSymbolKind, FunctionSymbol, ImportedSymbol},
            direct::{DirectCompiledFunction, DirectRelocation, DirectRelocationTarget},
        },
        runtime::{
            code_image::{
                CODE_IMAGE_VERSION, DataSlotKind, RelocationKind, RelocationTarget,
                SideMetadataSlotKind,
            },
            vm::thunks::RuntimeThunk,
        },
    };

    fn compiled(bytes: &[u8]) -> DirectCompiledFunction {
        DirectCompiledFunction {
            bytes: bytes.to_vec(),
            relocs: Vec::new(),
        }
    }

    #[test]
    fn code_entries_get_numeric_ids_from_function_symbols() {
        let image = assemble_code_image(CodeImageMetadata {
            constants_fasl: vec![1, 2, 3],
            functions: vec![
                CodeImageFunction {
                    symbol: FunctionSymbol::new(9),
                    name: "late".to_string(),
                    compiled: compiled(&[0xcc]),
                    entry_offset: 4,
                    arity: 2,
                    is_cont: false,
                    metadata_constant: Some(1),
                },
                CodeImageFunction {
                    symbol: FunctionSymbol::new(2),
                    name: "early".to_string(),
                    compiled: compiled(&[0xc3]),
                    entry_offset: 0,
                    arity: 0,
                    is_cont: true,
                    metadata_constant: None,
                },
            ],
            data_slots: Vec::new(),
            entry_code: FunctionSymbol::new(2),
            entry_is_cont: true,
            target_triple: "test-target".to_string(),
        })
        .expect("assemble code image");

        assert_eq!(image.version, CODE_IMAGE_VERSION);
        assert_eq!(image.target_triple, "test-target");
        assert_eq!(image.constants_fasl, vec![1, 2, 3]);
        assert_eq!(image.code[0].id, 9);
        assert_eq!(image.code[1].id, 2);
        assert_eq!(image.entry_code_id, 2);
        assert!(image.entry_is_cont);
    }

    #[test]
    fn data_slots_preserve_data_symbol_kind_mapping() {
        let image = assemble_code_image(CodeImageMetadata {
            constants_fasl: Vec::new(),
            functions: vec![CodeImageFunction {
                symbol: FunctionSymbol::new(7),
                name: "body".to_string(),
                compiled: compiled(&[0xc3]),
                entry_offset: 0,
                arity: 1,
                is_cont: false,
                metadata_constant: None,
            }],
            data_slots: vec![
                CodeImageDataSlot {
                    symbol: DataSymbol::new(1),
                    kind: DataSymbolKind::Constant,
                    constant_index: Some(3),
                    code: None,
                    pointer_target: None,
                    side_metadata: None,
                },
                CodeImageDataSlot {
                    symbol: DataSymbol::new(2),
                    kind: DataSymbolKind::CacheCell,
                    constant_index: None,
                    code: None,
                    pointer_target: None,
                    side_metadata: None,
                },
                CodeImageDataSlot {
                    symbol: DataSymbol::new(3),
                    kind: DataSymbolKind::CodeBlock,
                    constant_index: None,
                    code: Some(FunctionSymbol::new(7)),
                    pointer_target: None,
                    side_metadata: None,
                },
                CodeImageDataSlot {
                    symbol: DataSymbol::new(4),
                    kind: DataSymbolKind::PointerSlot,
                    constant_index: None,
                    code: None,
                    pointer_target: Some(RelocationTarget::Code { code_id: 7 }),
                    side_metadata: None,
                },
                CodeImageDataSlot {
                    symbol: DataSymbol::new(5),
                    kind: DataSymbolKind::SideMetadata,
                    constant_index: None,
                    code: None,
                    pointer_target: None,
                    side_metadata: Some(SideMetadataSlotKind::VoBit),
                },
            ],
            entry_code: FunctionSymbol::new(7),
            entry_is_cont: false,
            target_triple: "test-target".to_string(),
        })
        .expect("assemble code image");

        assert_eq!(
            image
                .data_slots
                .iter()
                .map(|slot| slot.id)
                .collect::<Vec<_>>(),
            vec![1, 2, 3, 4, 5]
        );
        assert_eq!(
            image
                .data_slots
                .iter()
                .map(|slot| slot.kind)
                .collect::<Vec<_>>(),
            vec![
                DataSlotKind::Constant { constant_index: 3 },
                DataSlotKind::CacheCell,
                DataSlotKind::CodeBlock { code_id: 7 },
                DataSlotKind::Pointer {
                    target: RelocationTarget::Code { code_id: 7 },
                },
                DataSlotKind::SideMetadata {
                    kind: SideMetadataSlotKind::VoBit,
                },
            ]
        );
    }

    #[test]
    fn runtime_thunk_relocations_are_encoded_by_imported_symbol_id() {
        let thunk_id = RuntimeThunk::Thunk_wrong_number_of_args.id();
        let image = assemble_code_image(CodeImageMetadata {
            constants_fasl: Vec::new(),
            functions: vec![CodeImageFunction {
                symbol: FunctionSymbol::new(5),
                name: "calls-thunk".to_string(),
                compiled: DirectCompiledFunction {
                    bytes: vec![0xe8, 0, 0, 0, 0],
                    relocs: vec![DirectRelocation {
                        offset: 1,
                        kind: Reloc::X86CallPCRel4,
                        addend: -4,
                        target: DirectRelocationTarget::BackendSymbol(
                            crate::compiler::codegen::BackendSymbol::Imported {
                                kind: crate::compiler::codegen::ImportedSymbolKind::RuntimeThunk,
                                symbol: ImportedSymbol::new(thunk_id),
                            },
                        ),
                    }],
                },
                entry_offset: 0,
                arity: 0,
                is_cont: false,
                metadata_constant: None,
            }],
            data_slots: Vec::new(),
            entry_code: FunctionSymbol::new(5),
            entry_is_cont: false,
            target_triple: "test-target".to_string(),
        })
        .expect("assemble code image");

        assert_eq!(image.relocations.len(), 1);
        assert_eq!(image.relocations[0].code_id, 5);
        assert_eq!(image.relocations[0].kind, RelocationKind::X86CallPcRel4);
        assert_eq!(
            image.relocations[0].target,
            RelocationTarget::RuntimeThunk { thunk_id }
        );
    }
}

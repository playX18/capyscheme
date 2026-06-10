use super::{
    FASL_COMPRESSION_GZIP, FASL_COMPRESSION_NONE, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF,
    FASL_TAG_UNLINKED_CODEBLOCK, FaslReader,
};

#[test]
fn fasl_writer_emits_uncompressed_image() {
    use super::FaslWriter;
    use crate::runtime::{Scheme, value::Value};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        FaslWriter::new(ctx, &mut bytes)
            .write(Value::new(42))
            .expect("write grouped FASL");

        assert_eq!(&bytes[0..8], super::FASL_MAGIC);
        assert_eq!(&bytes[8..12], &super::FASL_VERSION.to_le_bytes());
        assert_eq!(bytes[12], FASL_COMPRESSION_NONE);
        assert_eq!(&bytes[13..17], &0u32.to_le_bytes()); // lites
        let uncompressed_size = u32::from_le_bytes(bytes[17..21].try_into().unwrap()) as usize;
        let stored_size = u32::from_le_bytes(bytes[21..25].try_into().unwrap()) as usize;
        assert_eq!(uncompressed_size, stored_size);
        assert_eq!(stored_size, bytes.len() - 25);
    });
}

#[test]
fn fasl_reader_loads_gzip_image() {
    use super::{FaslCompression, FaslImage, FaslWriter};
    use crate::runtime::{Scheme, value::Value};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        FaslWriter::new(ctx, &mut bytes)
            .write_image(FaslImage::Value(Value::new(42)), FaslCompression::Gzip)
            .expect("write gzip FASL");
        assert_eq!(bytes[12], FASL_COMPRESSION_GZIP);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read gzip FASL");
        assert_eq!(value, Value::new(42));
    });
}

#[test]
fn fasl_reader_loads_pure_data_fasl_after_version_header() {
    use super::{FASL_TAG_FIXNUM, FaslReader};
    use crate::runtime::{Scheme, value::Value};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut payload = Vec::new();
        payload.push(FASL_TAG_FIXNUM);
        payload.extend_from_slice(&42i32.to_le_bytes());
        let bytes = fasl_image(payload);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read pure data FASL");
        assert_eq!(value, Value::new(42));
    });
}

#[test]
fn fasl_reader_rejects_old_fasl_version() {
    use crate::runtime::Scheme;

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(super::FASL_MAGIC);
        bytes.extend_from_slice(&(super::FASL_VERSION - 1).to_le_bytes());
        bytes.extend_from_slice(&0u32.to_le_bytes());

        let err = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect_err("old FASL version should be rejected");
        assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
    });
}

#[test]
fn fasl_relocation_records_roundtrip_all_current_target_families() {
    use super::reloc::{RelocKind, RelocTarget, Relocation, SideMetadataSlot};
    use asmkit::core::buffer::Reloc as AsmkitReloc;
    use cranelift_codegen::binemit::Reloc as CraneliftReloc;

    let relocations = vec![
        Relocation {
            offset: 4,
            kind: RelocKind::Asmkit(AsmkitReloc::X86CallPCRel4),
            target: RelocTarget::Entry(7),
            addend: -4,
        },
        Relocation {
            offset: 16,
            kind: RelocKind::Cranelift(CraneliftReloc::Arm64Call),
            target: RelocTarget::Entry(8),
            addend: -4,
        },
        Relocation {
            offset: 20,
            kind: RelocKind::Cranelift(CraneliftReloc::RiscvCallPlt),
            target: RelocTarget::Entry(9),
            addend: 0,
        },
        Relocation {
            offset: 24,
            kind: RelocKind::CraneliftDataSlot(CraneliftReloc::Aarch64AdrPrelPgHi21),
            target: RelocTarget::Entry(10),
            addend: 0,
        },
        Relocation {
            offset: 28,
            kind: RelocKind::AbsWord,
            target: RelocTarget::Object(3),
            addend: 8,
        },
        Relocation {
            offset: 32,
            kind: RelocKind::CodeEntry,
            target: RelocTarget::Entry(5),
            addend: 0,
        },
        Relocation {
            offset: 36,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::Object(9),
            addend: 16,
        },
        Relocation {
            offset: 40,
            kind: RelocKind::RuntimeThunk,
            target: RelocTarget::RuntimeSymbol(11),
            addend: 0,
        },
        Relocation {
            offset: 48,
            kind: RelocKind::RuntimeData,
            target: RelocTarget::RuntimeSymbol(12),
            addend: -8,
        },
        Relocation {
            offset: 56,
            kind: RelocKind::SideMetadata,
            target: RelocTarget::SideMetadata(SideMetadataSlot::VoBit),
            addend: 0,
        },
        Relocation {
            offset: 64,
            kind: RelocKind::CacheCell,
            target: RelocTarget::CacheCell(13),
            addend: 0,
        },
    ];

    let mut encoded = Vec::new();
    for relocation in &relocations {
        relocation.encode(&mut encoded).expect("encode relocation");
    }

    let mut input = std::io::Cursor::new(encoded.as_slice());
    let decoded = (0..relocations.len())
        .map(|_| Relocation::decode(&mut input))
        .collect::<Result<Vec<_>, _>>()
        .expect("decode relocations");

    assert_eq!(decoded, relocations);
    assert_eq!(input.position(), encoded.len() as u64);

    let unlinked = super::reloc::encode_unlinked_relocations(&relocations)
        .expect("encode unlinked relocations");
    let decoded_unlinked =
        super::reloc::decode_unlinked_relocations(&unlinked).expect("decode unlinked relocations");
    assert_eq!(decoded_unlinked, relocations);
}

fn put_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put_fasl_header(out: &mut Vec<u8>) -> usize {
    out.extend_from_slice(super::FASL_MAGIC);
    out.extend_from_slice(&super::FASL_VERSION.to_le_bytes());
    out.push(super::FASL_COMPRESSION_NONE);
    put_u32(out, 0); // lites
    let payload_start = out.len() + 8;
    put_u32(out, 0);
    put_u32(out, 0);
    payload_start
}

fn finish_fasl_image(out: &mut [u8], payload_start: usize) {
    let payload_size = (out.len() - payload_start) as u32;
    out[payload_start - 8..payload_start - 4].copy_from_slice(&payload_size.to_le_bytes());
    out[payload_start - 4..payload_start].copy_from_slice(&payload_size.to_le_bytes());
}

fn fasl_image(payload: Vec<u8>) -> Vec<u8> {
    let mut bytes = Vec::new();
    let payload_start = put_fasl_header(&mut bytes);
    bytes.extend_from_slice(&payload);
    finish_fasl_image(&mut bytes, payload_start);
    bytes
}

#[allow(dead_code)]
fn put_test_relocatable_code_block(out: &mut Vec<u8>, code: &[u8], entry_offset: u32, arity: i32) {
    out.push(super::FASL_TAG_UNLINKED_CODEBLOCK);
    put_u32(out, code.len() as u32);
    out.extend_from_slice(code);
    put_u32(out, entry_offset);
    out.extend_from_slice(&arity.to_le_bytes());
    out.push(0);
    out.push(super::FASL_TAG_F);
    put_u32(out, 0);
}

#[test]
fn graph_def_and_ref_preserve_shared_objects() {
    use super::{
        FASL_TAG_BVECTOR, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASL_TAG_VECTOR,
        FaslReader,
    };
    use crate::runtime::{Scheme, value::Vector};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 1); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_BVECTOR);
        bytes.extend_from_slice(&3u64.to_le_bytes());
        bytes.extend_from_slice(&[1, 2, 3]);
        bytes.push(FASL_TAG_GRAPH_REF);
        put_u32(&mut bytes, 0);
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read graph fasl");
        let vector = value.downcast::<Vector>();
        let first = vector[0].get();
        let second = vector[1].get();

        assert_eq!(first, second);
    });
}

#[test]
fn graph_def_vector_can_reference_itself() {
    use super::{
        FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASL_TAG_VECTOR, FaslReader,
    };
    use crate::{
        rsgc::Gc,
        runtime::{Scheme, value::Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 1); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_GRAPH_REF);
        put_u32(&mut bytes, 0);
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read cyclic vector graph");
        let vector = value.downcast::<Vector>();

        assert!(Gc::ptr_eq(vector[0].get().downcast::<Vector>(), vector));
    });
}

#[test]
fn legacy_ref_init_ref_roundtrips_shared_vector_element() {
    use super::{FaslReader, FaslWriter};
    use crate::runtime::{
        Scheme,
        value::{ByteVector, Value, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let shared = Value::new(ByteVector::from_slice(*ctx, &[1, 2, 3], true));
        let vector = Value::new(Vector::from_slice(*ctx, &[shared, shared]));
        let mut bytes = Vec::new();
        FaslWriter::new(ctx, &mut bytes)
            .write(vector)
            .expect("write legacy FASL refs");

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read legacy FASL refs");
        let vector = value.downcast::<Vector>();

        assert_eq!(vector[0].get(), vector[1].get());
    });
}

#[test]
fn fasl_writer_emits_loadable_zero_relocation_closure_with_code_block() {
    use super::{CodeSpec, FaslCompression, FaslImage, FaslWriter, GraphCodeSpec, ProgramSpec};
    use crate::runtime::{
        Scheme,
        value::{Closure, CodeBlockKind, Value},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let code = CodeSpec::new(&[0xc3], 0, 0, false, Value::new(false), &[]);
        let code_blocks = [GraphCodeSpec::new(0, code)];
        let program = ProgramSpec::new(1, &[], &code_blocks, 0, false);
        FaslWriter::new(ctx, &mut bytes)
            .write_image(FaslImage::Program(&program), FaslCompression::None)
            .expect("write unified FASL closure");

        assert_eq!(&bytes[0..8], super::FASL_MAGIC);
        assert_eq!(bytes[12], FASL_COMPRESSION_NONE);
        let payload_start = 25;
        assert_eq!(bytes[payload_start], FASL_TAG_GRAPH);
        assert_eq!(bytes[payload_start + 14], FASL_TAG_GRAPH_DEF);
        assert_eq!(bytes[payload_start + 19], FASL_TAG_UNLINKED_CODEBLOCK);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load writer-produced closure");
        let closure = value.downcast::<Closure>();
        let code_block = closure.code_block;
        assert_eq!(closure.nfree, 0);
        assert_eq!(closure.code, code_block.entrypoint);
        assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
        assert_eq!(code_block.arity.fixed_arity(), 0);
    });
}

#[test]
fn fasl_reader_decodes_relocatable_code_block_value() {
    use crate::runtime::{Scheme, value::RelocatableCodeBlock};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        put_test_relocatable_code_block(&mut bytes, &[0xc3], 0, 0);
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read unlinked code block");
        let unlinked = value.downcast::<RelocatableCodeBlock>();
        assert_eq!(unlinked.code(), [0xc3]);
        assert_eq!(unlinked.entry_offset, 0);
        assert_eq!(unlinked.relocations(), &[]);
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_reads_zero_relocation_closure_with_code_block() {
    use super::{
        FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF,
    };
    use crate::runtime::{
        Scheme,
        value::{Closure, CodeBlockKind},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 1); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_CLOSURE);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // code byte length
        bytes.push(0xc3); // ret
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes()); // arity
        bytes.push(0); // code-block is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        put_u32(&mut bytes, 0); // closure free count
        bytes.push(0); // closure is_cont
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load zero-relocation code closure");
        let closure = value.downcast::<Closure>();
        let code_block = closure.code_block;
        assert_eq!(closure.nfree, 0);
        assert_eq!(closure.code, code_block.entrypoint);
        assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
        assert_eq!(code_block.arity.fixed_arity(), 0);
        assert_eq!(code_block.unlinked.code(), [0xc3]);
        assert!(code_block.has_live_span());
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_entry_resolves_graph_defined_code_block() {
    use super::{
        FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_ENTRY, FASL_TAG_F, FASL_TAG_GRAPH,
        FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
    };
    use crate::rsgc::Gc;
    use crate::runtime::{
        Scheme,
        value::{Closure, CodeBlockKind, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 1); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2); // code block, closure
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // code byte length
        bytes.push(0xc3); // ret
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes()); // arity
        bytes.push(0); // code-block is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        bytes.push(FASL_TAG_CLOSURE);
        bytes.push(FASL_TAG_ENTRY);
        put_u32(&mut bytes, 0); // graph-defined code block
        put_u32(&mut bytes, 0); // closure free count
        bytes.push(0); // closure is_cont
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load closure using entry reference");
        let vector = value.downcast::<Vector>();
        let code_block = vector[0].get();
        let closure = vector[1].get().downcast::<Closure>();

        assert!(Gc::ptr_eq(closure.code_block, code_block.downcast()));
        assert_eq!(closure.code, closure.code_block.entrypoint);
        assert!(matches!(closure.code_block.kind, CodeBlockKind::Loaded));
    });
}

#[test]
fn fasl_reader_reads_code_block_as_value() {
    use super::{FASL_TAG_CODE_BLOCK, FASL_TAG_F};
    use crate::runtime::{Scheme, value::CodeBlock};

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // code byte length
        bytes.push(0xc3);
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("read code block through normal FASL reader");
        assert!(value.is::<CodeBlock>());
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_applies_asmkit_abs8_code_block_relocation() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F,
        reloc::{RelocKind, RelocTarget, Relocation, SideMetadataSlot},
    };
    use crate::runtime::{Scheme, value::CodeBlock};
    use asmkit::core::buffer::Reloc as AsmkitReloc;
    use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 9); // ret plus one word patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::Asmkit(AsmkitReloc::Abs8),
            target: RelocTarget::SideMetadata(SideMetadataSlot::Global),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load asmkit abs8 relocation");
        let code_block = value.downcast::<CodeBlock>();
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let patched = unsafe {
            std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
        };
        assert_eq!(patched, global_side_metadata_vm_base_address().as_usize());
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_applies_asmkit_x86_pc_rel4_code_entry_relocation() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Vector},
    };
    use asmkit::core::buffer::Reloc as AsmkitReloc;

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // target code byte length
        bytes.push(0xc3);
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 5); // ret plus rel32 patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0i32.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::Asmkit(AsmkitReloc::X86PCRel4),
            target: RelocTarget::Entry(0),
            addend: -4,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load asmkit rel32 relocation");
        let values = value.downcast::<Vector>();
        let target = values[0].get().downcast::<CodeBlock>();
        let source = values[1].get().downcast::<CodeBlock>();
        let patched =
// SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const i32) };
        let expected =
            target.entrypoint.as_usize() as i128 - (source.entrypoint.as_usize() + 1 + 4) as i128;
        assert_eq!(patched, i32::try_from(expected).unwrap());
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_applies_data_slot_address_relocation_to_graph_object() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Value, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 0); // relocation target object
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 9); // ret plus one word patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::Object(0),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load data-slot relocation");
        let values = value.downcast::<Vector>();
        let target = values[0].get();
        let code_block = values[1].get().downcast::<CodeBlock>();
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let slot_address = unsafe {
            std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
        };
        assert_eq!(slot_address, code_block.loaded_data_base.as_usize());
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
        assert_eq!(slot_value, target);
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_resolves_forward_code_entry_data_slot_address_relocation() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 9); // ret plus one word patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::Entry(1),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // target code byte length
        bytes.push(0xc3);
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load forward code-entry data-slot relocation");
        let values = value.downcast::<Vector>();
        let source = values[0].get().downcast::<CodeBlock>();
        let target = values[1].get().downcast::<CodeBlock>();
        assert_eq!(source.loaded_data_value_bitmap(), &[1]);
        let slot_address =
// SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const usize) };
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let slot_word = unsafe { std::ptr::read(slot_address as *const usize) };
        assert_eq!(slot_word, target.entrypoint.as_usize());
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_keeps_raw_data_slots_out_of_value_bitmap() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation, SideMetadataSlot},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 0); // value relocation target
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 17); // ret plus two word patch slots
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 2); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::Object(0),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode value relocation");
        Relocation {
            offset: 1 + std::mem::size_of::<usize>() as u32,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::SideMetadata(SideMetadataSlot::Global),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode raw relocation");
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load mixed data-slot relocations");
        let values = value.downcast::<Vector>();
        let code_block = values[1].get().downcast::<CodeBlock>();
        assert_eq!(code_block.loaded_data_slot_count, 2);
        assert_eq!(code_block.loaded_data_value_bitmap(), &[1]);
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_resolves_forward_data_slot_address_relocation() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Value, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 9); // ret plus one word patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::DataSlotAddress,
            target: RelocTarget::Object(1),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 0); // relocation target object
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load forward data-slot relocation");
        let values = value.downcast::<Vector>();
        let code_block = values[0].get().downcast::<CodeBlock>();
        let target = values[1].get();
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let slot_address = unsafe {
            std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
        };
        // SAFETY: The target pointer is valid, aligned, and points to initialized memory
        let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
        assert_eq!(slot_value, target);
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_resolves_forward_code_entry_relocation() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 2); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 2);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 9); // ret plus one word patch slot
        bytes.push(0xc3);
        bytes.extend_from_slice(&0usize.to_le_bytes());
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 1); // relocation count
        Relocation {
            offset: 1,
            kind: RelocKind::CodeEntry,
            target: RelocTarget::Entry(1),
            addend: 0,
        }
        .encode(&mut bytes)
        .expect("encode relocation");
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 1);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(&mut bytes, 1); // target code byte length
        bytes.push(0xc3);
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, 0); // relocation count
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load forward code-entry relocation");
        let values = value.downcast::<Vector>();
        let source = values[0].get().downcast::<CodeBlock>();
        let target = values[1].get().downcast::<CodeBlock>();
        let patched =
// SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const usize) };
        assert_eq!(patched, target.entrypoint.as_usize());
    });
}

#[test]
fn fasl_reader_shares_cache_cell_slots_across_code_blocks() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Value, Vector},
    };

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, 3); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, 3);
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, 0);
        bytes.push(FASL_TAG_F); // initial cache-cell value
        for code_index in [1, 2] {
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, code_index);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1 + std::mem::size_of::<usize>() as u32);
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            Relocation {
                offset: 1,
                kind: RelocKind::CacheCell,
                target: RelocTarget::CacheCell(0),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");
        }
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load shared cache-cell relocations");
        let values = value.downcast::<Vector>();
        let first = values[1].get().downcast::<CodeBlock>();
        let second = values[2].get().downcast::<CodeBlock>();
        let first_slot =
// SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read_unaligned((first.entrypoint.as_usize() + 1) as *const usize) };
        let second_slot =
// SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read_unaligned((second.entrypoint.as_usize() + 1) as *const usize) };

        assert_eq!(first_slot, second_slot);
        assert_eq!(
            // SAFETY: The target pointer is valid, aligned, and points to initialized memory
            unsafe { std::ptr::read(first_slot as *const Value) },
            Value::new(false)
        );
        assert_eq!(first.loaded_data_slot_count, 1);
        assert_eq!(second.loaded_data_slot_count, 1);
        assert_eq!(second.loaded_data_value_bitmap(), &[1]);
    });
}

#[cfg(target_arch = "x86_64")]
#[test]
fn fasl_reader_accepts_more_than_64_value_data_slots() {
    use super::{
        FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        reloc::{RelocKind, RelocTarget, Relocation},
    };
    use crate::runtime::{
        Scheme,
        value::{CodeBlock, Value, Vector},
    };

    const VALUE_SLOT_COUNT: usize = 65;

    let scm = Scheme::new_uninit();
    scm.enter(|ctx| {
        let mut bytes = Vec::new();
        let payload_start = put_fasl_header(&mut bytes);
        bytes.push(FASL_TAG_GRAPH);
        put_u32(&mut bytes, (VALUE_SLOT_COUNT + 1) as u32); // graph length
        put_u32(&mut bytes, 0); // external count
        bytes.push(FASL_TAG_VECTOR);
        put_u32(&mut bytes, (VALUE_SLOT_COUNT + 1) as u32);
        for index in 0..VALUE_SLOT_COUNT {
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, index as u32);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 0);
        }
        bytes.push(FASL_TAG_GRAPH_DEF);
        put_u32(&mut bytes, VALUE_SLOT_COUNT as u32);
        bytes.push(FASL_TAG_CODE_BLOCK);
        put_u32(
            &mut bytes,
            (1 + VALUE_SLOT_COUNT * std::mem::size_of::<usize>()) as u32,
        );
        bytes.push(0xc3);
        bytes.resize(
            bytes.len() + VALUE_SLOT_COUNT * std::mem::size_of::<usize>(),
            0,
        );
        put_u32(&mut bytes, 0); // entry offset
        bytes.extend_from_slice(&0i32.to_le_bytes());
        bytes.push(0); // is_cont
        bytes.push(FASL_TAG_F); // metadata
        put_u32(&mut bytes, VALUE_SLOT_COUNT as u32);
        for index in 0..VALUE_SLOT_COUNT {
            Relocation {
                offset: 1 + (index * std::mem::size_of::<usize>()) as u32,
                kind: RelocKind::DataSlotAddress,
                target: RelocTarget::Object(index as u32),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");
        }
        finish_fasl_image(&mut bytes, payload_start);

        let value = FaslReader::new(ctx, std::io::Cursor::new(bytes))
            .read()
            .expect("load data-slot relocations");
        let values = value.downcast::<Vector>();
        let code_block = values[VALUE_SLOT_COUNT].get().downcast::<CodeBlock>();
        for index in [0, VALUE_SLOT_COUNT - 1] {
            // SAFETY: The target pointer is valid, aligned, and points to initialized memory
            let slot_address = unsafe {
                std::ptr::read_unaligned(
                    (code_block.entrypoint.as_usize() + 1 + index * std::mem::size_of::<usize>())
                        as *const usize,
                )
            };
            // SAFETY: The target pointer is valid, aligned, and points to initialized memory
            let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
            assert_eq!(slot_value, values[index].get());
        }
        assert_eq!(code_block.loaded_data_slot_count as usize, VALUE_SLOT_COUNT);
    });
}

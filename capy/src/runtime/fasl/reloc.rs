use std::io::{self, Read, Write};

use asmkit::core::buffer::Reloc as AsmkitReloc;
use cranelift_codegen::binemit::Reloc as CraneliftReloc;

use crate::runtime::value::CodeRelocation;

use super::{
    FASL_RELOC_ABS_WORD, FASL_RELOC_ASMKIT, FASL_RELOC_CACHE_CELL, FASL_RELOC_CODE_ENTRY,
    FASL_RELOC_CRANELIFT, FASL_RELOC_CRANELIFT_DATA_SLOT, FASL_RELOC_DATA_SLOT,
    FASL_RELOC_RUNTIME_DATA, FASL_RELOC_RUNTIME_THUNK, FASL_RELOC_SIDE_METADATA,
};

const CRANELIFT_RELOCS: &[CraneliftReloc] = &[
    CraneliftReloc::Abs4,
    CraneliftReloc::Abs8,
    CraneliftReloc::X86PCRel4,
    CraneliftReloc::X86CallPCRel4,
    CraneliftReloc::X86CallPLTRel4,
    CraneliftReloc::X86GOTPCRel4,
    CraneliftReloc::X86SecRel,
    CraneliftReloc::Arm32Call,
    CraneliftReloc::Arm64Call,
    CraneliftReloc::S390xPCRel32Dbl,
    CraneliftReloc::S390xPLTRel32Dbl,
    CraneliftReloc::ElfX86_64TlsGd,
    CraneliftReloc::MachOX86_64Tlv,
    CraneliftReloc::MachOAarch64TlsAdrPage21,
    CraneliftReloc::MachOAarch64TlsAdrPageOff12,
    CraneliftReloc::Aarch64TlsDescAdrPage21,
    CraneliftReloc::Aarch64TlsDescLd64Lo12,
    CraneliftReloc::Aarch64TlsDescAddLo12,
    CraneliftReloc::Aarch64TlsDescCall,
    CraneliftReloc::Aarch64AdrGotPage21,
    CraneliftReloc::Aarch64AdrPrelPgHi21,
    CraneliftReloc::Aarch64AddAbsLo12Nc,
    CraneliftReloc::Aarch64Ld64GotLo12Nc,
    CraneliftReloc::RiscvCallPlt,
    CraneliftReloc::RiscvTlsGdHi20,
    CraneliftReloc::RiscvPCRelLo12I,
    CraneliftReloc::RiscvGotHi20,
    CraneliftReloc::RiscvPCRelHi20,
    CraneliftReloc::S390xTlsGd64,
    CraneliftReloc::S390xTlsGdCall,
    CraneliftReloc::PulleyPcRel,
    CraneliftReloc::PulleyCallIndirectHost,
];

const ASMKIT_RELOCS: &[AsmkitReloc] = &[
    AsmkitReloc::Abs4,
    AsmkitReloc::Abs8,
    AsmkitReloc::X86PCRel4,
    AsmkitReloc::X86CallPCRel4,
    AsmkitReloc::X86CallPLTRel4,
    AsmkitReloc::X86GOTPCRel4,
    AsmkitReloc::X86SecRel,
    AsmkitReloc::Arm32Call,
    AsmkitReloc::Arm64Call,
    AsmkitReloc::ElfX86_64TlsGd,
    AsmkitReloc::MachOX86_64Tlv,
    AsmkitReloc::MachOAarch64TlsAdrPage21,
    AsmkitReloc::MachOAarch64TlsAdrPageOff12,
    AsmkitReloc::Aarch64TlsDescAdrPage21,
    AsmkitReloc::Aarch64TlsDescLd64Lo12,
    AsmkitReloc::Aarch64TlsDescAddLo12,
    AsmkitReloc::Aarch64TlsDescCall,
    AsmkitReloc::Aarch64AdrGotPage21,
    AsmkitReloc::Aarch64Ld64GotLo12Nc,
    AsmkitReloc::Aarch64AdrPrelPgHi21,
    AsmkitReloc::Aarch64AddAbsLo12Nc,
    AsmkitReloc::RiscvAbs8,
    AsmkitReloc::RiscvCallPlt,
    AsmkitReloc::RiscvTlsGdHi20,
    AsmkitReloc::RiscvPCRelLo12I,
    AsmkitReloc::RiscvGotHi20,
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relocation {
    pub offset: u32,
    pub kind: RelocKind,
    pub target: RelocTarget,
    pub addend: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RelocKind {
    Asmkit(AsmkitReloc),
    Cranelift(CraneliftReloc),
    CraneliftDataSlot(CraneliftReloc),
    AbsWord,
    CodeEntry,
    DataSlotAddress,
    RuntimeThunk,
    RuntimeData,
    SideMetadata,
    CacheCell,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RelocTarget {
    Object(u32),
    Entry(u32),
    CacheCell(u32),
    RuntimeSymbol(u32),
    SideMetadata(SideMetadataSlot),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SideMetadataSlot {
    Global,
    VoBit,
}

const UNLINKED_TARGET_OBJECT: u8 = 0;
const UNLINKED_TARGET_ENTRY: u8 = 1;
const UNLINKED_TARGET_RUNTIME_SYMBOL: u8 = 2;
const UNLINKED_TARGET_SIDE_METADATA: u8 = 3;
const UNLINKED_TARGET_CACHE_CELL: u8 = 4;

impl Relocation {
    pub fn new(offset: u32, kind: RelocKind, target: RelocTarget, addend: i64) -> Self {
        Self {
            offset,
            kind,
            target,
            addend,
        }
    }

    pub fn encode(&self, out: &mut impl Write) -> io::Result<()> {
        write_u32(out, self.offset)?;
        self.kind.encode(out)?;
        self.target.encode(out)?;
        write_i64(out, self.addend)
    }

    pub fn decode(input: &mut impl Read) -> io::Result<Self> {
        Ok(Self {
            offset: read_u32(input)?,
            kind: RelocKind::decode(input)?,
            target: RelocTarget::decode(input)?,
            addend: read_i64(input)?,
        })
    }
}

pub(crate) fn encode_unlinked_relocations(
    relocations: &[Relocation],
) -> io::Result<Vec<CodeRelocation>> {
    relocations
        .iter()
        .map(|relocation| {
            let (kind_tag, aux_tag) = match &relocation.kind {
                RelocKind::Asmkit(reloc) => (FASL_RELOC_ASMKIT, asmkit_reloc_to_tag(*reloc)),
                RelocKind::Cranelift(reloc) => {
                    (FASL_RELOC_CRANELIFT, cranelift_reloc_to_tag(*reloc))
                }
                RelocKind::CraneliftDataSlot(reloc) => (
                    FASL_RELOC_CRANELIFT_DATA_SLOT,
                    cranelift_reloc_to_tag(*reloc),
                ),
                RelocKind::AbsWord => (FASL_RELOC_ABS_WORD, 0),
                RelocKind::CodeEntry => (FASL_RELOC_CODE_ENTRY, 0),
                RelocKind::DataSlotAddress => (FASL_RELOC_DATA_SLOT, 0),
                RelocKind::RuntimeThunk => (FASL_RELOC_RUNTIME_THUNK, 0),
                RelocKind::RuntimeData => (FASL_RELOC_RUNTIME_DATA, 0),
                RelocKind::SideMetadata => (FASL_RELOC_SIDE_METADATA, 0),
                RelocKind::CacheCell => (FASL_RELOC_CACHE_CELL, 0),
            };
            let (target_tag, target_payload) = match relocation.target {
                RelocTarget::Object(index) => (UNLINKED_TARGET_OBJECT, index),
                RelocTarget::Entry(index) => (UNLINKED_TARGET_ENTRY, index),
                RelocTarget::CacheCell(index) => (UNLINKED_TARGET_CACHE_CELL, index),
                RelocTarget::RuntimeSymbol(symbol) => (UNLINKED_TARGET_RUNTIME_SYMBOL, symbol),
                RelocTarget::SideMetadata(kind) => {
                    (UNLINKED_TARGET_SIDE_METADATA, kind.to_tag() as u32)
                }
            };
            Ok(CodeRelocation {
                offset: relocation.offset,
                kind_tag,
                target_tag,
                target_payload,
                addend: relocation.addend,
                aux_tag,
            })
        })
        .collect()
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn decode_unlinked_relocations(
    relocations: &[CodeRelocation],
) -> io::Result<Vec<Relocation>> {
    relocations
        .iter()
        .map(|relocation| {
            let kind = match relocation.kind_tag {
                FASL_RELOC_ASMKIT => RelocKind::Asmkit(asmkit_reloc_from_tag(relocation.aux_tag)?),
                FASL_RELOC_CRANELIFT => {
                    RelocKind::Cranelift(cranelift_reloc_from_tag(relocation.aux_tag)?)
                }
                FASL_RELOC_CRANELIFT_DATA_SLOT => {
                    RelocKind::CraneliftDataSlot(cranelift_reloc_from_tag(relocation.aux_tag)?)
                }
                FASL_RELOC_ABS_WORD => RelocKind::AbsWord,
                FASL_RELOC_CODE_ENTRY => RelocKind::CodeEntry,
                FASL_RELOC_DATA_SLOT => RelocKind::DataSlotAddress,
                FASL_RELOC_RUNTIME_THUNK => RelocKind::RuntimeThunk,
                FASL_RELOC_RUNTIME_DATA => RelocKind::RuntimeData,
                FASL_RELOC_SIDE_METADATA => RelocKind::SideMetadata,
                FASL_RELOC_CACHE_CELL => RelocKind::CacheCell,
                _ => return Err(invalid_data("invalid unlinked relocation kind tag")),
            };
            let target = match relocation.target_tag {
                UNLINKED_TARGET_OBJECT => RelocTarget::Object(relocation.target_payload),
                UNLINKED_TARGET_ENTRY => RelocTarget::Entry(relocation.target_payload),
                UNLINKED_TARGET_CACHE_CELL => RelocTarget::CacheCell(relocation.target_payload),
                UNLINKED_TARGET_RUNTIME_SYMBOL => {
                    RelocTarget::RuntimeSymbol(relocation.target_payload)
                }
                UNLINKED_TARGET_SIDE_METADATA => {
                    let tag = u8::try_from(relocation.target_payload)
                        .map_err(|_| invalid_data("invalid unlinked side metadata tag"))?;
                    RelocTarget::SideMetadata(SideMetadataSlot::from_tag(tag)?)
                }
                _ => return Err(invalid_data("invalid unlinked relocation target tag")),
            };
            Ok(Relocation {
                offset: relocation.offset,
                kind,
                target,
                addend: relocation.addend,
            })
        })
        .collect()
}

impl RelocKind {
    fn encode(&self, out: &mut impl Write) -> io::Result<()> {
        match self {
            Self::Asmkit(reloc) => {
                write_u8(out, FASL_RELOC_ASMKIT)?;
                write_u8(out, asmkit_reloc_to_tag(*reloc))
            }
            Self::Cranelift(reloc) => {
                write_u8(out, FASL_RELOC_CRANELIFT)?;
                write_u8(out, cranelift_reloc_to_tag(*reloc))
            }
            Self::CraneliftDataSlot(reloc) => {
                write_u8(out, FASL_RELOC_CRANELIFT_DATA_SLOT)?;
                write_u8(out, cranelift_reloc_to_tag(*reloc))
            }
            Self::AbsWord => write_u8(out, FASL_RELOC_ABS_WORD),
            Self::CodeEntry => write_u8(out, FASL_RELOC_CODE_ENTRY),
            Self::DataSlotAddress => write_u8(out, FASL_RELOC_DATA_SLOT),
            Self::RuntimeThunk => write_u8(out, FASL_RELOC_RUNTIME_THUNK),
            Self::RuntimeData => write_u8(out, FASL_RELOC_RUNTIME_DATA),
            Self::SideMetadata => write_u8(out, FASL_RELOC_SIDE_METADATA),
            Self::CacheCell => write_u8(out, FASL_RELOC_CACHE_CELL),
        }
    }

    fn decode(input: &mut impl Read) -> io::Result<Self> {
        match read_u8(input)? {
            FASL_RELOC_ASMKIT => Ok(Self::Asmkit(asmkit_reloc_from_tag(read_u8(input)?)?)),
            FASL_RELOC_CRANELIFT => Ok(Self::Cranelift(cranelift_reloc_from_tag(read_u8(input)?)?)),
            FASL_RELOC_CRANELIFT_DATA_SLOT => Ok(Self::CraneliftDataSlot(
                cranelift_reloc_from_tag(read_u8(input)?)?,
            )),
            FASL_RELOC_ABS_WORD => Ok(Self::AbsWord),
            FASL_RELOC_CODE_ENTRY => Ok(Self::CodeEntry),
            FASL_RELOC_DATA_SLOT => Ok(Self::DataSlotAddress),
            FASL_RELOC_RUNTIME_THUNK => Ok(Self::RuntimeThunk),
            FASL_RELOC_RUNTIME_DATA => Ok(Self::RuntimeData),
            FASL_RELOC_SIDE_METADATA => Ok(Self::SideMetadata),
            FASL_RELOC_CACHE_CELL => Ok(Self::CacheCell),
            _ => Err(invalid_data("invalid FASL relocation kind")),
        }
    }
}

pub(crate) fn cranelift_reloc_to_tag(reloc: CraneliftReloc) -> u8 {
    reloc_to_tag(
        CRANELIFT_RELOCS,
        reloc,
        "Cranelift relocation table is missing a variant",
    )
}

pub(crate) fn cranelift_reloc_from_tag(tag: u8) -> io::Result<CraneliftReloc> {
    reloc_from_tag(CRANELIFT_RELOCS, tag, "invalid Cranelift relocation tag")
}

impl RelocTarget {
    fn encode(self, out: &mut impl Write) -> io::Result<()> {
        match self {
            Self::Object(index) => {
                write_u8(out, 0)?;
                write_u32(out, index)
            }
            Self::Entry(index) => {
                write_u8(out, 1)?;
                write_u32(out, index)
            }
            Self::CacheCell(index) => {
                write_u8(out, 4)?;
                write_u32(out, index)
            }
            Self::RuntimeSymbol(symbol) => {
                write_u8(out, 2)?;
                write_u32(out, symbol)
            }
            Self::SideMetadata(kind) => {
                write_u8(out, 3)?;
                write_u8(out, kind.to_tag())
            }
        }
    }

    fn decode(input: &mut impl Read) -> io::Result<Self> {
        match read_u8(input)? {
            0 => Ok(Self::Object(read_u32(input)?)),
            1 => Ok(Self::Entry(read_u32(input)?)),
            2 => Ok(Self::RuntimeSymbol(read_u32(input)?)),
            3 => Ok(Self::SideMetadata(SideMetadataSlot::from_tag(read_u8(
                input,
            )?)?)),
            4 => Ok(Self::CacheCell(read_u32(input)?)),
            _ => Err(invalid_data("invalid FASL relocation target")),
        }
    }
}

impl SideMetadataSlot {
    pub(crate) fn to_tag(self) -> u8 {
        match self {
            Self::Global => 0,
            Self::VoBit => 1,
        }
    }

    pub(crate) fn from_tag(tag: u8) -> io::Result<Self> {
        match tag {
            0 => Ok(Self::Global),
            1 => Ok(Self::VoBit),
            _ => Err(invalid_data("invalid FASL side metadata slot kind")),
        }
    }
}

pub(crate) fn asmkit_reloc_to_tag(reloc: AsmkitReloc) -> u8 {
    reloc_to_tag(
        ASMKIT_RELOCS,
        reloc,
        "asmkit relocation table is missing a variant",
    )
}

pub(crate) fn asmkit_reloc_from_tag(tag: u8) -> io::Result<AsmkitReloc> {
    reloc_from_tag(ASMKIT_RELOCS, tag, "invalid asmkit relocation tag")
}

fn reloc_to_tag<T: Copy + Eq>(relocs: &[T], reloc: T, missing: &'static str) -> u8 {
    relocs
        .iter()
        .position(|candidate| *candidate == reloc)
        .map(|index| index as u8)
        .expect(missing)
}

fn reloc_from_tag<T: Copy>(relocs: &[T], tag: u8, invalid: &'static str) -> io::Result<T> {
    relocs
        .get(tag as usize)
        .copied()
        .ok_or_else(|| invalid_data(invalid))
}

fn write_u8(out: &mut impl Write, value: u8) -> io::Result<()> {
    out.write_all(&[value])
}

fn write_u32(out: &mut impl Write, value: u32) -> io::Result<()> {
    out.write_all(&value.to_le_bytes())
}

fn write_i64(out: &mut impl Write, value: i64) -> io::Result<()> {
    out.write_all(&value.to_le_bytes())
}

fn read_u8(input: &mut impl Read) -> io::Result<u8> {
    let mut buf = [0; 1];
    input.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn read_u32(input: &mut impl Read) -> io::Result<u32> {
    let mut buf = [0; 4];
    input.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

fn read_i64(input: &mut impl Read) -> io::Result<i64> {
    let mut buf = [0; 8];
    input.read_exact(&mut buf)?;
    Ok(i64::from_le_bytes(buf))
}

fn invalid_data(message: &'static str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

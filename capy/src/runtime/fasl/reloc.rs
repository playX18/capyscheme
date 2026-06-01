use std::io::{self, Read, Write};

use asmkit::core::buffer::Reloc as AsmkitReloc;

use super::{
    FASL_RELOC_ABS_WORD, FASL_RELOC_ASMKIT, FASL_RELOC_CACHE_CELL, FASL_RELOC_CODE_ENTRY,
    FASL_RELOC_DATA_SLOT, FASL_RELOC_RUNTIME_DATA, FASL_RELOC_RUNTIME_THUNK,
    FASL_RELOC_SIDE_METADATA,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaslRelocation {
    pub offset: u32,
    pub kind: FaslRelocKind,
    pub target: FaslRelocTarget,
    pub addend: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FaslRelocKind {
    Asmkit(AsmkitReloc),
    AbsWord,
    CodeEntry,
    DataSlotAddress,
    RuntimeThunk,
    RuntimeData,
    SideMetadata,
    CacheCell,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FaslRelocTarget {
    Object(u32),
    Entry(u32),
    RuntimeSymbol(u32),
    SideMetadata(SideMetadataSlotKind),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SideMetadataSlotKind {
    Global,
    VoBit,
}

impl FaslRelocation {
    pub fn encode(&self, out: &mut impl Write) -> io::Result<()> {
        write_u32(out, self.offset)?;
        self.kind.encode(out)?;
        self.target.encode(out)?;
        write_i64(out, self.addend)
    }

    pub fn decode(input: &mut impl Read) -> io::Result<Self> {
        Ok(Self {
            offset: read_u32(input)?,
            kind: FaslRelocKind::decode(input)?,
            target: FaslRelocTarget::decode(input)?,
            addend: read_i64(input)?,
        })
    }
}

impl FaslRelocKind {
    fn encode(&self, out: &mut impl Write) -> io::Result<()> {
        match self {
            Self::Asmkit(reloc) => {
                write_u8(out, FASL_RELOC_ASMKIT)?;
                write_u8(out, asmkit_reloc_to_tag(*reloc))
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

impl FaslRelocTarget {
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
            3 => Ok(Self::SideMetadata(SideMetadataSlotKind::from_tag(
                read_u8(input)?,
            )?)),
            _ => Err(invalid_data("invalid FASL relocation target")),
        }
    }
}

impl SideMetadataSlotKind {
    fn to_tag(self) -> u8 {
        match self {
            Self::Global => 0,
            Self::VoBit => 1,
        }
    }

    fn from_tag(tag: u8) -> io::Result<Self> {
        match tag {
            0 => Ok(Self::Global),
            1 => Ok(Self::VoBit),
            _ => Err(invalid_data("invalid FASL side metadata slot kind")),
        }
    }
}

fn asmkit_reloc_to_tag(reloc: AsmkitReloc) -> u8 {
    match reloc {
        AsmkitReloc::Abs4 => 0,
        AsmkitReloc::Abs8 => 1,
        AsmkitReloc::X86PCRel4 => 2,
        AsmkitReloc::X86CallPCRel4 => 3,
        AsmkitReloc::X86CallPLTRel4 => 4,
        AsmkitReloc::X86GOTPCRel4 => 5,
        AsmkitReloc::X86SecRel => 6,
        AsmkitReloc::Arm32Call => 7,
        AsmkitReloc::Arm64Call => 8,
        AsmkitReloc::ElfX86_64TlsGd => 9,
        AsmkitReloc::MachOX86_64Tlv => 10,
        AsmkitReloc::MachOAarch64TlsAdrPage21 => 11,
        AsmkitReloc::MachOAarch64TlsAdrPageOff12 => 12,
        AsmkitReloc::Aarch64TlsDescAdrPage21 => 13,
        AsmkitReloc::Aarch64TlsDescLd64Lo12 => 14,
        AsmkitReloc::Aarch64TlsDescAddLo12 => 15,
        AsmkitReloc::Aarch64TlsDescCall => 16,
        AsmkitReloc::Aarch64AdrGotPage21 => 17,
        AsmkitReloc::Aarch64Ld64GotLo12Nc => 18,
        AsmkitReloc::Aarch64AdrPrelPgHi21 => 19,
        AsmkitReloc::Aarch64AddAbsLo12Nc => 20,
        AsmkitReloc::RiscvAbs8 => 21,
        AsmkitReloc::RiscvCallPlt => 22,
        AsmkitReloc::RiscvTlsGdHi20 => 23,
        AsmkitReloc::RiscvPCRelLo12I => 24,
        AsmkitReloc::RiscvGotHi20 => 25,
    }
}

fn asmkit_reloc_from_tag(tag: u8) -> io::Result<AsmkitReloc> {
    match tag {
        0 => Ok(AsmkitReloc::Abs4),
        1 => Ok(AsmkitReloc::Abs8),
        2 => Ok(AsmkitReloc::X86PCRel4),
        3 => Ok(AsmkitReloc::X86CallPCRel4),
        4 => Ok(AsmkitReloc::X86CallPLTRel4),
        5 => Ok(AsmkitReloc::X86GOTPCRel4),
        6 => Ok(AsmkitReloc::X86SecRel),
        7 => Ok(AsmkitReloc::Arm32Call),
        8 => Ok(AsmkitReloc::Arm64Call),
        9 => Ok(AsmkitReloc::ElfX86_64TlsGd),
        10 => Ok(AsmkitReloc::MachOX86_64Tlv),
        11 => Ok(AsmkitReloc::MachOAarch64TlsAdrPage21),
        12 => Ok(AsmkitReloc::MachOAarch64TlsAdrPageOff12),
        13 => Ok(AsmkitReloc::Aarch64TlsDescAdrPage21),
        14 => Ok(AsmkitReloc::Aarch64TlsDescLd64Lo12),
        15 => Ok(AsmkitReloc::Aarch64TlsDescAddLo12),
        16 => Ok(AsmkitReloc::Aarch64TlsDescCall),
        17 => Ok(AsmkitReloc::Aarch64AdrGotPage21),
        18 => Ok(AsmkitReloc::Aarch64Ld64GotLo12Nc),
        19 => Ok(AsmkitReloc::Aarch64AdrPrelPgHi21),
        20 => Ok(AsmkitReloc::Aarch64AddAbsLo12Nc),
        21 => Ok(AsmkitReloc::RiscvAbs8),
        22 => Ok(AsmkitReloc::RiscvCallPlt),
        23 => Ok(AsmkitReloc::RiscvTlsGdHi20),
        24 => Ok(AsmkitReloc::RiscvPCRelLo12I),
        25 => Ok(AsmkitReloc::RiscvGotHi20),
        _ => Err(invalid_data("invalid asmkit relocation tag")),
    }
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

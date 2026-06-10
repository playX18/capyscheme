use std::io;

use asmkit::core::buffer::Reloc as AsmkitReloc;
use cranelift_codegen::binemit::Reloc as CraneliftReloc;
use mmtk::util::metadata::side_metadata::{
    global_side_metadata_vm_base_address, vo_bit_side_metadata_addr,
};

use crate::rsgc::mmtk::util::Address;

use super::reloc::{self, RelocKind};

pub(crate) fn side_metadata_address(kind: reloc::SideMetadataSlot) -> usize {
    match kind {
        reloc::SideMetadataSlot::Global => global_side_metadata_vm_base_address().as_usize(),
        reloc::SideMetadataSlot::VoBit => vo_bit_side_metadata_addr().as_usize(),
    }
}

pub(crate) fn relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: &RelocKind,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    match kind {
        RelocKind::Asmkit(asmkit) => {
            asmkit_relocation_patch_bytes(base, offset, *asmkit, target_address, addend)
        }
        RelocKind::Cranelift(cranelift) => cranelift_relocation_patch_bytes(
            base,
            offset,
            *cranelift,
            target_address,
            addend,
            original,
        ),
        RelocKind::CraneliftDataSlot(cranelift) => cranelift_relocation_patch_bytes(
            base,
            offset,
            *cranelift,
            target_address,
            addend,
            original,
        ),
        RelocKind::AbsWord
        | RelocKind::CodeEntry
        | RelocKind::DataSlotAddress
        | RelocKind::CacheCell
        | RelocKind::RuntimeThunk
        | RelocKind::RuntimeData
        | RelocKind::SideMetadata => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
    }
}

pub(crate) fn relocation_original_bytes<'a>(
    code: &'a [u8],
    offset: usize,
    kind: &RelocKind,
) -> io::Result<&'a [u8]> {
    let width = relocation_patch_width(kind);
    code.get(offset..)
        .and_then(|bytes| bytes.get(..width))
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL relocation is out of bounds",
            )
        })
}

fn relocation_patch_width(kind: &RelocKind) -> usize {
    match kind {
        RelocKind::Cranelift(CraneliftReloc::RiscvCallPlt)
        | RelocKind::CraneliftDataSlot(CraneliftReloc::RiscvCallPlt) => 8,
        RelocKind::Cranelift(
            CraneliftReloc::Abs4
            | CraneliftReloc::X86PCRel4
            | CraneliftReloc::X86CallPCRel4
            | CraneliftReloc::X86CallPLTRel4
            | CraneliftReloc::Arm64Call
            | CraneliftReloc::Aarch64AdrGotPage21
            | CraneliftReloc::Aarch64AdrPrelPgHi21
            | CraneliftReloc::Aarch64AddAbsLo12Nc
            | CraneliftReloc::Aarch64Ld64GotLo12Nc
            | CraneliftReloc::MachOAarch64TlsAdrPage21
            | CraneliftReloc::MachOAarch64TlsAdrPageOff12
            | CraneliftReloc::Aarch64TlsDescAdrPage21
            | CraneliftReloc::Aarch64TlsDescLd64Lo12
            | CraneliftReloc::Aarch64TlsDescAddLo12
            | CraneliftReloc::RiscvTlsGdHi20
            | CraneliftReloc::RiscvPCRelLo12I
            | CraneliftReloc::RiscvGotHi20
            | CraneliftReloc::RiscvPCRelHi20,
        )
        | RelocKind::CraneliftDataSlot(
            CraneliftReloc::Abs4
            | CraneliftReloc::X86PCRel4
            | CraneliftReloc::X86CallPCRel4
            | CraneliftReloc::X86CallPLTRel4
            | CraneliftReloc::Arm64Call
            | CraneliftReloc::Aarch64AdrGotPage21
            | CraneliftReloc::Aarch64AdrPrelPgHi21
            | CraneliftReloc::Aarch64AddAbsLo12Nc
            | CraneliftReloc::Aarch64Ld64GotLo12Nc
            | CraneliftReloc::MachOAarch64TlsAdrPage21
            | CraneliftReloc::MachOAarch64TlsAdrPageOff12
            | CraneliftReloc::Aarch64TlsDescAdrPage21
            | CraneliftReloc::Aarch64TlsDescLd64Lo12
            | CraneliftReloc::Aarch64TlsDescAddLo12
            | CraneliftReloc::RiscvTlsGdHi20
            | CraneliftReloc::RiscvPCRelLo12I
            | CraneliftReloc::RiscvGotHi20
            | CraneliftReloc::RiscvPCRelHi20,
        ) => 4,
        RelocKind::Asmkit(
            AsmkitReloc::Abs4
            | AsmkitReloc::X86PCRel4
            | AsmkitReloc::X86CallPCRel4
            | AsmkitReloc::X86CallPLTRel4,
        ) => 4,
        RelocKind::Cranelift(CraneliftReloc::Aarch64TlsDescCall)
        | RelocKind::CraneliftDataSlot(CraneliftReloc::Aarch64TlsDescCall) => 0,
        _ => std::mem::size_of::<usize>(),
    }
}

fn cranelift_relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: CraneliftReloc,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    match kind {
        CraneliftReloc::Abs8 => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
        CraneliftReloc::Abs4 => {
            let target = add_i64_to_usize(target_address, addend)?;
            let target = u32::try_from(target).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL Cranelift Abs4 relocation target is out of range",
                )
            })?;
            Ok(target.to_le_bytes().to_vec())
        }
        CraneliftReloc::X86PCRel4
        | CraneliftReloc::X86CallPCRel4
        | CraneliftReloc::X86CallPLTRel4 => pcrel32_patch(base, offset, target_address, addend),
        CraneliftReloc::Arm64Call => {
            arm64_call_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::Aarch64AdrGotPage21
        | CraneliftReloc::Aarch64AdrPrelPgHi21
        | CraneliftReloc::MachOAarch64TlsAdrPage21
        | CraneliftReloc::Aarch64TlsDescAdrPage21 => {
            aarch64_page21_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::Aarch64AddAbsLo12Nc
        | CraneliftReloc::Aarch64TlsDescAddLo12
        | CraneliftReloc::MachOAarch64TlsAdrPageOff12 => {
            aarch64_lo12_patch(target_address, addend, original, 0)
        }
        CraneliftReloc::Aarch64Ld64GotLo12Nc | CraneliftReloc::Aarch64TlsDescLd64Lo12 => {
            aarch64_lo12_patch(target_address, addend, original, 3)
        }
        CraneliftReloc::Aarch64TlsDescCall => Ok(Vec::new()),
        CraneliftReloc::RiscvCallPlt => {
            riscv_call_plt_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::RiscvTlsGdHi20
        | CraneliftReloc::RiscvGotHi20
        | CraneliftReloc::RiscvPCRelHi20 => {
            riscv_hi20_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::RiscvPCRelLo12I => {
            riscv_lo12_i_patch(base, offset, target_address, addend, original)
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL Cranelift relocation",
        )),
    }
}

fn asmkit_relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: AsmkitReloc,
    target_address: usize,
    addend: i64,
) -> io::Result<Vec<u8>> {
    match kind {
        AsmkitReloc::Abs8 | AsmkitReloc::RiscvAbs8 => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
        AsmkitReloc::Abs4 => {
            let target = add_i64_to_usize(target_address, addend)?;
            let target = u32::try_from(target).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL asmkit Abs4 relocation target is out of range",
                )
            })?;
            Ok(target.to_le_bytes().to_vec())
        }
        AsmkitReloc::X86PCRel4 | AsmkitReloc::X86CallPCRel4 | AsmkitReloc::X86CallPLTRel4 => {
            pcrel32_patch(base, offset, target_address, addend)
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL asmkit relocation",
        )),
    }
}

fn pcrel32_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let displacement = target as i128 - patch_address as i128;
    let displacement = i32::try_from(displacement).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL rel32 relocation target is out of range",
        )
    })?;
    Ok(displacement.to_le_bytes().to_vec())
}

fn arm64_call_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let displacement = target as i128 - patch_address as i128;
    if displacement % 4 != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call target is not instruction aligned",
        ));
    }
    let immediate = displacement / 4;
    if !(-(1i128 << 25)..(1i128 << 25)).contains(&immediate) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call relocation target is out of range",
        ));
    }
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !0x03ff_ffff) | ((immediate as u32) & 0x03ff_ffff);
    Ok(patched.to_le_bytes().to_vec())
}

fn aarch64_page21_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 page relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let patch_page = (patch_address as i128) & !0xfffi128;
    let target_page = (target as i128) & !0xfffi128;
    let page_delta = (target_page - patch_page) >> 12;
    if !(-(1i128 << 20)..(1i128 << 20)).contains(&page_delta) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 page relocation target is out of range",
        ));
    }
    let imm = (page_delta as u32) & 0x1f_ffff;
    let immlo = imm & 0x3;
    let immhi = (imm >> 2) & 0x7ffff;
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !((0x3 << 29) | (0x7ffff << 5))) | (immlo << 29) | (immhi << 5);
    Ok(patched.to_le_bytes().to_vec())
}

fn aarch64_lo12_patch(
    target_address: usize,
    addend: i64,
    original: &[u8],
    scale_shift: u32,
) -> io::Result<Vec<u8>> {
    let target = add_i64_to_usize(target_address, addend)?;
    let low = (target & 0xfff) as u32;
    if scale_shift != 0 && (low & ((1 << scale_shift) - 1)) != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 low relocation target is not aligned",
        ));
    }
    let immediate = low >> scale_shift;
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !(0xfff << 10)) | (immediate << 10);
    Ok(patched.to_le_bytes().to_vec())
}

fn riscv_call_plt_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let hi = riscv_hi20(delta)?;
    let lo = delta - (hi << 12);
    let auipc = patch_riscv_u_type(read_original_u32(&original[0..4])?, hi);
    let jalr = patch_riscv_i_type(read_original_u32(&original[4..8])?, lo)?;
    let mut bytes = Vec::with_capacity(8);
    bytes.extend_from_slice(&auipc.to_le_bytes());
    bytes.extend_from_slice(&jalr.to_le_bytes());
    Ok(bytes)
}

fn riscv_hi20_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let instruction = patch_riscv_u_type(read_original_u32(original)?, riscv_hi20(delta)?);
    Ok(instruction.to_le_bytes().to_vec())
}

fn riscv_lo12_i_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let hi = riscv_hi20(delta)?;
    let lo = delta - (hi << 12);
    let instruction = patch_riscv_i_type(read_original_u32(original)?, lo)?;
    Ok(instruction.to_le_bytes().to_vec())
}

fn riscv_pcrel_delta(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
) -> io::Result<i128> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    Ok(target as i128 - patch_address as i128)
}

fn riscv_hi20(delta: i128) -> io::Result<i128> {
    let hi = (delta + 0x800) >> 12;
    if !(-(1i128 << 19)..(1i128 << 19)).contains(&hi) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V hi20 relocation target is out of range",
        ));
    }
    Ok(hi)
}

fn patch_riscv_u_type(instruction: u32, hi: i128) -> u32 {
    (instruction & 0x00000fff) | (((hi as u32) & 0x000f_ffff) << 12)
}

fn patch_riscv_i_type(instruction: u32, lo: i128) -> io::Result<u32> {
    if !(-2048..2048).contains(&lo) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V lo12 relocation target is out of range",
        ));
    }
    Ok((instruction & 0x000f_ffff) | (((lo as u32) & 0xfff) << 20))
}

fn read_original_u32(original: &[u8]) -> io::Result<u32> {
    let bytes: [u8; 4] = original
        .get(0..4)
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL relocation is out of bounds",
            )
        })?
        .try_into()
        .expect("slice length was checked");
    Ok(u32::from_le_bytes(bytes))
}

fn add_i64_to_usize(value: usize, addend: i64) -> io::Result<usize> {
    if addend >= 0 {
        let addend = usize::try_from(addend).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation addend is too large",
            )
        })?;
        value.checked_add(addend)
    } else {
        let addend = usize::try_from(addend.unsigned_abs()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation addend is too small",
            )
        })?;
        value.checked_sub(addend)
    }
    .ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL code block relocation addend overflow",
        )
    })
}

#[cfg(test)]
mod tests {
    use cranelift_codegen::binemit::Reloc;

    use super::*;

    #[test]
    fn cranelift_arm64_call_relocation_patches_branch_immediate() {
        // SAFETY: The usize was derived from a valid address by the caller
        let base = unsafe { Address::from_usize(0x1_0000) };
        let original = 0x9400_0000u32.to_le_bytes();

        let bytes = relocation_patch_bytes(
            base,
            0,
            &RelocKind::Cranelift(Reloc::Arm64Call),
            0x1_0040,
            0,
            &original,
        )
        .expect("patch arm64 call");

        assert_eq!(u32::from_le_bytes(bytes.try_into().unwrap()), 0x9400_0010);
    }

    #[test]
    fn cranelift_aarch64_page21_relocation_patches_adrp_immediate() {
        // SAFETY: The usize was derived from a valid address by the caller
        let base = unsafe { Address::from_usize(0x1_0000) };
        let original = 0x9000_0000u32.to_le_bytes();

        let bytes = relocation_patch_bytes(
            base,
            0,
            &RelocKind::Cranelift(Reloc::Aarch64AdrPrelPgHi21),
            0x1_3000,
            0,
            &original,
        )
        .expect("patch aarch64 adrp");

        assert_eq!(u32::from_le_bytes(bytes.try_into().unwrap()), 0xF000_0000);
    }

    #[test]
    fn cranelift_riscv_call_plt_relocation_patches_auipc_jalr_pair() {
        // SAFETY: The usize was derived from a valid address by the caller
        let base = unsafe { Address::from_usize(0x1_0000) };
        let mut original = Vec::new();
        original.extend_from_slice(&0x0000_0097u32.to_le_bytes());
        original.extend_from_slice(&0x0000_8080u32.to_le_bytes());

        let bytes = relocation_patch_bytes(
            base,
            0,
            &RelocKind::Cranelift(Reloc::RiscvCallPlt),
            0x1_1804,
            0,
            &original,
        )
        .expect("patch riscv call");

        assert_eq!(
            u32::from_le_bytes(bytes[0..4].try_into().unwrap()),
            0x0000_2097
        );
        assert_eq!(
            u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
            0x8040_8080
        );
    }
}

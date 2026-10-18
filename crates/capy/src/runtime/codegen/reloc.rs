//! Relocations: values only known once the image is loaded.
//!
//! Code that needs a load-time address emits a patchable 64-bit immediate
//! ([`masm_move_patchable`]) and records a [`Reloc`] against the label at
//! its start. The linker ([`Emission::link`](super::emission::Emission::link))
//! resolves each one and rewrites the immediate with [`patch_imm64`], which
//! checks that it still holds the placeholder.

use onlineasm::{Label, VReg, onlineasm};

use super::image::LoadedImage;
use super::masm::Masm;

/// The placeholder a patchable immediate starts as. AArch64: every
/// halfword nonzero, so that the assembler emits `movz` and three `movk`s.
#[cfg(target_arch = "aarch64")]
pub(crate) const PATCHABLE_IMM64: i64 = 0x1357_2468_9ace_bdf1;

/// The placeholder a patchable immediate starts as. x86-64: an unmapped
/// low address, loaded with a full 10-byte `movabs`.
#[cfg(not(target_arch = "aarch64"))]
pub(crate) const PATCHABLE_IMM64: i64 = 0x5eed_5eed_5eed_5eedu64 as i64;

/// An address or word that is patched after load.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Reloc {
    /// The patchable load at `at` becomes the address of label `target`.
    /// AArch64 only: there `adrp` of a label assumes the image starts a
    /// page, so label addresses are baked as immediates. x86-64 uses a
    /// RIP-relative `lea`.
    #[cfg_attr(not(target_arch = "aarch64"), allow(dead_code))]
    LabelAddr { at: Label, target: Label },
    /// Word `index` of the dense switch table at `table` becomes the
    /// address of the instruction at bytecode `pc`.
    TableWord {
        table: Label,
        index: usize,
        pc: usize,
    },
    /// The patchable load at `at` becomes the address of the pending lazy
    /// cell `pending` (an index into the emission's pending cells).
    Cell { at: Label, pending: usize },
    /// The word at `at` becomes the address of the owner's slot. Patched
    /// at publish: the owner does not exist before then.
    OwnerSlot { at: Label },
}

/// Emit a load of [`PATCHABLE_IMM64`] into `reg`, preceded by a label bound
/// at its first byte. A [`Reloc`] against that label rewrites it.
pub(crate) fn masm_move_patchable(masm: &mut Masm, reg: VReg) -> Label {
    let at = masm.label();
    masm.bind(at);
    onlineasm!(*masm.assembler(); move {reg}, {PATCHABLE_IMM64};);
    at
}

/// Rewrite the `movz`/`movk`×3 sequence at `at` (a [`masm_move_patchable`]
/// load) to load `value`.
#[cfg(target_arch = "aarch64")]
pub(crate) fn patch_imm64(image: &mut LoadedImage, at: usize, value: u64) -> std::io::Result<()> {
    // SAFETY: `at` is the offset of four instructions inside the image.
    let words: Vec<u32> = unsafe {
        let code = image.base().add(at).to_ptr::<u32>();
        (0..4)
            .map(|index| code.add(index).read_unaligned())
            .collect()
    };
    // `movz` then three `movk`s, one per halfword in some order: keep each
    // word's opcode and halfword, and put `value`'s halfword in it.
    let rd = words[0] & 0x1f;
    let mut seen = 0u32;
    let mut placeholder = 0u64;
    let mut bytes = Vec::with_capacity(16);
    for (index, word) in words.iter().enumerate() {
        let opcode = word & 0xff80_0000;
        let hw = (word >> 21) & 0b11;
        let expected = if index == 0 { 0xd280_0000 } else { 0xf280_0000 };
        if opcode != expected || word & 0x1f != rd || seen & (1 << hw) != 0 {
            return Err(std::io::Error::other(format!(
                "unexpected address placeholder {words:08x?}"
            )));
        }
        seen |= 1 << hw;
        placeholder |= (((word >> 5) & 0xffff) as u64) << (16 * hw);
        let imm16 = ((value >> (16 * hw)) & 0xffff) as u32;
        bytes.extend_from_slice(&(opcode | hw << 21 | imm16 << 5 | rd).to_le_bytes());
    }
    if placeholder != PATCHABLE_IMM64 as u64 {
        return Err(std::io::Error::other(
            "address placeholder holds no placeholder",
        ));
    }
    image.patch(at, &bytes)
}

/// Rewrite the `movabs` at `at` (a [`masm_move_patchable`] load) to load
/// `value`, verifying it still holds the placeholder.
#[cfg(not(target_arch = "aarch64"))]
pub(crate) fn patch_imm64(image: &mut LoadedImage, at: usize, value: u64) -> std::io::Result<()> {
    // SAFETY: `at` is the offset of one `movabs` (10 bytes) inside the
    // image, and executable memory stays readable while it is alive.
    let bytes: [u8; 10] = unsafe {
        core::slice::from_raw_parts(image.base().add(at).to_ptr::<u8>(), 10)
            .try_into()
            .expect("a movabs is 10 bytes")
    };
    // REX.W plus `B8+rd`: the register in the opcode, the address in the
    // 8 bytes after it.
    if bytes[0] & 0xf8 != 0x48 || bytes[1] & 0xf8 != 0xb8 {
        return Err(std::io::Error::other(format!(
            "unexpected site placeholder {bytes:02x?}"
        )));
    }
    if u64::from_le_bytes(bytes[2..10].try_into().unwrap()) != PATCHABLE_IMM64 as u64 {
        return Err(std::io::Error::other(
            "site placeholder holds no placeholder",
        ));
    }
    let mut patched = bytes;
    patched[2..10].copy_from_slice(&value.to_le_bytes());
    image.patch(at, &patched)
}

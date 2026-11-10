//! Relocations of data words that hold image-relative addresses.

use onlineasm::Label;

use super::image::LoadedImage;

/// A data word that is patched after load.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Reloc {
    /// Word `index` of the dense switch table at `table` becomes the
    /// address of the instruction at bytecode `pc`.
    TableWord {
        table: Label,
        index: usize,
        pc: usize,
    },
    /// Word `index` of the dense switch table at `table` becomes the address
    /// of the block at `target`.
    TableLabel {
        table: Label,
        index: usize,
        target: Label,
    },
}

/// What a resolved relocation points at
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Resolved {
    /// An offset into the image: resolves to the load base plus the offset.
    Image(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Record {
    pub(crate) at: usize,
    pub(crate) target: Resolved,
}

/// Write every record into `image`, resolving image offsets against its
/// base.
pub(crate) fn apply(image: &mut LoadedImage, records: &[Record]) -> std::io::Result<()> {
    for record in records {
        let Resolved::Image(offset) = record.target;
        image.patch_reloc(record.at, offset)?;
    }
    Ok(())
}

use std::io;

use onlineasm::{Label, PatchHandle, PatchLocation};

use super::super::image::{self, LoadedImage, UnloadedImage};
use super::super::masm::{Masm, SymbolKind};
use super::super::template_image::resolve_slow_path;

/// A masm whose code, from `start` to the end, is one named stub.
pub(crate) struct StubBuilder {
    masm: Masm,
    name: String,
    start: Label,
    kind: SymbolKind,
    cfa_offset: Option<u32>,
}

impl StubBuilder {
    /// A stub named `name`, starting at `start` (bound in `masm`) and
    /// running to the end of the code.
    pub(crate) fn new(masm: Masm, name: &str, start: Label, kind: SymbolKind) -> Self {
        Self {
            masm,
            name: name.to_string(),
            start,
            kind,
            cfa_offset: None,
        }
    }

    /// Describe the stub's frame to the unwinder: `CFA = sp + cfa_offset`
    /// over the whole stub. Stubs entered by a jump with no frame of their
    /// own (lazy pieces) leave it out.
    pub(crate) fn with_cfi(mut self, cfa_offset: u32) -> Self {
        self.cfa_offset = Some(cfa_offset);
        self
    }

    /// Finish the masm: bind the end, record the symbol and the CFI region.
    pub(crate) fn finish(mut self) -> io::Result<UnloadedStub> {
        let end = self.masm.label();
        self.masm.bind(end);
        if let Some(cfa_offset) = self.cfa_offset {
            self.masm.cfi_region(self.start, end, cfa_offset);
        }
        self.masm
            .define_symbol(&self.name, self.start, end, self.kind);
        let image = self.masm.finish().map_err(io::Error::other)?;
        Ok(UnloadedStub { image })
    }

    /// Finish and load into the runtime code memory: one
    /// stub, one image.
    pub(crate) fn load(
        masm: Masm,
        name: &str,
        start: Label,
        kind: SymbolKind,
    ) -> io::Result<LoadedImage> {
        Self::new(masm, name, start, kind).finish()?.load()
    }
}

/// A finished stub, not placed in memory yet.
pub(crate) struct UnloadedStub {
    image: UnloadedImage,
}

impl UnloadedStub {
    /// The image offset of a bound `label`.
    pub(crate) fn offset(&self, label: Label) -> usize {
        self.image.finished.offset(label).expect("bound label") as usize
    }

    /// The image offset of the word to rewrite for the patchable jump
    /// `jump` (a `#[patch]` `jmp`): the `b` on AArch64, the rel32 field on
    /// x86-64.
    pub(crate) fn jump_offset(&self, jump: PatchHandle) -> Option<usize> {
        match self.image.finished.location(jump)? {
            PatchLocation::Jump(jump) => Some(jump.offset() as usize),
            PatchLocation::Data(_) => None,
        }
    }

    /// Load into the runtime code memory.
    pub(crate) fn load(self) -> io::Result<LoadedImage> {
        image::load(self.image, |name| resolve_slow_path(name))
    }
}

use crate::rsgc::{Trace, Visitor, WeakProcessor};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClassCategory {
    Abstract,
    Builtin,
    Immediate,
    Internal,
    Scheme,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ClassFlags(u32);

impl ClassFlags {
    const APPLICABLE: u32 = 1 << 0;
    const MALLEABLE: u32 = 1 << 1;
    const AGGREGATE: u32 = 1 << 2;
    const SEALED: u32 = 1 << 3;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn builtin() -> Self {
        Self(Self::SEALED)
    }

    pub const fn internal() -> Self {
        Self(Self::SEALED)
    }

    pub const fn scheme() -> Self {
        Self(Self::MALLEABLE | Self::AGGREGATE)
    }

    pub const fn scheme_applicable() -> Self {
        Self(Self::MALLEABLE | Self::AGGREGATE | Self::APPLICABLE)
    }

    pub const fn applicable(self) -> bool {
        self.0 & Self::APPLICABLE != 0
    }

    pub const fn malleable(self) -> bool {
        self.0 & Self::MALLEABLE != 0
    }

    pub const fn aggregate(self) -> bool {
        self.0 & Self::AGGREGATE != 0
    }

    pub const fn sealed(self) -> bool {
        self.0 & Self::SEALED != 0
    }

    pub const fn with_malleable(self) -> Self {
        Self(self.0 | Self::MALLEABLE)
    }

    pub const fn without_malleable(self) -> Self {
        Self(self.0 & !Self::MALLEABLE)
    }

    pub const fn with_sealed(self) -> Self {
        Self(self.0 | Self::SEALED)
    }

    pub const fn without_sealed(self) -> Self {
        Self(self.0 & !Self::SEALED)
    }
}

unsafe impl Trace for ClassFlags {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

impl ClassCategory {
    pub(crate) fn default_flags(self) -> ClassFlags {
        match self {
            Self::Scheme => ClassFlags::scheme(),
            Self::Builtin => ClassFlags::builtin(),
            Self::Internal => ClassFlags::internal(),
            Self::Abstract | Self::Immediate => ClassFlags::empty(),
        }
    }
}

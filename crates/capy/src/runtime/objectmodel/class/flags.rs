use crate::heap::{Trace, Visitor, WeakProcessor};

/// The native layout a metaobject class's instances share.
///
/// Standard metaobjects have a fixed Rust struct, and user subclasses of one
/// reuse that struct. The family lets a value test recognize such instances
/// by their class's family rather than by a builtin id.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum LayoutFamily {
    /// Not a metaobject (ordinary instances, builtins).
    None = 0,
    /// `ClassDescriptor`.
    Class = 1,
    /// `GenericDescriptor`.
    Generic = 2,
    /// `MethodDescriptor`.
    Method = 3,
    /// Slot definition objects (`SlotDescriptor` and accessors).
    SlotDefinition = 4,
    /// `EqlSpecializer` objects.
    EqlSpecializer = 5,
    /// Method combination objects.
    MethodCombination = 6,
}

impl LayoutFamily {
    /// The family of a builtin class id, if it is a metaobject layout.
    pub(crate) const fn of_builtin_id(id: u32) -> Option<Self> {
        use crate::heap::object::builtin_class_ids;
        match id {
            builtin_class_ids::CLASS => Some(Self::Class),
            builtin_class_ids::GENERIC => Some(Self::Generic),
            builtin_class_ids::METHOD => Some(Self::Method),
            builtin_class_ids::SLOT_DEFINITION | builtin_class_ids::SLOT_ACCESSOR => {
                Some(Self::SlotDefinition)
            }
            _ => None,
        }
    }
}

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
    /// The class is a forward-reference placeholder, or has one among its
    /// superclasses: it has no instances until the reference is resolved.
    const UNFINALIZED: u32 = 1 << 4;
    /// The class is final: a class may not name it as a direct superclass
    const NO_SUBCLASSES: u32 = 1 << 5;

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

    pub const fn unfinalized(self) -> bool {
        self.0 & Self::UNFINALIZED != 0
    }

    pub const fn with_unfinalized(self, unfinalized: bool) -> Self {
        if unfinalized {
            Self(self.0 | Self::UNFINALIZED)
        } else {
            Self(self.0 & !Self::UNFINALIZED)
        }
    }

    /// Whether defining a subclass of this class is refused.
    pub const fn no_subclasses(self) -> bool {
        self.0 & Self::NO_SUBCLASSES != 0
    }

    pub const fn with_no_subclasses(self) -> Self {
        Self(self.0 | Self::NO_SUBCLASSES)
    }

    pub const fn without_no_subclasses(self) -> Self {
        Self(self.0 & !Self::NO_SUBCLASSES)
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

// SAFETY: GC trace for `ClassFlags`, all reachable heap fields are visited
unsafe impl Trace for ClassFlags {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
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

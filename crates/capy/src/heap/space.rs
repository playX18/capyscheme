//! Where a freshly allocated object lives.

/// An allocation space, chosen at the call site of
/// [`Mutation::allocate_in`](crate::heap::Mutation::allocate_in).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Space {
    /// Default nursery / immix space (movable).
    Default,
    /// Non-moving space (FFI interiors, pinned bytevectors).
    NonMoving,
    /// Large object space.
    Los,
    /// Never collected.
    Immortal,
}

impl Space {
    pub(crate) fn to_semantics(self) -> mmtk::AllocationSemantics {
        match self {
            Space::Default => mmtk::AllocationSemantics::Default,
            Space::NonMoving => mmtk::AllocationSemantics::NonMoving,
            Space::Los => mmtk::AllocationSemantics::Los,
            Space::Immortal => mmtk::AllocationSemantics::Immortal,
        }
    }
}

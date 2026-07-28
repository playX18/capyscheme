//! Allocation space selectors for [`crate::runtime::Context::alloc_in`].

/// Where to place a freshly allocated object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Space {
    /// Default nursery / immix space (movable).
    Default,
    /// Non-moving space (FFI interiors, pinned bytevectors).
    NonMoving,
    /// Large object space.
    Los,
}

impl Space {
    pub fn to_semantics(self) -> mmtk::AllocationSemantics {
        match self {
            Space::Default => mmtk::AllocationSemantics::Default,
            Space::NonMoving => mmtk::AllocationSemantics::NonMoving,
            Space::Los => mmtk::AllocationSemantics::Los,
        }
    }
}

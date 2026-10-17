//! Operand types of the bytecode.

use bytecodegen::Operand;

macro_rules! operand {
    ($(#[$doc:meta])* $name:ident($inner:ty)) => {
        $(#[$doc])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Operand)]
        pub struct $name(pub $inner);
    };
}

operand!(
    /// Frame slot `fp[n]`.
    Register(u32)
);
operand!(
    /// Constant pool index.
    Constant(u32)
);
operand!(
    /// Signed immediate.
    Immediate(i32)
);
operand!(
    /// Unsigned count, size, index or small enum.
    Count(u32)
);
operand!(
    /// `Primitive::id()`.
    PrimitiveId(u32)
);
operand!(
    /// Word index into the code block's aux pool.
    PoolReference(u32)
);

pub use bytecodegen::MetadataId;

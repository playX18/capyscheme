//! A helper module for allocations with trailing data

/// Declare a type with trailing data.
#[macro_export]
macro_rules! trailing_type {
    ($(#[$outer:meta])* $name: ident ($data: ty, $trailing: ty)) => {
        $(#[$outer])*
        #[repr(C)]
        pub struct $name {
            pub data: $data,
            pub trailing: $trailing
        }

        impl $name {
            pub const OFFSET_OF_TRAILING: usize = ::offset_of!(Self, trailing);
            pub const OFFSET_OF_DATA: usize = ::offset_of!(Self, data);


        }
    };
}

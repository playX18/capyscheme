use bitflags::bitflags;

pub mod num;
pub mod reader;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Directives: u16 {
        /// Disable stack-trace generation.
        ///
        /// This makes compiler omit `debug_trace` calls on each function call
        /// which results in no stack-trace information being available at runtime.
        const NOTRACE = 1 << 0;
        /// Enable R6RS compatibility mode.
        const R6RS = 1 << 1;
        /// Enable R7RS compatibility mode.
        const R7RS = 1 << 2;
        /// Enable case-insensitive identifiers.
        const FOLD_CASES = 1 << 3;

    }
}

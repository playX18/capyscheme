//! `Masm` (macro assembler): `onlineasm::Assembler` plus SABI helpers.

use onlineasm::{Assembler, Extern, ExternalName, Label, Target, VReg, onlineasm};

use super::image::UnloadedImage;
use crate::runtime::objectmodel::Value;
use crate::runtime::sabi::{self, RpCompactHeader, builtin_class_ids, regs};
use crate::utils::flags;

mod arith;
mod profile;
mod state;
mod value;

pub const EXTERN_NAMESPACE_RUNTIME: u32 = 1;
#[cfg(test)]
pub const EXTERN_NAMESPACE_TEST: u32 = 2;

pub const EXTERN_NAMESPACE_PRIM: u32 = 3;

/// Declares [`RuntimeExtern`]: one line per Rust helper generated code
/// calls, giving its variant, symbol and address
macro_rules! runtime_externs {
    ($($kind:ident { $($variant:ident => $($path:ident)::+),* $(,)? })*) => {
        /// A Rust helper generated code can call. The discriminant is the
        /// extern index within [`EXTERN_NAMESPACE_RUNTIME`].
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        #[repr(u32)]
        pub enum RuntimeExtern {
            $($($variant,)*)*
        }

        impl RuntimeExtern {
            pub const ALL: &'static [RuntimeExtern] = &[$($(RuntimeExtern::$variant,)*)*];
            pub const COUNT: usize = Self::ALL.len();

            /// The helper's symbol, for disassembly and the debugger.
            pub fn name(self) -> &'static str {
                match self {
                    $($(Self::$variant => runtime_externs!(@last $($path)::+),)*)*
                }
            }

            /// The helper's address, resolved at load.
            pub fn address(self) -> *const u8 {
                use super::{lazy, opt, slow, stubs};
                match self {
                    $($(Self::$variant => $($path)::+ as *const u8,)*)*
                }
            }
        }
    };
    (@last $head:ident $(:: $tail:ident)+) => { runtime_externs!(@last $($tail)::+) };
    (@last $name:ident) => { stringify!($name) };
}

runtime_externs! {
    control {
        HotEnter => slow::capy_hot_enter,
        YieldLoop => slow::capy_yield_loop,
        HotLoop => slow::capy_hot_loop,
        Overflow => slow::capy_overflow,
        Arity => slow::capy_arity,
        EnterCase => slow::capy_enter_case,
        CallSlow => slow::capy_call_slow,
        ReturnMv => slow::capy_return_mv,
        Raise => slow::capy_raise,
        RaiseKind => slow::capy_raise_kind,
        NumberArith => slow::capy_number_arith,
        CompareJump => slow::capy_compare_jump,
        VectorOp => slow::capy_vector_op,
        VectorLength => slow::capy_vector_length,
        PrimN => slow::capy_prim_n,
        PrimError => slow::capy_prim_error,
        GetSlot => slow::capy_get_slot,
        PutSlot => slow::capy_put_slot,
        RuntimeProc => slow::capy_runtime_proc,
        Nop => slow::capy_nop,
    }
    operation {
        Alloc => slow::capy_op_alloc,
        RestList => slow::capy_op_rest_list,
        ValuesRest => slow::capy_op_values_rest,
        LogObject => slow::capy_log_object,
        VariableSet => slow::capy_op_variable_set,
    }
    opt_helper {
        OptRuntimeProc => opt::glue::capy_opt_runtime_proc,
        OptSwitchSymbol => opt::glue::capy_opt_switch_symbol,
        OptGetSlot => opt::glue::capy_opt_get_slot,
        OptPutSlot => opt::glue::capy_opt_put_slot,
        OptCollectRest => opt::glue::capy_opt_collect_rest,
        OptRestList => opt::glue::capy_opt_rest_list,
        OptPrimN => opt::glue::capy_opt_prim_n,
        OptVariableSet => opt::glue::capy_opt_variable_set,
        OptArith => opt::glue::capy_opt_arith,
        OptLength => opt::glue::capy_opt_length,
        OptAppend => opt::glue::capy_opt_append,
        OptReduceRational128 => opt::glue::capy_opt_reduce_rational128,
        OptGenericSlotGet => opt::glue::capy_opt_generic_slot_get,
        OptGenericSlotPut => opt::glue::capy_opt_generic_slot_put,
        OptGenericMethod => opt::glue::capy_opt_generic_method,
        OptStringSet => opt::glue::capy_opt_string_set,
    }
    stub {
        Bail => stubs::capy_bail,
        LazyHit => lazy::capy_lazy_hit,
        IcMissCall => stubs::capy_ic_miss_call,
        IcMissTcall => stubs::capy_ic_miss_tcall,
        SubrCall => slow::capy_subr_call,
        SubrRet => slow::capy_subr_ret,
        Underflow => slow::capy_underflow,
        RaiseRet => slow::capy_raise_ret,
        Exit => slow::capy_exit,
        ValuesError => slow::capy_values_error,
        OptDeopt => opt::deopt::runtime::capy_opt_deopt,
        OptYield => opt::glue::capy_opt_yield,
    }
}

impl RuntimeExtern {
    /// The assembler key for this helper.
    pub fn external(self) -> ExternalName {
        ExternalName::user(EXTERN_NAMESPACE_RUNTIME, self as u32)
    }

    /// The helper an assembler key names, if it is a runtime helper.
    pub fn from_external(name: &ExternalName) -> Option<Self> {
        let ExternalName::User(key) = name else {
            return None;
        };
        if key.namespace != EXTERN_NAMESPACE_RUNTIME {
            return None;
        }
        Self::ALL.get(key.index as usize).copied()
    }
}

/// A numeric comparison. Fixnum fast paths compare as signed 32-bit
/// integers; flonum fast paths compare ordered doubles, so NaN compares
/// false under every variant.
#[derive(Clone, Copy)]
pub enum Cmp {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

/// What a recorded symbol names
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SymbolKind {
    Function,
    Handler,
    Glue,
    Stub,
}

/// A named address range, published to the debugger at load.
pub struct SymbolRecord {
    pub name: String,
    pub start: Label,
    pub end: Label,
    pub kind: SymbolKind,
}

/// One `CFA = sp + K` rule over an address range
pub struct CfiRecord {
    pub start: Label,
    pub end: Label,
    pub cfa_offset: u32,
}

/// Target of an `RpHeader`'s mv return address.
#[derive(Clone, Copy)]
pub enum HeaderTarget {
    Absolute(usize),
    Label(Label),
}

/// A `toplink` resolved at load: the distance from the header's `toplink`
/// word back to the start of the image, which is the code chunk's header.
pub const TOPLINK_TO_IMAGE_START: usize = usize::MAX;

/// The words of a return-point header, filled at load.
pub enum HeaderWords {
    /// `rp-header`: 32 bytes.
    Full {
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    },
    /// `rp-compact-header`: 16 bytes.
    Compact(RpCompactHeader),
}

/// A return-point header reserved before its return address.
pub struct PendingHeader {
    pub header: Label,
    pub ra: Label,
    pub words: HeaderWords,
}

pub struct Masm {
    asm: Assembler,
    symbols: Vec<SymbolRecord>,
    cfi: Vec<CfiRecord>,
    headers: Vec<PendingHeader>,
    externs: Vec<ExternalName>,
    /// 64-bit constants already embedded as data islands, keyed by bits.
    pool_words: std::collections::HashMap<u64, Label>,
}

impl Masm {
    /// Finalizes the emitted image
    pub fn finish(&mut self) -> Result<UnloadedImage, onlineasm::asmkit::AsmError> {
        let symbols = std::mem::take(&mut self.symbols);
        let cfi = std::mem::take(&mut self.cfi);
        let headers = std::mem::take(&mut self.headers);
        let result = self.asm.finish();
        // The buffer is reset either way; drop any per-image records a
        // failed build left behind so they cannot leak into the next image.
        self.externs.clear();
        self.pool_words.clear();
        let finished = result?;
        Ok(UnloadedImage {
            finished,
            symbols,
            cfi,
            headers,
            tables: Vec::new(),
        })
    }
}

impl Default for Masm {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn emit_tiny(masm: &mut Masm, name: &str) {
        let start = masm.label();
        masm.bind(start);
        masm.reserve_words(8);
        let end = masm.label();
        masm.bind(end);
        masm.define_symbol(name, start, end, SymbolKind::Function);
        masm.cfi_region(start, end, 8);
    }

    #[test]
    fn reused_masm_matches_fresh_masm() {
        let mut fresh = Masm::new();
        emit_tiny(&mut fresh, "fresh");
        let want = fresh
            .finish()
            .expect("fresh finish")
            .finished
            .data()
            .to_vec();

        let mut reused = Masm::new();
        emit_tiny(&mut reused, "fresh");
        let first = reused
            .finish()
            .expect("first finish")
            .finished
            .data()
            .to_vec();
        assert_eq!(first, want, "first image differs from a fresh assembler");
        assert!(reused.symbols.is_empty(), "side tables survive finish");

        emit_tiny(&mut reused, "fresh");
        let second = reused
            .finish()
            .expect("second finish")
            .finished
            .data()
            .to_vec();
        assert_eq!(second, want, "reused assembler image differs");
        assert!(reused.symbols.is_empty(), "side tables survive reuse");
    }

    #[test]
    fn reset_discards_a_partial_image() {
        let mut masm = Masm::new();
        emit_tiny(&mut masm, "stale");
        masm.reset();
        assert!(masm.symbols.is_empty(), "reset keeps symbols");
        emit_tiny(&mut masm, "fresh");
        let got = masm.finish().expect("finish").finished.data().to_vec();

        let mut fresh = Masm::new();
        emit_tiny(&mut fresh, "fresh");
        let want = fresh
            .finish()
            .expect("fresh finish")
            .finished
            .data()
            .to_vec();
        assert_eq!(got, want, "image after reset differs from fresh");
    }
}

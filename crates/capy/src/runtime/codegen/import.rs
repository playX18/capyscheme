//! The import table: every absolute value generated code needs but cannot
//! know when it is assembled.
//!
//! Code never holds such a value in an instruction. It names it with an
//! [`ImportKey`], [`Masm::import`](super::masm::Masm::import) reserves an
//! 8-byte slot for it in the image, and code loads the slot PC-relative. The
//! slots are filled once, before the image is published, by the resolver
//! `Emission::link` is given (and, for the few values that only exist at
//! install, by the driver afterwards). A key holds names and offsets, never
//! an address, so an image and its keys can be stored and loaded at another
//! base: only the resolver turns a key into a number.

use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

use super::jit::config::{OPT_ENTRIES, template_interpreter};
use super::template_image::interpreter_enter_if_loaded;

/// A named runtime address: stable across runs, resolved by name.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum RuntimeSym {
    /// The interpreter image's `values_error` stub.
    ValuesError,
    /// The interpreter image's `interp_enter_cp` entry.
    InterpreterEnter,
    /// The interpreter image's `interp_return` marker.
    InterpreterReturn,
    /// The interpreter image's opcode map.
    OpcodeMap,
    /// The table of primitive wrappers, indexed by primitive id.
    PrimWrappers,
    /// The Tier 2 entry counter.
    OptEntries,
    /// The fast-path allocation counter.
    FastAllocs,
    /// The collector's global side-metadata base (unlog bits).
    UnlogBase,
    /// The executed-opcode counter for `[width][byte]`.
    OpCount { width: u8, byte: u8 },
}

impl RuntimeSym {
    /// The symbol's stable name, for storing and diagnostics.
    pub fn name(self) -> String {
        match self {
            Self::ValuesError => "values_error".into(),
            Self::InterpreterEnter => "interp_enter_cp".into(),
            Self::InterpreterReturn => "interp_return".into(),
            Self::OpcodeMap => "opcode_map".into(),
            Self::PrimWrappers => "prim_wrappers".into(),
            Self::OptEntries => "opt_entries".into(),
            Self::FastAllocs => "fast_allocs".into(),
            Self::UnlogBase => "unlog_base".into(),
            Self::OpCount { width, byte } => format!("op_count.{width}.{byte}"),
        }
    }

    /// The symbol's address in this process.
    pub fn address(self) -> u64 {
        match self {
            Self::ValuesError => template_interpreter().values_error as u64,
            Self::InterpreterEnter => {
                interpreter_enter_if_loaded().expect("the interpreter image is loaded") as u64
            }
            Self::InterpreterReturn => template_interpreter().interpreter_return as u64,
            Self::OpcodeMap => template_interpreter().map as u64,
            Self::PrimWrappers => crate::runtime::prims::calls::PRIM_WRAPPERS.as_ptr() as u64,
            Self::OptEntries => std::ptr::addr_of!(OPT_ENTRIES) as u64,
            Self::FastAllocs => std::ptr::addr_of!(super::gc::FAST_ALLOCS) as u64,
            Self::UnlogBase => global_side_metadata_vm_base_address().as_usize() as u64,
            Self::OpCount { width, byte } => super::stubs::op_count_addr(width as usize, byte) as u64,
        }
    }
}

/// A region of a unit's code block, by offset from a section base. Resolved
/// against the block the unit was built for, so the unit relocates with its
/// block.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Section {
    /// The bytecode entry: instruction `ip`s are offsets from it, and
    /// constants sit below it.
    Bytecode,
    /// The metadata area.
    Metadata,
    /// The bytecode's 32-bit operand pool.
    Pool,
}

/// What an import slot holds.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ImportKey {
    /// The unit's heap owner (its `CodeImage`).
    Owner,
    /// A named runtime address.
    Runtime(RuntimeSym),
    /// The address of a section of a code block of the unit, plus an offset.
    /// The block is the unit's code block `code` (0 is the root; an inlined
    /// callee has its own).
    Block(u32, Section, i32),
    /// The entry of the baseline unit the optimized code falls back to.
    BaselineEntry,
    /// The unit's lazy cell with this index.
    Cell(u32),
    /// The unit's invalidation word.
    InvalidWord,
    /// The address of the instruction at this bytecode pc of the unit's
    /// origin image.
    Origin(u32),
    /// The `CallLinkInfo` of the one call site a standalone IC stub
    /// serves (the loader's caller supplies it).
    CallLink,
}

/// Resolves an import key to the word its slot holds, or `None` when the
/// value only exists later (the driver fills those slots after `link`).
pub type Resolve<'a> = &'a dyn Fn(&ImportKey) -> Option<u64>;

/// Where one import landed in a linked image.
#[derive(Clone, Copy, Debug)]
pub struct ImportSlot {
    pub key: ImportKey,
    /// Image offset of the 8-byte slot.
    pub at: usize,
}

/// The addresses of one code block's sections.
#[derive(Clone, Copy, Debug)]
pub(crate) struct BlockAddrs {
    pub(crate) bytecode: usize,
    pub(crate) metadata: usize,
    pub(crate) pool: usize,
}

/// A resolver for the keys that name code blocks (`blocks[code]`), the
/// unit's invalidation word and its baseline entry. Everything else (`Owner`)
/// is filled by the driver once it exists.
pub(crate) fn block_resolver(
    blocks: Vec<BlockAddrs>,
    invalid_word: Option<usize>,
    baseline_entry: Option<usize>,
) -> impl Fn(&ImportKey) -> Option<u64> {
    move |key| match *key {
        ImportKey::Block(code, section, off) => {
            let block = blocks.get(code as usize)?;
            let base = match section {
                Section::Bytecode => block.bytecode,
                Section::Metadata => block.metadata,
                Section::Pool => block.pool,
            };
            Some(base.wrapping_add_signed(off as isize) as u64)
        }
        ImportKey::InvalidWord => invalid_word.map(|word| word as u64),
        ImportKey::BaselineEntry => baseline_entry.map(|entry| entry as u64),
        _ => None,
    }
}

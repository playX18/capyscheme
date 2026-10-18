//! Typed addresses and offsets for generated code.

use std::ops::Add;
use std::sync::atomic::Ordering;

use atomic::Atomic;
use mmtk::util::Address;

/// A byte offset inside one loaded image, relative to its base.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
#[repr(transparent)]
pub struct CodeOffset(pub u32);

impl CodeOffset {
    /// The offset of `at` from `base`, if it is at or above `base` and the
    /// distance fits.
    pub fn between(base: Address, at: Address) -> Option<Self> {
        if at < base {
            return None;
        }
        u32::try_from(at.get_extent(base)).ok().map(Self)
    }

    pub const fn get(self) -> usize {
        self.0 as usize
    }
}

impl From<CodeOffset> for usize {
    fn from(offset: CodeOffset) -> usize {
        offset.get()
    }
}

impl Add<CodeOffset> for Address {
    type Output = Address;

    fn add(self, offset: CodeOffset) -> Address {
        Address::add(self, offset.get())
    }
}

/// A bytecode offset inside one procedure. Absolute bytecode addresses are
/// `Ip`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
#[repr(transparent)]
pub struct BytecodePc(pub u32);

impl BytecodePc {
    pub fn new(pc: usize) -> Option<Self> {
        u32::try_from(pc).ok().map(Self)
    }

    pub const fn get(self) -> usize {
        self.0 as usize
    }
}

/// What a [`CodeEntry`] holds.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CodeState {
    /// Not compiled (yet).
    None,
    /// Never to be compiled.
    Poisoned,
    /// Compiled: the machine address a call enters at.
    Entry(Address),
}

/// The entry word of a procedure's compiled code: one machine word, so the
/// generated code loads it directly. Zero is [`CodeState::None`] and the
/// all-ones word is [`CodeState::Poisoned`].
#[repr(transparent)]
pub struct CodeEntry(Atomic<Address>);

impl CodeEntry {
    pub const fn new() -> Self {
        Self(Atomic::new(Address::ZERO))
    }

    pub fn load(&self, order: Ordering) -> CodeState {
        match self.0.load(order) {
            Address::ZERO => CodeState::None,
            Address::MAX => CodeState::Poisoned,
            entry => CodeState::Entry(entry),
        }
    }

    /// Whether the word is not [`CodeState::None`]: the block has code, or
    /// is marked never to get any.
    pub fn is_decided(&self, order: Ordering) -> bool {
        self.load(order) != CodeState::None
    }

    /// The entry address, if the procedure has code.
    pub fn entry(&self, order: Ordering) -> Option<Address> {
        match self.load(order) {
            CodeState::Entry(entry) => Some(entry),
            _ => None,
        }
    }

    /// Publish `entry`. Release, so what the code needs is visible to a
    /// reader that sees the entry.
    pub fn publish(&self, entry: Address) {
        debug_assert!(entry != Address::ZERO && entry != Address::MAX);
        self.0.store(entry, Ordering::Release);
    }

    /// Mark the procedure never to compile, unless it already has a state.
    pub fn poison(&self) {
        let _ = self.0.compare_exchange(
            Address::ZERO,
            Address::MAX,
            Ordering::AcqRel,
            Ordering::Relaxed,
        );
    }
}

impl Default for CodeEntry {
    fn default() -> Self {
        Self::new()
    }
}

/// An address as an assembler immediate.
pub fn imm(address: Address) -> i64 {
    address.as_usize() as i64
}

/// An atomic [`Address`] that starts out [`Address::ZERO`] (none).
#[repr(transparent)]
pub struct AtomicAddress(Atomic<Address>);

impl Default for AtomicAddress {
    fn default() -> Self {
        Self(Atomic::new(Address::ZERO))
    }
}

impl AtomicAddress {
    pub fn new(addr: Address) -> Self {
        Self(Atomic::new(addr))
    }

    pub fn load(&self, order: Ordering) -> Address {
        self.0.load(order)
    }

    pub fn store(&self, addr: Address, order: Ordering) {
        self.0.store(addr, order);
    }
}

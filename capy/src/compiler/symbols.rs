use std::marker::PhantomData;

use cranelift_codegen::ir::UserExternalName;

const FUNCTION_SYMBOL_NAMESPACE: u32 = 0;
const DATA_SYMBOL_NAMESPACE_BASE: u32 = 1;
const IMPORTED_SYMBOL_NAMESPACE_BASE: u32 = 100;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId<const KIND: u8> {
    index: u32,
    kind: PhantomData<fn() -> u8>,
}

impl<const KIND: u8> SymbolId<KIND> {
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            kind: PhantomData,
        }
    }

    pub const fn index(self) -> u32 {
        self.index
    }
}

pub type FunctionSymbol = SymbolId<0>;
pub type DataSymbol = SymbolId<1>;
pub type ImportedSymbol = SymbolId<2>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum DataKind {
    Constant = 0,
    CacheCell = 1,
    CodeBlock = 2,
    RuntimeData = 3,
    SideMetadata = 4,
    RootTable = 5,
    FaslBlob = 6,
    PointerSlot = 7,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum ImportKind {
    RuntimeThunk = 0,
    Trampoline = 1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    Function(FunctionSymbol),
    Data {
        kind: DataKind,
        symbol: DataSymbol,
    },
    Imported {
        kind: ImportKind,
        symbol: ImportedSymbol,
    },
}

impl Symbol {
    pub const fn function(symbol: FunctionSymbol) -> Self {
        Self::Function(symbol)
    }

    pub const fn data(kind: DataKind, symbol: DataSymbol) -> Self {
        Self::Data { kind, symbol }
    }

    pub const fn imported(kind: ImportKind, symbol: ImportedSymbol) -> Self {
        Self::Imported { kind, symbol }
    }

    pub fn to_external_name(self) -> UserExternalName {
        match self {
            Self::Function(symbol) => {
                UserExternalName::new(FUNCTION_SYMBOL_NAMESPACE, symbol.index())
            }
            Self::Data { kind, symbol } => UserExternalName::new(kind.namespace(), symbol.index()),
            Self::Imported { kind, symbol } => {
                UserExternalName::new(kind.namespace(), symbol.index())
            }
        }
    }

    pub const fn from_external_name(name: UserExternalName) -> Option<Self> {
        match name.namespace {
            FUNCTION_SYMBOL_NAMESPACE => Some(Self::Function(FunctionSymbol::new(name.index))),
            _ => match DataKind::from_namespace(name.namespace) {
                Some(kind) => Some(Self::Data {
                    kind,
                    symbol: DataSymbol::new(name.index),
                }),
                None => match ImportKind::from_namespace(name.namespace) {
                    Some(kind) => Some(Self::Imported {
                        kind,
                        symbol: ImportedSymbol::new(name.index),
                    }),
                    None => None,
                },
            },
        }
    }
}

impl DataKind {
    pub const fn namespace(self) -> u32 {
        DATA_SYMBOL_NAMESPACE_BASE + self as u32
    }

    pub const fn from_namespace(namespace: u32) -> Option<Self> {
        match namespace {
            1 => Some(Self::Constant),
            2 => Some(Self::CacheCell),
            3 => Some(Self::CodeBlock),
            4 => Some(Self::RuntimeData),
            5 => Some(Self::SideMetadata),
            6 => Some(Self::RootTable),
            7 => Some(Self::FaslBlob),
            8 => Some(Self::PointerSlot),
            _ => None,
        }
    }
}

impl ImportKind {
    pub const fn namespace(self) -> u32 {
        IMPORTED_SYMBOL_NAMESPACE_BASE + self as u32
    }

    pub const fn from_namespace(namespace: u32) -> Option<Self> {
        match namespace {
            100 => Some(Self::RuntimeThunk),
            101 => Some(Self::Trampoline),
            _ => None,
        }
    }
}

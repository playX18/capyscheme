use crate::{
    rsgc::{mmtk::util::Address, object::HeapTypeInfo},
    runtime::vm::thunks::{
        CLOSURE_K_INFO_STATIC, CLOSURE_PROC_INFO_STATIC, IMMUTABLE_VECTOR_INFO_STATIC,
        MUTABLE_VECTOR_INFO_STATIC, PAIR_INFO_STATIC, TUPLE_INFO_STATIC, VARIABLE_INFO_STATIC,
    },
};

pub use crate::runtime::vm::thunks::RuntimeThunk;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum RuntimeData {
    PairInfo,
    VariableInfo,
    ClosureProcInfo,
    ClosureKInfo,
    MutableVectorInfo,
    ImmutableVectorInfo,
    TupleInfo,
}

impl RuntimeData {
    pub const ALL: &'static [Self] = &[
        Self::PairInfo,
        Self::VariableInfo,
        Self::ClosureProcInfo,
        Self::ClosureKInfo,
        Self::MutableVectorInfo,
        Self::ImmutableVectorInfo,
        Self::TupleInfo,
    ];

    pub fn id(self) -> u32 {
        self as u32
    }

    pub fn from_id(id: u32) -> Option<Self> {
        match id {
            x if x == Self::PairInfo as u32 => Some(Self::PairInfo),
            x if x == Self::VariableInfo as u32 => Some(Self::VariableInfo),
            x if x == Self::ClosureProcInfo as u32 => Some(Self::ClosureProcInfo),
            x if x == Self::ClosureKInfo as u32 => Some(Self::ClosureKInfo),
            x if x == Self::MutableVectorInfo as u32 => Some(Self::MutableVectorInfo),
            x if x == Self::ImmutableVectorInfo as u32 => Some(Self::ImmutableVectorInfo),
            x if x == Self::TupleInfo as u32 => Some(Self::TupleInfo),
            _ => None,
        }
    }

    pub fn address(self) -> Address {
        match self {
            Self::PairInfo => static_pointer_address(&PAIR_INFO_STATIC),
            Self::VariableInfo => static_pointer_address(&VARIABLE_INFO_STATIC),
            Self::ClosureProcInfo => static_pointer_address(&CLOSURE_PROC_INFO_STATIC),
            Self::ClosureKInfo => static_pointer_address(&CLOSURE_K_INFO_STATIC),
            Self::MutableVectorInfo => static_pointer_address(&MUTABLE_VECTOR_INFO_STATIC),
            Self::ImmutableVectorInfo => static_pointer_address(&IMMUTABLE_VECTOR_INFO_STATIC),
            Self::TupleInfo => static_pointer_address(&TUPLE_INFO_STATIC),
        }
    }
}

fn static_pointer_address(symbol: &'static &'static HeapTypeInfo) -> Address {
    Address::from_ptr(symbol as *const &'static HeapTypeInfo)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_thunk_ids_roundtrip_and_resolve_to_addresses() {
        for thunk in RuntimeThunk::ALL {
            assert_eq!(RuntimeThunk::from_id(thunk.id()), Some(*thunk));
            assert!(!thunk.address().is_zero());
        }

        assert_eq!(RuntimeThunk::from_id(u32::MAX), None);
    }

    #[test]
    fn runtime_data_ids_roundtrip_and_resolve_to_addresses() {
        for data in RuntimeData::ALL {
            assert_eq!(RuntimeData::from_id(data.id()), Some(*data));
            assert!(!data.address().is_zero());
        }

        assert_eq!(RuntimeData::from_id(u32::MAX), None);
    }
}

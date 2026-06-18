use crate::rsgc::{
    mmtk::util::Address,
    object::{
        ClassId, builtin_class_ids, class_header_word, class_header_word_with_private_variant_flag,
    },
};
use std::sync::atomic::{AtomicU64, Ordering};

pub use crate::runtime::vm::thunks::RuntimeThunk;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum RuntimeData {
    PairHeaderWord = 7,
    ClosureProcHeaderWord = 8,
    ClosureKHeaderWord = 9,
    MutableVectorHeaderWord = 10,
}

static PAIR_HEADER_WORD: AtomicU64 = AtomicU64::new(0);
static CLOSURE_PROC_HEADER_WORD: AtomicU64 = AtomicU64::new(0);
static CLOSURE_K_HEADER_WORD: AtomicU64 = AtomicU64::new(0);
static MUTABLE_VECTOR_HEADER_WORD: AtomicU64 = AtomicU64::new(0);

impl RuntimeData {
    pub const ALL: &'static [Self] = &[
        Self::PairHeaderWord,
        Self::ClosureProcHeaderWord,
        Self::ClosureKHeaderWord,
        Self::MutableVectorHeaderWord,
    ];

    pub fn id(self) -> u32 {
        self as u32
    }

    pub fn from_id(id: u32) -> Option<Self> {
        match id {
            x if x == Self::PairHeaderWord as u32 => Some(Self::PairHeaderWord),
            x if x == Self::ClosureProcHeaderWord as u32 => Some(Self::ClosureProcHeaderWord),
            x if x == Self::ClosureKHeaderWord as u32 => Some(Self::ClosureKHeaderWord),
            x if x == Self::MutableVectorHeaderWord as u32 => Some(Self::MutableVectorHeaderWord),
            _ => None,
        }
    }

    pub fn address(self) -> Address {
        match self {
            Self::PairHeaderWord => static_class_header_word_address(
                &PAIR_HEADER_WORD,
                class_header_word(ClassId::new(builtin_class_ids::PAIR).unwrap()),
            ),
            Self::ClosureProcHeaderWord => static_class_header_word_address(
                &CLOSURE_PROC_HEADER_WORD,
                class_header_word(ClassId::new(builtin_class_ids::CLOSURE).unwrap()),
            ),
            Self::ClosureKHeaderWord => static_class_header_word_address(
                &CLOSURE_K_HEADER_WORD,
                class_header_word_with_private_variant_flag(
                    ClassId::new(builtin_class_ids::CLOSURE).unwrap(),
                ),
            ),
            Self::MutableVectorHeaderWord => static_class_header_word_address(
                &MUTABLE_VECTOR_HEADER_WORD,
                class_header_word(ClassId::new(builtin_class_ids::VECTOR).unwrap()),
            ),
        }
    }
}

fn static_class_header_word_address(
    cell: &'static AtomicU64,
    header_word: u64,
) -> Address {
    publish_static_header_word(cell, header_word)
}

fn publish_static_header_word(cell: &'static AtomicU64, header_word: u64) -> Address {
    let existing = cell.load(Ordering::Acquire);
    if existing == 0 {
        cell.store(header_word, Ordering::Release);
    } else {
        debug_assert_eq!(existing, header_word);
    }
    Address::from_ptr(cell as *const AtomicU64 as *const u64)
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

    #[test]
    fn allocation_header_word_runtime_data_points_to_header_words() {
        let cases = [
            (
                RuntimeData::PairHeaderWord,
                class_header_word(ClassId::new(builtin_class_ids::PAIR).unwrap()),
            ),
            (
                RuntimeData::ClosureProcHeaderWord,
                class_header_word(ClassId::new(builtin_class_ids::CLOSURE).unwrap()),
            ),
            (
                RuntimeData::ClosureKHeaderWord,
                class_header_word_with_private_variant_flag(
                    ClassId::new(builtin_class_ids::CLOSURE).unwrap(),
                ),
            ),
            (
                RuntimeData::MutableVectorHeaderWord,
                class_header_word(ClassId::new(builtin_class_ids::VECTOR).unwrap()),
            ),
        ];

        for (data, expected_header_word) in cases {
            // SAFETY: Preconditions verified by the surrounding code
            let header_word = unsafe { data.address().load::<u64>() };

            assert_eq!(header_word, expected_header_word);
        }
    }
}

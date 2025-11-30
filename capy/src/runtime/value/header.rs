use crate::rsgc::Trace;
use easy_bitfield::{BitField, BitFieldTrait};

pub type TypeBits = BitField<u64, u16, 0, 16, false>;

#[derive(Trace, Debug, Clone, Copy)]
#[collect(no_drop)]
#[repr(C, align(8))]
pub struct ScmHeader {
    pub word: u64,
}

impl ScmHeader {
    #[inline(always)]
    pub const fn new() -> Self {
        ScmHeader { word: 0 }
    }

    pub fn with_type_bits(bits: u16) -> Self {
        ScmHeader {
            word: TypeBits::encode(bits),
        }
    }

    #[inline(always)]
    pub fn set_type_bits(&mut self, bits: u16) {
        self.word = TypeBits::update(bits, self.word);
    }

    pub fn type_bits(&self) -> u16 {
        TypeBits::decode(self.word)
    }
}

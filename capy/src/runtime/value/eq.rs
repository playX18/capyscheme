use super::*;

impl<'gc> Value<'gc> {
    pub fn eq(self, other: Self) -> bool {
        self.raw_i64() == other.raw_i64()
    }

    pub fn eqv(self, other: Self) -> bool {
        if self.raw_i64() == other.raw_i64() {
            return true;
        }

        match (self.number(), other.number()) {
            (Some(a), Some(b)) => {
                if a.is_exact() && b.is_exact() {
                    return Number::exact_equal(a, b);
                } else {
                    if b.is_exact() {
                        return false;
                    }

                    return Number::exact_equal(a, b);
                }
            }

            _ => false,
        }
    }

    pub fn r5rs_equal(self, other: Self) -> bool {
        if self.raw_i64() == other.raw_i64() {
            return true;
        }

        if self.is_pair() {
            if other.is_pair() {
                if self.car().r5rs_equal(other.car()) {
                    return self.cdr().r5rs_equal(other.cdr());
                } else {
                    return false;
                }
            }
        }

        if self.is::<Vector>() && other.is::<Vector>() {
            let a = self.downcast::<Vector>();
            let b = other.downcast::<Vector>();

            if a.len() != b.len() {
                return false;
            }

            for i in 0..a.len() {
                if !a[i].get().r5rs_equal(b[i].get()) {
                    return false;
                }
            }

            return true;
        }

        if self.is::<ByteVector>() && other.is::<ByteVector>() {
            let a = self.downcast::<ByteVector>();
            let b = other.downcast::<ByteVector>();

            if a.len() != b.len() {
                return false;
            }

            for i in 0..a.len() {
                if a[i] != b[i] {
                    return false;
                }
            }

            return true;
        }

        if self.is::<Str>() && other.is::<Str>() {
            return self.downcast::<Str>().eq(&other.downcast::<Str>());
        }
        if self.is::<Boxed>() && other.is::<Boxed>() {
            return self
                .downcast::<Boxed>()
                .val
                .r5rs_equal(other.downcast::<Boxed>().val);
        }

        false
    }

    pub fn hash_equal(self) -> u64 {
        let mut hash = simplehash::CityHasher64::new();
        self.hash_equal_(&mut hash);
        hash.finish_raw()
    }

    fn hash_equal_<H: std::hash::Hasher>(self, hasher: &mut H) {
        if self.is_immediate() {
            hasher.write_u64(self.bits());
        } else if self.is_pair() {
            hasher.write_u8(0x0);
            self.car().hash_equal_(hasher);
            self.cdr().hash_equal_(hasher);
        } else if self.is::<Vector>() {
            let v = self.downcast::<Vector>();
            for i in 0..v.len() {
                hasher.write_u8(0x1);
                v[i].get().hash_equal_(hasher);
            }
        } else {
            <Self as std::hash::Hash>::hash(&self, hasher);
        }
    }
}

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
}

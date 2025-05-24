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

            _ => {
                if self.is::<Tuple>() && other.is::<Tuple>() {
                    let a = self.downcast::<Tuple>();
                    let b = other.downcast::<Tuple>();

                    if a.len() != b.len() {
                        return false;
                    }

                    for i in 0..a.len() {
                        if !a[i].eqv(b[i]) {
                            return false;
                        }
                    }

                    return true;
                } else {
                    return false;
                }
            }
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
                if !a[i].r5rs_equal(b[i]) {
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

        if self.is::<String>() && other.is::<String>() {
            return self.downcast::<String>().eq(&other.downcast::<String>());
        }

        false
    }
}

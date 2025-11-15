use im::HashMap;

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

                    return Number::inexact_equal(a, b);
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

        if self.is::<HashTable>() && other.is::<HashTable>() {
            let a = self.downcast::<HashTable>();
            let b = other.downcast::<HashTable>();

            if a.len() != b.len() {
                return false;
            }

            let kvs1 = a.iter().collect::<Vec<_>>();
            let kvs2 = b.iter().collect::<Vec<_>>();
            for (k1, v1) in kvs1.iter() {
                let mut found = false;
                for (k2, v2) in kvs2.iter() {
                    if k1.r5rs_equal(*k2) {
                        if !v1.r5rs_equal(*v2) {
                            return false;
                        }
                        found = true;
                        break;
                    }
                }
                if !found {
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

        if self.is::<Syntax>() && other.is::<Syntax>() {
            let s1 = self.downcast::<Syntax>();
            let s2 = other.downcast::<Syntax>();

            return s1.expr.r5rs_equal(s2.expr)
                && s1.wrap.r5rs_equal(s2.wrap)
                && s1.module.r5rs_equal(s2.module)
                && s1.source.r5rs_equal(s2.source);
        }

        self.eqv(other)
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

    fn is_terminal_list(self) -> usize {
        let mut count = 1;

        if self.is_null() {
            return count;
        }

        let mut fast = self;
        let mut slow = self;

        while fast.is_pair() {
            let elt = fast.car();
            if elt.is_pair() || elt.is::<Vector>() {
                return 0;
            }

            fast = fast.cdr();
            count += 1;

            if fast.is_pair() {
                let elt = fast.car();
                if elt.is_pair() || elt.is::<Vector>() {
                    return 0;
                }
            } else {
                return count;
            }

            fast = fast.cdr();
            count += 1;
            slow = slow.cdr();

            if slow == fast {
                return 0;
            }
        }

        if fast.is::<Vector>() {
            return 0;
        }

        count
    }

    fn find_and_merge_opponent(
        visited: &mut HashMap<Value<'gc>, Vec<Value<'gc>>>,
        lst1: Value<'gc>,
        lst2: Value<'gc>,
    ) -> bool {
        let opponents = visited.get_mut(&lst1);

        if let Some(opponents) = opponents {
            for opponent in opponents.iter() {
                if *opponent == lst2 {
                    return true;
                }
            }

            opponents.push(lst2);
        } else {
            let new_opponents = vec![lst2];
            visited.insert(lst1, new_opponents);
        }

        false
    }

    pub fn equal(self, other: Self, visited: &mut HashMap<Value<'gc>, Vec<Value<'gc>>>) -> bool {
        let c1 = self.is_terminal_list();
        if c1 != 0 {
            if c1 == other.is_terminal_list() {
                return self.r5rs_equal(other);
            }

            return false;
        } else {
            if other.is_terminal_list() != 0 {
                return false;
            }
        }

        let mut lst1 = self;
        let mut lst2 = other;

        'top: loop {
            if lst1 == lst2 {
                return true;
            }

            if lst1.is_pair() {
                if lst2.is_pair() {
                    if Self::find_and_merge_opponent(visited, lst1, lst2) {
                        return true;
                    }

                    if lst1.car().equal(lst2.car(), visited) {
                        lst1 = lst1.cdr();
                        lst2 = lst2.cdr();
                        continue 'top;
                    }
                }

                return false;
            }

            if lst1.is::<Vector>() {
                if lst2.is::<Vector>() {
                    if Self::find_and_merge_opponent(visited, lst1, lst2) {
                        return true;
                    }

                    let v1 = lst1.downcast::<Vector>();
                    let v2 = lst2.downcast::<Vector>();

                    if v1.len() != v2.len() {
                        return false;
                    }

                    for i in 0..v1.len() {
                        if !v1[i].get().equal(v2[i].get(), visited) {
                            return false;
                        }
                    }

                    return true;
                }

                return false;
            }

            return lst1.r5rs_equal(lst2);
        }
    }
}

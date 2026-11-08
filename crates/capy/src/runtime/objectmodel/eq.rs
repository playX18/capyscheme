//! Equality and structural hashing for Scheme values.

use std::sync::OnceLock;

use im::HashMap;

use super::*;

/// Nodes compared before the acyclic fast path gives up and restarts with the
/// visited-set implementation. Large enough for the structures `equal?`
/// normally sees, small enough to bound the work done twice on a structure the
/// acyclic walk cannot handle (a heavily shared graph, or a cycle that the cdr
/// spine does not expose).
const EQUAL_FAST_WORK: u32 = 1 << 20;

/// Recursion depth before the fast path gives up. A cycle through `car` or
/// through vector elements is only bounded by this, and the mutator thread has
/// the usual 2 MiB stack, so keep it to a few hundred frames.
const EQUAL_FAST_DEPTH: u32 = 512;

#[derive(Clone, Copy, PartialEq, Eq)]
enum EqualSemantics {
    /// `Value::equal`: its terminal-list pre-walk makes a non-pair,
    /// non-vector left operand unequal to a pair or vector without reaching
    /// `eqv?`.
    Prewalk,
    /// `Value::r5rs_equal`: no pre-walk, so a non-pair, non-vector left
    /// operand falls through to `eqv?` exactly as `r5rs_equal_visited` does.
    R5rs,
}

struct EqualFastBudget {
    work: u32,
    depth: u32,
}

impl EqualFastBudget {
    fn new() -> Self {
        Self {
            work: EQUAL_FAST_WORK,
            depth: 0,
        }
    }

    /// Charges one node comparison; `None` means the budget ran out and the
    /// caller must restart with the visited-set walk.
    #[inline]
    fn spend(&mut self) -> Option<()> {
        self.work = self.work.checked_sub(1)?;
        Some(())
    }
}

#[inline]
fn equal_fast_enabled() -> bool {
    // The flag is process-wide, so read it once. An `equal?`-heavy program
    // must not re-parse the environment (or even re-check a `LazyLock`) on
    // every call.
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(crate::utils::flags::equal_fast)
}

impl<'gc> Value<'gc> {
    pub fn eqv(self, other: Self) -> bool {
        if self.raw_i64() == other.raw_i64() {
            return true;
        }

        if let Some(result) = crate::runtime::objectmodel::class::compare_primitive_values(self, other) {
            return result;
        }

        match (self.number(), other.number()) {
            (Some(a), Some(b)) => {
                if a.is_exact() && b.is_exact() {
                    Number::exact_equal(a, b)
                } else {
                    if b.is_exact() {
                        return false;
                    }

                    Number::inexact_equal(a, b)
                }
            }

            _ => false,
        }
    }

    pub fn r5rs_equal(self, other: Self) -> bool {
        if equal_fast_enabled() {
            let mut budget = EqualFastBudget::new();
            if let Some(result) = self.equal_fast(other, &mut budget, EqualSemantics::R5rs) {
                return result;
            }
        }

        self.r5rs_equal_visited(other, &mut Default::default())
    }

    fn r5rs_equal_visited(
        self,
        other: Self,
        visited: &mut HashMap<Value<'gc>, Vec<Value<'gc>>>,
    ) -> bool {
        if self.raw_i64() == other.raw_i64() {
            return true;
        }

        if self.is_pair() && other.is_pair() {
            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }
            if self.car().r5rs_equal_visited(other.car(), visited) {
                return self.cdr().r5rs_equal_visited(other.cdr(), visited);
            } else {
                return false;
            }
        }

        if self.is::<Vector>() && other.is::<Vector>() {
            let a = self.downcast::<Vector>();
            let b = other.downcast::<Vector>();

            if a.len() != b.len() {
                return false;
            }
            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }

            for i in 0..a.len() {
                if !a[i].get().r5rs_equal_visited(b[i].get(), visited) {
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
            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }

            let kvs1 = a.iter().collect::<Vec<_>>();
            let kvs2 = b.iter().collect::<Vec<_>>();
            for (k1, v1) in kvs1.iter() {
                let mut found = false;
                for (k2, v2) in kvs2.iter() {
                    if k1.r5rs_equal_visited(*k2, visited) {
                        if !v1.r5rs_equal_visited(*v2, visited) {
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

        if self.is::<PersistentMap>() && other.is::<PersistentMap>() {
            let a = self.downcast::<PersistentMap>();
            let b = other.downcast::<PersistentMap>();

            if a.len() != b.len() {
                return false;
            }

            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }
            let mut equal = true;
            a.fold(
                |acc, key, value| {
                    if !equal {
                        return acc;
                    }
                    match b.get(key) {
                        Some(other_value) if value.r5rs_equal_visited(other_value, visited) => acc,
                        _ => {
                            equal = false;
                            acc
                        }
                    }
                },
                Value::new(false),
            );
            return equal;
        }

        if self.is::<PersistentSet>() && other.is::<PersistentSet>() {
            let a = self.downcast::<PersistentSet>();
            let b = other.downcast::<PersistentSet>();

            if a.len() != b.len() {
                return false;
            }

            let mut equal = true;
            a.fold(
                |acc, key, _| {
                    if !equal {
                        return acc;
                    }
                    if !b.contains(key) {
                        equal = false;
                    }
                    acc
                },
                Value::new(false),
            );
            return equal;
        }

        if self.is::<Str>() && other.is::<Str>() {
            return self.downcast::<Str>().eq(&other.downcast::<Str>());
        }
        if self.is::<Boxed>() && other.is::<Boxed>() {
            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }
            return self
                .downcast::<Boxed>()
                .get()
                .r5rs_equal_visited(other.downcast::<Boxed>().get(), visited);
        }

        if self.is::<Syntax>() && other.is::<Syntax>() {
            let s1 = self.downcast::<Syntax>();
            let s2 = other.downcast::<Syntax>();

            if Self::find_and_merge_opponent(visited, self, other) {
                return true;
            }
            return s1.expr.r5rs_equal_visited(s2.expr, visited)
                && s1.wrap.r5rs_equal_visited(s2.wrap, visited)
                && s1.module.r5rs_equal_visited(s2.module, visited)
                && s1.source.r5rs_equal_visited(s2.source, visited);
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
        } else if self.is::<Symbol>() {
            self.downcast::<Symbol>().hash(hasher);
        } else if self.is::<Str>() {
            self.downcast::<Str>().hash(hasher);
        } else if self.is::<ByteVector>() {
            (**self.downcast::<ByteVector>()).hash(hasher);
        } else if self.is::<Syntax>() {
            // `equal?` also compares wraps, modules and sources; hashing only
            // the expression stays consistent with it and avoids identity.
            hasher.write_u8(0x3);
            self.downcast::<Syntax>().expr.hash_equal_(hasher);
        } else if self.is::<Keyword>() {
            // Hash by name: object hash codes depend on allocation history,
            // which makes derived names (e.g. psyntax's `fresh-derived-name`)
            // differ between compiler processes.
            hasher.write_u8(0x2);
            hasher.write_u64(self.downcast::<Keyword>().symbol.hash.get());
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
        // Only a fresh visited set lets the fast path ignore it and restart
        // from the top on failure; every caller passes a fresh one.
        if visited.is_empty() {
            if let Some(result) = self.equal_with_fresh_visited(other) {
                return result;
            }
        }

        self.equal_slow(other, visited)
    }

    /// Structural equality with an internal visited set. Equivalent to
    /// `equal(other, &mut Default::default())`, but the empty set (an `Arc`
    /// allocation) is only built when the acyclic fast path gives up.
    pub fn equal_default(self, other: Self) -> bool {
        if let Some(result) = self.equal_with_fresh_visited(other) {
            return result;
        }

        self.equal_slow(other, &mut Default::default())
    }

    fn equal_with_fresh_visited(self, other: Self) -> Option<bool> {
        if !equal_fast_enabled() {
            return None;
        }

        let mut budget = EqualFastBudget::new();
        self.equal_fast(other, &mut budget, EqualSemantics::Prewalk)
    }

    /// Non-allocating structural comparison for acyclic values. Returns `None`
    /// when a cycle, the work budget or the depth budget is hit; the caller
    /// then restarts from the top with `equal_slow`/`r5rs_equal_visited`.
    /// `equal?` is pure, so restarting is safe.
    fn equal_fast(
        self,
        other: Self,
        budget: &mut EqualFastBudget,
        semantics: EqualSemantics,
    ) -> Option<bool> {
        if self.raw_i64() == other.raw_i64() {
            return Some(true);
        }

        budget.depth += 1;
        let result = if budget.depth > EQUAL_FAST_DEPTH {
            None
        } else {
            self.equal_fast_impl(other, budget, semantics)
        };
        budget.depth -= 1;
        result
    }

    fn equal_fast_impl(
        self,
        other: Self,
        budget: &mut EqualFastBudget,
        semantics: EqualSemantics,
    ) -> Option<bool> {
        if self.is_pair() {
            if !other.is_pair() {
                return Some(false);
            }
            return Self::equal_fast_spine(self, other, budget, semantics);
        }

        if self.is::<Vector>() {
            if !other.is::<Vector>() {
                return Some(false);
            }
            return Self::equal_fast_vector(self, other, budget, semantics);
        }

        Self::equal_fast_leaf(self, other, semantics)
    }

    /// Walks the pair cdr chain iteratively (no recursion, no allocation) and
    /// compares cars recursively. Floyd's tortoise/hare over the pair of
    /// spines detects a cdr cycle without allocating, which is the common
    /// cycle shape; cycles through cars or vector elements are caught by the
    /// work/depth budget.
    fn equal_fast_spine(
        mut a: Self,
        mut b: Self,
        budget: &mut EqualFastBudget,
        semantics: EqualSemantics,
    ) -> Option<bool> {
        let mut slow_a = a;
        let mut slow_b = b;
        let mut steps: u32 = 0;
        // A list of the same object and a list of the same partner object
        // compare each car pair the same way; skip a run of identical car
        // pairs. This keeps heavily shared lists such as equal?'s
        // `make-test-tree1` linear without a visited set. The skip is sound
        // because the comparison just performed is pure: it returned true, or
        // the whole spine would already have returned false.
        let mut last_car = None;

        loop {
            if a.raw_i64() == b.raw_i64() {
                return Some(true);
            }

            if !a.is_pair() {
                return Self::equal_fast_tail(a, b, budget, semantics);
            }
            if !b.is_pair() {
                return Some(false);
            }

            let (a_car, b_car) = (a.car(), b.car());
            if last_car != Some((a_car.raw_i64(), b_car.raw_i64())) {
                budget.spend()?;
                if !Self::equal_fast(a_car, b_car, budget, semantics)? {
                    return Some(false);
                }
                last_car = Some((a_car.raw_i64(), b_car.raw_i64()));
            }

            a = a.cdr();
            b = b.cdr();

            steps = steps.wrapping_add(1);
            if steps & 1 == 0 {
                if slow_a.is_pair() {
                    slow_a = slow_a.cdr();
                }
                if slow_b.is_pair() {
                    slow_b = slow_b.cdr();
                }
                if slow_a.raw_i64() == a.raw_i64() && slow_b.raw_i64() == b.raw_i64() {
                    // The cdr chain repeats; only the visited-set walk can
                    // decide a cyclic comparison, so restart from the top.
                    return None;
                }
            }
        }
    }

    /// Compares the non-pair tail of a pair spine. `a` is known not to be a
    /// pair; the dispatch mirrors the old main loop of `equal_slow`.
    fn equal_fast_tail(
        a: Self,
        b: Self,
        budget: &mut EqualFastBudget,
        semantics: EqualSemantics,
    ) -> Option<bool> {
        if a.raw_i64() == b.raw_i64() {
            return Some(true);
        }

        if a.is::<Vector>() {
            if !b.is::<Vector>() {
                return Some(false);
            }
            return Self::equal_fast_vector(a, b, budget, semantics);
        }

        Self::equal_fast_leaf(a, b, semantics)
    }

    fn equal_fast_vector(
        a: Self,
        b: Self,
        budget: &mut EqualFastBudget,
        semantics: EqualSemantics,
    ) -> Option<bool> {
        let a = a.downcast::<Vector>();
        let b = b.downcast::<Vector>();

        if a.len() != b.len() {
            return Some(false);
        }

        for i in 0..a.len() {
            budget.spend()?;
            if !Self::equal_fast(a[i].get(), b[i].get(), budget, semantics)? {
                return Some(false);
            }
        }

        Some(true)
    }

    /// Compares two values whose left operand is neither a pair nor a vector.
    /// This reproduces exactly what the old implementations reach: `equal`'s
    /// terminal-list pre-walk rejects a scalar against a pair or vector,
    /// while `r5rs_equal` falls through to `eqv?`. For every other type this
    /// mirrors the tail of `r5rs_equal_visited`: strings and bytevectors
    /// compare by content, the collection/box/syntax types are structural,
    /// and everything else (numbers, symbols, records/tuples/instances) is
    /// `eqv?`.
    fn equal_fast_leaf(a: Self, b: Self, semantics: EqualSemantics) -> Option<bool> {
        if semantics == EqualSemantics::Prewalk && (b.is_pair() || b.is::<Vector>()) {
            return Some(false);
        }

        if a.is::<Str>() {
            return Some(b.is::<Str>() && a.downcast::<Str>().eq(&b.downcast::<Str>()));
        }
        if a.is::<ByteVector>() {
            if !b.is::<ByteVector>() {
                return Some(false);
            }
            let a = a.downcast::<ByteVector>();
            let b = b.downcast::<ByteVector>();
            if a.len() != b.len() {
                return Some(false);
            }
            for i in 0..a.len() {
                if a[i] != b[i] {
                    return Some(false);
                }
            }
            return Some(true);
        }

        // These carry structure that the visited set must handle. Delegating
        // also keeps their cycle safety; a fresh set is enough because `a`
        // has no pair or vector above it in this comparison.
        if (a.is::<HashTable>() && b.is::<HashTable>())
            || (a.is::<PersistentMap>() && b.is::<PersistentMap>())
            || (a.is::<PersistentSet>() && b.is::<PersistentSet>())
            || (a.is::<Boxed>() && b.is::<Boxed>())
            || (a.is::<Syntax>() && b.is::<Syntax>())
        {
            return Some(a.r5rs_equal_visited(b, &mut Default::default()));
        }

        Some(a.eqv(b))
    }

    fn equal_slow(
        self,
        other: Self,
        visited: &mut HashMap<Value<'gc>, Vec<Value<'gc>>>,
    ) -> bool {
        let c1 = self.is_terminal_list();
        if c1 != 0 {
            if c1 == other.is_terminal_list() {
                return self.r5rs_equal_visited(other, visited);
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

                    if lst1.car().equal_slow(lst2.car(), visited) {
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
                        if !v1[i].get().equal_slow(v2[i].get(), visited) {
                            return false;
                        }
                    }

                    return true;
                }

                return false;
            }

            return lst1.r5rs_equal_visited(lst2, visited);
        }
    }
}

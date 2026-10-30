use crate::heap::Gc;
use crate::list;
use crate::prelude::*;
use crate::{runtime::prelude::*, static_symbols};
use simplehash::MurmurHasher64;
use std::hash::Hash;

pub fn init_hash<'gc>(ctx: MutationContext<'gc>) {
    hash_ops::register(ctx);
}

static_symbols!(
    SYM_EQ = "eq?"
    SYM_EQV = "eqv?"
    SYM_EQUAL = "equal?"
    SYM_STRING = "string=?"
    SYM_GENERIC = "generic"
    SYM_CAPY = "capy"
);

fn list_to_hash_impl<'gc>(
    call: &Call<'_, 'gc>,
    lst: Value<'gc>,
    ht_type: HashTableType<'gc>,
    name: &str,
) -> Value<'gc> {
    let ctx = call.ctx;
    if !lst.is_list() {
        return call.wrong_argument_violation(
            name,
            "expected a list",
            Some(lst),
            Some(1),
            1,
            &[lst],
        );
    }

    let ht = HashTable::new(ctx, ht_type, lst.list_length(), 0.75);

    let mut current = lst;
    while current.is_pair() {
        let cell = current.car();

        if !cell.is_pair() {
            return call.wrong_argument_violation(
                name,
                "expected a list of pairs",
                Some(lst),
                Some(1),
                1,
                &[lst],
            );
        }

        ht.put(ctx, cell.car(), cell.cdr());
        current = current.cdr();
    }

    ht.into()
}

#[scheme(path=capy)]
pub mod hash_ops {
    #[scheme(name = "eq?")]
    pub fn eq_p(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.bits() == b.bits()
    }

    #[scheme(name = "eqv?")]
    pub fn eqv_p(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.eqv(b)
    }

    #[scheme(name = "equal?")]
    pub fn equal_p(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.equal(b, &mut Default::default())
    }

    #[scheme(name = "core-hashtable?")]
    pub fn is_hash_table(val: Value<'gc>) -> bool {
        val.is::<HashTable>()
    }

    #[scheme(name = "make-core-hash-eq")]
    pub fn make_hash_eq() -> Value<'gc> {
        let ht = HashTable::new(ctx, HashTableType::Eq, 8, 0.75);

        ht.into()
    }

    #[scheme(name = "make-core-hash-eqv")]
    pub fn make_hash_eqv() -> Value<'gc> {
        let ht = HashTable::new(ctx, HashTableType::Eqv, 8, 0.75);

        ht.into()
    }
    #[scheme(name = "make-core-hash-equal")]
    pub fn make_hash_equal() -> Value<'gc> {
        let ht = HashTable::new(ctx, HashTableType::Equal, 8, 0.75);

        ht.into()
    }

    #[scheme(name = "make-core-hash-string")]
    pub fn make_hash_string() -> Value<'gc> {
        let ht = HashTable::new(ctx, HashTableType::String, 8, 0.75);

        ht.into()
    }

    #[scheme(name = "core-hash-ref")]
    pub fn hash_ref(
        ht: Gc<'gc, HashTable<'gc>>,
        key: Value<'gc>,
        default_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let res = ht
            .get(ctx, key)
            .unwrap_or(default_value.unwrap_or(Value::new(false)));
        res
    }

    #[scheme(name = "core-hash-put!")]
    pub fn hash_set(ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        ht.put(ctx, key, value);
        Value::undefined()
    }

    #[scheme(name = "core-hash-remove!")]
    pub fn hash_remove(ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>) -> Value<'gc> {
        ht.remove(ctx, key);
        Value::undefined()
    }

    #[scheme(name = "core-hash-contains?")]
    pub fn hash_contains(ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>) -> bool {
        let res = ht.get(ctx, key);
        res.is_some()
    }

    #[scheme(name = "core-hash-size")]
    pub fn hash_size(ht: Gc<'gc, HashTable<'gc>>) -> usize {
        let size = ht.len();
        size
    }

    #[scheme(name = "core-hash->list")]
    pub fn hash_to_list(
        ht: Either<Gc<'gc, HashTable<'gc>>, Gc<'gc, WeakTable<'gc>>>,
    ) -> Value<'gc> {
        match ht {
            Either::Right(ht) => {
                let ls = ht.fold(
                    ctx,
                    |k, v, acc| Value::cons(ctx, Value::cons(ctx, k, v), acc),
                    Value::null(),
                );

                ls
            }

            Either::Left(ht) => {
                let list = ht
                    .iter()
                    .map(|(k, v)| Value::cons(ctx, k, v))
                    .collect::<Vec<_>>();

                let mut ls = Value::null();

                for pair in list.into_iter().rev() {
                    ls = Value::cons(ctx, pair, ls);
                }
                ls
            }
        }
    }

    #[scheme(name = "list->core-hash-eq")]
    pub fn list_to_hash(lst: Value<'gc>) -> Value<'gc> {
        list_to_hash_impl(call, lst, HashTableType::Eq, "list->core-hash-eq")
    }

    #[scheme(name = "list->core-hash-eqv")]
    pub fn list_to_hash_eqv(lst: Value<'gc>) -> Value<'gc> {
        list_to_hash_impl(call, lst, HashTableType::Eqv, "list->core-hash-eqv")
    }

    #[scheme(name = "list->core-hash-equal")]
    pub fn list_to_hash_equal(lst: Value<'gc>) -> Value<'gc> {
        list_to_hash_impl(call, lst, HashTableType::Equal, "list->core-hash-equal")
    }

    #[scheme(name = "list->core-hash-string")]
    pub fn list_to_hash_string(lst: Value<'gc>) -> Value<'gc> {
        list_to_hash_impl(call, lst, HashTableType::String, "list->core-hash-string")
    }

    #[scheme(name = "hash")]
    pub fn hash(value: Value<'gc>, bound: Option<u64>) -> Value<'gc> {
        let hash = value.hash_equal() % bound.unwrap_or(u64::MAX);
        let hash = hash.into_value(ctx);

        hash
    }

    /// Computes the `eq?` hash of the value. If `bound` is provided, the result is taken modulo `bound`.
    ///
    /// The main difference between `hash` and `hash-eq` is that `hash` never hashes by object identity,
    /// while this function does.
    #[scheme(name = "hash-eq")]
    pub fn hash_eq(value: Value<'gc>, bound: Option<u64>) -> Value<'gc> {
        let mut hasher = simplehash::CityHasher64::new();
        value.hash(&mut hasher);
        let hash = hasher.finish_raw() % bound.unwrap_or(u64::MAX);

        let hash = hash.into_value(ctx);

        hash
    }

    #[scheme(name = "make-core-hashtable")]
    pub fn make_core_hashtable(
        kind: Option<SymbolRef<'gc>>,
        nsize_or_handlers: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let typ = if let Some(kind) = kind {
            if kind == sym_eq(ctx) {
                HashTableType::Eq
            } else if kind == sym_eqv(ctx) {
                HashTableType::Eqv
            } else if kind == sym_equal(ctx) {
                HashTableType::Equal
            } else if kind == sym_string(ctx) {
                HashTableType::String
            } else if kind == sym_generic(ctx) {
                let handlers = match nsize_or_handlers {
                    None => {
                        return call.wrong_argument_violation(
                            "make-core-hash",
                            "expected vector of handlers",
                            None,
                            Some(2),
                            1,
                            &[],
                        );
                    }
                    Some(v) => v,
                };
                if !handlers.is::<Vector>() {
                    return call.wrong_argument_violation(
                        "make-core-hash",
                        "expected vector of handlers",
                        Some(handlers),
                        Some(2),
                        1,
                        &[handlers],
                    );
                }

                if handlers.downcast::<Vector>().len() != 14 {
                    return call.wrong_argument_violation(
                        "make-core-hash",
                        "expected vector of 14 handlers",
                        Some(handlers),
                        Some(2),
                        1,
                        &[handlers],
                    );
                }

                return HashTable::new(ctx, HashTableType::Generic(handlers), 0, 0.0).into();
            } else {
                return call.wrong_argument_violation(
                    "make-core-hash",
                    "expected 'eq?, 'eqv?, 'equal?, 'string=? or 'generic",
                    Some(kind.into()),
                    Some(1),
                    1,
                    &[kind.into()],
                );
            }
        } else {
            HashTableType::Eq
        };

        let nsize = match nsize_or_handlers {
            None => 8,
            Some(v) => match usize::try_from_value(ctx, v) {
                Ok(size) => size,
                Err(_) => {
                    return call.wrong_argument_violation(
                        "make-core-hash",
                        "expected a non-negative integer for initial size",
                        Some(v),
                        Some(2),
                        1,
                        &[v],
                    );
                }
            },
        };

        let ht = HashTable::new(ctx, typ, nsize, 0.75);
        ht.into()
    }

    #[scheme(name = "make-weak-core-hashtable")]
    pub fn make_weak_core_hashtable() -> Value<'gc> {
        let ht = WeakTable::new(ctx, 8, 0.75);
        ht.into()
    }

    #[scheme(name = "core-hash-mutable?")]
    pub fn core_hashtable_mutablep(ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>) -> Value<'gc> {
        match ht {
            Either::Left(ht) => match ht.typ() {
                HashTableType::Generic(v) => {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_MUTABLE].get();
                    return call.tail(handler, &[ht.into()]);
                }
                _ => (ht.is_mutable()).into(),
            },
            Either::Right(_) => (true).into(),
        }
    }

    #[scheme(name = "core-hash-equivalence-function")]
    pub fn core_hashtable_equivalence_function(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
    ) -> Value<'gc> {
        match ht {
            Either::Left(ht) => match ht.typ() {
                /* if these lookups fail something is seriously messed up */
                HashTableType::Eq => capy_primitive(ctx, sym_eq(ctx).into()),
                HashTableType::Eqv => capy_primitive(ctx, sym_eqv(ctx).into()),
                HashTableType::Equal => capy_primitive(ctx, sym_equal(ctx).into()),
                HashTableType::String => capy_primitive(ctx, sym_string(ctx).into()),
                HashTableType::Generic(v) => {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_EQUIV_FUNC].get();
                    call.tail(handler, &[ht.into()])
                }
            },

            _ => capy_primitive(ctx, sym_eq(ctx).into()),
        }
    }

    #[scheme(name = "core-hash-hash-function")]
    pub fn core_hashtable_hash_function(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
    ) -> Value<'gc> {
        match ht {
            Either::Left(ht) => match ht.typ() {
                HashTableType::Generic(v) => {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_HASH_FUNC].get();
                    call.tail(handler, &[ht.into()])
                }

                _ => Value::new(false),
            },

            _ => Value::new(false),
        }
    }

    #[scheme(name = "core-hash-set!")]
    pub fn core_hashtable_set(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        match ht {
            Either::Left(ht) => {
                match ht.typ() {
                    HashTableType::Generic(v) => {
                        let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_SET].get();
                        return call.tail(handler, &[ht.into(), key, value]);
                    }
                    HashTableType::String if !key.is::<Str>() => {
                        return call.wrong_argument_violation(
                            "core-hash-set!",
                            "expected a string key",
                            Some(key),
                            Some(1),
                            3,
                            &[ht.into(), key, value],
                        );
                    }
                    _ => (),
                }

                if !ht.is_mutable() {
                    return call.wrong_argument_violation(
                        "core-hash-set!",
                        "hashtable is not mutable",
                        Some(ht.into()),
                        Some(0),
                        3,
                        &[ht.into(), key, value],
                    );
                }
                ht.put(ctx, key, value);
                Value::undefined()
            }
            Either::Right(ht) => {
                if !key.is_cell() {
                    return call.wrong_argument_violation(
                        "core-hash-set!",
                        "expected a non-immediate key",
                        Some(key),
                        Some(1),
                        3,
                        &[ht.into(), key, value],
                    );
                }

                ht.put(ctx, key, value);
                Value::undefined()
            }
        }
    }

    #[scheme(name = "core-hash-set")]
    pub fn core_hashtable_set_immut(
        ht: HashTableRef<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let typ = ht.typ();

        match typ {
            HashTableType::Generic(v) => {
                let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_SET].get();
                return call.tail(handler, &[ht.into(), key, value]);
            }
            HashTableType::String if !key.is::<Str>() => {
                return call.wrong_argument_violation(
                    "core-hash-set",
                    "expected a string key",
                    Some(key),
                    Some(1),
                    3,
                    &[ht.into(), key, value],
                );
            }
            _ => (),
        }

        let new_ht = ht.copy(ctx, true);
        new_ht.put(ctx, key, value);
        new_ht.into()
    }

    #[scheme(name = "core-hash-ref")]
    pub fn core_hashtable_ref(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
        key: Value<'gc>,
        default_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let default = default_value.unwrap_or(Value::new(false));
        match ht {
            Either::Left(ht) => {
                match ht.typ() {
                    HashTableType::Generic(v) => {
                        let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_REF].get();
                        return call.tail(handler, &[ht.into(), key, default]);
                    }
                    HashTableType::String if !key.is::<Str>() => {
                        return call.wrong_argument_violation(
                            "core-hash-ref",
                            "expected a string key",
                            Some(key),
                            Some(1),
                            3,
                            &[ht.into(), key, default],
                        );
                    }
                    _ => (),
                }

                let res = ht.get(ctx, key).unwrap_or(default);
                res
            }
            Either::Right(ht) => {
                let res = ht.get(ctx, key).unwrap_or(default);
                res
            }
        }
    }

    #[scheme(name = "core-hash-delete!")]
    pub fn core_hashtable_delete(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        match ht {
            Either::Left(ht) => {
                match ht.typ() {
                    HashTableType::Generic(v) => {
                        let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_DELETE].get();
                        return call.tail(handler, &[ht.into(), key]);
                    }
                    HashTableType::String if !key.is::<Str>() => {
                        return call.wrong_argument_violation(
                            "core-hash-delete!",
                            "expected a string key",
                            Some(key),
                            Some(1),
                            2,
                            &[ht.into(), key],
                        );
                    }
                    _ => (),
                }
                if !ht.is_mutable() {
                    return call.wrong_argument_violation(
                        "core-hash-delete!",
                        "hashtable is not mutable",
                        Some(ht.into()),
                        Some(0),
                        2,
                        &[ht.into(), key],
                    );
                }

                ht.remove(ctx, key);
                Value::undefined()
            }
            Either::Right(ht) => {
                ht.remove(ctx, key);
                Value::undefined()
            }
        }
    }

    #[scheme(name = "core-hash-clear!")]
    pub fn core_hashtable_clear(ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>) -> Value<'gc> {
        match ht {
            Either::Left(ht) => {
                if let HashTableType::Generic(v) = ht.typ() {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_CLEAR].get();
                    return call.tail(handler, &[ht.into()]);
                }
                if !ht.is_mutable() {
                    return call.wrong_argument_violation(
                        "core-hash-clear!",
                        "hashtable is not mutable",
                        Some(ht.into()),
                        Some(0),
                        1,
                        &[ht.into()],
                    );
                }
                ht.clear(ctx);
                Value::undefined()
            }
            Either::Right(ht) => {
                ht.clear(ctx);
                Value::undefined()
            }
        }
    }

    #[scheme(name = "core-hash-size")]
    pub fn core_hashtable_size(ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>) -> Value<'gc> {
        match ht {
            Either::Right(ht) => {
                let size = ht.len();
                return (size).into_value(ctx);
            }
            Either::Left(ht) => {
                if let HashTableType::Generic(v) = ht.typ() {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_SIZE].get();
                    return call.tail(handler, &[ht.into()]);
                }
                let size = ht.len();
                (size).into_value(ctx)
            }
        }
    }

    #[scheme(name = "core-hash-contains?")]
    pub fn core_hashtable_contains(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        match ht {
            Either::Left(ht) => {
                match ht.typ() {
                    HashTableType::Generic(v) => {
                        let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_CONTAINS].get();
                        return call.tail(handler, &[ht.into(), key]);
                    }
                    HashTableType::String if !key.is::<Str>() => {
                        return call.wrong_argument_violation(
                            "core-hash-contains?",
                            "expected a string key",
                            Some(key),
                            Some(1),
                            2,
                            &[ht.into(), key],
                        );
                    }
                    _ => (),
                }
                let res = ht.get(ctx, key);
                (res.is_some()).into()
            }

            Either::Right(ht) => {
                let res = ht.get(ctx, key);
                (res.is_some()).into()
            }
        }
    }

    #[scheme(name = "core-hash-copy")]
    pub fn core_hashtable_copy(
        ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>,
        mutable: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let immutable = match mutable {
            Some(mutable) => !mutable,
            None => true,
        };
        match ht {
            Either::Right(ht) => {
                let copy = ht.copy(ctx);
                return copy.into();
            }
            Either::Left(ht) => {
                if let HashTableType::Generic(v) = ht.typ() {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_COPY].get();
                    return call.tail(handler, &[ht.into(), Value::new(!immutable)]);
                }
                let copy = ht.copy(ctx, immutable);
                copy.into()
            }
        }
    }

    #[scheme(name = "core-hash->alist")]
    pub fn core_hashtable_to_alist(ht: Either<HashTableRef<'gc>, WeakTableRef<'gc>>) -> Value<'gc> {
        match ht {
            Either::Left(ht) => {
                if let HashTableType::Generic(v) = ht.typ() {
                    let handler = v.downcast::<Vector>()[HASHTABLE_HANDLER_ALIST].get();
                    return call.tail(handler, &[ht.into()]);
                }
                let list = ht
                    .iter()
                    .map(|(k, v)| Value::cons(ctx, k, v))
                    .collect::<Vec<_>>();

                let mut ls = Value::null();

                for pair in list.into_iter().rev() {
                    ls = Value::cons(ctx, pair, ls);
                }
                ls
            }

            Either::Right(ht) => {
                let ls = ht.fold(
                    ctx,
                    |k, v, acc| Value::cons(ctx, Value::cons(ctx, k, v), acc),
                    Value::null(),
                );

                ls
            }
        }
    }

    #[scheme(name = "weak-core-hashtable?")]
    pub fn is_weak_hash_table(val: Value<'gc>) -> bool {
        val.is::<WeakTable>()
    }

    #[scheme(name = "string-hash")]
    pub fn string_hash(s: Gc<'gc, Str<'gc>>) -> u64 {
        let mut hasher = MurmurHasher64::new(5381);
        s.hash(&mut hasher);
        hasher.finish_u64()
    }

    #[scheme(name = "symbol-hash")]
    pub fn symbol_hash(s: SymbolRef<'gc>) -> u64 {
        let mut hasher = MurmurHasher64::new(5381);
        s.hash(&mut hasher);
        hasher.finish_u64()
    }

    #[scheme(name = "equal-hash")]
    pub fn equal_hash(v: Value<'gc>) -> u64 {
        let mut hasher = MurmurHasher64::new(5381);
        EqualHash(v).hash(&mut hasher);
        hasher.finish_u64()
    }
}

fn capy_primitive<'gc>(ctx: MutationContext<'gc>, name: Value<'gc>) -> Value<'gc> {
    ctx.globals()
        .capy_instance()
        .variable(ctx, name)
        .filter(|var| var.is_bound())
        .unwrap_or_else(|| panic!("primitive {name} is not defined"))
        .get()
}

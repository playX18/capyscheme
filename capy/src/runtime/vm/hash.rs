use rsgc::Gc;

use crate::{native_fn, runtime::prelude::*};

pub fn init_hash<'gc>(ctx: Context<'gc>) {
    register_hash_fns(ctx);
}

native_fn!(
    register_hash_fns:

    pub ("core-hashtable?") fn is_hash_table<'gc>(nctx, val: Value<'gc>) -> bool {
        nctx.return_(val.is::<HashTable>())
    }

    pub ("make-core-hash-eq") fn make_hash_eq<'gc>(nctx) -> Value<'gc> {
        let ht = HashTable::new(&nctx.ctx, HashTableType::Eq, 8, 0.75);

        nctx.return_(ht.into())
    }

    pub ("make-core-hash-eqv") fn make_hash_eqv<'gc>(nctx) -> Value<'gc> {
        let ht = HashTable::new(&nctx.ctx, HashTableType::Eqv, 8, 0.75);

        nctx.return_(ht.into())
    }

    pub ("make-core-hash-equal") fn make_hash_equal<'gc>(nctx) -> Value<'gc> {
        let ht = HashTable::new(&nctx.ctx, HashTableType::Equal, 8, 0.75);

        nctx.return_(ht.into())
    }

    pub ("make-core-hash-string") fn make_hash_string<'gc>(nctx) -> Value<'gc> {
        let ht = HashTable::new(&nctx.ctx, HashTableType::String, 8, 0.75);

        nctx.return_(ht.into())
    }

    pub ("core-hash-ref") fn hash_ref<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>, default_value: Option<Value<'gc>>) -> Value<'gc> {
        let res = ht.get(nctx.ctx, key).unwrap_or(default_value.unwrap_or(Value::new(false)));
        nctx.return_(res)
    }

    pub ("core-hash-put!") fn hash_set<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        ht.put(nctx.ctx, key, value);
        nctx.return_(Value::undefined())
    }

    pub ("core-hash-remove!") fn hash_remove<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>) -> Value<'gc> {
        ht.remove(nctx.ctx, key);
        nctx.return_(Value::undefined())
    }

    pub ("core-hash-contains?") fn hash_contains<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>, key: Value<'gc>) -> bool {
        let res = ht.get(nctx.ctx, key);
        nctx.return_(res.is_some())
    }

    pub ("core-hash-clear!") fn hash_clear<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>) -> Value<'gc> {
        ht.clear(nctx.ctx);
        nctx.return_(Value::undefined())
    }

    pub ("core-hash-size") fn hash_size<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>) -> usize {
        let size = ht.len();
        nctx.return_(size.into())
    }

    pub ("core-hash->list") fn hash_to_list<'gc>(nctx, ht: Gc<'gc, HashTable<'gc>>) -> Value<'gc> {
        let list = ht.iter().map(|(k, v)| Value::cons(nctx.ctx, k, v)).collect::<Vec<_>>();
        println!("hash_to_list: {:?}", list);
        let mut ls = Value::null();

        for pair in list.into_iter().rev() {
            ls = Value::cons(nctx.ctx, pair, ls);
        }
        nctx.return_(ls)
    }

    pub ("list->core-hash-eq") fn list_to_hash<'gc>(nctx, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("list->core-hash-eq", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }

        let ht = HashTable::new(&nctx.ctx, HashTableType::Eq, lst.list_length(), 0.75);

        let mut current = lst;
        while current.is_pair() {
            let cell = current.car();

            if !cell.is_pair() {
                return nctx.wrong_argument_violation("list->core-hash-eq", "expected a list of pairs", Some(lst), Some(1), 1, &[lst]);
            }

            let key = cell.car();
            let value = cell.cdr();

            ht.put(nctx.ctx, key, value);
            current = current.cdr();
        }

        nctx.return_(ht.into())
    }

    pub ("list->core-hash-eqv") fn list_to_hash_eqv<'gc>(nctx, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("list->core-hash-eqv", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }

        let ht = HashTable::new(&nctx.ctx, HashTableType::Eqv, lst.list_length(), 0.75);

        let mut current = lst;
        while current.is_pair() {
            let cell = current.car();

            if !cell.is_pair() {
                return nctx.wrong_argument_violation("list->core-hash-eqv", "expected a list of pairs", Some(lst), Some(1), 1, &[lst]);
            }

            let key = cell.car();
            let value = cell.cdr();

            ht.put(nctx.ctx, key, value);
            current = current.cdr();
        }

        nctx.return_(ht.into())
    }

    pub ("list->core-hash-equal") fn list_to_hash_equal<'gc>(nctx, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("list->core-hash-equal", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }

        let ht = HashTable::new(&nctx.ctx, HashTableType::Equal, lst.list_length(), 0.75);

        let mut current = lst;
        while current.is_pair() {
            let cell = current.car();

            if !cell.is_pair() {
                return nctx.wrong_argument_violation("list->core-hash-equal", "expected a list of pairs", Some(lst), Some(1), 1, &[lst]);
            }

            let key = cell.car();
            let value = cell.cdr();

            ht.put(nctx.ctx, key, value);
            current = current.cdr();
        }

        nctx.return_(ht.into())
    }

    pub ("list->core-hash-string") fn list_to_hash_string<'gc>(nctx, lst: Value<'gc>) -> Value<'gc> {
        if !lst.is_list() {
            return nctx.wrong_argument_violation("list->core-hash-string", "expected a list", Some(lst), Some(1), 1, &[lst]);
        }

        let ht = HashTable::new(&nctx.ctx, HashTableType::String, lst.list_length(), 0.75);

        let mut current = lst;
        while current.is_pair() {
            let cell = current.car();

            if !cell.is_pair() {
                return nctx.wrong_argument_violation("list->core-hash-string", "expected a list of pairs", Some(lst), Some(1), 1, &[lst]);
            }

            let key = cell.car();
            let value = cell.cdr();

            ht.put(nctx.ctx, key, value);
            current = current.cdr();
        }

        nctx.return_(ht.into())
    }

    pub ("hash") fn hash<'gc>(nctx, value: Value<'gc>, bound: Option<u64>) -> Value<'gc> {
        let hash = value.hash_equal() % bound.unwrap_or(u64::MAX);
        let hash = hash.into_value(nctx.ctx);

        nctx.return_(hash)
    }
);

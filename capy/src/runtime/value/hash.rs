use std::{
    cell::Cell,
    hash::{Hash, Hasher},
};

use rsgc::{
    Gc, Mutation, Trace, alloc::array::Array, barrier, cell::Lock, collection::Visitor,
    sync::monitor::Monitor,
};

use crate::runtime::Context;

use super::*;
#[derive(Debug, Clone, PartialEq, Eq, Copy, Trace)]
#[collect(no_drop)]
pub enum HashTableType {
    Eq,
    Eqv,
    Equal,
    String,
    Generic,
}
use simplehash::murmur::MurmurHasher64;

impl HashTableType {
    pub fn hash<'gc>(self, value: Value<'gc>) -> u64 {
        let mut hasher = MurmurHasher64::new(5381);
        match self {
            HashTableType::Eq => value.hash(&mut hasher),
            HashTableType::Eqv => EqvHash(value).hash(&mut hasher),
            HashTableType::Equal => EqualHash(value).hash(&mut hasher),
            HashTableType::String => {
                if value.is::<Str>() {
                    let s = value.downcast::<Str<'gc>>();
                    s.hash(&mut hasher);
                } else {
                    value.hash(&mut hasher);
                }
            }
            HashTableType::Generic => unreachable!(),
        }

        hasher.finish()
    }

    pub fn equal<'gc>(self, lhs: Value<'gc>, rhs: Value<'gc>) -> bool {
        match self {
            Self::Eq => lhs == rhs,
            Self::Eqv => lhs.eqv(rhs),
            Self::Equal => lhs.r5rs_equal(rhs),
            Self::String => {
                if lhs.is::<Str>() && rhs.is::<Str>() {
                    let lhs_str = lhs.downcast::<Str<'gc>>();
                    let rhs_str = rhs.downcast::<Str<'gc>>();
                    lhs_str == rhs_str
                } else if lhs.is::<Symbol>() && rhs.is::<Symbol>() {
                    let lhs_sym = lhs.downcast::<Symbol<'gc>>();
                    let rhs_sym = rhs.downcast::<Symbol<'gc>>();
                    lhs_sym == rhs_sym
                } else {
                    false
                }
            }
            _ => unreachable!(),
        }
    }
}

#[repr(C, align(8))]
pub struct HashTable<'gc> {
    #[allow(dead_code)]
    hdr: ScmHeader,
    inner: Monitor<InnerHashTable<'gc>>,
}

unsafe impl<'gc> Trace for HashTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.inner.get_mut().trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        unsafe {
            self.inner.get_mut().process_weak_refs(weak_processor);
        }
    }
}

pub struct EqvHash<'gc>(pub Value<'gc>);

impl<'gc> Hash for EqvHash<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let val = self.0;

        if val.is_number() {
            if val.is_inline_number() {
                state.write_u64(val.raw_i64() as u64);
            } else {
                let n = val.number().unwrap();
                n.hash(state);
            }
        } else {
            val.hash(state);
        }
    }
}

pub struct EqualHash<'gc>(pub Value<'gc>);

impl<'gc> Hash for EqualHash<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let val = self.0;

        if val.is_number() {
            if val.is_inline_number() {
                state.write_u64(val.raw_i64() as u64);
            } else {
                let n = val.number().unwrap();
                n.hash(state);
            }
        } else if val.is::<Str>() {
            let s = val.downcast::<Str<'gc>>();
            s.hash(state);
        } else if val.is::<Symbol>() {
            let s = val.downcast::<Symbol<'gc>>();
            s.hash(state);
        } else if val.is::<Vector>() {
            let v = val.downcast::<Vector<'gc>>();
            for i in 0..v.len() {
                v[i].get().hash(state);
            }
        } else if val.is::<Pair>() {
            let p = val.downcast::<Pair>();
            p.car().hash(state);
            p.cdr().hash(state);
        } else if val.is::<ByteVector>() {
            let p = val.downcast::<ByteVector>();
            (**p).hash(state);
        } else {
            val.hash(state);
        }
    }
}

#[derive(Trace)]
#[collect(no_drop)]
struct Entry<'gc> {
    key: Value<'gc>,
    value: Lock<Value<'gc>>,
    hash: u64,
    next: Lock<Option<Gc<'gc, Entry<'gc>>>>,
}

type MaybeEntry<'gc> = Option<Gc<'gc, Entry<'gc>>>;

#[derive(Trace)]
#[collect(no_drop)]
pub struct InnerHashTable<'gc> {
    table: Lock<Gc<'gc, Array<Lock<MaybeEntry<'gc>>>>>,
    count: Cell<usize>,
    threshold: Cell<usize>,
    load_factor: f64,
    mod_count: Cell<usize>,
    pub typ: HashTableType,
}

impl<'gc> HashTable<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        typ: HashTableType,
        initial_capacity: usize,
        load_factor: f64,
    ) -> Gc<'gc, Self> {
        let initial_capacity = initial_capacity.min(1);
        let initial_capacity = (initial_capacity == 0)
            .then_some(8)
            .unwrap_or(initial_capacity);
        if load_factor <= 0.0 {
            panic!("Load factor must be greater than 0.0");
        }

        let table = Array::with(mc, initial_capacity, |_, _| Lock::new(None));

        let threshold = (initial_capacity as f64 * load_factor).ceil() as usize;

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::HASHTABLE.bits() as _);

        let this = Gc::new(
            mc,
            Self {
                hdr,
                inner: Monitor::new(InnerHashTable {
                    table: Lock::new(table),
                    count: Cell::new(0),
                    threshold: Cell::new(threshold),
                    load_factor,
                    mod_count: Cell::new(0),
                    typ,
                }),
            },
        );

        this
    }

    fn rehash(inner: &Write<InnerHashTable<'gc>>, ctx: Context<'gc>) {
        let old_capacity = inner.table.get().len();
        let old_table = inner.table.get();

        let new_capacity = (old_capacity << 1) * 2;

        let new_map = Array::with(&ctx, new_capacity, |_, _| Lock::new(None));

        inner.mod_count.set(inner.mod_count.get() + 1);
        inner
            .threshold
            .set((new_capacity as f64 * inner.load_factor).ceil() as usize);
        barrier::field!(inner, InnerHashTable, table)
            .unlock()
            .set(new_map);

        for i in (0..old_capacity).rev() {
            let mut old = old_table[i].get();

            while let Some(entry) = old {
                old = entry.next.get();
                let index = (entry.hash % new_capacity as u64) as usize;
                let wentry = Gc::write(&ctx, entry);
                barrier::field!(wentry, Entry, next)
                    .unlock()
                    .set(new_map[index].get());
                Gc::write(&ctx, new_map)[index].unlock().set(Some(entry));
            }
        }
    }

    fn add_entry(
        this: &Write<InnerHashTable<'gc>>,
        ctx: Context<'gc>,
        mut hash: u64,
        key: Value<'gc>,
        value: Value<'gc>,
        mut index: usize,
    ) {
        let inner = this;
        if inner.count.get() >= inner.threshold.get() {
            Self::rehash(inner, ctx);
            hash = inner.typ.hash(key);
            index = (inner.typ.hash(key) % inner.table.get().len() as u64) as usize;
        }

        let tab = Gc::write(&ctx, inner.table.get());
        let e = tab[index].get();

        tab[index].unlock().set(Some(Gc::new(
            &ctx,
            Entry {
                hash,
                key,
                value: Lock::new(value),
                next: Lock::new(e),
            },
        )));
        inner.count.set(inner.count.get() + 1);
        inner.mod_count.set(inner.mod_count.get() + 1);
    }

    pub fn put(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        key: impl IntoValue<'gc>,
        value: impl IntoValue<'gc>,
    ) -> Option<Value<'gc>> {
        let key = key.into_value(ctx);
        let value = value.into_value(ctx);

        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let hash = guard.typ.hash(key);
        let index = (hash % guard.table.get().len() as u64) as usize;

        let mut e = guard.table.get()[index].get();

        while let Some(entry) = e {
            if entry.hash == hash && guard.typ.equal(entry.key, key) {
                let old_value = entry.value.get();
                barrier::field!(Gc::write(&ctx, entry), Entry, value)
                    .unlock()
                    .set(value);
                return Some(old_value);
            }
            e = entry.next.get();
        }

        Self::add_entry(
            unsafe { &Write::assume(&*guard) },
            ctx,
            hash,
            key,
            value,
            index,
        );

        None
    }

    pub fn remove(self: Gc<'gc, Self>, ctx: Context<'gc>, key: Value<'gc>) -> Option<Value<'gc>> {
        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let hash = guard.typ.hash(key);
        let index = (hash % guard.table.get().len() as u64) as usize;

        let mut e = guard.table.get()[index].get();
        let mut prev: Option<Gc<'gc, Entry<'gc>>> = None;

        while let Some(entry) = e {
            if entry.hash == hash && guard.typ.equal(entry.key, key) {
                if let Some(prev_entry) = prev {
                    barrier::field!(Gc::write(&ctx, prev_entry), Entry, next)
                        .unlock()
                        .set(entry.next.get());
                } else {
                    Gc::write(&ctx, guard.table.get())[index]
                        .unlock()
                        .set(entry.next.get());
                }
                guard.count.set(guard.count.get() - 1);
                guard.mod_count.set(guard.mod_count.get() + 1);
                return Some(entry.value.get());
            }
            prev = Some(entry);
            e = entry.next.get();
        }

        None
    }

    pub fn clear(self: Gc<'gc, Self>, ctx: Context<'gc>) {
        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let table = Gc::write(&ctx, guard.table.get());
        for i in 0..table.len() {
            table[i].unlock().set(None);
        }
        guard.count.set(0);
        guard.mod_count.set(guard.mod_count.get() + 1);
    }

    pub fn get(&self, ctx: Context<'gc>, key: impl IntoValue<'gc>) -> Option<Value<'gc>> {
        let key = key.into_value(ctx);
        let guard = self.inner.lock();
        if guard.table.get().len() == 0 {
            return None;
        }
        let hash = guard.typ.hash(key);
        let index = (hash % guard.table.get().len() as u64) as usize;

        let mut e = guard.table.get()[index].get();

        while let Some(entry) = e {
            if entry.hash == hash && guard.typ.equal(entry.key, key) {
                return Some(entry.value.get());
            }
            e = entry.next.get();
        }

        None
    }

    pub fn get_or_insert<F>(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        key: impl IntoValue<'gc>,
        default: F,
    ) -> (bool, Value<'gc>)
    where
        F: FnOnce(Context<'gc>) -> Value<'gc>,
    {
        let key = key.into_value(ctx);

        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let hash = guard.typ.hash(key);
        let index = (hash % guard.table.get().len() as u64) as usize;

        let mut e = guard.table.get()[index].get();

        while let Some(entry) = e {
            if entry.hash == hash && guard.typ.equal(entry.key, key) {
                return (false, entry.value.get());
            }
            e = entry.next.get();
        }
        let default = default(ctx);

        Self::add_entry(
            unsafe { &Write::assume(&*guard) },
            ctx,
            hash,
            key,
            default,
            index,
        );

        (true, default)
    }

    pub fn contains_key(&self, ctx: Context<'gc>, key: Value<'gc>) -> bool {
        self.get(ctx, key).is_some()
    }

    pub fn contains_value(&self, value: Value<'gc>) -> bool {
        let guard = self.inner.lock();
        for i in 0..guard.table.get().len() {
            let mut e = guard.table.get()[i].get();
            while let Some(entry) = e {
                if entry.value.get() == value {
                    return true;
                }
                e = entry.next.get();
            }
        }
        false
    }

    /// Iterates over the entries in the hash table.
    ///
    /// Returns an iterator that yields tuples of (key, value). All modifications
    /// to the hash table during iteration are not guaranteed to be reflected in the
    /// iterator.
    pub fn iter(self: Gc<'gc, Self>) -> impl Iterator<Item = (Value<'gc>, Value<'gc>)> {
        let guard = self.inner.lock();

        HashTableIter {
            entry: None,
            index: 0,
            table: guard.table.get(),
        }
    }

    pub fn keys(self: Gc<'gc, Self>) -> impl Iterator<Item = Value<'gc>> {
        let guard = self.inner.lock();

        HashTableKeys {
            iter: HashTableIter {
                entry: None,
                index: 0,
                table: guard.table.get(),
            },
        }
    }

    pub fn values(self: Gc<'gc, Self>) -> impl Iterator<Item = Value<'gc>> {
        let guard = self.inner.lock();

        HashTableValues {
            iter: HashTableIter {
                entry: None,
                index: 0,
                table: guard.table.get(),
            },
        }
    }

    pub fn lock(&'gc self) -> rsgc::sync::monitor::MonitorGuard<'gc, InnerHashTable<'gc>> {
        self.inner.lock()
    }

    pub fn len(&self) -> usize {
        self.inner.lock().count.get()
    }
}

pub struct HashTableIter<'gc> {
    table: Gc<'gc, Array<Lock<MaybeEntry<'gc>>>>,
    index: usize,
    entry: Option<Gc<'gc, Entry<'gc>>>,
}

impl<'gc> Iterator for HashTableIter<'gc> {
    type Item = (Value<'gc>, Value<'gc>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.entry {
            self.entry = node.next.get();
        }

        if self.entry.is_none() {
            while let Some(bucket) = self.table.get(self.index) {
                self.index += 1;

                if let Some(head) = bucket.get() {
                    self.entry = Some(head);
                    break;
                }
            }
        }

        self.entry.map(|node| (node.key, node.value.get()))
    }
}

pub struct HashTableKeys<'gc> {
    iter: HashTableIter<'gc>,
}

impl<'gc> Iterator for HashTableKeys<'gc> {
    type Item = Value<'gc>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(key, _)| key)
    }
}

pub struct HashTableValues<'gc> {
    iter: HashTableIter<'gc>,
}

impl<'gc> Iterator for HashTableValues<'gc> {
    type Item = Value<'gc>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(_, value)| value)
    }
}

unsafe impl<'gc> Tagged for HashTable<'gc> {
    const TC8: TypeCode8 = TypeCode8::HASHTABLE;
}

pub const fn u64_hash(mut key: u64) -> u64 {
    key = (!key).wrapping_add(key << 21); // key = (key << 21) - key - 1;
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key = key ^ (key >> 28);
    key = key.wrapping_add(key << 31);
    key >>= 2;
    key
}

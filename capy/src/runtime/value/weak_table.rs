use std::{
    cell::Cell,
    hash::Hasher,
    sync::{Arc, Once, OnceLock},
};

use rsgc::{
    Gc, Rootable, Trace, Weak, WeakProcessor,
    alloc::array::Array,
    barrier::{self, Unlock, Write},
    cell::Lock,
    collection::Visitor,
    finalizer::FinalizationNotify,
    global::Global,
    mutator::Mutation,
    sync::monitor::Monitor,
};
use simplehash::MurmurHasher64;

use crate::runtime::{
    Context,
    vmthread::{VM_THREAD, VMThreadTask},
};

use super::{WeakValue, *};

/// Weak mapping between key and value.
///
/// For more information read [`GcEphemeron`](crate::rsgc::weak::GcEphemeron) documentation.
#[repr(C, align(8))]
pub struct WeakMapping<'gc> {
    #[allow(dead_code)]
    header: ScmHeader,
    pub(crate) _key: Value<'gc>,
    pub(crate) _value: Value<'gc>,
}

unsafe impl<'gc> Trace for WeakMapping<'gc> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor<'_>) {
        _visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        if self._key.is_bwp() {
            self._value = Value::bwp();
        } else {
            let key = unsafe { weak_processor.is_live_object(self._key.desc.ptr()) };
            if !key.is_null() {
                self._key.desc.ptr = key.to_address().to_mut_ptr();
                weak_processor.visitor().trace(&mut self._value);
            } else {
                self._key = Value::bwp();
                self._value = Value::bwp();
            }
        }
    }
}

unsafe impl<'gc> Tagged for WeakMapping<'gc> {
    const TC8: TypeCode8 = TypeCode8::WEAK_MAPPING;
    const TYPE_NAME: &'static str = "#<weak-mapping>";
}

impl<'gc> WeakMapping<'gc> {
    pub fn new(ctx: Context<'gc>, key: Value<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        assert!(key.is_cell(), "weak-mappings can only have cells as keys");
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::WEAK_MAPPING.bits() as _);
        let mapping = Gc::new(
            &ctx,
            Self {
                header: hdr,
                _key: key,
                _value: value,
            },
        );

        mapping
    }

    pub fn is_broken(&self) -> bool {
        self._key.is_bwp() || self._value.is_bwp()
    }

    pub fn key(&self, mc: &Mutation<'gc>) -> Value<'gc> {
        if self._key.is_bwp() {
            return Value::bwp();
        }
        unsafe {
            mc.raw_weak_reference_load(self._key.desc.ptr());
        }
        self._key
    }

    pub fn value(&self, mc: &Mutation<'gc>) -> Value<'gc> {
        if self._value.is_bwp() || !self._value.is_cell() {
            return self._value;
        }

        unsafe {
            mc.raw_weak_reference_load(self._value.desc.ptr());
        }

        self._value
    }
}

struct WeakEntry<'gc> {
    _key: WeakValue<'gc>,
    _value: Lock<Value<'gc>>,
    hash: u64,
    next: Lock<Option<Gc<'gc, WeakEntry<'gc>>>>,
}

impl<'gc> WeakEntry<'gc> {
    fn key(&self, mc: &Mutation<'gc>) -> Value<'gc> {
        self._key.get(mc)
    }

    fn value(&self, _mc: &Mutation<'gc>) -> Value<'gc> {
        self._value.get()
    }
}

unsafe impl<'gc> Trace for WeakEntry<'gc> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor<'_>) {
        _visitor.register_for_weak_processing();
        unsafe {
            self.next.trace(_visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        unsafe {
            self._key.process_weak_refs(weak_processor);
        }

        if self._key.is_broken() {
            unsafe { self._value.unlock_unchecked().set(Value::bwp()) };
        } else {
            let mut vis = weak_processor.visitor();
            vis.trace(&mut self._value);
        }

        weak_processor.process(&mut self.next);
    }
}

type Entry<'gc> = Gc<'gc, WeakEntry<'gc>>;

type Entries<'gc> = Gc<'gc, Array<Lock<Option<Entry<'gc>>>>>;

/// Weak table.
///
/// A weak table is a hash table that stores key-value pairs
/// as ephemerons which is equal to [`WeakMapping`].
#[repr(C, align(8))]
pub struct WeakTable<'gc> {
    #[allow(dead_code)]
    header: ScmHeader,
    inner: Monitor<WeakTableInner<'gc>>,
}

unsafe impl<'gc> Trace for WeakTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        unsafe {
            self.inner.get_mut().trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

struct WeakTableInner<'gc> {
    entries: Lock<Entries<'gc>>,
    pub count: Cell<usize>,
    threshold: Cell<usize>,
    load_factor: f64,
    mod_count: Cell<usize>,
}

unsafe impl<'gc> Trace for WeakTableInner<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        visitor.trace(&mut self.entries);
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut WeakProcessor) {}
}

fn make_hash<'gc>(key: Value<'gc>) -> u64 {
    let mut hasher = MurmurHasher64::new(5382);
    key.hash(&mut hasher);
    hasher.finish()
}

impl<'gc> WeakTable<'gc> {
    pub fn new(ctx: &Mutation<'gc>, initial_capacity: usize, load_factor: f64) -> Gc<'gc, Self> {
        assert!(
            initial_capacity > 0,
            "initial capacity must be greater than 0"
        );
        assert!(
            load_factor > 0.0 && load_factor < 1.0,
            "load factor must be in (0, 1)"
        );

        let entries = Array::with(&ctx, initial_capacity, |_, _| Lock::new(None));
        let inner = WeakTableInner {
            entries: Lock::new(entries),
            count: Cell::new(0),
            threshold: Cell::new((initial_capacity as f64 * load_factor) as usize),
            load_factor,
            mod_count: Cell::new(0),
        };

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::WEAKTABLE.bits() as _);

        let table = Gc::new(
            &ctx,
            Self {
                header: hdr,
                inner: Monitor::new(inner),
            },
        );

        ALL_WEAK_TABLES
            .get()
            .expect("Weak tables not initialized")
            .fetch(&ctx)
            .tables
            .lock()
            .push(Gc::downgrade(table));
        table
    }

    pub(crate) unsafe fn at_object(
        ctx: Context<'gc>,
        obj: GCObject,
        kvs: Vec<(Value<'gc>, Value<'gc>)>,
    ) {
        unsafe {
            let hdr = obj.to_address().to_mut_ptr::<ScmHeader>().read();
            let entries = Array::with(&ctx, 8, |_, _| Lock::new(None));
            let inner = WeakTableInner {
                entries: Lock::new(entries),
                count: Cell::new(0),
                threshold: Cell::new((8.0 * 0.75) as usize),
                load_factor: 0.75,
                mod_count: Cell::new(0),
            };

            obj.to_address()
                .to_mut_ptr::<WeakTable<'gc>>()
                .write(WeakTable {
                    header: hdr,
                    inner: Monitor::new(inner),
                });

            let ht: Gc<Self> = Gc::from_gcobj(obj);

            for (k, v) in kvs {
                ht.put(ctx, k, v);
            }

            ALL_WEAK_TABLES
                .get()
                .expect("Weak tables not initialized")
                .fetch(&ctx)
                .tables
                .lock()
                .push(Gc::downgrade(ht));
        }
    }

    fn rehash(inner: &Write<WeakTableInner<'gc>>, ctx: Context<'gc>) {
        let old_capacity = inner.entries.get().len();
        let old_table = inner.entries.get();

        let new_capacity = (old_capacity as f64 / inner.load_factor) as usize;

        let new_map = Array::with(&ctx, new_capacity, |_, _| Lock::new(None));

        inner.mod_count.set(inner.mod_count.get() + 1);
        inner
            .threshold
            .set((new_capacity as f64 * inner.load_factor) as usize);

        barrier::field!(inner, WeakTableInner, entries)
            .unlock()
            .set(new_map);

        for i in (0..old_capacity).rev() {
            let mut old = old_table[i].get();

            while let Some(entry) = old {
                old = entry.next.get();

                let index = (entry.hash % new_capacity as u64) as usize;
                let wentry = Gc::write(&ctx, entry);
                let key = entry.key(&ctx);
                if key.is_bwp() || !key.is_cell() {
                    continue;
                }
                barrier::field!(wentry, WeakEntry, next)
                    .unlock()
                    .set(new_map[index].get());
                Gc::write(&ctx, new_map)[index].unlock().set(Some(entry));
            }
        }
    }

    fn add_entry(
        this: &Write<WeakTableInner<'gc>>,
        ctx: Context<'gc>,
        mut hash: u64,
        key: Value<'gc>,
        value: Value<'gc>,
        mut index: usize,
    ) {
        let inner = this;
        if inner.count.get() >= inner.threshold.get() {
            Self::rehash(inner, ctx);
            hash = make_hash(key);
            index = (hash % inner.entries.get().len() as u64) as usize;
        }

        let tab = Gc::write(&ctx, inner.entries.get());
        let e = tab[index].get();

        tab[index].unlock().set(Some(Gc::new(
            &ctx,
            WeakEntry {
                hash,
                _key: WeakValue::from_value(key),
                _value: Lock::new(value),
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
        let hash = make_hash(key);
        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();

        while let Some(entry) = e {
            let ekey = entry.key(&ctx);
            if entry.hash == hash && ekey == key {
                let old_value = entry.value(&ctx);
                barrier::field!(Gc::write(&ctx, entry), WeakEntry, _value)
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
        drop(guard);

        None
    }

    pub fn remove(self: Gc<'gc, Self>, ctx: Context<'gc>, key: Value<'gc>) -> Option<Value<'gc>> {
        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let hash = make_hash(key);
        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();
        let mut prev: Option<Gc<'gc, WeakEntry<'gc>>> = None;

        while let Some(entry) = e {
            let ekey = entry.key(&ctx);
            if entry.hash == hash && ekey == key {
                if let Some(prev_entry) = prev {
                    barrier::field!(Gc::write(&ctx, prev_entry), WeakEntry, next)
                        .unlock()
                        .set(entry.next.get());
                } else {
                    Gc::write(&ctx, guard.entries.get())[index]
                        .unlock()
                        .set(entry.next.get());
                }
                guard.count.set(guard.count.get() - 1);
                guard.mod_count.set(guard.mod_count.get() + 1);
                return Some(entry._value.get());
            }
            prev = Some(entry);
            e = entry.next.get();
        }

        None
    }

    pub fn vacuum(self: Gc<'gc, Self>, mc: &Mutation<'gc>) {
        let guard = self.inner.lock();
        Gc::write(mc, self);
        let table = Gc::write(mc, guard.entries.get());
        for i in 0..table.len() {
            let mut e = table[i].get();
            let mut prev = None;
            while let Some(entry) = e {
                let ekey = entry.key(mc);
                if ekey.is_bwp() || !ekey.is_cell() {
                    if let Some(prev_entry) = prev {
                        barrier::field!(Gc::write(mc, prev_entry), WeakEntry, next)
                            .unlock()
                            .set(entry.next.get());
                    } else {
                        table[i].unlock().set(entry.next.get());
                    }

                    guard.count.set(guard.count.get() - 1);
                } else {
                    prev = Some(entry);
                }
                e = entry.next.get();
            }
        }
        guard.mod_count.set(guard.mod_count.get() + 1);
    }

    pub fn clear(self: Gc<'gc, Self>, ctx: Context<'gc>) {
        let guard = self.inner.lock();
        Gc::write(&ctx, self);
        let table = Gc::write(&ctx, guard.entries.get());
        for i in 0..table.len() {
            table[i].unlock().set(None);
        }
        guard.count.set(0);
        guard.mod_count.set(guard.mod_count.get() + 1);
    }

    pub fn get(&self, ctx: Context<'gc>, key: impl IntoValue<'gc>) -> Option<Value<'gc>> {
        let key = key.into_value(ctx);
        let guard = self.inner.lock();
        let hash = make_hash(key);

        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();

        while let Some(entry) = e {
            let ekey = entry.key(&ctx);
            if entry.hash == hash && ekey == key {
                return Some(entry.value(&ctx));
            }
            e = entry.next.get();
        }

        None
    }

    pub fn contains_key(&self, ctx: Context<'gc>, key: Value<'gc>) -> bool {
        self.get(ctx, key).is_some()
    }

    pub fn contains_value(&self, value: Value<'gc>) -> bool {
        let guard = self.inner.lock();
        for i in 0..guard.entries.get().len() {
            let mut e = guard.entries.get()[i].get();
            while let Some(entry) = e {
                if entry._value.get() == value {
                    return true;
                }
                e = entry.next.get();
            }
        }
        false
    }

    pub fn fold(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        mut f: impl FnMut(Value<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
        mut init: Value<'gc>,
    ) -> Value<'gc> {
        self.vacuum(&ctx);

        let mut alist = Value::null();
        let guard = self.inner.lock();

        for k in 0..guard.entries.get().len() {
            let mut entry = guard.entries.get()[k].get();

            while let Some(e) = entry {
                let ekey = e.key(&ctx);
                if !ekey.is_bwp() && !e.value(&ctx).is_bwp() {
                    alist = Value::acons(ctx, ekey, e.value(&ctx), alist);
                }
                entry = e.next.get();
            }
        }

        drop(guard);

        while !alist.is_null() {
            init = f(alist.caar(), alist.cdar(), init);
            alist = alist.cdr();
        }

        init
    }

    pub fn for_each(
        self: Gc<'gc, Self>,
        ctx: Context<'gc>,
        mut f: impl FnMut(Value<'gc>, Value<'gc>),
    ) {
        self.vacuum(&ctx);

        let guard = self.inner.lock();

        for k in 0..guard.entries.get().len() {
            let mut entry = guard.entries.get()[k].get();

            while let Some(e) = entry {
                let ekey = e.key(&ctx);
                if !ekey.is_bwp() && !e.value(&ctx).is_bwp() {
                    f(ekey, e.value(&ctx));
                }
                entry = e.next.get();
            }
        }
    }

    pub fn copy(self: Gc<'gc, Self>, ctx: Context<'gc>) -> Gc<'gc, Self> {
        let guard = self.inner.lock();
        let new_table = WeakTable::new(&ctx, guard.entries.get().len(), guard.load_factor);

        for i in 0..guard.entries.get().len() {
            let mut e = guard.entries.get()[i].get();

            while let Some(entry) = e {
                let ekey = entry.key(&ctx);
                if !ekey.is_bwp() && !entry.value(&ctx).is_bwp() {
                    new_table.put(ctx, ekey, entry.value(&ctx));
                }
                e = entry.next.get();
            }
        }

        new_table
    }

    pub fn len(&self) -> usize {
        let guard = self.inner.lock();
        guard.count.get()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub type WeakTableRef<'gc> = Gc<'gc, WeakTable<'gc>>;

unsafe impl<'gc> Tagged for WeakTable<'gc> {
    const TC8: TypeCode8 = TypeCode8::WEAKTABLE;
    const TYPE_NAME: &'static str = "#<weak-table>";
}

struct AllWeakTables<'gc> {
    tables: Monitor<Vec<Weak<'gc, WeakTable<'gc>>>>,
}

unsafe impl<'gc> Send for AllWeakTables<'gc> {}
unsafe impl<'gc> Sync for AllWeakTables<'gc> {}

static ALL_WEAK_TABLES: OnceLock<Global<Rootable!(AllWeakTables<'_>)>> = OnceLock::new();

unsafe impl<'gc> Trace for AllWeakTables<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let tables = self.tables.get_mut();
        tables.retain_mut(|table| {
            unsafe {
                table.process_weak_refs(weak_processor);
            }
            !table.is_broken()
        });
    }
}
struct WeakTableCleanup;

pub fn vacuum_weak_tables<'gc>(mc: &Mutation<'gc>) {
    let Some(all_weak_tables) = ALL_WEAK_TABLES.get() else {
        return;
    };
    let all_weak_tables = all_weak_tables.fetch(mc);

    let guard = all_weak_tables.tables.lock();
    let all_weak_tables = guard;

    for table in all_weak_tables.iter() {
        if let Some(table) = table.upgrade(mc) {
            table.vacuum(mc);
        }
    }
}

impl FinalizationNotify for WeakTableCleanup {
    fn notify_in_processing(&self) {}

    fn schedule(&self) {
        VM_THREAD.schedule_task(VMThreadTask::VacuumWeakTables);
    }
}

static ONCE: Once = Once::new();

pub fn init_weak_tables<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        let all_weak_tables = AllWeakTables {
            tables: Monitor::new(Vec::new()),
        };
        let all_weak_tables = Global::new(all_weak_tables);
        let _ = ALL_WEAK_TABLES.set(all_weak_tables);
        mc.finalizers().add_notifier(Arc::new(WeakTableCleanup));
    });
}

#[cfg(test)]
mod tests {
    #![allow(unsafe_op_in_unsafe_fn)]
    use crate::runtime::Scheme;

    use super::*;

    #[test]
    fn test_weak_mappings() {
        struct Rootset<'gc> {
            tmps: Vec<Value<'gc>>,
            wmaps: Vec<Gc<'gc, WeakMapping<'gc>>>,
            weaks: Vec<WeakValue<'gc>>,
        }

        unsafe impl<'gc> Trace for Rootset<'gc> {
            unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
                visitor.register_for_weak_processing();
                for tmp in &mut self.tmps {
                    tmp.trace(visitor);
                }
                for wmap in &mut self.wmaps {
                    wmap.trace(visitor);
                }
                for weak in &mut self.weaks {
                    weak.trace(visitor);
                }
            }

            unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
                for tmp in &mut self.tmps {
                    tmp.process_weak_refs(weak_processor);
                }
                for wmap in &mut self.wmaps {
                    wmap.process_weak_refs(weak_processor);
                }
                for weak in &mut self.weaks {
                    weak.process_weak_refs(weak_processor);
                }
            }
        }

        static ROOTSET: OnceLock<Global<Rootable!(Rootset<'_>)>> = OnceLock::new();

        let scm = Scheme::new();

        scm.enter(|ctx| {
            let k1 = Str::new(&ctx, "key1", false);
            let k2 = Str::new(&ctx, "key2", false);

            let v1 = Str::new(&ctx, "value1", false);
            let v2 = Str::new(&ctx, "value2", false);

            let wmap1 = WeakMapping::new(ctx, k1.into_value(ctx), v1.into_value(ctx));
            let wmap2 = WeakMapping::new(ctx, k2.into_value(ctx), v2.into_value(ctx));

            let rootset = Rootset {
                tmps: vec![k1.into_value(ctx)],
                wmaps: vec![wmap1, wmap2],
                weaks: vec![v1.into_value(ctx).into(), v2.into_value(ctx).into()],
            };

            let _ = ROOTSET.set(Global::new(rootset));
        });

        scm.mutator.collect_garbage();

        scm.enter(|ctx| {
            let rootset = ROOTSET.get().unwrap().fetch(&ctx);

            assert!(rootset.weaks[1].is_broken());
        })
    }

    #[test]
    fn test_weak_table() {
        env_logger::init();
        struct Rootset<'gc> {
            tmps: Vec<Value<'gc>>,
            table: Gc<'gc, WeakTable<'gc>>,
            //k1_weak: WeakValue<'gc>,
        }

        unsafe impl<'gc> Trace for Rootset<'gc> {
            unsafe fn trace(&mut self, visitor: &mut Visitor) {
                for tmp in &mut self.tmps {
                    tmp.trace(visitor);
                }
                self.table.trace(visitor);
                //self.k1_weak.trace(visitor);
            }

            unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
                for tmp in &mut self.tmps {
                    tmp.process_weak_refs(weak_processor);
                }
                self.table.process_weak_refs(weak_processor);
                //self.k1_weak.process_weak_refs(weak_processor);
            }
        }

        static ROOTSET: OnceLock<Global<Rootable!(Rootset<'_>)>> = OnceLock::new();

        let scm = Scheme::new();
        scm.enter(|mc| init_weak_tables(&mc));

        scm.enter(|ctx| {
            let table = WeakTable::new(&ctx, 8, 0.75);
            let k1 = Str::new(&ctx, "key1", false);
            let v1 = Str::new(&ctx, "value1", false);
            let k2 = Str::new(&ctx, "key2", false);
            let v2 = Str::new(&ctx, "value2", false);
            let k3 = Str::new(&ctx, "key3", false); // This key will not be rooted
            let v3 = Str::new(&ctx, "value3", false);

            table.put(ctx, k1, v1);
            table.put(ctx, k2, v2);
            table.put(ctx, k3, v3);

            assert!(matches!(table.get(ctx, k1), Some(val) if val == v1.into_value(ctx)));
            assert!(matches!(table.get(ctx, k2), Some(val) if val == v2.into_value(ctx)));
            assert!(matches!(table.get(ctx, k3), Some(val) if val == v3.into_value(ctx)));
            assert!(table.contains_key(ctx, k1.into_value(ctx)));
            assert!(table.contains_value(v1.into_value(ctx)));
            println!("k1: {:p}, table {:p}", k1, table);
            assert!(Value::from(table).is_cell());
            assert!(Value::from(k1).is_cell());
            let rootset = Rootset {
                tmps: vec![
                    k1.into_value(ctx),
                    v1.into_value(ctx),
                    k2.into_value(ctx),
                    v2.into_value(ctx),
                    v3.into_value(ctx),
                ], // k3's value is rooted, but k3 itself is not in tmps
                table,
            };
            let _ = ROOTSET.set(Global::new(rootset));
        });

        scm.mutator.collect_garbage();
        scm.enter(|mc| vacuum_weak_tables(&mc));

        scm.enter(|ctx| {
            let rootset_global = ROOTSET.get().unwrap();
            let rootset = rootset_global.fetch(&ctx);
            let table = rootset.table;
            table.vacuum(&ctx);
            // assert!(!rootset.k1_weak.is_broken());

            let k1_rooted = rootset.tmps[0];
            // For k3, its original Gc<String> object might be collected if not otherwise rooted.
            // We create a new string for lookup. The entry should be gone if the key was collected.
            let k3_lookup = Str::new(&ctx, "key3", false);

            assert!(table.get(ctx, k1_rooted).is_some());
            // The original k3 Gc object was not in rootset.tmps. If it was collected, the entry is gone.
            assert!(
                table.get(ctx, k3_lookup).is_none(),
                "Entry for k3 should be gone as k3 was not strongly rooted"
            );

            // Test remove
            let k1_val_expected = rootset.tmps[1]; // This is v1
            let removed_val_opt = table.remove(ctx, k1_rooted);
            assert!(matches!(removed_val_opt, Some(v) if v == k1_val_expected));
            assert!(table.get(ctx, k1_rooted).is_none());

            // Test clear
            table.clear(ctx);
            let k2_rooted = rootset.tmps[2];
            assert!(table.get(ctx, k2_rooted).is_none());
            assert_eq!(table.inner.lock().count.get(), 0);
        });
    }
}

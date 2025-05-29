use std::{
    cell::{Cell, RefCell},
    hash::Hasher,
    mem::transmute,
    sync::{Arc, Once, OnceLock},
};

use rsgc::{
    EnsureGCInfo, Gc, Rootable, Trace,
    alloc::array::Array,
    barrier::{self, Write},
    cell::Lock,
    context::Mutation,
    finalizer::FinalizationNotifier,
    mutator::Global,
    vmkit::{mmtk::util::ObjectReference, sync::Monitor},
};
use simplehash::MurmurHasher64;

use crate::runtime::{
    Context,
    vmthread::{VM_THREAD, VMThreadTask},
};

use super::{values::WeakValue, *};

/// Weak mapping between key and value.
///
/// For more information read [`GcEphemeron`](crate::rsgc::weak::GcEphemeron) documentation.
pub struct WeakMapping<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
}

unsafe impl<'gc> Trace for WeakMapping<'gc> {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {
        _visitor.register_for_weak_processing();
    }
    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        if self.key.is_bwp() {
            self.value = Value::bwp();
        } else {
            let key: ObjectReference = unsafe { transmute(self.key) };
            if key.is_reachable() {
                println!("key in wmap is reachable");
                let nkey = key.get_forwarded_object().unwrap_or(key);

                self.key = unsafe { transmute(nkey) };
                self.value.trace(&mut weak_processor.visitor());
            } else {
                println!("key in wmap is broken");
                self.key = Value::bwp();
                self.value = Value::bwp();
            }
        }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for WeakMapping<'gc> {}

unsafe impl<'gc> Tagged for WeakMapping<'gc> {
    const TC8: TypeCode8 = TypeCode8::WEAK_MAPPING;
}

impl<'gc> WeakMapping<'gc> {
    pub fn new(ctx: Context<'gc>, key: Value<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        assert!(key.is_cell(), "weak-mappings can only have cells as keys");
        let mapping = Gc::new(&ctx, Self { key, value });
        mapping.set_user_header(TypeCode8::WEAK_MAPPING.into());

        mapping
    }

    pub fn is_broken(&self) -> bool {
        self.key.is_bwp() || self.value.is_bwp()
    }

    pub fn key(&self) -> Value<'gc> {
        self.key
    }

    pub fn value(&self) -> Value<'gc> {
        self.value
    }
}

struct WeakEntry<'gc> {
    key: WeakValue<'gc>,
    value: Value<'gc>,
    hash: u64,
    next: Lock<Option<Gc<'gc, WeakEntry<'gc>>>>,
}

unsafe impl<'gc> EnsureGCInfo<'gc> for WeakEntry<'gc> {}

unsafe impl<'gc> Trace for WeakEntry<'gc> {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {
        _visitor.register_for_weak_processing();
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let orig = self.key;
        self.key.process_weak_refs(weak_processor);

        if self.key.is_broken() {
            println!("broken {:x}", orig.0.raw_i64());
            self.value = Value::bwp();
        } else {
            let mut vis = weak_processor.visitor();
            self.value.trace(&mut vis);

            println!("key {:x} is live", self.key.0.raw_i64());
        }
    }
}

type Entry<'gc> = Gc<'gc, WeakEntry<'gc>>;

type Entries<'gc> = Gc<'gc, Array<Lock<Option<Entry<'gc>>>>>;

/// Weak table.
///
/// A weak table is a hash table that stores key-value pairs
/// as ephemerons which is equal to [`WeakMapping`].
pub struct WeakTable<'gc> {
    inner: Monitor<WeakTableInner<'gc>>,
}

unsafe impl<'gc> Trace for WeakTable<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.inner.get_mut().trace(visitor);
    }

    fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

unsafe impl<'gc> EnsureGCInfo<'gc> for WeakTable<'gc> {}

struct WeakTableInner<'gc> {
    entries: Lock<Entries<'gc>>,
    count: Cell<usize>,
    threshold: Cell<usize>,
    load_factor: f64,
    mod_count: Cell<usize>,
}

unsafe impl<'gc> Trace for WeakTableInner<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.entries.trace(visitor);
    }
}

fn make_hash<'gc>(key: Value<'gc>) -> u64 {
    let mut hasher = MurmurHasher64::new(5382);
    key.hash(&mut hasher);
    hasher.finish()
}

impl<'gc> WeakTable<'gc> {
    pub fn new(ctx: Context<'gc>, initial_capacity: usize, load_factor: f64) -> Gc<'gc, Self> {
        assert!(
            initial_capacity > 0,
            "initial capacity must be greater than 0"
        );
        assert!(
            load_factor > 0.0 && load_factor < 1.0,
            "load factor must be in (0, 1)"
        );

        let entries = Array::with_fn(&ctx, initial_capacity, |_| Lock::new(None));
        let inner = WeakTableInner {
            entries: Lock::new(entries),
            count: Cell::new(0),
            threshold: Cell::new((initial_capacity as f64 * load_factor) as usize),
            load_factor,
            mod_count: Cell::new(0),
        };

        let table = Gc::new(
            &ctx,
            Self {
                inner: Monitor::new(inner),
            },
        );
        table.set_user_header(TypeCode8::WEAKTABLE.into());

        ALL_WEAK_TABLES
            .get()
            .expect("Weak tables not initialized")
            .fetch(&ctx)
            .tables
            .lock_no_handshake()
            .borrow_mut()
            .push(Gc::downgrade(table));
        table
    }

    fn rehash(inner: &Write<WeakTableInner<'gc>>, ctx: Context<'gc>) {
        let old_capacity = inner.entries.get().len();
        let old_table = inner.entries.get();

        let new_capacity = (old_capacity as f64 * inner.load_factor) as usize;

        let new_map = Array::with_fn(&ctx, new_capacity, |_| Lock::new(None));

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

                if entry.key.is_broken() || !entry.key.0.is_cell() {
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
                key: WeakValue(key),
                value,
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
        println!("put {:x}->{:x}", key.raw_i64(), value.raw_i64());
        let guard = self.inner.lock_no_handshake();
        Gc::write(&ctx, self);
        let hash = make_hash(key);
        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();

        while let Some(entry) = e {
            if entry.hash == hash && entry.key.0 == key {
                let old_value = entry.value;
                barrier::field!(Gc::write(&ctx, entry), WeakEntry, value).write(value);
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
        let guard = self.inner.lock_no_handshake();
        Gc::write(&ctx, self);
        let hash = make_hash(key);
        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();
        let mut prev: Option<Gc<'gc, WeakEntry<'gc>>> = None;

        while let Some(entry) = e {
            if entry.hash == hash && entry.key.0 == key {
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
                return Some(entry.value);
            }
            prev = Some(entry);
            e = entry.next.get();
        }

        None
    }

    pub fn vacuum(self: Gc<'gc, Self>, mc: &Mutation<'gc>) {
        let guard = self.inner.lock_no_handshake();
        Gc::write(mc, self);
        let table = Gc::write(mc, guard.entries.get());
        for i in 0..table.len() {
            let mut e = table[i].get();
            let mut prev = None;
            while let Some(entry) = e {
                if entry.key.is_broken() || !entry.key.0.is_cell() {
                    if let Some(prev_entry) = prev {
                        barrier::field!(Gc::write(mc, prev_entry), WeakEntry, next)
                            .unlock()
                            .set(entry.next.get());
                    } else {
                        table[i].unlock().set(entry.next.get());
                    }
                    println!(
                        "removing broken entry {:x}->{:x}",
                        entry.key.0.raw_i64(),
                        entry.value.raw_i64()
                    );
                    guard.count.set(guard.count.get() - 1);
                } else {
                    println!(
                        "keeping entry {:x}->{:x}",
                        entry.key.0.raw_i64(),
                        entry.value.raw_i64()
                    );
                    prev = Some(entry);
                }
                e = entry.next.get();
            }
        }
        guard.mod_count.set(guard.mod_count.get() + 1);
    }

    pub fn clear(self: Gc<'gc, Self>, ctx: Context<'gc>) {
        let guard = self.inner.lock_no_handshake();
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
        let guard = self.inner.lock_no_handshake();
        let hash = make_hash(key);
        let index = (hash % guard.entries.get().len() as u64) as usize;

        let mut e = guard.entries.get()[index].get();

        while let Some(entry) = e {
            if entry.hash == hash && entry.key.0 == key {
                return Some(entry.value);
            }
            e = entry.next.get();
        }

        None
    }

    pub fn contains_key(&self, ctx: Context<'gc>, key: Value<'gc>) -> bool {
        self.get(ctx, key).is_some()
    }

    pub fn contains_value(&self, value: Value<'gc>) -> bool {
        let guard = self.inner.lock_no_handshake();
        for i in 0..guard.entries.get().len() {
            let mut e = guard.entries.get()[i].get();
            while let Some(entry) = e {
                if entry.value == value {
                    return true;
                }
                e = entry.next.get();
            }
        }
        false
    }
}

unsafe impl<'gc> Tagged for WeakTable<'gc> {
    const TC8: TypeCode8 = TypeCode8::WEAKTABLE;
}

struct AllWeakTables<'gc> {
    tables: Monitor<RefCell<Vec<GcWeak<'gc, WeakTable<'gc>>>>>,
}

unsafe impl<'gc> Send for AllWeakTables<'gc> {}
unsafe impl<'gc> Sync for AllWeakTables<'gc> {}

static ALL_WEAK_TABLES: OnceLock<Global<Rootable!(AllWeakTables<'_>)>> = OnceLock::new();

unsafe impl<'gc> Trace for AllWeakTables<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        visitor.register_for_weak_processing();
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let tables = self.tables.get_mut();
        tables.get_mut().retain_mut(|table| {
            table.process_weak_refs(weak_processor);
            !table.is_broken()
        });
    }
}
struct WeakTableCleanup;

pub(crate) fn vacuum_weak_tables<'gc>(mc: &Mutation<'gc>) {
    let Some(all_weak_tables) = ALL_WEAK_TABLES.get() else {
        return;
    };
    let all_weak_tables = all_weak_tables.fetch(mc);

    let guard = all_weak_tables.tables.lock_no_handshake();
    let all_weak_tables = guard.borrow_mut();

    for table in all_weak_tables.iter() {
        if let Some(table) = table.upgrade() {
            table.vacuum(mc);
        }
    }
}

impl FinalizationNotifier for WeakTableCleanup {
    fn notify_in_processing(&self) {}

    fn schedule(&self) {
        VM_THREAD.schedule_task(VMThreadTask::VacuumWeakTables);
    }
}

static ONCE: Once = Once::new();

pub fn init_weak_tables<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        let all_weak_tables = AllWeakTables {
            tables: Monitor::new(RefCell::new(Vec::new())),
        };
        let all_weak_tables = Global::new(all_weak_tables);
        let _ = ALL_WEAK_TABLES.set(all_weak_tables);
        mc.finalizers().add_notifier(Arc::new(WeakTableCleanup));
    });
}

#[cfg(test)]
mod tests {

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
            fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
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

            fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
                println!("Processing weak references in rootset");
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
            let k1 = String::new(&ctx, "key1", false);
            let k2 = String::new(&ctx, "key2", false);

            let v1 = String::new(&ctx, "value1", false);
            let v2 = String::new(&ctx, "value2", false);

            let wmap1 = WeakMapping::new(ctx, k1.into_value(ctx), v1.into_value(ctx));
            let wmap2 = WeakMapping::new(ctx, k2.into_value(ctx), v2.into_value(ctx));

            let rootset = Rootset {
                tmps: vec![k1.into_value(ctx)],
                wmaps: vec![wmap1, wmap2],
                weaks: vec![v1.into_value(ctx).into(), v2.into_value(ctx).into()],
            };

            let _ = ROOTSET.set(Global::new(rootset));
        });

        scm.mutator.request_gc();

        scm.enter(|ctx| {
            let rootset = ROOTSET.get().unwrap().fetch(&ctx);

            assert!(rootset.weaks[1].is_broken());
        })
    }

    #[test]
    fn test_weak_table() {
        struct Rootset<'gc> {
            tmps: Vec<Value<'gc>>,
            table: Gc<'gc, WeakTable<'gc>>,
        }

        unsafe impl<'gc> Trace for Rootset<'gc> {
            fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
                for tmp in &mut self.tmps {
                    tmp.trace(visitor);
                }
                self.table.trace(visitor);
            }
        }

        static ROOTSET: OnceLock<Global<Rootable!(Rootset<'_>)>> = OnceLock::new();

        let scm = Scheme::new();
        scm.enter(|mc| init_weak_tables(&mc));

        scm.enter(|ctx| {
            let table = WeakTable::new(ctx, 8, 0.75);
            let k1 = String::new(&ctx, "key1", false);
            let v1 = String::new(&ctx, "value1", false);
            let k2 = String::new(&ctx, "key2", false);
            let v2 = String::new(&ctx, "value2", false);
            let k3 = String::new(&ctx, "key3", false); // This key will not be rooted
            let v3 = String::new(&ctx, "value3", false);

            table.put(ctx, k1, v1);
            table.put(ctx, k2, v2);
            table.put(ctx, k3, v3);

            assert!(matches!(table.get(ctx, k1), Some(val) if val == v1.into_value(ctx)));
            assert!(matches!(table.get(ctx, k2), Some(val) if val == v2.into_value(ctx)));
            assert!(matches!(table.get(ctx, k3), Some(val) if val == v3.into_value(ctx)));
            assert!(table.contains_key(ctx, k1.into_value(ctx)));
            assert!(table.contains_value(v1.into_value(ctx)));

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

        scm.mutator.request_gc();
        scm.enter(|mc| vacuum_weak_tables(&mc));

        scm.enter(|ctx| {
            let rootset_global = ROOTSET.get().unwrap();
            let rootset = rootset_global.fetch(&ctx);
            let table = rootset.table;

            let k1_rooted = rootset.tmps[0];
            // For k3, its original Gc<String> object might be collected if not otherwise rooted.
            // We create a new string for lookup. The entry should be gone if the key was collected.
            let k3_lookup = String::new(&ctx, "key3", false);

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
            assert_eq!(table.inner.lock_no_handshake().count.get(), 0);
        });
    }
}

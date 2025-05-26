use std::{
    cell::{Cell, RefCell},
    mem::transmute,
    sync::{Arc, Once, OnceLock},
};

use rsgc::{
    EnsureGCInfo, Gc, GcWeak, Rootable, Trace,
    alloc::array::Array,
    barrier::{self, Unlock, Write},
    cell::Lock,
    context::Mutation,
    finalizer::FinalizationNotifier,
    mutator::Global,
    vmkit::sync::Monitor,
};

use crate::runtime::vmthread::{VM_THREAD, VMThreadTask};

use super::{Tagged, TypeCode8, Value};

struct WeakEntry<'gc> {
    value: Value<'gc>,
    hash: u64,
}

impl<'gc> WeakEntry<'gc> {
    fn is_broken(&self) -> bool {
        self.value.is_bwp()
    }

    fn broken() -> Self {
        Self {
            value: Value::from_raw_i64(Value::VALUE_BWP),
            hash: 0,
        }
    }
}

unsafe impl<'gc> Trace for WeakEntry<'gc> {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {}
}

impl<'gc> Copy for WeakEntry<'gc> {}
impl<'gc> Clone for WeakEntry<'gc> {
    fn clone(&self) -> Self {
        *self
    }
}

type Entries<'gc> = Gc<'gc, Array<Lock<WeakEntry<'gc>>>>;

pub struct WeakSet<'gc> {
    inner: Monitor<WeakSetInner<'gc>>,
}

struct WeakSetInner<'gc> {
    entries: Lock<Entries<'gc>>,
    size: Cell<usize>,
    n_items: Cell<usize>,
    lower: Cell<usize>,
    upper: Cell<usize>,
    size_index: Cell<usize>,
    min_size_index: Cell<usize>,
}

const fn hash_to_index(hash: u64, size: usize) -> usize {
    ((hash >> 1) % size as u64) as usize
}

const fn entry_distance(hash: u64, k: usize, size: usize) -> usize {
    let origin = hash_to_index(hash, size);
    if k >= origin {
        k - origin
    } else {
        size - (origin + k)
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for WeakSet<'gc> {}

unsafe impl<'gc> Trace for WeakSet<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        let inner = self.inner.get_mut();
        inner.entries.trace(visitor);

        visitor.register_for_weak_processing();
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let inner = self.inner.get_mut();

        let entries = inner.entries.get();

        for entry in entries.iter() {
            let entry = unsafe { entry.unlock_unchecked() };
            if entry.get().value.is_bwp() {
                continue;
            }

            // SAFETY: GcWeak does not rely on layout of `T`
            unsafe {
                let mut value: GcWeak<'static, ()> = transmute(entry.get().value);
                weak_processor.process_weak(&mut value);
                if value.is_broken() {
                    entry.set(WeakEntry::broken());
                } else {
                }
            }
        }
    }
}

const HASHSET_SIZES: &[usize] = &[
    31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363, 224717, 449419,
    898823, 1797641, 3595271, 7190537, 14381041, 28762081, 57524111, 115048217, 230096423,
];

impl<'gc> WeakSetInner<'gc> {
    fn rob_from_rich(self: &Self, mc: &Mutation<'gc>, k: usize) {
        let size = self.size.get();

        let mut empty = k;
        let entries = self.entries.get();
        loop {
            empty = (empty + 1) % size;

            if entries[empty].get().value.is_bwp() {
                break;
            }
        }

        loop {
            let last = if empty != 0 { empty - 1 } else { size - 1 };

            let from = entries[last].get();
            Gc::write(mc, entries)[empty].unlock().set(from);
            empty = last;
            if empty == k {
                break;
            }
        }

        Gc::write(mc, entries)[empty].unlock().set(WeakEntry {
            value: Value::bwp(),
            hash: 0,
        });
    }

    fn give_to_poor(&self, mc: &Mutation<'gc>, mut k: usize) {
        let size = self.size.get();
        let entries = self.entries.get();
        loop {
            let next = (k + 1) % size;
            let next_entry = entries[next].get();
            let hash = next_entry.hash;

            if hash == 0 || hash_to_index(hash, size) == next {
                break;
            }

            if next_entry.value.is_bwp() {
                self.give_to_poor(mc, next);
                self.n_items.set(self.n_items.get() - 1);
                continue;
            }

            // Move next_entry to k
            Gc::write(mc, entries)[k].unlock().set(next_entry);

            k = next;
        }

        // Free the end
        Gc::write(mc, entries)[k].unlock().set(WeakEntry {
            value: Value::bwp(),
            hash: 0,
        });
    }

    fn give_to_poor_no_wb(&self, mut k: usize) {
        let size = self.size.get();
        let entries = self.entries.get();
        loop {
            let next = (k + 1) % size;
            let next_entry = entries[next].get();
            let hash = next_entry.hash;

            if hash == 0 || hash_to_index(hash, size) == next {
                break;
            }

            if next_entry.value.is_bwp() {
                self.give_to_poor_no_wb(next);
                self.n_items.set(self.n_items.get() - 1);
                continue;
            }

            // Move next_entry to k
            //Gc::write(mc, entries)[k].unlock().set(next_entry);
            unsafe { entries[k].unlock_unchecked() }.set(next_entry);

            k = next;
        }

        // Free the end
        unsafe { entries[k].unlock_unchecked() }.set(WeakEntry {
            value: Value::bwp(),
            hash: 0,
        });
    }

    fn compute_size_index(&self) -> usize {
        let mut i = self.size_index.get();

        if self.n_items.get() < self.lower.get() {
            loop {
                i -= 1;
                if !(i > self.min_size_index.get() && self.n_items.get() < HASHSET_SIZES[i] / 5) {
                    break;
                }
            }
        } else if self.n_items.get() > self.upper.get() {
            i += 1;
            if i >= HASHSET_SIZES.len() {
                panic!("WeakSet size index out of bounds");
            }
        }

        i
    }

    fn is_acceptable_size_index(&self, size_index: usize) -> bool {
        let computed = self.compute_size_index();

        if size_index == computed {
            return true;
        }

        if size_index == computed + 1 {
            let new_lower = HASHSET_SIZES[size_index] / 5;

            self.size.get() > new_lower
        } else if size_index == computed - 1 {
            false
        } else {
            false
        }
    }

    fn resize(self: &Write<Self>, mc: &Mutation<'gc>) {
        let mut new_size_index;
        let mut new_size;

        loop {
            new_size_index = self.compute_size_index();
            if new_size_index == self.size_index.get() {
                return;
            }

            new_size = HASHSET_SIZES[new_size_index];

            if self.is_acceptable_size_index(new_size_index) {
                break;
            }
        }

        let new_entries = Array::with_fn(mc, new_size, |_| Lock::new(WeakEntry::broken()));

        let old_entries = self.entries.get();
        let old_size = self.size.get();

        self.size_index.set(new_size_index);
        self.size.set(new_size);

        if new_size_index <= self.min_size_index.get() {
            self.lower.set(0);
        } else {
            self.lower.set(new_size / 5);
        }

        self.upper.set(9 * new_size / 10);
        self.n_items.set(0);
        barrier::field!(self, Self, entries)
            .unlock()
            .set(new_entries);

        for old_k in 0..old_size {
            if old_entries[old_k].get().hash == 0 {
                continue;
            }

            let entry = old_entries[old_k].get();

            if entry.is_broken() {
                continue;
            }

            let mut new_k = hash_to_index(entry.hash, new_size);
            let mut distance = 0;
            loop {
                let other_hash = new_entries[new_k].get().hash;

                if other_hash == 0 {
                    break;
                }

                if entry_distance(other_hash, new_k, new_size) < distance {
                    self.rob_from_rich(mc, new_k);
                    break;
                }

                distance += 1;
                new_k = (new_k + 1) % new_size;
            }

            self.n_items.set(self.n_items.get() + 1);
            Gc::write(mc, new_entries)[new_k].unlock().set(entry);
        }
    }

    fn vacuum_no_resize(&self) {
        let entries = self.entries.get();

        let size = self.size.get();
        for k in 0..size {
            let entry = entries[k].get();

            if entry.is_broken() {
                self.give_to_poor_no_wb(k);
                self.n_items.set(self.n_items.get() - 1);
            }
        }
    }
    fn vacuum(self: &Write<Self>, mc: &Mutation<'gc>) {
        let entries = self.entries.get();
        let size = self.size.get();

        for k in 0..size {
            let entry = entries[k].get();
            if entry.hash != 0 {
                if entry.is_broken() {
                    self.give_to_poor(mc, k);
                    self.n_items.set(self.n_items.get().saturating_sub(1));
                }
            }
        }

        if self.n_items.get() < self.lower.get() {
            self.resize(mc);
        }
    }

    fn lookup(
        self: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) -> Option<Value<'gc>> {
        let size = self.size.get();
        let entries = self.entries.get();

        let hash = (hash << 1) | 0x1;
        let mut k = hash_to_index(hash, size);

        for distance in 0..size {
            let mut other_hash = entries[k].get().hash;

            'retry: loop {
                if other_hash == 0 {
                    // Not found.
                    return None;
                }

                if hash == other_hash {
                    let entry = entries[k].get();

                    if entry.value.is_bwp() {
                        self.give_to_poor(mc, k);
                        self.n_items.set(self.n_items.get() - 1);
                        other_hash = entries[k].get().hash;
                        continue 'retry;
                    }

                    if pred(mc, entry.value) {
                        return Some(entry.value);
                    }
                }

                // If the entry's distance is less, our key is not in the set.
                if entry_distance(other_hash, k, size) < distance {
                    return None;
                }
                break;
            }

            k = (k + 1) % size;
        }

        // If we got here, then we were unfortunate enough to loop through the whole set.
        None
    }

    fn add(
        self: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let size = self.size.get();
        let entries = self.entries.get();

        let hash = (hash << 1) | 0x1;
        let mut k = hash_to_index(hash, size);

        'found: for distance in 0.. {
            let mut other_hash = entries[k].get().hash;

            'retry: loop {
                if other_hash == 0 {
                    // Found an empty entry.
                    break 'found;
                }

                if other_hash == hash {
                    let entry = entries[k].get();

                    if entry.value.is_bwp() {
                        self.give_to_poor(mc, k);
                        self.n_items.set(self.n_items.get() - 1);
                        other_hash = entries[k].get().hash;
                        continue 'retry;
                    }

                    if pred(mc, entry.value) {
                        // Found an entry with this key.
                        return entry.value;
                    }
                }

                if self.n_items.get() > self.upper.get() {
                    self.vacuum_no_resize();
                    self.resize(mc);
                    return self.add(mc, hash >> 1, pred, value);
                }

                // Displace the entry if our distance is less, otherwise keep looking.
                if entry_distance(other_hash, k, size) < distance {
                    self.rob_from_rich(mc, k);
                    break 'found;
                }
                break;
            }

            k = (k + 1) % size;
        }

        // Insert new entry.
        self.n_items.set(self.n_items.get() + 1);
        Gc::write(mc, entries)[k]
            .unlock()
            .set(WeakEntry { value, hash });

        value
    }

    fn remove(
        self: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) {
        let size = self.size.get();
        let entries = self.entries.get();

        let hash = (hash << 1) | 0x1;
        let mut k = hash_to_index(hash, size);

        for distance in 0..size {
            let mut other_hash = entries[k].get().hash;

            'retry: loop {
                if other_hash == 0 {
                    // Not found.
                    return;
                }

                if other_hash == hash {
                    let entry = entries[k].get();

                    if entry.value.is_bwp() {
                        self.give_to_poor_no_wb(k);
                        self.n_items.set(self.n_items.get().saturating_sub(1));
                        other_hash = entries[k].get().hash;
                        continue 'retry;
                    }

                    if pred(mc, entry.value) {
                        // Found an entry with this key.
                        Gc::write(mc, entries)[k].unlock().set(WeakEntry {
                            value: Value::bwp(),
                            hash: 0,
                        });

                        self.n_items.set(self.n_items.get().saturating_sub(1));
                        if self.n_items.get() < self.lower.get() {
                            self.resize(mc);
                        } else {
                            self.give_to_poor_no_wb(k);
                        }
                        return;
                    }
                }

                // If the entry's distance is less, our key is not in the set.
                if entry_distance(other_hash, k, size) < distance {
                    return;
                }
                break;
            }

            k = (k + 1) % size;
        }
    }
}

impl<'gc> WeakSet<'gc> {
    pub fn new(mc: &Mutation<'gc>, k: usize) -> Gc<'gc, Self> {
        let mut i = 0;
        let mut n = if k != 0 { k } else { 31 };

        while i + 1 < HASHSET_SIZES.len() && n > HASHSET_SIZES[i] {
            i += 1;
        }

        n = HASHSET_SIZES[i];

        let entries = Array::with_fn(mc, n, |_| Lock::new(WeakEntry::broken()));

        let weak_set = Gc::new(
            mc,
            Self {
                inner: Monitor::new(WeakSetInner {
                    entries: Lock::new(entries),
                    size: Cell::new(n),
                    n_items: Cell::new(0),
                    lower: Cell::new(0),
                    upper: Cell::new(9 * n / 10),
                    size_index: Cell::new(i),
                    min_size_index: Cell::new(i),
                }),
            },
        );

        weak_set.set_user_header(TypeCode8::WEAKSET.into());

        ALL_WEAK_SETS
            .get()
            .unwrap()
            .fetch(mc)
            .0
            .lock_no_handshake()
            .borrow_mut()
            .push(Gc::downgrade(weak_set));

        weak_set
    }

    #[cold]
    #[inline(never)]
    pub fn add(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let inner = self.inner.lock_no_handshake();
        Gc::write(mc, self);
        unsafe { Write::assume(&*inner).add(mc, hash, pred, value) }
    }

    pub fn lookup(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) -> Option<Value<'gc>> {
        let inner = self.inner.lock_no_handshake();
        Gc::write(mc, self);
        unsafe { Write::assume(&*inner).lookup(mc, hash, pred) }
    }

    pub fn remove(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) {
        let inner = self.inner.lock_no_handshake();
        Gc::write(mc, self);
        unsafe { Write::assume(&*inner).remove(mc, hash, pred) }
    }

    pub fn clear(self: Gc<'gc, Self>, mc: &Mutation<'gc>) {
        let inner = self.inner.lock_no_handshake();
        Gc::write(mc, self);
        // SAFETY: It is safe to clear references without write barrier
        unsafe {
            let entries = inner.entries.get();
            let entries = Write::assume(&*entries);

            for i in 0..entries.len() {
                entries[i].unlock().set(WeakEntry::broken());
            }

            inner.n_items.set(0);
        }

        drop(inner);
    }
}

struct AllWeakSets<'gc>(Monitor<RefCell<Vec<GcWeak<'gc, WeakSet<'gc>>>>>);

unsafe impl<'gc> Send for AllWeakSets<'gc> {}
unsafe impl<'gc> Sync for AllWeakSets<'gc> {}

unsafe impl<'gc> Trace for AllWeakSets<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        visitor.register_for_weak_processing();
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let all_weak_sets = self.0.get_mut().get_mut();
        all_weak_sets.retain_mut(|weak_set| {
            weak_processor.process_weak(weak_set);
            if weak_set.is_broken() {
                return false;
            }

            true
        });
    }
}

static ALL_WEAK_SETS: OnceLock<Global<Rootable!(AllWeakSets<'_>)>> = OnceLock::new();
struct WeakSetNotify;

impl FinalizationNotifier for WeakSetNotify {
    fn notify_in_processing(&self) {}

    fn schedule(&self) {
        //VM_THREAD.schedule_task(VMThreadTask::VacuumWeakSets);
    }
}

pub(crate) fn vacuum_weak_sets<'gc>(mc: &Mutation<'gc>) {
    if let Some(all_weak_sets) = ALL_WEAK_SETS.get() {
        let all_weak_sets = all_weak_sets.fetch(mc);
        let guard = all_weak_sets.0.lock_no_handshake();

        guard.borrow_mut().retain(|weak_set| {
            if let Some(weak_set) = weak_set.upgrade() {
                let wset = weak_set.inner.lock_no_handshake();
                Gc::write(mc, weak_set);
                unsafe {
                    Write::assume(&*wset).vacuum(mc);
                }
                return true;
            }

            false
        })
    }
}
static ONCE: Once = Once::new();
pub fn init_weak_sets<'gc>(mc: &Mutation<'gc>) {
    ONCE.call_once(|| {
        let all_weak_sets = AllWeakSets(Monitor::new(RefCell::new(Vec::new())));
        let all_weak_sets = Global::new(all_weak_sets);
        let _ = ALL_WEAK_SETS.set(all_weak_sets);

        let weak_set_notify = WeakSetNotify;
        mc.finalizers().add_notifier(Arc::new(weak_set_notify));
    });
}

unsafe impl<'gc> Tagged for WeakSet<'gc> {
    const TC8: super::TypeCode8 = TypeCode8::WEAKSET;
}

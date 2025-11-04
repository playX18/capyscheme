use std::{
    cell::Cell,
    sync::{Arc, Once, OnceLock},
};

use rsgc::{
    Gc, Rootable, Trace,
    alloc::array::Array,
    barrier::{self, Write},
    cell::Lock,
    collection::Visitor,
    finalizer::FinalizationNotify,
    global::Global,
    mmtk::util::Address,
    mutator::Mutation,
    object::GCObject,
    sync::monitor::Monitor,
    weak::Weak,
};

use crate::runtime::{
    value::{ScmHeader, Symbol},
    vmthread::{VM_THREAD, VMThreadTask},
};

use super::{Tagged, TypeCode8, Value};
#[repr(C, align(8))]
#[derive(Debug)]
pub(crate) struct WeakEntry<'gc> {
    pub(crate) value: Value<'gc>,
    pub(crate) hash: u64,
}

impl<'gc> WeakEntry<'gc> {
    fn is_broken(&self, mc: &Mutation<'gc>) -> bool {
        self.get(mc).is_bwp()
    }

    fn broken() -> Self {
        Self {
            value: Value::from_raw_i64(Value::VALUE_BWP),
            hash: 0,
        }
    }

    fn get(&self, mc: &Mutation<'gc>) -> Value<'gc> {
        if self.value.is_bwp() {
            return self.value;
        }

        unsafe {
            mc.raw_weak_reference_load(GCObject::from_address(Address::from_usize(
                self.value.bits() as _,
            )));
            self.value
        }
    }
}

unsafe impl<'gc> Trace for WeakEntry<'gc> {
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {
        _visitor.register_for_weak_processing();
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        if self.value.is_bwp() {
            assert!(!self.value.is::<Symbol>());
            return;
        }

        if !self.value.is_cell() {
            return;
        }

        let new = unsafe { weak_processor.is_live_object(self.value.desc.ptr()) };
        if new.is_null() {
            self.value = Value::bwp();
        } else {
            self.value.desc.ptr = new.to_address().to_mut_ptr();
        }
    }
}

impl<'gc> Copy for WeakEntry<'gc> {}
impl<'gc> Clone for WeakEntry<'gc> {
    fn clone(&self) -> Self {
        *self
    }
}

type Entries<'gc> = Gc<'gc, Array<Lock<WeakEntry<'gc>>>>;

#[repr(C, align(8))]
pub struct WeakSet<'gc> {
    #[allow(dead_code)]
    header: ScmHeader,
    pub(crate) inner: Monitor<WeakSetInner<'gc>>,
}

pub(crate) struct WeakSetInner<'gc> {
    pub(crate) entries: Lock<Entries<'gc>>,
    pub(crate) size: Cell<usize>,
    pub(crate) n_items: Cell<usize>,
    pub(crate) lower: Cell<usize>,
    pub(crate) upper: Cell<usize>,
    pub(crate) size_index: Cell<usize>,
    pub(crate) min_size_index: Cell<usize>,
}

const fn hash_to_index(hash: u64, size: usize) -> usize {
    ((hash >> 1) % size as u64) as usize
}

const fn entry_distance(hash: u64, k: usize, size: usize) -> usize {
    let origin = hash_to_index(hash, size);
    if k >= origin {
        k.wrapping_sub(origin)
    } else {
        size.wrapping_sub(origin).wrapping_add(k)
    }
}

unsafe impl<'gc> Trace for WeakSet<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        let inner = self.inner.get_mut();
        visitor.trace(&mut inner.entries);
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

const HASHSET_SIZES: &[usize] = &[
    31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363, 224717, 449419,
    898823, 1797641, 3595271, 7190537, 14381041, 28762081, 57524111, 115048217, 230096423,
];

impl<'gc> WeakSetInner<'gc> {
    fn rob_from_rich(&self, mc: &Mutation<'gc>, k: usize) {
        let size = self.size.get();

        let mut empty = k;
        let entries = self.entries.get();
        assert!(self.n_items.get() < size);

        loop {
            empty = (empty + 1) % size;
            let value = entries[empty].get().get(mc);
            if value.is_bwp() {
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
            let value = next_entry.get(mc);
            if value.is_bwp() {
                self.give_to_poor(mc, next);
                self.n_items.set(self.n_items.get() - 1);
                continue;
            }

            // Move next_entry to k
            Gc::write(mc, entries)[k].unlock().set(entries[next].get());

            k = next;
        }

        // Free the end
        Gc::write(mc, entries)[k].unlock().set(WeakEntry {
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
        }
        /*else if size_index == computed - 1 {
            false
        }*/
        else {
            false
        }
    }

    fn resize(this: &Write<Self>, mc: &Mutation<'gc>) {
        let mut new_size_index;
        let mut new_size;

        loop {
            new_size_index = this.compute_size_index();
            if new_size_index == this.size_index.get() {
                return;
            }

            new_size = HASHSET_SIZES[new_size_index];

            if this.is_acceptable_size_index(new_size_index) {
                break;
            }
        }

        let new_entries = Array::with(mc, new_size, |_, _| Lock::new(WeakEntry::broken()));

        let old_entries = this.entries.get();
        let old_size = this.size.get();

        this.size_index.set(new_size_index);
        this.size.set(new_size);

        if new_size_index <= this.min_size_index.get() {
            this.lower.set(0);
        } else {
            this.lower.set(new_size / 5);
        }

        this.upper.set(9 * new_size / 10);
        this.n_items.set(0);
        barrier::field!(this, Self, entries)
            .unlock()
            .set(new_entries);

        for old_k in 0..old_size {
            if old_entries[old_k].get().hash == 0 {
                continue;
            }

            let entry = old_entries[old_k].get();

            if entry.is_broken(mc) {
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
                    this.rob_from_rich(mc, new_k);
                    break;
                }

                distance += 1;
                new_k = (new_k + 1) % new_size;
            }

            this.n_items.set(this.n_items.get() + 1);

            Gc::write(mc, new_entries)[new_k].unlock().set(entry);
        }
    }

    pub fn vacuum(this: &Write<Self>, mc: &Mutation<'gc>) {
        let entries = this.entries.get();
        let size = this.size.get();

        for k in 0..size {
            let entry = entries[k].get();
            if entry.hash != 0 && entry.is_broken(mc) {
                Self::give_to_poor(this, mc, k);
                this.n_items.set(this.n_items.get().saturating_sub(1));
            }
        }

        if this.n_items.get() < this.lower.get() {
            Self::resize(this, mc);
        }
    }

    fn lookup(
        this: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) -> Option<Value<'gc>> {
        let size = this.size.get();
        let entries = this.entries.get();

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

                    if entry.is_broken(mc) {
                        this.give_to_poor(mc, k);
                        this.n_items.set(this.n_items.get() - 1);
                        other_hash = entries[k].get().hash;
                        continue 'retry;
                    }
                    let value = entry.get(mc);
                    if pred(mc, value) {
                        return Some(value);
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
        this: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let size = this.size.get();
        let entries = this.entries.get();

        let hash = (hash << 1) | 0x1;
        let mut k = hash_to_index(hash, size);

        'found: for distance in 0.. {
            'retry: loop {
                let other_hash = entries[k].get().hash;
                if other_hash == 0 {
                    // Found an empty entry.
                    break 'found;
                }

                if other_hash == hash {
                    let entry = entries[k].get();
                    let value = entry.get(mc);
                    if entry.is_broken(mc) {
                        this.give_to_poor(mc, k);
                        this.n_items.set(this.n_items.get() - 1);

                        continue 'retry;
                    }

                    if pred(mc, value) {
                        // Found an entry with this key.
                        return value;
                    }
                }

                if this.n_items.get() > this.upper.get() {
                    Self::vacuum(this, mc);
                    Self::resize(this, mc);
                    return Self::add(this, mc, hash >> 1, pred, value);
                }

                // Displace the entry if our distance is less, otherwise keep looking.
                let entry_distance = entry_distance(other_hash, k, size);

                if entry_distance < distance {
                    Self::rob_from_rich(this, mc, k);
                    break 'found;
                }
                break;
            }

            k = (k + 1) % size;
        }

        // Insert new entry.
        this.n_items.set(this.n_items.get() + 1);
        Gc::write(mc, entries)[k]
            .unlock()
            .set(WeakEntry { value, hash });

        value
    }

    fn remove(
        this: &Write<Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) {
        let size = this.size.get();
        let entries = this.entries.get();

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
                    let value = entry.get(mc);
                    if entry.is_broken(mc) {
                        this.give_to_poor(mc, k);
                        this.n_items.set(this.n_items.get().saturating_sub(1));
                        other_hash = entries[k].get().hash;
                        continue 'retry;
                    }

                    if pred(mc, value) {
                        // Found an entry with this key.
                        Gc::write(mc, entries)[k].unlock().set(WeakEntry {
                            value: Value::bwp(),
                            hash: 0,
                        });

                        this.n_items.set(this.n_items.get().saturating_sub(1));
                        if this.n_items.get() < this.lower.get() {
                            Self::resize(this, mc);
                        } else {
                            this.give_to_poor(mc, k);
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

        let entries = Array::with(mc, n, |_, _| Lock::new(WeakEntry::broken()));
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode8::WEAKSET.bits() as _);

        let weak_set = Gc::new(
            mc,
            Self {
                header: hdr,
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

        ALL_WEAK_SETS
            .get()
            .unwrap()
            .fetch(mc)
            .0
            .lock()
            .push(Gc::downgrade(weak_set));

        weak_set
    }

    /*pub(crate) unsafe fn at_object(ctx: Context<'gc>, obj: GCObject, keys: &[Value<'gc>]) {
        let mut i = 0;
        let mut n = keys.len().min(31);

        while i + 1 < HASHSET_SIZES.len() && n > HASHSET_SIZES[i] {
            i += 1;
        }

        n = HASHSET_SIZES[i];

        let entries = Array::with(&ctx, n, |_, _| Lock::new(WeakEntry::broken()));

        unsafe {
            let hdr = obj.to_address().to_mut_ptr::<ScmHeader>().read();
            let inner = WeakSetInner {
                entries: Lock::new(entries),
                size: Cell::new(n),
                n_items: Cell::new(0),
                lower: Cell::new(0),
                upper: Cell::new(9 * n / 10),
                size_index: Cell::new(i),
                min_size_index: Cell::new(i),
            };

            obj.to_address()
                .to_mut_ptr::<WeakSet<'gc>>()
                .write(WeakSet {
                    header: hdr,
                    inner: Monitor::new(inner),
                });
        }
    }*/

    #[cold]
    #[inline(never)]
    pub fn add(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let inner = this.inner.lock();
        Gc::write(mc, this);
        unsafe { WeakSetInner::add(Write::assume(&*inner), mc, hash, pred, value) }
    }

    pub fn lookup(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) -> Option<Value<'gc>> {
        let inner = this.inner.lock();
        Gc::write(mc, this);
        unsafe { WeakSetInner::lookup(Write::assume(&*inner), mc, hash, pred) }
    }

    pub fn remove(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        hash: u64,
        pred: impl Fn(&Mutation<'gc>, Value<'gc>) -> bool,
    ) {
        let inner = this.inner.lock();
        Gc::write(mc, this);
        unsafe { WeakSetInner::remove(Write::assume(&*inner), mc, hash, pred) }
    }

    pub fn clear(this: Gc<'gc, Self>, mc: &Mutation<'gc>) {
        let inner = this.inner.lock();
        Gc::write(mc, this);
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

    pub fn for_each<F>(self: Gc<'gc, Self>, mc: &Mutation<'gc>, mut f: F)
    where
        F: FnMut(Value<'gc>),
    {
        let inner = self.inner.lock();

        let entries = inner.entries.get();

        for i in 0..entries.len() {
            let entry = entries[i].get();
            if entry.hash != 0 {
                let value = entry.get(mc);
                if !value.is_bwp() {
                    f(value);
                }
            }
        }
    }
}

struct AllWeakSets<'gc>(Monitor<Vec<Weak<'gc, WeakSet<'gc>>>>);

unsafe impl<'gc> Send for AllWeakSets<'gc> {}
unsafe impl<'gc> Sync for AllWeakSets<'gc> {}

unsafe impl<'gc> Trace for AllWeakSets<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor<'_>) {
        visitor.register_for_weak_processing();
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let all_weak_sets = self.0.get_mut();
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

impl FinalizationNotify for WeakSetNotify {
    fn notify_in_processing(&self) {}

    fn schedule(&self) {
        VM_THREAD.schedule_task(VMThreadTask::VacuumWeakSets);
    }
}

pub(crate) fn vacuum_weak_sets<'gc>(mc: &Mutation<'gc>) {
    if let Some(all_weak_sets) = ALL_WEAK_SETS.get() {
        let all_weak_sets = all_weak_sets.fetch(mc);
        let mut guard = all_weak_sets.0.lock();

        guard.retain(|weak_set| {
            if let Some(weak_set) = weak_set.upgrade(mc) {
                let wset = weak_set.inner.lock();
                Gc::write(mc, weak_set);
                unsafe {
                    WeakSetInner::vacuum(Write::assume(&*wset), mc);
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
        let all_weak_sets = AllWeakSets(Monitor::new(Vec::new()));
        let all_weak_sets = Global::new(all_weak_sets);
        let _ = ALL_WEAK_SETS.set(all_weak_sets);

        let weak_set_notify = WeakSetNotify;
        mc.finalizers().add_notifier(Arc::new(weak_set_notify));
    });
}

unsafe impl<'gc> Tagged for WeakSet<'gc> {
    const TC8: super::TypeCode8 = TypeCode8::WEAKSET;
    const TYPE_NAME: &'static str = "#<weak-set>";
}

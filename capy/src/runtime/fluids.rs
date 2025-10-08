use crate::{
    native_fn,
    runtime::{
        Context,
        value::{HashTable, ScmHeader, Tagged, TypeBits, TypeCode8, Value, WeakTable},
    },
};
use easy_bitfield::{BitField, BitFieldTrait};
use rsgc::{
    Gc, Mutation, Trace,
    barrier::{IndexWrite, Unlock},
    cell::Lock,
};
use std::{cell::Cell, hash::Hash, ops::Index};

/// A cache entry containing a key-value pair and usage tracking
#[derive(Clone, Copy, Trace)]
#[collect(no_drop)]
pub struct CacheEntry<'gc> {
    pub key: Value<'gc>,
    pub value: Value<'gc>,
    /// Used for LRU tracking - higher values indicate more recent usage
    access_time: u32,
}

impl<'gc> CacheEntry<'gc> {
    /// Create an empty cache entry
    pub const fn empty() -> Self {
        Self {
            key: Value::empty(),
            value: Value::empty(),
            access_time: 0,
        }
    }
}

/// A fixed-size cache with 16 entries that evicts least recently used entries
#[derive(Trace)]
#[collect(no_drop)]
pub struct Cache<'gc> {
    entries: [Lock<CacheEntry<'gc>>; 16],
    clock: Cell<u32>,
}

impl<'gc> Cache<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            mc,
            Self {
                entries: [const { Lock::new(CacheEntry::empty()) }; 16],
                clock: Cell::new(1),
            },
        )
    }

    /// Hash a key to determine its slot in the cache
    fn hash_key(&self, key: Value<'gc>) -> usize {
        let mut hash = simplehash::Fnv1aHasher64::new();

        key.hash(&mut hash);
        hash.finish_raw() as _
    }

    /// Get the current time and increment the clock
    fn next_time(&self) -> u32 {
        let time = self.clock.get();
        self.clock.set(time.wrapping_add(1));
        time
    }

    /// Look up a value by key
    pub fn get(self: Gc<'gc, Self>, mc: &Mutation<'gc>, key: Value<'gc>) -> Option<Value<'gc>> {
        let slot = self.hash_key(key);

        // Linear probing for collision resolution
        for i in 0..16 {
            let index = (slot + i) & 15;
            let entry = self.entries[index].get();

            if entry.key == key {
                Gc::write(mc, self)[index].unlock().set(CacheEntry {
                    key: entry.key,
                    value: entry.value,
                    access_time: self.next_time(),
                });
                return Some(entry.value);
            }

            // Empty slot means key not found
            if entry.key.raw_i64() == Value::VALUE_EMPTY {
                break;
            }
        }

        None
    }

    pub fn update(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> bool {
        let slot = self.hash_key(key);
        let access_time = self.next_time();

        // First try to find existing key or empty slot
        for i in 0..16 {
            let index = (slot + i) & 15;
            let entry = self.entries[index].get();

            // Update existing key
            if entry.key == key {
                Gc::write(mc, self)[index].unlock().set(CacheEntry {
                    key,
                    value,
                    access_time,
                });
                return true;
            }
        }

        false
    }

    /// Insert or update a key-value pair
    pub fn insert(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Option<(Value<'gc>, Value<'gc>)> {
        let slot = self.hash_key(key);
        let access_time = self.next_time();

        // First try to find existing key or empty slot
        for i in 0..16 {
            let index = (slot + i) & 15;
            let entry = self.entries[index].get();

            // Update existing key
            if entry.key == key {
                Gc::write(mc, self)[index].unlock().set(CacheEntry {
                    key,
                    value,
                    access_time,
                });
                return None;
            }

            // Use empty slot
            if entry.key.raw_i64() == Value::VALUE_EMPTY {
                Gc::write(mc, self)[index].unlock().set(CacheEntry {
                    key,
                    value,
                    access_time,
                });
                return None;
            }
        }

        // All slots in probe sequence are occupied, evict least recently used
        let mut lru_index = slot;
        let mut lru_time = self.entries[slot].get().access_time;

        for i in 1..16 {
            let index = (slot + i) & 15;
            let entry_time = self.entries[index].get().access_time;
            if entry_time < lru_time {
                lru_time = entry_time;
                lru_index = index;
            }
        }
        let (evicted_key, evicted_value) = {
            let entry = self.entries[lru_index].get();
            (entry.key, entry.value)
        };

        // Replace the LRU entry
        Gc::write(mc, self)[lru_index].unlock().set(CacheEntry {
            key,
            value,
            access_time,
        });

        Some((evicted_key, evicted_value))
    }

    /// Remove a key-value pair from the cache
    pub fn remove(self: Gc<'gc, Self>, mc: &Mutation<'gc>, key: Value<'gc>) -> Option<Value<'gc>> {
        let slot = self.hash_key(key);

        for i in 0..16 {
            let index = (slot + i) & 15;
            let entry = self.entries[index].get();

            if entry.key == key {
                let old_value = entry.value;
                Gc::write(mc, self)[index].unlock().set(CacheEntry::empty());
                return Some(old_value);
            }

            // Empty slot means key not found
            if entry.key.is_empty() {
                break;
            }
        }

        None
    }

    /// Clear all entries from the cache
    pub fn clear(self: Gc<'gc, Self>, mc: &Mutation<'gc>) {
        for i in 0..16 {
            Gc::write(mc, self)[i].unlock().set(CacheEntry::empty());
        }
        self.clock.set(1);
    }

    /// Check if the cache contains a key
    pub fn contains_key(self: Gc<'gc, Self>, mc: &Mutation<'gc>, key: Value<'gc>) -> bool {
        self.get(mc, key).is_some()
    }

    /// Get the number of entries in the cache
    pub fn len(self: Gc<'gc, Self>) -> usize {
        self.entries
            .iter()
            .filter(|entry| !entry.get().key.is_empty())
            .count()
    }

    /// Check if the cache is empty
    pub fn is_empty(self: Gc<'gc, Self>) -> bool {
        self.len() == 0
    }
}

impl<'gc> Index<usize> for Cache<'gc> {
    type Output = Lock<CacheEntry<'gc>>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.entries[index % 16]
    }
}

unsafe impl<'gc> IndexWrite<usize> for Cache<'gc> {}

#[derive(Trace)]
#[collect(no_drop)]
pub struct DynamicState<'gc> {
    thread_local_values: Gc<'gc, HashTable<'gc>>,
    values: Lock<Gc<'gc, WeakTable<'gc>>>,
    cache: Gc<'gc, Cache<'gc>>,
    has_aliased_values: Cell<bool>,
}

impl<'gc> DynamicState<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            thread_local_values: HashTable::new(mc, super::value::HashTableType::Eq, 8, 0.75),
            values: Lock::new(WeakTable::new(mc, 9, 0.75)),
            cache: Cache::new(mc),
            has_aliased_values: Cell::new(false),
        }
    }

    pub fn save(&self, ctx: Context<'gc>) -> Value<'gc> {
        let mut saved = Value::from(self.values.get());

        for slot in 0..16 {
            let CacheEntry { key, value, .. } = self.cache[slot].get();

            if key.is_empty() {
                continue;
            }

            let fluid = key.downcast::<Fluid>();

            if fluid.is_thread_local() {
                self.thread_local_values.put(ctx, key, value);
            } else if self.values.get().get(ctx, key) != Some(value) {
                if self.has_aliased_values.get() {
                    saved = Value::acons(ctx, key, value, saved);
                } else {
                    self.values.get().put(ctx, key, value);
                }
            }
        }

        self.has_aliased_values.set(true);

        saved
    }

    pub fn restore(&self, ctx: Context<'gc>, mut saved: Value<'gc>) {
        for slot in 0..16 {
            let entry = &Gc::write(&ctx, self.cache)[slot];
            if saved.is_pair() {
                let key = saved.caar();
                let value = saved.cdar();
                saved = saved.cdr();
                entry.unlock().set(CacheEntry {
                    key,
                    value,
                    access_time: self.cache.next_time(),
                });
            } else {
                entry.unlock().set(CacheEntry::empty());
            }
        }

        unsafe { self.values.unlock_unchecked().set(saved.downcast()) };
        self.has_aliased_values.set(false);
    }
}

type FluidThreadLocal = BitField<u64, bool, { TypeBits::NEXT_BIT }, 1, false>;

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Fluid<'gc> {
    header: ScmHeader,
    value: Value<'gc>,
}

impl<'gc> Fluid<'gc> {
    pub fn new(mc: &Mutation<'gc>, default_value: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            mc,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::FLUID.bits() as _),
                value: default_value,
            },
        )
    }

    pub fn new_thread_local(mc: &Mutation<'gc>, default_value: Value<'gc>) -> Gc<'gc, Self> {
        let mut hdr = ScmHeader::with_type_bits(TypeCode8::FLUID.bits() as _);
        hdr.word |= FluidThreadLocal::encode(true);
        Gc::new(
            mc,
            Self {
                header: hdr,
                value: default_value,
            },
        )
    }

    pub fn get(self: Gc<'gc, Self>, ctx: Context<'gc>) -> Value<'gc> {
        let dynamic_state = &ctx.state.dynamic_state;

        let entry = dynamic_state.cache.get(&ctx, Value::from(self));

        if let Some(entry) = entry {
            return entry;
        }

        let val = if self.is_thread_local() {
            dynamic_state
                .thread_local_values
                .get(ctx, Value::from(self))
                .unwrap_or(self.value)
        } else {
            dynamic_state
                .values
                .get()
                .get(ctx, Value::from(self))
                .unwrap_or(self.value)
        };

        self.set(ctx, val);

        val
    }

    pub fn set(self: Gc<'gc, Self>, ctx: Context<'gc>, value: Value<'gc>) {
        let dynamic_state = &ctx.state.dynamic_state;

        if dynamic_state.cache.update(&ctx, Value::from(self), value) {
            return;
        }

        if let Some((fluid, value)) = dynamic_state.cache.insert(&ctx, Value::from(self), value) {
            if fluid.downcast::<Fluid>().is_thread_local() {
                dynamic_state.thread_local_values.put(ctx, fluid, value);
                return;
            }
            if dynamic_state.has_aliased_values.get() {
                if dynamic_state.values.get().get(ctx, fluid) == Some(value) {
                    return;
                }

                unsafe {
                    dynamic_state
                        .values
                        .unlock_unchecked()
                        .set(copy_value_table(ctx, dynamic_state.values.get()));
                }
                dynamic_state.has_aliased_values.set(true);
            }
            dynamic_state.values.get().put(ctx, fluid, value);
        }
    }

    pub fn default_value(&self) -> Value<'gc> {
        self.value
    }

    pub fn is_thread_local(&self) -> bool {
        FluidThreadLocal::decode(self.header.word)
    }
}

unsafe impl<'gc> Tagged for Fluid<'gc> {
    const TC8: TypeCode8 = TypeCode8::FLUID;
    const TYPE_NAME: &'static str = "fluid";
}

fn copy_value_table<'gc>(
    ctx: Context<'gc>,
    tab: Gc<'gc, WeakTable<'gc>>,
) -> Gc<'gc, WeakTable<'gc>> {
    let ret = WeakTable::new(&ctx, 9, 0.75);

    tab.fold(
        ctx,
        move |key, value, _| {
            ret.put(ctx, key, value);
            Value::void()
        },
        Value::void(),
    );
    ret
}

pub type FluidRef<'gc> = Gc<'gc, Fluid<'gc>>;

/// Declare a global fluid variable.
#[macro_export]
macro_rules! fluid {
    ($(#[$outer:meta])* $v: vis $name: ident = $default_value: expr; $($rest:tt)*) => {
        paste::paste! {
            $(#[$outer])*
            $v static [<$name: upper>]: ::std::sync::OnceLock<
                $crate::rsgc::global::Global<$crate::rsgc::Rootable!($crate::runtime::fluids::FluidRef<'_>)>> = ::std::sync::OnceLock::new();

            $(#[$outer])*
            $v fn [<$name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>) -> $crate::runtime::fluids::FluidRef<'gc> {
                *[<$name: upper>]
                    .get_or_init(|| {
                        $crate::rsgc::global::Global::new($crate::runtime::fluids::Fluid::new(&ctx, $default_value))
                    }).fetch(&ctx)
            }

            $v fn [<get_ $name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>) -> $crate::runtime::value::Value<'gc> {
                [<$name: snake>](ctx).get(ctx)
            }

            $v fn [<set_ $name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>, value: $crate::runtime::value::Value<'gc>) -> $crate::runtime::value::Value<'gc> {
                let old = [<get_ $name: snake>](ctx);
                [<$name: snake>](ctx).set(ctx, value);
                old
            }

        }

        fluid! { $($rest)* }
    };

    ($v: vis thread_local $name: ident = $default_value: expr; $($rest:tt)*) => {
        paste::paste! {
            $v static [<$name: upper>]: ::std::sync::OnceLock<
                $crate::rsgc::global::Global<$crate::rsgc::Rootable!($crate::runtime::fluids::FluidRef<'_>)>> = ::std::sync::OnceLock::new();

            $v fn [<$name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>) -> $crate::runtime::fluids::FluidRef<'gc> {
                *[<$name: upper>]
                    .get_or_init(|| {
                        $crate::rsgc::global::Global::new($crate::runtime::fluids::Fluid::new_thread_local(&ctx, $default_value))
                    }).fetch(&ctx)
            }

            $v fn [<get_ $name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>) -> $crate::runtime::value::Value<'gc> {
                [<$name: snake>](ctx).get(ctx)
            }

            $v fn [<set_ $name: snake>]<'gc>(ctx: $crate::runtime::Context<'gc>, value: $crate::runtime::value::Value<'gc>) {
                [<$name: snake>](ctx).set(ctx, value);
            }
        }

        fluid! { $($rest)* }
    };

    () => {}
}

#[macro_export]
macro_rules! global {
    ($($v: vis $name: ident <$l: lifetime>: $t: ty = ($ctx: ident) $init: expr;)*) => {
        $(
            paste::paste!{
                #[allow(unused)]
                $v static [<$name: upper>]: std::sync::OnceLock<$crate::rsgc::global::Global<$crate::rsgc::Rootable!($l => $t)>> = std::sync::OnceLock::new();

                #[allow(unused)]
                $v fn [<$name: lower>]<$l>($ctx: $crate::runtime::Context<$l>) -> &$l $t {
                   &[<$name: upper>]
                        .get_or_init(|| {
                            let init: $t = $init;
                            $crate::rsgc::global::Global::new(init)
                        }).fetch(&$ctx)
                }


            }
        )*
    };
}

native_fn!(
    register_fluid_fns:

    pub ("make-fluid") fn make_fluid<'gc>(nctx, default_value: Option<Value<'gc>>) -> Value<'gc> {
        let default_value = default_value.unwrap_or(Value::new(false));

        let fluid = Fluid::new(&nctx.ctx, default_value);

        nctx.return_(fluid.into())
    }

    pub ("make-thread-local-fluid") fn make_thread_local_fluid<'gc>(nctx, default_value: Option<Value<'gc>>) -> Value<'gc> {
        let default_value = default_value.unwrap_or(Value::new(false));

        let fluid = Fluid::new_thread_local(&nctx.ctx, default_value);

        nctx.return_(fluid.into())
    }

    pub ("fluid?") fn is_fluid<'gc>(nctx, obj: Value<'gc>) -> Value<'gc> {
        nctx.return_(Value::new(obj.is::<Fluid>()))
    }

    pub ("fluid-thread-local?") fn is_fluid_thread_local<'gc>(nctx, obj: Value<'gc>) -> Value<'gc> {
        if !obj.is::<Fluid>() {
            todo!()
        }

        let fluid = obj.downcast::<Fluid>();
        nctx.return_(Value::new(fluid.is_thread_local()))
    }

    pub ("fluid-ref") fn fluid_ref<'gc>(nctx, fluid: Value<'gc>) -> Value<'gc> {
        if !fluid.is::<Fluid>() {
            todo!()
        }

        let fluid = fluid.downcast::<Fluid>();
        let value = fluid.get(nctx.ctx);
        nctx.return_(value)
    }

    pub ("fluid-set!") fn fluid_set<'gc>(nctx, fluid: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        if !fluid.is::<Fluid>() {
            todo!()
        }

        let fluid = fluid.downcast::<Fluid>();
        let old_value = fluid.get(nctx.ctx);
        fluid.set(nctx.ctx, value);
        nctx.return_(old_value)
    }
);

pub(crate) fn init_fluids<'gc>(ctx: Context<'gc>) {
    register_fluid_fns(ctx);
}

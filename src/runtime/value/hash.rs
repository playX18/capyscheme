use std::{cell::Cell, hash::Hash};

use rsgc::{
    Collect, EnsureGCInfo, Gc, Trace, alloc::array::Array, cell::Lock, vmkit::sync::Monitor,
};

use crate::runtime::Context;

use super::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum HashTableType {
    Eq,
    Eqv,
    Equal,
    String,
    Generic,
}


pub struct HashTable<'gc> {
    pub typ: HashTableType,
    inner: Monitor<InnerHashTable<'gc>>,
}

unsafe impl<'gc> Trace for HashTable<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.inner.get_mut().trace(visitor);
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for HashTable<'gc> {}



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
        } else if val.is::<String>() {
            let s = val.downcast::<String<'gc>>();
            s.hash(state);
        } else if val.is::<Symbol>() {
            let s = val.downcast::<Symbol<'gc>>();
            s.hash(state);
        } else if val.is::<Vector>() {
            let v = val.downcast::<Vector<'gc>>();
            for i in 0..v.len() {
                v[i].hash(state);
            }
        } else if val.is::<Pair>() {
            let p = val.downcast::<Pair>();
            p.car().hash(state);
            p.cdr().hash(state);
        } else if val.is::<ByteVector>() {
            let p = val.downcast::<ByteVector<'gc>>();
            (**p).hash(state);
        } else if val.is::<Tuple>() {
            let t = val.downcast::<Tuple<'gc>>();
            for i in 0..t.len() {
                t[i].hash(state);
            }
        }else {
            val.hash(state);
        }
    }
}


#[derive(Collect)]
#[collect(no_drop)]
struct Entry<'gc> {
    key: Value<'gc>,
    value: Value<'gc>,
    next: Option<Gc<'gc, Entry<'gc>>>,
}

type MaybeEntry<'gc> = Option<Gc<'gc, Entry<'gc>>>;

#[derive(Collect)]
#[collect(no_drop)]
struct InnerHashTable<'gc> {
    table: Lock<Gc<'gc, Array<Lock<MaybeEntry<'gc>>>>>,
    count: Cell<usize>,
    threshold: Cell<usize>,
    load_factor: f64,
    mod_count: Cell<usize>,
}

impl<'gc> HashTable<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        typ: HashTableType,
        initial_capacity: usize,
        load_factor: f64,
    ) -> Gc<'gc, Self> {
        let initial_capacity = initial_capacity.min(1);

        if load_factor <= 0.0 {
            panic!("Load factor must be greater than 0.0");
        }

        let table = Array::with_fn(&ctx, initial_capacity, |_| Lock::new(None));

        let threshold = (initial_capacity as f64 * load_factor).ceil() as usize;

        Gc::new(
            &ctx,
            Self {
                typ,
                inner: Monitor::new(InnerHashTable {
                    table: Lock::new(table),
                    count: Cell::new(0),
                    threshold: Cell::new(threshold),
                    load_factor,
                    mod_count: Cell::new(0),
                }),
            },
        )
    }
    
}

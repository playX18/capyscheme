use std::cell::Cell;

use rsgc::{alloc::array::Array, vmkit::sync::Monitor, Gc};

use super::{Callback, Value};



pub struct HashTable<'gc> {
    inner: Monitor<HashTableInner<'gc>>
}


struct HashTableInner<'gc> {
    hash: Callback<'gc>,
    equiv: Callback<'gc>,

    capacity: Cell<usize>,
    used: Cell<usize>,
    live: Cell<usize>,

    elements: Gc<'gc, Array<Value<'gc>>>
}


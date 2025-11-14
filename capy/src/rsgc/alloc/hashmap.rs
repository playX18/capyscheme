//! Swiss-Table HashMap implementation compatible with GC
//!

use std::{cell::Cell, hash::RandomState};

use crate::rsgc::{Gc, Trace};

use super::array::Array;

const GROUP_SIZE: usize = 8;
const MAX_AVG_GROUP_LOAD: usize = 7;
const CTRL_EMPTY: u64 = 0b10000000;
const CTRL_DELETED: u64 = 0b11111110;
const BITSET_LSB: u64 = 0x0101010101010101;
const BITSET_MSB: u64 = 0x8080808080808080;
const BITSET_EMPTY: u64 = BITSET_LSB * CTRL_EMPTY;
const BITSET_DELETED: u64 = BITSET_LSB * CTRL_DELETED;

const DEFAULT_MAX_BUCKET_CAPACITY: usize = 4096;

const EXPECTED_BUCKET_SIZE: usize = size_of::<usize>() + 6 * 4;

struct Slot<K, V> {
    key: K,
    value: V,
}

type ControlGroup = u64;

struct Group<'gc, K: Trace, V: Trace> {
    ctrls: ControlGroup,
    slots: SlotGroup<'gc, K, V>,
}

struct SlotGroup<'gc, K: Trace, V: Trace> {
    slots: Gc<'gc, Array<Slot<K, V>>>,
}

struct Bucket<'gc, K: Trace, V: Trace> {
    groups: Option<Gc<'gc, Array<Group<'gc, K, V>>>>,
    group_mask: Cell<u32>,
    capacity: Cell<u32>,
    used: Cell<u32>,
    growth_left: Cell<u32>,
    local_depth: Cell<u32>,
    index: Cell<u32>,
}

unsafe impl<'gc, K: Trace, V: Trace> Trace for Bucket<'gc, K, V> {
    unsafe fn trace(&mut self, visitor: &mut crate::collection::Visitor) {
        visitor.trace(&mut self.groups);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        weak_processor.process(&mut self.groups);
    }
}

unsafe impl<'gc, K: Trace, V: Trace> Trace for SlotGroup<'gc, K, V> {
    unsafe fn trace(&mut self, visitor: &mut crate::collection::Visitor) {
        visitor.trace(&mut self.slots);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        weak_processor.process(&mut self.slots);
    }
}

unsafe impl<K: Trace, V: Trace> Trace for Slot<K, V> {
    unsafe fn trace(&mut self, visitor: &mut crate::collection::Visitor) {
        visitor.trace(&mut self.key);
        visitor.trace(&mut self.value);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        weak_processor.process(&mut self.key);
        weak_processor.process(&mut self.value);
    }
}

unsafe impl<'gc, K: Trace, V: Trace> Trace for Group<'gc, K, V> {
    unsafe fn trace(&mut self, visitor: &mut crate::collection::Visitor) {
        visitor.trace(&mut self.slots);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        weak_processor.process(&mut self.slots);
    }
}

pub struct HashMap<'gc, K: Trace, V: Trace, S = RandomState> {
    state: S,
    dir: Gc<'gc, Array<Gc<'gc, Bucket<'gc, K, V>>>>,
    used: Cell<usize>,
    global_shift: Cell<u32>,
    max_bucket_capacity: Cell<u32>,
}

const fn normalize_capacity(capacity: u32) -> u32 {
    1 << u32::BITS - (capacity - 1).leading_zeros()
}

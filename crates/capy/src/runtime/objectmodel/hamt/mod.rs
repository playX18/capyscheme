//! Hash Array Mapped Trie (HAMT) for persistent maps and sets.

use crate::heap::Space;
use std::mem::{align_of, offset_of, size_of};

use crate::heap::{
    Gc, Trace,
    cell::Lock,
    collection::Visitor,
    object::{AllocationHooks, ClassId, GcObject, builtin_class_ids, class_header_word},
    weak::WeakProcessor,
};

use crate::runtime::MutationContext;

use super::{ClassTagged, HashTableType, IntoValue, Value};

const HAMT_SHIFT: u32 = 5;
const HAMT_WIDTH: usize = 32;
const HAMT_MASK: u32 = 31;
const HAMT_ARRAY_THRESHOLD: u32 = 16;

const TAG_LEAF: u8 = 0;
const TAG_BITMAP: u8 = 1;
const TAG_ARRAY: u8 = 2;
const TAG_COLLISION: u8 = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HamtKind {
    Map,
    Set,
}

fn hamt_node_header_word() -> u64 {
    class_header_word(
        ClassId::new(builtin_class_ids::HAMT_NODE).expect("builtin class id is nonzero"),
    )
}

fn fragment(hash: u32, shift: u32) -> u32 {
    (hash >> shift) & HAMT_MASK
}

fn popcount(mut x: u32) -> usize {
    let mut count = 0usize;
    while x != 0 {
        count += (x & 1) as usize;
        x >>= 1;
    }
    count
}

fn bit_index(bitmap: u32, bit: u32) -> usize {
    popcount(bitmap & (bit.wrapping_sub(1)))
}

#[repr(C, align(8))]
struct HamtLeafNode<'gc> {
    tag: u8,
    _pad: [u8; 3],
    hash: u32,
    key: Value<'gc>,
    value: Value<'gc>,
}

#[repr(C, align(8))]
struct HamtBitmapHeader<'gc> {
    tag: u8,
    _pad: [u8; 3],
    bitmap: u32,
    child_count: u32,
    children: [Gc<'gc, HamtNode>; 0],
}

#[repr(C, align(8))]
struct HamtArrayNode<'gc> {
    tag: u8,
    _pad: [u8; 7],
    children: [Lock<Option<Gc<'gc, HamtNode>>>; HAMT_WIDTH],
}

#[repr(C)]
struct HamtCollisionEntry<'gc> {
    hash: u32,
    key: Value<'gc>,
    value: Value<'gc>,
}

#[repr(C, align(8))]
struct HamtCollisionHeader<'gc> {
    tag: u8,
    _pad: [u8; 3],
    hash: u32,
    entry_count: u32,
    entries: [HamtCollisionEntry<'gc>; 0],
}

/// Internal HAMT trie node (not exposed to Scheme).
#[repr(C, align(8))]
pub struct HamtNode {
    tag: u8,
}

const _: () = {
    assert!(offset_of!(HamtLeafNode<'static>, tag) == 0);
    assert!(offset_of!(HamtBitmapHeader<'static>, tag) == 0);
    assert!(offset_of!(HamtArrayNode<'static>, tag) == 0);
    assert!(offset_of!(HamtCollisionHeader<'static>, tag) == 0);
};

unsafe fn node_tag(obj: GcObject) -> u8 {
    unsafe { *obj.to_address().as_ref::<u8>() }
}

extern "C" fn trace_hamt_node(obj: GcObject, vis: &mut Visitor) {
    unsafe {
        match node_tag(obj) {
            TAG_LEAF => {
                let leaf = obj.to_address().as_mut_ref::<HamtLeafNode<'static>>();
                leaf.key.trace(vis);
                leaf.value.trace(vis);
            }
            TAG_BITMAP => {
                let header = obj.to_address().as_mut_ref::<HamtBitmapHeader<'static>>();
                for i in 0..header.child_count as usize {
                    // Trace the field itself: the visitor records its address
                    // and the collector writes the forwarded pointer there.
                    let child = &mut *header.children.as_mut_ptr().add(i);
                    child.trace(vis);
                }
            }
            TAG_ARRAY => {
                let array = obj.to_address().as_mut_ref::<HamtArrayNode<'static>>();
                for child in &mut array.children {
                    if let Some(node) = child.get_mut() {
                        node.trace(vis);
                    }
                }
            }
            TAG_COLLISION => {
                let header = obj
                    .to_address()
                    .as_mut_ref::<HamtCollisionHeader<'static>>();
                for i in 0..header.entry_count as usize {
                    let entry = header
                        .entries
                        .as_mut_ptr()
                        .add(i)
                        .as_mut()
                        .expect("pointer in allocated entry range");
                    entry.key.trace(vis);
                    entry.value.trace(vis);
                }
            }
            _ => {}
        }
    }
}

extern "C" fn weak_hamt_node(_: GcObject, _: &mut WeakProcessor) {}

extern "C" fn compute_hamt_node_size(obj: GcObject) -> usize {
    unsafe {
        match node_tag(obj) {
            TAG_BITMAP => {
                let header = obj.to_address().as_ref::<HamtBitmapHeader<'static>>();
                size_of::<HamtBitmapHeader<'static>>()
                    + header.child_count as usize * size_of::<Gc<'static, HamtNode>>()
            }
            TAG_COLLISION => {
                let header = obj.to_address().as_ref::<HamtCollisionHeader<'static>>();
                size_of::<HamtCollisionHeader<'static>>()
                    + header.entry_count as usize * size_of::<HamtCollisionEntry<'static>>()
            }
            _ => size_of::<HamtLeafNode<'static>>().max(size_of::<HamtArrayNode<'static>>()),
        }
    }
}

impl HamtNode {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "HamtNode",
        instance_size: size_of::<HamtLeafNode<'static>>(),
        alignment: align_of::<HamtLeafNode<'static>>(),
        compute_alignment: None,
        compute_size: Some(compute_hamt_node_size),
        trace: trace_hamt_node,
        weak_proc: weak_hamt_node,
    };
}

mod persistent;
mod trie;

pub use persistent::{PersistentMap, PersistentSet};
pub use trie::HamtTrie;

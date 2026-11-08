use super::*;

fn alloc_leaf<'gc>(
    mc: MutationContext<'gc>,
    hash: u32,
    key: Value<'gc>,
    value: Value<'gc>,
) -> Gc<'gc, HamtNode> {
    let leaf = mc.allocate_with(
        HamtLeafNode {
            tag: TAG_LEAF,
            _pad: [0; 3],
            hash,
            key,
            value,
        },
        hamt_node_header_word(),
    );
    // SAFETY: HamtLeafNode shares the HamtNode header prefix.
    unsafe { Gc::from_gc_object(leaf.as_gc_object()) }
}

fn alloc_bitmap<'gc>(
    mc: MutationContext<'gc>,
    bitmap: u32,
    children: &[Gc<'gc, HamtNode>],
) -> Gc<'gc, HamtNode> {
    let child_count = children.len() as u32;
    let size = size_of::<HamtBitmapHeader<'gc>>() + size_of_val(children);
    unsafe {
        let alloc = mc.allocate_raw(
            size,
            align_of::<HamtBitmapHeader<'gc>>(),
            hamt_node_header_word(),
            Space::Default,
        );
        let header = alloc.to_address().as_mut_ref::<HamtBitmapHeader<'gc>>();
        header.tag = TAG_BITMAP;
        header._pad = [0; 3];
        header.bitmap = bitmap;
        header.child_count = child_count;
        for (i, child) in children.iter().enumerate() {
            header.children.as_mut_ptr().add(i).write(*child);
        }
        // SAFETY: Allocation matches HamtNode layout prefix.
        Gc::from_gc_object(alloc)
    }
}

fn alloc_array<'gc>(
    mc: MutationContext<'gc>,
    children: [Option<Gc<'gc, HamtNode>>; HAMT_WIDTH],
) -> Gc<'gc, HamtNode> {
    let mut locks = [const { Lock::new(None) }; HAMT_WIDTH];
    for (i, child) in children.into_iter().enumerate() {
        locks[i] = Lock::new(child);
    }
    let array = mc.allocate_with(
        HamtArrayNode {
            tag: TAG_ARRAY,
            _pad: [0; 7],
            children: locks,
        },
        hamt_node_header_word(),
    );
    // SAFETY: HamtArrayNode shares the HamtNode header prefix.
    unsafe { Gc::from_gc_object(array.as_gc_object()) }
}

fn alloc_collision<'gc>(
    mc: MutationContext<'gc>,
    hash: u32,
    entries: &[(Value<'gc>, Value<'gc>, u32)],
) -> Gc<'gc, HamtNode> {
    let size = size_of::<HamtCollisionHeader<'gc>>()
        + entries.len() * size_of::<HamtCollisionEntry<'gc>>();
    unsafe {
        let alloc = mc.allocate_raw(
            size,
            align_of::<HamtCollisionHeader<'gc>>(),
            hamt_node_header_word(),
            Space::Default,
        );
        let header = alloc.to_address().as_mut_ref::<HamtCollisionHeader<'gc>>();
        header.tag = TAG_COLLISION;
        header._pad = [0; 3];
        header.hash = hash;
        header.entry_count = entries.len() as u32;
        for (i, (key, value, entry_hash)) in entries.iter().enumerate() {
            header
                .entries
                .as_mut_ptr()
                .add(i)
                .write(HamtCollisionEntry {
                    hash: *entry_hash,
                    key: *key,
                    value: *value,
                });
        }
        // SAFETY: Allocation matches HamtNode layout prefix.
        Gc::from_gc_object(alloc)
    }
}

fn array_to_bitmap<'gc>(
    mc: MutationContext<'gc>,
    children: &[Option<Gc<'gc, HamtNode>>; HAMT_WIDTH],
) -> Option<Gc<'gc, HamtNode>> {
    let mut bitmap = 0u32;
    let mut compact = Vec::new();
    for (i, child) in children.iter().enumerate() {
        if let Some(node) = child {
            bitmap |= 1 << i;
            compact.push(*node);
        }
    }
    if compact.is_empty() {
        None
    } else if compact.len() as u32 >= HAMT_ARRAY_THRESHOLD {
        Some(alloc_array(mc, *children))
    } else {
        Some(alloc_bitmap(mc, bitmap, &compact))
    }
}

fn hamt_get<'gc>(
    typ: HashTableType<'gc>,
    node: Gc<'gc, HamtNode>,
    shift: u32,
    key: Value<'gc>,
    hash: u32,
) -> Option<Value<'gc>> {
    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                if typ.equal(key, leaf.key) {
                    Some(leaf.value)
                } else {
                    None
                }
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                let idx = fragment(hash, shift);
                let bit = 1u32 << idx;
                if header.bitmap & bit == 0 {
                    return None;
                }
                let child_idx = bit_index(header.bitmap, bit);
                let child = header.children.as_ptr().add(child_idx).read();
                hamt_get(typ, child, shift + HAMT_SHIFT, key, hash)
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                let idx = fragment(hash, shift) as usize;
                if let Some(child) = array.children[idx].get() {
                    hamt_get(typ, child, shift + HAMT_SHIFT, key, hash)
                } else {
                    None
                }
            }
            TAG_COLLISION => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtCollisionHeader<'gc>>();
                for i in 0..header.entry_count as usize {
                    let entry = header
                        .entries
                        .as_ptr()
                        .add(i)
                        .as_ref()
                        .expect("pointer in allocated entry range");
                    if typ.equal(key, entry.key) {
                        return Some(entry.value);
                    }
                }
                None
            }
            _ => None,
        }
    }
}

#[allow(clippy::too_many_arguments)] // Represents the two leaf entries being merged into a HAMT node.
fn make_leaf_or_collision<'gc>(
    mc: MutationContext<'gc>,
    shift: u32,
    key1: Value<'gc>,
    value1: Value<'gc>,
    hash1: u32,
    key2: Value<'gc>,
    value2: Value<'gc>,
    hash2: u32,
) -> Gc<'gc, HamtNode> {
    if shift >= 32 {
        return alloc_collision(mc, hash1, &[(key1, value1, hash1), (key2, value2, hash2)]);
    }
    let f1 = fragment(hash1, shift);
    let f2 = fragment(hash2, shift);
    if f1 == f2 {
        let child = make_leaf_or_collision(
            mc,
            shift + HAMT_SHIFT,
            key1,
            value1,
            hash1,
            key2,
            value2,
            hash2,
        );
        let bit = 1u32 << f1;
        alloc_bitmap(mc, bit, &[child])
    } else {
        let leaf1 = alloc_leaf(mc, hash1, key1, value1);
        let leaf2 = alloc_leaf(mc, hash2, key2, value2);
        let bitmap = (1u32 << f1) | (1u32 << f2);
        let mut children = vec![leaf1, leaf2];
        if f1 > f2 {
            children.swap(0, 1);
        }
        alloc_bitmap(mc, bitmap, &children)
    }
}

fn hamt_assoc_leaf<'gc>(
    mc: MutationContext<'gc>,
    typ: HashTableType<'gc>,
    shift: u32,
    leaf: &HamtLeafNode<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    hash: u32,
) -> (Gc<'gc, HamtNode>, bool) {
    if typ.equal(key, leaf.key) {
        (alloc_leaf(mc, hash, key, value), false)
    } else {
        (
            make_leaf_or_collision(mc, shift, leaf.key, leaf.value, leaf.hash, key, value, hash),
            true,
        )
    }
}

fn hamt_assoc_collision<'gc>(
    mc: MutationContext<'gc>,
    typ: HashTableType<'gc>,
    header: &HamtCollisionHeader<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    hash: u32,
) -> (Gc<'gc, HamtNode>, bool) {
    let mut entries = Vec::with_capacity(header.entry_count as usize + 1);
    let mut found = false;
    for i in 0..header.entry_count as usize {
        let entry = unsafe {
            header
                .entries
                .as_ptr()
                .add(i)
                .as_ref()
                .expect("pointer in allocated entry range")
        };
        if typ.equal(key, entry.key) {
            entries.push((key, value, hash));
            found = true;
        } else {
            entries.push((entry.key, entry.value, entry.hash));
        }
    }
    if !found {
        entries.push((key, value, hash));
    }
    (alloc_collision(mc, header.hash, &entries), !found)
}

pub(super) fn hamt_assoc<'gc>(
    mc: MutationContext<'gc>,
    typ: HashTableType<'gc>,
    node: Option<Gc<'gc, HamtNode>>,
    shift: u32,
    key: Value<'gc>,
    value: Value<'gc>,
    hash: u32,
) -> (Option<Gc<'gc, HamtNode>>, bool) {
    let Some(node) = node else {
        return (Some(alloc_leaf(mc, hash, key, value)), true);
    };

    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                let (new_node, added) = hamt_assoc_leaf(mc, typ, shift, leaf, key, value, hash);
                (Some(new_node), added)
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                let idx = fragment(hash, shift);
                let bit = 1u32 << idx;
                if header.bitmap & bit == 0 {
                    let leaf = alloc_leaf(mc, hash, key, value);
                    let mut children: Vec<Gc<'gc, HamtNode>> = (0..header.child_count as usize)
                        .map(|i| header.children.as_ptr().add(i).read())
                        .collect();
                    let insert_at = bit_index(header.bitmap | bit, bit);
                    children.insert(insert_at, leaf);
                    let new_bitmap = header.bitmap | bit;
                    if popcount(new_bitmap) as u32 >= HAMT_ARRAY_THRESHOLD {
                        let mut slots = [None; HAMT_WIDTH];
                        let mut ci = 0usize;
                        for (i, slot) in slots.iter_mut().enumerate() {
                            let b = 1u32 << i;
                            if header.bitmap & b != 0 {
                                *slot = Some(header.children.as_ptr().add(ci).read());
                                ci += 1;
                            }
                        }
                        slots[idx as usize] = Some(leaf);
                        (Some(alloc_array(mc, slots)), true)
                    } else {
                        (Some(alloc_bitmap(mc, new_bitmap, &children)), true)
                    }
                } else {
                    let child_idx = bit_index(header.bitmap, bit);
                    let child = header.children.as_ptr().add(child_idx).read();
                    let (new_child, added) =
                        hamt_assoc(mc, typ, Some(child), shift + HAMT_SHIFT, key, value, hash);
                    let mut children: Vec<Gc<'gc, HamtNode>> = (0..header.child_count as usize)
                        .map(|i| header.children.as_ptr().add(i).read())
                        .collect();
                    children[child_idx] = new_child.expect("HAMT child present");
                    (Some(alloc_bitmap(mc, header.bitmap, &children)), added)
                }
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                let idx = fragment(hash, shift) as usize;
                if array.children[idx].get().is_none() {
                    let leaf = alloc_leaf(mc, hash, key, value);
                    let mut slots: [Option<Gc<'gc, HamtNode>>; HAMT_WIDTH] = [None; HAMT_WIDTH];
                    for (i, slot) in slots.iter_mut().enumerate() {
                        *slot = array.children[i].get();
                    }
                    slots[idx] = Some(leaf);
                    let occupied = slots.iter().filter(|s| s.is_some()).count() as u32;
                    if occupied < HAMT_ARRAY_THRESHOLD {
                        if let Some(bitmap_node) = array_to_bitmap(mc, &slots) {
                            (Some(bitmap_node), true)
                        } else {
                            (None, false)
                        }
                    } else {
                        (Some(alloc_array(mc, slots)), true)
                    }
                } else {
                    let child = array.children[idx].get().expect("index in range");
                    let (new_child, added) =
                        hamt_assoc(mc, typ, Some(child), shift + HAMT_SHIFT, key, value, hash);
                    let mut slots: [Option<Gc<'gc, HamtNode>>; HAMT_WIDTH] = [None; HAMT_WIDTH];
                    for (i, slot) in slots.iter_mut().enumerate() {
                        *slot = array.children[i].get();
                    }
                    slots[idx] = new_child;
                    (Some(alloc_array(mc, slots)), added)
                }
            }
            TAG_COLLISION => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtCollisionHeader<'gc>>();
                let (new_node, added) = hamt_assoc_collision(mc, typ, header, key, value, hash);
                (Some(new_node), added)
            }
            _ => (Some(alloc_leaf(mc, hash, key, value)), true),
        }
    }
}

fn hamt_remove_leaf<'gc>(
    _typ: HashTableType<'gc>,
    _leaf: &HamtLeafNode<'gc>,
    _key: Value<'gc>,
) -> Option<Gc<'gc, HamtNode>> {
    None
}

fn hamt_remove_collision<'gc>(
    typ: HashTableType<'gc>,
    mc: MutationContext<'gc>,
    header: &HamtCollisionHeader<'gc>,
    key: Value<'gc>,
) -> (Option<Gc<'gc, HamtNode>>, bool) {
    let mut entries = Vec::new();
    let mut removed = false;
    for i in 0..header.entry_count as usize {
        let entry = unsafe {
            header
                .entries
                .as_ptr()
                .add(i)
                .as_ref()
                .expect("pointer in allocated entry range")
        };
        if typ.equal(key, entry.key) {
            removed = true;
        } else {
            entries.push((entry.key, entry.value, entry.hash));
        }
    }
    if !removed {
        return (None, false);
    }
    if entries.is_empty() {
        (None, true)
    } else if entries.len() == 1 {
        let (k, v, h) = entries[0];
        (Some(alloc_leaf(mc, h, k, v)), true)
    } else {
        (Some(alloc_collision(mc, header.hash, &entries)), true)
    }
}

pub(super) fn hamt_remove<'gc>(
    mc: MutationContext<'gc>,
    typ: HashTableType<'gc>,
    node: Gc<'gc, HamtNode>,
    shift: u32,
    key: Value<'gc>,
    hash: u32,
) -> (Option<Gc<'gc, HamtNode>>, bool) {
    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                if typ.equal(key, leaf.key) {
                    (hamt_remove_leaf(typ, leaf, key), true)
                } else {
                    (Some(node), false)
                }
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                let idx = fragment(hash, shift);
                let bit = 1u32 << idx;
                if header.bitmap & bit == 0 {
                    return (Some(node), false);
                }
                let child_idx = bit_index(header.bitmap, bit);
                let child = header.children.as_ptr().add(child_idx).read();
                let (new_child, removed) =
                    hamt_remove(mc, typ, child, shift + HAMT_SHIFT, key, hash);
                if !removed {
                    return (Some(node), false);
                }
                if let Some(new_child) = new_child {
                    let mut children: Vec<Gc<'gc, HamtNode>> = (0..header.child_count as usize)
                        .map(|i| header.children.as_ptr().add(i).read())
                        .collect();
                    children[child_idx] = new_child;
                    (Some(alloc_bitmap(mc, header.bitmap, &children)), true)
                } else {
                    let new_bitmap = header.bitmap & !bit;
                    if new_bitmap == 0 {
                        return (None, true);
                    }
                    let mut children = Vec::new();
                    for i in 0..header.child_count as usize {
                        if i != child_idx {
                            children.push(header.children.as_ptr().add(i).read());
                        }
                    }
                    if children.len() == 1 && popcount(new_bitmap) == 1 {
                        return (Some(children[0]), true);
                    }
                    if popcount(new_bitmap) as u32 >= HAMT_ARRAY_THRESHOLD {
                        let mut slots = [None; HAMT_WIDTH];
                        let mut ci = 0usize;
                        for (i, slot) in slots.iter_mut().enumerate() {
                            let b = 1u32 << i;
                            if header.bitmap & b != 0 && i != idx as usize {
                                *slot = Some(header.children.as_ptr().add(ci).read());
                            }
                            if header.bitmap & b != 0 {
                                ci += 1;
                            }
                        }
                        (Some(alloc_array(mc, slots)), true)
                    } else {
                        (Some(alloc_bitmap(mc, new_bitmap, &children)), true)
                    }
                }
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                let idx = fragment(hash, shift) as usize;
                let Some(child) = array.children[idx].get() else {
                    return (Some(node), false);
                };
                let (new_child, removed) =
                    hamt_remove(mc, typ, child, shift + HAMT_SHIFT, key, hash);
                if !removed {
                    return (Some(node), false);
                }
                let mut slots: [Option<Gc<'gc, HamtNode>>; HAMT_WIDTH] = [None; HAMT_WIDTH];
                for (i, slot) in slots.iter_mut().enumerate() {
                    *slot = array.children[i].get();
                }
                slots[idx] = new_child;
                let occupied = slots.iter().filter(|s| s.is_some()).count();
                if occupied == 0 {
                    (None, true)
                } else if occupied == 1 {
                    (slots.into_iter().flatten().next(), true)
                } else if (occupied as u32) < HAMT_ARRAY_THRESHOLD {
                    (array_to_bitmap(mc, &slots), true)
                } else {
                    (Some(alloc_array(mc, slots)), true)
                }
            }
            TAG_COLLISION => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtCollisionHeader<'gc>>();
                hamt_remove_collision(typ, mc, header, key)
            }
            _ => (Some(node), false),
        }
    }
}

/// Shared HAMT trie storage for persistent maps and sets.
pub struct HamtTrie<'gc> {
    pub(crate) root: Lock<Option<Gc<'gc, HamtNode>>>,
    pub(crate) count: usize,
    pub(crate) typ: HashTableType<'gc>,
    pub(crate) kind: HamtKind,
}

// SAFETY: `gc` for `HamtTrie` upholds all trait invariants
unsafe impl<'gc> Trace for HamtTrie<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.root.trace(visitor);
            self.typ.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, _: &mut WeakProcessor) {}
}

impl<'gc> HamtTrie<'gc> {
    pub fn new(_mc: MutationContext<'gc>, typ: HashTableType<'gc>, kind: HamtKind) -> Self {
        Self {
            root: Lock::new(None),
            count: 0,
            typ,
            kind,
        }
    }

    pub fn len(&self) -> usize {
        self.count
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn typ(&self) -> HashTableType<'gc> {
        self.typ
    }

    pub fn kind(&self) -> HamtKind {
        self.kind
    }

    pub(super) fn key_hash(&self, key: Value<'gc>) -> u32 {
        self.typ.hash(key) as u32
    }

    pub fn get(&self, key: Value<'gc>) -> Option<Value<'gc>> {
        let root = self.root.get()?;
        let hash = self.key_hash(key);
        hamt_get(self.typ, root, 0, key, hash)
    }

    pub fn contains_key(&self, key: Value<'gc>) -> bool {
        self.get(key).is_some()
    }

    pub fn fold<F>(&self, mut f: F, init: Value<'gc>) -> Value<'gc>
    where
        F: FnMut(Value<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
    {
        fn walk<'gc, F>(
            node: Gc<'gc, HamtNode>,
            f: &mut F,
            acc: Value<'gc>,
            kind: HamtKind,
        ) -> Value<'gc>
        where
            F: FnMut(Value<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
        {
            unsafe {
                match node_tag(node.as_gc_object()) {
                    TAG_LEAF => {
                        let leaf = node
                            .as_gc_object()
                            .to_address()
                            .as_ref::<HamtLeafNode<'gc>>();
                        match kind {
                            HamtKind::Map => f(acc, leaf.key, leaf.value),
                            HamtKind::Set => f(acc, leaf.key, Value::undefined()),
                        }
                    }
                    TAG_BITMAP => {
                        let header = node
                            .as_gc_object()
                            .to_address()
                            .as_ref::<HamtBitmapHeader<'gc>>();
                        let mut acc = acc;
                        for i in 0..header.child_count as usize {
                            let child = header.children.as_ptr().add(i).read();
                            acc = walk(child, f, acc, kind);
                        }
                        acc
                    }
                    TAG_ARRAY => {
                        let array = node
                            .as_gc_object()
                            .to_address()
                            .as_ref::<HamtArrayNode<'gc>>();
                        let mut acc = acc;
                        for child in &array.children {
                            if let Some(node) = child.get() {
                                acc = walk(node, f, acc, kind);
                            }
                        }
                        acc
                    }
                    TAG_COLLISION => {
                        let header = node
                            .as_gc_object()
                            .to_address()
                            .as_ref::<HamtCollisionHeader<'gc>>();
                        let mut acc = acc;
                        for i in 0..header.entry_count as usize {
                            let entry = header
                                .entries
                                .as_ptr()
                                .add(i)
                                .as_ref()
                                .expect("pointer in allocated entry range");
                            acc = match kind {
                                HamtKind::Map => f(acc, entry.key, entry.value),
                                HamtKind::Set => f(acc, entry.key, Value::undefined()),
                            };
                        }
                        acc
                    }
                    _ => acc,
                }
            }
        }

        if let Some(root) = self.root.get() {
            walk(root, &mut f, init, self.kind)
        } else {
            init
        }
    }
}


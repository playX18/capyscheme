use super::*;
use super::trie::*;

fn persistent_header_word(kind: HamtKind) -> u64 {
    let class_id = match kind {
        HamtKind::Map => builtin_class_ids::PERSISTENT_MAP,
        HamtKind::Set => builtin_class_ids::PERSISTENT_SET,
    };
    class_header_word(ClassId::new(class_id).expect("builtin class id is nonzero"))
}

/// A persistent HAMT-backed map.
#[repr(C, align(8))]
#[derive(Trace)]
#[collect(no_drop)]
pub struct PersistentMap<'gc> {
    pub(crate) trie: HamtTrie<'gc>,
}

impl<'gc> PersistentMap<'gc> {
    pub fn new(mc: MutationContext<'gc>, typ: HashTableType<'gc>) -> Gc<'gc, Self> {
        mc.allocate_with(
            Self {
                trie: HamtTrie::new(mc, typ, HamtKind::Map),
            },
            persistent_header_word(HamtKind::Map),
        )
    }

    pub fn len(&self) -> usize {
        self.trie.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn typ(&self) -> HashTableType<'gc> {
        self.trie.typ()
    }

    pub fn get(&self, key: Value<'gc>) -> Option<Value<'gc>> {
        self.trie.get(key)
    }

    pub fn contains_key(&self, key: Value<'gc>) -> bool {
        self.trie.contains_key(key)
    }

    pub fn assoc(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        key: impl IntoValue<'gc>,
        value: impl IntoValue<'gc>,
    ) -> Gc<'gc, Self> {
        let key = key.into_value(ctx);
        let value = value.into_value(ctx);
        let hash = self.trie.key_hash(key);
        let (new_root, added) = hamt_assoc(
            ctx,
            self.trie.typ,
            self.trie.root.get(),
            0,
            key,
            value,
            hash,
        );
        ctx.allocate_with(
            Self {
                trie: HamtTrie {
                    root: Lock::new(new_root),
                    count: if added {
                        self.trie.count + 1
                    } else {
                        self.trie.count
                    },
                    typ: self.trie.typ,
                    kind: HamtKind::Map,
                },
            },
            persistent_header_word(HamtKind::Map),
        )
    }

    pub fn dissoc(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        key: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let Some(root) = self.trie.root.get() else {
            return self;
        };
        let hash = self.trie.key_hash(key);
        let (new_root, removed) = hamt_remove(ctx, self.trie.typ, root, 0, key, hash);
        if !removed {
            return self;
        }
        ctx.allocate_with(
            Self {
                trie: HamtTrie {
                    root: Lock::new(new_root),
                    count: self.trie.count.saturating_sub(1),
                    typ: self.trie.typ,
                    kind: HamtKind::Map,
                },
            },
            persistent_header_word(HamtKind::Map),
        )
    }

    pub fn fold<F>(&self, f: F, init: Value<'gc>) -> Value<'gc>
    where
        F: FnMut(Value<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
    {
        self.trie.fold(f, init)
    }

    pub fn from_alist(
        ctx: MutationContext<'gc>,
        typ: HashTableType<'gc>,
        pairs: &[(Value<'gc>, Value<'gc>)],
    ) -> Gc<'gc, Self> {
        let mut map = Self::new(ctx, typ);
        for (key, value) in pairs {
            map = map.assoc(ctx, *key, *value);
        }
        map
    }
}

// SAFETY: `gc` for `PersistentMap` upholds all trait invariants
unsafe impl<'gc> ClassTagged for PersistentMap<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::PERSISTENT_MAP];
    const TYPE_NAME: &'static str = "#<persistent-map>";
}

/// A persistent HAMT-backed set.
#[repr(C, align(8))]
#[derive(Trace)]
#[collect(no_drop)]
pub struct PersistentSet<'gc> {
    pub(crate) trie: HamtTrie<'gc>,
}

impl<'gc> PersistentSet<'gc> {
    pub fn new(mc: MutationContext<'gc>, typ: HashTableType<'gc>) -> Gc<'gc, Self> {
        mc.allocate_with(
            Self {
                trie: HamtTrie::new(mc, typ, HamtKind::Set),
            },
            persistent_header_word(HamtKind::Set),
        )
    }

    pub fn len(&self) -> usize {
        self.trie.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn typ(&self) -> HashTableType<'gc> {
        self.trie.typ()
    }

    pub fn contains(&self, key: Value<'gc>) -> bool {
        self.trie.contains_key(key)
    }

    pub fn add(self: Gc<'gc, Self>, ctx: MutationContext<'gc>, key: Value<'gc>) -> Gc<'gc, Self> {
        let hash = self.trie.key_hash(key);
        let (new_root, added) = hamt_assoc(
            ctx,
            self.trie.typ,
            self.trie.root.get(),
            0,
            key,
            Value::undefined(),
            hash,
        );
        ctx.allocate_with(
            Self {
                trie: HamtTrie {
                    root: Lock::new(new_root),
                    count: if added {
                        self.trie.count + 1
                    } else {
                        self.trie.count
                    },
                    typ: self.trie.typ,
                    kind: HamtKind::Set,
                },
            },
            persistent_header_word(HamtKind::Set),
        )
    }

    pub fn remove(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        key: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let Some(root) = self.trie.root.get() else {
            return self;
        };
        let hash = self.trie.key_hash(key);
        let (new_root, removed) = hamt_remove(ctx, self.trie.typ, root, 0, key, hash);
        if !removed {
            return self;
        }
        ctx.allocate_with(
            Self {
                trie: HamtTrie {
                    root: Lock::new(new_root),
                    count: self.trie.count.saturating_sub(1),
                    typ: self.trie.typ,
                    kind: HamtKind::Set,
                },
            },
            persistent_header_word(HamtKind::Set),
        )
    }

    pub fn fold<F>(&self, f: F, init: Value<'gc>) -> Value<'gc>
    where
        F: FnMut(Value<'gc>, Value<'gc>, Value<'gc>) -> Value<'gc>,
    {
        self.trie.fold(f, init)
    }

    pub fn union(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        other: Gc<'gc, Self>,
    ) -> Gc<'gc, Self> {
        let mut result = self;
        if let Some(root) = other.trie.root.get() {
            result = fold_set_into(ctx, result, root);
        }
        result
    }

    pub fn intersection(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        other: Gc<'gc, Self>,
    ) -> Gc<'gc, Self> {
        let mut result = Self::new(ctx, self.typ());
        if let Some(root) = self.trie.root.get() {
            result = intersect_nodes(ctx, result, root, &other);
        }
        result
    }

    pub fn difference(
        self: Gc<'gc, Self>,
        ctx: MutationContext<'gc>,
        other: Gc<'gc, Self>,
    ) -> Gc<'gc, Self> {
        let mut result = Self::new(ctx, self.typ());
        if let Some(root) = self.trie.root.get() {
            result = diff_nodes(ctx, result, root, &other);
        }
        result
    }
}

fn fold_set_into<'gc>(
    ctx: MutationContext<'gc>,
    mut set: Gc<'gc, PersistentSet<'gc>>,
    node: Gc<'gc, HamtNode>,
) -> Gc<'gc, PersistentSet<'gc>> {
    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                set.add(ctx, leaf.key)
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                for i in 0..header.child_count as usize {
                    let child = header.children.as_ptr().add(i).read();
                    set = fold_set_into(ctx, set, child);
                }
                set
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                for child in &array.children {
                    if let Some(node) = child.get() {
                        set = fold_set_into(ctx, set, node);
                    }
                }
                set
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
                    set = set.add(ctx, entry.key);
                }
                set
            }
            _ => set,
        }
    }
}

fn intersect_nodes<'gc>(
    ctx: MutationContext<'gc>,
    mut result: Gc<'gc, PersistentSet<'gc>>,
    node: Gc<'gc, HamtNode>,
    other: &PersistentSet<'gc>,
) -> Gc<'gc, PersistentSet<'gc>> {
    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                if other.contains(leaf.key) {
                    result = result.add(ctx, leaf.key);
                }
                result
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                for i in 0..header.child_count as usize {
                    let child = header.children.as_ptr().add(i).read();
                    result = intersect_nodes(ctx, result, child, other);
                }
                result
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                for child in &array.children {
                    if let Some(node) = child.get() {
                        result = intersect_nodes(ctx, result, node, other);
                    }
                }
                result
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
                    if other.contains(entry.key) {
                        result = result.add(ctx, entry.key);
                    }
                }
                result
            }
            _ => result,
        }
    }
}

fn diff_nodes<'gc>(
    ctx: MutationContext<'gc>,
    mut result: Gc<'gc, PersistentSet<'gc>>,
    node: Gc<'gc, HamtNode>,
    other: &PersistentSet<'gc>,
) -> Gc<'gc, PersistentSet<'gc>> {
    unsafe {
        match node_tag(node.as_gc_object()) {
            TAG_LEAF => {
                let leaf = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtLeafNode<'gc>>();
                if !other.contains(leaf.key) {
                    result = result.add(ctx, leaf.key);
                }
                result
            }
            TAG_BITMAP => {
                let header = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtBitmapHeader<'gc>>();
                for i in 0..header.child_count as usize {
                    let child = header.children.as_ptr().add(i).read();
                    result = diff_nodes(ctx, result, child, other);
                }
                result
            }
            TAG_ARRAY => {
                let array = node
                    .as_gc_object()
                    .to_address()
                    .as_ref::<HamtArrayNode<'gc>>();
                for child in &array.children {
                    if let Some(node) = child.get() {
                        result = diff_nodes(ctx, result, node, other);
                    }
                }
                result
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
                    if !other.contains(entry.key) {
                        result = result.add(ctx, entry.key);
                    }
                }
                result
            }
            _ => result,
        }
    }
}

// SAFETY: `gc` for `PersistentSet` upholds all trait invariants
unsafe impl<'gc> ClassTagged for PersistentSet<'gc> {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::PERSISTENT_SET];
    const TYPE_NAME: &'static str = "#<persistent-set>";
}

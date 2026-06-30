use crate::prelude::*;
use crate::rsgc::Gc;

pub fn init_persistent_set<'gc>(ctx: Context<'gc>) {
    persistent_set_ops::register(ctx);
}

fn list_to_set_impl<'gc>(
    nctx: NativeCallContext<'_, 'gc>,
    lst: Value<'gc>,
    typ: HashTableType<'gc>,
    name: &str,
) -> NativeCallReturn<'gc> {
    if !lst.is_list() {
        return nctx.wrong_argument_violation(
            name,
            "expected a list",
            Some(lst),
            Some(1),
            1,
            &[lst],
        );
    }

    let ctx = nctx.ctx;
    let mut set = PersistentSet::new(*ctx, typ);
    let mut current = lst;
    while current.is_pair() {
        set = set.add(ctx, current.car());
        current = current.cdr();
    }

    nctx.return_(set.into())
}

#[scheme(path = capy)]
pub mod persistent_set_ops {
    #[scheme(name = "core-persistent-set?")]
    pub fn is_persistent_set(val: Value<'gc>) -> bool {
        nctx.return_(val.is::<PersistentSet>())
    }

    #[scheme(name = "make-core-persistent-set-eq")]
    pub fn make_persistent_set_eq() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentSet::new(*ctx, HashTableType::Eq).into())
    }

    #[scheme(name = "make-core-persistent-set-eqv")]
    pub fn make_persistent_set_eqv() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentSet::new(*ctx, HashTableType::Eqv).into())
    }

    #[scheme(name = "make-core-persistent-set-equal")]
    pub fn make_persistent_set_equal() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentSet::new(*ctx, HashTableType::Equal).into())
    }

    #[scheme(name = "core-persistent-set-contains?")]
    pub fn persistent_set_contains(set: Gc<'gc, PersistentSet<'gc>>, key: Value<'gc>) -> bool {
        nctx.return_(set.contains(key))
    }

    #[scheme(name = "core-persistent-set-add")]
    pub fn persistent_set_add(set: Gc<'gc, PersistentSet<'gc>>, key: Value<'gc>) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(set.add(ctx, key).into())
    }

    #[scheme(name = "core-persistent-set-remove")]
    pub fn persistent_set_remove(set: Gc<'gc, PersistentSet<'gc>>, key: Value<'gc>) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(set.remove(ctx, key).into())
    }

    #[scheme(name = "core-persistent-set-count")]
    pub fn persistent_set_count(set: Gc<'gc, PersistentSet<'gc>>) -> usize {
        nctx.return_(set.len())
    }

    #[scheme(name = "core-persistent-set->list")]
    pub fn persistent_set_to_list(set: Gc<'gc, PersistentSet<'gc>>) -> Value<'gc> {
        let ls = set.fold(
            |acc, key, _| Value::cons(nctx.ctx, key, acc),
            Value::null(),
        );
        nctx.return_(ls)
    }

    #[scheme(name = "core-persistent-set-union")]
    pub fn persistent_set_union(
        set1: Gc<'gc, PersistentSet<'gc>>,
        set2: Gc<'gc, PersistentSet<'gc>>,
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(set1.union(ctx, set2).into())
    }

    #[scheme(name = "core-persistent-set-intersection")]
    pub fn persistent_set_intersection(
        set1: Gc<'gc, PersistentSet<'gc>>,
        set2: Gc<'gc, PersistentSet<'gc>>,
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(set1.intersection(ctx, set2).into())
    }

    #[scheme(name = "core-persistent-set-difference")]
    pub fn persistent_set_difference(
        set1: Gc<'gc, PersistentSet<'gc>>,
        set2: Gc<'gc, PersistentSet<'gc>>,
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(set1.difference(ctx, set2).into())
    }

    #[scheme(name = "list->core-persistent-set-eq")]
    pub fn list_to_persistent_set_eq(lst: Value<'gc>) -> Value<'gc> {
        list_to_set_impl(nctx, lst, HashTableType::Eq, "list->core-persistent-set-eq")
    }

    #[scheme(name = "list->core-persistent-set-eqv")]
    pub fn list_to_persistent_set_eqv(lst: Value<'gc>) -> Value<'gc> {
        list_to_set_impl(nctx, lst, HashTableType::Eqv, "list->core-persistent-set-eqv")
    }

    #[scheme(name = "list->core-persistent-set-equal")]
    pub fn list_to_persistent_set_equal(lst: Value<'gc>) -> Value<'gc> {
        list_to_set_impl(nctx, lst, HashTableType::Equal, "list->core-persistent-set-equal")
    }
}

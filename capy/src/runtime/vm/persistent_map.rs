use crate::prelude::*;
use crate::rsgc::Gc;

pub fn init_persistent_map<'gc>(ctx: Context<'gc>) {
    persistent_map_ops::register(ctx);
}

fn alist_to_map_impl<'gc>(
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
    let mut map = PersistentMap::new(*ctx, typ);
    let mut current = lst;
    while current.is_pair() {
        let cell = current.car();
        if !cell.is_pair() {
            return nctx.wrong_argument_violation(
                name,
                "expected a list of pairs",
                Some(lst),
                Some(1),
                1,
                &[lst],
            );
        }
        map = map.assoc(ctx, cell.car(), cell.cdr());
        current = current.cdr();
    }

    nctx.return_(map.into())
}

#[scheme(path = capy)]
pub mod persistent_map_ops {
    #[scheme(name = "core-persistent-map?")]
    pub fn is_persistent_map(val: Value<'gc>) -> bool {
        nctx.return_(val.is::<PersistentMap>())
    }

    #[scheme(name = "make-core-persistent-map-eq")]
    pub fn make_persistent_map_eq() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentMap::new(*ctx, HashTableType::Eq).into())
    }

    #[scheme(name = "make-core-persistent-map-eqv")]
    pub fn make_persistent_map_eqv() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentMap::new(*ctx, HashTableType::Eqv).into())
    }

    #[scheme(name = "make-core-persistent-map-equal")]
    pub fn make_persistent_map_equal() -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(PersistentMap::new(*ctx, HashTableType::Equal).into())
    }

    #[scheme(name = "core-persistent-map-ref")]
    pub fn persistent_map_ref(
        map: Gc<'gc, PersistentMap<'gc>>,
        key: Value<'gc>,
        default_value: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let res = map
            .get(key)
            .unwrap_or(default_value.unwrap_or(Value::new(false)));
        nctx.return_(res)
    }

    #[scheme(name = "core-persistent-map-assoc")]
    pub fn persistent_map_assoc(
        map: Gc<'gc, PersistentMap<'gc>>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(map.assoc(ctx, key, value).into())
    }

    #[scheme(name = "core-persistent-map-dissoc")]
    pub fn persistent_map_dissoc(
        map: Gc<'gc, PersistentMap<'gc>>,
        key: Value<'gc>,
    ) -> Value<'gc> {
        let ctx = nctx.ctx;
        nctx.return_(map.dissoc(ctx, key).into())
    }

    #[scheme(name = "core-persistent-map-contains?")]
    pub fn persistent_map_contains(map: Gc<'gc, PersistentMap<'gc>>, key: Value<'gc>) -> bool {
        nctx.return_(map.contains_key(key))
    }

    #[scheme(name = "core-persistent-map-count")]
    pub fn persistent_map_count(map: Gc<'gc, PersistentMap<'gc>>) -> usize {
        nctx.return_(map.len())
    }

    #[scheme(name = "core-persistent-map->alist")]
    pub fn persistent_map_to_alist(map: Gc<'gc, PersistentMap<'gc>>) -> Value<'gc> {
        let ls = map.fold(
            |acc, key, value| Value::cons(nctx.ctx, Value::cons(nctx.ctx, key, value), acc),
            Value::null(),
        );
        nctx.return_(ls)
    }

    #[scheme(name = "alist->core-persistent-map-eq")]
    pub fn alist_to_persistent_map_eq(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(nctx, lst, HashTableType::Eq, "alist->core-persistent-map-eq")
    }

    #[scheme(name = "alist->core-persistent-map-eqv")]
    pub fn alist_to_persistent_map_eqv(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(nctx, lst, HashTableType::Eqv, "alist->core-persistent-map-eqv")
    }

    #[scheme(name = "alist->core-persistent-map-equal")]
    pub fn alist_to_persistent_map_equal(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(nctx, lst, HashTableType::Equal, "alist->core-persistent-map-equal")
    }
}

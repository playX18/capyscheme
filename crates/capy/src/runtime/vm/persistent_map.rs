use crate::heap::Gc;
use crate::prelude::*;

pub fn init_persistent_map<'gc>(ctx: MutationContext<'gc>) {
    persistent_map_ops::register(ctx);
}

fn alist_to_map_impl<'gc>(
    call: &Call<'_, 'gc>,
    lst: Value<'gc>,
    typ: HashTableType<'gc>,
    name: &str,
) -> Value<'gc> {
    let ctx = call.ctx;
    if !lst.is_list() {
        return call.wrong_argument_violation(
            name,
            "expected a list",
            Some(lst),
            Some(1),
            1,
            &[lst],
        );
    }

    let mut map = PersistentMap::new(ctx, typ);
    let mut current = lst;
    while current.is_pair() {
        let cell = current.car();
        if !cell.is_pair() {
            return call.wrong_argument_violation(
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

    map.into()
}

#[scheme(path = capy)]
pub mod persistent_map_ops {
    #[scheme(name = "core-persistent-map?")]
    pub fn is_persistent_map(val: Value<'gc>) -> bool {
        val.is::<PersistentMap>()
    }

    #[scheme(name = "make-core-persistent-map-eq")]
    pub fn make_persistent_map_eq() -> Value<'gc> {
        PersistentMap::new(ctx, HashTableType::Eq).into()
    }

    #[scheme(name = "make-core-persistent-map-eqv")]
    pub fn make_persistent_map_eqv() -> Value<'gc> {
        PersistentMap::new(ctx, HashTableType::Eqv).into()
    }

    #[scheme(name = "make-core-persistent-map-equal")]
    pub fn make_persistent_map_equal() -> Value<'gc> {
        PersistentMap::new(ctx, HashTableType::Equal).into()
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
        res
    }

    #[scheme(name = "core-persistent-map-assoc")]
    pub fn persistent_map_assoc(
        map: Gc<'gc, PersistentMap<'gc>>,
        key: Value<'gc>,
        value: Value<'gc>,
    ) -> Value<'gc> {
        let ctx = ctx;
        map.assoc(ctx, key, value).into()
    }

    #[scheme(name = "core-persistent-map-dissoc")]
    pub fn persistent_map_dissoc(map: Gc<'gc, PersistentMap<'gc>>, key: Value<'gc>) -> Value<'gc> {
        let ctx = ctx;
        map.dissoc(ctx, key).into()
    }

    #[scheme(name = "core-persistent-map-contains?")]
    pub fn persistent_map_contains(map: Gc<'gc, PersistentMap<'gc>>, key: Value<'gc>) -> bool {
        map.contains_key(key)
    }

    #[scheme(name = "core-persistent-map-count")]
    pub fn persistent_map_count(map: Gc<'gc, PersistentMap<'gc>>) -> usize {
        map.len()
    }

    #[scheme(name = "core-persistent-map->alist")]
    pub fn persistent_map_to_alist(map: Gc<'gc, PersistentMap<'gc>>) -> Value<'gc> {
        let ls = map.fold(
            |acc, key, value| Value::cons(ctx, Value::cons(ctx, key, value), acc),
            Value::null(),
        );
        ls
    }

    #[scheme(name = "alist->core-persistent-map-eq")]
    pub fn alist_to_persistent_map_eq(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(
            call,
            lst,
            HashTableType::Eq,
            "alist->core-persistent-map-eq",
        )
    }

    #[scheme(name = "alist->core-persistent-map-eqv")]
    pub fn alist_to_persistent_map_eqv(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(
            call,
            lst,
            HashTableType::Eqv,
            "alist->core-persistent-map-eqv",
        )
    }

    #[scheme(name = "alist->core-persistent-map-equal")]
    pub fn alist_to_persistent_map_equal(lst: Value<'gc>) -> Value<'gc> {
        alist_to_map_impl(
            call,
            lst,
            HashTableType::Equal,
            "alist->core-persistent-map-equal",
        )
    }
}

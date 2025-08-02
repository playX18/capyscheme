use crate::{
    list,
    runtime::{Context, value::*},
    static_symbols,
};
use rsgc::{Gc, Global, Rootable};
use std::sync::OnceLock;

pub mod assignment_elimination;
pub mod compile_cps;
pub mod core;
pub mod fix_letrec;
pub mod library;
pub mod primitives;
pub mod synclo;

static SOURCE_PROPERTIES: OnceLock<Global<Rootable!(Gc<'_, WeakTable<'_>>)>> = OnceLock::new();

pub fn source_properties<'gc>(ctx: Context<'gc>) -> Gc<'gc, WeakTable<'gc>> {
    *SOURCE_PROPERTIES
        .get_or_init(|| Global::new(WeakTable::new(&ctx, 128, 0.75)))
        .fetch(&ctx)
}

pub fn has_source_properties<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> bool {
    source_properties(ctx).contains_key(ctx, obj)
}

pub fn set_source_property<'gc>(ctx: Context<'gc>, obj: Value<'gc>, alist: Value<'gc>) {
    let props = source_properties(ctx);
    props.put(ctx, obj, alist);
}

pub fn add_source<'gc>(
    ctx: Context<'gc>,
    obj: Value<'gc>,
    filename: Value<'gc>,
    line: i32,
    column: i32,
) {
    let alist = list![
        ctx,
        Value::cons(ctx, sym_filename(ctx).into(), filename),
        Value::cons(ctx, sym_line(ctx).into(), Value::new(line)),
        Value::cons(ctx, sym_column(ctx).into(), Value::new(column))
    ];
    set_source_property(ctx, obj, alist);
}

pub fn get_source_property<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Option<Value<'gc>> {
    source_properties(ctx).get(ctx, obj)
}

static_symbols!(
    SYM_FILENAME = "filename"
    SYM_LINE = "line"
    SYM_COLUMN = "column"
);

pub fn source_property<'gc>(
    ctx: Context<'gc>,
    obj: Value<'gc>,
    key: Value<'gc>,
) -> Option<Value<'gc>> {
    get_source_property(ctx, obj).and_then(|alist| alist.assq(key).map(|pair| pair.cdr()))
}

use crate::rsgc::{Gc, Global};
use crate::{
    expander::core::denotation_of_begin,
    frontend::reader::{LexicalError, TreeSitter},
    runtime::{Context, modules::Module, value::*},
    static_symbols,
};
use std::sync::OnceLock;

pub mod assignment_elimination;
pub mod compile_cps;
pub mod core;
pub mod eta_expand;
pub mod fix_letrec;
pub mod fold;
pub mod free_vars;
pub mod inlining;
pub mod letrectify;
pub mod primitives;

pub fn datum_sourcev<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    let Some(props) = get_source_property(ctx, obj) else {
        return Value::new(false);
    };

    if props.is_pair() {
        let filename = props
            .assq(sym_filename(ctx).into())
            .map(|pair| pair.cdr())
            .unwrap_or(Value::new(false));
        let line = props
            .assq(sym_line(ctx).into())
            .map(|pair| pair.cdr())
            .unwrap_or(Value::new(false));
        let column = props
            .assq(sym_column(ctx).into())
            .map(|pair| pair.cdr())
            .unwrap_or(Value::new(false));
        Vector::from_slice(*ctx, &[filename, line, column]).into()
    } else {
        Value::new(false)
    }
}

pub fn syntax_sourcev<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    datum_sourcev(ctx, obj)
}

pub fn syntax_annotation<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    datum_sourcev(ctx, obj)
}

static SOURCE_PROPERTIES: OnceLock<Global<crate::Rootable!(Gc<'_, WeakTable<'_>>)>> =
    OnceLock::new();

pub fn source_properties<'gc>(ctx: Context<'gc>) -> Gc<'gc, WeakTable<'gc>> {
    *SOURCE_PROPERTIES
        .get_or_init(|| Global::new(WeakTable::new(*ctx, 128, 0.75)))
        .fetch(*ctx)
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
    let alist = crate::vector![ctx, filename, Value::new(line), Value::new(column)];
    set_source_property(ctx, obj, alist.into());
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

pub fn read_from_string<'gc>(
    ctx: Context<'gc>,
    source: impl AsRef<str>,
    filename: impl AsRef<str>,
) -> Result<Value<'gc>, LexicalError<'gc>> {
    let filename = Str::new(*ctx, filename, true);
    let tree_sitter = TreeSitter::new(ctx, source.as_ref(), filename.into(), false);
    let program = tree_sitter.read_program()?;

    let mut ls = Value::null();

    for expr in program.iter().rev() {
        ls = Value::cons(ctx, *expr, ls);
    }

    if program.is_empty() {
        ls = Value::cons(ctx, Value::undefined(), ls);
    }

    Ok(Value::cons(ctx, denotation_of_begin(ctx).into(), ls))
}

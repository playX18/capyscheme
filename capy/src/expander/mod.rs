use crate::{
    cps::contify,
    expander::core::{Cenv, Error, denotation_of_begin},
    frontend::reader::{LexicalError, TreeSitter},
    list,
    runtime::{Context, modules::Module, value::*},
    static_symbols,
};
use rsgc::{Gc, Global, Rootable};
use std::sync::OnceLock;

pub mod assignment_elimination;
pub mod compile_cps;
pub mod core;
pub mod fix_letrec;
//pub mod letrectify;
pub mod fold;
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

pub fn read_from_string<'gc>(
    ctx: Context<'gc>,
    source: impl AsRef<str>,
    filename: impl AsRef<str>,
) -> Result<Value<'gc>, LexicalError<'gc>> {
    let filename = Str::new(&ctx, filename, true);
    let tree_sitter = TreeSitter::new(ctx, source.as_ref(), filename.into());
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

pub fn compile_program<'gc>(
    ctx: Context<'gc>,
    program: Value<'gc>,
    env: Gc<'gc, Module<'gc>>,
) -> Result<crate::cps::term::FuncRef<'gc>, Error<'gc>> {
    let mut cenv = Cenv::toplevel(ctx);
    let expanded = core::expand(&mut cenv, program)?;
    let fixed = fix_letrec::fix_letrec(ctx, expanded);
    let no_mutation = assignment_elimination::eliminate_assignments(ctx, fixed);
    let primitives = primitives::resolve_primitives(ctx, no_mutation, env);

    let cps = compile_cps::cps_toplevel(ctx, &[primitives]);

    let mut cps = crate::cps::rewrite_func(ctx, cps);
    cps = cps.with_body(ctx, contify::contify(ctx, cps.body));

    Ok(cps)
}

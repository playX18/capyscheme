use rsgc::{Gc, Trace};

use crate::{
    expander::{get_source_property, sym_column, sym_filename, sym_line},
    list,
    prelude::*,
    runtime::{
        Context,
        fluids::Fluid,
        modules::Module,
        value::{Closure, Pair, ScmHeader, Str, Tagged, Tuple, TypeCode8, Value, Vector},
        vm::ffi::Pointer,
    },
};

#[derive(Trace)]
#[collect(no_drop)]
pub struct Syntax<'gc> {
    header: ScmHeader,
    pub(crate) expr: Value<'gc>,
    pub(crate) wrap: Value<'gc>,
    pub(crate) module: Value<'gc>,
    pub(crate) source: Value<'gc>,
}

impl<'gc> Syntax<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        expr: Value<'gc>,
        wrap: Value<'gc>,
        module: Value<'gc>,
        source: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::SYNTAX.bits() as _),
                expr,
                wrap,
                module,
                source,
            },
        )
    }

    pub fn expr(&self) -> Value<'gc> {
        self.expr
    }

    pub fn wrap(&self) -> Value<'gc> {
        self.wrap
    }

    pub fn module(&self) -> Value<'gc> {
        self.module
    }

    pub fn source(&self) -> Value<'gc> {
        self.source
    }
}

unsafe impl<'gc> Tagged for Syntax<'gc> {
    const TC8: TypeCode8 = TypeCode8::SYNTAX;
    const TYPE_NAME: &'static str = "#<syntax>";
}
#[scheme(path=capy)]
pub mod syntax_ops {
    #[scheme(name = "syntax?")]
    pub fn is_syntax(v: Value<'gc>) -> bool {
        nctx.return_(v.is::<Syntax<'gc>>())
    }

    #[scheme(name = "make-syntax")]
    pub fn make_syntax(
        exp: Value<'gc>,
        wrap: Value<'gc>,
        module: Value<'gc>,
        source: Option<Value<'gc>>,
    ) -> Value<'gc> {
        let source = source.unwrap_or_else(|| datum_sourcev(nctx.ctx, exp));

        let syntax = Syntax::new(nctx.ctx, exp, wrap, module, source);

        nctx.return_(syntax.into())
    }

    #[scheme(name = "syntax-expression")]
    pub fn syntax_expression(syntax: Gc<'gc, Syntax<'gc>>) -> Value<'gc> {
        nctx.return_(syntax.expr)
    }

    #[scheme(name = "syntax-wrap")]
    pub fn syntax_wrap(syntax: Gc<'gc, Syntax<'gc>>) -> Value<'gc> {
        nctx.return_(syntax.wrap)
    }

    #[scheme(name = "syntax-module")]
    pub fn syntax_module(syntax: Gc<'gc, Syntax<'gc>>) -> Value<'gc> {
        nctx.return_(syntax.module)
    }

    #[scheme(name = "syntax-source")]
    pub fn syntax_source(syntax: Gc<'gc, Syntax<'gc>>) -> Value<'gc> {
        let src = syntax.source;
        let src = if src.is::<Vector>() {
            sourcev_to_props(nctx.ctx, src)
        } else {
            src
        };

        nctx.return_(src)
    }

    #[scheme(name = "syntax-sourcev")]
    pub fn syntax_sourcev(syntax: Gc<'gc, Syntax<'gc>>) -> Value<'gc> {
        let src = syntax.source;
        let src = if src.is_null() || src.is::<Pair>() {
            props_to_sourcev(nctx.ctx, src)
        } else {
            src
        };
        nctx.return_(src)
    }

    #[scheme(name = "make-syntax-transformer")]
    pub fn make_syntax_transformer(
        name: Value<'gc>,
        typ: Value<'gc>,
        binding: Value<'gc>,
    ) -> Value<'gc> {
        let transformer = SyntaxTransformer::new(nctx.ctx, name, typ, binding);
        nctx.return_(transformer.into())
    }

    #[scheme(name = "macro?")]
    pub fn is_macro(v: Value<'gc>) -> bool {
        nctx.return_(v.is::<SyntaxTransformer<'gc>>())
    }

    #[scheme(name = "macro-name")]
    pub fn macro_name(macro_: Gc<'gc, SyntaxTransformer<'gc>>) -> Value<'gc> {
        nctx.return_(macro_.name())
    }

    #[scheme(name = "macro-transformer")]
    pub fn macro_transformer(macro_: Gc<'gc, SyntaxTransformer<'gc>>) -> Value<'gc> {
        if macro_.binding().is::<Closure>() {
            nctx.return_(macro_.binding())
        } else {
            nctx.return_(Value::new(false))
        }
    }

    #[scheme(name = "macro-binding")]
    pub fn macro_binding(macro_: Gc<'gc, SyntaxTransformer<'gc>>) -> Value<'gc> {
        nctx.return_(macro_.binding())
    }

    #[scheme(name = "macro-type")]
    pub fn macro_type(macro_: Gc<'gc, SyntaxTransformer<'gc>>) -> Value<'gc> {
        nctx.return_(macro_.typ())
    }

    #[scheme(name = "datum-sourcev")]
    pub fn datum_sourcev_fn(obj: Value<'gc>) -> Value<'gc> {
        let src = datum_sourcev(nctx.ctx, obj);
        nctx.return_(src)
    }

    #[scheme(name = "self-evaluating?")]
    pub fn is_self_evaluating(obj: Value<'gc>) -> bool {
        nctx.return_(
            obj.is_bool()
                || obj.is_immediate()
                || obj.is_int32()
                || obj.is_flonum()
                || obj.is_bool()
                || obj.is_char()
                || obj.is_null()
                || obj == Value::undefined()
                || obj.is_pair()
                || obj.is::<Vector>()
                || obj.is::<Str>()
                || obj.is::<Closure>()
                || obj.is::<Syntax>()
                || obj.is::<Tuple>()
                || obj.is::<ByteVector>()
                || obj.is::<HashTable>()
                || obj.is::<WeakSet>()
                || obj.is::<WeakTable>()
                || obj.is::<WeakMapping>()
                || obj.is::<Fluid>()
                || obj.is::<Module>()
                || obj.is::<Pointer>()
                || obj.is_number(),
        )
    }

    #[scheme(name = "source-properties")]
    pub fn source_properties_of(obj: Value<'gc>) -> Value<'gc> {
        let props = get_source_property(nctx.ctx, obj).unwrap_or(Value::new(false));

        nctx.return_(props)
    }
}

pub(crate) fn init_syntax<'gc>(ctx: Context<'gc>) {
    syntax_ops::register(ctx);
}

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
        Vector::from_slice(&ctx, &[filename, line, column]).into()
    } else {
        Value::new(false)
    }
}

pub fn props_to_sourcev<'gc>(ctx: Context<'gc>, props: Value<'gc>) -> Value<'gc> {
    if props.is_null() || !props.is_pair() {
        return Value::new(false);
    }

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

    Vector::from_slice(&ctx, &[filename, line, column]).into()
}
pub fn sourcev_to_props<'gc>(ctx: Context<'gc>, sourcev: Value<'gc>) -> Value<'gc> {
    if sourcev.is::<Vector>() {
        let vec = sourcev.downcast::<Vector>();
        if vec.len() == 3 {
            let filename_cell = Value::cons(ctx, sym_filename(ctx).into(), vec[0].get());
            let line_cell = Value::cons(ctx, sym_line(ctx).into(), vec[1].get());
            let column_cell = Value::cons(ctx, sym_column(ctx).into(), vec[2].get());
            return list!(ctx, filename_cell, line_cell, column_cell);
        }
    }
    Value::new(false)
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct SyntaxTransformer<'gc> {
    header: ScmHeader,
    pub(crate) name: Value<'gc>,
    pub(crate) typ: Value<'gc>,
    pub(crate) binding: Value<'gc>,
}

impl<'gc> SyntaxTransformer<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        name: Value<'gc>,
        typ: Value<'gc>,
        binding: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(TypeCode8::SYNCLO.bits() as _),
                name,
                typ,
                binding,
            },
        )
    }

    pub fn name(&self) -> Value<'gc> {
        self.name
    }

    pub fn typ(&self) -> Value<'gc> {
        self.typ
    }

    pub fn binding(&self) -> Value<'gc> {
        self.binding
    }
}

unsafe impl<'gc> Tagged for SyntaxTransformer<'gc> {
    const TC8: TypeCode8 = TypeCode8::SYNCLO;
    const TYPE_NAME: &'static str = "#<syntax-transformer>";
}

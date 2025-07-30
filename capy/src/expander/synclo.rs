//! Hygienic macro expander based on Syntactic Closures.

use rsgc::{Gc, Trace};

use crate::{
    expander::{get_source_property, sym_column, sym_filename, sym_line},
    fluid,
    frontend::reader::Annotation,
    runtime::{
        Context,
        value::{HashTable, ScmHeader, Str, Symbol, Tagged, TypeCode8, Value, Vector},
    },
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Expander<'gc> {
    pub header: ScmHeader,
    pub transformer: Value<'gc>,
    pub environment: Value<'gc>,
}

unsafe impl<'gc> Tagged for Expander<'gc> {
    const TC8: TypeCode8 = TypeCode8::EXPANDER;
}

pub type ExpanderRef<'gc> = Gc<'gc, Expander<'gc>>;

impl<'gc> Expander<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        transformer: Value<'gc>,
        environment: Value<'gc>,
    ) -> ExpanderRef<'gc> {
        Gc::new(
            &ctx,
            Expander {
                header: ScmHeader::with_type_bits(TypeCode8::EXPANDER.bits() as _),
                transformer,
                environment,
            },
        )
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Environment<'gc> {
    pub header: ScmHeader,
    pub base: Option<EnvironmentRef<'gc>>,
    pub frame: Gc<'gc, HashTable<'gc>>,
    pub renamer: Value<'gc>,
}

unsafe impl<'gc> Tagged for Environment<'gc> {
    const TC8: TypeCode8 = TypeCode8::ENVIRONMENT;
}

pub type EnvironmentRef<'gc> = Gc<'gc, Environment<'gc>>;

impl<'gc> Environment<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        base: Option<EnvironmentRef<'gc>>,
        frame: Option<Gc<'gc, HashTable<'gc>>>,
        renamer: Value<'gc>,
    ) -> EnvironmentRef<'gc> {
        let frame = frame.unwrap_or_else(|| {
            HashTable::new(&ctx, crate::runtime::value::HashTableType::Eq, 8, 0.75)
        });
        Gc::new(
            &ctx,
            Environment {
                header: ScmHeader::with_type_bits(TypeCode8::ENVIRONMENT.bits() as _),
                base,
                frame,
                renamer,
            },
        )
    }

    pub fn is_toplevel(&self) -> bool {
        self.base.is_none()
    }

    pub fn get(
        self: EnvironmentRef<'gc>,
        ctx: Context<'gc>,
        id: Value<'gc>,
    ) -> Result<Option<Value<'gc>>, Value<'gc>> {
        let frame = self.frame;

        if let Some(binding) = frame.get(ctx, id) {
            return Ok(Some(binding));
        }

        if let Some(base) = self.base {
            return base.get(ctx, id);
        } else {
            if id.is::<Symbol>() {
                let new_name = ctx.call(self.renamer, &[id])?;
                self.frame.put(ctx, id, new_name);
                self.get(ctx, id)
            } else {
                Ok(None)
            }
        }
    }

    pub fn install_toplevel_binding(
        self: EnvironmentRef<'gc>,
        ctx: Context<'gc>,
        id: Value<'gc>,
        name: Value<'gc>,
    ) {
        let frame = self.frame;

        frame.put(ctx, id, name);
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct SyntacticClosure<'gc> {
    pub header: ScmHeader,
    pub env: EnvironmentRef<'gc>,
    pub free: Value<'gc>,
    pub form: Value<'gc>,
    /// Source vector. If no source then set to `#f`.
    pub sourcev: Value<'gc>,
}

unsafe impl<'gc> Tagged for SyntacticClosure<'gc> {
    const TC8: TypeCode8 = TypeCode8::SYNCLO;
}

pub type SyntacticClosureRef<'gc> = Gc<'gc, SyntacticClosure<'gc>>;

impl<'gc> SyntacticClosure<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        env: EnvironmentRef<'gc>,
        free: Value<'gc>,
        form: Value<'gc>,
        sourcev: Value<'gc>,
    ) -> SyntacticClosureRef<'gc> {
        Gc::new(
            &ctx,
            SyntacticClosure {
                header: ScmHeader::with_type_bits(TypeCode8::SYNCLO.bits() as _),
                env,
                free,
                form,
                sourcev,
            },
        )
    }
}

pub fn close_syntax<'gc>(
    ctx: Context<'gc>,
    form: Value<'gc>,
    env: EnvironmentRef<'gc>,
    sourcev: Value<'gc>,
) -> Value<'gc> {
    SyntacticClosure::new(ctx, env, Value::null(), form, sourcev).into()
}

pub fn unwrap_syntax<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        unwrap_syntax(ctx, synclo.form)
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        unwrap_syntax(ctx, annotation.expression)
    } else if obj.is_pair() {
        let a = unwrap_syntax(ctx, obj.car());
        let d = unwrap_syntax(ctx, obj.cdr());

        Value::cons(ctx, a, d)
    } else if obj.is::<Vector>() {
        obj.downcast::<Vector>()
            .to_list(ctx)
            .map(ctx, |v| unwrap_syntax(ctx, v))
            .list_to_vector(ctx)
            .into()
    } else {
        obj
    }
}

pub fn make_identifier<'gc>(
    ctx: Context<'gc>,
    id: Value<'gc>,
    env: EnvironmentRef<'gc>,
    sourcev: Value<'gc>,
) -> Value<'gc> {
    close_syntax(ctx, id, env, sourcev)
}

pub fn is_identifier<'gc>(obj: Value<'gc>) -> bool {
    obj.is::<Symbol>()
        || obj
            .try_as::<SyntacticClosure>()
            .map(|synclo| is_identifier(synclo.form))
            .unwrap_or(false)
        || obj
            .try_as::<Annotation>()
            .map_or(false, |annotation| is_identifier(annotation.expression))
}

pub fn syntax_kind<'gc, F>(obj: Value<'gc>, f: F) -> bool
where
    F: FnOnce(Value<'gc>) -> bool,
{
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        syntax_kind(synclo.form, f)
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        syntax_kind(annotation.expression, f)
    } else {
        f(obj)
    }
}

pub fn syntax_pair<'gc>(obj: Value<'gc>) -> bool {
    syntax_kind(obj, |v| v.is_pair())
}

pub fn syntax_vector<'gc>(obj: Value<'gc>) -> bool {
    syntax_kind(obj, |v| v.is::<Vector>())
}

pub fn syntax_list<'gc>(obj: Value<'gc>) -> bool {
    syntax_kind(obj, |v| v.is_list())
}

pub fn datum_sourcev<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    let Some(props) = get_source_property(ctx, obj) else {
        return Value::new(false);
    };

    if props.is_pair() {
        let filename = props
            .assq(sym_filename(ctx).into())
            .unwrap_or(Value::new(false));
        let line = props
            .assq(sym_line(ctx).into())
            .unwrap_or(Value::new(false));
        let column = props
            .assq(sym_column(ctx).into())
            .unwrap_or(Value::new(false));
        Vector::from_slice(&ctx, &[filename, line, column]).into()
    } else {
        Value::new(false)
    }
}

pub fn syntax_sourcev<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        synclo.sourcev
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        annotation.source
    } else {
        datum_sourcev(ctx, obj)
    }
}

pub fn syntax_annotation<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        synclo.sourcev
    } else {
        datum_sourcev(ctx, obj)
    }
}

pub fn syntax_car<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        let car = syntax_car(ctx, synclo.form);
        SyntacticClosure::new(ctx, synclo.env, synclo.free, car, synclo.sourcev).into()
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        syntax_car(ctx, annotation.expression)
    } else if obj.is_pair() {
        obj.car()
    } else {
        panic!("Expected a pair or syntactic closure, found: {}", obj);
    }
}

pub fn syntax_cdr<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        let cdr = syntax_cdr(ctx, synclo.form);
        SyntacticClosure::new(ctx, synclo.env, synclo.free, cdr, synclo.sourcev).into()
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        syntax_cdr(ctx, annotation.expression)
    } else if obj.is_pair() {
        obj.cdr()
    } else {
        panic!("Expected a pair or syntactic closure, found: {}", obj);
    }
}

pub fn syntax_cadr<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    syntax_car(ctx, syntax_cdr(ctx, obj))
}

pub fn syntax_cddr<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    syntax_cdr(ctx, syntax_cdr(ctx, obj))
}

pub fn syntax_cdar<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    syntax_cdr(ctx, syntax_car(ctx, obj))
}

pub fn syntax_caddr<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Value<'gc> {
    syntax_car(ctx, syntax_cdr(ctx, syntax_cdr(ctx, obj)))
}

pub fn syntax_length<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> usize {
    let mut len = 0;

    let mut xs = obj;

    while syntax_kind(xs, |x| x.is_pair()) {
        len += 1;
        xs = syntax_cdr(ctx, xs);
    }
    len
}

pub fn syntax_synclo<'gc>(ctx: Context<'gc>, obj: Value<'gc>) -> Option<SyntacticClosureRef<'gc>> {
    if let Some(synclo) = obj.try_as::<SyntacticClosure>() {
        Some(synclo)
    } else if let Some(annotation) = obj.try_as::<Annotation>() {
        syntax_synclo(ctx, annotation.expression)
    } else {
        None
    }
}

pub fn expand_identifier<'gc>(
    ctx: Context<'gc>,
    id: Value<'gc>,
    env: EnvironmentRef<'gc>,
) -> Result<Value<'gc>, Value<'gc>> {
    if let Some(annotation) = id.try_as::<Annotation>() {
        return expand_identifier(ctx, annotation.expression, env);
    }
    match env.get(ctx, id)? {
        Some(id) => Ok(id),
        None => {
            let id = id.downcast::<SyntacticClosure>();
            expand_identifier(ctx, id.form, id.env)
        }
    }
}

pub fn expand_synclo<'gc>(
    ctx: Context<'gc>,
    sc: SyntacticClosureRef<'gc>,
    env: EnvironmentRef<'gc>,
) -> Result<Value<'gc>, Value<'gc>> {
    let form = sc.form;

    let free = HashTable::new(
        &ctx,
        crate::runtime::value::HashTableType::Eq,
        sc.free.list_length(),
        0.75,
    );

    let mut xs = sc.free;

    while !xs.is_null() {
        let id = xs.car();
        let expanded = expand(ctx, id, env)?;
        free.put(ctx, id, expanded);
        xs = xs.cdr();
    }

    expand(
        ctx,
        form,
        Environment::new(ctx, Some(sc.env), Some(free), Value::new(false)),
    )
}

pub fn expression_position<'gc>(ctx: Context<'gc>, form: Value<'gc>) -> Value<'gc> {
    if let Some(annotation) = form.try_as::<Annotation>() {
        annotation.source
    } else if let Some(synclo) = form.try_as::<SyntacticClosure>() {
        expression_position(ctx, synclo.form)
    } else {
        Value::null()
    }
}

pub fn expand<'gc>(
    ctx: Context<'gc>,
    form: Value<'gc>,
    env: EnvironmentRef<'gc>,
) -> Result<Value<'gc>, Value<'gc>> {
    if is_identifier(form) {
        expand_identifier(ctx, form, env)
    } else if let Some(synclo) = syntax_synclo(ctx, form) {
        expand_synclo(ctx, synclo, env)
    } else if syntax_pair(form) && syntax_list(form) {
        if is_identifier(syntax_car(ctx, form)) {
            let e = expand(ctx, syntax_car(ctx, form), env)?;
            if let Some(_exp) = e.try_as::<Expander>() {
                todo!()
            } else {
                let mut args = Value::null();

                let mut xs = syntax_cdr(ctx, form);

                while syntax_pair(xs) {
                    let arg = syntax_car(ctx, xs);
                    let expanded_arg = expand(ctx, arg, env)?;
                    args = Value::cons(ctx, expanded_arg, args);
                    xs = syntax_cdr(ctx, xs);
                }

                Ok(Value::cons(ctx, e, args))
            }
        } else {
            form.try_map(ctx, |arg| expand(ctx, arg, env))
        }
    } else if !form.is_pair() {
        Ok(form)
    } else {
        Err(Str::new(&ctx, format!("invalid syntax: {}", form), true).into())
    }
}

pub fn identifier_eq<'gc>(
    ctx: Context<'gc>,
    id1: Value<'gc>,
    env1: EnvironmentRef<'gc>,
    id2: Value<'gc>,
    env2: EnvironmentRef<'gc>,
) -> Result<bool, Value<'gc>> {
    Ok(expand(ctx, id1, env1)? == expand(ctx, id2, env2)?)
}

pub fn generate_name<'gc>(ctx: Context<'gc>, id: Value<'gc>) -> Value<'gc> {
    static NAME_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    let n = NAME_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    Symbol::from_str(ctx, &format!("%{}.{}", unwrap_syntax(ctx, id), n)).into()
}

fluid!(
    pub current_toplevel_environment = Value::new(false);
);

pub fn with_current_toplevel_environment<'gc, F, R>(
    ctx: Context<'gc>,
    env: EnvironmentRef<'gc>,
    f: F,
) -> R
where
    F: FnOnce(Context<'gc>, EnvironmentRef<'gc>) -> R,
{
    let old_env = get_current_toplevel_environment(ctx);
    set_current_toplevel_environment(ctx, env.into());
    let result = f(ctx, env);
    set_current_toplevel_environment(ctx, old_env);
    result
}

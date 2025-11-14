use crate::cps::builder::CPSBuilder;
use crate::cps::term::{Atom, BranchHint, Cont, Func, FuncRef, Term, TermRef};
use crate::expander::core::{
    LVarRef, LetStyle, Proc, TermKind, TermRef as CoreTermRef, seq_from_slice,
};
use crate::rsgc::alloc::array::Array;
use crate::rsgc::cell::Lock;
use crate::rsgc::{Gc, Global, Trace, barrier};
use crate::runtime::Context;
use crate::runtime::prelude::*;
use crate::runtime::value::{Str, Vector};
use crate::runtime::value::{TypeCode8, Value};
use crate::{list, static_symbols, with_cps};
use std::cell::Cell;
use std::collections::HashMap;
use std::mem::offset_of;
use std::sync::OnceLock;

pub fn cps_func<'a, 'gc>(
    builder: &'a mut CPSBuilder<'gc>,
    proc: &Proc<'gc>,
    binding: LVarRef<'gc>,
) -> FuncRef<'gc> {
    let old_meta = builder.current_meta;
    builder.current_meta = proc.meta;
    let return_cont = builder.fresh_variable("return");
    let handler_cont = builder.fresh_variable("handler");

    let body = convert(builder, proc.body, return_cont, handler_cont);
    builder.current_meta = old_meta;
    Gc::new(
        &builder.ctx,
        Func {
            meta: proc.meta,
            args: proc.args,
            name: proc.name,

            binding,
            source: proc.source,
            return_cont,
            handler_cont,
            variadic: proc.variadic,
            body: Lock::new(body),
            free_vars: Lock::new(None),
        },
    )
}

pub fn cps_toplevel<'gc>(ctx: Context<'gc>, forms: &[CoreTermRef<'gc>]) -> FuncRef<'gc> {
    let form = if forms.len() == 1 {
        forms[0]
    } else if forms.is_empty() {
        panic!("Cannot compile an empty program to CPS")
    } else {
        let source = forms
            .iter()
            .find(|f| f.source() != Value::new(false))
            .map(|f| f.source())
            .unwrap_or(Value::new(false));

        let seq = seq_from_slice(ctx, forms);
        barrier::field!(Gc::write(&ctx, seq), super::core::Term, source)
            .unlock()
            .set(source);
        seq
    };

    let proc = Proc {
        args: Array::from_slice(&ctx, &[]),
        name: Value::new(false),
        body: form,
        source: form.source(),
        variadic: None,
        meta: Value::null(),
    };

    let mut builder = CPSBuilder::new(ctx);

    let bind = builder.fresh_variable("toplevel");
    let func = cps_func(&mut builder, &proc, bind);

    func
}

pub type PrimitiveTransformer = for<'a, 'gc, 'b> fn(
    cps: &'a mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    params: &'b [Atom<'gc>],
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
) -> Option<TermRef<'gc>>;

pub struct PrimitiveTable<'gc> {
    pub table: HashMap<Value<'gc>, PrimitiveTransformer>,
}
unsafe impl<'gc> Trace for PrimitiveTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        for (key, _) in self.table.iter_mut() {
            unsafe {
                let value = key as *const Value<'gc> as *mut Value<'gc>;
                (*value).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

static PRIMTABLE: OnceLock<Global<crate::Rootable!(PrimitiveTable<'_>)>> = OnceLock::new();

macro_rules! primitive_transformers {
    ($(
        $prim: literal => $name : ident ($cps: ident, $src: ident, $op: ident, $args: ident, $k: ident, $h: ident) $b: block
    )*) => {
        #[allow(dead_code, unused_mut, unused_variables)]
        fn make_primitive_table<'gc>(ctx: Context<'gc>) -> HashMap<Value<'gc>, PrimitiveTransformer> {
            let mut table: HashMap<Value<'gc>, PrimitiveTransformer> = HashMap::new();
            $(
                table.insert(
                    Value::new(Symbol::from_str(ctx, $prim)),
                    { fn $name<'gc>($cps: &mut CPSBuilder<'gc>, $src: Value<'gc>, $op: Value<'gc>, $args: &[Atom<'gc>], $k: LVarRef<'gc>, $h: LVarRef<'gc>) -> Option<TermRef<'gc>> {
                        $b
                    } $name },
                );
            )*
            table
        }
    };
}

pub fn get_primitive_table<'gc>(ctx: Context<'gc>) -> &'gc PrimitiveTable<'gc> {
    PRIMTABLE
        .get_or_init(|| {
            let table = make_primitive_table(ctx);
            Global::new(PrimitiveTable { table })
        })
        .fetch(&ctx)
}

impl<'gc> PrimitiveTable<'gc> {
    pub fn try_expand(
        &self,
        cps: &mut CPSBuilder<'gc>,
        src: Value<'gc>,
        prim: Value<'gc>,
        args: &[Atom<'gc>],
        k: LVarRef<'gc>,
        h: LVarRef<'gc>,
    ) -> Option<TermRef<'gc>> {
        if let Some(transformer) = self.table.get(&prim) {
            transformer(cps, src, prim, args, k, h)
        } else {
            None
        }
    }
}

static_symbols!(
    RAISE_TYPE_ERROR = "raise-type-error"
    RAISE_RANGE_ERROR = "raise-range-error"
    RAISE_ARITY_ERROR = "raise-arity-error"
);

/// Generate a call to `assertion-violation` in the `capy` module with the given
/// message and irritants.
///
/// Note that call passes `h` both as return and handler continuation
/// so there's no way that the call can return normally to the caller.
pub fn assertion_violation<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    opc: Value<'gc>,
    message: &str,
    irritants: &[Atom<'gc>],
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    let assertion_violation = Value::new(Symbol::from_str(cps.ctx, "assertion-violation"));
    let module = list!(cps.ctx, Value::new(Symbol::from_str(cps.ctx, "capy")));
    let message = Atom::Constant(Value::new(Str::new(&cps.ctx, message, true)));
    let opc = Atom::Constant(opc);

    module_box(
        cps,
        |cps, var| {
            let mut args = vec![opc, message];
            args.extend_from_slice(irritants);

            with_cps!(cps;
                let value = #% "variable-ref" (h, var) @ src;
                value(h, h,  args...) @ src
            )
        },
        h,
        module,
        assertion_violation,
        true,
        true,
        src,
    )
}

pub fn ensure_string<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    h: LVarRef<'gc>,

    have_length: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold (h) not_string () = assertion_violation(cps, src, op, "not a string", &[x], h);
        letk (h) k () = with_cps!(cps;
            let length = #% "%refptr" (h, x, offset_of!(Str, length) as i32) @ src;
            # {
                have_length(cps, Atom::Local(length))
            }
        );
        letk (h) check_str_typecode () = with_cps!(cps;
            let tc8 = #% "%typecode8" (h, x) @ src;
            let str_typecode = #% "u8=" (h, tc8, Value::new(TypeCode8::STRING.bits() as i32)) @ src;
            if str_typecode => k | not_string
        );

        let is_immediate = #% "immediate?" (h, x) @ src;
        if is_immediate => not_string | check_str_typecode
    )
}

pub fn ensure_pair<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    h: LVarRef<'gc>,
    have_pair: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold (h) not_pair () = assertion_violation(cps, src, op, "not a pair", &[x], h);
        letk (h) k () = with_cps!(cps;
            # {
                have_pair(cps, x)
            }
        );
        letk (h) check_pair_typecode () = with_cps!(cps;
            let tc8 = #% "%typecode8" (h, x) @ src;
            let pair_typecode = #% "u8=" (h, tc8, Value::new(TypeCode8::PAIR.bits() as i32)) @ src;
            if pair_typecode => k | not_pair
        );

        let is_immediate = #% "immediate?" (h, x) @ src;
        if is_immediate => not_pair | check_pair_typecode
    )
}

pub fn ensure_vector<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    h: LVarRef<'gc>,
    have_vector: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold (h) not_vector () = assertion_violation(cps, src, op, "not a vector", &[x], h);
        letk (h) k () = with_cps!(cps;
            let length = #% "%refptr" (h, x, offset_of!(Vector, length) as i32) @ src;
            # {
                have_vector(cps, Atom::Local(length))
            }
        );
        letk (h) check_vector_typecode () = with_cps!(cps;
            let tc8 = #% "%typecode8" (h, x) @ src;
            let vector_typecode = #% "u8=" (h, tc8, Value::new(TypeCode8::VECTOR.bits() as i32)) @ src;
            if vector_typecode => k | not_vector
        );

        let is_immediate = #% "immediate?" (h, x) @ src;
        if is_immediate => not_vector | check_vector_typecode
    )
}

fn ensure_variable<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    h: LVarRef<'gc>,
    have_variable: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold (h) not_variable () = assertion_violation(cps, src, op, &format!("not a variable {}", not_variable.name), &[x], h);
        letk (h) k () = with_cps!(cps;
            # {
                have_variable(cps, x)
            }
        );
        letk (h) check_variable_typecode () = with_cps!(cps;
            let tc8 = #% "%typecode8" (h, x) @ src;
            let variable_typecode = #% "u8=" (h, tc8, Value::new(TypeCode8::VARIABLE.bits() as i32)) @ src;
            if variable_typecode => k | not_variable
        );

        let is_immediate = #% "immediate?" (h, x) @ src;
        if is_immediate => not_variable | check_variable_typecode
    )
}

/// Attempt to convert a fixnum `x` to a `usize`, if it is in range.
///
/// Otherwise continue to the handler `h`.
pub fn fixnum_in_range_to_usize<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    idx: Atom<'gc>,
    ulen: Atom<'gc>,
    h: LVarRef<'gc>,
    have_index_in_range: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        letk (h) bound (uidx) = have_index_in_range(cps, Atom::Local(uidx));
        letk (h) boundlen (fix) = with_cps!(cps;
            let uidx = #% "s32->u64" (h, fix) @ src;
            let in_range = #% "u64<=" (h, uidx, ulen) @ src;
            if in_range => bound (uidx) | h()
        );
        letk (h) untag () = with_cps!(cps;
            let fix = #% "untag-fixnum" (h, idx) @ src;
            let below0 = #% "s32<" (h, fix, Value::new(0)) @ src;
            if below0 => h() | boundlen(fix)
        );

        let is_fixnum = #% "fixnum?" (h, idx) @ src;
        if is_fixnum =>  untag | h
    )
}

primitive_transformers!(
    "string-length" => string_length(cps, src, op, args, k, h) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Str>() {
            let x = with_cps!(cps;
                continue k (val.downcast::<Str>().len() as i32)
            );
            return Some(x);
        }
        Some(ensure_string(cps, src, op, x, h, |cps, len| {
            with_cps!(cps;
                let vlen = #% "usize->value" (h, len) @ src;
                continue k (vlen)
            )
        }))
    }

    "vector-length" => vector_length(cps, src, op, args, k, h) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Vector>() {
            let x = with_cps!(cps;
                continue k (val.downcast::<Vector>().len() as i32)
            );
            return Some(x);
        }
        Some(ensure_vector(cps, src, op, x, h, |cps, len| {
            with_cps!(cps;
                let vlen = #% "usize->value" (h, len) @ src;
                continue k (vlen)
            )
        }))
    }



    "car" => car(cps, src, op, args, k, h) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };

        Some(ensure_pair(cps, src, op, x, h, |cps, pair| {
            with_cps!(cps;
                let car = #% "car" (h, pair) @ src;
                continue k (car)
            )
        }))
    }

    "cdr" => cdr(cps, src, op, args, k, h) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };

        Some(ensure_pair(cps, src, op, x, h, |cps, pair| {
            with_cps!(cps;
                let cdr = #% "cdr" (h, pair) @ src;
                continue k (cdr) @ src
            )
        }))
    }

    "set-car!" => set_car(cps, src, op, args, k, h) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, src, op, pair, h, |cps, pair| {
            with_cps!(cps;
                let _v = #% "set-car!" (h, pair, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "set-cdr!" => set_cdr(cps, src, op, args, k, h) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, src, op, pair, h, |cps, pair| {
            with_cps!(cps;
                let _v = #% "set-cdr!" (h, pair, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "variable-ref" => variable_ref(cps, src, op, args, k, h) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };

        Some(ensure_variable(cps, src, op, x, h, |cps, var| {
            with_cps!(cps;
                let value = #% "variable-ref" (h, var) @ src;
                continue k (value) @ src
            )
        }))
    }

    "variable-set!" => variable_set(cps, src, op, args, k, h) {
        let Some((var, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_variable(cps, src, op, var, h, |cps, var| {
            with_cps!(cps;
                let _v = #% "variable-set!" (h, var, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "variable-bound?" => variable_bound(cps, src, op, args, k, h) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(assertion_violation(cps, src, op, "wrong number of arguments", args, h));
        };

        Some(ensure_variable(cps, src, op, x, h, |cps, var| {
            with_cps!(cps;
                let bound = #% "variable-bound?" (h, var) @ src;
                continue k (bound) @ src
            )
        }))
    }

);

pub fn toplevel_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
    have_var: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    match cps.current_topbox_scope {
        None => {
            if bound {
                with_cps!(cps;
                    let module = #% "current-module" (h,) @ src;
                    let variable = #% "lookup-bound" (h, Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            } else {
                with_cps!(cps;
                    let module = #% "current-module" (h,) @ src;
                    let variable = #% "lookup" (h, Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            }
        }

        Some(scope) => {
            with_cps!(cps;
                letk (h) kbox (box_) = have_var(cps, Atom::Local(box_));
                # cached_toplevel_box(cps, kbox, h, src, (scope as i32).into(), name, bound)
            )
        }
    }
}

pub fn module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    val_proc: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
    h: LVarRef<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
    bound: bool,
    src: Value<'gc>,
) -> TermRef<'gc> {
    let _ = bound;
    with_cps!(cps;
        letk (h) kbox (var) = val_proc(cps, Atom::Local(var));
        # cached_module_box(cps, kbox, h, src, module, name, public,)
    )
}

pub fn capture_toplevel_scope<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    scope_id: u32,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let module = #% "current-module" (h,) @ src;
        # cache_current_module(cps, fk, h, src, Atom::Constant(Value::new(scope_id as i32)), Atom::Local(module))
    )
}

pub fn cached_toplevel_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
    src: Value<'gc>,
    scope: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, scope, name);
    with_cps!(cps;
        let cached = #% "cache-ref" (h, cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (h, cached) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline (h) merge (cached) = with_cps!(cps; continue k (cached));
        letk cold (h) kinit () = with_cps!(cps;
            let module = #%"cache-ref" (h, Atom::Constant(scope)) @ src;
            # reify_lookup(cps, src, module, name, bound, h, |cps, var| {
                with_cps!(cps;
                    let _k = #%"cache-set!"(h, cache_key, var) @ src;
                    continue merge(var)
                )
            })
        );
        letk (h) kok () = with_cps!(cps; continue merge (cached));
        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, module, Value::cons(cps.ctx, name, public.into()));

    with_cps!(cps;
        let cache_entry = #% "cache-ref" (h, cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (h, cache_entry) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline (h) merge (cached) = with_cps!(cps; continue k (cached));
        letk cold (h) kinit () = if public {
            with_cps!(cps;
                let var = #% "lookup-bound-public" (h, Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (h, cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        } else {
            with_cps!(cps;
                let var = #% "lookup-bound-private" (h, Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (h, cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        };

        letk (h) kok () = with_cps!(cps; continue merge (cache_entry));

        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_boxk<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>, LVarRef<'gc>) -> TermRef<'gc>,
    h: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        letk (h) kbox (var) = fk(cps, var);
        # cached_toplevel_box(cps, kbox, h, src, module, name, public)
    )
}

pub fn cache_current_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
    h: LVarRef<'gc>,
    src: Value<'gc>,
    scope: Atom<'gc>,
    module: Atom<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let _k = #% "cache-set!" (h, scope, module) @ src;
        # fk(cps)
    )
}

pub fn reify_lookup<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    mod_var: LVarRef<'gc>,
    name: Value<'gc>,
    assert_bound: bool,
    h: LVarRef<'gc>,
    have_var: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    if assert_bound {
        with_cps!(cps;
            let variable = #%"lookup-bound" (h, Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    } else {
        with_cps!(cps;
            let variable = #%"lookup" (h, Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    }
}

pub fn reify_resolve_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        let resolved = #% "resolve-module" (h, Atom::Constant(public.into()), Atom::Constant(module)) @ src;
        continue k (resolved)
    )
}

fn is_zero_valued<'gc>(exp: CoreTermRef<'gc>) -> bool {
    match &exp.kind {
        TermKind::Values(vals) if vals.is_empty() => true,
        TermKind::ToplevelSet(_, _, _)
        | TermKind::ModuleSet(_, _, _, _)
        | TermKind::Define(_, _, _) => true,
        TermKind::Seq(_, tail) => is_zero_valued(*tail),
        TermKind::Receive(_, _, _, body) => is_zero_valued(*body),
        TermKind::Let(let_) => is_zero_valued(let_.body),
        _ => false,
    }
}

fn is_single_valued<'gc>(exp: CoreTermRef<'gc>) -> bool {
    match &exp.kind {
        TermKind::Values(vals) if vals.len() == 1 => true,
        TermKind::ToplevelRef(..) | TermKind::ModuleRef(..) | TermKind::LRef(..) => true,
        TermKind::Seq(_, tail) => is_single_valued(*tail),
        TermKind::Receive(_, _, _, body) => is_single_valued(*body),
        TermKind::Let(let_) => is_single_valued(let_.body),
        TermKind::Const(_) => true,
        _ => false,
    }
}

pub fn convert_arg<'gc, 'a>(
    cps: &mut CPSBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: Box<dyn FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc> + 'a>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    match exp.kind {
        TermKind::LRef(var) => k(cps, Atom::Local(var)),

        x if is_single_valued(exp) => {
            with_cps!(cps;
                letk (h) karg (arg) = k(cps, Atom::Local(arg));
                # {
                    convert(cps, exp, karg, h, )
                }
            )
        }

        _ => {
            with_cps!(cps;
                letk (h) karg (arg & rest) = k(cps, Atom::Local(arg));
                # convert(cps, exp, karg, h, )
            )
        }
    }
}

pub fn convert_args<'gc, 'a>(
    cps: &mut CPSBuilder<'gc>,
    exps: &'a [CoreTermRef<'gc>],
    fk: Box<dyn FnOnce(&mut CPSBuilder<'gc>, Vec<Atom<'gc>>) -> TermRef<'gc> + 'a>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    if exps.is_empty() {
        return fk(cps, Vec::new());
    }

    let exp = exps[0];
    let exps = &exps[1..];
    if exps.is_empty() {}
    convert_arg(
        cps,
        exp,
        Box::new(move |cps, arg| {
            convert_args(
                cps,
                exps,
                Box::new(move |cps, mut args| {
                    let mut all_args = Vec::with_capacity(args.len() + 1);
                    all_args.push(arg);
                    all_args.append(&mut args);
                    fk(cps, all_args)
                }),
                h,
            )
        }),
        h,
    )
}

pub fn convert<'gc>(
    cps: &mut CPSBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
    h: LVarRef<'gc>,
) -> TermRef<'gc> {
    let src = exp.source();

    match &exp.kind {
        TermKind::Values(vals) => convert_args(
            cps,
            vals,
            Box::new(move |cps, args| {
                with_cps!(cps;
                    continue k args ...
                )
            }),
            h,
        ),

        TermKind::If(test, consequent, alternate) => convert_arg(
            cps,
            *test,
            Box::new(move |cps, test| {
                with_cps!(cps;
                    letk (h) kconseq () = convert(cps, *consequent, k, h,);
                    letk (h) kalt () = convert(cps, *alternate, k, h,);
                    if test => kconseq | kalt
                )
            }),
            h,
        ),

        TermKind::LSet(..) => unreachable!(),
        TermKind::LRef(var) => {
            with_cps!(cps;
                continue k (Atom::Local(*var)) @ src
            )
        }

        TermKind::Const(c) => {
            with_cps!(cps;
                continue k (Atom::Constant(*c)) @ src
            )
        }

        TermKind::PrimRef(name) => toplevel_box(
            cps,
            src,
            *name,
            true,
            |cps, var| {
                with_cps!(cps;
                    let val = #% "variable-ref" (h, var) @ src;
                    continue k(Atom::Local(val))
                )
            },
            h,
        ),

        TermKind::ToplevelRef(_, name) => toplevel_box(
            cps,
            src,
            *name,
            true,
            |cps, var| {
                with_cps!(cps;
                    let val = #% "variable-ref" (h, var) @ src;
                    continue k(Atom::Local(val))
                )
            },
            h,
        ),

        TermKind::ModuleRef(module, name, public) => module_box(
            cps,
            |cps, var| {
                with_cps!(cps;
                    let val = #% "variable-ref" (h, var) @ src;
                    continue k(Atom::Local(val))
                )
            },
            h,
            *module,
            *name,
            *public,
            false,
            src,
        ),

        TermKind::ToplevelSet(_, name, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, atom| {
                toplevel_box(
                    cps,
                    src,
                    *name,
                    false,
                    |cps, var| {
                        with_cps!(cps;
                            let _val = #% "variable-set!" (h, var, atom) @ src;
                            continue k(_val) @ src
                        )
                    },
                    h,
                )
            }),
            h,
        ),

        TermKind::ModuleSet(module, name, public, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, exp| {
                module_box(
                    cps,
                    |cps, var| {
                        with_cps!(cps;
                            let _val = #% "variable-set!" (h, var, exp) @ src;
                            continue k(_val) @ src
                        )
                    },
                    h,
                    *module,
                    *name,
                    *public,
                    true,
                    src,
                )
            }),
            h,
        ),

        TermKind::Proc(proc) => {
            if cps.current_topbox_scope.is_some() {
                let tmp = cps.fresh_variable("proc");
                let func = cps_func(cps, &proc, tmp);
                let body = with_cps!(cps;
                    continue k (Atom::Local(tmp)) @ src
                );

                return Gc::new(
                    &cps.ctx,
                    Term::Fix(Array::from_slice(&cps.ctx, &[func]), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(
                cps,
                src,
                id,
                |cps| {
                    let form = convert(cps, exp, k, h);
                    cps.current_topbox_scope = prev;
                    form
                },
                h,
            )
        }

        TermKind::Define(_module, name, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, atom| {
                with_cps! {cps;
                    let _rv = #% "define" (h, Atom::Constant(*name), atom) @ src;
                     continue k (_rv) @ src
                }
            }),
            h,
        ),

        TermKind::Call(proc, args) => convert_arg(
            cps,
            *proc,
            Box::new(move |cps, proc| {
                convert_args(
                    cps,
                    args,
                    Box::new(move |cps, args| {
                        with_cps!(cps;
                            proc(k, h, args ...) @ src
                        )
                    }),
                    h,
                )
            }),
            h,
        ),

        TermKind::PrimCall(prim, args) => {
            let prim = *prim;
            convert_args(
                cps,
                args,
                Box::new(move |cps, args| {
                    if let Some(term) =
                        get_primitive_table(cps.ctx).try_expand(cps, src, prim, &args, k, h)
                    {
                        term
                    } else {
                        with_cps!(cps;
                            let atom = #% prim (h) args ... @ src;
                            continue k (atom) @ src
                        )
                    }
                }),
                h,
            )
        }

        TermKind::Let(let_) => {
            assert!(!matches!(
                let_.style,
                LetStyle::LetRec | LetStyle::LetRecStar | LetStyle::LetStar
            ));

            let mut body = convert(cps, let_.body, k, h);

            for (binding, expr) in let_.lhs.iter().zip(let_.rhs.iter()) {
                let single = Array::from_slice(&cps.ctx, &[*binding]);
                if is_single_valued(*expr) {
                    body = with_cps!(cps;
                        letk (h) r#let (single...) @ src = body;
                        # convert(cps, *expr, r#let, h)
                    );
                } else {
                    body = with_cps!(cps;
                        letk (h) r#let (single... & rest) @ src = body;
                        # convert(cps, *expr, r#let, h)
                    );
                }
            }

            body
        }

        TermKind::Seq(head, etail) => {
            if is_zero_valued(*head) {
                with_cps!(cps;
                    letk (h) ktail (&vals) = convert(cps, *etail, k, h, );
                    # convert(cps, *head, ktail, h)
                )
            } else {
                with_cps!(cps;
                    letk (h) ktail (&vals) = convert(cps, *etail, k, h);
                    # convert(cps, *head, ktail, h)
                )
            }
        }

        TermKind::Fix(fix) => {
            if cps.current_topbox_scope.is_some() {
                let funcs = fix
                    .lhs
                    .iter()
                    .zip(fix.rhs.iter())
                    .map(|(binding, func)| {
                        let func = cps_func(cps, func, *binding);
                        func
                    })
                    .collect::<Vec<_>>();

                let body = convert(cps, fix.body, k, h);

                return Gc::new(
                    &cps.ctx,
                    Term::Fix(Array::from_slice(&cps.ctx, &funcs), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(
                cps,
                src,
                id,
                |cps| {
                    let form = convert(cps, exp, k, h);
                    cps.current_topbox_scope = prev;
                    form
                },
                h,
            )
        }

        TermKind::Receive(vars, variadic, producer, consumer) => {
            let consumer_k_var = cps.fresh_variable("consumer");

            let consumer_k = Cont {
                meta: cps.current_meta,
                name: Value::new(false),
                binding: consumer_k_var,
                args: *vars,
                variadic: *variadic,
                noinline: false,

                body: Lock::new(convert(cps, *consumer, k, h)),

                source: src,
                free_vars: Lock::new(None),
                reified: Cell::new(false),
                handler: Lock::new(h),
                cold: false,
            };

            let letk_body = convert(cps, *producer, consumer_k_var, h);
            let consumer_k = Gc::new(&cps.ctx, consumer_k);

            Gc::new(
                &cps.ctx,
                Term::Letk(Array::from_slice(&cps.ctx, &[consumer_k]), letk_body),
            )
        }

        TermKind::WithContinuationMark(key, mark, result) => convert_arg(
            cps,
            *key,
            Box::new(move |cps, key| {
                convert_arg(
                    cps,
                    *mark,
                    Box::new(move |cps, mark| {
                        with_cps!(cps;
                            let cs = #% "push-cframe" (h, key, mark, k) @ src;
                            # convert(cps, *result, cs, h)
                        )
                    }),
                    h,
                )
            }),
            h,
        ),
    }
}

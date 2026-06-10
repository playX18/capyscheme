use crate::cps::builder::CPSBuilder;
use crate::cps::term::{Atom, BranchHint, Cont, Func, FuncRef, Term, TermRef};
use crate::expander::core::{
    LVarRef, LetStyle, Proc, TermKind, TermRef as CoreTermRef, seq_from_slice,
};
use crate::rsgc::alloc::array::Array;
use crate::rsgc::cell::Lock;
use crate::rsgc::object::builtin_class_ids;
use crate::rsgc::{Gc, Global, Trace, barrier};
use crate::runtime::Context;
use crate::runtime::prelude::*;
use crate::runtime::value::Value;
use crate::runtime::value::{Str, Vector};
use crate::runtime::vm::exceptions::RaiseKind;
use crate::{list, static_symbols, with_cps};
use std::cell::Cell;
use std::collections::HashMap;
use std::mem::offset_of;
use std::sync::OnceLock;

pub fn cps_func<'gc>(
    builder: &mut CPSBuilder<'gc>,
    proc: &Proc<'gc>,
    binding: LVarRef<'gc>,
) -> FuncRef<'gc> {
    let old_meta = builder.current_meta;
    builder.current_meta = proc.meta;
    let return_cont = builder.fresh_variable("return");

    let body = convert(builder, proc.body, return_cont);
    builder.current_meta = old_meta;
    Gc::new(
        *builder.ctx,
        Func {
            meta: proc.meta,
            args: proc.args,
            name: proc.name,

            binding,
            source: proc.source,
            return_cont,
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
        barrier::field!(Gc::write(*ctx, seq), super::core::Term, source)
            .unlock()
            .set(source);
        seq
    };

    let proc = Proc {
        args: Array::from_slice(*ctx, []),
        name: Value::new(false),
        body: form,
        source: form.source(),
        variadic: None,
        meta: Value::null(),
    };

    let mut builder = CPSBuilder::new(ctx);

    let bind = builder.fresh_variable("toplevel");

    cps_func(&mut builder, &proc, bind)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{Scheme, vm::exceptions::RaiseKind};

    static TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    #[test]
    fn assertion_violation_lowers_directly_to_raise_term() {
        with_ctx(|ctx| {
            let mut cps = CPSBuilder::new(ctx);
            let who = Symbol::from_str(ctx, "car").into();
            let irritant = Atom::Constant(Value::new(1));

            let term =
                assertion_violation(&mut cps, Value::new(false), who, "not a pair", &[irritant]);

            let Term::Raise { kind, args, .. } = *term else {
                panic!("assertion_violation should lower directly to Term::Raise");
            };

            assert_eq!(kind, RaiseKind::AssertionViolation);
            assert_eq!(args.len(), 3);
            assert_eq!(args[0], Atom::Constant(who));
            assert_eq!(args[2], irritant);
        });
    }

    #[test]
    fn wrong_number_of_arguments_lowers_to_specific_raise_term() {
        with_ctx(|ctx| {
            let mut cps = CPSBuilder::new(ctx);

            let term = primitive_wrong_number_of_arguments(
                &mut cps,
                Value::new(false),
                RaiseKind::WrongNumberOfArgumentsCar,
                5,
            );

            let Term::Raise { kind, args, .. } = *term else {
                panic!("wrong_number_of_arguments should lower directly to Term::Raise");
            };

            assert_eq!(kind, RaiseKind::WrongNumberOfArgumentsCar);
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Atom::Constant(Value::new(5)));
        });
    }
}

pub type PrimitiveTransformer = for<'a, 'gc, 'b> fn(
    cps: &'a mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    params: &'b [Atom<'gc>],
    k: LVarRef<'gc>,
) -> Option<TermRef<'gc>>;

pub struct PrimitiveTable<'gc> {
    pub table: HashMap<Value<'gc>, PrimitiveTransformer>,
}
// SAFETY: `gc` for `PrimitiveTable` upholds all trait invariants
unsafe impl<'gc> Trace for PrimitiveTable<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        for key in self.table.keys() {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe {
                let value = key as *const Value<'gc> as *mut Value<'gc>;
                (*value).trace(visitor);
            }
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

type RootedPrimitiveTable = crate::Rootable!(PrimitiveTable<'_>);
type ArgContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc> + 'a>;
type ArgsContinuation<'a, 'gc> =
    Box<dyn FnOnce(&mut CPSBuilder<'gc>, Vec<Atom<'gc>>) -> TermRef<'gc> + 'a>;

static PRIMTABLE: OnceLock<Global<RootedPrimitiveTable>> = OnceLock::new();

macro_rules! primitive_transformers {
    ($(
        $prim: literal => $name : ident ($cps: ident, $src: ident, $op: ident, $args: ident, $k: ident) $b: block
    )*) => {
        #[allow(dead_code, unused_mut, unused_variables)]
        fn make_primitive_table<'gc>(ctx: Context<'gc>) -> HashMap<Value<'gc>, PrimitiveTransformer> {
            let mut table: HashMap<Value<'gc>, PrimitiveTransformer> = HashMap::new();
            $(
                table.insert(
                    Value::new(Symbol::from_str(ctx, $prim)),
                    { fn $name<'gc>($cps: &mut CPSBuilder<'gc>, $src: Value<'gc>, $op: Value<'gc>, $args: &[Atom<'gc>], $k: LVarRef<'gc>) -> Option<TermRef<'gc>> {
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
        .fetch(*ctx)
}

impl<'gc> PrimitiveTable<'gc> {
    pub fn try_expand(
        &self,
        cps: &mut CPSBuilder<'gc>,
        src: Value<'gc>,
        prim: Value<'gc>,
        args: &[Atom<'gc>],
        k: LVarRef<'gc>,
    ) -> Option<TermRef<'gc>> {
        if let Some(transformer) = self.table.get(&prim) {
            transformer(cps, src, prim, args, k)
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
pub fn assertion_violation<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    opc: Value<'gc>,
    message: &str,
    irritants: &[Atom<'gc>],
) -> TermRef<'gc> {
    let message = Atom::Constant(Value::new(Str::new(*cps.ctx, message, true)));
    let opc = Atom::Constant(opc);
    let mut args = vec![opc, message];
    args.extend_from_slice(irritants);
    Gc::new(
        *cps.ctx,
        Term::Raise {
            kind: RaiseKind::AssertionViolation,
            args: Array::from_slice(*cps.ctx, &args),
            source: src,
        },
    )
}

pub fn primitive_wrong_number_of_arguments<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    kind: RaiseKind,
    got: usize,
) -> TermRef<'gc> {
    Gc::new(
        *cps.ctx,
        Term::Raise {
            kind,
            args: Array::from_slice(*cps.ctx, [Atom::Constant(Value::new(got as i32))]),
            source: src,
        },
    )
}

pub fn ensure_string<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,

    have_length: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold not_string () = assertion_violation(cps, src, op, "not a string", &[x]);
        letk k () = with_cps!(cps;
            let length = #% "%refptr" (x, offset_of!(Str, length) as i32) @ src;
            # {
                have_length(cps, Atom::Local(length))
            }
        );
        letk check_immutable_string () = with_cps!(cps;
            let is_immutable_string = #% "%class-id?" (x, Value::new(builtin_class_ids::IMMUTABLE_STRING as i32)) @ src;
            if is_immutable_string => k | not_string
        );
        letk check_string () = with_cps!(cps;
            let is_string = #% "%class-id?" (x, Value::new(builtin_class_ids::STRING as i32)) @ src;
            if is_string => k | check_immutable_string
        );

        let is_immediate = #% "immediate?" (x) @ src;
        if is_immediate => not_string | check_string
    )
}

pub fn ensure_pair<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_pair: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold not_pair () = assertion_violation(cps, src, op, "not a pair", &[x]);
        letk k () = with_cps!(cps;
            # {
                have_pair(cps, x)
            }
        );
        let is_pair = #% "pair?" (x) @ src;
        if is_pair => k | not_pair
    )
}

pub fn ensure_vector<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_vector: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold not_vector () = assertion_violation(cps, src, op, "not a vector", &[x]);
        letk k () = with_cps!(cps;
            let length = #% "%refptr" (x, offset_of!(Vector, length) as i32) @ src;
            # {
                have_vector(cps, Atom::Local(length))
            }
        );
        let is_vector = #% "vector?" (x) @ src;
        if is_vector => k | not_vector
    )
}

fn ensure_variable<'gc, F>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    op: Value<'gc>,
    x: Atom<'gc>,
    have_variable: F,
) -> TermRef<'gc>
where
    F: FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
{
    with_cps!(cps;
        letk cold not_variable () = assertion_violation(cps, src, op, &format!("not a variable {}", not_variable.name), &[x]);
        letk k () = with_cps!(cps;
            # {
                have_variable(cps, x)
            }
        );
        let is_variable = #% "variable?" (x) @ src;
        if is_variable => k | not_variable
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
        letk bound (uidx) = have_index_in_range(cps, Atom::Local(uidx));
        letk boundlen (fix) = with_cps!(cps;
            let uidx = #% "s32->u64" (fix) @ src;
            let in_range = #% "u64<=" (uidx, ulen) @ src;
            if in_range => bound (uidx) | h()
        );
        letk untag () = with_cps!(cps;
            let fix = #% "untag-fixnum" (idx) @ src;
            let below0 = #% "s32<" (fix, Value::new(0)) @ src;
            if below0 => h() | boundlen(fix)
        );

        let is_fixnum = #% "fixnum?" (idx) @ src;
        if is_fixnum =>  untag | h
    )
}

primitive_transformers!(
    "string-length" => string_length(cps, src, op, args, k) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Str>() {
            let x = with_cps!(cps;
                continue k (val.downcast::<Str>().len() as i32)
            );
            return Some(x);
        }
        Some(ensure_string(cps, src, op, x, |cps, len| {
            with_cps!(cps;
                let vlen = #% "usize->value" (len) @ src;
                continue k (vlen)
            )
        }))
    }

    "vector-length" => vector_length(cps, src, op, args, k) {
        let x = args.first().copied()?;
        if let Atom::Constant(val) = x
            && val.is::<Vector>() {
            let x = with_cps!(cps;
                continue k (val.downcast::<Vector>().len() as i32)
            );
            return Some(x);
        }
        Some(ensure_vector(cps, src, op, x, |cps, len| {
            with_cps!(cps;
                let vlen = #% "usize->value" (len) @ src;
                continue k (vlen)
            )
        }))
    }



    "car" => car(cps, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsCar,
                args.len(),
            ));
        };

        Some(
            with_cps!(cps;
                let car = #% "car" (args[0]) @ src;
                continue k (car)
            )
        )
    }

    "cdr" => cdr(cps, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsCdr,
                args.len(),
            ));
        };

        Some(
            with_cps!(cps;
                let cdr = #% "cdr" (args[0]) @ src;
                continue k (cdr) @ src
            )
        )
    }

    "set-car!" => set_car(cps, src, op, args, k) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsSetCar,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, src, op, pair, |cps, pair| {
            with_cps!(cps;
                let _v = #% "set-car!" (pair, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "set-cdr!" => set_cdr(cps, src, op, args, k) {
        let Some((pair, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsSetCdr,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_pair(cps, src, op, pair, |cps, pair| {
            with_cps!(cps;
                let _v = #% "set-cdr!" (pair, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "variable-ref" => variable_ref(cps, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableRef,
                args.len(),
            ));
        };

        Some(ensure_variable(cps, src, op, x, |cps, var| {
            with_cps!(cps;
                let value = #% "variable-ref" (var) @ src;
                continue k (value) @ src
            )
        }))
    }

    "variable-set!" => variable_set(cps, src, op, args, k) {
        let Some((var, value)) = (args.len() == 2).then(|| (args[0], args[1])) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableSet,
                args.len(),
            ));
        };
        let undef = Atom::Constant(Value::undefined());
        Some(ensure_variable(cps, src, op, var, |cps, var| {
            with_cps!(cps;
                let _v = #% "variable-set!" (var, value) @ src;
                continue k (undef) @ src
            )
        }))
    }

    "variable-bound?" => variable_bound(cps, src, op, args, k) {
        let Some(x) = (args.len() == 1).then(|| args[0]) else {
            return Some(primitive_wrong_number_of_arguments(
                cps,
                src,
                RaiseKind::WrongNumberOfArgumentsVariableBound,
                args.len(),
            ));
        };

        Some(ensure_variable(cps, src, op, x, |cps, var| {
            with_cps!(cps;
                let bound = #% "variable-bound?" (var) @ src;
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
) -> TermRef<'gc> {
    match cps.current_topbox_scope {
        None => {
            if bound {
                with_cps!(cps;
                    let module = #% "current-module" () @ src;
                    let variable = #% "lookup-bound" (Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            } else {
                with_cps!(cps;
                    let module = #% "current-module" () @ src;
                    let variable = #% "lookup" (Atom::Local(module), Atom::Constant(name)) @ src;
                    # have_var(cps, Atom::Local(variable))
                )
            }
        }

        Some(scope) => {
            with_cps!(cps;
                letk kbox (box_) = have_var(cps, Atom::Local(box_));
                # cached_toplevel_box(cps, kbox, src, (scope as i32).into(), name, bound)
            )
        }
    }
}

pub fn module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    val_proc: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
    bound: bool,
    src: Value<'gc>,
) -> TermRef<'gc> {
    let _ = bound;
    with_cps!(cps;
        letk kbox (var) = val_proc(cps, Atom::Local(var));
        # cached_module_box(cps, kbox, src, module, name, public,)
    )
}

pub fn capture_toplevel_scope<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    scope_id: u32,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let module = #% "current-module" () @ src;
        # cache_current_module(cps, fk, src, Atom::Constant(Value::new(scope_id as i32)), Atom::Local(module))
    )
}

pub fn cached_toplevel_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    scope: Value<'gc>,
    name: Value<'gc>,
    bound: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, scope, name);
    with_cps!(cps;
        let cached = #% "cache-ref" (cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (cached) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline merge (cached) = with_cps!(cps; continue k (cached));
        letk cold kinit () = with_cps!(cps;
            let module = #%"cache-ref" (Atom::Constant(scope)) @ src;
            # reify_lookup(cps, src, module, name, bound, |cps, var| {
                with_cps!(cps;
                    let _k = #%"cache-set!"(cache_key, var) @ src;
                    continue merge(var)
                )
            })
        );
        letk kok () = with_cps!(cps; continue merge (cached));
        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_box<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    let cache_key = Value::cons(cps.ctx, module, Value::cons(cps.ctx, name, public.into()));

    with_cps!(cps;
        let cache_entry = #% "cache-ref" (cache_key) @ src;
        let is_heap_obj = #% "heap-object?" (cache_entry) @ src;
        // do not inline results of the cache lookup, this can blow up the code size
        letk noinline merge (cached) = with_cps!(cps; continue k (cached));
        letk cold kinit () = if public {
            with_cps!(cps;
                let var = #% "lookup-bound-public" (Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        } else {
            with_cps!(cps;
                let var = #% "lookup-bound-private" (Atom::Constant(module), Atom::Constant(name)) @ src;
                let _k = #% "cache-set!" (cache_key, Atom::Local(var)) @ src;
                continue merge (Atom::Local(var))
            )
        };

        letk kok () = with_cps!(cps; continue merge (cache_entry));

        if is_heap_obj => kok | kinit
    )
}

pub fn cached_module_boxk<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>, LVarRef<'gc>) -> TermRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    name: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        letk kbox (var) = fk(cps, var);
        # cached_toplevel_box(cps, kbox, src, module, name, public)
    )
}

pub fn cache_current_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    fk: impl FnOnce(&mut CPSBuilder<'gc>) -> TermRef<'gc>,
    src: Value<'gc>,
    scope: Atom<'gc>,
    module: Atom<'gc>,
) -> TermRef<'gc> {
    with_cps!(cps;
        let _k = #% "cache-set!" (scope, module) @ src;
        # fk(cps)
    )
}

pub fn reify_lookup<'gc>(
    cps: &mut CPSBuilder<'gc>,
    src: Value<'gc>,
    mod_var: LVarRef<'gc>,
    name: Value<'gc>,
    assert_bound: bool,
    have_var: impl FnOnce(&mut CPSBuilder<'gc>, Atom<'gc>) -> TermRef<'gc>,
) -> TermRef<'gc> {
    if assert_bound {
        with_cps!(cps;
            let variable = #%"lookup-bound" (Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    } else {
        with_cps!(cps;
            let variable = #%"lookup" (Atom::Local(mod_var), Atom::Constant(name)) @ src;
            #have_var(cps, Atom::Local(variable))
        )
    }
}

pub fn reify_resolve_module<'gc>(
    cps: &mut CPSBuilder<'gc>,
    k: LVarRef<'gc>,
    src: Value<'gc>,
    module: Value<'gc>,
    public: bool,
) -> TermRef<'gc> {
    with_cps!(cps;
        let resolved = #% "resolve-module" (Atom::Constant(public.into()), Atom::Constant(module)) @ src;
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
    k: ArgContinuation<'a, 'gc>,
) -> TermRef<'gc> {
    let src = exp.source();
    match exp.kind {
        TermKind::LRef(var) => k(cps, Atom::Local(var)),

        _ if is_single_valued(exp) => {
            with_cps!(cps;
                letk karg (arg) @ src = k(cps, Atom::Local(arg));
                # {
                    convert(cps, exp, karg, )
            }
            )
        }

        _ => {
            with_cps!(cps;
                letk karg (arg & rest) @ src = k(cps, Atom::Local(arg));
                # convert(cps, exp, karg, )
            )
        }
    }
}

pub fn convert_args<'gc, 'a>(
    cps: &mut CPSBuilder<'gc>,
    exps: &'a [CoreTermRef<'gc>],
    fk: ArgsContinuation<'a, 'gc>,
) -> TermRef<'gc> {
    if exps.is_empty() {
        return fk(cps, Vec::new());
    }

    let exp = exps[0];
    let exps = &exps[1..];
    exps.is_empty();
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
            )
        }),
    )
}

pub fn convert<'gc>(
    cps: &mut CPSBuilder<'gc>,
    exp: CoreTermRef<'gc>,
    k: LVarRef<'gc>,
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
        ),

        TermKind::If(test, consequent, alternate) => convert_arg(
            cps,
            *test,
            Box::new(move |cps, test| {
                with_cps!(cps;
                    letk kconseq () = convert(cps, *consequent, k,);
                    letk kalt () = convert(cps, *alternate, k,);
                    if test => kconseq | kalt
                )
            }),
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

        TermKind::PrimRef(name) => {
            let module = list!(cps.ctx, cps.ctx.intern("capy"));

            module_box(
                cps,
                |cps, var| {
                    with_cps!(cps;
                        let val = #% "variable-ref" (var) @ src;
                        continue k(Atom::Local(val))
                    )
                },
                module,
                *name,
                true,
                true,
                src,
            )
        }

        TermKind::ToplevelRef(_, name) => toplevel_box(cps, src, *name, true, |cps, var| {
            with_cps!(cps;
                let val = #% "variable-ref" (var) @ src;
                continue k(Atom::Local(val))
            )
        }),

        TermKind::ModuleRef(module, name, public) => module_box(
            cps,
            |cps, var| {
                with_cps!(cps;
                    let val = #% "variable-ref" (var) @ src;
                    continue k(Atom::Local(val))
                )
            },
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
                toplevel_box(cps, src, *name, false, |cps, var| {
                    with_cps!(cps;
                        let _val = #% "variable-set!" (var, atom) @ src;
                        continue k(_val) @ src
                    )
                })
            }),
        ),

        TermKind::ModuleSet(module, name, public, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, exp| {
                module_box(
                    cps,
                    |cps, var| {
                        with_cps!(cps;
                            let _val = #% "variable-set!" (var, exp) @ src;
                            continue k(_val) @ src
                        )
                    },
                    *module,
                    *name,
                    *public,
                    true,
                    src,
                )
            }),
        ),

        TermKind::Proc(proc) => {
            if cps.current_topbox_scope.is_some() {
                let tmp = cps.fresh_variable("proc");
                let func = cps_func(cps, proc, tmp);
                let body = with_cps!(cps;
                    continue k (Atom::Local(tmp)) @ src
                );

                return Gc::new(
                    *cps.ctx,
                    Term::Fix(Array::from_slice(*cps.ctx, [func]), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(cps, src, id, |cps| {
                let form = convert(cps, exp, k);
                cps.current_topbox_scope = prev;
                form
            })
        }

        TermKind::Define(_module, name, exp) => convert_arg(
            cps,
            *exp,
            Box::new(move |cps, atom| {
                with_cps! {cps;
                    let _rv = #% "define" (Atom::Constant(*name), atom) @ src;
                     continue k (_rv) @ src
                }
            }),
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
                            proc(k, args ...) @ src
                        )
                    }),
                )
            }),
        ),

        TermKind::PrimCall(prim, args) => {
            let prim = *prim;
            convert_args(
                cps,
                args,
                Box::new(move |cps, args| {
                    if let Some(term) =
                        get_primitive_table(cps.ctx).try_expand(cps, src, prim, &args, k)
                    {
                        term
                    } else {
                        with_cps!(cps;
                            let atom = #% prim args ... @ src;
                            continue k (atom) @ src
                        )
                    }
                }),
            )
        }

        TermKind::Let(let_) => {
            assert!(!matches!(
                let_.style,
                LetStyle::LetRec | LetStyle::LetRecStar | LetStyle::LetStar
            ));

            let mut body = convert(cps, let_.body, k);

            for (binding, expr) in let_.lhs.iter().zip(let_.rhs.iter()) {
                let single = Array::from_slice(*cps.ctx, [*binding]);
                if is_single_valued(*expr) {
                    body = with_cps!(cps;
                        letk r#let (single...) @ src = body;
                        # convert(cps, *expr, r#let,)
                    );
                } else {
                    body = with_cps!(cps;
                        letk r#let (single... & rest) @ src = body;
                        # convert(cps, *expr, r#let,)
                    );
                }
            }

            body
        }

        TermKind::Seq(head, etail) => {
            with_cps!(cps;
                letk ktail (&vals) = convert(cps, *etail, k,);
                # convert(cps, *head, ktail,)
            )
        }

        TermKind::Fix(fix) => {
            if cps.current_topbox_scope.is_some() {
                let funcs = fix
                    .lhs
                    .iter()
                    .zip(fix.rhs.iter())
                    .map(|(binding, func)| cps_func(cps, func, *binding))
                    .collect::<Vec<_>>();

                let body = convert(cps, fix.body, k);

                return Gc::new(
                    *cps.ctx,
                    Term::Fix(Array::from_slice(*cps.ctx, &funcs), body),
                );
            }

            let prev = cps.current_topbox_scope;
            cps.enter_scope();
            let id = cps.current_topbox_scope.unwrap();
            capture_toplevel_scope(cps, src, id, |cps| {
                let form = convert(cps, exp, k);
                cps.current_topbox_scope = prev;
                form
            })
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

                body: Lock::new(convert(cps, *consumer, k)),

                source: src,
                free_vars: Lock::new(None),
                reified: Cell::new(false),
                cold: false,
            };

            let letk_body = convert(cps, *producer, consumer_k_var);
            let consumer_k = Gc::new(*cps.ctx, consumer_k);

            Gc::new(
                *cps.ctx,
                Term::Letk(Array::from_slice(*cps.ctx, [consumer_k]), letk_body),
            )
        }

        TermKind::WithContinuationMark(key, mark, result) => {
            let thunk = Gc::new(
                *cps.ctx,
                Proc {
                    args: Array::from_slice(*cps.ctx, []),
                    name: Value::new(false),
                    source: src,
                    variadic: None,
                    body: *result,
                    meta: cps.current_meta,
                },
            );
            let thunk = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    kind: TermKind::Proc(thunk),
                    source: src.into(),
                },
            );

            let mref = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    kind: TermKind::PrimRef(cps.ctx.intern("call-with-continuation-mark")),
                    source: src.into(),
                },
            );

            let call = Gc::new(
                *cps.ctx,
                crate::expander::core::Term {
                    source: src.into(),
                    kind: TermKind::Call(mref, Array::from_slice(*cps.ctx, [*key, *mark, thunk])),
                },
            );

            convert(cps, call, k)
        }
    }
}

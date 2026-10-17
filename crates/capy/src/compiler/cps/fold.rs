//! Constant-folding for CPS terms.

use crate::heap::Global;
use crate::heap::Trace;
use crate::runtime::MutationContext;
use crate::runtime::value::*;
use crate::runtime::value::{Number, Value};
use std::collections::HashMap;
use std::sync::OnceLock;

type RootedFoldingTable = crate::Rootable!(FoldingTable<'_>);

pub struct FoldingTable<'gc> {
    table: HashMap<Value<'gc>, FoldingEntry<'gc>>,
}

impl<'gc> FoldingTable<'gc> {
    pub fn try_fold_values(
        &self,
        ctx: MutationContext<'gc>,
        prim: Value<'gc>,
        args: &[Value<'gc>],
    ) -> Option<Value<'gc>> {
        let entry = self.table.get(&prim)?;
        entry.apply(ctx, args)
    }
}

// SAFETY: FoldingTable stores `Value` keys in a HashMap. During GC tracing we must update
// moved pointers in-place. The `*const Value as *mut Value` cast is safe here because:
// 1. We hold `&mut self` (exclusive access) during tracing, no concurrent readers exist.
// 2. The HashMap keys' addresses are stable during iteration (no rehashing occurs).
// 3. The GC only updates pointer bits inside Values, not their hash or equality behavior
//    (NaN-boxed cell pointers preserve their bucket identity after forwarding).
unsafe impl<'gc> Trace for FoldingTable<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            for val in self.table.keys() {
                // SAFETY: See above, exclusive `&mut self` access makes this const-to-mut cast sound.
                let val = val as *const Value<'gc> as *mut Value<'gc>;
                (*val).trace(visitor);
            }
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        let _weak_processor = weak_processor;
    }
}

struct FoldingEntry<'gc> {
    func: fn(MutationContext<'gc>, &[Value<'gc>]) -> Option<Value<'gc>>,
    args: usize,
    variadic: bool,
}

impl<'gc> FoldingEntry<'gc> {
    pub fn new(
        func: fn(MutationContext<'gc>, &[Value<'gc>]) -> Option<Value<'gc>>,
        args: usize,
        variadic: bool,
    ) -> Self {
        Self {
            func,
            args,
            variadic,
        }
    }

    pub fn apply(&self, ctx: MutationContext<'gc>, atoms: &[Value<'gc>]) -> Option<Value<'gc>> {
        if atoms.len() == self.args || (self.variadic && atoms.len() >= self.args) {
            (self.func)(ctx, atoms)
        } else {
            None
        }
    }
}

macro_rules! folding {
    ($ctx1: expr; $($prim: literal => $name: ident ($ctx: ident $(,)? $($arg: ident),*) $b: block)*) => {{
        let mut table = FoldingTable {
            table: HashMap::new(),
        };
        $(let mut argc = 0;
        $(
            let _ = stringify!($arg);
            argc += 1;
        )*

            // Generated bodies ignore some bindings (`ctx`, trailing `i`) by design.
            #[allow(unused_variables, unused_assignments)]
            fn $name<'gc>($ctx: MutationContext<'gc>, values: &[Value<'gc>]) -> Option<Value<'gc>> {
                let mut i = 0;
                $(
                    let $arg = values[i];
                    i += 1;
                )*
                    $b
            }

        table.table.insert(
            Value::new(Symbol::from_str($ctx1, $prim)),
            FoldingEntry::new($name, argc, false),
        );

        )*

        table
    }};
}

fn build_table<'gc>(ctx: MutationContext<'gc>) -> FoldingTable<'gc> {
    let table = folding!(ctx;
        "zero?" => is_zero(ctx, a) {
            let a = a.number()?;

            Some(Value::new(a.is_zero()))
        }
    "+" => plus(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some(Number::add(ctx, a, b).into_value(ctx))
        }
    "-" => minus(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some(Number::sub(ctx, a, b).into_value(ctx))
        }
    "*" => mul(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some(Number::mul(ctx, a, b).into_value(ctx))
        }
    "/" => div(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            if b.is_zero() && (b.is_exact() && a.is_exact()) {
                return None; // Division by zero
            }

            Some(Number::div(ctx, a, b).into_value(ctx))
        }
    "=" => eq(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some(Number::equal(ctx, a, b).into_value(ctx))
        }
    ">" => gt(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some((Number::compare(ctx, a, b) == Some(std::cmp::Ordering::Greater)).into_value(ctx))
        }
    "<" => lt(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some((Number::compare(ctx, a, b) == Some(std::cmp::Ordering::Less)).into_value(ctx))
        }
    ">=" => ge(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some((Number::compare(ctx, a, b) != Some(std::cmp::Ordering::Less)).into_value(ctx))
        }
    "<=" => le(ctx, a,b) {

            let a = a.number()?;
            let b = b.number()?;

            Some((Number::compare(ctx, a, b) != Some(std::cmp::Ordering::Greater)).into_value(ctx))
        }
    "not" => not(ctx, a) {
            Some(Value::new(!a.as_bool()))
        }
    "pair?" => is_pair(ctx, a) {
            Some(Value::new(a.is_pair()))
        }
    "null?" => is_null(ctx, a) {
            Some(Value::new(a.is_null()))
        }
    "unspecified?" => is_unspecified(ctx, a) {
            Some(Value::new(a == Value::undefined()))
        }
    "list?" => is_list(ctx, a) {
            Some(Value::new(a.is_list()))
        }
    "vector?" => is_vector(ctx, a) {
            Some(Value::new(a.is::<Vector>()))
        }
    "bytevector?" => is_bytevector(ctx, a) {
            Some(Value::new(a.is::<ByteVector>()))
        }
    "symbol?" => is_symbol(ctx, a) {
            Some(Value::new(a.is::<Symbol>()))
        }
    "string?" => is_string(ctx, a) {
            Some(Value::new(a.is::<Str>()))
        }
    "boolean?" => is_boolean(ctx, a) {
            Some(Value::new(a.is_bool()))
        }
    "number?" => is_number(ctx, a) {
            Some(Value::new(a.is_number()))
        }
    "char?" => is_char(ctx, a) {
            Some(Value::new(a.is_char()))
        }
    "char->integer" => char_to_integer(ctx, a) {
            if a.is_char() {
                Some(Value::new(a.char() as u32 as i32))
            } else {
                None
            }
        }
    "integer->char" => integer_to_char(ctx, a) {
            if a.is_int32() {
                let c = a.as_int32();
                if (0..=0x10FFFF).contains(&c) {
                    Some(Value::new(std::char::from_u32(c as u32)?))
                } else {
                    None
                }
            } else {
                None
            }
        }
    "string->symbol" => string_to_symbol(ctx, a) {
            a.try_as::<Str>().map(|s| Value::new(Symbol::from_string(ctx, s)))
        }
    "%class-id?" => class_idp(ctx, a, b) {
            let class_id = b.int32()?;
            Some(Value::new(
                a.class_id()
                    .is_some_and(|actual| actual.bits() == class_id as u32),
            ))
        }
    "immediate?" => is_immediate(ctx, a) {
            return None;
        }
    ".is-cell" => is_cell(ctx, a) {
            Some(Value::new(a.is_cell()))
        }
    "usize->value" => usize_to_value(ctx, a) {
            Some(Number::from_usize(ctx, a.bits() as usize).into_value(ctx))
        }

    );

    table
}

static FOLDING_TABLE: OnceLock<Global<RootedFoldingTable>> = OnceLock::new();

pub fn folding_table<'gc>(ctx: MutationContext<'gc>) -> &'gc FoldingTable<'gc> {
    FOLDING_TABLE
        .get_or_init(|| {
            let table = build_table(ctx);
            Global::new(table)
        })
        .fetch(ctx)
}

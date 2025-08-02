//! Constant-folding for CPS terms.
//!
#![allow(dead_code, unused_variables, unused_mut, unused_assignments)]

use rsgc::Gc;
use rsgc::Global;
use rsgc::Rootable;
use rsgc::Trace;
use rsgc::alloc::Array;
use rsgc::mmtk::util::Address;
use rsgc::mmtk::vm::slot::SimpleSlot;
use rsgc::object::GCObject;

use crate::cps::term::Expression;
use crate::cps::term::FuncRef;
use crate::cps::term::Term;
use crate::cps::term::TermRef;
use crate::cps::term::Throw;
use crate::runtime::Context;
use crate::runtime::value::*;
use crate::{
    cps::term::Atom,
    runtime::value::{Number, Value},
};
use std::collections::HashMap;
use std::sync::OnceLock;

pub fn fold<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    rec(ctx, &mut HashMap::new(), term)
}

pub fn fold_func<'gc>(ctx: Context<'gc>, func: FuncRef<'gc>) -> FuncRef<'gc> {
    let body = fold(ctx, func.body);
    func.with_body(ctx, body)
}

fn subst_atom<'gc>(subst: &mut HashMap<Atom<'gc>, Atom<'gc>>, atom: Atom<'gc>) -> Atom<'gc> {
    subst.get(&atom).cloned().unwrap_or(atom)
}

fn rec<'gc>(
    ctx: Context<'gc>,
    subst: &mut HashMap<Atom<'gc>, Atom<'gc>>,
    term: TermRef<'gc>,
) -> TermRef<'gc> {
    match *term {
        Term::Let(bind, expr, body) => match expr {
            Expression::PrimCall(prim, args, src) => {
                let args = args
                    .iter()
                    .map(|arg| subst.get(arg).cloned().unwrap_or(*arg))
                    .collect::<Vec<_>>();

                if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
                    let table = folding_table(ctx);

                    if let Some(entry) = table.table.get(&prim)
                        && entry.args == args.len()
                        && !entry.variadic
                    {
                        let args = args
                            .iter()
                            .map(|arg| match arg {
                                Atom::Constant(val) => val.clone(),
                                _ => unreachable!(),
                            })
                            .collect::<Vec<_>>();

                        if let Some(result) = entry.apply(ctx, &args) {
                            let new_atom = Atom::Constant(result);
                            subst.insert(Atom::Local(bind), new_atom.clone());
                        }

                        return rec(ctx, subst, body);
                    }
                }

                let args = Array::from_array(&ctx, args);

                Gc::new(
                    &ctx,
                    Term::Let(
                        bind,
                        Expression::PrimCall(prim, args, src),
                        rec(ctx, subst, body),
                    ),
                )
            }
        },

        Term::App(f, k, args, src) => {
            let args = args
                .iter()
                .map(|arg| subst_atom(subst, *arg))
                .collect::<Vec<_>>();

            Gc::new(
                &ctx,
                Term::App(subst_atom(subst, f), k, Array::from_array(&ctx, args), src),
            )
        }

        Term::Continue(k, args, src) => {
            let args = args
                .iter()
                .map(|arg| subst_atom(subst, *arg))
                .collect::<Vec<_>>();

            Gc::new(&ctx, Term::Continue(k, Array::from_array(&ctx, args), src))
        }

        Term::If(test, kcons, kalt, hint) => {
            let test = subst_atom(subst, test);
            if let Atom::Constant(val) = test {
                let k = if val.as_bool() { kcons } else { kalt };

                return Gc::new(
                    &ctx,
                    Term::Continue(k, Array::from_array(&ctx, []), Value::new(false)),
                );
            }

            Gc::new(&ctx, Term::If(test, kcons, kalt, hint))
        }

        Term::Letk(conts, body) => {
            let body = rec(ctx, subst, body);
            let conts = conts
                .iter()
                .map(|cont| {
                    let body = rec(ctx, subst, cont.body().unwrap());
                    cont.with_body(ctx, body)
                })
                .collect::<Vec<_>>();

            Gc::new(&ctx, Term::Letk(Array::from_array(&ctx, conts), body))
        }

        Term::Fix(funcs, body) => {
            let body = rec(ctx, subst, body);
            let funcs = funcs
                .iter()
                .map(|func| {
                    let body = rec(ctx, subst, func.body);
                    func.with_body(ctx, body)
                })
                .collect::<Vec<_>>();

            Gc::new(&ctx, Term::Fix(Array::from_array(&ctx, funcs), body))
        }

        Term::Throw(throw, src) => {
            let throw = match throw {
                Throw::Throw(key, args) => {
                    let key = subst_atom(subst, key);
                    let args = subst_atom(subst, args);
                    Throw::Throw(key, args)
                }
                Throw::Value(key, subr_and_message) => {
                    Throw::Value(subst_atom(subst, key), subst_atom(subst, subr_and_message))
                }
                Throw::ValueAndData(key, data) => {
                    Throw::ValueAndData(subst_atom(subst, key), subst_atom(subst, data))
                }
            };

            Gc::new(&ctx, Term::Throw(throw, src))
        }
    }
}

pub struct FoldingTable<'gc> {
    table: HashMap<Value<'gc>, FoldingEntry<'gc>>,
}

impl<'gc> FoldingTable<'gc> {
    pub fn try_fold(
        &self,
        ctx: Context<'gc>,
        prim: Value<'gc>,
        args: &[Atom<'gc>],
    ) -> Option<Value<'gc>> {
        if args.iter().all(|arg| matches!(arg, Atom::Constant(_))) {
            if let Some(entry) = self.table.get(&prim) {
                let args = args
                    .iter()
                    .map(|arg| match arg {
                        Atom::Constant(val) => val.clone(),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                let res = entry.apply(ctx, &args);
                if let Some(res) = res {
                    println!(
                        "folded {}({}) -> {}",
                        prim,
                        args.iter()
                            .map(|a| a.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        res
                    );
                }
                return res;
            }
        }

        None
    }
}

unsafe impl<'gc> Trace for FoldingTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        unsafe {
            for (val, _) in self.table.iter_mut() {
                let val = val as *const Value<'gc> as *mut Value<'gc>;
                (*val).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

struct FoldingEntry<'gc> {
    func: fn(Context<'gc>, &[Value<'gc>]) -> Option<Value<'gc>>,
    args: usize,
    variadic: bool,
}

impl<'gc> FoldingEntry<'gc> {
    pub fn new(
        func: fn(Context<'gc>, &[Value<'gc>]) -> Option<Value<'gc>>,
        args: usize,
        variadic: bool,
    ) -> Self {
        Self {
            func,
            args,
            variadic,
        }
    }

    pub fn apply(&self, ctx: Context<'gc>, atoms: &[Value<'gc>]) -> Option<Value<'gc>> {
        if atoms.len() == self.args || (self.variadic && atoms.len() > self.args) {
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


            fn $name<'gc>($ctx: Context<'gc>, values: &[Value<'gc>]) -> Option<Value<'gc>> {
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

        return table
    }};
}

fn build_table<'gc>(ctx: Context<'gc>) -> FoldingTable<'gc> {
    folding!(ctx;
        "+" => plus(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some(Number::add(ctx, a, b).into_value(ctx))
        }

        "-" => minus(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some(Number::sub(ctx, a, b).into_value(ctx))
        }

        "*" => mul(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some(Number::mul(ctx, a, b).into_value(ctx))
        }

        "/" => div(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            if b.is_zero() {
                return None; // Division by zero
            }

            Some(Number::div(ctx, a, b).into_value(ctx))
        }

        "=" => eq(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some(Number::equal(ctx, a, b).into_value(ctx))
        }

        ">" => gt(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some((Number::compare(ctx, a, b) == Some(std::cmp::Ordering::Greater)).into_value(ctx))
        }

        "<" => lt(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some((Number::compare(ctx, a, b) == Some(std::cmp::Ordering::Less)).into_value(ctx))
        }

        ">=" => ge(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

            Some((Number::compare(ctx, a, b) != Some(std::cmp::Ordering::Less)).into_value(ctx))
        }

        "<=" => le(ctx, a,b) {

            let Some(a) = a.number() else { return None; };
            let Some(b) = b.number() else { return None; };

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
                if c >= 0 && c <= 0x10FFFF {
                    Some(Value::new(std::char::from_u32(c as u32).unwrap()))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "string->symbol" => string_to_symbol(ctx, a) {
            if let Some(s) = a.try_as::<Str>() {
                Some(Value::new(Symbol::from_string(ctx, s)))
            } else {
                None
            }
        }

        "symbol->string" => symbol_to_string(ctx, a) {
            if let Some(sym) = a.try_as::<Symbol>() {
                Some(Value::new(sym.to_str(&ctx)))
            } else {
                None
            }
        }

        "car" => car(ctx, a) {
            if a.is_pair() {
                Some(a.car())
            } else {
                None
            }
        }

        "cdr" => cdr(ctx, a) {
            if a.is_pair() {
                Some(a.cdr())
            } else {
                None
            }
        }

        "cons" => cons(ctx, a,b) {
            Some(Value::cons(ctx, a, b))
        }

        /*
            raw primitive operations

            We need to be careful with these, they do not per-se check types,
            but we try to check them here to avoid runtime undefined behavior.
         */


        ".typecode8" => typecode8(ctx, a) {

            if a.is_cell() {
                Some(Value::from_raw(a.typ8().bits() as u64))
            } else {
                panic!("BUG: .typecode8 called on non-cell value")
            }
        }

        ".typecode16" => typecode16(ctx, a) {
            if a.is_cell() {
                Some(Value::from_raw(a.typ16().bits() as u64))
            } else {
                panic!("BUG: .typecode16 called on non-cell value")
            }
        }

        ".is-cell" => is_cell(ctx, a) {
            Some(Value::new(a.is_cell()))
        }

        ".iconst" => iconst(ctx, a) {
            if let Some(n) = a.number() {
                match n {
                    Number::Fixnum(fix) => Some(Value::from_raw(fix as u64)),
                    Number::BigInt(bignum) => Some(Value::from_raw(bignum.try_as_u64().expect("BUG: .iconst constant is too big"))),
                    Number::Flonum(flonum) => Some(Value::from_raw(flonum.to_bits() as u64)),

                    _ => panic!("BUG: .iconst called on non-integer value"),
                }
            } else {
                panic!("BUG: .iconst called on non-number value")
            }
        }

        ".iconst16" => iconst16(ctx, a) {
            if let Some(n) = a.number() {
                match n {
                    Number::Fixnum(fix) => Some(Value::from_raw((fix & 0xFFFF) as u64)),
                    Number::BigInt(bignum) => Some(Value::from_raw((bignum.try_as_u64().expect("BUG: .iconst16 constant is too big") & 0xFFFF) as u64)),
                    _ => panic!("BUG: .iconst16 called on non-integer value"),
                }
            } else {
                panic!("BUG: .iconst16 called on non-number value")
            }
        }

        ".iconst32" => iconst32(ctx, a) {
            if let Some(n) = a.number() {
                match n {
                    Number::Fixnum(fix) => Some(Value::from_raw(fix as u32 as u64)),
                    Number::BigInt(bignum) => Some(Value::from_raw((bignum.try_as_u64().expect("BUG: .iconst32 constant is too big") & 0xFFFFFFFF) as u64)),
                    _ => panic!("BUG: .iconst32 called on non-integer value"),
                }
            } else {
                panic!("BUG: .iconst32 called on non-number value")
            }
        }

        ".iconst64" => iconst64(ctx, a) {
            if let Some(n) = a.number() {
                match n {
                    Number::Fixnum(fix) => Some(Value::from_raw(fix as u64)),
                    Number::BigInt(bignum) => Some(Value::from_raw(bignum.try_as_u64().expect("BUG: .iconst64 constant is too big"))),
                    _ => panic!("BUG: .iconst64 called on non-integer value"),
                }
            } else {
                panic!("BUG: .iconst64 called on non-number value")
            }
        }

        ".ref8" => ref8(ctx, a, offset) {
            if !a.is_cell() {
                panic!("BUG: attempt to load reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                Some(Value::from_raw(addr.load::<u8>() as u64))
            }
        }

        ".ref16" => ref16(ctx, a, offset) {
            if !a.is_cell() {
                panic!("BUG: attempt to load reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                Some(Value::from_raw(addr.load::<u16>() as u64))
            }
        }

        ".ref32" => ref32(ctx, a, offset) {
            if !a.is_cell() {
                panic!("BUG: attempt to load reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                Some(Value::from_raw(addr.load::<u32>() as u64))
            }
        }

        ".ref64" => ref64(ctx, a, offset) {
            if !a.is_cell() {
                panic!("BUG: attempt to load reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                Some(Value::from_raw(addr.load::<u64>()))
            }
        }

        ".refptr" => refptr(ctx, a, offset) {
            if !a.is_cell() {
                panic!("BUG: attempt to load reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                Some(Value::from_raw(addr.load::<usize>() as u64))
            }
        }

        ".set8" => set8(ctx, a, offset, value) {
            if !a.is_cell() {
                panic!("BUG: attempt to store reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;
            let value = value.bits() as u8;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                addr.store(value);
            }
            Some(Value::undefined())
        }

        ".set16" => set16(ctx, a, offset, value) {
            if !a.is_cell() {
                panic!("BUG: attempt to store reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;
            let value = value.bits() as u16;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                addr.store(value);
            }
            Some(Value::undefined())
        }

        ".set32" => set32(ctx, a, offset, value) {
            if !a.is_cell() {
                panic!("BUG: attempt to store reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;
            let value = value.bits() as u32;

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                addr.store(value);
            }
            Some(Value::undefined())
        }

        ".set64" => set64(ctx, a, offset, value) {
            if !a.is_cell() {
                panic!("BUG: attempt to store reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }

            let offset = offset.as_int32() as isize;
            let value = value.bits();

            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset;
                addr.store(value);
            }
            Some(Value::undefined())
        }

        ".set-ref64" => set_ref64(ctx, a, offset, value) {
            if !a.is_cell() {
                panic!("BUG: attempt to store reference on non-cell value");
            }

            if !offset.is_int32() {
                return None;
            }


            unsafe {
                let addr = Address::from_usize(a.bits() as usize) + offset.as_int32() as isize;
                addr.store(value.bits() as u64);
                let slot = SimpleSlot::from_address(addr);
                ctx.mutation().raw_object_reference_write_post(a.as_cell_raw(), slot, GCObject::NULL);
            }
            Some(Value::undefined())
        }

        "u8+" => u8_add(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u8).wrapping_add(b.bits() as u8) as u64))
        }

        "u8-" => u8_sub(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u8).wrapping_sub(b.bits() as u8) as u64))
        }

        "u8*" => u8_mul(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u8).wrapping_mul(b.bits() as u8) as u64))
        }

        "u8/" => u8_div(ctx, a, b) {
            if b.bits() as u8 == 0 {
            return None; // Division by zero
            }
            Some(Value::from_raw((a.bits() as u8).wrapping_div(b.bits() as u8) as u64))
        }

        "u8=" => u8_eq(ctx, a, b) {
            Some(Value::new((a.bits() as u8) == (b.bits() as u8)))
        }

        "u8>" => u8_gt(ctx, a, b) {
            Some(Value::new((a.bits() as u8) > (b.bits() as u8)))
        }

        "u8<" => u8_lt(ctx, a, b) {
            Some(Value::new((a.bits() as u8) < (b.bits() as u8)))
        }

        "u8>=" => u8_ge(ctx, a, b) {
            Some(Value::new((a.bits() as u8) >= (b.bits() as u8)))
        }

        "u8<=" => u8_le(ctx, a, b) {
            Some(Value::new((a.bits() as u8) <= (b.bits() as u8)))
        }

        "u8not" => u8_not(ctx, a) {
            Some(Value::from_raw(!(a.bits() as u8) as u64))
        }

        "u8<<" => u8_shl(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u8).wrapping_shl(b.bits() as u32) as u64))
        }

        "u8>>" => u8_shr(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u8).wrapping_shr(b.bits() as u32) as u64))
        }

        "u8and" => u8_and(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u8) & (b.bits() as u8)) as u64))
        }

        "u8or" => u8_or(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u8) | (b.bits() as u8)) as u64))
        }

        "u8xor" => u8_xor(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u8) ^ (b.bits() as u8)) as u64))
        }

        "u8->u16" => u8_to_u16(ctx, a) {
            Some(Value::from_raw((a.bits() as u8) as u16 as u64))
        }

        "u8->u32" => u8_to_u32(ctx, a) {
            Some(Value::from_raw((a.bits() as u8) as u32 as u64))
        }

        "u8->u64" => u8_to_u64(ctx, a) {
            Some(Value::from_raw((a.bits() as u8) as u64))
        }

        "u16+" => u16_add(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u16).wrapping_add(b.bits() as u16) as u64))
        }

        "u16-" => u16_sub(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u16).wrapping_sub(b.bits() as u16) as u64))
        }

        "u16*" => u16_mul(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u16).wrapping_mul(b.bits() as u16) as u64))
        }

        "u16/" => u16_div(ctx, a, b) {
            if b.bits() as u16 == 0 {
            return None; // Division by zero
            }
            Some(Value::from_raw((a.bits() as u16).wrapping_div(b.bits() as u16) as u64))
        }

        "u16<<" => u16_shl(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u16).wrapping_shl(b.bits() as u32) as u64))
        }

        "u16>>" => u16_shr(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u16).wrapping_shr(b.bits() as u32) as u64))
        }

        "u16and" => u16_and(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u16) & (b.bits() as u16)) as u64))
        }

        "u16or" => u16_or(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u16) | (b.bits() as u16)) as u64))
        }

        "u16xor" => u16_xor(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u16) ^ (b.bits() as u16)) as u64))
        }

        "u16not" => u16_not(ctx, a) {
            Some(Value::from_raw(!(a.bits() as u16) as u64))
        }

        "u16=" => u16_eq(ctx, a, b) {
            Some(Value::new((a.bits() as u16) == (b.bits() as u16)))
        }

        "u16>" => u16_gt(ctx, a, b) {
            Some(Value::new((a.bits() as u16) > (b.bits() as u16)))
        }

        "u16<" => u16_lt(ctx, a, b) {
            Some(Value::new((a.bits() as u16) < (b.bits() as u16)))
        }

        "u16>=" => u16_ge(ctx, a, b) {
            Some(Value::new((a.bits() as u16) >= (b.bits() as u16)))
        }

        "u16<=" => u16_le(ctx, a, b) {
            Some(Value::new((a.bits() as u16) <= (b.bits() as u16)))
        }

        "u16->u8" => u16_to_u8(ctx, a) {
            Some(Value::from_raw((a.bits() as u16) as u8 as u64))
        }

        "u16->u32" => u16_to_u32(ctx, a) {
            Some(Value::from_raw((a.bits() as u16) as u32 as u64))
        }

        "u16->u64" => u16_to_u64(ctx, a) {
            Some(Value::from_raw((a.bits() as u16) as u64))
        }

        "u32+" => u32_add(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u32).wrapping_add(b.bits() as u32) as u64))
        }

        "u32-" => u32_sub(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u32).wrapping_sub(b.bits() as u32) as u64))
        }

        "u32*" => u32_mul(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u32).wrapping_mul(b.bits() as u32) as u64))
        }

        "u32/" => u32_div(ctx, a, b) {
            if b.bits() as u32 == 0 {
            return None; // Division by zero
            }
            Some(Value::from_raw((a.bits() as u32).wrapping_div(b.bits() as u32) as u64))
        }

        "u32<<" => u32_shl(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u32).wrapping_shl(b.bits() as u32) as u64))
        }

        "u32>>" => u32_shr(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u32).wrapping_shr(b.bits() as u32) as u64))
        }

        "u32and" => u32_and(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u32) & (b.bits() as u32)) as u64))
        }

        "u32or" => u32_or(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u32) | (b.bits() as u32)) as u64))
        }

        "u32xor" => u32_xor(ctx, a, b) {
            Some(Value::from_raw(((a.bits() as u32) ^ (b.bits() as u32)) as u64))
        }

        "u32not" => u32_not(ctx, a) {
            Some(Value::from_raw(!(a.bits() as u32) as u64))
        }

        "u32=" => u32_eq(ctx, a, b) {
            Some(Value::new((a.bits() as u32) == (b.bits() as u32)))
        }

        "u32>" => u32_gt(ctx, a, b) {
            Some(Value::new((a.bits() as u32) > (b.bits() as u32)))
        }

        "u32<" => u32_lt(ctx, a, b) {
            Some(Value::new((a.bits() as u32) < (b.bits() as u32)))
        }

        "u32>=" => u32_ge(ctx, a, b) {
            Some(Value::new((a.bits() as u32) >= (b.bits() as u32)))
        }

        "u32<=" => u32_le(ctx, a, b) {
            Some(Value::new((a.bits() as u32) <= (b.bits() as u32)))
        }

        "u32->u8" => u32_to_u8(ctx, a) {
            Some(Value::from_raw((a.bits() as u32) as u8 as u64))
        }

        "u32->u16" => u32_to_u16(ctx, a) {
            Some(Value::from_raw((a.bits() as u32) as u16 as u64))
        }

        "u32->u64" => u32_to_u64(ctx, a) {
            Some(Value::from_raw((a.bits() as u32) as u64))
        }

        "u64+" => u64_add(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64).wrapping_add(b.bits() as u64)))
        }

        "u64-" => u64_sub(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64).wrapping_sub(b.bits() as u64)))
        }

        "u64*" => u64_mul(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64).wrapping_mul(b.bits() as u64)))
        }

        "u64/" => u64_div(ctx, a, b) {
            if b.bits() as u64 == 0 {
            return None; // Division by zero
            }
            Some(Value::from_raw((a.bits() as u64).wrapping_div(b.bits() as u64)))
        }

        "u64<<" => u64_shl(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64).wrapping_shl(b.bits() as u32)))
        }

        "u64>>" => u64_shr(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64).wrapping_shr(b.bits() as u32)))
        }

        "u64and" => u64_and(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64) & (b.bits() as u64)))
        }

        "u64or" => u64_or(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64) | (b.bits() as u64)))
        }

        "u64xor" => u64_xor(ctx, a, b) {
            Some(Value::from_raw((a.bits() as u64) ^ (b.bits() as u64)))
        }

        "u64not" => u64_not(ctx, a) {
            Some(Value::from_raw(!a.bits()))
        }

        "u64=" => u64_eq(ctx, a, b) {
            Some(Value::new((a.bits() as u64) == (b.bits() as u64)))
        }

        "u64>" => u64_gt(ctx, a, b) {
            Some(Value::new((a.bits() as u64) > (b.bits() as u64)))
        }

        "u64<" => u64_lt(ctx, a, b) {
            Some(Value::new((a.bits() as u64) < (b.bits() as u64)))
        }

        "u64>=" => u64_ge(ctx, a, b) {
            Some(Value::new((a.bits() as u64) >= (b.bits() as u64)))
        }

        "u64<=" => u64_le(ctx, a, b) {
            Some(Value::new((a.bits() as u64) <= (b.bits() as u64)))
        }

        "u64->u8" => u64_to_u8(ctx, a) {
            Some(Value::from_raw((a.bits() as u64) as u8 as u64))
        }

        "u64->u16" => u64_to_u16(ctx, a) {
            Some(Value::from_raw((a.bits() as u64) as u16 as u64))
        }

        "u64->u32" => u64_to_u32(ctx, a) {
            Some(Value::from_raw((a.bits() as u64) as u32 as u64))
        }

        "u64->u64" => u64_to_u64(ctx, a) {
            Some(Value::from_raw(a.bits() as u64))
        }

        "u32->value" => u32_to_value(ctx, a) {
            Some(Number::from_u32(ctx, a.bits() as u32).into_value(ctx))
        }

        "u64->value" => u64_to_value(ctx, a) {
            Some(Number::from_u64(ctx, a.bits() as u64).into_value(ctx))
        }

        "usize->value" => usize_to_value(ctx, a) {
            Some(Number::from_usize(ctx, a.bits() as usize).into_value(ctx))
        }

    );
}

static FOLDING_TABLE: OnceLock<Global<Rootable!(FoldingTable<'_>)>> = OnceLock::new();

pub fn folding_table<'gc>(ctx: Context<'gc>) -> &'gc FoldingTable<'gc> {
    FOLDING_TABLE
        .get_or_init(|| {
            let table = build_table(ctx);
            Global::new(table)
        })
        .fetch(&ctx)
}

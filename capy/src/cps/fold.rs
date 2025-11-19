//! Constant-folding for CPS terms.
#![allow(dead_code, unused_variables, unused_mut, unused_assignments)]

use crate::expander::primitives::sym_tuple;
use crate::rsgc::Gc;
use crate::rsgc::Global;
use crate::rsgc::Trace;
use crate::rsgc::mmtk::util::Address;
use crate::rsgc::object::GCObject;
use crate::rsgc::ptr::ObjectSlot;
use crate::runtime::Context;
use crate::runtime::value::*;
use crate::runtime::vm::syntax::Syntax;
use crate::{
    cps::term::Atom,
    runtime::value::{Number, Value},
};
use std::collections::HashMap;
use std::sync::OnceLock;

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
                let result = entry.apply(ctx, &args)?;

                return Some(result);
            }
        }

        None
    }
}

unsafe impl<'gc> Trace for FoldingTable<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        unsafe {
            for (val, _) in self.table.iter_mut() {
                let val = val as *const Value<'gc> as *mut Value<'gc>;
                (*val).trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
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

        table
    }};
}

fn build_table<'gc>(ctx: Context<'gc>) -> FoldingTable<'gc> {
    let mut table = folding!(ctx;
        "zero?" => is_zero(ctx, a) {
            let Some(a) = a.number() else { return None };

            Some(Value::new(a.is_zero()))
        }
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

            if b.is_zero() && (b.is_exact() && a.is_exact()) {
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

        /*"tuple-ref" => tuple_ref(ctx, tuple, ix) {
            let Some(tuple) = tuple.try_as::<Tuple>() else {
                return None;
            };

            let Some(ix) = ix.number().filter(|n| n.is_fixnum()).map(|n| n.coerce_exact_integer_to_usize()) else {
                return None;
            };

            if ix >= tuple.len() {
                return None;
            }

            Some(tuple[ix].get())
        }*/

        /*"make-tuple" => make_tuple(ctx, count, init) {
            let Some(count) = count.number().filter(|n| n.is_fixnum()).map(|n| n.coerce_exact_integer_to_usize()) else {
                return None;
            };

            // Do not fold too large tuples
            if count > 32 {
                return None;
            }
            let tup = Tuple::new(ctx, count, init);

            Some(tup.into())
        }

        "tuple-size" => tuple_size(ctx, tuple) {
            let Some(tuple) = tuple.try_as::<Tuple>() else {
                return None;
            };

            Some(Value::new(tuple.len() as i32))
        }*/


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
                if c >= 0 && c <= 0x10FFFF {
                    Some(Value::new(std::char::from_u32(c as u32)?))
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
                Some(Value::new(sym.to_str(*ctx)))
            } else {
                None
            }
        }

        "cons" => cons(ctx, a, b) {
            Some(Value::cons(ctx, a, b))
        }

        "make-syntax" => make_syntax(ctx, exp, wrap, module, source) {
            Some(Syntax::new(
                ctx,
                exp,
                wrap,
                module,
                source,
            ).into())
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
                let slot = ObjectSlot::from_address(addr);
                ctx.mutation().raw_object_reference_write(a.as_cell_raw(), slot, GCObject::NULL);
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

    table.table.insert(
        sym_tuple(ctx).into(),
        FoldingEntry::new(
            |ctx, args| {
                if args.len() > 32 {
                    return None;
                }

                let tup = Tuple::new(*ctx, args.len(), Value::undefined());
                let wtup = Gc::write(*ctx, tup);

                for (i, val) in args.iter().enumerate() {
                    wtup[i].unlock().set(*val);
                }

                Some(tup.into())
            },
            0,
            true,
        ),
    );

    table
}

static FOLDING_TABLE: OnceLock<Global<crate::Rootable!(FoldingTable<'_>)>> = OnceLock::new();

pub fn folding_table<'gc>(ctx: Context<'gc>) -> &'gc FoldingTable<'gc> {
    FOLDING_TABLE
        .get_or_init(|| {
            let table = build_table(ctx);
            Global::new(table)
        })
        .fetch(*ctx)
}

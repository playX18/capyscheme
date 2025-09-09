use super::SSABuilder;
use crate::cps::term::Atom;
use crate::expander::core::LVarRef;
use crate::runtime::Context;
use crate::runtime::value::*;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::IntCC;
use cranelift::prelude::types;
use cranelift_codegen::ir;
use cranelift_codegen::ir::BlockArg;
use easy_bitfield::BitFieldTrait;
use std::collections::HashMap;
use std::mem::offset_of;
macro_rules! prim {
    ($($sname: literal => $name: ident ($ssa: ident, $args: ident, $h: ident) $b: block),*) => {
        pub struct PrimitiveLowerer<'gc> {
            pub map: HashMap<Value<'gc>, for <'a, 'f>fn(&mut SSABuilder<'gc, 'a, 'f>, &[Atom<'gc>], LVarRef<'gc>) -> PrimValue>,
        }


        $(

            pub fn $name<'gc_, 'a, 'f>($ssa: &mut SSABuilder<'gc_, 'a, 'f>, $args: &[Atom<'gc_>], $h: LVarRef<'gc_>) -> PrimValue $b
        )*

        impl<'gc> PrimitiveLowerer<'gc> {
            pub fn new(ctx: Context<'gc>) -> Self {
                let mut map = HashMap::new();
                $(
                    map.insert(Symbol::from_str(ctx, $sname).into(), $name as for <'a, 'f>fn(&mut SSABuilder<'gc, 'a, 'f>, &[Atom<'gc>], LVarRef<'gc>) -> PrimValue);
                )*
                Self { map }
            }
        }

    };
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum PrimValue {
    Value(ir::Value),
    Comparison(ir::Value),
}

prim!(
    "cache-ref" => cache_ref(ssa, args, _h) {
        assert_eq!(args.len(), 1);

        let Atom::Constant(cache_key) = args[0] else {
            panic!("invalid cache-ref")
        };

        let cell = ssa.module_builder.intern_cache_cell(cache_key);
        let cell = ssa.import_data(cell);
        let addr = ssa.builder.ins().global_value(types::I64, cell);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::new(), addr, 0))
    },

    "cache-set!" => cache_set(ssa, args, _h) {
        assert_eq!(args.len(), 2);

        let Atom::Constant(cache_key) = args[0] else {
            panic!("invalid cache-set!")
        };

        let value = ssa.atom(args[1]);

        let cell = ssa.module_builder.intern_cache_cell(cache_key);
        let cell = ssa.import_data(cell);
        let addr = ssa.builder.ins().global_value(types::I64, cell);
        ssa.builder.ins().store(ir::MemFlags::new(), value, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    ".is-immediate" => is_immediate(ssa, args, _h) {
        let arg = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.is_immediate(arg))
    },

    ".is-heap-object" => is_heap_object(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let x = ssa.is_heap_object(arg);


        PrimValue::Comparison(x)
    },

    "box-ref" => box_ref(ssa, args, _h) {
        let arg = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::new(), arg, offset_of!(Boxed, val) as i32))
    },

    "set-box!" => box_set(ssa, args, _h) {
        let arg = ssa.atom(args[0]);
        let val = ssa.atom(args[1]);

        ssa.builder.ins().store(ir::MemFlags::new(), val, arg, offset_of!(Boxed, val) as i32);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "box" => make_box(ssa, args, _h) {
        println!("make-box???");
        let arg = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.make_box, &[ctx, arg]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "lookup" => lookup(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup, &[ctx, module, name], handler))
    },

    "lookup-bound" => lookup_bound(ssa, args, handler) {
        let module = ssa.atom(args[0]);
        let name = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        PrimValue::Value(ssa.handle_thunk_call_result(ssa.thunks.lookup_bound, &[ctx, module, name], handler))
    },

    "current-module" => current_module(ssa, args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        if let Some(module) = args.get(0) {
            let module = ssa.atom(*module);
            let call = ssa.builder.ins().call(ssa.thunks.set_current_module, &[ctx, module]);
            PrimValue::Value(ssa.builder.inst_results(call)[0])
        } else {
            let call = ssa.builder.ins().call(ssa.thunks.current_module, &[ctx]);

            PrimValue::Value(ssa.builder.inst_results(call)[0])
        }
    },

    "define" => define(ssa, args, _h) {
        let key = ssa.atom(args[0]);
        let val = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);

        let call = ssa.builder.ins().call(ssa.thunks.define, &[ctx, key, val]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },


    "not" => not(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let false_ = ssa.builder.ins().iconst(types::I64, Value::new(false).bits() as i64);
        let val = ssa.builder.ins().icmp(IntCC::Equal, val, false_);



        PrimValue::Comparison(val)
    },

    "box" => box_(ssa, args, _h) {
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let box_vt = ssa.import_static("BOX_VTABLE", types::I64);
        let size = ssa.builder.ins().iconst(types::I64, size_of::<Boxed>() as i64);
        let tc8 = ssa.builder.ins().iconst(types::I8, TypeCode8::BOX.bits() as i64);

        let call = ssa.builder.ins().call(
            ssa.thunks.alloc_tc8,
            &[ctx, box_vt, tc8, size]
        );
        let res = ssa.builder.inst_results(call)[0];
        let val = ssa.atom(args[0]);
        ssa.builder.ins().store(ir::MemFlags::trusted(), val, res, offset_of!(Boxed, val) as i32);
        PrimValue::Value(res)
    },

    "box?" => is_box(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_box = ssa.has_typ8(val, TypeCode8::BOX.bits());
        PrimValue::Comparison(is_box)
    },

    "set-car!" => set_car(ssa, args, _h) {
        let pair = ssa.atom(args[0]);
        let new_car = ssa.atom(args[1]);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_car, pair, offset_of!(Pair, car) as i32);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "set-cdr!" => set_cdr(ssa, args, _h) {
        let pair = ssa.atom(args[0]);
        let new_cdr = ssa.atom(args[1]);
        ssa.builder.ins().store(ir::MemFlags::trusted(), new_cdr, pair, offset_of!(Pair, cdr) as i32);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "cons" => cons(ssa, args, _h) {
        let car = ssa.atom(args[0]);
        let cdr = ssa.atom(args[1]);

        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.cons, &[ctx, car, cdr]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "reverse" => reverse(ssa, args, _h) {
        let list = ssa.atom(args[0]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.reverse, &[ctx, list]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "null?" => is_null(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let null = ssa.builder.ins().iconst(types::I64, Value::null().bits() as i64);
        let is_null = ssa.builder.ins().icmp(IntCC::Equal, val, null);
        PrimValue::Comparison(is_null)
    },

    "pair?" => is_pair(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_pair = ssa.has_typ8(val, TypeCode8::PAIR.bits());
        PrimValue::Comparison(is_pair)
    },

    "append" => append(ssa, args, _h) {
        let ls1 = ssa.atom(args[0]);
        let ls2 = ssa.atom(args[1]);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let call = ssa.builder.ins().call(ssa.thunks.append, &[ctx, ls1, ls2]);

        PrimValue::Value(ssa.builder.inst_results(call)[0])
    },

    "list" => list(ssa, args, _h) {
        let args = args.iter().map(|x| ssa.atom(*x)).collect::<Vec<_>>();
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let mut ls = ssa.builder.ins().iconst(types::I64, Value::null().bits() as i64);
        for arg in args.iter().rev().copied() {
            let call = ssa.builder.ins().call(ssa.thunks.cons, &[ctx, arg, ls]);
            ls = ssa.builder.inst_results(call)[0];
        }
        PrimValue::Value(ls)
    },

    "vector?" => is_vector(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let is_vector = ssa.has_typ8(val, TypeCode8::VECTOR.bits());
        PrimValue::Comparison(is_vector)
    },

    "vector" => vector(ssa, args, _h) {
        let size = size_of::<Vector>() as i64 + args.len() as i64 * size_of::<Value>() as i64;
        let size = ssa.builder.ins().iconst(types::I64, size);
        let vt = ssa.import_static("VECTOR_VTABLE", types::I64);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let tc16 = ssa.builder.ins().iconst(types::I16, TypeCode16::MUTABLE_VECTOR.bits() as i64);

        let call = ssa.builder.ins().call(ssa.thunks.alloc_tc16, &[ctx, vt, tc16, size]);
        let vec = ssa.builder.inst_results(call)[0];


        let hdr = ScmHeader::with_type_bits(TypeCode16::MUTABLE_VECTOR.bits()).word | VectorLengthBits::encode(args.len() as _);
        let hdr = ssa.builder.ins().iconst(types::I64, hdr as i64);
        ssa.builder.ins().store(ir::MemFlags::trusted(), hdr, vec, offset_of!(Vector, hdr) as i32);
        for (i, &arg) in args.iter().enumerate() {
            let arg = ssa.atom(arg);
            ssa.builder.ins().store(ir::MemFlags::trusted(), arg, vec, offset_of!(Vector, data) as i32 + i as i32 * size_of::<Value>() as i32);
        }

        PrimValue::Value(vec)
    },

    "vector-ref" => vector_ref(ssa, args, _h) {
        let vec = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);

        PrimValue::Value(ssa.vector_ref(vec, ix))
    },

    "vector-set!" => vector_set(ssa, args, _h) {
        let vec = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);
        let value = ssa.atom(args[2]);

        ssa.vector_set(vec, ix, value);

        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "string?" => is_string(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.has_typ8(val, TypeCode8::STRING.bits()))
    },

    "string-length" => string_length(ssa, args, _h) {
        let str = ssa.atom(args[0]);
        let len = ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), str, offset_of!(Str, length) as i32);

        PrimValue::Value(len)
    },

    "boolean?" => is_boolean(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::BOOL_TAG as i64);
        PrimValue::Comparison(ssa.builder.ins().icmp_imm(IntCC::Equal, mask, Value::BOOL_TAG as i64))
    },

    "symbol?" => is_symbol(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        PrimValue::Comparison(ssa.has_typ8(val, TypeCode8::SYMBOL.bits()))
    },

    "eq?" => is_eq(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        PrimValue::Comparison(ssa.builder.ins().icmp(IntCC::Equal, x, y))
    },

    "eqv?" => is_eqv(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.eqv, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    },

    "equal?" => is_equal(ssa, args, _h) {
        let x = ssa.atom(args[0]);
        let y = ssa.atom(args[1]);
        let call = ssa.builder.ins().call(ssa.thunks.equal, &[x, y]);
        PrimValue::Comparison(ssa.builder.inst_results(call)[0])
    },

    "number?" => is_number(ssa, args, _h) {
        let val = ssa.atom(args[0]);
        let mask = ssa.builder.ins().band_imm(val, Value::NUMBER_TAG as i64);
        let is_inline_num = ssa.builder.ins().icmp_imm(IntCC::NotEqual, mask, 0);
        let succ = ssa.builder.create_block();
        let check_heap = ssa.builder.create_block();

        ssa.builder.append_block_param(succ, types::I8);
        ssa.builder.ins().brif(is_inline_num, succ, &[BlockArg::Value(is_inline_num)], check_heap, &[]);
        ssa.builder.switch_to_block(check_heap);
        {
            let check = ssa.has_typ8(val, TypeCode8::NUMBER.bits());
            ssa.builder.ins().jump(succ, &[BlockArg::Value(check)]);
        }

        ssa.builder.switch_to_block(succ);
        PrimValue::Comparison(ssa.builder.block_params(succ)[0])
    },

    "complex?" => is_complex(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        PrimValue::Comparison(ssa.has_typ16(val, TypeCode16::COMPLEX.bits()))
    },

    "+" => plus(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]))
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_plus(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },

    "-" => minus(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            let val = ssa.atom(args[0]);

            return PrimValue::Value(emit_negate(ssa, val, _h));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_minus(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },

    "*" => times(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        if args.len() == 1 {
            return PrimValue::Value(ssa.atom(args[0]));
        }

        let mut acc = ssa.atom(args[0]);

        for arg in &args[1..] {
            let rhs = ssa.atom(*arg);
            acc = emit_times(ssa, acc, rhs, _h);
        }

        PrimValue::Value(acc)
    },




    "zero?" => is_zero(ssa, args, _h) {
        if args.is_empty() {
            return PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64));
        }

        let val = ssa.atom(args[0]);

        let check_int = ssa.builder.create_block();
        let thunk_call = ssa.builder.create_block();
        let join = ssa.builder.create_block();
        ssa.builder.append_block_param(join, types::I8);
        ssa.branch_if_int32(val, check_int, &[], thunk_call, &[]);
        ssa.builder.switch_to_block(check_int);
        {
            let int32 = ssa.builder.ins().ireduce(types::I32, val);
            let is_zero = ssa.builder.ins().icmp_imm(IntCC::Equal, int32, 0);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_zero)]);
        }

        ssa.builder.switch_to_block(thunk_call);
        {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            let val = ssa.handle_thunk_call_result(ssa.thunks.is_zero, &[ctx, val], _h);
            let is_zero = ssa.builder.ins().icmp_imm(IntCC::Equal, val, Value::new(true).bits() as i64);
            ssa.builder.ins().jump(join, &[BlockArg::Value(is_zero)]);
        }

        ssa.builder.switch_to_block(join);
        let val = ssa.builder.block_params(join)[0];
        PrimValue::Comparison(val)

    },

    "car" => car(ssa, args, _h) {
        let pair = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), pair, offset_of!(Pair, car) as i32))
    },
    "cdr" => cdr(ssa, args, _h) {
        let pair = ssa.atom(args[0]);

        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), pair, offset_of!(Pair, cdr) as i32))
    },

    "tuple" => tuple(ssa, args, _h) {
        let mut hdr = ScmHeader::with_type_bits(TypeCode8::TUPLE.bits() as _);
        hdr.word |= TupleLengthBits::encode(args.len() as _);

        let size = size_of::<Tuple>() as i64 + args.len() as i64 * size_of::<Value>() as i64;

        let size = ssa.builder.ins().iconst(types::I64, size);
        let vt = ssa.import_static("TUPLE_VTABLE", types::I64);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let tc8 = ssa.builder.ins().iconst(types::I8, TypeCode8::TUPLE.bits() as i64);
        let call = ssa.builder.ins().call(ssa.thunks.alloc_tc8, &[ctx, vt, tc8, size]);
        let tuple = ssa.builder.inst_results(call)[0];
        let hdr = ssa.builder.ins().iconst(types::I64, hdr.word as i64);
        ssa.builder.ins().store(ir::MemFlags::trusted(), hdr, tuple, offset_of!(Tuple, hdr) as i32);
        for (i, arg) in args.iter().enumerate() {
            let val = ssa.atom(*arg);
            ssa.builder.ins().store(ir::MemFlags::trusted(), val, tuple, offset_of!(Tuple, data) as i32 + i as i32 * size_of::<Value>() as i32);
        }
        PrimValue::Value(tuple)
    },

    "make-tuple" => make_tuple(ssa, args, _h) {
        let len = ssa.atom(args[0]);
        let len = ssa.builder.ins().ireduce(types::I32, len);

        let hdr = ScmHeader::with_type_bits(TypeCode8::TUPLE.bits() as _);

        let hdr = ssa.builder.ins().iconst(types::I64, hdr.word as i64);
        let size = ssa.builder.ins().imul_imm(len, size_of::<Value>() as i64);
        let size = ssa.builder.ins().iadd_imm(size, size_of::<Tuple>() as i64);
        let size = ssa.builder.ins().uextend(types::I64, size);
        let vt = ssa.import_static("TUPLE_VTABLE", types::I64);
        let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
        let tc8 = ssa.builder.ins().iconst(types::I8, TypeCode8::TUPLE.bits() as i64);
        let call = ssa.builder.ins().call(ssa.thunks.alloc_tc8, &[ctx, vt, tc8, size]);
        let tuple = ssa.builder.inst_results(call)[0];

        let len = ssa.builder.ins().uextend(types::I64, len);
        let len = ssa.builder.ins().band_imm(len, TupleLengthBits::mask() as i64);
        let len = ssa.builder.ins().ishl_imm(len, TupleLengthBits::shift() as i64);
        let hdr = ssa.builder.ins().bor(hdr, len);
        ssa.builder.ins().store(ir::MemFlags::trusted(), hdr, tuple, offset_of!(Tuple, hdr) as i32);
        PrimValue::Value(tuple)
    },

    "tuple-ref" => tuple_ref(ssa, args, _h) {
        let tuple = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);

        let ix = ssa.builder.ins().ireduce(types::I32, ix);
        let offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
        let offset = ssa.builder.ins().iadd_imm(offset, offset_of!(Tuple, data) as i32 as i64);
        let offset = ssa.builder.ins().uextend(types::I64, offset);
        let addr = ssa.builder.ins().iadd(tuple, offset);
        PrimValue::Value(ssa.builder.ins().load(types::I64, ir::MemFlags::trusted().with_can_move(), addr, 0))
    },

    "tuple-set!" => tuple_set(ssa, args, _h) {
        let tuple = ssa.atom(args[0]);
        let ix = ssa.atom(args[1]);
        let value = ssa.atom(args[2]);

        let ix = ssa.builder.ins().ireduce(types::I32, ix);
        let offset = ssa.builder.ins().imul_imm(ix, size_of::<Value>() as i64);
        let offset = ssa.builder.ins().iadd_imm(offset, offset_of!(Tuple, data) as i32 as i64);
        let offset = ssa.builder.ins().uextend(types::I64, offset);
        let addr = ssa.builder.ins().iadd(tuple, offset);
        ssa.builder.ins().store(ir::MemFlags::trusted(), value, addr, 0);
        PrimValue::Value(ssa.builder.ins().iconst(types::I64, Value::undefined().bits() as i64))
    },

    "tuple?" => is_tuple(ssa, args, _h) {
        let val = ssa.atom(args[0]);

        let res = ssa.has_typ8(val, TypeCode8::TUPLE.bits());

        PrimValue::Comparison(res)
    }

);

fn emit_plus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().sadd_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();
            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.plus, &[ctx, lhs, rhs], h)
        },
    )
}

fn emit_minus<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().ssub_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();

            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.minus, &[ctx, lhs, rhs], h)
        },
    )
}

fn emit_negate<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    h: LVarRef<'gc>,
) -> ir::Value {
    ssa.inline_unary_op(
        a,
        |ssa, val| {
            let zero = ssa.builder.ins().iconst(types::I32, 0);
            let (result, ovf) = ssa.builder.ins().ssub_overflow(zero, val);
            let succ = ssa.builder.create_block();

            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, succ, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, val| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.negate, &[ctx, val], h)
        },
    )
}

fn emit_times<'gc, 'a, 'f>(
    ssa: &mut SSABuilder<'gc, 'a, 'f>,
    a: ir::Value,
    b: ir::Value,
    h: LVarRef<'gc>,
) -> ir::Value {
    ssa.inline_binary_op(
        a,
        b,
        |ssa, lhs, rhs, slow| {
            let (result, ovf) = ssa.builder.ins().smul_overflow(lhs, rhs);
            let succ = ssa.builder.create_block();
            ssa.builder.append_block_param(succ, types::I32);
            ssa.builder
                .ins()
                .brif(ovf, slow, &[], succ, &[BlockArg::Value(result)]);
            ssa.builder.switch_to_block(succ);
            ssa.builder.block_params(succ)[0]
        },
        |ssa, lhs, rhs| {
            let ctx = ssa.builder.ins().get_pinned_reg(types::I64);
            ssa.handle_thunk_call_result(ssa.thunks.times, &[ctx, lhs, rhs], h)
        },
    )
}

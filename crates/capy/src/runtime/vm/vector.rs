use crate::heap::Gc;
use crate::{prelude::*, static_symbols};
pub fn init_vectors<'gc>(ctx: MutationContext<'gc>) {
    vector_ops::register(ctx);
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Endianness {
    #[default]
    Little,
    Big,
}

static_symbols!(
    pub SYM_BIG = "big"
    pub SYM_LITTLE = "little"
);

impl<'gc> FromValue<'gc> for Endianness {
    fn try_from_value(
        ctx: MutationContext<'gc>,
        value: Value<'gc>,
    ) -> Result<Self, ConversionError<'gc>> {
        if value == sym_big(ctx).into() {
            Ok(Endianness::Big)
        } else if value == sym_little(ctx).into() {
            Ok(Endianness::Little)
        } else {
            Err(ConversionError::type_mismatch(0, "endianness", value))
        }
    }
}

impl<'gc> IntoValue<'gc> for Endianness {
    fn into_value(self, ctx: MutationContext<'gc>) -> Value<'gc> {
        match self {
            Endianness::Big => sym_big(ctx).into(),
            Endianness::Little => sym_little(ctx).into(),
        }
    }
}

#[scheme(path=capy)]
pub mod vector_ops {

    #[scheme(name = "vector")]
    pub fn vector(values: &'gc [Value<'gc>]) -> Value<'gc> {
        let v = Vector::from_slice(ctx, values);

        v.into()
    }

    #[scheme(name = "make-vector")]
    pub fn make_vector(nelems: usize, init: Option<Value<'gc>>) -> Value<'gc> {
        let v = Vector::new::<false>(ctx, nelems, init.unwrap_or(Value::new(false)));

        v.into()
    }

    #[scheme(name = "vector-length")]
    pub fn vector_length(vec: Gc<'gc, Vector<'gc>>) -> Value<'gc> {
        let len = vec.len().into_value(ctx);
        len
    }
    #[scheme(name = "vector?")]
    pub fn vector_p(value: Value<'gc>) -> bool {
        value.is::<Vector>()
    }

    #[scheme(name = "vector-ref")]
    pub fn vector_ref(vec: Gc<'gc, Vector<'gc>>, k: usize) -> Result<Value<'gc>, Value<'gc>> {
        if k >= vec.len() {
            let k = k.into_value(ctx);
            call.wrong_argument_violation(
                "vector-ref",
                "index out of bounds",
                Some(k),
                Some(1),
                2,
                &[vec.into(), k],
            );
            return Err(Value::undefined());
        }

        let v = vec[k].get();

        Ok(v)
    }

    #[scheme(name = "vector-set!")]
    pub fn vector_set(
        vec: Gc<'gc, Vector<'gc>>,
        k: usize,
        value: Value<'gc>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        if k >= vec.len() {
            let k = k.into_value(ctx);
            call.wrong_argument_violation(
                "vector-set!",
                "index out of bounds",
                Some(k),
                Some(1),
                3,
                &[vec.into(), k, value],
            );
            return Err(Value::undefined());
        }

        let wvec = Gc::write(ctx, vec);
        wvec[k].unlock().set(value);
        Ok(value)
    }

    #[scheme(name = "vector-fill!")]
    pub fn vector_fill(
        vec: Gc<'gc, Vector<'gc>>,
        value: Value<'gc>,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Value<'gc> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(vec.len());

        if start > end {
            let start_value = start.into_value(ctx);
            let end_value = end.into_value(ctx);
            return call.wrong_argument_violation(
                "vector-fill!",
                "start < end",
                Some(Value::cons(ctx, start_value, end_value)),
                Some(1),
                4,
                &[vec.into(), value, start_value, end_value],
            );
        }

        if end > vec.len() {
            let end_value = end.into_value(ctx);
            return call.wrong_argument_violation(
                "vector-fill!",
                "end index out of bounds",
                Some(end_value),
                Some(1),
                4,
                &[vec.into(), value, end_value],
            );
        }

        let wvec = Gc::write(ctx, vec);
        for i in start..end {
            wvec[i].unlock().set(value);
        }
        vec.into()
    }

    #[scheme(name = "vector-copy")]
    pub fn vector_copy(
        vec: Gc<'gc, Vector<'gc>>,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Value<'gc> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(vec.len());
        if start > end || end > vec.len() {
            let start_value = start.into_value(ctx);
            let end_value = end.into_value(ctx);
            return call.wrong_argument_violation(
                "vector-copy",
                "invalid start or end indices",
                Some(Value::cons(ctx, start_value, end_value)),
                Some(1),
                3,
                &[vec.into(), start_value, end_value],
            );
        }
        let slice = &vec.as_slice()[start..end];
        let new_vec = Vector::from_slice(ctx, slice);
        (new_vec).into()
    }

    #[scheme(name = "tuple")]
    pub fn tuple(values: &'gc [Value<'gc>]) -> Value<'gc> {
        let v = Tuple::from_slice(ctx, values);

        v.into()
    }

    #[scheme(name = "make-tuple")]
    pub fn make_tuple(nelems: usize, init: Option<Value<'gc>>) -> Value<'gc> {
        let v = Tuple::new(ctx, nelems, init.unwrap_or(Value::new(false)));

        v.into()
    }

    #[scheme(name = "make-bytevector")]
    pub fn make_bytevector(nelems: usize, init: Option<i16>) -> Value<'gc> {
        let v = ByteVector::new::<false>(ctx, nelems, true);
        if let Some(init) = init {
            if init >= i8::MIN as i16 && init <= u8::MAX as i16 {
                let byte = (init & 0xff) as u8;
                v.fill(byte);
            } else {
                return call.wrong_argument_violation(
                    "make-bytevector",
                    "initial value out of range",
                    Some(init.into_value(ctx)),
                    Some(1),
                    2,
                    &[v.into(), init.into_value(ctx)],
                );
            }
        }
        v.into()
    }

    #[scheme(name = "make-bytevector/nonmoving")]
    pub fn make_bytevector_nonmoving(nelems: usize, init: Option<u8>) -> Value<'gc> {
        let v = ByteVector::new::<false>(ctx, nelems, false);
        if let Some(b) = init {
            v.fill(b);
        }
        v.into()
    }

    #[scheme(name = "bytevector-length")]
    pub fn bytevector_length(bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let len = bv.len().into_value(ctx);
        len
    }

    #[scheme(name = "bytevector->list")]
    pub fn bytevector_to_list(bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let mut lst = Value::null();
        for &b in bv.as_slice().iter().rev() {
            lst = Value::cons(ctx, b.into_value(ctx), lst);
        }
        lst
    }

    #[scheme(name = "bytevector?")]
    pub fn bytevector_p(value: Value<'gc>) -> bool {
        value.is::<ByteVector>()
    }

    #[scheme(name = "bytevector-u8-ref")]
    pub fn bytevector_u8_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        _endian: Option<Endianness>,
    ) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let v = bv[k];

        Ok(v)
    }

    #[scheme(name = "bytevector-ref")]
    pub fn bytevector_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        _endian: Option<Endianness>,
    ) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let v = bv[k];

        Ok(v)
    }
    #[scheme(name = "bytevector-u8-set!")]
    pub fn bytevector_u8_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u8,
        _endian: Option<Endianness>,
    ) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-set!")]
    pub fn bytevector_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u8,
        _endian: Option<Endianness>,
    ) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s8-ref")]
    pub fn bytevector_s8_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        _endian: Option<Endianness>,
    ) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-s8-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let v = bv[k] as i8;

        Ok(v)
    }

    #[scheme(name = "bytevector-s8-set!")]
    pub fn bytevector_s8_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i8,
        _endian: Option<Endianness>,
    ) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-s8-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            bv.as_slice_mut_unchecked()[k] = value as u8;
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u16-ref")]
    pub fn bytevector_u16_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<u16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u16-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = match endian {
            Endianness::Little => u16::from_le_bytes([bytes[0], bytes[1]]),
            Endianness::Big => u16::from_be_bytes([bytes[0], bytes[1]]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-s16-ref")]
    pub fn bytevector_s16_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<i16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s16-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = match endian {
            Endianness::Little => i16::from_le_bytes([bytes[0], bytes[1]]),
            Endianness::Big => i16::from_be_bytes([bytes[0], bytes[1]]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-u16-set!")]
    pub fn bytevector_u16_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u16,
        endian: Option<Endianness>,
    ) -> Result<u16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u16-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s16-set!")]
    pub fn bytevector_s16_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i16,
        endian: Option<Endianness>,
    ) -> Result<i16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s16-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u32-ref")]
    pub fn bytevector_u32_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<u32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u32-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-s32-ref")]
    pub fn bytevector_s32_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<i32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s32-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => i32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-u32-set!")]
    pub fn bytevector_u32_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u32,
        endian: Option<Endianness>,
    ) -> Result<u32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u32-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s32-set!")]
    pub fn bytevector_s32_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i32,
        endian: Option<Endianness>,
    ) -> Result<i32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s32-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u64-ref")]
    pub fn bytevector_u64_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<u64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u64-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => u64::from_le_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
            Endianness::Big => u64::from_be_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-s64-ref")]
    pub fn bytevector_s64_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<i64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s64-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => i64::from_le_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
            Endianness::Big => i64::from_be_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-u64-set!")]
    pub fn bytevector_u64_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u64,
        endian: Option<Endianness>,
    ) -> Result<u64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u64-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s64-set!")]
    pub fn bytevector_s64_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i64,
        endian: Option<Endianness>,
    ) -> Result<i64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s64-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-single-ref")]
    pub fn bytevector_ieee_single_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<f32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-single-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => f32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-single-set!")]
    pub fn bytevector_ieee_single_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: f32,
        endian: Option<Endianness>,
    ) -> Result<f32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-single-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-double-ref")]
    pub fn bytevector_ieee_double_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        endian: Option<Endianness>,
    ) -> Result<f64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-double-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => f64::from_le_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
            Endianness::Big => f64::from_be_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ]),
        };
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-double-set!")]
    pub fn bytevector_ieee_double_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: f64,
        endian: Option<Endianness>,
    ) -> Result<f64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-double-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-single-native-ref")]
    pub fn bytevector_ieee_single_native_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
    ) -> Result<f32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-single-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        if (k + 4) & 0x3 != 0 {
            call.wrong_argument_violation(
                "bytevector-ieee-single-native-ref",
                "unaligned access",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = f32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-single-native-set!")]
    pub fn bytevector_ieee_single_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: f32,
    ) -> Result<f32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-single-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        if (k + 4) & 0x3 != 0 {
            call.wrong_argument_violation(
                "bytevector-ieee-single-native-set!",
                "unaligned access",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-double-native-ref")]
    pub fn bytevector_ieee_double_native_ref(
        bv: Gc<'gc, ByteVector>,
        k: usize,
    ) -> Result<f64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-double-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        if (k + 8) & 0x7 != 0 {
            call.wrong_argument_violation(
                "bytevector-ieee-double-native-ref",
                "unaligned access",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = f64::from_ne_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        Ok(value)
    }

    #[scheme(name = "bytevector-ieee-double-native-set!")]
    pub fn bytevector_ieee_double_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: f64,
    ) -> Result<f64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-ieee-double-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        if (k + 8) & 0x7 != 0 {
            call.wrong_argument_violation(
                "bytevector-ieee-double-native-set!",
                "unaligned access",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u8-native-ref")]
    pub fn bytevector_u8_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let v = bv[k];
        Ok(v)
    }

    #[scheme(name = "bytevector-u8-native-set!")]
    pub fn bytevector_u8_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u8,
    ) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-u8-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s8-native-ref")]
    pub fn bytevector_s8_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-s8-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let v = bv[k] as i8;
        Ok(v)
    }

    #[scheme(name = "bytevector-s8-native-set!")]
    pub fn bytevector_s8_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i8,
    ) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            call.wrong_argument_violation(
                "bytevector-s8-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            bv.as_slice_mut_unchecked()[k] = value as u8;
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u16-native-ref")]
    pub fn bytevector_u16_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<u16, Value<'gc>> {
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u16-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = u16::from_ne_bytes([bytes[0], bytes[1]]);
        Ok(value)
    }

    #[scheme(name = "bytevector-u16-native-set!")]
    pub fn bytevector_u16_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u16,
    ) -> Result<u16, Value<'gc>> {
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u16-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s16-native-ref")]
    pub fn bytevector_s16_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<i16, Value<'gc>> {
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s16-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = i16::from_ne_bytes([bytes[0], bytes[1]]);
        Ok(value)
    }

    #[scheme(name = "bytevector-s16-native-set!")]
    pub fn bytevector_s16_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i16,
    ) -> Result<i16, Value<'gc>> {
        if k + 2 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s16-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u32-native-ref")]
    pub fn bytevector_u32_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<u32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u32-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = u32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        Ok(value)
    }

    #[scheme(name = "bytevector-u32-native-set!")]
    pub fn bytevector_u32_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u32,
    ) -> Result<u32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u32-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s32-native-ref")]
    pub fn bytevector_s32_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<i32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s32-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = i32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        Ok(value)
    }

    #[scheme(name = "bytevector-s32-native-set!")]
    pub fn bytevector_s32_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i32,
    ) -> Result<i32, Value<'gc>> {
        if k + 4 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s32-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-u64-native-ref")]
    pub fn bytevector_u64_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<u64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u64-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = u64::from_ne_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        Ok(value)
    }

    #[scheme(name = "bytevector-u64-native-set!")]
    pub fn bytevector_u64_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: u64,
    ) -> Result<u64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-u64-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector-s64-native-ref")]
    pub fn bytevector_s64_native_ref(bv: Gc<'gc, ByteVector>, k: usize) -> Result<i64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s64-native-ref",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                2,
                &[bv.into(), k.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = i64::from_ne_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]);
        Ok(value)
    }

    #[scheme(name = "bytevector-s64-native-set!")]
    pub fn bytevector_s64_native_set(
        bv: Gc<'gc, ByteVector>,
        k: usize,
        value: i64,
    ) -> Result<i64, Value<'gc>> {
        if k + 8 > bv.len() {
            call.wrong_argument_violation(
                "bytevector-s64-native-set!",
                "index out of bounds",
                Some(k.into_value(ctx)),
                Some(1),
                3,
                &[bv.into(), k.into_value(ctx), value.into_value(ctx)],
            );
            return Err(Value::undefined());
        }

        let bytes = value.to_ne_bytes();
        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
            slice[k + 4] = bytes[4];
            slice[k + 5] = bytes[5];
            slice[k + 6] = bytes[6];
            slice[k + 7] = bytes[7];
        }
        Ok(value)
    }

    #[scheme(name = "bytevector=?")]
    pub fn bytevector_eq_p(bv1: Gc<'gc, ByteVector>, bv2: Gc<'gc, ByteVector>) -> bool {
        bv1.as_slice() == bv2.as_slice()
    }

    #[scheme(name = "bytevector-fill!")]
    pub fn bytevector_fill(bv: Gc<'gc, ByteVector>, value: i16) -> Value<'gc> {
        let byte = if value >= i8::MIN as i16 && value <= u8::MAX as i16 {
            value as u8
        } else {
            let value = value.into_value(ctx);
            return call.wrong_argument_violation(
                "bytevector-fill!",
                "value out of range",
                Some(value),
                Some(1),
                2,
                &[bv.into(), value],
            );
        };
        bv.fill(byte);
        bv.into()
    }

    /// `(r6rs:bytevector-copy! source source-start target target-start count)`
    ///
    /// Overlap-safe (memmove semantics).
    #[scheme(name = "r6rs:bytevector-copy!")]
    pub fn r6rs_bytevector_copy(
        source: Gc<'gc, ByteVector>,
        source_start: usize,
        target: Gc<'gc, ByteVector>,
        target_start: usize,
        count: usize,
    ) -> Value<'gc> {
        let src_ok = source_start
            .checked_add(count)
            .is_some_and(|end| end <= source.len());
        let dst_ok = target_start
            .checked_add(count)
            .is_some_and(|end| end <= target.len());
        if !src_ok || !dst_ok {
            return call.wrong_argument_violation(
                "bytevector-copy!",
                "range out of bounds",
                None,
                None,
                5,
                &[
                    source.into(),
                    source_start.into_value(ctx),
                    target.into(),
                    target_start.into_value(ctx),
                    count.into_value(ctx),
                ],
            );
        }
        if target.is_immutable() {
            return call.wrong_argument_violation(
                "bytevector-copy!",
                "target bytevector is immutable",
                Some(target.into()),
                Some(2),
                3,
                &[target.into()],
            );
        }
        if count != 0 {
            // SAFETY: bounds checked above; ptr::copy handles overlap.
            unsafe {
                let src = source.contents().to_ptr::<u8>().add(source_start);
                let dst = target.contents().to_mut_ptr::<u8>().add(target_start);
                std::ptr::copy(src, dst, count);
            }
        }
        Value::unspecified()
    }

    #[scheme(name = "u8-list->bytevector")]
    pub fn u8_list_to_bytevector(lst: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let mut bytes = Vec::new();
        let mut current = lst;
        if !current.is_list() {
            call.wrong_argument_violation(
                "u8-list->bytevector",
                "not a proper list",
                Some(lst),
                Some(0),
                1,
                &[lst],
            );
            return Err(Value::undefined());
        }

        while current.is_pair() {
            let n = match u8::try_from_value(ctx, current.car()) {
                Ok(n) => n,
                Err(_) => {
                    call.wrong_argument_violation(
                        "u8-list->bytevector",
                        "not a u8",
                        Some(current.car()),
                        Some(0),
                        1,
                        &[lst],
                    );
                    return Err(Value::undefined());
                }
            };
            bytes.push(n);
            current = current.cdr();
        }
        let bv = ByteVector::from_slice(ctx, &bytes, true);
        Ok(bv.into())
    }

    #[scheme(name = "u8-list->bytevector/nonmoving")]
    pub fn u8_list_to_bytevector_nonmoving(lst: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let mut bytes = Vec::new();
        let mut current = lst;
        if !current.is_list() {
            call.wrong_argument_violation(
                "u8-list->bytevector",
                "not a proper list",
                Some(lst),
                Some(0),
                1,
                &[lst],
            );
            return Err(Value::undefined());
        }

        while current.is_pair() {
            let n = match u8::try_from_value(ctx, current.car()) {
                Ok(n) => n,
                Err(_) => {
                    call.wrong_argument_violation(
                        "u8-list->bytevector",
                        "not a u8",
                        Some(current.car()),
                        Some(0),
                        1,
                        &[lst],
                    );
                    return Err(Value::undefined());
                }
            };
            bytes.push(n);
            current = current.cdr();
        }
        let bv = ByteVector::from_slice(ctx, &bytes, false);
        Ok(bv.into())
    }

    #[scheme(name = "bytevector-mapping?")]
    pub fn bytevector_mapping_p(bv: Gc<'gc, ByteVector>) -> bool {
        bv.is_mapping()
    }
}

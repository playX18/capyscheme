use crate::{native_fn, runtime::prelude::*, static_symbols};
use rsgc::Gc;
pub fn init_vectors<'gc>(ctx: Context<'gc>) {
    register_vector_fns(ctx);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
}

impl Default for Endianness {
    fn default() -> Self {
        #[cfg(target_endian = "little")]
        {
            Endianness::Little
        }
        #[cfg(target_endian = "big")]
        {
            Endianness::Big
        }
    }
}

static_symbols!(
    SYM_BIG = "big"
    SYM_LITTLE = "little"
);

impl<'gc> FromValue<'gc> for Endianness {
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
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
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        match self {
            Endianness::Big => sym_big(ctx).into(),
            Endianness::Little => sym_little(ctx).into(),
        }
    }
}

native_fn! {
    register_vector_fns:

    pub ("vector") fn vector<'gc>(nctx, values: &'gc [Value<'gc>]) -> Value<'gc> {
        let v = Vector::from_slice(&nctx.ctx, values);

        nctx.return_(v.into())
    }

    pub ("make-vector") fn make_vector<'gc>(nctx, nelems: usize, init: Option<Value<'gc>>) -> Value<'gc> {
        let v = Vector::new::<false>(&nctx.ctx, nelems, init.unwrap_or(Value::new(false)));

        nctx.return_(v.into())
    }

    pub ("vector-length") fn vector_length<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>) -> Value<'gc> {
        let len = vec.len().into_value(nctx.ctx);
        nctx.return_(len)
    }
    pub ("vector?") fn vector_p<'gc>(nctx, value: Value<'gc>) -> bool {
        nctx.return_(value.is::<Vector>())
    }

    pub ("vector-ref") fn vector_ref<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>, k: usize) -> Result<Value<'gc>, Value<'gc>> {
        if k >= vec.len() {
            let k = k.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("vector-ref", "index out of bounds", Some(k), Some(1), 2, &[vec.into(), k]);
        }

        let v = vec[k].get();

        nctx.return_(Ok(v))
    }

    pub ("vector-set!") fn vector_set<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>, k: usize, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if k >= vec.len() {
            let k = k.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("vector-set!", "index out of bounds", Some(k), Some(1), 3, &[vec.into(), k, value]);
        }

        let wvec = Gc::write(&nctx.ctx, vec);
        wvec[k].unlock().set(value);
        nctx.return_(Ok(value))
    }

    pub ("vector-fill!") fn vector_fill<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>, value: Value<'gc>, start: Option<usize>, end: Option<usize>) -> Value<'gc> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(vec.len());

        if start > end {
            let ctx = nctx.ctx;
            let start_value = start.into_value(ctx);
            let end_value = end.into_value(ctx);
            return nctx.wrong_argument_violation("vector-fill!", "start < end", Some(Value::cons(ctx, start_value, end_value)), Some(1), 4, &[vec.into(), value, start_value, end_value]);
        }

        if end > vec.len() {
            let ctx = nctx.ctx;
            let end_value = end.into_value(ctx);
            return nctx.wrong_argument_violation("vector-fill!", "end index out of bounds", Some(end_value), Some(1), 4, &[vec.into(), value, end_value]);
        }


        let wvec = Gc::write(&nctx.ctx, vec);
        for i in start..end {
            wvec[i].unlock().set(value);
        }
        nctx.return_(vec.into())
    }

    pub ("vector-copy") fn vector_copy<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>, start: Option<usize>, end: Option<usize>) -> Gc<'gc, Vector<'gc>> {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(vec.len());
        if start > end || end > vec.len() {
            let ctx = nctx.ctx;
            let start_value = start.into_value(ctx);
            let end_value = end.into_value(ctx);
            return nctx.wrong_argument_violation("vector-copy", "invalid start or end indices", Some(Value::cons(ctx, start_value, end_value)), Some(1), 3, &[vec.into(), start_value, end_value]);
        }
        let slice = &vec.as_slice()[start..end];
        let new_vec = Vector::from_slice(&nctx.ctx, slice);
        nctx.return_(new_vec)
    }

    pub ("tuple") fn tuple<'gc>(nctx, values: &'gc [Value<'gc>]) -> Value<'gc> {
        let v = Tuple::from_slice(&nctx.ctx, values);

        nctx.return_(v.into())
    }

    pub ("make-tuple") fn make_tuple<'gc>(nctx, nelems: usize, init: Option<Value<'gc>>) -> Value<'gc> {
        let v = Tuple::new(&nctx.ctx, nelems, init.unwrap_or(Value::new(false)));

        nctx.return_(v.into())
    }



    pub ("make-bytevector") fn make_bytevector<'gc>(nctx, nelems: usize, init: Option<i16>) -> Value<'gc> {

        let v = ByteVector::new::<false>(&nctx.ctx, nelems, true);
        if let Some(init) = init {
            if init >= i8::MIN as i16 && init <= u8::MAX as i16 {
                let byte = (init & 0xff) as u8;
                v.fill(byte);
            } else {
                let ctx = nctx.ctx;
                return nctx.wrong_argument_violation("make-bytevector", "initial value out of range", Some(init.into_value(ctx)), Some(1), 2, &[v.into(), init.into_value(ctx)]);
            }
        }
        nctx.return_(v.into())
    }

    pub ("make-bytevector/nonmoving") fn make_bytevector_nonmoving<'gc>(nctx, nelems: usize, init: Option<u8>) -> Value<'gc> {
        let v = ByteVector::new::<false>(&nctx.ctx, nelems, false);
        if let Some(b) = init {
            v.fill(b);
        }
        nctx.return_(v.into())
    }

    pub ("bytevector-length") fn bytevector_length<'gc>(nctx, bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let len = bv.len().into_value(nctx.ctx);
        nctx.return_(len)
    }

    pub ("bytevector->list") fn bytevector_to_list<'gc>(nctx, bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let mut lst = Value::null();
        for &b in bv.as_slice().iter().rev() {
            lst = Value::cons(nctx.ctx, b.into_value(nctx.ctx), lst);
        }
        nctx.return_(lst)
    }


    pub ("bytevector?") fn bytevector_p<'gc>(nctx, value: Value<'gc>) -> bool {
        nctx.return_(value.is::<ByteVector>())
    }

    pub ("bytevector-u8-ref") fn bytevector_u8_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, _endian: Option<Endianness>) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k];

        nctx.return_(Ok(v))
    }

    pub ("bytevector-ref") fn bytevector_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, _endian: Option<Endianness>) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k];

        nctx.return_(Ok(v))
    }
    pub ("bytevector-u8-set!") fn bytevector_u8_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u8, _endian: Option<Endianness>) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-set!") fn bytevector_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u8, _endian: Option<Endianness>) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s8-ref") fn bytevector_s8_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, _endian: Option<Endianness>) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s8-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k] as i8;

        nctx.return_(Ok(v))
    }

    pub ("bytevector-s8-set!") fn bytevector_s8_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i8, _endian: Option<Endianness>) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s8-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value as u8;
        }
        nctx.return_(Ok(value))
    }


    pub ("bytevector-u16-ref") fn bytevector_u16_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<u16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u16-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = match endian {
            Endianness::Little => u16::from_le_bytes([bytes[0], bytes[1]]),
            Endianness::Big => u16::from_be_bytes([bytes[0], bytes[1]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s16-ref") fn bytevector_s16_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<i16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s16-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = match endian {
            Endianness::Little => i16::from_le_bytes([bytes[0], bytes[1]]),
            Endianness::Big => i16::from_be_bytes([bytes[0], bytes[1]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u16-set!") fn bytevector_u16_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u16, endian: Option<Endianness>) -> Result<u16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u16-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s16-set!") fn bytevector_s16_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i16, endian: Option<Endianness>) -> Result<i16, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s16-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u32-ref") fn bytevector_u32_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<u32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u32-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        nctx.return_(Ok(value))
    }


    pub ("bytevector-s32-ref") fn bytevector_s32_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<i32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s32-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => i32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u32-set!") fn bytevector_u32_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u32, endian: Option<Endianness>) -> Result<u32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u32-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s32-set!") fn bytevector_s32_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i32, endian: Option<Endianness>) -> Result<i32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s32-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u64-ref") fn bytevector_u64_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<u64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u64-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
            Endianness::Big => u64::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s64-ref") fn bytevector_s64_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<i64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s64-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => i64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
            Endianness::Big => i64::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u64-set!") fn bytevector_u64_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u64, endian: Option<Endianness>) -> Result<u64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u64-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
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
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s64-set!") fn bytevector_s64_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i64, endian: Option<Endianness>) -> Result<i64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s64-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
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
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-single-ref") fn bytevector_ieee_single_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<f32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-single-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = match endian {
            Endianness::Little => f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => f32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-single-set!") fn bytevector_ieee_single_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: f32, endian: Option<Endianness>) -> Result<f32, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-single-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-double-ref") fn bytevector_ieee_double_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, endian: Option<Endianness>) -> Result<f64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-double-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = match endian {
            Endianness::Little => f64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
            Endianness::Big => f64::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]),
        };
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-double-set!") fn bytevector_ieee_double_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: f64, endian: Option<Endianness>) -> Result<f64, Value<'gc>> {
        let endian = endian.unwrap_or_default();
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-double-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = match endian {
            Endianness::Little => value.to_le_bytes(),
            Endianness::Big => value.to_be_bytes(),
        };
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
        nctx.return_(Ok(value))
    }



    pub ("bytevector-ieee-single-native-ref") fn bytevector_ieee_single_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<f32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-single-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = f32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-single-native-set!") fn bytevector_ieee_single_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: f32) -> Result<f32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-single-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-double-native-ref") fn bytevector_ieee_double_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<f64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-double-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = f64::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-ieee-double-native-set!") fn bytevector_ieee_double_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: f64) -> Result<f64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-ieee-double-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
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
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u8-native-ref") fn bytevector_u8_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k];
        nctx.return_(Ok(v))
    }

    pub ("bytevector-u8-native-set!") fn bytevector_u8_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u8) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s8-native-ref") fn bytevector_s8_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s8-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k] as i8;
        nctx.return_(Ok(v))
    }

    pub ("bytevector-s8-native-set!") fn bytevector_s8_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i8) -> Result<i8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s8-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value as u8;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u16-native-ref") fn bytevector_u16_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u16, Value<'gc>> {
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u16-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = u16::from_ne_bytes([bytes[0], bytes[1]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u16-native-set!") fn bytevector_u16_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u16) -> Result<u16, Value<'gc>> {
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u16-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s16-native-ref") fn bytevector_s16_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<i16, Value<'gc>> {
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s16-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 2];
        let value = i16::from_ne_bytes([bytes[0], bytes[1]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s16-native-set!") fn bytevector_s16_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i16) -> Result<i16, Value<'gc>> {
        if k + 2 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s16-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u32-native-ref") fn bytevector_u32_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u32-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = u32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u32-native-set!") fn bytevector_u32_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u32) -> Result<u32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u32-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s32-native-ref") fn bytevector_s32_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<i32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s32-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 4];
        let value = i32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s32-native-set!") fn bytevector_s32_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i32) -> Result<i32, Value<'gc>> {
        if k + 4 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s32-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
        unsafe {
            let slice = bv.as_slice_mut_unchecked();
            slice[k] = bytes[0];
            slice[k + 1] = bytes[1];
            slice[k + 2] = bytes[2];
            slice[k + 3] = bytes[3];
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u64-native-ref") fn bytevector_u64_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u64-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = u64::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-u64-native-set!") fn bytevector_u64_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u64) -> Result<u64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u64-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
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
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s64-native-ref") fn bytevector_s64_native_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<i64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s64-native-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let bytes = &bv.as_slice()[k..k + 8];
        let value = i64::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]);
        nctx.return_(Ok(value))
    }

    pub ("bytevector-s64-native-set!") fn bytevector_s64_native_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: i64) -> Result<i64, Value<'gc>> {
        if k + 8 > bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-s64-native-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        let bytes = value.to_ne_bytes();
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
        nctx.return_(Ok(value))
    }


    pub ("bytevector=?") fn bytevector_eq_p<'gc>(nctx, bv1: Gc<'gc, ByteVector>, bv2: Gc<'gc, ByteVector>) -> bool {
        nctx.return_(bv1.as_slice() == bv2.as_slice())
    }

    pub ("bytevector-fill!") fn bytevector_fill<'gc>(nctx, bv: Gc<'gc, ByteVector>, value: i16) -> Value<'gc> {
        let byte = if value >= i8::MIN as i16 && value <= u8::MAX as i16 {
            value as u8
        } else {
            let value = value.into_value(nctx.ctx);
            return nctx.wrong_argument_violation("bytevector-fill!", "value out of range", Some(value), Some(1), 2, &[bv.into(), value]);
        };
        bv.fill(byte);
        nctx.return_(bv.into())
    }

    pub ("u8-list->bytevector") fn u8_list_to_bytevector<'gc>(nctx, lst: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let mut bytes = Vec::new();
        let mut current = lst;
        let ctx = nctx.ctx;
        if !current.is_list() {

            return nctx.wrong_argument_violation("u8-list->bytevector", "not a proper list", Some(lst), Some(0), 1, &[lst]);
        }

        while current.is_pair() {
            let n = match u8::try_from_value(ctx, current.car()) {
                Ok(n) => n,
                Err(_) => {
                    return nctx.wrong_argument_violation("u8-list->bytevector", "not a u8", Some(current.car()), Some(0), 1, &[lst]);
                }
            };
            bytes.push(n);
            current = current.cdr();
        }
        let bv = ByteVector::from_slice(&nctx.ctx, &bytes, true);
        nctx.return_(Ok(bv.into()))
    }

    pub ("u8-list->bytevector/nonmoving") fn u8_list_to_bytevector_nonmoving<'gc>(nctx, lst: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        let mut bytes = Vec::new();
        let mut current = lst;
        let ctx = nctx.ctx;
        if !current.is_list() {

            return nctx.wrong_argument_violation("u8-list->bytevector", "not a proper list", Some(lst), Some(0), 1, &[lst]);
        }

        while current.is_pair() {
            let n = match u8::try_from_value(ctx, current.car()) {
                Ok(n) => n,
                Err(_) => {
                    return nctx.wrong_argument_violation("u8-list->bytevector", "not a u8", Some(current.car()), Some(0), 1, &[lst]);
                }
            };
            bytes.push(n);
            current = current.cdr();
        }
        let bv = ByteVector::from_slice(&nctx.ctx, &bytes, false);
        nctx.return_(Ok(bv.into()))
    }

    pub ("bytevector-mapping?") fn bytevector_mapping_p<'gc>(nctx, bv: Gc<'gc, ByteVector>) -> bool {
        nctx.return_(bv.is_mapping())
    }

}

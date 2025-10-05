use crate::{native_fn, runtime::prelude::*};
use rsgc::Gc;
pub fn init_vectors<'gc>(ctx: Context<'gc>) {
    register_vector_fns(ctx);
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
            todo!()
        }

        let v = vec[k].get();

        nctx.return_(Ok(v))
    }

    pub ("vector-set!") fn vector_set<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>, k: usize, value: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if k >= vec.len() {
            todo!()
        }

        let wvec = Gc::write(&nctx.ctx, vec);
        wvec[k].unlock().set(value);
        nctx.return_(Ok(value))
    }

    pub ("tuple") fn tuple<'gc>(nctx, values: &'gc [Value<'gc>]) -> Value<'gc> {
        let v = Tuple::from_slice(&nctx.ctx, values);

        nctx.return_(v.into())
    }

    pub ("make-tuple") fn make_tuple<'gc>(nctx, nelems: usize, init: Option<Value<'gc>>) -> Value<'gc> {
        let v = Tuple::new(&nctx.ctx, nelems, init.unwrap_or(Value::new(false)));

        nctx.return_(v.into())
    }



    pub ("make-bytevector") fn make_bytevector<'gc>(nctx, nelems: usize, init: Option<u8>) -> Value<'gc> {
        let v = ByteVector::new::<false>(&nctx.ctx, nelems);
        if let Some(b) = init {
            v.fill(b);
        }
        nctx.return_(v.into())
    }

    pub ("bytevector-length") fn bytevector_length<'gc>(nctx, bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let len = bv.len().into_value(nctx.ctx);
        nctx.return_(len)
    }



    pub ("bytevector?") fn bytevector_p<'gc>(nctx, value: Value<'gc>) -> bool {
        nctx.return_(value.is::<ByteVector>())
    }

    pub ("bytevector-u8-ref") fn bytevector_u8_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k];

        nctx.return_(Ok(v))
    }

    pub ("bytevector-ref") fn bytevector_ref<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-ref", "index out of bounds", Some(k.into_value(ctx)), Some(1), 2, &[bv.into(), k.into_value(ctx)]);
        }

        let v = bv[k];

        nctx.return_(Ok(v))
    }
    pub ("bytevector-u8-set!") fn bytevector_u8_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u8) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector-set!") fn bytevector_set<'gc>(nctx, bv: Gc<'gc, ByteVector>, k: usize, value: u8) -> Result<u8, Value<'gc>> {
        if k >= bv.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation("bytevector-u8-set!", "index out of bounds", Some(k.into_value(ctx)), Some(1), 3, &[bv.into(), k.into_value(ctx), value.into_value(ctx)]);
        }

        unsafe {
            bv.as_slice_mut_unchecked()[k] = value;
        }
        nctx.return_(Ok(value))
    }

    pub ("bytevector=?") fn bytevector_eq_p<'gc>(nctx, bv1: Gc<'gc, ByteVector>, bv2: Gc<'gc, ByteVector>) -> bool {
        nctx.return_(bv1.as_slice() == bv2.as_slice())
    }

    pub ("bytevector-fill!") fn bytevector_fill<'gc>(nctx, bv: Gc<'gc, ByteVector>, value: u8) -> Value<'gc> {
        bv.fill(value);
        nctx.return_(bv.into())
    }



}

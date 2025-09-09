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

    pub ("make-vector") fn make_vector<'gc>(nctx, nelems: usize, init: Value<'gc>) -> Value<'gc> {
        let v = Vector::new::<false>(&nctx.ctx, nelems, init);

        nctx.return_(v.into())
    }

    pub ("vector-length") fn vector_length<'gc>(nctx, vec: Gc<'gc, Vector<'gc>>) -> Value<'gc> {
        let len = vec.len().into_value(nctx.ctx);
        nctx.return_(len)
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
}

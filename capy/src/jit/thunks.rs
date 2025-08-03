use rsgc::{Gc, Mutation, mmtk::util::Address};

use crate::runtime::{
    Context, State,
    value::{Closure, ScmHeader, TypeCode16, Value, Vector},
};

#[unsafe(no_mangle)]
pub extern "C-unwind" fn alloc_closure<'gc>(
    mc: &'gc Mutation<'gc>,
    state: &'gc State<'gc>,
    nfree: usize,
    meta: Value<'gc>,
) -> Value<'gc> {
    let ctx = Context::from((mc, state));

    let free = if nfree == 0 {
        Value::null()
    } else {
        Vector::new::<false>(&ctx, nfree, Value::undefined()).into()
    };

    Value::new(Gc::new(
        &ctx,
        Closure {
            header: ScmHeader::with_type_bits(TypeCode16::CLOSURE_PROC.bits() as _),
            free,
            code: Address::ZERO,
            meta,
        },
    ))
}

#[unsafe(no_mangle)]
pub extern "C-unwind" fn alloc_closure_k<'gc>(
    mc: &'gc Mutation<'gc>,
    state: &'gc State<'gc>,
    nfree: usize,
    meta: Value<'gc>,
) -> Value<'gc> {
    let ctx = Context::from((mc, state));

    let free = if nfree == 0 {
        Value::null()
    } else {
        Vector::new::<false>(&ctx, nfree, Value::undefined()).into()
    };

    Value::new(Gc::new(
        &ctx,
        Closure {
            header: ScmHeader::with_type_bits(TypeCode16::CLOSURE_K.bits() as _),
            free,
            code: Address::ZERO,
            meta,
        },
    ))
}

#[unsafe(no_mangle)]
pub extern "C-unwind" fn cons_rest<'gc>(
    mc: &'gc Mutation<'gc>,
    state: &'gc State<'gc>,
    from: usize,
) -> Value<'gc> {
    let ctx = Context::from((mc, state));

    let mut ls = Value::null();

    for arg in state.vm_state.arguments()[from..].iter().rev() {
        ls = Value::cons(ctx, *arg, ls);
    }

    ls
}

use crate::runtime::{
    Context,
    value::{Str, Value},
};

pub fn mangle_library_spec<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> Value<'gc> {
    let mut spec = spec;

    let mut result = String::new();

    while !spec.is_null() {
        let next = spec.cdr();
        result.push_str(&spec.car().to_string());
        if next.is_null() {
            result.push(':');
        } else {
            result.push('.');
        }
        spec = next;
    }

    Str::new(&ctx, result, true).into()
}

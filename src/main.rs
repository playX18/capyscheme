use capyscheme::runtime::value::values::Values;
use capyscheme::runtime::value::Value;
use capyscheme::runtime::{Context, Scheme};
use capyscheme::runtime::value::conversions::*;


fn foo<'gc>(_: Context<'gc>, x: i32, y: i32) -> (i32, bool) {
    match x.checked_add(y) {
        Some(sum) => (sum, false),
        None => (x.wrapping_add(y), true),
    }
}

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let f = foo.into_primitive();

        let result = f(ctx, &[Value::new(i32::MAX), Value::new(1)]).unwrap();
        let (x, y): (i32, bool) = <_>::from_values(ctx, result.downcast::<Values>().iter().copied()).unwrap();
        println!("result: {}, carry: {}", x, y);
    })
}
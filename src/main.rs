use capyscheme::runtime::{value::*, Context, Scheme};


fn main() {
    let scm = Scheme::new();

    let x = vec![9; 32];

    scm.enter(|ctx| {
        let mut x = Number::Fixnum(100);

        for i in 0..1000 {
            x = Number::mul(ctx, x, (i+1*42).into_number(ctx));
        }
        let divided = Number::div(ctx, x, (1000*42).into_number(ctx));
        println!("x: {}", x);
        println!("divided: {}", divided);   

    
    });
}

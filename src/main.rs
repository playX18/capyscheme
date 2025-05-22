use capyscheme::runtime::{value::*, Context, Scheme};


fn main() {
    let scm = Scheme::new();

    let x = vec![9; 32];

    scm.enter(|ctx| {
        let x = Number::Fixnum(67);
        let y = Number::Fixnum(8);

        let z = Number::div(ctx, x, y);
        println!("z={}, floor(z)={}", z, Number::floor(ctx, z));

        println!("{}", 67.0 / 8.0);

        println!("{}", Number::Flonum(8.375).to_exact(ctx));
    
    });
}

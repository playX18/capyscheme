use capyscheme::runtime::{Scheme, value::*};

fn main() {
    let scm = Scheme::new();

    let x = vec![9; 32];

    scm.enter(|ctx| {
        let big = BigInt::parse(ctx, "1234567890123456789012345678901234567890", &Base::DEC).unwrap();
        let big2 = BigInt::parse(ctx, "42", &Base::DEC).unwrap();
        println!("{}", big.to_string_with_options(&Default::default())) ;
        let big3 = big.times(ctx, big2);
        
        println!("{}", big3.to_string_with_options(&Default::default()));
        println!("{:+#}", &*big3);
    });
}

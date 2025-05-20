use capyscheme::runtime::{Scheme, value::*};

fn main() {
    let scm = Scheme::new();

    let x = vec![9; 32];

    scm.enter(|ctx| {
        let big = BigInt::from_string(ctx, &(u64::MAX - 1).to_string(), &BigIntBase::DEC).unwrap();
        let big2 = BigInt::from_u64(ctx, u64::MAX);

        println!(
            "{} {}",
            big.to_string(&BigIntBase::DEC),
            big2.to_string(&BigIntBase::DEC)
        );
    });
}

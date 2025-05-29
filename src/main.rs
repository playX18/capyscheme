use capyscheme::runtime::{Scheme, value::Value, value::port::Port};

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let port = Port::new(ctx);
        let file = std::fs::File::open("README.md").unwrap();
        port.set_device(
            Box::new(file),
            Value::new(false),
            Value::new(true),
        );

        for _ in 0..4 {
            println!("{:?}", port.lookahead_byte());
        }
    });
}

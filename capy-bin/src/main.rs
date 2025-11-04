use capy::{
    prelude::*,
    runtime::{Scheme, vm::base::program_arguments_fluid},
};

fn main() {
    let image = std::fs::read("capy.heap").unwrap();

    let scm = Scheme::from_image(&image);

    scm.call_value(
        |ctx, fargs| {
            let args = std::env::args().rev().fold(Value::null(), |acc, arg| {
                Value::cons(*ctx, Str::new(&ctx, arg, true).into(), acc)
            });

            program_arguments_fluid(*ctx).set(*ctx, args);
            fargs.push(args);

            ctx.accumulator()
        },
        |_ctx, res| match res {
            Ok(val) => {
                println!("Result: {}", val);
            }

            Err(err) => {
                eprintln!("Error: {}", err);
            }
        },
    );
}

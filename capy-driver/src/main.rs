use capy::runtime::{
    Scheme,
    modules::{current_module, root_module},
    vm::{VMResult, call_scheme, debug::print_stacktraces_impl, load::load_thunk_in_vicinity},
};

//#[global_allocator]
//static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    env_logger::init();
    let scm = Scheme::new();

    let mut did_yield = scm.enter(|ctx| {
        current_module(ctx).set(ctx, (*root_module(ctx)).into());
        let start = std::time::Instant::now();
        let thunk = load_thunk_in_vicinity(ctx, "boot/main.scm", None::<&str>)
            .expect("Failed to load boot/main.scm");
        println!("load thunk in {}ms", start.elapsed().as_millis());
        let start = std::time::Instant::now();
        let result = call_scheme(ctx, thunk, []);
        let duration = start.elapsed();
        println!(
            "Execution time: {:.4}ms",
            duration.as_micros() as f64 / 1000.0
        );

        match result {
            VMResult::Ok(val) => {
                println!("Program finished with value: {}", val);
            }
            VMResult::Err(err) => {
                //print_stacktraces_impl(ctx);
                println!("Program terminated with error: {}", err);
            }
            VMResult::Yield => {
                println!("Program yielded");
                return true;
            }
        }

        false
    });

    while did_yield {
        did_yield = scm.enter(|ctx| {
            if ctx.has_suspended_call() {
                match ctx.resume_suspended_call() {
                    VMResult::Ok(val) => {
                        println!("Resumed program finished with value: {}", val);
                        false
                    }
                    VMResult::Err(err) => {
                        println!("Resumed program terminated with error: {}", err);
                        false
                    }
                    VMResult::Yield => {
                        println!("Resumed program yielded");
                        true
                    }
                }
            } else {
                false
            }
        })
    }
}

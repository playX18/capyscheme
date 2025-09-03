use capy::runtime::{
    Scheme,
    modules::{current_module, root_module},
    vm::{VMResult, call_scheme, load::load_in_vicinity},
};

//#[global_allocator]
//static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    env_logger::init();
    let scm = Scheme::new();

    let mut did_yield = scm.enter(|ctx| {
        current_module(ctx).set(ctx, (*root_module(ctx)).into());

        let cwd = std::env::current_dir().unwrap();
        let file = load_in_vicinity(ctx, cwd, "boot/main.scm");

        match file {
            Ok(thunk) => {
                let result = call_scheme(ctx, thunk, &[]);

                match result {
                    VMResult::Ok(val) => {
                        println!("Program finished with value: {}", val);
                    }
                    VMResult::Err(err) => {
                        println!("Program terminated with error: {}", err);
                    }
                    VMResult::Yield => {
                        println!("Program yielded");
                        return true;
                    }
                }
            }
            Err(err) => {
                println!("Error loading file: {}", err);
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

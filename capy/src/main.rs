use capy::{
    expander::{compile_program, read_from_string},
    runtime::Scheme,
};
use pretty::BoxAllocator;

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let src = std::fs::read_to_string("main.scm").unwrap();
        let program = read_from_string(ctx, src, "main.scm").unwrap();

        let cps = compile_program(ctx, program).unwrap();

        let doc = cps.pretty::<_, &BoxAllocator>(&BoxAllocator);
        let stdout = std::io::stdout();
        doc.1.render(70, &mut stdout.lock()).unwrap();
        println!();
    });
}

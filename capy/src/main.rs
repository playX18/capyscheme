use capy::expander::core::{Cenv, expand, seq};
use capy::frontend::reader::TreeSitter;
use capy::runtime::{Scheme, value::*};
use pretty::BoxAllocator;
use rsgc::GarbageCollector;
use rsgc::alloc::Array;
use std::io::Write;

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let code = std::fs::read_to_string("r7expander.scm").unwrap();
        let bytes = rsgc::mmtk::memory_manager::used_bytes(&GarbageCollector::get().mmtk);
        println!(
            "MMTk used bytes: {:.2}MB",
            bytes as f64 / (1024 * 1024) as f64
        );
        let start = std::time::Instant::now();
        let parser = TreeSitter::new(ctx, &code, Value::new(false));

        let program = parser.read_program().unwrap();

        println!(
            "Parsed program in {:.2}ms",
            start.elapsed().as_micros() as f64 / 1000.0
        );

        let mut terms = Vec::new();
        let mut cenv = Cenv::toplevel(ctx);

        let start = std::time::Instant::now();

        for expr in program {
            let term = expand(&mut cenv, expr).unwrap();
            terms.push(term);
        }

        println!(
            "Expanded program in {:.2}ms",
            start.elapsed().as_micros() as f64 / 1000.0
        );

        let seq = seq(ctx, Array::from_array(&ctx, &terms));

        let start = std::time::Instant::now();
        let opt = capy::expander::core::optimize(ctx, seq);
        println!(
            "Optimized program in {:.2}ms",
            start.elapsed().as_micros() as f64 / 1000.0
        );

        let start = std::time::Instant::now();

        let cps = capy::expander::compile_cps::cps_toplevel(ctx, &[opt]);

        let cps = capy::cps::rewrite_func(ctx, cps);

        let _rv = capy::cps::reify(cps);

        println!(
            "CPS conversion in {:.2}ms",
            start.elapsed().as_micros() as f64 / 1000.0
        );

        let doc = cps.pretty::<_, ()>(&BoxAllocator);

        let file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("cps.scm")
            .unwrap();
        let mut writer = std::io::BufWriter::new(file);
        doc.1.render(70, &mut writer).unwrap();
        writer.flush().unwrap();
    });

    scm.mutator.collect_garbage();
}

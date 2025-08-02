use capyc::{
    ast::collect_errors_and_report,
    cps::{self},
    expand::{Cenv, pass1},
    il::{self, term::IForm},
    parser::TreeSitter,
    quote,
    source::*,
};
use clap::Parser;
use pretty::BoxAllocator;
use std::path::PathBuf;

#[derive(clap::Parser, Debug)]
pub struct CommandArgs {
    #[arg(
        long = "program",
        short,
        default_value_t = true,
        help = "Compile file as a program (default: true)"
    )]
    pub program: bool,

    pub file: PathBuf,

    #[arg(long = "path", help = "Paths to search for libraries")]
    pub path: Vec<PathBuf>,
    #[arg(long = "feature", help = "Features to enable")]
    pub features: Vec<String>,
}

macro_rules! simple_match_pat {
    ($v: expr, (), $kt: expr, $kf: expr) => {
        if matches!($v.value(), DatumValue::Null) {
            $kt
        } else {
            $kf
        }
    };

    ($v: expr, true, $kt: expr, $kf: expr) => {
        if matches!($v.value(), DatumValue::True) {
            $kt
        } else {
            $kf
        }
    };

    ($v: expr, false, $kt: expr, $kf: expr) => {
        if matches!($v.value(), DatumValue::False) {
            $kt
        } else {
            $kf
        }
    };

    ($v: expr, $lit: literal, $kt: expr, $kf: expr) => {
        if $v.value() == Datum::from($lit).value() {
            $kt
        } else {
            $kf
        }
    };

    ($v: expr, (quote $lit: literal), $kt: expr, $kf: expr) => {
        if matches!($v.value(), DatumValue::Quote(l) if l == $lit) {
            $kt
        } else {
            $kf
        }
    };

    ($v: expr, (unquote $x: tt), $kt: expr, $kf: expr) => {
        simple_match_pat!($v, $x, $kf, $kt)
    };

    ($v: expr, (and), $kt: expr, $kf: expr) => {
        $kt
    };

    ($v: expr, (and $x: tt $($rest:tt)*), $kt: expr, $kf: expr) => {
        simple_match_pat!($v, $x, simple_match_pat!($v, (and $($rest)*), $kt, $kf), $kf)
    };

    ($v: expr, (or), $kt: expr, $kf: expr) => {
        $kf
    };

    ($v: expr, (or $x: tt $($rest:tt)*), $kt: expr, $kf: expr) => {
        let tk = || $kt;

        simple_match_pat!($v, $x, tk(), simple_match_pat!($v, (or $($rest)*), tk(), $kf))
    };

    ($v: expr, (not $x: tt), $kt: expr, $kf: expr) => {
        simple_match_pat!($v, $x, $kf, $kt)
    };

    ($v: expr, ($x: tt $($rest:tt)*), $kt: expr, $kf: expr) => {
        if let Some((vx, vy)) = $v.try_pair() {
            simple_match_pat!(vx, $x, simple_match_pat!(vy, ($($rest)*), $kt, $kf), $kf)
        } else {
            $kf
        }
    };

    ($v: expr, (. $x: tt), $kt: expr, $kf: expr) => {
        simple_match_pat!($v, $x, $kf, $kt)
    };

    ($v: expr, (quote $lit: expr), $kt: expr, $kf: expr) => {
        todo!()
    };


}
use capyc::ast::*;
fn main() {

    /*
    let args = CommandArgs::parse();

    println!("file: {}", args.file.display());
    println!("path: {:?}", args.path);
    println!("features: {:?}", args.features);
    let mut source_manager = SourceManager::new();

    let src = match source_manager.open_file(&args.file) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            std::process::exit(1);
        }
    };

    let ts = TreeSitter::new(src, &source_manager);
    let _root_span = ts.root_span();
    let program = ts.parse_program();
    let mut errors = Vec::new();
    collect_errors_and_report(program.iter(), &mut errors);
    if !errors.is_empty() {
        for error in errors {
            error.eprint(&source_manager).unwrap();
        }
        std::process::exit(1);
    }

    let mut cenv = Cenv::new();

    let mut seq = vec![];

    for expr in program.iter() {
        let iform = match pass1(&expr, &mut cenv) {
            Ok(form) => form,
            Err(error) => {
                let mut output = Vec::new();
                error.build_reports(&mut output);

                for x in output {
                    x.eprint(&source_manager).unwrap();
                }
                std::process::exit(1);
            }
        };

        seq.push(iform);
    }

    let seq = IForm {
        span: Default::default(),
        term: il::term::ITerm::Seq(seq),
    };

    seq.pretty::<_, ()>(&BoxAllocator)
        .1
        .render(70, &mut std::io::stdout())
        .unwrap();
    println!();
    let form = il::optimize(std::rc::Rc::new(seq));

    let code = il::to_cps(form);
    let body = &code.body;

    let optimized = cps::optimize(body.clone());
    optimized
        .pretty::<_, ()>(&BoxAllocator)
        .1
        .render(70, &mut std::io::stdout())
        .unwrap();
    println!();*/
}

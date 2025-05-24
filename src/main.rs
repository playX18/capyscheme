use std::io::{BufRead, BufReader, Cursor};

use capyscheme::{
    parser::*,
    runtime::{Context, Scheme, value::*},
};

fn main() {
    let scm = Scheme::new();

    let x = vec![9; 32];

    scm.enter(|ctx| {
        let mut source = "hello";
        let reader = Cursor::new(source.as_bytes());
        let mut reader = Reader::new(ctx, BufReader::new(reader));

        println!("{}", reader.lookahead_char().unwrap().unwrap());
    });
}

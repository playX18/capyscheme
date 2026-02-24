use capy_bytecode::*;

fn main() {
    let mut buf = Vec::new();

    OpLdcI4 { value: 42 }.encode(&mut buf).unwrap();
    OpLdarg1 {}.encode(&mut buf).unwrap();
    OpAdd {}.encode(&mut buf).unwrap();
    OpLdarg0 {}.encode(&mut buf).unwrap();
    OpReturnCall { argc: 1 }.encode(&mut buf).unwrap();

    let mut cursor = std::io::Cursor::new(buf);
    let mut dump = StdoutDumper::new();
    while cursor.position() < cursor.get_ref().len() as u64 {
        let loc = cursor.position() as usize;
        Instruction::decode(&mut cursor)
            .unwrap()
            .dump(&mut dump, loc)
            .unwrap();
    }
}

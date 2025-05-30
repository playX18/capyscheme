use std::net::TcpStream;

use capyscheme::runtime::{
    Scheme,
    value::port::{Port, PortDirection},
};

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let port = Port::new(ctx);

        let socket = TcpStream::connect("127.0.0.1:8080").unwrap();

        port.set_device(Box::new(socket), false.into(), true.into());

        let mut buf = [0; 1024];
        port.puts(ctx, "Hello, World!").unwrap();
        let n = port.get_bytes(ctx, &mut buf).unwrap();
        println!("Received {} bytes: {:?}", n, &buf[..n]);
    });
}

use parking_lot::Mutex;
use rsgc::{EnsureGCInfo, Gc, Trace, context::Mutation, finalizer::FinalizerQueue};
use std::{
    fs,
    io::{self, Read, SeekFrom, Write},
    net::TcpStream,
    sync::{Arc, LazyLock},
};

use crate::runtime::{
    Context,
    vmthread::{VM_THREAD, VMThreadTask},
};

use super::*;

pub trait SeekMarkExt {
    fn offset(&self, offset: i64) -> SeekFrom;
    fn get_offset(&self) -> i64;
}

impl SeekMarkExt for SeekFrom {
    fn offset(&self, offset: i64) -> SeekFrom {
        match self {
            SeekFrom::Start(pos) => SeekFrom::Start((*pos as i64 + offset) as u64),
            SeekFrom::End(pos) => SeekFrom::End(pos + offset),
            SeekFrom::Current(pos) => SeekFrom::Current(pos + offset),
        }
    }

    fn get_offset(&self) -> i64 {
        match self {
            SeekFrom::Start(pos) => *pos as i64,
            SeekFrom::End(pos) => *pos,
            SeekFrom::Current(pos) => *pos,
        }
    }
}

pub trait Device<'gc>: Trace {
    fn read(&mut self, ctx: Context<'gc>, buf: &mut [u8], mark: SeekFrom) -> io::Result<usize>;
    fn write(&mut self, ctx: Context<'gc>, buf: &[u8], mark: SeekFrom) -> io::Result<usize>;
    fn seek(&mut self, ctx: Context<'gc>, pos: SeekFrom) -> io::Result<u64>;
    fn close(&mut self, ctx: Context<'gc>) -> io::Result<()>;
    fn flush(&mut self, ctx: Context<'gc>) -> io::Result<()>;
    fn can_seek(&self, _ctx: Context<'gc>) -> bool {
        true
    }

    fn write_all(&mut self, ctx: Context<'gc>, buf: &[u8], mut mark: SeekFrom) -> io::Result<()> {
        let mut remaining = buf;
        while !remaining.is_empty() {
            let written = self.write(ctx, remaining, mark)?;
            if written == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::WriteZero,
                    "Failed to write all bytes",
                ));
            }
            remaining = &remaining[written..];
            mark = mark.offset(written as i64);
        }
        Ok(())
    }

    fn direction(&self) -> PortDirection {
        PortDirection::Both
    }
}

impl<'gc> Device<'gc> for fs::File {
    fn read(&mut self, _: Context<'gc>, buf: &mut [u8], mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Seek>::seek(self, mark)?;
        <Self as io::Read>::read(self, buf)
    }

    fn write(&mut self, _: Context<'gc>, buf: &[u8], mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Seek>::seek(self, mark)?;
        <Self as io::Write>::write(self, buf)
    }

    fn close(&mut self, _: Context<'gc>) -> io::Result<()> {
        <Self as io::Write>::flush(self)?;
        Ok(())
    }

    fn flush(&mut self, _: Context<'gc>) -> io::Result<()> {
        <Self as io::Write>::flush(self)
    }

    fn seek(&mut self, _: Context<'gc>, pos: SeekFrom) -> io::Result<u64> {
        <Self as io::Seek>::seek(self, pos)
    }
}

impl<'gc> Device<'gc> for TcpStream {
    fn read(&mut self,_: Context<'gc>, buf: &mut [u8], _mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Read>::read(self, buf)
    }

    fn write(&mut self, _: Context<'gc>, buf: &[u8], _mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Write>::write(self, buf)
    }

    fn close(&mut self, _: Context<'gc>) -> io::Result<()> {
        <Self as io::Write>::flush(self)?;
        Ok(())
    }

    fn flush(&mut self, _: Context<'gc>) -> io::Result<()> {
        <Self as io::Write>::flush(self)
    }

    fn seek(&mut self, _: Context<'gc>,  _pos: SeekFrom) -> io::Result<u64> {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "TcpStream does not support seeking",
        ));
    }
}

pub enum StdDevice {
    Stdin,
    Stdout,
    Stderr,
}
unsafe impl Trace for StdDevice {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {}
}

impl<'gc> Device<'gc> for StdDevice {
    fn can_seek(&self, _: Context<'gc>) -> bool {
        false
    }

    fn close(&mut self, _: Context<'gc>) -> io::Result<()> {
        // Standard streams do not need to be closed
        Ok(())
    }

    fn direction(&self) -> PortDirection {
        match self {
            StdDevice::Stdin => PortDirection::In,
            StdDevice::Stdout => PortDirection::Out,
            StdDevice::Stderr => PortDirection::Out,
        }
    }

    fn flush(&mut self, _: Context<'gc>) -> io::Result<()> {
        match self {
            StdDevice::Stdout | StdDevice::Stderr => {
                // Flushing standard output and error streams
                std::io::stdout().flush()?;
                std::io::stderr().flush()?;
                Ok(())
            }
            _ => Ok(()), // Stdin does not need flushing
        }
    }

    fn read(&mut self, _: Context<'gc>, buf: &mut [u8], mark: SeekFrom) -> io::Result<usize> {
        if let StdDevice::Stdin = self {
            // Reading from standard input
            let mut stdin = std::io::stdin();
            let _ = mark;
            stdin.read(buf)
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "Cannot read from stdout or stderr",
            ))
        }
    }

    fn seek(&mut self, _: Context<'gc>, _pos: SeekFrom) -> io::Result<u64> {
        Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Standard streams do not support seeking",
        ))
    }

    fn write(&mut self, _: Context<'gc>, buf: &[u8], mark: SeekFrom) -> io::Result<usize> {
        match self {
            StdDevice::Stdout => {
                // Writing to standard output
                let mut stdout = std::io::stdout();
                let _ = mark;
                stdout.write(buf)
            }
            StdDevice::Stderr => {
                // Writing to standard error
                let mut stderr = std::io::stderr();
                let _ = mark;
                stderr.write(buf)
            }
            StdDevice::Stdin => Err(io::Error::new(
                io::ErrorKind::Other,
                "Cannot write to stdin",
            )),
        }
    }

    fn write_all(&mut self, ctx: Context<'gc>, buf: &[u8], mut mark: SeekFrom) -> io::Result<()> {
        let mut remaining = buf;
        while !remaining.is_empty() {
            let written = self.write(ctx, remaining, mark)?;
            if written == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::WriteZero,
                    "Failed to write all bytes",
                ));
            }
            remaining = &remaining[written..];
            mark = mark.offset(written as i64);
        }
        Ok(())
    }
}
pub enum PortDevice<'gc> {
    Device(Box<dyn Device<'gc> + 'gc>),
    ByteVector(Value<'gc>),
}

unsafe impl<'gc> Trace for PortDevice<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        match self {
            PortDevice::Device(device) => device.trace(visitor),
            PortDevice::ByteVector(bvec) => bvec.trace(visitor),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortType {
    NamedFile = 1,
    ByteVector = 2,
    Custom = 3,
    Socket = 4,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortSubtype {
    None = 0,
    CharSpecial = 1,
    Fifo = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortDirection {
    In = 0x01,
    Out = 0x02,
    Both = 0x03,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortBufferMode {
    None = 1,
    Line = 2,
    Block = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortFileOption {
    None = 0,
    NoCreate = 0x01,
    NoFail = 0x02,
    NoTruncate = 0x04,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FileOptions: u8 {
        const NONE = 0;
        const NO_CREATE = PortFileOption::NoCreate as u8;
        const NO_FAIL = PortFileOption::NoFail as u8;
        const NO_TRUNCATE = PortFileOption::NoTruncate as u8;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Codec {
    Latin1 = 1,
    Utf8 = 2,
    Utf16 = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EolStyle {
    None = 1,
    Lf = 2,
    Cr = 3,
    CrLf = 4,
    Nel = 5,
    CrNel = 6,
    Ls = 7,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortErrorHandlingMode {
    Ignore = 1,
    Raise = 2,
    Replace = 3,
}

pub const PORT_BLOCK_BUFFER_SIZE: usize = 4096;
pub const PORT_LINE_BUFFER_SIZE: usize = 256;
pub const PORT_CUSTOM_BUFFER_SIZE: usize = PORT_BLOCK_BUFFER_SIZE;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortBufState {
    Unspecified = 0,
    Read = 1,
    Write = 2,
    Accumulate = 3,
}

pub const PORT_UCS4_BOM: u32 = 0x0feff;
pub const PORT_UCS4_REPLACEMENT_CHAR: u32 = 0x0fffd;
pub const PORT_UCS4_LF: u32 = 0x0000a;
pub const PORT_UCS4_CR: u32 = 0x0000d;
pub const PORT_UCS4_NEL: u32 = 0x00085;
pub const PORT_UCS4_LS: u32 = 0x02028;

pub const PORT_BYTE_REPLACEMENT_CHAR: u8 = b'?';

pub const PORT_BYTEVECTOR_OUTPUT_CHUNK: usize = 128;

pub const PORT_CODEC_NATIVE: Codec = Codec::Utf8;
pub const PORT_EOL_STYLE_NATIVE: EolStyle = EolStyle::Lf;

pub struct Port<'gc> {
    inner: Mutex<PortImpl<'gc>>,
}

unsafe impl<'gc> Send for Port<'gc> {}
unsafe impl<'gc> Sync for Port<'gc> {}

unsafe impl<'gc> Trace for Port<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.inner.get_mut().trace(visitor);
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Port<'gc> {}

struct PortImpl<'gc> {
    device: Option<PortDevice<'gc>>,
    bytevector: bool,
    handlers: Value<'gc>,
    lookahead: [u8; 8],
    buffer: Vec<u8>,
    buffer_tail: usize,
    buffer_head: usize,
    name: Value<'gc>,
    transcoder: Value<'gc>,
    mark: SeekFrom,
    lookahead_size: usize,
    buf_state: PortBufState,
    line: u32,
    column: u32,
    codec: Codec,
    eol_style: EolStyle,
    error_handling_mode: PortErrorHandlingMode,
    file_options: FileOptions,
    buffer_mode: PortBufferMode,
    #[allow(dead_code)]
    subtype: PortSubtype,
    direction: PortDirection,
    force_sync: bool,
    bom_le: bool,
    bom_be: bool,
    track_line_column: bool,
}

impl<'gc> Port<'gc> {
    pub fn new(ctx: Context<'gc>) -> Gc<'gc, Self> {
        let this = Gc::new(
            &ctx,
            Self {
                inner: Mutex::new(PortImpl {
                    device: None,
                    handlers: Value::new(false),
                    bytevector: false,
                    lookahead: [0; 8],
                    buffer: Vec::new(),
                    buffer_tail: 0,
                    buffer_head: 0,
                    name: Value::new(Value::void()),
                    transcoder: Value::new(false),
                    mark: SeekFrom::Start(0),
                    lookahead_size: 0,
                    buf_state: PortBufState::Unspecified,
                    line: 1,
                    column: 1,
                    codec: Codec::Utf8,
                    eol_style: EolStyle::Lf,
                    error_handling_mode: PortErrorHandlingMode::Replace,
                    file_options: FileOptions::NONE,
                    buffer_mode: PortBufferMode::None,
                    subtype: PortSubtype::None,
                    direction: PortDirection::Both,
                    force_sync: false,
                    bom_le: false,
                    bom_be: false,
                    track_line_column: true,
                }),
            },
        );
        this.set_user_header(TypeCode8::PORT.0 as _);

        ctx.finalizers().register_candidate(&QUEUE, this);

        this
    }

    pub fn set_buffer_mode(&self, mode: PortBufferMode) {
        let mut port = self.inner.lock();
        port.buffer_mode = mode;
        port.init_buffer();
    }

    pub fn set_codec(&self, codec: Codec) {
        let mut port = self.inner.lock();
        port.codec = codec;
    }

    pub fn set_device(
        &self,
     
        device: Box<dyn Device<'gc> + 'gc>,
        name: Value<'gc>,
        transcoder: Value<'gc>,
    ) {
        let mut port = self.inner.lock();
        port.device = Some(PortDevice::Device(device));
        port.name = name;
        port.transcoder = transcoder;
        port.init_buffer();
        port.init_transcoder();
        port.init_tracking();
    }

    pub fn no_output_buffered(&self) -> bool {
        let port = self.inner.lock();
        port.no_output_buffered()
    }

    pub fn no_input_buffered(&self) -> bool {
        let port = self.inner.lock();
        port.no_input_buffered()
    }

    pub fn lookahead_byte(&self, ctx: Context<'gc>) -> io::Result<Option<u8>> {
        let mut port = self.inner.lock();
        port.lookahead_byte(ctx)
    }

    pub fn get_byte(&self, ctx: Context<'gc>) -> io::Result<Option<u8>> {
        let mut port = self.inner.lock();
        port.get_byte(ctx)
    }

    pub fn get_bytes(&self,ctx: Context<'gc>,  buf: &mut [u8]) -> io::Result<usize> {
        let mut port = self.inner.lock();
        port.get_bytes(ctx, buf)
    }

    pub fn lookahead_utf8(&self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        let mut port = self.inner.lock();
        port.lookahead_utf8(ctx)
    }

    pub fn get_utf8(&self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        let mut port = self.inner.lock();
        port.get_utf8(ctx)
    }

    pub fn set_mark(&self, ctx: Context<'gc>, offset: i64) -> std::io::Result<()> {
        let mut port = self.inner.lock();
        port.set_mark(ctx, offset)
    }

    pub fn put_byte(&self, ctx: Context<'gc>, byte: u8) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.put_byte(ctx, byte)
    }

    pub fn put_bytes(&self, ctx: Context<'gc>, buf: &[u8]) -> std::io::Result<()> {
        let mut port = self.inner.lock();
        port.put_bytes(ctx, buf)
    }

    pub fn flush_output(&self, ctx: Context<'gc>) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.flush_output(ctx)
    }

    pub fn close(&self, ctx: Context<'gc>) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.close(ctx)
    }

    pub fn put_utf8(&self, ctx: Context<'gc>, c: char) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.put_utf8(ctx, c)
    }

    pub fn is_input(&self) -> bool {
        self.inner.lock().direction as u8 & PortDirection::In as u8 != 0
    }

    pub fn is_output(&self) -> bool {
        self.inner.lock().direction as u8 & PortDirection::Out as u8 != 0
    }

    pub fn is_textual(&self) -> bool {
        self.inner.lock().transcoder != false
    }

    pub fn is_binary(&self) -> bool {
        !self.is_textual()
    }

    pub fn is_open(&self) -> bool {
        self.inner.lock().device.is_some()
    }

    pub fn is_bytevector(&self) -> bool {
        self.inner.lock().bytevector
    }

    pub fn get_bytevector(&self, ctx: Context<'gc>) -> Gc<'gc, ByteVector<'gc>> {
        let inner = self.inner.lock();

        if inner.buffer.is_empty() {
            ByteVector::new(&ctx, 0, 0)
        } else {
            ByteVector::from_slice(&ctx, &inner.buffer[inner.buffer_head..inner.buffer_tail])
        }
    }

    pub fn extract_bytevector(&self, ctx: Context<'gc>) -> Gc<'gc, ByteVector<'gc>> {
        let bvector = self.get_bytevector(ctx);
        let mut inner = self.inner.lock();
        inner.buffer.clear();
        inner.buffer_head = 0;
        inner.buffer_tail = 0;
        inner.mark = SeekFrom::Start(0);

        bvector
    }

    pub fn get_string(&self, ctx: Context<'gc>) -> Gc<'gc, String<'gc>> {
        let inner = self.inner.lock();

        if inner.buffer.is_empty() {
            String::new(&ctx, "", false)
        } else {
            String::new(
                &ctx,
                std::str::from_utf8(&inner.buffer[inner.buffer_head..inner.buffer_tail])
                    .unwrap_or(""),
                false,
            )
        }
    }

    pub fn extract_string(&self, ctx: Context<'gc>) -> Gc<'gc, String<'gc>> {
        let string = self.get_string(ctx);
        let mut inner = self.inner.lock();
        inner.buffer.clear();
        inner.buffer_head = 0;
        inner.buffer_tail = 0;
        inner.mark = SeekFrom::Start(0);

        string
    }

    pub fn lookahead_ch(&self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        let mut inner = self.inner.lock();
        match inner.codec {
            Codec::Utf8 => inner.lookahead_utf8(ctx),
            Codec::Latin1 => Ok(inner.lookahead_byte(ctx)?.map(|b| b as char)),
            _ => todo!(),
        }
    }

    pub fn get_ch(&self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        let mut inner = self.inner.lock();
        match inner.codec {
            Codec::Utf8 => inner.get_utf8(ctx),
            Codec::Latin1 => Ok(inner.get_byte(ctx)?.map(|b| b as char)),
            _ => todo!(),
        }
    }

    pub fn get_char(&self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        let Some(ch) = self.get_ch(ctx)? else {
            return Ok(None);
        };
        let port = self.inner.lock();
        if port.transcoder == false {
            return Ok(Some(ch));
        }

        if port.eol_style == EolStyle::None {
            return Ok(Some(ch));
        }

        match ch as u32 {
            PORT_UCS4_NEL | PORT_UCS4_LS => Ok(Some(PORT_UCS4_LF as u8 as char)),
            PORT_UCS4_CR => {
                let post = self.lookahead_ch(ctx)?;
                match post.map(|x| x as u32) {
                    Some(PORT_UCS4_LF) | Some(PORT_UCS4_NEL) => {
                        self.get_ch(ctx)?;
                        Ok(Some(PORT_UCS4_LF as u8 as char))
                    }
                    _ => Ok(Some(PORT_UCS4_LF as u8 as char)),
                }
            }
            _ => Ok(Some(ch)),
        }
    }

    pub fn puts(&self, ctx: Context<'gc>, s: impl AsRef<str>) -> io::Result<()> {
        self.put_bytes(ctx, s.as_ref().as_bytes())
    }

    pub fn put_string(&self, ctx: Context<'gc>, s: Gc<'gc, String<'gc>>) -> io::Result<()> {
        let mut port = self.inner.lock();

        for i in 0..s.len() {
            let ch = s.get(i).unwrap();
            port.put_utf8(ctx, ch)?;
        }

        Ok(())
    }

    pub fn open_std(
        &self,
        fd: StdDevice,
        name: Value<'gc>,
        file_options: FileOptions,
        _buffer_mode: PortBufferMode,
        transcoder: Value<'gc>,
    ) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.direction = fd.direction();
        port.device = Some(PortDevice::Device(Box::new(fd)));
        port.name = name;
        port.handlers = Value::new(false);

        port.transcoder = transcoder;
        port.file_options = file_options;
        port.force_sync = false;
        port.init_tracking();
        port.init_transcoder();
        port.init_buffer();
        Ok(())
    }

    pub fn open_file(
        &self,
        name: Value<'gc>,
        direction: PortDirection,
        file_options: FileOptions,
        buffer_mode: PortBufferMode,
        transcoder: Value<'gc>,
    ) -> io::Result<()> {
        let path = name.downcast::<String>().to_string();

        let mut ops = fs::OpenOptions::new();

        match direction {
            PortDirection::In => ops.read(true),
            PortDirection::Out => ops.write(true).create(true).truncate(true),
            PortDirection::Both => ops.read(true).write(true).create(true),
        };

        if file_options.contains(FileOptions::NO_CREATE) {
            ops.create(false);
        } else {
            ops.create(true);
        }

        if file_options.contains(FileOptions::NO_TRUNCATE) {
            ops.truncate(false);
        } else {
            ops.truncate(true);
        }

        let file = ops.open(path)?;
        let mut port = self.inner.lock();

        port.direction = direction;
        port.device = Some(PortDevice::Device(Box::new(file)));
        port.name = name;
        port.handlers = Value::new(false);
        port.transcoder = transcoder;
        port.file_options = file_options;
        port.force_sync = false;
        port.buffer_mode = buffer_mode;
        port.init_tracking();
        port.init_transcoder();
        port.init_buffer();
        Ok(())
    }

    pub fn open_bytevector(
        &self,
        name: Value<'gc>,
        direction: PortDirection,
        transcoder: Value<'gc>,
        bytes: Value<'gc>,
    ) -> io::Result<()> {
        let mut port = self.inner.lock();
        port.direction = direction;
        port.device = Some(PortDevice::ByteVector(bytes));
        port.name = name;
        port.handlers = Value::new(false);
        port.transcoder = transcoder;
        port.bytevector = true;
        port.file_options = FileOptions::NONE;
        port.force_sync = false;
        port.init_tracking();
        port.init_transcoder();
        port.buffer = vec![];
        port.buf_state = if port.direction as u8 & PortDirection::Out as u8 != 0 {
            PortBufState::Accumulate
        } else {
            PortBufState::Unspecified
        };
        Ok(())
    }

    pub fn is_eof(&self, ctx: Context<'gc>) -> io::Result<bool> {
        let port = self.inner.lock();

        if port.device.is_none() {
            return Ok(true);
        }

        Ok(self.lookahead_byte(ctx)?.is_none())
    }
}

unsafe impl<'gc> Trace for PortImpl<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        if let Some(ref mut device) = self.device {
            device.trace(visitor);
        }
        self.handlers.trace(visitor);
        self.name.trace(visitor);
        self.transcoder.trace(visitor);
    }
}

impl<'gc> PortImpl<'gc> {
    fn device_set_mark(&mut self, ctx: Context<'gc>, mark: SeekFrom) -> std::io::Result<()> {
        if let Some(PortDevice::Device(ref mut dev)) = self.device {
            dev.seek(ctx, mark)?;
        }
        self.mark = mark;
        self.buffer_head = 0;
        self.buffer_tail = 0;
        self.buf_state = PortBufState::Unspecified;
        Ok(())
    }

    fn device_read(&mut self, ctx: Context<'gc>, buf: &mut [u8], mark: SeekFrom) -> std::io::Result<usize> {
        if let Some(PortDevice::Device(ref mut dev)) = self.device {
            dev.read(ctx, buf, mark)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn read_self(&mut self, ctx: Context<'gc>, mark: SeekFrom) -> std::io::Result<usize> {
        if let Some(ref mut device) = self.device {
            let n = match device {
                PortDevice::Device(dev) => dev.read(ctx, &mut self.buffer, mark)?,
                PortDevice::ByteVector(bvec) => {
                    let bvec = bvec.downcast::<ByteVector>();
                    let b = bvec.mut_bytes();
                    let n = b.len().min(self.buffer.len());
                    b[..n].copy_from_slice(&self.buffer[..n]);
                    n
                }
            };

            Ok(n)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn read_self_from(&mut self, ctx: Context<'gc>, offset: usize, mark: SeekFrom) -> std::io::Result<usize> {
        if let Some(PortDevice::Device(ref mut device)) = self.device {
            let n = device.read(ctx, &mut self.buffer[offset..], mark)?;

            Ok(n)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn device_write(&mut self, ctx: Context<'gc>, buf: &[u8], mark: SeekFrom) -> std::io::Result<()> {
        if let Some(PortDevice::Device(ref mut device)) = self.device {
            device.write_all(ctx, buf, mark)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn init_buffer(&mut self) {
        match self.buffer_mode {
            PortBufferMode::None => {
                self.buffer.clear();
                self.buffer_head = 0;
                self.buffer_tail = 0;
            }

            PortBufferMode::Line => {
                self.buffer = vec![0; PORT_LINE_BUFFER_SIZE];
                self.buffer_head = 0;
                self.buffer_tail = 0;
            }

            PortBufferMode::Block => {
                self.buffer = vec![0; PORT_BLOCK_BUFFER_SIZE];
                self.buffer_head = 0;
                self.buffer_tail = 0;
            }
        }
    }

    fn init_transcoder(&mut self) {
        let transcoder = self.transcoder;

        if transcoder.is::<ByteVector>() {
            let bvector = transcoder.downcast::<ByteVector>();

            match bvector[0] {
                0 => self.codec = Codec::Latin1,
                1 => self.codec = Codec::Utf8,
                2 => self.codec = Codec::Utf16,
                _ => self.codec = Codec::Utf8, // Default to UTF-8 if unknown
            }

            match bvector[1] {
                0 => self.eol_style = EolStyle::None,
                1 => self.eol_style = EolStyle::Lf,
                2 => self.eol_style = EolStyle::Cr,
                3 => self.eol_style = EolStyle::CrLf,
                4 => self.eol_style = EolStyle::Nel,
                5 => self.eol_style = EolStyle::CrNel,
                6 => self.eol_style = EolStyle::Ls,
                _ => self.eol_style = EolStyle::Lf, // Default to LF if unknown
            }

            match bvector[2] {
                0 => self.error_handling_mode = PortErrorHandlingMode::Ignore,
                1 => self.error_handling_mode = PortErrorHandlingMode::Raise,
                2 => self.error_handling_mode = PortErrorHandlingMode::Replace,
                _ => self.error_handling_mode = PortErrorHandlingMode::Replace, // Default to Replace if unknown
            }

            if self.codec == Codec::Utf8
                && self.eol_style == EolStyle::Lf
                && self.error_handling_mode == PortErrorHandlingMode::Replace
            {
                self.transcoder = Value::new(true);
            }
        } else {
            self.codec = Codec::Utf8; // Default codec
            self.eol_style = EolStyle::Lf; // Default EOL style
            self.error_handling_mode = PortErrorHandlingMode::Replace; // Default error handling mode
        }
    }

    fn init_tracking(&mut self) {
        self.mark = SeekFrom::Start(0);
        self.line = 1;
        self.column = 1;
        self.track_line_column = self.transcoder != Value::new(false);
        self.lookahead_size = 0;
        self.bom_le = false;
        self.bom_be = false;
    }

    fn flush_output(&mut self, ctx: Context<'gc>) -> io::Result<()> {
        if self.no_output_buffered() {
            return Ok(());
        }
        if let Some(mut device) = self.device.as_mut() {
            let n = self.buffer_tail - self.buffer_head;
            let buf = &self.buffer[self.buffer_head..self.buffer_tail];
            if let PortDevice::Device(device) = &mut device {
                device.write_all(ctx, buf, self.mark.offset(-(n as i64)))?;
            }
            self.buffer_head = 0;
            self.buffer_tail = 0;
            self.buf_state = PortBufState::Unspecified;
        }

        Ok(())
    }

    fn no_output_buffered(&self) -> bool {
        self.buf_state != PortBufState::Write
            || self.buffer.is_empty()
            || self.buffer_head == self.buffer_tail
    }

    fn no_input_buffered(&self) -> bool {
        self.lookahead_size == 0
            && (self.buffer.is_empty()
                || self.buffer_head == self.buffer_tail
                || self.buf_state != PortBufState::Read)
    }

    fn discard_buffer(&mut self) {
        if !self.buffer.is_empty() {
            self.buffer.clear();
            self.buffer_head = 0;
            self.buffer_tail = 0;
            self.buf_state = PortBufState::Unspecified;
        }
    }

    fn close(&mut self, ctx: Context<'gc>) -> io::Result<()> {
        let Some(mut device) = self.device.take() else {
            return Ok(());
        };
        self.flush_output(ctx)?;
        self.discard_buffer();
        if let PortDevice::Device(ref mut dev) = device {
            dev.close(ctx)?;
        }
        Ok(())
    }

    fn lookahead_byte(&mut self, ctx: Context<'gc>) -> io::Result<Option<u8>> {
        if self.device.is_none() {
            return Ok(None);
        }

        if let Some(PortDevice::ByteVector(ref bvec)) = self.device {
            let bvec = bvec.downcast::<ByteVector>();
            if bvec.len() <= self.mark.get_offset() as usize {
                return Ok(None);
            }
            return Ok(Some(bvec[self.mark.get_offset() as usize]));
        }

        if !self.buffer.is_empty() {
            if self.buf_state == PortBufState::Read && self.buffer_head != self.buffer_tail {
                return Ok(Some(self.buffer[self.buffer_head]));
            }
            self.flush_output(ctx)?;
            self.buf_state = PortBufState::Read;
            self.buffer_head = 0;
            self.buffer_tail = 0;

            let n = self.read_self(ctx, self.mark)?;

            if n == 0 {
                return Ok(None);
            }
            self.buffer_tail = n;
            return Ok(Some(self.buffer[self.buffer_head]));
        } else {
            if self.lookahead_size > 0 {
                return Ok(Some(self.lookahead[0]));
            }
            let mut b = [0; 1];
            self.lookahead_size = self.device_read(ctx, &mut b, self.mark)?;
            if self.lookahead_size == 0 {
                return Ok(None);
            }
            self.lookahead[0] = b[0];

            return Ok(Some(self.lookahead[0]));
        }
    }

    fn update_line_column(&mut self, c: u8) {
        if c == b'\n' {
            self.line += 1;
            self.column = 1;
            return;
        }

        self.column += 1;
    }

    fn get_byte(&mut self, ctx: Context<'gc>) -> io::Result<Option<u8>> {
        if self.device.is_none() {
            return Ok(None);
        }

        if let Some(PortDevice::ByteVector(ref bvec)) = self.device {
            let bvec = bvec.downcast::<ByteVector>();
            if bvec.len() <= self.mark.get_offset() as usize {
                return Ok(None);
            }
            let b = bvec[self.mark.get_offset() as usize];
            self.mark = self.mark.offset(1);
            if self.track_line_column {
                self.update_line_column(b);
            }
            return Ok(Some(b));
        }

        if !self.buffer.is_empty() {
            if self.buf_state != PortBufState::Read || self.buffer_head == self.buffer_tail {
                self.flush_output(ctx)?;
                self.buf_state = PortBufState::Read;
                self.buffer_head = 0;
                self.buffer_tail = 0;

                let n = self.read_self(ctx, self.mark)?;

                if n == 0 {
                    return Ok(None);
                }
                self.buffer_tail = n;
            }
            let b = self.buffer[self.buffer_head];
            self.buffer_head += 1;
            if self.track_line_column {
                self.update_line_column(b);
            }
            self.mark = self.mark.offset(1);
            return Ok(Some(b));
        } else {
            let b;
            if self.lookahead_size != 0 {
                b = self.lookahead[0];
                self.lookahead[0] = self.lookahead[1];
                self.lookahead[1] = self.lookahead[2];
                self.lookahead[2] = self.lookahead[3];
                self.lookahead_size -= 1;
            } else {
                let mut buf = [0; 1];
                let n = self.device_read(ctx, &mut buf, self.mark)?;
                if n == 0 {
                    return Ok(None);
                }
                b = buf[0];
            }

            self.mark = self.mark.offset(1);
            return Ok(Some(b));
        }
    }

    fn get_bytes(&mut self, ctx: Context<'gc>, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() || self.device.is_none() {
            return Ok(0);
        }

        if let Some(PortDevice::ByteVector(ref bvec)) = self.device {
            let bvec = bvec.downcast::<ByteVector>();
            let available = bvec.len() - self.mark.get_offset() as usize;
            let to_read = available.min(buf.len());
            if to_read == 0 {
                return Ok(0);
            }
            buf[..to_read].copy_from_slice(
                &bvec[self.mark.get_offset() as usize..self.mark.get_offset() as usize + to_read],
            );
            self.mark = self.mark.offset(to_read as i64);
            if self.track_line_column {
                for &b in &buf[..to_read] {
                    self.update_line_column(b);
                }
            }
            return Ok(to_read);
        }

        if !self.buffer.is_empty() {
            if self.buf_state != PortBufState::Read || self.buffer_head == self.buffer_tail {
                self.flush_output(ctx)?;
                self.buf_state = PortBufState::Read;
                self.buffer_head = 0;
                self.buffer_tail = 0;

                let n = self.read_self(ctx, self.mark)?;

                if n == 0 {
                    return Ok(0);
                }
                self.buffer_tail = n;
            }
            let available = self.buffer_tail - self.buffer_head;
            let to_read = available.min(buf.len());
            buf[..to_read]
                .copy_from_slice(&self.buffer[self.buffer_head..self.buffer_head + to_read]);
            self.buffer_head += to_read;
            if self.track_line_column {
                for &b in &buf[..to_read] {
                    self.update_line_column(b);
                }
            }
            self.mark = self.mark.offset(to_read as i64);
            if to_read == buf.len() {
                return Ok(to_read);
            }

            return Ok(to_read + self.device_read(ctx, &mut buf[to_read..], self.mark)?);
        } else {
            if self.lookahead_size != 0 {
                let b = self.lookahead[0];
                self.lookahead[0] = self.lookahead[1];
                self.lookahead[1] = self.lookahead[2];
                self.lookahead[2] = self.lookahead[3];
                self.lookahead_size -= 1;
                buf[0] = b;
                if self.track_line_column {
                    self.update_line_column(b);
                }

                self.mark = self.mark.offset(1);
                if buf.len() == 1 {
                    return Ok(1);
                }

                return Ok(1 + self.device_read(ctx, &mut buf[1..], self.mark)?);
            } else {
                let n = self.device_read(ctx, buf, self.mark)?;
                if self.track_line_column {
                    for &b in &buf[..n] {
                        self.update_line_column(b);
                    }
                }
                self.mark = self.mark.offset(n as i64);
                return Ok(n);
            }
        }
    }

    pub fn lookahead_utf8(&mut self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        if self.device.is_none() {
            return Ok(None);
        }

        let eof = |this: &mut Self| match this.error_handling_mode {
            PortErrorHandlingMode::Ignore => Ok(None),
            PortErrorHandlingMode::Replace => {
                this.lookahead[0] = PORT_BYTE_REPLACEMENT_CHAR;
                this.lookahead_size = 1;
                Ok(Some(PORT_BYTE_REPLACEMENT_CHAR as char))
            }
            PortErrorHandlingMode::Raise => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected end of file",
            )),
        };

        'top: loop {
            let mut utf8 = [0u8; 6];
            let Some(b) = self.lookahead_byte(ctx)? else {
                return Ok(None);
            };

            if b > 127 {
                let code_length = utf8_byte_count(b);

                utf8[0] = b;
                if code_length > 1 {
                    if let Some(PortDevice::ByteVector(ref bvec)) = self.device {
                        let bvec = bvec.downcast::<ByteVector>();
                        if bvec.len() <= self.mark.get_offset() as usize + code_length as usize {
                            return eof(self);
                        }
                        utf8[..code_length].copy_from_slice(
                            &bvec[self.mark.get_offset() as usize
                                ..self.mark.get_offset() as usize + code_length as usize],
                        );
                    } else if !self.buffer.is_empty() {
                        let mut n1 = self.buffer_tail - self.buffer_head;
                        while n1 < code_length {
                            if self.buffer_head != 0 {
                                self.buffer
                                    .copy_within(self.buffer_head..self.buffer_tail, 0);
                                self.buffer_head = 0;
                            }

                            let n2 = self.read_self_from(ctx, n1, self.mark.offset(n1 as _))?;
                            n1 += n2;
                            self.buffer_tail = n1;
                            self.buffer_head = n1;
                            if n2 == 0 {
                                return eof(self);
                            }
                        }
                        for i in 0..code_length {
                            utf8[i] = self.buffer[self.buffer_head + i];
                        }
                    } else {
                        while self.lookahead_size < code_length {
                            let mut l = self.lookahead;
                            let n = self.device_read(
                                ctx,
                                &mut l[self.lookahead_size..self.lookahead_size + code_length],
                                self.mark.offset(self.lookahead_size as _),
                            )?;
                            self.lookahead = l;
                            self.lookahead_size += n;
                            if n == 0 {
                                return eof(self);
                            }
                        }
                        for i in 0..code_length {
                            utf8[i] = self.lookahead[i];
                        }
                    }
                }

                let ucs4 = match cnvt_utf8_to_ucs4(&utf8) {
                    Some((c, l)) if l >= 1 => char::from_u32(c).unwrap(),

                    _ => match self.error_handling_mode {
                        PortErrorHandlingMode::Ignore => {
                            for _ in 0..code_length {
                                self.get_byte(ctx)?;
                            }
                            continue 'top;
                        }
                        PortErrorHandlingMode::Replace => {
                            if self.lookahead_size != 0 {
                                self.lookahead[0] = PORT_BYTE_REPLACEMENT_CHAR;
                                self.lookahead_size = 1;
                                return Ok(Some(PORT_BYTE_REPLACEMENT_CHAR as char));
                            }
                            char::from_u32(PORT_UCS4_REPLACEMENT_CHAR).unwrap()
                        }

                        PortErrorHandlingMode::Raise => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "Invalid UTF-8 sequence",
                            ));
                        }
                    },
                };

                if self.mark == SeekFrom::Start(0) && ucs4 as u32 == PORT_UCS4_BOM {
                    self.get_byte(ctx)?;
                    self.get_byte(ctx)?;
                    self.get_byte(ctx)?;
                    continue 'top;
                }

                return Ok(Some(ucs4));
            }

            return Ok(Some(b as char));
        }
    }

    pub fn get_utf8(&mut self, ctx: Context<'gc>) -> io::Result<Option<char>> {
        if self.device.is_none() {
            return Ok(None);
        }

        let _ = |this: &mut Self| match this.error_handling_mode {
            PortErrorHandlingMode::Ignore => Ok(None),
            PortErrorHandlingMode::Replace => {
                this.lookahead[0] = PORT_BYTE_REPLACEMENT_CHAR;
                this.lookahead_size = 1;
                Ok(Some(PORT_BYTE_REPLACEMENT_CHAR as char))
            }
            PortErrorHandlingMode::Raise => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Unexpected end of file",
            )),
        };

        'top: loop {
            let mut utf8 = [0u8; 6];
            let Some(b) = self.get_byte(ctx)? else {
                return Ok(None);
            };

            if b > 127 {
                let code_length = utf8_byte_count(b);
                utf8[0] = b;

                for i in 1..code_length {
                    let Some(b) = self.get_byte(ctx)? else {
                        match self.error_handling_mode {
                            PortErrorHandlingMode::Ignore => return Ok(None),
                            PortErrorHandlingMode::Replace => {
                                return Ok(Some(PORT_BYTE_REPLACEMENT_CHAR as char));
                            }
                            PortErrorHandlingMode::Raise => {
                                return Err(io::Error::new(
                                    io::ErrorKind::UnexpectedEof,
                                    "Unexpected end of file",
                                ));
                            }
                        }
                    };
                    utf8[i] = b;
                }

                let ucs4 = match cnvt_utf8_to_ucs4(&utf8) {
                    Some((c, l)) if l >= 1 => char::from_u32(c).unwrap(),

                    _ => match self.error_handling_mode {
                        PortErrorHandlingMode::Ignore => continue 'top,
                        PortErrorHandlingMode::Replace => {
                            char::from_u32(PORT_UCS4_REPLACEMENT_CHAR).unwrap()
                        }

                        PortErrorHandlingMode::Raise => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "Invalid UTF-8 sequence",
                            ));
                        }
                    },
                };

                if matches!(self.mark, SeekFrom::Start(3) | SeekFrom::Current(3))
                    && ucs4 as u32 == PORT_UCS4_BOM
                {
                    continue 'top;
                }

                return Ok(Some(ucs4));
            }

            return Ok(Some(b as char));
        }
    }

    fn set_mark(&mut self, ctx: Context<'gc>, offset: i64) -> std::io::Result<()> {
        if self.device.is_none() {
            return Ok(());
        }

        self.flush_output(ctx)?;
        self.lookahead_size = 0;
        self.device_set_mark(ctx, SeekFrom::Start(offset as _))?;
        self.track_line_column = false;
        self.column = 0;
        self.line = 0;
        Ok(())
    }

    fn put_byte(&mut self, ctx: Context<'gc>, byte: u8) -> io::Result<()> {
        if self.device.is_none() {
            return Ok(());
        }

        if self.track_line_column {
            self.update_line_column(byte);
        }

        if let Some(PortDevice::ByteVector(ref _bvec)) = self.device {
            if self.mark.get_offset() as usize >= self.buffer.len() {
                let mut newsize = self.mark.get_offset() as usize + 128;
                let cursize = self.buffer_tail - self.buffer_head;
                if newsize < (cursize + (cursize >> 2)) {
                    newsize = cursize + (cursize >> 2);
                }
                if newsize < self.mark.get_offset() as usize + 128 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "ByteVector is too small",
                    ));
                }

                self.buffer.resize(newsize, 0);
                self.buffer_head = 0;
                self.buffer_tail = self.buffer_head + cursize;
            }

            self.buffer[self.buffer_head + self.mark.get_offset() as usize] = byte;
            self.mark = self.mark.offset(1);
            if self.buffer_head + self.mark.get_offset() as usize > self.buffer_tail {
                self.buffer_tail = self.buffer_head + self.mark.get_offset() as usize;
            }
        } else if !self.buffer.is_empty() {
            if self.buf_state != PortBufState::Write {
                self.buf_state = PortBufState::Write;
                self.buffer_head = 0;
                self.buffer_tail = 0;
            } else {
                if self.buffer_tail >= self.buffer.len() {
                    self.flush_output(ctx)?;
                    self.buf_state = PortBufState::Write;
                }
            }

            self.buffer[self.buffer_tail] = byte;
            self.buffer_tail += 1;
            if self.buffer_mode == PortBufferMode::Line && byte == b'\n' {
                self.flush_output(ctx)?;
            }
        } else {
            self.device_write(ctx, &[byte], self.mark)?;
            self.mark = self.mark.offset(1);
        }

        Ok(())
    }

    fn put_bytes(&mut self, ctx: Context<'gc>, buf: &[u8]) -> std::io::Result<()> {
        if self.device.is_none() {
            return Ok(());
        }

        self.lookahead_size = 0;

        if let Some(PortDevice::ByteVector(ref _bvec)) = self.device {
            
            if self.mark.get_offset() as usize >= self.buffer.len() {
                let mut newsize = self.mark.get_offset() as usize + 128;
                let cursize = self.buffer_tail - self.buffer_head;
                if newsize < (cursize + (cursize >> 2)) {
                    newsize = cursize + (cursize >> 2);
                }
                if newsize < self.mark.get_offset() as usize + 128 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "ByteVector is too small",
                    ));
                }

                self.buffer.resize(newsize, 0);
                self.buffer_head = 0;
                self.buffer_tail = self.buffer_head + cursize;
            }
            let space = self.buffer.len() - self.mark.get_offset() as usize;
            let n = space.min(buf.len());
            self.buffer[self.buffer_head + self.mark.get_offset() as usize
                ..self.buffer_head + self.mark.get_offset() as usize + n]
                .copy_from_slice(&buf[..n]);
            self.mark = self.mark.offset(n as i64);
            if self.buffer_head + self.mark.get_offset() as usize > self.buffer_tail {
                self.buffer_tail = self.buffer_head + self.mark.get_offset() as usize;
            }

            if self.track_line_column {
                for &b in &buf[..n] {
                    self.update_line_column(b);
                }
            }

            if n != buf.len() {
                return self.put_bytes(ctx, &buf[n..]);
            }
        } else if !self.buffer.is_empty() {
            if self.buf_state != PortBufState::Write {
                self.buf_state = PortBufState::Write;
                self.buffer_head = 0;
                self.buffer_tail = 0;
            } else {
                if self.buffer_tail + buf.len() > self.buffer.len() {
                    self.flush_output(ctx)?;
                    self.buf_state = PortBufState::Write;
                }
            }

            let space = self.buffer.len() - self.buffer_tail;
            let n = space.min(buf.len());
            self.buffer[self.buffer_tail..self.buffer_tail + n].copy_from_slice(&buf[..n]);
            self.buffer_tail += n;
            self.mark = self.mark.offset(n as i64);
            if self.track_line_column {
                for &b in &buf[..n] {
                    self.update_line_column(b);
                }
            }

            if n == buf.len() && self.buffer_mode == PortBufferMode::Line {
                let mut flush = false;
                for i in 0..n {
                    flush |= buf[i] == b'\n';
                }

                if flush {
                    self.flush_output(ctx)?;
                }
            }

            if n != buf.len() {
                return self.put_bytes(ctx, &buf[n..]);
            }
        } else {
            self.device_write(ctx, buf, self.mark)?;
            self.mark = self.mark.offset(buf.len() as i64);
            if self.track_line_column {
                for b in buf.iter().copied().filter(|&b| b == b'\n') {
                    self.update_line_column(b);
                }
            }
        }

        Ok(())
    }

    fn put_utf8(&mut self, ctx: Context<'gc>, ucs4: char) -> io::Result<()> {
        let mut utf8 = [0; 4];
        ucs4.encode_utf8(&mut utf8);
        self.put_bytes(ctx, &utf8[..ucs4.len_utf8()])
    }
}


struct PortFinalizerQueue {
    objects: Mutex<Vec<ObjectReference>>,
}

unsafe impl FinalizerQueue for PortFinalizerQueue {
    fn mark_ready_to_run(&self, object: ObjectReference) {
        let mut queue = self.objects.lock();
        queue.push(object);
    }

    fn schedule(&self) {
        VM_THREAD.schedule_task(VMThreadTask::ClosePorts);
    }
}

static QUEUE: LazyLock<Arc<PortFinalizerQueue>> = LazyLock::new(|| {
    Arc::new(PortFinalizerQueue {
        objects: Mutex::new(Vec::new()),
    })
});

pub(crate) fn close_ports<'gc>(_ctx: &'gc Mutation<'gc>) {
    let queue = &QUEUE;
    let mut objects = queue.objects.lock();

    for object in objects.drain(..) {
        unsafe {
            let port: Gc<'gc, Port<'gc>> = Gc::from_mmtk_object(object);
            let context= Context::empty(_ctx);
            let _ = port.close(context);
        }
    }
}

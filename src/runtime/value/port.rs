use parking_lot::Mutex;
use rsgc::{Collect, EnsureGCInfo, Gc, Trace};
use std::{
    fs,
    io::{self, SeekFrom},
    net::TcpStream,
};

use crate::runtime::Context;

use super::*;

pub trait SeekMarkExt {
    fn offset(&self, offset: i64) -> SeekFrom;
}

impl SeekMarkExt for SeekFrom {
    fn offset(&self, offset: i64) -> SeekFrom {
        match self {
            SeekFrom::Start(pos) => SeekFrom::Start((*pos as i64 + offset) as u64),
            SeekFrom::End(pos) => SeekFrom::End(pos + offset),
            SeekFrom::Current(pos) => SeekFrom::Current(pos + offset),
        }
    }
}

pub trait Device: Trace {
    fn read(&mut self, buf: &mut [u8], mark: SeekFrom) -> io::Result<usize>;
    fn write(&mut self, buf: &[u8], mark: SeekFrom) -> io::Result<usize>;
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64>;
    fn close(&mut self) -> io::Result<()>;
    fn flush(&mut self) -> io::Result<()>;

    fn write_all(&mut self, buf: &[u8], mut mark: SeekFrom) -> io::Result<()> {
        let mut remaining = buf;
        while !remaining.is_empty() {
            let written = self.write(remaining, mark)?;
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

impl Device for fs::File {
    fn read(&mut self, buf: &mut [u8], mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Seek>::seek(self, mark)?;
        <Self as io::Read>::read(self, buf)
    }

    fn write(&mut self, buf: &[u8], mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Seek>::seek(self, mark)?;
        <Self as io::Write>::write(self, buf)
    }

    fn close(&mut self) -> io::Result<()> {
        <Self as io::Write>::flush(self)?;
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        <Self as io::Write>::flush(self)
    }

    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        <Self as io::Seek>::seek(self, pos)
    }
}

impl Device for TcpStream {
    fn read(&mut self, buf: &mut [u8], _mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Read>::read(self, buf)
    }

    fn write(&mut self, buf: &[u8], _mark: SeekFrom) -> io::Result<usize> {
        <Self as io::Write>::write(self, buf)
    }

    fn close(&mut self) -> io::Result<()> {
        <Self as io::Write>::flush(self)?;
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        <Self as io::Write>::flush(self)
    }

    fn seek(&mut self, _pos: SeekFrom) -> io::Result<u64> {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "TcpStream does not support seeking",
        ));
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ByteVectorDevice<'gc> {
    bvec: Gc<'gc, ByteVector<'gc>>,
    cursor: usize,
}

impl<'gc> ByteVectorDevice<'gc> {
    pub fn new(bvec: Gc<'gc, ByteVector>) -> Self {
        Self { bvec, cursor: 0 }
    }
}

impl Device for ByteVectorDevice<'_> {
    fn read(&mut self, buf: &mut [u8], _mark: SeekFrom) -> io::Result<usize> {
        let len = buf.len();
        let available = self.bvec.len() - self.cursor;
        let to_read = available.min(len);
        if to_read == 0 {
            return Ok(0);
        }
        buf[..to_read].copy_from_slice(&self.bvec[self.cursor..self.cursor + to_read]);
        self.cursor += to_read;
        Ok(to_read)
    }

    fn write(&mut self, buf: &[u8], _mark: SeekFrom) -> io::Result<usize> {
        let len = buf.len();
        if self.cursor + len > self.bvec.len() {
            return Err(io::Error::new(
                io::ErrorKind::WriteZero,
                "ByteVector overflow",
            ));
        }
        self.bvec.mut_bytes()[self.cursor..self.cursor + len].copy_from_slice(buf);
        self.cursor += len;
        Ok(len)
    }

    fn close(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        match pos {
            SeekFrom::Start(offset) => {
                if offset > self.bvec.len() as u64 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Seek position out of bounds",
                    ));
                }
                self.cursor = offset as usize;
            }
            SeekFrom::End(offset) => {
                let end_pos = self.bvec.len() as i64;
                if offset > 0 || (end_pos + offset) < 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Seek position out of bounds",
                    ));
                }
                self.cursor = (end_pos + offset) as usize;
            }

            SeekFrom::Current(offset) => {
                let new_pos = self.cursor as i64 + offset;
                if new_pos < 0 || new_pos > self.bvec.len() as i64 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Seek position out of bounds",
                    ));
                }
                self.cursor = new_pos as usize;
            }
        }

        Ok(self.cursor as u64)
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

unsafe impl<'gc> Trace for Port<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        self.inner.get_mut().trace(visitor);
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Port<'gc> {}

struct PortImpl<'gc> {
    device: Option<Box<dyn Device + 'gc>>,
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
    subtype: PortSubtype,
    direction: PortDirection,
    force_sync: bool,
    bom_le: bool,
    bom_be: bool,
    track_line_column: bool,
}

impl<'gc> Port<'gc> {
    pub fn new(ctx: Context<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                inner: Mutex::new(PortImpl {
                    device: None,
                    handlers: Value::new(false),
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
        )
    }

    pub fn set_device(
        &self,
        device: Box<dyn Device + 'gc>,
        name: Value<'gc>,
        transcoder: Value<'gc>,
    ) {
        let mut port = self.inner.lock();
        port.device = Some(device);
        port.name = name;
        port.transcoder = transcoder;
        port.init_buffer();
        port.init_transcoder();
        port.init_tracking();
    }

    pub fn lookahead_byte(&self) -> io::Result<Option<u8>> {
        let mut port = self.inner.lock();
        port.lookahead_byte()
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
    fn set_mark(&mut self, mark: SeekFrom) -> std::io::Result<()> {
        if let Some(ref mut device) = self.device {
            device.seek(mark)?;
        }
        self.mark = mark;
        self.buffer_head = 0;
        self.buffer_tail = 0;
        self.buf_state = PortBufState::Unspecified;
        Ok(())
    }

    fn read(&mut self, buf: &mut [u8], mark: SeekFrom) -> std::io::Result<usize> {
        if let Some(ref mut device) = self.device {
            device.read(buf, mark)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn read_self(&mut self, mark: SeekFrom) -> std::io::Result<usize> {
        if let Some(ref mut device) = self.device {
            let n = device.read(&mut self.buffer, mark)?;

            Ok(n)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Port is closed",
            ))
        }
    }

    fn write(&mut self, buf: &mut [u8], mark: SeekFrom) -> std::io::Result<()> {
        if let Some(ref mut device) = self.device {
            device.write_all(buf, mark)
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

    fn flush_output(&mut self) -> io::Result<()> {
        if self.no_output_buffered() {
            return Ok(());
        }
        if let Some(device) = self.device.as_mut() {
            let n = self.buffer_tail - self.buffer_head;
            let buf = &self.buffer[self.buffer_head..self.buffer_tail];
            device.write_all(buf, self.mark.offset(-(n as i64)))?;
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

    fn close(&mut self) -> io::Result<()> {
        let Some(mut device) = self.device.take() else {
            return Ok(());
        };
        self.flush_output()?;
        self.discard_buffer();
        device.close()?;
        Ok(())
    }

    fn lookahead_byte(&mut self) -> io::Result<Option<u8>> {
        if self.device.is_none() {
            return Ok(None);
        }
        if !self.buffer.is_empty() {
            if self.buf_state == PortBufState::Read && self.buffer_head != self.buffer_tail {
                return Ok(Some(self.buffer[self.buffer_head]));
            }
            self.flush_output()?;
            self.buf_state = PortBufState::Read;
            self.buffer_head = 0;
            self.buffer_tail = 0;

            let n = self.read_self(self.mark)?;

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
            self.lookahead_size = self.read(&mut b, self.mark)?;
            if self.lookahead_size == 0 {
                return Ok(None);
            }
            self.lookahead[0] = b[0];

            return Ok(Some(self.lookahead[0]));
        }
    }
}

//! Stats for FASL files
//!
//! Allows us measure the memory usage of specific files for profiling.

use std::io::{self, Read, Write};

use flate2::{Compression, read::GzDecoder, write::GzEncoder};

use super::*;

/// Sections of FASL image
pub const SECTION_NAMES: [&str; 8] = [
    "block header",
    "metadata",
    "bytecode",
    "operand pool",
    "metadata counts",
    "constants",
    "spans",
    "srcmap",
];

pub struct Section {
    pub name: &'static str,
    pub raw: usize,
    /// The size of the section compressed on its own.
    pub gzip: usize,
}

pub struct Stats {
    /// The file as given
    pub file: usize,
    /// The lites table and the payload, uncompressed.
    pub uncompressed: usize,
    /// The payload as stored: compressed when the image says so.
    pub stored_payload: usize,
    pub compressed: bool,
    pub blocks: usize,
    /// The table of symbols and strings, which the writer stores outside the compressed payload.
    pub lites: Section,
    pub lite_count: usize,
    pub sections: Vec<Section>,
}

struct Cursor<'a> {
    bytes: &'a [u8],
    pos: usize,
}

fn truncated() -> io::Error {
    io::Error::new(io::ErrorKind::UnexpectedEof, "FASL image is truncated")
}

impl<'a> Cursor<'a> {
    fn take(&mut self, n: usize) -> io::Result<&'a [u8]> {
        let end = self.pos.checked_add(n).ok_or_else(truncated)?;
        let slice = self.bytes.get(self.pos..end).ok_or_else(truncated)?;
        self.pos = end;
        Ok(slice)
    }

    fn u8(&mut self) -> io::Result<u8> {
        Ok(self.take(1)?[0])
    }

    fn u32(&mut self) -> io::Result<u32> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    fn u64(&mut self) -> io::Result<u64> {
        Ok(u64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }

    fn uleb128(&mut self) -> io::Result<()> {
        while self.u8()? & 0x80 != 0 {}
        Ok(())
    }

    /// Skips one serialized value.
    fn value(&mut self) -> io::Result<()> {
        match self.u8()? {
            FASL_TAG_T | FASL_TAG_F | FASL_TAG_NIL => Ok(()),
            FASL_TAG_LOOKUP | FASL_TAG_FIXNUM | FASL_TAG_CHAR | FASL_TAG_REF => {
                self.take(4).map(drop)
            }
            FASL_TAG_FLONUM | FASL_TAG_IMMEDIATE => self.take(8).map(drop),
            FASL_TAG_REF_INIT => {
                self.take(4)?;
                self.value()
            }
            FASL_TAG_PLIST => self.values(),
            FASL_TAG_DLIST => {
                let count = self.u32()?;
                self.value()?;
                (0..count).try_for_each(|_| self.value())
            }
            FASL_TAG_VECTOR | FASL_TAG_TUPLE => self.values(),
            FASL_TAG_BVECTOR => {
                let len = usize::try_from(self.u64()?).map_err(|_| truncated())?;
                self.take(len).map(drop)
            }
            FASL_TAG_BIGINT => {
                self.u8()?;
                let digits = self.u32()? as usize;
                self.take(digits.checked_mul(8).ok_or_else(truncated)?)
                    .map(drop)
            }
            FASL_TAG_RATIONAL | FASL_TAG_COMPLEX => {
                self.value()?;
                self.value()
            }
            FASL_TAG_SYNTAX => (0..5).try_for_each(|_| self.value()),
            FASL_TAG_KEYWORD => self.value(),
            FASL_TAG_RTD => {
                // name, uid, flags (u8), own field count (u32), fields, parent
                self.value()?;
                self.value()?;
                self.take(1)?;
                self.take(4)?;
                self.value()?;
                self.value()
            }
            FASL_TAG_RCD => (0..3).try_for_each(|_| self.value()),
            tag => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown FASL tag {tag} at byte {}", self.pos - 1),
            )),
        }
    }

    /// A count followed by that many values.
    fn values(&mut self) -> io::Result<()> {
        let count = self.u32()?;
        (0..count).try_for_each(|_| self.value())
    }
}

fn gzip_len(data: &[u8]) -> io::Result<usize> {
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(data)?;
    Ok(encoder.finish()?.len())
}

pub fn analyze(file: &[u8]) -> io::Result<Stats> {
    let image = fasl_payload(file);
    if !image.starts_with(FASL_MAGIC) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a FASL image",
        ));
    }
    let mut head = Cursor {
        bytes: image,
        pos: FASL_MAGIC.len(),
    };

    let compressed = head.u8()? == FASL_COMPRESSION_GZIP;

    let lites_start = head.pos;
    let lite_count = head.u32()? as usize;
    for _ in 0..lite_count {
        head.take(4)?;
        head.u8()?;
        let len = head.u32()? as usize;
        head.take(len)?;
    }
    let lites_bytes = &image[lites_start..head.pos];

    head.u32()?;
    let stored = head.u32()? as usize;
    let stored_bytes = head.take(stored)?;
    let mut inflated = Vec::new();
    let payload: &[u8] = if compressed {
        GzDecoder::new(stored_bytes).read_to_end(&mut inflated)?;
        &inflated
    } else {
        stored_bytes
    };

    let mut cur = Cursor {
        bytes: payload,
        pos: 0,
    };
    if cur.u8()? != FASL_TAG_PROGRAM {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "the image is not a program",
        ));
    }
    cur.u64()?;
    let blocks = cur.u32()? as usize;
    let mut bytes: [Vec<u8>; SECTION_NAMES.len()] = Default::default();
    for _ in 0..blocks {
        let mut section = |index: usize,
                           cur: &mut Cursor<'_>,
                           read: &dyn Fn(&mut Cursor<'_>) -> io::Result<()>|
         -> io::Result<()> {
            let start = cur.pos;
            read(cur)?;
            bytes[index].extend_from_slice(&payload[start..cur.pos]);
            Ok(())
        };
        section(0, &mut cur, &|c| c.take(8).map(drop))?;
        section(1, &mut cur, &|c| c.value())?;
        section(2, &mut cur, &|c| {
            let len = c.u32()? as usize;
            c.take(len).map(drop)
        })?;
        section(3, &mut cur, &|c| {
            let words = c.u32()? as usize;
            c.take(words * 4).map(drop)
        })?;
        section(4, &mut cur, &|c| {
            let words = c.u32()? as usize;
            c.take(words * 4).map(drop)
        })?;
        section(5, &mut cur, &|c| {
            let count = c.u32()?;
            for _ in 0..count {
                if c.u8()? == 0 {
                    c.value()?;
                } else {
                    c.take(4)?;
                }
            }
            Ok(())
        })?;
        section(6, &mut cur, &|c| c.values())?;
        section(7, &mut cur, &|c| {
            let runs = c.u32()?;
            for _ in 0..runs {
                c.uleb128()?;
                c.uleb128()?;
                c.u8()?;
                c.uleb128()?;
            }
            Ok(())
        })?;
    }
    cur.u32()?;

    let sections = SECTION_NAMES
        .iter()
        .zip(&bytes)
        .map(|(name, data)| {
            Ok(Section {
                name,
                raw: data.len(),
                gzip: gzip_len(data)?,
            })
        })
        .collect::<io::Result<Vec<_>>>()?;
    Ok(Stats {
        file: file.len(),
        uncompressed: lites_bytes.len() + payload.len(),
        stored_payload: stored,
        compressed,
        blocks,
        lites: Section {
            name: "lites",
            raw: lites_bytes.len(),
            gzip: gzip_len(lites_bytes)?,
        },
        lite_count,
        sections,
    })
}

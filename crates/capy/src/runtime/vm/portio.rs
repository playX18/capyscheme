//! Native helpers for the Scheme port layer (`lib/boot/*ports*.scm`).
//!
//! These operate on plain bytevectors/strings and know nothing about port
//! records, so the Scheme side keeps full control of buffering and state.
//! They exist to move per-character loops out of interpreted/compiled Scheme.

use crate::heap::Gc;
use crate::prelude::*;

/// Codec selectors shared with `lib/boot/ports-core.scm`.
const CODEC_LATIN1: i32 = 0;
const CODEC_UTF8: i32 = 1;

pub fn init_portio<'gc>(ctx: MutationContext<'gc>) {
    port_ops::register(ctx);
}

/// Result of decoding one UTF-8 scalar from a slice.
enum Decoded {
    Char(u32, usize),
    /// Valid prefix, but the slice ends before the sequence completes.
    Incomplete,
    /// Invalid: number of bytes of the maximal invalid subpart (1..=3).
    Invalid(usize),
}

#[inline]
fn decode_utf8(b: &[u8]) -> Decoded {
    let b0 = b[0];
    if b0 < 0x80 {
        return Decoded::Char(b0 as u32, 1);
    }
    let (len, min2, max2, init) = match b0 {
        0xC2..=0xDF => (2, 0x80, 0xBF, (b0 & 0x1F) as u32),
        0xE0 => (3, 0xA0, 0xBF, (b0 & 0x0F) as u32),
        0xE1..=0xEC | 0xEE..=0xEF => (3, 0x80, 0xBF, (b0 & 0x0F) as u32),
        0xED => (3, 0x80, 0x9F, (b0 & 0x0F) as u32),
        0xF0 => (4, 0x90, 0xBF, (b0 & 0x07) as u32),
        0xF1..=0xF3 => (4, 0x80, 0xBF, (b0 & 0x07) as u32),
        0xF4 => (4, 0x80, 0x8F, (b0 & 0x07) as u32),
        _ => return Decoded::Invalid(1),
    };
    let mut cp = init;
    for i in 1..len {
        if i >= b.len() {
            return Decoded::Incomplete;
        }
        let c = b[i];
        let (lo, hi) = if i == 1 { (min2, max2) } else { (0x80, 0xBF) };
        if c < lo || c > hi {
            return Decoded::Invalid(i);
        }
        cp = (cp << 6) | (c & 0x3F) as u32;
    }
    Decoded::Char(cp, len)
}

#[inline]
fn encode_utf8(cp: u32, out: &mut [u8]) -> usize {
    if cp < 0x80 {
        out[0] = cp as u8;
        1
    } else if cp < 0x800 {
        out[0] = 0xC0 | (cp >> 6) as u8;
        out[1] = 0x80 | (cp & 0x3F) as u8;
        2
    } else if cp < 0x10000 {
        out[0] = 0xE0 | (cp >> 12) as u8;
        out[1] = 0x80 | ((cp >> 6) & 0x3F) as u8;
        out[2] = 0x80 | (cp & 0x3F) as u8;
        3
    } else {
        out[0] = 0xF0 | (cp >> 18) as u8;
        out[1] = 0x80 | ((cp >> 12) & 0x3F) as u8;
        out[2] = 0x80 | ((cp >> 6) & 0x3F) as u8;
        out[3] = 0x80 | (cp & 0x3F) as u8;
        4
    }
}

#[scheme(path=capy)]
pub mod port_ops {
    /// `(%utf8-decode-char bv pos lim)` => code point | -1 (incomplete) |
    /// -(2+n) (invalid, n = bytes of the maximal invalid subpart).
    #[scheme(name = "%utf8-decode-char")]
    pub fn utf8_decode_char(bv: Gc<'gc, ByteVector>, pos: usize, lim: usize) -> i64 {
        let lim = lim.min(bv.len());
        if pos >= lim {
            return -1;
        }
        match decode_utf8(&bv.as_slice()[pos..lim]) {
            Decoded::Char(cp, _) => cp as i64,
            Decoded::Incomplete => -1,
            Decoded::Invalid(n) => -(2 + n as i64),
        }
    }

    /// `(%utf8-encode-char! bv pos ch)` => new position, or #f when the
    /// encoding does not fit.
    #[scheme(name = "%utf8-encode-char!")]
    pub fn utf8_encode_char(bv: Gc<'gc, ByteVector>, pos: usize, ch: char) -> Value<'gc> {
        let mut tmp = [0u8; 4];
        let n = encode_utf8(ch as u32, &mut tmp);
        if pos.checked_add(n).is_none_or(|e| e > bv.len()) {
            return Value::new(false);
        }
        // SAFETY: bounds checked above.
        unsafe {
            bv.as_slice_mut_unchecked()[pos..pos + n].copy_from_slice(&tmp[..n]);
        }
        (pos + n).into_value(ctx)
    }

    /// `(%encode-string! codec bv pos str start end)` =>
    /// (new-pos new-start newlines tail).
    ///
    /// `newlines` counts U+000A among the encoded characters, `tail` is the
    /// number of encoded characters after the last one (-1 if none).
    ///
    /// Encodes characters of `str[start..end)` into `bv` from `pos` as long as
    /// each whole character fits. For latin-1 it also stops at the first
    /// character above U+00FF (the caller applies the error mode).
    #[scheme(name = "%encode-string!")]
    pub fn encode_string(
        codec: i32,
        bv: Gc<'gc, ByteVector>,
        pos: usize,
        s: Gc<'gc, Str<'gc>>,
        start: usize,
        end: usize,
    ) -> (usize, usize, usize, i64) {
        let end = end.min(s.len());
        let buf_len = bv.len();
        if pos > buf_len || start > end {
            return (pos, start, 0, -1);
        }
        // SAFETY: exclusive access for the duration of the call.
        let out = unsafe { bv.as_slice_mut_unchecked() };
        let mut p = pos;
        let mut i = start;
        let mut newlines = 0usize;
        let mut tail: i64 = -1;
        let mut put = |cp: u32, p: &mut usize| -> bool {
            if codec == CODEC_LATIN1 {
                if cp > 0xFF || *p >= buf_len {
                    return false;
                }
                out[*p] = cp as u8;
                *p += 1;
                true
            } else {
                let need = if cp < 0x80 {
                    1
                } else if cp < 0x800 {
                    2
                } else if cp < 0x10000 {
                    3
                } else {
                    4
                };
                if *p + need > buf_len {
                    return false;
                }
                *p += encode_utf8(cp, &mut out[*p..]);
                true
            }
        };
        let mut note = |cp: u32| {
            if cp == 0x0A {
                newlines += 1;
                tail = 0;
            } else if tail >= 0 {
                tail += 1;
            }
        };
        if let Some(chars) = s.chars() {
            while i < end {
                let cp = chars[i] as u32;
                if !put(cp, &mut p) {
                    break;
                }
                note(cp);
                i += 1;
            }
        } else if let Some(chars) = s.wide_chars() {
            while i < end {
                let cp = chars[i] as u32;
                if !put(cp, &mut p) {
                    break;
                }
                note(cp);
                i += 1;
            }
        }
        (p, i, newlines, tail)
    }

    /// `(%decode-string codec bv start end max-chars nl-mode stop-eol?)`
    /// => (string bytes-consumed newlines tail)
    ///
    /// Decodes valid characters from `bv[start..end)`. Stops at: `max-chars`;
    /// a linefeed when `nl-mode` is 1 (linefeed included in the result) or 2
    /// (linefeed consumed but left out of the result); just before CR / NEL /
    /// LS when `stop-eol?` (those need end-of-line translation by the
    /// caller); an invalid or truncated sequence. `newlines` counts U+000A
    /// consumed; `tail` is the number of characters in the result after the
    /// last linefeed, or -1 when there is none.
    #[scheme(name = "%decode-string")]
    pub fn decode_string(
        codec: i32,
        bv: Gc<'gc, ByteVector>,
        start: usize,
        end: usize,
        max_chars: usize,
        nl_mode: i32,
        stop_eol: bool,
    ) -> (Gc<'gc, Str<'gc>>, usize, usize, i64) {
        let end = end.min(bv.len());
        let bytes: &[u8] = if start <= end {
            &bv.as_slice()[start..end]
        } else {
            &[]
        };
        let mut out = String::new();
        let mut i = 0usize;
        let mut chars = 0usize;
        let mut newlines = 0usize;
        let mut tail: i64 = -1;
        while i < bytes.len() && chars < max_chars {
            let (cp, len) = if codec == CODEC_LATIN1 {
                (bytes[i] as u32, 1)
            } else if bytes[i] < 0x80 {
                (bytes[i] as u32, 1)
            } else {
                match decode_utf8(&bytes[i..]) {
                    Decoded::Char(cp, n) => (cp, n),
                    _ => break,
                }
            };
            if stop_eol && (cp == 0x0D || cp == 0x85 || cp == 0x2028) {
                break;
            }
            i += len;
            if cp == 0x0A {
                newlines += 1;
                tail = 0;
                if nl_mode == 2 {
                    break;
                }
                out.push('\n');
                chars += 1;
                if nl_mode == 1 {
                    break;
                }
                continue;
            }
            // decoder only yields scalar values
            out.push(char::from_u32(cp).unwrap_or('\u{FFFD}'));
            chars += 1;
            if tail >= 0 {
                tail += 1;
            }
        }
        let s = Str::new(ctx, &out, false);
        (s, i, newlines, tail)
    }

    /// `(%utf8->string bv start end)`: strict decode of a bytevector range.
    #[scheme(name = "%utf8->string")]
    pub fn utf8_range_to_string(
        bv: Gc<'gc, ByteVector>,
        start: usize,
        end: usize,
    ) -> Result<Gc<'gc, Str<'gc>>, Value<'gc>> {
        if start > end || end > bv.len() {
            return Err(call.wrong_argument_violation(
                "utf8->string",
                "range out of bounds",
                None,
                None,
                2,
                &[bv.into(), start.into_value(ctx), end.into_value(ctx)],
            ));
        }
        match std::str::from_utf8(&bv.as_slice()[start..end]) {
            Ok(s) => Ok(Str::new(ctx, s, false)),
            Err(_) => Err(call.wrong_argument_violation(
                "utf8->string",
                "invalid UTF-8 bytevector",
                None,
                None,
                1,
                &[bv.into()],
            )),
        }
    }

    /// `(%bytevector-index bv start end byte)` => index or #f.
    #[scheme(name = "%bytevector-index")]
    pub fn bytevector_index(
        bv: Gc<'gc, ByteVector>,
        start: usize,
        end: usize,
        byte: u8,
    ) -> Value<'gc> {
        let end = end.min(bv.len());
        if start >= end {
            return Value::new(false);
        }
        match bv.as_slice()[start..end].iter().position(|&b| b == byte) {
            Some(i) => (start + i).into_value(ctx),
            None => Value::new(false),
        }
    }
}

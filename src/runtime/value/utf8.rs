pub const fn utf8_sizeof_ucs4(ucs4: u32) -> usize {
    if ucs4 < 0x80 {
        return 1;
    }

    if ucs4 < 0x800 {
        return 2;
    }

    if ucs4 < 0x10000 {
        return 3;
    }

    if ucs4 < 0x200000 {
        return 4;
    }

    unreachable!()
}

pub const fn utf8_byte_count(datum: u8) -> usize {
    if datum < 0x80 {
        return 1;
    }

    if datum < 0xC2 {
        return 1;
    }

    if datum < 0xE0 {
        return 2;
    }

    if datum < 0xF0 {
        return 3;
    }

    if datum < 0xF8 {
        return 4;
    }

    if datum < 0xfc {
        return 5;
    }

    unreachable!()
}

pub fn utf8_char_index_to_byte_offset(datum: &[u8], index: usize, limit: usize) -> Option<usize> {
    let mut n = 0;
    for _ in 0..index {
        if n >= limit {
            return None;
        }
        if n >= datum.len() {
            return None;
        }
        let byte_count = utf8_byte_count(datum[n]);
        n += byte_count;
    }
    if n >= limit { None } else { Some(n) }
}
pub fn cnvt_utf8_to_ucs4(utf8: &[u8]) -> Option<(u32, usize)> {
    if utf8.is_empty() {
        return None;
    }
    let first = utf8[0];
    if first < 0x80 {
        let sv = first as u32;
        if sv >= 0x80 {
            return None;
        }
        return Some((sv, 1));
    } else if first < 0xc2 {
        return None;
    } else if first < 0xe0 {
        if utf8.len() < 2 {
            return None;
        }
        let b1 = utf8[1];
        if b1 < 0x80 || b1 > 0xbf {
            return None;
        }
        let sv = (((first & 0x1f) as u32) << 6) | ((b1 & 0x3f) as u32);
        if sv < 0x80 || sv > 0x7ff {
            return None;
        }
        return Some((sv, 2));
    } else if first < 0xf0 {
        if utf8.len() < 3 {
            return None;
        }
        let b1 = utf8[1];
        let b2 = utf8[2];
        if b1 < 0x80 || b1 > 0xbf || b2 < 0x80 || b2 > 0xbf {
            return None;
        }
        let sv = (((first & 0x0f) as u32) << 12)
            | (((b1 & 0x3f) as u32) << 6)
            | ((b2 & 0x3f) as u32);
        if sv < 0x800 || sv > 0xffff {
            return None;
        }
        if sv >= 0xd800 && sv <= 0xdfff {
            return None;
        }
        return Some((sv, 3));
    } else if first < 0xf8 {
        if utf8.len() < 4 {
            return None;
        }
        let b1 = utf8[1];
        let b2 = utf8[2];
        let b3 = utf8[3];
        if b1 < 0x80 || b1 > 0xbf || b2 < 0x80 || b2 > 0xbf || b3 < 0x80 || b3 > 0xbf {
            return None;
        }
        let sv = (((first & 0x07) as u32) << 18)
            | (((b1 & 0x3f) as u32) << 12)
            | (((b2 & 0x3f) as u32) << 6)
            | ((b3 & 0x3f) as u32);
        if sv < 0x10000 || sv > 0x10ffff {
            return None;
        }
        return Some((sv, 4));
    }
    None
}

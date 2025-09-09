use crate::runtime::value::*;
use easy_bitfield::BitFieldTrait;
use rsgc::{
    Mutation, Rootable, Trace, barrier,
    cell::Lock,
    collection::Visitor,
    global::Global,
    mmtk::{
        AllocationSemantics,
        util::{Address, conversions::raw_align_up},
    },
    object::VTable,
};
use std::{cell::Cell, sync::OnceLock};

#[repr(C, align(8))]
pub(crate) struct Stringbuf {
    hdr: ScmHeader,
    pub(crate) length: usize,
    mutable: Cell<bool>,
    pad: [u8; 7],
    data: [u8; 0],
}

const STRINGBUF_TC16_WIDE: TypeCode16 = TypeCode16(TypeCode8::STRINGBUF.bits() as u16 + 1 * 256);
const STRINGBUF_TC16_NARROW: TypeCode16 = TypeCode16(TypeCode8::STRINGBUF.bits() as u16 + 2 * 256);

unsafe impl Tagged for Stringbuf {
    const TC16: &[TypeCode16] = &[STRINGBUF_TC16_WIDE, STRINGBUF_TC16_NARROW];
    const TC8: TypeCode8 = TypeCode8::STRINGBUF;
    const TYPE_NAME: &'static str = "#<stringbuf>";
}

unsafe impl Trace for Stringbuf {
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {
        // No pointers to trace
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

impl Stringbuf {
    const VT: &'static VTable = &VTable {
        instance_size: 0,
        alignment: std::mem::align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(|sb| unsafe {
            let sb = sb.to_address().as_ref::<Stringbuf>();
            let raw_size = sb.length
                * if sb.is_wide() {
                    std::mem::size_of::<char>()
                } else {
                    1
                }
                + std::mem::size_of::<Stringbuf>();

            raw_align_up(raw_size, align_of::<Stringbuf>())
        }),
        trace: |_, _| {},
        weak_proc: |_, _| {},
    };

    pub fn contents(&self) -> Address {
        Address::from_ptr(self.data.as_ptr())
    }

    fn is_wide(&self) -> bool {
        self.hdr.type_bits() == STRINGBUF_TC16_WIDE.0
    }

    fn is_narrow(&self) -> bool {
        self.hdr.type_bits() == STRINGBUF_TC16_NARROW.0
    }

    fn len(&self) -> usize {
        self.length
    }

    fn chars<'a>(&self) -> &'a [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    fn wide_chars<'a>(&self) -> &'a [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    fn wide_chars_mut<'a>(&self) -> &'a mut [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    fn chars_mut<'a>(&self) -> &'a mut [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    fn new<'gc>(mc: &Mutation<'gc>, length: usize, is_wide: bool) -> Gc<'gc, Self> {
        let size =
            std::mem::size_of::<Self>() + length * if is_wide { size_of::<char>() } else { 1 };
        let bytesize_data = length * if is_wide { size_of::<char>() } else { 1 };
        unsafe {
            let mut hdr = ScmHeader::new();
            hdr.set_type_bits(if is_wide {
                STRINGBUF_TC16_WIDE.0 as _
            } else {
                STRINGBUF_TC16_NARROW.0 as _
            });

            let stringbuf_ = mc.raw_allocate(
                size,
                align_of::<Self>(),
                &Self::VT,
                AllocationSemantics::Default,
            );

            stringbuf_.to_address().store(Stringbuf {
                hdr,
                length,
                mutable: Cell::new(false),
                pad: [0; 7],
                data: [],
            });

            let stringbuf = stringbuf_.to_address().as_mut_ref::<Self>();
            stringbuf
                .contents()
                .to_mut_ptr::<u8>()
                .write_bytes(0, bytesize_data);
            Gc::from_gcobj(stringbuf_)
        }
    }

    #[allow(dead_code)]
    fn wide<'gc>(&self, mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        if self.is_wide() {
            return unsafe { Gc::from_ptr(self) };
        }

        let lenght = self.len();
        let new_stringbuf = Stringbuf::new(mc, lenght, true);
        let chars = self.chars();
        let wide_chars = new_stringbuf.wide_chars_mut();

        for i in 0..self.len() {
            wide_chars[i] = chars[i] as char;
        }

        new_stringbuf
    }

    fn narrow<'gc>(&self, mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        if self.is_narrow() {
            return unsafe { Gc::from_ptr(self) };
        }

        if self.wide_chars().iter().any(|&c| c as u32 > 0xFF) {
            panic!("Cannot convert wide string to narrow string");
        }

        let lenght = self.len();
        let new_stringbuf = Stringbuf::new(mc, lenght, false);
        let wide_chars = self.wide_chars();
        let chars = new_stringbuf.chars_mut();

        for i in 0..self.len() {
            chars[i] = wide_chars[i] as u8;
        }

        new_stringbuf
    }

    fn is_mutable(&self) -> bool {
        self.mutable.get()
    }

    fn set_mutable(&self, mutable: bool) {
        self.mutable.set(mutable);
    }
}

static NULL_STRINGBUF: OnceLock<Global<Rootable!(Gc<'_, Stringbuf>)>> = OnceLock::new();

fn null_stringbuf<'gc>(mc: &Mutation<'gc>) -> Gc<'gc, Stringbuf> {
    *NULL_STRINGBUF
        .get_or_init(|| Global::new(Stringbuf::new(mc, 0, false)))
        .fetch(mc)
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct Str<'gc> {
    hdr: ScmHeader,
    pub(crate) stringbuf: Lock<Gc<'gc, Stringbuf>>,
    pub(crate) start: Cell<usize>,
    pub(crate) length: usize,
}

impl<'gc> Str<'gc> {
    #[doc(hidden)]
    pub fn strbuf(&self) -> Address {
        self.stringbuf.get().to_object_reference().to_raw_address()
    }

    /// Create a new string with the given UTF-8 contents.
    pub fn new(mc: &Mutation<'gc>, contents: impl AsRef<str>, read_only: bool) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let is_ascii = str.is_ascii();

        let len = if is_ascii {
            str.len()
        } else {
            str.chars().count()
        };
        let buf = if str.len() == 0 {
            null_stringbuf(mc)
        } else if is_ascii {
            Stringbuf::new(mc, len, false)
        } else {
            Stringbuf::new(mc, len, true)
        };

        if str.is_ascii() {
            let chars = buf.chars_mut();
            chars.copy_from_slice(str.as_bytes());
        } else {
            let chars = buf.wide_chars_mut();
            for (i, c) in str.chars().enumerate() {
                chars[i] = c;
            }
        }
        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(if read_only {
            TypeCode16::IMMUTABLE_STRING.0
        } else {
            TypeCode16::STRING.0
        });

        Gc::new(
            mc,
            Self {
                hdr,
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: len,
            },
        )
    }

    pub fn new_wide(
        mc: &Mutation<'gc>,
        contents: impl AsRef<str>,
        read_only: bool,
    ) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let buf = if str.len() == 0 {
            null_stringbuf(mc)
        } else {
            Stringbuf::new(mc, str.chars().count(), true)
        };

        let chars = buf.wide_chars_mut();
        for (i, c) in str.chars().enumerate() {
            chars[i] = c;
        }

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(if read_only {
            TypeCode16::IMMUTABLE_STRING.0
        } else {
            TypeCode16::STRING.0
        });

        Gc::new(
            mc,
            Self {
                hdr,
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: str.len(),
            },
        )
    }

    fn substring_with_immutable_stringbuf(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
        force_copy: bool,
        read_only: bool,
    ) -> Gc<'gc, Self> {
        let buf = this.stringbuf.get();
        let tag = if read_only {
            TypeCode16::IMMUTABLE_STRING
        } else {
            TypeCode16::STRING
        };

        let len = end - start;
        let start = this.start.get() + start;

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(tag.bits());

        if len == 0 {
            return Self::new(mc, "", read_only);
        } else if !force_copy && !buf.is_mutable() {
            return Gc::new(
                mc,
                Self {
                    hdr,
                    stringbuf: Lock::new(buf),
                    start: Cell::new(start),
                    length: len,
                },
            );
        } else {
            let (_new_buf, new_str) = if buf.is_wide() {
                let new_buf = Stringbuf::new(mc, len, true);
                new_buf
                    .wide_chars_mut()
                    .copy_from_slice(&buf.wide_chars()[start..start + len]);
                (
                    new_buf,
                    Gc::new(
                        mc,
                        Self {
                            hdr,
                            stringbuf: Lock::new(new_buf),
                            start: Cell::new(0),
                            length: len,
                        },
                    ),
                )
            } else {
                let new_buf = Stringbuf::new(mc, len, false);
                new_buf
                    .chars_mut()
                    .copy_from_slice(&buf.chars()[start..start + len]);
                (
                    new_buf,
                    Gc::new(
                        mc,
                        Self {
                            hdr,
                            stringbuf: Lock::new(new_buf),
                            start: Cell::new(0),
                            length: len,
                        },
                    ),
                )
            };

            new_str
        }
    }

    pub fn substring(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= this.length);
        assert!(start <= this.length);
        Self::substring_with_immutable_stringbuf(this, mc, start, end, false, false)
    }

    pub fn substring_copy(
        this: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= this.length);
        assert!(start <= this.length);
        Self::substring_with_immutable_stringbuf(this, mc, start, end, true, false)
    }

    pub fn try_narrow(this: Gc<'gc, Self>, mc: &Mutation<'gc>) -> bool {
        if this.is_narrow() {
            return true;
        }
        let buf = this.stringbuf.get().narrow(mc);
        barrier::field!(Gc::write(mc, this), Self, stringbuf)
            .unlock()
            .set(buf);
        this.stringbuf.get().is_narrow()
    }

    pub fn try_as_str(this: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Option<&'gc str> {
        if Self::try_narrow(this, mc) {
            let chars = this.chars().unwrap();
            let slice = unsafe { std::slice::from_raw_parts(chars.as_ptr(), this.len()) };
            Some(std::str::from_utf8(slice).unwrap())
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_narrow(&self) -> bool {
        self.stringbuf.get().is_narrow()
    }

    pub fn is_wide(&self) -> bool {
        self.stringbuf.get().is_wide()
    }

    pub fn is_mutable(&self) -> bool {
        self.hdr.type_bits() == TypeCode16::STRING.0
    }

    pub fn data(&self) -> Address {
        let addr = self.stringbuf.get().contents();
        let start = self.start.get();
        let offset = if self.stringbuf.get().is_wide() {
            start * size_of::<char>()
        } else {
            start
        };
        addr.add(offset)
    }

    pub fn chars<'a>(&self) -> Option<&'a [u8]> {
        if self.is_narrow() {
            Some(&self.stringbuf.get().chars()[self.start.get()..self.start.get() + self.length])
        } else {
            None
        }
    }

    pub fn wide_chars<'a>(&self) -> Option<&'a [char]> {
        if self.is_wide() {
            Some(
                &self.stringbuf.get().wide_chars()
                    [self.start.get()..self.start.get() + self.length],
            )
        } else {
            None
        }
    }

    pub fn chars_mut<'a>(&self) -> Option<&'a mut [u8]> {
        if self.is_narrow() {
            Some(
                &mut self.stringbuf.get().chars_mut()
                    [self.start.get()..self.start.get() + self.length],
            )
        } else {
            None
        }
    }

    pub fn wide_chars_mut<'a>(&self) -> Option<&'a mut [char]> {
        if self.is_wide() {
            Some(
                &mut self.stringbuf.get().wide_chars_mut()
                    [self.start.get()..self.start.get() + self.length],
            )
        } else {
            None
        }
    }

    pub fn ensure_mutable(this: Gc<'gc, Self>, mc: &Mutation<'gc>) -> bool {
        if !this.is_mutable() {
            return false;
        }

        let buf = this.stringbuf.get();

        if buf.is_mutable() {
            return true;
        }

        let len = buf.len();
        let new_buf = if buf.is_wide() {
            let new_buf = Stringbuf::new(mc, len, true);
            new_buf.wide_chars_mut().copy_from_slice(buf.wide_chars());
            new_buf
        } else {
            let new_buf = Stringbuf::new(mc, len, false);
            new_buf.chars_mut().copy_from_slice(buf.chars());
            new_buf
        };

        new_buf.set_mutable(true);
        barrier::field!(Gc::write(mc, this), Self, stringbuf)
            .unlock()
            .set(new_buf);
        true
    }

    /// Get the character at the given index.
    ///
    /// Complexity: O(1)
    pub fn get(&self, index: usize) -> Option<char> {
        self.chars()
            .and_then(|chars| {
                if index < chars.len() {
                    Some(chars[index] as char)
                } else {
                    None
                }
            })
            .or_else(|| {
                self.wide_chars().and_then(|chars| {
                    if index < chars.len() {
                        Some(chars[index])
                    } else {
                        None
                    }
                })
            })
    }

    /// Assignment to the character at the given index.
    ///
    /// ## Complexity
    ///
    /// - O(1) if the string is narrow and the character is a valid ASCII character.
    /// - O(n) if the string is ASCII but the character is UTF-8
    pub fn set(this: Gc<'gc, Self>, mc: &Mutation<'gc>, index: usize, value: char) {
        if value as u32 > 0xff && this.is_narrow() {
            let new_buf = this.stringbuf.get().wide(mc);
            barrier::field!(Gc::write(mc, this), Self, stringbuf)
                .unlock()
                .set(new_buf);
        }

        if this.is_narrow() {
            this.chars_mut().unwrap()[index] = value as u8;
        } else {
            this.wide_chars_mut().unwrap()[index] = value;
        }
    }

    pub fn contains_char(&self, c: char) -> bool {
        if self.is_narrow() && c.is_ascii() {
            self.chars()
                .map_or(false, |chars| chars.contains(&(c as u8)))
        } else {
            self.wide_chars().map_or(false, |chars| chars.contains(&c))
        }
    }

    pub fn compare(
        &self,
        other: &Self,
        ci: bool,
        start1: usize,
        start2: usize,
        end1: usize,
        end2: usize,
    ) -> Option<std::cmp::Ordering> {
        compare_strings(ci, self, other, start1, start2, end1, end2)
    }
}

unsafe impl<'gc> Tagged for Str<'gc> {
    const TC16: &'static [TypeCode16] = &[TypeCode16::STRING, TypeCode16::IMMUTABLE_STRING];
    const TC8: TypeCode8 = TypeCode8::STRING;
    const TYPE_NAME: &'static str = "string";
}

fn compare_strings<'gc>(
    ci: bool,
    s1: &Str<'gc>,
    s2: &Str<'gc>,
    start1: usize,
    start2: usize,
    end1: usize,
    end2: usize,
) -> Option<std::cmp::Ordering> {
    let mut cstart1;
    let mut cstart2;
    let cend1;
    let cend2;

    cstart1 = if start1 < s1.length {
        start1
    } else {
        return None;
    };

    cstart2 = if start2 < s2.length {
        start2
    } else {
        return None;
    };

    cend1 = if end1 <= s1.length {
        end1
    } else {
        return None;
    };

    cend2 = if end2 <= s2.length {
        end2
    } else {
        return None;
    };

    while cstart1 < cend1 && cstart2 < cend2 {
        if ci {
            let a = s1.get(cstart1).unwrap().to_uppercase().next().unwrap();
            let b = s2.get(cstart2).unwrap().to_uppercase().next().unwrap();
            if a < b {
                return Some(std::cmp::Ordering::Less);
            } else if a > b {
                return Some(std::cmp::Ordering::Greater);
            }
            cstart1 += 1;
            cstart2 += 1;
        } else {
            let a = s1.get(cstart1).unwrap();
            let b = s2.get(cstart2).unwrap();

            if a < b {
                return Some(std::cmp::Ordering::Less);
            } else if a > b {
                return Some(std::cmp::Ordering::Greater);
            }
            cstart1 += 1;
            cstart2 += 1;
        }
    }

    if cstart1 < cend1 {
        return Some(std::cmp::Ordering::Greater);
    } else if cstart2 < cend2 {
        return Some(std::cmp::Ordering::Less);
    } else {
        return Some(std::cmp::Ordering::Equal);
    }
}

impl<'gc> std::fmt::Debug for Str<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(chars) = self.chars() {
            write!(f, "{:?}", std::str::from_utf8(chars).unwrap())
        } else if let Some(wide_chars) = self.wide_chars() {
            let mut str = std::string::String::new();
            for c in wide_chars {
                str.push(*c);
            }
            write!(f, "{:?}", str)
        } else {
            unreachable!()
        }
    }
}

impl<'gc> std::fmt::Display for Str<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(chars) = self.chars() {
            write!(f, "{}", std::str::from_utf8(chars).unwrap())
        } else if let Some(wide_chars) = self.wide_chars() {
            let mut str = std::string::String::new();
            for c in wide_chars {
                str.push(*c);
            }
            write!(f, "{}", str)
        } else {
            unreachable!()
        }
    }
}

impl<'gc> PartialEq for Str<'gc> {
    fn eq(&self, other: &Self) -> bool {
        compare_strings(false, self, other, 0, 0, self.len(), other.len())
            == Some(std::cmp::Ordering::Equal)
    }
}

impl<'gc> Eq for Str<'gc> {}

impl<'gc> PartialOrd for Str<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        compare_strings(false, self, other, 0, 0, self.len(), other.len())
    }
}

impl<'gc> Ord for Str<'gc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_strings(false, self, other, 0, 0, self.len(), other.len()).unwrap()
    }
}

impl<'gc> PartialEq<str> for Str<'gc> {
    fn eq(&self, other: &str) -> bool {
        if let Some(chars) = self.chars() {
            return std::str::from_utf8(chars).unwrap() == other;
        }

        if let Some(wide_chars) = self.wide_chars() {
            return wide_chars.iter().collect::<String>() == other;
        }

        false
    }
}

impl<'gc> Hash for Str<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.is_narrow() {
            state.write_u8(0xde);
            for c in self.chars().unwrap() {
                state.write_u8(*c);
            }
        } else {
            state.write_u8(0xee);
            for c in self.wide_chars().unwrap() {
                state.write_u32(*c as u32);
            }
        }
    }
}

impl<'gc> Symbol<'gc> {
    pub(super) fn new<const INTERNED: bool>(
        mc: &Mutation<'gc>,
        mut name: Gc<'gc, Str<'gc>>,
        hash: u64,
        prefix_offset: Option<u16>,
    ) -> Gc<'gc, Self> {
        let len = name.len();
        let start = name.start.get();
        let mut buf = name.stringbuf.get();
        if buf.is_mutable() || start != 0 || buf.len() != len {
            name = Str::substring_copy(name, mc, 0, len);
            buf = name.stringbuf.get();
        }

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(if INTERNED {
            SYMBOL_TC16_INTERNED.bits()
        } else {
            SYMBOL_TC16_UNINTERNED.bits()
        });

        if let Some(offset) = prefix_offset {
            hdr.word |= SymbolPrefixOffsetBits::encode(offset);
        }

        let symbol = Gc::new(
            mc,
            Self {
                header: hdr,
                stringbuf: buf,
                hash: Cell::new(hash),
            },
        );

        symbol
    }

    pub fn len(&self) -> usize {
        self.stringbuf.len()
    }

    pub fn is_narrow(&self) -> bool {
        self.stringbuf.is_narrow()
    }

    pub fn chars(&self) -> Option<&[u8]> {
        if self.stringbuf.is_narrow() {
            Some(unsafe {
                std::slice::from_raw_parts(self.stringbuf.contents().to_ptr(), self.len())
            })
        } else {
            None
        }
    }

    pub fn wide_chars(&self) -> Option<&[char]> {
        if self.stringbuf.is_wide() {
            Some(unsafe {
                std::slice::from_raw_parts(self.stringbuf.contents().to_ptr(), self.len())
            })
        } else {
            None
        }
    }

    pub fn substring(&self, mc: &Mutation<'gc>, start: usize, end: usize) -> Gc<'gc, Str<'gc>> {
        let buf = self.stringbuf;

        let mut hdr = ScmHeader::new();
        hdr.set_type_bits(TypeCode16::IMMUTABLE_STRING.bits());

        let string = Gc::new(
            mc,
            Str {
                hdr,
                stringbuf: Lock::new(buf),
                start: Cell::new(start),
                length: end - start,
            },
        );

        string
    }

    pub fn get(&self, index: usize) -> Option<char> {
        self.chars()
            .and_then(|chars| {
                if index < chars.len() {
                    Some(chars[index] as char)
                } else {
                    None
                }
            })
            .or_else(|| {
                self.wide_chars().and_then(|chars| {
                    if index < chars.len() {
                        Some(chars[index])
                    } else {
                        None
                    }
                })
            })
    }
}

impl<'gc> Hash for Symbol<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash.get());
    }
}

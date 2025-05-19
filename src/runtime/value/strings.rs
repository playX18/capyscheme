use std::{
    cell::Cell,
    hash::Hash,
    mem::MaybeUninit,
    sync::{
        OnceLock,
        atomic::{AtomicU16, Ordering},
    },
};

use rsgc::{
    Collect, EnsureGCInfo, GCInfo, GCInfoIndex, GCInfoIndexForT, Gc, Rootable, Trace, barrier,
    cell::Lock,
    context::Mutation,
    gc::GLOBAL_GC_INFO_TABLE,
    generic_static::Namespace,
    mutator::Global,
    vmkit::{
        mmtk::util::Address,
        prelude::{GCMetadata, TraceCallback},
    },
};

use super::{SYMBOL_TC16_INTERNED, Symbol, Tagged, TypeCode8, TypeCode16, Value, ValuesNamespace};

#[repr(C, align(8))]
pub(super) struct Stringbuf {
    length: usize,
    mutable: Cell<bool>,
    pad: [u8; 7],
    data: [u8; 0],
}

unsafe impl Trace for Stringbuf {
    fn trace(&mut self, _visitor: &mut rsgc::Visitor<'_>) {
        // No pointers to trace
    }
}

impl Stringbuf {
    pub fn contents(&self) -> Address {
        Address::from_ptr(self.data.as_ptr())
    }

    fn is_wide(self: Gc<'_, Self>) -> bool {
        let value: Value<'static> =
            Value::from_raw_i64(self.as_vmkit_object().as_address().as_usize() as i64);
        value.has_typ16(STRINGBUF_TC16_WIDE)
    }

    fn is_narrow(self: Gc<'_, Self>) -> bool {
        let value: Value<'static> =
            Value::from_raw_i64(self.as_vmkit_object().as_address().as_usize() as i64);
        value.has_typ16(STRINGBUF_TC16_NARROW)
    }

    fn len(&self) -> usize {
        self.length
    }

    fn chars<'a>(self: Gc<'_, Self>) -> &'a [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    fn wide_chars<'a>(self: Gc<'_, Self>) -> &'a [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    fn wide_chars_mut<'a>(self: Gc<'_, Self>) -> &'a mut [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    fn chars_mut<'a>(self: Gc<'_, Self>) -> &'a mut [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    fn new<'gc>(mc: &Mutation<'gc>, length: usize, is_wide: bool) -> Gc<'gc, Self> {
        let size =
            std::mem::size_of::<Self>() + length * if is_wide { size_of::<char>() } else { 1 };
        let bytesize_data = length * if is_wide { size_of::<char>() } else { 1 };
        unsafe {
            let layout =
                std::alloc::Layout::from_size_align_unchecked(size, std::mem::align_of::<Self>());
            let stringbuf = mc.allocate_with_layout::<Self>(layout);
            stringbuf.set_user_header(if is_wide {
                STRINGBUF_TC16_WIDE.into()
            } else {
                STRINGBUF_TC16_NARROW.into()
            });
            stringbuf.as_ptr().write(MaybeUninit::new(Stringbuf {
                length,
                pad: [0; 7],
                data: [],
                mutable: Cell::new(false),
            }));

            let stringbuf = stringbuf.assume_init();
            stringbuf
                .contents()
                .to_mut_ptr::<u8>()
                .write_bytes(0, bytesize_data);
            stringbuf
        }
    }

    #[allow(dead_code)]
    fn wide<'gc>(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        if self.is_wide() {
            return self;
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

    fn narrow<'gc>(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        if self.is_narrow() {
            return self;
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

    fn is_mutable<'gc>(self: Gc<'gc, Self>) -> bool {
        self.mutable.get()
    }

    fn set_mutable<'gc>(self: Gc<'gc, Self>, mutable: bool) {
        self.mutable.set(mutable);
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Stringbuf {
    fn ensure_gc_info() -> GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<Stringbuf>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_stringbuf_info(&registered_index.index)
    }
}

const STRINGBUF_TC16_WIDE: TypeCode16 = TypeCode16(TypeCode8::STRINGBUF.bits() as u16 + 1 * 256);
const STRINGBUF_TC16_NARROW: TypeCode16 = TypeCode16(TypeCode8::STRINGBUF.bits() as u16 + 2 * 256);

fn register_stringbuf_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: align_of::<Value<'static>>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let vec: Gc<'_, Stringbuf> = unsafe { Gc::from_vmkit_object(object) };
                    let value: Value<'static> =
                        Value::from_raw_i64(object.as_address().as_usize() as i64);

                    let is_wide = value.has_typ16(STRINGBUF_TC16_WIDE);

                    vec.length * if is_wide { size_of::<char>() } else { 1 }
                }),
                instance_size: size_of::<Stringbuf>(),
                trace: TraceCallback::None,
            },
        },
    )
}

unsafe impl Tagged for Stringbuf {
    const TC8: TypeCode8 = TypeCode8::STRINGBUF;
    const TC16: &'static [TypeCode16] = &[STRINGBUF_TC16_WIDE, STRINGBUF_TC16_NARROW];
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct String<'gc> {
    stringbuf: Lock<Gc<'gc, Stringbuf>>,
    start: Cell<usize>,
    length: usize,
}

static NULL_STRINGBUF: OnceLock<Global<Rootable!(Gc<'_, Stringbuf>)>> = OnceLock::new();

fn null_stringbuf<'gc>(mc: &Mutation<'gc>) -> Gc<'gc, Stringbuf> {
    *NULL_STRINGBUF
        .get_or_init(|| Global::new(Stringbuf::new(mc, 0, false)))
        .fetch(mc)
}

impl<'gc> String<'gc> {
    /// Create a new string with the given UTF-8 contents.
    pub fn new(mc: &Mutation<'gc>, contents: impl AsRef<str>, read_only: bool) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let is_ascii = str.is_ascii();
        let buf = if str.len() == 0 {
            null_stringbuf(mc)
        } else {
            Stringbuf::new(mc, str.len(), !is_ascii)
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

        let string = Gc::new(
            mc,
            Self {
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: str.len(),
            },
        );
        string.set_user_header(if read_only {
            TypeCode16::IMMUTABLE_STRING.into()
        } else {
            TypeCode16::STRING.into()
        });

        string
    }

    pub fn new_wide(mc: &Mutation<'gc>, contents: impl AsRef<str>) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let buf = if str.len() == 0 {
            null_stringbuf(mc)
        } else {
            Stringbuf::new(mc, str.len(), true)
        };
        let chars = buf.wide_chars_mut();
        for (i, c) in str.chars().enumerate() {
            chars[i] = c;
        }

        Gc::new(
            mc,
            Self {
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: str.len(),
            },
        )
    }

    fn substring_with_immutable_stringbuf(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
        force_copy: bool,
        read_only: bool,
    ) -> Gc<'gc, Self> {
        let buf = self.stringbuf.get();
        let tag = if read_only {
            TypeCode16::IMMUTABLE_STRING
        } else {
            TypeCode16::STRING
        };

        let len = end - start;
        let start = self.start.get() + start;

        if len == 0 {
            return Self::new(mc, "", read_only);
        } else if !force_copy && !buf.is_mutable() {
            return Gc::new(
                mc,
                Self {
                    stringbuf: Lock::new(buf),
                    start: Cell::new(start),
                    length: len,
                },
            );
        } else {
            let (new_buf, new_str) = if buf.is_wide() {
                let new_buf = Stringbuf::new(mc, len, true);
                new_buf
                    .wide_chars_mut()
                    .copy_from_slice(&buf.wide_chars()[start..start + len]);
                (
                    new_buf,
                    Gc::new(
                        mc,
                        Self {
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
                            stringbuf: Lock::new(new_buf),
                            start: Cell::new(0),
                            length: len,
                        },
                    ),
                )
            };
            let _ = new_buf;
            new_str.set_user_header(tag.into());
            new_str
        }
    }

    pub fn substring(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= self.length);
        assert!(start <= self.length);
        self.substring_with_immutable_stringbuf(mc, start, end, false, false)
    }

    pub fn substring_readonly(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= self.length);
        assert!(start <= self.length);
        self.substring_with_immutable_stringbuf(mc, start, end, false, true)
    }

    pub fn substring_copy(
        self: Gc<'gc, Self>,
        mc: &Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= self.length);
        assert!(start <= self.length);
        self.substring_with_immutable_stringbuf(mc, start, end, true, false)
    }

    pub fn try_narrow(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> bool {
        if self.is_narrow() {
            return true;
        }
        let buf = self.stringbuf.get().narrow(mc);
        barrier::field!(Gc::write(mc, self), Self, stringbuf)
            .unlock()
            .set(buf);
        self.stringbuf.get().is_narrow()
    }

    pub fn try_as_str(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Option<&'gc str> {
        if self.try_narrow(mc) {
            let chars = self.chars().unwrap();
            let slice = unsafe { std::slice::from_raw_parts(chars.as_ptr(), self.len()) };
            Some(std::str::from_utf8(slice).unwrap())
        } else {
            None
        }
    }

    pub fn is_narrow(&self) -> bool {
        self.stringbuf.get().is_narrow()
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_mutable(self: Gc<'gc, Self>) -> bool {
        let value: Value<'gc> = Value::from(self);

        value.has_typ16(TypeCode16::STRING)
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
        if self.stringbuf.get().is_narrow() {
            Some(unsafe { std::slice::from_raw_parts(self.data().to_ptr(), self.len()) })
        } else {
            None
        }
    }

    pub fn wide_chars<'a>(&self) -> Option<&'a [char]> {
        if self.stringbuf.get().is_wide() {
            Some(unsafe { std::slice::from_raw_parts(self.data().to_ptr(), self.len()) })
        } else {
            None
        }
    }

    pub fn chars_mut<'a>(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Option<&'a mut [u8]> {
        if self.stringbuf.get().is_narrow() && self.ensure_mutable(mc) {
            Some(unsafe { std::slice::from_raw_parts_mut(self.data().to_mut_ptr(), self.len()) })
        } else {
            None
        }
    }

    pub fn wide_chars_mut<'a>(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> Option<&'a mut [char]> {
        if self.stringbuf.get().is_wide() && self.ensure_mutable(mc) {
            Some(unsafe { std::slice::from_raw_parts_mut(self.data().to_mut_ptr(), self.len()) })
        } else {
            None
        }
    }

    pub fn ensure_mutable(self: Gc<'gc, Self>, mc: &Mutation<'gc>) -> bool {
        if !self.is_mutable() {
            return false;
        }

        let buf = self.stringbuf.get();

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
        barrier::field!(Gc::write(mc, self), Self, stringbuf)
            .unlock()
            .set(new_buf);
        true
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

    pub fn set(self: Gc<'gc, Self>, mc: &Mutation<'gc>, index: usize, value: char) {
        if value as u32 > 0xff && self.is_narrow() {
            let new_buf = self.stringbuf.get().wide(mc);
            barrier::field!(Gc::write(mc, self), Self, stringbuf)
                .unlock()
                .set(new_buf);
        }

        if self.is_narrow() {
            self.chars_mut(mc).unwrap()[index] = value as u8;
        } else {
            self.wide_chars_mut(mc).unwrap()[index] = value;
        }
    }

    pub fn contains_char(&self, c: char) -> bool {
        if self.is_narrow() {
            self.chars()
                .map_or(false, |chars| chars.contains(&(c as u8)))
        } else {
            self.wide_chars().map_or(false, |chars| chars.contains(&c))
        }
    }
}

impl<'gc> PartialEq<str> for String<'_> {
    fn eq(&self, other: &str) -> bool {
        self.chars()
            .and_then(|chars| Some(chars == other.as_bytes()))
            .or_else(|| {
                self.wide_chars().and_then(|chars| {
                    let bytelen = chars.iter().map(|c| c.len_utf8()).sum::<usize>();
                    if bytelen != other.len() {
                        return None;
                    }

                    let mut i = 0;
                    for c in chars {
                        let mut buf = [0; 4];
                        let len = c.encode_utf8(&mut buf).len();
                        if buf[..len] != other.as_bytes()[i..i + len] {
                            return None;
                        }
                        i += len;
                    }
                    Some(true)
                })
            })
            .unwrap_or(false)
    }
}

unsafe impl<'gc> Tagged for String<'gc> {
    const TC8: TypeCode8 = TypeCode8::STRING;
    const TC16: &'static [TypeCode16] = &[TypeCode16::STRING, TypeCode16::IMMUTABLE_STRING];
}

pub fn init_strings<'gc>(mc: &Mutation<'gc>) {
    NULL_STRINGBUF.get_or_init(|| Global::new(Stringbuf::new(mc, 0, false)));
}

impl<'gc> PartialEq for String<'gc> {
    fn eq(&self, other: &Self) -> bool {
        compare_strings(false, self, other, 0, 0, self.len(), other.len())
            == Some(std::cmp::Ordering::Equal)
    }
}

impl<'gc> Eq for String<'gc> {}

impl<'gc> PartialOrd for String<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        compare_strings(false, self, other, 0, 0, self.len(), other.len())
    }
}

impl<'gc> Ord for String<'gc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_strings(false, self, other, 0, 0, self.len(), other.len()).unwrap()
    }
}

impl<'gc> Hash for String<'gc> {
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

fn compare_strings<'gc>(
    ci: bool,
    s1: &String<'gc>,
    s2: &String<'gc>,
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

    cend1 = if end1 < s1.length && end1 >= start1 {
        end1
    } else {
        return None;
    };

    cend2 = if end2 < s2.length && end2 >= start2 {
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

impl<'gc> std::fmt::Debug for String<'gc> {
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

impl<'gc> std::fmt::Display for String<'gc> {
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

impl<'gc> Symbol<'gc> {
    pub(super) fn new(
        mc: &Mutation<'gc>,
        mut name: Gc<'gc, String<'gc>>,
        hash: u64,
    ) -> Gc<'gc, Self> {
        let len = name.len();
        let start = name.start.get();
        let mut buf = name.stringbuf.get();
        if buf.is_mutable() || start != 0 || buf.len() != len {
            name = name.substring_copy(mc, 0, len);
            buf = name.stringbuf.get();
        }

        let symbol = Gc::new(
            mc,
            Self {
                stringbuf: buf,
                hash: Cell::new(hash),
            },
        );

        symbol.set_user_header(SYMBOL_TC16_INTERNED.into());
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

    pub fn substring(&self, mc: &Mutation<'gc>, start: usize, end: usize) -> Gc<'gc, String<'gc>> {
        let buf = self.stringbuf;

        let string = Gc::new(
            mc,
            String {
                stringbuf: Lock::new(buf),
                start: Cell::new(start),
                length: end - start,
            },
        );

        string.set_user_header(TypeCode16::IMMUTABLE_STRING.into());

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

//! String and string-buffer heap objects.

use crate::runtime::value::*;

use crate::rsgc::{
    Mutation, Trace, barrier,
    cell::Lock,
    collection::Visitor,
    global::Global,
    mmtk::{
        AllocationSemantics,
        util::{Address, conversions::raw_align_up},
    },
    object::{AllocationHooks, ClassId, builtin_class_ids, class_header_word},
};
use std::{
    cell::{Cell, UnsafeCell},
    sync::OnceLock,
};

#[repr(C, align(8))]
pub(crate) struct Stringbuf {
    pub(crate) length: usize,
    pub(crate) mutable: Cell<bool>,
    pad: [u8; 7],
    data: [UnsafeCell<u8>; 0],
}

fn stringbuf_header_word(is_wide: bool) -> u64 {
    let class_id = if is_wide {
        builtin_class_ids::STRINGBUF_WIDE
    } else {
        builtin_class_ids::STRINGBUF_NARROW
    };

    class_header_word(ClassId::new(class_id).unwrap())
}

unsafe impl ClassTagged for Stringbuf {
    const CLASS_IDS: &'static [u32] = &[
        crate::rsgc::object::builtin_class_ids::STRINGBUF_WIDE,
        crate::rsgc::object::builtin_class_ids::STRINGBUF_NARROW,
    ];
    const TYPE_NAME: &'static str = "#<stringbuf>";
}

unsafe impl Trace for Stringbuf {
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {
        // No pointers to trace
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

extern "C" fn compute_stringbuf_size(sb: GCObject) -> usize {
    unsafe {
        let sb = sb.to_address().as_ref::<Stringbuf>();
        let raw_size = sb.length
            * if sb.is_wide() {
                std::mem::size_of::<char>()
            } else {
                1
            };

        raw_align_up(raw_size, align_of::<Stringbuf>())
    }
}

extern "C" fn trace_stringbuf(_: GCObject, _: &mut Visitor) {}

extern "C" fn process_weak_stringbuf(_: GCObject, _: &mut crate::rsgc::WeakProcessor) {}

impl Stringbuf {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        type_name: "Stringbuf",
        instance_size: size_of::<Self>(),
        alignment: std::mem::align_of::<Self>(),
        compute_alignment: None,
        compute_size: Some(compute_stringbuf_size),
        trace: trace_stringbuf,
        weak_proc: process_weak_stringbuf,
    };

    pub fn contents(&self) -> Address {
        Address::from_ptr(self.data.as_ptr())
    }

    pub(crate) fn is_wide(&self) -> bool {
        payload_class_id(self).bits() == builtin_class_ids::STRINGBUF_WIDE
    }

    pub(crate) fn is_narrow(&self) -> bool {
        payload_class_id(self).bits() == builtin_class_ids::STRINGBUF_NARROW
    }

    pub(crate) fn len(&self) -> usize {
        self.length
    }

    pub(crate) fn chars<'a>(&self) -> &'a [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    pub(crate) fn wide_chars<'a>(&self) -> &'a [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts(self.contents().to_ptr(), self.len()) }
    }

    pub(crate) fn wide_chars_mut<'a>(&self) -> &'a mut [char] {
        assert!(self.is_wide());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    pub(crate) fn chars_mut<'a>(&self) -> &'a mut [u8] {
        assert!(self.is_narrow());
        unsafe { std::slice::from_raw_parts_mut(self.contents().to_mut_ptr(), self.len()) }
    }

    pub(crate) fn new<'gc>(mc: Mutation<'gc>, length: usize, is_wide: bool) -> Gc<'gc, Self> {
        let size =
            std::mem::size_of::<Self>() + length * if is_wide { size_of::<char>() } else { 1 };
        let bytesize_data = length * if is_wide { size_of::<char>() } else { 1 };
        unsafe {
            let stringbuf_ = mc.raw_allocate_with_header_word(
                size,
                align_of::<Self>(),
                stringbuf_header_word(is_wide),
                AllocationSemantics::Default,
            );

            stringbuf_.to_address().store(Stringbuf {
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
    pub(crate) fn wide<'gc>(&self, mc: Mutation<'gc>) -> Gc<'gc, Self> {
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

    fn narrow<'gc>(&self, mc: Mutation<'gc>) -> Gc<'gc, Self> {
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

    pub(crate) fn is_mutable(&self) -> bool {
        self.mutable.get()
    }

    pub(crate) fn set_mutable(&self, mutable: bool) {
        self.mutable.set(mutable);
    }
}

type RootedStringbuf = crate::Rootable!(Gc<'_, Stringbuf>);

static NULL_STRINGBUF: OnceLock<Global<RootedStringbuf>> = OnceLock::new();

fn null_stringbuf<'gc>(mc: Mutation<'gc>) -> Gc<'gc, Stringbuf> {
    *NULL_STRINGBUF
        .get_or_init(|| Global::new(Stringbuf::new(mc, 0, false)))
        .fetch(mc)
}

#[derive(Trace)]
#[collect(no_drop)]
/// A Scheme string view over a shared string buffer.
pub struct Str<'gc> {
    pub(crate) stringbuf: Lock<Gc<'gc, Stringbuf>>,
    pub(crate) start: Cell<usize>,
    pub(crate) length: usize,
}

fn string_header_word(read_only: bool) -> u64 {
    let class_id = if read_only {
        builtin_class_ids::IMMUTABLE_STRING
    } else {
        builtin_class_ids::STRING
    };

    class_header_word(ClassId::new(class_id).unwrap())
}

impl<'gc> Str<'gc> {
    #[doc(hidden)]
    pub fn strbuf(&self) -> Address {
        self.stringbuf.get().to_object_reference().to_raw_address()
    }

    /// Allocates a mutable string from UTF-8 contents.
    pub fn from_str(mc: Mutation<'gc>, s: impl AsRef<str>) -> Gc<'gc, Self> {
        Self::new(mc, s, false)
    }

    /// Allocates a string with the given UTF-8 contents.
    pub fn new(mc: Mutation<'gc>, contents: impl AsRef<str>, read_only: bool) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let is_ascii = str.is_ascii();

        let len = if is_ascii {
            str.len()
        } else {
            str.chars().count()
        };
        let buf = if str.is_empty() {
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
        Gc::new_with_header_word(
            mc,
            Self {
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: len,
            },
            string_header_word(read_only),
        )
    }

    /// Allocates a mutable string by repeating one character.
    pub fn from_char(mc: Mutation<'gc>, c: char, count: usize) -> Gc<'gc, Self> {
        if c as u32 <= 0xff {
            let buf = if count == 0 {
                null_stringbuf(mc)
            } else {
                Stringbuf::new(mc, count, false)
            };
            let chars = buf.chars_mut();
            unsafe {
                std::ptr::write_bytes(chars.as_mut_ptr(), c as u8, count);
            }

            Gc::new_with_header_word(
                mc,
                Self {
                    stringbuf: Lock::new(buf),
                    start: Cell::new(0),
                    length: count,
                },
                string_header_word(false),
            )
        } else {
            let buf = if count == 0 {
                null_stringbuf(mc)
            } else {
                Stringbuf::new(mc, count, true)
            };
            let chars = buf.wide_chars_mut();
            for ch in chars.iter_mut().take(count) {
                *ch = c;
            }

            Gc::new_with_header_word(
                mc,
                Self {
                    stringbuf: Lock::new(buf),
                    start: Cell::new(0),
                    length: count,
                },
                string_header_word(false),
            )
        }
    }

    /// Allocates a string backed by a wide character buffer.
    pub fn new_wide(
        mc: Mutation<'gc>,
        contents: impl AsRef<str>,
        read_only: bool,
    ) -> Gc<'gc, Self> {
        let str = contents.as_ref();
        let buf = if str.is_empty() {
            null_stringbuf(mc)
        } else {
            Stringbuf::new(mc, str.chars().count(), true)
        };

        let chars = buf.wide_chars_mut();
        for (i, c) in str.chars().enumerate() {
            chars[i] = c;
        }

        Gc::new_with_header_word(
            mc,
            Self {
                stringbuf: Lock::new(buf),
                start: Cell::new(0),
                length: str.len(),
            },
            string_header_word(read_only),
        )
    }

    fn substring_with_immutable_stringbuf(
        this: Gc<'gc, Self>,
        mc: Mutation<'gc>,
        start: usize,
        end: usize,
        force_copy: bool,
        read_only: bool,
    ) -> Gc<'gc, Self> {
        let buf = this.stringbuf.get();
        let len = end - start;
        let start = this.start.get() + start;

        let header_word = string_header_word(read_only);

        if len == 0 {
            Self::new(mc, "", read_only)
        } else if !force_copy && !buf.is_mutable() {
            Gc::new_with_header_word(
                mc,
                Self {
                    stringbuf: Lock::new(buf),
                    start: Cell::new(start),
                    length: len,
                },
                header_word,
            )
        } else {
            let (_new_buf, new_str) = if buf.is_wide() {
                let new_buf = Stringbuf::new(mc, len, true);
                new_buf
                    .wide_chars_mut()
                    .copy_from_slice(&buf.wide_chars()[start..start + len]);
                (
                    new_buf,
                    Gc::new_with_header_word(
                        mc,
                        Self {
                            stringbuf: Lock::new(new_buf),
                            start: Cell::new(0),
                            length: len,
                        },
                        header_word,
                    ),
                )
            } else {
                let new_buf = Stringbuf::new(mc, len, false);
                new_buf
                    .chars_mut()
                    .copy_from_slice(&buf.chars()[start..start + len]);
                (
                    new_buf,
                    Gc::new_with_header_word(
                        mc,
                        Self {
                            stringbuf: Lock::new(new_buf),
                            start: Cell::new(0),
                            length: len,
                        },
                        header_word,
                    ),
                )
            };

            new_str
        }
    }

    pub fn substring(
        this: Gc<'gc, Self>,
        mc: Mutation<'gc>,
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
        mc: Mutation<'gc>,
        start: usize,
        end: usize,
    ) -> Gc<'gc, Self> {
        assert!(start <= end);
        assert!(end <= this.length);
        assert!(start <= this.length);
        Self::substring_with_immutable_stringbuf(this, mc, start, end, true, false)
    }

    pub fn try_narrow(this: Gc<'gc, Self>, mc: Mutation<'gc>) -> bool {
        if this.is_narrow() {
            return true;
        }
        let buf = this.stringbuf.get().narrow(mc);
        barrier::field!(Gc::write(mc, this), Self, stringbuf)
            .unlock()
            .set(buf);
        this.stringbuf.get().is_narrow()
    }

    pub fn try_as_str(this: Gc<'gc, Self>, mc: Mutation<'gc>) -> Option<&'gc str> {
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_narrow(&self) -> bool {
        self.stringbuf.get().is_narrow()
    }

    pub fn is_wide(&self) -> bool {
        self.stringbuf.get().is_wide()
    }

    pub fn is_mutable(&self) -> bool {
        payload_class_id(self).bits() == builtin_class_ids::STRING
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

    pub fn ensure_mutable(this: Gc<'gc, Self>, mc: Mutation<'gc>) -> bool {
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
    pub fn set(this: Gc<'gc, Self>, mc: Mutation<'gc>, index: usize, value: char) {
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
            self.chars().is_some_and(|chars| chars.contains(&(c as u8)))
        } else {
            self.wide_chars().is_some_and(|chars| chars.contains(&c))
        }
    }

    pub fn compare(
        &self,
        other: &Self,
        ci: bool,
        start1: Option<usize>,
        start2: Option<usize>,
        end1: Option<usize>,
        end2: Option<usize>,
    ) -> Option<std::cmp::Ordering> {
        compare_strings(ci, self, other, start1, start2, end1, end2)
    }
}

unsafe impl<'gc> ClassTagged for Str<'gc> {
    const CLASS_IDS: &'static [u32] = &[
        crate::rsgc::object::builtin_class_ids::STRING,
        crate::rsgc::object::builtin_class_ids::IMMUTABLE_STRING,
    ];
    const TYPE_NAME: &'static str = "string";
}

fn compare_strings<'gc>(
    ci: bool,
    s1: &Str<'gc>,
    s2: &Str<'gc>,
    start1: Option<usize>,
    start2: Option<usize>,
    end1: Option<usize>,
    end2: Option<usize>,
) -> Option<std::cmp::Ordering> {
    let mut cstart1;
    let mut cstart2;

    cstart1 = if let Some(s) = start1 {
        if s > s1.length {
            return None;
        }
        s
    } else {
        0
    };

    cstart2 = if let Some(s) = start2 {
        if s > s2.length {
            return None;
        }
        s
    } else {
        0
    };

    let cend1 = if let Some(end1) = end1 {
        if end1 <= s1.length {
            end1
        } else {
            return None;
        }
    } else {
        s1.length
    };

    let cend2 = if let Some(end2) = end2 {
        if end2 <= s2.length {
            end2
        } else {
            return None;
        }
    } else {
        s2.length
    };

    if cend1 == 0 && cend2 == 0 {
        return Some(std::cmp::Ordering::Equal);
    }

    let cm = icu::casemap::CaseMapperBorrowed::new();

    while cstart1 < cend1 && cstart2 < cend2 {
        if ci {
            let a = cm.simple_fold(s1.get(cstart1).unwrap()); // s1.get(cstart1).unwrap().to_uppercase().next().unwrap();
            let b = cm.simple_fold(s2.get(cstart2).unwrap()); //s2.get(cstart2).unwrap().to_uppercase().next().unwrap();
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
        Some(std::cmp::Ordering::Greater)
    } else if cstart2 < cend2 {
        Some(std::cmp::Ordering::Less)
    } else {
        Some(std::cmp::Ordering::Equal)
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
        compare_strings(false, self, other, None, None, None, None)
            == Some(std::cmp::Ordering::Equal)
    }
}

impl<'gc> Eq for Str<'gc> {}

impl<'gc> PartialOrd for Str<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'gc> Ord for Str<'gc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_strings(false, self, other, None, None, None, None).unwrap()
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

fn symbol_header_word(interned: bool) -> u64 {
    let class_id = if interned {
        builtin_class_ids::SYMBOL
    } else {
        builtin_class_ids::UNINTERNED_SYMBOL
    };

    class_header_word(ClassId::new(class_id).unwrap())
}

impl<'gc> Symbol<'gc> {
    pub(super) fn new<const INTERNED: bool>(
        mc: Mutation<'gc>,
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

        Gc::new_with_header_word(
            mc,
            Self {
                stringbuf: buf,
                hash: Cell::new(hash),
                prefix_offset: prefix_offset.unwrap_or(0),
            },
            symbol_header_word(INTERNED),
        )
    }

    pub fn len(&self) -> usize {
        self.stringbuf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_narrow(&self) -> bool {
        self.stringbuf.is_narrow()
    }

    pub fn chars(&self) -> Option<&'gc [u8]> {
        if self.stringbuf.is_narrow() {
            Some(unsafe {
                std::slice::from_raw_parts(self.stringbuf.contents().to_ptr(), self.len())
            })
        } else {
            None
        }
    }

    pub fn wide_chars(&self) -> Option<&'gc [char]> {
        if self.stringbuf.is_wide() {
            Some(unsafe {
                std::slice::from_raw_parts(self.stringbuf.contents().to_ptr(), self.len())
            })
        } else {
            None
        }
    }

    pub fn substring(&self, mc: Mutation<'gc>, start: usize, end: usize) -> Gc<'gc, Str<'gc>> {
        let buf = self.stringbuf;

        Gc::new_with_header_word(
            mc,
            Str {
                stringbuf: Lock::new(buf),
                start: Cell::new(start),
                length: end - start,
            },
            string_header_word(true),
        )
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;

    #[test]
    fn string_buffers_allocate_with_class_only_headers() {
        Scheme::new_uninit().enter(|ctx| {
            let narrow = Stringbuf::new(*ctx, 3, false);
            let wide = Stringbuf::new(*ctx, 3, true);

            let cases = [
                (narrow, builtin_class_ids::STRINGBUF_NARROW),
                (wide, builtin_class_ids::STRINGBUF_WIDE),
            ];

            for (buffer, raw_class_id) in cases {
                assert_eq!(
                    buffer.as_gcobj().header().class_id(),
                    ClassId::new(raw_class_id).unwrap()
                );
            }
        });
    }
}

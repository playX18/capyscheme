//! GDB JIT interface support.
//!
//! GDB discovers JITed code by watching `__jit_debug_register_code` and reading
//! `__jit_debug_descriptor`. Object generation is wired separately; this module
//! owns the process-global registration list and entry lifetime.

use std::{
    collections::HashMap,
    io, ptr,
    sync::{LazyLock, Mutex},
};

use object::elf;

const JIT_NOACTION: u32 = 0;
const JIT_REGISTER_FN: u32 = 1;
const JIT_UNREGISTER_FN: u32 = 2;

#[repr(C)]
pub struct JitCodeEntry {
    next_entry: *mut JitCodeEntry,
    prev_entry: *mut JitCodeEntry,
    symfile_addr: *const u8,
    symfile_size: u64,
}

#[repr(C)]
pub struct JitDescriptor {
    version: u32,
    action_flag: u32,
    relevant_entry: *mut JitCodeEntry,
    first_entry: *mut JitCodeEntry,
}

#[unsafe(no_mangle)]
pub static mut __jit_debug_descriptor: JitDescriptor = JitDescriptor {
    version: 1,
    action_flag: JIT_NOACTION,
    relevant_entry: ptr::null_mut(),
    first_entry: ptr::null_mut(),
};

#[unsafe(no_mangle)]
#[inline(never)]
pub extern "C" fn __jit_debug_register_code() {
    std::hint::black_box(());
}

static JIT_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));
static PERMANENT_REGISTRATIONS: LazyLock<Mutex<Vec<GdbJitRegistration>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

pub struct GdbJitRegistration {
    object: Box<[u8]>,
    entry: Box<JitCodeEntry>,
    registered: bool,
}

// The entry points at bytes owned by the same registration. Mutating the global
// descriptor is serialized by `JIT_LOCK`.
// SAFETY: `GdbJitRegistration` is `Send` because all mutable state is protected by synchronization or it owns no mutable shared state
unsafe impl Send for GdbJitRegistration {}

impl GdbJitRegistration {
    pub fn register(object: Vec<u8>) -> Option<Self> {
        if object.is_empty() {
            return None;
        }

        let object = object.into_boxed_slice();
        let mut entry = Box::new(JitCodeEntry {
            next_entry: ptr::null_mut(),
            prev_entry: ptr::null_mut(),
            symfile_addr: object.as_ptr(),
            symfile_size: object.len() as u64,
        });

        let _guard = JIT_LOCK.lock().expect("GDB JIT lock poisoned");
        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of_mut!(__jit_debug_descriptor);
            let entry_ptr: *mut JitCodeEntry = &mut *entry;
            (*entry_ptr).next_entry = (*descriptor).first_entry;
            if !(*descriptor).first_entry.is_null() {
                (*(*descriptor).first_entry).prev_entry = entry_ptr;
            }
            (*descriptor).first_entry = entry_ptr;
            (*descriptor).relevant_entry = entry_ptr;
            (*descriptor).action_flag = JIT_REGISTER_FN;
            __jit_debug_register_code();
            (*descriptor).action_flag = JIT_NOACTION;
        }

        Some(Self {
            object,
            entry,
            registered: true,
        })
    }

    pub fn unregister(&mut self) {
        if !self.registered {
            return;
        }

        let _guard = JIT_LOCK.lock().expect("GDB JIT lock poisoned");
        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of_mut!(__jit_debug_descriptor);
            let entry_ptr: *mut JitCodeEntry = &mut *self.entry;
            if !(*entry_ptr).prev_entry.is_null() {
                (*(*entry_ptr).prev_entry).next_entry = (*entry_ptr).next_entry;
            } else {
                (*descriptor).first_entry = (*entry_ptr).next_entry;
            }

            if !(*entry_ptr).next_entry.is_null() {
                (*(*entry_ptr).next_entry).prev_entry = (*entry_ptr).prev_entry;
            }

            (*descriptor).relevant_entry = entry_ptr;
            (*descriptor).action_flag = JIT_UNREGISTER_FN;
            __jit_debug_register_code();
            (*descriptor).action_flag = JIT_NOACTION;
            (*descriptor).relevant_entry = ptr::null_mut();
            (*entry_ptr).next_entry = ptr::null_mut();
            (*entry_ptr).prev_entry = ptr::null_mut();
        }
        self.registered = false;
    }

    pub fn object_len(&self) -> usize {
        self.object.len()
    }
}

impl Drop for GdbJitRegistration {
    fn drop(&mut self) {
        self.unregister();
    }
}

pub fn register_permanent_object(object: Vec<u8>) {
    if let Some(registration) = GdbJitRegistration::register(object) {
        PERMANENT_REGISTRATIONS
            .lock()
            .expect("GDB JIT registration list poisoned")
            .push(registration);
    }
}

pub(crate) fn patch_capy_jit_symbol_addresses(
    object: &mut [u8],
    addresses: &HashMap<u32, usize>,
) -> io::Result<usize> {
    if object.len() < 64 || &object[0..4] != b"\x7fELF" {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object is not an ELF64 file",
        ));
    }
    if object[4] != elf::ELFCLASS64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object is not ELF64",
        ));
    }

    let endian = ElfEndian::from_ident(object[5])?;
    let shoff = endian.read_u64(object, 0x28)? as usize;
    let shentsize = endian.read_u16(object, 0x3a)? as usize;
    let shnum = endian.read_u16(object, 0x3c)? as usize;
    let shstrndx = endian.read_u16(object, 0x3e)? as usize;
    if shentsize < 64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object has invalid ELF64 section header size",
        ));
    }

    let shstr = section_header(object, endian, shoff, shentsize, shnum, shstrndx)?;
    let shstr_offset = endian.read_u64(object, shstr + 24)? as usize;
    let shstr_size = endian.read_u64(object, shstr + 32)? as usize;

    let mut patched = 0;
    let mut patched_sections = HashMap::new();
    for section_index in 0..shnum {
        let section = section_header(object, endian, shoff, shentsize, shnum, section_index)?;
        let name_offset = endian.read_u32(object, section)? as usize;
        let Some(symbol_id) =
            synthetic_section_symbol_index(object, shstr_offset, shstr_size, name_offset)?
        else {
            continue;
        };
        let Some(address) = addresses.get(&symbol_id).copied() else {
            continue;
        };
        endian.write_u64(object, section + 16, address as u64)?;
        patched_sections.insert(section_index, symbol_id);
    }

    for section_index in 0..shnum {
        let section = checked_add(
            shoff,
            section_index.checked_mul(shentsize).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "GDB JIT object section table offset overflow",
                )
            })?,
        )?;
        checked_range(object, section, 64)?;
        if endian.read_u32(object, section + 4)? != elf::SHT_SYMTAB {
            continue;
        }

        let symtab_offset = endian.read_u64(object, section + 24)? as usize;
        let symtab_size = endian.read_u64(object, section + 32)? as usize;
        let linked_string_section = endian.read_u32(object, section + 40)? as usize;
        let symtab_entry_size = endian.read_u64(object, section + 56)? as usize;
        if symtab_entry_size < 24 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "GDB JIT object has invalid ELF64 symbol size",
            ));
        }
        let strtab = section_header(
            object,
            endian,
            shoff,
            shentsize,
            shnum,
            linked_string_section,
        )?;
        let strtab_offset = endian.read_u64(object, strtab + 24)? as usize;
        let strtab_size = endian.read_u64(object, strtab + 32)? as usize;
        checked_range(object, strtab_offset, strtab_size)?;
        checked_range(object, symtab_offset, symtab_size)?;

        let symbol_count = symtab_size / symtab_entry_size;
        for symbol_index in 0..symbol_count {
            let symbol = checked_add(
                symtab_offset,
                symbol_index.checked_mul(symtab_entry_size).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "GDB JIT object symbol table offset overflow",
                    )
                })?,
            )?;
            checked_range(object, symbol, 24)?;
            let symbol_section = endian.read_u16(object, symbol + 6)? as usize;
            let name_offset = endian.read_u32(object, symbol)? as usize;
            let synthetic_symbol =
                synthetic_symbol_index(object, strtab_offset, strtab_size, name_offset)?;
            if patched_sections.contains_key(&symbol_section) {
                if synthetic_symbol.is_some() {
                    patched += 1;
                }
                continue;
            }

            let Some(symbol_id) = synthetic_symbol else {
                continue;
            };
            let Some(address) = addresses.get(&symbol_id).copied() else {
                continue;
            };
            endian.write_u16(object, symbol + 6, elf::SHN_ABS)?;
            endian.write_u64(object, symbol + 8, address as u64)?;
            patched += 1;
        }
    }

    Ok(patched)
}

#[derive(Clone, Copy)]
enum ElfEndian {
    Little,
    Big,
}

impl ElfEndian {
    fn from_ident(data: u8) -> io::Result<Self> {
        match data {
            elf::ELFDATA2LSB => Ok(Self::Little),
            elf::ELFDATA2MSB => Ok(Self::Big),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "GDB JIT object has invalid ELF data encoding",
            )),
        }
    }

    fn read_u16(self, bytes: &[u8], offset: usize) -> io::Result<u16> {
        let data = read_array::<2>(bytes, offset)?;
        Ok(match self {
            Self::Little => u16::from_le_bytes(data),
            Self::Big => u16::from_be_bytes(data),
        })
    }

    fn read_u32(self, bytes: &[u8], offset: usize) -> io::Result<u32> {
        let data = read_array::<4>(bytes, offset)?;
        Ok(match self {
            Self::Little => u32::from_le_bytes(data),
            Self::Big => u32::from_be_bytes(data),
        })
    }

    fn read_u64(self, bytes: &[u8], offset: usize) -> io::Result<u64> {
        let data = read_array::<8>(bytes, offset)?;
        Ok(match self {
            Self::Little => u64::from_le_bytes(data),
            Self::Big => u64::from_be_bytes(data),
        })
    }

    fn write_u16(self, bytes: &mut [u8], offset: usize, value: u16) -> io::Result<()> {
        let data = match self {
            Self::Little => value.to_le_bytes(),
            Self::Big => value.to_be_bytes(),
        };
        write_array(bytes, offset, &data)
    }

    fn write_u64(self, bytes: &mut [u8], offset: usize, value: u64) -> io::Result<()> {
        let data = match self {
            Self::Little => value.to_le_bytes(),
            Self::Big => value.to_be_bytes(),
        };
        write_array(bytes, offset, &data)
    }
}

fn read_array<const N: usize>(bytes: &[u8], offset: usize) -> io::Result<[u8; N]> {
    checked_range(bytes, offset, N)?;
    let mut data = [0; N];
    data.copy_from_slice(&bytes[offset..offset + N]);
    Ok(data)
}

fn write_array(bytes: &mut [u8], offset: usize, data: &[u8]) -> io::Result<()> {
    checked_range(bytes, offset, data.len())?;
    bytes[offset..offset + data.len()].copy_from_slice(data);
    Ok(())
}

fn checked_add(lhs: usize, rhs: usize) -> io::Result<usize> {
    lhs.checked_add(rhs)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "GDB JIT object offset overflow"))
}

fn checked_range(bytes: &[u8], offset: usize, len: usize) -> io::Result<()> {
    let end = checked_add(offset, len)?;
    if end > bytes.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object table is out of bounds",
        ));
    }
    Ok(())
}

fn section_header(
    bytes: &[u8],
    _endian: ElfEndian,
    shoff: usize,
    shentsize: usize,
    shnum: usize,
    index: usize,
) -> io::Result<usize> {
    if index >= shnum {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object section link is out of bounds",
        ));
    }
    let offset = checked_add(
        shoff,
        index.checked_mul(shentsize).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "GDB JIT object section link offset overflow",
            )
        })?,
    )?;
    checked_range(bytes, offset, 64)?;
    Ok(offset)
}

fn synthetic_symbol_index(
    bytes: &[u8],
    strtab_offset: usize,
    strtab_size: usize,
    name_offset: usize,
) -> io::Result<Option<u32>> {
    let name = string_table_name(bytes, strtab_offset, strtab_size, name_offset)?;
    Ok(name
        .strip_prefix("capy_jit_fn_")
        .and_then(|suffix| suffix.parse::<u32>().ok()))
}

fn synthetic_section_symbol_index(
    bytes: &[u8],
    strtab_offset: usize,
    strtab_size: usize,
    name_offset: usize,
) -> io::Result<Option<u32>> {
    let name = string_table_name(bytes, strtab_offset, strtab_size, name_offset)?;
    Ok(name
        .strip_prefix(".text.capy_jit_fn_")
        .and_then(|suffix| suffix.parse::<u32>().ok()))
}

fn string_table_name(
    bytes: &[u8],
    strtab_offset: usize,
    strtab_size: usize,
    name_offset: usize,
) -> io::Result<&str> {
    if name_offset >= strtab_size {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object symbol name is out of bounds",
        ));
    }
    let start = checked_add(strtab_offset, name_offset)?;
    let end_limit = checked_add(strtab_offset, strtab_size)?;
    let nul = bytes[start..end_limit]
        .iter()
        .position(|byte| *byte == 0)
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "GDB JIT object symbol name is unterminated",
            )
        })?;
    std::str::from_utf8(&bytes[start..start + nul]).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "GDB JIT object symbol name is not UTF-8",
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use object::{Object, ObjectSection, ObjectSymbol};

    static TEST_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

    fn reset_descriptor_for_test() {
        PERMANENT_REGISTRATIONS
            .lock()
            .expect("GDB JIT registration list poisoned")
            .clear();
        let _guard = JIT_LOCK.lock().expect("GDB JIT lock poisoned");
        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of_mut!(__jit_debug_descriptor);
            (*descriptor).action_flag = JIT_NOACTION;
            (*descriptor).relevant_entry = ptr::null_mut();
            (*descriptor).first_entry = ptr::null_mut();
        }
    }

    #[test]
    fn registration_links_and_unlinks_descriptor_entry() {
        let _test_guard = TEST_LOCK.lock().expect("GDB JIT test lock poisoned");
        reset_descriptor_for_test();

        let mut registration =
            GdbJitRegistration::register(vec![0x7f, b'E', b'L', b'F']).expect("register object");
        assert_eq!(registration.object_len(), 4);

        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of!(__jit_debug_descriptor);
            assert!(!(*descriptor).first_entry.is_null());
            assert_eq!((*(*descriptor).first_entry).symfile_size, 4);
        }

        registration.unregister();

        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of!(__jit_debug_descriptor);
            assert!((*descriptor).first_entry.is_null());
            assert!((*descriptor).relevant_entry.is_null());
            assert_eq!((*descriptor).action_flag, JIT_NOACTION);
        }
    }

    #[test]
    fn empty_object_is_not_registered() {
        let _test_guard = TEST_LOCK.lock().expect("GDB JIT test lock poisoned");
        reset_descriptor_for_test();

        assert!(GdbJitRegistration::register(Vec::new()).is_none());
        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of!(__jit_debug_descriptor);
            assert!((*descriptor).first_entry.is_null());
        }
    }

    #[test]
    fn patch_capy_jit_symbol_addresses_patches_synthetic_text_section() {
        let _test_guard = TEST_LOCK.lock().expect("GDB JIT test lock poisoned");
        let mut object = object::write::Object::new(
            object::BinaryFormat::Elf,
            object::Architecture::X86_64,
            object::Endianness::Little,
        );
        let text = object.add_section(
            Vec::new(),
            b".text.capy_jit_fn_7".to_vec(),
            object::SectionKind::Text,
        );
        object.section_mut(text).set_data(vec![0xcc; 4], 1);
        object.add_symbol(object::write::Symbol {
            name: b"capy_jit_fn_7".to_vec(),
            value: 0,
            size: 4,
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Section(text),
            flags: object::SymbolFlags::None,
        });
        object.add_symbol(object::write::Symbol {
            name: b"unrelated".to_vec(),
            value: 0,
            size: 4,
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Section(text),
            flags: object::SymbolFlags::None,
        });
        let mut bytes = object.write().expect("write ELF object");
        let addresses = HashMap::from([(7, 0x1234_5678usize)]);

        let patched =
            patch_capy_jit_symbol_addresses(&mut bytes, &addresses).expect("patch symbols");

        assert_eq!(patched, 1);
        let parsed = object::File::parse(bytes.as_slice()).expect("parse patched object");
        let symbol = parsed
            .symbols()
            .find(|symbol| symbol.name() == Ok("capy_jit_fn_7"))
            .expect("synthetic symbol");
        assert_eq!(symbol.address(), 0);
        assert_ne!(symbol.section(), object::SymbolSection::Absolute);
        let section = parsed
            .sections()
            .find(|section| section.name() == Ok(".text.capy_jit_fn_7"))
            .expect("synthetic section");
        assert_eq!(section.address(), 0x1234_5678);
        let unrelated = parsed
            .symbols()
            .find(|symbol| symbol.name() == Ok("unrelated"))
            .expect("unrelated symbol");
        assert_ne!(unrelated.section(), object::SymbolSection::Absolute);
    }

    #[test]
    fn permanent_registration_keeps_descriptor_entry_linked() {
        let _test_guard = TEST_LOCK.lock().expect("GDB JIT test lock poisoned");
        reset_descriptor_for_test();

        register_permanent_object(vec![0x7f, b'E', b'L', b'F']);

        // SAFETY: Invariants are upheld by the surrounding context
        unsafe {
            let descriptor = ptr::addr_of!(__jit_debug_descriptor);
            assert!(!(*descriptor).first_entry.is_null());
            assert_eq!((*(*descriptor).first_entry).symfile_size, 4);
        }

        PERMANENT_REGISTRATIONS
            .lock()
            .expect("GDB JIT registration list poisoned")
            .clear();
    }
}

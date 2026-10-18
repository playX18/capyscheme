//! GDB JIT interface support.
//!
//! GDB discovers JITed code by watching `__jit_debug_register_code` and reading
//! `__jit_debug_descriptor`. This module owns the process-global registration
//! list and entry lifetime, plus the in-memory ELF symfile builder used for
//! generated code with `object::write`. Registration is opt-in per image
//! (`CAPY_JIT_GDB=1`).

use std::{
    ptr,
    sync::{LazyLock, Mutex},
};

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

/// Copies of every symfile currently linked in the descriptor, for tests. The
/// bytes are copied under the JIT lock: entries owned by other threads may be
/// unregistered and freed at any time.
#[cfg(test)]
pub fn symfiles_for_test() -> Vec<Vec<u8>> {
    let _guard = JIT_LOCK.lock().expect("GDB JIT lock poisoned");
    // SAFETY: Invariants are upheld by the surrounding context
    unsafe {
        let descriptor = ptr::addr_of!(__jit_debug_descriptor);
        let mut out = Vec::new();
        let mut entry = (*descriptor).first_entry;
        while !entry.is_null() {
            let bytes =
                std::slice::from_raw_parts((*entry).symfile_addr, (*entry).symfile_size as usize);
            out.push(bytes.to_vec());
            entry = (*entry).next_entry;
        }
        out
    }
}

/// One symbol in a [`build_symfile`] symfile: its name and absolute runtime
/// address range.
pub struct JitSymbol<'a> {
    pub name: &'a str,
    pub address: usize,
    pub size: usize,
}

/// Builds an in-memory ELF symfile for generated code: one `.text` section
/// holding a copy of `code` plus one function symbol per entry in `symbols`.
///
/// `base` is the address `code[0]` runs at. The file is `ET_EXEC` with
/// `.text` at its real load address and absolute `st_value`s, the way LLVM's
/// and JSC's JIT symfiles look. GDB reads either form, but lldb's JIT loader
/// only resolves addresses through sections: `SHN_ABS` symbols unwind fine
/// yet never get names.
pub fn build_symfile(
    base: usize,
    code: &[u8],
    symbols: &[JitSymbol],
    eh_frame: Option<&[u8]>,
) -> Vec<u8> {
    let arch = {
        #[cfg(target_arch = "x86_64")]
        {
            object::Architecture::X86_64
        }
        #[cfg(target_arch = "aarch64")]
        {
            object::Architecture::Aarch64
        }
    };
    let mut object =
        object::write::Object::new(object::BinaryFormat::Elf, arch, object::Endianness::Little);
    let text = object.add_section(Vec::new(), b".text".to_vec(), object::SectionKind::Text);
    object.section_mut(text).set_data(code, 16);
    if let Some(eh_frame) = eh_frame {
        // Unwind info for debuggers that read the symfile (lldb): a plain
        // unloaded section found by name, with absolute FDE addresses.
        let unwind = object.add_section(
            Vec::new(),
            b".eh_frame".to_vec(),
            object::SectionKind::Other,
        );
        object.section_mut(unwind).set_data(eh_frame, 8);
    }
    for symbol in symbols {
        object.add_symbol(object::write::Symbol {
            name: symbol.name.as_bytes().to_vec(),
            value: symbol.address as u64,
            size: symbol.size as u64,
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Dynamic,
            weak: false,
            section: object::write::SymbolSection::Section(text),
            flags: object::SymbolFlags::None,
        });
    }
    let mut bytes = object
        .write()
        .expect("object writer failed on a plain symbol table");
    retarget_text(&mut bytes, base as u64);
    bytes
}

/// Patches the writer's relocatable output into a load-addressed `ET_EXEC`.
/// `object::write` always emits `ET_REL` with `sh_addr == 0` and no program
/// headers, lldb's `ObjectFileELF` takes section load addresses from
/// `sh_addr`, so the type and the address are all it needs.
fn retarget_text(bytes: &mut [u8], base: u64) {
    assert_eq!(&bytes[..4], &[0x7f, b'E', b'L', b'F']);
    assert_eq!(bytes[4], 2); // 64-bit
    assert_eq!(bytes[5], 1); // little-endian
    bytes[16..18].copy_from_slice(&object::elf::ET_EXEC.to_le_bytes());

    let u16le = |at: usize| u16::from_le_bytes([bytes[at], bytes[at + 1]]);
    let u64le = |at: usize| u64::from_le_bytes(bytes[at..at + 8].try_into().unwrap());
    let shoff = u64le(0x28) as usize;
    let shentsize = u16le(0x3a) as usize;
    let shnum = u16le(0x3c) as usize;
    let shstrndx = u16le(0x3e) as usize;
    let str_off = u64le(shoff + shstrndx * shentsize + 24) as usize;
    for i in 1..shnum {
        let hdr = shoff + i * shentsize;
        let name = u32::from_le_bytes(bytes[hdr..hdr + 4].try_into().unwrap()) as usize;
        let mut end = str_off + name;
        while bytes[end] != 0 {
            end += 1;
        }
        if &bytes[str_off + name..end] == b".text" {
            bytes[hdr + 16..hdr + 24].copy_from_slice(&base.to_le_bytes());
            return;
        }
    }
    panic!("symfile lost its .text section");
}

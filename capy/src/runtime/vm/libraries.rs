//! Collection of loaded shared libraries

use crate::runtime::{Context, value::Value};
use addr2line::{Context as Addr2LineContext, LookupResult};
use object::{Object, ObjectSection};
use rsgc::{Global, Rootable, Trace, sync::monitor::Monitor};
use std::{
    borrow::Cow,
    ffi::OsStr,
    path::Path,
    rc::Rc,
    sync::{Arc, LazyLock, OnceLock},
};

pub struct SchemeLibrary<'gc> {
    pub library: libloading::Library,
    pub path: std::path::PathBuf,
    pub entrypoint: Value<'gc>,

    pub globals: &'static [*mut Value<'static>],

    pub debug_context: OnceLock<Option<LibraryDebugInfo>>,
}

unsafe impl<'gc> Trace for SchemeLibrary<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        unsafe {
            for global in self.globals.iter() {
                visitor.trace(&mut **global);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> SchemeLibrary<'gc> {
    pub fn load(ctx: Context<'gc>, path: impl AsRef<OsStr>) -> Result<Self, libloading::Error> {
        unsafe {
            let path = path.as_ref();
            let lib = libloading::Library::new(path)?;

            let globals: libloading::Symbol<*const *mut Value> = lib.get(b"CAPY_GLOBALS\0")?;
            let globals_len: libloading::Symbol<*const u32> = lib.get(b"CAPY_GLOBALS_LEN\0")?;

            let globals: &'static [*mut Value] =
                std::slice::from_raw_parts(*globals, **globals_len as usize);

            let module_init: libloading::Symbol<extern "C-unwind" fn(&Context<'gc>) -> Value<'gc>> =
                lib.get(b"capy_module_init\0")?;

            let entrypoint = module_init(&ctx);

            Ok(Self {
                path: std::path::PathBuf::from(path),
                library: lib,
                debug_context: OnceLock::new(),
                globals,
                entrypoint,
            })
        }
    }

    /// Get debug information associated with this library, if any.
    ///
    /// If debug information loading was attempted before, the result is cached. And
    /// if we can't load debug information, `None` is cached.
    pub fn debug_info(&self) -> Option<&LibraryDebugInfo> {
        let info = self
            .debug_context
            .get_or_init(|| LibraryDebugInfo::new(&self.path));
        info.as_ref()
    }
}

pub struct LibraryCollection<'gc> {
    pub libs: Monitor<Vec<SchemeLibrary<'gc>>>,
}

unsafe impl<'gc> Trace for LibraryCollection<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        unsafe {
            self.libs.get_mut().trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> LibraryCollection<'gc> {
    pub fn new() -> Self {
        Self {
            libs: Monitor::new(Vec::with_capacity(2)),
        }
    }

    pub fn load(
        &self,
        path: impl AsRef<OsStr>,
        ctx: Context<'gc>,
    ) -> Result<Value<'gc>, libloading::Error> {
        let lib = SchemeLibrary::load(ctx, path)?;
        let entrypoint = lib.entrypoint;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }
}

pub static LIBRARY_COLLECTION: LazyLock<Global<Rootable!(LibraryCollection<'_>)>> =
    LazyLock::new(|| Global::new(LibraryCollection::new()));

fn load_file_section<'input>(
    id: gimli::SectionId,
    file: &object::File<'input>,
) -> Result<gimli::EndianReader<gimli::RunTimeEndian, Rc<[u8]>>, ()> {
    let name = id.name();

    match file.section_by_name(name) {
        Some(section) => match section.uncompressed_data().unwrap() {
            Cow::Borrowed(b) => Ok(gimli::EndianReader::new(
                Rc::from(b),
                gimli::RunTimeEndian::Little,
            )),
            Cow::Owned(b) => Ok(gimli::EndianReader::new(
                Rc::from(b),
                gimli::RunTimeEndian::Little,
            )),
        },
        None => Ok(gimli::EndianReader::new(
            Rc::from(&[][..]),
            gimli::RunTimeEndian::Little,
        )),
    }
}

#[cfg(unix)]
mod imp {
    use std::{
        fs::File,
        io::{BufRead, BufReader},
        path::Path,
    };

    pub(super) fn try_find_mappings(path: &Path) -> Option<Vec<(u64, u64)>> {
        let file = File::open("/proc/self/maps").ok()?;
        let reader = BufReader::new(file);
        let mut mappings = Vec::new();

        for line in reader.lines() {
            let line = line.ok()?;
            let parts = line.split_whitespace().collect::<Vec<_>>();

            if parts.len() >= 6 {
                let path_from_map = Path::new(parts[5]);

                if path_from_map == path {
                    let address_range = parts[0];
                    let mut addrs = address_range.split('-');
                    let start_addr_str = addrs.next().unwrap();
                    let end_addr_str = addrs.next().unwrap();
                    let start_addr = usize::from_str_radix(start_addr_str, 16).ok()?;
                    let end_addr = usize::from_str_radix(end_addr_str, 16).ok()?;
                    log::debug!(
                        "Found mapping: 0x{:x}-0x{:x} for {}",
                        start_addr,
                        end_addr,
                        path.display()
                    );
                    mappings.push((start_addr as u64, end_addr as u64));
                }
            }
        }

        if mappings.is_empty() {
            None
        } else {
            Some(mappings)
        }
    }
}

pub struct LibraryDebugInfo {
    pub mappings: Vec<(u64, u64)>,
    pub dwarf: Arc<gimli::Dwarf<gimli::EndianReader<gimli::RunTimeEndian, Rc<[u8]>>>>,
    pub context: Addr2LineContext<gimli::EndianReader<gimli::RunTimeEndian, Rc<[u8]>>>,
}

impl LibraryDebugInfo {
    pub fn new(path: &Path) -> Option<Self> {
        // give up if we can't open the file or parse it
        let file = std::fs::File::open(path).ok()?;
        let mappings = imp::try_find_mappings(path)?;
        let mmap = unsafe { memmap2::Mmap::map(&file).ok()? };
        let object = &object::File::parse(&*mmap).ok()?;

        let mut load_section =
            |id: gimli::SectionId| -> Result<_, _> { load_file_section(id, object) };

        let dwarf = gimli::Dwarf::load(&mut load_section).ok()?;
        let dwarf = Arc::new(dwarf);

        let context = Addr2LineContext::from_arc_dwarf(dwarf.clone()).ok()?;

        Some(Self {
            mappings,
            context,
            dwarf,
        })
    }

    pub fn has_address(&self, addr: u64) -> Option<u64> {
        for (start, end) in &self.mappings {
            if addr >= *start && addr < *end {
                return Some(addr - *start);
            }
        }
        None
    }

    pub fn function_name(&self, addr: u64) -> Option<String> {
        let addr = self.has_address(addr)?;

        let frames = self.context.find_frames(addr);
        match frames {
            LookupResult::Output(frames) => {
                let mut frames = frames.ok()?;
                while let Ok(Some(frame)) = frames.next() {
                    if let Some(name) = frame.function {
                        return Some(name.demangle().ok()?.to_string());
                    }
                }

                None
            }

            _ => todo!("Split DWARF not supported yet"),
        }
    }

    pub fn location(&self, addr: u64) -> Option<addr2line::Location<'_>> {
        let addr = self.has_address(addr)?;
        self.context.find_location(addr).ok().flatten()
    }
}

/// Given address in the process space, try to look up its source location among loaded
/// Scheme dynamic libraries.
///
/// # Notes
///
/// Might return `None` if:
/// - Library is loaded, but does not have debug info.
/// - Address does not belong to any loaded library.
/// - Address belongs to a loaded library, but does not map to any source location.
pub fn lookup_scheme_location<'gc>(
    ctx: Context<'gc>,
    addr: u64,
) -> Option<(Option<String>, Option<u32>, Option<u32>)> {
    let lbis = LIBRARY_COLLECTION.fetch(&ctx);

    let libs = lbis.libs.lock();

    for lib in libs.iter() {
        if let Some(debug_info) = lib.debug_info() {
            if let Some(loc) = debug_info.location(addr) {
                return Some((loc.file.map(|s| s.to_string()), loc.line, loc.column));
            }
        }
    }

    None
}

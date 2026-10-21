//! Load a `Masm` image into executable memory with debug info.
//!
//! `load` resolves externs to Rust `extern "C"` addresses, patches the
//! reserved `RpHeader`s, publishes symbols through the GDB JIT interface
//! and a perf map, and registers synthesized CIE/FDEs with the unwinder (one
//! `CFA = sp + K` rule per region).

use std::io;

use mmtk::util::Address;
use onlineasm::Finished;
use onlineasm::Label;
use onlineasm::asmkit::{ExternalName, LoadedCode, Span};

use super::masm::{
    CfiRecord, HeaderTarget, HeaderWords, PendingHeader, SymbolKind, SymbolRecord,
    TOPLINK_TO_IMAGE_START,
};
use crate::runtime::code_memory::{CodeMemory, code_memory};
use crate::runtime::gdb_jit::{self, GdbJitRegistration, JitSymbol};
use crate::utils::flags;

pub struct UnloadedImage {
    pub finished: Finished,
    pub symbols: Vec<SymbolRecord>,
    pub cfi: Vec<CfiRecord>,
    pub headers: Vec<PendingHeader>,
    pub tables: Vec<TablePatch>,
}

/// One write-once address table: 8-byte slots at `base`, filled after load
/// with the runtime address of each entry label.
pub struct TablePatch {
    pub base: Label,
    pub entries: Vec<Label>,
}

pub struct LoadedSymbol {
    pub name: String,
    pub addr: usize,
    pub len: usize,
    pub kind: SymbolKind,
}

pub struct LoadedImage {
    loaded: LoadedCode,
    symbols: Vec<LoadedSymbol>,
    eh_frame: Option<EhFrame>,
    gdb: Option<GdbJitRegistration>,
}

/// Loads the image: extern resolution, header patching, symbol publishing and
/// FDE registration. `resolve` maps each declared extern to the address of a
/// Rust `extern "C"` function.
pub fn load(
    image: UnloadedImage,
    resolve: impl Fn(&ExternalName) -> *const u8,
) -> io::Result<LoadedImage> {
    let memory = code_memory();
    let mut loaded = memory
        .with_allocator(|allocator| image.finished.load(allocator, &resolve))
        .map_err(|err| io::Error::other(format!("failed to load generated code: {err:?}")))?;
    let rx = loaded.rx() as usize;
    // From here on, an error drops `image`, which releases the span.
    let mut image_out = LoadedImage {
        loaded,
        symbols: Vec::new(),
        eh_frame: None,
        gdb: None,
    };

    let mut symbols = Vec::with_capacity(image.symbols.len());
    for symbol in &image.symbols {
        let start = label_offset(&image.finished, symbol.start, &symbol.name)?;
        let end = label_offset(&image.finished, symbol.end, &symbol.name)?;
        if end < start {
            return Err(io::Error::other(format!(
                "symbol {} ends before it starts",
                symbol.name
            )));
        }
        symbols.push(LoadedSymbol {
            name: symbol.name.clone(),
            addr: rx + start as usize,
            len: (end - start) as usize,
            kind: symbol.kind,
        });
    }

    patch_headers(
        memory,
        &mut image_out.loaded,
        rx,
        &image.finished,
        &image.headers,
    )?;
    patch_tables(
        memory,
        &mut image_out.loaded,
        rx,
        &image.finished,
        &image.tables,
    )?;

    if flags::jit_gdb() && !symbols.is_empty() {
        let jit_symbols: Vec<JitSymbol> = symbols
            .iter()
            .map(|s| JitSymbol {
                name: &s.name,
                address: s.addr,
                size: s.len,
            })
            .collect();
        // Absolute addresses: the symfile is read from the file, not relocated.
        let eh_frame = EhFrame::for_symfile(rx, &image.finished, &image.cfi)?;
        let symfile =
            gdb_jit::build_symfile(rx, image.finished.data(), &jit_symbols, eh_frame.as_deref());
        image_out.gdb = GdbJitRegistration::register(symfile);
    }

    publish_perf_map(&symbols);

    image_out.eh_frame = EhFrame::register(rx, &image.finished, &image.cfi)?;
    image_out.symbols = symbols;
    Ok(image_out)
}

impl LoadedImage {
    pub fn symbols(&self) -> &[LoadedSymbol] {
        &self.symbols
    }

    pub fn symbol_addr(&self, name: &str) -> Option<*const u8> {
        self.symbols
            .iter()
            .find(|s| s.name == name)
            .map(|s| s.addr as *const u8)
    }

    /// The symbol whose range contains `addr`, for mapping backtrace frames.
    pub fn symbol_for(&self, addr: usize) -> Option<&LoadedSymbol> {
        self.symbols
            .iter()
            .find(|s| addr >= s.addr && addr < s.addr + s.len)
    }

    pub fn base(&self) -> Address {
        Address::from_ptr(self.loaded.rx())
    }

    pub fn code_size(&self) -> usize {
        self.loaded.code_size()
    }

    /// The code span, for live single-word repatching (stub sites).
    pub fn span(&self) -> Span {
        *self.loaded.span()
    }

    /// Write `bytes` at `offset` into the loaded code.
    pub fn patch(&mut self, offset: usize, bytes: &[u8]) -> io::Result<()> {
        code_memory().patch(self.loaded.span_mut(), offset, bytes)
    }

    /// Write `bytes` at `offset` into code that is not published yet, through
    /// a shared reference: the image already sits in its heap owner.
    ///
    /// No thread may execute the patched range, or patch it concurrently.
    pub fn patch_unpublished(&self, offset: usize, bytes: &[u8]) -> io::Result<()> {
        let mut span = *self.loaded.span();
        code_memory().patch(&mut span, offset, bytes)
    }
}

/// Deregisters the debug info and returns the span to the code memory, so
/// an image that is dropped on any path (an error in `link`, a lost install
/// race, a finalized owner) gives its memory back.
impl Drop for LoadedImage {
    fn drop(&mut self) {
        self.eh_frame.take();
        drop(self.gdb.take());
        if let Err(err) = code_memory().release_span(*self.loaded.span()) {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, " {err}");
        }
    }
}

fn label_offset(finished: &Finished, label: Label, what: &str) -> io::Result<u32> {
    finished
        .offset(label)
        .ok_or_else(|| io::Error::other(format!("label for {what} was never bound")))
}

fn patch_headers(
    memory: &CodeMemory,
    loaded: &mut LoadedCode,
    rx: usize,
    finished: &Finished,
    headers: &[PendingHeader],
) -> io::Result<()> {
    if headers.is_empty() {
        return Ok(());
    }
    let mut blobs: Vec<(usize, Vec<u8>)> = Vec::with_capacity(headers.len());
    for header in headers {
        let at = label_offset(finished, header.header, "RpHeader")? as usize;
        let ra = label_offset(finished, header.ra, "RpHeader return")? as usize;
        let resolve_toplink = |toplink: usize| {
            if toplink == TOPLINK_TO_IMAGE_START {
                (ra - 16) as u64
            } else {
                toplink as u64
            }
        };
        let words: Vec<u64> = match &header.words {
            HeaderWords::Full {
                mv_return_address,
                livemask,
                toplink,
                frame_size,
            } => {
                let mv_return_address = match *mv_return_address {
                    HeaderTarget::Absolute(addr) => addr,
                    HeaderTarget::Label(label) => {
                        rx + label_offset(finished, label, "RpHeader mv return address")? as usize
                    }
                };
                vec![
                    mv_return_address as u64,
                    *livemask as u64,
                    resolve_toplink(*toplink),
                    *frame_size as u64,
                ]
            }
            HeaderWords::Compact(compact) => {
                vec![
                    resolve_toplink(compact.toplink),
                    compact.mask_size_mode as u64,
                ]
            }
        };
        if ra.wrapping_sub(at) != words.len() * 8 {
            return Err(io::Error::other(format!(
                "return-point header reservation is {} bytes, not {}",
                ra.wrapping_sub(at),
                words.len() * 8
            )));
        }
        blobs.push((
            at,
            words.iter().flat_map(|word| word.to_ne_bytes()).collect(),
        ));
    }
    let patches: Vec<(usize, &[u8])> = blobs.iter().map(|(at, blob)| (*at, &blob[..])).collect();
    memory.patch_many(loaded.span_mut(), &patches)
}

fn patch_tables(
    memory: &CodeMemory,
    loaded: &mut LoadedCode,
    rx: usize,
    finished: &Finished,
    tables: &[TablePatch],
) -> io::Result<()> {
    if tables.is_empty() {
        return Ok(());
    }
    let mut blobs: Vec<(usize, Vec<u8>)> = Vec::with_capacity(tables.len());
    for table in tables {
        let at = label_offset(finished, table.base, "address table")? as usize;
        let mut blob = Vec::with_capacity(table.entries.len() * 8);
        for entry in &table.entries {
            let off = label_offset(finished, *entry, "address table entry")? as usize;
            blob.extend_from_slice(&(rx + off).to_ne_bytes());
        }
        blobs.push((at, blob));
    }
    let patches: Vec<(usize, &[u8])> = blobs
        .iter()
        .map(|(at, blob)| (*at, blob.as_slice()))
        .collect();
    memory.patch_many(loaded.span_mut(), &patches)
}

fn publish_perf_map(symbols: &[LoadedSymbol]) {
    if !crate::utils::flags::perf_map() {
        return;
    }
    // perf reads `/tmp/perf-PID.map` on Linux, where `temp_dir()` is `/tmp`.
    let path = std::env::temp_dir().join(format!("perf-{}.map", std::process::id()));
    let mut file = match std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
    {
        Ok(file) => file,
        Err(err) => {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "CAPY_PERF_MAP: cannot open {}: {err}", path.display());
            return;
        }
    };
    use std::io::Write;
    for symbol in symbols {
        if writeln!(file, "{:x} {:x} {}", symbol.addr, symbol.len, symbol.name).is_err() {
            log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "CAPY_PERF_MAP: cannot write {}", path.display());
            return;
        }
    }
}

unsafe extern "C" {
    fn __register_frame(fde: *const u8);
    fn __deregister_frame(fde: *const u8);
}

/// Synthesized `.eh_frame` (CIE + one FDE per region + terminator), kept alive
/// while registered. libunwind on macOS takes a single FDE pointer per call,
/// libgcc on Linux takes the `.eh_frame` start.
struct EhFrame {
    bytes: Box<[u8]>,
    fdes: Vec<*const u8>,
}

const DW_CFA_NOP: u8 = 0x00;
const DW_CFA_DEF_CFA: u8 = 0x0c;
const DW_CFA_OFFSET: u8 = 0x80;
/// FDE address encodings: macOS unwinding expects the same
/// PC-relative form its static frames use, elsewhere absolute is fine. The
/// symfile copy always uses absolute addresses (see `for_symfile`).
const DW_EH_PE_ABSPTR: u8 = 0x00;
const DW_EH_PE_PCREL: u8 = 0x10;

#[cfg(target_arch = "x86_64")]
const CFA_REG: u64 = 7; // RSP
#[cfg(target_arch = "aarch64")]
const CFA_REG: u64 = 31; // SP
// The return address rule for the `scheme_entry`-style prologue: x86-64 keeps
// the pushed return address at `[CFA-8]`. AArch64 saves `lr` at `[CFA-16]`.
#[cfg(target_arch = "x86_64")]
const RA_REG: u64 = 16; // RIP
#[cfg(target_arch = "x86_64")]
const RA_FACTORED_OFFSET: u64 = 1;
#[cfg(target_arch = "aarch64")]
const RA_REG: u64 = 30; // X30 (LR)
#[cfg(target_arch = "aarch64")]
const RA_FACTORED_OFFSET: u64 = 2;

fn push_uleb(out: &mut Vec<u8>, mut value: u64) {
    loop {
        let chunk = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            out.push(chunk);
            return;
        }
        out.push(chunk | 0x80);
    }
}

impl EhFrame {
    fn register(rx: usize, finished: &Finished, regions: &[CfiRecord]) -> io::Result<Option<Self>> {
        if regions.is_empty() {
            return Ok(None);
        }

        let (bytes, _fde_offsets) =
            Self::build(rx, finished, regions, cfg!(target_vendor = "apple"))?;
        // SAFETY: the pointers stay valid until `drop` deregisters them with
        // the exact registered pointer. Concurrent registration races the
        // exit-time static deregistration in `_dl_fini`, so the JIT exit
        // drain stops and joins every worker before `exit()` can reach
        // it
        unsafe {
            #[cfg(target_vendor = "apple")]
            let fdes: Vec<*const u8> = {
                let base = bytes.as_ptr();
                let fdes: Vec<*const u8> = _fde_offsets.iter().map(|&off| base.add(off)).collect();
                for &fde in &fdes {
                    __register_frame(fde);
                }
                fdes
            };
            #[cfg(not(target_vendor = "apple"))]
            let fdes: Vec<*const u8> = {
                __register_frame(bytes.as_ptr());
                Vec::new()
            };
            Ok(Some(Self { bytes, fdes }))
        }
    }

    /// The same CIE/FDEs for the GDB JIT symfile, always with absolute
    /// addresses: debuggers read the section from the symfile rather than
    /// from relocated process memory. Without it lldb unwinds generated
    /// frames from the frame-pointer chain instead of this CFI and shows the
    /// bail handler twice.
    fn for_symfile(
        rx: usize,
        finished: &Finished,
        regions: &[CfiRecord],
    ) -> io::Result<Option<Vec<u8>>> {
        if regions.is_empty() {
            return Ok(None);
        }
        let (bytes, _) = Self::build(rx, finished, regions, false)?;
        Ok(Some(bytes.into_vec()))
    }

    /// Synthesize the CIE, one FDE per region and the terminator. `pcrel`
    /// selects PC-relative FDE addresses (Apple registration), otherwise the
    /// addresses are absolute. Returns the bytes and the FDE offsets.
    fn build(
        rx: usize,
        finished: &Finished,
        regions: &[CfiRecord],
        pcrel: bool,
    ) -> io::Result<(Box<[u8]>, Vec<usize>)> {
        let mut bytes = Vec::new();
        let mut fde_offsets = Vec::with_capacity(regions.len());

        // CIE with augmentation "zR" and no initial instructions, each FDE
        // sets its own CFA rule.
        let cie_at = bytes.len();
        bytes.extend_from_slice(&0u32.to_le_bytes()); // length, filled below
        bytes.extend_from_slice(&0u32.to_le_bytes()); // CIE id
        bytes.push(1); // version
        bytes.extend_from_slice(b"zR\0");
        push_uleb(&mut bytes, 1); // code alignment
        bytes.push(0x78); // data alignment: SLEB128(-8)
        bytes.push(RA_REG as u8);
        push_uleb(&mut bytes, 1); // augmentation data length
        bytes.push(if pcrel {
            DW_EH_PE_PCREL
        } else {
            DW_EH_PE_ABSPTR
        });
        while (bytes.len() - cie_at) % 4 != 0 {
            bytes.push(DW_CFA_NOP);
        }
        let cie_len = (bytes.len() - cie_at - 4) as u32;
        bytes[cie_at..cie_at + 4].copy_from_slice(&cie_len.to_le_bytes());

        // pc_begin targets, for the PC-relative fixup below.
        let mut pc_begins = Vec::with_capacity(regions.len());
        for region in regions {
            let start = label_offset(finished, region.start, "CFI region")? as usize;
            let end = label_offset(finished, region.end, "CFI region")? as usize;
            if end < start {
                return Err(io::Error::other("CFI region ends before it starts"));
            }
            pc_begins.push(rx + start);
            let fde_at = bytes.len();
            fde_offsets.push(fde_at);
            bytes.extend_from_slice(&0u32.to_le_bytes()); // length, filled below
            // CIE pointer: this field's address minus the CIE address.
            bytes.extend_from_slice(&((fde_at + 4 - cie_at) as u32).to_le_bytes());
            // NB: in `.eh_frame` (unlike `.debug_frame`) pc_begin/pc_range
            // come before the augmentation data length.
            // pc_begin: absolute, or a placeholder fixed up PC-relative below.
            if pcrel {
                bytes.extend_from_slice(&0u64.to_le_bytes());
            } else {
                bytes.extend_from_slice(&((rx + start) as u64).to_le_bytes());
            }
            bytes.extend_from_slice(&((end - start) as u64).to_le_bytes());
            push_uleb(&mut bytes, 0); // FDE augmentation data length
            bytes.push(DW_CFA_DEF_CFA);
            push_uleb(&mut bytes, CFA_REG);
            push_uleb(&mut bytes, region.cfa_offset as u64);
            bytes.push(DW_CFA_OFFSET | RA_REG as u8);
            push_uleb(&mut bytes, RA_FACTORED_OFFSET);
            while (bytes.len() - fde_at) % 4 != 0 {
                bytes.push(DW_CFA_NOP);
            }
            let fde_len = (bytes.len() - fde_at - 4) as u32;
            bytes[fde_at..fde_at + 4].copy_from_slice(&fde_len.to_le_bytes());
        }
        bytes.extend_from_slice(&0u32.to_le_bytes()); // terminator

        let mut bytes = bytes.into_boxed_slice();
        // With PC-relative addresses the fixup needs the final buffer address.
        // pc_begin sits 8 bytes into the FDE (length + CIE pointer), the range
        // is a plain length either way.
        if pcrel {
            let base = bytes.as_ptr() as usize;
            for (i, &fde_at) in fde_offsets.iter().enumerate() {
                let field = fde_at + 8;
                let rel = (pc_begins[i] as i64).wrapping_sub((base + field) as i64);
                bytes[field..field + 8].copy_from_slice(&rel.to_le_bytes());
            }
        }
        Ok((bytes, fde_offsets))
    }
}

impl Drop for EhFrame {
    fn drop(&mut self) {
        // SAFETY: each pointer was registered and the bytes are still alive,
        // fields drop after this runs.
        unsafe {
            #[cfg(target_vendor = "apple")]
            for &fde in &self.fdes {
                __deregister_frame(fde);
            }
            #[cfg(not(target_vendor = "apple"))]
            __deregister_frame(self.bytes.as_ptr());
        }
    }
}

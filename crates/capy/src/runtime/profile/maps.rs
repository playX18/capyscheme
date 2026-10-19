//! Debug-info maps
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, RwLock};

use crate::runtime::interp::markers;
use crate::runtime::value::proc::{SRC_FLAG_SYNTHETIC, span_short};
use crate::runtime::value::{Str, Symbol, Value, Vector};
use crate::runtime::{MutationContext, codegen};

/// One srcmap run: `[start, start + len)` carries span id `span` with
/// `flags` (`SRC_FLAG_*`).
#[derive(Clone, Copy)]
pub struct SpanRun {
    pub start: u32,
    pub len: u32,
    pub flags: u8,
    pub span: u32,
}

/// A procedure's span table
pub struct SpanTable {
    pub runs: Vec<SpanRun>,
    pub spans: Vec<String>,
}

impl SpanTable {
    /// The span covering `pc`
    pub fn span_at(&self, pc: u32) -> Option<Option<&str>> {
        let mut lo = 0usize;
        let mut hi = self.runs.len();
        while lo < hi {
            let mid = lo + (hi - lo) / 2;
            if self.runs[mid].start <= pc {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        if lo == 0 {
            return None;
        }
        let run = &self.runs[lo - 1];
        if pc.wrapping_sub(run.start) >= run.len {
            return None;
        }
        if run.span == 0 || run.flags & SRC_FLAG_SYNTHETIC != 0 {
            return Some(None);
        }
        let text = self.spans.get(run.span as usize - 1)?;
        Some(Some(text))
    }
}

/// Parse a packed srcmap value
fn parse_srcmap(srcmap: Value<'_>) -> SpanTable {
    let mut table = SpanTable {
        runs: Vec::new(),
        spans: Vec::new(),
    };
    if !srcmap.is::<Vector>() {
        return table;
    }
    let map = srcmap.downcast::<Vector>();
    if map.len() != 2 || !map[0].get().is::<Vector>() || !map[1].get().is::<Vector>() {
        return table;
    }
    let spans = map[0].get().downcast::<Vector>();
    for index in 0..spans.len() {
        match span_short(spans[index].get()) {
            Some(text) => table.spans.push(text),
            None => table.spans.push(String::new()),
        }
    }
    let runs = map[1].get().downcast::<Vector>();
    if runs.len() % 4 != 0 {
        return table;
    }
    for chunk in 0..runs.len() / 4 {
        let base = chunk * 4;
        let (Some(start), Some(len), Some(flags), Some(id)) = (
            runs[base].get().int32(),
            runs[base + 1].get().int32(),
            runs[base + 2].get().int32(),
            runs[base + 3].get().int32(),
        ) else {
            continue;
        };
        if start < 0 || len <= 0 || id < 0 || id as usize > table.spans.len() {
            continue;
        }
        table.runs.push(SpanRun {
            start: start as u32,
            len: len as u32,
            flags: flags as u8,
            span: id as u32,
        });
    }
    table
}

const CHECKSUM_BYTES: usize = 64;

pub(crate) fn checksum(code_len: usize, code: &[u8]) -> u32 {
    let mut hash = 0x811c_9dc5u32;
    for byte in code_len.to_le_bytes() {
        hash ^= byte as u32;
        hash = hash.wrapping_mul(0x0100_0193);
    }
    let prefix = &code[..code.len().min(CHECKSUM_BYTES)];
    for byte in prefix {
        hash ^= *byte as u32;
        hash = hash.wrapping_mul(0x0100_0193);
    }
    hash
}

/// Interpreted procedure: bytecode range, content checksum, name, spans.
pub struct InterpEntry {
    pub start: u64,
    pub len: u32,
    checksum: u32,
    pub name: String,
    pub table: Arc<SpanTable>,
}

/// Compiled procedure
pub struct JitEntry {
    pub base: u64,
    pub end: u64,
    pub code_end: u32,
    pub name: String,
    pub reverse: Vec<(u32, u32)>,
    pub table: Arc<SpanTable>,
}

/// Bytecode `pc` for a machine offset in sorted `reverse` rows, or
/// `None` past `code_end`.
pub fn reverse_lookup(reverse: &[(u32, u32)], code_end: u32, offset: u32) -> Option<u32> {
    if offset >= code_end {
        return None;
    }
    let mut lo = 0usize;
    let mut hi = reverse.len();
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if reverse[mid].0 <= offset {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    if lo == 0 {
        return None;
    }
    Some(reverse[lo - 1].1)
}

static INTERP: RwLock<BTreeMap<u64, Arc<InterpEntry>>> = RwLock::new(BTreeMap::new());
static JIT: RwLock<BTreeMap<u64, Arc<JitEntry>>> = RwLock::new(BTreeMap::new());

/// Greatest entry at or below `addr` whose range contains it.
fn lookup_range<T>(
    map: &BTreeMap<u64, T>,
    addr: u64,
    range: impl Fn(&T) -> (u64, u64),
) -> Option<&T> {
    let (_, entry) = map.range(..=addr).next_back()?;
    let (start, end) = range(entry);
    if addr >= start && addr < end {
        Some(entry)
    } else {
        None
    }
}

/// Drop entries overlapping `[start, end)`: a new live object there
/// proves them stale (live objects never overlap).
fn evict_overlapping<T>(
    map: &mut BTreeMap<u64, T>,
    start: u64,
    end: u64,
    range: impl Fn(&T) -> (u64, u64),
) {
    map.retain(|_, entry| {
        let (entry_start, entry_end) = range(entry);
        entry_end <= start || entry_start >= end
    });
}

/// Procedure name from block metadata, best effort.
fn block_name<'gc>(ctx: MutationContext<'gc>, info: Value<'gc>) -> String {
    if info.is_pair() {
        let name = info.assq(ctx.intern("name"));
        if let Some(pair) = name {
            let value = pair.cdr();
            if value.is::<Symbol>() {
                return value.downcast::<Symbol>().as_str().into_owned();
            }
            if value.is::<Str>() {
                return value.downcast::<Str>().to_string();
            }
        }
    }
    String::from("anonymous")
}

/// Publish an interpreted procedure's debug info. Called from
/// [`CodeBlock::new_bytecode`](crate::runtime::value::proc::CodeBlock)
pub(crate) fn publish_interp<'gc>(
    ctx: MutationContext<'gc>,
    entry: u64,
    code: &[u8],
    srcmap: Value<'gc>,
    info: Value<'gc>,
) {
    let table = Arc::new(parse_srcmap(srcmap));
    let name = block_name(ctx, info);
    let end = entry.wrapping_add(code.len() as u64);
    let mut map = INTERP.write().unwrap();
    evict_overlapping(&mut map, entry, end, |entry| {
        (entry.start, entry.start + entry.len as u64)
    });
    map.insert(
        entry,
        Arc::new(InterpEntry {
            start: entry,
            len: code.len() as u32,
            checksum: checksum(code.len(), code),
            name,
            table,
        }),
    );
}

/// Greatest interpreted entry containing `addr`, checksum-verified.
/// A mismatch removes the entry (freed and reused memory) and reports
/// nothing.
fn lookup_interp(addr: u64) -> Option<Arc<InterpEntry>> {
    let map = INTERP.read().unwrap();
    let entry = lookup_range(&map, addr, |entry| {
        (entry.start, entry.start + entry.len as u64)
    })?;
    // SAFETY: bytecode buffers live in `NonMoving` space, which the GC
    // never unmaps: the bytes are readable (if stale) whatever state
    // the buffer is in.
    let live = unsafe { std::slice::from_raw_parts(entry.start as *const u8, entry.len as usize) };
    if checksum(entry.len as usize, live) != entry.checksum {
        let start = entry.start;
        drop(map);
        INTERP.write().unwrap().remove(&start);
        return None;
    }
    Some(Arc::clone(entry))
}

pub(crate) fn reap_interp() {
    let starts: Vec<u64> = INTERP.read().unwrap().keys().copied().collect();
    for start in starts {
        lookup_interp(start);
    }
}

/// Publish a compiled procedure's debug info
pub(crate) fn publish_jit<'gc>(
    ctx: MutationContext<'gc>,
    base: u64,
    end: u64,
    code_end: u32,
    mut reverse: Vec<(u32, u32)>,
    bytecode_entry: u64,
    srcmap: Value<'gc>,
    info: Value<'gc>,
) {
    reverse.sort_by_key(|row| row.0);
    reverse.dedup_by_key(|row| row.0);
    let (name, table) = match lookup_interp(bytecode_entry) {
        Some(entry) => (entry.name.clone(), Arc::clone(&entry.table)),
        None => (block_name(ctx, info), Arc::new(parse_srcmap(srcmap))),
    };
    let mut map = JIT.write().unwrap();
    evict_overlapping(&mut map, base, end, |entry| (entry.base, entry.end));
    map.insert(
        base,
        Arc::new(JitEntry {
            base,
            end,
            code_end,
            name,
            reverse,
            table,
        }),
    );
}

/// Remove a compiled procedure's entry
pub(crate) fn remove_jit(base: u64) {
    JIT.write().unwrap().remove(&base);
}

fn lookup_jit(addr: u64) -> Option<Arc<JitEntry>> {
    let map = JIT.read().unwrap();
    let entry = lookup_range(&map, addr, |entry| (entry.base, entry.end))?;
    Some(Arc::clone(entry))
}

/// Machine span containing `ra`
pub(crate) fn handler_code_span(ra: u64) -> Option<(u64, u64)> {
    let map = JIT.try_read().ok()?;
    let entry = lookup_range(&map, ra, |entry| (entry.base, entry.end))?;
    Some((entry.base, entry.end))
}

/// Bytecode span containing `return-point`
pub(crate) fn handler_bytecode_span(retpt: u64) -> Option<(u64, u64)> {
    let map = INTERP.try_read().ok()?;
    let entry = lookup_range(&map, retpt, |entry| {
        (entry.start, entry.start + entry.len as u64)
    })?;
    Some((entry.start, entry.start + entry.len as u64))
}

static NATIVE_NAMES: std::sync::LazyLock<RwLock<HashMap<u64, String>>> =
    std::sync::LazyLock::new(|| RwLock::new(HashMap::new()));

pub(crate) fn note_native_closure(closure_bits: u64, name: String) {
    NATIVE_NAMES.write().unwrap().insert(closure_bits, name);
}

fn native_name(closure_bits: u64) -> Option<String> {
    NATIVE_NAMES.read().unwrap().get(&closure_bits).cloned()
}

fn template_range() -> Option<(u64, u64)> {
    codegen::template_image::code_range()
}

/// Whether `addr` lies in template-interpreter code
fn in_interp_code(addr: u64) -> bool {
    if let Some((base, end)) = template_range() {
        if addr >= base && addr < end {
            return true;
        }
    }
    codegen::template_lazy::contains(addr)
}

static NATIVE_SYMBOLS: RwLock<BTreeMap<u64, String>> = RwLock::new(BTreeMap::new());
const NATIVE_SYMBOL_CAP: usize = 4096;

fn backtrace_name(rip: u64) -> Option<String> {
    {
        let cache = NATIVE_SYMBOLS.read().unwrap();
        if let Some((_, name)) = cache.range(..=rip).next_back() {
            return Some(name.clone());
        }
    }
    let mut found: Option<(u64, String)> = None;
    backtrace::resolve(rip as *mut libc::c_void, |symbol| {
        if found.is_some() {
            return;
        }
        let name = symbol
            .name()
            .map(|name| name.to_string())
            .unwrap_or_default();
        if name.is_empty() {
            return;
        }
        let start = symbol.addr().map(|addr| addr as u64).unwrap_or(rip);
        found = Some((start, name));
    });
    let (start, name) = found?;
    let mut cache = NATIVE_SYMBOLS.write().unwrap();
    if cache.len() >= NATIVE_SYMBOL_CAP {
        cache.clear();
    }
    cache.insert(start, name.clone());
    Some(name)
}

fn sanitize(label: &str) -> String {
    label
        .chars()
        .map(|cell| match cell {
            ';' => ':',
            '\n' | '\r' => ' ',
            _ => cell,
        })
        .collect()
}

fn span_label(name: &str, table: &SpanTable, pc: u64, base: u64) -> String {
    let offset = pc.wrapping_sub(base) as u32;
    match table.span_at(offset) {
        Some(Some(span)) => format!("{name} {span}"),
        Some(None) => format!("{name} <generated>"),
        None => format!("{name} ?:?:?"),
    }
}

/// Symbolize a caller frame
pub fn symbolize_caller(ra: u64, resume: u64, closure: u64) -> String {
    if let Some(name) = native_name(closure) {
        return sanitize(&name);
    }
    if ra == markers::interpreter_return() as u64 {
        if let Some(entry) = lookup_interp(resume) {
            return sanitize(&span_label(&entry.name, &entry.table, resume, entry.start));
        }
        return String::from("???");
    }
    if ra == markers::raise_ret() as u64
        || ra == markers::subr_ret() as u64
        || ra == markers::scheme_exit() as u64
        || ra == markers::underflow() as u64
    {
        return String::from("???");
    }
    if in_interp_code(ra) {
        return String::from("???");
    }
    if let Some(entry) = lookup_jit(ra) {
        let offset = ra.wrapping_sub(entry.base) as u32;
        // The span table is keyed by bytecode pc.
        if let Some(pc) = reverse_lookup(&entry.reverse, entry.code_end, offset) {
            let label = match entry.table.span_at(pc) {
                Some(Some(span)) => format!("{} {span}", entry.name),
                Some(None) => format!("{} <generated>", entry.name),
                None => format!("{} ?:?:?", entry.name),
            };
            return sanitize(&label);
        }
        return sanitize(&format!("{} ?:?:?", entry.name));
    }
    String::from("???")
}

pub fn symbolize_running(rip: u64, pcreg: u64, closure: u64) -> String {
    if let Some(name) = native_name(closure) {
        return sanitize(&name);
    }
    if in_interp_code(rip) {
        if let Some(entry) = lookup_interp(pcreg) {
            return sanitize(&span_label(&entry.name, &entry.table, pcreg, entry.start));
        }
        return String::from("???");
    }
    if let Some(entry) = lookup_jit(rip) {
        let offset = rip.wrapping_sub(entry.base) as u32;
        if let Some(pc) = reverse_lookup(&entry.reverse, entry.code_end, offset) {
            let label = match entry.table.span_at(pc) {
                Some(Some(span)) => format!("{} {span}", entry.name),
                Some(None) => format!("{} <generated>", entry.name),
                None => format!("{} ?:?:?", entry.name),
            };
            return sanitize(&label);
        }
        return sanitize(&format!("{} ?:?:?", entry.name));
    }
    if let Some(name) = backtrace_name(rip) {
        return sanitize(&name);
    }
    String::from("???")
}

use std::{
    collections::HashMap,
    io::{self, Cursor, Read},
};

use mmtk::util::metadata::side_metadata::{
    global_side_metadata_vm_base_address, vo_bit_side_metadata_addr,
};

use crate::{
    rsgc::{Gc, Trace, Visitor, WeakProcessor},
    runtime::{
        Context,
        code_memory::{CodeMemory, LoadedCodeRef},
        fasl::FASLReader,
        symbols::{RuntimeData, RuntimeThunk},
        value::{Closure, CodeArity, CodeBlock, Value, Vector},
    },
};

pub(crate) const CODE_IMAGE_VERSION: u32 = 1;
const MAGIC: &[u8; 8] = b"CAPYFSL\0";
#[cfg(target_arch = "x86_64")]
const X86_64_ABSOLUTE_JUMP_STUB_LEN: usize = 13;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompiledCodeImage {
    pub version: u32,
    pub target_triple: String,
    pub constants_fasl: Vec<u8>,
    pub code: Vec<CodeEntry>,
    pub data_slots: Vec<DataSlot>,
    pub relocations: Vec<Relocation>,
    pub entry_code_id: u32,
    pub entry_is_cont: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodeEntry {
    pub id: u32,
    pub name: String,
    pub bytes: Vec<u8>,
    pub entry_offset: u32,
    pub arity: i32,
    pub is_cont: bool,
    pub metadata_constant: Option<u32>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataSlot {
    pub id: u32,
    pub kind: DataSlotKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataSlotKind {
    Constant { constant_index: u32 },
    CacheCell,
    CodeBlock { code_id: u32 },
    Pointer { target: RelocationTarget },
    SideMetadata { kind: SideMetadataSlotKind },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SideMetadataSlotKind {
    Global,
    VoBit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Relocation {
    pub code_id: u32,
    pub offset: u32,
    pub kind: RelocationKind,
    pub target: RelocationTarget,
    pub addend: i64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocationKind {
    Abs8,
    X86PcRel4,
    X86CallPcRel4,
    PointerSlotLoad,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocationTarget {
    Code { code_id: u32 },
    DataSlot { slot_id: u32 },
    RuntimeThunk { thunk_id: u32 },
    RuntimeData { data_id: u32 },
}

pub struct LoadedCodeImage<'gc> {
    memory: CodeMemory,
    entry_code_id: u32,
    constants: Vec<Value<'gc>>,
    code_blocks: Vec<(u32, Gc<'gc, CodeBlock<'gc>>)>,
    data_slots: Vec<LoadedDataSlot<'gc>>,
    entry_closure: Value<'gc>,
}

unsafe impl<'gc> Trace for LoadedCodeImage<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            self.constants.trace(visitor);
            self.code_blocks.trace(visitor);
            self.data_slots.trace(visitor);
            self.entry_closure.trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            self.constants.process_weak_refs(weak_processor);
            self.code_blocks.process_weak_refs(weak_processor);
            self.data_slots.process_weak_refs(weak_processor);
            self.entry_closure.process_weak_refs(weak_processor);
        }
    }
}

#[derive(Clone)]
struct LoadedCodeEntry {
    id: u32,
    handle: u32,
    base: crate::rsgc::mmtk::util::Address,
    entrypoint: crate::rsgc::mmtk::util::Address,
    call_stub_offsets: HashMap<u32, u32>,
}

pub(crate) struct RuntimeRelocationSite {
    pub handle: u32,
    pub base: crate::rsgc::mmtk::util::Address,
    pub call_stub_offsets: HashMap<u32, u32>,
}

impl RuntimeRelocationSite {
    pub fn from_loaded_code(loaded: LoadedCodeRef, call_stub_offsets: HashMap<u32, u32>) -> Self {
        Self {
            handle: loaded.handle,
            base: loaded.entrypoint,
            call_stub_offsets,
        }
    }
}

struct LoadedDataSlot<'gc> {
    id: u32,
    storage: LoadedDataSlotStorage<'gc>,
}

unsafe impl<'gc> Trace for LoadedDataSlot<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        unsafe {
            if let LoadedDataSlotStorage::Value(value) = &mut self.storage {
                value.trace(visitor);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        unsafe {
            if let LoadedDataSlotStorage::Value(value) = &mut self.storage {
                value.process_weak_refs(weak_processor);
            }
        }
    }
}

enum LoadedDataSlotStorage<'gc> {
    Value(Value<'gc>),
    Raw(usize),
}

impl<'gc> LoadedDataSlot<'gc> {
    fn value_address(&self) -> crate::rsgc::mmtk::util::Address {
        match &self.storage {
            LoadedDataSlotStorage::Value(value) => {
                crate::rsgc::mmtk::util::Address::from_ptr(value)
            }
            LoadedDataSlotStorage::Raw(word) => crate::rsgc::mmtk::util::Address::from_ptr(word),
        }
    }

    fn trace_value(id: u32, value: Value<'gc>) -> Self {
        Self {
            id,
            storage: LoadedDataSlotStorage::Value(value),
        }
    }

    fn raw_word(id: u32, word: usize) -> Self {
        Self {
            id,
            storage: LoadedDataSlotStorage::Raw(word),
        }
    }

    fn value(&self) -> Option<Value<'gc>> {
        match self.storage {
            LoadedDataSlotStorage::Value(value) => Some(value),
            LoadedDataSlotStorage::Raw(_) => None,
        }
    }

    #[cfg(test)]
    fn raw_word_value(&self) -> Option<usize> {
        match self.storage {
            LoadedDataSlotStorage::Value(_) => None,
            LoadedDataSlotStorage::Raw(word) => Some(word),
        }
    }
}

impl<'gc> LoadedCodeImage<'gc> {
    pub fn load(ctx: Context<'gc>, image: CompiledCodeImage) -> io::Result<Self> {
        if image.version != CODE_IMAGE_VERSION {
            return Err(invalid_data("unsupported code image version"));
        }
        if image.target_triple != target_lexicon::Triple::host().to_string() {
            return Err(invalid_data("code image target does not match host"));
        }
        let constants = decode_constants(ctx, &image.constants_fasl)?;
        let mut memory = CodeMemory::new();
        let mut loaded_entries = Vec::with_capacity(image.code.len());
        for entry in &image.code {
            let (bytes, call_stub_offsets) = code_bytes_with_call_stubs(entry, &image.relocations)?;
            let loaded = memory.allocate_copy(&bytes)?;
            loaded_entries.push(LoadedCodeEntry {
                id: entry.id,
                handle: loaded.handle,
                base: loaded.entrypoint,
                entrypoint: loaded.entrypoint + entry.entry_offset as usize,
                call_stub_offsets,
            });
        }

        let mut code_blocks = Vec::with_capacity(image.code.len());
        for entry in &image.code {
            let loaded = loaded_entries
                .iter()
                .find(|loaded| loaded.id == entry.id)
                .ok_or_else(|| invalid_data("loaded code entry missing"))?;
            let metadata = entry
                .metadata_constant
                .map(|index| constant_at(&constants, index))
                .transpose()?
                .unwrap_or_else(|| Value::new(false));
            let code_block = CodeBlock::new_loaded(
                ctx,
                loaded.entrypoint,
                CodeArity::new(entry.arity),
                entry.is_cont,
                metadata,
                loaded.handle,
            );
            code_blocks.push((entry.id, code_block));
        }
        let data_slots =
            initialize_data_slots(&constants, &code_blocks, &loaded_entries, &image.data_slots)?;
        for reloc in &image.relocations {
            apply_relocation(&mut memory, &loaded_entries, &data_slots, reloc)?;
        }
        let entry_code_block = find_code_block_in(&code_blocks, image.entry_code_id)?;
        let entry_closure = Value::from(Closure::new(
            ctx,
            entry_code_block,
            &[],
            image.entry_is_cont,
        ));

        let loaded = Self {
            memory,
            entry_code_id: image.entry_code_id,
            constants,
            code_blocks,
            data_slots,
            entry_closure,
        };
        loaded.find_code_block(image.entry_code_id)?;
        Ok(loaded)
    }

    pub fn entry_code_block(&self) -> Gc<'gc, CodeBlock<'gc>> {
        self.find_code_block(self.entry_code_id)
            .expect("loaded code image entry block should exist")
    }

    pub fn entry_closure(&self) -> Value<'gc> {
        self.entry_closure
    }

    pub fn code_span_count(&self) -> usize {
        self.memory.span_count()
    }

    pub fn data_slot_value(&self, slot_id: u32) -> Option<Value<'gc>> {
        self.data_slots
            .iter()
            .find_map(|slot| (slot.id == slot_id).then(|| slot.value()).flatten())
    }

    fn find_code_block(&self, code_id: u32) -> io::Result<Gc<'gc, CodeBlock<'gc>>> {
        self.code_blocks
            .iter()
            .find_map(|(id, code_block)| (*id == code_id).then_some(*code_block))
            .ok_or_else(|| invalid_data("code image entry code id not found"))
    }
}

fn decode_constants<'gc>(ctx: Context<'gc>, bytes: &[u8]) -> io::Result<Vec<Value<'gc>>> {
    if bytes.is_empty() {
        return Ok(Vec::new());
    }
    let constants = FASLReader::new(ctx, Cursor::new(bytes)).read()?;
    let constants = constants
        .try_as::<Vector>()
        .ok_or_else(|| invalid_data("code image constants payload is not a vector"))?;
    Ok(constants.as_slice().to_vec())
}

fn constant_at<'gc>(constants: &[Value<'gc>], index: u32) -> io::Result<Value<'gc>> {
    constants
        .get(index as usize)
        .copied()
        .ok_or_else(|| invalid_data("code image constant index out of bounds"))
}

fn initialize_data_slots<'gc>(
    constants: &[Value<'gc>],
    code_blocks: &[(u32, Gc<'gc, CodeBlock<'gc>>)],
    loaded_entries: &[LoadedCodeEntry],
    slots: &[DataSlot],
) -> io::Result<Vec<LoadedDataSlot<'gc>>> {
    let mut data_slots = Vec::with_capacity(slots.len());
    for slot in slots {
        let data_slot = match slot.kind {
            DataSlotKind::Constant { constant_index } => {
                LoadedDataSlot::trace_value(slot.id, constant_at(constants, constant_index)?)
            }
            DataSlotKind::CacheCell => LoadedDataSlot::trace_value(slot.id, Value::new(false)),
            DataSlotKind::CodeBlock { code_id } => {
                let code_block = code_blocks
                    .iter()
                    .find_map(|(id, code_block)| (*id == code_id).then_some(*code_block))
                    .ok_or_else(|| invalid_data("code-block data slot target not found"))?;
                LoadedDataSlot::trace_value(slot.id, Value::from(code_block))
            }
            DataSlotKind::Pointer { target } => {
                let word =
                    resolve_relocation_target(loaded_entries, &data_slots, &target)?.as_usize();
                LoadedDataSlot::raw_word(slot.id, word)
            }
            DataSlotKind::SideMetadata { kind } => {
                LoadedDataSlot::raw_word(slot.id, side_metadata_address(kind))
            }
        };
        data_slots.push(data_slot);
    }
    Ok(data_slots)
}

fn side_metadata_address(kind: SideMetadataSlotKind) -> usize {
    match kind {
        SideMetadataSlotKind::Global => global_side_metadata_vm_base_address().as_usize(),
        SideMetadataSlotKind::VoBit => vo_bit_side_metadata_addr().as_usize(),
    }
}

fn find_code_block_in<'gc>(
    code_blocks: &[(u32, Gc<'gc, CodeBlock<'gc>>)],
    code_id: u32,
) -> io::Result<Gc<'gc, CodeBlock<'gc>>> {
    code_blocks
        .iter()
        .find_map(|(id, code_block)| (*id == code_id).then_some(*code_block))
        .ok_or_else(|| invalid_data("code image code id not found"))
}

fn apply_relocation(
    memory: &mut CodeMemory,
    loaded_entries: &[LoadedCodeEntry],
    data_slots: &[LoadedDataSlot<'_>],
    reloc: &Relocation,
) -> io::Result<()> {
    let code = loaded_entries
        .iter()
        .find(|entry| entry.id == reloc.code_id)
        .ok_or_else(|| invalid_data("relocation code id not found"))?;
    let target = resolve_relocation_target(loaded_entries, data_slots, &reloc.target)?;

    let site = RuntimeRelocationSite {
        handle: code.handle,
        base: code.base,
        call_stub_offsets: code.call_stub_offsets.clone(),
    };
    apply_resolved_relocation(
        memory,
        &site,
        reloc.offset,
        reloc.kind,
        target,
        reloc.addend,
    )
}

pub(crate) fn code_bytes_with_call_stubs(
    entry: &CodeEntry,
    relocations: &[Relocation],
) -> io::Result<(Vec<u8>, HashMap<u32, u32>)> {
    code_bytes_with_call_stubs_for(entry.id, &entry.bytes, relocations)
}

pub(crate) fn code_bytes_with_call_stubs_for(
    code_id: u32,
    input_bytes: &[u8],
    relocations: &[Relocation],
) -> io::Result<(Vec<u8>, HashMap<u32, u32>)> {
    let mut bytes = input_bytes.to_vec();
    let mut call_stub_offsets = HashMap::new();
    for reloc in relocations
        .iter()
        .filter(|reloc| reloc.code_id == code_id && reloc.kind == RelocationKind::X86CallPcRel4)
    {
        if call_stub_offsets.contains_key(&reloc.offset) {
            return Err(invalid_data("duplicate call relocation offset"));
        }
        let stub_offset = u32::try_from(bytes.len())
            .map_err(|_| invalid_data("code image entry is too large"))?;
        call_stub_offsets.insert(reloc.offset, stub_offset);
        bytes.extend_from_slice(&empty_absolute_jump_stub()?);
    }
    Ok((bytes, call_stub_offsets))
}

pub(crate) fn apply_runtime_relocation(
    memory: &mut CodeMemory,
    site: &RuntimeRelocationSite,
    reloc: &Relocation,
) -> io::Result<()> {
    let target = match reloc.target {
        RelocationTarget::RuntimeThunk { .. } | RelocationTarget::RuntimeData { .. } => {
            resolve_runtime_relocation_target(&reloc.target)?
        }
        RelocationTarget::Code { .. } | RelocationTarget::DataSlot { .. } => {
            return Err(invalid_data(
                "runtime relocation target is not supported here",
            ));
        }
    };
    apply_resolved_relocation(memory, site, reloc.offset, reloc.kind, target, reloc.addend)
}

fn apply_resolved_relocation(
    memory: &mut CodeMemory,
    site: &RuntimeRelocationSite,
    offset: u32,
    kind: RelocationKind,
    target: crate::rsgc::mmtk::util::Address,
    addend: i64,
) -> io::Result<()> {
    match kind {
        RelocationKind::Abs8 => {
            let target = add_i64_to_usize(target.as_usize(), addend)?;
            let bytes = target.to_le_bytes();
            memory.patch(site.handle, offset as usize, &bytes)
        }
        RelocationKind::X86PcRel4 | RelocationKind::X86CallPcRel4 => {
            apply_x86_pc_rel4(memory, site, offset, kind, target.as_usize(), addend)
        }
        RelocationKind::PointerSlotLoad => Err(invalid_data(
            "pointer-slot relocations are not supported by loader yet",
        )),
    }
}

fn empty_absolute_jump_stub() -> io::Result<Vec<u8>> {
    #[cfg(target_arch = "x86_64")]
    {
        Ok(vec![0; X86_64_ABSOLUTE_JUMP_STUB_LEN])
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        Err(invalid_data(
            "call stubs are only supported on x86_64 targets",
        ))
    }
}

fn apply_x86_pc_rel4(
    memory: &mut CodeMemory,
    site: &RuntimeRelocationSite,
    offset: u32,
    kind: RelocationKind,
    target: usize,
    addend: i64,
) -> io::Result<()> {
    let patch_address = add_i64_to_usize(site.base.as_usize(), offset as i64)?;
    let relocated_target = add_i64_to_usize(target, addend)?;
    if let Some(displacement) = rel32_displacement(patch_address, relocated_target) {
        return memory.patch(site.handle, offset as usize, &displacement.to_le_bytes());
    }

    if kind != RelocationKind::X86CallPcRel4 {
        return Err(invalid_data("pc-relative relocation target out of range"));
    }

    let stub_offset = site
        .call_stub_offsets
        .get(&offset)
        .copied()
        .ok_or_else(|| invalid_data("missing call stub for out-of-range relocation"))?;
    let stub_address = add_i64_to_usize(site.base.as_usize(), stub_offset as i64)?;
    let displacement = rel32_displacement(patch_address, add_i64_to_usize(stub_address, addend)?)
        .ok_or_else(|| invalid_data("call stub relocation target out of range"))?;
    memory.patch(site.handle, offset as usize, &displacement.to_le_bytes())?;
    memory.patch(
        site.handle,
        stub_offset as usize,
        &absolute_jump_stub(target)?,
    )
}

fn rel32_displacement(patch_address: usize, target: usize) -> Option<i32> {
    let displacement = target as i128 - patch_address as i128;
    i32::try_from(displacement).ok()
}

fn absolute_jump_stub(target: usize) -> io::Result<Vec<u8>> {
    #[cfg(target_arch = "x86_64")]
    {
        let mut stub = Vec::with_capacity(X86_64_ABSOLUTE_JUMP_STUB_LEN);
        // movabs r11, imm64; jmp r11
        stub.extend_from_slice(&[0x49, 0xbb]);
        stub.extend_from_slice(&target.to_le_bytes());
        stub.extend_from_slice(&[0x41, 0xff, 0xe3]);
        Ok(stub)
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        let _ = target;
        Err(invalid_data(
            "call stubs are only supported on x86_64 targets",
        ))
    }
}

fn resolve_relocation_target(
    loaded_entries: &[LoadedCodeEntry],
    data_slots: &[LoadedDataSlot<'_>],
    target: &RelocationTarget,
) -> io::Result<crate::rsgc::mmtk::util::Address> {
    match target {
        RelocationTarget::Code { code_id } => loaded_entries
            .iter()
            .find(|entry| entry.id == *code_id)
            .map(|entry| entry.entrypoint)
            .ok_or_else(|| invalid_data("relocation target code id not found")),
        RelocationTarget::DataSlot { slot_id } => data_slots
            .iter()
            .find(|slot| slot.id == *slot_id)
            .map(LoadedDataSlot::value_address)
            .ok_or_else(|| invalid_data("relocation target data slot id not found")),
        RelocationTarget::RuntimeThunk { .. } | RelocationTarget::RuntimeData { .. } => {
            resolve_runtime_relocation_target(target)
        }
    }
}

pub(crate) fn resolve_runtime_relocation_target(
    target: &RelocationTarget,
) -> io::Result<crate::rsgc::mmtk::util::Address> {
    match target {
        RelocationTarget::RuntimeThunk { thunk_id } => RuntimeThunk::from_id(*thunk_id)
            .map(|thunk| thunk.address())
            .ok_or_else(|| invalid_data("unknown runtime thunk relocation target")),
        RelocationTarget::RuntimeData { data_id } => RuntimeData::from_id(*data_id)
            .map(|data| data.address())
            .ok_or_else(|| invalid_data("unknown runtime data relocation target")),
        RelocationTarget::Code { .. } | RelocationTarget::DataSlot { .. } => {
            Err(invalid_data("relocation target is not a runtime symbol"))
        }
    }
}

fn add_i64_to_usize(base: usize, addend: i64) -> io::Result<usize> {
    let value = base as i128 + addend as i128;
    usize::try_from(value).map_err(|_| invalid_data("relocation address overflow"))
}

impl CompiledCodeImage {
    pub fn encode(&self) -> io::Result<Vec<u8>> {
        let mut out = Vec::new();
        out.extend_from_slice(MAGIC);
        put_u32(&mut out, self.version);
        put_string(&mut out, &self.target_triple)?;
        put_bytes(&mut out, &self.constants_fasl)?;
        put_u32(&mut out, checked_len(self.code.len())?);
        for entry in &self.code {
            put_u32(&mut out, entry.id);
            put_string(&mut out, &entry.name)?;
            put_bytes(&mut out, &entry.bytes)?;
            put_u32(&mut out, entry.entry_offset);
            put_i32(&mut out, entry.arity);
            put_bool(&mut out, entry.is_cont);
            match entry.metadata_constant {
                Some(index) => {
                    put_bool(&mut out, true);
                    put_u32(&mut out, index);
                }
                None => put_bool(&mut out, false),
            }
        }
        put_u32(&mut out, checked_len(self.data_slots.len())?);
        for slot in &self.data_slots {
            put_u32(&mut out, slot.id);
            match slot.kind {
                DataSlotKind::Constant { constant_index } => {
                    put_u8(&mut out, 0);
                    put_u32(&mut out, constant_index);
                }
                DataSlotKind::CacheCell => put_u8(&mut out, 1),
                DataSlotKind::CodeBlock { code_id } => {
                    put_u8(&mut out, 2);
                    put_u32(&mut out, code_id);
                }
                DataSlotKind::Pointer { target } => {
                    put_u8(&mut out, 3);
                    put_relocation_target(&mut out, &target);
                }
                DataSlotKind::SideMetadata { kind } => {
                    put_u8(&mut out, 4);
                    put_u8(&mut out, kind.to_tag());
                }
            }
        }
        put_u32(&mut out, checked_len(self.relocations.len())?);
        for reloc in &self.relocations {
            put_u32(&mut out, reloc.code_id);
            put_u32(&mut out, reloc.offset);
            put_u8(&mut out, reloc.kind.to_tag());
            put_relocation_target(&mut out, &reloc.target);
            put_i64(&mut out, reloc.addend);
        }
        put_u32(&mut out, self.entry_code_id);
        put_bool(&mut out, self.entry_is_cont);
        Ok(out)
    }

    pub fn decode(bytes: &[u8]) -> io::Result<Self> {
        let mut input = Cursor::new(bytes);
        let mut magic = [0; MAGIC.len()];
        input.read_exact(&mut magic)?;
        if &magic != MAGIC {
            return Err(invalid_data("invalid code image magic"));
        }
        let version = read_u32(&mut input)?;
        let target_triple = read_string(&mut input)?;
        let constants_fasl = read_bytes(&mut input)?;

        let code_len = read_len(&mut input)?;
        let mut code = Vec::with_capacity(code_len);
        for _ in 0..code_len {
            let id = read_u32(&mut input)?;
            let name = read_string(&mut input)?;
            let bytes = read_bytes(&mut input)?;
            let entry_offset = read_u32(&mut input)?;
            let arity = read_i32(&mut input)?;
            let is_cont = read_bool(&mut input)?;
            let metadata_constant = if read_bool(&mut input)? {
                Some(read_u32(&mut input)?)
            } else {
                None
            };
            code.push(CodeEntry {
                id,
                name,
                bytes,
                entry_offset,
                arity,
                is_cont,
                metadata_constant,
            });
        }

        let data_slots_len = read_len(&mut input)?;
        let mut data_slots = Vec::with_capacity(data_slots_len);
        for _ in 0..data_slots_len {
            let id = read_u32(&mut input)?;
            let kind = match read_u8(&mut input)? {
                0 => DataSlotKind::Constant {
                    constant_index: read_u32(&mut input)?,
                },
                1 => DataSlotKind::CacheCell,
                2 => DataSlotKind::CodeBlock {
                    code_id: read_u32(&mut input)?,
                },
                3 => DataSlotKind::Pointer {
                    target: read_relocation_target(&mut input)?,
                },
                4 => DataSlotKind::SideMetadata {
                    kind: SideMetadataSlotKind::from_tag(read_u8(&mut input)?)?,
                },
                _ => return Err(invalid_data("invalid data slot kind")),
            };
            data_slots.push(DataSlot { id, kind });
        }

        let reloc_len = read_len(&mut input)?;
        let mut relocations = Vec::with_capacity(reloc_len);
        for _ in 0..reloc_len {
            let code_id = read_u32(&mut input)?;
            let offset = read_u32(&mut input)?;
            let kind = RelocationKind::from_tag(read_u8(&mut input)?)?;
            let target = read_relocation_target(&mut input)?;
            let addend = read_i64(&mut input)?;
            relocations.push(Relocation {
                code_id,
                offset,
                kind,
                target,
                addend,
            });
        }

        let entry_code_id = read_u32(&mut input)?;
        let entry_is_cont = read_bool(&mut input)?;
        if input.position() != bytes.len() as u64 {
            return Err(invalid_data("trailing bytes in code image"));
        }

        Ok(Self {
            version,
            target_triple,
            constants_fasl,
            code,
            data_slots,
            relocations,
            entry_code_id,
            entry_is_cont,
        })
    }
}

impl RelocationKind {
    fn to_tag(self) -> u8 {
        match self {
            Self::Abs8 => 0,
            Self::X86PcRel4 => 1,
            Self::X86CallPcRel4 => 2,
            Self::PointerSlotLoad => 3,
        }
    }

    fn from_tag(tag: u8) -> io::Result<Self> {
        match tag {
            0 => Ok(Self::Abs8),
            1 => Ok(Self::X86PcRel4),
            2 => Ok(Self::X86CallPcRel4),
            3 => Ok(Self::PointerSlotLoad),
            _ => Err(invalid_data("invalid relocation kind")),
        }
    }
}

impl SideMetadataSlotKind {
    fn to_tag(self) -> u8 {
        match self {
            Self::Global => 0,
            Self::VoBit => 1,
        }
    }

    fn from_tag(tag: u8) -> io::Result<Self> {
        match tag {
            0 => Ok(Self::Global),
            1 => Ok(Self::VoBit),
            _ => Err(invalid_data("invalid side metadata slot kind")),
        }
    }
}

fn checked_len(len: usize) -> io::Result<u32> {
    u32::try_from(len).map_err(|_| invalid_data("code image field too large"))
}

fn put_u8(out: &mut Vec<u8>, value: u8) {
    out.push(value);
}

fn put_bool(out: &mut Vec<u8>, value: bool) {
    put_u8(out, value as u8);
}

fn put_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put_i32(out: &mut Vec<u8>, value: i32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put_i64(out: &mut Vec<u8>, value: i64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put_bytes(out: &mut Vec<u8>, bytes: &[u8]) -> io::Result<()> {
    put_u32(out, checked_len(bytes.len())?);
    out.extend_from_slice(bytes);
    Ok(())
}

fn put_string(out: &mut Vec<u8>, value: &str) -> io::Result<()> {
    put_bytes(out, value.as_bytes())
}

fn read_u8(input: &mut Cursor<&[u8]>) -> io::Result<u8> {
    let mut buf = [0; 1];
    input.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn read_bool(input: &mut Cursor<&[u8]>) -> io::Result<bool> {
    match read_u8(input)? {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(invalid_data("invalid boolean value")),
    }
}

fn read_u32(input: &mut Cursor<&[u8]>) -> io::Result<u32> {
    let mut buf = [0; 4];
    input.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

fn read_i32(input: &mut Cursor<&[u8]>) -> io::Result<i32> {
    let mut buf = [0; 4];
    input.read_exact(&mut buf)?;
    Ok(i32::from_le_bytes(buf))
}

fn read_i64(input: &mut Cursor<&[u8]>) -> io::Result<i64> {
    let mut buf = [0; 8];
    input.read_exact(&mut buf)?;
    Ok(i64::from_le_bytes(buf))
}

fn read_len(input: &mut Cursor<&[u8]>) -> io::Result<usize> {
    usize::try_from(read_u32(input)?).map_err(|_| invalid_data("invalid length"))
}

fn read_bytes(input: &mut Cursor<&[u8]>) -> io::Result<Vec<u8>> {
    let len = read_len(input)?;
    let mut bytes = vec![0; len];
    input.read_exact(&mut bytes)?;
    Ok(bytes)
}

fn read_string(input: &mut Cursor<&[u8]>) -> io::Result<String> {
    let bytes = read_bytes(input)?;
    String::from_utf8(bytes).map_err(|_| invalid_data("invalid utf-8 string"))
}

fn put_relocation_target(out: &mut Vec<u8>, target: &RelocationTarget) {
    match target {
        RelocationTarget::Code { code_id } => {
            put_u8(out, 0);
            put_u32(out, *code_id);
        }
        RelocationTarget::DataSlot { slot_id } => {
            put_u8(out, 1);
            put_u32(out, *slot_id);
        }
        RelocationTarget::RuntimeThunk { thunk_id } => {
            put_u8(out, 2);
            put_u32(out, *thunk_id);
        }
        RelocationTarget::RuntimeData { data_id } => {
            put_u8(out, 3);
            put_u32(out, *data_id);
        }
    }
}

fn read_relocation_target(input: &mut Cursor<&[u8]>) -> io::Result<RelocationTarget> {
    match read_u8(input)? {
        0 => Ok(RelocationTarget::Code {
            code_id: read_u32(input)?,
        }),
        1 => Ok(RelocationTarget::DataSlot {
            slot_id: read_u32(input)?,
        }),
        2 => Ok(RelocationTarget::RuntimeThunk {
            thunk_id: read_u32(input)?,
        }),
        3 => Ok(RelocationTarget::RuntimeData {
            data_id: read_u32(input)?,
        }),
        _ => Err(invalid_data("invalid relocation target")),
    }
}

fn invalid_data(message: &'static str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn encode_constants<'gc>(ctx: Context<'gc>, values: &[Value<'gc>]) -> Vec<u8> {
        let constants = Value::new(Vector::from_slice(*ctx, values));
        let mut bytes = Vec::new();
        crate::runtime::fasl::FASLWriter::new(ctx, &mut bytes)
            .write(constants)
            .expect("write constants fasl");
        bytes
    }

    #[test]
    fn code_image_roundtrips_all_task1_fields() {
        let image = CompiledCodeImage {
            version: CODE_IMAGE_VERSION,
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            constants_fasl: vec![1, 2, 3, 4],
            code: vec![CodeEntry {
                id: 7,
                name: "entry".to_string(),
                bytes: vec![0x90, 0xc3],
                entry_offset: 0,
                arity: -2,
                is_cont: false,
                metadata_constant: Some(0),
            }],
            data_slots: vec![
                DataSlot {
                    id: 3,
                    kind: DataSlotKind::SideMetadata {
                        kind: SideMetadataSlotKind::Global,
                    },
                },
                DataSlot {
                    id: 4,
                    kind: DataSlotKind::Pointer {
                        target: RelocationTarget::Code { code_id: 7 },
                    },
                },
            ],
            relocations: vec![Relocation {
                code_id: 7,
                offset: 1,
                kind: RelocationKind::X86CallPcRel4,
                target: RelocationTarget::RuntimeThunk { thunk_id: 11 },
                addend: -4,
            }],
            entry_code_id: 7,
            entry_is_cont: false,
        };

        let encoded = image.encode().expect("encode code image");
        let decoded = CompiledCodeImage::decode(&encoded).expect("decode code image");

        assert_eq!(decoded, image);
    }

    #[test]
    fn code_image_rejects_bad_magic() {
        let err = CompiledCodeImage::decode(b"not a code image").unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn code_image_rejects_trailing_bytes() {
        let image = CompiledCodeImage {
            version: CODE_IMAGE_VERSION,
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            constants_fasl: Vec::new(),
            code: Vec::new(),
            data_slots: Vec::new(),
            relocations: Vec::new(),
            entry_code_id: 0,
            entry_is_cont: false,
        };
        let mut encoded = image.encode().expect("encode code image");
        encoded.push(0);

        let err = CompiledCodeImage::decode(&encoded).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_hand_built_code_image_creates_loaded_code_block() {
        use crate::runtime::{
            Scheme,
            value::{CodeBlockKind, Value},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xb8, 42, 0, 0, 0, 0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: Vec::new(),
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let entry = loaded.entry_code_block();

            assert_eq!(loaded.code_span_count(), 1);
            assert!(matches!(entry.kind, CodeBlockKind::Loaded));
            assert_eq!(entry.arity.fixed_arity(), 0);
            assert_eq!(entry.metadata.get(), Value::null());
            assert_ne!(entry.loaded_code_handle, 0);

            let f: extern "C" fn() -> i32 =
                unsafe { std::mem::transmute(entry.entrypoint.as_usize()) };
            assert_eq!(f(), 42);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_applies_abs8_runtime_data_relocation() {
        use crate::runtime::{
            Scheme,
            symbols::RuntimeData,
            value::{CodeBlockKind, Value},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3, 0, 0, 0, 0, 0, 0, 0, 0],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: Vec::new(),
                relocations: vec![Relocation {
                    code_id: 1,
                    offset: 1,
                    kind: RelocationKind::Abs8,
                    target: RelocationTarget::RuntimeData {
                        data_id: RuntimeData::PairInfo.id(),
                    },
                    addend: 0,
                }],
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let entry = loaded.entry_code_block();

            assert!(matches!(entry.kind, CodeBlockKind::Loaded));
            assert_eq!(entry.metadata.get(), Value::null());
            let patched =
                unsafe { ((entry.entrypoint.as_usize() + 1) as *const usize).read_unaligned() };
            assert_eq!(patched, RuntimeData::PairInfo.address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn runtime_relocation_patcher_uses_numeric_runtime_data_resolver() {
        use crate::runtime::{code_memory::CodeMemory, symbols::RuntimeData};

        let mut memory = CodeMemory::new();
        let loaded = memory
            .allocate_copy(&[0; 8])
            .expect("allocate relocation target");
        let site = RuntimeRelocationSite::from_loaded_code(loaded, HashMap::new());
        let reloc = Relocation {
            code_id: 0,
            offset: 0,
            kind: RelocationKind::Abs8,
            target: RelocationTarget::RuntimeData {
                data_id: RuntimeData::PairInfo.id(),
            },
            addend: 0,
        };

        apply_runtime_relocation(&mut memory, &site, &reloc).expect("patch runtime relocation");

        let patched = unsafe { (loaded.entrypoint.as_usize() as *const usize).read_unaligned() };
        assert_eq!(patched, RuntimeData::PairInfo.address().as_usize());
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_initializes_code_block_data_slot() {
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Value},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: vec![DataSlot {
                    id: 7,
                    kind: DataSlotKind::CodeBlock { code_id: 1 },
                }],
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let slot = loaded.data_slot_value(7).expect("code block slot");

            assert!(slot.is::<CodeBlock>());
            let slot_code_block = slot.downcast::<CodeBlock>();
            let entry_code_block = loaded.entry_code_block();
            assert_eq!(slot_code_block.entrypoint, entry_code_block.entrypoint);
            assert_eq!(
                slot_code_block.loaded_code_handle,
                entry_code_block.loaded_code_handle
            );
            assert_ne!(slot, Value::new(false));
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_initializes_pointer_slot_to_code_entrypoint() {
        use crate::runtime::Scheme;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: vec![DataSlot {
                    id: 8,
                    kind: DataSlotKind::Pointer {
                        target: RelocationTarget::Code { code_id: 1 },
                    },
                }],
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let pointer = loaded
                .data_slots
                .iter()
                .find(|slot| slot.id == 8)
                .and_then(LoadedDataSlot::raw_word_value)
                .expect("pointer slot should contain a raw word");

            assert_eq!(pointer, loaded.entry_code_block().entrypoint.as_usize());
        });
    }

    #[test]
    fn load_code_image_initializes_side_metadata_data_slots_as_raw_words() {
        use crate::runtime::Scheme;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: vec![
                    DataSlot {
                        id: 7,
                        kind: DataSlotKind::SideMetadata {
                            kind: SideMetadataSlotKind::Global,
                        },
                    },
                    DataSlot {
                        id: 8,
                        kind: DataSlotKind::SideMetadata {
                            kind: SideMetadataSlotKind::VoBit,
                        },
                    },
                ],
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let global = loaded
                .data_slots
                .iter()
                .find(|slot| slot.id == 7)
                .expect("global side metadata slot");
            let vo_bit = loaded
                .data_slots
                .iter()
                .find(|slot| slot.id == 8)
                .expect("vo-bit side metadata slot");

            assert_eq!(loaded.data_slot_value(7), None);
            assert_eq!(loaded.data_slot_value(8), None);
            assert_eq!(
                global.raw_word_value(),
                Some(global_side_metadata_vm_base_address().as_usize())
            );
            assert_eq!(
                vo_bit.raw_word_value(),
                Some(vo_bit_side_metadata_addr().as_usize())
            );
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_decodes_constants_for_slots_and_metadata() {
        use crate::runtime::{Scheme, value::Value};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let constant = Value::new(12345i32);
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: encode_constants(ctx, &[constant]),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: Some(0),
                }],
                data_slots: vec![DataSlot {
                    id: 7,
                    kind: DataSlotKind::Constant { constant_index: 0 },
                }],
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let entry = loaded.entry_code_block();

            assert_eq!(loaded.data_slot_value(7), Some(constant));
            assert_eq!(entry.metadata.get(), constant);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_applies_abs8_data_slot_relocation() {
        use crate::runtime::{Scheme, value::Value};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let constant = Value::new(9988i32);
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: encode_constants(ctx, &[constant]),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3, 0, 0, 0, 0, 0, 0, 0, 0],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: vec![DataSlot {
                    id: 7,
                    kind: DataSlotKind::Constant { constant_index: 0 },
                }],
                relocations: vec![Relocation {
                    code_id: 1,
                    offset: 1,
                    kind: RelocationKind::Abs8,
                    target: RelocationTarget::DataSlot { slot_id: 7 },
                    addend: 0,
                }],
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let entry = loaded.entry_code_block();
            let patched =
                unsafe { ((entry.entrypoint.as_usize() + 1) as *const usize).read_unaligned() };
            let relocated_value = unsafe { (patched as *const Value).read() };

            assert_eq!(loaded.data_slot_value(7), Some(constant));
            assert_eq!(relocated_value, constant);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn call_relocations_reserve_near_stubs() {
        let entry = CodeEntry {
            id: 7,
            name: "entry".to_string(),
            bytes: vec![0xe8, 0, 0, 0, 0, 0xc3],
            entry_offset: 0,
            arity: 0,
            is_cont: false,
            metadata_constant: None,
        };
        let (bytes, stubs) = code_bytes_with_call_stubs(
            &entry,
            &[Relocation {
                code_id: 7,
                offset: 1,
                kind: RelocationKind::X86CallPcRel4,
                target: RelocationTarget::RuntimeThunk { thunk_id: 0 },
                addend: -4,
            }],
        )
        .expect("reserve call stub");

        assert_eq!(stubs.get(&1), Some(&6));
        assert!(bytes.len() > entry.bytes.len());
    }

    #[test]
    fn rel32_displacement_rejects_out_of_range_targets() {
        assert_eq!(rel32_displacement(100, 96), Some(-4));
        assert_eq!(rel32_displacement(0, (i32::MAX as usize) + 1), None);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn x86_64_absolute_jump_stub_uses_numeric_address() {
        let stub = absolute_jump_stub(0x0102_0304_0506_0708).expect("build stub");

        assert_eq!(&stub[..2], &[0x49, 0xbb]);
        assert_eq!(&stub[2..10], &0x0102_0304_0506_0708usize.to_le_bytes());
        assert_eq!(&stub[10..], &[0x41, 0xff, 0xe3]);
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn load_code_image_returns_entry_closure() {
        use crate::runtime::{Scheme, value::Closure};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: Vec::new(),
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };

            let loaded = LoadedCodeImage::load(ctx, image).expect("load code image");
            let entry = loaded.entry_closure();

            assert!(entry.is::<Closure>());
            let closure = entry.downcast::<Closure>();
            assert_eq!(closure.code, loaded.entry_code_block().entrypoint);
            assert_eq!(closure.nfree, 0);
        });
    }
}

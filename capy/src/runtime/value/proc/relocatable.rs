//! Relocatable compiled-code payloads.

use super::*;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CodeRelocation {
    pub offset: u32,
    pub kind_tag: u8,
    pub target_tag: u8,
    pub target_payload: u32,
    pub addend: i64,
    pub aux_tag: u8,
}

#[repr(C)]
pub struct RelocatableCodeBlock<'gc> {
    pub entry_offset: u32,
    code_len: u32,
    relocation_count: u32,
    loaded_data_value_bitmap_word_count: u32,
    _marker: std::marker::PhantomData<&'gc ()>,
    payload: [CodeRelocation; 0],
}

#[derive(Clone, Copy)]
struct RelocatableCodeBlockLayout {
    relocation_offset: usize,
    bitmap_offset: usize,
    payload_size: usize,
}

fn align_payload_offset(offset: usize, alignment: usize) -> Option<usize> {
    debug_assert!(alignment.is_power_of_two());
    offset
        .checked_add(alignment - 1)
        .map(|offset| offset & !(alignment - 1))
}

fn relocatable_code_block_layout(
    code_len: usize,
    relocation_count: usize,
    loaded_data_value_bitmap_word_count: usize,
) -> Option<RelocatableCodeBlockLayout> {
    let relocation_offset = align_payload_offset(code_len, align_of::<CodeRelocation>())?;
    let relocation_size = relocation_count.checked_mul(size_of::<CodeRelocation>())?;
    let bitmap_offset = align_payload_offset(
        relocation_offset.checked_add(relocation_size)?,
        align_of::<usize>(),
    )?;
    let bitmap_size = loaded_data_value_bitmap_word_count.checked_mul(size_of::<usize>())?;
    let payload_size = bitmap_offset.checked_add(bitmap_size)?;
    Some(RelocatableCodeBlockLayout {
        relocation_offset,
        bitmap_offset,
        payload_size,
    })
}

extern "C" fn compute_relocatable_code_block_size(obj: GCObject) -> usize {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let obj = obj.to_address().as_ref::<RelocatableCodeBlock<'static>>();
        relocatable_code_block_layout(
            obj.code_len as usize,
            obj.relocation_count as usize,
            obj.loaded_data_value_bitmap_word_count as usize,
        )
        .expect("unlinked code block layout is too large")
        .payload_size
    }
}

extern "C" fn trace_relocatable_code_block(obj: GCObject, vis: &mut Visitor) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let obj = obj
            .to_address()
            .as_mut_ref::<RelocatableCodeBlock<'static>>();
        obj.trace(vis);
    }
}

extern "C" fn process_weak_relocatable_code_block(
    obj: GCObject,
    weak_processor: &mut WeakProcessor,
) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let obj = obj
            .to_address()
            .as_mut_ref::<RelocatableCodeBlock<'static>>();
        obj.process_weak_refs(weak_processor);
    }
}

fn relocatable_code_block_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::RELOCATABLE_CODE_BLOCK).unwrap())
}

impl<'gc> RelocatableCodeBlock<'gc> {
    pub const HOOKS: AllocationHooks = AllocationHooks {
        alignment: align_of::<RelocatableCodeBlock>(),
        compute_alignment: None,
        instance_size: size_of::<RelocatableCodeBlock>(),
        compute_size: Some(compute_relocatable_code_block_size),
        trace: trace_relocatable_code_block,
        weak_proc: process_weak_relocatable_code_block,
        type_name: "unlinked-code-block",
    };

    pub fn new(
        ctx: Context<'gc>,
        code: &[u8],
        entry_offset: u32,
        relocations: &[CodeRelocation],
        loaded_data_value_bitmap: &[usize],
    ) -> Gc<'gc, Self> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let code_len =
                u32::try_from(code.len()).expect("unlinked code block code is too large");
            let relocation_count = u32::try_from(relocations.len())
                .expect("unlinked code block relocation table is too large");
            let bitmap_word_count = u32::try_from(loaded_data_value_bitmap.len())
                .expect("unlinked code block value bitmap is too large");
            let layout = relocatable_code_block_layout(
                code.len(),
                relocations.len(),
                loaded_data_value_bitmap.len(),
            )
            .expect("unlinked code block is too large");
            let size = size_of::<Self>()
                .checked_add(layout.payload_size)
                .expect("unlinked code block is too large");

            let alloc = ctx.raw_allocate_with_header_word(
                size,
                align_of::<Self>(),
                relocatable_code_block_header_word(),
                AllocationSemantics::Default,
            );
            let this = alloc.to_address().as_mut_ref::<Self>();
            this.entry_offset = entry_offset;
            this.code_len = code_len;
            this.relocation_count = relocation_count;
            this.loaded_data_value_bitmap_word_count = bitmap_word_count;
            this._marker = std::marker::PhantomData;

            let payload = this.payload.as_mut_ptr().cast::<u8>();
            if !code.is_empty() {
                std::ptr::copy_nonoverlapping(code.as_ptr(), payload, code.len());
            }
            if !relocations.is_empty() {
                std::ptr::copy_nonoverlapping(
                    relocations.as_ptr(),
                    payload
                        .add(layout.relocation_offset)
                        .cast::<CodeRelocation>(),
                    relocations.len(),
                );
            }
            if !loaded_data_value_bitmap.is_empty() {
                std::ptr::copy_nonoverlapping(
                    loaded_data_value_bitmap.as_ptr(),
                    payload.add(layout.bitmap_offset).cast::<usize>(),
                    loaded_data_value_bitmap.len(),
                );
            }

            Gc::from_gcobj(alloc)
        }
    }

    pub fn empty(ctx: Context<'gc>) -> Gc<'gc, Self> {
        Self::new(ctx, &[], 0, &[], &[])
    }

    pub fn code(&self) -> &[u8] {
        // SAFETY: Pointer is valid for the given element count
        unsafe { std::slice::from_raw_parts(self.payload.as_ptr().cast(), self.code_len as usize) }
    }

    pub fn relocations(&self) -> &[CodeRelocation] {
        let layout = relocatable_code_block_layout(
            self.code_len as usize,
            self.relocation_count as usize,
            self.loaded_data_value_bitmap_word_count as usize,
        )
        .expect("unlinked code block layout is too large");
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts(
                self.payload
                    .as_ptr()
                    .cast::<u8>()
                    .add(layout.relocation_offset)
                    .cast(),
                self.relocation_count as usize,
            )
        }
    }

    pub fn loaded_data_value_bitmap(&self) -> &[usize] {
        let layout = relocatable_code_block_layout(
            self.code_len as usize,
            self.relocation_count as usize,
            self.loaded_data_value_bitmap_word_count as usize,
        )
        .expect("unlinked code block layout is too large");
        // SAFETY: Pointer is valid for the given element count
        unsafe {
            std::slice::from_raw_parts(
                self.payload
                    .as_ptr()
                    .cast::<u8>()
                    .add(layout.bitmap_offset)
                    .cast(),
                self.loaded_data_value_bitmap_word_count as usize,
            )
        }
    }
}

// SAFETY: `gc` for `RelocatableCodeBlock` upholds all trait invariants
unsafe impl<'gc> ClassTagged for RelocatableCodeBlock<'gc> {
    const CLASS_IDS: &'static [u32] =
        &[crate::rsgc::object::builtin_class_ids::RELOCATABLE_CODE_BLOCK];

    const TYPE_NAME: &'static str = "unlinked-code-block";
}

// SAFETY: `gc` for `RelocatableCodeBlock` upholds all trait invariants
unsafe impl<'gc> Trace for RelocatableCodeBlock<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, _visitor: &mut Visitor) {}

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

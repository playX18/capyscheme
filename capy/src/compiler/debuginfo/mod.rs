use std::collections::HashMap;

use cranelift_codegen::MachSrcLoc;
use cranelift_codegen::binemit::CodeOffset;
use cranelift_codegen::ir::{Endianness, SourceLoc};
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::FuncId;
use gimli::write::{
    Address, AttributeValue, DwarfUnit, EndianVec, Expression, FileId, LineProgram, LineString,
    Range, RangeList, Sections, UnitEntryId, Writer,
};
use gimli::{AArch64, Encoding, Format, LineEncoding, Register, RiscV, RunTimeEndian, X86_64};

use crate::cps::ReifyInfo;
use crate::cps::term::{ContRef, FuncRef};
use crate::runtime::value::{Value, Vector};

pub(crate) struct DebugContext<'gc> {
    endian: RunTimeEndian,

    dwarf: DwarfUnit,
    unit_range_list: RangeList,
    created_files: HashMap<Value<'gc>, FileId>,
    stack_pointer_register: Register,
}

pub(crate) struct FunctionDebugContext {
    entry_id: UnitEntryId,

    srcloc: (FileId, u64, u64),
    source_loc_set: HashMap<SourceLoc, (FileId, u64, u64)>,
}

impl<'gc> DebugContext<'gc> {
    pub(crate) fn new(reify_info: &ReifyInfo<'gc>, isa: &dyn TargetIsa) -> Self {
        let encoding = Encoding {
            format: Format::Dwarf32,
            version: 4,
            address_size: isa.frontend_config().pointer_bytes() as u8,
        };

        let endian = match isa.endianness() {
            Endianness::Big => RunTimeEndian::Big,
            Endianness::Little => RunTimeEndian::Little,
        };

        let stack_pointer_register = match isa.triple().architecture {
            target_lexicon::Architecture::X86_64 | target_lexicon::Architecture::X86_64h => {
                X86_64::RSP
            }
            target_lexicon::Architecture::Aarch64(_) => AArch64::SP,
            target_lexicon::Architecture::Riscv64(_) => RiscV::SP,
            _ => Register(u16::MAX),
        };

        let mut dwarf = DwarfUnit::new(encoding);

        let main_srcloc = reify_info.entrypoint.source();
        let file_name = if main_srcloc == Value::new(false) {
            "<unknown>".to_owned()
        } else {
            main_srcloc.car().to_string()
        };

        let line_program = LineProgram::new(
            encoding,
            LineEncoding::default(),
            LineString::new("", encoding, &mut dwarf.line_strings),
            None,
            LineString::new(file_name, encoding, &mut dwarf.line_strings),
            None,
        );

        dwarf.unit.line_program = line_program;

        {
            let name = if reify_info.entrypoint.name == Value::new(false) {
                "<entrypoint>".to_owned()
            } else {
                reify_info.entrypoint.name.to_string()
            };
            let name = dwarf.strings.add(name);

            let root = dwarf.unit.root();
            let root = dwarf.unit.get_mut(root);

            root.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

            root.set(gimli::DW_AT_stmt_list, AttributeValue::Udata(0));
            root.set(
                gimli::DW_AT_low_pc,
                AttributeValue::Address(Address::Constant(0)),
            );
        }

        Self {
            endian,
            dwarf,
            unit_range_list: RangeList(Vec::new()),
            created_files: HashMap::new(),
            stack_pointer_register,
        }
    }

    pub(crate) fn define_function(
        &mut self,
        func: FuncRef<'gc>,
        linkage_name: &str,
    ) -> FunctionDebugContext {
        let (file_id, line, column) = self.get_span_loc(func.source());

        let scope = self.dwarf.unit.root();
        let entry_id = self.dwarf.unit.add(scope, gimli::DW_TAG_subprogram);
        let entry = self.dwarf.unit.get_mut(entry_id);
        let linkage_name_id = if linkage_name != func.name.to_string() {
            Some(self.dwarf.strings.add(linkage_name))
        } else {
            None
        };

        let name_id = if func.name != Value::new(false) {
            self.dwarf.strings.add(func.name.to_string())
        } else {
            self.dwarf.strings.add(func.binding.name.to_string())
        };

        entry.set(gimli::DW_AT_low_pc, AttributeValue::Udata(0));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(0));

        let mut frame_base_expr = Expression::new();

        frame_base_expr.op_reg(self.stack_pointer_register);
        entry.set(
            gimli::DW_AT_frame_base,
            AttributeValue::Exprloc(frame_base_expr),
        );

        if let Some(linkage_name_id) = linkage_name_id {
            entry.set(
                gimli::DW_AT_linkage_name,
                AttributeValue::StringRef(linkage_name_id),
            );
        }
        entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name_id));

        entry.set(
            gimli::DW_AT_decl_file,
            AttributeValue::FileIndex(Some(file_id)),
        );
        entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(line));

        FunctionDebugContext {
            entry_id,

            srcloc: (file_id, line, column),
            source_loc_set: HashMap::new(),
        }
    }

    pub(crate) fn define_cont(
        &mut self,
        func: ContRef<'gc>,
        linkage_name: &str,
    ) -> FunctionDebugContext {
        let (file_id, line, column) = self.get_span_loc(func.source());

        let scope = self.dwarf.unit.root();
        let entry_id = self.dwarf.unit.add(scope, gimli::DW_TAG_subprogram);
        let entry = self.dwarf.unit.get_mut(entry_id);
        let linkage_name_id = if linkage_name != func.name.to_string() {
            Some(self.dwarf.strings.add(linkage_name))
        } else {
            None
        };

        let name_id = if func.name != Value::new(false) {
            self.dwarf.strings.add(func.name.to_string())
        } else {
            self.dwarf.strings.add(func.binding.name.to_string())
        };

        entry.set(gimli::DW_AT_low_pc, AttributeValue::Udata(0));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(0));

        let mut frame_base_expr = Expression::new();

        frame_base_expr.op_reg(self.stack_pointer_register);
        entry.set(
            gimli::DW_AT_frame_base,
            AttributeValue::Exprloc(frame_base_expr),
        );

        if let Some(linkage_name_id) = linkage_name_id {
            entry.set(
                gimli::DW_AT_linkage_name,
                AttributeValue::StringRef(linkage_name_id),
            );
        }
        entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name_id));

        entry.set(
            gimli::DW_AT_decl_file,
            AttributeValue::FileIndex(Some(file_id)),
        );
        entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(line));

        FunctionDebugContext {
            entry_id,

            srcloc: (file_id, line, column),
            source_loc_set: HashMap::new(),
        }
    }

    pub(crate) fn get_span_loc(&mut self, span: Value<'gc>) -> (FileId, u64, u64) {
        if span == Value::new(false) {
            return (self.add_file(span), 0, 0);
        }

        let filename = span.downcast::<Vector>()[0].get();
        let line = span.downcast::<Vector>()[1].get().as_int32() as u64;
        let column = span.downcast::<Vector>()[2].get().as_int32() as u64;

        (self.add_file(filename), line + 1, column + 1)
    }

    pub(crate) fn add_file(&mut self, filename: Value<'gc>) -> FileId {
        *self.created_files.entry(filename).or_insert_with(|| {
            let line_program = &mut self.dwarf.unit.line_program;
            let line_strings = &mut self.dwarf.line_strings;

            if filename == Value::new(false) {
                let dummy_directory =
                    LineString::new("<unknown>", line_program.encoding(), line_strings);
                let dummy_directory = line_program.add_directory(dummy_directory);
                line_program.add_file(
                    LineString::new("<unknown>", line_program.encoding(), line_strings),
                    dummy_directory,
                    None,
                )
            } else {
                let path = filename.to_string();
                let path = std::path::Path::new(&path);
                let dir = if let Some(dir) = path.parent() {
                    let dir_name = LineString::new(
                        dir.to_string_lossy().as_bytes().to_vec(),
                        line_program.encoding(),
                        line_strings,
                    );
                    line_program.add_directory(dir_name)
                } else {
                    line_program.default_directory()
                };

                let file_name = path
                    .file_name()
                    .map(|s| s.to_string_lossy())
                    .unwrap_or("<unknown>".into());
                let file_name = LineString::new(
                    file_name.as_bytes().to_vec(),
                    line_program.encoding(),
                    line_strings,
                );
                line_program.add_file(file_name, dir, None)
            }
        })
    }

    pub(crate) fn emit(&mut self, product: &mut ObjectProduct) {
        let unit_range_list_id = self.dwarf.unit.ranges.add(self.unit_range_list.clone());
        let root = self.dwarf.unit.root();
        let root = self.dwarf.unit.get_mut(root);
        root.set(
            gimli::DW_AT_ranges,
            AttributeValue::RangeListRef(unit_range_list_id),
        );

        let mut sections = Sections::new(WriterRelocate::new(self.endian));
        self.dwarf.write(&mut sections).unwrap();

        let mut section_map = HashMap::default();
        let _: gimli::write::Result<()> = sections.for_each_mut(|id, section| {
            if !section.writer.slice().is_empty() {
                let section_id = product.add_debug_section(id, section.writer.take());
                section_map.insert(id, section_id);
            }
            Ok(())
        });

        let _: gimli::write::Result<()> = sections.for_each(|id, section| {
            if let Some(section_id) = section_map.get(&id) {
                for reloc in &section.relocs {
                    product.add_debug_reloc(&section_map, section_id, reloc);
                }
            }
            Ok(())
        });
    }
}

impl FunctionDebugContext {
    pub(crate) fn create_debug_lines<'gc>(
        &mut self,
        debug_context: &mut DebugContext<'gc>,
        func_id: FuncId,
        context: &cranelift_codegen::Context,
    ) -> CodeOffset {
        let create_row_for_span =
            |debug_context: &mut DebugContext, source_loc: (FileId, u64, u64)| {
                let (file_id, line, col) = source_loc;

                debug_context.dwarf.unit.line_program.row().file = file_id;
                debug_context.dwarf.unit.line_program.row().line = line;
                debug_context.dwarf.unit.line_program.row().column = col;
                debug_context.dwarf.unit.line_program.generate_row();
            };

        debug_context
            .dwarf
            .unit
            .line_program
            .begin_sequence(Some(address_for_func(func_id)));

        let mut func_end = 0;

        let mcr = context.compiled_code().unwrap();

        for &MachSrcLoc { start, end, loc } in mcr.buffer.get_srclocs_sorted() {
            debug_context.dwarf.unit.line_program.row().address_offset = u64::from(start);
            if !loc.is_default() {
                let source_loc = self.source_loc_set[&loc];
                create_row_for_span(debug_context, source_loc);
            } else {
                create_row_for_span(debug_context, self.srcloc);
            }
            func_end = end;
        }

        debug_context
            .dwarf
            .unit
            .line_program
            .end_sequence(u64::from(func_end));

        let func_end = mcr.buffer.total_size();

        let entry = debug_context.dwarf.unit.get_mut(self.entry_id);
        entry.set(
            gimli::DW_AT_low_pc,
            AttributeValue::Address(address_for_func(func_id)),
        );
        entry.set(
            gimli::DW_AT_high_pc,
            AttributeValue::Udata(u64::from(func_end)),
        );

        func_end
    }

    pub(crate) fn add_dbg_loc(&mut self, file_id: FileId, line: u64, column: u64) -> SourceLoc {
        let src = SourceLoc::new(self.source_loc_set.len() as u32);
        self.source_loc_set.insert(src, (file_id, line, column));
        src
    }

    pub(crate) fn finalize<'gc>(
        mut self,
        debug_context: &mut DebugContext<'gc>,
        func_id: FuncId,
        context: &cranelift_codegen::Context,
    ) {
        let end = self.create_debug_lines(debug_context, func_id, context);

        debug_context.unit_range_list.0.push(Range::StartLength {
            begin: address_for_func(func_id),
            length: u64::from(end),
        });
        let func_entry = debug_context.dwarf.unit.get_mut(self.entry_id);
        func_entry.set(
            gimli::DW_AT_low_pc,
            AttributeValue::Address(address_for_func(func_id)),
        );
        func_entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(u64::from(end)));
    }
}

fn address_for_func(func_id: FuncId) -> Address {
    let symbol = func_id.as_u32();

    Address::Symbol {
        symbol: symbol as usize,
        addend: 0,
    }
}

use cranelift_object::ObjectProduct;
use gimli::SectionId;
use object::write::{Relocation, StandardSegment};
use object::{RelocationEncoding, RelocationFlags, SectionKind};

pub(super) trait WriteDebugInfo {
    type SectionId: Copy;

    fn add_debug_section(&mut self, name: SectionId, data: Vec<u8>) -> Self::SectionId;
    fn add_debug_reloc(
        &mut self,
        section_map: &HashMap<SectionId, Self::SectionId>,
        from: &Self::SectionId,
        reloc: &DebugReloc,
    );
}

#[derive(Clone)]
pub(crate) struct DebugReloc {
    pub(crate) offset: u32,
    pub(crate) size: u8,
    pub(crate) name: DebugRelocName,
    pub(crate) addend: i64,
    pub(crate) kind: object::RelocationKind,
}

#[derive(Clone)]
pub(crate) enum DebugRelocName {
    Section(SectionId),
    Symbol(usize),
}

/// A [`Writer`] that collects all necessary relocations.
#[derive(Clone)]
pub(super) struct WriterRelocate {
    pub(super) relocs: Vec<DebugReloc>,
    pub(super) writer: EndianVec<RunTimeEndian>,
}

impl WriterRelocate {
    pub(super) fn new(endian: RunTimeEndian) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: EndianVec::new(endian),
        }
    }
}

impl Writer for WriterRelocate {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                let offset = self.len() as u64;
                self.relocs.push(DebugReloc {
                    offset: offset as u32,
                    size,
                    name: DebugRelocName::Symbol(symbol),
                    addend,
                    kind: object::RelocationKind::Absolute,
                });
                self.write_udata(0, size)
            }
        }
    }

    fn write_offset(
        &mut self,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        let offset = self.len() as u32;
        self.relocs.push(DebugReloc {
            offset,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata(0, size)
    }

    fn write_offset_at(
        &mut self,
        offset: usize,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> gimli::write::Result<()> {
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });
        self.write_udata_at(offset, 0, size)
    }

    fn write_eh_pointer(
        &mut self,
        address: Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> gimli::write::Result<()> {
        match address {
            // Address::Constant arm copied from gimli
            Address::Constant(val) => {
                // Indirect doesn't matter here.
                let val = match eh_pe.application() {
                    gimli::DW_EH_PE_absptr => val,
                    gimli::DW_EH_PE_pcrel => {
                        // FIXME better handling of sign
                        let offset = self.len() as u64;
                        offset.wrapping_sub(val)
                    }
                    _ => {
                        return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
                    }
                };
                self.write_eh_pointer_data(val, eh_pe.format(), size)
            }
            Address::Symbol { symbol, addend } => match eh_pe.application() {
                gimli::DW_EH_PE_pcrel => {
                    let size = match eh_pe.format() {
                        gimli::DW_EH_PE_sdata4 => 4,
                        gimli::DW_EH_PE_sdata8 => 8,
                        _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                    };
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });
                    self.write_udata(0, size)
                }
                gimli::DW_EH_PE_absptr => {
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size: size.into(),
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Absolute,
                    });
                    self.write_udata(0, size.into())
                }
                _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
            },
        }
    }
}

impl WriteDebugInfo for ObjectProduct {
    type SectionId = (object::write::SectionId, object::write::SymbolId);

    fn add_debug_section(
        &mut self,
        id: SectionId,
        data: Vec<u8>,
    ) -> (object::write::SectionId, object::write::SymbolId) {
        let name = if self.object.format() == object::BinaryFormat::MachO {
            id.name().replace('.', "__") // machO expects __debug_info instead of .debug_info
        } else {
            id.name().to_string()
        }
        .into_bytes();

        let segment = self.object.segment_name(StandardSegment::Debug).to_vec();
        // FIXME use SHT_X86_64_UNWIND for .eh_frame
        let section_id = self.object.add_section(
            segment,
            name,
            if id == SectionId::DebugStr || id == SectionId::DebugLineStr {
                SectionKind::DebugString
            } else if id == SectionId::EhFrame {
                SectionKind::ReadOnlyData
            } else {
                SectionKind::Debug
            },
        );
        self.object
            .section_mut(section_id)
            .set_data(data, if id == SectionId::EhFrame { 8 } else { 1 });
        let symbol_id = self.object.section_symbol(section_id);
        (section_id, symbol_id)
    }

    fn add_debug_reloc(
        &mut self,
        section_map: &HashMap<SectionId, Self::SectionId>,
        from: &Self::SectionId,
        reloc: &DebugReloc,
    ) {
        let (symbol, symbol_offset) = match reloc.name {
            DebugRelocName::Section(id) => (section_map.get(&id).unwrap().1, 0),
            DebugRelocName::Symbol(id) => {
                let id = id.try_into().unwrap();
                let symbol_id = if id & 1 << 31 == 0 {
                    self.function_symbol(FuncId::from_u32(id))
                } else {
                    todo!()
                };
                self.object
                    .symbol_section_and_offset(symbol_id)
                    .unwrap_or((symbol_id, 0))
            }
        };
        self.object
            .add_relocation(
                from.0,
                Relocation {
                    offset: u64::from(reloc.offset),
                    symbol,
                    flags: RelocationFlags::Generic {
                        kind: reloc.kind,
                        encoding: RelocationEncoding::Generic,
                        size: reloc.size * 8,
                    },
                    addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
                },
            )
            .unwrap();
    }
}

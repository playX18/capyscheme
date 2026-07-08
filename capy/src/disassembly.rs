use std::fmt::Write;

use capstone::{Capstone, arch::BuildsCapstone};

use crate::runtime::fasl::{CodeSourceLocation, CodeSourceMapEntry};

pub(crate) fn disassemble_host(bytes: &[u8], base_address: u64) -> Result<String, String> {
    disassemble_host_with_source(bytes, base_address, None)
}

pub(crate) fn disassemble_host_with_source(
    bytes: &[u8],
    base_address: u64,
    source: Option<&SourceAnnotation>,
) -> Result<String, String> {
    let source = source.cloned().map(DisassemblySource::from_function_source);
    disassemble_host_with_annotations(bytes, base_address, source.as_ref())
}

pub(crate) fn disassemble_host_with_annotations(
    bytes: &[u8],
    base_address: u64,
    source: Option<&DisassemblySource>,
) -> Result<String, String> {
    let capstone = host_capstone()?;
    let instructions = capstone
        .disasm_all(bytes, base_address)
        .map_err(|err| format!("failed to disassemble code: {err}"))?;

    let rendered = instructions
        .iter()
        .map(|instruction| {
            (
                render_bytes(instruction.bytes()),
                instruction.mnemonic().unwrap_or("").to_string(),
                instruction.op_str().unwrap_or("").to_string(),
            )
        })
        .collect::<Vec<_>>();
    let bytes_width = rendered
        .iter()
        .map(|(bytes, _, _)| bytes.len())
        .max()
        .unwrap_or(0);
    let mnemonic_width = rendered
        .iter()
        .map(|(_, mnemonic, _)| mnemonic.len())
        .max()
        .unwrap_or(0);

    let mut out = String::new();
    let mut last_source = None;
    if let Some(function_source) = source.and_then(|source| source.function.as_ref()) {
        write_source_annotation(&mut out, function_source);
        last_source = Some(function_source.clone());
    }

    for (instruction, (bytes, mnemonic, operands)) in instructions.iter().zip(rendered.iter()) {
        let offset = instruction.address().saturating_sub(base_address);
        let instruction_source = source.and_then(|source| source.instruction_source(offset as u32));
        if let Some(instruction_source) = instruction_source {
            if last_source.as_ref() != Some(instruction_source) {
                write_source_annotation(&mut out, instruction_source);
                last_source = Some(instruction_source.clone());
            }
        }
        if operands.is_empty() {
            writeln!(out, "{offset:08x}  {bytes:<bytes_width$}  {mnemonic}")
                .expect("writing to String cannot fail");
        } else {
            writeln!(
                out,
                "{offset:08x}  {bytes:<bytes_width$}  {mnemonic:<mnemonic_width$} {operands}"
            )
            .expect("writing to String cannot fail");
        }
    }

    Ok(out)
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct DisassemblySource {
    function: Option<SourceAnnotation>,
    instructions: Vec<SourceRangeAnnotation>,
}

impl DisassemblySource {
    pub(crate) fn new(
        function: Option<SourceAnnotation>,
        instructions: Vec<SourceRangeAnnotation>,
    ) -> Self {
        Self {
            function,
            instructions,
        }
    }

    pub(crate) fn from_function_source(function: SourceAnnotation) -> Self {
        Self::new(Some(function), Vec::new())
    }

    pub(crate) fn from_code_source_map(
        function: Option<SourceAnnotation>,
        source_map: &[CodeSourceMapEntry],
    ) -> Self {
        Self::new(
            function,
            source_map
                .iter()
                .map(SourceRangeAnnotation::from_code_source_map_entry)
                .collect(),
        )
    }

    pub(crate) fn instructions(&self) -> &[SourceRangeAnnotation] {
        &self.instructions
    }

    fn instruction_source(&self, offset: u32) -> Option<&SourceAnnotation> {
        self.instructions
            .iter()
            .find(|range| range.contains(offset))
            .map(|range| &range.source)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SourceRangeAnnotation {
    start: u32,
    end: u32,
    source: SourceAnnotation,
}

impl SourceRangeAnnotation {
    pub(crate) fn new(start: u32, end: u32, source: SourceAnnotation) -> Self {
        Self { start, end, source }
    }

    pub(crate) fn start(&self) -> u32 {
        self.start
    }

    pub(crate) fn end(&self) -> u32 {
        self.end
    }

    pub(crate) fn source(&self) -> &SourceAnnotation {
        &self.source
    }

    fn from_code_source_map_entry(entry: &CodeSourceMapEntry) -> Self {
        Self::new(
            entry.start,
            entry.end,
            SourceAnnotation::from_code_source_location(&entry.source),
        )
    }

    fn contains(&self, offset: u32) -> bool {
        self.start <= offset && offset < self.end
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SourceAnnotation {
    file: String,
    line: u32,
    column: u32,
    end_line: Option<u32>,
    end_column: Option<u32>,
    lines: Vec<(u32, String)>,
    truncated: bool,
}

impl SourceAnnotation {
    pub(crate) fn new(
        file: impl Into<String>,
        line: u32,
        column: u32,
        end_line: Option<u32>,
        end_column: Option<u32>,
    ) -> Self {
        let file = file.into();
        let (lines, truncated) = source_lines(&file, line, end_line);
        Self {
            file,
            line,
            column,
            end_line,
            end_column,
            lines,
            truncated,
        }
    }

    fn location(&self) -> String {
        if let (Some(end_line), Some(end_column)) = (self.end_line, self.end_column)
            && (self.line != end_line || self.column != end_column)
        {
            return format!(
                "{}:{}:{}-{}:{}",
                self.file, self.line, self.column, end_line, end_column
            );
        }

        format!("{}:{}:{}", self.file, self.line, self.column)
    }

    pub(crate) fn from_code_source_location(source: &CodeSourceLocation) -> Self {
        Self::new(
            source.file.clone(),
            source.line,
            source.column,
            source.end_line,
            source.end_column,
        )
    }

    pub(crate) fn to_code_source_location(&self) -> CodeSourceLocation {
        CodeSourceLocation::new(
            self.file.clone(),
            self.line,
            self.column,
            self.end_line,
            self.end_column,
        )
    }
}

fn source_lines(file: &str, line: u32, end_line: Option<u32>) -> (Vec<(u32, String)>, bool) {
    let Ok(contents) = std::fs::read_to_string(file) else {
        return (Vec::new(), false);
    };

    let start = line.max(1);
    let end = end_line.unwrap_or(start).max(start);
    let capped_end = end.min(start.saturating_add(3));
    let truncated = capped_end < end;
    let start_index = start.saturating_sub(1) as usize;
    let count = capped_end.saturating_sub(start).saturating_add(1) as usize;
    let lines = contents
        .lines()
        .skip(start_index)
        .take(count)
        .enumerate()
        .map(|(index, line_text)| (start + index as u32, line_text.to_string()))
        .collect();

    (lines, truncated)
}

fn write_source_annotation(out: &mut String, source: &SourceAnnotation) {
    writeln!(out, ";; source: {}", source.location()).expect("writing to String cannot fail");
    for (line, text) in &source.lines {
        writeln!(out, ";; {line:>6} | {text}").expect("writing to String cannot fail");
    }
    if source.truncated {
        writeln!(out, ";;      ...").expect("writing to String cannot fail");
    }
    writeln!(out).expect("writing to String cannot fail");
}

#[cfg(target_arch = "x86_64")]
fn host_capstone() -> Result<Capstone, String> {
    use capstone::arch::x86;

    Capstone::new()
        .x86()
        .mode(x86::ArchMode::Mode64)
        .build()
        .map_err(|err| format!("failed to create x86-64 disassembler: {err}"))
}

#[cfg(target_arch = "aarch64")]
fn host_capstone() -> Result<Capstone, String> {
    use capstone::arch::arm64;

    Capstone::new()
        .arm64()
        .mode(arm64::ArchMode::Arm)
        .build()
        .map_err(|err| format!("failed to create AArch64 disassembler: {err}"))
}

#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
fn host_capstone() -> Result<Capstone, String> {
    Err(format!(
        "disassembly is not supported on {}",
        std::env::consts::ARCH
    ))
}

fn render_bytes(bytes: &[u8]) -> String {
    let mut out = String::new();
    for (index, byte) in bytes.iter().enumerate() {
        if index != 0 {
            out.push(' ');
        }
        write!(out, "{byte:02x}").expect("writing to String cannot fail");
    }
    out
}

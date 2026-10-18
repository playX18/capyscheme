//! Opcode lengths, names, and metadata tables generated from [`def`](crate::bytecode::def).

use bytecodegen::OperandWidth;

use crate::bytecode::def::{self, OpcodeId};
use crate::utils::flags;

macro_rules! metadata_tables {
    ($(($Op:ident, $Meta:ident, $kind:expr, { $($field:ident : $ty:ty),* }),)*) => {
        pub(crate) fn metadata_kind(op: OpcodeId) -> usize {
            match op {
                $(OpcodeId::$Op => $kind,)*
                _ => panic!("template: opcode {op:?} has no metadata"),
            }
        }

        pub(crate) fn metadata_entry_size(op: OpcodeId) -> usize {
            match op {
                $(OpcodeId::$Op => size_of::<def::$Meta>(),)*
                _ => panic!("template: opcode {op:?} has no metadata"),
            }
        }

        pub(crate) fn metadata_field_offset(op: OpcodeId, field: usize) -> usize {
            match op {
                $(OpcodeId::$Op => [$(core::mem::offset_of!(def::$Meta, $field)),*][field],)*
                _ => panic!("template: opcode {op:?} has no metadata"),
            }
        }

        pub(crate) fn metadata_id_operand(op: OpcodeId) -> usize {
            match op {
                $(OpcodeId::$Op => def::$Op::OPERAND_METADATA_ID,)*
                _ => panic!("template: opcode {op:?} has no metadata"),
            }
        }
    };
}

def::for_each_metadata!(metadata_tables);

macro_rules! op_len_table {
    ($(($Op:ident, $snake:ident),)*) => {
        pub(crate) fn op_len(op: OpcodeId, width: OperandWidth) -> usize {
            match op {
                $(OpcodeId::$Op => def::$Op::LEN[width.index() as usize],)*
            }
        }
    };
}

def::for_each_opcode!(op_len_table);

macro_rules! opcode_names {
    ($(($Op:ident, $snake:ident),)*) => {
        /// Snake-case name of an opcode byte (`op_add`), or `op_invalid`.
        pub fn opcode_name(byte: u8) -> &'static str {
            match byte {
                $(x if x == OpcodeId::$Op as u8 => stringify!($snake),)*
                _ => "op_invalid",
            }
        }

        /// Parse a `CAPY_TEMPLATE_BAIL` value: opcode names with or without the
        /// `op_` prefix, comma-separated, forcing those handlers back to
        /// bail. Split from the env read so tests need no env mutation.
        pub fn parse_bail_filter_str(raw: &str) -> [bool; 256] {
            let mut forced = [false; 256];
            if raw.is_empty() {
                return forced;
            }
            let names: &[(&str, u8)] = &[
                $((stringify!($snake), OpcodeId::$Op as u8),)*
            ];
            for mut name in raw.split(',').map(str::trim) {
                if name.is_empty() {
                    continue;
                }
                name = name.strip_prefix("op_").unwrap_or(name);
                let mut matched = false;
                for (snake, byte) in names {
                    if snake.strip_prefix("op_").unwrap_or(snake) == name {
                        forced[*byte as usize] = true;
                        matched = true;
                    }
                }
                if !matched {
                    log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "CAPY_TEMPLATE_BAIL='{name}' names no opcode");
                }
            }
            forced
        }
    };
}

def::for_each_opcode!(opcode_names);

/// Parse the `CAPY_TEMPLATE_BAIL` flag.
pub fn parse_bail_filter() -> [bool; 256] {
    parse_bail_filter_str(flags::template_bail().unwrap_or_default())
}

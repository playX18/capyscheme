//! The per-unit constant table

use std::collections::HashMap;

use super::ir::func::Func;
use super::ir::types::CodeSource;
use crate::heap::Gc;
use crate::runtime::objectmodel::{CodeBlock, Value};

/// The values of one unit's constant table, each with its reference slot.
pub(crate) struct ConstantTable<'gc> {
    /// Entry `i` is reference slot `i + 1`.
    values: Vec<Value<'gc>>,
    /// The slot of each value, by its bits, so a value is stored once.
    slots: HashMap<u64, u32>,
}

impl<'gc> ConstantTable<'gc> {
    pub(crate) fn new() -> Self {
        Self {
            values: Vec::new(),
            slots: HashMap::new(),
        }
    }

    /// The reference slot of `value`, adding it on first use. Slots start at 1.
    pub(crate) fn intern(&mut self, value: Value<'gc>) -> u32 {
        if let Some(&slot) = self.slots.get(&value.bits()) {
            return slot;
        }
        self.values.push(value);
        let slot = self.values.len() as u32;
        self.slots.insert(value.bits(), slot);
        slot
    }

    /// The number of table slots, which are `1..=len()`.
    pub(crate) fn len(&self) -> u32 {
        self.values.len() as u32
    }

    /// The values in slot order, still rooted by the build's mutation scope.
    pub(crate) fn into_values(self) -> Vec<Value<'gc>> {
        self.values
    }
}

/// The table for `func`'s code blocks: the root and each nested block. Returns
/// the table and the table slot of each [`CodeRef`](super::ir::types::CodeRef),
/// by index.
///
/// A nested block is named by its pool constant, which the root's buffer holds
/// and which is copied here, so code reaches it through the table like the
/// root.
pub(crate) fn for_codes<'gc>(
    func: &Func<'gc>,
    root: Gc<'gc, CodeBlock<'gc>>,
) -> Result<(ConstantTable<'gc>, Vec<u32>), String> {
    let mut table = ConstantTable::new();
    // The folded heap values take the first slots, in the order the builder
    // interned them (`Func::heap_constant`).
    for &value in &func.heap_constants {
        table.intern(value);
    }
    if table.len() as usize != func.heap_constants.len() {
        return Err("two folded constants share a table slot".to_string());
    }
    let mut code_slots = Vec::with_capacity(func.codes.len());
    for (_, source) in func.codes.iter() {
        let value = match *source {
            CodeSource::Root => Value::from(root),
            CodeSource::Heap(value) => value,
            CodeSource::Pool(index) => {
                let value = root.buffer.constant(index as usize);
                if !value.is_cell() {
                    return Err(format!("pool constant {index} is not a code block"));
                }
                value
            }
        };
        code_slots.push(table.intern(value));
    }
    Ok((table, code_slots))
}

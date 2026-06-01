use std::io;

use crate::runtime::value::Value;

#[derive(Clone)]
pub struct FaslGraphTable<'gc> {
    entries: Vec<Option<Value<'gc>>>,
}

impl<'gc> FaslGraphTable<'gc> {
    pub fn new(len: usize) -> Self {
        Self {
            entries: vec![None; len],
        }
    }

    pub fn define(&mut self, index: u32, value: Value<'gc>) -> io::Result<()> {
        let slot = self
            .entries
            .get_mut(index as usize)
            .ok_or_else(|| invalid_data("FASL graph definition index out of bounds"))?;
        if slot.is_some() {
            return Err(invalid_data("duplicate FASL graph definition"));
        }
        *slot = Some(value);
        Ok(())
    }

    pub fn get(&self, index: u32) -> io::Result<Value<'gc>> {
        self.get_optional(index)?
            .ok_or_else(|| invalid_data("unknown FASL graph reference"))
    }

    pub fn get_optional(&self, index: u32) -> io::Result<Option<Value<'gc>>> {
        self.entries
            .get(index as usize)
            .copied()
            .ok_or_else(|| invalid_data("FASL graph reference index out of bounds"))
    }
}

fn invalid_data(message: &'static str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

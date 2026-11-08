use super::*;
use crate::runtime::codegen::opt::ir::func::Block;

pub struct LirBlock {
    pub params: Vec<VReg>,
    pub insts: Vec<LirInst>,
    /// `None` only while the block is being built.
    pub term: Option<LirTerm>,
    /// The IR block this came from, if any.
    pub source: Option<Block>,
}

impl LirBlock {
    pub(super) fn new(source: Option<Block>) -> Self {
        Self {
            params: Vec::new(),
            insts: Vec::new(),
            term: None,
            source,
        }
    }
}

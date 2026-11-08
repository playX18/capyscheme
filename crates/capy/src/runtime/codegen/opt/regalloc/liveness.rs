//! Block numbering and block-level liveness.

use cranelift_entity::{EntityRef, SecondaryMap};

use crate::runtime::codegen::opt::lir::{LBlock, LirFunc, Operand, Role};

/// Linear numbering of the blocks and instructions of a function.
pub(crate) struct Numbering {
    /// Blocks in linear order.
    pub order: Vec<LBlock>,
    /// Global index of the first instruction of each block.
    pub base: SecondaryMap<LBlock, u32>,
    /// Global index of each block's terminator.
    pub term: SecondaryMap<LBlock, u32>,
    /// The block of each global instruction index.
    pub block_at: Vec<LBlock>,
    /// Total number of instructions, terminators included.
    pub count: u32,
}

impl Numbering {
    pub(crate) fn new(func: &LirFunc) -> Result<Self, String> {
        for (b, data) in func.blocks.iter() {
            if data.term.is_none() {
                return Err(format!("{b} has no terminator"));
            }
        }
        let order = reverse_postorder(func);
        if order.len() != func.blocks.len() {
            return Err("the LIR has unreachable blocks".to_string());
        }
        let mut num = Numbering {
            order: order.clone(),
            base: SecondaryMap::new(),
            term: SecondaryMap::new(),
            block_at: Vec::new(),
            count: 0,
        };
        for &b in &order {
            let len = func.blocks[b].insts.len() as u32;
            num.base[b] = num.count;
            num.term[b] = num.count + len;
            for _ in 0..=len {
                num.block_at.push(b);
            }
            num.count += len + 1;
        }
        Ok(num)
    }

    /// The first position of a block.
    pub(crate) fn from(&self, b: LBlock) -> u32 {
        2 * self.base[b]
    }

    /// One past the last position of a block.
    pub(crate) fn to(&self, b: LBlock) -> u32 {
        2 * (self.term[b] + 1)
    }

    /// The position where a block's terminator reads its operands.
    pub(crate) fn term_pos(&self, b: LBlock) -> u32 {
        2 * self.term[b]
    }
}

fn reverse_postorder(func: &LirFunc) -> Vec<LBlock> {
    let succs = |b: LBlock| -> Vec<LBlock> {
        match &func.blocks[b].term {
            Some(term) => term.edges().iter().map(|e| e.block).collect(),
            None => Vec::new(),
        }
    };
    let entry = func.entry;
    let mut visited: SecondaryMap<LBlock, bool> = SecondaryMap::new();
    let mut post = Vec::new();
    visited[entry] = true;
    let mut stack: Vec<(LBlock, Vec<LBlock>, usize)> = vec![(entry, succs(entry), 0)];
    while let Some(top) = stack.last_mut() {
        if top.2 < top.1.len() {
            let s = top.1[top.2];
            top.2 += 1;
            if !visited[s] {
                visited[s] = true;
                let ss = succs(s);
                stack.push((s, ss, 0));
            }
        } else {
            post.push(top.0);
            stack.pop();
        }
    }
    post.reverse();
    post
}

/// A dense set of virtual registers.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub(crate) struct BitSet {
    words: Vec<u64>,
}

impl BitSet {
    pub(crate) fn new(bits: usize) -> Self {
        Self {
            words: vec![0; bits.div_ceil(64)],
        }
    }

    /// Inserts `i`, returning whether it was absent.
    pub(crate) fn insert(&mut self, i: usize) -> bool {
        let word = &mut self.words[i / 64];
        let old = *word;
        *word |= 1u64 << (i % 64);
        *word != old
    }

    pub(crate) fn contains(&self, i: usize) -> bool {
        self.words
            .get(i / 64)
            .is_some_and(|w| (w >> (i % 64)) & 1 == 1)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.words.iter().all(|&w| w == 0)
    }

    /// Adds every member of `other`, returning whether `self` changed.
    pub(crate) fn union_with(&mut self, other: &BitSet) -> bool {
        let mut changed = false;
        for (a, b) in self.words.iter_mut().zip(&other.words) {
            let n = *a | *b;
            if n != *a {
                *a = n;
                changed = true;
            }
        }
        changed
    }

    /// Removes every member of `other`.
    pub(crate) fn subtract(&mut self, other: &BitSet) {
        for (a, b) in self.words.iter_mut().zip(&other.words) {
            *a &= !*b;
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.words.iter().enumerate().flat_map(|(wi, &w)| {
            (0..64)
                .filter(move |b| (w >> b) & 1 == 1)
                .map(move |b| wi * 64 + b)
        })
    }
}

/// Live-in and live-out sets of every block.
pub(crate) struct Liveness {
    pub live_in: SecondaryMap<LBlock, BitSet>,
    pub live_out: SecondaryMap<LBlock, BitSet>,
}

fn apply_ops(ops: &[Operand], genset: &mut BitSet, kill: &mut BitSet) {
    for op in ops {
        if op.role == Role::Use && !kill.contains(op.vreg.index()) {
            genset.insert(op.vreg.index());
        }
    }
    for op in ops {
        if op.role == Role::Def {
            kill.insert(op.vreg.index());
        }
    }
}

/// Computes block liveness. Edge arguments are uses at the end of the
/// predecessor, so they are live-out of it and never live-in to the target.
pub(crate) fn compute(func: &LirFunc, num: &Numbering) -> Result<Liveness, String> {
    let n = func.vregs.len();
    let mut genset: SecondaryMap<LBlock, BitSet> = SecondaryMap::new();
    let mut kill: SecondaryMap<LBlock, BitSet> = SecondaryMap::new();
    let mut live_in: SecondaryMap<LBlock, BitSet> = SecondaryMap::new();
    let mut live_out: SecondaryMap<LBlock, BitSet> = SecondaryMap::new();
    for &b in &num.order {
        let data = &func.blocks[b];
        let mut g = BitSet::new(n);
        let mut k = BitSet::new(n);
        for &p in &data.params {
            k.insert(p.index());
        }
        for inst in &data.insts {
            apply_ops(&inst.operands(func), &mut g, &mut k);
        }
        if let Some(term) = &data.term {
            apply_ops(&term.operands(func), &mut g, &mut k);
        }
        genset[b] = g;
        kill[b] = k;
        live_in[b] = BitSet::new(n);
        live_out[b] = BitSet::new(n);
    }

    loop {
        let mut changed = false;
        for &b in num.order.iter().rev() {
            let Some(term) = &func.blocks[b].term else {
                continue;
            };
            let mut out = BitSet::new(n);
            for e in term.edges() {
                out.union_with(&live_in[e.block]);
                for a in &e.args {
                    out.insert(a.index());
                }
            }
            let mut inn = out.clone();
            inn.subtract(&kill[b]);
            inn.union_with(&genset[b]);
            if out != live_out[b] || inn != live_in[b] {
                changed = true;
                live_out[b] = out;
                live_in[b] = inn;
            }
        }
        if !changed {
            break;
        }
    }

    if !live_in[func.entry].is_empty() {
        return Err("a value is used before its definition".to_string());
    }
    Ok(Liveness { live_in, live_out })
}

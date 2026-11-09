//! Natural loops of the LIR, needed for greedy regalloc.
use cranelift_entity::SecondaryMap;

use super::liveness::Numbering;
use crate::runtime::codegen::opt::lir::{LBlock, LirFunc};

pub(crate) struct Loop {
    pub header: LBlock,
    /// The blocks of the loop, header included, in linear order.
    pub blocks: Vec<LBlock>,
    /// The loop containing this one with the smallest body.
    pub outer: Option<usize>,
    /// Nesting depth; an outermost loop has depth 1.
    pub depth: u32,
    /// The positions the loop occupies, sorted and merged.
    pub range: Vec<(u32, u32)>,
    /// Points that mark entering or leaving the loop: the end of each entry
    /// predecessor and the start of each exit successor, sorted.
    pub boundary: Vec<u32>,
    /// The entry predecessors of the header (outside the loop).
    pub entries: Vec<LBlock>,
}

pub(crate) struct Loops {
    pub loops: Vec<Loop>,
    /// The innermost loop of each block.
    pub innermost: SecondaryMap<LBlock, Option<usize>>,
}

impl Loops {
    pub(crate) fn new(func: &LirFunc, num: &Numbering) -> Self {
        let n = num.order.len();
        let mut index: SecondaryMap<LBlock, usize> = SecondaryMap::new();
        for (i, &b) in num.order.iter().enumerate() {
            index[b] = i;
        }
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
        let mut succs: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, &b) in num.order.iter().enumerate() {
            if let Some(term) = &func.blocks[b].term {
                for e in term.edges() {
                    let j = index[e.block];
                    if !succs[i].contains(&j) {
                        succs[i].push(j);
                        preds[j].push(i);
                    }
                }
            }
        }

        // Immediate dominators over the order, which is a reverse postorder.
        let entry = index[func.entry];
        let mut idom: Vec<Option<usize>> = vec![None; n];
        idom[entry] = Some(entry);
        let mut changed = true;
        while changed {
            changed = false;
            for b in 0..n {
                if b == entry {
                    continue;
                }
                let mut new: Option<usize> = None;
                for &p in &preds[b] {
                    if idom[p].is_none() {
                        continue;
                    }
                    new = Some(match new {
                        None => p,
                        Some(cur) => {
                            let (mut x, mut y) = (p, cur);
                            while x != y {
                                while x > y {
                                    x = idom[x].unwrap();
                                }
                                while y > x {
                                    y = idom[y].unwrap();
                                }
                            }
                            x
                        }
                    });
                }
                if new != idom[b] {
                    idom[b] = new;
                    changed = true;
                }
            }
        }
        let dominates = |a: usize, mut b: usize| -> bool {
            loop {
                if a == b {
                    return true;
                }
                match idom[b] {
                    Some(d) if d != b => b = d,
                    _ => return false,
                }
            }
        };

        // One loop per header: the union of the natural loops of its back edges.
        let mut headers: Vec<usize> = Vec::new();
        let mut bodies: Vec<Vec<bool>> = Vec::new();
        for b in 0..n {
            for &s in &succs[b] {
                if !dominates(s, b) {
                    continue;
                }
                let k = match headers.iter().position(|&h| h == s) {
                    Some(k) => k,
                    None => {
                        headers.push(s);
                        let mut body = vec![false; n];
                        body[s] = true;
                        bodies.push(body);
                        headers.len() - 1
                    }
                };
                let mut work = vec![b];
                while let Some(x) = work.pop() {
                    if bodies[k][x] {
                        continue;
                    }
                    bodies[k][x] = true;
                    work.extend(preds[x].iter().copied());
                }
            }
        }

        let mut loops: Vec<Loop> = Vec::new();
        for (k, &h) in headers.iter().enumerate() {
            let blocks: Vec<LBlock> = (0..n)
                .filter(|&i| bodies[k][i])
                .map(|i| num.order[i])
                .collect();
            let mut range: Vec<(u32, u32)> = Vec::new();
            for &b in &blocks {
                let (lo, hi) = (num.from(b), num.to(b));
                match range.last_mut() {
                    Some(last) if last.1 == lo => last.1 = hi,
                    _ => range.push((lo, hi)),
                }
            }
            let mut boundary: Vec<u32> = Vec::new();
            let mut entries: Vec<LBlock> = Vec::new();
            for &p in &preds[h] {
                if !bodies[k][p] {
                    entries.push(num.order[p]);
                    boundary.push(num.to(num.order[p]) - 1);
                }
            }
            for &b in &blocks {
                for &s in &succs[index[b]] {
                    if !bodies[k][s] {
                        boundary.push(num.from(num.order[s]));
                    }
                }
            }
            boundary.sort_unstable();
            boundary.dedup();
            loops.push(Loop {
                header: num.order[h],
                blocks,
                outer: None,
                depth: 0,
                range,
                boundary,
                entries,
            });
        }

        // Nesting: the containing loop with the smallest body is the outer.
        let contains = |outer: usize, inner: usize| -> bool {
            outer != inner && bodies[outer][headers[inner]]
        };
        for i in 0..loops.len() {
            let mut outer: Option<usize> = None;
            let mut depth = 1;
            for j in 0..loops.len() {
                if contains(j, i) {
                    depth += 1;
                    if outer.map_or(true, |o| loops[j].blocks.len() < loops[o].blocks.len()) {
                        outer = Some(j);
                    }
                }
            }
            loops[i].outer = outer;
            loops[i].depth = depth;
        }

        let mut innermost: SecondaryMap<LBlock, Option<usize>> = SecondaryMap::new();
        for (i, l) in loops.iter().enumerate() {
            for &b in &l.blocks {
                match innermost[b] {
                    Some(cur) if loops[cur].blocks.len() <= l.blocks.len() => {}
                    _ => innermost[b] = Some(i),
                }
            }
        }
        Self { loops, innermost }
    }
}

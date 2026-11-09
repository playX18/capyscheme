//! Functions: blocks with parameters, the instruction and value arenas, and
//! dominators.

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use cranelift_entity::{EntityList, EntityRef, ListPool, PrimaryMap, SecondaryMap, entity_impl};

use super::frame_state::{FrameState, FrameStateData};
use super::inst::{Edge, Inst, InstData, Op, Terminator};
use super::types::{CodeRef, CodeSource, Type};

/// A basic block.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block(u32);
entity_impl!(Block, "block");

/// An SSA value.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);
entity_impl!(Value, "v");

/// Where a value is defined.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueDef {
    /// Parameter `index` of `block`.
    Param { block: Block, index: u32 },
    /// The result of an instruction.
    Inst(Inst),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ValueData {
    pub ty: Type,
    pub def: ValueDef,
}

#[derive(Clone, Debug, Default)]
pub struct BlockData {
    pub params: EntityList<Value>,
    /// Instructions in execution order.
    pub insts: Vec<Inst>,
    /// `None` only while the block is being built.
    pub term: Option<Terminator>,
}

/// An OSR entry: a loop header that the entry block can transfer into from
/// a running baseline frame.
///
/// The entry block dispatches on its selector parameter. Selector `selector`
/// sends control to `block`, which passes its own parameters (the registers
/// live at the header) on to `header`. The prologue for this entry loads
/// every entry parameter from the frame, exactly as it does for a call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OsrEntry {
    /// The OSR block. Its parameters are the header's parameters.
    pub block: Block,
    /// The loop header it jumps to.
    pub header: Block,
    /// The bytecode pc of the loop hint that starts the header.
    pub pc: u32,
    /// The bytecode register of each parameter of `block`, in order.
    pub regs: Vec<u32>,
    /// The selector value that enters this block. Selector 0 is the normal
    /// entry.
    pub selector: u32,
}

/// The variadic entry of a procedure that starts with `enter_rest`.
///
/// The extra arguments are collected into a vector when the function is
/// entered, and the vector lives in the pseudo register `reg` (one past the
/// bytecode frame), which every join passes on and every frame state holds.
/// A deopt copies its elements back to `r[base..]`, where baseline reads them.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RestInfo {
    /// The number of required arguments.
    pub required: u32,
    /// The register that holds the extra argument count.
    pub cnt: u32,
    /// The first register of the rest area.
    pub base: u32,
    /// The pseudo register of the rest vector. In scalar mode it is the first
    /// of the `scalars` extra-argument registers.
    pub reg: u32,
    /// Scalar mode: the first `n` extra arguments live in the pseudo registers
    /// `reg..reg + n` (the word `undefined` past the actual count), and the
    /// register `reg + n` holds a vector of the rest (extras from index `n`
    /// on) or the empty-list marker, which only a deopt reads. `None` is the
    /// vector mode, where `reg` holds every extra.
    pub scalars: Option<u32>,
}

impl RestInfo {
    /// The number of pseudo registers the rest area takes after the frame.
    pub fn width(&self) -> u32 {
        self.scalars.map_or(1, |n| n + 1)
    }
}

/// One entry of a multi-entry `case-lambda` code block.
///
/// The dispatch picks a clause from the argument count and jumps to `block`;
/// that block's parameters are the registers live at the clause entry plus,
/// for a variadic clause, the shared rest-vector pseudo register. Control
/// enters the block from the function's entry `Switch`, never by falling
/// through.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CaseClause {
    /// The selector the entry sets to enter this clause (the table row).
    pub selector: u32,
    /// The clause's arity range: `min == max` for a fixed clause, `max` is
    /// `u32::MAX` for a variadic one.
    pub min: u32,
    pub max: u32,
    /// The bytecode pc where the clause starts (its first instruction).
    pub start: u32,
    /// The pc one past the clause: the next clause's start, or the code end.
    pub end: u32,
    /// The IR block that starts the clause.
    pub block: Block,
    /// The frame registers loaded into the block's parameters, in order.
    /// The rest-vector pseudo register is not part of this list.
    pub regs: Vec<u32>,
    /// Set for a variadic clause: its rest count/base and required count.
    pub rest: Option<RestInfo>,
}

/// A function in SSA form.
///
/// Instructions, values and frame states live in arenas. Operand lists are
/// [`EntityList`]s in a shared pool, so the arenas stay `Copy`-friendly.

pub struct Func<'gc> {
    /// `name at file:line:col` of the root procedure, for dumps and logs.
    pub label: String,
    pub entry: Block,
    pub blocks: PrimaryMap<Block, BlockData>,
    pub insts: PrimaryMap<Inst, InstData>,
    pub values: PrimaryMap<Value, ValueData>,
    pub frames: PrimaryMap<FrameState, FrameStateData>,
    /// The code blocks the IR names. `CodeRef(0)` is always the root.
    pub codes: PrimaryMap<CodeRef, CodeSource<'gc>>,
    pub pool: ListPool<Value>,
    /// The bytecode register of each entry parameter, in parameter order.
    /// The entry glue loads them from `fp[reg]`. A selector parameter, when
    /// there is one, follows them and has no register.
    pub entry_regs: Vec<u32>,
    /// The OSR entries, in selector order. Empty when no loop can be
    /// entered from a baseline frame.
    pub osr: Vec<OsrEntry>,
    /// The entry's selector parameter, present exactly when `osr` is not
    /// empty. It is the last parameter of the entry block.
    pub entry_selector: Option<Value>,
    /// Set for a variadic procedure. The entry then accepts any argument
    /// count of at least `required`, and `entry_regs` ends with `rest.reg`,
    /// whose parameter the entry glue fills with the collected vector.
    pub rest: Option<RestInfo>,
    /// The clauses of a multi-entry `case-lambda`, in table (source) order.
    /// Empty for an ordinary procedure.
    pub case_clauses: Vec<CaseClause>,
    /// The shared pseudo register of every variadic clause's rest vector, one
    /// past the bytecode frame. Set exactly when some clause has a rest.
    pub rest_reg: Option<u32>,

    pub heap_constants: Vec<crate::runtime::objectmodel::Value<'gc>>,
    /// The globals whose values this function folded.
    pub deps: Vec<GlobalDep<'gc>>,
    /// The class descriptors whose layout this function assumed when it
    /// emitted an inline instance allocation. A redefinition of one of them
    /// invalidates the unit.
    pub class_deps: Vec<ClassDep<'gc>>,
    /// Whether the builder emitted `CheckNotInvalid` guards (after every
    /// call and helper, or under the `fold_guards` policy only after
    /// operations that can fire a watchpoint the unit depends on).
    pub checks_invalid: bool,
    /// The `CheckNotInvalid` guards that carry the OSR entry's frame. They
    /// stay whatever the dependencies are. Builder bookkeeping, emptied by
    /// [`prune_checks`](crate::runtime::codegen::opt::build).
    pub osr_checks: HashSet<Inst>,
    /// The guards that follow a write to one statically known variable and
    /// are needed only if the unit depends on that variable. Every other
    /// guard is needed when the unit has any dependency. Builder bookkeeping,
    /// emptied by `prune_checks`.
    pub cell_checks: HashMap<Inst, crate::runtime::objectmodel::Value<'gc>>,
    /// Loop headers the versioning pass already cloned. The slow (original)
    /// loop keeps its guards, so without this mark the pass would clone it
    /// again on every pipeline round.
    pub versioned: HashSet<Block>,
    /// Headers of loops that [`unswitch`](crate::runtime::codegen::opt::passes::unswitch)
    /// has split, and of its clones, so a loop is split at most once.
    pub unswitched: HashSet<Block>,
    /// Headers of loops whose first iteration [`peel`](crate::runtime::codegen::opt::passes::peel)
    /// has copied out, so a loop is peeled at most once.
    pub peeled: HashSet<Block>,
    /// Headers of loops that the generic unroller has replicated.
    pub unrolled: HashSet<Block>,
    /// Bytecode pcs of the root code block where the exit profile says the
    /// last compile speculated too often. A speculative hoist whose loop
    /// entry frame is at one of them is not made.
    pub spec_blocked: HashSet<u32>,
    /// The last dominator tree, with the control-flow signature it was built
    /// from. See [`Func::dom_tree`].
    dom_cache: RefCell<Option<DomCache>>,
    /// A spare buffer for the signature of the current graph, so a lookup
    /// does not allocate.
    dom_scratch: Cell<Vec<u32>>,
}

/// A dominator tree and the control-flow signature it was built from
struct DomCache {
    sig: Vec<u32>,
    tree: Rc<DomTree>,
}

/// A variable whose value the function folded into constants
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GlobalDep<'gc> {
    /// The variable, rooted until the install.
    pub variable: crate::runtime::objectmodel::Value<'gc>,
    /// The value folded, rooted until the install.
    pub value: crate::runtime::objectmodel::Value<'gc>,
}

/// An assumption that a class descriptor is the class table's current one and
/// still has the layout the build saw. The install registers a dependent on
/// the descriptor's redefinition watchpoint and rechecks both conditions; a
/// later redefinition fires the set and invalidates the unit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ClassDep<'gc> {
    /// The descriptor, rooted until the install.
    pub class: crate::runtime::objectmodel::Value<'gc>,
    /// The class id the descriptor was looked up by.
    pub id: u32,
    /// Its layout stamp at build time.
    pub stamp: u32,
}

impl<'gc> Func<'gc> {
    /// The table constant of heap `value`, interned.
    pub fn heap_constant(&mut self, value: crate::runtime::objectmodel::Value<'gc>) -> u32 {
        let index = match self.heap_constants.iter().position(|&b| b == value) {
            Some(index) => index,
            None => {
                self.heap_constants.push(value);
                self.heap_constants.len() - 1
            }
        };
        index as u32 + 1
    }

    /// The `case-lambda` clause whose code contains `pc`, if any.
    pub fn clause_at(&self, pc: u32) -> Option<usize> {
        self.case_clauses
            .iter()
            .position(|clause| pc >= clause.start && pc < clause.end)
    }

    /// The rest area that a frame at `pc` rebuilds. For a case-lambda clause
    /// it is that clause's; for a single variadic entry it is the entry's.
    pub fn rest_info(&self, pc: u32) -> Option<RestInfo> {
        if let Some(rest) = self.rest {
            return Some(rest);
        }
        let clause = self.clause_at(pc)?;
        self.case_clauses[clause].rest
    }

    /// A function with one empty entry block.
    pub fn new() -> Self {
        let mut blocks = PrimaryMap::new();
        let entry = blocks.push(BlockData::default());
        let mut codes = PrimaryMap::new();
        codes.push(CodeSource::Root);
        Self {
            label: "<unknown procedure>".to_string(),
            entry,
            blocks,
            insts: PrimaryMap::new(),
            values: PrimaryMap::new(),
            frames: PrimaryMap::new(),
            codes,
            pool: ListPool::new(),
            entry_regs: Vec::new(),
            osr: Vec::new(),
            entry_selector: None,
            rest: None,
            case_clauses: Vec::new(),
            rest_reg: None,
            heap_constants: Vec::new(),
            deps: Vec::new(),
            class_deps: Vec::new(),
            checks_invalid: false,
            osr_checks: HashSet::new(),
            cell_checks: HashMap::new(),
            versioned: HashSet::new(),
            unswitched: HashSet::new(),
            peeled: HashSet::new(),
            unrolled: HashSet::new(),
            spec_blocked: HashSet::new(),
            dom_cache: RefCell::new(None),
            dom_scratch: Cell::new(Vec::new()),
        }
    }

    pub fn new_block(&mut self) -> Block {
        self.blocks.push(BlockData::default())
    }

    /// Adds a parameter to the end of `block`'s parameter list.
    pub fn append_param(&mut self, block: Block, ty: Type) -> Value {
        let index = self.blocks[block].params.len(&self.pool) as u32;
        let value = self.values.push(ValueData {
            ty,
            def: ValueDef::Param { block, index },
        });
        self.blocks[block].params.push(value, &mut self.pool);
        value
    }

    /// Appends an instruction to `block`. Returns its result when `result`
    /// is given, which must agree with [`Op::has_result`].
    pub fn push_inst(
        &mut self,
        block: Block,
        op: Op,
        args: &[Value],
        result: Option<Type>,
        frame: Option<FrameState>,
    ) -> Option<Value> {
        let inst = self.insts.next_key();
        let args = EntityList::from_slice(args, &mut self.pool);
        let result = result.map(|ty| {
            self.values.push(ValueData {
                ty,
                def: ValueDef::Inst(inst),
            })
        });
        let pushed = self.insts.push(InstData {
            op,
            args,
            result,
            frame,
        });
        debug_assert_eq!(pushed, inst);
        self.blocks[block].insts.push(inst);
        result
    }

    pub fn set_terminator(&mut self, block: Block, term: Terminator) {
        self.blocks[block].term = Some(term);
    }

    /// Builds an edge to `target` carrying `args`.
    pub fn edge(&mut self, target: Block, args: &[Value]) -> Edge {
        Edge {
            block: target,
            args: EntityList::from_slice(args, &mut self.pool),
        }
    }

    pub fn push_frame(&mut self, frame: FrameStateData) -> FrameState {
        self.frames.push(frame)
    }

    pub fn args(&self, inst: Inst) -> &[Value] {
        self.insts[inst].args.as_slice(&self.pool)
    }

    pub fn params(&self, block: Block) -> &[Value] {
        self.blocks[block].params.as_slice(&self.pool)
    }

    pub fn edge_args(&self, edge: &Edge) -> &[Value] {
        edge.args.as_slice(&self.pool)
    }

    pub fn value_type(&self, value: Value) -> Type {
        self.values[value].ty
    }

    pub fn value_def(&self, value: Value) -> ValueDef {
        self.values[value].def
    }

    /// Every value a terminator reads, including edge arguments.
    pub fn terminator_uses(&self, term: &Terminator) -> Vec<Value> {
        let mut uses = match term {
            Terminator::Branch { cond, .. } => vec![*cond],
            Terminator::Switch { value, .. } => vec![*value],
            Terminator::Return(value) => vec![*value],
            Terminator::ReturnMany { values, .. } => values.clone(),
            Terminator::TailCall { callee, args, .. } => {
                let mut uses = vec![*callee];
                uses.extend_from_slice(args.as_slice(&self.pool));
                uses
            }
            Terminator::Jump(_) | Terminator::Deopt { .. } | Terminator::Unreachable => vec![],
        };
        for edge in term.edges() {
            uses.extend_from_slice(self.edge_args(&edge));
        }
        uses
    }

    pub fn successors(&self, block: Block) -> Vec<Block> {
        match &self.blocks[block].term {
            Some(term) => term.edges().into_iter().map(|e| e.block).collect(),
            None => vec![],
        }
    }

    /// The predecessor lists. An edge that appears twice is listed twice.
    pub fn predecessors(&self) -> SecondaryMap<Block, Vec<Block>> {
        let mut preds: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
        for block in self.blocks.keys() {
            for succ in self.successors(block) {
                preds[succ].push(block);
            }
        }
        preds
    }

    /// Blocks reachable from the entry, in reverse postorder.
    pub fn reverse_postorder(&self) -> Vec<Block> {
        let mut visited: SecondaryMap<Block, bool> = SecondaryMap::new();
        let mut post = Vec::new();
        let mut stack = vec![(self.entry, self.successors(self.entry))];
        visited[self.entry] = true;
        loop {
            let Some((block, succs)) = stack.last_mut() else {
                break;
            };
            match succs.pop() {
                Some(next) => {
                    if !visited[next] {
                        visited[next] = true;
                        stack.push((next, self.successors(next)));
                    }
                }
                None => {
                    post.push(*block);
                    stack.pop();
                }
            }
        }
        post.reverse();
        post
    }

    /// The control-flow graph as a flat list of ids: the entry, then each
    /// block with its successor list. Dominators depend on nothing else, so
    /// two equal signatures mean two equal trees.
    fn cfg_signature(&self, out: &mut Vec<u32>) {
        out.clear();
        out.push(self.entry.index() as u32);
        for (block, data) in self.blocks.iter() {
            out.push(block.index() as u32);
            match &data.term {
                None => out.push(u32::MAX),
                Some(term) => {
                    let edges = term.edges();
                    out.push(edges.len() as u32);
                    out.extend(edges.iter().map(|edge| edge.block.index() as u32));
                }
            }
        }
    }

    /// The dominator tree of the current control-flow graph. It is cached
    /// and reused while the graph's signature is unchanged, so passes that
    /// ask several times per round build it once. Passes may edit the graph
    /// freely: a changed graph is detected by comparing signatures, never
    /// trusted from a flag, so a stale tree cannot be returned.
    pub fn dom_tree(&self) -> Rc<DomTree> {
        let mut sig = self.dom_scratch.take();
        self.cfg_signature(&mut sig);
        let cached = self
            .dom_cache
            .borrow()
            .as_ref()
            .filter(|cache| cache.sig == sig)
            .map(|cache| cache.tree.clone());
        if let Some(tree) = cached {
            self.dom_scratch.set(sig);
            return tree;
        }
        let tree = Rc::new(self.compute_dom_tree());
        let stale = self
            .dom_cache
            .replace(Some(DomCache { sig, tree: tree.clone() }));
        if let Some(stale) = stale {
            self.dom_scratch.set(stale.sig);
        }
        tree
    }

    /// Computes dominators with the Cooper–Harvey–Kennedy iterative algorithm.
    fn compute_dom_tree(&self) -> DomTree {
        let rpo = self.reverse_postorder();
        let mut rpo_num: SecondaryMap<Block, u32> = SecondaryMap::with_default(UNREACHABLE);
        for (num, &block) in rpo.iter().enumerate() {
            rpo_num[block] = num as u32;
        }

        let preds = self.predecessors();
        let mut idom: SecondaryMap<Block, Option<Block>> = SecondaryMap::new();
        idom[self.entry] = Some(self.entry);

        let mut changed = true;
        while changed {
            changed = false;
            for &block in rpo.iter().skip(1) {
                // Only processed predecessors take part. Unreachable ones
                // never get an `idom`.
                let mut new_idom = None;
                for &pred in &preds[block] {
                    if idom[pred].is_none() {
                        continue;
                    }
                    new_idom = Some(match new_idom {
                        None => pred,
                        Some(cur) => intersect(&idom, &rpo_num, pred, cur),
                    });
                }
                if new_idom != idom[block] {
                    idom[block] = new_idom;
                    changed = true;
                }
            }
        }
        // The entry is its own dominator only while the algorithm runs.
        idom[self.entry] = None;

        DomTree { idom, rpo_num, rpo }
    }
}

impl Default for Func<'_> {
    fn default() -> Self {
        Self::new()
    }
}

const UNREACHABLE: u32 = u32::MAX;

fn intersect(
    idom: &SecondaryMap<Block, Option<Block>>,
    rpo_num: &SecondaryMap<Block, u32>,
    mut a: Block,
    mut b: Block,
) -> Block {
    while a != b {
        while rpo_num[a] > rpo_num[b] {
            a = idom[a].expect("processed block has an immediate dominator");
        }
        while rpo_num[b] > rpo_num[a] {
            b = idom[b].expect("processed block has an immediate dominator");
        }
    }
    a
}

/// Immediate dominators of the blocks reachable from the entry.
pub struct DomTree {
    /// `None` for the entry and for unreachable blocks.
    idom: SecondaryMap<Block, Option<Block>>,
    /// Reverse postorder number, or `UNREACHABLE`.
    rpo_num: SecondaryMap<Block, u32>,
    rpo: Vec<Block>,
}

impl DomTree {
    pub fn is_reachable(&self, block: Block) -> bool {
        self.rpo_num[block] != UNREACHABLE
    }

    pub fn idom(&self, block: Block) -> Option<Block> {
        self.idom[block]
    }

    /// Reachable blocks in reverse postorder.
    pub fn rpo(&self) -> &[Block] {
        &self.rpo
    }

    /// Whether `a` dominates `b`. A block dominates itself. Unreachable
    /// blocks dominate nothing and are dominated by nothing.
    pub fn dominates(&self, a: Block, b: Block) -> bool {
        if !self.is_reachable(a) || !self.is_reachable(b) {
            return false;
        }
        let a_num = self.rpo_num[a];
        let mut cur = b;
        loop {
            if cur == a {
                return true;
            }
            // A dominator comes before what it dominates in reverse postorder.
            if self.rpo_num[cur] <= a_num {
                return false;
            }
            match self.idom[cur] {
                Some(parent) => cur = parent,
                None => return false,
            }
        }
    }
}

//! Lower a CFG procedure to bytecode.
//!
//! Register allocation is a linear scan over one interval per uvar. A frame
//! is laid out as
//!
//! ```text
//!   r0            return address
//!   r1            the closure (the procedure's `binding`)
//!   r2 ..         parameters, then allocated registers
//!   temporaries   constants materialized for one instruction
//!   rest area     extra arguments of a variadic procedure
//! ```
//!
//! A non-tail call puts the callee's frame at `base`, two above the highest
//! register that is live after the call, and writes the callee and its
//! arguments straight into that frame. `r[base-1]` stays free: it is
//! reserved and raw across the call. The registers that are live across
//! the call are recorded in the mask of the return point, which is what the
//! collector reads. A single returned value lands directly in its
//! destination register through the return point's `dst` operand.

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::bytecode::def::{self as bc, *};
use crate::compiler::cfg::{
    Block, BlockId, BranchTarget, CodeId, Instruction, Operand, Procedure, Program, RestPredicate,
    SwitchCase, SwitchCaseValue, SwitchKind, Terminator, UVar,
};
use crate::heap::Gc;
use crate::runtime::interp::runtime_procs::RuntimeProc;
use crate::runtime::prims::def::Primitive;
use crate::runtime::value::Symbol;
use crate::runtime::value::Value;
use crate::runtime::value::proc::SRC_FLAG_SYNTHETIC;
use crate::utils::flags;
use bytecodegen::{Label, LinkedBytecodeView};

/// One entry of a code block's constant pool.
#[derive(Clone, Copy, Debug)]
pub enum Constant<'gc> {
    Value(Value<'gc>),
    /// The code block of another procedure of the same program.
    Code(CodeId),
    /// The cache cell with this key. Cells are shared by every procedure of
    /// the program and start out as `#f`.
    Cell(Value<'gc>),
}

pub struct CompiledProcedure<'gc> {
    pub code_id: CodeId,
    pub code: Vec<u8>,
    pub pool: Vec<u32>,
    pub metadata_counts: [u32; bc::NUM_METADATA_KINDS],
    pub constants: Vec<Constant<'gc>>,
    /// Raw `CodeArity` value.
    pub arity: i32,
    pub frame_size: u32,
    pub name: Value<'gc>,
    pub meta: Value<'gc>,
    pub source: Value<'gc>,
    /// Source runs in increasing byte-offset order: disjoint
    /// `[start, start + len)` ranges, each carrying a span id into
    /// [`CompiledProcedure::spans`] (1-based, 0 = none) with
    /// [`SRC_FLAG_*`] flags.
    pub srcmap: Vec<SrcRun>,
    /// Deduplicated span values referenced by [`CompiledProcedure::srcmap`].
    pub spans: Vec<Value<'gc>>,
}

pub struct SrcRun {
    pub start: u32,
    pub len: u32,
    pub flags: u8,
    pub span: u32,
}

pub struct CompiledProgram<'gc> {
    pub procedures: Vec<CompiledProcedure<'gc>>,
    /// Index of the entry procedure in `procedures`.
    pub entry: usize,
}

pub fn compile_program<'gc>(program: &Program<'gc>) -> Result<CompiledProgram<'gc>, String> {
    let procedures = program
        .procedures
        .iter()
        .map(compile_procedure)
        .collect::<Result<Vec<_>, _>>()?;
    let entry = program
        .procedures
        .iter()
        .position(|procedure| procedure.code == program.entry)
        .expect("entry procedure is part of the program");
    Ok(CompiledProgram { procedures, entry })
}

pub fn compile_procedure<'gc>(
    procedure: &Procedure<'gc>,
) -> Result<CompiledProcedure<'gc>, String> {
    let mut lower = Lower::new(procedure);
    lower.run()?;
    lower.finish()
}

#[derive(Clone, Copy)]
enum Src<'gc> {
    Register(u32),
    Const(Value<'gc>),
}

#[derive(Clone, Copy)]
enum BranchKind {
    Null,
    NNull,
    Pair,
    NPair,
    Fixnum,
    NFixnum,
    Undefined,
    NUndefined,
    Eq,
    Neq,
    Lt,
    Le,
    NumEq,
    Ge,
    Gt,
    NLt,
    NLe,
    NNumEq,
    NGe,
    NGt,
}

#[derive(Clone, Copy)]
enum Condition {
    /// `register` is not `#f`.
    Truthy(u32),
    /// `register` is `#f`.
    Falsy(u32),
    /// A one-register type test
    Type {
        yes: BranchKind,
        no: BranchKind,
        register: u32,
    },
    /// A two-register comparison
    Cmp {
        yes: BranchKind,
        no: BranchKind,
        a: u32,
        b: u32,
    },
}

fn fused_jump_kinds(prim: Primitive, arity: usize) -> Option<(BranchKind, BranchKind)> {
    use Primitive as P;
    match (prim, arity) {
        (P::IsNull, 1) => Some((BranchKind::Null, BranchKind::NNull)),
        (P::IsPair, 1) => Some((BranchKind::Pair, BranchKind::NPair)),
        (P::IsFixnum, 1) => Some((BranchKind::Fixnum, BranchKind::NFixnum)),
        (P::IsUnspecified, 1) => Some((BranchKind::Undefined, BranchKind::NUndefined)),
        (P::IsEq, 2) => Some((BranchKind::Eq, BranchKind::Neq)),
        (P::NumericLt, 2) => Some((BranchKind::Lt, BranchKind::NLt)),
        (P::NumericLte, 2) => Some((BranchKind::Le, BranchKind::NLe)),
        (P::NumericEqual, 2) => Some((BranchKind::NumEq, BranchKind::NNumEq)),
        (P::NumericGte, 2) => Some((BranchKind::Ge, BranchKind::NGe)),
        (P::NumericGt, 2) => Some((BranchKind::Gt, BranchKind::NGt)),
        _ => None,
    }
}

struct TestLink {
    pos: usize,
    var: UVar,
}

struct FusedBranch<'gc> {
    prim: Primitive,
    args: Vec<Operand<'gc>>,
    sense: bool,
    elided: Vec<usize>,
}

/// An out-of-line slow path for one `get_slot`/`put_slot`: a call to the
/// real `slot-ref`/`slot-set!` procedure, emitted after all blocks.
struct SlotStub<'gc> {
    label_slow: Label,
    label_next: Label,
    /// `get_slot` (true) or `put_slot` (false).
    get: bool,
    dst_var: UVar,
    dst: u32,
    obj: u32,
    name: Value<'gc>,
    /// `put_slot` value register, unused for `get_slot`.
    src: u32,
    base: u32,
    mask: PoolReference,
}

/// A switch table to append to the aux pool in `finish`, once all liveness
/// masks are emitted.
/// Symbols whose masked hash is `.0`: the bucket's label and the members
/// it compares one by one.
type SymbolBucket<'gc> = (u64, Label, Vec<(Value<'gc>, Label)>);

/// Dense switch tables (`[default][span|FLAG][min][disp × span]`, indexed by
/// `key - min`) hold one word per key in the range, so they only pay when
/// the keys cluster: the span must be small in absolute terms (pool size,
/// L1 residency) as well as relative to the case count.
const SWITCH_DENSE_MAX_SPAN: usize = 256;
/// The span may be at most this multiple of the case count: a dense table
/// is then at most about twice the words of the sparse one it replaces.
const SWITCH_DENSE_MAX_SPARSE_MULTIPLE: usize = 4;

/// `(min, span)` when sorted, deduplicated int-switch `keys` should use
/// the dense layout: a non-empty range whose span fits
/// [`SWITCH_DENSE_MAX_SPAN`] and is at most
/// [`SWITCH_DENSE_MAX_SPARSE_MULTIPLE`] times the case count.
pub(crate) fn dense_span_for_keys(keys: &[i32]) -> Option<(i32, usize)> {
    let (min, max) = (*keys.first()?, *keys.last()?);
    let span = (max as i64 - min as i64 + 1) as usize;
    (span <= SWITCH_DENSE_MAX_SPAN && span <= SWITCH_DENSE_MAX_SPARSE_MULTIPLE * keys.len())
        .then_some((min, span))
}

enum SwitchFixup {
    /// `[default][count]{[key:i32][disp:i32]}`, sorted by key, dense
    /// (`[default][span|FLAG][min:i32][disp × span]`) when the keys
    /// cluster (see [`SWITCH_DENSE_MAX_SPAN`]).
    Int {
        atom: usize,
        default: Label,
        cases: Vec<(i32, Label)>,
    },
    /// `[default][count][mask_lo][mask_hi]{[key_lo][key_hi][disp]}`, sorted
    /// by key.
    Sym {
        atom: usize,
        mask: u64,
        default: Label,
        cases: Vec<(u64, Label)>,
    },
}

/// A value-producing numeric comparison.
macro_rules! emit_cmp {
    ($this:expr, $Op:ident, $dst:expr, $args:expr) => {{
        let lhs = $this.operand($args[0], 0);
        let rhs = $this.operand($args[1], 1);
        let ins = $Op::new(Register($dst), Register(lhs), Register(rhs))
            .with_metadata(Default::default());
        $this.ins(ins);
    }};
}

/// One pre-link atom: an instruction (jumps carry their target label) or
/// a label marking the next instruction's address.
enum Atom {
    Ins { ins: bc::LinkableInstruction },
    Label(Label),
}

fn metadata_kind_counts(link_counts: &[u32]) -> [u32; bc::NUM_METADATA_KINDS] {
    macro_rules! collect {
        ($(($Op:ident, $Meta:ident, $kind:expr, { $($fname:ident : $fty:ty),* }),)*) => {{
            let mut counts = [0u32; bc::NUM_METADATA_KINDS];
            $(counts[$kind] = link_counts[bc::OpcodeId::$Op.table_index()];)*
            counts
        }};
    }
    bc::for_each_metadata!(collect)
}

struct Lower<'a, 'gc> {
    procedure: &'a Procedure<'gc>,
    blocks: HashMap<BlockId, &'a Block<'gc>>,
    order: Vec<BlockId>,
    order_index: HashMap<BlockId, usize>,
    live_in: HashMap<BlockId, HashSet<UVar>>,
    loop_headers: HashSet<BlockId>,
    used: HashSet<UVar>,
    registers: HashMap<UVar, u32>,
    /// First register above the allocated ones.
    tmp_base: u32,
    max_tmp: u32,
    cnt_register: Option<u32>,
    extent: u32,

    atoms: Vec<Atom>,
    labels: HashMap<BlockId, Label>,
    constants: Vec<Constant<'gc>>,
    value_index: HashMap<u64, u32>,
    code_index: HashMap<CodeId, u32>,
    cell_index: HashMap<u64, u32>,
    sources: Vec<(usize, Value<'gc>)>,
    /// Atom indices of `enter_rest`/`rest_ref`/`rest_list` instructions
    /// whose `base` is patched once the rest area is known.
    rest_fixups: Vec<usize>,
    pool: Vec<u32>,
    mask_index: HashMap<Vec<u32>, u32>,
    switch_fixups: Vec<SwitchFixup>,
    slot_stubs: Vec<SlotStub<'gc>>,
}

impl<'a, 'gc> Lower<'a, 'gc> {
    fn new(procedure: &'a Procedure<'gc>) -> Self {
        let blocks = procedure
            .blocks
            .iter()
            .map(|block| (block.id, block))
            .collect();
        Self {
            procedure,
            blocks,
            order: Vec::new(),
            order_index: HashMap::new(),
            live_in: HashMap::new(),
            loop_headers: HashSet::new(),
            used: HashSet::new(),
            registers: HashMap::new(),
            tmp_base: 0,
            max_tmp: 0,
            cnt_register: None,
            extent: 0,
            atoms: Vec::new(),
            labels: HashMap::new(),
            constants: Vec::new(),
            value_index: HashMap::new(),
            code_index: HashMap::new(),
            cell_index: HashMap::new(),
            sources: Vec::new(),
            rest_fixups: Vec::new(),
            pool: Vec::new(),
            mask_index: HashMap::new(),
            switch_fixups: Vec::new(),
            slot_stubs: Vec::new(),
        }
    }

    fn run(&mut self) -> Result<(), String> {
        self.order_blocks();
        self.compute_liveness();
        self.allocate_registers();
        self.emit_procedure()
    }

    /// Reverse post-order from the entry, with blocks that end in `raise`
    /// moved to the end.
    fn order_blocks(&mut self) {
        let mut visited = HashSet::new();
        let mut post = Vec::new();

        let mut stack: Vec<(BlockId, usize)> = vec![(self.procedure.entry, 0)];
        visited.insert(self.procedure.entry);
        while let Some((block, next)) = stack.last().copied() {
            let successors = self.successors(block);
            if next < successors.len() {
                stack.last_mut().expect("nonempty").1 += 1;
                let successor = successors[next];
                if self.blocks.contains_key(&successor) && visited.insert(successor) {
                    stack.push((successor, 0));
                }
            } else {
                post.push(block);
                stack.pop();
            }
        }
        post.reverse();
        let (cold, hot): (Vec<_>, Vec<_>) = post.into_iter().partition(|id| {
            *id != self.procedure.entry
                && matches!(self.blocks[id].terminator, Terminator::Raise { .. })
        });
        self.order = hot;
        self.order.extend(cold);
        self.order_index = self
            .order
            .iter()
            .enumerate()
            .map(|(index, id)| (*id, index))
            .collect();

        for (index, id) in self.order.iter().enumerate() {
            for successor in self.successors(*id) {
                if self
                    .order_index
                    .get(&successor)
                    .is_some_and(|s| *s <= index)
                {
                    self.loop_headers.insert(successor);
                }
            }
        }
    }

    fn successors(&self, block: BlockId) -> Vec<BlockId> {
        match &self.blocks[&block].terminator {
            Terminator::Branch {
                consequent,
                alternative,
                ..
            }
            | Terminator::BranchPrim {
                consequent,
                alternative,
                ..
            } => [alternative.local_successor(), consequent.local_successor()]
                .into_iter()
                .flatten()
                .collect(),
            other => other.successors(),
        }
    }

    fn instruction_uses(instruction: &Instruction<'gc>) -> Vec<UVar> {
        match instruction {
            Instruction::RestToList { .. }
            | Instruction::RestRef { .. }
            | Instruction::RestLength { .. }
            | Instruction::RestPredicate { .. } => vec![],
            other => other
                .uses()
                .into_iter()
                .filter_map(|operand| match operand {
                    Operand::Local(var) => Some(var),
                    Operand::Constant(_) => None,
                })
                .collect(),
        }
    }

    fn terminator_uses(terminator: &Terminator<'gc>) -> Vec<UVar> {
        terminator
            .uses()
            .into_iter()
            .filter_map(|operand| match operand {
                Operand::Local(var) => Some(var),
                Operand::Constant(_) => None,
            })
            .collect()
    }

    fn compute_liveness(&mut self) {
        let mut live_in: HashMap<BlockId, HashSet<UVar>> =
            self.order.iter().map(|id| (*id, HashSet::new())).collect();
        let mut predecessors: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
        for id in &self.order {
            for successor in self.blocks[id].terminator.successors() {
                predecessors.entry(successor).or_default().push(*id);
            }
        }
        let mut worklist: VecDeque<BlockId> = self.order.iter().rev().copied().collect();
        let mut queued: HashSet<BlockId> = worklist.iter().copied().collect();
        while let Some(id) = worklist.pop_front() {
            queued.remove(&id);
            let block = self.blocks[&id];
            let mut live: HashSet<UVar> = HashSet::new();
            for successor in block.terminator.successors() {
                if let Some(successor_live) = live_in.get(&successor) {
                    live.extend(successor_live.iter().copied());
                }
            }
            live.extend(Self::terminator_uses(&block.terminator));
            for instruction in block.instructions.iter().rev() {
                for def in instruction.defs() {
                    live.remove(&def);
                }
                live.extend(Self::instruction_uses(instruction));
            }
            if live != live_in[&id] {
                live_in.insert(id, live);
                for predecessor in predecessors.get(&id).into_iter().flatten() {
                    if queued.insert(*predecessor) {
                        worklist.push_back(*predecessor);
                    }
                }
            }
        }
        self.live_in = live_in;

        for id in &self.order {
            let block = self.blocks[id];
            for instruction in &block.instructions {
                self.used.extend(Self::instruction_uses(instruction));
            }
            self.used.extend(Self::terminator_uses(&block.terminator));
        }
    }

    fn allocate_registers(&mut self) {
        let procedure = self.procedure;
        let binding = procedure.binding;

        // One interval per uvar: from its first definition to its last use,
        // widened to whole blocks it is live through.
        let mut intervals: HashMap<UVar, (u32, u32)> = HashMap::new();
        let mut extend = |var: UVar, position: u32| {
            if var == binding {
                return;
            }
            intervals
                .entry(var)
                .and_modify(|(start, end)| {
                    *start = (*start).min(position);
                    *end = (*end).max(position);
                })
                .or_insert((position, position));
        };
        for param in &procedure.params {
            extend(*param, 0);
        }
        // Assign copies whose source dies at the copy: candidates for
        // sharing a register.
        let mut copies: Vec<(UVar, UVar, u32)> = Vec::new();
        let mut position = 1u32;
        for id in &self.order {
            let block = self.blocks[id];
            let start = position;
            for var in &self.live_in[id] {
                extend(*var, start);
            }
            for instruction in &block.instructions {
                for var in Self::instruction_uses(instruction) {
                    extend(var, position);
                }
                for def in instruction.defs() {
                    match instruction {
                        // Bound on the edge from the call, before anything
                        // in the block runs.
                        Instruction::Receive { .. } | Instruction::ReceiveRest { .. } => {
                            extend(def, start)
                        }
                        _ => extend(def, position),
                    }
                }
                if let Instruction::Assign {
                    dst,
                    src: Operand::Local(src),
                } = instruction
                {
                    copies.push((*dst, *src, position));
                }
                position += 1;
            }
            for var in Self::terminator_uses(&block.terminator) {
                extend(var, position);
            }
            for successor in block.terminator.successors() {
                if let Some(live) = self.live_in.get(&successor) {
                    for var in live {
                        extend(*var, position);
                    }
                }
            }
            position += 1;
        }

        let mut first_free = 2 + procedure.params.len() as u32;
        if procedure.variadic.is_some() {
            self.cnt_register = Some(first_free);
            first_free += 1;
        }

        let mut active: Vec<(u32, u32)> = Vec::new(); // (end, register)
        let mut in_use: BTreeSet<u32> = BTreeSet::new();
        self.registers.insert(binding, 1);
        for (index, param) in procedure.params.iter().enumerate() {
            let register = 2 + index as u32;
            self.registers.insert(*param, register);
            let end = intervals.get(param).map_or(0, |interval| interval.1);
            active.push((end, register));
            in_use.insert(register);
        }
        let hints: HashMap<UVar, (UVar, u32)> = copies
            .into_iter()
            .map(|(dst, src, at)| (dst, (src, at)))
            .collect();

        let mut pending: Vec<(UVar, u32, u32)> = intervals
            .iter()
            .filter(|(var, _)| !self.registers.contains_key(var))
            .map(|(var, (start, end))| (*var, *start, *end))
            .collect();
        pending.sort_by_key(|(var, start, _)| (*start, var.0));

        let mut high = first_free;
        for (var, start, end) in pending {
            active.retain(|(active_end, register)| {
                if *active_end < start {
                    in_use.remove(register);
                    false
                } else {
                    true
                }
            });
            // A copy whose source ends exactly here may take over its
            // register, the copy then costs nothing.
            let mut chosen = None;
            if let Some((src, at)) = hints.get(&var)
                && *at == start
                && let Some(register) = self.registers.get(src).copied()
                && register >= first_free
                && intervals
                    .get(src)
                    .is_some_and(|interval| interval.1 == start)
                && let Some(slot) = active.iter().position(|(_, r)| *r == register)
            {
                active.swap_remove(slot);
                chosen = Some(register);
            }
            let register = chosen.unwrap_or_else(|| {
                let mut candidate = first_free;
                for taken in in_use.range(first_free..) {
                    if *taken != candidate {
                        break;
                    }
                    candidate += 1;
                }
                candidate
            });
            in_use.insert(register);
            active.push((end, register));
            self.registers.insert(var, register);
            high = high.max(register + 1);
        }
        self.tmp_base = high;
    }

    fn ins(&mut self, ins: bc::LinkableInstruction) {
        if !self.try_fuse(&ins) {
            self.atoms.push(Atom::Ins { ins });
        }
    }

    fn try_fuse(&mut self, next: &bc::LinkableInstruction) -> bool {
        let fused = match self.atoms.last() {
            Some(Atom::Ins { ins: prev }) => match (prev, next) {
                (
                    bc::LinkableInstruction::OpMove(first),
                    bc::LinkableInstruction::OpMove(second),
                ) => Some(
                    OpMoveTwo {
                        dst1: first.dst,
                        src1: first.src,
                        dst2: second.dst,
                        src2: second.src,
                    }
                    .into(),
                ),
                (
                    bc::LinkableInstruction::OpAddImmediate { opcode: first, .. },
                    bc::LinkableInstruction::OpMove(second),
                ) if second.src == first.dst => Some(
                    OpAddImmediateMove::new(first.dst, first.src, first.value, second.dst)
                        .with_metadata(Default::default()),
                ),
                (
                    bc::LinkableInstruction::OpVectorRef { opcode: first, .. },
                    bc::LinkableInstruction::OpMul { opcode: second, .. },
                ) => Some(
                    OpVectorRefMul::new(
                        first.dst,
                        first.vector,
                        first.index,
                        second.dst,
                        second.lhs,
                        second.rhs,
                    )
                    .with_metadata(Default::default()),
                ),
                _ => None,
            },
            _ => None,
        };
        let Some(fused) = fused else {
            return false;
        };

        let fused_at = self.atoms.len() - 1;

        for (pos, _) in self
            .sources
            .iter_mut()
            .rev()
            .take_while(|(pos, _)| *pos > fused_at)
        {
            *pos = fused_at;
        }
        self.atoms[fused_at] = Atom::Ins { ins: fused };
        true
    }

    fn branch(&mut self, ins: bc::LinkableInstruction) {
        self.atoms.push(Atom::Ins { ins });
    }

    fn block_label(&mut self, block: BlockId) -> Label {
        if let Some(label) = self.labels.get(&block) {
            return *label;
        }
        let label = Label::new();
        self.labels.insert(block, label);
        label
    }

    fn note_source(&mut self, source: Value<'gc>) {
        self.sources.push((self.atoms.len(), source));
    }

    fn bump_extent(&mut self, extent: u32) {
        self.extent = self.extent.max(extent);
    }

    fn register(&self, var: UVar) -> u32 {
        *self
            .registers
            .get(&var)
            .unwrap_or_else(|| panic!("uvar {var:?} has no register"))
    }

    fn temp(&mut self, index: u32) -> u32 {
        self.max_tmp = self.max_tmp.max(index + 1);
        self.tmp_base + index
    }

    fn mov(&mut self, dst: u32, src: u32) {
        if dst != src {
            self.ins(
                OpMove {
                    dst: Register(dst),
                    src: Register(src),
                }
                .into(),
            );
        }
    }

    fn value_constant(&mut self, value: Value<'gc>) -> u32 {
        if let Some(index) = self.value_index.get(&value.bits()) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(Constant::Value(value));
        self.value_index.insert(value.bits(), index);
        index
    }

    fn code_constant(&mut self, code: CodeId) -> u32 {
        if let Some(index) = self.code_index.get(&code) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(Constant::Code(code));
        self.code_index.insert(code, index);
        index
    }

    fn cell_constant(&mut self, key: Value<'gc>) -> u32 {
        if let Some(index) = self.cell_index.get(&key.bits()) {
            return *index;
        }
        let index = self.constants.len() as u32;
        self.constants.push(Constant::Cell(key));
        self.cell_index.insert(key.bits(), index);
        index
    }

    fn load_const(&mut self, dst: u32, value: Value<'gc>) {
        let dst = Register(dst);
        if value.is_int32() {
            self.ins(
                OpLoadImmediate {
                    dst,
                    value: Immediate(value.as_int32()),
                }
                .into(),
            );
        } else if !value.is_cell() && value.bits() <= u32::MAX as u64 {
            self.ins(
                OpLoadRaw {
                    dst,
                    bits: Count(value.bits() as u32),
                }
                .into(),
            );
        } else {
            let index = self.value_constant(value);
            self.ins(
                OpLoadConstant {
                    dst,
                    constant: Constant(index),
                }
                .into(),
            );
        }
    }

    /// Register holding `operand`, materializing a constant in temporary
    /// `slot`.
    fn operand(&mut self, operand: Operand<'gc>, slot: u32) -> u32 {
        match operand {
            Operand::Local(var) => self.register(var),
            Operand::Constant(value) => {
                let temp = self.temp(slot);
                self.load_const(temp, value);
                temp
            }
        }
    }

    fn src(&self, operand: Operand<'gc>) -> Src<'gc> {
        match operand {
            Operand::Local(var) => Src::Register(self.register(var)),
            Operand::Constant(value) => Src::Const(value),
        }
    }

    /// Perform `moves` (source, destination) as one parallel assignment.
    /// `spare` is a register no move reads or writes.
    fn parallel_moves(&mut self, moves: Vec<(Src<'gc>, u32)>, spare: u32) {
        let mut pending: Vec<(u32, u32)> = Vec::new();
        let mut constants: Vec<(Value<'gc>, u32)> = Vec::new();
        for (src, dst) in moves {
            match src {
                Src::Register(src) if src != dst => pending.push((src, dst)),
                Src::Register(_) => {}
                Src::Const(value) => constants.push((value, dst)),
            }
        }
        while !pending.is_empty() {
            // A move whose destination no other pending move reads is safe.
            let ready = pending
                .iter()
                .position(|(_, dst)| !pending.iter().any(|(src, _)| src == dst));
            match ready {
                Some(index) => {
                    let (src, dst) = pending.swap_remove(index);
                    self.mov(dst, src);
                }
                None => {
                    // Every destination is still read: a cycle. Save one
                    // destination and redirect its readers.
                    let (_, dst) = pending[0];
                    self.mov(spare, dst);
                    self.bump_extent(spare + 1);
                    for (src, _) in pending.iter_mut() {
                        if *src == dst {
                            *src = spare;
                        }
                    }
                }
            }
        }
        for (value, dst) in constants {
            self.load_const(dst, value);
        }
    }

    fn mask(&self, live: impl Iterator<Item = UVar>) -> Vec<u32> {
        let mut bits: Vec<u32> = Vec::new();
        for var in live {
            let Some(register) = self.registers.get(&var).copied() else {
                continue;
            };
            if register < 2 {
                continue;
            }
            let word = (register / 32) as usize;
            if bits.len() <= word {
                bits.resize(word + 1, 0);
            }
            bits[word] |= 1 << (register % 32);
        }
        bits
    }

    /// Pool index of a liveness mask, shared with identical masks.
    fn mask_ref(&mut self, live: impl Iterator<Item = UVar>) -> PoolReference {
        let bits = self.mask(live);
        if let Some(index) = self.mask_index.get(&bits) {
            return PoolReference(*index);
        }
        let index = self.pool.len() as u32;
        self.pool.push(bits.len() as u32);
        self.pool.extend_from_slice(&bits);
        self.mask_index.insert(bits, index);
        PoolReference(index)
    }

    /// Uvars live just after instruction `index` of `block`.
    fn live_after(&self, block: BlockId, index: usize) -> HashSet<UVar> {
        let instructions = &self.blocks[&block].instructions;
        let terminator = &self.blocks[&block].terminator;
        let mut live: HashSet<UVar> = HashSet::new();
        for successor in terminator.successors() {
            if let Some(live_in) = self.live_in.get(&successor) {
                live.extend(live_in.iter().copied());
            }
        }
        live.extend(Self::terminator_uses(terminator));
        for instruction in instructions[index + 1..].iter().rev() {
            for def in instruction.defs() {
                live.remove(&def);
            }
            live.extend(Self::instruction_uses(instruction));
        }
        live
    }

    fn emit_procedure(&mut self) -> Result<(), String> {
        let procedure = self.procedure;

        self.note_source(Value::new(false));
        self.ins(OpNop {}.into());
        self.bump_extent(self.tmp_base);

        let order = self.order.clone();
        for (index, id) in order.iter().enumerate() {
            let next = order.get(index + 1).copied();
            let block = self.blocks[id];
            let label = self.block_label(*id);
            self.atoms.push(Atom::Label(label));
            if self.loop_headers.contains(id) {
                let live: Vec<UVar> = self.live_in[id].iter().copied().collect();
                let mask = self.mask_ref(live.into_iter());
                self.ins(OpLoopHint { mask }.into());
            }
            self.note_source(block.source);
            let fusion = self.plan_branch_fusion(*id, &block.terminator);
            for (index, instruction) in block.instructions.iter().enumerate() {
                if fusion
                    .as_ref()
                    .is_some_and(|fusion| fusion.elided.contains(&index))
                {
                    continue;
                }
                self.emit_instruction(*id, index, instruction);
            }
            self.emit_terminator(&block.terminator, fusion.as_ref(), next);
        }
        self.emit_slot_stubs();

        let rest_base = self.tmp_base + self.max_tmp;
        self.bump_extent(rest_base);
        let size = self.extent + 1;

        if size >= 65536 {
            return Err(format!("frame of {} is too large", procedure.name));
        }
        for atom in std::mem::take(&mut self.rest_fixups) {
            let Atom::Ins { ins, .. } = &mut self.atoms[atom] else {
                unreachable!("rest fixup names an instruction atom");
            };
            match ins {
                bc::LinkableInstruction::OpEnterRest(op) => op.base = Register(rest_base),
                bc::LinkableInstruction::OpRestRef(op) => op.base = Register(rest_base),
                bc::LinkableInstruction::OpRestList(op) => op.base = Register(rest_base),
                _ => unreachable!("rest fixup names a rest instruction"),
            }
        }
        let entry = if let Some(cnt) = self.cnt_register {
            OpEnterRest {
                required: Count(procedure.params.len() as u32),
                cnt: Register(cnt),
                base: Register(rest_base),
                frame: Count(size),
            }
            .into()
        } else {
            OpEnter {
                argc: Count(procedure.params.len() as u32),
                frame: Count(size),
            }
            .into()
        };
        self.atoms[0] = Atom::Ins { ins: entry };
        Ok(())
    }

    fn finish(mut self) -> Result<CompiledProcedure<'gc>, String> {
        let procedure = self.procedure;
        let size = self.extent + 1;

        let mut pending: Vec<(usize, usize, Label)> = Vec::new();
        for fixup in std::mem::take(&mut self.switch_fixups) {
            match fixup {
                SwitchFixup::Int {
                    atom,
                    default,
                    cases,
                } => {
                    let table = PoolReference(self.pool.len() as u32);
                    match &mut self.atoms[atom] {
                        Atom::Ins {
                            ins: bc::LinkableInstruction::OpSwitchFixnum(op),
                            ..
                        } => op.table = table,
                        Atom::Ins {
                            ins: bc::LinkableInstruction::OpSwitchChar(op),
                            ..
                        } => op.table = table,
                        _ => unreachable!("switch fixup names a switch instruction"),
                    }
                    // `cases` is sorted and deduplicated by key.
                    let keys: Vec<i32> = cases.iter().map(|(key, _)| *key).collect();
                    let dense = dense_span_for_keys(&keys);
                    pending.push((self.pool.len(), atom, default));
                    self.pool.push(0);
                    if let Some((min, span)) = dense {
                        self.pool.push(span as u32 | bc::SWITCH_DENSE_FLAG);
                        self.pool.push(min as u32);
                        let mut cases = cases.iter().peekable();
                        for offset in 0..span {
                            let key = min.wrapping_add(offset as i32);
                            let target = match cases.peek() {
                                Some((case_key, _)) if *case_key == key => {
                                    cases.next().map(|(_, target)| *target).unwrap_or(default)
                                }
                                _ => default,
                            };
                            pending.push((self.pool.len(), atom, target));
                            self.pool.push(0);
                        }
                    } else {
                        // The count shares its word with the dense flag, so it
                        // must stay below bit 31.
                        if cases.len() >= 1 << 31 {
                            return Err(format!(
                                "switch in {} has too many cases",
                                self.procedure.name
                            ));
                        }
                        self.pool.push(cases.len() as u32);
                        for (key, target) in cases {
                            self.pool.push(key as u32);
                            pending.push((self.pool.len(), atom, target));
                            self.pool.push(0);
                        }
                    }
                }
                SwitchFixup::Sym {
                    atom,
                    mask,
                    default,
                    cases,
                } => {
                    let table = PoolReference(self.pool.len() as u32);
                    match &mut self.atoms[atom] {
                        Atom::Ins {
                            ins: bc::LinkableInstruction::OpSwitchSymbol(op),
                            ..
                        } => op.table = table,
                        _ => unreachable!("switch fixup names a switch instruction"),
                    }
                    pending.push((self.pool.len(), atom, default));
                    self.pool.push(0);
                    self.pool.push(cases.len() as u32);
                    self.pool.push(mask as u32);
                    self.pool.push((mask >> 32) as u32);
                    for (key, target) in cases {
                        self.pool.push(key as u32);
                        self.pool.push((key >> 32) as u32);
                        pending.push((self.pool.len(), atom, target));
                        self.pool.push(0);
                    }
                }
            }
        }
        let atoms = std::mem::take(&mut self.atoms);
        let mut label_at: Vec<Option<Label>> = Vec::with_capacity(atoms.len());
        let linkables = atoms.into_iter().map(|atom| match atom {
            Atom::Ins { ins } => {
                label_at.push(None);
                ins
            }
            Atom::Label(label) => {
                label_at.push(Some(label));
                bc::bind(label)
            }
        });
        let linked = bc::link(linkables)
            .map_err(|err| format!("cannot link {}: {err:?}", self.procedure.name))?;

        let code = linked.bytes().to_vec();
        let mut atom_offsets = vec![0u32; label_at.len() + 1];
        let mut ordinal = 0;
        for (index, slot) in label_at.iter().enumerate() {
            if slot.is_none() {
                atom_offsets[index] = linked.instructions()[ordinal].offset;
                ordinal += 1;
            }
        }
        atom_offsets[label_at.len()] = code.len() as u32;
        let mut next = code.len() as u32;
        for index in (0..label_at.len()).rev() {
            if label_at[index].is_some() {
                atom_offsets[index] = next;
            } else {
                next = atom_offsets[index];
            }
        }
        let mut labels: HashMap<Label, u32> = HashMap::new();
        for (index, slot) in label_at.iter().enumerate() {
            if let Some(label) = slot {
                labels.insert(*label, atom_offsets[index]);
            }
        }
        for (slot, atom, target) in pending {
            let disp = labels[&target] as i64 - atom_offsets[atom] as i64;
            let disp = i32::try_from(disp).map_err(|_| {
                format!(
                    "switch displacement in {} overflows i32",
                    self.procedure.name
                )
            })?;
            self.pool[slot] = disp as u32;
        }

        let code_len = code.len() as u32;
        let atom_count = label_at.len();
        let mut spans: Vec<Value<'gc>> = Vec::new();
        let mut span_ids: HashMap<u64, u32> = HashMap::new();
        let mut srcmap: Vec<SrcRun> = Vec::with_capacity(self.sources.len());
        let mut notes = self.sources.into_iter().peekable();
        while let Some((atom, source)) = notes.next() {
            let end_atom = notes.peek().map_or(atom_count, |(next, _)| *next);
            if end_atom <= atom {
                continue;
            }
            let start = atom_offsets[atom];
            let end = if end_atom >= atom_count {
                code_len
            } else {
                atom_offsets[end_atom]
            };
            if end <= start {
                continue;
            }
            let (flags, span) = if source == Value::new(false) || source.is_null() {
                (SRC_FLAG_SYNTHETIC, 0)
            } else {
                let id = *span_ids.entry(source.bits()).or_insert_with(|| {
                    spans.push(source);
                    spans.len() as u32
                });
                (0, id)
            };
            match srcmap.last_mut() {
                Some(last)
                    if last.span == span
                        && last.flags == flags
                        && last.start + last.len == start =>
                {
                    last.len = end - last.start;
                }
                _ => srcmap.push(SrcRun {
                    start,
                    len: end - start,
                    flags,
                    span,
                }),
            }
        }
        let arity = if procedure.variadic.is_some() {
            -(procedure.params.len() as i32 + 1)
        } else {
            procedure.params.len() as i32
        };
        Ok(CompiledProcedure {
            code_id: procedure.code,
            code,
            pool: self.pool,
            metadata_counts: metadata_kind_counts(linked.metadata_counts()),
            constants: self.constants,
            arity,
            frame_size: size,
            name: procedure.name,
            meta: procedure.meta,
            source: procedure.source,
            srcmap,
            spans,
        })
    }

    fn cnt(&self) -> u32 {
        self.cnt_register
            .expect("rest instruction in a procedure without a rest argument")
    }

    fn emit_instruction(&mut self, block: BlockId, index: usize, instruction: &Instruction<'gc>) {
        match instruction {
            Instruction::Assign { dst, src } => {
                let dst = self.register(*dst);
                match *src {
                    Operand::Local(src) => {
                        let src = self.register(src);
                        self.mov(dst, src);
                    }
                    Operand::Constant(value) => self.load_const(dst, value),
                }
            }
            Instruction::Const { dst, value } => {
                let dst = self.register(*dst);
                self.load_const(dst, *value);
            }
            Instruction::MakeClosure {
                dst,
                code,
                free_count,
                env,
            } => {
                let dst = self.register(*dst);
                let index = self.code_constant(*code);
                match env {
                    Some(env) => {
                        let env = self.operand(*env, 0);
                        self.ins(
                            OpClosureEnv {
                                dst: Register(dst),
                                constant: Constant(index),
                                nfree: Count(*free_count as u32),
                                env: Register(env),
                            }
                            .into(),
                        );
                    }
                    None => {
                        self.ins(
                            OpClosure {
                                dst: Register(dst),
                                constant: Constant(index),
                                nfree: Count(*free_count as u32),
                            }
                            .into(),
                        );
                    }
                }
            }
            Instruction::MakeEnv { dst, size } => {
                let dst = self.register(*dst);
                self.ins(
                    OpMakeEnv {
                        dst: Register(dst),
                        size: Count(*size as u32),
                    }
                    .into(),
                );
            }
            Instruction::EnvRef { dst, env, index } => {
                let env = self.operand(*env, 0);
                let dst = self.register(*dst);
                self.ins(
                    OpEnvRef {
                        dst: Register(dst),
                        env: Register(env),
                        index: Count(*index as u32),
                    }
                    .into(),
                );
            }
            Instruction::EnvSet { env, index, value } => {
                let env = self.operand(*env, 0);
                let value = self.operand(*value, 1);
                self.ins(
                    OpEnvSet {
                        env: Register(env),
                        index: Count(*index as u32),
                        src: Register(value),
                    }
                    .into(),
                );
            }
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => {
                let procedure = self.procedure;
                let is_self = matches!(closure, Operand::Local(var) if *var == procedure.binding);
                let dst = self.register(*dst);
                if is_self {
                    // The self reference has no slot: it is the closure in r1.
                    let self_reference = (*index != 0 || !procedure.env_shared)
                        && procedure.free_vars.get(*index).is_some_and(|var| {
                            Gc::ptr_eq(
                                procedure.sources[var],
                                procedure.sources[&procedure.binding],
                            )
                        });
                    if self_reference {
                        self.mov(dst, 1);
                    } else {
                        self.ins(
                            OpFreeRef {
                                dst: Register(dst),
                                index: Count(*index as u32),
                            }
                            .into(),
                        );
                    }
                } else {
                    let closure = self.operand(*closure, 0);
                    self.ins(
                        OpClosureRef {
                            dst: Register(dst),
                            closure: Register(closure),
                            index: Count(*index as u32),
                        }
                        .into(),
                    );
                }
            }
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => {
                let closure = self.operand(*closure, 0);
                let value = self.operand(*value, 1);
                self.ins(
                    OpClosureSet {
                        closure: Register(closure),
                        index: Count(*index as u32),
                        src: Register(value),
                    }
                    .into(),
                );
            }
            Instruction::CacheRef {
                dst,
                cache_key,
                source,
            } => {
                self.note_source(*source);
                let Operand::Constant(key) = cache_key else {
                    panic!("cache-ref expects a constant key, got {cache_key:?}");
                };
                let index = self.cell_constant(*key);
                let dst = self.register(*dst);
                let ins = OpCellRef::new(Register(dst), Constant(index))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            Instruction::CacheSet {
                dst,
                cache_key,
                value,
                source,
            } => {
                self.note_source(*source);
                let Operand::Constant(key) = cache_key else {
                    panic!("cache-set! expects a constant key, got {cache_key:?}");
                };
                let index = self.cell_constant(*key);
                let value = self.operand(*value, 0);
                self.ins(
                    OpCellSet {
                        constant: Constant(index),
                        src: Register(value),
                    }
                    .into(),
                );
                self.set_undefined(*dst);
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                self.note_source(*source);
                self.emit_prim(block, index, *dst, *prim, args);
            }
            Instruction::RestToList { dst, source, .. } => {
                self.note_source(*source);
                let cnt = self.cnt();
                let dst = self.register(*dst);
                self.rest_fixups.push(self.atoms.len());
                self.ins(
                    OpRestList {
                        dst: Register(dst),
                        cnt: Register(cnt),
                        base: Register(0),
                        skip: Count(0),
                    }
                    .into(),
                );
            }
            Instruction::RestRef {
                dst, index, source, ..
            } => {
                self.note_source(*source);
                let cnt = self.cnt();
                let dst = self.register(*dst);
                self.rest_fixups.push(self.atoms.len());
                self.ins(
                    OpRestRef {
                        dst: Register(dst),
                        cnt: Register(cnt),
                        base: Register(0),
                        index: Count(*index as u32),
                    }
                    .into(),
                );
            }
            Instruction::RestLength {
                dst, skip, source, ..
            } => {
                self.note_source(*source);
                let cnt = self.cnt();
                let dst = self.register(*dst);
                self.ins(
                    OpRestLen {
                        dst: Register(dst),
                        cnt: Register(cnt),
                        skip: Count(*skip as u32),
                    }
                    .into(),
                );
            }
            Instruction::RestPredicate {
                dst,
                predicate,
                skip,
                source,
                ..
            } => {
                self.note_source(*source);
                let dst = self.register(*dst);
                match predicate {
                    RestPredicate::List => self.load_const(dst, Value::new(true)),
                    RestPredicate::Null | RestPredicate::Pair => {
                        let cnt = self.cnt();
                        let ins = if matches!(predicate, RestPredicate::Null) {
                            OpRestNull {
                                dst: Register(dst),
                                cnt: Register(cnt),
                                skip: Count(*skip as u32),
                            }
                            .into()
                        } else {
                            OpRestPair {
                                dst: Register(dst),
                                cnt: Register(cnt),
                                skip: Count(*skip as u32),
                            }
                            .into()
                        };
                        self.ins(ins);
                    }
                }
            }
            // Bound by the moves after the return point of each call that
            // reaches this block.
            Instruction::Receive { .. } | Instruction::ReceiveRest { .. } => {}
        }
    }

    fn set_undefined(&mut self, dst: UVar) {
        if self.used.contains(&dst) {
            let dst = self.register(dst);
            self.load_const(dst, Value::undefined());
        }
    }

    fn emit_generic_prim(&mut self, dst: u32, prim: Primitive, args: &[Operand<'gc>]) {
        let dst = Register(dst);
        let primitive = PrimitiveId(prim.id() as u32);
        if args.len() <= 3 {
            let registers: Vec<u32> = args
                .iter()
                .enumerate()
                .map(|(slot, arg)| self.operand(*arg, slot as u32))
                .collect();
            let ins = match registers.as_slice() {
                [] => OpCallPrimitive0 { dst, primitive }.into(),
                [a] => OpCallPrimitive1 {
                    dst,
                    primitive,
                    a: Register(*a),
                }
                .into(),
                [a, b] => OpCallPrimitive2 {
                    dst,
                    primitive,
                    a: Register(*a),
                    b: Register(*b),
                }
                .into(),
                [a, b, c] => OpCallPrimitive3 {
                    dst,
                    primitive,
                    a: Register(*a),
                    b: Register(*b),
                    c: Register(*c),
                }
                .into(),
                _ => unreachable!("at most three arguments"),
            };
            self.ins(ins);
        } else {
            // `primitive_n` reads contiguous registers.
            let first = self.temp(0);
            for slot in 1..args.len() as u32 {
                self.temp(slot);
            }
            let moves: Vec<(Src<'gc>, u32)> = args
                .iter()
                .enumerate()
                .map(|(index, arg)| (self.src(*arg), first + index as u32))
                .collect();
            let spare = self.temp(args.len() as u32);
            self.parallel_moves(moves, spare);
            self.ins(
                OpCallPrimitiveN {
                    dst,
                    primitive,
                    first: Register(first),
                    argc: Count(args.len() as u32),
                }
                .into(),
            );
        }
    }

    fn const_fixnum(&self, block: BlockId, index: usize, operand: &Operand<'gc>) -> Option<i32> {
        match operand {
            Operand::Constant(value) if value.is_int32() => Some(value.as_int32()),
            Operand::Local(first) if flags::fold_arith_immediate() => {
                let instructions = &self.blocks[&block].instructions;
                let mut current = *first;
                let mut end = index;
                loop {
                    let defining = instructions[..end]
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, instruction)| instruction.def() == Some(current));
                    match defining {
                        Some((_, Instruction::Const { value, .. })) => {
                            return value.is_int32().then(|| value.as_int32());
                        }
                        Some((pos, Instruction::Assign { src, .. })) => match src {
                            Operand::Constant(value) => {
                                return value.is_int32().then(|| value.as_int32());
                            }
                            Operand::Local(next) => {
                                current = *next;
                                end = pos;
                            }
                        },

                        _ => return None,
                    }
                }
            }
            _ => None,
        }
    }

    fn emit_prim(
        &mut self,
        block: BlockId,
        index: usize,
        dst_var: UVar,
        prim: Primitive,
        args: &[Operand<'gc>],
    ) {
        use Primitive as P;
        let dst = self.register(dst_var);
        match (prim, args.len()) {
            (P::Car, 1) => {
                let src = self.operand(args[0], 0);
                let ins =
                    OpCar::new(Register(dst), Register(src)).with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Cdr, 1) => {
                let src = self.operand(args[0], 0);
                let ins =
                    OpCdr::new(Register(dst), Register(src)).with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Cons, 2) => {
                let car = self.operand(args[0], 0);
                let cdr = self.operand(args[1], 1);
                self.ins(
                    OpCons {
                        dst: Register(dst),
                        car: Register(car),
                        cdr: Register(cdr),
                    }
                    .into(),
                );
            }
            (P::SetCar, 2) => {
                let pair = self.operand(args[0], 0);
                let src = self.operand(args[1], 1);
                self.ins(
                    OpSetCar {
                        pair: Register(pair),
                        src: Register(src),
                    }
                    .into(),
                );
                self.set_undefined(dst_var);
            }
            (P::SetCdr, 2) => {
                let pair = self.operand(args[0], 0);
                let src = self.operand(args[1], 1);
                self.ins(
                    OpSetCdr {
                        pair: Register(pair),
                        src: Register(src),
                    }
                    .into(),
                );
                self.set_undefined(dst_var);
            }
            (P::IsEq, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                self.ins(
                    OpEq {
                        dst: Register(dst),
                        lhs: Register(lhs),
                        rhs: Register(rhs),
                    }
                    .into(),
                );
            }
            (P::Not, 1) => {
                let src = self.operand(args[0], 0);
                self.ins(
                    OpNot {
                        dst: Register(dst),
                        src: Register(src),
                    }
                    .into(),
                );
            }
            (P::Plus, 2) => match (
                self.const_fixnum(block, index, &args[0]),
                self.const_fixnum(block, index, &args[1]),
            ) {
                (None, Some(value)) => self.emit_add_immediate(dst, args[0], value),
                (Some(value), None) => self.emit_add_immediate(dst, args[1], value),
                _ => {
                    let lhs = self.operand(args[0], 0);
                    let rhs = self.operand(args[1], 1);
                    let ins = OpAdd::new(Register(dst), Register(lhs), Register(rhs))
                        .with_metadata(Default::default());
                    self.ins(ins);
                }
            },
            (P::Minus, 2) => {
                match self
                    .const_fixnum(block, index, &args[1])
                    .and_then(|value| value.checked_neg())
                {
                    Some(negated) => self.emit_add_immediate(dst, args[0], negated),
                    None => {
                        let lhs = self.operand(args[0], 0);
                        let rhs = self.operand(args[1], 1);
                        let ins = OpSub::new(Register(dst), Register(lhs), Register(rhs))
                            .with_metadata(Default::default());
                        self.ins(ins);
                    }
                }
            }
            (P::Times, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                let ins = OpMul::new(Register(dst), Register(lhs), Register(rhs))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Div, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                let ins = OpDiv::new(Register(dst), Register(lhs), Register(rhs))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Quotient, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                let ins = OpQuotient::new(Register(dst), Register(lhs), Register(rhs))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Remainder, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                let ins = OpRemainder::new(Register(dst), Register(lhs), Register(rhs))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Modulo, 2) => {
                let lhs = self.operand(args[0], 0);
                let rhs = self.operand(args[1], 1);
                let ins = OpModulo::new(Register(dst), Register(lhs), Register(rhs))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Minus, 1) => {
                let src = self.operand(args[0], 0);
                let ins =
                    OpNegate::new(Register(dst), Register(src)).with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Abs, 1) => {
                let src = self.operand(args[0], 0);
                let ins =
                    OpAbs::new(Register(dst), Register(src)).with_metadata(Default::default());
                self.ins(ins);
            }
            (P::Sqrt, 1) => {
                let src = self.operand(args[0], 0);
                let ins =
                    OpSqrt::new(Register(dst), Register(src)).with_metadata(Default::default());
                self.ins(ins);
            }
            (P::NumericLt, 2) => emit_cmp!(self, OpLess, dst, args),
            (P::NumericLte, 2) => emit_cmp!(self, OpLessEqual, dst, args),
            (P::NumericEqual, 2) => emit_cmp!(self, OpNumberEqual, dst, args),
            (P::NumericGte, 2) => emit_cmp!(self, OpGreaterEqual, dst, args),
            (P::NumericGt, 2) => emit_cmp!(self, OpGreater, dst, args),
            (P::VectorRef, 2) => {
                let vector = self.operand(args[0], 0);
                let index = self.operand(args[1], 1);
                let ins = OpVectorRef::new(Register(dst), Register(vector), Register(index))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::VectorSet, 3) => {
                let vector = self.operand(args[0], 0);
                let index = self.operand(args[1], 1);
                let src = self.operand(args[2], 2);
                let ins = OpVectorSet::new(Register(vector), Register(index), Register(src))
                    .with_metadata(Default::default());
                self.ins(ins);
                self.set_undefined(dst_var);
            }
            (P::VariableRef, 1) => {
                let var = self.operand(args[0], 0);
                let ins = OpVariableRef::new(Register(dst), Register(var))
                    .with_metadata(Default::default());
                self.ins(ins);
            }
            (P::VariableSet, 2) => {
                let var = self.operand(args[0], 0);
                let src = self.operand(args[1], 1);
                self.ins(
                    OpVariableSet {
                        var: Register(var),
                        src: Register(src),
                    }
                    .into(),
                );
                self.set_undefined(dst_var);
            }
            (P::CacheRef, _) | (P::CacheSet, _) => {
                panic!("{prim} reached bytecode lowering unlowered")
            }
            (P::SlotRef, 2) => self.emit_slot_ref(block, index, dst_var, args),
            (P::SlotSet, 3) => self.emit_slot_set(block, index, dst_var, args),
            // Malformed by the time lowering sees it: call the real
            // procedure inline, which raises the arity error itself.
            (P::SlotRef, _) | (P::SlotSet, _) => {
                self.emit_slot_slow_inline(block, index, dst_var, prim, args)
            }
            _ => self.emit_generic_prim(dst, prim, args),
        }
    }

    /// Call frame base above the registers in `live`, the same rule as
    /// `emit_call`: two above the highest live register, so `r[base-1]` is
    /// free. `r1` (the closure) is always live, so the base is at least 3.
    fn call_base(&self, live: &HashSet<UVar>) -> u32 {
        live.iter()
            .filter_map(|var| self.registers.get(var).copied())
            .max()
            .unwrap_or(1)
            + 2
    }

    fn emit_slot_ref(
        &mut self,
        block: BlockId,
        index: usize,
        dst_var: UVar,
        args: &[Operand<'gc>],
    ) {
        let [obj, Operand::Constant(name)] = args else {
            return self.emit_slot_slow_inline(block, index, dst_var, Primitive::SlotRef, args);
        };
        if !name.is::<Symbol>() {
            return self.emit_slot_slow_inline(block, index, dst_var, Primitive::SlotRef, args);
        }
        let dst = self.register(dst_var);
        let obj = self.operand(*obj, 0);
        let name_const = self.value_constant(*name);
        let label_slow = Label::new();
        let label_next = Label::new();
        let ins = OpGetSlot::linkable_with_metadata(
            Register(dst),
            Register(obj),
            Constant(name_const),
            label_slow,
            Default::default(),
        );
        self.branch(ins);
        self.atoms.push(Atom::Label(label_next));

        let mut live = self.live_after(block, index);
        live.remove(&dst_var);
        let base = self.call_base(&live);
        let mask = self.mask_ref(live.into_iter());
        self.slot_stubs.push(SlotStub {
            label_slow,
            label_next,
            get: true,
            dst_var,
            dst,
            obj,
            name: *name,
            src: 0,
            base,
            mask,
        });
    }

    fn emit_slot_set(
        &mut self,
        block: BlockId,
        index: usize,
        dst_var: UVar,
        args: &[Operand<'gc>],
    ) {
        let [obj, Operand::Constant(name), src] = args else {
            return self.emit_slot_slow_inline(block, index, dst_var, Primitive::SlotSet, args);
        };
        if !name.is::<Symbol>() {
            return self.emit_slot_slow_inline(block, index, dst_var, Primitive::SlotSet, args);
        }
        let obj = self.operand(*obj, 0);
        let src = self.operand(*src, 1);
        let name_const = self.value_constant(*name);
        let label_slow = Label::new();
        let label_next = Label::new();
        let ins = OpPutSlot::linkable_with_metadata(
            Register(obj),
            Constant(name_const),
            Register(src),
            label_slow,
            Default::default(),
        );
        self.branch(ins);
        self.atoms.push(Atom::Label(label_next));
        // Both the fast path and the stub (which jumps here) produce no
        // value.
        self.set_undefined(dst_var);
        let mut live = self.live_after(block, index);
        live.remove(&dst_var);
        let base = self.call_base(&live);
        let mask = self.mask_ref(live.into_iter());
        self.slot_stubs.push(SlotStub {
            label_slow,
            label_next,
            get: false,
            dst_var,
            dst: 0,
            obj,
            name: *name,
            src,
            base,
            mask,
        });
    }

    /// An unconditional slow call to the real slot procedure, for shapes
    /// `get_slot`/`put_slot` cannot encode.
    fn emit_slot_slow_inline(
        &mut self,
        block: BlockId,
        index: usize,
        dst_var: UVar,
        prim: Primitive,
        args: &[Operand<'gc>],
    ) {
        let mut live = self.live_after(block, index);
        live.remove(&dst_var);
        let base = self.call_base(&live);
        let mask = self.mask_ref(live.into_iter());
        let proc = if prim == Primitive::SlotSet {
            RuntimeProc::SlotSet
        } else {
            RuntimeProc::SlotRef
        };
        let operands: Vec<Src<'gc>> = args.iter().map(|arg| self.src(*arg)).collect();
        if prim == Primitive::SlotSet {
            self.emit_runtime_call(proc, &operands, base, mask, MV_IGNORE, base + 2);
            self.set_undefined(dst_var);
        } else if self.used.contains(&dst_var) {
            let dst = self.register(dst_var);
            self.emit_runtime_call(proc, &operands, base, mask, MV_ERROR, dst);
        } else {
            self.emit_runtime_call(proc, &operands, base, mask, MV_IGNORE, base + 2);
        }
    }

    /// A call to a runtime procedure with `args` in the frame at `base`,
    /// received with `mv` value semantics into `dst`. `mask` is live across
    /// the call. A dropped value (`MV_IGNORE`) lands in `r[base+2]`, where a
    /// single value arrives anyway.
    fn emit_runtime_call(
        &mut self,
        proc: RuntimeProc,
        args: &[Src<'gc>],
        base: u32,
        mask: PoolReference,
        mv: u32,
        dst: u32,
    ) {
        // The moves go first: a source may sit in `base + 1` (usually a
        // temporary), which the procedure load below overwrites.
        let moves: Vec<(Src<'gc>, u32)> = args
            .iter()
            .enumerate()
            .map(|(index, arg)| (*arg, base + 2 + index as u32))
            .collect();
        let top = base + 2 + args.len() as u32;
        let spare = self.spare_above(top);
        self.bump_extent(top);
        self.parallel_moves(moves, spare);
        self.ins(
            OpRuntimeProcedure {
                dst: Register(base + 1),
                id: Count(proc as u32),
            }
            .into(),
        );
        let ins =
            OpCall::new(Count(args.len() as u32), Register(base)).with_metadata(Default::default());
        self.ins(ins);
        let ins = OpReturnPoint::new(Count(base), mask, Count(mv), Register(dst))
            .with_metadata(Default::default());
        self.ins(ins);
    }

    /// The cold section
    fn emit_slot_stubs(&mut self) {
        self.note_source(Value::new(false));
        for stub in std::mem::take(&mut self.slot_stubs) {
            self.atoms.push(Atom::Label(stub.label_slow));
            if stub.get {
                let (mv, dst) = if self.used.contains(&stub.dst_var) {
                    (MV_ERROR, stub.dst)
                } else {
                    (MV_IGNORE, stub.base + 2)
                };
                self.emit_runtime_call(
                    RuntimeProc::SlotRef,
                    &[Src::Register(stub.obj), Src::Const(stub.name)],
                    stub.base,
                    stub.mask,
                    mv,
                    dst,
                );
            } else {
                self.emit_runtime_call(
                    RuntimeProc::SlotSet,
                    &[
                        Src::Register(stub.obj),
                        Src::Const(stub.name),
                        Src::Register(stub.src),
                    ],
                    stub.base,
                    stub.mask,
                    MV_IGNORE,
                    stub.base + 2,
                );
            }
            self.branch(OpJump::linkable(stub.label_next));
        }
    }

    fn emit_add_immediate(&mut self, dst: u32, operand: Operand<'gc>, value: i32) {
        let src = self.operand(operand, 0);
        let ins = OpAddImmediate::new(Register(dst), Register(src), Immediate(value))
            .with_metadata(Default::default());
        self.ins(ins);
    }

    fn emit_terminator(
        &mut self,
        terminator: &Terminator<'gc>,
        fusion: Option<&FusedBranch<'gc>>,
        next: Option<BlockId>,
    ) {
        match terminator {
            Terminator::Jump { target } => self.goto_block(*target, next),
            Terminator::Return { args, source } => {
                self.note_source(*source);
                self.emit_return(args);
            }
            Terminator::TailCall {
                callee,
                args,
                source,
            } => {
                self.note_source(*source);
                // A direct self call needs no callee move: the callee is `r1`
                // already, and the template reads it from there.
                let direct = self.is_direct_self_call(*callee, args.len());
                let mut moves = if direct {
                    Vec::new()
                } else {
                    vec![(self.src(*callee), 1)]
                };
                for (index, arg) in args.iter().enumerate() {
                    moves.push((self.src(*arg), 2 + index as u32));
                }
                let top = 2 + args.len() as u32;
                self.bump_extent(top);
                let spare = self.spare_above(top);
                self.parallel_moves(moves, spare);
                let ins = if direct {
                    OpTailCallSelf::new(Count(args.len() as u32)).with_metadata(Default::default())
                } else {
                    OpTailCall::new(Count(args.len() as u32)).with_metadata(Default::default())
                };
                self.ins(ins);
            }
            Terminator::Call {
                callee,
                ret,
                args,
                source,
            } => {
                self.note_source(*source);
                self.emit_call(*callee, args, *ret, next);
            }
            Terminator::Raise { kind, args, source } => {
                self.note_source(*source);
                let moves: Vec<_> = args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| (self.src(*arg), 2 + index as u32))
                    .collect();
                let top = 2 + args.len() as u32;
                self.bump_extent(top);
                let spare = self.spare_above(top);
                self.parallel_moves(moves, spare);
                self.ins(
                    OpRaise {
                        kind: Count(kind.code() as u32),
                        count: Count(args.len() as u32),
                    }
                    .into(),
                );
            }
            Terminator::Branch {
                test,
                consequent,
                alternative,
                ..
            } => {
                let cond = match (test, fusion) {
                    (_, Some(fusion)) => self.fused_cond(fusion),
                    (Operand::Constant(value), None) => {
                        // A constant test is a jump.
                        let target = if *value != Value::new(false) {
                            consequent
                        } else {
                            alternative
                        };
                        self.goto(target, next);
                        return;
                    }
                    (Operand::Local(var), None) => Condition::Truthy(self.register(*var)),
                };
                self.emit_branch(cond, consequent, alternative, next);
            }
            Terminator::BranchPrim {
                prim,
                args,
                consequent,
                alternative,
                ..
            } => {
                let cond = match fusion {
                    Some(fusion) => self.fused_cond(fusion),
                    None => self.prim_cond(*prim, args),
                };
                self.emit_branch(cond, consequent, alternative, next);
            }
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => self.emit_switch(*kind, *scrutinee, cases, default, next),
        }
    }

    /// A register above `top` that nothing in the procedure holds a value in.
    fn spare_above(&self, top: u32) -> u32 {
        top.max(self.tmp_base + self.max_tmp)
    }

    fn emit_return(&mut self, args: &[Operand<'gc>]) {
        if let [Operand::Local(var)] = args {
            let register = self.register(*var);
            self.bump_extent(3);
            self.ins(
                OpReturn {
                    src: Register(register),
                }
                .into(),
            );
            return;
        }
        let moves: Vec<_> = args
            .iter()
            .enumerate()
            .map(|(index, arg)| (self.src(*arg), 2 + index as u32))
            .collect();
        let top = 2 + args.len() as u32;
        self.bump_extent(top);
        let spare = self.spare_above(top);
        self.parallel_moves(moves, spare);
        if args.len() == 1 {
            self.ins(OpReturn { src: Register(2) }.into());
        } else {
            self.ins(
                OpReturnValues {
                    count: Count(args.len() as u32),
                }
                .into(),
            );
        }
    }

    /// Whether a call of `callee` with `argc` arguments is a direct self
    /// call: the callee is the procedure's own `binding`, the closure in
    /// `r1`, which the register allocator never reassigns. Both sides of
    /// the arity check are known here, a count the shared `enter` would
    /// reject stays an indirect call, so its arity error travels the
    /// bit-for-bit legacy raise path.
    fn is_direct_self_call(&self, callee: Operand<'gc>, argc: usize) -> bool {
        let Operand::Local(var) = callee else {
            return false;
        };
        if var != self.procedure.binding {
            return false;
        }
        let required = self.procedure.params.len();
        if self.procedure.variadic.is_some() {
            argc >= required
        } else {
            argc == required
        }
    }

    fn emit_call(
        &mut self,
        callee: Operand<'gc>,
        args: &[Operand<'gc>],
        ret: crate::compiler::cfg::ReturnPoint,
        next: Option<BlockId>,
    ) {
        let live: HashSet<UVar> = self.live_in[&ret.block].iter().copied().collect();
        let base = self.call_base(&live);

        // A direct self call needs no callee move: the template stores `r1`
        // into the callee frame itself.
        let direct = self.is_direct_self_call(callee, args.len());
        let mut moves = if direct {
            Vec::new()
        } else {
            vec![(self.src(callee), base + 1)]
        };
        for (index, arg) in args.iter().enumerate() {
            moves.push((self.src(*arg), base + 2 + index as u32));
        }
        let top = base + 2 + args.len() as u32;
        let spare = self.spare_above(top);
        self.bump_extent(top);
        self.parallel_moves(moves, spare);
        let ins = if direct {
            OpCallSelf::new(Count(args.len() as u32), Register(base))
                .with_metadata(Default::default())
        } else {
            OpCall::new(Count(args.len() as u32), Register(base)).with_metadata(Default::default())
        };
        self.ins(ins);

        let block = self.blocks[&ret.block];
        let rest_used = block.instructions.iter().any(|instruction| {
            matches!(instruction, Instruction::ReceiveRest { dst, .. } if self.used.contains(dst))
        });
        let any_used = rest_used
            || block.instructions.iter().any(|instruction| {
                matches!(instruction, Instruction::Receive { dst, .. } if self.used.contains(dst))
            });
        let mask = self.mask_ref(live.into_iter());
        // An effect context takes the single header too: with no fixed values
        // and an ignored rest, every count is accepted either way, so the
        // multiple-value entry and its count store are dead.
        let single =
            (ret.nvals == 1 && !rest_used) || (ret.variadic && ret.nvals == 0 && !any_used);
        // The single value lands directly in the destination of the first
        // used `Receive` of value 0. A dropped value lands in `r[base+2]`,
        // where it arrives anyway.
        let mut bound = base + 2;
        if single {
            let found = block
                .instructions
                .iter()
                .find_map(|instruction| match instruction {
                    Instruction::Receive { dst, index }
                        if *index == 0 && self.used.contains(dst) =>
                    {
                        Some(*dst)
                    }
                    _ => None,
                });
            if let Some(var) = found {
                bound = self.register(var);
            }
        }
        if single {
            // A value context, or an effect context when nothing received
            // is used: then any number of values is fine.
            let mv = if any_used { MV_ERROR } else { MV_IGNORE };
            let ins = OpReturnPoint::new(Count(base), mask, Count(mv), Register(bound))
                .with_metadata(Default::default());
            self.ins(ins);
        } else {
            // Several values, none, or a rest list: one value enters the
            // multiple-value entry right after the header with a count of 1.
            self.ins(
                OpReturnPointForward {
                    frame: Count(base),
                    mask,
                }
                .into(),
            );
            let ins = if ret.variadic {
                OpReturnPointValuesRest {
                    nvals: Count(ret.nvals as u32),
                    frame: Count(base),
                }
                .into()
            } else {
                OpReturnPointValues {
                    nvals: Count(ret.nvals as u32),
                    frame: Count(base),
                }
                .into()
            };
            self.ins(ins);
        }

        // Bind the returned values. Several values sit where the arguments
        // were, a single value is already in its destination.
        let mut receives: Vec<(Src<'gc>, u32)> = Vec::new();
        let mut highest = base + 2;
        // The `Receive` the return point bound is skipped, once.
        let mut skipped = !single;
        for instruction in &block.instructions {
            match instruction {
                Instruction::Receive { dst, index } => {
                    let source = base + 2 + *index as u32;
                    highest = highest.max(source + 1);
                    if self.used.contains(dst) {
                        if single && *index == 0 && !skipped {
                            skipped = true;
                        } else {
                            receives.push((Src::Register(source), self.register(*dst)));
                        }
                    }
                }
                // Extra values nobody looks at: the return point has already
                // checked the count, and there is no list to build.
                Instruction::ReceiveRest { dst, .. } if !self.used.contains(dst) => {}
                Instruction::ReceiveRest { dst, from } => {
                    // The list replaces the count in the slot at `base`.
                    self.ins(
                        OpValuesRest {
                            base: Register(base),
                            from: Count(*from as u32),
                        }
                        .into(),
                    );
                    receives.push((Src::Register(base), self.register(*dst)));
                }
                _ => {}
            }
        }
        self.bump_extent(highest);
        let spare = self.spare_above(highest.max(top));
        self.parallel_moves(receives, spare);
        self.goto_block(ret.block, next);
    }

    fn goto_block(&mut self, block: BlockId, next: Option<BlockId>) {
        if Some(block) != next {
            let label = self.block_label(block);
            self.branch(OpJump::linkable(label));
        }
    }

    fn goto(&mut self, target: &BranchTarget<'gc>, next: Option<BlockId>) {
        match target {
            BranchTarget::Local { block, .. } => self.goto_block(*block, next),
            BranchTarget::Return { args } => self.emit_return(args),
        }
    }

    /// A one-register jump of `kind` on `register` to `label`.
    fn type_jump(
        &mut self,
        kind: BranchKind,
        register: u32,
        label: Label,
    ) -> bc::LinkableInstruction {
        let register = Register(register);
        match kind {
            BranchKind::Null => OpJumpIfNull::linkable(register, label),
            BranchKind::NNull => OpJumpIfNotNull::linkable(register, label),
            BranchKind::Pair => OpJumpIfPair::linkable(register, label),
            BranchKind::NPair => OpJumpIfNotPair::linkable(register, label),
            BranchKind::Fixnum => OpJumpIfFixnum::linkable(register, label),
            BranchKind::NFixnum => OpJumpIfNotFixnum::linkable(register, label),
            BranchKind::Undefined => OpJumpIfUndefined::linkable(register, label),
            BranchKind::NUndefined => OpJumpIfNotUndefined::linkable(register, label),
            _ => unreachable!("two-register branch kind in a type test"),
        }
    }

    /// A two-register jump of `kind` on `a` and `b` to `label`.
    fn cmp_jump(
        &mut self,
        kind: BranchKind,
        a: u32,
        b: u32,
        label: Label,
    ) -> bc::LinkableInstruction {
        let (lhs, rhs) = (Register(a), Register(b));
        match kind {
            BranchKind::Eq => OpJumpIfEq::linkable(lhs, rhs, label),
            BranchKind::Neq => OpJumpIfNotEq::linkable(lhs, rhs, label),
            BranchKind::Lt => {
                OpJumpIfLess::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::Le => {
                OpJumpIfLessEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NumEq => {
                OpJumpIfNumberEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::Ge => {
                OpJumpIfGreaterEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::Gt => {
                OpJumpIfGreater::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NLt => {
                OpJumpIfNotLess::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NLe => {
                OpJumpIfNotLessEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NNumEq => {
                OpJumpIfNotNumberEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NGe => {
                OpJumpIfNotGreaterEqual::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            BranchKind::NGt => {
                OpJumpIfNotGreater::linkable_with_metadata(lhs, rhs, label, Default::default())
            }
            _ => unreachable!("one-register branch kind in a comparison"),
        }
    }

    /// Emit a jump to `label` taken when `cond` is `sense`.
    fn jump_if(&mut self, cond: Condition, sense: bool, label: Label) {
        match cond {
            Condition::Truthy(register) | Condition::Falsy(register) => {
                let truthy = matches!(cond, Condition::Truthy(_)) == sense;
                let ins = if truthy {
                    OpJumpIfTrue::linkable(Register(register), label)
                } else {
                    OpJumpIfFalse::linkable(Register(register), label)
                };
                self.branch(ins);
            }
            Condition::Type { yes, no, register } => {
                let kind = if sense { yes } else { no };
                let ins = self.type_jump(kind, register, label);
                self.branch(ins);
            }
            Condition::Cmp { yes, no, a, b } => {
                let kind = if sense { yes } else { no };
                let ins = self.cmp_jump(kind, a, b, label);
                self.branch(ins);
            }
        }
    }

    fn emit_branch(
        &mut self,
        cond: Condition,
        consequent: &BranchTarget<'gc>,
        alternative: &BranchTarget<'gc>,
        next: Option<BlockId>,
    ) {
        match (consequent, alternative) {
            (BranchTarget::Local { block: yes, .. }, BranchTarget::Local { block: no, .. }) => {
                if Some(*yes) == next {
                    let label = self.block_label(*no);
                    self.jump_if(cond, false, label);
                } else {
                    let label = self.block_label(*yes);
                    self.jump_if(cond, true, label);
                    self.goto_block(*no, next);
                }
            }
            (BranchTarget::Local { block: yes, .. }, BranchTarget::Return { args }) => {
                let label = self.block_label(*yes);
                self.jump_if(cond, true, label);
                self.emit_return(args);
            }
            (BranchTarget::Return { args }, BranchTarget::Local { block: no, .. }) => {
                let label = self.block_label(*no);
                self.jump_if(cond, false, label);
                self.emit_return(args);
            }
            (BranchTarget::Return { args: yes }, BranchTarget::Return { args: no }) => {
                let label = Label::new();
                self.jump_if(cond, false, label);
                self.emit_return(yes);
                self.atoms.push(Atom::Label(label));
                self.emit_return(no);
            }
        }
    }

    /// The test a branch on `prim(args)` jumps on, fused where an
    /// instruction exists and computed into a temporary otherwise.
    fn prim_cond(&mut self, prim: Primitive, args: &[Operand<'gc>]) -> Condition {
        use Primitive as P;
        if let Some((yes, no)) = fused_jump_kinds(prim, args.len()) {
            return match args.len() {
                1 => Condition::Type {
                    yes,
                    no,
                    register: self.operand(args[0], 0),
                },
                2 => Condition::Cmp {
                    yes,
                    no,
                    a: self.operand(args[0], 0),
                    b: self.operand(args[1], 1),
                },
                _ => unreachable!("a fusable prim takes one or two arguments"),
            };
        }
        if (prim, args.len()) == (P::Not, 1) {
            return Condition::Falsy(self.operand(args[0], 0));
        }
        let slot = args.len() as u32;
        let result = self.temp(slot);
        self.emit_generic_prim(result, prim, args);
        Condition::Truthy(result)
    }

    fn fused_cond(&mut self, fusion: &FusedBranch<'gc>) -> Condition {
        let (mut yes, mut no) = fused_jump_kinds(fusion.prim, fusion.args.len())
            .expect("a planned fusion resolves to a fusable prim");
        if !fusion.sense {
            std::mem::swap(&mut yes, &mut no);
        }
        match fusion.args.len() {
            1 => Condition::Type {
                yes,
                no,
                register: self.operand(fusion.args[0], 0),
            },
            2 => Condition::Cmp {
                yes,
                no,
                a: self.operand(fusion.args[0], 0),
                b: self.operand(fusion.args[1], 1),
            },
            _ => unreachable!("a fusable prim takes one or two arguments"),
        }
    }

    fn resolve_branch_test(
        &self,
        block: BlockId,
        operand: Operand<'gc>,
        sense: bool,
    ) -> Option<(Primitive, Vec<Operand<'gc>>, bool, Vec<TestLink>)> {
        let instructions = &self.blocks[&block].instructions;
        let branch = instructions.len();
        let mut links = Vec::new();
        let mut current = operand;
        let mut current_sense = sense;
        let mut end = branch;
        loop {
            let Operand::Local(var) = current else {
                return None;
            };
            let (pos, instruction) = instructions[..end]
                .iter()
                .enumerate()
                .rev()
                .find(|(_, instruction)| instruction.def() == Some(var))?;
            match instruction {
                Instruction::PrimCall { prim, args, .. } => {
                    if (*prim, args.len()) == (Primitive::Not, 1) {
                        links.push(TestLink { pos, var });
                        current = args[0];
                        current_sense = !current_sense;
                        end = pos;
                        continue;
                    }
                    fused_jump_kinds(*prim, args.len())?;
                    let rebound = instructions[pos..branch].iter().any(|instruction| {
                        matches!(instruction.def(), Some(def)
                            if args.contains(&Operand::Local(def)))
                    });
                    if rebound {
                        return None;
                    }
                    links.push(TestLink { pos, var });
                    return Some((*prim, args.clone(), current_sense, links));
                }
                Instruction::Assign {
                    src: Operand::Local(next),
                    ..
                } => {
                    links.push(TestLink { pos, var });
                    current = Operand::Local(*next);
                    end = pos;
                }
                _ => return None,
            }
        }
    }

    /// Whether `var` has exactly one syntactic use in the procedure: that
    /// use is necessarily the fused chain/branch use resolving here, so
    /// the defining link is dead once the branch fuses.
    fn single_use(&self, var: UVar) -> bool {
        let mut uses = 0;
        for block in &self.procedure.blocks {
            for instruction in &block.instructions {
                uses += Self::instruction_uses(instruction)
                    .iter()
                    .filter(|used| **used == var)
                    .count();
                if uses > 1 {
                    return false;
                }
            }
            uses += Self::terminator_uses(&block.terminator)
                .iter()
                .filter(|used| **used == var)
                .count();
            if uses > 1 {
                return false;
            }
        }
        uses == 1
    }

    fn plan_branch_fusion(
        &self,
        block: BlockId,
        terminator: &Terminator<'gc>,
    ) -> Option<FusedBranch<'gc>> {
        if !flags::fuse_branch_cond() {
            return None;
        }
        let (operand, sense) = match terminator {
            Terminator::Branch { test, .. } => (*test, true),
            Terminator::BranchPrim {
                prim: Primitive::Not,
                args,
                ..
            } if args.len() == 1 => (args[0], false),
            _ => return None,
        };
        let (prim, args, sense, links) = self.resolve_branch_test(block, operand, sense)?;
        let positions: Vec<usize> = links.iter().map(|link| link.pos).collect();
        let instructions = &self.blocks[&block].instructions;
        let mut elided = Vec::new();
        let mut reader_gone = true;
        for link in &links {
            let clean = instructions[link.pos + 1..]
                .iter()
                .enumerate()
                .all(|(offset, _)| positions.contains(&(link.pos + 1 + offset)));
            if reader_gone && clean && self.single_use(link.var) {
                elided.push(link.pos);
            } else {
                reader_gone = false;
            }
        }
        Some(FusedBranch {
            prim,
            args,
            sense,
            elided,
        })
    }

    /// Label of a branch target, a `Return` target gets a stub, emitted by
    /// the caller once the instruction that jumps to it is complete.
    fn target_label(
        &mut self,
        target: &BranchTarget<'gc>,
        stubs: &mut Vec<(Label, Vec<Operand<'gc>>)>,
    ) -> Label {
        match target {
            BranchTarget::Local { block, .. } => self.block_label(*block),
            BranchTarget::Return { args } => {
                let label = Label::new();
                stubs.push((label, args.clone()));
                label
            }
        }
    }

    fn emit_switch(
        &mut self,
        kind: SwitchKind,
        scrutinee: Operand<'gc>,
        cases: &[SwitchCase<'gc>],
        default: &BranchTarget<'gc>,
        next: Option<BlockId>,
    ) {
        let mut stubs: Vec<(Label, Vec<Operand<'gc>>)> = Vec::new();
        let default_label = self.target_label(default, &mut stubs);
        let scrutinee_register = self.operand(scrutinee, 0);

        match kind {
            SwitchKind::SymbolEq { mask } => {
                // Symbols with the same masked hash share a bucket, which
                // compares them one by one.
                let mut buckets: Vec<SymbolBucket<'gc>> = Vec::new();
                for case in cases {
                    let SwitchCaseValue::Symbol { hash, value } = case.value else {
                        continue;
                    };
                    let key = hash & mask;
                    let target = self.target_label(&case.target, &mut stubs);
                    match buckets.iter_mut().find(|bucket| bucket.0 == key) {
                        Some(bucket) => bucket.2.push((value, target)),
                        None => {
                            let label = Label::new();
                            buckets.push((key, label, vec![(value, target)]));
                        }
                    }
                }
                buckets.sort_by_key(|bucket| bucket.0);
                let atom = self.atoms.len();
                self.ins(
                    OpSwitchSymbol {
                        src: Register(scrutinee_register),
                        table: PoolReference(0),
                    }
                    .into(),
                );
                self.switch_fixups.push(SwitchFixup::Sym {
                    atom,
                    mask,
                    default: default_label,
                    cases: buckets
                        .iter()
                        .map(|(key, label, _)| (*key, *label))
                        .collect(),
                });
                // Not a symbol.
                self.branch(OpJump::linkable(default_label));
                for (_, label, members) in buckets {
                    self.atoms.push(Atom::Label(label));
                    for (symbol, target) in members {
                        let constant = self.temp(1);
                        self.load_const(constant, symbol);
                        self.branch(OpJumpIfEq::linkable(
                            Register(scrutinee_register),
                            Register(constant),
                            target,
                        ));
                    }
                    self.branch(OpJump::linkable(default_label));
                }
            }
            SwitchKind::Eq
            | SwitchKind::Fixnum
            | SwitchKind::Numeric
            | SwitchKind::Char
            | SwitchKind::CharEq => {
                // The first case with a given key wins.
                let mut table: Vec<(i32, Label)> = Vec::new();
                let mut integer_cases: Vec<(i32, Label)> = Vec::new();
                for case in cases {
                    let SwitchCaseValue::Integer(key) = case.value else {
                        continue;
                    };
                    let target = self.target_label(&case.target, &mut stubs);
                    integer_cases.push((key, target));
                    if !table.iter().any(|(existing, _)| *existing == key) {
                        table.push((key, target));
                    }
                }
                table.sort_by_key(|(key, _)| *key);
                let atom = self.atoms.len();
                if matches!(kind, SwitchKind::Char | SwitchKind::CharEq) {
                    self.ins(
                        OpSwitchChar {
                            src: Register(scrutinee_register),
                            table: PoolReference(0),
                        }
                        .into(),
                    );
                } else {
                    self.ins(
                        OpSwitchFixnum {
                            src: Register(scrutinee_register),
                            table: PoolReference(0),
                        }
                        .into(),
                    );
                }
                self.switch_fixups.push(SwitchFixup::Int {
                    atom,
                    default: default_label,
                    cases: table,
                });
                // The scrutinee has another type.
                match kind {
                    SwitchKind::Fixnum => {
                        if let Some((key, _)) = integer_cases.first() {
                            // Raises for a non-fixnum, as `fx=?` would.
                            let result = self.temp(2);
                            self.emit_generic_prim(
                                result,
                                Primitive::FxEq,
                                &[scrutinee, Operand::Constant(Value::new(*key))],
                            );
                        }
                    }
                    SwitchKind::Numeric => {
                        for (key, target) in &integer_cases {
                            let result = self.temp(2);
                            self.emit_generic_prim(
                                result,
                                Primitive::NumericEqual,
                                &[scrutinee, Operand::Constant(Value::new(*key))],
                            );
                            self.jump_if(Condition::Truthy(result), true, *target);
                        }
                    }
                    SwitchKind::Char => {
                        // Raises for a non-char, as `char->integer` would.
                        let result = self.temp(2);
                        self.emit_generic_prim(result, Primitive::CharToInteger, &[scrutinee]);
                    }
                    _ => {}
                }
                self.branch(OpJump::linkable(default_label));
            }
        }
        for (label, args) in stubs {
            self.atoms.push(Atom::Label(label));
            self.emit_return(&args);
        }
        let _ = next;
    }
}

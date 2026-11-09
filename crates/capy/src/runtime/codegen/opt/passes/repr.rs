//! Representation selection.
//!
//! Flonum code builds `box.double` of every intermediate and `check.double`
//! of every operand. The checks of a boxed value are folded away by
//! [`typeprop`](super::typeprop), but two things keep the boxes alive: frame
//! states that name the boxed register, and block parameters (loop-carried
//! values) that stay tagged. This pass removes both.
//!
//! 1. Typed block parameters. A tagged parameter becomes a raw `Double` (or
//!    `Int32`) when every incoming edge supplies that representation, or the
//!    block guards it. An edge argument is supplied raw when it is the
//!    result of `box.double`, a flonum constant, or another typed parameter.
//!    Any other edge argument gets a `check.double` on the edge, which is
//!    allowed only when the block starts with a guard on the parameter or
//!    every iteration of its loop runs one (see "Edge guards"). The guards
//!    inside the block that this satisfies are dropped. A use that needs a
//!    tagged value (a call argument, a store, a return, an edge to a tagged
//!    parameter) gets a `box.double` at the start of its block.
//!    An unguarded parameter is typed only when none of its uses is tagged.
//!    The selection is a fixed point over the parameters: a parameter is
//!    typed when all of its incoming types agree, and the assumption that a
//!    cycle of parameters is typed is kept unless an edge refutes it.
//!
//! 2. Raw frame states. A frame register that holds `box.double(x)`,
//!    `box.int32(x)` or `box.int64(x)` names `x` instead. The deopt metadata
//!    records the raw location (`DeoptLoc::UnboxedDouble` / `UnboxedI32` /
//!    `UnboxedI64`), and the deopt runtime tags or reboxes the value when it
//!    rebuilds the baseline frame. Boxing a flonum or an int32 never
//!    allocates; an int64 that leaves i32 is reboxed by allocating a one-limb
//!    bignum in the deopt runtime, under the non-collecting slow-path
//!    contract. The boxes that only frames used are then dead.
//!
//! With range facts, an `Int64` candidate parameter is kept raw even
//! when it has a tagged use, provided the range analysis proves every value
//! it can take fits `i32`. The `box.int64` at that use is then a pure tag
//! (it can neither allocate a bignum nor deopt), so the parameter does not
//! have to fall back to the tagged representation.
//!
//! Edge guards. A guard on the edge into block `B` deopts before `B` runs,
//! with the frame state of `B`'s first deopting instruction, whose
//! parameters are replaced by the edge's arguments. That is the state at the
//! start of `B` when nothing before the instruction has an effect. Baseline
//! resumes at that pc and runs the whole block itself, so the only change is
//! that the guard fails earlier than it would have. When the guard sits in a
//! later block of `B`'s loop, it is hoisted only if that block dominates
//! every latch of the loop (every full iteration runs it).

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, SecondaryMap};

use super::simplify::edges_mut;
use super::{PassOutcome, Subst, range, reachable, replace_uses};
use crate::runtime::codegen::opt::ir::frame_state::{FrameState, FrameStateData};
use crate::runtime::codegen::opt::ir::func::{Block, DomTree, Func, Value, ValueData, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Inst, InstData, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::objectmodel::Value as SchemeValue;

/// Whether int32 block parameters are selected too. Frame states take raw
/// int32 either way.
const INT32_PARAMS: bool = false;

/// The representation lattice of a candidate parameter. `Undef` is the
/// optimistic start, `Bot` means the parameter stays tagged.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Ty {
    Undef,
    Int,
    I64,
    Dbl,
    Bot,
}

impl Ty {
    fn repr(self) -> Option<Repr> {
        match self {
            Ty::Int => Some(Repr::Int32),
            Ty::I64 => Some(Repr::Int64),
            Ty::Dbl => Some(Repr::Double),
            _ => None,
        }
    }

    fn ir_type(self) -> Type {
        match self {
            Ty::Int => Type::Int32,
            Ty::I64 => Type::Int64,
            _ => Type::Double,
        }
    }

    fn guard_op(self) -> Op {
        match self {
            Ty::Int => Op::CheckInt32,
            // The general guard: accepts a fixnum and a one-limb bignum, so
            // an edge from an OSR entry can carry either representation.
            Ty::I64 => Op::CheckBigIntFits64,
            _ => Op::CheckDouble,
        }
    }

    fn of_guard(op: Op) -> Option<Ty> {
        match op {
            Op::CheckDouble => Some(Ty::Dbl),
            Op::CheckInt32 if INT32_PARAMS => Some(Ty::Int),
            // `CheckInt64` and `CheckBigIntFits64` mark an int64 parameter.
            Op::CheckInt64 | Op::CheckBigIntFits64 => Some(Ty::I64),
            _ => None,
        }
    }
}

/// How an edge argument reaches a typed parameter.
#[derive(Clone, Copy, Debug)]
enum Cls {
    /// A raw value of this type is at hand: the operand of the `box`, or an
    /// immediate to materialize.
    Raw(Ty, Source),
    /// A candidate parameter that has no type yet.
    Undef,
    /// A tagged value: it needs a guard.
    Unknown,
}

#[derive(Clone, Copy, Debug)]
enum Source {
    Value(Value),
    Imm(Constant),
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = false;
    let mut work = func.insts.len();
    changed |= select_params(func, &mut work);
    changed |= raw_frames(func, &mut work);
    PassOutcome { changed, work }
}

/// The immediate of a constant tagged value.
fn imm_of(bits: u64) -> Option<(Ty, Constant)> {
    let value = SchemeValue::from_raw(bits);
    if let Some(n) = value.int32() {
        return Some(if INT32_PARAMS {
            (Ty::Int, Constant::Int32(n))
        } else {
            (Ty::I64, Constant::Int64(n as i64))
        });
    }
    value
        .flonum()
        .map(|f| (Ty::Dbl, Constant::Double(f.to_bits())))
}

struct Ctx {
    dom: std::rc::Rc<DomTree>,
    live: SecondaryMap<Block, bool>,
    inst_block: SecondaryMap<Inst, Option<Block>>,
    /// The type of each candidate parameter.
    ty: HashMap<Value, Ty>,
    /// The guard type each candidate parameter has.
    guard: HashMap<Value, Ty>,
    /// Parameters that must stay tagged.
    forced: HashMap<Value, ()>,
    /// The incoming edges of each block: the predecessor and the edge index.
    incoming: SecondaryMap<Block, Vec<(Block, usize)>>,
    /// The frame an edge guard into the block uses, if it has one.
    entry_frame: HashMap<Block, Option<FrameState>>,
    /// The `(osr block, header)` pairs. An OSR block carries its own entry
    /// frame, so its edge can be guarded even when the header has none.
    osr_blocks: HashSet<Block>,
}

impl Ctx {
    /// The frame an edge `pred -> block` guard can deopt with, if any.
    fn edge_frame(&self, func: &Func, block: Block, pred: Block) -> Option<FrameState> {
        if self.osr_blocks.contains(&pred) {
            // The OSR block holds a `CheckNotInvalid` carrying the baseline
            // loop-header state, whose values are the block's parameters.
            find_entry_frame(func, &self.dom, &self.inst_block, pred)
        } else {
            self.entry_frame.get(&block).copied().flatten()
        }
    }
    fn classify(&self, func: &Func, arg: Value) -> Cls {
        match func.value_def(arg) {
            ValueDef::Inst(def) => match func.insts[def].op {
                Op::Box(Repr::Double) => Cls::Raw(Ty::Dbl, Source::Value(func.args(def)[0])),
                Op::Box(Repr::Int32) if INT32_PARAMS => {
                    Cls::Raw(Ty::Int, Source::Value(func.args(def)[0]))
                }
                // A boxed int64 carries the raw value it was built from, so a
                // parameter can be kept raw and the box left for an edge or a
                // tagged use.
                Op::Box(Repr::Int64) => Cls::Raw(Ty::I64, Source::Value(func.args(def)[0])),
                Op::Const(Constant::Bits(bits)) => match imm_of(bits) {
                    Some((ty, constant)) => Cls::Raw(ty, Source::Imm(constant)),
                    None => Cls::Unknown,
                },
                _ => Cls::Unknown,
            },
            ValueDef::Param { .. } => match self.ty.get(&arg) {
                Some(Ty::Undef) => Cls::Undef,
                Some(&ty @ (Ty::Int | Ty::I64 | Ty::Dbl)) => Cls::Raw(ty, Source::Value(arg)),
                _ => Cls::Unknown,
            },
        }
    }

    /// The type `param` gets from its guards and incoming edges.
    fn compute(&self, func: &Func, block: Block, index: usize, param: Value) -> Ty {
        if self.forced.contains_key(&param) {
            return Ty::Bot;
        }
        let guard = self.guard.get(&param).copied().unwrap_or(Ty::Undef);
        if guard == Ty::Bot {
            return Ty::Bot;
        }
        let guardable = guard != Ty::Undef;
        let mut cur = guard;
        for &(pred, k) in &self.incoming[block] {
            let Some(term) = &func.blocks[pred].term else {
                return Ty::Bot;
            };
            let edge = term.edges()[k];
            let arg = func.edge_args(&edge)[index];
            match self.classify(func, arg) {
                Cls::Unknown => {
                    if !guardable || self.edge_frame(func, block, pred).is_none() {
                        return Ty::Bot;
                    }
                }
                Cls::Undef => {}
                Cls::Raw(ty, _) => {
                    if cur == Ty::Undef {
                        cur = ty;
                    } else if cur != ty {
                        return Ty::Bot;
                    }
                }
            }
        }
        cur
    }
}

/// Whether the instructions of `block` before its first guard have no
/// effect, and the frame of that guard names only values that exist on an
/// edge into the block.
fn find_entry_frame(
    func: &Func,
    dom: &DomTree,
    inst_block: &SecondaryMap<Inst, Option<Block>>,
    block: Block,
) -> Option<FrameState> {
    let mut frame = None;
    for &inst in &func.blocks[block].insts {
        let data = &func.insts[inst];
        if data.op == Op::Safepoint {
            continue;
        }
        if data.op.can_deopt() {
            frame = data.frame;
            break;
        }
        let effects = data.op.effects();
        if data.op.is_call() || !effects.writes.is_empty() || !effects.causes.is_empty() {
            return None;
        }
    }
    let frame = frame?;
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        if data.returned.is_some() {
            return None;
        }
        let ok = |v: Value| -> bool {
            let def_block = match func.value_def(v) {
                ValueDef::Param { block: d, .. } => d,
                ValueDef::Inst(i) => match inst_block[i] {
                    Some(d) => d,
                    None => return false,
                },
            };
            match func.value_def(v) {
                ValueDef::Param { .. } if def_block == block => true,
                _ => def_block != block && dom.dominates(def_block, block),
            }
        };
        if !ok(data.closure) || !data.registers.iter().all(|&(_, v)| ok(v)) {
            return None;
        }
        cur = data.parent;
    }
    Some(frame)
}

fn select_params(func: &mut Func, work: &mut usize) -> bool {
    let dom = func.dom_tree();
    let live = reachable(func);
    let rpo = dom.rpo().to_vec();
    // Natural loops, for the exit-only tagged-use rule on i64 parameters.
    let loops = super::loops::find_loops(func, &dom);

    let mut inst_block: SecondaryMap<Inst, Option<Block>> = SecondaryMap::new();
    for (block, data) in func.blocks.iter() {
        for &inst in &data.insts {
            inst_block[inst] = Some(block);
        }
    }
    let mut incoming: SecondaryMap<Block, Vec<(Block, usize)>> = SecondaryMap::new();
    let mut latches: SecondaryMap<Block, Vec<Block>> = SecondaryMap::new();
    for &x in &rpo {
        if let Some(term) = &func.blocks[x].term {
            for (k, edge) in term.edges().iter().enumerate() {
                incoming[edge.block].push((x, k));
                if dom.dominates(edge.block, x) {
                    latches[edge.block].push(x);
                }
            }
        }
    }

    // Candidates and their guards.
    let mut ty: HashMap<Value, Ty> = HashMap::new();
    let mut guard: HashMap<Value, Ty> = HashMap::new();
    let mut order: Vec<(Block, usize, Value)> = Vec::new();
    for &block in &rpo {
        if block == func.entry {
            continue;
        }
        for (index, &param) in func.params(block).iter().enumerate() {
            if func.value_type(param) == Type::Any && !incoming[block].is_empty() {
                ty.insert(param, Ty::Undef);
                order.push((block, index, param));
            }
        }
    }
    if order.is_empty() {
        return false;
    }

    let range_analysis = range::analyze(func);
    for &g in &rpo {
        for &inst in &func.blocks[g].insts {
            *work += 1;
            let Some(gty) = Ty::of_guard(func.insts[inst].op) else {
                continue;
            };
            let arg = func.args(inst)[0];
            let ValueDef::Param { block: b, .. } = func.value_def(arg) else {
                continue;
            };
            if !ty.contains_key(&arg) {
                continue;
            }
            let eligible = b == g
                || (!latches[b].is_empty()
                    && dom.dominates(b, g)
                    && latches[b].iter().all(|&l| dom.dominates(g, l)));
            if !eligible {
                continue;
            }
            let entry = guard.entry(arg).or_insert(gty);
            if *entry != gty {
                *entry = Ty::Bot;
            }
        }
    }
    // A guard on a block parameter demands the same type of a parameter that
    // feeds it along an unconditional edge (a loop preheader). The demand
    // reaches the loop's entries, where an OSR block can carry the guard
    // frame, so a loop entered through a preheader keeps its parameter raw.
    loop {
        let mut changed = false;
        for &(block, index, param) in &order {
            let Some(gty) = guard.get(&param).copied() else {
                continue;
            };
            if matches!(gty, Ty::Undef | Ty::Bot) {
                continue;
            }
            for &(pred, k) in &incoming[block] {
                let Some(term) = &func.blocks[pred].term else {
                    continue;
                };
                if !matches!(term, Terminator::Jump(_)) {
                    continue;
                }
                let Some(&arg) = func.edge_args(&term.edges()[k]).get(index) else {
                    continue;
                };
                if !ty.contains_key(&arg) {
                    continue;
                }
                match guard.get(&arg).copied() {
                    // `Bot` is the top of the lattice: re-inserting it would
                    // report a change on every pass and never reach a fixed
                    // point.
                    Some(existing) if existing == gty || existing == Ty::Bot => {}
                    Some(_) => {
                        guard.insert(arg, Ty::Bot);
                        changed = true;
                    }
                    None => {
                        guard.insert(arg, gty);
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    let mut entry_frame: HashMap<Block, Option<FrameState>> = HashMap::new();
    for &(block, _, param) in &order {
        if guard.contains_key(&param) && !entry_frame.contains_key(&block) {
            entry_frame.insert(block, find_entry_frame(func, &dom, &inst_block, block));
        }
    }
    let mut cx = Ctx {
        dom,
        live,
        inst_block,
        ty,
        guard,
        forced: HashMap::new(),
        incoming,
        entry_frame,
        osr_blocks: func.osr.iter().map(|entry| entry.block).collect(),
    };

    // The fixed point, with the profitability rule for unguarded parameters.
    loop {
        loop {
            let mut changed = false;
            for &(block, index, param) in &order {
                let new = cx.compute(func, block, index, param);
                *work += cx.incoming[block].len() + 1;
                if cx.ty[&param] != new {
                    cx.ty.insert(param, new);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        // A parameter that is still Undef is a cycle that nothing types.
        let mut forced_any = false;
        for &(_, _, param) in &order {
            if cx.ty[&param] == Ty::Undef && !cx.forced.contains_key(&param) {
                cx.forced.insert(param, ());
                forced_any = true;
            }
        }
        let tagged = tagged_uses(func, &cx);
        for &(block, _, param) in &order {
            let t = cx.ty[&param];
            // An int64 parameter whose value is needed as a Scheme word would
            // need a `box.int64` at the use. That box can allocate, so keep
            // the parameter tagged when the use runs inside its loop. When
            // every tagged use is outside the loop the box runs at most once
            // per loop exit, so the raw loop-carried value is still a win
            // (an accumulator returned after the loop, say). The range
            // analysis keeps the old exemption: when every value fits `i32`
            // the box is a pure tag. Int32 and Double box without allocating,
            // so they keep the old rule.
            let tagged_use = tagged.contains_key(&param);
            let i64_fits = t == Ty::I64 && range_analysis.fits_i32(block, param);
            // A loop-carried i64 whose tagged uses are all outside the loop.
            let i64_exit_only = t == Ty::I64
                && loops
                    .iter()
                    .find(|lp| lp.header == block)
                    .is_some_and(|lp| {
                        tagged
                            .get(&param)
                            .is_some_and(|uses| uses.iter().all(|&use_block| !lp.member[use_block]))
                    });
            let force = matches!(t, Ty::Int | Ty::Dbl | Ty::I64)
                && tagged_use
                && match t {
                    Ty::I64 => !i64_fits && !i64_exit_only,
                    _ => !cx.guard.contains_key(&param),
                }
                && !cx.forced.contains_key(&param);
            if force {
                cx.forced.insert(param, ());
                forced_any = true;
            }
        }
        if !forced_any {
            break;
        }
    }

    let selected: Vec<(Block, usize, Value, Ty)> = order
        .iter()
        .filter_map(|&(b, i, p)| {
            let t = cx.ty[&p];
            matches!(t, Ty::Int | Ty::I64 | Ty::Dbl).then_some((b, i, p, t))
        })
        .collect();
    if selected.is_empty() {
        return false;
    }
    drop(range_analysis);
    apply(func, &mut cx, &selected, work);
    true
}

/// The candidate parameters that have a use needing a tagged value, with the
/// block of each such use.
fn tagged_uses(func: &Func, cx: &Ctx) -> HashMap<Value, Vec<Block>> {
    let typed = |v: Value| matches!(cx.ty.get(&v), Some(Ty::Int | Ty::I64 | Ty::Dbl));
    let mut out: HashMap<Value, Vec<Block>> = HashMap::new();
    for (block, data) in func.blocks.iter() {
        if !cx.live[block] {
            continue;
        }
        for &inst in &data.insts {
            let op = func.insts[inst].op;
            for &arg in func.args(inst) {
                if !typed(arg) {
                    continue;
                }
                if Ty::of_guard(op) == cx.ty.get(&arg).copied() {
                    continue;
                }
                // The wide lane's operand probes are collapsed for a raw i64
                // parameter (`apply`), so they do not need a tagged value.
                if matches!(op, Op::TestInt64 | Op::CheckBigIntFits64)
                    && cx.ty.get(&arg) == Some(&Ty::I64)
                {
                    continue;
                }
                // A raw i64 operand needs no box: the int64 unbox is the
                // operand itself, and the int32 lane truncates it.
                if matches!(op, Op::Unbox(Repr::Int32) | Op::Unbox(Repr::Int64))
                    && cx.ty.get(&arg) == Some(&Ty::I64)
                {
                    continue;
                }
                // The raw-index bounds check consumes the i64 directly.
                if op == Op::CheckIndex && cx.ty.get(&arg) == Some(&Ty::I64) {
                    continue;
                }
                out.entry(arg).or_default().push(block);
            }
        }
        let Some(term) = &data.term else { continue };
        let mut plain: Vec<Value> = Vec::new();
        match term {
            Terminator::Branch { cond, .. } => plain.push(*cond),
            Terminator::Switch { value, .. } => plain.push(*value),
            Terminator::Return(v) => plain.push(*v),
            Terminator::ReturnMany { values, .. } => plain.extend_from_slice(values),
            Terminator::TailCall { callee, args, .. } => {
                plain.push(*callee);
                plain.extend_from_slice(args.as_slice(&func.pool));
            }
            _ => {}
        }
        for v in plain {
            if typed(v) {
                out.entry(v).or_default().push(block);
            }
        }
        for edge in term.edges() {
            let params = func.params(edge.block);
            for (j, &arg) in func.edge_args(&edge).iter().enumerate() {
                if !typed(arg) {
                    continue;
                }
                let same = params
                    .get(j)
                    .is_some_and(|p| cx.ty.get(p) == cx.ty.get(&arg));
                if !same {
                    // The box runs on the edge, so the use is "in" the
                    // block the edge enters: an edge leaving a loop is an
                    // exit use, however deep in the loop its source is.
                    out.entry(arg).or_default().push(edge.block);
                }
            }
        }
    }
    out
}

fn subst_frame(func: &mut Func, frame: FrameState, map: &HashMap<Value, Value>) -> FrameState {
    let data = func.frames[frame].clone();
    let parent = data.parent.map(|p| subst_frame(func, p, map));
    let new = FrameStateData {
        closure: map.get(&data.closure).copied().unwrap_or(data.closure),
        registers: data
            .registers
            .iter()
            .map(|&(r, v)| (r, map.get(&v).copied().unwrap_or(v)))
            .collect(),
        parent,
        ..data
    };
    func.push_frame(new)
}

/// Materializes a source of type `ty` in `block`.
fn materialize(func: &mut Func, block: Block, ty: Ty, source: Source) -> Value {
    match source {
        Source::Value(v) => v,
        Source::Imm(constant) => func
            .push_inst(block, Op::Const(constant), &[], Some(ty.ir_type()), None)
            .expect("a constant has a result"),
    }
}

fn apply(func: &mut Func, cx: &mut Ctx, selected: &[(Block, usize, Value, Ty)], work: &mut usize) {
    // Unreachable blocks that exist now (the blocks made below are live).
    let dead: Vec<Block> = func.blocks.keys().filter(|&b| !cx.live[b]).collect();
    // 1. The edges. Edge arguments become raw; a tagged one gets a guard.
    let mut by_block: Vec<(Block, Vec<(usize, Value, Ty)>)> = Vec::new();
    for &(b, i, p, t) in selected {
        match by_block.last_mut() {
            Some((last, list)) if *last == b => list.push((i, p, t)),
            _ => by_block.push((b, vec![(i, p, t)])),
        }
    }
    for (block, sel) in &by_block {
        let block = *block;
        let params: Vec<Value> = func.params(block).to_vec();
        let incoming = cx.incoming[block].clone();
        for (pred, k) in incoming {
            let edge = func.blocks[pred].term.as_ref().expect("terminator").edges()[k];
            let orig: Vec<Value> = func.edge_args(&edge).to_vec();
            *work += orig.len();
            let classes: Vec<Cls> = sel
                .iter()
                .map(|&(i, _, _)| cx.classify(func, orig[i]))
                .collect();
            let needs_guard = classes.iter().any(|c| matches!(c, Cls::Unknown));
            let jump = matches!(func.blocks[pred].term, Some(Terminator::Jump(_)));
            let mut site = pred;
            let mut split = None;
            if needs_guard && !jump {
                let n = func.new_block();
                let to_b = func.edge(block, &[]);
                func.set_terminator(n, Terminator::Jump(to_b));
                {
                    let mut term = func.blocks[pred].term.take().expect("terminator");
                    let mut edges = edges_mut(&mut term);
                    edges[k].block = n;
                    edges[k].args = EntityList::new();
                    func.blocks[pred].term = Some(term);
                }
                site = n;
                split = Some(n);
            }
            let mut new_args = orig.clone();
            let mut map: Option<HashMap<Value, Value>> = None;
            for (&(i, _, t), class) in sel.iter().zip(&classes) {
                new_args[i] = match *class {
                    Cls::Raw(rt, source) => {
                        debug_assert_eq!(rt, t);
                        materialize(func, site, t, source)
                    }
                    Cls::Undef => orig[i],
                    Cls::Unknown => {
                        let frame = cx
                            .edge_frame(func, block, pred)
                            .expect("a guarded edge has a frame");
                        let map = map.get_or_insert_with(|| {
                            params.iter().copied().zip(orig.iter().copied()).collect()
                        });
                        let frame = subst_frame(func, frame, map);
                        func.push_inst(
                            site,
                            t.guard_op(),
                            &[orig[i]],
                            Some(t.ir_type()),
                            Some(frame),
                        )
                        .expect("a guard has a result")
                    }
                };
            }
            let args = EntityList::from_slice(&new_args, &mut func.pool);
            let mut term = func.blocks[site].term.take().expect("terminator");
            match split {
                Some(_) => {
                    if let Terminator::Jump(e) = &mut term {
                        e.args = args;
                    }
                }
                None => edges_mut(&mut term)[k].args = args,
            }
            func.blocks[site].term = Some(term);
        }
    }
    // Edges from unreachable blocks: any value of the right type will do.
    for x in dead {
        let Some(mut term) = func.blocks[x].term.take() else {
            continue;
        };
        let mut fixes: Vec<(usize, Vec<(usize, Ty)>)> = Vec::new();
        for (k, edge) in term.edges().iter().enumerate() {
            let list: Vec<(usize, Ty)> = by_block
                .iter()
                .find(|(b, _)| *b == edge.block)
                .map(|(_, sel)| sel.iter().map(|&(i, _, t)| (i, t)).collect())
                .unwrap_or_default();
            if !list.is_empty() {
                fixes.push((k, list));
            }
        }
        for (k, list) in fixes {
            let mut args: Vec<Value> = func.edge_args(&term.edges()[k]).to_vec();
            for (i, t) in list {
                let constant = match t {
                    Ty::Int => Constant::Int32(0),
                    Ty::I64 => Constant::Int64(0),
                    _ => Constant::Double(0),
                };
                args[i] = func
                    .push_inst(x, Op::Const(constant), &[], Some(t.ir_type()), None)
                    .expect("a constant has a result");
            }
            edges_mut(&mut term)[k].args = EntityList::from_slice(&args, &mut func.pool);
        }
        func.blocks[x].term = Some(term);
    }

    // 2. Retype the parameters.
    let mut sel_ty: HashMap<Value, Ty> = HashMap::new();
    for &(_, _, p, t) in selected {
        func.values[p].ty = t.ir_type();
        sel_ty.insert(p, t);
    }

    // 3. The uses. A guard on a typed parameter is satisfied; every other use
    // that needs a tagged value reads a box made at the start of its block.
    let mut subst = Subst::default();
    let mut dead_guards: HashSet<Inst> = HashSet::new();
    let blocks: Vec<Block> = func.blocks.keys().collect();
    for &block in &blocks {
        let mut boxed: HashMap<Value, Value> = HashMap::new();
        let mut new_boxes: Vec<Inst> = Vec::new();
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            *work += 1;
            let op = func.insts[inst].op;
            let args: Vec<Value> = func.args(inst).to_vec();
            if let (Some(t), Some(result)) = (Ty::of_guard(op), func.insts[inst].result) {
                if sel_ty.get(&args[0]) == Some(&t) {
                    subst.add(result, args[0]);
                    dead_guards.insert(inst);
                    continue;
                }
            }
            // A raw i64 value is an exact integer: the wide lane's operand
            // probe is always true and its bignum-fit check always passes.
            // Collapse them instead of boxing the parameter on every
            // iteration (the box allocates a bignum once the value leaves
            // i32, which is exactly the accumulator case).
            if op == Op::TestInt64 && args.len() == 1 && sel_ty.get(&args[0]) == Some(&Ty::I64) {
                let data = &mut func.insts[inst];
                data.op = Op::Const(Constant::Bits(1));
                data.args = EntityList::new();
                data.frame = None;
                continue;
            }
            if op == Op::CheckBigIntFits64
                && args.len() == 1
                && sel_ty.get(&args[0]) == Some(&Ty::I64)
            {
                if let Some(result) = func.insts[inst].result {
                    subst.add(result, args[0]);
                    dead_guards.insert(inst);
                    continue;
                }
            }
            if op == Op::Unbox(Repr::Int32)
                && args.len() == 1
                && sel_ty.get(&args[0]) == Some(&Ty::I64)
            {
                let data = &mut func.insts[inst];
                data.op = Op::Int64ToInt32;
                data.frame = None;
                continue;
            }
            if op == Op::Unbox(Repr::Int64)
                && args.len() == 1
                && sel_ty.get(&args[0]) == Some(&Ty::I64)
            {
                if let Some(result) = func.insts[inst].result {
                    subst.add(result, args[0]);
                    dead_guards.insert(inst);
                }
                continue;
            }
            if op == Op::CheckIndex && args.len() == 2 && sel_ty.get(&args[1]) == Some(&Ty::I64) {
                // The bounds check takes the raw i64 and truncates it itself.
                func.insts[inst].op = Op::CheckIndexRaw;
                continue;
            }
            if !args.iter().any(|a| sel_ty.contains_key(a)) {
                continue;
            }
            let new: Vec<Value> = args
                .iter()
                .map(|&a| box_use(func, &sel_ty, &mut boxed, &mut new_boxes, block, a))
                .collect();
            func.insts[inst].args = EntityList::from_slice(&new, &mut func.pool);
        }
        if let Some(mut term) = func.blocks[block].term.take() {
            let mut map = |func: &mut Func, v: Value| {
                box_use(func, &sel_ty, &mut boxed, &mut new_boxes, block, v)
            };
            match &mut term {
                Terminator::Branch { cond, .. } => *cond = map(func, *cond),
                Terminator::Switch { value, .. } => *value = map(func, *value),
                Terminator::Return(v) => *v = map(func, *v),
                Terminator::ReturnMany { values, .. } => {
                    for v in values.iter_mut() {
                        *v = map(func, *v);
                    }
                }
                Terminator::TailCall { callee, args, .. } => {
                    *callee = map(func, *callee);
                    let old: Vec<Value> = args.as_slice(&func.pool).to_vec();
                    let new: Vec<Value> = old.iter().map(|&a| map(func, a)).collect();
                    *args = EntityList::from_slice(&new, &mut func.pool);
                }
                _ => {}
            }
            let n_edges = term.edges().len();
            for k in 0..n_edges {
                let edge = term.edges()[k];
                let target_params: Vec<Value> = func.params(edge.block).to_vec();
                let old: Vec<Value> = func.edge_args(&edge).to_vec();
                let raw_ok = |func: &Func, j: usize, a: Value| {
                    sel_ty.contains_key(&a)
                        && target_params
                            .get(j)
                            .is_some_and(|&p| func.value_type(p) == func.value_type(a))
                };
                let needs_box = old
                    .iter()
                    .enumerate()
                    .any(|(j, &a)| sel_ty.contains_key(&a) && !raw_ok(func, j, a));
                if needs_box && n_edges > 1 {
                    // Box on the edge, so a loop's exit path pays for the
                    // tag instead of every iteration.
                    let n = func.new_block();
                    let new: Vec<Value> = old
                        .iter()
                        .enumerate()
                        .map(|(j, &a)| {
                            if raw_ok(func, j, a) || !sel_ty.contains_key(&a) {
                                a
                            } else {
                                make_box(func, &sel_ty, n, a)
                            }
                        })
                        .collect();
                    let to = func.edge(edge.block, &new);
                    func.set_terminator(n, Terminator::Jump(to));
                    let mut edges = edges_mut(&mut term);
                    edges[k].block = n;
                    edges[k].args = EntityList::new();
                    continue;
                }
                let mut changed = false;
                let new: Vec<Value> = old
                    .iter()
                    .enumerate()
                    .map(|(j, &a)| {
                        if raw_ok(func, j, a) || !sel_ty.contains_key(&a) {
                            a
                        } else {
                            let b = map(func, a);
                            changed |= b != a;
                            b
                        }
                    })
                    .collect();
                if changed {
                    edges_mut(&mut term)[k].args = EntityList::from_slice(&new, &mut func.pool);
                }
            }
            func.blocks[block].term = Some(term);
        }
        if !new_boxes.is_empty() {
            let mut insts = new_boxes;
            insts.extend(func.blocks[block].insts.iter().copied());
            func.blocks[block].insts = insts;
        }
    }
    for (_, data) in func.blocks.iter_mut() {
        data.insts.retain(|i| !dead_guards.contains(i));
    }
    replace_uses(func, &subst);
}

/// Appends a `box` of the typed parameter `v` to `block` and returns it.
fn make_box(func: &mut Func, sel_ty: &HashMap<Value, Ty>, block: Block, v: Value) -> Value {
    let t = sel_ty[&v];
    func.push_inst(
        block,
        Op::Box(t.repr().expect("typed")),
        &[v],
        Some(Type::Any),
        None,
    )
    .expect("a box has a result")
}

/// The tagged value that a use of `v` in `block` reads: `v` itself unless it
/// is a typed parameter, which is boxed once per block.
fn box_use(
    func: &mut Func,
    sel_ty: &HashMap<Value, Ty>,
    boxed: &mut HashMap<Value, Value>,
    new_boxes: &mut Vec<Inst>,
    _block: Block,
    v: Value,
) -> Value {
    let Some(&t) = sel_ty.get(&v) else {
        return v;
    };
    if let Some(&b) = boxed.get(&v) {
        return b;
    }
    let inst = func.insts.next_key();
    let list = EntityList::from_slice(&[v], &mut func.pool);
    let b = func.values.push(ValueData {
        ty: Type::Any,
        def: ValueDef::Inst(inst),
    });
    let pushed = func.insts.push(InstData {
        op: Op::Box(t.repr().expect("typed")),
        args: list,
        result: Some(b),
        frame: None,
    });
    debug_assert_eq!(pushed, inst);
    new_boxes.push(inst);
    boxed.insert(v, b);
    b
}

/// Frame registers that hold `box(x)` name `x`.
fn raw_frames(func: &mut Func, work: &mut usize) -> bool {
    let mut changed = false;
    let ids: Vec<FrameState> = func.frames.keys().collect();
    for fs in ids {
        let regs = func.frames[fs].registers.clone();
        let mut new = regs.clone();
        for (_, value) in new.iter_mut() {
            *work += 1;
            if let ValueDef::Inst(def) = func.value_def(*value) {
                if matches!(func.insts[def].op, Op::Box(_)) {
                    *value = func.args(def)[0];
                }
            }
        }
        if new != regs {
            func.frames[fs].registers = new;
            changed = true;
        }
    }
    changed
}

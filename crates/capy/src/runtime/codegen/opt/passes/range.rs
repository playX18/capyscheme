//! Forward interval/Range analysis and the rewrites its facts prove.
//!
//! The analysis is a forward dataflow over the CFG with a simple interval
//! lattice per SSA value:
//!
//! - [`State::Bottom`]: no value reaches here (an unreachable path, or an
//!   operation that always deopts);
//! - [`State::Range`]: the value is an integer in `[lo, hi]`. For an unboxed
//!   `Int32`/`Int64` value it is the raw number; for a tagged `Any` value it
//!   additionally means the value is certainly a fixnum with that payload;
//! - [`State::Top`]: no information.
//!
//! Block parameters are the only path-dependent values, so the dataflow state
//! is a map from a block's entry to the range of each parameter and of every
//! value a branch condition refines. Instructions are not part of the state:
//! their value follows from their operands ([`Analysis::transfer`]), and the
//! use site supplies the current block's entry facts. A loop header's
//! parameters are widened (`hi` to `i64::MAX`, `lo` to `i64::MIN`) on the
//! second growth, so a counter such as `i = i + 1` reaches a fixed point.
//!
//! Guards and unboxing chain to one numeric value: `check.int64(x)`,
//! `unbox.int32(x)` and a tagged `x` are canonicalized to the same key (see
//! [`canon`]), and the guard's own range (`a fixnum payload fits `i32``) is
//! reapplied on the way out. That is what lets a branch `check.int64(i) < n`
//! refine the tagged `i` that a later `check.index` reads.
//!
//! Transfer functions cover constants, add/sub/mul (with i128 bounds so the
//! checked forms can prove that no overflow occurs), quotient/remainder/
//! modulo (with constant divisors), comparisons, `check.int32`/`check.int64`/
//! `check.index`, and branches. The branch edge into a block refines both
//! comparison operands, intersected with what each side already knows.
//!
//! Consumers:
//!
//! - this pass drops guards proved to succeed (`check.int32`/`check.int64` to
//!   `unbox`, and `check.index` to `unbox.int32` when the index is a proven
//!   fixnum in `[0, length)`), folds branches whose condition the ranges make
//!   true or false, folds `box.bool` of such a comparison, and rewrites
//!   `add/sub/mul.i64.ovf` to the total operation when the bounds prove no
//!   overflow (the total op needs no frame);
//! - [`bce`](super::bce) asks the analysis for an in-bounds index range, which
//!   proves bounds checks whose vector length is known (for example an
//!   `alloc.vector` of constant length);
//! - [`repr`](super::repr) keeps an `Int64` block parameter raw even when a
//!   tagged use eventually boxes it, once the parameter's range proves that
//!   `box.int64` can neither allocate nor deopt.

use std::collections::{HashMap, HashSet};

use cranelift_entity::SecondaryMap;

use super::PassOutcome;
use super::forward::def_inst;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Cmp, Constant, Edge, Inst, Op, Repr, Terminator};
use crate::runtime::codegen::opt::ir::types::Type;
use crate::runtime::objectmodel::Value as SchemeValue;

/// A value's range in the dataflow lattice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum State {
    /// No value reaches this point.
    Bottom,
    /// An integer in `[lo, hi]`. For a tagged value this also means "is a
    /// fixnum"; the payload fits `i32`.
    Range { lo: i64, hi: i64 },
    /// No information.
    Top,
}

impl State {
    /// The fixnum payload range. A `check.int32`/`check.int64` result is
    /// always in it, and so is a tagged value this domain calls a fixnum.
    const FIXNUM: State = State::Range {
        lo: i32::MIN as i64,
        hi: i32::MAX as i64,
    };
    const I64: State = State::Range {
        lo: i64::MIN,
        hi: i64::MAX,
    };

    /// A range, or [`State::Bottom`] when the bounds are inconsistent.
    fn range(lo: i64, hi: i64) -> State {
        if lo <= hi {
            State::Range { lo, hi }
        } else {
            State::Bottom
        }
    }

    /// The interval, if this state has one.
    fn bounds(self) -> Option<(i64, i64)> {
        match self {
            State::Range { lo, hi } => Some((lo, hi)),
            _ => None,
        }
    }

    /// The union of two states.
    fn join(self, other: State) -> State {
        match (self, other) {
            (State::Bottom, x) | (x, State::Bottom) => x,
            (State::Top, _) | (_, State::Top) => State::Top,
            (State::Range { lo: a, hi: b }, State::Range { lo: c, hi: d }) => {
                State::range(a.min(c), b.max(d))
            }
        }
    }

    /// The intersection of two states.
    fn intersect(self, other: State) -> State {
        match (self, other) {
            (State::Bottom, _) | (_, State::Bottom) => State::Bottom,
            (State::Top, x) | (x, State::Top) => x,
            (State::Range { lo: a, hi: b }, State::Range { lo: c, hi: d }) => {
                State::range(a.max(c), b.min(d))
            }
        }
    }
}

/// A per-query memo of interval results. .
type Memo = HashMap<(Block, Value), State>;

/// The type's full integer range, for raw `Int32`/`Int64` values.
fn type_state(ty: Type) -> Option<State> {
    match ty {
        Type::Int32 => Some(State::FIXNUM),
        Type::Int64 => Some(State::I64),
        _ => None,
    }
}

/// Clamps a state to the value's own type. For an `Any` value a range means
/// "a fixnum", whose payload is an `i32`.
fn clamp_for_type(ty: Type, state: State) -> State {
    match ty {
        Type::Int32 => state.intersect(State::FIXNUM),
        Type::Int64 => state.intersect(State::I64),
        // For a tagged value a range already means "a fixnum", so only a
        // range is clamped. `Top` must stay `Top`: an unknown Scheme value
        // is not a fixnum just because it might be an integer.
        Type::Any => match state {
            State::Range { .. } => state.intersect(State::FIXNUM),
            other => other,
        },
        _ => state,
    }
}

/// The canonical numeric value: guards and unboxing do not change the number.
fn canon(func: &Func, mut value: Value) -> Value {
    for _ in 0..4096 {
        let Some(inst) = def_inst(func, value) else {
            break;
        };
        match func.insts[inst].op {
            Op::CheckInt32 | Op::CheckInt64 | Op::Unbox(Repr::Int32) | Op::Unbox(Repr::Int64) => {
                value = func.args(inst)[0]
            }
            _ => break,
        }
    }
    value
}

/// The object a value names, looking through class checks.
fn object_of(func: &Func, mut value: Value) -> Value {
    for _ in 0..4096 {
        let Some(inst) = def_inst(func, value) else {
            break;
        };
        match func.insts[inst].op {
            Op::CheckClass(_) => value = func.args(inst)[0],
            _ => break,
        }
    }
    value
}

/// The fixed point of the interval dataflow.
pub(super) struct Analysis<'a> {
    func: &'a Func<'a>,
    /// Block entry facts. The keys are canonical values: the block's
    /// parameters and the values an incoming branch refines.
    entries: SecondaryMap<Block, HashMap<Value, State>>,
    /// The reachable predecessors of each block and the edge index.
    incoming: SecondaryMap<Block, Vec<(Block, usize)>>,
    /// Reachable blocks in reverse postorder.
    rpo: Vec<Block>,
}

/// Computes the interval fixed point for `func`.
pub(super) fn analyze<'a>(func: &'a Func<'a>) -> Analysis<'a> {
    let rpo = func.reverse_postorder();
    let mut reachable: SecondaryMap<Block, bool> = SecondaryMap::new();
    for &block in &rpo {
        reachable[block] = true;
    }

    let mut incoming: SecondaryMap<Block, Vec<(Block, usize)>> = SecondaryMap::new();
    for &pred in &rpo {
        let Some(term) = &func.blocks[pred].term else {
            continue;
        };
        for (k, edge) in term.edges().iter().enumerate() {
            if reachable[edge.block] {
                incoming[edge.block].push((pred, k));
            }
        }
    }

    let mut entries: SecondaryMap<Block, HashMap<Value, State>> = SecondaryMap::new();
    for &param in func.params(func.entry) {
        let state = type_state(func.value_type(param)).unwrap_or(State::Top);
        entries[func.entry].insert(param, state);
    }

    let mut analysis = Analysis {
        func,
        entries,
        incoming,
        rpo,
    };

    // Gauss-Seidel over reverse postorder: predecessors are usually updated
    // before their successors, so acyclic code converges in one sweep. Only
    // parameters are widened
    let mut iterations = 0;
    loop {
        let mut changed = false;
        let blocks = analysis.rpo.clone();
        for &block in &blocks {
            if block == analysis.func.entry {
                continue;
            }
            let computed = analysis.block_entry(block);
            let old = analysis.entries[block].clone();
            let merged = analysis.merge_entry(block, &old, computed);
            if merged != old {
                analysis.entries[block] = merged;
                changed = true;
            }
        }
        iterations += 1;
        if !changed || iterations >= 512 {
            break;
        }
    }
    analysis
}

impl<'a> Analysis<'a> {
    /// The reachable blocks in reverse postorder.
    pub(super) fn rpo(&self) -> &[Block] {
        &self.rpo
    }

    /// The range of `value` at the entry of `block`, if one is known.
    pub(super) fn range_of(&self, block: Block, value: Value) -> Option<(i64, i64)> {
        let mut memo = Memo::new();
        self.interval_ctx(block, value, None, &mut memo).bounds()
    }

    /// Whether `value` is certainly a tagged fixnum at the entry of `block`.
    pub(super) fn proves_fixnum(&self, block: Block, value: Value) -> bool {
        let mut memo = Memo::new();
        self.func.value_type(value) == Type::Any
            && matches!(
                self.interval_ctx(block, value, None, &mut memo),
                State::Range { .. }
            )
    }

    /// Whether every value `value` can take fits an `i32`.
    pub(super) fn fits_i32(&self, block: Block, value: Value) -> bool {
        matches!(
            self.range_of(block, value),
            Some((lo, hi)) if lo >= i32::MIN as i64 && hi <= i32::MAX as i64
        )
    }

    /// Whether `check.index` on `vector` and `index` is provably in bounds:
    /// the index is a fixnum in `[0, length)`, and the vector's length is at
    /// least `hi + 1`.
    pub(super) fn bounds_proof(&self, block: Block, vector: Value, index: Value) -> bool {
        if self.func.value_type(index) != Type::Any {
            return false;
        }
        let Some((lo, hi)) = self.range_of(block, index) else {
            return false;
        };
        if lo < 0 {
            return false;
        }
        let mut memo = Memo::new();
        let Some(length) = vector_length(self, block, vector, None, &mut memo) else {
            return false;
        };
        match length {
            // The length is at least `length.lo`, so an index below it is
            // in bounds whatever the exact length is.
            State::Range { lo: length_lo, .. } => hi < length_lo,
            _ => false,
        }
    }

    /// Whether comparison `cond` is provably true or false at `block`.
    pub(super) fn truth(&self, block: Block, cond: Value) -> Option<bool> {
        let inst = def_inst(self.func, cond)?;
        let data = &self.func.insts[inst];
        let args = self.func.args(inst);
        let mut memo = Memo::new();
        match data.op {
            Op::CmpI32(cmp) | Op::CmpI64(cmp) => {
                // A value compared with itself needs no range at all.
                if canon(self.func, args[0]) == canon(self.func, args[1]) {
                    return Some(matches!(cmp, Cmp::Eq | Cmp::Le | Cmp::Ge));
                }
                let a = self.interval_ctx(block, args[0], None, &mut memo);
                let b = self.interval_ctx(block, args[1], None, &mut memo);
                cmp_truth(cmp, a, b)
            }
            Op::TestInt32 | Op::TestInt64 => {
                let arg = args[0];
                // A fixnum test on a value the domain already knows is a
                // fixnum cannot fail; on a non-fixnum it says nothing new.
                (self.func.value_type(arg) == Type::Any
                    && matches!(
                        self.interval_ctx(block, arg, None, &mut memo),
                        State::Range { .. }
                    ))
                .then_some(true)
            }
            _ => None,
        }
    }

    /// The state of `value` at the entry of `block`, with `refs` refining the
    /// values a branch on the incoming edge constrained.
    fn interval_ctx(
        &self,
        block: Block,
        value: Value,
        refs: Option<&HashMap<Value, State>>,
        memo: &mut Memo,
    ) -> State {
        if let Some(&state) = memo.get(&(block, value)) {
            return state;
        }
        let key = canon(self.func, value);
        let mut state = if let Some(refined) = refs.and_then(|map| map.get(&key).copied()) {
            refined
        } else if let Some(&known) = self.entries[block].get(&key) {
            known
        } else {
            match self.func.value_def(key) {
                ValueDef::Param { block: owner, .. } => {
                    self.entries[owner].get(&key).copied().unwrap_or(State::Top)
                }
                ValueDef::Inst(inst) => self.transfer(block, inst, refs, memo),
            }
        };
        // Reapply what each guard on the chain to `key` guarantees.
        let mut cur = value;
        for _ in 0..4096 {
            if cur == key {
                break;
            }
            let Some(inst) = def_inst(self.func, cur) else {
                break;
            };
            match self.func.insts[inst].op {
                Op::CheckInt32
                | Op::CheckInt64
                | Op::Unbox(Repr::Int32)
                | Op::Unbox(Repr::Int64) => {
                    state = state.intersect(State::FIXNUM);
                    cur = self.func.args(inst)[0];
                }
                _ => break,
            }
        }
        let state = clamp_for_type(self.func.value_type(key), state);
        memo.insert((block, value), state);
        state
    }

    /// An operand's state. A raw operand with no fact is still within its own
    /// type's range.
    fn operand_state(
        &self,
        block: Block,
        args: &[Value],
        refs: Option<&HashMap<Value, State>>,
        memo: &mut Memo,
        position: usize,
        typed_fallback: bool,
    ) -> State {
        let Some(&value) = args.get(position) else {
            return State::Top;
        };
        let state = self.interval_ctx(block, value, refs, memo);
        if typed_fallback && state == State::Top {
            type_state(self.func.value_type(value)).unwrap_or(State::Top)
        } else {
            state
        }
    }

    /// The interval transfer function of one instruction, read in the context
    /// of `block` and its edge refinements.
    fn transfer(
        &self,
        block: Block,
        inst: Inst,
        refs: Option<&HashMap<Value, State>>,
        memo: &mut Memo,
    ) -> State {
        let func = self.func;
        let data = &func.insts[inst];
        let args = func.args(inst);
        let result_ty = match data.result {
            Some(result) => func.value_type(result),
            None => Type::Any,
        };
        let state_of = |memo: &mut Memo, position: usize| -> State {
            self.operand_state(block, args, refs, memo, position, false)
        };
        let raw = |memo: &mut Memo, position: usize| -> State {
            self.operand_state(block, args, refs, memo, position, true)
        };
        let state = match data.op {
            Op::Const(Constant::Int32(n)) => State::range(n as i64, n as i64),
            Op::Const(Constant::Int64(n)) => State::range(n, n),
            Op::Const(Constant::Bits(bits)) => match SchemeValue::from_raw(bits).int32() {
                Some(n) if result_ty == Type::Any => State::range(n as i64, n as i64),
                _ => State::Top,
            },
            // A box produces a tagged value. `box.int32` is always a fixnum;
            // `box.int64` is one exactly when the value fits i32, since a
            // wider value becomes a bignum.
            Op::Box(Repr::Int32) => clamp_for_type(Type::Any, raw(memo, 0)),
            Op::Box(Repr::Int64) => match raw(memo, 0) {
                State::Bottom => State::Bottom,
                State::Range { lo, hi } if lo >= i32::MIN as i64 && hi <= i32::MAX as i64 => {
                    State::Range { lo, hi }
                }
                _ => State::Top,
            },
            Op::CheckInt32 | Op::CheckInt64 | Op::Unbox(Repr::Int32) | Op::Unbox(Repr::Int64) => {
                raw(memo, 0).intersect(State::FIXNUM)
            }
            // A checked character is a Unicode scalar value.
            Op::CheckChar => State::range(0, 0x10FFFF),
            Op::VectorLength => State::range(0, i32::MAX as i64),
            Op::CheckIndex => {
                let index = state_of(memo, 1);
                let mut state = match index {
                    State::Bottom => State::Bottom,
                    State::Top => State::range(0, i32::MAX as i64),
                    State::Range { lo, hi } => State::range(lo.max(0), hi.min(i32::MAX as i64)),
                };
                if let Some(State::Range { hi: length_hi, .. }) =
                    vector_length(self, block, args[0], refs, memo)
                {
                    state = state.intersect(State::range(0, length_hi.saturating_sub(1)));
                }
                state
            }
            Op::CheckBigIntFits64 => State::I64,
            Op::LoadRationalComponent { .. }
            | Op::LoadRationalComponentWide { .. }
            | Op::Int128Low
            | Op::Int128High => State::I64,
            Op::AddI32Ovf => arith(Arith::Add, raw(memo, 0), raw(memo, 1), Type::Int32, false),
            Op::SubI32Ovf => arith(Arith::Sub, raw(memo, 0), raw(memo, 1), Type::Int32, false),
            Op::MulI32Ovf => arith(Arith::Mul, raw(memo, 0), raw(memo, 1), Type::Int32, false),
            // The total i64 operations wrap, so an out-of-range bound says
            // nothing; the checked ones leave the function instead, so their
            // continuing value is the intersection with the i64 range.
            Op::AddI64 => arith(Arith::Add, raw(memo, 0), raw(memo, 1), Type::Int64, true),
            Op::SubI64 => arith(Arith::Sub, raw(memo, 0), raw(memo, 1), Type::Int64, true),
            Op::MulI64 => arith(Arith::Mul, raw(memo, 0), raw(memo, 1), Type::Int64, true),
            Op::AddI64Ovf => arith(Arith::Add, raw(memo, 0), raw(memo, 1), Type::Int64, false),
            Op::SubI64Ovf => arith(Arith::Sub, raw(memo, 0), raw(memo, 1), Type::Int64, false),
            Op::MulI64Ovf => arith(Arith::Mul, raw(memo, 0), raw(memo, 1), Type::Int64, false),
            Op::QuotI32 | Op::RemI32 | Op::ModI32 | Op::QuotI64 | Op::RemI64 | Op::ModI64 => {
                division(data.op, result_ty, raw(memo, 0), raw(memo, 1))
            }
            _ => State::Top,
        };
        // Every unboxed integer is at least within its own type.
        match state {
            State::Top => type_state(result_ty).unwrap_or(State::Top),
            _ => state,
        }
    }

    /// The entry facts of `block`: parameter ranges and refined values.
    fn block_entry(&self, block: Block) -> HashMap<Value, State> {
        let params = self.func.params(block).to_vec();
        let mut edges: Vec<(Block, Edge, HashMap<Value, State>)> = Vec::new();
        for &(pred, k) in &self.incoming[block] {
            let Some(term) = &self.func.blocks[pred].term else {
                continue;
            };
            let edge = term.edges()[k];
            let refs = self.edge_refinements(pred, k);
            edges.push((pred, edge, refs));
        }

        // A value refined on one incoming edge is unconstrained on the
        // others, so its entry range joins every edge's view of it.
        let mut keys: HashSet<Value> = HashSet::new();
        for (_, _, refs) in &edges {
            keys.extend(refs.keys().copied());
        }
        let keys: Vec<Value> = keys.into_iter().collect();

        // One memo per edge: its `refs` are fixed for all the queries made
        // in that edge's context.
        let mut param_states: Vec<Vec<State>> = Vec::with_capacity(edges.len());
        let mut key_states: Vec<Vec<State>> = Vec::with_capacity(edges.len());
        for (pred, edge, refs) in &edges {
            let mut memo = Memo::new();
            let mut states = Vec::with_capacity(params.len());
            for index in 0..params.len() {
                let state = match self.func.edge_args(edge).get(index) {
                    Some(&arg) => self.interval_ctx(*pred, arg, Some(refs), &mut memo),
                    None => State::Bottom,
                };
                states.push(state);
            }
            param_states.push(states);
            let mut states = Vec::with_capacity(keys.len());
            for &key in &keys {
                states.push(self.interval_ctx(*pred, key, Some(refs), &mut memo));
            }
            key_states.push(states);
        }

        let mut out: HashMap<Value, State> = HashMap::new();
        for (index, &param) in params.iter().enumerate() {
            let state = param_states
                .iter()
                .map(|states| states[index])
                .fold(State::Bottom, State::join);
            if state != State::Bottom && state != State::Top {
                out.insert(param, clamp_for_type(self.func.value_type(param), state));
            }
        }
        for (slot, &key) in keys.iter().enumerate() {
            let state = key_states
                .iter()
                .map(|states| states[slot])
                .fold(State::Bottom, State::join);
            if state != State::Bottom && state != State::Top {
                out.insert(key, state);
            }
        }
        out
    }

    /// Joins freshly computed entry facts with the previous iteration and
    /// widens parameters that grew.
    fn merge_entry(
        &self,
        block: Block,
        old: &HashMap<Value, State>,
        computed: HashMap<Value, State>,
    ) -> HashMap<Value, State> {
        let params = self.func.params(block).to_vec();
        let mut out = computed;
        for (&key, &state) in old {
            match out.get(&key).copied() {
                Some(current) => {
                    out.insert(key, current.join(state));
                }
                None => {
                    out.insert(key, state);
                }
            }
        }
        for &param in &params {
            let new = out.get(&param).copied().unwrap_or(State::Bottom);
            let state = match old.get(&param).copied() {
                Some(previous) if previous != State::Bottom => widen(previous, new),
                _ => new,
            };
            if state == State::Bottom {
                out.remove(&param);
            } else {
                out.insert(param, clamp_for_type(self.func.value_type(param), state));
            }
        }
        out
    }

    /// The facts a branch edge into an arm proves about its comparison
    /// operands, keyed by canonical value.
    fn edge_refinements(&self, pred: Block, k: usize) -> HashMap<Value, State> {
        let mut out = HashMap::new();
        let Some(term) = &self.func.blocks[pred].term else {
            return out;
        };
        let edge = term.edges()[k];
        let Terminator::Branch {
            cond,
            then,
            otherwise,
        } = term
        else {
            return out;
        };
        // Only a branch whose arms are distinct tells which polarity holds.
        let positive = if then.block == edge.block && otherwise.block != edge.block {
            true
        } else if otherwise.block == edge.block && then.block != edge.block {
            false
        } else {
            return out;
        };
        let Some(inst) = def_inst(self.func, *cond) else {
            return out;
        };
        let args = self.func.args(inst);
        let mut memo = Memo::new();
        match self.func.insts[inst].op {
            Op::CmpI32(cmp) | Op::CmpI64(cmp) => {
                let cmp = if positive { cmp } else { negate(cmp) };
                let a = self.interval_ctx(pred, args[0], None, &mut memo);
                let b = self.interval_ctx(pred, args[1], None, &mut memo);
                let (a, b) = refine(cmp, a, b);
                insert_refinement(&mut out, self.func, args[0], a);
                insert_refinement(&mut out, self.func, args[1], b);
            }
            Op::TestInt32 | Op::TestInt64 => {
                let arg = args[0];
                if self.func.value_type(arg) != Type::Any {
                    return out;
                }
                let current = self.interval_ctx(pred, arg, None, &mut memo);
                if positive {
                    // The taken arm proves a fixnum payload.
                    let state = current.intersect(State::FIXNUM);
                    insert_refinement(&mut out, self.func, arg, state);
                } else if matches!(current, State::Range { .. }) {
                    // A known fixnum cannot fail the test, so the other arm
                    // is unreachable.
                    insert_refinement(&mut out, self.func, arg, State::Bottom);
                }
            }
            _ => {}
        }
        out
    }
}

/// Widens a parameter that grew: either bound that moved goes to infinity.
fn widen(previous: State, new: State) -> State {
    match (previous, new) {
        (State::Range { lo: plo, hi: phi }, State::Range { lo: nlo, hi: nhi }) => {
            if nlo >= plo && nhi <= phi {
                return previous;
            }
            State::range(
                if nlo < plo { i64::MIN } else { plo },
                if nhi > phi { i64::MAX } else { phi },
            )
        }
        (State::Top, _) | (_, State::Top) => State::Top,
        (State::Bottom, state) | (state, State::Bottom) => state,
    }
}

/// The vector whose length `value` is, when the analysis can name it.
fn vector_length(
    analysis: &Analysis<'_>,
    block: Block,
    vector: Value,
    refs: Option<&HashMap<Value, State>>,
    memo: &mut Memo,
) -> Option<State> {
    let object = object_of(analysis.func, vector);
    let inst = def_inst(analysis.func, object)?;
    match analysis.func.insts[inst].op {
        // A freshly allocated vector's length is its operand count.
        Op::AllocVector => {
            let length = analysis.func.args(inst).len() as i64;
            Some(State::range(length, length))
        }
        Op::VectorLength => {
            let result = analysis.func.insts[inst].result?;
            let state = analysis.interval_ctx(block, result, refs, memo);
            matches!(state, State::Range { .. }).then_some(state)
        }
        _ => None,
    }
}

/// The negation of a comparison.
fn negate(cmp: Cmp) -> Cmp {
    match cmp {
        Cmp::Eq => Cmp::Ne,
        Cmp::Ne => Cmp::Eq,
        Cmp::Lt => Cmp::Ge,
        Cmp::Le => Cmp::Gt,
        Cmp::Gt => Cmp::Le,
        Cmp::Ge => Cmp::Lt,
    }
}

/// What `a cmp b` proves about its operands, intersected with what each side
/// already knows. An empty side means the arm is impossible.
fn refine(cmp: Cmp, a: State, b: State) -> (State, State) {
    if a == State::Bottom || b == State::Bottom {
        return (State::Bottom, State::Bottom);
    }
    let (Some((alo, ahi)), Some((blo, bhi))) = (a.bounds(), b.bounds()) else {
        return (a, b);
    };
    match cmp {
        Cmp::Gt => {
            let (b, a) = refine(Cmp::Lt, b, a);
            (a, b)
        }
        Cmp::Ge => {
            let (b, a) = refine(Cmp::Le, b, a);
            (a, b)
        }
        Cmp::Lt => {
            // a < b, with `bhi - 1` and `alo + 1` computed carefully: a
            // boundary that cannot be represented makes the arm impossible.
            let Some(other) = bhi.checked_sub(1) else {
                return (State::Bottom, State::Bottom);
            };
            let (lo, hi) = (alo, ahi.min(other));
            if lo > hi {
                return (State::Bottom, State::Bottom);
            }
            let Some(other) = alo.checked_add(1) else {
                return (State::Bottom, State::Bottom);
            };
            let (blo, bhi) = (blo.max(other), bhi);
            if blo > bhi {
                return (State::Bottom, State::Bottom);
            }
            (State::Range { lo, hi }, State::Range { lo: blo, hi: bhi })
        }
        Cmp::Le => {
            let (lo, hi) = (alo, ahi.min(bhi));
            if lo > hi {
                return (State::Bottom, State::Bottom);
            }
            let (blo, bhi) = (blo.max(alo), bhi);
            if blo > bhi {
                return (State::Bottom, State::Bottom);
            }
            (State::Range { lo, hi }, State::Range { lo: blo, hi: bhi })
        }
        Cmp::Eq => {
            let lo = alo.max(blo);
            let hi = ahi.min(bhi);
            if lo > hi {
                (State::Bottom, State::Bottom)
            } else {
                (State::Range { lo, hi }, State::Range { lo, hi })
            }
        }
        Cmp::Ne => {
            // Only a singleton can be excluded while staying an interval.
            if alo == ahi {
                (a, exclude(b, alo))
            } else if blo == bhi {
                (exclude(a, blo), b)
            } else {
                (a, b)
            }
        }
    }
}

/// `state` without the singleton `value`, when the result is still an
/// interval.
fn exclude(state: State, value: i64) -> State {
    match state {
        State::Range { lo, hi } if lo <= value && value <= hi => {
            if lo == hi {
                State::Bottom
            } else if lo == value {
                State::Range { lo: lo + 1, hi }
            } else if hi == value {
                State::Range { lo, hi: hi - 1 }
            } else {
                state
            }
        }
        state => state,
    }
}

/// Records a refinement of `value`, intersected with any other refinement
/// the same edge already made of its canonical value.
fn insert_refinement(out: &mut HashMap<Value, State>, func: &Func, value: Value, state: State) {
    if state == State::Top {
        return;
    }
    let key = canon(func, value);
    match out.get(&key).copied() {
        Some(previous) => {
            out.insert(key, previous.intersect(state));
        }
        None => {
            out.insert(key, state);
        }
    }
}

/// The truth of `a cmp b` when the ranges settle it.
fn cmp_truth(cmp: Cmp, a: State, b: State) -> Option<bool> {
    match cmp {
        Cmp::Ne => cmp_truth(Cmp::Eq, a, b).map(|value| !value),
        _ => {
            let (alo, ahi) = a.bounds()?;
            let (blo, bhi) = b.bounds()?;
            match cmp {
                Cmp::Eq => {
                    if ahi < blo || bhi < alo {
                        Some(false)
                    } else if alo == ahi && blo == bhi {
                        Some(true)
                    } else {
                        None
                    }
                }
                Cmp::Lt => {
                    if ahi < blo {
                        Some(true)
                    } else if alo >= bhi {
                        Some(false)
                    } else {
                        None
                    }
                }
                Cmp::Le => {
                    if ahi <= blo {
                        Some(true)
                    } else if alo > bhi {
                        Some(false)
                    } else {
                        None
                    }
                }
                Cmp::Gt => cmp_truth(Cmp::Lt, b, a),
                Cmp::Ge => cmp_truth(Cmp::Le, b, a),
                Cmp::Ne => None,
            }
        }
    }
}

#[derive(Clone, Copy)]
enum Arith {
    Add,
    Sub,
    Mul,
}

/// The range of an arithmetic result.
///
/// `total` is the wrapping `add.i64`/`sub.i64`/`mul.i64` family: when the
/// exact bounds leave the type the wrapped value can be anything, so the
/// answer is the whole type range. The checked forms deopt instead, so their
/// continuing value is the exact bounds intersected with the type.
fn arith(kind: Arith, a: State, b: State, ty: Type, total: bool) -> State {
    if a == State::Bottom || b == State::Bottom {
        return State::Bottom;
    }
    let (Some((alo, ahi)), Some((blo, bhi))) = (a.bounds(), b.bounds()) else {
        return type_state(ty).unwrap_or(State::Top);
    };
    let (alo, ahi, blo, bhi) = (alo as i128, ahi as i128, blo as i128, bhi as i128);
    let (lo, hi) = match kind {
        Arith::Add => (alo + blo, ahi + bhi),
        Arith::Sub => (alo - bhi, ahi - blo),
        Arith::Mul => {
            let values = [alo * blo, alo * bhi, ahi * blo, ahi * bhi];
            let lo = values.iter().copied().min().unwrap();
            let hi = values.iter().copied().max().unwrap();
            (lo, hi)
        }
    };
    let Some((type_lo, type_hi)) = type_span(ty) else {
        return State::Top;
    };
    if total {
        if lo < type_lo || hi > type_hi {
            return type_state(ty).unwrap_or(State::Top);
        }
        return State::range(lo as i64, hi as i64);
    }
    let lo = lo.max(type_lo);
    let hi = hi.min(type_hi);
    if lo > hi {
        State::Bottom
    } else {
        State::Range {
            lo: lo as i64,
            hi: hi as i64,
        }
    }
}

/// The range of a quotient, remainder or modulo.
fn division(op: Op, ty: Type, a: State, b: State) -> State {
    if a == State::Bottom || b == State::Bottom {
        return State::Bottom;
    }
    let full = type_state(ty).unwrap_or(State::Top);
    let (Some((alo, ahi)), Some((blo, bhi))) = (a.bounds(), b.bounds()) else {
        return full;
    };
    let singleton = (blo == bhi).then_some(blo);
    match op {
        Op::QuotI32 | Op::QuotI64 => {
            if let Some(divisor) = singleton {
                if divisor == 0 {
                    return State::Bottom;
                }
                // i128 keeps `MIN / -1` from overflowing while we bound it.
                let a = alo as i128 / divisor as i128;
                let b = ahi as i128 / divisor as i128;
                return clip(a.min(b), a.max(b), ty);
            }
            if blo > 0 && alo >= 0 {
                let lo = alo as i128 / bhi as i128;
                let hi = ahi as i128 / blo as i128;
                return clip(lo, hi, ty);
            }
            full
        }
        Op::RemI32 | Op::RemI64 => {
            if let Some(divisor) = singleton {
                if divisor == 0 {
                    return State::Bottom;
                }
                let limit = (divisor as i128).abs() - 1;
                let (lo, hi) = if alo >= 0 {
                    (0, limit.min(ahi as i128))
                } else if ahi <= 0 {
                    (-limit.max(-(alo as i128)), 0)
                } else {
                    (-limit, limit)
                };
                return clip(lo, hi, ty);
            }
            let magnitude = blo.unsigned_abs().max(bhi.unsigned_abs()) as i128;
            let limit = magnitude.saturating_sub(1);
            clip(-limit, limit, ty)
        }
        Op::ModI32 | Op::ModI64 => {
            if let Some(divisor) = singleton {
                if divisor == 0 {
                    return State::Bottom;
                }
                return if divisor > 0 {
                    clip(0, divisor as i128 - 1, ty)
                } else {
                    clip(divisor as i128 + 1, 0, ty)
                };
            }
            if blo > 0 {
                return clip(0, bhi as i128 - 1, ty);
            }
            if bhi < 0 {
                return clip(blo as i128 + 1, 0, ty);
            }
            full
        }
        _ => full,
    }
}

/// The integer span of a raw type.
fn type_span(ty: Type) -> Option<(i128, i128)> {
    match ty {
        Type::Int32 => Some((i32::MIN as i128, i32::MAX as i128)),
        Type::Int64 => Some((i64::MIN as i128, i64::MAX as i128)),
        _ => None,
    }
}

/// Intersects an exact range with the type; empty means the operation always
/// leaves the function.
fn clip(lo: i128, hi: i128, ty: Type) -> State {
    let Some((type_lo, type_hi)) = type_span(ty) else {
        return State::Top;
    };
    let lo = lo.max(type_lo);
    let hi = hi.min(type_hi);
    if lo > hi {
        State::Bottom
    } else {
        State::Range {
            lo: lo as i64,
            hi: hi as i64,
        }
    }
}

/// Whether the checked arithmetic `op` cannot overflow for these operands.
fn cannot_overflow(op: Op, a: (i64, i64), b: (i64, i64)) -> bool {
    let (alo, ahi) = (a.0 as i128, a.1 as i128);
    let (blo, bhi) = (b.0 as i128, b.1 as i128);
    let (lo, hi) = match op {
        Op::AddI64Ovf => (alo + blo, ahi + bhi),
        Op::SubI64Ovf => (alo - bhi, ahi - blo),
        Op::MulI64Ovf => {
            let values = [alo * blo, alo * bhi, ahi * blo, ahi * bhi];
            (
                values.iter().copied().min().unwrap(),
                values.iter().copied().max().unwrap(),
            )
        }
        _ => return false,
    };
    lo >= i64::MIN as i128 && hi <= i64::MAX as i128
}

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let analysis = analyze(func);
    let mut guards: Vec<(Inst, Repr, usize)> = Vec::new();
    let mut totals: Vec<(Inst, Op)> = Vec::new();
    let mut bools: Vec<(Inst, bool)> = Vec::new();
    let mut jumps: Vec<(Block, Edge)> = Vec::new();
    let mut work = 0;

    for &block in analysis.rpo() {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            let data = func.insts[inst];
            let args = func.args(inst);
            match data.op {
                Op::CheckInt32 | Op::CheckInt64 => {
                    let Some(&source) = args.first() else {
                        continue;
                    };
                    if analysis.proves_fixnum(block, source) {
                        let repr = if data.op == Op::CheckInt32 {
                            Repr::Int32
                        } else {
                            Repr::Int64
                        };
                        guards.push((inst, repr, 0));
                    }
                }
                Op::CheckIndex => {
                    let (Some(&vector), Some(&index)) = (args.first(), args.get(1)) else {
                        continue;
                    };
                    if analysis.bounds_proof(block, vector, index) {
                        guards.push((inst, Repr::Int32, 1));
                    }
                }
                Op::AddI64Ovf | Op::SubI64Ovf | Op::MulI64Ovf => {
                    let (Some(a), Some(b)) = (args.first(), args.get(1)) else {
                        continue;
                    };
                    let (Some(a), Some(b)) =
                        (analysis.range_of(block, *a), analysis.range_of(block, *b))
                    else {
                        continue;
                    };
                    if cannot_overflow(data.op, a, b) {
                        let total = match data.op {
                            Op::AddI64Ovf => Op::AddI64,
                            Op::SubI64Ovf => Op::SubI64,
                            _ => Op::MulI64,
                        };
                        totals.push((inst, total));
                    }
                }
                Op::BoolToValue => {
                    let Some(&cond) = args.first() else {
                        continue;
                    };
                    if let Some(value) = analysis.truth(block, cond) {
                        bools.push((inst, value));
                    }
                }
                _ => {}
            }
        }

        if let Some(Terminator::Branch {
            cond,
            then,
            otherwise,
        }) = func.blocks[block].term.clone()
        {
            if let Some(taken) = analysis.truth(block, cond) {
                let edge = if taken { then } else { otherwise };
                jumps.push((block, edge));
            }
        }
    }

    // The proofs are complete; the immutable analysis borrow ends here.
    drop(analysis);

    let mut changed = false;
    for (inst, repr, position) in guards {
        let arg = func.args(inst)[position];
        let data = &mut func.insts[inst];
        data.op = Op::Unbox(repr);
        data.args = cranelift_entity::EntityList::from_slice(&[arg], &mut func.pool);
        // The check can no longer deopt.
        data.frame = None;
        changed = true;
    }
    for (inst, op) in totals {
        let data = &mut func.insts[inst];
        data.op = op;
        data.frame = None;
        changed = true;
    }
    for (inst, value) in bools {
        let bits = if value {
            SchemeValue::VALUE_TRUE
        } else {
            SchemeValue::VALUE_FALSE
        };
        let data = &mut func.insts[inst];
        data.op = Op::Const(Constant::Bits(bits as u64));
        data.args = cranelift_entity::EntityList::new();
        data.frame = None;
        changed = true;
    }
    for (block, edge) in jumps {
        func.set_terminator(block, Terminator::Jump(edge));
        changed = true;
    }

    PassOutcome {
        changed,
        work: work + func.insts.len() + func.blocks.len(),
    }
}

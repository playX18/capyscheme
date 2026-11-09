//! Local cell elimination.
//!
//! Assignment conversion boxes every mutated lexical variable in a
//! `make-variable` cell, and the bytecode lowers its reads and writes to
//! `LoadCell` and to a `Helper::VariableSet` call. When the cell cannot be
//! observed by anything else, the box is pure overhead: the loads become SSA
//! values, the writes become redefinitions, and a loop that carries the cell
//! carries the value instead.

use std::collections::{HashMap, HashSet};

use cranelift_entity::{EntityList, EntityRef, SecondaryMap};

use super::simplify::edges_mut;
use super::{PassOutcome, Subst, reachable, replace_uses};
use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Func, Value, ValueDef};
use crate::runtime::codegen::opt::ir::inst::{Constant, Helper, Inst, Op, RematKind, Terminator};
use crate::runtime::codegen::opt::ir::types::{ClassRef, Type};
use crate::runtime::objectmodel::Value as SchemeValue;
use crate::runtime::prims::def::Primitive;

/// A version of the cell's value during the dataflow.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum State {
    /// An SSA value that already exists.
    Val(Value),
    /// A parameter this pass will add to a block where incoming versions
    /// differ.
    Phi(Block),
    /// A `load.cell` this pass will add on an OSR edge to seed the value from
    /// the cell the baseline frame carries.
    Seed(Block),
}

/// One `make-variable` call.
struct Candidate {
    alloc: Inst,
    block: Block,
    root: Value,
}

/// The mapped uses of one cell group.
struct Uses {
    loads: Vec<Inst>,
    stores: Vec<Inst>,
    checks: Vec<Inst>,
    tests: Vec<Inst>,
    /// The value each store writes.
    store_values: HashMap<Inst, Value>,
    /// Every block parameter that denotes the cell.
    params: Vec<(Block, u32)>,
}

/// A frame-bearing site whose frame names the cell.
struct FrameSite {
    block: Block,
    /// `Some(inst)` inserts the marker before the instruction; `None` appends
    /// it at the end of the block, before the terminator.
    before: Option<Inst>,
    version: State,
}

/// Everything the rewrite needs, decided before the first irreversible edit.
struct Plan {
    /// Every SSA value that denotes the cell.
    members: HashSet<Value>,
    dead: HashSet<Inst>,
    loads: Vec<(Value, State)>,
    tests: Vec<(Block, Inst, Value)>,
    params: Vec<(Block, u32)>,
    phis: Vec<Block>,
    seeds: Vec<(Block, Value)>,
    out: HashMap<Block, Option<State>>,
    frames: Vec<FrameSite>,
}

/// The pass proper, without the configuration gate, so tests exercise it
/// regardless of the environment.
pub(super) fn run(func: &mut Func) -> PassOutcome {
    let candidates = candidates(func);
    if candidates.is_empty() {
        return PassOutcome::default();
    }
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        " cells: {} candidates in {}",
        candidates.len(),
        func.label
    );
    let mut changed = false;
    let mut work = func.insts.len();
    for candidate in candidates {
        if !is_placed(func, candidate.alloc) {
            continue;
        }
        work += func.insts.len();
        if let Some(plan) = analyze(func, &candidate) {
            apply(func, plan);
            changed = true;
        }
    }
    if !changed {
        return PassOutcome::default();
    }
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        " cells: eliminated local cell groups"
    );
    PassOutcome {
        changed: true,
        work: work + func.insts.len(),
    }
}

/// Every `make-variable` call in a reachable block.
fn candidates(func: &Func) -> Vec<Candidate> {
    let mut out = Vec::new();
    for block in func.reverse_postorder() {
        for &inst in &func.blocks[block].insts {
            let data = &func.insts[inst];
            let Op::CallPrim { prim, .. } = data.op else {
                continue;
            };
            if Primitive::from_id(prim.index() as u16) != Some(Primitive::MakeBox) {
                continue;
            }
            let (Some(root), [_initial]) = (data.result, func.args(inst)) else {
                continue;
            };
            out.push(Candidate {
                alloc: inst,
                block,
                root,
            });
        }
    }
    out
}

fn is_placed(func: &Func, inst: Inst) -> bool {
    func.blocks.values().any(|data| data.insts.contains(&inst))
}

/// Decides whether the group rooted at `candidate` can be eliminated. The
/// analysis is pure; [`apply`] performs every edit.
fn analyze(func: &Func, candidate: &Candidate) -> Option<Plan> {
    macro_rules! bail {
        ($stage:expr) => {{
            log::debug!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                " cells: {} in {} rejected at {}",
                candidate.root,
                candidate.block,
                $stage
            );
            return None;
        }};
    }
    let live = reachable(func);
    let incoming = edge_sites(func);
    let osr_edges = osr_edges(func);
    // A previous group in this run may have rewritten the initial value, so
    // read it from the instruction as it stands now.
    let Some(&initial) = func.args(candidate.alloc).first() else {
        bail!("no initial value");
    };

    let members = match members(func, candidate, initial, &live, &incoming, &osr_edges) {
        Some(members) => members,
        None => bail!("membership"),
    };

    // Escape analysis over every use of every member.
    let used = used_values(func);
    let uses = match classify(func, &live, &members, &used) {
        Some(uses) => uses,
        None => bail!("escape"),
    };

    // The value dataflow. Seeds come from the OSR edges into a header whose
    // cell parameter is a member: that edge carries the cell from the
    // baseline frame.
    let seeds = seeds(func, &incoming, &osr_edges, &members);
    let rpo = func.reverse_postorder();
    let (in_state, out, phis) = match solve(
        func, candidate, initial, &live, &incoming, &rpo, &seeds, &uses,
    ) {
        Some(solved) => solved,
        None => bail!("dataflow"),
    };

    // The version of every read and every frame that names the cell.
    let mut loads = Vec::with_capacity(uses.loads.len());
    let mut frames = Vec::new();
    let mut frame_versions: HashMap<FrameState, State> = HashMap::new();
    for &block in &rpo {
        let mut state = match in_state.get(&block) {
            Some(Some(state)) => Some(*state),
            _ => None,
        };
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            let before = state;
            if inst == candidate.alloc {
                state = Some(State::Val(initial));
            } else if let Some(&value) = uses.store_values.get(&inst) {
                state = Some(State::Val(value));
            }
            if uses.loads.contains(&inst) {
                let Some(result) = func.insts[inst].result else {
                    bail!("load without result");
                };
                let Some(version) = before else {
                    bail!("load without a value version");
                };
                loads.push((result, version));
            }
            let Some(frame) = func.insts[inst].frame else {
                continue;
            };
            if !frame_has_member(func, frame, &members) {
                continue;
            }
            let Some(version) = before else {
                bail!("frame without a value version");
            };
            if record_frame_version(&mut frame_versions, func, frame, &members, version).is_none() {
                bail!("shared frame with two versions");
            }
            frames.push(FrameSite {
                block,
                before: Some(inst),
                version,
            });
        }
        let Some(term) = &func.blocks[block].term else {
            continue;
        };
        let Some(frame) = term_frame(term) else {
            continue;
        };
        if !frame_has_member(func, frame, &members) {
            continue;
        }
        let Some(version) = state else {
            bail!("terminator frame without a value version");
        };
        if record_frame_version(&mut frame_versions, func, frame, &members, version).is_none() {
            bail!("shared terminator frame with two versions");
        }
        frames.push(FrameSite {
            block,
            before: None,
            version,
        });
    }

    let mut dead: HashSet<Inst> = HashSet::new();
    dead.extend(uses.loads.iter().copied());
    dead.extend(uses.stores.iter().copied());
    dead.extend(uses.checks.iter().copied());
    dead.extend(uses.tests.iter().copied());
    dead.insert(candidate.alloc);

    let mut tests = Vec::with_capacity(uses.tests.len());
    for &inst in &uses.tests {
        tests.push((block_of(func, inst), inst, func.insts[inst].result?));
    }

    Some(Plan {
        members,
        dead,
        loads,
        tests,
        params: uses.params,
        phis,
        seeds,
        out,
        frames,
    })
}

fn used_values(func: &Func) -> HashSet<Value> {
    let mut used = HashSet::new();
    for (_, data) in func.blocks.iter() {
        for &inst in &data.insts {
            used.extend(func.args(inst).iter().copied());
        }
        if let Some(term) = &data.term {
            used.extend(func.terminator_uses(term));
        }
    }
    for (_, frame) in func.frames.iter() {
        used.insert(frame.closure);
        used.extend(frame.registers.iter().map(|&(_, value)| value));
    }
    used
}

/// The group of values that denote the candidate's cell.
fn members(
    func: &Func,
    candidate: &Candidate,
    initial: Value,
    live: &SecondaryMap<Block, bool>,
    incoming: &SecondaryMap<Block, Vec<(Block, usize)>>,
    osr: &HashSet<(Block, Block)>,
) -> Option<HashSet<Value>> {
    let root = candidate.root;
    if root == initial {
        return None;
    }

    // The position of every live block parameter, to walk its predecessors.
    let mut param_pos: HashMap<Value, (Block, u32)> = HashMap::new();
    for block in func.blocks.keys().collect::<Vec<_>>() {
        if !live[block] {
            continue;
        }
        for (index, &param) in func.params(block).iter().enumerate() {
            param_pos.insert(param, (block, index as u32));
        }
    }

    let mut potential: HashSet<Value> = HashSet::from([root]);
    let bound = func.insts.len() + func.blocks.len() + 8;
    for _ in 0..bound {
        let mut changed = false;
        for (block, data) in func.blocks.iter() {
            if !live[block] {
                continue;
            }
            for &inst in &data.insts {
                let data = &func.insts[inst];
                if data.op != Op::CheckClass(ClassRef::VARIABLE) {
                    continue;
                }
                let (Some(result), Some(&operand)) = (data.result, func.args(inst).first()) else {
                    continue;
                };
                if potential.contains(&operand) && potential.insert(result) {
                    changed = true;
                }
                if potential.contains(&result) && potential.insert(operand) {
                    changed = true;
                }
            }
        }
        for (&param, &(block, index)) in &param_pos {
            let slot = index as usize;
            // Forward: an incoming member makes the parameter a candidate.
            if !potential.contains(&param)
                && incoming[block].iter().any(|&(pred, k)| {
                    live[pred]
                        && func.blocks[pred]
                            .term
                            .as_ref()
                            .and_then(|term| term.edges().get(k).copied())
                            .and_then(|edge| func.edge_args(&edge).get(slot).copied())
                            .is_some_and(|arg| arg != param && potential.contains(&arg))
                })
            {
                potential.insert(param);
                changed = true;
            }
            // Backward: a candidate parameter pulls in the parameters that
            // feed it, so a cycle of loop headers stays connected.
            if potential.contains(&param) {
                for &(pred, k) in &incoming[block] {
                    if !live[pred] {
                        continue;
                    }
                    let Some(edge) = func.blocks[pred]
                        .term
                        .as_ref()
                        .and_then(|term| term.edges().get(k).copied())
                    else {
                        continue;
                    };
                    let Some(&arg) = func.edge_args(&edge).get(slot) else {
                        continue;
                    };
                    if arg != param && param_pos.contains_key(&arg) && potential.insert(arg) {
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    let mut members = potential;
    loop {
        let mut removed: Vec<Value> = Vec::new();
        for (&param, &(block, index)) in &param_pos {
            if !members.contains(&param) {
                continue;
            }

            if block == func.entry || !incoming[block].iter().any(|&(pred, _)| live[pred]) {
                removed.push(param);
                continue;
            }
            let slot = index as usize;
            for &(pred, k) in &incoming[block] {
                if !live[pred] || osr.contains(&(pred, block)) {
                    continue;
                }
                let Some(edge) = func.blocks[pred]
                    .term
                    .as_ref()
                    .and_then(|term| term.edges().get(k).copied())
                else {
                    continue;
                };
                let Some(&arg) = func.edge_args(&edge).get(slot) else {
                    continue;
                };
                if arg != param && !members.contains(&arg) {
                    removed.push(param);
                    break;
                }
            }
        }
        for (block, data) in func.blocks.iter() {
            if !live[block] {
                continue;
            }
            for &inst in &data.insts {
                let data = &func.insts[inst];
                if data.op != Op::CheckClass(ClassRef::VARIABLE) {
                    continue;
                }
                let (Some(result), Some(&operand)) = (data.result, func.args(inst).first()) else {
                    continue;
                };
                if members.contains(&result) && !members.contains(&operand) {
                    removed.push(result);
                }
            }
        }
        if removed.is_empty() {
            break;
        }
        for value in removed {
            members.remove(&value);
        }
        if !members.contains(&root) {
            return None;
        }
    }
    if members.contains(&initial) {
        return None;
    }
    Some(members)
}

/// Classifies every use of every member. `None` means the cell escapes.
fn classify(
    func: &Func,
    live: &SecondaryMap<Block, bool>,
    members: &HashSet<Value>,
    used: &HashSet<Value>,
) -> Option<Uses> {
    let mut uses = Uses {
        loads: Vec::new(),
        stores: Vec::new(),
        checks: Vec::new(),
        tests: Vec::new(),
        store_values: HashMap::new(),
        params: Vec::new(),
    };
    macro_rules! escape {
        ($why:expr) => {{
            log::debug!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                " cells: escape: {}",
                $why
            );
            return None;
        }};
    }
    for (block, data) in func.blocks.iter() {
        if !live[block] {
            continue;
        }
        for (index, &param) in func.params(block).iter().enumerate() {
            if members.contains(&param) {
                uses.params.push((block, index as u32));
            }
        }
        for &inst in &data.insts {
            let inst_data = &func.insts[inst];
            let args = func.args(inst);
            for (pos, &arg) in args.iter().enumerate() {
                if !members.contains(&arg) {
                    continue;
                }
                match (pos, inst_data.op) {
                    (0, Op::LoadCell) if inst_data.result.is_some() => uses.loads.push(inst),
                    (0, Op::CheckClass(ClassRef::VARIABLE)) if args.len() == 1 => {
                        uses.checks.push(inst)
                    }
                    (0, Op::TestClass(ClassRef::VARIABLE)) if args.len() == 1 => {
                        uses.tests.push(inst)
                    }
                    (0, Op::StoreCell) if args.len() == 2 => {
                        uses.stores.push(inst);
                        uses.store_values.insert(inst, args[1]);
                    }
                    (0, Op::CallOp(helper))
                        if helper == Helper::VariableSet.helper_ref() && args.len() == 2 =>
                    {
                        if inst_data.result.is_some_and(|r| used.contains(&r)) {
                            escape!(format!("used store result {inst}"));
                        }
                        uses.stores.push(inst);
                        uses.store_values.insert(inst, args[1]);
                    }

                    (1, Op::StoreCell) => {
                        escape!(format!("cell stored as itself in {inst}"))
                    }
                    (1, Op::CallOp(helper)) if helper == Helper::VariableSet.helper_ref() => {
                        escape!(format!("cell stored as itself in {inst}"))
                    }
                    _ => escape!(format!(
                        "operand {pos} of {:?} in {inst} ({block})",
                        inst_data.op
                    )),
                }
            }
            if let Some(frame) = inst_data.frame {
                if frame_closure_is_member(func, frame, members) {
                    escape!(format!("frame closure in {inst}"));
                }
            }
        }
        let Some(term) = &data.term else {
            continue;
        };
        match term {
            Terminator::Branch { cond, .. } => {
                if members.contains(cond) {
                    escape!(format!("branch condition in {block}"));
                }
            }
            Terminator::Switch { value, .. } => {
                if members.contains(value) {
                    escape!(format!("switch value in {block}"));
                }
            }
            Terminator::Return(value) => {
                if members.contains(value) {
                    escape!(format!("returned in {block}"));
                }
            }
            Terminator::TailCall { callee, args, .. } => {
                if members.contains(callee)
                    || args
                        .as_slice(&func.pool)
                        .iter()
                        .any(|arg| members.contains(arg))
                {
                    escape!(format!("tail-call operand in {block}"));
                }
            }
            Terminator::ReturnMany { values, .. } => {
                if values.iter().any(|value| members.contains(value)) {
                    escape!(format!("returned in {block}"));
                }
            }
            Terminator::Jump(_) | Terminator::Deopt { .. } | Terminator::Unreachable => {}
        }
        for edge in term.edges() {
            let params = func.params(edge.block);
            for (index, &arg) in func.edge_args(&edge).iter().enumerate() {
                if !members.contains(&arg) {
                    continue;
                }

                if !params.get(index).is_some_and(|p| members.contains(p)) {
                    escape!(format!(
                        "edge argument {index} from {block} reaches a non-member parameter"
                    ));
                }
            }
        }
        if let Some(frame) = term_frame(term) {
            if frame_closure_is_member(func, frame, members) {
                escape!(format!("terminator frame closure in {block}"));
            }
        }
    }
    Some(uses)
}

fn frame_closure_is_member(func: &Func, frame: FrameState, members: &HashSet<Value>) -> bool {
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        if members.contains(&data.closure) {
            return true;
        }
        cur = data.parent;
    }
    false
}

/// Whether a frame state (or a parent) names a member in a register.
fn frame_has_member(func: &Func, frame: FrameState, members: &HashSet<Value>) -> bool {
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        if data
            .registers
            .iter()
            .any(|&(_, value)| members.contains(&value))
        {
            return true;
        }
        cur = data.parent;
    }
    false
}

fn record_frame_version(
    versions: &mut HashMap<FrameState, State>,
    func: &Func,
    frame: FrameState,
    members: &HashSet<Value>,
    version: State,
) -> Option<()> {
    let mut cur = Some(frame);
    while let Some(fs) = cur {
        let data = &func.frames[fs];
        if data
            .registers
            .iter()
            .any(|&(_, value)| members.contains(&value))
        {
            match versions.get(&fs) {
                Some(&existing) if existing != version => return None,
                Some(_) => {}
                None => {
                    versions.insert(fs, version);
                }
            }
        }
        cur = data.parent;
    }
    Some(())
}

fn term_frame(term: &Terminator) -> Option<FrameState> {
    match term {
        Terminator::TailCall { frame, .. }
        | Terminator::Deopt { frame } => Some(*frame),
        _ => None,
    }
}

/// The predecessor and edge index of every edge into a block.
fn edge_sites(func: &Func) -> SecondaryMap<Block, Vec<(Block, usize)>> {
    let mut incoming: SecondaryMap<Block, Vec<(Block, usize)>> = SecondaryMap::new();
    for (pred, data) in func.blocks.iter() {
        let Some(term) = &data.term else {
            continue;
        };
        for (k, edge) in term.edges().iter().enumerate() {
            if func.blocks.is_valid(edge.block) {
                incoming[edge.block].push((pred, k));
            }
        }
    }
    incoming
}

fn osr_edges(func: &Func) -> HashSet<(Block, Block)> {
    func.osr
        .iter()
        .map(|entry| (entry.block, entry.header))
        .collect()
}

/// One OSR seed per OSR block that feeds a header with a cell parameter.
fn seeds(
    func: &Func,
    incoming: &SecondaryMap<Block, Vec<(Block, usize)>>,
    osr: &HashSet<(Block, Block)>,
    members: &HashSet<Value>,
) -> Vec<(Block, Value)> {
    let mut out = Vec::new();
    for entry in &func.osr {
        let header = entry.header;
        let Some(sites) = incoming.get(header) else {
            continue;
        };
        for &(pred, k) in sites {
            if pred != entry.block || !osr.contains(&(pred, header)) {
                continue;
            }
            let Some(term) = &func.blocks[pred].term else {
                continue;
            };
            let edge = term.edges()[k];
            let args = func.edge_args(&edge);
            let params = func.params(header);
            // The first position whose header parameter is the cell. Every
            // such position is the same variable in a different register.
            let Some(index) = params.iter().position(|param| members.contains(param)) else {
                log::debug!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    " cells: osr edge {pred}->{header} has no member parameter"
                );
                continue;
            };
            if let Some(&arg) = args.get(index) {
                // The runtime cell is the argument. If it is itself a group
                // member the seed would keep it alive past its removal; the
                // group is then left boxed.
                if !members.contains(&arg) {
                    out.push((pred, arg));
                } else {
                    log::debug!(
                        target: crate::heap::logging::JIT_LOG_TARGET,
                        " cells: osr edge {pred}->{header} passes a member {arg} at {index}"
                    );
                }
            } else {
                log::debug!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    " cells: osr edge {pred}->{header} has {} args for {index}",
                    args.len()
                );
            }
        }
    }
    out
}

/// Solves the value dataflow. Returns the version on *entry* to every block and
/// the blocks that need a value parameter. The exit versions stay internal:
/// they only fill the arguments of the parameters the flow adds.
fn solve(
    func: &Func,
    candidate: &Candidate,
    initial: Value,
    live: &SecondaryMap<Block, bool>,
    incoming: &SecondaryMap<Block, Vec<(Block, usize)>>,
    rpo: &[Block],
    seeds: &[(Block, Value)],
    uses: &Uses,
) -> Option<(
    HashMap<Block, Option<State>>,
    HashMap<Block, Option<State>>,
    Vec<Block>,
)> {
    let seed_blocks: HashMap<Block, Value> = seeds.iter().copied().collect();
    let mut in_state: HashMap<Block, Option<State>> = HashMap::new();
    let mut out: HashMap<Block, Option<State>> = HashMap::new();
    let mut phis: HashSet<Block> = HashSet::new();
    let bound = (rpo.len() + 2) * (func.blocks.len() + 2).max(8);
    let mut rounds = 0usize;
    loop {
        let mut changed = false;
        rounds += 1;
        if rounds > bound {
            log::debug!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                " cells: dataflow did not converge"
            );
            return None;
        }
        for &block in rpo {
            let incoming_state = match merge(
                func,
                block,
                candidate,
                live,
                incoming,
                &seed_blocks,
                &out,
                &mut phis,
            ) {
                Merge::Fail => {
                    log::debug!(
                        target: crate::heap::logging::JIT_LOG_TARGET,
                        " cells: merge failed at {block}"
                    );
                    return None;
                }
                Merge::Pending => continue,
                Merge::State(state) => state,
            };
            if in_state.get(&block) != Some(&incoming_state) {
                in_state.insert(block, incoming_state);
                changed = true;
            }
            let out_state = if seed_blocks.contains_key(&block) {
                Some(State::Seed(block))
            } else {
                match outgoing(func, block, candidate, initial, incoming_state, uses) {
                    Some(out) => out,
                    None => {
                        log::debug!(
                            target: crate::heap::logging::JIT_LOG_TARGET,
                            " cells: store without a version in {block}"
                        );
                        return None;
                    }
                }
            };
            if out.get(&block) != Some(&out_state) {
                out.insert(block, out_state);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    // Every live predecessor of a parameter block must carry a version.
    for &block in rpo {
        if !phis.contains(&block) {
            continue;
        }
        for &(pred, _) in &incoming[block] {
            if live[pred] && !matches!(out.get(&pred), Some(Some(_))) {
                log::debug!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    " cells: {block} parameter has a predecessor {pred} without a version ({:?})",
                    out.get(&pred)
                );
                return None;
            }
        }
    }
    let mut ordered: Vec<Block> = Vec::new();
    for &block in rpo {
        if phis.contains(&block) {
            ordered.push(block);
        }
    }
    // The rewrite scan needs each block's *entry* version; the exit map fills
    // the predecessor edge arguments of the parameters the flow adds.
    Some((in_state, out, ordered))
}

enum Merge {
    State(Option<State>),
    Pending,
    Fail,
}

#[allow(clippy::too_many_arguments)]
fn merge(
    func: &Func,
    block: Block,
    candidate: &Candidate,
    live: &SecondaryMap<Block, bool>,
    incoming: &SecondaryMap<Block, Vec<(Block, usize)>>,
    seeds: &HashMap<Block, Value>,
    out: &HashMap<Block, Option<State>>,
    phis: &mut HashSet<Block>,
) -> Merge {
    // The allocation redefines the cell, so a block that contains it starts
    // without a carried version: the allocation sets the initial value.
    if block == candidate.block {
        return Merge::State(None);
    }
    // An OSR block seeds from the baseline frame's cell.
    if seeds.contains_key(&block) {
        return Merge::State(None);
    }
    let mut values: Vec<State> = Vec::new();
    let mut has_none = false;
    let mut pending = false;
    let mut saw = false;
    for &(pred, _) in &incoming[block] {
        if !live[pred] {
            continue;
        }
        saw = true;
        match out.get(&pred) {
            None => pending = true,
            Some(None) => has_none = true,
            Some(Some(state)) => values.push(*state),
        }
    }
    if !saw {
        return Merge::State(None);
    }
    if values.is_empty() {
        return if pending {
            Merge::Pending
        } else {
            Merge::State(None)
        };
    }
    if has_none {
        // The cell is defined on one incoming path and not on another. The
        // value is then not defined on entry: exactly what must-definedness
        // says. A block that really uses the cell has a parameter of the
        // group, so it does not take this path (and would fail later rather
        // than guess).
        return Merge::State(None);
    }
    if phis.contains(&block) {
        return Merge::State(Some(State::Phi(block)));
    }
    if values.iter().all(|state| *state == values[0]) {
        return Merge::State(Some(values[0]));
    }
    if block == func.entry {
        log::debug!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            " cells: the entry would need a value parameter"
        );
        return Merge::Fail;
    }
    phis.insert(block);
    Merge::State(Some(State::Phi(block)))
}

/// The cell's version on exit from `block`, or `None` when the block cannot
/// carry one. A write with no carried version means the dataflow missed a
/// path, which aborts the group rather than guessing.
fn outgoing(
    func: &Func,
    block: Block,
    candidate: &Candidate,
    initial: Value,
    incoming: Option<State>,
    uses: &Uses,
) -> Option<Option<State>> {
    let mut state = incoming;
    for &inst in &func.blocks[block].insts {
        if inst == candidate.alloc {
            state = Some(State::Val(initial));
        } else if let Some(&value) = uses.store_values.get(&inst) {
            state?;
            state = Some(State::Val(value));
        }
    }
    Some(state)
}

fn block_of(func: &Func, target: Inst) -> Block {
    for (block, data) in func.blocks.iter() {
        if data.insts.contains(&target) {
            return block;
        }
    }
    // Only reachable, placed instructions are passed here.
    func.entry
}

/// Applies a fully decided plan.
fn apply(func: &mut Func, plan: Plan) {
    // 1. Value parameters at the joins where versions differ.
    let mut phi_value: HashMap<Block, Value> = HashMap::new();
    for &block in &plan.phis {
        let param = func.append_param(block, Type::Any);
        phi_value.insert(block, param);
    }

    // 2. Seed loads on the OSR edges.
    let mut seed_value: HashMap<Block, Value> = HashMap::new();
    for &(block, cell) in &plan.seeds {
        let value = func
            .push_inst(block, Op::LoadCell, &[cell], Some(Type::Any), None)
            .expect("a load has a result");
        seed_value.insert(block, value);
    }

    // 3. Pass each predecessor's version on every edge to a parameter block.
    for &block in &plan.phis {
        let Some(&param) = phi_value.get(&block) else {
            continue;
        };
        let pos = func
            .params(block)
            .iter()
            .position(|&p| p == param)
            .expect("the parameter was appended");
        let preds: Vec<(Block, usize)> = edge_sites(func).get(block).cloned().unwrap_or_default();
        for (pred, k) in preds {
            let value = match plan.out.get(&pred) {
                Some(Some(state)) => resolve(*state, &phi_value, &seed_value),
                // An unreachable predecessor passes anything: a tagged
                // constant keeps the edge well formed. A reachable one left
                // without a version would be a solver bug.
                _ => {
                    debug_assert!(!is_reachable_block(func, pred));
                    placeholder(func, pred)
                }
            };
            set_edge_arg(func, pred, k, pos, value);
        }
    }

    // 4. The true constant that replaces each `variable?` test, and the
    //    remat markers that rebuild the cell at each frame that names it.
    let mut subst = Subst::default();
    for &(block, inst, result) in &plan.tests {
        let value = true_bool(func, block, inst);
        if subst.resolve(result) == result {
            subst.add(result, value);
        }
    }
    for site in &plan.frames {
        let mut chain = Vec::new();
        let mut cur = match site.before {
            Some(inst) => func.insts[inst].frame,
            None => func.blocks[site.block].term.as_ref().and_then(term_frame),
        };
        let mut has_member = false;
        while let Some(fs) = cur {
            let data = &func.frames[fs];
            has_member |= data
                .registers
                .iter()
                .any(|&(_, value)| plan.members.contains(&value));
            chain.push(fs);
            cur = data.parent;
        }
        // A shared frame may already have been rewritten from another site
        // with the same version; nothing is left to do then.
        if !has_member {
            continue;
        }
        let operand = resolve(site.version, &phi_value, &seed_value);
        let marker = match site.before {
            Some(inst) => insert_before(
                func,
                site.block,
                inst,
                Op::Remat(RematKind::Variable),
                &[operand],
                Type::Any,
            ),
            None => func
                .push_inst(
                    site.block,
                    Op::Remat(RematKind::Variable),
                    &[operand],
                    Some(Type::Any),
                    None,
                )
                .expect("a marker has a result"),
        };
        for &fs in &chain {
            for (_, value) in func.frames[fs].registers.iter_mut() {
                if plan.members.contains(value) {
                    *value = marker;
                }
            }
        }
    }
    for (result, version) in &plan.loads {
        let value = resolve(*version, &phi_value, &seed_value);
        if subst.resolve(*result) == *result {
            subst.add(*result, value);
        }
    }
    replace_uses(func, &subst);

    // 5. Delete the rewritten instructions.
    for (_, data) in func.blocks.iter_mut() {
        data.insts.retain(|inst| !plan.dead.contains(inst));
    }

    // 6. The cell's parameters and the edge arguments that fed them.
    remove_params(func, &plan.params);
}

fn resolve(state: State, phis: &HashMap<Block, Value>, seeds: &HashMap<Block, Value>) -> Value {
    match state {
        State::Val(value) => value,
        State::Phi(block) => phis[&block],
        State::Seed(block) => seeds[&block],
    }
}

fn is_reachable_block(func: &Func, block: Block) -> bool {
    func.reverse_postorder().contains(&block)
}

fn placeholder(func: &mut Func, block: Block) -> Value {
    func.push_inst(
        block,
        Op::Const(Constant::Bits(0)),
        &[],
        Some(Type::Any),
        None,
    )
    .expect("a constant has a result")
}

/// Sets argument `pos` of edge `k` of `pred`'s terminator, padding with
/// placeholders up to `pos`.
fn set_edge_arg(func: &mut Func, pred: Block, k: usize, pos: usize, value: Value) {
    let Some(mut term) = func.blocks[pred].term.take() else {
        return;
    };
    {
        let mut edges = edges_mut(&mut term);
        if let Some(edge) = edges.get_mut(k) {
            let mut args: Vec<Value> = func.edge_args(edge).to_vec();
            while args.len() <= pos {
                let at = placeholder(func, pred);
                args.push(at);
            }
            args[pos] = value;
            edge.args = EntityList::from_slice(&args, &mut func.pool);
        }
    }
    func.blocks[pred].term = Some(term);
}

/// Pushes a true `Bool` before `before`. A `Bool` is a raw flag, so a tagged
/// fixnum tested as a fixnum is the truth.
fn true_bool(func: &mut Func, block: Block, before: Inst) -> Value {
    let bits = SchemeValue::from_i32(0).bits();
    let constant = insert_before(
        func,
        block,
        before,
        Op::Const(Constant::Bits(bits)),
        &[],
        Type::Any,
    );
    insert_before(func, block, before, Op::TestInt32, &[constant], Type::Bool)
}

/// Pushes an instruction and moves it in front of `before`. The instruction
/// arena is append-only; only the block's order changes.
fn insert_before(
    func: &mut Func,
    block: Block,
    before: Inst,
    op: Op,
    args: &[Value],
    ty: Type,
) -> Value {
    let value = func
        .push_inst(block, op, args, Some(ty), None)
        .expect("an operation with a result returns one");
    let list = &mut func.blocks[block].insts;
    let new = list.pop().expect("the instruction was just pushed");
    let at = list
        .iter()
        .position(|&inst| inst == before)
        .expect("the folded instruction is in its block");
    list.insert(at, new);
    value
}

/// Removes the listed parameters and the matching arguments of every edge
/// into their blocks, then renumbers the remaining parameters.
fn remove_params(func: &mut Func, params: &[(Block, u32)]) {
    if params.is_empty() {
        return;
    }
    let mut by_block: HashMap<Block, Vec<u32>> = HashMap::new();
    for &(block, index) in params {
        by_block.entry(block).or_default().push(index);
    }
    for list in by_block.values_mut() {
        list.sort_unstable();
        list.dedup();
    }
    // Drop the edge arguments first, while positions still match the old
    // parameter lists. The terminators are taken out of the blocks so the
    // operand pool can be borrowed.
    let mut terms: Vec<(Block, Terminator)> = Vec::new();
    for (pred, data) in func.blocks.iter_mut() {
        if let Some(term) = data.term.take() {
            terms.push((pred, term));
        }
    }
    for (_, term) in terms.iter_mut() {
        for edge in edges_mut(term) {
            let Some(remove) = by_block.get(&edge.block) else {
                continue;
            };
            let args: Vec<Value> = edge.args.as_slice(&func.pool).to_vec();
            let kept: Vec<Value> = args
                .iter()
                .enumerate()
                .filter(|(index, _)| !remove.contains(&(*index as u32)))
                .map(|(_, &value)| value)
                .collect();
            edge.args = EntityList::from_slice(&kept, &mut func.pool);
        }
    }
    for (pred, term) in terms {
        func.blocks[pred].term = Some(term);
    }
    // Then the parameters themselves, renumbering the survivors.
    for (&block, remove) in &by_block {
        let params = func.params(block).to_vec();
        let kept: Vec<Value> = params
            .iter()
            .enumerate()
            .filter(|(index, _)| !remove.contains(&(*index as u32)))
            .map(|(_, &value)| value)
            .collect();
        for (index, &value) in kept.iter().enumerate() {
            func.values[value].def = ValueDef::Param {
                block,
                index: index as u32,
            };
        }
        func.blocks[block].params = EntityList::from_slice(&kept, &mut func.pool);
    }
}

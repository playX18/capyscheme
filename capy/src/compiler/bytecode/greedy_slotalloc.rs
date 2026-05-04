use crate::{
    bytecode::bytecode_list::VirtualRegister,
    cps::{
        linear::{
            BlockId, BranchTarget, LinearAtom, LinearProgram, Procedure, Terminator, ValueId,
        },
        term::BranchHint,
    },
};
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Write as _},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallWindow {
    pub start: VirtualRegister,
    pub size: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallSiteKind {
    Call,
    TailCall,
    ReifiedContinuation,
}

impl fmt::Display for CallSiteKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Call => write!(f, "call"),
            Self::TailCall => write!(f, "tail-call"),
            Self::ReifiedContinuation => write!(f, "reified-continuation"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallSitePos {
    Terminator,
    BranchTarget { index: usize },
}

impl fmt::Display for CallSitePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Terminator => write!(f, "terminator"),
            Self::BranchTarget { index } => write!(f, "branch-target{index}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallWindowUse {
    pub source: BlockId,
    pub site: CallSitePos,
    pub kind: CallSiteKind,
    pub start: VirtualRegister,
    pub size: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BranchCopyTemp {
    pub source: BlockId,
    pub edge: usize,
    pub target: BlockId,
    pub slot: VirtualRegister,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveRange {
    pub start: usize,
    pub end: usize,
}

impl LiveRange {
    fn include(&mut self, point: usize) {
        self.start = self.start.min(point);
        self.end = self.end.max(point);
    }
}

#[derive(Debug, Clone)]
pub struct Liveness {
    block_live_in: HashMap<BlockId, HashSet<ValueId>>,
    block_live_out: HashMap<BlockId, HashSet<ValueId>>,
    live_ranges: HashMap<ValueId, LiveRange>,
    empty: HashSet<ValueId>,
}

impl Liveness {
    pub fn live_in(&self, block: BlockId) -> &HashSet<ValueId> {
        self.block_live_in.get(&block).unwrap_or(&self.empty)
    }

    pub fn live_out(&self, block: BlockId) -> &HashSet<ValueId> {
        self.block_live_out.get(&block).unwrap_or(&self.empty)
    }

    pub fn live_range(&self, value: ValueId) -> Option<LiveRange> {
        self.live_ranges.get(&value).copied()
    }

    pub fn live_ranges(&self) -> &HashMap<ValueId, LiveRange> {
        &self.live_ranges
    }
}

#[derive(Debug, Clone)]
pub struct SlotAllocation {
    registers: HashMap<ValueId, VirtualRegister>,
    nlocals: usize,
    call_window: Option<CallWindow>,
    call_window_uses: Vec<CallWindowUse>,
    branch_copy_temps: Vec<BranchCopyTemp>,
    liveness: Liveness,
}

impl SlotAllocation {
    pub fn register(&self, value: ValueId) -> VirtualRegister {
        self.registers
            .get(&value)
            .copied()
            .unwrap_or_else(|| panic!("value {value:?} has no allocated slot"))
    }

    pub fn slot(&self, value: ValueId) -> Option<VirtualRegister> {
        self.registers.get(&value).copied()
    }

    pub fn registers(&self) -> &HashMap<ValueId, VirtualRegister> {
        &self.registers
    }

    pub fn nlocals(&self) -> usize {
        self.nlocals
    }

    pub fn call_window(&self) -> Option<CallWindow> {
        self.call_window
    }

    pub fn call_window_uses(&self) -> &[CallWindowUse] {
        &self.call_window_uses
    }

    pub fn branch_copy_temps(&self) -> &[BranchCopyTemp] {
        &self.branch_copy_temps
    }

    pub fn liveness(&self) -> &Liveness {
        &self.liveness
    }
}

pub fn render_program_allocations<'gc>(program: &LinearProgram<'gc>) -> String {
    let mut out = String::new();
    writeln!(out, "(slot-allocations").unwrap();
    for (index, procedure) in program.procedures.iter().enumerate() {
        let allocation = allocate_slots(procedure);
        render_procedure_allocation(&mut out, index, procedure, &allocation);
    }
    writeln!(out, ")").unwrap();
    out
}

fn render_procedure_allocation<'gc>(
    out: &mut String,
    index: usize,
    procedure: &Procedure<'gc>,
    allocation: &SlotAllocation,
) {
    writeln!(out, "  (procedure {index}").unwrap();
    writeln!(out, "    (entry block{})", procedure.entry.0).unwrap();
    writeln!(out, "    (nlocals {})", allocation.nlocals()).unwrap();
    match allocation.call_window() {
        Some(window) => {
            writeln!(out, "    (call-window {} {})", window.start, window.size).unwrap()
        }
        None => writeln!(out, "    (call-window #f)").unwrap(),
    }
    writeln!(out, "    (call-window-uses").unwrap();
    for call in allocation.call_window_uses() {
        writeln!(
            out,
            "      (block{} {} {} {} {})",
            call.source.0, call.site, call.kind, call.start, call.size
        )
        .unwrap();
    }
    writeln!(out, "    )").unwrap();
    writeln!(out, "    (parallel-copy-tmps").unwrap();
    for temp in allocation.branch_copy_temps() {
        writeln!(
            out,
            "      (block{} edge{} block{} {})",
            temp.source.0, temp.edge, temp.target.0, temp.slot
        )
        .unwrap();
    }
    writeln!(out, "    )").unwrap();
    writeln!(out, "    (registers").unwrap();

    let mut registers = allocation
        .registers()
        .iter()
        .map(|(value, register)| (*value, *register))
        .collect::<Vec<_>>();
    registers.sort_by_key(|(value, _)| value.0);

    for (value, register) in registers {
        match allocation.liveness().live_range(value) {
            Some(range) => writeln!(
                out,
                "      (%v{} {} (live {} {}))",
                value.0, register, range.start, range.end
            )
            .unwrap(),
            None => writeln!(out, "      (%v{} {} (live #f))", value.0, register).unwrap(),
        }
    }

    writeln!(out, "    )").unwrap();
    writeln!(out, "  )").unwrap();
}

#[derive(Debug, Clone)]
struct BlockUseDef {
    uses: HashSet<ValueId>,
    defs: HashSet<ValueId>,
}

#[derive(Debug, Clone)]
struct BlockLayout {
    start: usize,
    instruction_use_points: Vec<usize>,
    instruction_def_points: Vec<usize>,
    terminator: usize,
}

#[derive(Debug, Clone, Copy)]
struct ActiveInterval {
    end: usize,
    slot: usize,
}

#[derive(Debug, Clone, Copy)]
struct CopyAffinity {
    other: ValueId,
    weight: i64,
}

#[derive(Debug, Clone, Copy)]
struct SlotAffinity {
    slot: usize,
    weight: i64,
}

#[derive(Debug, Clone)]
struct CallSite<'gc> {
    source: BlockId,
    site: CallSitePos,
    kind: CallSiteKind,
    args: Vec<LinearAtom<'gc>>,
    live: HashSet<ValueId>,
}

pub fn allocate_slots<'gc>(procedure: &Procedure<'gc>) -> SlotAllocation {
    let liveness = analyze_liveness(procedure);
    let call_window_size = max_call_window_size(procedure);
    let fixed = fixed_argument_registers(procedure);
    let copy_affinities = copy_affinities(procedure);
    let call_sites = call_sites(procedure, &liveness);
    let call_affinities = call_affinities(&call_sites, call_window_size);

    let mut registers = fixed;
    let mut active: Vec<ActiveInterval> = Vec::new();
    let mut max_local_slot = None;
    let mut intervals = liveness
        .live_ranges()
        .iter()
        .map(|(value, range)| (*value, *range))
        .filter(|(value, _)| !registers.contains_key(value))
        .collect::<Vec<_>>();
    intervals.sort_by_key(|(value, range)| (range.start, range.end, value.0));

    for (value, range) in intervals {
        active.retain(|active| active.end > range.start);
        let slot = choose_slot(
            value,
            &active,
            &registers,
            &copy_affinities,
            &call_affinities,
            call_window_size,
            max_local_slot,
        );
        registers.insert(value, VirtualRegister::for_local(slot));
        max_local_slot = Some(max_local_slot.map_or(slot, |max| max.max(slot)));
        active.push(ActiveInterval {
            end: range.end,
            slot,
        });
    }

    let allocated_locals = max_local_slot.map_or(0, |slot| slot + 1);
    let mut nlocals = allocated_locals;
    let branch_copy_temps = branch_copy_temps(procedure, &liveness, &registers, nlocals);
    for temp in &branch_copy_temps {
        nlocals = nlocals.max(temp.slot.to_local() as usize + 1);
    }
    let call_window_uses = call_window_uses(&call_sites, &registers, call_window_size, nlocals);
    for call in &call_window_uses {
        nlocals = nlocals.max(call.start.to_local() as usize + call.size);
    }
    let call_window = call_window_uses
        .iter()
        .map(|call| call.start.to_local() as usize + call.size)
        .max()
        .map(|size| CallWindow {
            start: VirtualRegister::for_local(0),
            size,
        });

    SlotAllocation {
        registers,
        nlocals,
        call_window,
        call_window_uses,
        branch_copy_temps,
        liveness,
    }
}

pub fn analyze_liveness<'gc>(procedure: &Procedure<'gc>) -> Liveness {
    let use_defs = block_use_defs(procedure);
    let mut live_in = procedure
        .blocks
        .iter()
        .map(|block| (block.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    let mut live_out = live_in.clone();
    let block_params = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block.params.as_slice()))
        .collect::<HashMap<_, _>>();

    let mut changed = true;
    while changed {
        changed = false;

        for block in procedure.blocks.iter().rev() {
            let mut next_out = HashSet::new();
            for edge in local_successor_edges(block) {
                if let Some(successor_live_in) = live_in.get(&edge.target) {
                    let successor_params = block_params.get(&edge.target).copied().unwrap_or(&[]);
                    next_out.extend(
                        successor_live_in
                            .iter()
                            .copied()
                            .filter(|value| !successor_params.contains(value)),
                    );
                }
                for arg in edge.args {
                    if let LinearAtom::Local(value) = arg {
                        next_out.insert(*value);
                    }
                }
            }

            let use_def = &use_defs[&block.id];
            let mut next_in = use_def.uses.clone();
            next_in.extend(next_out.difference(&use_def.defs).copied());

            if live_out.get(&block.id) != Some(&next_out) {
                live_out.insert(block.id, next_out);
                changed = true;
            }
            if live_in.get(&block.id) != Some(&next_in) {
                live_in.insert(block.id, next_in);
                changed = true;
            }
        }
    }

    let layout = program_layout(procedure);
    let live_ranges = live_ranges(procedure, &live_in, &live_out, &layout);

    Liveness {
        block_live_in: live_in,
        block_live_out: live_out,
        live_ranges,
        empty: HashSet::new(),
    }
}

fn fixed_argument_registers<'gc>(procedure: &Procedure<'gc>) -> HashMap<ValueId, VirtualRegister> {
    let param_offset = usize::from(procedure.return_cont.is_some());
    let mut fixed = HashMap::new();

    if let Some(return_cont) = procedure.return_cont {
        fixed.insert(return_cont, VirtualRegister::for_argument(0));
    }

    fixed.extend(
        procedure
            .params
            .iter()
            .copied()
            .enumerate()
            .map(|(index, value)| (value, VirtualRegister::for_argument(index + param_offset))),
    );

    if let Some(variadic) = procedure.variadic {
        fixed.insert(
            variadic,
            VirtualRegister::for_argument(procedure.params.len() + param_offset),
        );
    }

    fixed
}

fn block_use_defs<'gc>(procedure: &Procedure<'gc>) -> HashMap<BlockId, BlockUseDef> {
    let fixed_arguments = fixed_argument_registers(procedure)
        .keys()
        .copied()
        .collect::<HashSet<_>>();
    procedure
        .blocks
        .iter()
        .map(|block| {
            let mut uses = HashSet::new();
            let mut defs = HashSet::new();

            for param in &block.params {
                if block.id != procedure.entry || !fixed_arguments.contains(param) {
                    defs.insert(*param);
                }
            }

            let mut defined = defs.clone();
            for instruction in &block.instructions {
                for value in local_uses(instruction.uses()) {
                    if !defined.contains(&value) {
                        uses.insert(value);
                    }
                }
                if let Some(def) = instruction.def() {
                    defined.insert(def);
                    defs.insert(def);
                }
            }

            for value in local_uses(block.terminator.uses()) {
                if !defined.contains(&value) {
                    uses.insert(value);
                }
            }

            (block.id, BlockUseDef { uses, defs })
        })
        .collect()
}

fn live_ranges<'gc>(
    procedure: &Procedure<'gc>,
    live_in: &HashMap<BlockId, HashSet<ValueId>>,
    live_out: &HashMap<BlockId, HashSet<ValueId>>,
    layout: &HashMap<BlockId, BlockLayout>,
) -> HashMap<ValueId, LiveRange> {
    let mut ranges = HashMap::new();

    for block in &procedure.blocks {
        let block_layout = &layout[&block.id];

        for value in live_in.get(&block.id).into_iter().flatten() {
            include_range_point(&mut ranges, *value, block_layout.start);
        }
        for param in &block.params {
            include_range_point(&mut ranges, *param, block_layout.start);
        }

        for (index, instruction) in block.instructions.iter().enumerate() {
            let use_point = block_layout.instruction_use_points[index];
            let def_point = block_layout.instruction_def_points[index];
            for value in local_uses(instruction.uses()) {
                include_range_point(&mut ranges, value, use_point);
            }
            if let Some(def) = instruction.def() {
                include_range_point(&mut ranges, def, def_point);
            }
        }

        for value in local_uses(block.terminator.uses()) {
            include_range_point(&mut ranges, value, block_layout.terminator);
        }
        for value in live_out.get(&block.id).into_iter().flatten() {
            include_range_point(&mut ranges, *value, block_layout.terminator);
        }
    }

    ranges
}

fn include_range_point(ranges: &mut HashMap<ValueId, LiveRange>, value: ValueId, point: usize) {
    ranges
        .entry(value)
        .and_modify(|range| range.include(point))
        .or_insert(LiveRange {
            start: point,
            end: point,
        });
}

fn program_layout<'gc>(procedure: &Procedure<'gc>) -> HashMap<BlockId, BlockLayout> {
    let mut next_index = 0;
    let mut layout = HashMap::new();

    for block in &procedure.blocks {
        let start = next_index * 2;
        let mut instruction_use_points = Vec::with_capacity(block.instructions.len());
        let mut instruction_def_points = Vec::with_capacity(block.instructions.len());
        for _ in &block.instructions {
            instruction_use_points.push(next_index * 2);
            instruction_def_points.push(next_index * 2 + 1);
            next_index += 1;
        }
        let terminator = next_index * 2;
        next_index += 1;

        layout.insert(
            block.id,
            BlockLayout {
                start,
                instruction_use_points,
                instruction_def_points,
                terminator,
            },
        );
    }

    layout
}

fn local_uses<'gc>(uses: Vec<LinearAtom<'gc>>) -> impl Iterator<Item = ValueId> {
    uses.into_iter().filter_map(|atom| match atom {
        LinearAtom::Local(value) => Some(value),
        LinearAtom::Constant(_) => None,
    })
}

fn max_call_window_size<'gc>(procedure: &Procedure<'gc>) -> usize {
    let mut max_args = 0;
    for block in &procedure.blocks {
        max_args = max_args.max(terminator_call_window_size(&block.terminator));
    }
    max_args
}

fn terminator_call_window_size<'gc>(terminator: &Terminator<'gc>) -> usize {
    match terminator {
        Terminator::Call { args, .. } | Terminator::TailCall { args, .. } => args.len(),
        Terminator::Jump { .. } => 0,
        Terminator::Branch {
            consequent,
            alternative,
            ..
        } => branch_target_call_window_size(consequent)
            .max(branch_target_call_window_size(alternative)),
        Terminator::Switch { cases, default, .. } => cases
            .iter()
            .map(|case| branch_target_call_window_size(&case.target))
            .chain(std::iter::once(branch_target_call_window_size(default)))
            .max()
            .unwrap_or(0),
    }
}

fn branch_target_call_window_size<'gc>(target: &BranchTarget<'gc>) -> usize {
    match target {
        BranchTarget::Local { .. } => 0,
        BranchTarget::Reified { args, .. } => args.len(),
    }
}

struct LocalSuccessorEdge<'a, 'gc> {
    target: BlockId,
    args: &'a [LinearAtom<'gc>],
}

fn local_successor_edges<'a, 'gc>(
    block: &'a crate::cps::linear::Block<'gc>,
) -> Vec<LocalSuccessorEdge<'a, 'gc>> {
    let mut edges = Vec::new();
    match &block.terminator {
        Terminator::Jump { target, args } => edges.push(LocalSuccessorEdge {
            target: *target,
            args,
        }),
        Terminator::Branch {
            consequent,
            alternative,
            ..
        } => {
            push_local_successor_edge(&mut edges, consequent);
            push_local_successor_edge(&mut edges, alternative);
        }
        Terminator::Switch { cases, default, .. } => {
            for case in cases {
                push_local_successor_edge(&mut edges, &case.target);
            }
            push_local_successor_edge(&mut edges, default);
        }
        Terminator::Call { .. } | Terminator::TailCall { .. } => {}
    }
    edges
}

fn push_local_successor_edge<'a, 'gc>(
    edges: &mut Vec<LocalSuccessorEdge<'a, 'gc>>,
    target: &'a BranchTarget<'gc>,
) {
    if let BranchTarget::Local { block, args } = target {
        edges.push(LocalSuccessorEdge {
            target: *block,
            args,
        });
    }
}

fn branch_copy_temps<'gc>(
    procedure: &Procedure<'gc>,
    liveness: &Liveness,
    registers: &HashMap<ValueId, VirtualRegister>,
    base_nlocals: usize,
) -> Vec<BranchCopyTemp> {
    let target_params = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block.params.as_slice()))
        .collect::<HashMap<_, _>>();
    let mut temps = Vec::new();

    for block in &procedure.blocks {
        for edge in local_parallel_copy_edges(block) {
            let Some(params) = target_params.get(&edge.target) else {
                continue;
            };
            let mut occupied = HashSet::new();
            let mut live = liveness.live_out(block.id).clone();
            live.extend(local_uses(block.terminator.uses()));

            for value in live {
                if let Some(slot) = local_slot(registers.get(&value).copied()) {
                    occupied.insert(slot);
                }
            }

            for value in local_uses(edge.args.iter().copied().collect()) {
                if let Some(slot) = local_slot(registers.get(&value).copied()) {
                    occupied.insert(slot);
                }
            }

            for param in params.iter().copied().take(edge.args.len()) {
                if let Some(slot) = local_slot(registers.get(&param).copied()) {
                    occupied.insert(slot);
                }
            }

            let slot = (0..=base_nlocals)
                .find(|slot| !occupied.contains(slot))
                .expect("there is always a fresh parallel-copy temp slot candidate");
            temps.push(BranchCopyTemp {
                source: block.id,
                edge: edge.edge,
                target: edge.target,
                slot: VirtualRegister::for_local(slot),
            });
        }
    }

    temps
}

fn local_slot(register: Option<VirtualRegister>) -> Option<usize> {
    let register = register?;
    register.is_local().then_some(register.to_local() as usize)
}

struct LocalParallelCopyEdge<'a, 'gc> {
    edge: usize,
    target: BlockId,
    args: &'a [LinearAtom<'gc>],
}

fn local_parallel_copy_edges<'a, 'gc>(
    block: &'a crate::cps::linear::Block<'gc>,
) -> Vec<LocalParallelCopyEdge<'a, 'gc>> {
    let mut edges = Vec::new();
    match &block.terminator {
        Terminator::Jump { target, args } => {
            if !args.is_empty() {
                edges.push(LocalParallelCopyEdge {
                    edge: 0,
                    target: *target,
                    args,
                });
            }
        }
        Terminator::Branch {
            consequent,
            alternative,
            ..
        } => {
            push_local_parallel_copy_edge(&mut edges, 0, consequent);
            push_local_parallel_copy_edge(&mut edges, 1, alternative);
        }
        Terminator::Switch { cases, default, .. } => {
            for (index, case) in cases.iter().enumerate() {
                push_local_parallel_copy_edge(&mut edges, index, &case.target);
            }
            push_local_parallel_copy_edge(&mut edges, cases.len(), default);
        }
        Terminator::Call { .. } | Terminator::TailCall { .. } => {}
    }
    edges
}

fn push_local_parallel_copy_edge<'a, 'gc>(
    edges: &mut Vec<LocalParallelCopyEdge<'a, 'gc>>,
    edge: usize,
    target: &'a BranchTarget<'gc>,
) {
    let BranchTarget::Local { block, args } = target else {
        return;
    };
    if args.is_empty() {
        return;
    }
    edges.push(LocalParallelCopyEdge {
        edge,
        target: *block,
        args,
    });
}

fn call_sites<'gc>(procedure: &Procedure<'gc>, liveness: &Liveness) -> Vec<CallSite<'gc>> {
    let mut sites = Vec::new();
    for block in &procedure.blocks {
        match &block.terminator {
            Terminator::Call {
                callee, retk, args, ..
            } if !args.is_empty() => {
                let mut live = liveness.live_out(block.id).clone();
                insert_local_atom(&mut live, *callee);
                insert_local_atom(&mut live, *retk);
                sites.push(CallSite {
                    source: block.id,
                    site: CallSitePos::Terminator,
                    kind: CallSiteKind::Call,
                    args: args.iter().copied().collect(),
                    live,
                });
            }
            Terminator::TailCall { callee, args, .. } if !args.is_empty() => {
                let mut live = liveness.live_out(block.id).clone();
                insert_local_atom(&mut live, *callee);
                sites.push(CallSite {
                    source: block.id,
                    site: CallSitePos::Terminator,
                    kind: CallSiteKind::TailCall,
                    args: args.iter().copied().collect(),
                    live,
                });
            }
            Terminator::Branch {
                consequent,
                alternative,
                ..
            } => {
                push_reified_call_site(
                    &mut sites,
                    block.id,
                    CallSitePos::BranchTarget { index: 0 },
                    consequent,
                );
                push_reified_call_site(
                    &mut sites,
                    block.id,
                    CallSitePos::BranchTarget { index: 1 },
                    alternative,
                );
            }
            Terminator::Switch { cases, default, .. } => {
                for (index, case) in cases.iter().enumerate() {
                    push_reified_call_site(
                        &mut sites,
                        block.id,
                        CallSitePos::BranchTarget { index },
                        &case.target,
                    );
                }
                push_reified_call_site(
                    &mut sites,
                    block.id,
                    CallSitePos::BranchTarget { index: cases.len() },
                    default,
                );
            }
            Terminator::Call { .. } | Terminator::TailCall { .. } | Terminator::Jump { .. } => {}
        }
    }
    sites
}

fn push_reified_call_site<'gc>(
    sites: &mut Vec<CallSite<'gc>>,
    source: BlockId,
    site: CallSitePos,
    target: &BranchTarget<'gc>,
) {
    let BranchTarget::Reified { continuation, args } = target else {
        return;
    };
    if args.is_empty() {
        return;
    }
    let mut live = HashSet::new();
    insert_local_atom(&mut live, *continuation);
    sites.push(CallSite {
        source,
        site,
        kind: CallSiteKind::ReifiedContinuation,
        args: args.iter().copied().collect(),
        live,
    });
}

fn insert_local_atom<'gc>(values: &mut HashSet<ValueId>, atom: LinearAtom<'gc>) {
    if let LinearAtom::Local(value) = atom {
        values.insert(value);
    }
}

fn call_window_slot_contains<'gc>(call: &CallSite<'gc>, slot: usize, value: ValueId) -> bool {
    call.args.get(slot).copied() == Some(LinearAtom::Local(value))
}

fn call_affinities<'gc>(
    call_sites: &[CallSite<'gc>],
    call_window_size: usize,
) -> HashMap<ValueId, Vec<SlotAffinity>> {
    let mut affinities = HashMap::new();

    for call in call_sites {
        if call.args.len() > call_window_size {
            continue;
        }
        for (index, arg) in call.args.iter().copied().enumerate() {
            if let LinearAtom::Local(value) = arg {
                add_slot_affinity(&mut affinities, value, index, 24);
            }
        }
    }

    affinities
}

fn add_slot_affinity(
    affinities: &mut HashMap<ValueId, Vec<SlotAffinity>>,
    value: ValueId,
    slot: usize,
    weight: i64,
) {
    affinities
        .entry(value)
        .or_default()
        .push(SlotAffinity { slot, weight });
}

fn call_window_uses<'gc>(
    call_sites: &[CallSite<'gc>],
    registers: &HashMap<ValueId, VirtualRegister>,
    call_window_size: usize,
    base_nlocals: usize,
) -> Vec<CallWindowUse> {
    if call_window_size == 0 {
        return vec![];
    }

    call_sites
        .iter()
        .filter(|call| !call.args.is_empty() && call.args.len() <= call_window_size)
        .map(|call| {
            let start = choose_call_window_start(call, registers, base_nlocals);
            CallWindowUse {
                source: call.source,
                site: call.site,
                kind: call.kind,
                start: VirtualRegister::for_local(start),
                size: call.args.len(),
            }
        })
        .collect()
}

fn choose_call_window_start<'gc>(
    call: &CallSite<'gc>,
    registers: &HashMap<ValueId, VirtualRegister>,
    base_nlocals: usize,
) -> usize {
    (0..=base_nlocals)
        .filter(|start| call_window_start_is_safe(call, registers, *start))
        .max_by_key(|start| call_window_start_score(call, registers, *start, base_nlocals))
        .expect("fresh call window after allocated locals is always safe")
}

fn call_window_start_is_safe<'gc>(
    call: &CallSite<'gc>,
    registers: &HashMap<ValueId, VirtualRegister>,
    start: usize,
) -> bool {
    let mut protected = call.live.clone();
    protected.extend(local_uses(call.args.clone()));

    for value in protected {
        let Some(slot) = local_slot(registers.get(&value).copied()) else {
            continue;
        };
        if (start..start + call.args.len()).contains(&slot)
            && !call_window_slot_contains(call, slot - start, value)
        {
            return false;
        }
    }

    true
}

fn call_window_start_score<'gc>(
    call: &CallSite<'gc>,
    registers: &HashMap<ValueId, VirtualRegister>,
    start: usize,
    base_nlocals: usize,
) -> i64 {
    let mut score = -(start as i64);
    let end = start + call.args.len();
    if end > base_nlocals {
        score -= ((end - base_nlocals) as i64) * 8;
    }

    for (index, arg) in call.args.iter().copied().enumerate() {
        let LinearAtom::Local(value) = arg else {
            continue;
        };
        if registers.get(&value).copied() == Some(VirtualRegister::for_local(start + index)) {
            score += 100;
        }
    }

    score
}

fn copy_affinities<'gc>(procedure: &Procedure<'gc>) -> HashMap<ValueId, Vec<CopyAffinity>> {
    let block_params = procedure
        .blocks
        .iter()
        .map(|block| (block.id, block.params.as_slice()))
        .collect::<HashMap<_, _>>();
    let mut affinities = HashMap::new();

    for block in &procedure.blocks {
        match &block.terminator {
            Terminator::Jump { target, args } => {
                add_local_target_affinities(&mut affinities, &block_params, *target, args, 30);
            }
            Terminator::Branch {
                consequent,
                alternative,
                hints,
                ..
            } => {
                add_branch_target_affinities(
                    &mut affinities,
                    &block_params,
                    consequent,
                    branch_weight(hints[0]),
                );
                add_branch_target_affinities(
                    &mut affinities,
                    &block_params,
                    alternative,
                    branch_weight(hints[1]),
                );
            }
            Terminator::Switch { cases, default, .. } => {
                for case in cases {
                    add_branch_target_affinities(&mut affinities, &block_params, &case.target, 12);
                }
                add_branch_target_affinities(&mut affinities, &block_params, default, 12);
            }
            Terminator::Call { .. } | Terminator::TailCall { .. } => {}
        }
    }

    affinities
}

fn branch_weight(hint: BranchHint) -> i64 {
    match hint {
        BranchHint::Hot => 40,
        BranchHint::Normal => 20,
        BranchHint::Cold => 4,
    }
}

fn add_branch_target_affinities<'gc>(
    affinities: &mut HashMap<ValueId, Vec<CopyAffinity>>,
    block_params: &HashMap<BlockId, &[ValueId]>,
    target: &BranchTarget<'gc>,
    weight: i64,
) {
    if let BranchTarget::Local { block, args } = target {
        add_local_target_affinities(affinities, block_params, *block, args, weight);
    }
}

fn add_local_target_affinities<'gc>(
    affinities: &mut HashMap<ValueId, Vec<CopyAffinity>>,
    block_params: &HashMap<BlockId, &[ValueId]>,
    target: BlockId,
    args: &[LinearAtom<'gc>],
    weight: i64,
) {
    let Some(params) = block_params.get(&target) else {
        return;
    };

    for (param, arg) in params.iter().copied().zip(args.iter().copied()) {
        let LinearAtom::Local(arg) = arg else {
            continue;
        };
        if param == arg {
            continue;
        }
        add_copy_affinity(affinities, param, arg, weight);
        add_copy_affinity(affinities, arg, param, weight);
    }
}

fn add_copy_affinity(
    affinities: &mut HashMap<ValueId, Vec<CopyAffinity>>,
    value: ValueId,
    other: ValueId,
    weight: i64,
) {
    affinities
        .entry(value)
        .or_default()
        .push(CopyAffinity { other, weight });
}

fn choose_slot(
    value: ValueId,
    active: &[ActiveInterval],
    registers: &HashMap<ValueId, VirtualRegister>,
    copy_affinities: &HashMap<ValueId, Vec<CopyAffinity>>,
    call_affinities: &HashMap<ValueId, Vec<SlotAffinity>>,
    call_window_size: usize,
    max_local_slot: Option<usize>,
) -> usize {
    let active_slots = active
        .iter()
        .map(|active| active.slot)
        .collect::<HashSet<_>>();
    let top_existing_slot = max_local_slot
        .map(|slot| slot + 1)
        .unwrap_or(0)
        .max(call_window_size);

    (0..=top_existing_slot)
        .filter(|slot| !active_slots.contains(slot))
        .max_by_key(|slot| {
            slot_score(
                value,
                *slot,
                registers,
                copy_affinities,
                call_affinities,
                max_local_slot,
            )
        })
        .expect("there is always at least one free local slot")
}

fn slot_score(
    value: ValueId,
    slot: usize,
    registers: &HashMap<ValueId, VirtualRegister>,
    copy_affinities: &HashMap<ValueId, Vec<CopyAffinity>>,
    call_affinities: &HashMap<ValueId, Vec<SlotAffinity>>,
    max_local_slot: Option<usize>,
) -> i64 {
    let mut score = -(slot as i64);

    if max_local_slot.is_some_and(|max| slot > max) {
        score -= 4;
    }

    for affinity in call_affinities.get(&value).into_iter().flatten() {
        if affinity.slot == slot {
            score += affinity.weight;
        }
    }

    for affinity in copy_affinities.get(&value).into_iter().flatten() {
        let Some(register) = registers.get(&affinity.other).copied() else {
            continue;
        };
        if register == VirtualRegister::for_local(slot) {
            score += affinity.weight;
        }
    }

    score
}

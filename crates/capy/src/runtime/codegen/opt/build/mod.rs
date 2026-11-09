//! Bytecode to SSA.

mod arith;
pub(crate) mod calls;
mod generics;
mod inline;
mod instance;
pub mod liveness;
mod object;
pub mod profile;
mod records;

use cranelift_entity::{EntityList, EntityRef};
use std::collections::HashSet;
use std::mem::{offset_of, size_of};

use crate::bytecode::def::{
    Instruction, MV_ERROR, OpGetSlot, OpPutSlot, switch_table_is_dense, switch_table_len,
};
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::codegen::jit::config::opt;
use crate::runtime::codegen::opt::ir::frame_state::{
    CallSite, FrameState, FrameStateData, ReturnedValues,
};
use crate::runtime::codegen::opt::ir::func::{
    Block, CaseClause, Func, OsrEntry, RestInfo, Value, ValueDef,
};
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, Edge, Effects, Helper, Inst, MemoryRegion, Op, PrimRef, Repr, ReturnValues,
    SwitchDensity, Terminator,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, CodeSource, Type};
use crate::runtime::codegen::opt::ir::verify::verify;
use crate::runtime::objectmodel::{CodeBlock, CodeBuffer, EnvRecord, Value as SchemeValue};
use crate::runtime::prims::calls::PRIM_ARITY;
use crate::runtime::prims::def::Primitive;
use crate::utils::flags;
use liveness::Liveness;

/// The registers a block's abstract state maps to IR values.
type Regs = Vec<Option<Value>>;

/// Lowers `code_block`'s bytecode to SSA, with no speculation.
///
/// `code_block` must be rooted by the caller. `mutation` is the mutation
/// scope the caller runs in: the builder reads the buffer's constants and
/// the GC references in its call links, which stay valid while no collection
/// runs. A collection cannot run while the scope has no yieldpoint or native
/// transition, and this function has neither.
///
/// Honours `CAPY_OPT_DUMP=build` (written to `CAPY_JIT_DUMP_DIR`, else
/// stderr) and runs the verifier when `CAPY_OPT_VERIFY` is set.
pub fn build<'gc>(
    mutation: MutationContext<'gc>,
    code_block: Gc<'gc, CodeBlock<'gc>>,
    root_closure: Option<Gc<'gc, crate::runtime::objectmodel::proc::Closure<'gc>>>,
) -> Result<Func<'gc>, String> {
    let buffer = code_block.buffer;
    // The exit profile says where the last compile's speculation failed too
    // often. Such a pc stays on baseline.
    let blocked = |pc: usize| code_block.speculation_blocked(pc as u32);
    let config = opt();
    let root_value = SchemeValue::from(code_block);
    // The requesting closure's free variables, when it is an instance of
    // this code block. A global read through one of these cells can fold
    // (see `fold_free_variable`)
    let free_cells: Vec<SchemeValue<'gc>> = match root_closure {
        Some(closure)
            if SchemeValue::from(crate::runtime::objectmodel::proc::Closure::code_block(
                closure,
            )) == root_value =>
        {
            (0..closure.nfree)
                .map(|index| crate::runtime::objectmodel::proc::Closure::free_get(closure, index))
                .collect()
        }
        _ => Vec::new(),
    };
    let label = crate::runtime::objectmodel::proc::procedure_label(code_block);
    let inline = || inline::InlineCx::root(true, true, root_value);

    let mut func = Builder::new(
        mutation,
        code_block.frame_size,
        &buffer,
        &blocked,
        true,
        true,
        true,
        true,
        inline(),
        true,
        &free_cells,
        &label,
    )?
    .run()?;
    prune_checks(&mut func);
    func.spec_blocked = code_block.blocked_pcs().into_iter().collect();

    if config.dump.iter().any(|stage| stage == "build") {
        dump(&func, &label);
    }
    if config.verify {
        verify(&func)
            .map_err(|errors| format!("builder produced invalid IR: {}", errors.join("; ")))?;
    }

    Ok(func)
}

fn prune_checks(func: &mut Func) {
    let no_deps = func.deps.is_empty() && func.class_deps.is_empty();
    let osr_checks = std::mem::take(&mut func.osr_checks);
    let cell_checks = std::mem::take(&mut func.cell_checks);
    let mut dead = HashSet::new();
    for data in func.blocks.values() {
        for &inst in &data.insts {
            if func.insts[inst].op != Op::CheckNotInvalid || osr_checks.contains(&inst) {
                continue;
            }
            let needed = !no_deps
                && cell_checks
                    .get(&inst)
                    .is_none_or(|cell| func.deps.iter().any(|dep| dep.variable == *cell));
            if !needed {
                dead.insert(inst);
            }
        }
    }
    if dead.is_empty() {
        return;
    }
    for block in func.blocks.keys().collect::<Vec<_>>() {
        func.blocks[block].insts.retain(|inst| !dead.contains(inst));
    }
    // A frame no instruction names would still count as a use of its
    // registers, which keeps their values from being scalar replaced.
    for inst in dead {
        if let Some(frame) = func.insts[inst].frame {
            func.frames[frame].registers.clear();
        }
    }
}

fn dump(func: &Func, label: &str) {
    let text = func.to_string();
    let Some(dir) = flags::jit_dump_dir() else {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::info!(target: crate::heap::logging::DUMP_LOG_TARGET, "{label}\n{text}");
        return;
    };
    let name = format!(
        "opt-build-{:04}-{}.ir",
        super::next_dump_id(),
        super::file_safe(label)
    );
    let written =
        std::fs::create_dir_all(dir).and_then(|()| std::fs::write(dir.join(&name), &text));
    if let Err(err) = written {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::warn!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "cannot write IR dump to {}: {err}",
            dir.display()
        );
    }
}

/// The code block the builder names: the root procedure.
fn root() -> CodeRef {
    CodeRef::new(0)
}

/// The word of an environment record that holds slot `index`. Slot 0 follows
/// the length word.
fn env_slot_word(index: u32) -> u32 {
    (offset_of!(EnvRecord<'static>, slots) / size_of::<usize>()) as u32 + index
}

/// What an instruction may invalidate, which decides its `CheckNotInvalid`.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Fire<'gc> {
    /// Nothing: no check.
    No,
    /// Any dependency of the unit: the check stays if the unit has one.
    Any,
    /// The dependency on this variable only: the check stays if the unit
    /// depends on it.
    Cell(SchemeValue<'gc>),
}

impl<'gc> Fire<'gc> {
    /// What both `self` and `other` may invalidate.
    fn join(self, other: Fire<'gc>) -> Fire<'gc> {
        match (self, other) {
            (Fire::No, other) | (other, Fire::No) => other,
            (Fire::Cell(a), Fire::Cell(b)) if a == b => Fire::Cell(a),
            _ => Fire::Any,
        }
    }
}

/// The arithmetic of a speculated add, subtract or multiply.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Arith {
    Add,
    Sub,
    Mul,
}

/// How a lowered instruction left its block.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Flow {
    /// Control falls to the next instruction.
    Next,
    /// The instruction set the block's terminator.
    Terminated,
}

/// The state a call followed by `return_point_forward` leaves for the
/// return point instructions after it.
#[derive(Clone, Copy)]
struct PendingMv {
    /// The pc of the multiple-value entry after the forward.
    mv_pc: usize,
    /// The call's base register.
    base: u32,
    /// The call's single result, which sits in `r[base + 2]` for the entry.
    result: Value,
    /// Whether one value continues in Tier 2 code: the entry takes exactly
    /// one value. Otherwise even a single value exits at the entry.
    continues: bool,
}

/// Where a specialized call leaves its one result.
#[derive(Clone, Copy)]
enum ResultSite {
    /// A plain call: the result goes to `dst` and the return point at `next`
    /// reads it.
    Normal { dst: u32 },
    /// A call followed by `return_point_forward`: the result lands in
    /// `r[base + 2]` and the entry at `mv_pc` continues when it takes one
    /// value.
    Forward {
        first_value: u32,
        mv_pc: usize,
        base: u32,
        continues: bool,
    },
}

/// One row of a `case-lambda` clause table.
#[derive(Clone, Copy, Debug)]
struct CaseRow {
    min: u32,
    max: u32,
    /// The pc where the clause starts.
    target: usize,
    /// The clause's frame size (the max is the block's `frame_size`).
    frame: u32,
    /// A variadic clause's `(count register, rest-area base)`.
    rest: Option<(u32, u32)>,
}

/// The clause table of a code block that starts with `enter_case`, if it
/// does.
fn case_rows(buffer: &CodeBuffer<'_>) -> Option<Vec<CaseRow>> {
    let code = buffer.code();
    let (Instruction::OpEnterCase(op), _, _) = Instruction::decode_slice(code, 0)? else {
        return None;
    };
    let pool = buffer.pool();
    let table = op.table.0 as usize;
    let count = *pool.get(table)? as usize;
    let mut rows = Vec::with_capacity(count);
    for row in 0..count {
        let at = table + 1 + row * 6;
        let min = *pool.get(at)?;
        let max = *pool.get(at + 1)?;
        let disp = *pool.get(at + 2)? as i32;
        let frame = *pool.get(at + 3)?;
        let rest_base = *pool.get(at + 4)?;
        let rest_cnt = *pool.get(at + 5)?;
        let target = usize::try_from(disp as i64).ok()?;
        let rest =
            (rest_base != crate::bytecode::def::CASE_NO_REST).then_some((rest_cnt, rest_base));
        rows.push(CaseRow {
            min,
            max,
            target,
            frame,
            rest,
        });
    }
    Some(rows)
}

/// Whether a call passing `argc` arguments can jump straight to `code_block`'s
/// compiled entry: a fixed-arity block of that arity, or a multi-entry block
/// whose dispatch first selects a fixed clause of that arity. A variadic
/// clause, or a call no clause takes, keeps the generic protocol.
fn direct_arity_matches(code_block: &CodeBlock<'_>, argc: u32) -> bool {
    if !code_block.arity.is_multi() {
        return !code_block.arity.is_variadic() && code_block.arity.fixed_arity() == argc as usize;
    }
    let code = code_block.buffer.code();
    let Some((Instruction::OpEnterCase(op), _, _)) = Instruction::decode_slice(code, 0) else {
        return false;
    };
    let pool = code_block.buffer.pool();
    let table = op.table.0 as usize;
    let Some(&count) = pool.get(table) else {
        return false;
    };
    for row in 0..count as usize {
        let at = table + 1 + row * 6;
        let (Some(&min), Some(&max)) = (pool.get(at), pool.get(at + 1)) else {
            return false;
        };
        if argc >= min && argc <= max {
            // The first matching row is the one the dispatch takes.
            return min == max;
        }
    }
    false
}

struct Builder<'s, 'gc> {
    /// The mutation scope this build runs in. The class table is looked up
    /// through it when an inline instance allocation is considered.
    ctx: MutationContext<'gc>,
    /// The root's buffer: its constants and pool. Never moves.
    buffer: &'s CodeBuffer<'gc>,
    code: &'s [u8],
    live: Liveness,
    func: Func<'gc>,
    /// The clause table when this is a multi-entry `case-lambda`.
    is_case: bool,
    case_rows: Vec<CaseRow>,
    /// Per clause, its code range `[start, end)` and rest info (with the
    /// shared pseudo register).
    case_ranges: Vec<(usize, usize, Option<RestInfo>)>,
    /// The IR block of each reachable liveness block.
    ir_block: Vec<Option<Block>>,
    /// The registers a block takes as parameters, in parameter order.
    /// `None` for a block that inherits its single predecessor's state.
    param_regs: Vec<Option<Vec<u32>>>,
    /// The single predecessor of each inheriting block.
    single_pred: Vec<usize>,
    /// The abstract state at the end of each lowered block.
    out_states: Vec<Option<Regs>>,
    /// The argument count of the entry, which a self tail call must match.
    argc: Option<u32>,
    /// The return point the last call expects, which must lower next.
    pending_return: Option<usize>,
    /// The variadic entry, when the procedure starts with `enter_rest`.
    rest: Option<RestInfo>,
    /// The shared pseudo register of a `case-lambda`'s variadic clauses.
    rest_reg: Option<u32>,
    /// The multiple-value entry of the last call, when it is followed by a
    /// `return_point_forward`.
    pending_mv: Option<PendingMv>,
    /// The entry and count of a call whose `ReturnValues::Take` return left
    /// its values in `r[base + 2..]`, for the `return_point_values` there.
    mv_taken: Option<(usize, u32)>,
    /// What a `ReturnValues::TakeRest` call read out of the callee frame, for
    /// the `return_point_values_rest` entry that follows: the extra count,
    /// and the list of the extras when the entry's `values_rest` wants one.
    mv_rest: Option<(Value, Option<Value>)>,
    /// The list of the extras, between the rest entry and its `values_rest`.
    rest_list: Option<Value>,
    /// A `CheckNotInvalid` owed to the instructions that last returned
    /// `Flow::Next` but could not emit it yet, because a frame resuming there
    /// would miss the extras that only the `values-rest` after it stores.
    check_owed: Fire<'gc>,
    /// Blocks no IR edge reaches: the entry after a `return_point_forward`
    /// that exits (only a multiple-value return reaches it), and the forward
    /// of a call that deopts because its entry is blocked. They are not part
    /// of the IR.
    mv_only: HashSet<usize>,
    /// The block being built.
    cur: Block,
    /// The abstract register file of the current point.
    state: Regs,
    /// The pc of the instruction being lowered.
    pc: usize,
    /// The pc and register file just before a tail call's moves wrote
    /// register 1, while the rest of those moves are lowered. See
    /// [`Self::frame`].
    tail_setup: Option<(usize, Regs)>,
    /// Whether the exit profile forbids speculation at a pc.
    blocked: &'s dyn Fn(usize) -> bool,
    /// Whether `variable-ref` of a constant variable folds its value.
    fold: bool,
    /// Whether every call and helper is followed by a `CheckNotInvalid`.
    check_invalid: bool,
    /// Whether a check is emitted only after an operation that can fire a
    /// watchpoint this unit depends on (the `fold_guards` policy).
    fold_guards: bool,
    /// Whether a guard after a write to a statically known variable is
    /// recorded as needed only if the unit depends on that variable. False
    /// for a callee build, where the write could touch a dependency of the
    /// caller that this build does not name.
    track_cells: bool,
    /// The inlining this build is part of.
    inline: inline::InlineCx<'gc>,
    /// Whether the function may be entered in the middle of a loop. A
    /// callee spliced into another function may not.
    osr: bool,
    /// The root closure's free variables at build time (empty for a
    /// callee build or when no requesting closure was known). A global
    /// read through free variable `i` may fold the value of the variable
    /// in `free_cells[i]`, guarded at runtime (see `fold_free_variable`).
    free_cells: &'s [SchemeValue<'gc>],
    /// The SSA value of the entry block's closure (register 1), once the
    /// first block is lowered. A `load.free` folds only when it loads
    /// through this value.
    entry_closure: Option<Value>,
    /// `name at file:line:col` of this build's procedure, for log lines.
    label: &'s str,
}

/// The most extra arguments a scalar-mode variadic procedure takes by value.
const MAX_REST_SCALARS: u32 = 3;

/// Scalar mode for the variadic entry whose `enter_rest` has `required`
/// arguments and count register `cnt`: `Some(n)` when the body reads the extras
/// only by constant index (all below `n`, at most [`MAX_REST_SCALARS`]), never
/// builds the rest list, and the first `n` extras sit below the spill slots.
/// The last condition is what lets the entry read them after the allocator has
/// stored its entry parameters: a spill slot is `fp[frame_size + s]` and the
/// extras start at `fp[2 + required]`. (The entry never writes `r[cnt]`, so the
/// count register may share a slot with an extra.)
fn scalar_rest(code: &[u8], frame_size: usize, required: u32, cnt: u32) -> Option<u32> {
    let mut count = 0u32;
    let mut pc = 0;
    while let Some((ins, _, len)) = Instruction::decode_slice(code, pc) {
        match ins {
            Instruction::OpRestList(op) if op.cnt.0 == cnt => return None,
            Instruction::OpRestRef(op) if op.cnt.0 == cnt => {
                count = count.max(op.index.0 as u32 + 1)
            }
            _ => {}
        }
        pc += len;
    }
    if count == 0 || count > MAX_REST_SCALARS {
        return None;
    }
    let first = 2 + required as usize;
    let window = first..first + count as usize;
    if window.end > frame_size {
        return None;
    }
    Some(count)
}

impl<'s, 'gc> Builder<'s, 'gc> {
    fn new(
        ctx: MutationContext<'gc>,
        frame_size: u32,
        buffer: &'s CodeBuffer<'gc>,
        blocked: &'s dyn Fn(usize) -> bool,
        fold: bool,
        check_invalid: bool,
        fold_guards: bool,
        track_cells: bool,
        inline: inline::InlineCx<'gc>,
        osr: bool,
        free_cells: &'s [SchemeValue<'gc>],
        label: &'s str,
    ) -> Result<Self, String> {
        let frame_size = frame_size as usize;
        if frame_size < 2 {
            return Err(format!(
                "frame of {frame_size} slots has no closure register"
            ));
        }
        let code = buffer.code();
        let live = Liveness::compute(code, buffer.pool(), frame_size)?;
        let nblocks = live.blocks().len();

        let case_rows = case_rows(buffer);
        let is_case = case_rows.is_some();
        let case_rows = case_rows.unwrap_or_default();
        // Every variadic clause collects its rest vector into the same pseudo
        // register, one past the (max) frame; only one clause is active.
        let rest_reg = (is_case && case_rows.iter().any(|row| row.rest.is_some()))
            .then_some(frame_size as u32);
        let rest = if is_case {
            None
        } else {
            match Instruction::decode_slice(code, 0) {
                Some((Instruction::OpEnterRest(op), _, _)) => {
                    if op.cnt.0 as usize >= frame_size || op.base.0 as usize >= frame_size {
                        return Err("enter_rest names a register outside the frame".to_string());
                    }
                    Some(RestInfo {
                        required: op.required.0,
                        cnt: op.cnt.0,
                        base: op.base.0,
                        reg: frame_size as u32,
                        scalars: scalar_rest(code, frame_size, op.required.0, op.cnt.0),
                    })
                }
                _ => None,
            }
        };
        // Clause code is concatenated in table order, so each clause runs
        // from its target up to the next clause's target (or the code end).
        let mut case_ranges = Vec::with_capacity(case_rows.len());
        for (index, row) in case_rows.iter().enumerate() {
            let end = case_rows
                .get(index + 1)
                .map_or(code.len(), |next| next.target);
            if end <= row.target {
                return Err("case-lambda clause targets are not in source order".to_string());
            }
            let rest = row.rest.map(|(cnt, base)| RestInfo {
                required: row.min,
                cnt,
                base,
                reg: rest_reg.unwrap_or(frame_size as u32),
                scalars: None,
            });
            case_ranges.push((row.target, end, rest));
        }
        let clause_at = |pc: usize| {
            case_ranges
                .iter()
                .position(|(start, end, _)| pc >= *start && pc < *end)
        };
        let mut mv_only = HashSet::new();
        for block in live.blocks().iter() {
            if let Some((Instruction::OpReturnPointForward(_), _, len)) =
                Instruction::decode_slice(code, block.start)
            {
                let entry = block.start + len;
                // A forward whose entry is blocked belongs to a call that
                // deopts before it (see `lower_forward_call`), so neither the
                // forward nor the entry is reached.
                if blocked(entry) {
                    if let Some(index) = live.block_at(block.start) {
                        mv_only.insert(index);
                    }
                }
            }
        }
        let order = reverse_postorder(&live, &mv_only);
        let mut reachable = vec![false; nblocks];
        for &block in &order {
            reachable[block] = true;
        }
        let mut preds = vec![Vec::new(); nblocks];
        for &block in &order {
            for &succ in &live.blocks()[block].successors {
                if !mv_only.contains(&succ) {
                    preds[succ].push(block);
                }
            }
        }

        let mut func = Func::new();
        func.label = label.to_string();
        let mut ir_block = vec![None; nblocks];
        let mut param_regs = vec![None; nblocks];
        let mut single_pred = vec![0; nblocks];
        for block in 0..nblocks {
            if !reachable[block] {
                continue;
            }
            // A case-lambda's block 0 is the dispatch: its IR form is the
            // function entry, built below from the clause table.
            if is_case && block == 0 {
                continue;
            }
            let start = live.blocks()[block].start;
            let clause = if is_case { clause_at(start) } else { None };
            let clause_rest = clause.and_then(|index| case_ranges[index].2);
            let body_rest = if is_case { clause_rest } else { rest };
            let clause_entry = clause.is_some_and(|index| start == case_ranges[index].0);
            // The entry has a synthetic predecessor, the function's entry.
            // A clause entry is entered from the function entry's selector
            // switch, so it takes parameters like a join.
            let joins = clause_entry || block == 0 || preds[block].len() > 1;
            let ir = func.new_block();
            ir_block[block] = Some(ir);
            if joins {
                // The rest area is not state: its elements live in the vector.
                let mut regs: Vec<u32> = live
                    .live_in(block)
                    .iter()
                    .map(|r| r as u32)
                    .filter(|&r| body_rest.is_none_or(|rest| r < rest.base))
                    .collect();
                if !regs.contains(&1) {
                    regs.push(1);
                }
                if let Some(rest) = body_rest {
                    // The entry glue defines the count and the rest vector.
                    // A join passes the vector on, as it does the closure.
                    if block == 0 && !regs.contains(&rest.cnt) {
                        regs.push(rest.cnt);
                    }
                    regs.extend(rest.reg..rest.reg + rest.width());
                }
                regs.sort_unstable();
                for _ in &regs {
                    func.append_param(ir, Type::Any);
                }
                param_regs[block] = Some(regs);
            } else {
                single_pred[block] = preds[block][0];
            }
        }

        let entry = func.entry;
        if is_case {
            // The entry loads the union of every clause's frame registers,
            // then a shared rest vector and a selector. Its `Switch` sends
            // the selector to the clause block, passing that clause's
            // parameters.
            let mut union: Vec<u32> = Vec::new();
            for (index, (start, _, rest)) in case_ranges.iter().enumerate() {
                let block = live
                    .block_at(*start)
                    .ok_or_else(|| format!("case-lambda clause {index} starts no block"))?;
                let regs = param_regs[block]
                    .as_ref()
                    .ok_or("a case-lambda clause entry takes no parameters")?;
                for &reg in regs {
                    if Some(reg) != rest.map(|rest| rest.reg) && !union.contains(&reg) {
                        union.push(reg);
                    }
                }
                let _ = rest;
            }
            union.sort_unstable();
            func.entry_regs = union;
            if let Some(rest_reg) = rest_reg {
                func.entry_regs.push(rest_reg);
            }
            for _ in 0..func.entry_regs.len() {
                func.append_param(entry, Type::Any);
            }
            let selector = func.append_param(entry, Type::Int32);
            func.entry_selector = Some(selector);
            func.rest_reg = rest_reg;

            let entry_regs = func.entry_regs.clone();
            let entry_params = func.params(entry).to_vec();
            let value_of = |reg: u32| {
                let index = entry_regs
                    .iter()
                    .position(|&r| r == reg)
                    .expect("a clause register is an entry parameter");
                entry_params[index]
            };
            let mut cases = Vec::with_capacity(case_ranges.len());
            let mut default = None;
            for (index, (start, end, clause_rest)) in case_ranges.iter().enumerate() {
                let block = live
                    .block_at(*start)
                    .ok_or_else(|| format!("case-lambda clause {index} starts no block"))?;
                let ir = ir_block[block]
                    .ok_or_else(|| format!("case-lambda clause {index} is unreachable"))?;
                let mut params = param_regs[block].clone().unwrap_or_default();
                let rest_reg = clause_rest.map(|rest| rest.reg);
                if rest_reg.is_some() {
                    params.retain(|&reg| Some(reg) != rest_reg);
                }
                let mut args: Vec<Value> = params.iter().map(|&reg| value_of(reg)).collect();
                if let Some(rest_reg) = rest_reg {
                    args.push(value_of(rest_reg));
                }
                func.case_clauses.push(CaseClause {
                    selector: index as u32,
                    min: case_rows[index].min,
                    max: case_rows[index].max,
                    start: *start as u32,
                    end: *end as u32,
                    block: ir,
                    regs: params,
                    rest: *clause_rest,
                });
                let edge = func.edge(ir, &args);
                if default.is_none() {
                    default = Some(edge);
                }
                cases.push((index as i32, edge));
            }
            let default = default.ok_or("a case-lambda has no clauses")?;
            func.set_terminator(
                entry,
                Terminator::Switch {
                    value: selector,
                    cases,
                    default,
                    // Clause selectors are consecutive from zero, so the
                    // entry switch is dense.
                    density: SwitchDensity::Dense { min: 0 },
                },
            );
        } else {
            // The entry block passes its own parameters to the first block.
            let body = ir_block[0].ok_or("the first block is unreachable")?;
            let regs = param_regs[0].clone().unwrap_or_default();
            for _ in &regs {
                func.append_param(entry, Type::Any);
            }
            let args = func.params(entry).to_vec();
            let edge = func.edge(body, &args);
            func.set_terminator(entry, Terminator::Jump(edge));
            func.entry_regs = regs.iter().map(|&r| r as u32).collect();
        }
        func.rest = rest;
        func.checks_invalid = check_invalid;

        Ok(Builder {
            ctx,
            buffer,
            code,
            live,
            func,
            is_case,
            case_rows,
            case_ranges,
            ir_block,
            param_regs,
            single_pred,
            out_states: vec![None; nblocks],
            argc: None,
            rest,
            rest_reg,
            pending_return: None,
            pending_mv: None,
            mv_taken: None,
            mv_rest: None,
            rest_list: None,
            check_owed: Fire::No,
            mv_only,
            cur: entry,
            state: Vec::new(),
            pc: 0,
            tail_setup: None,
            blocked,
            fold,
            check_invalid,
            fold_guards,
            track_cells,
            inline,
            osr,
            free_cells,
            entry_closure: None,
            label,
        })
    }

    /// The registers of the abstract state: the bytecode frame, and the rest
    /// vector's pseudo register after it.
    fn state_len(&self) -> usize {
        let width = match self.rest {
            Some(rest) => rest.width() as usize,
            None => usize::from(self.rest_reg.is_some()),
        };
        self.live.frame_size() + width
    }

    fn run(mut self) -> Result<Func<'gc>, String> {
        for block in reverse_postorder(&self.live, &self.mv_only) {
            // Block 0 of a case-lambda is the dispatch; its IR form is the
            // function entry, built from the clause table.
            if self.is_case && block == 0 {
                continue;
            }
            self.lower_block(block)?;
        }
        // A multi-entry block's entry already dispatches on the clause
        // selector; per-clause OSR is not implemented yet.
        if self.osr && !self.is_case {
            self.add_osr_entries()?;
        }
        Ok(self.func)
    }

    /// Whether liveness block `block` starts with a loop hint.
    fn starts_at_loop_hint(&self, block: usize) -> bool {
        let start = self.live.blocks()[block].start;
        matches!(
            Instruction::decode_slice(self.code, start),
            Some((Instruction::OpLoopHint(_), _, _))
        )
    }

    /// Adds an OSR entry for each reachable loop header.
    ///
    /// A header is a join block, so it already takes its live registers as
    /// parameters. Each entry gets one block that takes the same parameters
    /// and jumps to the header. The function entry takes the union of every
    /// such register as a parameter, loaded from the frame by its prologue,
    /// plus a selector. Its `Switch` sends selector 0 to the normal body and
    /// selector `k` to the OSR block `k`. Every block then stays dominated by
    /// the entry, so the passes need no extra roots.
    fn add_osr_entries(&mut self) -> Result<(), String> {
        let headers: Vec<usize> = (1..self.live.blocks().len())
            .filter(|&block| {
                self.ir_block[block].is_some()
                    && self.param_regs[block].is_some()
                    && self.starts_at_loop_hint(block)
            })
            .collect();
        // An OSR entry would have to rebuild the rest vector from the frame.
        if headers.is_empty() || self.rest.is_some() {
            return Ok(());
        }
        if headers.len() > i32::MAX as usize {
            return Err("too many loops for OSR".to_string());
        }
        let entry = self.func.entry;
        let body = self.ir_block[0].ok_or("the first block is unreachable")?;
        let mut regs = self.func.entry_regs.clone();
        for &header in &headers {
            for &reg in self.param_regs[header].as_deref().unwrap_or_default() {
                if !regs.contains(&reg) {
                    regs.push(reg);
                    self.func.append_param(entry, Type::Any);
                }
            }
        }
        let selector = self.func.append_param(entry, Type::Int32);
        let entry_params = self.func.params(entry).to_vec();
        let value_of = |reg: u32| {
            let index = regs
                .iter()
                .position(|&r| r == reg)
                .expect("a loop register is an entry parameter");
            entry_params[index]
        };

        let body_regs = self.func.entry_regs.clone();
        let body_args: Vec<Value> = body_regs.iter().map(|&reg| value_of(reg)).collect();
        let body_edge = self.func.edge(body, &body_args);
        let mut cases = Vec::with_capacity(headers.len());
        for (k, &header) in headers.iter().enumerate() {
            let header_regs = self.param_regs[header].clone().unwrap_or_default();
            let header_ir = self.ir_block[header].ok_or("a loop header is unreachable")?;
            let osr = self.func.new_block();
            for _ in &header_regs {
                self.func.append_param(osr, Type::Any);
            }
            let osr_params = self.func.params(osr).to_vec();
            // A frame for the baseline state the OSR edge enters from: the
            // loop header's live registers, at the loop hint's pc. It gives
            // representation selection a state to deopt with when it guards
            // the edge (an OSR entry can carry a tagged value where the loop
            // keeps an unboxed one). `CheckNotInvalid` is the carrier; it also
            // rejects an invalidated unit before entering the loop.
            if let Some(closure) = header_regs.iter().position(|&reg| reg == 1) {
                let pc = self.live.blocks()[header].start as u32;
                let registers = header_regs
                    .iter()
                    .copied()
                    .zip(osr_params.iter().copied())
                    .collect();
                let frame = self.func.push_frame(FrameStateData {
                    code: root(),
                    pc,
                    closure: osr_params[closure],
                    registers,
                    parent: None,
                    call_base: None,
                    returned: None,
                    rest: None,
                    inline_site: None,
                });
                self.func
                    .push_inst(osr, Op::CheckNotInvalid, &[], None, Some(frame));
                if let Some(&check) = self.func.blocks[osr].insts.last() {
                    self.func.osr_checks.insert(check);
                }
            }
            let jump = self.func.edge(header_ir, &osr_params);
            self.func.set_terminator(osr, Terminator::Jump(jump));
            let args: Vec<Value> = header_regs.iter().map(|&reg| value_of(reg)).collect();
            let edge = self.func.edge(osr, &args);
            cases.push((k as i32 + 1, edge));
            self.func.osr.push(OsrEntry {
                block: osr,
                header: header_ir,
                pc: self.live.blocks()[header].start as u32,
                regs: header_regs,
                selector: k as u32 + 1,
            });
        }
        self.func.set_terminator(
            entry,
            Terminator::Switch {
                value: selector,
                cases,
                default: body_edge,
                // OSR selectors start at one and need not be contiguous, so
                // keep the search form.
                density: SwitchDensity::Sparse,
            },
        );
        self.func.entry_regs = regs;
        self.func.entry_selector = Some(selector);
        Ok(())
    }

    fn lower_block(&mut self, block: usize) -> Result<(), String> {
        let ir = self.ir_block[block].ok_or_else(|| format!("block {block} is unreachable"))?;
        self.cur = ir;
        self.state = match &self.param_regs[block] {
            Some(regs) => {
                let mut state = vec![None; self.state_len()];
                for (&reg, &value) in regs.iter().zip(self.func.params(ir)) {
                    state[reg as usize] = Some(value);
                }
                state
            }
            None => {
                let pred = self.single_pred[block];
                self.out_states[pred]
                    .clone()
                    .ok_or_else(|| format!("block {pred} was not lowered before block {block}"))?
            }
        };
        let start = self.live.blocks()[block].start;
        if block == 0 || self.case_ranges.iter().any(|(s, _, _)| *s == start) {
            // The closure every entry of this unit runs with (register 1).
            // Only a `load.free` through this value can fold a global.
            self.entry_closure = self.state.get(1).copied().flatten();
        }

        let (start, end) = (
            self.live.blocks()[block].start,
            self.live.blocks()[block].end,
        );
        let mut pc = start;
        let mut terminated = false;
        self.tail_setup = None;
        while pc < end {
            let (ins, _, len) = Instruction::decode_slice(self.code, pc)
                .ok_or_else(|| format!("undecodable instruction at pc {pc}"))?;
            self.pc = pc;
            self.note_tail_setup(&ins, pc);
            let flow = self.lower(&ins, pc, len)?;
            pc += len;
            if flow == Flow::Terminated {
                if pc != end {
                    return Err(self.error(
                        "a terminator is not the last instruction of its block".to_string(),
                    ));
                }
                terminated = true;
            }
        }
        if !terminated {
            if end >= self.code.len() {
                return Err(format!("code falls off the end at pc {end}"));
            }
            let edge = self.edge_to(end)?;
            self.func.set_terminator(self.cur, Terminator::Jump(edge));
        }
        self.out_states[block] = Some(self.state.clone());
        Ok(())
    }

    /// Lowers the instruction at `pc`, which is `len` bytes long, and checks
    /// the unit's invalid word after anything that can fire a watchpoint the
    /// unit depends on. The conservative policy
    /// (`fold_guards` false) treats every call, primitive, variable
    /// write and yieldpoint that way; the cheaper policy emits the check only
    /// after a Scheme call, a loop-hint yield, a `variable-set!` or a write
    /// to a dependency (see [`Self::may_fire_watchpoint`]).
    fn lower(&mut self, ins: &Instruction, pc: usize, len: usize) -> Result<Flow, String> {
        let flow = self.lower_one(ins, pc, len)?;
        let owed = std::mem::replace(&mut self.check_owed, Fire::No);
        if flow == Flow::Next && self.rest_list.is_some() {
            // A rest entry's extras are a list that reaches a register only
            // at the `values-rest` after it. A frame resuming at that
            // instruction would rebuild without them, so the check waits for
            // it and resumes after, with the list in place.
            self.check_owed = owed.join(self.fire(ins));
            return Ok(flow);
        }
        let fire = owed.join(self.fire(ins));
        if self.check_invalid && flow == Flow::Next && fire != Fire::No {
            // Resume after the instruction, with its result in place. A loop
            // hint resumes at itself, which only repeats a poll.
            let at = if matches!(ins, Instruction::OpLoopHint(_)) {
                pc
            } else {
                pc + len
            };
            // With the guard policy on, two checks with nothing between them
            // are one check: the second cannot observe a new value. The
            // surviving check must then cover both of them.
            if at < self.code.len() {
                match self.last_check() {
                    Some(last) if self.fold_guards => self.widen_check(last, fire),
                    _ => {
                        let frame = self.frame(at, at)?;
                        self.effect_op(Op::CheckNotInvalid, &[], Some(frame));
                        if let (Fire::Cell(cell), true, Some(&check)) = (
                            fire,
                            self.track_cells,
                            self.func.blocks[self.cur].insts.last(),
                        ) {
                            self.func.cell_checks.insert(check, cell);
                        }
                    }
                }
            }
        }
        Ok(flow)
    }

    /// The last instruction appended to the current block, when it is a
    /// `CheckNotInvalid`.
    fn last_check(&self) -> Option<Inst> {
        self.func.blocks[self.cur]
            .insts
            .last()
            .copied()
            .filter(|&inst| self.func.insts[inst].op == Op::CheckNotInvalid)
    }

    /// Makes the existing check `check` also cover a `fire` that follows it.
    fn widen_check(&mut self, check: Inst, fire: Fire<'gc>) {
        let Some(&cell) = self.func.cell_checks.get(&check) else {
            // Already needed whenever the unit has any dependency.
            return;
        };
        if fire != Fire::Cell(cell) {
            self.func.cell_checks.remove(&check);
        }
    }

    /// What `ins` may invalidate: nothing, the unit through any dependency, or
    /// only the unit's dependency on one variable.
    fn fire(&self, ins: &Instruction) -> Fire<'gc> {
        if !self.fold_guards {
            return if self.runs_other_code(ins) {
                Fire::Any
            } else {
                Fire::No
            };
        }
        let any = |fires: bool| if fires { Fire::Any } else { Fire::No };
        match ins {
            // A Scheme call ran arbitrary compiled or interpreted code, which
            // may write any watched global.
            Instruction::OpReturnPoint(_) | Instruction::OpReturnPointValues(_) => Fire::Any,
            // A yield point lets another thread run; it may write a watched
            // global before this thread resumes.
            Instruction::OpLoopHint(_) => Fire::Any,
            // A direct write fires only the written variable's set.
            Instruction::OpVariableSet(op) => self.written_variable(op.var.0),
            // A cache cell is a `Variable`; `cell-set!` now fires its set too.
            Instruction::OpCellSet(op) => self.written_constant_cell(op.constant.0),
            // A call returned into the variadic multiple-value entry; the
            // callee could have invalidated the unit, and the continuation
            // may use folded values.
            Instruction::OpReturnPointValuesRest(_) => Fire::Any,
            Instruction::OpCallPrimitive0(op) => any(Self::prim_may_fire(op.primitive.0)),
            Instruction::OpCallPrimitive1(op) => any(Self::prim_may_fire(op.primitive.0)),
            // `variable-set!` takes the cell and the value; a statically
            // known cell that is not a dependency cannot invalidate the unit.
            Instruction::OpCallPrimitive2(op)
                if Primitive::from_id(op.primitive.0 as u16) == Some(Primitive::VariableSet) =>
            {
                self.written_variable(op.a.0)
            }
            Instruction::OpCallPrimitive2(op) => any(Self::prim_may_fire(op.primitive.0)),
            Instruction::OpCallPrimitive3(op) => any(Self::prim_may_fire(op.primitive.0)),
            Instruction::OpCallPrimitiveN(op) => any(Self::prim_may_fire(op.primitive.0)),
            _ => Fire::No,
        }
    }

    /// Whether the primitive `prim` can fire a watchpoint. `variable-set!` is
    /// the only primitive that stores a `Variable` and fires its set; every
    /// other primitive runs Rust code that never runs Scheme code and never
    /// writes a variable cell.
    fn prim_may_fire(prim: u32) -> bool {
        Primitive::from_id(prim as u16) == Some(Primitive::VariableSet)
    }

    /// What a write through the pool cell `index` may invalidate.
    /// `cell-set!` targets a constant, so the variable is exact when the
    /// build tracks cells.
    fn written_constant_cell(&self, index: u32) -> Fire<'gc> {
        if !self.track_cells {
            return Fire::Any;
        }
        let cell = self.buffer.constant(index as usize);
        if cell.is::<crate::runtime::variable::Variable>() {
            Fire::Cell(cell)
        } else {
            Fire::Any
        }
    }

    /// What a write through the cell in register `reg` may invalidate. A
    /// statically known cell can fire only its own watchpoint; an unknown
    /// one might fire any.
    fn written_variable(&self, reg: u32) -> Fire<'gc> {
        // A callee build does not carry the caller's dependencies, so it
        // must keep the check.
        if !self.track_cells {
            return Fire::Any;
        }
        let Ok(var) = self.reg(reg) else {
            return Fire::Any;
        };
        match self.pool_constant(var) {
            Some(var) if var.is::<crate::runtime::variable::Variable>() => Fire::Cell(var),
            _ => Fire::Any,
        }
    }

    /// Whether `ins` may run code that invalidates the unit. The conservative
    /// policy behind `fold_guards` false.
    fn runs_other_code(&self, ins: &Instruction) -> bool {
        matches!(
            ins,
            Instruction::OpReturnPoint(_)
                | Instruction::OpReturnPointValues(_)
                | Instruction::OpReturnPointValuesRest(_)
                | Instruction::OpCallPrimitive0(_)
                | Instruction::OpCallPrimitive1(_)
                | Instruction::OpCallPrimitive2(_)
                | Instruction::OpCallPrimitive3(_)
                | Instruction::OpCallPrimitiveN(_)
                | Instruction::OpVariableSet(_)
                | Instruction::OpCellSet(_)
                | Instruction::OpLoopHint(_)
        )
    }

    fn lower_one(&mut self, ins: &Instruction, pc: usize, len: usize) -> Result<Flow, String> {
        match ins {
            Instruction::OpNop(_) => Ok(Flow::Next),
            // The entry's argument count check and frame room check are the
            // entry glue's job. The count is kept for self tail calls.
            Instruction::OpEnter(op) => {
                if pc == 0
                    || self
                        .case_ranges
                        .iter()
                        .any(|(start, _, _)| *start as usize == pc)
                {
                    self.argc = Some(op.argc.0);
                }
                Ok(Flow::Next)
            }
            // The dispatch is the function entry's selector switch; the only
            // `enter_case` reached here would be a nested one, which no
            // caller emits.
            Instruction::OpEnterCase(_) if pc == 0 => Ok(Flow::Next),
            Instruction::OpEnterCase(_) => {
                Err(self.unsupported(ins, "an enter_case that is not the procedure's entry"))
            }
            Instruction::OpReturnPoint(_) => {
                // A call already put its result in `dst`.
                if self.pending_return.take() == Some(pc) {
                    Ok(Flow::Next)
                } else {
                    Err(self.unsupported(ins, "a return point that no call precedes"))
                }
            }

            Instruction::OpMove(op) => {
                let value = self.reg(op.src.0)?;
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpMoveTwo(op) => {
                let first = self.reg(op.src1.0)?;
                self.set(op.dst1.0, first)?;
                // The second move reads its source after the first write.
                let second = self.reg(op.src2.0)?;
                self.set(op.dst2.0, second)?;
                Ok(Flow::Next)
            }
            Instruction::OpLoadConstant(op) => {
                let constant = self.constant(op.constant.0);
                let value = self.konst(constant);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpLoadImmediate(op) => {
                let value = self.bits(SchemeValue::from_i32(op.value.0).bits());
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpLoadRaw(op) => {
                let value = self.bits(op.bits.0 as u64);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpCellRef(op) => {
                // The constant is the cell itself. Its value is read in
                // place, or folded when the cell is a bound variable: the
                // cell's identity is baked into the constant pool, so no
                // runtime guard is needed beyond the dependency.
                if self.fold {
                    let cell = self.buffer.constant(op.constant.0 as usize);
                    if let Some(current) = self.foldable_value(cell) {
                        if let Some(value) = self.folded_value(cell, current) {
                            self.set(op.dst.0, value)?;
                            return Ok(Flow::Next);
                        }
                    }
                }
                let cell = self.konst(Constant::Pool {
                    code: root(),
                    index: op.constant.0,
                });
                let value = self.value_op(Op::LoadCell, &[cell], Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpCellSet(op) => {
                let cell = self.konst(Constant::Pool {
                    code: root(),
                    index: op.constant.0,
                });
                let value = self.reg(op.src.0)?;
                // The pool cell is a `Variable`. A write must fire its
                // watchpoint set just like `variable-set!`, or a unit that
                // folded the cell (a lazy primitive-reference cache, say)
                // would never be invalidated. The helper stores, logs the
                // barrier and fires, and never fails.
                let frame = self.frame(pc, pc)?;
                let _ = self.value_op(
                    Op::CallOp(Helper::VariableSet.helper_ref()),
                    &[cell, value],
                    Type::Any,
                    Some(frame),
                );
                Ok(Flow::Next)
            }
            Instruction::OpFreeRef(op) => {
                let closure = self.reg(1)?;
                let value = self.value_op(
                    Op::LoadFree { index: op.index.0 },
                    &[closure],
                    Type::Any,
                    None,
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpClosureRef(op) => {
                let closure = self.reg(op.closure.0)?;
                let value = self.value_op(
                    Op::LoadFree { index: op.index.0 },
                    &[closure],
                    Type::Any,
                    None,
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }

            Instruction::OpJump(_) => {
                let target = self.target(ins, pc)?;
                let edge = self.edge_to(target)?;
                self.terminate(Terminator::Jump(edge));
                Ok(Flow::Terminated)
            }
            Instruction::OpJumpIfTrue(op) => {
                let value = self.reg(op.cond.0)?;
                self.truth_jump(ins, pc, len, value, false)
            }
            Instruction::OpJumpIfFalse(op) => {
                let value = self.reg(op.cond.0)?;
                self.truth_jump(ins, pc, len, value, true)
            }
            Instruction::OpJumpIfNull(op) => {
                let value = self.reg(op.src.0)?;
                let null = self.bits(SchemeValue::VALUE_NULL as u64);
                let cond = self.value_op(Op::EqBits, &[value, null], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, true)
            }
            Instruction::OpJumpIfNotNull(op) => {
                let value = self.reg(op.src.0)?;
                let null = self.bits(SchemeValue::VALUE_NULL as u64);
                let cond = self.value_op(Op::EqBits, &[value, null], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, false)
            }
            Instruction::OpJumpIfUndefined(op) => {
                let value = self.reg(op.src.0)?;
                let undefined = self.undef();
                let cond = self.value_op(Op::EqBits, &[value, undefined], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, true)
            }
            Instruction::OpJumpIfNotUndefined(op) => {
                let value = self.reg(op.src.0)?;
                let undefined = self.undef();
                let cond = self.value_op(Op::EqBits, &[value, undefined], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, false)
            }
            Instruction::OpJumpIfPair(op) => {
                let value = self.reg(op.src.0)?;
                let cond = self.value_op(Op::TestPair, &[value], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, true)
            }
            Instruction::OpJumpIfNotPair(op) => {
                let value = self.reg(op.src.0)?;
                let cond = self.value_op(Op::TestPair, &[value], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, false)
            }
            Instruction::OpJumpIfFixnum(op) => {
                let value = self.reg(op.src.0)?;
                let cond = self.value_op(Op::TestInt32, &[value], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, true)
            }
            Instruction::OpJumpIfNotFixnum(op) => {
                let value = self.reg(op.src.0)?;
                let cond = self.value_op(Op::TestInt32, &[value], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, false)
            }
            Instruction::OpJumpIfEq(op) => {
                let (lhs, rhs) = (self.reg(op.lhs.0)?, self.reg(op.rhs.0)?);
                let cond = self.value_op(Op::EqBits, &[lhs, rhs], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, true)
            }
            Instruction::OpJumpIfNotEq(op) => {
                let (lhs, rhs) = (self.reg(op.lhs.0)?, self.reg(op.rhs.0)?);
                let cond = self.value_op(Op::EqBits, &[lhs, rhs], Type::Bool, None);
                self.cond_jump(ins, pc, len, cond, false)
            }

            Instruction::OpLoopHint(_) => {
                self.safepoint(pc)?;
                Ok(Flow::Next)
            }
            Instruction::OpCall(op) => {
                self.lower_call(ins, pc, len, op.base.0 + 1, op.base.0 + 2, op.argc.0, false)
            }
            Instruction::OpCallSelf(op) => {
                // The callee of a self call is the running closure. In a
                // multi-entry block that is the clause closure, not the
                // group the entry dispatches on, so keep the clause on
                // baseline.
                if self.is_case {
                    return Err(self.unsupported(ins, "a self call in a case-lambda clause"));
                }
                self.lower_call(ins, pc, len, 1, op.base.0 + 2, op.argc.0, true)
            }
            Instruction::OpTailCall(op) => {
                self.lower_tail_call(pc, op.argc.0)?;
                Ok(Flow::Terminated)
            }
            Instruction::OpTailCallSelf(op) => {
                // The frame is reused and the entry runs again, so the new
                // arguments are the current values of the argument registers.
                // A multi-entry block re-dispatches on `argc`, which is known
                // here; the first clause it selects decides the selector.
                if self.is_case {
                    return Err(self.unsupported(ins, "a self tail call in a case-lambda clause"));
                }
                if self.argc != Some(op.argc.0) {
                    return Err(
                        self.unsupported(ins, "the argument count differs from the entry's")
                    );
                }
                calls::tail(true);
                self.safepoint(pc)?;
                let edge = self.edge_to(0)?;
                self.terminate(Terminator::Jump(edge));
                Ok(Flow::Terminated)
            }
            Instruction::OpReturn(op) => {
                let value = self.reg(op.src.0)?;
                self.terminate(Terminator::Return(value));
                Ok(Flow::Terminated)
            }
            Instruction::OpRaise(_) => {
                // Baseline runs the raise from this pc.
                let frame = self.frame(pc, pc)?;
                self.terminate(Terminator::Deopt { frame });
                Ok(Flow::Terminated)
            }

            Instruction::OpCallPrimitive0(op) => {
                self.lower_prim(ins, op.dst.0, op.primitive.0, &[])
            }
            Instruction::OpCallPrimitive1(op) => {
                self.lower_prim(ins, op.dst.0, op.primitive.0, &[op.a.0])
            }
            Instruction::OpCallPrimitive2(op) => {
                self.lower_prim(ins, op.dst.0, op.primitive.0, &[op.a.0, op.b.0])
            }
            Instruction::OpCallPrimitive3(op) => {
                self.lower_prim(ins, op.dst.0, op.primitive.0, &[op.a.0, op.b.0, op.c.0])
            }
            // The baseline runs a variadic call through `capy_prim_n`. Tier 2
            // packs the arguments into consecutive words above the frame and
            // calls a returning helper.
            Instruction::OpCallPrimitiveN(op) => {
                let args: Vec<u32> = (op.first.0..op.first.0 + op.argc.0).collect();
                self.lower_prim(ins, op.dst.0, op.primitive.0, &args)
            }

            Instruction::OpCons(op) => {
                let (car, cdr) = (self.reg(op.car.0)?, self.reg(op.cdr.0)?);
                let value = self.value_op(Op::AllocPair, &[car, cdr], Type::Pair, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpEq(op) => {
                let (lhs, rhs) = (self.reg(op.lhs.0)?, self.reg(op.rhs.0)?);
                let flag = self.value_op(Op::EqBits, &[lhs, rhs], Type::Bool, None);
                let value = self.bool_value(flag);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpNot(op) => {
                let src = self.reg(op.src.0)?;
                let flag = self.false_test(src);
                let value = self.bool_value(flag);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }

            // The entry glue collected the extra arguments into the rest
            // vector and stored their count in `cnt`; both are entry
            // parameters. A self tail call would need to collect again.
            Instruction::OpEnterRest(op) => match self.rest {
                Some(rest) if pc == 0 && op.cnt.0 == rest.cnt && op.base.0 == rest.base => {
                    Ok(Flow::Next)
                }
                _ => Err(self.unsupported(ins, "is not the first instruction")),
            },
            Instruction::OpReturnPointForward(_) => self.lower_forward(ins, pc, len),
            Instruction::OpReturnPointValues(op) => {
                // Reached only when the entry takes the one value that
                // continued from the forward; the call put it in `r[base+2]`.
                // A call that took several values left them in `r[base+2..]`.
                match self.pending_return.take() {
                    Some(at)
                        if at == pc
                            && (op.nvals.0 == 1
                                || self.mv_taken.take() == Some((pc, op.nvals.0))) =>
                    {
                        Ok(Flow::Next)
                    }
                    _ => {
                        Err(self
                            .unsupported(ins, "is a multiple-value entry no call continues into"))
                    }
                }
            }
            Instruction::OpReturnPointValuesRest(op) => {
                // Reached when a call took the callee's values: the fixed
                // ones are in `r[base + 2..]`, the extra count goes into the
                // caller's `frame` register, as baseline's
                // `return-point-values-rest` writes it.
                match self.pending_return.take() {
                    Some(at) if at == pc && self.mv_taken.take() == Some((pc, op.nvals.0)) => {
                        let (count, list) = self
                            .mv_rest
                            .take()
                            .ok_or_else(|| self.unsupported(ins, "has no extra count"))?;
                        self.set(op.frame.0, count)?;
                        self.rest_list = list;
                        Ok(Flow::Next)
                    }
                    _ => {
                        Err(self
                            .unsupported(ins, "is a multiple-value entry no call continues into"))
                    }
                }
            }
            Instruction::OpReturnValues(op) => {
                // The values are `r2..`, live here. One value is a plain
                // return. Any other count leaves through the caller's
                // multiple-values target; a return address the runtime owns
                // exits to baseline, which runs the instruction from `pc`.
                let count = op.count.0;
                if count == 1 {
                    let value = self.reg(2)?;
                    self.terminate(Terminator::Return(value));
                    return Ok(Flow::Terminated);
                }
                let mut values = Vec::with_capacity(count as usize);
                for i in 0..count {
                    values.push(self.reg(2 + i)?);
                }
                self.terminate(Terminator::ReturnMany { values });
                Ok(Flow::Terminated)
            }
            Instruction::OpRuntimeProcedure(op) => {
                let frame = self.frame(pc, pc)?;
                let id = self.value_op(
                    Op::Const(Constant::Int32(op.id.0 as i32)),
                    &[],
                    Type::Int32,
                    None,
                );
                let value = self.value_op(
                    Op::CallOp(Helper::RuntimeProc.helper_ref()),
                    &[id],
                    Type::Any,
                    Some(frame),
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpClosure(op) => {
                let code = self.code_ref(op.constant.0);
                let undef = self.undef();
                let free = vec![undef; op.nfree.0 as usize];
                let value = self.value_op(
                    Op::AllocClosure(code),
                    &free,
                    Type::Closure(Some(code)),
                    None,
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpClosureEnv(op) => {
                if op.nfree.0 == 0 {
                    return Err(self.unsupported(ins, "has no free slot for its environment"));
                }
                let code = self.code_ref(op.constant.0);
                let env = self.reg(op.env.0)?;
                let undef = self.undef();
                let mut free = vec![env];
                free.resize(op.nfree.0 as usize, undef);
                let value = self.value_op(
                    Op::AllocClosure(code),
                    &free,
                    Type::Closure(Some(code)),
                    None,
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpClosureSet(op) => {
                let frame = self.frame(pc, pc)?;
                let closure = self.reg(op.closure.0)?;
                let closure = self.check_class(closure, ClassRef::CLOSURE, frame);
                let src = self.reg(op.src.0)?;
                self.write_barrier(closure, src);
                self.effect_op(Op::StoreFree { index: op.index.0 }, &[closure, src], None);
                Ok(Flow::Next)
            }
            Instruction::OpMakeEnv(op) => {
                let undef = self.undef();
                let slots = vec![undef; op.size.0 as usize];
                let value = self.value_op(Op::AllocEnv, &slots, Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpEnvRef(op) => {
                let field = env_slot_word(op.index.0);
                self.load_field(pc, op.dst.0, op.env.0, ClassRef::ENV_RECORD, field)
            }
            Instruction::OpEnvSet(op) => {
                let field = env_slot_word(op.index.0);
                self.store_field(pc, op.env.0, op.src.0, ClassRef::ENV_RECORD, field)
            }

            Instruction::OpAdd(op) => {
                self.arith_binary(pc, op.dst.0, op.lhs.0, op.rhs.0, Arith::Add)
            }
            Instruction::OpSub(op) => {
                self.arith_binary(pc, op.dst.0, op.lhs.0, op.rhs.0, Arith::Sub)
            }
            Instruction::OpMul(op) => {
                self.arith_binary(pc, op.dst.0, op.lhs.0, op.rhs.0, Arith::Mul)
            }
            Instruction::OpDiv(op) => self.divide_value(ins, pc, op.dst.0, op.lhs.0, op.rhs.0),
            Instruction::OpQuotient(op) => self.divide(
                ins,
                pc,
                op.dst.0,
                Primitive::Quotient,
                Op::QuotI32,
                op.lhs.0,
                op.rhs.0,
            ),
            Instruction::OpRemainder(op) => self.divide(
                ins,
                pc,
                op.dst.0,
                Primitive::Remainder,
                Op::RemI32,
                op.lhs.0,
                op.rhs.0,
            ),
            Instruction::OpModulo(op) => self.divide(
                ins,
                pc,
                op.dst.0,
                Primitive::Modulo,
                Op::ModI32,
                op.lhs.0,
                op.rhs.0,
            ),
            Instruction::OpLess(op) => {
                self.compare_value(pc, op.dst.0, Cmp::Lt, op.lhs.0, op.rhs.0)
            }
            Instruction::OpLessEqual(op) => {
                self.compare_value(pc, op.dst.0, Cmp::Le, op.lhs.0, op.rhs.0)
            }
            Instruction::OpNumberEqual(op) => {
                self.compare_value(pc, op.dst.0, Cmp::Eq, op.lhs.0, op.rhs.0)
            }
            Instruction::OpGreaterEqual(op) => {
                self.compare_value(pc, op.dst.0, Cmp::Ge, op.lhs.0, op.rhs.0)
            }
            Instruction::OpGreater(op) => {
                self.compare_value(pc, op.dst.0, Cmp::Gt, op.lhs.0, op.rhs.0)
            }
            Instruction::OpAddImmediate(op) => {
                let sum = self.add_immediate(pc, op.src.0, op.value.0)?;
                self.set(op.dst.0, sum)?;
                Ok(Flow::Next)
            }
            Instruction::OpAddImmediateMove(op) => {
                // Both registers get the sum, so the move is visible at once.
                let sum = self.add_immediate(pc, op.src.0, op.value.0)?;
                self.set(op.tmp.0, sum)?;
                self.set(op.dst.0, sum)?;
                Ok(Flow::Next)
            }
            Instruction::OpAbs(op) => self.unary_prim(ins, pc, op.dst.0, Primitive::Abs, op.src.0),
            Instruction::OpSqrt(op) => {
                self.unary_prim(ins, pc, op.dst.0, Primitive::Sqrt, op.src.0)
            }
            Instruction::OpNegate(op) => self.negate(pc, op.dst.0, op.src.0),
            Instruction::OpJumpIfLess(op) => {
                self.compare_branch(ins, pc, len, Cmp::Lt, op.lhs.0, op.rhs.0, true)
            }
            Instruction::OpJumpIfNotLess(op) => {
                self.compare_branch(ins, pc, len, Cmp::Lt, op.lhs.0, op.rhs.0, false)
            }
            Instruction::OpJumpIfLessEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Le, op.lhs.0, op.rhs.0, true)
            }
            Instruction::OpJumpIfNotLessEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Le, op.lhs.0, op.rhs.0, false)
            }
            Instruction::OpJumpIfNumberEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Eq, op.lhs.0, op.rhs.0, true)
            }
            Instruction::OpJumpIfNotNumberEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Eq, op.lhs.0, op.rhs.0, false)
            }
            Instruction::OpJumpIfGreaterEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Ge, op.lhs.0, op.rhs.0, true)
            }
            Instruction::OpJumpIfNotGreaterEqual(op) => {
                self.compare_branch(ins, pc, len, Cmp::Ge, op.lhs.0, op.rhs.0, false)
            }
            Instruction::OpJumpIfGreater(op) => {
                self.compare_branch(ins, pc, len, Cmp::Gt, op.lhs.0, op.rhs.0, true)
            }
            Instruction::OpJumpIfNotGreater(op) => {
                self.compare_branch(ins, pc, len, Cmp::Gt, op.lhs.0, op.rhs.0, false)
            }

            Instruction::OpSwitchFixnum(op) => {
                let frame = self.frame(pc, pc)?;
                let src = self.reg(op.src.0)?;
                let key = self.value_op(Op::CheckInt32, &[src], Type::Int32, Some(frame));
                self.lower_int_switch(pc, op.table.0, key)
            }
            Instruction::OpSwitchChar(op) => {
                let frame = self.frame(pc, pc)?;
                let src = self.reg(op.src.0)?;
                let key = self.value_op(Op::CheckChar, &[src], Type::Int32, Some(frame));
                self.lower_int_switch(pc, op.table.0, key)
            }
            Instruction::OpSwitchSymbol(op) => {
                // The helper maps the symbol to its table entry, which is a
                // case index for an int32 switch. Non-symbols deopt.
                let frame = self.frame(pc, pc)?;
                let src = self.reg(op.src.0)?;
                let table = self.table_address(op.table.0);
                let entry = self.value_op(
                    Op::CallOp(Helper::SwitchSymbol.helper_ref()),
                    &[src, table],
                    Type::Any,
                    Some(frame),
                );
                let key = self.value_op(Op::CheckInt32, &[entry], Type::Int32, Some(frame));
                self.lower_symbol_switch(pc, op.table.0, key)
            }

            Instruction::OpCar(op) => self.load_field(pc, op.dst.0, op.src.0, ClassRef::PAIR, 0),
            Instruction::OpCdr(op) => self.load_field(pc, op.dst.0, op.src.0, ClassRef::PAIR, 1),
            Instruction::OpSetCar(op) => {
                self.store_field(pc, op.pair.0, op.src.0, ClassRef::PAIR, 0)
            }
            Instruction::OpSetCdr(op) => {
                self.store_field(pc, op.pair.0, op.src.0, ClassRef::PAIR, 1)
            }

            Instruction::OpVectorRef(op) => {
                let frame = self.frame(pc, pc)?;
                let value = self.vector_ref(frame, op.vector.0, op.index.0)?;
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpVectorRefMul(op) => {
                // The frame keeps the registers from before the instruction,
                // so a failing guard deopts and baseline runs all of it.
                let frame = self.frame(pc, pc)?;
                let value = self.vector_ref(frame, op.vector.0, op.index.0)?;
                self.set(op.vdst.0, value)?;
                // A site that exited too often, or has no single shape, takes
                // the generic path (see `arith.rs`).
                let plan = self.arith_plan(pc);
                let (lhs, rhs) = (self.reg(op.lhs.0)?, self.reg(op.rhs.0)?);
                let product = self.binary_value(plan, Arith::Mul, lhs, rhs, frame);
                self.set(op.mdst.0, product)?;
                Ok(Flow::Next)
            }
            Instruction::OpVectorSet(op) => {
                let frame = self.frame(pc, pc)?;
                let vector = self.reg(op.vector.0)?;
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                let index = self.vector_index(frame, vector, op.index.0)?;
                let value = self.reg(op.src.0)?;
                self.write_barrier(vector, value);
                self.effect_op(Op::StoreElement, &[vector, index, value], None);
                Ok(Flow::Next)
            }
            Instruction::OpVectorLength(op) => {
                let frame = self.frame(pc, pc)?;
                let vector = self.reg(op.src.0)?;
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                let length = self.value_op(Op::VectorLength, &[vector], Type::Int32, Some(frame));
                let value = self.value_op(Op::Box(Repr::Int32), &[length], Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }

            Instruction::OpVariableRef(op) => {
                let var = self.reg(op.var.0)?;
                if let Some(value) = self.fold_variable(var) {
                    self.set(op.dst.0, value)?;
                    return Ok(Flow::Next);
                }
                if let Some(value) = self.fold_free_variable(pc, var)? {
                    self.set(op.dst.0, value)?;
                    return Ok(Flow::Next);
                }
                let frame = self.frame(pc, pc)?;
                let cell = self.check_class(var, ClassRef::VARIABLE, frame);
                let value = self.value_op(Op::LoadCell, &[cell], Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpVariableSet(op) => {
                let frame = self.frame(pc, pc)?;
                let var = self.reg(op.var.0)?;
                let cell = self.check_class(var, ClassRef::VARIABLE, frame);
                let value = self.reg(op.src.0)?;
                // A watched variable fires its set on a write, so the store
                // goes through the helper.
                let _ = self.value_op(
                    Op::CallOp(Helper::VariableSet.helper_ref()),
                    &[cell, value],
                    Type::Any,
                    Some(frame),
                );
                Ok(Flow::Next)
            }

            // A site that has an inline entry gets the inline check (object.rs).
            Instruction::OpGetSlot(op) if self.get_slot_warm(op) => self.lower_get_slot(pc, op),
            Instruction::OpPutSlot(op) if self.put_slot_warm(op) => self.lower_put_slot(pc, op),
            // The hit path runs in the helper. A miss deopts before the
            // instruction, so baseline does the lookup and fills the cache.
            Instruction::OpGetSlot(op) => {
                let frame = self.frame(pc, pc)?;
                let obj = self.reg(op.obj.0)?;
                let meta = self.metadata_address(|md| {
                    // SAFETY: the metadata area holds this opcode's entries.
                    unsafe { OpGetSlot::metadata_ptr(md, op.metadata_id) as u64 }
                });
                let value = self.value_op(
                    Op::CallOp(Helper::GetSlot.helper_ref()),
                    &[obj, meta],
                    Type::Any,
                    Some(frame),
                );
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpPutSlot(op) => {
                let frame = self.frame(pc, pc)?;
                let obj = self.reg(op.obj.0)?;
                let src = self.reg(op.src.0)?;
                let meta = self.metadata_address(|md| {
                    // SAFETY: as for `OpGetSlot`.
                    unsafe { OpPutSlot::metadata_ptr(md, op.metadata_id) as u64 }
                });
                self.effect_op(
                    Op::CallOp(Helper::PutSlot.helper_ref()),
                    &[obj, src, meta],
                    Some(frame),
                );
                Ok(Flow::Next)
            }
            Instruction::OpRestRef(op) => {
                let rest = self.rest_of(ins, op.cnt.0, Some(op.base.0))?;
                let frame = self.frame(pc, pc)?;
                if let Some(n) = rest.scalars {
                    // The extras are values. Past the count baseline raises
                    // `car` of a non-pair from this pc, so deopt there.
                    if op.index.0 as u32 >= n {
                        return Err(self.unsupported(ins, "reads past the scalar extras"));
                    }
                    let count = self.reg(op.cnt.0)?;
                    let count = self.value_op(Op::CheckInt32, &[count], Type::Int32, Some(frame));
                    let index = self.value_op(
                        Op::Const(Constant::Int32(op.index.0 as i32)),
                        &[],
                        Type::Int32,
                        None,
                    );
                    let past =
                        self.value_op(Op::CmpI32(Cmp::Le), &[count, index], Type::Bool, None);
                    self.deopt_if(past, frame);
                    let value = self.reg(rest.reg + op.index.0 as u32)?;
                    self.set(op.dst.0, value)?;
                    return Ok(Flow::Next);
                }
                let vector = self.reg(rest.reg)?;
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                // The index is past the extras when `rest_ref` raises, which
                // baseline does from this pc.
                let index = self.bits(SchemeValue::from_i32(op.index.0 as i32).bits());
                let index =
                    self.value_op(Op::CheckIndex, &[vector, index], Type::Int32, Some(frame));
                let value = self.value_op(Op::LoadElement, &[vector, index], Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpRestLen(op) => {
                self.rest_of(ins, op.cnt.0, None)?;
                let frame = self.frame(pc, pc)?;
                let count = self.reg(op.cnt.0)?;
                let count = self.value_op(Op::CheckInt32, &[count], Type::Int32, Some(frame));
                let skip = self.value_op(
                    Op::Const(Constant::Int32(op.skip.0 as i32)),
                    &[],
                    Type::Int32,
                    None,
                );
                let left = self.value_op(Op::SubI32Ovf, &[count, skip], Type::Int32, Some(frame));
                let value = self.value_op(Op::Box(Repr::Int32), &[left], Type::Any, None);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpRestNull(op) => {
                self.rest_of(ins, op.cnt.0, None)?;
                let count = self.reg(op.cnt.0)?;
                let skip = self.bits(SchemeValue::from_i32(op.skip.0 as i32).bits());
                let flag = self.value_op(Op::EqBits, &[count, skip], Type::Bool, None);
                let value = self.bool_value(flag);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpRestPair(op) => {
                self.rest_of(ins, op.cnt.0, None)?;
                let frame = self.frame(pc, pc)?;
                let count = self.reg(op.cnt.0)?;
                let count = self.value_op(Op::CheckInt32, &[count], Type::Int32, Some(frame));
                let skip = self.value_op(
                    Op::Const(Constant::Int32(op.skip.0 as i32)),
                    &[],
                    Type::Int32,
                    None,
                );
                let flag = self.value_op(Op::CmpI32(Cmp::Gt), &[count, skip], Type::Bool, None);
                let value = self.bool_value(flag);
                self.set(op.dst.0, value)?;
                Ok(Flow::Next)
            }
            Instruction::OpRestList(op) => {
                let rest = self.rest_of(ins, op.cnt.0, Some(op.base.0))?;
                let frame = self.frame(pc, pc)?;
                let vector = self.reg(rest.reg)?;
                let skip = self.bits(SchemeValue::from_i32(op.skip.0 as i32).bits());
                let list = self.value_op(
                    Op::CallOp(Helper::RestList.helper_ref()),
                    &[vector, skip],
                    Type::Any,
                    Some(frame),
                );
                self.set(op.dst.0, list)?;
                Ok(Flow::Next)
            }
            // The extras of a `return_point_values_rest` entry, which the
            // call read out of the callee frame (`lower_forward_call`).
            Instruction::OpValuesRest(op) => {
                match self.rest_list.take() {
                    Some(list) => {
                        self.set(op.base.0, list)?;
                        Ok(Flow::Next)
                    }
                    None => Err(self
                        .unsupported(ins, "follows no multiple-value entry that read its extras")),
                }
            }
        }
    }

    /// Records a specialized call's single result at `site` and leaves the
    /// return point pending at `next`.
    fn finish_result(&mut self, site: ResultSite, next: usize, value: Value) -> Result<(), String> {
        match site {
            ResultSite::Normal { dst } => {
                self.set(dst, value)?;
                self.pending_return = Some(next);
            }
            ResultSite::Forward {
                first_value,
                mv_pc,
                base,
                continues,
            } => {
                self.set(first_value, value)?;
                self.pending_return = Some(next);
                self.pending_mv = Some(PendingMv {
                    mv_pc,
                    base,
                    result: value,
                    continues,
                });
            }
        }
        Ok(())
    }

    /// Lowers a non-tail call to `callee_reg` with the arguments in
    /// `first_arg..first_arg + argc`. The return point after it must be the
    /// next instruction, since a call with several results is not lowered.
    #[allow(clippy::too_many_arguments)]
    fn lower_call(
        &mut self,
        ins: &Instruction,
        pc: usize,
        len: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        self_call: bool,
    ) -> Result<Flow, String> {
        let next = pc + len;
        calls::site(self_call);
        let (dst, values) = match Instruction::decode_slice(self.code, next) {
            Some((Instruction::OpReturnPoint(point), _, _)) => {
                let values = if point.mv.0 == MV_ERROR {
                    ReturnValues::Error
                } else {
                    ReturnValues::Ignore
                };
                (point.dst.0, values)
            }
            Some((Instruction::OpReturnPointForward(forward), _, forward_len)) => {
                return self.lower_forward_call(
                    ins,
                    pc,
                    len,
                    forward.frame.0,
                    forward_len,
                    callee_reg,
                    first_arg,
                    argc,
                    self_call,
                );
            }
            Some((other, _, _)) => {
                return Err(self.unsupported(
                    ins,
                    &format!(
                        "is followed by {}, which is not a return point",
                        opcode_name(&other)
                    ),
                ));
            }
            None => return Err(self.unsupported(ins, "is not followed by a return point")),
        };
        // The result is written before the return point's block, which
        // inherits it only when it has one predecessor.
        let point_block = self
            .live
            .block_at(next)
            .ok_or_else(|| self.unsupported(ins, "return point starts no block"))?;
        if self.param_regs[point_block].is_some() {
            return Err(self.unsupported(ins, "return point has several predecessors"));
        }

        if !self_call {
            // Record accessors, mutators and predicates are native closures
            // whose code-block metadata has the RTD and field. Lower to direct ops
            if let Some(flow) = self.try_record_call(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                ResultSite::Normal { dst },
            )? {
                return Ok(flow);
            }
            // A `make-instance` call with no initargs and a known class is an
            // inline allocation, guarded by the descriptor and the class's
            // redefinition watchpoint.
            if let Some(result) = self.try_instance_call(pc, callee_reg, first_arg, argc)? {
                self.set(dst, result)?;
                self.pending_return = Some(next);
                return Ok(Flow::Next);
            }
            // A generic closure's caches can serve the call without the
            // dispatch stub. A miss deopts before the call.
            if let Some(flow) = self.try_generic_call(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                values,
                ResultSite::Normal { dst },
            )? {
                return Ok(flow);
            }
            let call_frame = self.frame(next, next)?;
            if let Some(result) = self.try_inline(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                (values, call_frame),
                false,
                false,
            )? {
                self.set(dst, result[0])?;
                self.pending_return = Some(next);
                return Ok(Flow::Next);
            }
        }
        let mut operands = vec![self.reg(callee_reg)?];
        operands.extend(self.regs(first_arg..first_arg + argc)?);

        let frame = self.frame(next, next)?;

        let direct = if self_call {
            None
        } else {
            self.direct_callee(operands[0], argc)
                .or_else(|| self.direct_profile_callee(pc, operands[0], argc))
        };
        calls::call(self_call || direct.is_some());
        let op = if self_call {
            Op::CallKnown(root(), values)
        } else if let Some((code, checked)) = direct {
            operands[0] = checked;
            Op::CallKnown(code, values)
        } else {
            Op::CallGeneric(values)
        };
        let result = self.value_op(op, &operands, Type::Any, Some(frame));
        self.set(dst, result)?;
        self.pending_return = Some(next);
        Ok(Flow::Next)
    }

    /// A call followed by `return_point_forward` and a multiple-value entry
    /// at `pc + len + forward_len`. One value returns to the forward and
    /// lands in `r[base + 2]`, the first of the callee's values. Any other
    /// count reaches the entry address, which exits to baseline there with
    /// the callee's values and `AC` (the call's `Forward` return point).
    #[allow(clippy::too_many_arguments)]
    fn lower_forward_call(
        &mut self,
        ins: &Instruction,
        pc: usize,
        len: usize,
        base: u32,
        forward_len: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        self_call: bool,
    ) -> Result<Flow, String> {
        let next = pc + len;
        let mv_pc = next + forward_len;
        // The return's values went through this entry too often: the call
        // stays on baseline, which runs it from its pc. The rest of the
        // procedure still compiles, and the code after the deopt is dead.
        if (self.blocked)(mv_pc) {
            let frame = self.frame(pc, pc)?;
            self.terminate(Terminator::Deopt { frame });
            return Ok(Flow::Terminated);
        }
        let shape = forward_entry(self.code, mv_pc).ok_or_else(|| {
            self.unsupported(ins, "has no multiple-value entry after its forward")
        })?;
        if shape.frame != base {
            return Err(self.unsupported(ins, "has a multiple-value entry with another frame base"));
        }
        let first_value = base + 2;
        if first_value as usize >= self.live.frame_size() {
            return Err(self.unsupported(ins, "has no frame slot for its first returned value"));
        }

        let mode = if shape.rest {
            ReturnValues::TakeRest(shape.nvals)
        } else if shape.nvals != 1 {
            ReturnValues::Take(shape.nvals)
        } else {
            ReturnValues::Forward
        };
        let taking = mode != ReturnValues::Forward;
        if taking && (first_value + shape.nvals) as usize > self.live.frame_size() {
            return Err(self.unsupported(ins, "has no frame slots for its returned values"));
        }

        let wants_list = shape.rest && {
            let entry_len = Instruction::decode_slice(self.code, mv_pc).map_or(0, |(_, _, n)| n);
            match Instruction::decode_slice(self.code, mv_pc + entry_len) {
                Some((Instruction::OpValuesRest(op), _, _)) if op.base.0 == base => {
                    if op.from.0 != shape.nvals {
                        return Err(
                            self.unsupported(ins, "has extras that start past its fixed values")
                        );
                    }
                    true
                }
                _ => false,
            }
        };
        let point_block = self
            .live
            .block_at(next)
            .ok_or_else(|| self.unsupported(ins, "return point starts no block"))?;
        if self.param_regs[point_block].is_some() {
            return Err(self.unsupported(ins, "return point has several predecessors"));
        }
        // The specialized paths below return exactly one value, which an
        // entry taking another count rejects, so they leave it to the call.
        if !self_call && !taking {
            let site = ResultSite::Forward {
                first_value,
                mv_pc,
                base,
                continues: shape.nvals == 1,
            };
            // The specialized record and generic paths return exactly one
            // value, which the forward moves to `r[base + 2]`.
            if let Some(flow) = self.try_record_call(pc, next, callee_reg, first_arg, argc, site)? {
                return Ok(flow);
            }
            if let Some(flow) = self.try_generic_call(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                ReturnValues::Forward,
                site,
            )? {
                return Ok(flow);
            }
            // An inline instance allocation returns one value, which the
            // forward moves to the caller's first value register.
            if let Some(result) = self.try_instance_call(pc, callee_reg, first_arg, argc)? {
                self.set(first_value, result)?;
                self.pending_return = Some(next);
                self.pending_mv = Some(PendingMv {
                    mv_pc,
                    base,
                    result,
                    continues: shape.nvals == 1,
                });
                return Ok(Flow::Next);
            }
            // The call's frame is the entry's, which takes the callee's values.
            let call_frame = self.frame(mv_pc, next)?;
            self.func.frames[call_frame].returned = Some(ReturnedValues { base, count: None });
            let inlined = self.try_inline(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                (ReturnValues::Forward, call_frame),
                false,
                false,
            )?;
            if let Some(result) = inlined {
                // One value returns to the forward, and the body returns one.
                let result = result[0];
                self.set(first_value, result)?;
                self.pending_return = Some(next);
                self.pending_mv = Some(PendingMv {
                    mv_pc,
                    base,
                    result,
                    continues: shape.nvals == 1,
                });
                return Ok(Flow::Next);
            }
        }
        if !self_call && taking {
            // The body returns the entry's values to a block of their own.
            let call_frame = self.frame(mv_pc, next)?;
            self.func.frames[call_frame].returned = Some(ReturnedValues { base, count: None });
            let inlined = self.try_inline(
                pc,
                next,
                callee_reg,
                first_arg,
                argc,
                (mode, call_frame),
                wants_list,
                false,
            )?;
            if let Some(results) = inlined {
                let count = shape.nvals as usize;
                for (i, &value) in results.iter().take(count).enumerate() {
                    self.set(first_value + i as u32, value)?;
                }
                if shape.rest {
                    self.mv_rest = Some((results[count], wants_list.then(|| results[count + 1])));
                }
                let result = match results.first() {
                    Some(&value) if count > 0 => value,
                    _ => self.undef(),
                };
                self.pending_return = Some(next);
                self.pending_mv = Some(PendingMv {
                    mv_pc,
                    base,
                    result,
                    continues: true,
                });
                self.mv_taken = Some((mv_pc, shape.nvals));
                return Ok(Flow::Next);
            }
        }
        let mut operands = vec![self.reg(callee_reg)?];
        operands.extend(self.regs(first_arg..first_arg + argc)?);
        // The call's frame is the entry's: the registers live there are the
        // ones live after the call. Its exit hands the callee frame's values
        // over (`AC` counts them).
        let frame = self.frame(mv_pc, next)?;
        self.func.frames[frame].returned = Some(ReturnedValues { base, count: None });
        // A callee the profile or a folded global names directly goes through
        // `CheckCode`, which deopts before the call when the runtime value is
        // another closure. The forward return point is the same one a generic
        // call would use; only the entry protocol differs.
        let direct = if self_call {
            None
        } else {
            self.direct_callee(operands[0], argc)
                .or_else(|| self.direct_profile_callee(pc, operands[0], argc))
        };
        calls::call(self_call || direct.is_some());
        let op = if self_call {
            Op::CallKnown(root(), mode)
        } else if let Some((code, checked)) = direct {
            operands[0] = checked;
            Op::CallKnown(code, mode)
        } else {
            Op::CallGeneric(mode)
        };
        let result = self.value_op(op, &operands, Type::Any, Some(frame));
        if taking {
            // The values are in the callee frame, read before anything else
            // can reuse it; the entry finds them in `r[base + 2..]`.
            let count = shape.nvals;
            for i in 0..count {
                let value = self.value_op(Op::ReturnedValue(i), &[result], Type::Any, None);
                self.set(first_value + i, value)?;
            }
            if shape.rest {
                let extra = self.value_op(Op::ReturnedCount(count), &[result], Type::Int32, None);
                let list = if wants_list {
                    let from = self.value_op(Op::ReturnedAddr(count), &[result], Type::Int64, None);
                    let list = self.value_op(
                        Op::CallOp(Helper::ValuesRest.helper_ref()),
                        &[from, extra],
                        Type::Any,
                        Some(frame),
                    );
                    Some(list)
                } else {
                    None
                };
                self.mv_rest = Some((extra, list));
            }
            self.pending_return = Some(next);
            self.pending_mv = Some(PendingMv {
                mv_pc,
                base,
                result,
                continues: true,
            });
            self.mv_taken = Some((mv_pc, count));
            return Ok(Flow::Next);
        }
        self.set(first_value, result)?;
        self.pending_return = Some(next);
        self.pending_mv = Some(PendingMv {
            mv_pc,
            base,
            result,
            continues: shape.nvals == 1,
        });
        Ok(Flow::Next)
    }

    /// The `return_point_forward` after a call: the single value continues at
    /// the entry when it takes one value, otherwise baseline runs the entry
    /// with the value in `r[base + 2]` and a count of one.
    fn lower_forward(&mut self, ins: &Instruction, pc: usize, len: usize) -> Result<Flow, String> {
        let pending = match (self.pending_return.take(), self.pending_mv.take()) {
            (Some(at), Some(pending)) if at == pc && pending.mv_pc == pc + len => pending,
            _ => return Err(self.unsupported(ins, "is a forward no call precedes")),
        };
        if pending.continues {
            self.pending_return = Some(pending.mv_pc);
            return Ok(Flow::Next);
        }
        let frame = self.frame(pending.mv_pc, pc)?;
        let data = &mut self.func.frames[frame];
        data.returned = Some(ReturnedValues {
            base: pending.base,
            count: Some(1),
        });
        let slot = pending.base + 2;
        match data.registers.binary_search_by_key(&slot, |&(reg, _)| reg) {
            Ok(index) => data.registers[index].1 = pending.result,
            Err(index) => data.registers.insert(index, (slot, pending.result)),
        }
        self.terminate(Terminator::Deopt { frame });
        Ok(Flow::Terminated)
    }

    /// The variadic entry that the rest instruction at the current pc reads:
    /// it must name the register that holds the count and, when it takes
    /// one, the rest area's base.
    fn rest_of(&self, ins: &Instruction, cnt: u32, base: Option<u32>) -> Result<RestInfo, String> {
        // An ordinary variadic entry has one rest area; a case-lambda clause
        // has its own, selected by the pc of the instruction.
        let rest = self.rest.or_else(|| {
            self.case_ranges
                .iter()
                .find(|(start, end, _)| self.pc >= *start && self.pc < *end)
                .and_then(|(_, _, rest)| *rest)
        });
        match rest {
            Some(rest) if rest.cnt == cnt && base.is_none_or(|base| base == rest.base) => Ok(rest),
            _ => Err(self.unsupported(ins, "reads a rest area the entry did not make")),
        }
    }

    /// The code block a closure or guard names. A nested block is named by
    /// its pool constant, and each one gets one entry in `func.codes`.
    fn code_ref(&mut self, pool_index: u32) -> CodeRef {
        let source = CodeSource::Pool(pool_index);
        if let Some((code, _)) = self.func.codes.iter().find(|(_, s)| **s == source) {
            return code;
        }
        self.func.codes.push(source)
    }

    /// The code block named by its heap value, for a callee known from a
    /// folded global.
    fn code_ref_value(&mut self, value: SchemeValue<'gc>) -> CodeRef {
        // The code block this unit compiles for is `CodeRef` 0; naming it
        // directly lets a self call enter this unit's entry with a plain
        // jump (see `enter_known`).
        if let Some(&closure) = self.inline.chain.first() {
            let root_block = crate::runtime::objectmodel::proc::Closure::code_block(
                crate::runtime::objectmodel::proc::Closure::from_value(closure),
            );
            if SchemeValue::from(root_block) == value {
                return root();
            }
        }
        let source = CodeSource::Heap(value);
        if let Some((code, _)) = self.func.codes.iter().find(|(_, s)| **s == source) {
            return code;
        }
        self.func.codes.push(source)
    }

    /// The direct-call target for `callee`, when it is a constant folded
    /// from a global: the callee's code and the callee guarded by
    /// `CheckCode`, which deopts before the call at `self.pc` when the
    /// value in the register is not that closure. Only a bytecode callee
    /// whose fixed arity is the call's argument count qualifies; anything
    /// else (variadic, native, another code block) keeps the generic call,
    /// which raises the same errors baseline would.
    fn direct_callee(&mut self, callee: Value, argc: u32) -> Option<(CodeRef, Value)> {
        let ValueDef::Inst(inst) = self.func.value_def(callee) else {
            return None;
        };
        let Op::Const(Constant::Table(slot)) = self.func.insts[inst].op else {
            return None;
        };
        let value = *self.func.heap_constants.get(slot as usize - 1)?;
        if !value.is::<crate::runtime::objectmodel::proc::Closure>() {
            return None;
        }
        let closure = value.downcast::<crate::runtime::objectmodel::proc::Closure>();
        let code_block = crate::runtime::objectmodel::proc::Closure::code_block(closure);
        if code_block.kind != crate::runtime::objectmodel::CodeBlockKind::Bytecode
            || !direct_arity_matches(&code_block, argc)
        {
            return None;
        }
        let code = self.code_ref_value(SchemeValue::from(code_block));
        let frame = self.frame(self.pc, self.pc).ok()?;
        let checked = self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        Some((code, checked))
    }

    /// The direct-call target for a monomorphic call site, from the call
    /// profile. The profile names the callee code block the site entered
    /// last; `CheckCode` deopts before the call when the runtime value is
    /// another closure, so the profile is only a hint. A bytecode callee of
    /// this call's fixed arity qualifies; variadic and native callees keep
    /// the generic call, which performs the arity and class protocol.
    fn direct_profile_callee(
        &mut self,
        pc: usize,
        callee: Value,
        argc: u32,
    ) -> Option<(CodeRef, Value)> {
        let code_block = match profile::call_shape(self.buffer, pc)? {
            profile::CallShape::Mono(Some(target)) => target.callee,
            _ => return None,
        };
        if code_block.kind != crate::runtime::objectmodel::proc::CodeBlockKind::Bytecode
            || !direct_arity_matches(&code_block, argc)
        {
            return None;
        }
        let code = self.code_ref_value(SchemeValue::from(code_block));
        let frame = self.frame(self.pc, self.pc).ok()?;
        let checked = self.value_op(
            Op::CheckCode(code),
            &[callee],
            Type::Closure(Some(code)),
            Some(frame),
        );
        Some((code, checked))
    }

    /// Deopts before the instruction unless `value` is an instance of
    /// `class`. The result is `value`, refined to the class.
    fn check_class(&mut self, value: Value, class: ClassRef, frame: FrameState) -> Value {
        self.value_op(
            Op::CheckClass(class),
            &[value],
            Type::Heap(class),
            Some(frame),
        )
    }

    /// Calls the primitive `prim` on `args` through its direct wrapper, as
    /// the baseline's `prim_direct` does. A failing call deopts before the
    /// instruction, so `frame` holds the registers from before it. `ins`
    /// names the instruction in the error when the call has no direct wrapper.
    fn prim_call(
        &mut self,
        ins: &Instruction,
        prim: Primitive,
        args: &[Value],
        frame: FrameState,
    ) -> Result<Value, String> {
        if let Some(why) = direct_prim_error(prim.id() as u32, args.len()) {
            return Err(self.unsupported(ins, &why));
        }
        let op = Op::CallPrim {
            prim: PrimRef::new(prim.id() as usize),
            effects: prim_effects(prim.id() as u32),
        };
        Ok(self.value_op(op, args, Type::Any, Some(frame)))
    }

    /// `dst = prim(lhs, rhs)`.
    /// The write barrier of a store of `value` into `obj`, emitted *before*
    /// the store. Under the SATB barrier (ConcurrentImmix) the slow path
    /// enqueues the fields `obj` holds when it runs, so it must see the value
    /// being overwritten; a barrier after the store loses that value and the
    /// collector can free an object that is still reachable. The barrier logs
    /// `obj` for the collector, which a store of an immediate needs not do: the
    /// stored word holds no reference to a heap object. A boxed int32, double
    /// or boolean and an immediate constant are immediates (a flonum is
    /// NaN-boxed, not a cell). `Box(Repr::Int64)` and `Box(Repr::Int128)` are
    /// not: a value wider than i32 becomes a one- or two-limb `BIGINT`, which
    /// is a heap reference and needs the barrier.
    fn write_barrier(&mut self, obj: Value, value: Value) {
        if let ValueDef::Inst(inst) = self.func.value_def(value) {
            if matches!(
                self.func.insts[inst].op,
                Op::BoolToValue
                    | Op::Box(Repr::Int32)
                    | Op::Box(Repr::Double)
                    | Op::Const(Constant::Bits(_))
            ) {
                return;
            }
        }
        self.effect_op(Op::WriteBarrier, &[obj, value], None);
    }

    fn binary_prim(
        &mut self,
        ins: &Instruction,
        pc: usize,
        dst: u32,
        prim: Primitive,
        lhs: u32,
        rhs: u32,
    ) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let (lhs, rhs) = (self.reg(lhs)?, self.reg(rhs)?);
        let value = self.prim_call(ins, prim, &[lhs, rhs], frame)?;
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// `dst = prim(src)`.
    fn unary_prim(
        &mut self,
        ins: &Instruction,
        pc: usize,
        dst: u32,
        prim: Primitive,
        src: u32,
    ) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let src = self.reg(src)?;

        if prim == Primitive::Sqrt {
            let value = self.flonum_sqrt_fast(src, frame);
            self.set(dst, value)?;
            return Ok(Flow::Next);
        }
        let value = self.prim_call(ins, prim, &[src], frame)?;
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// `dst = obj[field]`, where `obj` must be an instance of `class`.
    fn load_field(
        &mut self,
        pc: usize,
        dst: u32,
        obj: u32,
        class: ClassRef,
        field: u32,
    ) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let obj = self.reg(obj)?;
        let obj = self.check_class(obj, class, frame);
        let value = self.value_op(
            Op::Load {
                field,
                region: MemoryRegion::Heap,
            },
            &[obj],
            Type::Any,
            None,
        );
        self.set(dst, value)?;
        Ok(Flow::Next)
    }

    /// `obj[field] = src`, where `obj` must be an instance of `class`.
    fn store_field(
        &mut self,
        pc: usize,
        obj: u32,
        src: u32,
        class: ClassRef,
        field: u32,
    ) -> Result<Flow, String> {
        let frame = self.frame(pc, pc)?;
        let obj = self.reg(obj)?;
        let obj = self.check_class(obj, class, frame);
        let value = self.reg(src)?;
        self.write_barrier(obj, value);
        self.effect_op(
            Op::Store {
                field,
                region: MemoryRegion::Heap,
            },
            &[obj, value],
            None,
        );
        Ok(Flow::Next)
    }

    /// The element `index` of `vector`, which is a checked vector. Deopts
    /// before the instruction when the index is out of bounds.
    fn vector_index(
        &mut self,
        frame: FrameState,
        vector: Value,
        index: u32,
    ) -> Result<Value, String> {
        let index = self.reg(index)?;
        Ok(self.value_op(Op::CheckIndex, &[vector, index], Type::Int32, Some(frame)))
    }

    /// The element of vector register `vector` at register `index`.
    fn vector_ref(&mut self, frame: FrameState, vector: u32, index: u32) -> Result<Value, String> {
        let vector = self.reg(vector)?;
        let vector = self.check_class(vector, ClassRef::VECTOR, frame);
        let index = self.vector_index(frame, vector, index)?;
        Ok(self.value_op(Op::LoadElement, &[vector, index], Type::Any, None))
    }

    /// The address of a pool table or metadata entry. Both live in the
    /// non-moving code buffer, which the unit keeps alive, so the address is
    /// a plain constant.
    fn table_address(&mut self, pool_index: u32) -> Value {
        let address = self.buffer.pool().as_ptr() as u64 + 4 * u64::from(pool_index);
        self.bits(address)
    }

    /// The address of a metadata entry, computed from the code buffer's
    /// metadata area by `entry`.
    fn metadata_address(&mut self, entry: impl FnOnce(*mut u8) -> u64) -> Value {
        let address = entry(self.buffer.metadata());
        self.bits(address)
    }

    /// Ends the block with a symbol switch whose case index is `key`, an
    /// int32 entry index into the table at pool index `table`. Each entry
    /// index is one case, so its target is the entry's displacement.
    fn lower_symbol_switch(&mut self, pc: usize, table: u32, key: Value) -> Result<Flow, String> {
        let (default, entries) = self.symbol_switch_table(pc, table as usize)?;
        let default = self.edge_to(default)?;
        let mut cases = Vec::with_capacity(entries.len());
        for (index, target) in entries.into_iter().enumerate() {
            cases.push((index as i32, self.edge_to(target)?));
        }
        self.terminate(Terminator::Switch {
            value: key,
            cases,
            default,
            // The helper maps a hit to the entry index, and entry indices are
            // consecutive from zero, so the index switch is always dense.
            density: SwitchDensity::Dense { min: 0 },
        });
        Ok(Flow::Terminated)
    }

    /// The default target and the targets of each entry of the symbol switch
    /// table at pool index `table`, as pcs. The layout is `switch_symbol_probe`'s.
    fn symbol_switch_table(&self, pc: usize, table: usize) -> Result<(usize, Vec<usize>), String> {
        let pool = self.buffer.pool();
        let word = |index: usize| -> Result<u32, String> {
            pool.get(index)
                .copied()
                .ok_or_else(|| self.error(format!("switch table at word {index} is past the pool")))
        };
        let target = |disp: u32| -> Result<usize, String> {
            usize::try_from(pc as i64 + disp as i32 as i64)
                .map_err(|_| self.error("switch displacement leaves the code".to_string()))
        };
        let default = target(word(table)?)?;
        let count = word(table + 1)? as usize;
        let mut entries = Vec::with_capacity(count);
        for entry in 0..count {
            entries.push(target(word(table + 4 + 3 * entry + 2)?)?);
        }
        Ok((default, entries))
    }

    /// Ends the block with a switch on the int32 `key`, following the
    /// displacements of the switch table at pool index `table`.
    fn lower_int_switch(&mut self, pc: usize, table: u32, key: Value) -> Result<Flow, String> {
        let (default, cases, density) = self.int_switch_table(pc, table as usize)?;
        let default = self.edge_to(default)?;
        let mut edges = Vec::with_capacity(cases.len());
        for (case, target) in cases {
            edges.push((case, self.edge_to(target)?));
        }
        self.terminate(Terminator::Switch {
            value: key,
            cases: edges,
            default,
            density,
        });
        Ok(Flow::Terminated)
    }

    /// The default target, the `(key, target)` cases and the key layout of a
    /// fixnum or char switch table, as pcs. Mirrors the interpreter's
    /// `switch_int_disp`.
    fn int_switch_table(
        &self,
        pc: usize,
        table: usize,
    ) -> Result<(usize, Vec<(i32, usize)>, SwitchDensity), String> {
        let pool = self.buffer.pool();
        let word = |index: usize| -> Result<u32, String> {
            pool.get(index)
                .copied()
                .ok_or_else(|| self.error(format!("switch table at word {index} is past the pool")))
        };
        let target = |disp: u32| -> Result<usize, String> {
            usize::try_from(pc as i64 + disp as i32 as i64)
                .map_err(|_| self.error("switch displacement leaves the code".to_string()))
        };
        let default = target(word(table)?)?;
        let count = word(table + 1)?;
        let mut cases = Vec::new();
        let density;
        if switch_table_is_dense(count) {
            let span = switch_table_len(count);
            let min = word(table + 2)? as i32;
            for index in 0..span {
                let disp = word(table + 3 + index)?;
                cases.push((min.wrapping_add(index as i32), target(disp)?));
            }
            density = SwitchDensity::Dense { min };
        } else {
            for case in 0..count as usize {
                let key = word(table + 2 + 2 * case)? as i32;
                let disp = word(table + 3 + 2 * case)?;
                cases.push((key, target(disp)?));
            }
            density = SwitchDensity::Sparse;
        }
        Ok((default, cases, density))
    }

    /// Lowers a primitive call through its direct wrapper. A failing
    /// primitive deopts before the instruction, so the frame keeps the
    /// registers live before it.
    fn lower_prim(
        &mut self,
        ins: &Instruction,
        dst: u32,
        prim: u32,
        args: &[u32],
    ) -> Result<Flow, String> {
        if let Some(why) = direct_prim_error(prim, args.len()) {
            return Err(self.unsupported(ins, &why));
        }
        let operands = self.regs_of(args)?;
        let frame = self.frame(self.pc, self.pc)?;
        if args.len() == 1 && Primitive::from_id(prim as u16) == Some(Primitive::VariableBound) {
            let result = self.variable_bound(operands[0], frame);
            self.set(dst, result)?;
            return Ok(Flow::Next);
        }
        if let Some(result) = self.inline_prim(prim, &operands, frame) {
            self.set(dst, result)?;
            return Ok(Flow::Next);
        }
        let op = Op::CallPrim {
            prim: PrimRef::new(prim as usize),
            effects: prim_effects(prim),
        };
        let result = self.value_op(op, &operands, Type::Any, Some(frame));
        self.set(dst, result)?;
        Ok(Flow::Next)
    }

    /// The constant fixnum `value` is, if it is the result of a constant
    /// load.
    fn fixnum_constant(&self, value: Value) -> Option<i32> {
        match self.func.value_def(value) {
            ValueDef::Inst(inst) => match self.func.insts[inst].op {
                Op::Const(Constant::Bits(bits)) => SchemeValue::from_raw(bits).int32(),
                Op::Const(Constant::Int32(n)) => Some(n),
                _ => None,
            },
            ValueDef::Param { .. } => None,
        }
    }

    /// A primitive that Tier 2 lowers without a call, as the baseline's
    /// templates do: the total predicates, constants, and a constant-size
    /// vector allocation. Returns the register value, or `None` to call the
    /// primitive. None of these can fail, so none needs the frame, except the
    /// helper calls, whose nonzero status deopts before the call (baseline
    /// raises from the pc).
    fn inline_prim(&mut self, prim: u32, operands: &[Value], frame: FrameState) -> Option<Value> {
        use Primitive as P;
        let prim = Primitive::from_id(prim as u16)?;
        let flag = |this: &mut Self, flag: Value| this.bool_value(flag);
        Some(match (prim, operands) {
            (P::Unspec, []) => self.undef(),
            (P::IsNull, &[x]) => {
                let null = self.bits(SchemeValue::VALUE_NULL as u64);
                let test = self.value_op(Op::EqBits, &[x, null], Type::Bool, None);
                flag(self, test)
            }
            (P::IsEofObject, &[x]) => {
                let eof = self.bits(SchemeValue::VALUE_EOF as u64);
                let test = self.value_op(Op::EqBits, &[x, eof], Type::Bool, None);
                flag(self, test)
            }
            (P::IsUnspecified, &[x]) => {
                let undef = self.undef();
                let test = self.value_op(Op::EqBits, &[x, undef], Type::Bool, None);
                flag(self, test)
            }
            (P::IsEq, &[x, y]) => {
                let test = self.value_op(Op::EqBits, &[x, y], Type::Bool, None);
                flag(self, test)
            }
            (P::Not, &[x]) => {
                let test = self.false_test(x);
                flag(self, test)
            }
            (P::IsPair, &[x]) => {
                let test = self.value_op(Op::TestPair, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsFixnum, &[x]) => {
                let test = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsFlonum, &[x]) => {
                let test = self.value_op(Op::TestDouble, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsHeapObject, &[x]) => {
                let test = self.value_op(Op::TestHeap, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsVariable, &[x]) => self.class_predicate(x, ClassRef::VARIABLE),
            (P::IsProcedure, &[x]) => self.class_predicate(x, ClassRef::CLOSURE),
            (P::IsVector, &[x]) => self.class_predicate(x, ClassRef::VECTOR),
            (P::IsSymbol, &[x]) => self.class_predicate(x, ClassRef::SYMBOL),
            (P::IsString, &[x]) => self.class_predicate(x, ClassRef::STRING),
            (P::IsTuple, &[x]) => self.class_predicate(x, ClassRef::TUPLE),
            // `make-vector` of a small constant size, with the fill given or
            // undefined: the same object the primitive makes. A runtime size
            // takes the inline allocate-and-fill path, which deopts to the
            // primitive on a non-fixnum/negative/too-large size and on the
            // allocator's slow path.
            (P::MakeVector, [size]) | (P::MakeVector, [size, _]) => {
                let fill = match operands.get(1) {
                    Some(&fill) => fill,
                    None => self.undef(),
                };
                if let Some(n) = self.fixnum_constant(*size) {
                    if !(0..=MAX_INLINE_VECTOR as i32).contains(&n) {
                        return None;
                    }
                    let elements = vec![fill; n as usize];
                    self.value_op(Op::AllocVector, &elements, Type::Vector, None)
                } else {
                    self.value_op(
                        Op::AllocVectorFill,
                        &[*size, fill],
                        Type::Vector,
                        Some(frame),
                    )
                }
            }
            (P::Vector, elements) if elements.len() <= MAX_INLINE_VECTOR => {
                self.value_op(Op::AllocVector, elements, Type::Vector, None)
            }
            (P::Length, &[list]) => self.value_op(
                Op::CallOp(Helper::Length.helper_ref()),
                &[list],
                Type::Any,
                Some(frame),
            ),
            (P::Append, &[a, b]) => self.value_op(
                Op::CallOp(Helper::Append.helper_ref()),
                &[a, b],
                Type::Any,
                Some(frame),
            ),
            // The primitive fast paths the baseline template already emits.
            // Each one matches the baseline's type checks and deopts to
            // baseline (rather than emitting an error path) on anything else.
            (P::IsBoolean, &[x]) => {
                let test = self.value_op(Op::TestBoolean, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsChar, &[x]) => {
                let test = self.value_op(Op::TestChar, &[x], Type::Bool, None);
                flag(self, test)
            }
            (P::IsNumber, &[x]) => {
                let test = self.value_op(Op::TestNumber, &[x], Type::Bool, None);
                flag(self, test)
            }
            // `exact?`, `inexact?` and `exact-integer?`: a fixnum and a flonum
            // answer inline; every other value (heap numbers included) takes
            // the primitive, whose answer is baseline's.
            (P::IsExact, &[x]) => {
                let yes = self.bits(SchemeValue::VALUE_TRUE as u64);
                let no = self.bits(SchemeValue::VALUE_FALSE as u64);
                self.fixnum_or_flonum_answer(P::IsExact, x, yes, no, frame)
            }
            (P::IsInexact, &[x]) => {
                let yes = self.bits(SchemeValue::VALUE_TRUE as u64);
                let no = self.bits(SchemeValue::VALUE_FALSE as u64);
                self.fixnum_or_flonum_answer(P::IsInexact, x, no, yes, frame)
            }
            (P::IsExactInteger, &[x]) => {
                let yes = self.bits(SchemeValue::VALUE_TRUE as u64);
                let no = self.bits(SchemeValue::VALUE_FALSE as u64);
                self.fixnum_or_flonum_answer(P::IsExactInteger, x, yes, no, frame)
            }
            // `integer?`: a fixnum answers inline; a flonum or any other value
            // takes the primitive (a flonum's answer depends on its value).
            (P::IsInteger, &[x]) => {
                let (join, result) = self.join_block(Type::Any);
                let slow = self.func.new_block();
                let yes = self.func.new_block();
                let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
                self.branch_to(is_fixnum, yes, slow);
                self.cur = yes;
                let true_value = self.bits(SchemeValue::VALUE_TRUE as u64);
                self.jump_with(join, &[true_value]);
                self.cur = slow;
                let value = self.prim_value(P::IsInteger, &[x], frame);
                self.jump_with(join, &[value]);
                self.cur = join;
                result
            }
            // `exact->inexact`: a fixnum converts exactly, a flonum is its own
            // inexact value, and a heap number takes the primitive (which
            // rounds it to a double).
            (P::ExactToInexact, &[x]) => {
                let (join, result) = self.join_block(Type::Any);
                let maybe_flonum = self.func.new_block();
                let slow = self.func.new_block();
                let fixnum = self.func.new_block();
                let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
                self.branch_to(is_fixnum, fixnum, maybe_flonum);
                self.cur = fixnum;
                let raw = self.value_op(Op::Unbox(Repr::Int32), &[x], Type::Int32, None);
                let converted = self.value_op(Op::Int32ToDouble, &[raw], Type::Double, None);
                let boxed = self.value_op(Op::Box(Repr::Double), &[converted], Type::Any, None);
                self.jump_with(join, &[boxed]);
                self.cur = maybe_flonum;
                let double = self.func.new_block();
                let is_double = self.value_op(Op::TestDouble, &[x], Type::Bool, None);
                self.branch_to(is_double, double, slow);
                self.cur = double;
                self.jump_with(join, &[x]);
                self.cur = slow;
                let value = self.prim_value(P::ExactToInexact, &[x], frame);
                self.jump_with(join, &[value]);
                self.cur = join;
                result
            }
            // Only identical bits are known to be equivalent without the
            // primitive: fixnums are canonical, chars are not numbers, and
            // every other mixed case goes to `Value::eqv` through the call.
            (P::IsEqv, &[x, y]) => {
                let (join, result) = self.join_block(Type::Any);
                let same = self.func.new_block();
                let different = self.func.new_block();
                let equal = self.value_op(Op::EqBits, &[x, y], Type::Bool, None);
                self.branch_to(equal, same, different);
                self.cur = same;
                let yes = self.bits(SchemeValue::VALUE_TRUE as u64);
                self.jump_with(join, &[yes]);
                self.cur = different;
                // A character is neither a cell nor a number, so with
                // different bits it is not equivalent to anything (the same
                // answer the baseline gives without calling).
                let check_y = self.func.new_block();
                let slow = self.func.new_block();
                let no = self.func.new_block();
                let x_char = self.value_op(Op::TestChar, &[x], Type::Bool, None);
                self.branch_to(x_char, no, check_y);
                self.cur = check_y;
                let y_char = self.value_op(Op::TestChar, &[y], Type::Bool, None);
                self.branch_to(y_char, no, slow);
                self.cur = no;
                let falsy = self.bits(SchemeValue::VALUE_FALSE as u64);
                self.jump_with(join, &[falsy]);
                self.cur = slow;
                let value = self.prim_value(P::IsEqv, &[x, y], frame);
                self.jump_with(join, &[value]);
                self.cur = join;
                result
            }
            // `floor`, `ceiling` and `truncate`: a fixnum is its own answer,
            // a flonum takes the inline floating-point rounding the baseline
            // emits, and anything else (bignums, rationals) takes the
            // primitive.
            (P::Floor, &[x]) => self.float_round_fast(P::Floor, Op::FloorF64, x, frame),
            (P::Ceiling, &[x]) => self.float_round_fast(P::Ceiling, Op::CeilingF64, x, frame),
            (P::Truncate, &[x]) => self.float_round_fast(P::Truncate, Op::TruncateF64, x, frame),
            // `char->integer`: the check deopts on a non-character, exactly as
            // the wrapper raises from the instruction.
            (P::CharToInteger, &[x]) => {
                let code = self.value_op(Op::CheckChar, &[x], Type::Int32, Some(frame));
                self.value_op(Op::Box(Repr::Int32), &[code], Type::Any, None)
            }
            // Tuple access: class check, then the same length word, bounds
            // test and data offset a vector uses (tuple data starts one word
            // in, like vector data). A failing check deopts to baseline.
            (P::TupleRef, &[tuple, index]) => {
                let tuple = self.check_class(tuple, ClassRef::TUPLE, frame);
                let index =
                    self.value_op(Op::CheckIndex, &[tuple, index], Type::Int32, Some(frame));
                self.value_op(Op::LoadElement, &[tuple, index], Type::Any, None)
            }
            (P::TupleSize, &[tuple]) => {
                let tuple = self.check_class(tuple, ClassRef::TUPLE, frame);
                let len = self.value_op(Op::VectorLength, &[tuple], Type::Int32, Some(frame));
                self.value_op(Op::Box(Repr::Int32), &[len], Type::Any, None)
            }
            (P::TupleSet, &[tuple, index, value]) => {
                let tuple = self.check_class(tuple, ClassRef::TUPLE, frame);
                let index =
                    self.value_op(Op::CheckIndex, &[tuple, index], Type::Int32, Some(frame));
                self.write_barrier(tuple, value);
                self.effect_op(Op::StoreElement, &[tuple, index, value], None);
                self.undef()
            }
            // `car` and `cdr`: a checked pair, then the first or second word.
            (P::Car, &[pair]) => {
                let pair = self.check_class(pair, ClassRef::PAIR, frame);
                self.value_op(
                    Op::Load {
                        field: 0,
                        region: MemoryRegion::Heap,
                    },
                    &[pair],
                    Type::Any,
                    None,
                )
            }
            (P::Cdr, &[pair]) => {
                let pair = self.check_class(pair, ClassRef::PAIR, frame);
                self.value_op(
                    Op::Load {
                        field: 1,
                        region: MemoryRegion::Heap,
                    },
                    &[pair],
                    Type::Any,
                    None,
                )
            }
            // Vector primitives: the same checks as the `vector-ref`,
            // `vector-set!` and `vector-length` bytecodes.
            (P::VectorRef, &[vector, index]) => {
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                let index =
                    self.value_op(Op::CheckIndex, &[vector, index], Type::Int32, Some(frame));
                self.value_op(Op::LoadElement, &[vector, index], Type::Any, None)
            }
            (P::VectorSet, &[vector, index, value]) => {
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                let index =
                    self.value_op(Op::CheckIndex, &[vector, index], Type::Int32, Some(frame));
                self.write_barrier(vector, value);
                self.effect_op(Op::StoreElement, &[vector, index, value], None);
                self.undef()
            }
            (P::VectorLength, &[vector]) => {
                let vector = self.check_class(vector, ClassRef::VECTOR, frame);
                let len = self.value_op(Op::VectorLength, &[vector], Type::Int32, Some(frame));
                self.value_op(Op::Box(Repr::Int32), &[len], Type::Any, None)
            }
            // `sqrt` on a flonum: the hardware square root, boxed. Anything
            // else deopts, so baseline runs the instruction (and raises on a
            // non-number), exactly like the baseline's flonum fast path.
            (P::Sqrt, &[x]) => self.flonum_sqrt_fast(x, frame),
            // Bytevectors. The primitive fast paths accept both bytevector
            // classes; `bytevector-length` reads the length word (deopting
            // when it leaves int32, as the baseline slow path boxes a bignum),
            // and the u8 accessors check the same fixnum index against that
            // length. The store's value must be a fixnum in `0..=255`; every
            // other case deopts before the instruction so baseline performs
            // the same check and raises the same error.
            (P::BytevectorLength, &[bv]) => {
                let bv = self.check_bytevector(bv, frame);
                let len = self.value_op(Op::VectorLength, &[bv], Type::Int32, Some(frame));
                self.value_op(Op::Box(Repr::Int32), &[len], Type::Any, None)
            }
            (P::BytevectorU8Ref, &[bv, index]) => {
                let bv = self.check_bytevector(bv, frame);
                let index = self.value_op(Op::CheckIndex, &[bv, index], Type::Int32, Some(frame));
                let byte = self.value_op(Op::LoadBytevectorU8, &[bv, index], Type::Int32, None);
                self.value_op(Op::Box(Repr::Int32), &[byte], Type::Any, None)
            }
            (P::BvU8Set, &[bv, index, value]) => {
                let bv = self.check_bytevector(bv, frame);
                let index = self.value_op(Op::CheckIndex, &[bv, index], Type::Int32, Some(frame));
                let value = self.check_u8(value, frame);
                self.effect_op(Op::StoreBytevectorU8, &[bv, index, value], None);
                self.undef()
            }
            // `string-length` reads the length word of a checked string. The
            // baseline fast path does exactly this (payload word
            // `STRING_LENGTH / 8`), then boxes the int32.
            (P::StringLength, &[x]) => {
                let string = self.check_class(x, ClassRef::STRING, frame);
                let len = self.value_op(
                    Op::Load {
                        field: (crate::runtime::sabi::STRING_LENGTH / 8) as u32,
                        region: MemoryRegion::Heap,
                    },
                    &[string],
                    Type::Int32,
                    None,
                );
                self.value_op(Op::Box(Repr::Int32), &[len], Type::Any, None)
            }
            // `string-ref`: class check, the same unsigned bounds check as a
            // vector but against the string's length word, then a narrow
            // character load. A wide string deopts inside the load.
            (P::StringRef, &[x, index]) => {
                let string = self.check_class(x, ClassRef::STRING, frame);
                let index = self.value_op(
                    Op::CheckStringIndex,
                    &[string, index],
                    Type::Int32,
                    Some(frame),
                );
                self.value_op(Op::StringRef, &[string, index], Type::Any, Some(frame))
            }
            // `string-set!`: the same class and bounds checks as `string-ref`,
            // then a helper store. The helper deopts on an immutable or shared
            // string and on a wide character in a narrow buffer, so baseline
            // raises or detaches exactly as the primitive does.
            (P::StringSet, &[x, index, value]) => {
                let string = self.check_class(x, ClassRef::STRING, frame);
                let index = self.value_op(
                    Op::CheckStringIndex,
                    &[string, index],
                    Type::Int32,
                    Some(frame),
                );
                self.value_op(
                    Op::CallOp(Helper::StringSet.helper_ref()),
                    &[string, index, value],
                    Type::Any,
                    Some(frame),
                )
            }
            // R6RS fixnum operations: both operands must be fixnums. The
            // fast path unboxes them, performs the checked operation and
            // boxes the result; anything else (a non-fixnum, or an overflow)
            // deopts to baseline, which runs the primitive and raises.
            (P::FxAdd, &[a, b]) => self.fx_arith(P::FxAdd, Op::AddI32Ovf, a, b, frame),
            (P::FxSub, &[a, b]) => self.fx_arith(P::FxSub, Op::SubI32Ovf, a, b, frame),
            (P::FxMul, &[a, b]) => self.fx_arith(P::FxMul, Op::MulI32Ovf, a, b, frame),
            (P::FxEq, &[a, b]) => self.fx_compare(P::FxEq, Cmp::Eq, a, b, frame),
            (P::FxLt, &[a, b]) => self.fx_compare(P::FxLt, Cmp::Lt, a, b, frame),
            (P::FxLe, &[a, b]) => self.fx_compare(P::FxLe, Cmp::Le, a, b, frame),
            (P::FxGt, &[a, b]) => self.fx_compare(P::FxGt, Cmp::Gt, a, b, frame),
            (P::FxGe, &[a, b]) => self.fx_compare(P::FxGe, Cmp::Ge, a, b, frame),
            (P::FxZero, &[x]) => self.fx_predicate(P::FxZero, Cmp::Eq, x, frame),
            (P::FxPositive, &[x]) => self.fx_predicate(P::FxPositive, Cmp::Gt, x, frame),
            (P::FxNegative, &[x]) => self.fx_predicate(P::FxNegative, Cmp::Lt, x, frame),
            // `zero?`: a fixnum and a flonum answer inline; every other value
            // (bignums, rationals, non-numbers) takes the primitive.
            (P::IsZero, &[x]) => self.number_zero_fast(P::IsZero, x, frame),
            // `even?`/`odd?`: a fixnum answers from its remainder by two;
            // bignums, flonums and non-numbers take the primitive.
            (P::IsEven, &[x]) => self.number_parity_fast(P::IsEven, x, frame),
            (P::IsOdd, &[x]) => self.number_parity_fast(P::IsOdd, x, frame),
            _ => return None,
        })
    }

    /// The primitive `prim` on `args`, as the direct wrapper call. Used for
    /// the part of an inlined fast path that has no inline lowering.
    fn prim_value(&mut self, prim: Primitive, args: &[Value], frame: FrameState) -> Value {
        let op = Op::CallPrim {
            prim: PrimRef::new(prim.id() as usize),
            effects: prim_effects(prim.id() as u32),
        };
        self.value_op(op, args, Type::Any, Some(frame))
    }

    /// A raw `i32` constant.
    fn const_i32(&mut self, value: i32) -> Value {
        self.value_op(Op::Const(Constant::Int32(value)), &[], Type::Int32, None)
    }

    /// `fx+`, `fx-` and `fx*` on two fixnums: unbox to the runtime int32
    /// lane, run the checked operation and tag the result. A non-fixnum or an
    /// overflow deopts to baseline, which runs the primitive and raises
    /// exactly as it would have. The int32 lane matters: the int64 lane's
    /// box would turn an out-of-range result into a bignum instead of
    /// raising.
    fn fx_arith(
        &mut self,
        prim: Primitive,
        op: Op,
        a: Value,
        b: Value,
        frame: FrameState,
    ) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let test_b = self.func.new_block();
        let ok = self.func.new_block();
        let slow = self.func.new_block();
        let a_ok = self.value_op(Op::TestInt32, &[a], Type::Bool, None);
        self.branch_to(a_ok, test_b, slow);
        self.cur = test_b;
        let b_ok = self.value_op(Op::TestInt32, &[b], Type::Bool, None);
        self.branch_to(b_ok, ok, slow);
        self.cur = ok;
        let raw_a = self.value_op(Op::Unbox(Repr::Int32), &[a], Type::Int32, None);
        let raw_b = self.value_op(Op::Unbox(Repr::Int32), &[b], Type::Int32, None);
        let value = self.value_op(op, &[raw_a, raw_b], Type::Int32, Some(frame));
        let boxed = self.value_op(Op::Box(Repr::Int32), &[value], Type::Any, None);
        self.jump_with(join, &[boxed]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[a, b], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// The R6RS `fx=?` family: both operands fixnums, compare, box the flag.
    fn fx_compare(
        &mut self,
        prim: Primitive,
        cmp: Cmp,
        a: Value,
        b: Value,
        frame: FrameState,
    ) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let test_b = self.func.new_block();
        let ok = self.func.new_block();
        let slow = self.func.new_block();
        let a_ok = self.value_op(Op::TestInt32, &[a], Type::Bool, None);
        self.branch_to(a_ok, test_b, slow);
        self.cur = test_b;
        let b_ok = self.value_op(Op::TestInt32, &[b], Type::Bool, None);
        self.branch_to(b_ok, ok, slow);
        self.cur = ok;
        let raw_a = self.value_op(Op::Unbox(Repr::Int32), &[a], Type::Int32, None);
        let raw_b = self.value_op(Op::Unbox(Repr::Int32), &[b], Type::Int32, None);
        let test = self.value_op(Op::CmpI32(cmp), &[raw_a, raw_b], Type::Bool, None);
        let flag = self.bool_value(test);
        self.jump_with(join, &[flag]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[a, b], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// `fxzero?`, `fxpositive?` and `fxnegative?`: a fixnum compares with
    /// zero; anything else deopts to the primitive.
    fn fx_predicate(&mut self, prim: Primitive, cmp: Cmp, x: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let ok = self.func.new_block();
        let slow = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, ok, slow);
        self.cur = ok;
        let raw = self.value_op(Op::Unbox(Repr::Int32), &[x], Type::Int32, None);
        let zero = self.const_i32(0);
        let test = self.value_op(Op::CmpI32(cmp), &[raw, zero], Type::Bool, None);
        let flag = self.bool_value(test);
        self.jump_with(join, &[flag]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[x], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// `zero?`: a fixnum compares with zero, a flonum with `0.0`, and every
    /// other value takes the primitive (which handles bignums, rationals and
    /// errors).
    fn number_zero_fast(&mut self, prim: Primitive, x: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let maybe_double = self.func.new_block();
        let fixnum = self.func.new_block();
        let double = self.func.new_block();
        let slow = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, fixnum, maybe_double);
        self.cur = fixnum;
        let raw = self.value_op(Op::Unbox(Repr::Int32), &[x], Type::Int32, None);
        let zero = self.const_i32(0);
        let test = self.value_op(Op::CmpI32(Cmp::Eq), &[raw, zero], Type::Bool, None);
        let flag = self.bool_value(test);
        self.jump_with(join, &[flag]);
        self.cur = maybe_double;
        let is_double = self.value_op(Op::TestDouble, &[x], Type::Bool, None);
        self.branch_to(is_double, double, slow);
        self.cur = double;
        let raw = self.value_op(Op::Unbox(Repr::Double), &[x], Type::Double, None);
        let zero = self.value_op(
            Op::Const(Constant::Double(0.0f64.to_bits())),
            &[],
            Type::Double,
            None,
        );
        let test = self.value_op(Op::CmpF64(Cmp::Eq), &[raw, zero], Type::Bool, None);
        let flag = self.bool_value(test);
        self.jump_with(join, &[flag]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[x], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// `even?`/`odd?` for a fixnum: the remainder by two cannot overflow, so
    /// the checked remainder is a guard that never fires. Every other integer
    /// representation (bignums, and the flonums that `integer?` accepts) takes
    /// the primitive.
    fn number_parity_fast(&mut self, prim: Primitive, x: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let fixnum = self.func.new_block();
        let slow = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, fixnum, slow);
        self.cur = fixnum;
        let raw = self.value_op(Op::Unbox(Repr::Int32), &[x], Type::Int32, None);
        let two = self.const_i32(2);
        let rem = self.value_op(Op::RemI32, &[raw, two], Type::Int32, Some(frame));
        let zero = self.const_i32(0);
        let cmp = if prim == Primitive::IsEven {
            Cmp::Eq
        } else {
            Cmp::Ne
        };
        let test = self.value_op(Op::CmpI32(cmp), &[rem, zero], Type::Bool, None);
        let flag = self.bool_value(test);
        self.jump_with(join, &[flag]);
        self.cur = slow;
        let fallback = self.prim_value(prim, &[x], frame);
        self.jump_with(join, &[fallback]);
        self.cur = join;
        result
    }

    /// `fixnum` when `x` is a fixnum, `flonum` when it is a flonum, and the
    /// primitive call otherwise. The two answers are already boxed values.
    fn fixnum_or_flonum_answer(
        &mut self,
        prim: Primitive,
        x: Value,
        fixnum: Value,
        flonum: Value,
        frame: FrameState,
    ) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let maybe_flonum = self.func.new_block();
        let slow = self.func.new_block();
        let fix = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, fix, maybe_flonum);
        self.cur = fix;
        self.jump_with(join, &[fixnum]);
        self.cur = maybe_flonum;
        let fl = self.func.new_block();
        let is_flonum = self.value_op(Op::TestDouble, &[x], Type::Bool, None);
        self.branch_to(is_flonum, fl, slow);
        self.cur = fl;
        self.jump_with(join, &[flonum]);
        self.cur = slow;
        let value = self.prim_value(prim, &[x], frame);
        self.jump_with(join, &[value]);
        self.cur = join;
        result
    }

    /// `(floor x)`, `(ceiling x)` or `(truncate x)`: a fixnum is its own
    /// answer, a flonum rounds with `op` and is boxed again, and every other
    /// value takes the primitive.
    fn float_round_fast(&mut self, prim: Primitive, op: Op, x: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let maybe_flonum = self.func.new_block();
        let slow = self.func.new_block();
        let fixnum = self.func.new_block();
        let is_fixnum = self.value_op(Op::TestInt32, &[x], Type::Bool, None);
        self.branch_to(is_fixnum, fixnum, maybe_flonum);
        self.cur = fixnum;
        self.jump_with(join, &[x]);
        self.cur = maybe_flonum;
        let double = self.func.new_block();
        let is_double = self.value_op(Op::TestDouble, &[x], Type::Bool, None);
        self.branch_to(is_double, double, slow);
        self.cur = double;
        let raw = self.value_op(Op::Unbox(Repr::Double), &[x], Type::Double, None);
        let rounded = self.value_op(op, &[raw], Type::Double, None);
        let boxed = self.value_op(Op::Box(Repr::Double), &[rounded], Type::Any, None);
        self.jump_with(join, &[boxed]);
        self.cur = slow;
        let value = self.prim_value(prim, &[x], frame);
        self.jump_with(join, &[value]);
        self.cur = join;
        result
    }

    /// `(sqrt x)` for a flonum: unbox, take the hardware square root, box.
    /// A non-flonum deopts before the instruction, so baseline's `sqrt`
    /// handles it (including the error for a non-number).
    fn flonum_sqrt_fast(&mut self, x: Value, frame: FrameState) -> Value {
        let raw = self.value_op(Op::CheckDouble, &[x], Type::Double, Some(frame));
        let root = self.value_op(Op::SqrtF64, &[raw], Type::Double, None);
        self.value_op(Op::Box(Repr::Double), &[root], Type::Any, None)
    }

    /// The bytevector `value`, checked against either bytevector class. A
    /// non-bytevector deopts, exactly as the primitive's class check raises.
    fn check_bytevector(&mut self, value: Value, frame: FrameState) -> Value {
        let (join, result) = self.join_block(Type::Any);
        let maybe_immutable = self.func.new_block();
        let mutable = self.func.new_block();
        let is_mutable = self.value_op(
            Op::TestClass(ClassRef::MUTABLE_BYTEVECTOR),
            &[value],
            Type::Bool,
            None,
        );
        self.branch_to(is_mutable, mutable, maybe_immutable);
        self.cur = mutable;
        self.jump_with(join, &[value]);
        self.cur = maybe_immutable;
        let checked = self.check_class(value, ClassRef::IMMUTABLE_BYTEVECTOR, frame);
        self.jump_with(join, &[checked]);
        self.cur = join;
        result
    }

    /// The fixnum `value` as an int32 in `0..=255`. Anything else deopts, so
    /// baseline's `bytevector-u8-set!` checks it and raises if needed.
    fn check_u8(&mut self, value: Value, frame: FrameState) -> Value {
        let raw = self.value_op(Op::CheckInt32, &[value], Type::Int32, Some(frame));
        let zero = self.value_op(Op::Const(Constant::Int32(0)), &[], Type::Int32, None);
        let negative = self.value_op(Op::CmpI32(Cmp::Lt), &[raw, zero], Type::Bool, None);
        self.deopt_if(negative, frame);
        let limit = self.value_op(Op::Const(Constant::Int32(256)), &[], Type::Int32, None);
        let too_big = self.value_op(Op::CmpI32(Cmp::Ge), &[raw, limit], Type::Bool, None);
        self.deopt_if(too_big, frame);
        raw
    }

    /// `#t` when `x` is a heap object of `class`, else `#f`. Total.
    fn class_predicate(&mut self, x: Value, class: ClassRef) -> Value {
        let test = self.value_op(Op::TestClass(class), &[x], Type::Bool, None);
        self.bool_value(test)
    }

    /// `variable-bound?` without the call: the baseline's direct wrapper
    /// reads the car of the variable and tests it against the undefined
    /// word. The variable is class-checked first, so a non-variable deopts
    /// before the instruction, as the failing call does. The result is boxed
    /// the same way the wrapper's is.
    fn variable_bound(&mut self, var: Value, frame: FrameState) -> Value {
        let cell = self.check_class(var, ClassRef::VARIABLE, frame);
        let value = self.value_op(Op::LoadCell, &[cell], Type::Any, None);
        let undef = self.undef();
        let unbound = self.value_op(Op::EqBits, &[value, undef], Type::Bool, None);
        let unbound_value = self.value_op(Op::BoolToValue, &[unbound], Type::Any, None);
        // `bound` is the negation of `unbound`: a false boxed flag is bound.
        let bound = self.false_test(unbound_value);
        self.value_op(Op::BoolToValue, &[bound], Type::Any, None)
    }

    /// Branches on the truth of the register value `value`. The jump is
    /// taken when `value` is false if `when_false`, and when it is true
    /// otherwise. A value that is a boxed flag branches on the flag itself.
    fn truth_jump(
        &mut self,
        ins: &Instruction,
        pc: usize,
        len: usize,
        value: Value,
        when_false: bool,
    ) -> Result<Flow, String> {
        if let Some(flag) = self.bool_source(value) {
            // The flag is the truth itself, so no `#f` comparison is needed.
            return self.cond_jump(ins, pc, len, flag, !when_false);
        }
        let is_false = self.false_test(value);
        self.cond_jump(ins, pc, len, is_false, when_false)
    }

    /// The flag that `value` boxes, if `value` is the result of
    /// [`Op::BoolToValue`].
    fn bool_source(&self, value: Value) -> Option<Value> {
        match self.func.value_def(value) {
            ValueDef::Inst(inst) if self.func.insts[inst].op == Op::BoolToValue => {
                Some(self.func.args(inst)[0])
            }
            _ => None,
        }
    }

    /// `#t` or `#f` for the boolean `flag`. A register holds Scheme values,
    /// so a Bool is boxed where it is stored into one.
    fn bool_value(&mut self, flag: Value) -> Value {
        self.value_op(Op::BoolToValue, &[flag], Type::Any, None)
    }

    /// Ends the block with a conditional branch on `cond`, taken when
    /// `cond` is `taken_when`. The jump target is the instruction's target,
    /// and the other arm is the next instruction.
    fn cond_jump(
        &mut self,
        ins: &Instruction,
        pc: usize,
        len: usize,
        cond: Value,
        taken_when: bool,
    ) -> Result<Flow, String> {
        let target = self.target(ins, pc)?;
        let fallthrough = pc + len;
        let (then_pc, otherwise_pc) = if taken_when {
            (target, fallthrough)
        } else {
            (fallthrough, target)
        };
        let then = self.edge_to(then_pc)?;
        let otherwise = self.edge_to(otherwise_pc)?;
        self.terminate(Terminator::Branch {
            cond,
            then,
            otherwise,
        });
        Ok(Flow::Terminated)
    }

    /// Emits a safepoint, where a collection or a yield may run.
    fn safepoint(&mut self, pc: usize) -> Result<(), String> {
        let frame = self.frame(pc, pc)?;
        self.effect_op(Op::Safepoint, &[], Some(frame));
        Ok(())
    }

    /// The jump target of the instruction at `pc`.
    fn target(&self, ins: &Instruction, pc: usize) -> Result<usize, String> {
        let offset = ins
            .jump_offset()
            .ok_or_else(|| self.unsupported(ins, "has no jump target"))?;
        usize::try_from(pc as i64 + offset as i64)
            .map_err(|_| self.unsupported(ins, "jumps before the code"))
    }

    /// The edge into the block that starts at `target`, carrying the
    /// current values of its parameters.
    fn edge_to(&mut self, target: usize) -> Result<Edge, String> {
        let block = self
            .live
            .block_at(target)
            .ok_or_else(|| self.error(format!("jump to pc {target}, which starts no block")))?;
        let ir = self.ir_block[block]
            .ok_or_else(|| self.error(format!("jump to unreachable block {block}")))?;
        let mut args = Vec::new();
        if let Some(regs) = self.param_regs[block].clone() {
            for reg in regs {
                args.push(self.value_or_undef(reg));
            }
        }
        Ok(self.func.edge(ir, &args))
    }

    /// The value of the pool constant that `value` is, when it is one.
    fn pool_constant(&self, value: Value) -> Option<SchemeValue<'gc>> {
        let ValueDef::Inst(inst) = self.func.value_def(value) else {
            return None;
        };
        match self.func.insts[inst].op {
            Op::Const(Constant::Pool { code, index }) if code == root() => {
                Some(self.buffer.constant(index as usize))
            }
            _ => None,
        }
    }

    /// The constant that reading the variable `var` yields, when `var` is a
    /// bound variable constant.
    fn fold_variable(&mut self, var: Value) -> Option<Value> {
        if !self.fold {
            return None;
        }
        let variable = self.pool_constant(var)?;
        let current = self.foldable_value(variable)?;
        self.folded_value(variable, current)
    }

    /// The value folding the variable cell `variable` would yield, when it
    /// is a bound variable whose watchpoint set has not fired. Registering
    /// the dependency is [`folded_value`](Self::folded_value)'s job, so a
    /// caller can first emit the runtime guard its fold needs.
    fn foldable_value(&self, variable: SchemeValue<'gc>) -> Option<SchemeValue<'gc>> {
        if !variable.is::<crate::runtime::variable::Variable>() {
            return None;
        }
        let variable_ref = variable.downcast::<crate::runtime::variable::Variable>();
        let current = variable_ref.get();
        if current == SchemeValue::undefined() {
            return None;
        }
        // A set that fired once stays invalid, so a variable that changes is
        // not folded again.
        if let Some(set) = variable_ref.watch.get() {
            if set.is_invalid() {
                return None;
            }
        }
        Some(current)
    }

    /// Records the unit's dependency on `variable` holding `current` and
    /// returns the constant for `current`. `None` when the same cell was
    /// already folded to another value in this build.
    fn folded_value(
        &mut self,
        variable: SchemeValue<'gc>,
        current: SchemeValue<'gc>,
    ) -> Option<Value> {
        let dep = crate::runtime::codegen::opt::ir::func::GlobalDep {
            variable,
            value: current,
        };
        match self.func.deps.iter().find(|d| d.variable == dep.variable) {
            Some(old) if old.value != dep.value => return None,
            Some(_) => {}
            None => {
                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "  opt: folded variable {} = {}",
                    variable,
                    current
                );
                self.func.deps.push(dep)
            }
        }
        Some(self.value_const(current))
    }

    /// The constant that reading the global through free variable `index`
    /// of the entry closure yields, when that cell held a foldable variable
    /// at build time. The build only knows the requesting closure's cells,
    /// so the fold is guarded: the emitted code compares the cell the read
    /// actually came through with the folded cell and loads the cell on a
    /// mismatch (another closure of the same code, a reloaded module). A
    /// write to the folded variable invalidates the unit through the
    /// dependency, as for a pool-constant variable.
    fn fold_free_variable(&mut self, pc: usize, var: Value) -> Result<Option<Value>, String> {
        // `free_cells` comes from the one clause closure that requested the
        // tier-up, but the unit spans every clause: the same free-slot index
        // means a different capture in each. Keep the loads unfolded here so
        // every clause reads its own cell instead of relying on a guard to
        // detect the mismatch.
        if !self.fold || self.inline.depth != 0 || self.is_case {
            return Ok(None);
        }
        // The cell must come from a `load.free` through the entry closure.
        let ValueDef::Inst(inst) = self.func.value_def(var) else {
            return Ok(None);
        };
        let (index, closure) = match self.func.insts[inst].op {
            Op::LoadFree { index } => {
                let args = self.func.args(inst);
                (index, args.first().copied())
            }
            _ => return Ok(None),
        };
        if closure != self.entry_closure {
            return Ok(None);
        }
        let Some(&cell) = self.free_cells.get(index as usize) else {
            return Ok(None);
        };
        let Some(current) = self.foldable_value(cell) else {
            return Ok(None);
        };
        // Guard the cell identity. A mismatch is not a deopt: another
        // closure of this code (a second library instance, say) reads its
        // own cell, so the fallback path loads it, exactly as the
        // unfolded read does. Only the folded path registers the
        // dependency, since only it must be invalidated on a write.
        let frame = self.frame(pc, pc)?;
        let expected = self.bits(cell.bits());
        let same = self.value_op(Op::EqBits, &[var, expected], Type::Bool, None);
        let folded_block = self.func.new_block();
        let load_block = self.func.new_block();
        let join = self.func.new_block();
        let result = self.func.append_param(join, Type::Any);
        let then = self.func.edge(folded_block, &[]);
        let otherwise = self.func.edge(load_block, &[]);
        self.terminate(Terminator::Branch {
            cond: same,
            then,
            otherwise,
        });
        // When the folded value is a procedure, both paths into the join must
        // agree on its code block, so the join parameter carries the code
        // fact and the checks guarding the inlined body fold away. The load
        // path checks the code; a different one deopts and baseline runs the
        // call generically.
        let folded_code = current
            .is::<crate::runtime::objectmodel::Closure>()
            .then(|| {
                let closure = crate::runtime::objectmodel::Closure::from_value(current);
                self.code_for_heap(SchemeValue::from(
                    crate::runtime::objectmodel::Closure::code_block(closure),
                ))
            });
        self.cur = load_block;
        let cell_value = self.check_class(var, ClassRef::VARIABLE, frame);
        let loaded = self.value_op(Op::LoadCell, &[cell_value], Type::Any, None);
        let loaded = match folded_code {
            Some(code) => self.value_op(
                Op::CheckCode(code),
                &[loaded],
                Type::Closure(Some(code)),
                Some(frame),
            ),
            None => loaded,
        };
        let edge = self.func.edge(join, &[loaded]);
        self.terminate(Terminator::Jump(edge));
        self.cur = folded_block;
        // On a dependency conflict there is no folded value; the folded
        // path then also loads the cell (already class-checked by the
        // guard's compare, but `check_class` is what refines the type).
        let folded = self.folded_value(cell, current);
        let value = match folded {
            Some(value) => value,
            None => {
                let frame = self.frame(pc, pc)?;
                let cell_value = self.check_class(var, ClassRef::VARIABLE, frame);
                let loaded = self.value_op(Op::LoadCell, &[cell_value], Type::Any, None);
                match folded_code {
                    Some(code) => self.value_op(
                        Op::CheckCode(code),
                        &[loaded],
                        Type::Closure(Some(code)),
                        Some(frame),
                    ),
                    None => loaded,
                }
            }
        };
        let edge = self.func.edge(join, &[value]);
        self.terminate(Terminator::Jump(edge));
        self.cur = join;
        Ok(Some(result))
    }

    /// The IR value of the constant `value`: an immediate in the code, or a
    /// slot of the unit's constant table for a heap object.
    fn value_const(&mut self, value: SchemeValue<'gc>) -> Value {
        if !value.is_cell() {
            return self.bits(value.bits());
        }
        let slot = self.func.heap_constant(value);
        let ty = if value.is::<crate::runtime::objectmodel::Closure>() {
            // The code block is known at build time: type the constant with
            // it so the class and code checks that guard an inlined body fold
            // away.
            let closure = crate::runtime::objectmodel::Closure::from_value(value);
            let code = self.code_for_heap(SchemeValue::from(
                crate::runtime::objectmodel::Closure::code_block(closure),
            ));
            Type::Closure(Some(code))
        } else {
            Type::Any
        };
        self.value_op(Op::Const(Constant::Table(slot)), &[], ty, None)
    }

    /// The register `r`, or an error when it has no value on this path.
    fn reg(&self, r: u32) -> Result<Value, String> {
        self.state
            .get(r as usize)
            .copied()
            .flatten()
            .ok_or_else(|| self.error(format!("r{r} is read before it is written")))
    }

    fn regs(&self, range: std::ops::Range<u32>) -> Result<Vec<Value>, String> {
        range.map(|r| self.reg(r)).collect()
    }

    fn regs_of(&self, regs: &[u32]) -> Result<Vec<Value>, String> {
        regs.iter().map(|&r| self.reg(r)).collect()
    }

    fn set(&mut self, r: u32, value: Value) -> Result<(), String> {
        match self.state.get_mut(r as usize) {
            Some(slot) => {
                *slot = Some(value);
                Ok(())
            }
            None => Err(self.error(format!("r{r} is outside the frame"))),
        }
    }

    /// The register `r`, or a placeholder when it has no value. A register
    /// that is live on one path and unset on another gets one at a join.
    fn value_or_undef(&mut self, r: u32) -> Value {
        match self.state.get(r as usize).copied().flatten() {
            Some(value) => value,
            None => self.undef(),
        }
    }

    /// Tracks the moves that set up a tail call. The bytecode compiler
    /// lowers a tail call to parallel moves into registers 1 (the callee)
    /// and up, then `OpTailCall`. Once a move writes register 1, the frame's
    /// closure slot holds the callee, while baseline and the interpreter
    /// find a frame's code block through that slot. A frame state taken
    /// there would resume the callee's code at this procedure's pc.
    ///
    /// So the first move into register 1 records the register file before
    /// it, and frames up to the tail call resume at that move instead. The
    /// moves in between are register moves and constant loads, which
    /// baseline can run again. Any other instruction ends the window.
    fn note_tail_setup(&mut self, ins: &Instruction, pc: usize) {
        let writes_r1 = match ins {
            Instruction::OpMove(op) => op.dst.0 == 1,
            Instruction::OpMoveTwo(op) => op.dst1.0 == 1 || op.dst2.0 == 1,
            Instruction::OpLoadConstant(op) => op.dst.0 == 1,
            Instruction::OpLoadImmediate(op) => op.dst.0 == 1,
            Instruction::OpLoadRaw(op) => op.dst.0 == 1,
            Instruction::OpTailCall(_) => return,
            _ => {
                self.tail_setup = None;
                return;
            }
        };
        if writes_r1 && self.tail_setup.is_none() {
            self.tail_setup = Some((pc, self.state.clone()));
        }
    }

    /// The frame state of the instruction at `live_pc`, for the caller
    /// resuming at `state_pc`. Inside a tail call's setup the frame resumes
    /// at the setup's first write of register 1 (see
    /// [`Self::note_tail_setup`]).
    fn frame(&mut self, state_pc: usize, live_pc: usize) -> Result<FrameState, String> {
        let Some((at, regs)) = self.tail_setup.clone() else {
            return self.frame_here(state_pc, live_pc);
        };
        let current = std::mem::replace(&mut self.state, regs);
        let frame = self.frame_here(at, at);
        self.state = current;
        frame
    }

    fn frame_here(&mut self, state_pc: usize, live_pc: usize) -> Result<FrameState, String> {
        // The rest area holds the rest elements, which live in the vector
        // until a register is written. Its registers are also temporaries
        // (a call's frame reuses them), so one written on this path is state
        // like any other and must reach the frame.
        let rest = self.rest.or_else(|| {
            self.case_ranges
                .iter()
                .find(|(start, end, _)| live_pc >= *start && live_pc < *end)
                .and_then(|(_, _, rest)| *rest)
        });
        let live: Vec<u32> = self
            .live
            .live_at(live_pc)
            .ok_or_else(|| self.error(format!("no instruction at pc {live_pc}")))?
            .iter()
            .map(|r| r as u32)
            .filter(|&r| match rest {
                Some(rest) if r >= rest.base => {
                    self.state.get(r as usize).copied().flatten().is_some()
                }
                _ => true,
            })
            .collect();
        let closure = self.reg(1)?;
        let mut registers = Vec::with_capacity(live.len() + 1);
        for reg in live {
            let value = self.value_or_undef(reg);
            registers.push((reg, value));
        }
        // Baseline reads the rest area wherever it resumes, so every frame
        // keeps the vector that rebuilds it.
        if let Some(rest) = rest {
            for reg in rest.reg..rest.reg + rest.width() {
                registers.push((reg, self.reg(reg)?));
            }
        }
        Ok(self.func.push_frame(FrameStateData {
            code: root(),
            pc: state_pc as u32,
            closure,
            registers,
            parent: None,
            call_base: None,
            returned: None,
            rest,
            inline_site: None,
        }))
    }

    fn constant(&self, index: u32) -> Constant {
        let value = self.buffer.constant(index as usize);
        if value.is_cell() {
            Constant::Pool {
                code: root(),
                index,
            }
        } else {
            Constant::Bits(value.bits())
        }
    }

    fn bits(&mut self, bits: u64) -> Value {
        self.konst(Constant::Bits(bits))
    }

    fn undef(&mut self) -> Value {
        self.bits(SchemeValue::VALUE_UNDEFINED as u64)
    }

    /// `#f` as a test: true when `value` is false.
    fn false_test(&mut self, value: Value) -> Value {
        let false_value = self.bits(SchemeValue::VALUE_FALSE as u64);
        self.value_op(Op::EqBits, &[value, false_value], Type::Bool, None)
    }

    /// A constant, emitted into the current block.
    fn konst(&mut self, constant: Constant) -> Value {
        self.value_op(Op::Const(constant), &[], Type::Any, None)
    }

    fn terminate(&mut self, term: Terminator) {
        self.func.set_terminator(self.cur, term);
    }

    /// Emits an operation whose result has type `ty`.
    fn value_op(&mut self, op: Op, args: &[Value], ty: Type, frame: Option<FrameState>) -> Value {
        self.func
            .push_inst(self.cur, op, args, Some(ty), frame)
            .expect("an operation with a result returns one")
    }

    /// Emits an operation that produces no value.
    fn effect_op(&mut self, op: Op, args: &[Value], frame: Option<FrameState>) {
        self.func.push_inst(self.cur, op, args, None, frame);
    }

    fn unsupported(&self, ins: &Instruction, why: &str) -> String {
        self.error(format!("{} {why}", opcode_name(ins)))
    }

    fn error(&self, message: String) -> String {
        format!("pc {}: {message}", self.pc)
    }
}

/// The largest vector `make-vector` and `vector` allocate inline.
const MAX_INLINE_VECTOR: usize = 16;

/// The most extra arguments a call site passes to an inlined variadic callee.
/// The deopt of the inlined frame writes them past the callee's frame, which
/// the prologue reserves.
pub(crate) const MAX_INLINED_EXTRAS: u32 = 8;

/// The most arguments a direct primitive wrapper takes: the first three
/// argument registers after the context and the count.
const MAX_DIRECT_PRIM_ARGS: usize = 3;

/// The most arguments a packed variadic primitive call takes. They sit in
/// words above the frame, which the prologue checks.
const MAX_PACKED_PRIM_ARGS: usize = 32;

/// Why primitive `prim` cannot be called through its direct wrapper with
/// `argc` arguments, or `None` when it can. The baseline's `prim_direct`
/// calls the wrapper, and only a fixed-arity wrapper takes exactly the count
/// it names. A variadic primitive goes through the packed slow path instead,
/// so it has no direct call here.
/// The memory effects of a primitive call. A primitive does not run Scheme
/// code and does not write a variable cell; `variable-set!` is the one
/// exception (the builder normally routes it through `Helper::VariableSet`,
/// but a primitive call site can still name it).
fn prim_effects(prim: u32) -> Effects {
    if Primitive::from_id(prim as u16) == Some(Primitive::VariableSet) {
        Effects::for_call()
    } else {
        Effects::for_primitive()
    }
}

/// Whether the direct wrapper of the primitive `prim` accepts `argc`
/// arguments; see [`Op::CallPrim`](crate::runtime::codegen::opt::ir::inst::Op).
fn direct_prim_error(prim: u32, argc: usize) -> Option<String> {
    let name = Primitive::from_id(prim as u16)
        .map_or_else(|| format!("primitive {prim}"), |p| p.name().to_string());
    match PRIM_ARITY.get(prim as usize) {
        None => Some(format!("calls {name}, which is not a primitive")),
        // A variadic primitive takes any count through the packed helper.
        Some(&arity) if arity < 0 => (argc > MAX_PACKED_PRIM_ARGS).then(|| {
            format!("passes {argc} arguments to {name}, and a packed call takes at most {MAX_PACKED_PRIM_ARGS}")
        }),
        Some(_) if argc > MAX_DIRECT_PRIM_ARGS => Some(format!(
            "passes {argc} arguments to {name}, and a direct wrapper takes at most {MAX_DIRECT_PRIM_ARGS}"
        )),
        Some(&arity) if arity as usize != argc => Some(format!(
            "passes {argc} arguments to {name}, which takes {arity}"
        )),
        Some(_) => None,
    }
}

/// The blocks reachable from the entry, in reverse postorder.
fn reverse_postorder(live: &Liveness, skip: &HashSet<usize>) -> Vec<usize> {
    let blocks = live.blocks();
    let mut visited = vec![false; blocks.len()];
    let mut post = Vec::new();
    let mut stack = vec![(0usize, 0usize)];
    visited[0] = true;
    while let Some(&(block, next)) = stack.last() {
        let succs = &blocks[block].successors;
        if next < succs.len() {
            stack.last_mut().expect("stack is not empty").1 += 1;
            let succ = succs[next];
            if !visited[succ] && !skip.contains(&succ) {
                visited[succ] = true;
                stack.push((succ, 0));
            }
        } else {
            post.push(block);
            stack.pop();
        }
    }
    post.reverse();
    post
}

/// The multiple-value entry at `pc`.
struct ForwardEntry {
    nvals: u32,
    rest: bool,
    frame: u32,
}

fn forward_entry(code: &[u8], pc: usize) -> Option<ForwardEntry> {
    match Instruction::decode_slice(code, pc)? {
        (Instruction::OpReturnPointValues(op), _, _) => Some(ForwardEntry {
            nvals: op.nvals.0,
            rest: false,
            frame: op.frame.0,
        }),
        (Instruction::OpReturnPointValuesRest(op), _, _) => Some(ForwardEntry {
            nvals: op.nvals.0,
            rest: true,
            frame: op.frame.0,
        }),
        _ => None,
    }
}

/// The name of an instruction's opcode, from its `Debug` form.
fn opcode_name(ins: &Instruction) -> String {
    format!("{ins:?}")
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect()
}

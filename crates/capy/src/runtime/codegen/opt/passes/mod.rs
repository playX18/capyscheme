//! Optimization passes over the SSA IR.
//!
//! [`run`] applies the passes in rounds until a round changes nothing or the
//! gas budget runs out. A pass is a complete rewrite of the function: it leaves
//! the IR valid when it returns, so the pipeline may stop between passes.
//!
//! The passes:
//!
//! - [`simplify`]: constant folding, branch folding, trivial block
//!   parameters, block merging.
//! - [`range`]: forward interval analysis over `Int32`/`Int64`
//!   values and the rewrites its facts prove: guards shown to succeed,
//!   branches and `box.bool` shown constant, and checked i64 arithmetic shown
//!   not to overflow. It runs before [`typeprop`], which then sees the
//!   reduced guards.
//! - [`typeprop`]: redundant guard elimination along the dominator
//!   tree, with guard facts from dominating guards and branches.
//! - [`gvn`]: dominator-scoped numbering of pure values, and
//!   extended-basic-block forwarding of loads past non-clobbering code.
//! - [`devirt`]: `CallGeneric` whose callee is a checked or freshly
//!   allocated closure becomes a `CallKnown` to that code block.
//! - [`peel`]: copies the first iteration of a loop with
//!   loop-invariant guards out of the loop. The copy runs the guards once and
//!   dominates the loop proper, so [`typeprop`] and [`gvn`] delete the loop's
//!   copies and [`licm`] hoists the loads they protect.
//! - [`licm`]: hoisting of invariant pure operations, loads and
//!   header guards to a preheader.
//! - [`version`]: loop versioning on invariant guards. A pure test
//!   on the preheader selects a clone of the loop with the guards removed, or
//!   the original loop, so a guard inside an inlined body leaves a pre-tested
//!   loop without changing error timing.
//! - [`unswitch`]: loop unswitching. A branch on a loop-invariant
//!   condition is tested once on the preheader, which enters one of two clones
//!   of the loop with the branch folded to one arm. Loop-defined values read
//!   after the loop go through exit-block parameters first
//!   ([`lcssa`]).
//! - [`strength`]: integer strength reduction with range facts:
//!   multiply by a power of two becomes an addition, quotient/remainder/modulo
//!   by a constant fold to an identity or a constant offset, and a product of a
//!   loop counter with a constant becomes a loop-carried value.
//! - [`unroll`]: 2x unrolling of natural loops whose header exit
//!   test is a canonical comparison on a counter that steps by one. The second
//!   copy runs without re-testing, while the original header remains the
//!   one-iteration tail so an OSR entry or a half-finished pair still runs the
//!   right number of iterations.
//! - [`forward`]: store-to-load forwarding and write-barrier
//!   elision on objects allocated in the same block.
//! - [`bce`]: bounds checks proved by a dominating `index < length`
//!   test and a non-negative counter.
//! - [`sroa`]: whole-function escape analysis and scalar replacement
//!   of non-escaping pairs, closures, envs, vectors and tuples, with remat
//!   markers for objects that frame states reference. It runs before [`repr`] so
//!   representation selection can type the scalarized fields.
//! - [`cells`]: local cell elimination. A `make-variable` cell that
//!   cannot escape becomes SSA values: reads forward, writes redefine, and a
//!   loop-carried value becomes a block parameter. Frames that reference the cell
//!   are rebuilt with `RematKind::Variable` at deopt. It runs
//!   before [`repr`] so the loop-carried value can become a raw `Double`.
//! - [`repr`]: typed block parameters (loop-carried flonums stay in
//!   FPRs) and raw frame states.
//! - [`dce`]: dead pure instructions and unreachable blocks.

pub(crate) mod bce;
pub(crate) mod cells;
pub(crate) mod dce;
pub(crate) mod devirt;
pub(crate) mod forward;
pub(crate) mod gvn;
pub(crate) mod layout;
pub(crate) mod lcssa;
pub(crate) mod licm;
pub(crate) mod loops;
pub(crate) mod peel;
pub(crate) mod predicate;
pub(crate) mod range;
pub(crate) mod repr;
pub(crate) mod simplify;
pub(crate) mod sroa;
pub(crate) mod strength;
pub(crate) mod typeprop;
pub(crate) mod unroll;
pub(crate) mod unswitch;
pub(crate) mod version;

#[cfg(test)]
pub(crate) mod testutil;

use cranelift_entity::{EntityList, ListPool, SecondaryMap};

use super::ir::func::{Block, Func, Value};
use super::ir::inst::{Edge, EffectFlags, MemoryRanges, Op, Terminator};
use super::ir::verify::verify;
use crate::runtime::codegen::jit::config::opt;
use crate::utils::flags;

pub(crate) const DEFAULT_GAS: usize = 400_000;

/// What one run of a pass did.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct PassOutcome {
    /// Whether the pass changed the function.
    pub(crate) changed: bool,
    /// Work units spent, charged against the gas budget.
    pub(crate) work: usize,
}

/// Totals of one [`run`].
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct PipelineStats {
    /// Rounds through the whole pipeline.
    pub(crate) rounds: usize,
    pub(crate) gas_used: usize,
    pub(crate) ran_out_of_gas: bool,
}

struct PassDef {
    name: &'static str,
    run: fn(&mut Func) -> PassOutcome,
}

const PIPELINE: [PassDef; 19] = [
    PassDef {
        name: "simplify",
        run: simplify::run,
    },
    PassDef {
        name: "range",
        run: range::run,
    },
    PassDef {
        name: "typeprop",
        run: typeprop::run,
    },
    PassDef {
        name: "gvn",
        run: gvn::run,
    },
    PassDef {
        name: "devirt",
        run: devirt::run,
    },
    PassDef {
        name: "peel",
        run: peel::run,
    },
    PassDef {
        name: "licm",
        run: licm::run,
    },
    PassDef {
        name: "version",
        run: version::run,
    },
    PassDef {
        name: "unswitch",
        run: unswitch::run,
    },
    PassDef {
        name: "strength",
        run: strength::run,
    },
    PassDef {
        name: "unroll",
        run: unroll::run,
    },
    PassDef {
        name: "unroll_all",
        run: peel::run_unroll,
    },
    PassDef {
        name: "forward",
        run: forward::run,
    },
    PassDef {
        name: "bce",
        run: bce::run,
    },
    PassDef {
        name: "predicate",
        run: predicate::run,
    },
    PassDef {
        name: "sroa",
        run: sroa::run,
    },
    PassDef {
        name: "cells",
        run: cells::run,
    },
    PassDef {
        name: "repr",
        run: repr::run,
    },
    PassDef {
        name: "dce",
        run: dce::run,
    },
];

/// Runs the pass pipeline on `func` until it reaches a fixed point or runs
/// out of gas. `name` names the dump files.
pub(crate) fn run(func: &mut Func, name: &str) -> Result<PipelineStats, String> {
    let config = opt();
    let mut stats = PipelineStats::default();
    let mut gas = DEFAULT_GAS;
    'rounds: loop {
        stats.rounds += 1;
        let mut changed = false;
        for pass in &PIPELINE {
            if gas == 0 {
                stats.ran_out_of_gas = true;
                break 'rounds;
            }
            let outcome = (pass.run)(func);
            gas = gas.saturating_sub(outcome.work);
            stats.gas_used += outcome.work;
            changed |= outcome.changed;
            if config.verify {
                verify(func).map_err(|errors| {
                    format!(
                        "pass {} produced invalid IR: {}",
                        pass.name,
                        errors.join("; ")
                    )
                })?;
            }
            if config.dump.iter().any(|stage| stage == pass.name) {
                dump(func, name, pass.name, stats.rounds);
            }
        }
        if !changed {
            break;
        }
    }
    log::debug!(
        target: crate::heap::logging::JIT_LOG_TARGET,
        " opt passes {name}: {} rounds, {} gas{}",
        stats.rounds,
        stats.gas_used,
        if stats.ran_out_of_gas { ", out of gas" } else { "" }
    );
    Ok(stats)
}

/// Writes `func` after `pass` ran in `round`, for `CAPY_OPT_DUMP`.
fn dump(func: &Func, name: &str, pass: &str, round: usize) {
    let text = func.to_string();
    let Some(dir) = flags::jit_dump_dir() else {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::info!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "{name} after {pass} round {round}\n{text}"
        );
        return;
    };
    let file = format!(
        "opt-{pass}-{:04}-{}-{round}.ir",
        super::next_dump_id(),
        super::file_safe(name)
    );
    let written =
        std::fs::create_dir_all(dir).and_then(|()| std::fs::write(dir.join(&file), &text));
    if let Err(err) = written {
        crate::heap::logging::ensure_max_level(log::LevelFilter::Info);
        log::warn!(
            target: crate::heap::logging::DUMP_LOG_TARGET,
            "cannot write {pass} dump to {}: {err}",
            dir.display()
        );
    }
}

/// A map from values to their replacements, applied in one sweep by
/// [`replace_uses`]. A replacement may itself be replaced, so lookups go
/// through [`Subst::resolve`].
#[derive(Default)]
pub(crate) struct Subst {
    map: SecondaryMap<Value, Option<Value>>,
    len: usize,
}

impl Subst {
    /// Makes every use of `from` use `to` instead. `from` must not already
    /// have a replacement, and `to` must not resolve to `from`.
    pub(crate) fn add(&mut self, from: Value, to: Value) {
        debug_assert!(self.map[from].is_none(), "{from} is replaced twice");
        let to = self.resolve(to);
        debug_assert_ne!(to, from, "replacement cycle at {from}");
        if to == from {
            return;
        }
        self.map[from] = Some(to);
        self.len += 1;
    }

    /// The value `value` stands for after all replacements.
    pub(crate) fn resolve(&self, value: Value) -> Value {
        let mut cur = value;
        let mut steps = 0;
        while let Some(next) = self.map[cur] {
            cur = next;
            steps += 1;
            debug_assert!(steps <= self.len, "replacement cycle through {value}");
            if steps > self.len {
                break;
            }
        }
        cur
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Whether `value` already has a replacement.
    pub(crate) fn contains(&self, value: Value) -> bool {
        self.map[value].is_some()
    }
}

/// Rewrites every operand, edge argument and frame-state value through
/// `subst`: instructions, terminators and all frame states.
pub(crate) fn replace_uses(func: &mut Func, subst: &Subst) {
    if subst.is_empty() {
        return;
    }
    let Func {
        blocks,
        insts,
        frames,
        pool,
        ..
    } = func;
    for (_, data) in blocks.iter_mut() {
        for &inst in &data.insts {
            let args = insts[inst].args;
            if let Some(new) = remap_list(pool, args, subst) {
                insts[inst].args = new;
            }
        }
        if let Some(term) = data.term.as_mut() {
            remap_terminator(term, pool, subst);
        }
    }
    for (_, frame) in frames.iter_mut() {
        frame.closure = subst.resolve(frame.closure);
        for (_, value) in frame.registers.iter_mut() {
            *value = subst.resolve(*value);
        }
    }
}

fn remap_list(
    pool: &mut ListPool<Value>,
    list: EntityList<Value>,
    subst: &Subst,
) -> Option<EntityList<Value>> {
    let old: Vec<Value> = list.as_slice(pool).to_vec();
    if old.iter().all(|&value| subst.resolve(value) == value) {
        return None;
    }
    let new: Vec<Value> = old.iter().map(|&value| subst.resolve(value)).collect();
    Some(EntityList::from_slice(&new, pool))
}

fn remap_edge(edge: &mut Edge, pool: &mut ListPool<Value>, subst: &Subst) {
    if let Some(new) = remap_list(pool, edge.args, subst) {
        edge.args = new;
    }
}

fn remap_terminator(term: &mut Terminator, pool: &mut ListPool<Value>, subst: &Subst) {
    match term {
        Terminator::Jump(edge) => remap_edge(edge, pool, subst),
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => {
            *cond = subst.resolve(*cond);
            remap_edge(then, pool, subst);
            remap_edge(otherwise, pool, subst);
        }
        Terminator::Switch {
            value,
            cases,
            default,
            ..
        } => {
            *value = subst.resolve(*value);
            for (_, edge) in cases.iter_mut() {
                remap_edge(edge, pool, subst);
            }
            remap_edge(default, pool, subst);
        }
        Terminator::Return(value) => *value = subst.resolve(*value),
        Terminator::TailCall { callee, args, .. } => {
            *callee = subst.resolve(*callee);
            if let Some(new) = remap_list(pool, *args, subst) {
                *args = new;
            }
        }
        Terminator::Deopt { .. } | Terminator::Unreachable => {}
    }
}

/// The blocks reachable from the entry.
pub(crate) fn reachable(func: &Func) -> SecondaryMap<Block, bool> {
    let mut seen: SecondaryMap<Block, bool> = SecondaryMap::new();
    for block in func.reverse_postorder() {
        seen[block] = true;
    }
    seen
}

/// Whether `op` reads memory without writing it, and can neither fail nor
/// call. Such an operation is a load, and its result stays valid until a
/// clobber of the region it reads.
fn is_load(op: Op) -> bool {
    let effects = op.effects();
    op.has_result()
        && !op.is_call()
        && !op.can_deopt()
        && effects.causes.is_empty()
        && effects.writes.is_empty()
        && !effects.reads.is_empty()
}

/// The memory an operation may write in an *existing* object. An allocation
/// writes only the object it creates, and a write barrier only logs an object
/// for the collector, so neither can change what a load of another object
/// reads. Both are still effects (an allocation is not neutral), but they do
/// not invalidate a load.
pub(crate) fn writes_existing(op: Op) -> MemoryRanges {
    if op == Op::WriteBarrier || op.effects().allocation_only() {
        MemoryRanges::NONE
    } else {
        clobbers(op)
    }
}

/// The memory an operation may write. An operation that may run arbitrary
/// code, such as a Scheme call or a safepoint, clobbers all of it, so the
/// result is [`MemoryRanges::TOP`]. A primitive call carries a precise effect
/// summary ([`Effects::for_primitive`]) and is trusted; `EXITS_SIDEWAYS`
/// marks the calls that run arbitrary code.
fn clobbers(op: Op) -> MemoryRanges {
    let effects = op.effects();
    let barrier = effects.flags.contains(EffectFlags::EXITS_SIDEWAYS)
        || effects.flags.contains(EffectFlags::TERMINAL)
        || effects.flags.contains(EffectFlags::WRITES_LOCAL_STATE)
        || effects.flags.contains(EffectFlags::WRITES_PINNED)
        || effects.flags.contains(EffectFlags::FENCE);
    if barrier {
        MemoryRanges::TOP
    } else {
        effects.writes
    }
}

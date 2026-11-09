//! Inlining
//!
//! The callee is built as a function of its own (so its profile, its exit
//! profile and its constants are its own) and then spliced into the caller's
//! function. The call keeps its place in the frame states: every frame state
//! of the callee gets the call-site frame of the caller as its parent, so a
//! deopt inside the body rebuilds the callee's interpreter frame on top of
//! the caller's, as the interpreter would have left them.
//!
//! The guard is a closure check plus a code-block check, so a closure of the
//! same code with other free variables takes the inlined path too. A failing
//! guard deopts at the call, where baseline runs it generically.

use std::cell::Cell;
use std::rc::Rc;

use cranelift_entity::{EntityList, EntityRef, SecondaryMap};

use super::profile::{self, CallShape};
use super::*;
use crate::bytecode::def::Instruction;
use crate::runtime::codegen::opt::ir::inst::InstData;

/// The deepest inlining nest. The root is depth 0.
pub(super) const MAX_DEPTH: u32 = 5;
/// The largest callee, in bytecode instructions.
pub(super) const MAX_CALLEE_INSTRUCTIONS: usize = 64;
/// The bytecode instructions all inlined callees of a root may add up to.
pub(super) const ROOT_BUDGET: u32 = 256;
/// The most targets a polymorphic site inlines.
pub(super) const MAX_POLY: usize = 4;

/// Calls a monomorphic site must have made before it may inline. A site in a
/// hot procedure can still be cold on its own, and its profile is cheapest to
/// trust once it has run a few dozen times.
pub(super) const MIN_SITE_HITS: u32 = 8;
/// Calls one target of a polymorphic site must have made. The site itself
/// also needs [`MIN_SITE_HITS`] in total.
pub(super) const MIN_TARGET_HITS: u32 = 4;
/// Hits at which a site earns the whole hit credit.
const FULL_HIT_CREDIT: u32 = 4096;

/// Score of a site with the minimum hits and a tiny callee in a small caller.
const BASE_SCORE: i64 = 10;
/// Additional score at [`FULL_HIT_CREDIT`].
const MAX_HIT_SCORE: i64 = 34;
/// A loop in the callee makes the call boundary worth removing (the loop
/// stays in one frame and its values stay in registers).
const LOOP_BONUS: i64 = 8;
const MAX_SCORED_LOOPS: u32 = 2;
/// A call inside the callee is not removed by inlining and can grow the
/// caller, so it costs a little.
const CALL_PENALTY: i64 = 3;
const MAX_SCORED_CALLS: u32 = 8;
/// An allocation inside the callee stays an allocation (until SROA can see
/// it) and grows the caller's frame states.
const ALLOC_PENALTY: i64 = 2;
const MAX_SCORED_ALLOCS: u32 = 8;
/// One bytecode instruction of callee body costs a quarter point.
const SIZE_PENALTY_DIV: i64 = 4;
/// Each enclosing inlined body costs; deep nests are the hardest to deopt.
const DEPTH_PENALTY: i64 = 8;
/// A large caller is register-pressured, and an inlined body's guards run
/// with its frame live. One point per this many caller instructions.
const CALLER_PENALTY_DIV: usize = 96;
const MAX_CALLER_PENALTY: i64 = 10;
const TINY_ACCESSOR_INSTRUCTIONS: usize = 12;
const LARGE_CALLER_INSTRUCTIONS: usize = 128;

#[derive(Clone)]
pub(super) struct InlineCx<'gc> {
    pub enabled: bool,
    /// Whether a site with several targets may be inlined.
    pub poly: bool,
    pub depth: u32,
    /// The budget left to the whole root, shared by every nest.
    pub remaining: Rc<Cell<u32>>,
    /// The code blocks being built, outermost first, as rooted values. Only
    /// compared, never dereferenced beyond the comparison.
    pub chain: Vec<crate::runtime::objectmodel::Value<'gc>>,
}

impl<'gc> InlineCx<'gc> {
    pub fn root(
        enabled: bool,
        poly: bool,
        code_block: crate::runtime::objectmodel::Value<'gc>,
    ) -> Self {
        Self {
            enabled,
            poly,
            depth: 0,
            remaining: Rc::new(Cell::new(ROOT_BUDGET)),
            chain: vec![code_block],
        }
    }

    pub fn disabled() -> Self {
        Self::root(false, false, crate::runtime::objectmodel::Value::null())
    }
}

/// What a bytecode body contains, as the cost model sees it.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct CodeFacts {
    /// Bytecode instructions.
    size: usize,
    /// `OpLoopHint` headers, i.e. loops.
    loops: u32,
    /// Call instructions, including primitive calls.
    calls: u32,
    /// Allocation instructions (closures, environments, pairs).
    allocs: u32,
    /// Vector accesses, whose inlined forms carry class/index/bounds guards.
    vector_accesses: u32,
}

/// Scans `code` once for the facts the inliner scores.
fn code_facts(code: &[u8]) -> CodeFacts {
    let mut facts = CodeFacts::default();
    let mut pc = 0;
    while let Some((ins, _, len)) = Instruction::decode_slice(code, pc) {
        pc += len;
        facts.size += 1;
        match ins {
            Instruction::OpLoopHint(_) => facts.loops += 1,
            Instruction::OpCall(_)
            | Instruction::OpTailCall(_)
            | Instruction::OpCallSelf(_)
            | Instruction::OpTailCallSelf(_)
            | Instruction::OpCallPrimitive0(_)
            | Instruction::OpCallPrimitive1(_)
            | Instruction::OpCallPrimitive2(_)
            | Instruction::OpCallPrimitive3(_)
            | Instruction::OpCallPrimitiveN(_) => facts.calls += 1,
            Instruction::OpClosure(_)
            | Instruction::OpClosureEnv(_)
            | Instruction::OpMakeEnv(_)
            | Instruction::OpCons(_) => facts.allocs += 1,
            Instruction::OpVectorRef(_)
            | Instruction::OpVectorRefMul(_)
            | Instruction::OpVectorSet(_) => facts.vector_accesses += 1,
            _ => {}
        }
    }
    facts
}

/// The hard reasons a callee cannot be spliced at all. `Err` names one.
fn structural_refusal(
    cx: &InlineCx<'_>,
    callee: crate::runtime::objectmodel::Value<'_>,
    facts: &CodeFacts,
    callee_code: &[u8],
    argc: u32,
) -> Result<(), String> {
    if cx.depth >= MAX_DEPTH {
        return Err(format!("inlining depth {} is the limit", cx.depth));
    }
    if facts.size > MAX_CALLEE_INSTRUCTIONS {
        return Err(format!(
            "{} instructions is over {MAX_CALLEE_INSTRUCTIONS}",
            facts.size
        ));
    }
    if facts.size as u32 > cx.remaining.get() {
        return Err(format!(
            "{} instructions is over the {} left of the budget",
            facts.size,
            cx.remaining.get()
        ));
    }
    // Recursive calls stay calls: an inlined body would call itself through
    // the caller's frame, and the direct-call path is the cheaper fix.
    if cx.chain.contains(&callee) {
        return Err("it is already inlined into the caller (recursive)".to_string());
    }
    match Instruction::decode_slice(callee_code, 0) {
        Some((Instruction::OpEnter(op), _, _)) if op.argc.0 == argc => Ok(()),
        // A variadic callee: the extras are known at the site.
        Some((Instruction::OpEnterRest(op), _, _))
            if argc >= op.required.0 && argc - op.required.0 <= MAX_INLINED_EXTRAS =>
        {
            Ok(())
        }
        Some((Instruction::OpEnterRest(op), _, _)) => Err(format!(
            "it takes {} arguments and a rest list and the call passes {argc}",
            op.required.0
        )),
        Some((Instruction::OpEnter(op), _, _)) => Err(format!(
            "it takes {} arguments and the call passes {argc}",
            op.argc.0
        )),
        // A native procedure's code is the shared one-`nop` buffer.
        Some((Instruction::OpNop(_), _, _)) => Err(format!(
            "it is a native procedure ({})",
            crate::runtime::objectmodel::proc::procedure_label(
                callee.downcast::<crate::runtime::objectmodel::CodeBlock<'_>>()
            )
        )),
        other => Err(format!(
            "it does not start with a fixed-arity entry (starts with {})",
            other.map_or("an undecodable instruction".to_string(), |(ins, _, _)| {
                opcode_name(&ins)
            })
        )),
    }
}

/// The profitability score of one target, or the reason it is not profitable.
///
/// `hits` is this target's call count and `min_hits` the bar it must clear
/// (the site's for monomorphic, the target's for polymorphic). A positive
/// score means the call is worth replacing; the score itself is only used to
/// order and explain decisions.
fn inline_score(
    hits: u32,
    min_hits: u32,
    callee: &CodeFacts,
    caller: &CodeFacts,
    depth: u32,
) -> Result<i64, String> {
    if hits < min_hits {
        return Err(format!("the site is cold: {hits} hits is under {min_hits}"));
    }

    if callee.vector_accesses > 0
        && callee.size <= TINY_ACCESSOR_INSTRUCTIONS
        && caller.loops > 0
        && caller.size >= LARGE_CALLER_INSTRUCTIONS
    {
        return Err(format!(
            "a tiny accessor in a {} instruction loop-bearing caller",
            caller.size
        ));
    }
    let hot = hits.min(FULL_HIT_CREDIT) as i64;
    let mut score = BASE_SCORE + MAX_HIT_SCORE * hot / i64::from(FULL_HIT_CREDIT);
    score += LOOP_BONUS * i64::from(callee.loops.min(MAX_SCORED_LOOPS));
    score -= CALL_PENALTY * i64::from(callee.calls.min(MAX_SCORED_CALLS));
    score -= ALLOC_PENALTY * i64::from(callee.allocs.min(MAX_SCORED_ALLOCS));
    score -= callee.size as i64 / SIZE_PENALTY_DIV;
    score -= DEPTH_PENALTY * i64::from(depth);
    score -= (caller.size / CALLER_PENALTY_DIV).min(MAX_CALLER_PENALTY as usize) as i64;
    if score <= 0 {
        return Err(format!(
            "the cost score {score} is not positive (hits {hits}, {} instructions, \
             {} loops, {} calls, {} allocations, caller {} instructions, depth {depth})",
            callee.size, callee.loops, callee.calls, callee.allocs, caller.size
        ));
    }
    Ok(score)
}

impl<'s, 'gc> Builder<'s, 'gc> {
    pub(super) fn code_for_heap(&mut self, value: SchemeValue<'gc>) -> CodeRef {
        let source = CodeSource::Heap(value);
        if let Some((code, _)) = self.func.codes.iter().find(|(_, s)| **s == source) {
            return code;
        }
        self.func.codes.push(source)
    }

    fn refuse(&self, pc: usize, why: &str) {
        super::calls::refused(why);
        log::info!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "  opt: no inlining at pc {pc} of {}: {why}",
            self.label
        );
    }

    fn skip_target(&self, pc: usize, why: &str) {
        super::calls::refused(why);
        super::calls::target_skipped();
        log::info!(
            target: crate::heap::logging::JIT_LOG_TARGET,
            "  opt: no inlining of one target at pc {pc} of {}: {why}",
            self.label
        );
    }

    /// Inlines the call at `pc` according to its profile and the cost model.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_inline(
        &mut self,
        pc: usize,
        next: usize,
        callee_reg: u32,
        first_arg: u32,
        argc: u32,
        call: (ReturnValues, FrameState),
        list: bool,
        tail: bool,
    ) -> Result<Option<Vec<Value>>, String> {
        let expect = Expect::of(call.0, tail, list);
        if !self.inline.enabled {
            return Ok(None);
        }
        if (self.blocked)(pc) {
            self.refuse(pc, "a guard of this site failed too often");
            return Ok(None);
        }
        let (site_hits, targets, poly) = match profile::call_shape(self.buffer, pc) {
            Some(CallShape::Mono(Some(target))) => (target.hits, vec![target], false),
            Some(CallShape::Mono(None)) => {
                self.refuse(pc, "the site never called a procedure");
                return Ok(None);
            }
            Some(CallShape::Poly(targets)) if self.inline.poly => {
                let site_hits = targets
                    .iter()
                    .map(|target| u64::from(target.hits))
                    .sum::<u64>()
                    .min(u64::from(u32::MAX)) as u32;
                (site_hits, targets, true)
            }
            Some(CallShape::Poly(_)) => {
                self.refuse(
                    pc,
                    "the site is polymorphic and polymorphic inlining is off",
                );
                return Ok(None);
            }
            Some(CallShape::Mega) => {
                self.refuse(pc, "the site is megamorphic");
                return Ok(None);
            }
            None => {
                self.refuse(pc, "no call profile");
                return Ok(None);
            }
        };
        if targets.is_empty() {
            self.refuse(pc, "the site never called a procedure");
            return Ok(None);
        }

        // A tail call has no return point to fall back to, so only a
        // monomorphic site inlines in tail position.
        if tail && poly {
            self.refuse(pc, "a polymorphic tail call is not inlined");
            return Ok(None);
        }

        // The site as a whole must be hot, however the targets split.
        if site_hits < MIN_SITE_HITS {
            self.refuse(
                pc,
                &format!("the site is cold: {site_hits} hits is under {MIN_SITE_HITS}"),
            );
            return Ok(None);
        }

        let caller = code_facts(self.code);
        let named = targets.len();
        let mut bodies: Vec<Candidate> = Vec::with_capacity(named.min(MAX_POLY));
        let mut total = 0usize;
        for target in targets {
            if bodies.len() >= MAX_POLY {
                break;
            }
            let callee_value = SchemeValue::from(target.callee);
            let buffer = target.callee.buffer;
            if bodies
                .iter()
                .any(|candidate| candidate.value == callee_value)
            {
                continue;
            }
            let facts = code_facts(buffer.code());
            let min_hits = if poly { MIN_TARGET_HITS } else { MIN_SITE_HITS };
            let decision =
                structural_refusal(&self.inline, callee_value, &facts, buffer.code(), argc)
                    .and_then(|()| {
                        inline_score(target.hits, min_hits, &facts, &caller, self.inline.depth)
                    });
            if let Err(why) = decision {
                if poly {
                    self.skip_target(pc, &why);
                    continue;
                }
                self.refuse(pc, &why);
                return Ok(None);
            }
            if total + facts.size > self.inline.remaining.get() as usize {
                if poly {
                    self.skip_target(pc, "the targets are over the budget");
                    continue;
                }
                self.refuse(pc, "the targets are over the budget");
                return Ok(None);
            }

            // A rejected candidate must not charge the root for the nested
            // inlining its build already did.
            let saved = self.inline.remaining.get();
            let mut cx = self.inline.clone();
            cx.depth += 1;
            cx.chain.push(callee_value);
            let blocked = |at: usize| target.callee.speculation_blocked(at as u32);
            let callee_label = crate::runtime::objectmodel::proc::procedure_label(target.callee);
            let built = Builder::new(
                self.ctx,
                target.callee.frame_size,
                &buffer,
                &blocked,
                self.fold,
                self.check_invalid,
                self.fold_guards,
                // The callee does not carry the caller's dependency list, so
                // a write to a statically known cell must keep its check.
                false,
                cx,
                false,
                &[],
                &callee_label,
            )
            .and_then(Builder::run);
            let body = match built {
                Ok(func) => func,
                Err(why) => {
                    self.inline.remaining.set(saved);
                    let why = format!("the callee does not build: {why}");
                    if poly {
                        self.skip_target(pc, &why);
                        continue;
                    }
                    self.refuse(pc, &why);
                    return Ok(None);
                }
            };
            if let Err(why) = spliceable(&body, expect) {
                self.inline.remaining.set(saved);
                if poly {
                    self.skip_target(pc, &why);
                    continue;
                }
                self.refuse(pc, &why);
                return Ok(None);
            }
            if body.deps.iter().any(|dep| {
                self.func
                    .deps
                    .iter()
                    .any(|mine| mine.variable == dep.variable && mine.value != dep.value)
            }) {
                self.inline.remaining.set(saved);
                let why = "the callee folded a global to another value";
                if poly {
                    self.skip_target(pc, why);
                    continue;
                }
                self.refuse(pc, why);
                return Ok(None);
            }

            // Every entry register is the closure or an argument.
            let mut entry = Vec::with_capacity(body.entry_regs.len());
            let mut unreadable = None;
            let rest = body.rest;
            let extras = rest.map_or(0, |rest| argc.saturating_sub(rest.required));
            for &reg in &body.entry_regs {
                if let Some(rest) = rest.filter(|rest| {
                    reg == rest.cnt || (reg >= rest.reg && reg < rest.reg + rest.width())
                }) {
                    let value = if reg == rest.cnt {
                        self.bits(SchemeValue::from_i32(extras as i32).bits())
                    } else if reg < rest.reg + rest.scalars.unwrap_or(0) {
                        let i = reg - rest.reg;
                        if i < extras {
                            self.reg(first_arg + rest.required + i)?
                        } else {
                            self.undef()
                        }
                    } else {
                        let skip = rest.scalars.unwrap_or(0);
                        if extras <= skip {
                            self.bits(SchemeValue::null().bits())
                        } else {
                            let elements =
                                self.regs(first_arg + rest.required + skip..first_arg + argc)?;
                            self.value_op(Op::AllocVector, &elements, Type::Vector, None)
                        }
                    };
                    entry.push(Some(value));
                } else if reg == 1 {
                    entry.push(None);
                } else if reg >= 2 && reg < 2 + argc.min(rest.map_or(argc, |rest| rest.required)) {
                    entry.push(Some(self.reg(first_arg + reg - 2)?));
                } else {
                    unreadable = Some(format!("the callee reads r{reg} on entry"));
                    break;
                }
            }
            if let Some(why) = unreadable {
                self.inline.remaining.set(saved);
                if poly {
                    self.skip_target(pc, &why);
                    continue;
                }
                self.refuse(pc, &why);
                return Ok(None);
            }
            bodies.push(Candidate {
                value: callee_value,
                buffer,
                body,
                entry,
                size: facts.size,
                hits: target.hits,
                label: callee_label,
            });
            total += facts.size;
        }

        if bodies.is_empty() {
            self.refuse(pc, "no target scored positive");
            return Ok(None);
        }

        if total as u32 > self.inline.remaining.get() {
            self.refuse(pc, "the nested inlining used up the budget");
            return Ok(None);
        }

        self.inline
            .remaining
            .set(self.inline.remaining.get().saturating_sub(total as u32));

        let raw_callee = self.reg(callee_reg)?;
        let fallback_args = if poly {
            Some(self.regs(first_arg..first_arg + argc)?)
        } else {
            None
        };
        let guard_frame = self.frame(pc, pc)?;
        let closure = self.check_class(raw_callee, ClassRef::CLOSURE, guard_frame);

        let caller_code = self.func.frames[guard_frame].code;
        let site = if tail {
            None
        } else {
            Some(self.frame(next, next)?)
        };
        let base = callee_reg - 1;
        let cont = self.func.new_block();
        let results: Vec<Value> = expect
            .param_types()
            .into_iter()
            .map(|ty| self.func.append_param(cont, ty))
            .collect();
        let count = bodies.len();
        let partial = poly && count < named;

        if poly {
            for (index, candidate) in bodies.iter().enumerate() {
                let code = self.code_for_heap(candidate.value);
                let test = self.value_op(Op::TestCode(code), &[closure], Type::Bool, None);
                let body = self.func.new_block();
                let rest = self.func.new_block();
                let then = self.func.edge(body, &[]);
                let otherwise = self.func.edge(rest, &[]);
                self.terminate(Terminator::Branch {
                    cond: test,
                    then,
                    otherwise,
                });
                self.cur = body;
                self.splice_candidate(
                    pc,
                    body,
                    closure,
                    (site, caller_code, tail),
                    base,
                    cont,
                    call,
                    expect,
                    candidate,
                    code,
                )?;
                self.cur = rest;
                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "  opt: inlined {} instructions of {} at pc {pc} of {} ({} hits, depth {}, {} of {count})",
                    candidate.size,
                    candidate.label,
                    self.label,
                    candidate.hits,
                    self.inline.depth + 1,
                    index + 1
                );
            }

            let mut operands = vec![raw_callee];
            operands.extend(fallback_args.unwrap_or_default());
            let frame = call.1;
            let value = self.value_op(Op::CallGeneric(call.0), &operands, Type::Any, Some(frame));
            let args = taken_args(&mut self.func, self.cur, expect, value, frame);
            let edge = self.func.edge(cont, &args);
            self.terminate(Terminator::Jump(edge));
        } else {
            let candidate = &bodies[0];
            let code = self.code_for_heap(candidate.value);
            let checked = self.value_op(
                Op::CheckCode(code),
                &[closure],
                Type::Closure(Some(code)),
                Some(guard_frame),
            );
            self.splice_candidate(
                pc,
                self.cur,
                checked,
                (site, caller_code, tail),
                base,
                cont,
                call,
                expect,
                candidate,
                code,
            )?;
            log::info!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "  opt: inlined {} instructions of {} at pc {pc} of {} ({} hits, depth {}, 1 of 1)",
                candidate.size,
                candidate.label,
                self.label,
                candidate.hits,
                self.inline.depth + 1
            );
        }
        super::calls::inlined(count, site_hits, poly, partial);
        self.cur = cont;
        Ok(Some(results))
    }

    #[allow(clippy::too_many_arguments)]
    fn splice_candidate(
        &mut self,
        pc: usize,
        at: Block,
        closure: Value,
        (site, caller_code, tail): (Option<FrameState>, CodeRef, bool),
        base: u32,
        cont: Block,
        call: (ReturnValues, FrameState),
        expect: Expect,
        candidate: &Candidate<'gc>,
        code: CodeRef,
    ) -> Result<(), String> {
        let entry_values: Vec<Value> = candidate
            .entry
            .iter()
            .map(|value| value.unwrap_or(closure))
            .collect();
        let mut splice = Splice {
            dest: &mut self.func,
            src: &candidate.body,
            src_buffer: &candidate.buffer,
            callee_code: code,
            site,
            caller_code,
            tail,
            base,
            cont,
            call,
            expect,
            call_pc: pc as u32,
            blocks: SecondaryMap::new(),
            values: vec![None; candidate.body.values.len()],
            frames: vec![None; candidate.body.frames.len()],
        };
        splice.run(at, &entry_values)?;
        for dep in &candidate.body.deps {
            if !self
                .func
                .deps
                .iter()
                .any(|mine| mine.variable == dep.variable)
            {
                self.func.deps.push(*dep);
            }
        }
        // The callee's inline instance allocations assumed the class
        // descriptors. The installer registers the caller's unit with them.
        for dep in &candidate.body.class_deps {
            if !self
                .func
                .class_deps
                .iter()
                .any(|mine| mine.class == dep.class)
            {
                self.func.class_deps.push(*dep);
            }
        }
        Ok(())
    }
}

/// A callee whose body was built and can be spliced.
struct Candidate<'gc> {
    value: SchemeValue<'gc>,
    buffer: Gc<'gc, CodeBuffer<'gc>>,
    body: Func<'gc>,
    /// The value for each entry register: `None` for the closure.
    entry: Vec<Option<Value>>,
    size: usize,
    /// Calls the site made to this callee, for the diagnostic log.
    hits: u32,
    /// The callee's procedure label, for the diagnostic log.
    label: String,
}

/// Whether `body` can be spliced: it has no OSR entry and does not loop back
/// to its entry.
fn spliceable(body: &Func<'_>, expect: Expect) -> Result<(), String> {
    if !body.osr.is_empty() || body.entry_selector.is_some() {
        return Err("the callee has OSR entries".to_string());
    }
    // A self tail call of the callee jumps to its entry block, which the
    // splice maps to the call site's block. The entry's parameters are not
    // block parameters there, so such a jump cannot be spliced.
    if body
        .blocks
        .keys()
        .any(|block| body.successors(block).contains(&body.entry))
    {
        return Err("the callee loops back to its entry".to_string());
    }
    // The call site's return point takes a fixed number of values, so a
    // body that returns another number is a call, not a splice.
    for data in body.blocks.values() {
        let count = match &data.term {
            Some(Terminator::Return(_)) => 1,
            Some(Terminator::ReturnMany { values, .. }) => values.len(),
            _ => continue,
        };
        if !expect.accepts(count) {
            return Err(format!(
                "the callee returns {count} value{} to a return point that takes {}",
                if count == 1 { "" } else { "s" },
                expect.describe()
            ));
        }
    }
    Ok(())
}

/// What the return point of an inlined call takes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Expect {
    /// A tail call: whatever the body returns, the caller returns.
    Tail,
    /// Exactly one value.
    One,
    /// Exactly this many values (never one).
    Fixed(u32),
    /// At least `fixed` values, and the extras as a list when `list`.
    Rest { fixed: u32, list: bool },
}

impl Expect {
    fn of(values: ReturnValues, tail: bool, list: bool) -> Expect {
        match values {
            _ if tail => Expect::Tail,
            ReturnValues::Take(count) => Expect::Fixed(count),
            ReturnValues::TakeRest(fixed) => Expect::Rest { fixed, list },
            _ => Expect::One,
        }
    }

    fn accepts(self, count: usize) -> bool {
        match self {
            Expect::Tail => true,
            Expect::One => count == 1,
            Expect::Fixed(fixed) => count == fixed as usize,
            Expect::Rest { fixed, .. } => count >= fixed as usize,
        }
    }

    fn describe(self) -> String {
        match self {
            Expect::Tail => "any number".to_string(),
            Expect::One => "one".to_string(),
            Expect::Fixed(fixed) => fixed.to_string(),
            Expect::Rest { fixed, .. } => format!("{fixed} or more"),
        }
    }

    /// The parameters of the block the body returns to: the values, and for
    /// a rest entry the extra count and the list of the extras.
    fn param_types(self) -> Vec<Type> {
        match self {
            Expect::Tail | Expect::One => vec![Type::Any],
            Expect::Fixed(fixed) => vec![Type::Any; fixed as usize],
            Expect::Rest { fixed, .. } => {
                let mut types = vec![Type::Any; fixed as usize];
                types.push(Type::Int32);
                types.push(Type::Any);
                types
            }
        }
    }
}

/// The arguments of the edge into the returned-to block for the values the
/// call `result` left in the callee frame, read in `block` right after it.
fn taken_args<'gc>(
    func: &mut Func<'gc>,
    block: Block,
    expect: Expect,
    result: Value,
    frame: FrameState,
) -> Vec<Value> {
    let mut push = |func: &mut Func<'gc>, op, args: &[Value], ty, frame| {
        func.push_inst(block, op, args, Some(ty), frame)
            .expect("an operation with a result returns one")
    };
    match expect {
        Expect::Tail | Expect::One => vec![result],
        Expect::Fixed(fixed) => (0..fixed)
            .map(|i| push(func, Op::ReturnedValue(i), &[result], Type::Any, None))
            .collect(),
        Expect::Rest { fixed, list } => {
            let mut args: Vec<Value> = (0..fixed)
                .map(|i| push(func, Op::ReturnedValue(i), &[result], Type::Any, None))
                .collect();
            let count = push(func, Op::ReturnedCount(fixed), &[result], Type::Int32, None);
            let extras = if list {
                let from = push(func, Op::ReturnedAddr(fixed), &[result], Type::Int64, None);
                push(
                    func,
                    Op::CallOp(Helper::ValuesRest.helper_ref()),
                    &[from, count],
                    Type::Any,
                    Some(frame),
                )
            } else {
                push(
                    func,
                    Op::Const(Constant::Bits(SchemeValue::null().bits())),
                    &[],
                    Type::Any,
                    None,
                )
            };
            args.push(count);
            args.push(extras);
            args
        }
    }
}

/// One splice of a callee function into the caller's.
struct Splice<'a, 'gc> {
    dest: &'a mut Func<'gc>,
    src: &'a Func<'gc>,
    src_buffer: &'a CodeBuffer<'gc>,
    /// The caller's name for the callee's root code block.
    callee_code: CodeRef,
    /// The caller's frame at the call, the parent of the callee's outermost
    /// frames. `None` for a tail call, whose callee frames replace the
    /// caller's.
    site: Option<FrameState>,
    /// The caller's code block, for the callee frames' call-site metadata.
    caller_code: CodeRef,
    /// Whether the call is in tail position: the callee's own tail calls stay
    /// tail calls instead of becoming calls whose result returns here.
    tail: bool,
    /// The call's base register.
    base: u32,
    /// Where a `Return` goes.
    cont: Block,
    /// How a tail call of the callee becomes a call of the caller: what its
    /// return point does with other than one value, and its frame.
    call: (ReturnValues, FrameState),
    /// What the call site's return point takes from the body.
    expect: Expect,
    /// The caller's bytecode pc of the call, for deopt diagnostics.
    call_pc: u32,
    blocks: SecondaryMap<Block, Option<Block>>,
    values: Vec<Option<Value>>,
    frames: Vec<Option<FrameState>>,
}

impl<'a, 'gc> Splice<'a, 'gc> {
    fn run(&mut self, at: Block, entry_values: &[Value]) -> Result<(), String> {
        let src = self.src;
        let entry_params = src.params(src.entry).to_vec();
        if entry_params.len() != entry_values.len() {
            return Err(
                "the callee's entry parameters do not match its entry registers".to_string(),
            );
        }
        for (&param, &value) in entry_params.iter().zip(entry_values) {
            self.values[param.index()] = Some(value);
        }
        // Blocks and their parameters. The entry's code goes into `at`.
        self.blocks[src.entry] = Some(at);
        for (block, data) in src.blocks.iter() {
            if block == src.entry {
                continue;
            }
            let new = self.dest.new_block();
            self.blocks[block] = Some(new);
            for &param in data.params.as_slice(&src.pool) {
                let ty = self.ty(src.value_type(param))?;
                let new_param = self.dest.append_param(new, ty);
                self.values[param.index()] = Some(new_param);
            }
        }
        // Instructions, with their results, before any operand is mapped.
        let mut insts = Vec::new();
        for (block, data) in src.blocks.iter() {
            let target = self.block(block)?;
            for &inst in &data.insts {
                let old = &src.insts[inst];
                let op = self.op(old.op)?;
                let ty = match old.result {
                    Some(result) => Some(self.ty(src.value_type(result))?),
                    None => None,
                };
                self.dest.push_inst(target, op, &[], ty, None);
                let new = *self.dest.blocks[target].insts.last().expect("just pushed");
                if let (Some(old_result), Some(new_result)) =
                    (old.result, self.dest.insts[new].result)
                {
                    self.values[old_result.index()] = Some(new_result);
                }
                insts.push((inst, new));
            }
        }
        // Operands and frame states.
        for (old, new) in insts {
            let args: Vec<Value> = src
                .args(old)
                .iter()
                .map(|&v| self.value(v))
                .collect::<Result<_, _>>()?;
            let frame = match src.insts[old].frame {
                Some(frame) => Some(self.frame(frame)?),
                None => None,
            };
            let list = EntityList::from_slice(&args, &mut self.dest.pool);
            let data: &mut InstData = &mut self.dest.insts[new];
            data.args = list;
            data.frame = frame;
        }
        for (block, data) in src.blocks.iter() {
            let target = self.block(block)?;
            let term = data
                .term
                .as_ref()
                .ok_or_else(|| "a callee block has no terminator".to_string())?;
            if let (
                true,
                Terminator::TailCall {
                    callee,
                    args,
                    frame,
                    check,
                },
            ) = (self.tail, term)
            {
                // In tail position the callee's tail call stays one.
                let callee = self.value(*callee)?;
                let mut mapped = Vec::new();
                for &arg in args.as_slice(&src.pool) {
                    mapped.push(self.value(arg)?);
                }
                let frame = self.frame(*frame)?;
                let check = match check {
                    Some(code) => Some(self.code(*code)?),
                    None => None,
                };
                let args = EntityList::from_slice(&mapped, &mut self.dest.pool);
                self.dest.set_terminator(
                    target,
                    Terminator::TailCall {
                        callee,
                        args,
                        frame,
                        check,
                    },
                );
                continue;
            }
            if let Terminator::TailCall { callee, args, .. } = term {
                // The callee's tail call is a call whose result the call
                // site gets, since the callee returns whatever it returns.
                let mut operands = vec![self.value(*callee)?];
                for &arg in args.as_slice(&src.pool) {
                    operands.push(self.value(arg)?);
                }
                let (values, frame) = self.call;
                let result = self
                    .dest
                    .push_inst(
                        target,
                        Op::CallGeneric(values),
                        &operands,
                        Some(Type::Any),
                        Some(frame),
                    )
                    .expect("a call has a result");
                let args = taken_args(self.dest, target, self.expect, result, frame);
                let cont = self.cont;
                let edge = self.dest.edge(cont, &args);
                self.dest.set_terminator(target, Terminator::Jump(edge));
                continue;
            }
            if let Terminator::Return(value) = term {
                let value = self.value(*value)?;
                self.return_to(target, &[value])?;
                continue;
            }
            if let Terminator::ReturnMany { values } = term {
                let mut mapped = Vec::with_capacity(values.len());
                for &value in values {
                    mapped.push(self.value(value)?);
                }
                if self.expect == Expect::Tail {
                    // The body returns to this function's caller.
                    self.dest
                        .set_terminator(target, Terminator::ReturnMany { values: mapped });
                } else {
                    self.return_to(target, &mapped)?;
                }
                continue;
            }
            let term = self.terminator(term)?;
            self.dest.set_terminator(target, term);
        }
        // The callee's heap constants and globals are the caller's now.
        Ok(())
    }

    /// Ends `block`, where the body returned `values`, with a jump to the
    /// block the call site continues in.
    fn return_to(&mut self, block: Block, values: &[Value]) -> Result<(), String> {
        let mut args = Vec::new();
        match self.expect {
            Expect::Tail | Expect::One | Expect::Fixed(_) => args.extend_from_slice(values),
            Expect::Rest { fixed, list } => {
                let (fixed, extras) = values.split_at((fixed as usize).min(values.len()));
                args.extend_from_slice(fixed);
                let count = self
                    .dest
                    .push_inst(
                        block,
                        Op::Const(Constant::Int32(extras.len() as i32)),
                        &[],
                        Some(Type::Int32),
                        None,
                    )
                    .expect("a constant has a result");
                let null = SchemeValue::null().bits();
                let mut chain = self
                    .dest
                    .push_inst(
                        block,
                        Op::Const(Constant::Bits(null)),
                        &[],
                        Some(Type::Any),
                        None,
                    )
                    .expect("a constant has a result");
                if list {
                    for &extra in extras.iter().rev() {
                        chain = self
                            .dest
                            .push_inst(block, Op::AllocPair, &[extra, chain], Some(Type::Pair), None)
                            .expect("an allocation has a result");
                    }
                }
                args.push(count);
                args.push(chain);
            }
        }
        let cont = self.cont;
        let edge = self.dest.edge(cont, &args);
        self.dest.set_terminator(block, Terminator::Jump(edge));
        Ok(())
    }

    fn block(&self, block: Block) -> Result<Block, String> {
        self.blocks[block].ok_or_else(|| "a callee block was not copied".to_string())
    }

    fn value(&self, value: Value) -> Result<Value, String> {
        self.values[value.index()].ok_or_else(|| "a callee value was not copied".to_string())
    }

    fn code(&mut self, code: CodeRef) -> Result<CodeRef, String> {
        match *self
            .src
            .codes
            .get(code)
            .ok_or_else(|| "a callee names an unknown code block".to_string())?
        {
            CodeSource::Root => Ok(self.callee_code),
            CodeSource::Heap(value) => Ok(self.heap_code(value)),
            CodeSource::Pool(index) => {
                let value = self.src_buffer.constant(index as usize);
                Ok(self.heap_code(value))
            }
        }
    }

    fn heap_code(&mut self, value: SchemeValue<'gc>) -> CodeRef {
        let source = CodeSource::Heap(value);
        if let Some((code, _)) = self.dest.codes.iter().find(|(_, s)| **s == source) {
            return code;
        }
        self.dest.codes.push(source)
    }

    fn ty(&mut self, ty: Type) -> Result<Type, String> {
        Ok(match ty {
            Type::Closure(Some(code)) => Type::Closure(Some(self.code(code)?)),
            other => other,
        })
    }

    fn constant(&mut self, constant: Constant) -> Result<Constant, String> {
        Ok(match constant {
            Constant::Table(index) => {
                let value = *self
                    .src
                    .heap_constants
                    .get(index as usize - 1)
                    .ok_or_else(|| "a callee names a missing table constant".to_string())?;
                Constant::Table(self.dest.heap_constant(value))
            }
            Constant::Pool { code, index } => {
                if !matches!(self.src.codes[code], CodeSource::Root) {
                    return Err("a callee constant belongs to a nested code block".to_string());
                }
                let value = self.src_buffer.constant(index as usize);
                if value.is_cell() {
                    Constant::Table(self.dest.heap_constant(value))
                } else {
                    Constant::Bits(value.bits())
                }
            }
            other => other,
        })
    }

    fn op(&mut self, op: Op) -> Result<Op, String> {
        Ok(match op {
            Op::Const(constant) => Op::Const(self.constant(constant)?),
            Op::CheckCode(code) => Op::CheckCode(self.code(code)?),
            // A polymorphic guard in a nested body names its code ref in the
            // callee's table; unmapped it would test another code block.
            Op::TestCode(code) => Op::TestCode(self.code(code)?),
            Op::AllocClosure(code) => Op::AllocClosure(self.code(code)?),
            // A direct call in the callee stays direct: its code ref is
            // remapped like every other code ref (a self-call maps to the
            // callee's own code block, which the caller now names).
            Op::CallKnown(code, values) => Op::CallKnown(self.code(code)?, values),
            other => other,
        })
    }

    fn frame(&mut self, frame: FrameState) -> Result<FrameState, String> {
        if let Some(done) = self.frames[frame.index()] {
            return Ok(done);
        }
        let data = self.src.frames[frame].clone();
        let (parent, call_base) = match data.parent {
            Some(parent) => (Some(self.frame(parent)?), data.call_base),
            None => match self.site {
                Some(site) => (Some(site), Some(self.base)),
                None => (None, None),
            },
        };
        // The frames of the spliced body are inside an inlined block now. A
        // nested body already names its own call site; remap its caller code
        // block into this function. The caller of a top-level spliced frame
        // is the frame that holds the call.
        let inline_site = match data.inline_site {
            Some(site) => Some(CallSite {
                call_pc: site.call_pc,
                caller: self.code(site.caller)?,
            }),
            None => Some(CallSite {
                call_pc: self.call_pc,
                caller: self.caller_code,
            }),
        };
        let mut registers = Vec::with_capacity(data.registers.len());
        for &(reg, value) in &data.registers {
            registers.push((reg, self.value(value)?));
        }
        let new = FrameStateData {
            code: self.code(data.code)?,
            pc: data.pc,
            closure: self.value(data.closure)?,
            registers,
            parent,
            call_base,
            returned: data.returned,
            rest: data.rest,
            inline_site,
        };
        let new = self.dest.push_frame(new);
        self.frames[frame.index()] = Some(new);
        Ok(new)
    }

    fn edge(&mut self, edge: &Edge) -> Result<Edge, String> {
        let args: Vec<Value> = self
            .src
            .edge_args(edge)
            .iter()
            .map(|&v| self.value(v))
            .collect::<Result<_, _>>()?;
        let block = self.block(edge.block)?;
        Ok(self.dest.edge(block, &args))
    }

    fn terminator(&mut self, term: &Terminator) -> Result<Terminator, String> {
        Ok(match term {
            Terminator::Jump(edge) => Terminator::Jump(self.edge(edge)?),
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => Terminator::Branch {
                cond: self.value(*cond)?,
                then: self.edge(then)?,
                otherwise: self.edge(otherwise)?,
            },
            Terminator::Switch {
                value,
                cases,
                default,
                density,
            } => {
                let mut mapped = Vec::with_capacity(cases.len());
                for (key, edge) in cases {
                    mapped.push((*key, self.edge(edge)?));
                }
                Terminator::Switch {
                    value: self.value(*value)?,
                    cases: mapped,
                    default: self.edge(default)?,
                    density: *density,
                }
            }
            Terminator::Return(_) | Terminator::ReturnMany { .. } => {
                return Err("a return reached the generic terminator copy".to_string());
            }
            Terminator::TailCall { .. } => return Err("a callee tail calls".to_string()),
            Terminator::Deopt { frame } => Terminator::Deopt {
                frame: self.frame(*frame)?,
            },
            Terminator::Unreachable => Terminator::Unreachable,
        })
    }
}

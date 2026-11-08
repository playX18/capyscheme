//! Loop recognition: turn a function's self-calls into a real loop.
//!
//! A fixed-arity function whose self-calls are tail calls gets a local continuation
//! holding the loop body.
//!
//! The function prologue initializes the loop and every self tail call becomes
//! a `Continue` back to it.
//!
//! # Accumulator loops
//!
//! A non-tail self call still reaches the loop through a continuation, so it
//! cannot become a plain `Continue` (Letk continuations are CFG blocks and are
//! never closures). When every non-tail self call has the shape
//!
//! ```text
//! (let ((r (f head (loop args)))) r)   ; f = cons, or an exact associative op
//! ```
//!
//! the pass instead adds an accumulator parameter to the loop:
//!
//! - `cons`: `acc := (cons head acc)` and the exit returns `(reverse acc)`
//!   (requires every exit to return the literal `'()`);
//! - exact associative/commutative primitives (`logand`, `logior`, `fxand`,
//!   `fxior`, `fxxor`, `fxlogand`, `fxlogior`, `fxlogxor`, `fxmin`, `fxmax`):
//!   `acc := (f acc head)`, initialized to the literal every exit returns.
//!
//! Generic `+`/`*` are excluded as they also operate on flonums,
//! where reassociation changes rounding. `append` is excluded because a left fold would copy the
//! accumulator at every step (O(n^2)).
//!
//! All non-tail self calls must match and all exits must agree, otherwise the
//! function falls back to tail-only loopification. Mixing is unsound: a
//! leftover call would return to its continuation and drop the accumulator.

use std::collections::HashMap;

use cranelift_entity::EntitySet;

use crate::expander::core::fresh_lvar_derived;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::{Symbol, Value};
use crate::utils::flags;

use super::graph::{
    BoundVar, ExprKind, FreeVar, Function, FunctionId, Graph, Parent, Subterm, TermKind,
};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct LoopStats {
    pub loops: usize,
    pub self_calls: usize,
    pub loops_accumulated: usize,
    pub accumulated_self_calls: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AccMode {
    /// `f = cons`: accumulate in reverse, then `reverse` at the exit.
    ConsReverse,
    /// Exact associative/commutative `f`: accumulate directly.
    LeftFold,
}

#[derive(Clone, Copy, Debug)]
struct AccPlan<'gc> {
    mode: AccMode,
    prim: Value<'gc>,
    /// Literal value every exit returns; also the accumulator's initial value.
    base: Value<'gc>,
}

#[derive(Clone, Copy, Debug)]
struct SelfCall {
    link: Subterm,
    /// The continuation the call returns to. Equal to the function's return
    /// continuation for a tail self call.
    cont: BoundVar,
    tail: bool,
}

struct Candidate<'gc> {
    calls: Vec<SelfCall>,
    exits: Vec<Subterm>,
    /// `Some` when the accumulator transform applies to every non-tail call.
    plan: Option<AccPlan<'gc>>,
}

/// Recognize loops in every reachable function of `graph`.
pub fn introduce_loops<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    entry: FunctionId,
) -> LoopStats {
    let mut functions = Vec::new();
    let mut seen_functions = EntitySet::new();
    collect_functions(
        graph,
        graph[entry].body,
        &mut functions,
        &mut seen_functions,
    );

    let mut functions_by_binder = HashMap::new();
    for function in functions.iter().copied() {
        functions_by_binder.insert(graph[function].var, function);
    }

    let mut stats = LoopStats::default();
    for function in functions {
        if function == entry {
            continue;
        }
        let Some(candidate) = self_calls_of(graph, function, &functions_by_binder) else {
            continue;
        };
        if candidate.calls.is_empty() {
            continue;
        }

        // Without the accumulator transform only tail calls can become jumps;
        // a function with no tail self call would get a useless loop block.
        if candidate.plan.is_none() && !candidate.calls.iter().any(|call| call.tail) {
            continue;
        }
        let accumulated = candidate.plan.is_some();
        if introduce_loop(ctx, graph, function, &candidate, &functions_by_binder) {
            stats.loops += 1;
            stats.self_calls += candidate.calls.len();
            if accumulated {
                stats.loops_accumulated += 1;
                stats.accumulated_self_calls +=
                    candidate.calls.iter().filter(|call| !call.tail).count();
            }
        }
    }

    stats
}

/// Enumerate every function reachable from `link`, including nested groups.
fn collect_functions<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    out: &mut Vec<FunctionId>,
    seen: &mut EntitySet<FunctionId>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    match graph[term].kind {
        TermKind::LetVal(_, body) => collect_functions(graph, body, out, seen),
        TermKind::Letk(functions, body) | TermKind::Fix(functions, body) => {
            for function in graph.live_functions(&functions) {
                if seen.insert(function) {
                    out.push(function);
                    collect_functions(graph, graph[function].body, out, seen);
                }
            }
            collect_functions(graph, body, out, seen);
        }
        TermKind::If(_, consequent, alternative, _) => {
            collect_functions(graph, consequent, out, seen);
            collect_functions(graph, alternative, out, seen);
        }
        TermKind::App(..)
        | TermKind::Continue(..)
        | TermKind::BranchPrim(..)
        | TermKind::Raise(..) => {}
    }
}

/// Collect literal bindings in a procedure body. Binders are unique, so a
/// single map suffices for the exit checks.
fn collect_literals<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    out: &mut HashMap<BoundVar, Value<'gc>>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    match graph[term].kind {
        TermKind::LetVal((binding, expr), body) => {
            if let Some(expr) = graph.read_expr_link(expr) {
                if let ExprKind::Literal(value) = graph[expr].kind {
                    out.insert(binding, value);
                }
            }
            collect_literals(graph, body, out);
        }
        TermKind::Letk(functions, body) | TermKind::Fix(functions, body) => {
            for function in graph.live_functions(&functions) {
                collect_literals(graph, graph[function].body, out);
            }
            collect_literals(graph, body, out);
        }
        TermKind::If(_, consequent, alternative, _) => {
            collect_literals(graph, consequent, out);
            collect_literals(graph, alternative, out);
        }
        TermKind::App(..)
        | TermKind::Continue(..)
        | TermKind::BranchPrim(..)
        | TermKind::Raise(..) => {}
    }
}

/// Collect the self calls of `function`, or `None` when the function is not a
/// loop candidate.
fn self_calls_of<'gc>(
    graph: &Graph<'gc>,
    function: FunctionId,
    functions_by_binder: &HashMap<BoundVar, FunctionId>,
) -> Option<Candidate<'gc>> {
    let data = graph[function];
    if data.cont.is_none() || data.case_group.is_some() {
        return None;
    }
    if data.is_variadic || data.variadic.is_some() {
        return None;
    }

    let self_binder = graph.function_binder_rooted(data.var);
    let self_cont = graph.function_binder_rooted(data.cont.expect("lambda has a return cont"));
    let arity = graph.bound_vars_slice(&data.vars).len();

    let mut literals = HashMap::new();
    collect_literals(graph, data.body, &mut literals);

    let mut calls = Vec::new();
    let mut exits = Vec::new();
    let mut accumulate_ok = true;
    if !collect_self_calls(
        graph,
        data.body,
        self_binder,
        self_cont,
        arity,
        &mut calls,
        &mut exits,
        &mut accumulate_ok,
    ) {
        return None;
    }

    let mut plan = None;
    if accumulate_ok
        && flags::gcps_accumulate()
        && calls.iter().any(|call| !call.tail)
        && !exits.is_empty()
    {
        if let Some((acc_plan, remaining_exits)) = accumulate_plan(
            graph,
            self_cont,
            &calls,
            &exits,
            &literals,
            functions_by_binder,
        ) {
            plan = Some(acc_plan);
            exits = remaining_exits;
        }
    }

    Some(Candidate { calls, exits, plan })
}

/// Walk a procedure body looking for self calls and exits.
///
///
/// Returns `false` when the function must not be loopified at all (a wrong
/// arity self call). `accumulate_ok` is cleared when a construct would lose an
/// accumulator (a call to another function whose continuation is the loop's
/// return continuation, or a branch directly to it).
fn collect_self_calls<'gc>(
    graph: &Graph<'gc>,
    link: Subterm,
    self_binder: BoundVar,
    self_cont: BoundVar,
    arity: usize,
    out_calls: &mut Vec<SelfCall>,
    out_exits: &mut Vec<Subterm>,
    accumulate_ok: &mut bool,
) -> bool {
    let Some(term) = graph.read_term_link(link) else {
        return true;
    };
    match graph[term].kind {
        TermKind::LetVal(_, body) => collect_self_calls(
            graph,
            body,
            self_binder,
            self_cont,
            arity,
            out_calls,
            out_exits,
            accumulate_ok,
        ),
        TermKind::Letk(functions, body) => {
            for function in graph.live_functions(&functions) {
                if !collect_self_calls(
                    graph,
                    graph[function].body,
                    self_binder,
                    self_cont,
                    arity,
                    out_calls,
                    out_exits,
                    accumulate_ok,
                ) {
                    return false;
                }
            }
            collect_self_calls(
                graph,
                body,
                self_binder,
                self_cont,
                arity,
                out_calls,
                out_exits,
                accumulate_ok,
            )
        }
        TermKind::Fix(_, body) => collect_self_calls(
            graph,
            body,
            self_binder,
            self_cont,
            arity,
            out_calls,
            out_exits,
            accumulate_ok,
        ),
        TermKind::If(_, consequent, alternative, _) => {
            collect_self_calls(
                graph,
                consequent,
                self_binder,
                self_cont,
                arity,
                out_calls,
                out_exits,
                accumulate_ok,
            ) && collect_self_calls(
                graph,
                alternative,
                self_binder,
                self_cont,
                arity,
                out_calls,
                out_exits,
                accumulate_ok,
            )
        }
        TermKind::App(callee, args, cont) => {
            let cont_binder = graph.free_binder_rooted(cont);
            if graph.free_binder_rooted(callee) == self_binder {
                // One wrong-arity self call bails the whole function: some
                // calls would jump and others would stay calls, changing which
                // arity error `enter` reports.
                if graph.free_vars_slice(&args).len() != arity {
                    return false;
                }
                out_calls.push(SelfCall {
                    link,
                    cont: cont_binder,
                    tail: cont_binder == self_cont,
                });
            } else if cont_binder == self_cont {
                // Returning another call's result through the loop's return
                // continuation bypasses the loop's accumulator.
                *accumulate_ok = false;
            }
            true
        }
        TermKind::Continue(cont, _) => {
            if graph.free_binder_rooted(cont) == self_cont {
                out_exits.push(link);
            }
            true
        }
        TermKind::BranchPrim(_, consequent, alternative, _) => {
            if graph.free_binder_rooted(consequent) == self_cont
                || graph.free_binder_rooted(alternative) == self_cont
            {
                *accumulate_ok = false;
            }
            true
        }
        TermKind::Raise(..) => true,
    }
}

/// Build the accumulator plan, or `None` when some non-tail call or exit does
/// not fit.
fn accumulate_plan<'gc>(
    graph: &Graph<'gc>,
    self_cont: BoundVar,
    calls: &[SelfCall],
    exits: &[Subterm],
    literals: &HashMap<BoundVar, Value<'gc>>,
    functions_by_binder: &HashMap<BoundVar, FunctionId>,
) -> Option<(AccPlan<'gc>, Vec<Subterm>)> {
    // The terminal `continue self_cont` inside an accumulate continuation is
    // replaced by the accumulation, not treated as a loop exit.
    let mut consumed = EntitySet::new();
    let mut plan: Option<(AccMode, Value<'gc>)> = None;
    for call in calls.iter().filter(|call| !call.tail) {
        let (_, prim, mode, exit) =
            match_accumulate_cont(graph, call.cont, self_cont, functions_by_binder)?;
        consumed.insert(exit);
        match plan {
            None => plan = Some((mode, prim)),
            Some((planned_mode, planned_prim)) if planned_mode == mode && planned_prim == prim => {}
            _ => return None,
        }
    }

    let remaining: Vec<Subterm> = exits
        .iter()
        .copied()
        .filter(|exit| !consumed.contains(*exit))
        .collect();
    if remaining.is_empty() {
        return None;
    }
    let base = exit_base(graph, self_cont, &remaining, literals)?;
    let (mode, prim) = plan?;
    if mode == AccMode::ConsReverse && !base.is_null() {
        return None;
    }
    Some((AccPlan { mode, prim, base }, remaining))
}

/// The literal returned by every exit, which becomes the accumulator's initial
/// value. All exits must agree.
fn exit_base<'gc>(
    graph: &Graph<'gc>,
    self_cont: BoundVar,
    exits: &[Subterm],
    literals: &HashMap<BoundVar, Value<'gc>>,
) -> Option<Value<'gc>> {
    let mut base = None;
    for exit in exits.iter().copied() {
        let Some(term) = graph.read_term_link(exit) else {
            return None;
        };
        let TermKind::Continue(cont, args) = graph[term].kind else {
            return None;
        };
        if graph.free_binder_rooted(cont) != self_cont {
            return None;
        }
        let args = graph.free_vars_slice(&args);
        if args.len() != 1 {
            return None;
        }
        let value = literals.get(&graph.free_binder_rooted(args[0])).copied()?;
        match base {
            None => base = Some(value),
            Some(previous) if previous == value => {}
            Some(_) => return None,
        }
    }
    base
}

/// Recognize `(cons head v)` / `(f head v)` continuations and return the
/// accumulator update.
fn match_accumulate_cont<'gc>(
    graph: &Graph<'gc>,
    cont: BoundVar,
    self_cont: BoundVar,
    functions_by_binder: &HashMap<BoundVar, FunctionId>,
) -> Option<(BoundVar, Value<'gc>, AccMode, Subterm)> {
    let function = *functions_by_binder.get(&cont)?;
    let data = graph[function];
    // Only second-class continuations (Letk members) can be bypassed.
    if data.cont.is_some() {
        return None;
    }

    // The continuation must be used only by the self call being rewritten.
    // Another caller returning through it would bypass the accumulator.
    if !graph.binder_is_singleton_or_dead(cont) {
        return None;
    }

    let vars = graph.bound_vars_slice(&data.vars);
    if vars.len() != 1 {
        return None;
    }
    let value_arg = vars[0];
    if let Some(rest) = data.variadic {
        if !graph.collect_occurrences(rest).is_empty() {
            return None;
        }
    }

    let term = graph.read_term_link(data.body)?;
    let TermKind::LetVal((result, expr), body) = graph[term].kind else {
        return None;
    };
    let expr = graph.read_expr_link(expr)?;
    let ExprKind::PrimCall(prim, args) = graph[expr].kind else {
        return None;
    };
    let mode = primitive_mode(prim)?;
    let args = graph.free_vars_slice(&args);
    if args.len() != 2 {
        return None;
    }
    let first = graph.free_binder_rooted(args[0]);
    let second = graph.free_binder_rooted(args[1]);
    let head = match mode {
        // cons is not associative: only `(cons head value)` fits.
        AccMode::ConsReverse => {
            if second != value_arg {
                return None;
            }
            first
        }
        // Exact associative primitives are commutative in the supported set.
        AccMode::LeftFold => {
            if first == value_arg {
                second
            } else if second == value_arg {
                first
            } else {
                return None;
            }
        }
    };
    if head == value_arg || head == result {
        return None;
    }

    let body_link = body;
    let Some(body) = graph.read_term_link(body_link) else {
        return None;
    };
    let TermKind::Continue(cont, args) = graph[body].kind else {
        return None;
    };
    if graph.free_binder_rooted(cont) != self_cont {
        return None;
    }
    let args = graph.free_vars_slice(&args);
    if args.len() != 1 || graph.free_binder_rooted(args[0]) != result {
        return None;
    }

    Some((head, prim, mode, body_link))
}

fn primitive_mode<'gc>(prim: Value<'gc>) -> Option<AccMode> {
    let name = prim.downcast::<Symbol>().to_string();
    match name.as_str() {
        "cons" => Some(AccMode::ConsReverse),
        "logand" | "logior" | "fxand" | "fxior" | "fxxor" | "fxlogand" | "fxlogior"
        | "fxlogxor" | "fxmin" | "fxmax" => Some(AccMode::LeftFold),
        _ => None,
    }
}

fn introduce_loop<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    function: FunctionId,
    candidate: &Candidate<'gc>,
    functions_by_binder: &HashMap<BoundVar, FunctionId>,
) -> bool {
    let data = graph[function];
    let old_params = data.vars;
    let old_body = data.body;
    let self_cont = graph.function_binder_rooted(data.cont.expect("lambda has a return cont"));
    let source = data.source;

    let old_param_lvars: Vec<_> = graph
        .bound_vars_slice(&old_params)
        .iter()
        .map(|param| graph[*param].var)
        .collect();
    let new_params: Vec<BoundVar> = old_param_lvars
        .into_iter()
        .map(|lvar| {
            let lvar = fresh_lvar_derived(ctx, lvar, "loop");
            graph.new_bound_var(lvar)
        })
        .collect();
    let new_args = graph.new_bound_vars(new_params.iter().copied());

    let accumulator = candidate.plan.map(|_| {
        let lvar = fresh_lvar_derived(ctx, graph[data.var].var, "loop-acc");
        graph.new_bound_var(lvar)
    });

    let loop_lvar = fresh_lvar_derived(ctx, graph[data.var].var, "loop-body");
    let loop_binder = graph.new_bound_var(loop_lvar);
    let mut loop_formals = graph.bound_vars_slice(&old_params).to_vec();
    if let Some(accumulator) = accumulator {
        loop_formals.push(accumulator);
    }
    let loop_formals = graph.new_bound_vars(loop_formals);
    let loop_function = graph.new_function(Function {
        name: data.name,
        source,
        var: loop_binder,
        vars: loop_formals,
        variadic: None,
        cont: None,
        is_variadic: false,
        body: old_body,
        is_rec: true,
        is_cold: false,
        is_noinline: false,
        is_reified: false,
        meta: data.meta,
        case_group: None,
    });
    graph.backpatch_function(loop_function);

    // The function's new body is
    //   letk loop(params [acc]) = <old body> in continue loop(new params [base])
    let body_link = graph.new_term_link(None);
    let letk_link = graph.new_function_link(None);
    graph.set_function_link(letk_link, loop_function);
    let letk_functions = graph.new_function_links([letk_link]);
    let continue_link = graph.new_term_link(None);
    let letk_body_link = if candidate.plan.is_some() {
        graph.new_term_link(None)
    } else {
        continue_link
    };

    let letk_parent = graph.new_parent_link(None);
    let letk_term = graph.new_term(
        letk_parent,
        TermKind::Letk(letk_functions, letk_body_link),
        source,
    );
    graph.set_parent_link(letk_parent, Parent::Func(function));
    graph.set_term_link(body_link, letk_term);

    let mut arg_occs = Vec::with_capacity(new_params.len() + 1);
    let loop_occ = graph.new_free_occ_for_binder(loop_binder, continue_link);
    for param in &new_params {
        arg_occs.push(graph.new_free_occ_for_binder(*param, continue_link));
    }

    if let Some(plan) = candidate.plan {
        let base_lvar = fresh_lvar_derived(ctx, graph[data.var].var, "loop-base");
        let base_binder = graph.new_bound_var(base_lvar);
        arg_occs.push(graph.new_free_occ_for_binder(base_binder, continue_link));

        let base_expr_link = graph.new_expr_link(None);
        let base_parent = graph.new_parent_link(None);
        let base_term = graph.new_term(
            base_parent,
            TermKind::LetVal((base_binder, base_expr_link), continue_link),
            source,
        );
        graph.set_parent_link(base_parent, Parent::Term(letk_term));
        graph.set_term_link(letk_body_link, base_term);
        let base_expr_parent = graph.new_parent_link(None);
        let base_expr = graph.new_expr(base_expr_parent, ExprKind::Literal(plan.base), source);
        graph.set_expr_link(base_expr_link, base_expr);
        graph.set_parent_link(base_expr_parent, Parent::Term(base_term));

        let continue_args = graph.new_free_vars(arg_occs);
        let continue_parent = graph.new_parent_link(None);
        let continue_term = graph.new_term(
            continue_parent,
            TermKind::Continue(loop_occ, continue_args),
            source,
        );
        graph.set_term_link(continue_link, continue_term);
        graph.set_parent_link(continue_parent, Parent::Term(base_term));
    } else {
        let args = graph.new_free_vars(arg_occs);
        let continue_parent = graph.new_parent_link(None);
        let continue_term =
            graph.new_term(continue_parent, TermKind::Continue(loop_occ, args), source);
        graph.set_term_link(continue_link, continue_term);
        graph.set_parent_link(continue_parent, Parent::Term(letk_term));
    }

    graph[function].vars = new_args;
    graph[function].body = body_link;
    graph.backpatch_function(function);

    for call in &candidate.calls {
        if call.tail {
            rewrite_tail_call(graph, call.link, loop_binder, accumulator);
        } else if let Some(plan) = candidate.plan {
            rewrite_accumulate_call(
                ctx,
                graph,
                *call,
                self_cont,
                loop_binder,
                accumulator.expect("accumulator loop has an accumulator"),
                plan.prim,
                plan.mode,
                functions_by_binder,
            );
        }
    }

    if let Some(plan) = candidate.plan {
        let accumulator = accumulator.expect("accumulator loop has an accumulator");
        for exit in &candidate.exits {
            rewrite_exit(ctx, graph, *exit, accumulator, plan);
        }
    }

    // The function no longer recurses unless it escapes as a value.
    if graph
        .collect_occurrences(graph.function_binder_rooted(graph[function].var))
        .is_empty()
    {
        graph[function].is_rec = false;
    }

    true
}

fn rewrite_tail_call<'gc>(
    graph: &mut Graph<'gc>,
    link: Subterm,
    loop_binder: BoundVar,
    accumulator: Option<BoundVar>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    let TermKind::App(callee, args, cont) = graph[term].kind else {
        return;
    };

    // Drop the closure and return-continuation operands, then jump to the loop
    // block. `args` occurrences stay owned by this term.
    graph.remove_free_occ(callee);
    graph.remove_free_occ(cont);
    let mut vars = graph.new_free_vars(graph.free_vars_slice(&args).to_vec());
    if let Some(accumulator) = accumulator {
        let occ = graph.new_free_occ_for_binder(accumulator, link);
        graph.free_vars_push(&mut vars, occ);
    }
    let loop_occ = graph.new_free_occ_for_binder(loop_binder, link);
    graph[term].kind = TermKind::Continue(loop_occ, vars);
}

#[allow(clippy::too_many_arguments)]
fn rewrite_accumulate_call<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    call: SelfCall,
    self_cont: BoundVar,
    loop_binder: BoundVar,
    accumulator: BoundVar,
    prim: Value<'gc>,
    mode: AccMode,
    functions_by_binder: &HashMap<BoundVar, FunctionId>,
) {
    let Some(term) = graph.read_term_link(call.link) else {
        return;
    };
    let TermKind::App(callee, args, cont) = graph[term].kind else {
        return;
    };
    let Some((head, _, _, _)) =
        match_accumulate_cont(graph, call.cont, self_cont, functions_by_binder)
    else {
        return;
    };
    let source = graph[term].source;

    graph.remove_free_occ(callee);
    graph.remove_free_occ(cont);

    let next_lvar = fresh_lvar_derived(ctx, graph[head].var, "loop-acc");
    let next_acc = graph.new_bound_var(next_lvar);

    // The jump back to the loop becomes the body of the accumulator update.
    let continue_link = graph.new_term_link(None);
    let mut arg_occs: Vec<FreeVar> = graph.free_vars_slice(&args).to_vec();
    for occ in arg_occs.iter().copied() {
        graph.set_free_owner(occ, continue_link);
    }
    arg_occs.push(graph.new_free_occ_for_binder(next_acc, continue_link));
    let loop_occ = graph.new_free_occ_for_binder(loop_binder, continue_link);
    let continue_args = graph.new_free_vars(arg_occs);
    let continue_parent = graph.new_parent_link(None);
    let continue_term = graph.new_term(
        continue_parent,
        TermKind::Continue(loop_occ, continue_args),
        source,
    );
    graph.set_term_link(continue_link, continue_term);
    graph.set_parent_link(continue_parent, Parent::Term(term));

    // `acc' = (f ... head acc)`.
    let head_occ = graph.new_free_occ_for_binder(head, call.link);
    let acc_occ = graph.new_free_occ_for_binder(accumulator, call.link);
    let operands = match mode {
        AccMode::ConsReverse => [head_occ, acc_occ],
        AccMode::LeftFold => [acc_occ, head_occ],
    };
    let operands = graph.new_free_vars(operands);
    let expr_link = graph.new_expr_link(None);
    let expr_parent = graph.new_parent_link(None);
    let expr = graph.new_expr(expr_parent, ExprKind::PrimCall(prim, operands), source);
    graph.set_expr_link(expr_link, expr);
    graph.set_parent_link(expr_parent, Parent::Term(term));
    graph[term].kind = TermKind::LetVal((next_acc, expr_link), continue_link);
}

fn rewrite_exit<'gc>(
    ctx: MutationContext<'gc>,
    graph: &mut Graph<'gc>,
    link: Subterm,
    accumulator: BoundVar,
    plan: AccPlan<'gc>,
) {
    let Some(term) = graph.read_term_link(link) else {
        return;
    };
    let TermKind::Continue(cont, args) = graph[term].kind else {
        return;
    };
    let source = graph[term].source;
    let args = graph.free_vars_slice(&args).to_vec();
    if args.len() != 1 {
        return;
    }
    graph.remove_free_occ(args[0]);

    match plan.mode {
        AccMode::LeftFold => {
            // The accumulator already holds the exit's base value.
            let acc_occ = graph.new_free_occ_for_binder(accumulator, link);
            let args = graph.new_free_vars([acc_occ]);
            graph[term].kind = TermKind::Continue(cont, args);
        }
        AccMode::ConsReverse => {
            // `(reverse acc)`.
            let result_lvar = fresh_lvar_derived(ctx, graph[accumulator].var, "loop-result");
            let result = graph.new_bound_var(result_lvar);
            let continue_link = graph.new_term_link(None);
            graph.set_free_owner(cont, continue_link);
            let result_occ = graph.new_free_occ_for_binder(result, continue_link);
            let continue_args = graph.new_free_vars([result_occ]);
            let continue_parent = graph.new_parent_link(None);
            let continue_term = graph.new_term(
                continue_parent,
                TermKind::Continue(cont, continue_args),
                source,
            );
            graph.set_term_link(continue_link, continue_term);
            graph.set_parent_link(continue_parent, Parent::Term(term));

            let acc_occ = graph.new_free_occ_for_binder(accumulator, link);
            let reverse_args = graph.new_free_vars([acc_occ]);
            let expr_link = graph.new_expr_link(None);
            let expr_parent = graph.new_parent_link(None);
            let expr = graph.new_expr(
                expr_parent,
                ExprKind::PrimCall(Symbol::from_str(ctx, "reverse").into(), reverse_args),
                source,
            );
            graph.set_expr_link(expr_link, expr);
            graph.set_parent_link(expr_parent, Parent::Term(term));
            graph[term].kind = TermKind::LetVal((result, expr_link), continue_link);
        }
    }
}

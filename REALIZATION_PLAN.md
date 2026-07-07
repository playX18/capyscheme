# Realization Plan: Graph CPS Cutover

## Objective

Move CapyScheme from a tree-CPS-centered compiler pipeline to a Graph CPS
pipeline:

1. Complete the missing safe GCPS optimizations that make `capy/src/gcps`
   strong enough to replace the tree CPS optimizer.
2. Keep the current tree CPS path only as a temporary compatibility and
   differential-testing path.
3. Make `capy/src/expander/compile_cps.rs` emit Graph CPS directly.
4. Lower optimized Graph CPS directly to `capy/src/cps/linear`, without
   reifying back through `capy/src/cps/term.rs` on the default path.

The immediate implementation should stay disciplined: no one-off helper
functions, no unbounded code duplication, no ad-hoc primitive semantics, and no
large unrelated refactors.

## Current Facts

### Pipeline Shape

- `capy/src/compiler/pipeline.rs` still calls
  `compile_cps::cps_toplevel` to produce a tree `FuncRef`. This is now the
  earliest remaining tree-CPS dependency on the default path.
- The default path calls `gcps::optimize::optimize_func_to_graph_linear`, which
  converts the tree CPS function to Graph CPS, optimizes it, reifies graph
  procedure/free-variable metadata, and lowers the graph directly to
  `cps::linear::LinearProgram`.
- `LoweredProgram` carries the optimized tree CPS only as a compatibility and
  dump value; `linear_cps` carries the graph-linear program used by codegen when
  present.
- `CAPY_CPS_PIPELINE=tree`, `tree-cps`, or `shrink-contify` still selects the
  old tree CPS rewrite plus tree contification path.
- `capy/src/compiler/object.rs` still exposes `compile_cps_to_fasl_bytes` for
  tree compatibility, but runtime expanded-Scheme compilation now sends the
  full `LoweredProgram` to codegen so graph-linear code is preferred.
- `capy/src/cps/linear/ir.rs` now supports both tree identity and graph-native
  code identity through `CodeId::{Function, Continuation, GraphFunction,
  GraphContinuation}` and `LinearProgram.entry: CodeId`.
- `ModuleBuilder` and SSA lowering now declare, locate, and emit FASL code/data
  slots by generic `CodeId`, so graph-linear programs compile without
  reifying the optimized graph back through tree CPS for code generation.

### Existing GCPS Optimizer

`capy/src/gcps/optimize.rs` already has the right worklist structure for
incremental graph rewrites:

- occurrence-ring dead binding processing
- known function and known expression maps
- singleton function beta reduction
- singleton continuation beta reduction with complexity guards
- SCC and dominator contification
- graph repair helpers for active links, occurrence owners, parent links, and
  function links
- focused tests in the `#[cfg(test)]` block at the end of the file

The missing optimizations should extend this machinery instead of introducing a
second tree rewrite after `graph_to_cps`.

### Tree CPS Optimizer

`capy/src/cps/optimizer.rs` already contains a bounded inline pass:

- `rewrite` runs shrink rounds and then two inline rounds.
- `inline` uses size caps and Fibonacci thresholds.
- `copy_t`, `copy_c`, and `copy_f` provide the tree cloning machinery needed for
  multi-use inlining.

For the cutover, the tree optimizer should be treated as compatibility code. It
can receive small correctness fixes while it is still selectable, but new core
optimization work should land in GCPS.

## ICFP'01 Contification Verification

I re-read `icfp01.pdf` with `pdftotext`. The relevant paper algorithm is the
ADom dominator analysis in Figure 6:

- nodes are `{Root} union Cont union Func`
- edges include:
  - `Root -> fm`
  - `Root -> k` for every continuation
  - `Root -> f` for uncalled functions
  - `f -> g` for reachable tail calls
  - `k -> g` for reachable non-tail calls with return continuation `k`
- `ADom(f)` is the topmost non-root dominator of `f`; if the parent is root,
  the result is `Unknown` for reachable functions and `Uncalled` for
  unreachable functions.

The implementation matches the core algorithm:

- `capy/src/gcps/dom_contify.rs` defines `DomLoc::{Root, Func, Cont}` and
  collects `CallFact` records.
- It builds root-to-continuation edges, root-to-unreachable-function edges,
  function-to-function tail edges, and continuation-to-function non-tail edges.
- It computes dominators with `petgraph::algo::dominators::simple_fast`.
- `topmost_non_root_dominator` implements the "closest to Root" result used by
  ADom.
- `capy/src/gcps/optimize.rs` performs the transformation by substituting the
  selected return continuation, clearing function return-cont metadata, wrapping
  the chosen site with `Letk`, and rewriting calls to contified functions into
  `Continue`.

The implementation is not a literal whole-program copy of the paper:

- It runs per live `Fix` through the graph worklist, not as a whole-program
  `Func -> Return` annotation pass.
- It delegates insertion placement to the SCC contification-site logic rather
  than directly placing `A(f)=g` functions at the start of `g` and `A(f)=k`
  functions after `k`.
- Nested `Fix` scanning is conservative and should be tested explicitly.
- The ADom `Uncalled` concept is represented indirectly through general dead
  binding cleanup rather than as a standalone analysis result.

Conclusion: the core ADom analysis and transformation idea is implemented, but
the plan must add tests that prove the repo-specific per-`Fix` implementation is
safe and obtains the important paper examples.

## Design Rules

- Prefer reusable graph APIs and builder abstractions over pass-local one-off
  helpers.
- Add a graph clone helper before any optimization that duplicates non-singleton
  bodies.
- Keep primitive folding tied to the existing folding table or an explicit pure
  primitive whitelist. Do not infer purity from symbol names.
- Keep code-size growth bounded by gas, size caps, and pass-specific counters.
- Keep tree CPS as a fallback until graph-native lowering passes differential
  tests.
- Every graph rewrite must repair:
  - active term links
  - parent links
  - occurrence owners
  - dead binding queue state
  - known function/expression maps where applicable
  - worklist entries for affected subterms and occurrences

## Thought Experiments Before Coding

1. Replacing `optimize_func` with a graph return type immediately breaks
   `LoweredProgram`, dump generation, FASL compilation, `cps::reify`, and
   linear `CodeId`. Therefore the migration needs parallel graph APIs before
   the default path changes.

2. Adding arbitrary GCPS inlining by reusing the existing function body would
   mutate a shared body and corrupt other call sites. Therefore multi-use
   inlining and recursive unrolling require a graph clone helper first.

3. Simplifying `If` by replacing it with one branch must kill the skipped
   branch's direct free occurrences and the test occurrence, or dead bindings
   will remain live incorrectly.

4. Contifying a function to a return continuation outside the insertion site's
   scope is unsound. The existing scope guards should stay central and receive
   direct tests.

5. Direct graph-to-linear lowering cannot keep `CodeId::Function(FuncRef)` in
   the long term. Either linear code IDs must become graph-native, or a
   compatibility identity layer must be introduced. The former is the intended
   cutover direction.

## Milestone 0: Baseline And Paper Tests

Start by adding evidence, not new behavior.

Tasks:

1. Add direct tests for the ICFP'01 ADom cases in
   `capy/src/gcps/optimize.rs` or a dedicated GCPS test module:
   - Figure 7 shape: `f` reaches `g1`, `g2`, and tail-reaches `h`; `Dom` should
     contify what `Scc` cannot.
   - destination continuation case: multiple non-tail calls with the same local
     continuation contify to that continuation.
   - destination function case: tail-call chain contifies to a function
     dominator and inserts the wrapper at a legal site.
   - negative case: same function called with different continuations remains
     unknown.
   - escape case: function binder used as a value, argument, branch test, or
     raise payload blocks dominator contification.
   - nested `Fix` case: assert the current conservative behavior or prove and
     test a safe transformation.

2. Add a short source comment in `dom_contify.rs` mapping the implementation to
   Figure 6 edge classes. Keep it factual and concise.

3. Record baseline optimizer stats on representative examples with:
   - `CAPY_GCPS_CONTIFY=off`
   - `CAPY_GCPS_CONTIFY=scc`
   - `CAPY_GCPS_CONTIFY=dom`
   - `CAPY_GCPS_CONTIFY=dom-then-scc`

Deliverable commit:

- `gcps: cover dominator contification invariants`

## Milestone 1: Complete Safe Shrinking In GCPS

### 1. Dead `LetVal` Removal

Current state:

- `collect_redexes` records `known_exprs` for `LetVal`.
- dead binding processing kills expression free occurrences when a binder dies.
- the optimizer does not currently rewrite a dead `LetVal` term to its body.

Implementation:

1. Queue `TermKind::LetVal` in `collect_redexes`.
2. Add a `reduce_letval` path in `OptimizerState::run`.
3. If `graph.binder_is_dead(binder)`:
   - kill free occurrences in the expression
   - clear `known_exprs[binder]`
   - replace the `LetVal` active link with the body using the existing
     replacement helpers
   - enqueue the replacement body
4. Add `OptimizationStats::dead_letvals_removed`.
5. Profile the counter in `optimize_func`.

Tests:

- dead literal `LetVal` disappears
- dead primitive `LetVal` kills free occurrences in its expression
- live `LetVal` remains
- stale queued link after removal does not panic

### 2. Eta-Contraction

Implement shrinking eta rules in GCPS, not in tree CPS.

Eta-continuation pattern:

- `Letk` binds a continuation `k`.
- `k` has fixed arity.
- body is exactly `Continue(j, [x1, ..., xn])`.
- actuals are the formals in order.
- `j != k`.
- wrapper is not noinline, cold, reified, or variadic.
- substituting `j` for `k` is scope-safe.

Eta-function pattern:

- `Fix` binds a function `f`.
- `f` has fixed arity.
- body is exactly `App(g, args, kret)`.
- `args` are the formals in order.
- `kret` is the function return continuation.
- `g` is not `f`.
- wrapper is not recursive, variadic, cold, noinline, or metadata-sensitive.
- substituting `g` for `f` is scope-safe.

API notes:

- Use `subst_var_for_binders` for binder-to-binder substitutions such as wrapper
  binder to target binder.
- Use `subst_var_for_bound` only when substituting a specific free occurrence
  for a formal binder.
- Use existing graph repair helpers rather than tree-style rewrites.

Tests:

- eta-cont wrapper collapses
- eta-cont rejects reordered arguments
- eta-cont rejects self wrappers
- eta-cont rejects variadic, cold, noinline, and reified continuations
- eta-function wrapper collapses
- eta-function rejects recursive, variadic, escaping, and scope-unsafe wrappers

Counters:

- `eta_continuations`
- `eta_functions`

### 3. Boolean Branch Cleanup

Current GCPS has `TermKind::If`, but no branch simplification.

Implementation:

1. Queue `If` terms.
2. If the test binder is known to a literal through `known_exprs`, and the
   literal is a Scheme boolean:
   - replace the `If` with the selected branch
   - kill the test occurrence
   - kill direct occurrences in the skipped branch
   - repair owners and parent links through existing helpers
   - enqueue the selected branch
3. Add identical-branch cleanup only if structural equality is cheap and
   reliable. Otherwise defer it.

Tests:

- `#f` selects else branch
- non-false literal selects then branch if that matches current Scheme truth
  semantics in this IR
- unknown test keeps the branch
- skipped branch free vars are removed from occurrence rings

Counter:

- `known_boolean_branches`

### 4. Known Primitive Folding

Do this after dead `LetVal` and branch cleanup.

Implementation:

1. Start with all-constant arguments only.
2. Reuse `cps::fold::folding_table` or an explicit pure primitive table.
3. Replace foldable `PrimCall` `LetVal` with `ExprKind::Literal`.
4. Queue users of the binder so branch and beta rewrites can see the result.

Tests:

- arithmetic and comparison primitives fold
- non-constant arguments do not fold
- effectful or unknown primitives do not fold
- folded boolean feeds branch cleanup

Counter:

- `known_prim_propagations`

Deliverable commits:

- `gcps: remove dead letvals`
- `gcps: add eta contractions`
- `gcps: simplify known boolean branches`
- `gcps: fold known pure primcalls`

## Milestone 2: Graph Cloning And Bounded Inlining

GCPS already performs singleton beta reduction, which is the safe in-place form
of inlining. Multi-use inlining and recursive unrolling are not safe until graph
cloning exists.

### 1. Graph Clone Helper

Add a reusable graph cloning facility, likely near `gcps::graph` or
`gcps::convert`, that can clone a subterm with:

- fresh bound variables
- fresh function IDs and links
- fresh term and expression links
- fresh free occurrences owned by the cloned links
- substitution maps for value variables and continuation variables
- parent link backpatching
- known expression/function registration hooks

This helper should not know about a specific optimization. It should be usable
by future inlining, unrolling, and code motion.

Tests:

- clone a term with `LetVal`
- clone nested `Letk`
- clone nested `Fix`
- clone direct and indirect free occurrences
- clone with substitutions for formals and return continuation

### 2. Bounded Direct Recursive Unrolling

Only after the clone helper lands:

- detect direct self-recursive calls first
- require fixed arity
- reject cold/noinline functions
- reject mutually recursive SCC unrolling initially
- enforce a small unroll depth, starting at 1
- enforce a term-size budget
- update `Function::unroll_count` or an equivalent clone-local budget

Tests:

- one self-recursive unroll exposes singleton beta
- gas exhaustion stops unrolling
- max depth is respected
- noinline/cold functions are skipped
- mutually recursive cases are skipped

Counter:

- `recursive_unrolls`

### 3. General Bounded Inlining

Only after recursive unrolling proves the clone helper:

- port the useful parts of tree `inline` policy to GCPS
- keep size limits at least as conservative as tree CPS
- prefer small known non-recursive fixed-arity functions
- do not inline escaping functions

Deliverable commits:

- `gcps: add graph subterm cloning`
- `gcps: add bounded recursive unrolling`
- `gcps: add bounded known-call inlining`

## Milestone 3: Graph-Native Program And Reification

Status: mostly implemented on this branch. `GraphFunctionProgram`,
`GraphReifyInfo`, graph subterm cloning, and branch-hint preservation exist and
are used by graph-linear lowering. Remaining work is graph-native dump support
that can replace tree `.cps.scm` dumps.

The original `GraphProgram { graph, root }` only represented a function body.
The remaining graph-native expander work should build on the stable program
identity added for graph reification and graph-linear codegen.

Tasks:

1. Add a graph-native entry function/program wrapper:
   - entry function ID or equivalent code ID
   - binding
   - return continuation
   - params and variadic param
   - source, name, meta
   - body root

2. Add `GraphReifyInfo`:
   - entry code
   - live function list
   - live continuation list
   - free vars per function and continuation
   - continuation reification marking
   - binding-to-function and binding-to-continuation maps needed by direct-call
     lowering

3. Preserve branch hints in graph `If`.
   The current graph conversion loses tree `BranchHint` information by lowering
   back with normal hints. Add hints to `TermKind::If` or document why the loss
   is acceptable. Prefer preserving them.

4. Add graph pretty/dump support that can replace the current `.cps.scm` dump
   path for the graph pipeline.

Tests:

- graph reify matches tree reify on converted programs
- local continuations remain local
- escaping continuations are marked reified
- free vars match for functions and continuations
- branch hints round-trip through graph

Deliverable commits:

- `gcps: introduce graph program identity`
- `gcps: add graph reification`
- `gcps: preserve branch hints`

## Milestone 4: Direct Graph-To-Linear Lowering

Status: implemented on this branch for the default runtime compilation path.
`linearize_graph` lowers graph procedures to `cps::linear`, linear `CodeId` is
graph-capable, and SSA consumes graph code IDs. Remaining hardening work is
differential coverage, graph-native direct-call recognition in SSA, and broader
end-to-end Scheme examples.

Add graph lowering beside the existing tree lowering first. Do not replace
`cps::linear::linearize(&ReifyInfo)` until parity is proven.

Tasks:

1. Add `linearize_graph(&GraphProgram, &GraphReifyInfo)`.
2. Lower every `TermKind`:
   - `LetVal` to `Const` or `PrimCall`
   - `Fix` to `MakeClosure` and `ClosureSet`
   - `Letk` to local blocks or reified continuation closures
   - `If` to `Branch`
   - `Continue` to local `Jump` or tail call to reified continuation
   - `App` to `Call`
   - `Raise` to `Raise`
3. Keep the existing linear cleanup passes:
   - cache lowering
   - rest lowering
   - switch inference
   - constant hoisting
4. Decouple `CodeId` from tree `FuncRef` and `ContRef`.
   The target shape should use stable graph or linear code IDs, then adapt SSA
   declaration and direct-call lookup to those IDs.

Tests:

- one unit per graph term kind
- recursive closure allocation
- closure free-var initialization
- local continuation jump
- reified continuation closure
- variadic/rest lowering
- cache literal key lowering
- direct-call recognition
- switch inference still fires where expected

Differential test:

- tree CPS -> tree reify -> tree linear
- tree CPS -> graph -> graph reify -> graph linear
- compare normalized `LinearProgram`, ignoring unavoidable code ID formatting
  differences

Deliverable commits:

- `linear: add graph cps lowering`
- `linear: decouple code ids from tree cps refs`
- `ssa: consume graph-native linear code ids`

## Milestone 5: Graph-Emitting `compile_cps.rs`

Only start this after graph reify and graph-to-linear are working.

Tasks:

1. Add a graph builder that mirrors `CPSBuilder` semantics:
   - fresh variable generation
   - current topbox scope tracking
   - current meta tracking
   - reusable constructors for let value, let continuation, fix, app, continue,
     raise, and if
   - proper graph occurrence ownership at construction time

2. Avoid cloning the macro design blindly.
   Keep the builder ergonomic, but prefer typed helper methods over adding more
   macro arms unless the existing conversion logic becomes much worse without
   them.

3. Port in narrow slices:
   - literals and primitive calls
   - `Continue`, `App`, and `Raise`
   - `Letk` and local continuation bodies
   - `Fix` and function bodies
   - `If`
   - argument conversion helpers
   - top-level box/module/cache helpers
   - variadic/rest paths
   - `cps_toplevel` and `cps_func`

4. Keep the existing tree emitter during the migration:
   - graph emitter behind an env flag or feature
   - both emitters exercised in tests
   - tree emitter removed only after bootstrap and install verification passes
     reliably on the graph path

Tests:

- convert each core IL construct with both emitters and compare graph-linear to
  tree-linear
- compile representative Scheme snippets with both modes and compare results
- test module lookup, public/private refs, cache scopes, exceptions, multiple
  values, and variadic procedures

Deliverable commits:

- `gcps: add graph cps builder`
- `expander: emit graph cps for literals and calls`
- `expander: emit graph cps for control and procedures`
- `compiler: enable graph cps lowering mode`

## Milestone 6: Pipeline Cutover And Cleanup

Status: partially implemented. `LoweredProgram` can carry graph-linear code and
runtime expanded-Scheme compilation prefers it for codegen. The default path is
not fully graph-native yet because `compile_cps.rs` still emits tree CPS and the
pipeline still keeps a graph-to-tree fallback for dumps/debug compatibility.

Tasks:

1. Add `compile_graph_to_fasl_bytes` beside `compile_cps_to_fasl_bytes`.
2. Update `LoweredProgram` to represent either graph CPS or tree fallback during
   transition.
3. Update dump artifacts:
   - tree CPS dump for tree fallback
   - graph CPS dump for graph path
   - linear CPS dump from graph-linear path
4. Switch default compilation to graph-native:
   - core IL -> graph CPS
   - graph optimize
   - graph reify
   - graph linearize
   - SSA/codegen
5. Keep `CAPY_CPS_PIPELINE=tree` temporarily for differential debugging.
6. Once stage and install pass repeatedly:
   - remove tree optimizer from the default path
   - remove graph-to-tree reification from the default path
   - decide whether to keep tree CPS as a debug-only adapter or delete it in a
     later cleanup

Deliverable commits:

- `compiler: add graph fasl compilation path`
- `compiler: default to graph cps pipeline`
- `compiler: retire tree cps optimizer from default pipeline`

## Verification Plan

Run small checks after each milestone:

```sh
cargo fmt --all
cargo test -p capy gcps
cargo test -p capy cps::linear
cargo test -p capy compiler::pipeline
```

Before the requested bootstrap/install verification, clean stage directories.
Run the exact high-level gates requested:

```sh
rm -rf stage-0 stage-1 stage-2
make stage-0
rm -rf stage-0 stage-1 stage-2
make INSTALL=/home/adel/.local install -j4
```

If the Makefile variable expected by the install target is `PREFIX` rather than
`INSTALL`, verify with the requested command first, then rerun with the Makefile
variable only if needed:

```sh
rm -rf stage-0 stage-1 stage-2
make PREFIX=/home/adel/.local install -j4
```

Also run mode comparisons while both paths exist:

```sh
CAPY_CPS_PIPELINE=tree cargo test -p capy
CAPY_GCPS_CONTIFY=off cargo test -p capy gcps
CAPY_GCPS_CONTIFY=scc cargo test -p capy gcps
CAPY_GCPS_CONTIFY=dom cargo test -p capy gcps
CAPY_GCPS_CONTIFY=dom-then-scc cargo test -p capy gcps
```

## Commit Discipline

Use small, reviewable commits. Suggested sequence:

1. `docs: add graph cps realization plan`
2. `gcps: cover dominator contification invariants`
3. `gcps: remove dead letvals`
4. `gcps: add eta contractions`
5. `gcps: simplify known boolean branches`
6. `gcps: fold known pure primcalls`
7. `gcps: add graph subterm cloning`
8. `gcps: add graph reification`
9. `linear: add graph cps lowering`
10. `expander: emit graph cps`
11. `compiler: default to graph cps pipeline`

Before every commit:

```sh
git status --short
git diff --check
```

Do not include unrelated worktree changes. Existing unrelated local edits should
be left alone unless they directly conflict with the migration.

## Acceptance Criteria

- `icfp01.pdf` ADom behavior is covered by direct tests, including at least one
  Figure 6/Figure 7-style case.
- Missing GCPS shrinking optimizations are implemented with counters and tests.
- Any GCPS body-duplicating optimization uses a reusable graph clone helper.
- Graph reification provides all metadata currently supplied by tree
  `ReifyInfo`.
- Graph-to-linear lowering passes differential tests against tree-to-linear.
- `compile_cps.rs` can emit graph IR directly.
- The default compiler path no longer needs tree CPS optimization or
  graph-to-tree reification.
- `make stage-0` and `make INSTALL=/home/adel/.local install -j4` pass after
  cleaning `stage-*` directories.

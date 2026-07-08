# Implementation

CapyScheme aims to implement R6RS/R7RS specification and also be performant. Implementation of language
consists of "core expander", CPS compiler, runtime, and GC.

## Compiler layout

All native-compilation code lives under `capy/src/compiler/`:

```
compiler/
├── tree/        TreeIL types (`Term`, `TermKind`, `LVar`, `Proc`, …)
├── cps/         Graph CPS (GCPS): graph IR, optimization, reification, linearization
├── ssa/         Linear CPS (LCPS): basic blocks, cache/rest/switch/constant passes
├── cranelift/   Cranelift backend (`ModuleBuilder`, `SSABuilder`, `PrimitiveLowerer`)
├── pipeline.rs  TreeIL → graph CPS → linear CPS lowering orchestration
├── object.rs    lowered program → unified FASL bytes
├── direct.rs    Cranelift IR → machine code + relocations
├── debuginfo/   source locations for debug info and disassembly annotations
└── native/      platform ABI helpers (x64)
```

Macro expansion and TreeIL transformation passes remain in `expander/`. TreeIL type definitions live in `compiler/tree/` and are re-exported from `expander/term.rs` and `expander/core.rs`. TreeIL → graph CPS lowering is implemented in `expander/compile_cps.rs`.

The end-to-end lowering pipeline (`compiler/pipeline.rs`) is:

1. optional primitive resolution/expansion, free-variable resolution, and `letrectify` (when `expand_primitives` is enabled)
2. `fix_letrec`, `eta_expand`, `assignment_elimination`
3. `compile_cps::cps_toplevel` — TreeIL → graph CPS
4. `optimize_graph_func_to_linear` — optimize graph CPS, reify, linearize to LCPS, then run LCPS cleanup passes

## Frontend

Frontend is implemented as a Tree-Sitter based parser for Scheme. It reads Tree-Sitter nodes
and converts them to S-expressions. Each pair in source is annotated by putting source vector of that pair in format `#(filename line col)` into weak-table.

## Core expander

Core expander is a simple expander for R5RSque language without macros. It has the following forms supported:

- `begin`
- `lambda`
- `define`
- `if`
- `cond`: does not support `=>` form
- `case`: does not support `=>` form
- `let`, `let*`, `letrec`, and `letrec*`
- `and`
- `or`
- `do`

These forms all expand into TreeIL, which is based on what Guile has. TreeIL has following forms:

- `(lref <lvar>)`: loads local variable `<lvar>`
- `(lset <lvar> <term>)`: updates local variable `<lvar>` to the result of executing `<term>`.
- `(module-ref <module> <name> <public?>)`: lookups `<name>` in `<module>` or its public interface if `public?` is `#t`.
- `(module-set! <module> <name> <public?> <term>)`:
  lookups `<name>` in `<module>` or its public interface if `public?` is `#t`
  and then sets it to result of executing `<term>`
- `(toplevel-ref <module> <name>)`: lookups `<name>` in `<module>`
- `(toplevel-set! <module> <name> <term>)`: lookups `<name>` in `<module>` and updates its binding
  to the value of `<term>`.
- `(define <module> <name> <term>)`: defines variable `<name>` in `<module>` and binds
  its value to the value of `<term>`
- `(primref <name>)`: lookups primitive in `(capy)` modules and returns its reference
- `(primcall <name> <term*> ...)` : calls primitive in `(capy)` module with `<term*> ...` arguments. Internally primitive calls might be expanded into simpler forms or converted to `(call (module-ref ...) ...)` when some primitive conditions are violated e.g argument count is wrong.
- `(if <test> <cons> <alt>)`: if `<test>` term evaluates to `#t` will jump to `<cons>`, otherwise
  executes `<alt>`.
- `(seq <head> <tail>)`: evaluates `<head>` for effect, then `<tail>`.
- `(call <proc> <args> ...)`: calls procedure value produced by `<proc>`.
- `(values <term> ...)`: produces multiple values.
- `(receive <bindings> <producer> <consumer>)`: receives multiple values from `<producer>`, binds
  them to `<bindings>`, and executes `<consumer>`.
- `(with-continuation-mark <key> <mark> <result>)`: installs a continuation mark for the dynamic extent of `<result>`.
- `(fix <procs> <body>)`: binds mutually recursive procedures `<procs>`, and executes `<body>`. This form is produced only after fixing letrec pass.
- `(let <style> <lhs> <rhs> <body>)`: let-form, `style` indicates if its letrec, let\* or anything else. Executes `<rhs>` expressions and binds them to `<lhs>`, and then runs `<body>`.
- `(lambda <args> <body>)` / `(proc <args> <variadic> <body>)`: produces a procedure value with `args` and `variadic` being optional variadic argument binder.

### Optimizations on TreeIL

TreeIL passes run before CPS lowering and are orchestrated from `compiler/pipeline.rs`:

#### Fixing letrec

"Fixing" is a process of transforming letrec expressions into
`let` for simple or complex variables, and `fix` for mutually recursive
functions.

- Complex variables are those that are not free in their initializers, and also might be mutated.
- Simple variables are those that are free in their initializers, and are not mutated.
- Mutually recursive functions are those that are defined in terms of each other, and are not free
  in their initializers.

For a detailed discussion, see ["Fixing Letrec: A Faithful Yet
Efficient Implementation of Scheme's Recursive Binding Construct"](https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf), by
Oscar Waddell, Dipanwita Sarkar, and R. Kent Dybvig, as well as
"Fixing Letrec (reloaded)", by Abdulaziz Ghuloum and R. Kent Dybvig.

#### Eta expansion

Eta-expands procedure values where needed so later passes and CPS lowering can assume a uniform calling convention.

#### Assignment elimination

This pass removes `lset` forms and replaces them by boxes. Each mutable variable is wrapped by `box`, and accesses are converted to `box-ref` while assignment are converted to `box-set!`.

#### Primitive resolution

This pass must run to produce faster code. It detects calls to primitives, and converts them to `primcall` or `primref` in case of references. Later on, primcalls can be lowered directly to machine code rather than always calling into runtime.

#### Primitive expansion

Performs expansion of `primcall` forms into simpler forms where possible, also sometimes undoes work of primitive resolution by converting primcalls back into regular calls. Some examples of optimizations:

```
(memq x '(a b c)) => (if (eq? x 'a) #t (if (eq? x 'b) #t (if (eq? x 'c) #t) #f))
(+ 1 2 3 4) => (+ (1 2) (+ 3 4))
```

If argument count to primitive does not match this pass will convert primitive back to regular call.

### Graph CPS stage

CapyScheme relies on CPS in order to get cheap first-class continuations and easy exception handling. After TreeIL is optimized, `expander/compile_cps.rs` lowers it to **graph CPS** (GCPS), based on ["Compiling with Continuations, Continued"](https://matt.might.net/articles/compile-with-continuations-continued/).

GCPS is a graph-structured IR (`compiler/cps/graph.rs`) with three node kinds:

- **terms** — control flow
- **expressions** — values and primitive calls
- **functions** — procedures and continuations

Term forms:

- `(let-val <expr> <body>)`: bind the result of `<expr>` and continue in `<body>`
- `(fix <functions> <body>)`: bind mutually recursive functions
- `(letk <continuations> <body>)`: bind continuations, including mutually recursive ones
- `(if <test> <then> <else>)`: conditional branch
- `(continue <k> <args> ...)`: jump to continuation `<k>`
- `(app <proc> <args> ... <retk>)`: call `<proc>` with return continuation `<retk>`
- `(raise <kind> <args> ...)`: non-local control transfer for exceptions and assertions

Expression forms:

- `(literal <value>)`
- `(primcall <prim> <args> ...)`

Functions record parameters, optional variadic binder, optional return continuation (present for procedures, absent for continuations), free variables, and metadata used by later contification/reification.

#### Graph CPS optimization

`compiler/cps/optimize.rs` optimizes the graph in place using a worklist algorithm and a gas budget (default `42_000`). Major optimizations include:

- dead binding removal and `let-val` cleanup
- eta reduction on functions and continuations
- constant and identical-branch simplification
- known-primitive propagation
- singleton call and continuation inlining
- limited recursive unrolling
- contification: turn procedures into continuations so they can become basic blocks instead of heap closures. Modes are selectable via `CAPY_GCPS_CONTIFY` (`off`, `scc`, `dom`, or `dom-then-scc`)
- reification: mark continuations that still need a closure allocation (typically return/handler continuations passed to `app`)

After optimization, `compiler/cps/reify.rs` decides which graph functions become native functions vs reified continuations, and `compiler/cps/linear.rs` converts the graph into linear CPS.

Some primitive checks (for example `car` on a non-pair) are expanded during TreeIL → GCPS lowering into explicit branches and `raise` terms. Later, `primcall` forms in LCPS are lowered directly to machine code where possible.

Globals references are lowered to cache-cell sequences during LCPS lowering (`compiler/ssa/cache.rs`), roughly:

```
cache-ref 'variable
=> on miss: lookup, cache-set!, then variable-ref; on hit: variable-ref
```

This allows fast global accesses once they are cached.

### Linear CPS stage

Graph CPS is linearized into **linear CPS** (LCPS) in `compiler/ssa/`. Despite the module name, this is not traditional SSA yet: it is a collection of procedures, each consisting of basic blocks with explicit predecessors/successors.

A `LinearProgram` contains:

- an entry `CodeId` (graph function or reified continuation)
- a list of `Procedure`s, each either a `function` or a `continuation`

Each procedure has:

- parameters and optional variadic/rest binder
- free-variable list and source mapping back to TreeIL binders
- basic `Block`s containing `Instruction`s and a `Terminator`

Instructions include `const`, `make-closure`, `closure-ref` / `closure-set`, `cache-ref` / `cache-set`, `prim-call`, and rest-argument helpers (`rest-ref`, `rest-length`, `rest-predicate`, `rest-to-list`).

Terminators include `call`, `tail-call`, `raise`, `jump`, `branch`, and `switch`.

#### LCPS cleanup passes

`compiler/ssa/mod.rs::finish_procedure` runs post-linearization passes on each procedure:

- switch inference (`ssa/switch.rs`)
- rest-argument lowering (`ssa/rest.rs`)
- cache-operation lowering (`ssa/cache.rs`)
- constant hoisting (`ssa/constant.rs`)

### Cranelift backend

LCPS is lowered to Cranelift IR in `compiler/cranelift/`.

`ModuleBuilder` owns the whole compilation unit: symbol tables for functions/data/cache cells/constants, runtime thunks, and FASL emission. For each LCPS procedure, `SSABuilder` (`cranelift/linear.rs`) builds a Cranelift function:

- continuations that were not reified become intraprocedural basic blocks
- reified continuations and functions become separate Cranelift functions
- `PrimitiveLowerer` (`cranelift/primitive.rs`) lowers `prim-call` instructions to Cranelift instructions or runtime calls

`compiler/direct.rs` compiles finalized Cranelift functions to machine code bytes and relocations. `compiler/debuginfo/` attaches source locations used in Cranelift dumps and annotated disassembly.

### Output

The normal compiler output is a **unified FASL image** (`compiler/object.rs`), not a platform shared object. `compile_lowered_to_fasl_bytes` builds a `ModuleBuilder` from the linear CPS program, compiles each procedure with Cranelift, and writes a compressed FASL program containing code bytes, constants, cache cells, relocations, and source maps.

For debugging, `compiler/pipeline.rs` can dump intermediate artifacts:

- `.ir.scm` / `.ir.noopt.scm` — TreeIL before/after optimization
- `.gcps.scm` — graph CPS
- `.lcps.scm` — linear CPS
- Cranelift IR and host disassembly (via `BackendDumpOptions`)

`compiler/linkutils.rs` contains platform linker helpers, but the main bootstrap and load path uses FASL images (see `docs/BOOTSTRAP.md`).

## Runtime
